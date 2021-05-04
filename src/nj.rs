use crate::debruijn::{DbCtx, Idx, Shift};
use crate::prop::Prop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proof {
    pub prop: Prop,
    pub kind: ProofKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProofKind {
    /// variable in de Bruijn index
    Var(Idx),
    /// `\x => t`
    Abs(Box<Proof>),
    /// `t1 t2`
    App(Box<Proof>, Box<Proof>),
    /// `(t1, t2, ..., tN)`
    ConjIntro(Vec<Proof>),
    /// `let (.., x, ..) = t in x`
    ConjElim(
        Box<Proof>,
        /// i = projection index (0 <= i < n)
        usize,
        /// n = length of the tuple
        usize,
    ),
    /// `CtorI<p1, p2, ..., pN>(t)`
    DisjIntro(Box<Proof>, usize, usize),
    /// `(match t1 { Ctor1 => u1, Ctor2 => u2, ..., CtorN => uN }): p`
    DisjElim(Box<Proof>, Vec<Proof>),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeError;

impl Proof {
    pub fn check_has_type(&self, prop: &Prop) {
        self.check_has_type_in(&mut DbCtx::new(), prop);
    }

    pub fn check_type(&self) {
        self.check_type_in(&mut DbCtx::new());
    }

    pub fn check_has_type_in(&self, ctx: &mut DbCtx<Prop>, prop: &Prop) {
        assert_eq!(self.prop, *prop);
        self.check_type_in(ctx);
    }

    pub fn check_type_in(&self, ctx: &mut DbCtx<Prop>) {
        match self.kind {
            ProofKind::Var(idx) => {
                assert_eq!(ctx[idx], self.prop);
            }
            ProofKind::Abs(ref body) => {
                let (abstype, body_type) = self.prop.as_impl().unwrap();
                let mut ctx = ctx.push(abstype.clone());
                body.check_has_type_in(&mut ctx, body_type);
            }
            ProofKind::App(ref lhs, ref rhs) => {
                let (ltl, ltr) = lhs.prop.as_impl().unwrap();
                assert_eq!(*ltr, self.prop);
                lhs.check_type_in(ctx);
                rhs.check_has_type_in(ctx, ltl);
            }
            ProofKind::ConjIntro(ref children) => {
                let child_types = self.prop.as_conj().unwrap();
                assert_eq!(children.len(), child_types.len());
                for (child, child_type) in children.iter().zip(child_types) {
                    child.check_has_type_in(ctx, child_type);
                }
            }
            ProofKind::ConjElim(ref sub, i, n) => {
                let child_types = sub.prop.as_conj().unwrap();
                assert_eq!(child_types.len(), n);
                assert_eq!(child_types[i], self.prop);
                sub.check_type_in(ctx);
            }
            ProofKind::DisjIntro(ref sub, i, n) => {
                let child_types = self.prop.as_disj().unwrap();
                assert_eq!(child_types.len(), n);
                sub.check_has_type_in(ctx, &child_types[i]);
            }
            ProofKind::DisjElim(ref sub, ref branches) => {
                let child_types = sub.prop.as_disj().unwrap();
                sub.check_type_in(ctx);
                assert_eq!(child_types.len(), branches.len());
                for (branch, child_type) in branches.iter().zip(child_types) {
                    let mut ctx = ctx.push(child_type.clone());
                    branch.check_has_type_in(&mut ctx, child_type);
                }
            }
        }
    }

    pub fn reduce_all(&mut self) {
        while self.try_reduce_here() {}
        match self.kind {
            ProofKind::Var(_) => {}
            ProofKind::Abs(ref mut body) => body.reduce_all(),
            ProofKind::App(ref mut lhs, ref mut rhs) => {
                lhs.reduce_all();
                rhs.reduce_all();
            }
            ProofKind::ConjIntro(ref mut children) => {
                for child in children {
                    child.reduce_all();
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _) => sub.reduce_all(),
            ProofKind::DisjIntro(ref mut sub, _, _) => sub.reduce_all(),
            ProofKind::DisjElim(ref mut sub, ref mut branches) => {
                sub.reduce_all();
                for branch in branches {
                    branch.reduce_all();
                }
            }
        }
    }

    pub fn try_reduce_here(&mut self) -> bool {
        const SENTINEL: Proof = Proof {
            kind: ProofKind::Var(Idx(0)),
            prop: Prop::Conj(Vec::new()),
        };

        match self.kind {
            ProofKind::App(ref mut fun, ref mut arg) => {
                if let ProofKind::Abs(ref mut body) = fun.kind {
                    body.subst(Idx(0), arg, 0);
                    let body = std::mem::replace(&mut **body, SENTINEL);
                    *self = body;
                    return true;
                }
            }
            ProofKind::ConjElim(ref mut tup, i, _) => {
                if let ProofKind::ConjIntro(ref mut elems) = tup.kind {
                    let elem = elems.swap_remove(i);
                    *self = elem;
                    return true;
                }
            }
            ProofKind::DisjElim(ref mut discr, ref mut branches) => {
                if let ProofKind::DisjIntro(ref mut arg, i, _) = discr.kind {
                    branches[i].subst(Idx(0), arg, 0);
                    let body = branches.swap_remove(i);
                    *self = body;
                    return true;
                }
            }
            _ => {}
        }
        false
    }

    pub fn subst(&mut self, target: Idx, repl: &Proof, by: usize) {
        match self.kind {
            ProofKind::Var(ref mut idx) => {
                if *idx > target {
                    idx.0 -= 1
                } else if *idx == target {
                    *self = repl.shifted(target.s(), by);
                }
            }
            ProofKind::Abs(ref mut body) => {
                body.subst(target.s(), repl, by + 1);
            }
            ProofKind::App(ref mut lhs, ref mut rhs) => {
                lhs.subst(target, repl, by);
                rhs.subst(target, repl, by);
            }
            ProofKind::ConjIntro(ref mut children) => {
                for child in children {
                    child.subst(target, repl, by);
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _) => {
                sub.subst(target, repl, by);
            }
            ProofKind::DisjIntro(ref mut sub, _, _) => {
                sub.subst(target, repl, by);
            }
            ProofKind::DisjElim(ref mut sub, ref mut branches) => {
                sub.subst(target, repl, by);
                for branch in branches {
                    branch.subst(target, repl, by + 1);
                }
            }
        }
    }
}

impl ProofKind {
    #[allow(non_snake_case)]
    pub fn AbsS(body: Proof) -> Self {
        Self::Abs(Box::new(body))
    }

    #[allow(non_snake_case)]
    pub fn AppS(lhs: Proof, rhs: Proof) -> Self {
        Self::App(Box::new(lhs), Box::new(rhs))
    }

    #[allow(non_snake_case)]
    pub fn ConjElimS(sub: Proof, i: usize, n: usize) -> Self {
        Self::ConjElim(Box::new(sub), i, n)
    }

    #[allow(non_snake_case)]
    pub fn DisjIntroS(sub: Proof, i: usize, n: usize) -> Self {
        Self::DisjIntro(Box::new(sub), i, n)
    }

    #[allow(non_snake_case)]
    pub fn DisjElimS(sub: Proof, branches: Vec<Proof>) -> Self {
        Self::DisjElim(Box::new(sub), branches)
    }
}

impl Shift for Proof {
    fn shift(&mut self, after: Idx, by: usize) {
        match self.kind {
            ProofKind::Var(ref mut idx) => {
                idx.shift(after, by);
            }
            ProofKind::Abs(ref mut body) => {
                body.shift(after.s(), by);
            }
            ProofKind::App(ref mut lhs, ref mut rhs) => {
                lhs.shift(after, by);
                rhs.shift(after, by);
            }
            ProofKind::ConjIntro(ref mut children) => {
                for child in children {
                    child.shift(after, by);
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _) => {
                sub.shift(after, by);
            }
            ProofKind::DisjIntro(ref mut sub, _, _) => {
                sub.shift(after, by);
            }
            ProofKind::DisjElim(ref mut sub, ref mut branches) => {
                sub.shift(after, by);
                for branch in branches {
                    branch.shift(after.s(), by);
                }
            }
        }
    }
}

#[allow(non_snake_case)]
pub mod ProofKindShorthands {
    use super::*;
    pub use ProofKind::*;

    #[allow(non_snake_case)]
    pub fn AbsS(body: Proof) -> ProofKind {
        ProofKind::AbsS(body)
    }

    #[allow(non_snake_case)]
    pub fn AppS(lhs: Proof, rhs: Proof) -> ProofKind {
        ProofKind::AppS(lhs, rhs)
    }

    #[allow(non_snake_case)]
    pub fn ConjElimS(sub: Proof, i: usize, n: usize) -> ProofKind {
        ProofKind::ConjElimS(sub, i, n)
    }

    #[allow(non_snake_case)]
    pub fn DisjIntroS(sub: Proof, i: usize, n: usize) -> ProofKind {
        ProofKind::DisjIntroS(sub, i, n)
    }

    #[allow(non_snake_case)]
    pub fn DisjElimS(sub: Proof, branches: Vec<Proof>) -> ProofKind {
        ProofKind::DisjElimS(sub, branches)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prop::{IdGen, PropShorthands};

    #[test]
    fn test_check_type1() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let pf = Proof {
            prop: ImplS(Atom(id1), Atom(id1)),
            kind: AbsS(Proof {
                prop: Atom(id1),
                kind: Var(Idx(0)),
            }),
        };
        pf.check_type();
    }

    #[test]
    #[should_panic(expected = "called `Option::unwrap()` on a `None` value")]
    fn test_check_type2() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let pf = Proof {
            prop: Atom(id1),
            kind: Var(Idx(0)),
        };
        pf.check_type();
    }

    #[test]
    fn test_subst1() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let mut body = Proof {
            prop: ImplS(a(), a()),
            kind: Var(Idx(0)),
        };
        let arg = Proof {
            prop: ImplS(a(), a()),
            kind: AbsS(Proof {
                prop: a(),
                kind: Var(Idx(0)),
            }),
        };
        body.subst(Idx(0), &arg, 0);
        assert_eq!(
            body,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(Proof {
                    prop: a(),
                    kind: Var(Idx(0))
                })
            }
        );
    }

    #[test]
    fn test_try_reduce_here1() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let mut pf = Proof {
            prop: ImplS(a(), a()),
            kind: AppS(
                Proof {
                    prop: ImplS(ImplS(a(), a()), ImplS(a(), a())),
                    kind: AbsS(Proof {
                        prop: ImplS(a(), a()),
                        kind: Var(Idx(0)),
                    }),
                },
                Proof {
                    prop: ImplS(a(), a()),
                    kind: AbsS(Proof {
                        prop: a(),
                        kind: Var(Idx(0)),
                    }),
                },
            ),
        };
        let updated = pf.try_reduce_here();
        assert_eq!(updated, true);
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(Proof {
                    prop: a(),
                    kind: Var(Idx(0))
                })
            }
        );
    }

    #[test]
    fn test_reduce_all1() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let mut pf = Proof {
            prop: ImplS(Atom(id1), Atom(id1)),
            kind: AbsS(Proof {
                prop: Atom(id1),
                kind: Var(Idx(0)),
            }),
        };
        pf.reduce_all();
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(Atom(id1), Atom(id1)),
                kind: AbsS(Proof {
                    prop: Atom(id1),
                    kind: Var(Idx(0))
                })
            }
        );
    }

    #[test]
    fn test_reduce_all2() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let mut pf = Proof {
            prop: ImplS(a(), a()),
            kind: AppS(
                Proof {
                    prop: ImplS(ImplS(a(), a()), ImplS(a(), a())),
                    kind: AbsS(Proof {
                        prop: ImplS(a(), a()),
                        kind: Var(Idx(0)),
                    }),
                },
                Proof {
                    prop: ImplS(a(), a()),
                    kind: AbsS(Proof {
                        prop: a(),
                        kind: Var(Idx(0)),
                    }),
                },
            ),
        };
        pf.reduce_all();
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(Proof {
                    prop: a(),
                    kind: Var(Idx(0))
                })
            }
        );
    }
}
