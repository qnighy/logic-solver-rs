use std::borrow::Cow;

use crate::debruijn::{DbCtx, Idx, Shift};
use crate::prop::{ConjType, ImplType, Prop, BOTTOM};

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
    Abs(Box<Proof>, ImplType),
    /// `t1 t2`
    App(Box<Proof>, Box<Proof>, ImplType),
    /// `(t1, t2, ..., tN)`
    ConjIntro(Vec<Proof>, ConjType),
    /// `let (.., x, ..) = t in x`
    ConjElim(
        Box<Proof>,
        /// i = projection index (0 <= i < n)
        usize,
        /// n = length of the tuple
        usize,
        ConjType,
    ),
    /// `CtorI<p1, p2, ..., pN>(t)`
    DisjIntro(Box<Proof>, usize, usize),
    /// `(match t1 { Ctor1 => u1, Ctor2 => u2, ..., CtorN => uN }): p`
    DisjElim(Box<Proof>, Vec<Proof>),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeError;

macro_rules! assert_eqtype {
    ($lhs:expr, $rhs:expr) => {
        match (&$lhs, &$rhs) {
            (lhs, rhs) => {
                if !Prop::eqtype(lhs, rhs) {
                    panic!(
                        "assert_eqtype!({}, {})\n\n    left  = {:?}\n    right = {:?}\n\n",
                        stringify!($lhs),
                        stringify!($rhs),
                        lhs,
                        rhs
                    );
                }
            }
        }
    };
}

impl Proof {
    pub fn check_has_type(&self, prop: &Prop) {
        self.check_has_type_in(&mut DbCtx::new(), prop);
    }

    pub fn check_type(&self) {
        self.check_type_in(&mut DbCtx::new());
    }

    pub fn check_has_type_in(&self, ctx: &mut DbCtx<Prop>, prop: &Prop) {
        assert_eqtype!(self.prop, *prop);
        self.check_type_in(ctx);
    }

    pub fn check_type_in(&self, ctx: &mut DbCtx<Prop>) {
        match self.kind {
            ProofKind::Var(idx) => {
                assert_eqtype!(ctx[idx], self.prop);
            }
            ProofKind::Abs(ref body, it) => {
                let (abstype, body_type) = match it {
                    ImplType::Normal => self.prop.as_impl().unwrap(),
                    ImplType::Neg => (self.prop.as_neg().unwrap(), &BOTTOM),
                };
                let mut ctx = ctx.push(abstype.clone());
                body.check_has_type_in(&mut ctx, body_type);
            }
            ProofKind::App(ref lhs, ref rhs, it) => {
                let (ltl, ltr) = match it {
                    ImplType::Normal => lhs.prop.as_impl().unwrap(),
                    ImplType::Neg => (lhs.prop.as_neg().unwrap(), &BOTTOM),
                };
                assert_eqtype!(*ltr, self.prop);
                lhs.check_type_in(ctx);
                rhs.check_has_type_in(ctx, ltl);
            }
            ProofKind::ConjIntro(ref children, ct) => {
                let child_types = match ct {
                    ConjType::Normal => Cow::Borrowed(self.prop.as_conj().unwrap()),
                    ConjType::Equiv => {
                        let (lhs, rhs) = self.prop.as_equiv().unwrap();
                        Cow::Owned(vec![
                            Prop::ImplS(lhs.clone(), rhs.clone()),
                            Prop::ImplS(rhs.clone(), lhs.clone()),
                        ])
                    }
                };
                assert_eq!(children.len(), child_types.len());
                for (child, child_type) in children.iter().zip(&child_types[..]) {
                    child.check_has_type_in(ctx, child_type);
                }
            }
            ProofKind::ConjElim(ref sub, i, n, ct) => {
                let child_types = match ct {
                    ConjType::Normal => Cow::Borrowed(sub.prop.as_conj().unwrap()),
                    ConjType::Equiv => {
                        let (lhs, rhs) = sub.prop.as_equiv().unwrap();
                        Cow::Owned(vec![
                            Prop::ImplS(lhs.clone(), rhs.clone()),
                            Prop::ImplS(rhs.clone(), lhs.clone()),
                        ])
                    }
                };
                assert_eq!(child_types.len(), n);
                assert_eqtype!(child_types[i], self.prop);
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
                    branch.check_has_type_in(&mut ctx, &self.prop);
                }
            }
        }
    }

    pub fn reduce_all(&mut self) {
        while self.try_reduce_here() {}
        match self.kind {
            ProofKind::Var(_) => {}
            ProofKind::Abs(ref mut body, _) => body.reduce_all(),
            ProofKind::App(ref mut lhs, ref mut rhs, _) => {
                lhs.reduce_all();
                rhs.reduce_all();
            }
            ProofKind::ConjIntro(ref mut children, _) => {
                for child in children {
                    child.reduce_all();
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _, _) => sub.reduce_all(),
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
            ProofKind::App(ref mut fun, ref mut arg, _) => {
                if let ProofKind::Abs(ref mut body, _) = fun.kind {
                    body.subst(Idx(0), arg, 0);
                    let body = std::mem::replace(&mut **body, SENTINEL);
                    *self = body;
                    return true;
                }
            }
            ProofKind::ConjElim(ref mut tup, i, _, _) => {
                if let ProofKind::ConjIntro(ref mut elems, _) = tup.kind {
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
            ProofKind::Var(ref mut idx) =>
            {
                #[allow(clippy::comparison_chain)]
                if *idx > target {
                    idx.0 -= 1
                } else if *idx == target {
                    *self = repl.shifted(Idx(0), by);
                }
            }
            ProofKind::Abs(ref mut body, _) => {
                body.subst(target.s(), repl, by + 1);
            }
            ProofKind::App(ref mut lhs, ref mut rhs, _) => {
                lhs.subst(target, repl, by);
                rhs.subst(target, repl, by);
            }
            ProofKind::ConjIntro(ref mut children, _) => {
                for child in children {
                    child.subst(target, repl, by);
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _, _) => {
                sub.subst(target, repl, by);
            }
            ProofKind::DisjIntro(ref mut sub, _, _) => {
                sub.subst(target, repl, by);
            }
            ProofKind::DisjElim(ref mut sub, ref mut branches) => {
                sub.subst(target, repl, by);
                for branch in branches {
                    branch.subst(target.s(), repl, by + 1);
                }
            }
        }
    }
}

impl ProofKind {
    #[allow(non_snake_case)]
    pub fn AbsS(body: Proof, it: ImplType) -> Self {
        Self::Abs(Box::new(body), it)
    }

    #[allow(non_snake_case)]
    pub fn AppS(lhs: Proof, rhs: Proof, it: ImplType) -> Self {
        Self::App(Box::new(lhs), Box::new(rhs), it)
    }

    #[allow(non_snake_case)]
    pub fn ConjElimS(sub: Proof, i: usize, n: usize, ct: ConjType) -> Self {
        Self::ConjElim(Box::new(sub), i, n, ct)
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
            ProofKind::Abs(ref mut body, _) => {
                body.shift(after.s(), by);
            }
            ProofKind::App(ref mut lhs, ref mut rhs, _) => {
                lhs.shift(after, by);
                rhs.shift(after, by);
            }
            ProofKind::ConjIntro(ref mut children, _) => {
                for child in children {
                    child.shift(after, by);
                }
            }
            ProofKind::ConjElim(ref mut sub, _, _, _) => {
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
    pub fn AbsS(body: Proof, it: ImplType) -> ProofKind {
        ProofKind::AbsS(body, it)
    }

    #[allow(non_snake_case)]
    pub fn AppS(lhs: Proof, rhs: Proof, it: ImplType) -> ProofKind {
        ProofKind::AppS(lhs, rhs, it)
    }

    #[allow(non_snake_case)]
    pub fn ConjElimS(sub: Proof, i: usize, n: usize, ct: ConjType) -> ProofKind {
        ProofKind::ConjElimS(sub, i, n, ct)
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
            kind: AbsS(
                Proof {
                    prop: Atom(id1),
                    kind: Var(Idx(0)),
                },
                ImplType::Normal,
            ),
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
    fn test_check_type3() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let pf = Proof {
            prop: ImplS(Disj(vec![Atom(id1), Disj(vec![])]), Atom(id1)),
            kind: AppS(
                Proof {
                    prop: ImplS(
                        ImplS(Disj(vec![Atom(id1), Disj(vec![])]), Atom(id1)),
                        ImplS(Disj(vec![Atom(id1), Disj(vec![])]), Atom(id1)),
                    ),
                    kind: AbsS(
                        Proof {
                            prop: ImplS(Disj(vec![Atom(id1), Disj(vec![])]), Atom(id1)),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                Proof {
                    prop: ImplS(Disj(vec![Atom(id1), Disj(vec![])]), Atom(id1)),
                    kind: AbsS(
                        Proof {
                            prop: Atom(id1),
                            kind: DisjElimS(
                                Proof {
                                    prop: Disj(vec![Atom(id1), Disj(vec![])]),
                                    kind: Var(Idx(0)),
                                },
                                vec![
                                    Proof {
                                        prop: Atom(id1),
                                        kind: Var(Idx(0)),
                                    },
                                    Proof {
                                        prop: Atom(id1),
                                        kind: DisjElimS(
                                            Proof {
                                                prop: Disj(vec![]),
                                                kind: Var(Idx(0)),
                                            },
                                            vec![],
                                        ),
                                    },
                                ],
                            ),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
        };
        pf.check_type();
    }

    #[test]
    fn test_check_type4() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let pf = Proof {
            prop: NegS(Conj(vec![
                ImplS(Atom(id1), NegS(Atom(id1))),
                ImplS(NegS(Atom(id1)), Atom(id1)),
            ])),
            kind: AbsS(
                Proof {
                    prop: Disj(vec![]),
                    kind: AppS(
                        Proof {
                            prop: NegS(Atom(id1)),
                            kind: AppS(
                                Proof {
                                    prop: ImplS(Atom(id1), NegS(Atom(id1))),
                                    kind: ConjElimS(
                                        Proof {
                                            prop: Conj(vec![
                                                ImplS(Atom(id1), NegS(Atom(id1))),
                                                ImplS(NegS(Atom(id1)), Atom(id1)),
                                            ]),
                                            kind: Var(Idx(0)),
                                        },
                                        0,
                                        2,
                                        ConjType::Normal,
                                    ),
                                },
                                Proof {
                                    prop: Atom(id1),
                                    kind: AppS(
                                        Proof {
                                            prop: ImplS(NegS(Atom(id1)), Atom(id1)),
                                            kind: ConjElimS(
                                                Proof {
                                                    prop: Conj(vec![
                                                        ImplS(Atom(id1), NegS(Atom(id1))),
                                                        ImplS(NegS(Atom(id1)), Atom(id1)),
                                                    ]),
                                                    kind: Var(Idx(0)),
                                                },
                                                1,
                                                2,
                                                ConjType::Normal,
                                            ),
                                        },
                                        Proof {
                                            prop: NegS(Atom(id1)),
                                            kind: AbsS(
                                                Proof {
                                                    prop: Disj(vec![]),
                                                    kind: AppS(
                                                        Proof {
                                                            prop: NegS(Atom(id1)),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        Atom(id1),
                                                                        NegS(Atom(id1)),
                                                                    ),
                                                                    kind: ConjElimS(
                                                                        Proof {
                                                                            prop: Conj(vec![
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    NegS(Atom(id1)),
                                                                                ),
                                                                                ImplS(
                                                                                    NegS(Atom(id1)),
                                                                                    Atom(id1),
                                                                                ),
                                                                            ]),
                                                                            kind: Var(Idx(1)),
                                                                        },
                                                                        0,
                                                                        2,
                                                                        ConjType::Normal,
                                                                    ),
                                                                },
                                                                Proof {
                                                                    prop: Atom(id1),
                                                                    kind: Var(Idx(0)),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        Proof {
                                                            prop: Atom(id1),
                                                            kind: Var(Idx(0)),
                                                        },
                                                        ImplType::Neg,
                                                    ),
                                                },
                                                ImplType::Neg,
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        Proof {
                            prop: Atom(id1),
                            kind: AppS(
                                Proof {
                                    prop: ImplS(NegS(Atom(id1)), Atom(id1)),
                                    kind: ConjElimS(
                                        Proof {
                                            prop: Conj(vec![
                                                ImplS(Atom(id1), NegS(Atom(id1))),
                                                ImplS(NegS(Atom(id1)), Atom(id1)),
                                            ]),
                                            kind: Var(Idx(0)),
                                        },
                                        1,
                                        2,
                                        ConjType::Normal,
                                    ),
                                },
                                Proof {
                                    prop: NegS(Atom(id1)),
                                    kind: AbsS(
                                        Proof {
                                            prop: Disj(vec![]),
                                            kind: AppS(
                                                Proof {
                                                    prop: NegS(Atom(id1)),
                                                    kind: AppS(
                                                        Proof {
                                                            prop: ImplS(Atom(id1), NegS(Atom(id1))),
                                                            kind: ConjElimS(
                                                                Proof {
                                                                    prop: Conj(vec![
                                                                        ImplS(
                                                                            Atom(id1),
                                                                            NegS(Atom(id1)),
                                                                        ),
                                                                        ImplS(
                                                                            NegS(Atom(id1)),
                                                                            Atom(id1),
                                                                        ),
                                                                    ]),
                                                                    kind: Var(Idx(1)),
                                                                },
                                                                0,
                                                                2,
                                                                ConjType::Normal,
                                                            ),
                                                        },
                                                        Proof {
                                                            prop: Atom(id1),
                                                            kind: Var(Idx(0)),
                                                        },
                                                        ImplType::Normal,
                                                    ),
                                                },
                                                Proof {
                                                    prop: Atom(id1),
                                                    kind: Var(Idx(0)),
                                                },
                                                ImplType::Neg,
                                            ),
                                        },
                                        ImplType::Neg,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        ImplType::Neg,
                    ),
                },
                ImplType::Neg,
            ),
        };
        pf.check_type();
    }

    #[test]
    fn test_check_type5() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let pf = Proof {
            prop: NegS(EquivS(Atom(id1), NegS(Atom(id1)))),
            kind: AbsS(
                Proof {
                    prop: Disj(vec![]),
                    kind: AppS(
                        Proof {
                            prop: NegS(Atom(id1)),
                            kind: AppS(
                                Proof {
                                    prop: ImplS(Atom(id1), NegS(Atom(id1))),
                                    kind: ConjElimS(
                                        Proof {
                                            prop: EquivS(Atom(id1), NegS(Atom(id1))),
                                            kind: Var(Idx(0)),
                                        },
                                        0,
                                        2,
                                        ConjType::Equiv,
                                    ),
                                },
                                Proof {
                                    prop: Atom(id1),
                                    kind: AppS(
                                        Proof {
                                            prop: ImplS(NegS(Atom(id1)), Atom(id1)),
                                            kind: ConjElimS(
                                                Proof {
                                                    prop: EquivS(Atom(id1), NegS(Atom(id1))),
                                                    kind: Var(Idx(0)),
                                                },
                                                1,
                                                2,
                                                ConjType::Equiv,
                                            ),
                                        },
                                        Proof {
                                            prop: NegS(Atom(id1)),
                                            kind: AbsS(
                                                Proof {
                                                    prop: Disj(vec![]),
                                                    kind: AppS(
                                                        Proof {
                                                            prop: NegS(Atom(id1)),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        Atom(id1),
                                                                        NegS(Atom(id1)),
                                                                    ),
                                                                    kind: ConjElimS(
                                                                        Proof {
                                                                            prop: EquivS(
                                                                                Atom(id1),
                                                                                NegS(Atom(id1)),
                                                                            ),
                                                                            kind: Var(Idx(1)),
                                                                        },
                                                                        0,
                                                                        2,
                                                                        ConjType::Equiv,
                                                                    ),
                                                                },
                                                                Proof {
                                                                    prop: Atom(id1),
                                                                    kind: Var(Idx(0)),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        Proof {
                                                            prop: Atom(id1),
                                                            kind: Var(Idx(0)),
                                                        },
                                                        ImplType::Neg,
                                                    ),
                                                },
                                                ImplType::Neg,
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        Proof {
                            prop: Atom(id1),
                            kind: AppS(
                                Proof {
                                    prop: ImplS(NegS(Atom(id1)), Atom(id1)),
                                    kind: ConjElimS(
                                        Proof {
                                            prop: EquivS(Atom(id1), NegS(Atom(id1))),
                                            kind: Var(Idx(0)),
                                        },
                                        1,
                                        2,
                                        ConjType::Equiv,
                                    ),
                                },
                                Proof {
                                    prop: NegS(Atom(id1)),
                                    kind: AbsS(
                                        Proof {
                                            prop: Disj(vec![]),
                                            kind: AppS(
                                                Proof {
                                                    prop: NegS(Atom(id1)),
                                                    kind: AppS(
                                                        Proof {
                                                            prop: ImplS(Atom(id1), NegS(Atom(id1))),
                                                            kind: ConjElimS(
                                                                Proof {
                                                                    prop: EquivS(
                                                                        Atom(id1),
                                                                        NegS(Atom(id1)),
                                                                    ),
                                                                    kind: Var(Idx(1)),
                                                                },
                                                                0,
                                                                2,
                                                                ConjType::Equiv,
                                                            ),
                                                        },
                                                        Proof {
                                                            prop: Atom(id1),
                                                            kind: Var(Idx(0)),
                                                        },
                                                        ImplType::Normal,
                                                    ),
                                                },
                                                Proof {
                                                    prop: Atom(id1),
                                                    kind: Var(Idx(0)),
                                                },
                                                ImplType::Neg,
                                            ),
                                        },
                                        ImplType::Neg,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        ImplType::Neg,
                    ),
                },
                ImplType::Neg,
            ),
        };
        pf.check_type();
    }

    #[test]
    fn test_check_type6() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let pf = Proof {
            prop: ImplS(
                ImplS(
                    ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                    Disj(vec![]),
                ),
                Disj(vec![]),
            ),
            kind: AbsS(
                Proof {
                    prop: Disj(vec![]),
                    kind: AppS(
                        Proof {
                            prop: ImplS(
                                ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                                Disj(vec![]),
                            ),
                            kind: Var(Idx(0)),
                        },
                        Proof {
                            prop: ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                            kind: AbsS(
                                Proof {
                                    prop: Atom(id1),
                                    kind: AppS(
                                        Proof {
                                            prop: ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)),
                                            kind: Var(Idx(0)),
                                        },
                                        Proof {
                                            prop: ImplS(Atom(id1), Atom(id2)),
                                            kind: AbsS(
                                                Proof {
                                                    prop: Atom(id2),
                                                    kind: DisjElimS(
                                                        Proof {
                                                            prop: Disj(vec![]),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    Atom(id2),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Disj(vec![]),
                                                                    ),
                                                                    kind: Var(Idx(2)), // !!
                                                                },
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                Atom(id1),
                                                                                Atom(id2),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Atom(id1),
                                                                    ),
                                                                    kind: AbsS(
                                                                        Proof {
                                                                            prop: Atom(id1),
                                                                            kind: Var(Idx(1)),
                                                                        },
                                                                        ImplType::Normal,
                                                                    ),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        vec![],
                                                    ),
                                                },
                                                ImplType::Normal,
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
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
            kind: AbsS(
                Proof {
                    prop: a(),
                    kind: Var(Idx(0)),
                },
                ImplType::Normal,
            ),
        };
        body.subst(Idx(0), &arg, 0);
        assert_eq!(
            body,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(
                    Proof {
                        prop: a(),
                        kind: Var(Idx(0))
                    },
                    ImplType::Normal
                )
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
                    kind: AbsS(
                        Proof {
                            prop: ImplS(a(), a()),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                Proof {
                    prop: ImplS(a(), a()),
                    kind: AbsS(
                        Proof {
                            prop: a(),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
        };
        let updated = pf.try_reduce_here();
        assert_eq!(updated, true);
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(
                    Proof {
                        prop: a(),
                        kind: Var(Idx(0))
                    },
                    ImplType::Normal
                )
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
            kind: AbsS(
                Proof {
                    prop: Atom(id1),
                    kind: Var(Idx(0)),
                },
                ImplType::Normal,
            ),
        };
        pf.reduce_all();
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(Atom(id1), Atom(id1)),
                kind: AbsS(
                    Proof {
                        prop: Atom(id1),
                        kind: Var(Idx(0))
                    },
                    ImplType::Normal
                )
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
                    kind: AbsS(
                        Proof {
                            prop: ImplS(a(), a()),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                Proof {
                    prop: ImplS(a(), a()),
                    kind: AbsS(
                        Proof {
                            prop: a(),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
        };
        pf.reduce_all();
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(a(), a()),
                kind: AbsS(
                    Proof {
                        prop: a(),
                        kind: Var(Idx(0))
                    },
                    ImplType::Normal
                )
            }
        );
    }

    #[test]
    fn test_reduce_all3() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let mut pf = Proof {
            prop: ImplS(
                ImplS(
                    ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                    Disj(vec![]),
                ),
                Disj(vec![]),
            ),
            kind: AppS(
                Proof {
                    prop: ImplS(
                        ImplS(
                            ImplS(
                                ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                                Disj(vec![]),
                            ),
                            Disj(vec![]),
                        ),
                        ImplS(
                            ImplS(
                                ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                                Disj(vec![]),
                            ),
                            Disj(vec![]),
                        ),
                    ),
                    kind: AbsS(
                        Proof {
                            prop: ImplS(
                                ImplS(
                                    ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                                    Disj(vec![]),
                                ),
                                Disj(vec![]),
                            ),
                            kind: Var(Idx(0)),
                        },
                        ImplType::Normal,
                    ),
                },
                Proof {
                    prop: ImplS(
                        ImplS(
                            ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                            Disj(vec![]),
                        ),
                        Disj(vec![]),
                    ),
                    kind: AbsS(
                        Proof {
                            prop: Disj(vec![]),
                            kind: AppS(
                                Proof {
                                    prop: ImplS(ImplS(Atom(id1), Atom(id2)), Disj(vec![])),
                                    kind: AbsS(
                                        Proof {
                                            prop: Disj(vec![]),
                                            kind: AppS(
                                                Proof {
                                                    prop: ImplS(
                                                        ImplS(
                                                            ImplS(
                                                                ImplS(Atom(id1), Atom(id2)),
                                                                Atom(id1),
                                                            ),
                                                            Atom(id1),
                                                        ),
                                                        Disj(vec![]),
                                                    ),
                                                    kind: AbsS(
                                                        Proof {
                                                            prop: Disj(vec![]),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    Atom(id2),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Disj(vec![]),
                                                                    ),
                                                                    kind: Var(Idx(2)),
                                                                },
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                Atom(id1),
                                                                                Atom(id2),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Atom(id1),
                                                                    ),
                                                                    kind: Var(Idx(0)),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        ImplType::Normal,
                                                    ),
                                                },
                                                Proof {
                                                    prop: ImplS(
                                                        ImplS(
                                                            ImplS(Atom(id1), Atom(id2)),
                                                            Atom(id1),
                                                        ),
                                                        Atom(id1),
                                                    ),
                                                    kind: AbsS(
                                                        Proof {
                                                            prop: Atom(id1),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(Atom(id1), Atom(id2)),
                                                                        Atom(id1),
                                                                    ),
                                                                    kind: Var(Idx(0)),
                                                                },
                                                                Proof {
                                                                    prop: ImplS(
                                                                        Atom(id1),
                                                                        Atom(id2),
                                                                    ),
                                                                    kind: Var(Idx(1)),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        ImplType::Normal,
                                                    ),
                                                },
                                                ImplType::Normal,
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                Proof {
                                    prop: ImplS(Atom(id1), Atom(id2)),
                                    kind: AbsS(
                                        Proof {
                                            prop: Atom(id2),
                                            kind: DisjElimS(
                                                Proof {
                                                    prop: Disj(vec![]),
                                                    kind: AppS(
                                                        Proof {
                                                            prop: ImplS(
                                                                ImplS(
                                                                    ImplS(
                                                                        ImplS(Atom(id1), Atom(id2)),
                                                                        Atom(id1),
                                                                    ),
                                                                    Atom(id1),
                                                                ),
                                                                Disj(vec![]),
                                                            ),
                                                            kind: Var(Idx(1)),
                                                        },
                                                        Proof {
                                                            prop: ImplS(
                                                                ImplS(
                                                                    ImplS(Atom(id1), Atom(id2)),
                                                                    Atom(id1),
                                                                ),
                                                                Atom(id1),
                                                            ),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    Atom(id2),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        ImplS(
                                                                            ImplS(
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    Atom(id2),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                    ),
                                                                    kind: AbsS(
                                                                        Proof {
                                                                            prop: ImplS(
                                                                                ImplS(
                                                                                    ImplS(
                                                                                        Atom(id1),
                                                                                        Atom(id2),
                                                                                    ),
                                                                                    Atom(id1),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            kind: Var(Idx(0)),
                                                                        },
                                                                        ImplType::Normal,
                                                                    ),
                                                                },
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                Atom(id1),
                                                                                Atom(id2),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Atom(id1),
                                                                    ),
                                                                    kind: AbsS(
                                                                        Proof {
                                                                            prop: Atom(id1),
                                                                            kind: Var(Idx(1)),
                                                                        },
                                                                        ImplType::Normal,
                                                                    ),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        ImplType::Normal,
                                                    ),
                                                },
                                                vec![],
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
        };
        pf.reduce_all();
        let expect_pf = Proof {
            prop: ImplS(
                ImplS(
                    ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                    Disj(vec![]),
                ),
                Disj(vec![]),
            ),
            kind: AbsS(
                Proof {
                    prop: Disj(vec![]),
                    kind: AppS(
                        Proof {
                            prop: ImplS(
                                ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                                Disj(vec![]),
                            ),
                            kind: Var(Idx(0)),
                        },
                        Proof {
                            prop: ImplS(ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)), Atom(id1)),
                            kind: AbsS(
                                Proof {
                                    prop: Atom(id1),
                                    kind: AppS(
                                        Proof {
                                            prop: ImplS(ImplS(Atom(id1), Atom(id2)), Atom(id1)),
                                            kind: Var(Idx(0)),
                                        },
                                        Proof {
                                            prop: ImplS(Atom(id1), Atom(id2)),
                                            kind: AbsS(
                                                Proof {
                                                    prop: Atom(id2),
                                                    kind: DisjElimS(
                                                        Proof {
                                                            prop: Disj(vec![]),
                                                            kind: AppS(
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                ImplS(
                                                                                    Atom(id1),
                                                                                    Atom(id2),
                                                                                ),
                                                                                Atom(id1),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Disj(vec![]),
                                                                    ),
                                                                    kind: Var(Idx(2)), // !!
                                                                },
                                                                Proof {
                                                                    prop: ImplS(
                                                                        ImplS(
                                                                            ImplS(
                                                                                Atom(id1),
                                                                                Atom(id2),
                                                                            ),
                                                                            Atom(id1),
                                                                        ),
                                                                        Atom(id1),
                                                                    ),
                                                                    kind: AbsS(
                                                                        Proof {
                                                                            prop: Atom(id1),
                                                                            kind: Var(Idx(1)),
                                                                        },
                                                                        ImplType::Normal,
                                                                    ),
                                                                },
                                                                ImplType::Normal,
                                                            ),
                                                        },
                                                        vec![],
                                                    ),
                                                },
                                                ImplType::Normal,
                                            ),
                                        },
                                        ImplType::Normal,
                                    ),
                                },
                                ImplType::Normal,
                            ),
                        },
                        ImplType::Normal,
                    ),
                },
                ImplType::Normal,
            ),
        };
        assert_eq!(pf, expect_pf);
    }
}
