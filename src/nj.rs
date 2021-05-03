use crate::debruijn::{DbCtx, Idx, Shift};
use crate::prop::Prop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proof {
    /// variable in de Bruijn index
    Var(Idx),
    /// `\x => t`
    Abs(Prop, Box<Proof>),
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
    DisjIntro(Vec<Prop>, Box<Proof>, usize),
    /// `(match t1 { Ctor1 => u1, Ctor2 => u2, ..., CtorN => uN }): p`
    DisjElim(Prop, Box<Proof>, Vec<Proof>),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeError;

impl Proof {
    pub fn check_type(&self, ctx: &mut DbCtx<Prop>, prop: &Prop) -> Result<(), TypeError> {
        let ptype = self.ptype(ctx)?;
        if ptype == *prop {
            Ok(())
        } else {
            Err(TypeError)
        }
    }

    pub fn ptype(&self, ctx: &mut DbCtx<Prop>) -> Result<Prop, TypeError> {
        match *self {
            Proof::Var(idx) => {
                let prop = ctx.get(idx).ok_or_else(|| TypeError)?.clone();
                Ok(prop)
            }
            Proof::Abs(ref abstype, ref body) => {
                let mut ctx = ctx.push(abstype.clone());
                let tbody = body.ptype(&mut ctx)?;
                Ok(Prop::Impl(Box::new(abstype.clone()), Box::new(tbody)))
            }
            Proof::App(ref lhs, ref rhs) => {
                let lt = lhs.ptype(ctx)?;
                let rt = rhs.ptype(ctx)?;
                if let Prop::Impl(ltl, ltr) = lt {
                    if *ltl == rt {
                        Ok(*ltr)
                    } else {
                        Err(TypeError)
                    }
                } else {
                    Err(TypeError)
                }
            }
            Proof::ConjIntro(ref children) => {
                let ts = children
                    .iter()
                    .map(|child| child.ptype(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Prop::Conj(ts))
            }
            Proof::ConjElim(ref sub, i, n) => {
                let t = sub.ptype(ctx)?;
                if let Prop::Conj(mut children) = t {
                    if children.len() == n && i < n {
                        Ok(children.swap_remove(i))
                    } else {
                        Err(TypeError)
                    }
                } else {
                    Err(TypeError)
                }
            }
            Proof::DisjIntro(ref props, ref sub, i) => {
                let t = sub.ptype(ctx)?;
                if i < props.len() && props[i] == t {
                    Ok(Prop::Disj(props.clone()))
                } else {
                    Err(TypeError)
                }
            }
            Proof::DisjElim(ref goal, ref sub, ref branches) => {
                let t = sub.ptype(ctx)?;
                if let Prop::Disj(children) = t {
                    if children.len() == branches.len() {
                        for (child, branch) in children.iter().zip(branches) {
                            let mut ctx = ctx.push(child.clone());
                            if branch.ptype(&mut ctx)? != *goal {
                                return Err(TypeError);
                            }
                        }
                        Ok(goal.clone())
                    } else {
                        Err(TypeError)
                    }
                } else {
                    Err(TypeError)
                }
            }
        }
    }

    pub fn reduce_all(&mut self) {
        while self.try_reduce_here() {}
        match *self {
            Proof::Var(_) => {}
            Proof::Abs(_, ref mut body) => body.reduce_all(),
            Proof::App(ref mut lhs, ref mut rhs) => {
                lhs.reduce_all();
                rhs.reduce_all();
            }
            Proof::ConjIntro(ref mut children) => {
                for child in children {
                    child.reduce_all();
                }
            }
            Proof::ConjElim(ref mut sub, _, _) => sub.reduce_all(),
            Proof::DisjIntro(_, ref mut sub, _) => sub.reduce_all(),
            Proof::DisjElim(_, ref mut sub, ref mut branches) => {
                sub.reduce_all();
                for branch in branches {
                    branch.reduce_all();
                }
            }
        }
    }

    pub fn try_reduce_here(&mut self) -> bool {
        const SENTINEL: Proof = Proof::Var(Idx(0));

        match *self {
            Proof::App(ref mut fun, ref mut arg) => {
                if let Proof::Abs(_, ref mut body) = **fun {
                    body.subst(Idx(0), arg, 0);
                    let body = std::mem::replace(&mut **body, SENTINEL);
                    *self = body;
                    return true;
                }
            }
            Proof::ConjElim(ref mut tup, i, _) => {
                if let Proof::ConjIntro(ref mut elems) = **tup {
                    let elem = elems.swap_remove(i);
                    *self = elem;
                    return true;
                }
            }
            Proof::DisjElim(_, ref mut discr, ref mut branches) => {
                if let Proof::DisjIntro(_, ref mut arg, i) = **discr {
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
        match *self {
            Proof::Var(ref mut idx) => {
                if *idx > target {
                    idx.0 -= 1
                } else if *idx == target {
                    *self = repl.shifted(target.s(), by);
                }
            }
            Proof::Abs(_, ref mut body) => {
                body.subst(target.s(), repl, by + 1);
            }
            Proof::App(ref mut lhs, ref mut rhs) => {
                lhs.subst(target, repl, by);
                rhs.subst(target, repl, by);
            }
            Proof::ConjIntro(ref mut children) => {
                for child in children {
                    child.subst(target, repl, by);
                }
            }
            Proof::ConjElim(ref mut sub, _, _) => {
                sub.subst(target, repl, by);
            }
            Proof::DisjIntro(_, ref mut sub, _) => {
                sub.subst(target, repl, by);
            }
            Proof::DisjElim(_, ref mut sub, ref mut branches) => {
                sub.subst(target, repl, by);
                for branch in branches {
                    branch.subst(target, repl, by + 1);
                }
            }
        }
    }
}

impl Shift for Proof {
    fn shift(&mut self, after: Idx, by: usize) {
        match *self {
            Proof::Var(ref mut idx) => {
                idx.shift(after, by);
            }
            Proof::Abs(_, ref mut body) => {
                body.shift(after.s(), by);
            }
            Proof::App(ref mut lhs, ref mut rhs) => {
                lhs.shift(after, by);
                rhs.shift(after, by);
            }
            Proof::ConjIntro(ref mut children) => {
                for child in children {
                    child.shift(after, by);
                }
            }
            Proof::ConjElim(ref mut sub, _, _) => {
                sub.shift(after, by);
            }
            Proof::DisjIntro(_, ref mut sub, _) => {
                sub.shift(after, by);
            }
            Proof::DisjElim(_, ref mut sub, ref mut branches) => {
                sub.shift(after, by);
                for branch in branches {
                    branch.shift(after.s(), by);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prop::IdGen;

    #[test]
    fn test_ptype1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let mut ctx = DbCtx::new();
        let pf = Proof::Abs(Prop::Atom(id1), Box::new(Proof::Var(Idx(0))));
        let ptype = pf.ptype(&mut ctx).unwrap();
        assert_eq!(
            ptype,
            Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)))
        );
    }

    #[test]
    fn test_ptype2() {
        let mut ctx = DbCtx::new();
        let pf = Proof::Var(Idx(0));
        pf.ptype(&mut ctx).unwrap_err();
    }

    #[test]
    fn test_subst1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Prop::Atom(id1);
        let mut body = Proof::Var(Idx(0));
        let arg = Proof::Abs(a(), Box::new(Proof::Var(Idx(0))));
        body.subst(Idx(0), &arg, 0);
        assert_eq!(body, Proof::Abs(a(), Box::new(Proof::Var(Idx(0)))));
    }

    #[test]
    fn test_try_reduce_here1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Prop::Atom(id1);
        let mut pf = Proof::App(
            Box::new(Proof::Abs(
                Prop::Impl(Box::new(a()), Box::new(a())),
                Box::new(Proof::Var(Idx(0))),
            )),
            Box::new(Proof::Abs(a(), Box::new(Proof::Var(Idx(0))))),
        );
        let updated = pf.try_reduce_here();
        assert_eq!(updated, true);
        assert_eq!(pf, Proof::Abs(a(), Box::new(Proof::Var(Idx(0)))));
    }

    #[test]
    fn test_reduce_all1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let mut pf = Proof::Abs(Prop::Atom(id1), Box::new(Proof::Var(Idx(0))));
        pf.reduce_all();
        assert_eq!(
            pf,
            Proof::Abs(Prop::Atom(id1), Box::new(Proof::Var(Idx(0))))
        );
    }

    #[test]
    fn test_reduce_all2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Prop::Atom(id1);
        let mut pf = Proof::App(
            Box::new(Proof::Abs(
                Prop::Impl(Box::new(a()), Box::new(a())),
                Box::new(Proof::Var(Idx(0))),
            )),
            Box::new(Proof::Abs(a(), Box::new(Proof::Var(Idx(0))))),
        );
        pf.reduce_all();
        assert_eq!(pf, Proof::Abs(a(), Box::new(Proof::Var(Idx(0)))));
    }
}
