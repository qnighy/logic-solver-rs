use crate::debruijn::{DbCtx, Idx};
use crate::prop::Prop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proof {
    Var(Idx),
    Abs(Prop, Box<Proof>),
    App(Box<Proof>, Box<Proof>),
    ConjIntro(Vec<Proof>),
    ConjElim(Box<Proof>, usize, usize),
    DisjIntro(Vec<Prop>, Box<Proof>, usize),
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
        match self {
            Proof::Var(idx) => {
                let prop = ctx.get(*idx).ok_or_else(|| TypeError)?.clone();
                Ok(prop)
            }
            Proof::Abs(abstype, body) => {
                let mut ctx = ctx.push(abstype.clone());
                let tbody = body.ptype(&mut ctx)?;
                Ok(Prop::Impl(Box::new(abstype.clone()), Box::new(tbody)))
            }
            Proof::App(lhs, rhs) => {
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
            Proof::ConjIntro(children) => {
                let ts = children
                    .iter()
                    .map(|child| child.ptype(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Prop::Conj(ts))
            }
            Proof::ConjElim(sub, i, n) => {
                let t = sub.ptype(ctx)?;
                if let Prop::Conj(mut children) = t {
                    if children.len() == *n && i < n {
                        Ok(children.swap_remove(*i))
                    } else {
                        Err(TypeError)
                    }
                } else {
                    Err(TypeError)
                }
            }
            Proof::DisjIntro(props, sub, i) => {
                let t = sub.ptype(ctx)?;
                if *i < props.len() && props[*i] == t {
                    Ok(Prop::Disj(props.clone()))
                } else {
                    Err(TypeError)
                }
            }
            Proof::DisjElim(goal, sub, branches) => {
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
}
