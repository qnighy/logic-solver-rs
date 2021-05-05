use super::prop::promote_prop;
use crate::debruijn::DbCtx;
use crate::nj::{Proof, ProofKind};
use crate::prop::Env;
use crate::visible_proof::{VisibleProof, VisibleProofKind};

pub fn promote_nj(pf: &Proof, env: &Env) -> VisibleProof {
    let mut ctx = DbCtx::new();
    let mut ctr = HypothesisCounter::new();
    promote_nj_rec(pf, env, &mut ctx, &mut ctr)
}

fn promote_nj_rec(
    pf: &Proof,
    env: &Env,
    ctx: &mut DbCtx<String>,
    ctr: &mut HypothesisCounter,
) -> VisibleProof {
    let kind = match pf.kind {
        ProofKind::Var(idx) => VisibleProofKind::Axiom(ctx[idx].clone()),
        ProofKind::Abs(ref body) => {
            let hyp_name = ctr.fresh();
            let mut ctx = ctx.push(hyp_name);
            let body = promote_nj_rec(body, env, &mut ctx, ctr);
            VisibleProofKind::SubProof {
                subproofs: vec![body],
            }
        }
        ProofKind::App(ref lhs, ref rhs) => {
            let lhs = promote_nj_rec(lhs, env, ctx, ctr);
            let rhs = promote_nj_rec(rhs, env, ctx, ctr);
            VisibleProofKind::SubProof {
                subproofs: vec![lhs, rhs],
            }
        }
        ProofKind::ConjIntro(ref children) => {
            let children = children
                .iter()
                .map(|child| promote_nj_rec(child, env, ctx, ctr))
                .collect::<Vec<_>>();
            VisibleProofKind::SubProof {
                subproofs: children,
            }
        }
        ProofKind::ConjElim(ref sub, _, _) => {
            let sub = promote_nj_rec(sub, env, ctx, ctr);
            VisibleProofKind::SubProof {
                subproofs: vec![sub],
            }
        }
        ProofKind::DisjIntro(ref sub, _, _) => {
            let sub = promote_nj_rec(sub, env, ctx, ctr);
            VisibleProofKind::SubProof {
                subproofs: vec![sub],
            }
        }
        ProofKind::DisjElim(ref sub, ref branches) => {
            let mut children = vec![promote_nj_rec(sub, env, ctx, ctr)];
            for branch in branches {
                children.push(promote_nj_rec(branch, env, ctx, ctr));
            }
            VisibleProofKind::SubProof {
                subproofs: children,
            }
        }
    };
    VisibleProof {
        kind,
        prop: promote_prop(&pf.prop, env),
    }
}

#[derive(Debug)]
struct HypothesisCounter(usize);

impl HypothesisCounter {
    fn new() -> Self {
        Self(0)
    }

    fn fresh(&mut self) -> String {
        self.0 += 1;
        format!("H{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debruijn::Idx;
    use crate::nj::ProofKindShorthands;
    use crate::parsing::Prop as PropAst;
    use crate::prop::{IdGen, PropShorthands};
    use big_s::S;

    #[test]
    fn test_promote_nj() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut env = Env::new();
        let mut idgen = IdGen::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        let a = || Atom(id1);

        let pf = Proof {
            prop: ImplS(a(), a()),
            kind: AbsS(Proof {
                prop: a(),
                kind: Var(Idx(0)),
            }),
        };
        pf.check_type();
        let pf = promote_nj(&pf, &env);
        assert_eq!(
            pf,
            VisibleProof {
                prop: PropAst::Impl(
                    Box::new(PropAst::Atom(S("A"))),
                    Box::new(PropAst::Atom(S("A")))
                ),
                kind: VisibleProofKind::SubProof {
                    subproofs: vec![VisibleProof {
                        prop: PropAst::Atom(S("A")),
                        kind: VisibleProofKind::Axiom(S("H1")),
                    },],
                }
            }
        )
    }
}
