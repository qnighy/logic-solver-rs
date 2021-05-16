use super::prop::promote_prop;
use crate::debruijn::DbCtx;
use crate::nj::{Proof, ProofKind};
use crate::prop::Env;
use crate::visible_proof::{HypothesisGen, HypothesisId, RuleName, VisibleProof, VisibleProofKind};

pub fn promote_nj(pf: &Proof, env: &Env) -> VisibleProof {
    let mut ctx = DbCtx::new();
    let mut ctr = HypothesisGen::new();
    promote_nj_rec(pf, env, &mut ctx, &mut ctr)
}

fn promote_nj_rec(
    pf: &Proof,
    env: &Env,
    ctx: &mut DbCtx<HypothesisId>,
    ctr: &mut HypothesisGen,
) -> VisibleProof {
    let kind = match pf.kind {
        ProofKind::Var(idx) => VisibleProofKind::Axiom(ctx[idx]),
        ProofKind::Abs(ref body, _) => {
            let hyp_id = ctr.fresh();
            let mut ctx = ctx.push(hyp_id);
            let body = promote_nj_rec(body, env, &mut ctx, ctr);
            VisibleProofKind::SubProof {
                rule: RuleName::ImplIntro,
                introduces: Some(hyp_id),
                subproofs: vec![body],
            }
        }
        ProofKind::App(ref lhs, ref rhs, _) => {
            let lhs = promote_nj_rec(lhs, env, ctx, ctr);
            let rhs = promote_nj_rec(rhs, env, ctx, ctr);
            VisibleProofKind::SubProof {
                rule: RuleName::ImplElim,
                introduces: None,
                subproofs: vec![lhs, rhs],
            }
        }
        ProofKind::ConjIntro(ref children, _) => {
            let children = children
                .iter()
                .map(|child| promote_nj_rec(child, env, ctx, ctr))
                .collect::<Vec<_>>();
            VisibleProofKind::SubProof {
                rule: RuleName::ConjIntro(children.len()),
                introduces: None,
                subproofs: children,
            }
        }
        ProofKind::ConjElim(ref sub, i, n, _) => {
            let sub = promote_nj_rec(sub, env, ctx, ctr);
            VisibleProofKind::SubProof {
                rule: RuleName::ConjElim(i, n),
                introduces: None,
                subproofs: vec![sub],
            }
        }
        ProofKind::DisjIntro(ref sub, i, n) => {
            let sub = promote_nj_rec(sub, env, ctx, ctr);
            VisibleProofKind::SubProof {
                rule: RuleName::DisjIntro(i, n),
                introduces: None,
                subproofs: vec![sub],
            }
        }
        ProofKind::DisjElim(ref sub, ref branches) => {
            let mut children = vec![promote_nj_rec(sub, env, ctx, ctr)];
            let hyp_name = ctr.fresh();
            for branch in branches {
                let mut ctx = ctx.push(hyp_name);
                children.push(promote_nj_rec(branch, env, &mut ctx, ctr));
            }
            VisibleProofKind::SubProof {
                rule: RuleName::DisjElim(branches.len()),
                introduces: Some(hyp_name),
                subproofs: children,
            }
        }
        ProofKind::DNegElim(ref sub) => VisibleProofKind::SubProof {
            rule: RuleName::DNegElim,
            introduces: None,
            subproofs: vec![promote_nj_rec(sub, env, ctx, ctr)],
        },
    };
    VisibleProof {
        kind,
        prop: promote_prop(&pf.prop, env),
        split_here: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debruijn::Idx;
    use crate::nj::ProofKindShorthands;
    use crate::parsing::Prop as PropAst;
    use crate::prop::{IdGen, ImplType, PropShorthands};
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
            kind: AbsS(
                Proof {
                    prop: a(),
                    kind: Var(Idx(0)),
                },
                ImplType::Normal,
            ),
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
                    rule: RuleName::ImplIntro,
                    introduces: Some(HypothesisId(1)),
                    subproofs: vec![VisibleProof {
                        prop: PropAst::Atom(S("A")),
                        kind: VisibleProofKind::Axiom(HypothesisId(1)),
                        split_here: false,
                    },],
                },
                split_here: false,
            }
        )
    }
}
