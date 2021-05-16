use crate::prop::Prop;

use crate::ipc::icnf::VarGen;
use crate::ipc::icnf_decomp::Decomposition;
use crate::ipc::icnf_solver::solve_icnf;
use crate::nj::{Proof, ProofKind};
use crate::result::SolverResult;

pub fn solve_res<R>(prop: &Prop) -> SolverResult<Proof, R> {
    if let Some(pf) = solve(prop) {
        SolverResult::Provable(Some(pf))
    } else {
        SolverResult::NotProvable(None)
    }
}

pub fn solve(prop: &Prop) -> Option<Proof> {
    let mut vargen = VarGen::new();
    let (icnf, decomp) = Decomposition::decompose(&mut vargen, prop);
    let icnf_proof = solve_icnf(&vargen, &icnf)?;
    let mut proof = decomp.convert_nj(&icnf_proof, icnf.suc);
    if cfg!(debug_assertions) {
        proof.check_has_type(prop);
    }
    proof.reduce_all();
    if cfg!(debug_assertions) {
        proof.check_has_type(prop);
    }
    Some(proof)
}

pub fn solve_cpc(prop: &Prop) -> Option<Proof> {
    let prop_dn = Prop::NegS(Prop::NegS(prop.clone()));
    let proof_dn = solve(&prop_dn)?;
    let mut proof = Proof {
        prop: prop.clone(),
        kind: ProofKind::DNegElimS(proof_dn),
    };
    proof.reduce_all();
    if cfg!(debug_assertions) {
        proof.check_has_type(prop);
    }
    Some(proof)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debruijn::Idx;
    use crate::nj::ProofKindShorthands;
    use crate::prop::{IdGen, ImplType, PropShorthands};

    #[test]
    fn test_solve1() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Atom(id1);
        assert!(solve(&prop).is_none());
    }

    #[test]
    fn test_solve2() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let prop = ImplS(a(), a());
        let pf = solve(&prop).unwrap();
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
    fn test_solve3() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let a = || Atom(id1);
        let b = || Atom(id2);
        let prop = ImplS(a(), ImplS(b(), a()));
        let pf = solve(&prop).unwrap();
        assert_eq!(
            pf,
            Proof {
                prop: ImplS(a(), ImplS(b(), a())),
                kind: AbsS(
                    Proof {
                        prop: ImplS(b(), a()),
                        kind: AbsS(
                            Proof {
                                prop: a(),
                                kind: Var(Idx(1))
                            },
                            ImplType::Normal
                        )
                    },
                    ImplType::Normal
                )
            }
        );
    }
}
