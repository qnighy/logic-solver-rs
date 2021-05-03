use crate::prop::Prop;

use crate::ipc::icnf::VarGen;
use crate::ipc::icnf_decomp::Decomposition;
use crate::ipc::icnf_solver::solve_icnf;
use crate::nj::Proof;

pub fn solve(prop: &Prop) -> Option<Proof> {
    let mut vargen = VarGen::new();
    let (icnf, decomp) = Decomposition::decompose(&mut vargen, prop);
    let icnf_proof = solve_icnf(&vargen, &icnf)?;
    let mut proof = decomp.convert_nj(&icnf_proof, icnf.suc);
    proof.reduce_all();
    Some(proof)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debruijn::Idx;
    use crate::nj::ProofShorthands;
    use crate::prop::{IdGen, PropShorthands};

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
        use ProofShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let prop = ImplS(a(), a());
        let pf = solve(&prop).unwrap();
        assert_eq!(pf, AbsS(a(), Var(Idx(0))));
    }

    #[test]
    fn test_solve3() {
        use ProofShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let a = || Atom(id1);
        let b = || Atom(id2);
        let prop = ImplS(a(), ImplS(b(), a()));
        let pf = solve(&prop).unwrap();
        assert_eq!(pf, AbsS(a(), AbsS(b(), Var(Idx(1)))));
    }
}
