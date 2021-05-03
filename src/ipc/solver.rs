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
    use crate::prop::IdGen;

    #[test]
    fn test_solve1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Atom(id1);
        assert!(solve(&prop).is_none());
    }

    #[test]
    fn test_solve2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Prop::Atom(id1);
        let prop = Prop::Impl(Box::new(a()), Box::new(a()));
        let pf = solve(&prop).unwrap();
        assert_eq!(pf, Proof::Abs(a(), Box::new(Proof::Var(Idx(0)))));
    }

    #[test]
    fn test_solve3() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let a = || Prop::Atom(id1);
        let b = || Prop::Atom(id2);
        let prop = Prop::Impl(
            Box::new(a()),
            Box::new(Prop::Impl(Box::new(b()), Box::new(a()))),
        );
        let pf = solve(&prop).unwrap();
        assert_eq!(
            pf,
            Proof::Abs(a(), Box::new(Proof::Abs(b(), Box::new(Proof::Var(Idx(1))))))
        );
    }
}
