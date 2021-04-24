use bitvec::prelude::*;
use bitvec::vec::BitVec;

use crate::icnf::{Clause, Icnf};
use crate::prop::{Id, IdGen};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TruthTable(BitVec);

impl TruthTable {
    fn new(idgen: &IdGen) -> Self {
        Self(bitvec![0; idgen.max_id()])
    }

    fn get(&self, id: Id) -> bool {
        self.0[id.index()]
    }

    fn set(&mut self, id: Id, truth: bool) {
        self.0.set(id.index(), truth);
    }

    fn with<R, F>(&mut self, id: Id, truth: bool, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_truth = self.get(id);
        self.set(id, truth);
        let r = f(self);
        self.set(id, old_truth);
        r
    }
}

pub fn solve_icnf(idgen: &IdGen, icnf: &Icnf) -> bool {
    let truth = TruthTable::new(idgen);
    solve_dfs(&icnf.ant, icnf.suc, &truth)
}

fn solve_dfs(ant: &[Clause], goal: Id, truth: &TruthTable) -> bool {
    let mut truth = truth.clone();
    loop {
        let mut updated = false;
        for clause in ant {
            if let Clause::Conj(lhs, rhs) = clause {
                if !truth.get(*rhs) && lhs.iter().all(|&hyp| truth.get(hyp)) {
                    truth.set(*rhs, true);
                    updated = true;
                }
            }
        }
        if !updated {
            break;
        }
    }
    if truth.get(goal) {
        return true;
    }
    for clause in ant {
        match clause {
            Clause::Conj(..) => {
                // do nothing
            }
            Clause::Impl(a, b, c) => {
                if truth.get(*c) {
                    continue;
                }
                // TODO: avoid infinite recursion here
                let sub0 = truth.with(*a, true, |truth| solve_dfs(ant, *b, truth));
                if !sub0 {
                    continue;
                }
                let sub1 = truth.with(*c, true, |truth| solve_dfs(ant, goal, truth));
                if sub1 {
                    return true;
                }
            }
            Clause::Disj(lhs, rhs) => {
                todo!();
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_icnf1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![],
                suc: id1,
            },
        );
        assert_eq!(provable, false);
    }

    #[test]
    fn test_solve_icnf2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Conj(vec![], id1)],
                suc: id1,
            },
        );
        assert_eq!(provable, true);
    }

    #[test]
    fn test_solve_icnf3() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Conj(vec![], id1)],
                suc: id2,
            },
        );
        assert_eq!(provable, false);
    }

    #[test]
    fn test_solve_icnf4() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Impl(id1, id1, id2)],
                suc: id2,
            },
        );
        assert_eq!(provable, true);
    }

    // #[test]
    // fn test_solve_icnf5() {
    //     let mut idgen = IdGen::new();
    //     let id1 = idgen.fresh();
    //     let id2 = idgen.fresh();
    //     let id3 = idgen.fresh();
    //     let provable = solve_icnf(
    //         &idgen,
    //         &Icnf {
    //             ant: vec![Clause::Impl(id1, id2, id3)],
    //             suc: id3,
    //         },
    //     );
    //     assert_eq!(provable, false);
    // }
}
