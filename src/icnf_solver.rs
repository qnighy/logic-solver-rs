use bitvec::prelude::*;
use bitvec::vec::BitVec;

use crate::icnf::{Clause, Icnf};
use crate::prop::{Id, IdGen};

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
            } else if let Clause::Impl(_, lhs, rhs) = clause {
                // ((a -> b) -> c) implies (b -> c)
                if !truth.get(*rhs) && truth.get(*lhs) {
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
                // If c is already true, the second branch produces the same sequent.
                // If a is already true, the clause is already handled above in the closure calculation.
                if truth.get(*c) || truth.get(*a) {
                    continue;
                }
                let sub0 = {
                    let mut truth = truth.set_temp(*a, true);
                    solve_dfs(ant, *b, &mut truth)
                };
                if !sub0 {
                    continue;
                }
                let sub1 = {
                    let mut truth = truth.set_temp(*c, true);
                    solve_dfs(ant, goal, &mut truth)
                };
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct TruthTable(BitVec);

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

    fn set_temp(&mut self, id: Id, truth: bool) -> TruthTableRewind<'_> {
        let old_truth = self.get(id);
        self.set(id, truth);
        TruthTableRewind {
            table: self,
            id,
            truth: old_truth,
        }
    }
}

struct TruthTableRewind<'a> {
    table: &'a mut TruthTable,
    id: Id,
    truth: bool,
}

impl std::ops::Deref for TruthTableRewind<'_> {
    type Target = TruthTable;
    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl std::ops::DerefMut for TruthTableRewind<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.table
    }
}

impl Drop for TruthTableRewind<'_> {
    fn drop(&mut self) {
        self.table.set(self.id, self.truth);
    }
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

    #[test]
    fn test_solve_icnf5() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Impl(id1, id2, id3)],
                suc: id3,
            },
        );
        assert_eq!(provable, false);
    }
}
