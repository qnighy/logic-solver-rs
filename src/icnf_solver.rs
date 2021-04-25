use bitvec::prelude::*;
use bitvec::vec::BitVec;

use crate::icnf::{ClId, Clause, Icnf, Proof};
use crate::prop::{Id, IdGen};

pub fn solve_icnf(idgen: &IdGen, icnf: &Icnf) -> Option<Proof> {
    let truth = TruthTable::new(idgen);
    let trace = solve_dfs(&icnf.ant, icnf.suc, &truth)?;
    Some(reconstruct_proof(
        idgen.max_id(),
        &icnf.ant,
        icnf.suc,
        &trace,
    ))
}

fn solve_dfs(ant: &[Clause], goal: Id, truth: &TruthTable) -> Option<Trace> {
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
        return Some(Trace::Trivial);
    }
    for (i, clause) in ant.iter().enumerate() {
        let cl_id = ClId(i);
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
                if let Some(sub0) = sub0 {
                    let sub1 = {
                        let mut truth = truth.set_temp(*c, true);
                        solve_dfs(ant, goal, &mut truth)
                    };
                    if let Some(sub1) = sub1 {
                        return Some(Trace::Impl(cl_id, Box::new(sub0), Box::new(sub1)));
                    }
                }
            }
            Clause::Disj(lhs, rhs) => {
                // We only need Disj when all props in lhs are met.
                if lhs.iter().any(|&hyp| !truth.get(hyp))
                    || rhs.iter().any(|&option| truth.get(option))
                {
                    continue;
                }
                let children = rhs
                    .iter()
                    .map(|&option| {
                        let mut truth = truth.set_temp(option, true);
                        solve_dfs(ant, goal, &mut truth)
                    })
                    .collect::<Option<Vec<_>>>();
                if let Some(children) = children {
                    return Some(Trace::Disj(cl_id, children));
                }
            }
        }
    }
    None
}

fn reconstruct_proof(max_id: usize, ant: &[Clause], goal: Id, trace: &Trace) -> Proof {
    #[derive(Debug, Clone)]
    struct ReconstructionStack {
        backrefs: Vec<Option<Backref>>,
        backref_stack: Vec<(Id, Option<Backref>)>,
        binding_stack: Vec<Id>,
    }
    #[derive(Debug, Clone, Copy)]
    struct RollbackPoint(usize, usize);

    impl ReconstructionStack {
        fn get(&self, id: Id) -> bool {
            self.backrefs[id.index()].is_some()
        }

        fn set(&mut self, id: Id, backref: Backref) {
            self.backref_stack.push((id, self.backrefs[id.index()]));
            self.backrefs[id.index()] = Some(backref);
        }

        fn bind(&mut self, id: Id) {
            let idx = self.binding_stack.len();
            self.set(id, Backref::Stack(idx));
            self.binding_stack.push(id);
        }

        fn remember(&self) -> RollbackPoint {
            RollbackPoint(self.backref_stack.len(), self.binding_stack.len())
        }

        fn rollback(&mut self, pt: RollbackPoint) {
            for (id, backref) in self.backref_stack.drain(pt.0..) {
                self.backrefs[id.index()] = backref;
            }
            self.binding_stack.truncate(pt.1);
        }

        fn reconstruct(&self, ant: &[Clause], goal: Id, stack_aug: usize) -> Proof {
            let br = self.backrefs[goal.index()]
                .expect("bug: reconstruction failed: backref doesn't exist");
            match br {
                Backref::Stack(idx) => Proof::Hypothesis(self.binding_stack.len() - 1 - idx),
                Backref::Cl(cl_id) => match &ant[cl_id.0] {
                    Clause::Conj(lhs, _) => Proof::ApplyConj(
                        cl_id,
                        lhs.iter()
                            .map(|hyp| self.reconstruct(ant, *hyp, stack_aug))
                            .collect::<Vec<_>>(),
                    ),
                    Clause::Impl(_, b, _) => Proof::ApplyImpl(
                        cl_id,
                        Box::new(self.reconstruct(ant, *b, stack_aug + 1)),
                        Box::new(Proof::Hypothesis(self.binding_stack.len() + stack_aug)),
                    ),
                    Clause::Disj(..) => {
                        unreachable!("bug: reconstruction failed: unexpected Disj clause")
                    }
                },
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    enum Backref {
        // In global index (not de Brujin)
        Stack(usize),
        Cl(ClId),
    }

    fn dfs(ant: &[Clause], goal: Id, stack: &mut ReconstructionStack, trace: &Trace) -> Proof {
        loop {
            let mut updated = false;
            for (i, clause) in ant.iter().enumerate() {
                let cl_id = ClId(i);
                if let Clause::Conj(lhs, rhs) = clause {
                    if !stack.get(*rhs) && lhs.iter().all(|&hyp| stack.get(hyp)) {
                        stack.set(*rhs, Backref::Cl(cl_id));
                        updated = true;
                    }
                } else if let Clause::Impl(_, lhs, rhs) = clause {
                    if !stack.get(*rhs) && stack.get(*lhs) {
                        stack.set(*rhs, Backref::Cl(cl_id));
                        updated = true;
                    }
                }
            }
            if !updated {
                break;
            }
        }
        match trace {
            Trace::Trivial => stack.reconstruct(ant, goal, 0),
            Trace::Impl(cl_id, lhs, rhs) => {
                if let Clause::Impl(a, b, _) = &ant[cl_id.0] {
                    let pt = stack.remember();
                    stack.bind(*a);
                    let lhs = dfs(ant, *b, stack, lhs);
                    stack.rollback(pt);
                    stack.bind(*b);
                    let rhs = dfs(ant, goal, stack, rhs);
                    stack.rollback(pt);
                    Proof::ApplyImpl(*cl_id, Box::new(lhs), Box::new(rhs))
                } else {
                    unreachable!("bug: reconstruction failed: expected Trace::Impl");
                }
            }
            Trace::Disj(cl_id, branches) => {
                if let Clause::Disj(lhs, rhs) = &ant[cl_id.0] {
                    let requirements = lhs
                        .iter()
                        .map(|&req| stack.reconstruct(ant, req, 0))
                        .collect::<Vec<_>>();
                    let pt = stack.remember();
                    let branch_proofs = rhs
                        .iter()
                        .zip(branches)
                        .map(|(&hyp, branch)| {
                            stack.bind(hyp);
                            let pf = dfs(ant, goal, stack, branch);
                            stack.rollback(pt);
                            pf
                        })
                        .collect::<Vec<_>>();
                    Proof::ApplyDisj(*cl_id, requirements, branch_proofs)
                } else {
                    unreachable!("bug: reconstruction failed: expected Trace::Disj");
                }
            }
        }
    }

    let mut stack = ReconstructionStack {
        backrefs: vec![None; max_id],
        backref_stack: Vec::new(),
        binding_stack: Vec::new(),
    };

    dfs(ant, goal, &mut stack, trace)
}

#[derive(Debug, Clone)]
enum Trace {
    Trivial,
    Impl(ClId, Box<Trace>, Box<Trace>),
    Disj(ClId, Vec<Trace>),
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
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf2() {
        use Proof::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Conj(vec![], id1)],
                suc: id1,
            },
        );
        assert_eq!(provable, Some(ApplyConj(ClId(0), vec![])));
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
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf4() {
        use Proof::*;

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
        // TODO: redundant proof tree
        assert_eq!(
            provable,
            Some(ApplyImpl(
                ClId(0),
                Box::new(Hypothesis(0)),
                Box::new(ApplyImpl(
                    ClId(0),
                    Box::new(Hypothesis(0)),
                    Box::new(Hypothesis(1))
                ))
            ))
        );
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
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf6() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Impl(id1, id2, id1)],
                suc: id1,
            },
        );
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf7() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let id4 = idgen.fresh();
        let id5 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![
                    Clause::Impl(id1, id2, id3),
                    Clause::Conj(vec![id4, id3], id1),
                    Clause::Impl(id4, id1, id5),
                ],
                suc: id5,
            },
        );
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf8() {
        use Proof::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Disj(vec![id1], vec![]), Clause::Impl(id1, id2, id3)],
                suc: id3,
            },
        );
        // TODO: redundant proof
        assert_eq!(
            provable,
            Some(ApplyImpl(
                ClId(1),
                Box::new(ApplyDisj(ClId(0), vec![Hypothesis(0)], vec![])),
                Box::new(ApplyImpl(
                    ClId(1),
                    Box::new(Hypothesis(0)),
                    Box::new(Hypothesis(1))
                ))
            ))
        );
    }

    #[test]
    fn test_solve_icnf9() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Disj(vec![], vec![id1, id2])],
                suc: id2,
            },
        );
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf10() {
        use Proof::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![Clause::Disj(vec![], vec![id1, id1])],
                suc: id1,
            },
        );
        assert_eq!(
            provable,
            Some(ApplyDisj(
                ClId(0),
                vec![],
                vec![Hypothesis(0), Hypothesis(0)]
            ))
        );
    }

    #[test]
    fn test_solve_icnf11() {
        use Proof::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![
                    Clause::Disj(vec![], vec![id1, id2]),
                    Clause::Conj(vec![id2], id3),
                    Clause::Conj(vec![id1], id3),
                ],
                suc: id3,
            },
        );
        assert_eq!(
            provable,
            Some(ApplyDisj(
                ClId(0),
                vec![],
                vec![
                    ApplyConj(ClId(2), vec![Hypothesis(0)]),
                    ApplyConj(ClId(1), vec![Hypothesis(0)])
                ]
            ))
        );
    }

    #[test]
    fn test_solve_icnf12() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let id4 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![
                    Clause::Impl(id1, id2, id3),
                    Clause::Conj(vec![id1], id4),
                    Clause::Conj(vec![id3], id4),
                ],
                suc: id4,
            },
        );
        assert_eq!(provable, None);
    }

    #[test]
    fn test_solve_icnf13() {
        use Proof::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let id3 = idgen.fresh();
        let id4 = idgen.fresh();
        let id5 = idgen.fresh();
        let id6 = idgen.fresh();
        let provable = solve_icnf(
            &idgen,
            &Icnf {
                ant: vec![
                    Clause::Disj(vec![id2], vec![]),
                    Clause::Impl(id1, id2, id3),
                    Clause::Conj(vec![id1], id4),
                    Clause::Conj(vec![id3], id4),
                    Clause::Conj(vec![id5, id4], id2),
                    Clause::Impl(id5, id2, id6),
                ],
                suc: id6,
            },
        );
        assert_eq!(
            provable,
            Some(ApplyImpl(
                ClId(5),
                Box::new(ApplyImpl(
                    ClId(1),
                    Box::new(ApplyConj(
                        ClId(4),
                        vec![Hypothesis(1), ApplyConj(ClId(2), vec![Hypothesis(0)])]
                    )),
                    Box::new(Hypothesis(0))
                )),
                Box::new(ApplyImpl(
                    ClId(5),
                    Box::new(Hypothesis(0)),
                    Box::new(Hypothesis(1))
                ))
            ))
        );
    }
}
