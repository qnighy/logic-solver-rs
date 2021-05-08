use std::collections::{HashMap, HashSet};

use crate::debruijn::Idx;
use crate::ipc::icnf::{ClId, Clause, ClauseSet, Icnf, Proof, Var, VarGen};
use crate::nj::{Proof as NjProof, ProofKind as NjProofKind};
use crate::prop::{Id, Prop};

#[derive(Debug, Clone)]
pub struct Decomposition {
    sources: HashMap<ClId, ClauseSource>,
    prop_map: PropMap,
}

impl Decomposition {
    pub fn sources(&self) -> &HashMap<ClId, ClauseSource> {
        &self.sources
    }
    pub fn prop_map(&self) -> &PropMap {
        &self.prop_map
    }

    pub fn decompose(vargen: &mut VarGen, prop: &Prop) -> (Icnf, Decomposition) {
        let mut prop_map = PropMap::default();
        let prop_var = prop_map.insert_prop(vargen, prop);

        let mut pol_map = PolarityMap::default();
        pol_map.mark_polarity(&prop_map, prop_var, true);

        let mut sources = HashMap::<ClId, ClauseSource>::new();

        let mut ant = ClauseSet::default();
        for (v, sp) in prop_map.iter() {
            let (pos, neg) = pol_map.polarity(v);
            match *sp {
                ShallowProp::Atom(..) => {}
                ShallowProp::Impl(lhs, rhs, _) => {
                    if pos {
                        let cl = ant.push(Clause::Impl(lhs, rhs, v));
                        sources.insert(cl, ClauseSource::ImplIntro(v));
                    }
                    if neg {
                        let cl = ant.push(Clause::Conj(vec![v, lhs], rhs));
                        sources.insert(cl, ClauseSource::ImplElim(v));
                    }
                }
                ShallowProp::Conj(ref children, _) => {
                    if pos {
                        let cl = ant.push(Clause::Conj(children.clone(), v));
                        sources.insert(cl, ClauseSource::ConjIntro(v));
                    }
                    if neg {
                        for (i, &child) in children.iter().enumerate() {
                            let cl = ant.push(Clause::Conj(vec![v], child));
                            sources.insert(cl, ClauseSource::ConjElim(v, i, children.len()));
                        }
                    }
                }
                ShallowProp::Disj(ref children) => {
                    if pos {
                        for (i, &child) in children.iter().enumerate() {
                            let cl = ant.push(Clause::Conj(vec![child], v));
                            sources.insert(cl, ClauseSource::DisjIntro(v, i, children.len()));
                        }
                    }
                    if neg {
                        let cl = ant.push(Clause::Disj(vec![v], children.clone()));
                        sources.insert(cl, ClauseSource::DisjElim(v));
                    }
                }
            }
        }

        (
            Icnf { ant, suc: prop_var },
            Decomposition { sources, prop_map },
        )
    }

    pub fn convert_nj(&self, pf: &Proof, goal: Var) -> NjProof {
        let goal_prop = self.prop_map.get_prop(goal);
        let kind = match *pf {
            Proof::Hypothesis(i) => NjProofKind::Var(Idx(i)),
            Proof::ApplyConj(cl_id, ref children) => match *self.sources.get(&cl_id).unwrap() {
                ClauseSource::ImplElim(v) => {
                    let (vl, _) = self.prop_map.get(v).unwrap().as_impl().unwrap();
                    NjProofKind::AppS(
                        self.convert_nj(&children[0], v),
                        self.convert_nj(&children[1], vl),
                    )
                }
                ClauseSource::ConjIntro(v) => {
                    let vc = self.prop_map.get(v).unwrap().as_conj().unwrap();
                    NjProofKind::ConjIntro(
                        children
                            .iter()
                            .zip(vc)
                            .map(|(child, &subgoal)| self.convert_nj(child, subgoal))
                            .collect(),
                    )
                }
                ClauseSource::ConjElim(v, i, n) => {
                    NjProofKind::ConjElimS(self.convert_nj(&children[0], v), i, n)
                }
                ClauseSource::DisjIntro(v, i, n) => {
                    let vc = self.prop_map.get(v).unwrap().as_disj().unwrap();
                    NjProofKind::DisjIntroS(self.convert_nj(&children[0], vc[i]), i, n)
                }
                ClauseSource::ImplIntro(_) | ClauseSource::DisjElim(_) => unreachable!(),
            },
            Proof::ApplyDisj(cl_id, ref children, ref branches) => {
                if let ClauseSource::DisjElim(v) = *self.sources.get(&cl_id).unwrap() {
                    NjProofKind::DisjElimS(
                        self.convert_nj(&children[0], v),
                        branches
                            .iter()
                            .map(|child| self.convert_nj(child, goal))
                            .collect(),
                    )
                } else {
                    unreachable!()
                }
            }
            Proof::ApplyImpl(cl_id, ref lhs, ref rhs) => {
                if let ClauseSource::ImplIntro(v) = *self.sources.get(&cl_id).unwrap() {
                    let (_, vr) = self.prop_map.get(v).unwrap().as_impl().unwrap();
                    let nj_lhs = NjProof {
                        prop: self.prop_map.get_prop(v),
                        kind: NjProofKind::AbsS(self.convert_nj(lhs, vr)),
                    };
                    let nj_rhs = NjProof {
                        prop: Prop::ImplS(self.prop_map.get_prop(v), self.prop_map.get_prop(goal)),
                        kind: NjProofKind::AbsS(self.convert_nj(rhs, goal)),
                    };
                    NjProofKind::AppS(nj_rhs, nj_lhs)
                } else {
                    unreachable!()
                }
            }
        };
        NjProof {
            kind,
            prop: goal_prop,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClauseSource {
    ImplIntro(Var),
    ImplElim(Var),
    ConjIntro(Var),
    ConjElim(Var, usize, usize),
    DisjIntro(Var, usize, usize),
    DisjElim(Var),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PropMap {
    rev_vars: HashMap<ShallowProp, Var>,
    vars: Vec<Option<ShallowProp>>,
}

impl PropMap {
    pub fn iter(&self) -> impl Iterator<Item = (Var, &ShallowProp)> {
        self.vars
            .iter()
            .enumerate()
            .filter_map(|(i, sp)| Some((Var(i), sp.as_ref()?)))
    }
    pub fn get(&self, v: Var) -> Option<&ShallowProp> {
        self.vars.get(v.0).and_then(|sp| sp.as_ref())
    }
    pub fn get_prop(&self, v: Var) -> Prop {
        match *self.get(v).unwrap() {
            ShallowProp::Atom(id) => Prop::Atom(id),
            ShallowProp::Impl(lhs, rhs, ImplType::Normal) => {
                Prop::ImplS(self.get_prop(lhs), self.get_prop(rhs))
            }
            ShallowProp::Impl(lhs, rhs, ImplType::Neg) => {
                debug_assert_eq!(self.get_prop(rhs), Prop::Disj(vec![]));
                Prop::NegS(self.get_prop(lhs))
            }
            ShallowProp::Conj(ref children, ConjType::Normal) => {
                Prop::Conj(children.iter().map(|&child| self.get_prop(child)).collect())
            }
            ShallowProp::Conj(ref children, ConjType::Equiv) => {
                debug_assert_eq!(children.len(), 2);
                let (lhs, rhs) = self.get(children[0]).unwrap().as_impl().unwrap();
                if cfg!(debug_assert) {
                    let (lhs_rev, rhs_rev) = self.get(children[1]).unwrap().as_impl().unwrap();
                    assert_eq!(lhs, rhs_rev);
                    assert_eq!(rhs, lhs_rev);
                }
                Prop::EquivS(self.get_prop(lhs), self.get_prop(rhs))
            }
            ShallowProp::Disj(ref children) => {
                Prop::Disj(children.iter().map(|&child| self.get_prop(child)).collect())
            }
        }
    }
    pub fn insert_prop(&mut self, vargen: &mut VarGen, prop: &Prop) -> Var {
        let sp = match *prop {
            Prop::Atom(id) => ShallowProp::Atom(id),
            Prop::Impl(ref lhs, ref rhs) => {
                let lhs = self.insert_prop(vargen, lhs);
                let rhs = self.insert_prop(vargen, rhs);
                ShallowProp::Impl(lhs, rhs, ImplType::Normal)
            }
            Prop::Conj(ref children) => {
                let children = children
                    .iter()
                    .map(|child| self.insert_prop(vargen, child))
                    .collect::<Vec<_>>();
                ShallowProp::Conj(children, ConjType::Normal)
            }
            Prop::Disj(ref children) => {
                let children = children
                    .iter()
                    .map(|child| self.insert_prop(vargen, child))
                    .collect::<Vec<_>>();
                ShallowProp::Disj(children)
            }
            Prop::Equiv(ref lhs, ref rhs) => {
                let lhs = self.insert_prop(vargen, lhs);
                let rhs = self.insert_prop(vargen, rhs);
                let lr = self
                    .shallow_insert_prop(vargen, &ShallowProp::Impl(lhs, rhs, ImplType::Normal));
                let rl = self
                    .shallow_insert_prop(vargen, &ShallowProp::Impl(rhs, lhs, ImplType::Normal));
                ShallowProp::Conj(vec![lr, rl], ConjType::Equiv)
            }
            Prop::Neg(ref sub) => {
                let lhs = self.insert_prop(vargen, sub);
                let rhs = self.shallow_insert_prop(vargen, &ShallowProp::Disj(vec![]));
                ShallowProp::Impl(lhs, rhs, ImplType::Neg)
            }
        };
        self.shallow_insert_prop(vargen, &sp)
    }
    pub fn shallow_insert_prop(&mut self, vargen: &mut VarGen, sp: &ShallowProp) -> Var {
        if let Some(&v) = self.rev_vars.get(sp) {
            v
        } else {
            let v = vargen.fresh();
            self.rev_vars.insert(sp.clone(), v);
            while self.vars.len() <= v.0 {
                self.vars.push(None);
            }
            self.vars[v.0] = Some(sp.clone());
            v
        }
    }

    pub fn to_map(&self) -> HashMap<Var, ShallowProp> {
        self.iter().map(|(v, sp)| (v, sp.clone())).collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct PolarityMap {
    pos: HashSet<Var>,
    neg: HashSet<Var>,
}

impl PolarityMap {
    pub fn polarity(&self, v: Var) -> (bool, bool) {
        (self.pos.contains(&v), self.neg.contains(&v))
    }

    pub fn mark_polarity(&mut self, decomp: &PropMap, v: Var, positive: bool) {
        let polset = if positive {
            &mut self.pos
        } else {
            &mut self.neg
        };
        if polset.contains(&v) {
            return;
        }
        polset.insert(v);
        match *decomp.get(v).unwrap() {
            ShallowProp::Atom(..) => {}
            ShallowProp::Impl(lhs, rhs, _) => {
                self.mark_polarity(decomp, lhs, !positive);
                self.mark_polarity(decomp, rhs, positive);
            }
            ShallowProp::Conj(ref children, _) => {
                for &child in children {
                    self.mark_polarity(decomp, child, positive);
                }
            }
            ShallowProp::Disj(ref children) => {
                for &child in children {
                    self.mark_polarity(decomp, child, positive);
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ShallowProp {
    Atom(Id),
    Impl(Var, Var, ImplType),
    Conj(Vec<Var>, ConjType),
    Disj(Vec<Var>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImplType {
    Normal,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConjType {
    Normal,
    Equiv,
}

impl ShallowProp {
    pub fn as_atom(&self) -> Option<Id> {
        if let ShallowProp::Atom(id) = *self {
            Some(id)
        } else {
            None
        }
    }

    pub fn as_impl(&self) -> Option<(Var, Var)> {
        if let ShallowProp::Impl(lhs, rhs, _) = *self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn as_conj(&self) -> Option<&[Var]> {
        if let ShallowProp::Conj(ref children, _) = *self {
            Some(children)
        } else {
            None
        }
    }

    pub fn as_disj(&self) -> Option<&[Var]> {
        if let ShallowProp::Disj(ref children) = *self {
            Some(children)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nj::ProofKindShorthands;
    use crate::prop::{IdGen, PropShorthands};
    use maplit::hashmap;

    #[test]
    fn test_from_prop1() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Atom(id1);
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet { vec: vec![] },
                suc: Var(0)
            },
        );
        assert_eq!(
            decomp.prop_map().to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
            ]
        );
        assert_eq!(*decomp.sources(), hashmap![]);
    }

    #[test]
    fn test_from_prop2() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = ImplS(Atom(id1), Atom(id1));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet {
                    vec: vec![Clause::Impl(Var(0), Var(0), Var(1)),]
                },
                suc: Var(1)
            },
        );
        assert_eq!(
            decomp.prop_map().to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Impl(Var(0), Var(0), ImplType::Normal),
            ],
        );
        assert_eq!(
            *decomp.sources(),
            hashmap![
                ClId(0) => ClauseSource::ImplIntro(Var(1)),
            ]
        );
    }

    #[test]
    fn test_from_prop3() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = ImplS(
            Disj(vec![Atom(id1), Atom(id2)]),
            Disj(vec![Atom(id2), Atom(id1)]),
        );
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet {
                    vec: vec![
                        Clause::Disj(vec![Var(2)], vec![Var(0), Var(1)]),
                        Clause::Conj(vec![Var(1)], Var(3)),
                        Clause::Conj(vec![Var(0)], Var(3)),
                        Clause::Impl(Var(2), Var(3), Var(4)),
                    ]
                },
                suc: Var(4),
            },
        );
        assert_eq!(
            decomp.prop_map().to_map(),
            hashmap! [
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Atom(Id(1)),
                Var(2) => ShallowProp::Disj(vec![Var(0), Var(1)]),
                Var(3) => ShallowProp::Disj(vec![Var(1), Var(0)]),
                Var(4) => ShallowProp::Impl(Var(2), Var(3), ImplType::Normal),
            ],
        );
        assert_eq!(
            *decomp.sources(),
            hashmap![
                ClId(0) => ClauseSource::DisjElim(Var(2)),
                ClId(1) => ClauseSource::DisjIntro(Var(3), 0, 2),
                ClId(2) => ClauseSource::DisjIntro(Var(3), 1, 2),
                ClId(3) => ClauseSource::ImplIntro(Var(4)),
            ]
        );
    }

    #[test]
    fn test_from_prop4() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = ImplS(
            Conj(vec![Atom(id1), Atom(id2)]),
            Conj(vec![Atom(id2), Atom(id1)]),
        );
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet {
                    vec: vec![
                        Clause::Conj(vec![Var(2)], Var(0)),
                        Clause::Conj(vec![Var(2)], Var(1)),
                        Clause::Conj(vec![Var(1), Var(0)], Var(3)),
                        Clause::Impl(Var(2), Var(3), Var(4)),
                    ]
                },
                suc: Var(4),
            },
        );
        assert_eq!(
            decomp.prop_map().to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Atom(Id(1)),
                Var(2) => ShallowProp::Conj(vec![Var(0), Var(1)], ConjType::Normal),
                Var(3) => ShallowProp::Conj(vec![Var(1), Var(0)], ConjType::Normal),
                Var(4) => ShallowProp::Impl(Var(2), Var(3), ImplType::Normal),
            ],
        );
        assert_eq!(
            *decomp.sources(),
            hashmap![
                ClId(0) => ClauseSource::ConjElim(Var(2), 0, 2),
                ClId(1) => ClauseSource::ConjElim(Var(2), 1, 2),
                ClId(2) => ClauseSource::ConjIntro(Var(3)),
                ClId(3) => ClauseSource::ImplIntro(Var(4)),
            ]
        );
    }

    #[test]
    fn test_from_prop5() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = ImplS(EquivS(Atom(id1), NegS(Atom(id1))), Disj(vec![]));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet {
                    vec: vec![
                        Clause::Disj(vec![Var(1)], vec![]),
                        Clause::Impl(Var(0), Var(1), Var(2)),
                        Clause::Conj(vec![Var(2), Var(0)], Var(1)),
                        Clause::Conj(vec![Var(3), Var(0)], Var(2)),
                        Clause::Conj(vec![Var(4), Var(2)], Var(0)),
                        Clause::Conj(vec![Var(5)], Var(3)),
                        Clause::Conj(vec![Var(5)], Var(4)),
                        Clause::Impl(Var(5), Var(1), Var(6)),
                    ]
                },
                suc: Var(6),
            },
        );
        assert_eq!(
            decomp.prop_map().to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Disj(vec![]),
                Var(2) => ShallowProp::Impl(Var(0), Var(1), ImplType::Neg),
                Var(3) => ShallowProp::Impl(Var(0), Var(2), ImplType::Normal),
                Var(4) => ShallowProp::Impl(Var(2), Var(0), ImplType::Normal),
                Var(5) => ShallowProp::Conj(vec![Var(3), Var(4)], ConjType::Equiv),
                Var(6) => ShallowProp::Impl(Var(5), Var(1), ImplType::Normal),
            ],
        );
        assert_eq!(
            *decomp.sources(),
            hashmap![
                ClId(0) => ClauseSource::DisjElim(Var(1)),
                ClId(1) => ClauseSource::ImplIntro(Var(2)),
                ClId(2) => ClauseSource::ImplElim(Var(2)),
                ClId(3) => ClauseSource::ImplElim(Var(3)),
                ClId(4) => ClauseSource::ImplElim(Var(4)),
                ClId(5) => ClauseSource::ConjElim(Var(5), 0, 2),
                ClId(6) => ClauseSource::ConjElim(Var(5), 1, 2),
                ClId(7) => ClauseSource::ImplIntro(Var(6)),
            ]
        );
    }

    #[test]
    fn test_convert_nj1() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = ImplS(Atom(id1), Atom(id1));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(ClId(0), Proof::Hypothesis(0), Proof::Hypothesis(0));
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            NjProof {
                prop: ImplS(Atom(id1), Atom(id1)),
                kind: AppS(
                    NjProof {
                        prop: ImplS(ImplS(Atom(id1), Atom(id1)), ImplS(Atom(id1), Atom(id1))),
                        kind: AbsS(NjProof {
                            prop: ImplS(Atom(id1), Atom(id1)),
                            kind: Var(Idx(0))
                        })
                    },
                    NjProof {
                        prop: ImplS(Atom(id1), Atom(id1)),
                        kind: AbsS(NjProof {
                            prop: Atom(id1),
                            kind: Var(Idx(0))
                        })
                    }
                )
            }
        );
    }

    #[test]
    fn test_convert_nj2() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = ImplS(Atom(id1), Atom(id1));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(
            ClId(0),
            Proof::Hypothesis(0),
            Proof::ApplyImplS(ClId(0), Proof::Hypothesis(0), Proof::Hypothesis(1)),
        );
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            NjProof {
                prop: ImplS(Atom(Id(0)), Atom(Id(0))),
                kind: AppS(
                    NjProof {
                        prop: ImplS(
                            ImplS(Atom(Id(0)), Atom(Id(0))),
                            ImplS(Atom(Id(0)), Atom(Id(0)))
                        ),
                        kind: AbsS(NjProof {
                            prop: ImplS(Atom(Id(0)), Atom(Id(0))),
                            kind: AppS(
                                NjProof {
                                    prop: ImplS(
                                        ImplS(Atom(Id(0)), Atom(Id(0))),
                                        ImplS(Atom(Id(0)), Atom(Id(0)))
                                    ),
                                    kind: AbsS(NjProof {
                                        prop: ImplS(Atom(Id(0)), Atom(Id(0))),
                                        kind: Var(Idx(1))
                                    })
                                },
                                NjProof {
                                    prop: ImplS(Atom(Id(0)), Atom(Id(0))),
                                    kind: AbsS(NjProof {
                                        prop: Atom(Id(0)),
                                        kind: Var(Idx(0))
                                    })
                                }
                            )
                        })
                    },
                    NjProof {
                        prop: ImplS(Atom(Id(0)), Atom(Id(0))),
                        kind: AbsS(NjProof {
                            prop: Atom(Id(0)),
                            kind: Var(Idx(0))
                        })
                    }
                )
            }
        );
    }

    #[test]
    fn test_convert_nj3() {
        use ProofKindShorthands::*;
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1)));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(
            ClId(1),
            Proof::ApplyImplS(ClId(0), Proof::Hypothesis(1), Proof::Hypothesis(0)),
            Proof::Hypothesis(0),
        );
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            NjProof {
                prop: ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1))),
                kind: AppS(
                    NjProof {
                        prop: ImplS(
                            ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1))),
                            ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1)))
                        ),
                        kind: AbsS(NjProof {
                            prop: ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1))),
                            kind: Var(Idx(0))
                        })
                    },
                    NjProof {
                        prop: ImplS(Atom(id1), ImplS(Atom(id2), Atom(id1))),
                        kind: AbsS(NjProof {
                            prop: ImplS(Atom(id2), Atom(id1)),
                            kind: AppS(
                                NjProof {
                                    prop: ImplS(
                                        ImplS(Atom(id2), Atom(id1)),
                                        ImplS(Atom(id2), Atom(id1))
                                    ),
                                    kind: AbsS(NjProof {
                                        prop: ImplS(Atom(id2), Atom(id1)),
                                        kind: Var(Idx(0))
                                    })
                                },
                                NjProof {
                                    prop: ImplS(Atom(Id(1)), Atom(Id(0))),
                                    kind: AbsS(NjProof {
                                        prop: Atom(Id(0)),
                                        kind: Var(Idx(1))
                                    })
                                }
                            )
                        })
                    }
                )
            },
        );
    }
}
