use std::collections::{HashMap, HashSet};

use crate::debruijn::Idx;
use crate::ipc::icnf::{ClId, Clause, ClauseSet, Icnf, Proof, Var, VarGen};
use crate::nj::Proof as NjProof;
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
                ShallowProp::Impl(lhs, rhs) => {
                    if pos {
                        let cl = ant.push(Clause::Impl(lhs, rhs, v));
                        sources.insert(cl, ClauseSource::ImplIntro(v));
                    }
                    if neg {
                        let cl = ant.push(Clause::Conj(vec![v, lhs], rhs));
                        sources.insert(cl, ClauseSource::ImplElim(v));
                    }
                }
                ShallowProp::Conj(ref children) => {
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
        match *pf {
            Proof::Hypothesis(i) => NjProof::Var(Idx(i)),
            Proof::ApplyConj(cl_id, ref children) => match *self.sources.get(&cl_id).unwrap() {
                ClauseSource::ImplElim(v) => {
                    let (vl, _) = self.prop_map.get(v).unwrap().as_impl().unwrap();
                    NjProof::AppS(
                        self.convert_nj(&children[0], v),
                        self.convert_nj(&children[1], vl),
                    )
                }
                ClauseSource::ConjIntro(v) => {
                    let vc = self.prop_map.get(v).unwrap().as_conj().unwrap();
                    NjProof::ConjIntro(
                        children
                            .iter()
                            .zip(vc)
                            .map(|(child, &subgoal)| self.convert_nj(child, subgoal))
                            .collect(),
                    )
                }
                ClauseSource::ConjElim(v, i, n) => {
                    NjProof::ConjElimS(self.convert_nj(&children[0], v), i, n)
                }
                ClauseSource::DisjIntro(v, i, _) => {
                    let vc = self.prop_map.get(v).unwrap().as_disj().unwrap();
                    let cprops = vc
                        .iter()
                        .map(|&v| self.prop_map.get_prop(v))
                        .collect::<Vec<_>>();
                    NjProof::DisjIntroS(cprops, self.convert_nj(&children[0], vc[i]), i)
                }
                ClauseSource::ImplIntro(_) | ClauseSource::DisjElim(_) => unreachable!(),
            },
            Proof::ApplyDisj(cl_id, ref children, ref branches) => {
                if let ClauseSource::DisjElim(v) = *self.sources.get(&cl_id).unwrap() {
                    NjProof::DisjElimS(
                        self.prop_map.get_prop(goal),
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
                    let (vl, vr) = self.prop_map.get(v).unwrap().as_impl().unwrap();
                    let nj_lhs =
                        NjProof::AbsS(self.prop_map.get_prop(vl), self.convert_nj(lhs, vr));
                    let nj_rhs =
                        NjProof::AbsS(self.prop_map.get_prop(v), self.convert_nj(rhs, goal));
                    NjProof::AppS(nj_rhs, nj_lhs)
                } else {
                    unreachable!()
                }
            }
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
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (Var, &'a ShallowProp)> {
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
            ShallowProp::Impl(lhs, rhs) => {
                Prop::Impl(Box::new(self.get_prop(lhs)), Box::new(self.get_prop(rhs)))
            }
            ShallowProp::Conj(ref children) => {
                Prop::Conj(children.iter().map(|&child| self.get_prop(child)).collect())
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
                ShallowProp::Impl(lhs, rhs)
            }
            Prop::Conj(ref children) => {
                let children = children
                    .iter()
                    .map(|child| self.insert_prop(vargen, child))
                    .collect::<Vec<_>>();
                ShallowProp::Conj(children)
            }
            Prop::Disj(ref children) => {
                let children = children
                    .iter()
                    .map(|child| self.insert_prop(vargen, child))
                    .collect::<Vec<_>>();
                ShallowProp::Disj(children)
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
            ShallowProp::Impl(lhs, rhs) => {
                self.mark_polarity(decomp, lhs, !positive);
                self.mark_polarity(decomp, rhs, positive);
            }
            ShallowProp::Conj(ref children) => {
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
    Impl(Var, Var),
    Conj(Vec<Var>),
    Disj(Vec<Var>),
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
        if let ShallowProp::Impl(lhs, rhs) = *self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn as_conj(&self) -> Option<&[Var]> {
        if let ShallowProp::Conj(ref children) = *self {
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
    use crate::nj::ProofShorthands as NjProofShorthands;
    use crate::prop::IdGen;
    use maplit::hashmap;

    #[test]
    fn test_from_prop1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Atom(id1);
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
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
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
                Var(1) => ShallowProp::Impl(Var(0), Var(0)),
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
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = Prop::Impl(
            Box::new(Prop::Disj(vec![Prop::Atom(id1), Prop::Atom(id2)])),
            Box::new(Prop::Disj(vec![Prop::Atom(id2), Prop::Atom(id1)])),
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
                Var(4) => ShallowProp::Impl(Var(2), Var(3)),
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
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = Prop::Impl(
            Box::new(Prop::Conj(vec![Prop::Atom(id1), Prop::Atom(id2)])),
            Box::new(Prop::Conj(vec![Prop::Atom(id2), Prop::Atom(id1)])),
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
                Var(2) => ShallowProp::Conj(vec![Var(0), Var(1)]),
                Var(3) => ShallowProp::Conj(vec![Var(1), Var(0)]),
                Var(4) => ShallowProp::Impl(Var(2), Var(3)),
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
    fn test_convert_nj1() {
        use NjProofShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(ClId(0), Proof::Hypothesis(0), Proof::Hypothesis(0));
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            AppS(
                AbsS(
                    Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1))),
                    Var(Idx(0))
                ),
                AbsS(Prop::Atom(id1), Var(Idx(0)))
            )
        );
    }

    #[test]
    fn test_convert_nj2() {
        use NjProofShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(
            ClId(0),
            Proof::Hypothesis(0),
            Proof::ApplyImplS(ClId(0), Proof::Hypothesis(0), Proof::Hypothesis(1)),
        );
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            AppS(
                AbsS(
                    Prop::Impl(Box::new(Prop::Atom(Id(0))), Box::new(Prop::Atom(Id(0)))),
                    AppS(
                        AbsS(
                            Prop::Impl(Box::new(Prop::Atom(Id(0))), Box::new(Prop::Atom(Id(0)))),
                            Var(Idx(1))
                        ),
                        AbsS(Prop::Atom(Id(0)), Var(Idx(0)))
                    )
                ),
                AbsS(Prop::Atom(Id(0)), Var(Idx(0)))
            )
        );
    }

    #[test]
    fn test_convert_nj3() {
        use NjProofShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let prop = Prop::Impl(
            Box::new(Prop::Atom(id1)),
            Box::new(Prop::Impl(
                Box::new(Prop::Atom(id2)),
                Box::new(Prop::Atom(id1)),
            )),
        );
        let (icnf, decomp) = Decomposition::decompose(&mut VarGen::new(), &prop);
        let pf = Proof::ApplyImplS(
            ClId(1),
            Proof::ApplyImplS(ClId(0), Proof::Hypothesis(1), Proof::Hypothesis(0)),
            Proof::Hypothesis(0),
        );
        let nj = decomp.convert_nj(&pf, icnf.suc);
        assert_eq!(
            nj,
            AppS(
                AbsS(
                    Prop::Impl(
                        Box::new(Prop::Atom(id1)),
                        Box::new(Prop::Impl(
                            Box::new(Prop::Atom(id2)),
                            Box::new(Prop::Atom(id1))
                        ))
                    ),
                    Var(Idx(0))
                ),
                AbsS(
                    Prop::Atom(id1),
                    AppS(
                        AbsS(
                            Prop::Impl(Box::new(Prop::Atom(id2)), Box::new(Prop::Atom(id1))),
                            Var(Idx(0))
                        ),
                        AbsS(Prop::Atom(Id(1)), Var(Idx(1)))
                    )
                )
            ),
        );
    }
}
