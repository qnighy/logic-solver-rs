use std::collections::{HashMap, HashSet};

use crate::ipc::icnf::{Clause, ClauseSet, Icnf, Var, VarGen};
use crate::prop::{Id, Prop};

impl Icnf {
    pub fn from_prop(vargen: &mut VarGen, prop: &Prop) -> (Icnf, PropMap) {
        let mut prop_map = PropMap::default();
        let prop_var = prop_map.insert_prop(vargen, prop);

        let mut pol_map = PolarityMap::default();
        pol_map.mark_polarity(&prop_map, prop_var, true);

        let mut ant = ClauseSet::default();
        for (v, sp) in prop_map.iter() {
            let (pos, neg) = pol_map.polarity(v);
            match sp {
                ShallowProp::Atom(..) => {}
                ShallowProp::Impl(lhs, rhs) => {
                    if pos {
                        ant.push(Clause::Impl(*lhs, *rhs, v));
                    }
                    if neg {
                        ant.push(Clause::Conj(vec![v, *lhs], *rhs));
                    }
                }
                ShallowProp::Conj(children) => {
                    if pos {
                        ant.push(Clause::Conj(children.clone(), v));
                    }
                    if neg {
                        for &child in children {
                            ant.push(Clause::Conj(vec![v], child));
                        }
                    }
                }
                ShallowProp::Disj(children) => {
                    if pos {
                        for &child in children {
                            ant.push(Clause::Conj(vec![child], v));
                        }
                    }
                    if neg {
                        ant.push(Clause::Disj(vec![v], children.clone()));
                    }
                }
            }
        }

        (Self { ant, suc: prop_var }, prop_map)
    }
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
    pub fn insert_prop(&mut self, vargen: &mut VarGen, prop: &Prop) -> Var {
        let sp = match prop {
            Prop::Atom(id) => ShallowProp::Atom(*id),
            Prop::Impl(lhs, rhs) => {
                let lhs = self.insert_prop(vargen, lhs);
                let rhs = self.insert_prop(vargen, rhs);
                ShallowProp::Impl(lhs, rhs)
            }
            Prop::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| self.insert_prop(vargen, child))
                    .collect::<Vec<_>>();
                ShallowProp::Conj(children)
            }
            Prop::Disj(children) => {
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
        match decomp.get(v).unwrap() {
            ShallowProp::Atom(..) => {}
            ShallowProp::Impl(lhs, rhs) => {
                self.mark_polarity(decomp, *lhs, !positive);
                self.mark_polarity(decomp, *rhs, positive);
            }
            ShallowProp::Conj(children) => {
                for &child in children {
                    self.mark_polarity(decomp, child, positive);
                }
            }
            ShallowProp::Disj(children) => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prop::IdGen;
    use maplit::hashmap;

    #[test]
    fn test_from_prop1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Atom(id1);
        let (icnf, prop_map) = Icnf::from_prop(&mut VarGen::new(), &prop);
        assert_eq!(
            icnf,
            Icnf {
                ant: ClauseSet { vec: vec![] },
                suc: Var(0)
            },
        );
        assert_eq!(
            prop_map.to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
            ]
        );
    }

    #[test]
    fn test_from_prop2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
        let (icnf, prop_map) = Icnf::from_prop(&mut VarGen::new(), &prop);
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
            prop_map.to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Impl(Var(0), Var(0)),
            ],
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
        let (icnf, prop_map) = Icnf::from_prop(&mut VarGen::new(), &prop);
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
            prop_map.to_map(),
            hashmap! [
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Atom(Id(1)),
                Var(2) => ShallowProp::Disj(vec![Var(0), Var(1)]),
                Var(3) => ShallowProp::Disj(vec![Var(1), Var(0)]),
                Var(4) => ShallowProp::Impl(Var(2), Var(3)),
            ],
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
        let (icnf, prop_map) = Icnf::from_prop(&mut VarGen::new(), &prop);
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
            prop_map.to_map(),
            hashmap![
                Var(0) => ShallowProp::Atom(Id(0)),
                Var(1) => ShallowProp::Atom(Id(1)),
                Var(2) => ShallowProp::Conj(vec![Var(0), Var(1)]),
                Var(3) => ShallowProp::Conj(vec![Var(1), Var(0)]),
                Var(4) => ShallowProp::Impl(Var(2), Var(3)),
            ],
        );
    }
}
