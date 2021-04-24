use std::collections::HashMap;

use crate::prop::{Id, IdGen, Prop};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Icnf {
    ant: Vec<Clause>,
    suc: Id,
}

impl Icnf {
    pub fn from_prop(idgen: &mut IdGen, prop: &Prop) -> (Icnf, Decomposition) {
        let mut ant = vec![];
        let mut decomp = Decomposition {
            map: HashMap::new(),
        };
        let suc = Self::from_prop_with(idgen, &mut decomp, &mut ant, prop);
        (Self { ant, suc }, decomp)
    }
    fn from_prop_with(
        idgen: &mut IdGen,
        decomp: &mut Decomposition,
        ant: &mut Vec<Clause>,
        prop: &Prop,
    ) -> Id {
        match prop {
            Prop::Atom(id) => {
                decomp.map.insert(*id, ShallowProp::Atom);
                *id
            }
            Prop::Impl(lhs, rhs) => {
                let lhs = Self::from_prop_with(idgen, decomp, ant, lhs);
                let rhs = Self::from_prop_with(idgen, decomp, ant, rhs);
                let id = idgen.fresh();
                ant.push(Clause::Impl(lhs, rhs, id));
                ant.push(Clause::Conj(vec![id, lhs], rhs));
                decomp.map.insert(id, ShallowProp::Impl(lhs, rhs));
                id
            }
            Prop::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| Self::from_prop_with(idgen, decomp, ant, child))
                    .collect::<Vec<_>>();
                let id = idgen.fresh();
                ant.push(Clause::Conj(children.clone(), id));
                for &child in &children {
                    ant.push(Clause::Conj(vec![id], child));
                }
                decomp.map.insert(id, ShallowProp::Conj(children));
                id
            }
            Prop::Disj(children) => {
                let children = children
                    .iter()
                    .map(|child| Self::from_prop_with(idgen, decomp, ant, child))
                    .collect::<Vec<_>>();
                let id = idgen.fresh();
                for &child in &children {
                    ant.push(Clause::Conj(vec![child], id));
                }
                ant.push(Clause::Disj(vec![id], children.clone()));
                decomp.map.insert(id, ShallowProp::Disj(children));
                id
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Clause {
    Conj(Vec<Id>, Id),
    Disj(Vec<Id>, Vec<Id>),
    Impl(Id, Id, Id),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decomposition {
    map: HashMap<Id, ShallowProp>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShallowProp {
    Atom,
    Impl(Id, Id),
    Conj(Vec<Id>),
    Disj(Vec<Id>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use maplit::hashmap;

    #[test]
    fn test_from_prop1() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Atom(id1);
        let result = Icnf::from_prop(&mut idgen, &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: vec![],
                    suc: Id(1)
                },
                Decomposition {
                    map: hashmap! {Id(1) => ShallowProp::Atom},
                }
            )
        );
    }

    #[test]
    fn test_from_prop2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
        let result = Icnf::from_prop(&mut idgen, &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: vec![
                        Clause::Impl(Id(1), Id(1), Id(2)),
                        Clause::Conj(vec![Id(2), Id(1)], Id(1)),
                    ],
                    suc: Id(2)
                },
                Decomposition {
                    map: hashmap! {
                        Id(1) => ShallowProp::Atom,
                        Id(2) => ShallowProp::Impl(Id(1), Id(1)),
                    },
                }
            )
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
        let result = Icnf::from_prop(&mut idgen, &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: vec![
                        Clause::Conj(vec![Id(1)], Id(3)),
                        Clause::Conj(vec![Id(2)], Id(3)),
                        Clause::Disj(vec![Id(3)], vec![Id(1), Id(2)]),
                        Clause::Conj(vec![Id(2)], Id(4)),
                        Clause::Conj(vec![Id(1)], Id(4)),
                        Clause::Disj(vec![Id(4)], vec![Id(2), Id(1)]),
                        Clause::Impl(Id(3), Id(4), Id(5)),
                        Clause::Conj(vec![Id(5), Id(3)], Id(4)),
                    ],
                    suc: Id(5),
                },
                Decomposition {
                    map: hashmap! {
                        Id(4) => ShallowProp::Disj(vec![Id(2), Id(1)]),
                        Id(2) => ShallowProp::Atom,
                        Id(1) => ShallowProp::Atom,
                        Id(3) => ShallowProp::Disj(vec![Id(1), Id(2)]),
                        Id(5) => ShallowProp::Impl(Id(3), Id(4)),
                    },
                },
            )
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
        let result = Icnf::from_prop(&mut idgen, &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: vec![
                        Clause::Conj(vec![Id(1), Id(2)], Id(3)),
                        Clause::Conj(vec![Id(3)], Id(1)),
                        Clause::Conj(vec![Id(3)], Id(2)),
                        Clause::Conj(vec![Id(2), Id(1)], Id(4)),
                        Clause::Conj(vec![Id(4)], Id(2)),
                        Clause::Conj(vec![Id(4)], Id(1)),
                        Clause::Impl(Id(3), Id(4), Id(5)),
                        Clause::Conj(vec![Id(5), Id(3)], Id(4))
                    ],
                    suc: Id(5),
                },
                Decomposition {
                    map: hashmap![
                        Id(4) => ShallowProp::Conj(vec![Id(2), Id(1)]),
                        Id(2) => ShallowProp::Atom,
                        Id(1) => ShallowProp::Atom,
                        Id(3) => ShallowProp::Conj(vec![Id(1), Id(2)]),
                        Id(5) => ShallowProp::Impl(Id(3), Id(4)),
                    ],
                },
            )
        );
    }
}
