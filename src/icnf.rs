use std::collections::HashMap;

use crate::prop::{Id, IdGen, Prop};

/// iCNF (implicational CNF), a modified version of CNF for intuitionistic logic.
///
/// Traditional CNF can be seen as the following sequent:
///
/// ```text
/// (a \/ ~b), (b \/ ~c), (c \/ ~d \/ e) |- false
/// ```
///
/// In this interpretation, CNF-SAT's goal is to **refute** the sequent.
///
/// We extend the "CNF" above in the following ways:
///
/// - Unlike classical CNF, there is a succedent (conclusion part of a sequent).
///   iCNF's succedent is always a variable.
/// - Negative literals are treated differently. Instead of `a \/ b \/ ~c \/ ~d`, we use `c /\ d -> a \/ b` as a clause.
///   Note that they're classically equivalent, but intuitionistically different.
/// - There is a special type of clause in the form of `(a -> b) -> c`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Icnf {
    /// Hypotheses
    pub ant: Vec<Clause>,
    /// Goal
    pub suc: Id,
}

impl Icnf {
    pub fn from_prop(idgen: &mut IdGen, prop: &Prop) -> (Icnf, Decomposition) {
        let mut ant = vec![];
        let mut decomp = Decomposition {
            map: HashMap::new(),
        };
        let suc = Self::from_prop_with(idgen, &mut decomp, &mut ant, prop, true, false);
        (Self { ant, suc }, decomp)
    }
    fn from_prop_with(
        idgen: &mut IdGen,
        decomp: &mut Decomposition,
        ant: &mut Vec<Clause>,
        prop: &Prop,
        positive: bool,
        negative: bool,
    ) -> Id {
        match prop {
            Prop::Atom(id) => {
                decomp.map.insert(*id, ShallowProp::Atom);
                *id
            }
            Prop::Impl(lhs, rhs) => {
                let lhs = Self::from_prop_with(idgen, decomp, ant, lhs, negative, positive);
                let rhs = Self::from_prop_with(idgen, decomp, ant, rhs, positive, negative);
                let id = idgen.fresh();
                if positive {
                    ant.push(Clause::Impl(lhs, rhs, id));
                }
                if negative {
                    ant.push(Clause::Conj(vec![id, lhs], rhs));
                }
                decomp.map.insert(id, ShallowProp::Impl(lhs, rhs));
                id
            }
            Prop::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| {
                        Self::from_prop_with(idgen, decomp, ant, child, positive, negative)
                    })
                    .collect::<Vec<_>>();
                let id = idgen.fresh();
                if positive {
                    ant.push(Clause::Conj(children.clone(), id));
                }
                if negative {
                    for &child in &children {
                        ant.push(Clause::Conj(vec![id], child));
                    }
                }
                decomp.map.insert(id, ShallowProp::Conj(children));
                id
            }
            Prop::Disj(children) => {
                let children = children
                    .iter()
                    .map(|child| {
                        Self::from_prop_with(idgen, decomp, ant, child, positive, negative)
                    })
                    .collect::<Vec<_>>();
                let id = idgen.fresh();
                if positive {
                    for &child in &children {
                        ant.push(Clause::Conj(vec![child], id));
                    }
                }
                if negative {
                    ant.push(Clause::Disj(vec![id], children.clone()));
                }
                decomp.map.insert(id, ShallowProp::Disj(children));
                id
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Clause {
    /// (p1 & p2 & ... & pn) -> q
    Conj(Vec<Id>, Id),
    /// (p1 & p2 & ... & pn) -> (q1 | q2 | ... | qn)
    Disj(Vec<Id>, Vec<Id>),
    /// (p -> q) -> r
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
                    suc: Id(0)
                },
                Decomposition {
                    map: hashmap! {Id(0) => ShallowProp::Atom},
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
                    ant: vec![Clause::Impl(Id(0), Id(0), Id(1)),],
                    suc: Id(1)
                },
                Decomposition {
                    map: hashmap! {
                        Id(0) => ShallowProp::Atom,
                        Id(1) => ShallowProp::Impl(Id(0), Id(0)),
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
                        Clause::Disj(vec![Id(2)], vec![Id(0), Id(1)]),
                        Clause::Conj(vec![Id(1)], Id(3)),
                        Clause::Conj(vec![Id(0)], Id(3)),
                        Clause::Impl(Id(2), Id(3), Id(4)),
                    ],
                    suc: Id(4),
                },
                Decomposition {
                    map: hashmap! {
                        Id(3) => ShallowProp::Disj(vec![Id(1), Id(0)]),
                        Id(1) => ShallowProp::Atom,
                        Id(0) => ShallowProp::Atom,
                        Id(2) => ShallowProp::Disj(vec![Id(0), Id(1)]),
                        Id(4) => ShallowProp::Impl(Id(2), Id(3)),
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
                        Clause::Conj(vec![Id(2)], Id(0)),
                        Clause::Conj(vec![Id(2)], Id(1)),
                        Clause::Conj(vec![Id(1), Id(0)], Id(3)),
                        Clause::Impl(Id(2), Id(3), Id(4)),
                    ],
                    suc: Id(4),
                },
                Decomposition {
                    map: hashmap![
                        Id(3) => ShallowProp::Conj(vec![Id(1), Id(0)]),
                        Id(1) => ShallowProp::Atom,
                        Id(0) => ShallowProp::Atom,
                        Id(2) => ShallowProp::Conj(vec![Id(0), Id(1)]),
                        Id(4) => ShallowProp::Impl(Id(2), Id(3)),
                    ],
                },
            )
        );
    }
}
