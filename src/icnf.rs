use std::collections::HashMap;

use crate::prop::{Id, Prop};

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
    pub ant: ClauseSet,
    /// Goal
    pub suc: Var,
}

impl Icnf {
    pub fn from_prop(vargen: &mut VarGen, prop: &Prop) -> (Icnf, Decomposition) {
        let mut ant = ClauseSet::default();
        let mut decomp = Decomposition {
            id_map: HashMap::new(),
            map: HashMap::new(),
        };
        let suc = Self::from_prop_with(vargen, &mut decomp, &mut ant, prop, true, false);
        (Self { ant, suc }, decomp)
    }
    fn from_prop_with(
        vargen: &mut VarGen,
        decomp: &mut Decomposition,
        ant: &mut ClauseSet,
        prop: &Prop,
        positive: bool,
        negative: bool,
    ) -> Var {
        match prop {
            Prop::Atom(id) => {
                let var = *decomp.id_map.entry(*id).or_insert_with(|| vargen.fresh());
                decomp.map.insert(var, ShallowProp::Atom(*id));
                var
            }
            Prop::Impl(lhs, rhs) => {
                let lhs = Self::from_prop_with(vargen, decomp, ant, lhs, negative, positive);
                let rhs = Self::from_prop_with(vargen, decomp, ant, rhs, positive, negative);
                let var = vargen.fresh();
                if positive {
                    ant.push(Clause::Impl(lhs, rhs, var));
                }
                if negative {
                    ant.push(Clause::Conj(vec![var, lhs], rhs));
                }
                decomp.map.insert(var, ShallowProp::Impl(lhs, rhs));
                var
            }
            Prop::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| {
                        Self::from_prop_with(vargen, decomp, ant, child, positive, negative)
                    })
                    .collect::<Vec<_>>();
                let var = vargen.fresh();
                if positive {
                    ant.push(Clause::Conj(children.clone(), var));
                }
                if negative {
                    for &child in &children {
                        ant.push(Clause::Conj(vec![var], child));
                    }
                }
                decomp.map.insert(var, ShallowProp::Conj(children));
                var
            }
            Prop::Disj(children) => {
                let children = children
                    .iter()
                    .map(|child| {
                        Self::from_prop_with(vargen, decomp, ant, child, positive, negative)
                    })
                    .collect::<Vec<_>>();
                let var = vargen.fresh();
                if positive {
                    for &child in &children {
                        ant.push(Clause::Conj(vec![child], var));
                    }
                }
                if negative {
                    ant.push(Clause::Disj(vec![var], children.clone()));
                }
                decomp.map.insert(var, ShallowProp::Disj(children));
                var
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Clause {
    /// (p1 & p2 & ... & pn) -> q
    Conj(Vec<Var>, Var),
    /// (p1 & p2 & ... & pn) -> (q1 | q2 | ... | qn)
    Disj(Vec<Var>, Vec<Var>),
    /// (p -> q) -> r
    Impl(Var, Var, Var),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decomposition {
    id_map: HashMap<Id, Var>,
    map: HashMap<Var, ShallowProp>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShallowProp {
    Atom(Id),
    Impl(Var, Var),
    Conj(Vec<Var>),
    Disj(Vec<Var>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub usize);

#[derive(Debug, Clone, Default)]
pub struct VarGen {
    next_id: usize,
}

impl VarGen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fresh(&mut self) -> Var {
        let var = Var(self.next_id);
        self.next_id += 1;
        var
    }

    pub fn max_id(&self) -> usize {
        self.next_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ClauseSet {
    pub vec: Vec<Clause>,
}

impl ClauseSet {
    pub fn push(&mut self, clause: Clause) -> ClId {
        let id = ClId(self.vec.len());
        self.vec.push(clause);
        id
    }

    pub fn enumerate<'a>(&'a self) -> impl Iterator<Item = (ClId, &'a Clause)> {
        self.vec.iter().enumerate().map(|(i, cl)| (ClId(i), cl))
    }
}

impl<'a> IntoIterator for &'a ClauseSet {
    type Item = &'a Clause;
    type IntoIter = std::slice::Iter<'a, Clause>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl std::ops::Index<ClId> for ClauseSet {
    type Output = Clause;
    fn index(&self, idx: ClId) -> &Self::Output {
        &self.vec[idx.0]
    }
}

impl std::ops::IndexMut<ClId> for ClauseSet {
    fn index_mut(&mut self, idx: ClId) -> &mut Self::Output {
        &mut self.vec[idx.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proof {
    /// Use the k-th hypothesis in the global index (not de Brujin)
    Hypothesis(usize),
    /// `clX p1 p2 ... pN`
    ApplyConj(ClId, Vec<Proof>),
    /// `match (clX p1 p2 ... pN) with hyp1 => q1 | hyp2 => q2 | ... | hypM => qM end`
    ApplyDisj(ClId, Vec<Proof>, Vec<Proof>),
    /// `let concl = clX (hyp => p1) in p2`
    ApplyImpl(ClId, Box<Proof>, Box<Proof>),
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
        let result = Icnf::from_prop(&mut VarGen::new(), &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: ClauseSet { vec: vec![] },
                    suc: Var(0)
                },
                Decomposition {
                    id_map: hashmap![Id(0) => Var(0)],
                    map: hashmap! {Var(0) => ShallowProp::Atom(Id(0))},
                }
            )
        );
    }

    #[test]
    fn test_from_prop2() {
        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Prop::Impl(Box::new(Prop::Atom(id1)), Box::new(Prop::Atom(id1)));
        let result = Icnf::from_prop(&mut VarGen::new(), &prop);
        assert_eq!(
            result,
            (
                Icnf {
                    ant: ClauseSet {
                        vec: vec![Clause::Impl(Var(0), Var(0), Var(1)),]
                    },
                    suc: Var(1)
                },
                Decomposition {
                    id_map: hashmap![Id(0) => Var(0)],
                    map: hashmap! {
                        Var(0) => ShallowProp::Atom(Id(0)),
                        Var(1) => ShallowProp::Impl(Var(0), Var(0)),
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
        let result = Icnf::from_prop(&mut VarGen::new(), &prop);
        assert_eq!(
            result,
            (
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
                Decomposition {
                    id_map: hashmap![Id(0) => Var(0), Id(1) => Var(1)],
                    map: hashmap! {
                        Var(3) => ShallowProp::Disj(vec![Var(1), Var(0)]),
                        Var(1) => ShallowProp::Atom(Id(1)),
                        Var(0) => ShallowProp::Atom(Id(0)),
                        Var(2) => ShallowProp::Disj(vec![Var(0), Var(1)]),
                        Var(4) => ShallowProp::Impl(Var(2), Var(3)),
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
        let result = Icnf::from_prop(&mut VarGen::new(), &prop);
        assert_eq!(
            result,
            (
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
                Decomposition {
                    id_map: hashmap![Id(0) => Var(0), Id(1) => Var(1)],
                    map: hashmap![
                        Var(3) => ShallowProp::Conj(vec![Var(1), Var(0)]),
                        Var(1) => ShallowProp::Atom(Id(1)),
                        Var(0) => ShallowProp::Atom(Id(0)),
                        Var(2) => ShallowProp::Conj(vec![Var(0), Var(1)]),
                        Var(4) => ShallowProp::Impl(Var(2), Var(3)),
                    ],
                },
            )
        );
    }
}
