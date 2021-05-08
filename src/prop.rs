use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prop {
    Atom(Id),
    Impl(Box<Prop>, Box<Prop>),
    Conj(Vec<Prop>),
    Disj(Vec<Prop>),
    Equiv(Box<Prop>, Box<Prop>),
    Neg(Box<Prop>),
}

static BOTTOM: Prop = Prop::Disj(vec![]);

impl Prop {
    pub fn eqtype(&self, other: &Self) -> bool {
        use Prop::*;
        match (self, other) {
            (&Atom(l_id), &Atom(r_id)) => l_id == r_id,
            (Impl(l_lhs, l_rhs), Impl(r_lhs, r_rhs))
            | (Equiv(l_lhs, l_rhs), Equiv(r_lhs, r_rhs)) => {
                l_lhs.eqtype(r_lhs) && l_rhs.eqtype(r_rhs)
            }
            (Conj(l_children), Conj(r_children)) | (Disj(l_children), Disj(r_children)) => {
                l_children.len() == r_children.len()
                    && l_children
                        .iter()
                        .zip(r_children)
                        .all(|(l_child, r_child)| l_child.eqtype(r_child))
            }
            (Neg(l_sub), Neg(r_sub)) => l_sub.eqtype(r_sub),
            (Impl(l_lhs, l_rhs), Neg(r_sub)) => l_lhs.eqtype(r_sub) && l_rhs.eqtype(&BOTTOM),
            (Neg(l_sub), Impl(r_lhs, r_rhs)) => l_sub.eqtype(r_lhs) && BOTTOM.eqtype(r_rhs),
            (Conj(l_children), Equiv(r_lhs, r_rhs)) => {
                l_children.len() == 2
                    && PropRef::expand(&l_children[0]).eqtype(&PropRef::Impl(r_lhs, r_rhs))
                    && PropRef::expand(&l_children[1]).eqtype(&PropRef::Impl(r_rhs, r_lhs))
            }
            (Equiv(l_lhs, l_rhs), Conj(r_children)) => {
                2 == r_children.len()
                    && PropRef::Impl(l_lhs, l_rhs).eqtype(&PropRef::expand(&r_children[0]))
                    && PropRef::Impl(l_rhs, l_lhs).eqtype(&PropRef::expand(&r_children[1]))
            }
            (_, _) => false,
        }
    }

    pub fn as_atom(&self) -> Option<Id> {
        if let Prop::Atom(id) = *self {
            Some(id)
        } else {
            None
        }
    }

    pub fn as_impl(&self) -> Option<(&Prop, &Prop)> {
        if let Prop::Impl(ref lhs, ref rhs) = *self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn as_conj(&self) -> Option<&[Prop]> {
        if let Prop::Conj(ref children) = *self {
            Some(children)
        } else {
            None
        }
    }

    pub fn as_disj(&self) -> Option<&[Prop]> {
        if let Prop::Disj(ref children) = *self {
            Some(children)
        } else {
            None
        }
    }

    #[allow(non_snake_case)]
    pub fn ImplS(lhs: Prop, rhs: Prop) -> Self {
        Self::Impl(Box::new(lhs), Box::new(rhs))
    }

    #[allow(non_snake_case)]
    pub fn EquivS(lhs: Prop, rhs: Prop) -> Self {
        Self::Equiv(Box::new(lhs), Box::new(rhs))
    }

    #[allow(non_snake_case)]
    pub fn NegS(sub: Prop) -> Self {
        Self::Neg(Box::new(sub))
    }
}

#[allow(non_snake_case)]
pub mod PropShorthands {
    use super::*;
    pub use Prop::*;

    #[allow(non_snake_case)]
    pub fn ImplS(lhs: Prop, rhs: Prop) -> Prop {
        Prop::ImplS(lhs, rhs)
    }

    #[allow(non_snake_case)]
    pub fn EquivS(lhs: Prop, rhs: Prop) -> Prop {
        Prop::EquivS(lhs, rhs)
    }

    #[allow(non_snake_case)]
    pub fn NegS(sub: Prop) -> Prop {
        Prop::NegS(sub)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropRef<'a> {
    Direct(&'a Prop),
    Impl(&'a Prop, &'a Prop),
}

impl<'a> PropRef<'a> {
    pub fn expand(prop: &'a Prop) -> Self {
        match prop {
            Prop::Impl(lhs, rhs) => PropRef::Impl(lhs, rhs),
            Prop::Neg(sub) => PropRef::Impl(sub, &BOTTOM),
            _ => PropRef::Direct(prop),
        }
    }
    pub fn eqtype(&self, other: &PropRef<'a>) -> bool {
        match (*self, *other) {
            (PropRef::Direct(l), PropRef::Direct(r)) => l.eqtype(r),
            (PropRef::Impl(l_lhs, l_rhs), PropRef::Impl(r_lhs, r_rhs)) => {
                l_lhs.eqtype(r_lhs) && l_rhs.eqtype(r_rhs)
            }
            (_, _) => false,
        }
    }
}

impl<'a> From<PropRef<'a>> for Prop {
    fn from(r: PropRef<'a>) -> Self {
        match r {
            PropRef::Direct(prop) => prop.clone(),
            PropRef::Impl(lhs, rhs) => Prop::Impl(Box::new(lhs.clone()), Box::new(rhs.clone())),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Env {
    name_map: HashMap<String, Id>,
    id_map: HashMap<Id, String>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn find_name_or_fresh(&mut self, idgen: &mut IdGen, name: &str) -> Id {
        if let Some(&id) = self.name_map.get(name) {
            return id;
        }
        let id = idgen.fresh();
        self.name_map.insert(name.to_owned(), id);
        self.id_map.insert(id, name.to_owned());
        id
    }

    pub fn get_name(&self, id: Id) -> Option<&str> {
        self.id_map.get(&id).map(|s| &**s)
    }
}

#[derive(Debug)]
pub struct IdGen {
    next_id: usize,
}

impl IdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn fresh(&mut self) -> Id {
        let num = self.next_id;
        self.next_id += 1;
        Id(num)
    }

    pub fn max_id(&self) -> usize {
        self.next_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(#[cfg(not(test))] usize, #[cfg(test)] pub usize);

impl Id {
    pub fn index(&self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eqtype1() {
        use PropShorthands::*;

        let lhs = Atom(Id(0));
        let rhs = Atom(Id(0));
        assert!(lhs.eqtype(&rhs));
        assert!(rhs.eqtype(&lhs));
    }

    #[test]
    fn test_eqtype2() {
        use PropShorthands::*;

        let lhs = Atom(Id(0));
        let rhs = Atom(Id(1));
        assert!(!lhs.eqtype(&rhs));
        assert!(!rhs.eqtype(&lhs));
    }

    #[test]
    fn test_eqtype3() {
        use PropShorthands::*;

        let lhs = Conj(vec![Atom(Id(0)), Atom(Id(0))]);
        let rhs = Conj(vec![Atom(Id(0)), Atom(Id(0)), Atom(Id(0))]);
        assert!(!lhs.eqtype(&rhs));
        assert!(!rhs.eqtype(&lhs));
    }

    #[test]
    fn test_eqtype4() {
        use PropShorthands::*;

        let lhs = NegS(Atom(Id(0)));
        let rhs = ImplS(Atom(Id(0)), Disj(vec![]));
        assert!(lhs.eqtype(&rhs));
        assert!(rhs.eqtype(&lhs));
    }

    #[test]
    fn test_eqtype5() {
        use PropShorthands::*;

        let lhs = EquivS(Atom(Id(0)), Atom(Id(1)));
        let rhs = Conj(vec![
            ImplS(Atom(Id(0)), Atom(Id(1))),
            ImplS(Atom(Id(1)), Atom(Id(0))),
        ]);
        assert!(lhs.eqtype(&rhs));
        assert!(rhs.eqtype(&lhs));
    }

    #[test]
    fn test_eqtype6() {
        use PropShorthands::*;

        let lhs = EquivS(Atom(Id(0)), Atom(Id(1)));
        let rhs = Conj(vec![
            ImplS(Atom(Id(1)), Atom(Id(0))),
            ImplS(Atom(Id(0)), Atom(Id(1))),
        ]);
        assert!(!lhs.eqtype(&rhs));
        assert!(!rhs.eqtype(&lhs));
    }
}
