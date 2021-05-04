use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prop {
    Atom(Id),
    Impl(Box<Prop>, Box<Prop>),
    Conj(Vec<Prop>),
    Disj(Vec<Prop>),
}

impl Prop {
    #[allow(non_snake_case)]
    pub fn ImplS(lhs: Prop, rhs: Prop) -> Self {
        Self::Impl(Box::new(lhs), Box::new(rhs))
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
