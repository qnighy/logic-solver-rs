use std::collections::HashMap;

use crate::ast::Prop as PropAst;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prop {
    Atom(Id),
    Impl(Box<Prop>, Box<Prop>),
    Conj(Vec<Prop>),
    Disj(Vec<Prop>),
}

impl Prop {
    pub fn from_ast(idgen: &mut IdGen, env: &mut Env, ast: &PropAst) -> Prop {
        match ast {
            PropAst::Atom(ident) => {
                let id = env.find_name_or_fresh(idgen, ident);
                Prop::Atom(id)
            }
            PropAst::Impl(lhs, rhs) => {
                let lhs = Self::from_ast(idgen, env, lhs);
                let rhs = Self::from_ast(idgen, env, rhs);
                Prop::Impl(Box::new(lhs), Box::new(rhs))
            }
            PropAst::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| Self::from_ast(idgen, env, child))
                    .collect::<Vec<_>>();
                Prop::Conj(children)
            }
            PropAst::Disj(children) => {
                let children = children
                    .iter()
                    .map(|child| Self::from_ast(idgen, env, child))
                    .collect::<Vec<_>>();
                Prop::Disj(children)
            }
        }
    }

    pub fn to_ast(&self, env: &Env) -> PropAst {
        match self {
            Prop::Atom(id) => PropAst::Atom(env.get_name(*id).unwrap().to_owned()),
            Prop::Impl(lhs, rhs) => {
                let lhs = lhs.to_ast(env);
                let rhs = rhs.to_ast(env);
                PropAst::Impl(Box::new(lhs), Box::new(rhs))
            }
            Prop::Conj(children) => {
                let children = children
                    .iter()
                    .map(|child| child.to_ast(env))
                    .collect::<Vec<_>>();
                PropAst::Conj(children)
            }
            Prop::Disj(children) => {
                let children = children
                    .iter()
                    .map(|child| child.to_ast(env))
                    .collect::<Vec<_>>();
                PropAst::Disj(children)
            }
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
        Self { next_id: 1 }
    }

    pub fn fresh(&mut self) -> Id {
        let num = self.next_id;
        self.next_id += 1;
        Id(num)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(#[cfg(not(test))] usize, #[cfg(test)] pub usize);

#[cfg(test)]
mod tests {
    use super::*;
    use big_s::S;

    #[test]
    fn test_from_ast1() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = Prop::from_ast(&mut idgen, &mut env, &PropAst::Atom(S("A")));
        assert_eq!(prop, Prop::Atom(Id(1)));
    }

    #[test]
    fn test_from_ast2() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = Prop::from_ast(
            &mut idgen,
            &mut env,
            &PropAst::Impl(
                Box::new(PropAst::Atom(S("A"))),
                Box::new(PropAst::Atom(S("B"))),
            ),
        );
        assert_eq!(
            prop,
            Prop::Impl(Box::new(Prop::Atom(Id(1))), Box::new(Prop::Atom(Id(2))))
        );
    }

    #[test]
    fn test_from_ast3() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = Prop::from_ast(
            &mut idgen,
            &mut env,
            &PropAst::Impl(
                Box::new(PropAst::Atom(S("A"))),
                Box::new(PropAst::Atom(S("A"))),
            ),
        );
        assert_eq!(
            prop,
            Prop::Impl(Box::new(Prop::Atom(Id(1))), Box::new(Prop::Atom(Id(1))))
        );
    }

    #[test]
    fn test_to_ast1() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        assert_eq!(Prop::Atom(id1).to_ast(&env), PropAst::Atom(S("A")));
    }

    #[test]
    fn test_to_ast2() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        let id2 = env.find_name_or_fresh(&mut idgen, "B");
        assert_eq!(
            Prop::Conj(vec![Prop::Atom(id1), Prop::Atom(id2)]).to_ast(&env),
            PropAst::Conj(vec![PropAst::Atom(S("A")), PropAst::Atom(S("B"))])
        );
    }

    #[test]
    fn test_to_ast3() {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        assert_eq!(
            Prop::Disj(vec![Prop::Atom(id1), Prop::Atom(id1)]).to_ast(&env),
            PropAst::Disj(vec![PropAst::Atom(S("A")), PropAst::Atom(S("A"))])
        );
    }
}
