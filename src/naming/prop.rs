use crate::parsing::Prop as PropAst;
use crate::prop::Prop;
use crate::prop::{Env, IdGen};

pub fn lower_prop(idgen: &mut IdGen, env: &mut Env, ast: &PropAst) -> Prop {
    match ast {
        PropAst::Atom(ident) => {
            let id = env.find_name_or_fresh(idgen, ident);
            Prop::Atom(id)
        }
        PropAst::Impl(lhs, rhs) => {
            let lhs = lower_prop(idgen, env, lhs);
            let rhs = lower_prop(idgen, env, rhs);
            Prop::ImplS(lhs, rhs)
        }
        PropAst::Conj(children) => {
            let children = children
                .iter()
                .map(|child| lower_prop(idgen, env, child))
                .collect::<Vec<_>>();
            Prop::Conj(children)
        }
        PropAst::Disj(children) => {
            let children = children
                .iter()
                .map(|child| lower_prop(idgen, env, child))
                .collect::<Vec<_>>();
            Prop::Disj(children)
        }
    }
}

pub fn promote_prop(prop: &Prop, env: &Env) -> PropAst {
    match prop {
        Prop::Atom(id) => PropAst::Atom(env.get_name(*id).unwrap().to_owned()),
        Prop::Impl(lhs, rhs) => {
            let lhs = promote_prop(lhs, env);
            let rhs = promote_prop(rhs, env);
            PropAst::Impl(Box::new(lhs), Box::new(rhs))
        }
        Prop::Conj(children) => {
            let children = children
                .iter()
                .map(|child| promote_prop(child, env))
                .collect::<Vec<_>>();
            PropAst::Conj(children)
        }
        Prop::Disj(children) => {
            let children = children
                .iter()
                .map(|child| promote_prop(child, env))
                .collect::<Vec<_>>();
            PropAst::Disj(children)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prop::{Id, PropShorthands};
    use big_s::S;

    #[test]
    fn test_lower_prop1() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = lower_prop(&mut idgen, &mut env, &PropAst::Atom(S("A")));
        assert_eq!(prop, Atom(Id(0)));
    }

    #[test]
    fn test_lower_prop2() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = lower_prop(
            &mut idgen,
            &mut env,
            &PropAst::Impl(
                Box::new(PropAst::Atom(S("A"))),
                Box::new(PropAst::Atom(S("B"))),
            ),
        );
        assert_eq!(prop, ImplS(Atom(Id(0)), Atom(Id(1))));
    }

    #[test]
    fn test_lower_prop3() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let prop = lower_prop(
            &mut idgen,
            &mut env,
            &PropAst::Impl(
                Box::new(PropAst::Atom(S("A"))),
                Box::new(PropAst::Atom(S("A"))),
            ),
        );
        assert_eq!(prop, ImplS(Atom(Id(0)), Atom(Id(0))));
    }

    #[test]
    fn test_promote_prop1() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        assert_eq!(promote_prop(&Atom(id1), &env), PropAst::Atom(S("A")));
    }

    #[test]
    fn test_promote_prop2() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        let id2 = env.find_name_or_fresh(&mut idgen, "B");
        assert_eq!(
            promote_prop(&Conj(vec![Atom(id1), Atom(id2)]), &env),
            PropAst::Conj(vec![PropAst::Atom(S("A")), PropAst::Atom(S("B"))])
        );
    }

    #[test]
    fn test_promote_prop3() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let id1 = env.find_name_or_fresh(&mut idgen, "A");
        assert_eq!(
            promote_prop(&Disj(vec![Atom(id1), Atom(id1)]), &env),
            PropAst::Disj(vec![PropAst::Atom(S("A")), PropAst::Atom(S("A"))])
        );
    }
}
