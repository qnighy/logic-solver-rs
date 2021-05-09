use super::prop::promote_prop;
use crate::kripke::{KripkeRefutation, VisibleKripkeRefutation};
use crate::prop::Env;

pub fn promote_kripke(rft: &KripkeRefutation, env: &Env) -> VisibleKripkeRefutation {
    let num_worlds = rft.num_worlds;
    let accessibility = rft.accessibility.clone();
    let valuation = rft
        .subprops
        .iter()
        .map(|prop| {
            let prop_ast = promote_prop(prop, env);
            let val = rft.valuation[prop].clone();
            (prop_ast, val)
        })
        .collect::<Vec<_>>();
    VisibleKripkeRefutation {
        num_worlds,
        accessibility,
        valuation,
    }
}
