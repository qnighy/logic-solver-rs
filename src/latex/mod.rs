use askama::Template;

use self::kripke::kripke_frame_latex;
use self::nj::nj_latex;
use self::prop::prop_latex;
use self::split::split_proof;
use crate::kripke::VisibleKripkeRefutation;
use crate::parsing::{ParseError, Prop as PropAst};
use crate::visible_proof::VisibleProof;

mod kripke;
mod nj;
mod prop;
mod split;

#[derive(Template)]
#[template(path = "success.tex", escape = "none", syntax = "erbvariant")]
struct SuccessTemplate {
    #[allow(dead_code)]
    prop: String,
    #[allow(dead_code)]
    provable: bool,
    #[allow(dead_code)]
    proofs: Vec<(String, String)>,
    #[allow(dead_code)]
    refutation: Option<Refutation>,
}

#[derive(Debug, Clone)]
struct Refutation {
    frame: String,
}

pub fn success_latex(
    prop: &PropAst,
    pf: Option<&VisibleProof>,
    rft: Option<&VisibleKripkeRefutation>,
) -> String {
    let provable = pf.is_some();
    let proofs = if let Some(pf) = pf {
        split_proof(pf)
            .into_iter()
            .enumerate()
            .map(|(i, pf)| {
                let name = if i == 0 {
                    "Main proof".to_owned()
                } else {
                    format!("Subproof {}", i)
                };
                (name, nj_latex(&pf))
            })
            .collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    SuccessTemplate {
        prop: prop_latex(prop),
        provable,
        proofs,
        refutation: rft.map(|rft| Refutation {
            frame: kripke_frame_latex(rft),
        }),
    }
    .render()
    .unwrap()
}

#[derive(Template)]
#[template(path = "error.tex", escape = "none", syntax = "erbvariant")]
struct ErrorTemplate {
    #[allow(dead_code)]
    source: String,
    #[allow(dead_code)]
    error: String,
}

pub fn parse_error_latex(src: &str, e: ParseError) -> String {
    ErrorTemplate {
        source: src.to_owned(),
        error: e.to_string(),
    }
    .render()
    .unwrap()
}
