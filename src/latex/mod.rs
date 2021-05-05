use askama::Template;

use self::nj::nj_latex;
use self::prop::prop_latex;
use crate::parsing::{ParseError, Prop as PropAst};
use crate::visible_proof::VisibleProof;

mod nj;
mod prop;

#[derive(Template)]
#[template(path = "success.tex", escape = "none", syntax = "erbvariant")]
struct SuccessTemplate {
    #[allow(dead_code)]
    prop: String,
    #[allow(dead_code)]
    proof: Option<String>,
}

pub fn success_latex(prop: &PropAst, pf: Option<&VisibleProof>) -> String {
    SuccessTemplate {
        prop: prop_latex(prop),
        proof: pf.map(|pf| nj_latex(pf)),
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
