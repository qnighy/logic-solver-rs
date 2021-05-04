use askama::Template;

use crate::parsing::ParseError;

#[derive(Template)]
#[template(path = "result.tex", escape = "none", syntax = "erbvariant")]
struct LatexTemplate {
    #[allow(dead_code)]
    source: String,
    #[allow(dead_code)]
    error: String,
}

pub fn parse_error_latex(src: &str, e: ParseError) -> String {
    LatexTemplate {
        source: src.to_owned(),
        error: e.to_string(),
    }
    .render()
    .unwrap()
}
