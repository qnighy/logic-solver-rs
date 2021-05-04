use askama::Template;

use crate::parsing::ParseError;

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
