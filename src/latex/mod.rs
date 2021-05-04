use askama::Template; // bring trait in scope

#[derive(Template)]
#[template(path = "result.tex", escape = "none")]
struct LatexTemplate {}

pub fn to_latex() -> String {
    LatexTemplate {}.render().unwrap()
}
