use askama::Template;

use self::kripke::{classical_assignment_latex, kripke_assignment_latex, kripke_frame_latex};
use self::nj::nj_latex;
use self::prop::prop_latex;
use self::split::split_proof;
use crate::naming::{promote_kripke, promote_nj};
use crate::parsing::{ParseError, Prop as PropAst};
use crate::prop::Env;
use crate::result::{SolverResult, SolverResultPair};
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
    cl_result: SolverResult<Vec<ProofFragment>, ClassicalRefutation>,
    #[allow(dead_code)]
    int_result: SolverResult<Vec<ProofFragment>, Refutation>,
}

#[derive(Debug, Clone)]
struct ProofFragment {
    name: String,
    source: String,
}

#[derive(Debug, Clone)]
struct ClassicalRefutation {
    assignment: String,
}

#[derive(Debug, Clone)]
struct Refutation {
    frame: String,
    assignment: String,
}

pub fn success_latex(prop: &PropAst, res: &SolverResultPair, env: &Env) -> String {
    let cl_result = match &res.cl {
        SolverResult::NotProvable(rft) => SolverResult::NotProvable(rft.as_ref().map(|rft| {
            let rft = promote_kripke(rft, env);
            ClassicalRefutation {
                assignment: classical_assignment_latex(&rft),
            }
        })),
        SolverResult::Unknown => SolverResult::Unknown,
        SolverResult::Provable(pf) => SolverResult::Provable(pf.as_ref().map(|pf| {
            let pf = promote_nj(pf, env);

            proof_fragments(&pf)
        })),
    };
    let int_result = match &res.int {
        SolverResult::NotProvable(rft) => SolverResult::NotProvable(rft.as_ref().map(|rft| {
            let rft = promote_kripke(rft, env);
            Refutation {
                frame: kripke_frame_latex(&rft),
                assignment: kripke_assignment_latex(&rft),
            }
        })),
        SolverResult::Unknown => SolverResult::Unknown,
        SolverResult::Provable(pf) => SolverResult::Provable(pf.as_ref().map(|pf| {
            let pf = promote_nj(pf, env);

            proof_fragments(&pf)
        })),
    };
    SuccessTemplate {
        prop: prop_latex(prop),
        cl_result,
        int_result,
    }
    .render()
    .unwrap()
}

fn proof_fragments(pf: &VisibleProof) -> Vec<ProofFragment> {
    split_proof(&pf)
        .into_iter()
        .enumerate()
        .map(|(i, pf)| {
            let name = if i == 0 {
                "Main proof".to_owned()
            } else {
                format!("Subproof {}", i)
            };
            ProofFragment {
                name,
                source: nj_latex(&pf),
            }
        })
        .collect::<Vec<_>>()
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
