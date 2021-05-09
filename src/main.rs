use std::io::{self, Read, Write};
use structopt::StructOpt;
use thiserror::Error;

use crate::ipc::{solve_res, try_refute_res};
use crate::latex::{parse_error_latex, success_latex};
use crate::naming::lower_prop;
use crate::parsing::{parse_prop, ParseError};
use crate::prop::{Env, IdGen};
use crate::result::SolverResultPair;

pub mod debruijn;
pub mod ipc;
pub mod kripke;
pub mod latex;
pub mod naming;
pub mod nj;
pub mod parsing;
pub mod prop;
pub mod result;
pub mod rollback;
#[cfg(test)]
mod tests;
pub mod visible_proof;

#[derive(Debug, Error)]
enum LogicSolverError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("{0}")]
    Parse(#[from] ParseError),
}

/// A basic example
#[derive(StructOpt, Debug)]
#[structopt(name = "logic-solver")]
struct Opt {
    #[structopt(short, long)]
    expr: Option<String>,

    #[structopt(long)]
    latex: bool,
}

fn main() {
    let result = main2();
    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn main2() -> Result<(), LogicSolverError> {
    let opt = Opt::from_args();
    let expr = if let Some(ref e) = opt.expr {
        e.clone()
    } else {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        let mut s = String::new();
        stdin.read_to_string(&mut s)?;
        s
    };

    let mut idgen = IdGen::new();
    let mut env = Env::new();

    let ast = match parse_prop(&expr) {
        Ok(x) => x,
        Err(e) if opt.latex => {
            let latex_src = parse_error_latex(&expr, e);
            let stdout = io::stdout();
            let mut stdout = stdout.lock();
            stdout.write_all(latex_src.as_bytes())?;
            return Ok(());
        }
        Err(e) => {
            return Err(e.into());
        }
    };
    let prop = lower_prop(&mut idgen, &mut env, &ast);
    let mut res = SolverResultPair::default();
    res.update_int(solve_res(&prop));
    if !res.int.is_provable() && opt.latex {
        res.update_int(try_refute_res(&prop));
    }
    if opt.latex {
        let latex_src = success_latex(&ast, &res, &env);
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        stdout.write_all(latex_src.as_bytes())?;
        return Ok(());
    }

    if res.int.is_provable() {
        println!("Provable");
    } else if res.int.is_not_provable() {
        println!("Not provable");
    } else {
        println!("Unknown");
    }

    Ok(())
}
