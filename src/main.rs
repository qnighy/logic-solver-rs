use std::io::{self, Read};
use structopt::StructOpt;
use thiserror::Error;

use crate::ipc::solve;
use crate::parsing::{parse_prop, ParseError};
use crate::prop::{Env, IdGen, Prop};

pub mod debruijn;
pub mod ipc;
pub mod nj;
pub mod parsing;
pub mod prop;
pub mod rollback;
#[cfg(test)]
mod tests;

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
    let ast = parse_prop(&expr)?;
    let prop = Prop::from_ast(&mut idgen, &mut env, &ast);
    let provable = solve(&prop).is_some();
    if provable {
        println!("Provable");
    } else {
        println!("Not provable");
    }

    Ok(())
}
