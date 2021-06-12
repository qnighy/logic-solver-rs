use serde::{Deserialize, Serialize};
use std::io::{self, BufRead, Write};
use std::sync::Mutex;

use crate::ipc::{solve_cpc, solve_res, try_refute_res};
use crate::kripke::KripkeRefutation;
use crate::nj::Proof;
use crate::prop::Prop;
use crate::result::{ReporterPair, SolverResult};

pub trait Commander {
    fn solve_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop);
    fn refute_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop);
    fn solve_cl(&self, reporter: &mut dyn ReporterPair, prop: &Prop);
    fn refute_cl(&self, reporter: &mut dyn ReporterPair, prop: &Prop);
}

pub struct InProcessCommander;

impl Commander for InProcessCommander {
    fn solve_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        reporter.update_int(solve_res(prop));
    }

    fn refute_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        reporter.update_int(try_refute_res(prop));
    }

    fn solve_cl(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        if let Some(pf) = solve_cpc(prop) {
            reporter.update_cl(SolverResult::Provable(Some(pf)));
        }
    }

    fn refute_cl(&self, _reporter: &mut dyn ReporterPair, _prop: &Prop) {
        todo!();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Command {
    SolveInt(Prop),
    RefuteInt(Prop),
    SolveCl(Prop),
    RefuteCl(Prop),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Response {
    UpdateInt(SolverResult<Proof, KripkeRefutation>),
    UpdateCl(SolverResult<Proof, KripkeRefutation>),
    Done,
}

#[derive(Debug)]
pub struct RpcClient<'a> {
    inner: Mutex<RpcClientInner<'a>>,
}

struct RpcClientInner<'a> {
    r: &'a mut dyn BufRead,
    w: &'a mut dyn Write,
}

impl std::fmt::Debug for RpcClientInner<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        struct Placeholder;
        impl std::fmt::Debug for Placeholder {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("<obj>")
            }
        }
        f.debug_struct("RpcClient")
            .field("r", &Placeholder)
            .field("w", &Placeholder)
            .finish()
    }
}

impl RpcClient<'_> {
    fn command(&self, reporter: &mut dyn ReporterPair, command: Command) -> io::Result<()> {
        let mut inner = self.inner.lock().unwrap();
        serde_json::to_writer(&mut *inner.w, &command)?;
        let mut line = String::new();
        loop {
            line.clear();
            inner.r.read_line(&mut line)?;
            let resp: Response = serde_json::from_str(&line)?;
            match resp {
                Response::Done => break,
                Response::UpdateInt(res) => reporter.update_int(res),
                Response::UpdateCl(res) => reporter.update_cl(res),
            }
        }
        Ok(())
    }
}

impl Commander for RpcClient<'_> {
    fn solve_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        self.command(reporter, Command::SolveInt(prop.clone()))
            .unwrap();
    }

    fn refute_int(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        self.command(reporter, Command::RefuteInt(prop.clone()))
            .unwrap();
    }

    fn solve_cl(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        self.command(reporter, Command::SolveCl(prop.clone()))
            .unwrap();
    }

    fn refute_cl(&self, reporter: &mut dyn ReporterPair, prop: &Prop) {
        self.command(reporter, Command::RefuteCl(prop.clone()))
            .unwrap();
    }
}

pub fn rpc_server(
    r: &mut dyn BufRead,
    w: &mut dyn Write,
    commander: &dyn Commander,
) -> io::Result<()> {
    let mut reporter = RpcReporter { w };
    let mut line = String::new();
    loop {
        line.clear();
        let len = r.read_line(&mut line)?;
        if len == 0 {
            break;
        }
        let command: Command = serde_json::from_str(&line)?;
        match &command {
            Command::SolveInt(prop) => commander.solve_int(&mut reporter, prop),
            Command::RefuteInt(prop) => commander.refute_int(&mut reporter, prop),
            Command::SolveCl(prop) => commander.solve_cl(&mut reporter, prop),
            Command::RefuteCl(prop) => commander.refute_cl(&mut reporter, prop),
        }
        reporter.done()?;
    }
    Ok(())
}

struct RpcReporter<'a> {
    w: &'a mut dyn Write,
}

impl RpcReporter<'_> {
    fn done(&mut self) -> io::Result<()> {
        serde_json::to_writer(&mut *self.w, &Response::Done)?;
        Ok(())
    }
}

impl ReporterPair for RpcReporter<'_> {
    fn update_int(&mut self, res: SolverResult<Proof, KripkeRefutation>) {
        // TODO: handle I/O errors
        serde_json::to_writer(&mut *self.w, &Response::UpdateInt(res)).unwrap();
    }

    fn update_cl(&mut self, res: SolverResult<Proof, KripkeRefutation>) {
        // TODO: handle I/O errors
        serde_json::to_writer(&mut *self.w, &Response::UpdateInt(res)).unwrap();
    }
}
