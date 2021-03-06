use serde::{Deserialize, Serialize};
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Relaxed;
use std::sync::Arc;

use crate::kripke::KripkeRefutation;
use crate::nj::Proof;

#[derive(Debug, Clone, Default)]
pub struct Cancellation {
    cancelled: Arc<AtomicBool>,
}

impl Cancellation {
    pub fn new() -> Self {
        Self {
            cancelled: Arc::new(AtomicBool::new(false)),
        }
    }
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Relaxed)
    }
    pub fn cancel(&self) {
        self.cancelled.store(true, Relaxed);
    }
}

pub trait Reporter {
    fn update(&mut self, res: SolverResult<Proof, KripkeRefutation>);
}

pub trait ReporterPair {
    fn update_int(&mut self, res: SolverResult<Proof, KripkeRefutation>);
    fn update_cl(&mut self, res: SolverResult<Proof, KripkeRefutation>);
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SolverResultPair {
    // Classical logic result
    pub cl: SolverResult<Proof, KripkeRefutation>,
    // Intuitionistic logic result
    pub int: SolverResult<Proof, KripkeRefutation>,
}

impl SolverResultPair {
    pub fn update_int(&mut self, other_int: SolverResult<Proof, KripkeRefutation>) {
        self.int.update(other_int);
        if matches!(&self.int, SolverResult::Provable(_)) {
            self.cl.update(SolverResult::Provable(None));
        } else if let SolverResult::NotProvable(Some(rf)) = &self.int {
            if rf.num_worlds > 1 {
                self.cl.update(SolverResult::Provable(None));
            } else {
                self.cl.update(SolverResult::NotProvable(Some(rf.clone())));
            }
        }
    }

    pub fn update_cl(&mut self, other_cl: SolverResult<Proof, KripkeRefutation>) {
        self.cl.update(other_cl);
        if matches!(&self.cl, SolverResult::NotProvable(_)) {
            self.int.update(SolverResult::NotProvable(None));
        }
    }

    pub fn reduce_duplicate_details(&mut self) {
        if matches!(&self.int, SolverResult::Provable(Some(_)))
            && matches!(&self.cl, SolverResult::Provable(Some(_)))
        {
            self.cl = SolverResult::Provable(None);
        }
        if matches!(&self.int, SolverResult::NotProvable(Some(_)))
            && matches!(&self.cl, SolverResult::NotProvable(Some(_)))
        {
            self.int = SolverResult::NotProvable(None);
        }
    }

    pub fn classical_proof_needed(&self) -> bool {
        !matches!(&self.int, SolverResult::Provable(Some(_)))
            && matches!(&self.cl, SolverResult::Provable(None))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Void {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SolverResult<P, R> {
    NotProvable(Option<R>),
    Unknown,
    Provable(Option<P>),
}

impl<P, R> SolverResult<P, R> {
    pub fn is_not_provable(&self) -> bool {
        matches!(self, SolverResult::NotProvable(_))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, SolverResult::Unknown)
    }

    pub fn is_provable(&self) -> bool {
        matches!(self, SolverResult::Provable(_))
    }

    pub fn update(&mut self, other: Self) {
        use SolverResult::*;

        match (&*self, &other) {
            (NotProvable(_), Provable(_)) | (Provable(_), NotProvable(_)) => {
                panic!("Contradicting result")
            }
            (Provable(_), Provable(None)) | (NotProvable(_), NotProvable(None)) | (_, Unknown) => {
                return;
            }
            (_, _) => {}
        };
        *self = other;
    }
}

impl<P, R> Default for SolverResult<P, R> {
    fn default() -> Self {
        SolverResult::Unknown
    }
}
