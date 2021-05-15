use crate::kripke::KripkeRefutation;
use crate::nj::Proof;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SolverResultPair {
    // Classical logic result
    pub cl: SolverResult<Void, KripkeRefutation>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Void {}

#[derive(Debug, Clone, PartialEq, Eq)]
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
