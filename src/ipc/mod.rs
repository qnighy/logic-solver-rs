pub mod icnf;
pub mod icnf_decomp;
pub mod icnf_refutation;
pub mod icnf_solver;
pub mod refutation;
pub mod solver;

pub use refutation::{try_refute, try_refute_res};
pub use solver::{solve, solve_res};
