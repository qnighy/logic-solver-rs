use std::fmt;

use super::prop::write_prop_latex;
use crate::visible_proof::{VisibleProof, VisibleProofKind};

pub(super) fn nj_latex(pf: &VisibleProof) -> String {
    struct D<'a>(&'a VisibleProof);
    impl fmt::Display for D<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_nj_latex(self.0, f)
        }
    }
    D(pf).to_string()
}

const NARY: &[&str] = &[
    "",
    "UnaryInfC",
    "BinaryInfC",
    "TrinaryInfC",
    "QuaternaryInfC",
    "QuinaryInfC",
];

pub(crate) fn write_nj_latex(pf: &VisibleProof, f: &mut fmt::Formatter) -> fmt::Result {
    match pf.kind {
        VisibleProofKind::Axiom(_) => {
            f.write_str("\\AxiomC{[$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$]}\n")?;
        }
        VisibleProofKind::Open => {
            f.write_str("\\AxiomC{$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
        VisibleProofKind::SubProof { ref subproofs } if subproofs.len() == 0 => {
            f.write_str("\\AxiomC{}\n")?;
            f.write_str("\\UnaryInfC{$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
        VisibleProofKind::SubProof { ref subproofs } => {
            for subproof in subproofs {
                write_nj_latex(subproof, f)?;
            }
            // TODO: more than 5 branches
            let cmd = NARY[subproofs.len()];
            f.write_str("\\")?;
            f.write_str(cmd)?;
            f.write_str("{$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
    }
    Ok(())
}
