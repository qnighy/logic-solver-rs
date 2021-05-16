use std::fmt;

use super::prop::write_prop_latex;
use crate::visible_proof::{RuleName, VisibleProof, VisibleProofKind};

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
        VisibleProofKind::Axiom(ref hyp_id) => {
            f.write_str("\\AxiomC{$[")?;
            write_prop_latex(&pf.prop, f)?;
            writeln!(f, "]_{{{}}}$}}", hyp_id.0)?;
        }
        VisibleProofKind::Open => {
            f.write_str("\\AxiomC{$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
        VisibleProofKind::SubProof {
            rule,
            ref introduces,
            ref subproofs,
        } => {
            for subproof in subproofs {
                write_nj_latex(subproof, f)?;
            }
            f.write_str("\\RightLabel{\\scriptsize$")?;
            write_rule_latex(rule, f)?;
            if let Some(ref introduces) = *introduces {
                write!(f, " ({})", introduces.0)?;
            }
            f.write_str("$}\n")?;
            if subproofs.is_empty() {
                f.write_str("\\AxiomC{}\n")?;
                f.write_str("\\UnaryInfC{$")?;
            } else {
                // TODO: more than 5 branches
                let cmd = NARY[subproofs.len()];
                f.write_str("\\")?;
                f.write_str(cmd)?;
                f.write_str("{$")?;
            }
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
        VisibleProofKind::SplitRef(split_proof_id) => {
            writeln!(f, "\\AxiomC{{sub {}}}", split_proof_id.0)?;
            writeln!(f, "\\noLine")?;
            writeln!(f, "\\UnaryInfC{{$\\vdots$}}")?;
            writeln!(f, "\\noLine")?;
            f.write_str("\\UnaryInfC{$")?;
            write_prop_latex(&pf.prop, f)?;
            f.write_str("$}\n")?;
        }
    }
    Ok(())
}

pub(crate) fn write_rule_latex(rule: RuleName, f: &mut fmt::Formatter) -> fmt::Result {
    match rule {
        RuleName::ImplIntro => f.write_str("{\\to}_I")?,
        RuleName::ImplElim => f.write_str("{\\to}_E")?,
        RuleName::ConjIntro(0) => f.write_str("{\\top}_I")?,
        RuleName::ConjIntro(_) => f.write_str("{\\wedge}_I")?,
        RuleName::ConjElim(i, _) => {
            f.write_str("{\\wedge}_{E ")?;
            write!(f, "{}", i + 1)?;
            f.write_str("}")?;
        }
        RuleName::DisjIntro(i, _) => {
            f.write_str("{\\vee}_{I ")?;
            write!(f, "{}", i + 1)?;
            f.write_str("}")?;
        }
        RuleName::DisjElim(0) => f.write_str("{\\bot}_E")?,
        RuleName::DisjElim(_) => f.write_str("{\\vee}_E")?,
        RuleName::DNegElim => f.write_str("\\lnot\\lnot_E")?,
    }
    Ok(())
}
