use std::fmt;

use crate::parsing::Prop as PropAst;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Precedence {
    Primary,
    Conj,
    Disj,
    Impl,
}

impl<'a> From<&'a PropAst> for Precedence {
    fn from(prop: &'a PropAst) -> Self {
        use Precedence::*;

        match *prop {
            PropAst::Atom(_) => Primary,
            PropAst::Impl(_, _) => Impl,
            PropAst::Conj(ref children) if children.len() == 0 => Primary,
            PropAst::Conj(_) => Conj,
            PropAst::Disj(ref children) if children.len() == 0 => Primary,
            PropAst::Disj(_) => Disj,
            PropAst::Equiv(_, _) => todo!(),
            PropAst::Neg(_) => todo!(),
        }
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        use Precedence::*;

        match (*self, *other) {
            (Primary, Primary) | (Conj, Conj) | (Disj, Disj) | (Impl, Impl) => Some(Equal),
            (Primary, Conj) | (Primary, Disj) | (Conj, Impl) | (Disj, Impl) | (Primary, Impl) => {
                Some(Less)
            }
            (Conj, Primary) | (Disj, Primary) | (Impl, Conj) | (Impl, Disj) | (Impl, Primary) => {
                Some(Greater)
            }
            _ => None,
        }
    }
}

pub(super) fn prop_latex(prop: &PropAst) -> String {
    struct D<'a>(&'a PropAst);
    impl fmt::Display for D<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_prop_latex(self.0, f)
        }
    }
    D(prop).to_string()
}

pub(super) fn write_prop_latex(prop: &PropAst, f: &mut fmt::Formatter) -> fmt::Result {
    write_prop_latex_prec(prop, Precedence::Impl, true, f)
}

fn write_prop_latex_prec(
    prop: &PropAst,
    pprec: Precedence,
    allow_same: bool,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    let prec = Precedence::from(prop);
    if !(prec < pprec || prec <= pprec && allow_same) {
        f.write_str("(")?;
    }
    match *prop {
        PropAst::Atom(ref name) => f.write_str(name)?,
        PropAst::Impl(ref lhs, ref rhs) => {
            write_prop_latex_prec(lhs, prec, false, f)?;
            f.write_str(" \\to ")?;
            write_prop_latex_prec(rhs, prec, true, f)?;
        }
        PropAst::Conj(ref children) if children.len() == 0 => {
            f.write_str("\\top")?;
        }
        PropAst::Conj(ref children) => {
            for (i, child) in children.iter().enumerate() {
                if i > 0 {
                    f.write_str(" \\wedge ")?;
                }
                write_prop_latex_prec(child, prec, false, f)?;
            }
        }
        PropAst::Disj(ref children) if children.len() == 0 => {
            f.write_str("\\bot")?;
        }
        PropAst::Disj(ref children) => {
            for (i, child) in children.iter().enumerate() {
                if i > 0 {
                    f.write_str(" \\vee ")?;
                }
                write_prop_latex_prec(child, prec, false, f)?;
            }
        }
        PropAst::Equiv(_, _) => todo!(),
        PropAst::Neg(_) => todo!(),
    }
    if !(prec < pprec || prec <= pprec && allow_same) {
        f.write_str(")")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::parse_prop;

    #[test]
    fn test_prop_latex1() {
        let prop = parse_prop("A").unwrap();
        let s = prop_latex(&prop);
        assert_eq!(s, "A");
    }

    #[test]
    fn test_prop_latex2() {
        let prop = parse_prop("A → A").unwrap();
        let s = prop_latex(&prop);
        assert_eq!(s, "A \\to A");
    }

    #[test]
    fn test_prop_latex3() {
        let prop = parse_prop("(A ∧ B ∧ C) ∧ (B ∨ C) → A ∧ B").unwrap();
        let s = prop_latex(&prop);
        assert_eq!(
            s,
            "(A \\wedge B \\wedge C) \\wedge (B \\vee C) \\to A \\wedge B"
        );
    }

    #[test]
    fn test_prec_ordering_sanity() {
        use Precedence::*;
        const PRECS: &[Precedence] = &[Primary, Conj, Disj, Impl];
        for &p1 in PRECS {
            for &p2 in PRECS {
                assert_eq!(
                    p1.partial_cmp(&p2).map(|o| o.reverse()),
                    p2.partial_cmp(&p1)
                );
                assert_eq!(
                    p1 == p2,
                    p1.partial_cmp(&p2) == Some(std::cmp::Ordering::Equal)
                );
            }
        }
        for &p1 in PRECS {
            for &p2 in PRECS {
                for &p3 in PRECS {
                    if p1 <= p2 && p2 <= p3 {
                        assert!(p1 <= p3);
                    }
                }
            }
        }
    }
}
