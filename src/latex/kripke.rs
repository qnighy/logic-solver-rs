use std::fmt;

use super::prop::write_prop_latex;
use crate::kripke::VisibleKripkeRefutation;

pub(super) fn kripke_frame_latex(rft: &VisibleKripkeRefutation) -> String {
    struct D<'a>(&'a VisibleKripkeRefutation);
    impl fmt::Display for D<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_kripke_frame_latex(self.0, f)
        }
    }
    D(rft).to_string()
}

pub(super) fn kripke_assignment_latex(rft: &VisibleKripkeRefutation) -> String {
    struct D<'a>(&'a VisibleKripkeRefutation);
    impl fmt::Display for D<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_kripke_assignment_latex(self.0, f)
        }
    }
    D(rft).to_string()
}

pub(super) fn classical_assignment_latex(rft: &VisibleKripkeRefutation) -> String {
    struct D<'a>(&'a VisibleKripkeRefutation);
    impl fmt::Display for D<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_classical_assignment_latex(self.0, f)
        }
    }
    D(rft).to_string()
}

fn write_kripke_frame_latex(rft: &VisibleKripkeRefutation, f: &mut fmt::Formatter) -> fmt::Result {
    let mut hasse = rft.accessibility.clone();
    for (i, nexts) in rft.accessibility.iter().enumerate() {
        hasse[i].retain(|&x| x != i);
        for &j in nexts {
            for &k in &rft.accessibility[j] {
                if i < j && j < k {
                    hasse[i].retain(|&x| x != k);
                }
            }
        }
    }
    f.write_str(
        "\\begin{tikzpicture}[layered layout, level distance=2cm, sibling distance=2cm, nodes={{circle, draw}}, acc/.style={{decorate,decoration=snake}}]\n",
    )?;
    for i in 0..hasse.len() {
        writeln!(f, "\\node (w{}) {{$W_{}$}};", i, i)?;
    }
    for (i, nexts) in hasse.iter().enumerate() {
        for &j in nexts {
            writeln!(f, "\\draw (w{}) edge[->,acc] (w{});", i, j)?;
        }
    }
    f.write_str("\\end{tikzpicture}\n")?;
    Ok(())
}

fn write_kripke_assignment_latex(
    rft: &VisibleKripkeRefutation,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    write!(f, "\\begin{{tabular}}{{r|")?;
    for _ in 0..rft.num_worlds {
        write!(f, "c")?;
    }
    writeln!(f, "}}")?;
    for i in 0..rft.num_worlds {
        write!(f, "& $W_{}$", i)?;
    }
    writeln!(f)?;
    writeln!(f, "\\\\\\hline\\hline")?;
    let mut init = true;
    for (prop, val) in &rft.valuation {
        if init {
            init = false;
        } else {
            writeln!(f, "\\\\")?;
        }
        write_prop_latex(prop, f)?;
        for &v in val {
            if v {
                write!(f, " & 1")?;
            } else {
                write!(f, " & 0")?;
            }
        }
        writeln!(f)?;
    }
    writeln!(f, "\\end{{tabular}}")?;
    Ok(())
}

fn write_classical_assignment_latex(
    rft: &VisibleKripkeRefutation,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    assert_eq!(rft.num_worlds, 1);
    writeln!(f, "\\begin{{tabular}}{{r|c}}")?;
    let mut init = true;
    for (prop, val) in &rft.valuation {
        if init {
            init = false;
        } else {
            writeln!(f, "\\\\")?;
        }
        write_prop_latex(prop, f)?;
        if val[0] {
            writeln!(f, " & 1")?;
        } else {
            writeln!(f, " & 0")?;
        }
    }
    writeln!(f, "\\end{{tabular}}")?;
    Ok(())
}
