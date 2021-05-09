use std::fmt;

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
        "\\begin{tikzpicture}[layered layout, level distance=2cm, sibling distance=2cm, nodes={{circle, draw}}, acc/.style={{decorate,decoration=snake,arrow head=5mm}}]\n",
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
