use std::collections::{HashMap, HashSet};
use varisat::{ExtendFormula, Solver, Var as SatVar};

use crate::ipc::icnf::{Clause, Icnf, Var, VarGen};

const MAX_NUM_WORLDS: usize = 5;

pub fn try_refute_icnf(vargen: &VarGen, icnf: &Icnf) -> Option<Refutation> {
    for num_worlds in 1..=MAX_NUM_WORLDS {
        let mut solver = Solver::new();
        let mapping = register(vargen, icnf, num_worlds, &mut solver);
        let satisfiable = solver.solve().unwrap();
        if satisfiable {
            let model = solver.model().unwrap().into_iter().collect::<HashSet<_>>();
            eprintln!("model = {:?}", model);
            let mut accessibility = HashSet::new();
            for w0 in 0..num_worlds {
                accessibility.insert((w0, w0));
                for w1 in (w0 + 1)..num_worlds {
                    let rel = mapping.wrel[w0 * num_worlds + w1];
                    if model.contains(&rel.positive()) {
                        accessibility.insert((w0, w1));
                    }
                }
            }
            let mut valuation = HashMap::new();
            for i in 0..vargen.max_id() {
                let val = (0..num_worlds)
                    .map(|w| {
                        let sat_var = mapping.varmap[i * num_worlds + w];
                        model.contains(&sat_var.positive())
                    })
                    .collect::<Vec<_>>();
                valuation.insert(Var(i), val);
            }
            return Some(Refutation {
                num_worlds,
                accessibility,
                valuation,
            });
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RefutationMapping {
    wrel: Vec<SatVar>,
    varmap: Vec<SatVar>,
}

fn register<S: ExtendFormula>(
    vargen: &VarGen,
    icnf: &Icnf,
    num_worlds: usize,
    s: &mut S,
) -> RefutationMapping {
    // accessibility relation
    let mut wrel = vec![SatVar::from_index(0); num_worlds * num_worlds];
    // By symmetry we can assume the accessibility relation is a subrelation of the natural total order.
    // We also assume antisymmetry.
    for i in 0..num_worlds {
        for j in (i + 1)..num_worlds {
            wrel[i * num_worlds + j] = s.new_var();
        }
    }
    // Transitivity
    for i in 0..num_worlds {
        for j in (i + 1)..num_worlds {
            for k in (j + 1)..num_worlds {
                let ij = wrel[i * num_worlds + j];
                let jk = wrel[j * num_worlds + k];
                let ik = wrel[i * num_worlds + k];
                s.add_clause(&[ij.negative(), jk.negative(), ik.positive()]);
            }
        }
    }

    // Valuations
    let mut varmap = vec![SatVar::from_index(0); num_worlds * vargen.max_id()];
    for vi in 0..vargen.max_id() {
        for i in 0..num_worlds {
            varmap[vi * num_worlds + i] = s.new_var();
        }
    }

    // Persistency condition for each variable
    for vi in 0..vargen.max_id() {
        for w0 in 0..num_worlds {
            for w1 in (w0 + 1)..num_worlds {
                let rel = wrel[w0 * num_worlds + w1];
                let vw0 = varmap[vi * num_worlds + w0];
                let vw1 = varmap[vi * num_worlds + w1];
                s.add_clause(&[vw0.negative(), rel.negative(), vw1.positive()]);
            }
        }
    }

    let mut sat_cl = Vec::new();
    let mut impl_var_cache = Vec::new();
    // Encoding clauses
    for (_, cl) in icnf.ant.enumerate() {
        match cl {
            &Clause::Conj(ref lhs, rhs) => {
                for w in 0..num_worlds {
                    sat_cl.clear();
                    for &v in lhs {
                        let sat_v = varmap[v.0 * num_worlds + w];
                        sat_cl.push(sat_v.negative());
                    }
                    let sat_v = varmap[rhs.0 * num_worlds + w];
                    sat_cl.push(sat_v.positive());
                    s.add_clause(&sat_cl);
                }
            }
            &Clause::Disj(ref lhs, ref rhs) => {
                for w in 0..num_worlds {
                    sat_cl.clear();
                    for &v in lhs {
                        let sat_v = varmap[v.0 * num_worlds + w];
                        sat_cl.push(sat_v.negative());
                    }
                    for &v in rhs {
                        let sat_v = varmap[v.0 * num_worlds + w];
                        sat_cl.push(sat_v.positive());
                    }
                    s.add_clause(&sat_cl);
                }
            }
            &Clause::Impl(a, b, c) => {
                impl_var_cache.clear();
                for w0 in 0..num_worlds {
                    sat_cl.clear();
                    for w1 in w0..num_worlds {
                        let sat_var = s.new_var();
                        impl_var_cache.push(sat_var);
                        let sat_aw1 = varmap[a.0 * num_worlds + w1];
                        let sat_bw1 = varmap[b.0 * num_worlds + w1];
                        s.add_clause(&[sat_aw1.positive(), sat_var.positive()]);
                        s.add_clause(&[sat_bw1.negative(), sat_var.positive()]);
                        if w0 < w1 {
                            let rel = wrel[w0 * num_worlds + w1];
                            s.add_clause(&[rel.positive(), sat_var.positive()]);
                        }
                        sat_cl.push(sat_var.negative());
                    }
                    let sat_cw0 = varmap[c.0 * num_worlds + w0];
                    sat_cl.push(sat_cw0.positive());
                    s.add_clause(&sat_cl);
                }
            }
        }
    }

    {
        // Add goal refutation clause
        sat_cl.clear();
        for w in 0..num_worlds {
            let sat_var = varmap[icnf.suc.0 * num_worlds + w];
            sat_cl.push(sat_var.negative());
        }
        s.add_clause(&sat_cl);
    }
    RefutationMapping { wrel, varmap }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Refutation {
    num_worlds: usize,
    accessibility: HashSet<(usize, usize)>,
    valuation: HashMap<Var, Vec<bool>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::icnf::ClauseSet;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use maplit::hashset;
    use varisat::dimacs::write_dimacs;
    use varisat::CnfFormula;

    #[test]
    fn test_register1() {
        let mut s = CnfFormula::new();
        let mut vargen = VarGen::new();
        let a = vargen.fresh();
        let b = vargen.fresh();
        let ab = vargen.fresh();
        let ba = vargen.fresh();
        let abba = vargen.fresh();
        let icnf = Icnf {
            ant: ClauseSet {
                vec: vec![
                    Clause::Impl(a, b, ab),
                    Clause::Impl(b, a, ba),
                    Clause::Conj(vec![ab], abba),
                    Clause::Conj(vec![ba], abba),
                ],
            },
            suc: abba,
        };
        let mapping = register(&vargen, &icnf, 3, &mut s);
        let dimacs = {
            let mut dimacs = Vec::<u8>::new();
            write_dimacs(&mut dimacs, &s).unwrap();
            String::from_utf8(dimacs).unwrap()
        };
        assert_snapshot!(dimacs);
        assert_debug_snapshot!(mapping);
    }

    #[test]
    fn test_try_refute_icnf1() {
        let mut vargen = VarGen::new();
        let a = vargen.fresh();
        let b = vargen.fresh();
        let ab = vargen.fresh();
        let ba = vargen.fresh();
        let abba = vargen.fresh();
        let icnf = Icnf {
            ant: ClauseSet {
                vec: vec![
                    Clause::Impl(a, b, ab),
                    Clause::Impl(b, a, ba),
                    Clause::Conj(vec![ab], abba),
                    Clause::Conj(vec![ba], abba),
                ],
            },
            suc: abba,
        };
        let refutation = try_refute_icnf(&vargen, &icnf).unwrap();
        assert_eq!(refutation.num_worlds, 3);
        assert_eq!(
            refutation.accessibility,
            hashset![(0, 0), (0, 1), (0, 2), (1, 1), (2, 2)]
        );
        let aval = &refutation.valuation[&a];
        let bval = &refutation.valuation[&b];
        if aval == &[false, false, true] {
            assert_eq!(aval, &[false, false, true]);
            assert_eq!(bval, &[false, true, false]);
        } else {
            assert_eq!(aval, &[false, true, false]);
            assert_eq!(bval, &[false, false, true]);
        }
    }
}
