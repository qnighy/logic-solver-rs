use super::icnf::VarGen;
use super::icnf_decomp::Decomposition;
use super::icnf_refutation::try_refute_icnf;
use crate::kripke::KripkeRefutation;
use crate::prop::Prop;

pub fn try_refute(prop: &Prop) -> Option<KripkeRefutation> {
    let mut vargen = VarGen::new();
    let (icnf, decomp) = Decomposition::decompose(&mut vargen, prop);
    let icnf_rft = try_refute_icnf(&vargen, &icnf)?;
    let rft = decomp.convert_kripke(&icnf_rft);
    Some(rft)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prop::{IdGen, PropShorthands};
    use maplit::hashmap;

    #[test]
    fn test_try_refute1() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let prop = Atom(id1);
        let rft = try_refute(&prop).unwrap();
        assert_eq!(
            rft,
            KripkeRefutation {
                num_worlds: 1,
                accessibility: vec![vec![0]],
                subprops: vec![Atom(id1)],
                valuation: hashmap![
                    Atom(id1) => vec![false],
                ],
            }
        );
    }

    #[test]
    fn test_try_refute2() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let prop = ImplS(a(), a());
        assert!(try_refute(&prop).is_none());
    }

    #[test]
    fn test_try_refute3() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let id2 = idgen.fresh();
        let a = || Atom(id1);
        let b = || Atom(id2);
        let prop = ImplS(a(), ImplS(b(), a()));
        assert!(try_refute(&prop).is_none());
    }

    #[test]
    fn test_try_refute4() {
        use PropShorthands::*;

        let mut idgen = IdGen::new();
        let id1 = idgen.fresh();
        let a = || Atom(id1);
        let prop = Disj(vec![a(), NegS(a())]);
        let rft = try_refute(&prop).unwrap();
        assert_eq!(
            rft,
            KripkeRefutation {
                num_worlds: 2,
                accessibility: vec![vec![0, 1], vec![1]],
                subprops: vec![a(), Disj(vec![]), NegS(a()), Disj(vec![a(), NegS(a())])],
                valuation: hashmap![
                    a() => vec![false, true],
                    Disj(vec![]) => vec![false, false],
                    NegS(a()) => vec![false, false],
                    Disj(vec![a(), NegS(a())]) => vec![false, true],
                ],
            }
        );
    }
}
