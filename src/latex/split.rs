use crate::parsing::Prop as PropAst;
use crate::visible_proof::{SplitProofId, VisibleProof, VisibleProofKind, VisibleProofNode};

const LIMIT: u32 = 30;
const SENTINEL: VisibleProof = VisibleProof {
    node: VisibleProofNode::Prop(PropAst::Atom(String::new())),
    kind: VisibleProofKind::Open,
    split_here: false,
};

pub(super) fn split_proof(pf: &VisibleProof) -> Vec<VisibleProof> {
    let mut pf = pf.clone();
    mark_split(&mut pf);
    let mut pfs = vec![SENTINEL];
    split_rec(&mut pf, &mut pfs);
    pfs[0] = pf;
    pfs
}

fn split_rec(pf: &mut VisibleProof, pfs: &mut Vec<VisibleProof>) {
    if pf.split_here {
        pf.split_here = false;
        let node = pf.node.clone();
        let pf_id = SplitProofId(pfs.len());
        let mut pf = std::mem::replace(
            pf,
            VisibleProof {
                node,
                kind: VisibleProofKind::SplitRef(pf_id),
                split_here: false,
            },
        );
        pfs.push(SENTINEL);
        split_rec(&mut pf, pfs);
        pfs[pf_id.0] = pf;
        return;
    }
    match pf.kind {
        VisibleProofKind::Open | VisibleProofKind::Axiom(_) => {}
        VisibleProofKind::SubProof {
            ref mut subproofs, ..
        } => {
            for subproof in subproofs {
                split_rec(subproof, pfs);
            }
        }
        VisibleProofKind::SplitRef(_) => unreachable!("this tree is already split"),
    }
}

fn mark_split(pf: &mut VisibleProof) -> (u32, u32) {
    pf.split_here = false;
    let self_weight = node_weight(&pf.node) + 1;
    let subproof_weight = match pf.kind {
        VisibleProofKind::Open | VisibleProofKind::Axiom(_) => 0,
        VisibleProofKind::SubProof { ref subproofs, .. } if subproofs.is_empty() => 0,
        VisibleProofKind::SubProof {
            ref mut subproofs, ..
        } => {
            let weights = subproofs
                .iter_mut()
                .map(|subproof| mark_split(subproof))
                .collect::<Vec<_>>();
            let mut weight = weights.iter().map(|&(_, w)| w).sum::<u32>();
            if weight > LIMIT {
                let mut indices = (0..subproofs.len()).collect::<Vec<_>>();
                indices.sort_by_key(|&i| weights[i].1 - weights[i].0);
                while weight > LIMIT && !indices.is_empty() {
                    let i = indices.pop().unwrap();
                    let diff = weights[i].1 - weights[i].0;
                    if diff == 0 {
                        break;
                    }
                    weight -= diff;
                    subproofs[i].split_here = true;
                }
            }
            weight
        }
        VisibleProofKind::SplitRef(_) => unreachable!("this tree is already split"),
    };
    (self_weight, std::cmp::max(self_weight, subproof_weight))
}

fn node_weight(node: &VisibleProofNode) -> u32 {
    match node {
        VisibleProofNode::Prop(prop) => prop_weight(prop),
    }
}

fn prop_weight(prop: &PropAst) -> u32 {
    match *prop {
        PropAst::Atom(ref name) => name.chars().count() as u32,
        PropAst::Neg(ref sub) => prop_weight(sub) + 1,
        PropAst::Impl(ref lhs, ref rhs) | PropAst::Equiv(ref lhs, ref rhs) => {
            prop_weight(lhs) + prop_weight(rhs) + 1
        }
        PropAst::Conj(ref children) | PropAst::Disj(ref children) if children.is_empty() => 1,
        PropAst::Conj(ref children) | PropAst::Disj(ref children) => {
            children.iter().map(|child| prop_weight(child)).sum::<u32>()
                + (children.len() as u32 - 1)
        }
    }
}
