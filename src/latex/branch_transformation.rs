use std::ops::Range;

use crate::parsing::Prop as PropAst;
use crate::visible_proof::{RuleName, VisibleProof, VisibleProofKind, VisibleProofNode};

const MAX_BRANCHES: usize = 5;
const SENTINEL: VisibleProof = VisibleProof {
    node: VisibleProofNode::Prop(PropAst::Atom(String::new())),
    kind: VisibleProofKind::Open,
    split_here: false,
};

pub(super) fn transform_branches(pf: &mut VisibleProof) {
    match &mut pf.kind {
        VisibleProofKind::Open | VisibleProofKind::Axiom(_) | VisibleProofKind::SplitRef(_) => {}
        VisibleProofKind::SubProof {
            rule: _,
            introduces: _,
            subproofs,
        } => {
            for subproof in subproofs.iter_mut() {
                transform_branches(subproof);
            }

            let mut level = 0;
            let mut max_total_branches = MAX_BRANCHES;
            while subproofs.len() > max_total_branches {
                level += 1;
                max_total_branches *= MAX_BRANCHES;
            }
            if level > 0 {
                let mut sp_copy = std::mem::replace(subproofs, Vec::new());
                let range = 0..sp_copy.len();
                let sp_new = split_rec(&mut sp_copy, level, max_total_branches, range);
                *subproofs = sp_new;
            }
        }
    }
}

fn split_rec(
    branches: &mut Vec<VisibleProof>,
    level: u32,
    max_total_branches: usize,
    range: Range<usize>,
) -> Vec<VisibleProof> {
    if level == 0 {
        return branches[range]
            .iter_mut()
            .map(|elem| std::mem::replace(elem, SENTINEL))
            .collect();
    }
    let len = range.end - range.start;
    let max_each = max_total_branches / MAX_BRANCHES;
    let split_this_level = (len + max_each - 1) / max_each;
    let mut new_branches = Vec::new();
    for i in 0..split_this_level {
        let relstart = i * len / split_this_level;
        let relend = (i + 1) * len / split_this_level;
        let subrange = range.start + relstart..range.start + relend;
        let subbranches = split_rec(branches, level - 1, max_each, subrange.clone());
        new_branches.push(VisibleProof {
            node: VisibleProofNode::BranchRange(subrange),
            kind: VisibleProofKind::SubProof {
                rule: RuleName::None,
                introduces: None,
                subproofs: subbranches,
            },
            split_here: false,
        });
    }
    new_branches
}
