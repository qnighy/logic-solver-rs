use std::ops::Range;

use crate::parsing::Prop as PropAst;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VisibleProof {
    pub node: VisibleProofNode,
    pub kind: VisibleProofKind,
    pub split_here: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VisibleProofNode {
    Prop(PropAst),
    BranchRange(Range<usize>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VisibleProofKind {
    Open,
    Axiom(HypothesisId),
    SubProof {
        rule: RuleName,
        introduces: Option<HypothesisId>,
        subproofs: Vec<VisibleProof>,
    },
    SplitRef(SplitProofId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HypothesisId(pub u32);

#[derive(Debug, Clone, Default)]
pub struct HypothesisGen(u32);

impl HypothesisGen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fresh(&mut self) -> HypothesisId {
        self.0 += 1;
        HypothesisId(self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SplitProofId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleName {
    None,
    ImplIntro,
    ImplElim,
    ConjIntro(usize),
    ConjElim(usize, usize),
    DisjIntro(usize, usize),
    DisjElim(usize),
    DNegElim,
}
