use crate::parsing::Prop as PropAst;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VisibleProof {
    pub prop: PropAst,
    pub kind: VisibleProofKind,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HypothesisId(pub u32);

#[derive(Debug, Clone)]
pub struct HypothesisGen(u32);

impl HypothesisGen {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn fresh(&mut self) -> HypothesisId {
        self.0 += 1;
        HypothesisId(self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleName {
    ImplIntro,
    ImplElim,
    ConjIntro(usize),
    ConjElim(usize, usize),
    DisjIntro(usize, usize),
    DisjElim(usize),
}
