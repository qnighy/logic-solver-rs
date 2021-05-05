use crate::parsing::Prop as PropAst;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VisibleProof {
    pub prop: PropAst,
    pub kind: VisibleProofKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VisibleProofKind {
    Open,
    Axiom(String),
    SubProof {
        rule: RuleName,
        introduces: Option<String>,
        subproofs: Vec<VisibleProof>,
    },
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
