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
        // name: RuleName
        subproofs: Vec<VisibleProof>,
    },
}
