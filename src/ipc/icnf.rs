/// iCNF (implicational CNF), a modified version of CNF for intuitionistic logic.
///
/// Traditional CNF can be seen as the following sequent:
///
/// ```text
/// (a \/ ~b), (b \/ ~c), (c \/ ~d \/ e) |- false
/// ```
///
/// In this interpretation, CNF-SAT's goal is to **refute** the sequent.
///
/// We extend the "CNF" above in the following ways:
///
/// - Unlike classical CNF, there is a succedent (conclusion part of a sequent).
///   iCNF's succedent is always a variable.
/// - Negative literals are treated differently. Instead of `a \/ b \/ ~c \/ ~d`, we use `c /\ d -> a \/ b` as a clause.
///   Note that they're classically equivalent, but intuitionistically different.
/// - There is a special type of clause in the form of `(a -> b) -> c`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Icnf {
    /// Hypotheses
    pub ant: ClauseSet,
    /// Goal
    pub suc: Var,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Clause {
    /// (p1 & p2 & ... & pn) -> q
    Conj(Vec<Var>, Var),
    /// (p1 & p2 & ... & pn) -> (q1 | q2 | ... | qn)
    Disj(Vec<Var>, Vec<Var>),
    /// (p -> q) -> r
    Impl(Var, Var, Var),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub usize);

#[derive(Debug, Clone, Default)]
pub struct VarGen {
    next_id: usize,
}

impl VarGen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fresh(&mut self) -> Var {
        let var = Var(self.next_id);
        self.next_id += 1;
        var
    }

    pub fn max_id(&self) -> usize {
        self.next_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ClauseSet {
    pub vec: Vec<Clause>,
}

impl ClauseSet {
    pub fn push(&mut self, clause: Clause) -> ClId {
        let id = ClId(self.vec.len());
        self.vec.push(clause);
        id
    }

    pub fn enumerate<'a>(&'a self) -> impl Iterator<Item = (ClId, &'a Clause)> {
        self.vec.iter().enumerate().map(|(i, cl)| (ClId(i), cl))
    }
}

impl<'a> IntoIterator for &'a ClauseSet {
    type Item = &'a Clause;
    type IntoIter = std::slice::Iter<'a, Clause>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl std::ops::Index<ClId> for ClauseSet {
    type Output = Clause;
    fn index(&self, idx: ClId) -> &Self::Output {
        &self.vec[idx.0]
    }
}

impl std::ops::IndexMut<ClId> for ClauseSet {
    fn index_mut(&mut self, idx: ClId) -> &mut Self::Output {
        &mut self.vec[idx.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Proof {
    /// Use the k-th hypothesis in the global index (not de Brujin)
    Hypothesis(usize),
    /// `clX p1 p2 ... pN`
    ApplyConj(ClId, Vec<Proof>),
    /// `match (clX p1 p2 ... pN) with hyp1 => q1 | hyp2 => q2 | ... | hypM => qM end`
    ApplyDisj(ClId, Vec<Proof>, Vec<Proof>),
    /// `let concl = clX (hyp => p1) in p2`
    ApplyImpl(ClId, Box<Proof>, Box<Proof>),
}
