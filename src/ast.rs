#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prop {
    Atom(String),
    Conj(Vec<Prop>),
    Disj(Vec<Prop>),
}
