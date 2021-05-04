#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prop {
    Atom(String),
    Impl(Box<Prop>, Box<Prop>),
    Conj(Vec<Prop>),
    Disj(Vec<Prop>),
}
