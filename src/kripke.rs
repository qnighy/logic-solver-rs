use std::collections::HashMap;

use crate::prop::Prop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KripkeRefutation {
    pub num_worlds: usize,
    // accessibility[w0] = list of worlds reachable from w0
    pub accessibility: Vec<Vec<usize>>,
    pub subprops: Vec<Prop>,
    pub valuation: HashMap<Prop, Vec<bool>>,
}
