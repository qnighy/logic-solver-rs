use serde::{Deserialize, Serialize};

use crate::rollback::{Rollback, RollbackGuard};

pub trait Shift: Clone {
    fn shift(&mut self, after: Idx, by: usize);
    fn shifted(&self, after: Idx, by: usize) -> Self {
        let mut shifted = self.clone();
        shifted.shift(after, by);
        shifted
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Idx(pub usize);

impl Idx {
    pub fn s(self) -> Self {
        Self(self.0 + 1)
    }
}

impl Shift for Idx {
    fn shift(&mut self, after: Idx, by: usize) {
        if *self >= after {
            self.0 += by;
        }
    }
}

#[derive(Debug, Clone)]
pub struct DbCtx<T> {
    stack: Vec<T>,
}

impl<T> DbCtx<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, idx: Idx) -> Option<&T> {
        self.stack.get(self.stack.len().wrapping_sub(1 + idx.0))
    }

    pub fn push(&mut self, val: T) -> impl std::ops::DerefMut<Target = DbCtx<T>> + '_ {
        struct R(usize);

        impl<T> Rollback<DbCtx<T>> for R {
            fn rollback(&self, this: &mut DbCtx<T>) {
                this.stack.truncate(self.0);
            }
        }

        let len = self.stack.len();
        self.stack.push(val);
        RollbackGuard::new(self, R(len))
    }
}

impl<T> Default for DbCtx<T> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
        }
    }
}

impl<T> std::ops::Index<Idx> for DbCtx<T> {
    type Output = T;
    fn index(&self, idx: Idx) -> &Self::Output {
        self.get(idx).unwrap()
    }
}
