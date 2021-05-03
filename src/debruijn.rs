#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Idx(pub usize);

impl Idx {
    pub fn s(self) -> Self {
        Self(self.0 + 1)
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

pub trait Rollback<T> {
    fn rollback(&self, orig: &mut T);
}

#[derive(Debug)]
pub struct RollbackGuard<'a, T, R>
where
    R: Rollback<T>,
{
    inner: &'a mut T,
    rollback: R,
}

impl<'a, T, R> RollbackGuard<'a, T, R>
where
    R: Rollback<T>,
{
    pub fn new(inner: &'a mut T, rollback: R) -> Self {
        Self { inner, rollback }
    }
}

impl<'a, T, R> Drop for RollbackGuard<'a, T, R>
where
    R: Rollback<T>,
{
    fn drop(&mut self) {
        self.rollback.rollback(self.inner);
    }
}

impl<'a, T, R> std::ops::Deref for RollbackGuard<'a, T, R>
where
    R: Rollback<T>,
{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'a, T, R> std::ops::DerefMut for RollbackGuard<'a, T, R>
where
    R: Rollback<T>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner
    }
}
