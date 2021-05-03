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
