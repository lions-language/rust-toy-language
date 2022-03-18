pub trait SharedCell<R> {
    fn cell(&self) -> std::cell::Ref<'_, R>;
    fn cell_mut(&self) -> std::cell::RefMut<'_, R>;
}

impl<T, R> SharedCell<R> for T
where
    T: std::ops::Deref<Target = std::cell::RefCell<R>>,
{
    fn cell(&self) -> std::cell::Ref<'_, R> {
        self.deref().borrow()
    }

    fn cell_mut(&self) -> std::cell::RefMut<'_, R> {
        self.deref().borrow_mut()
    }
}
