use std::cell::RefCell;
use std::ops::Deref;
use std::rc::{Rc, Weak};

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

pub struct Shared<R>(Rc<RefCell<R>>);

impl<R> Shared<R> {
    pub fn weak(&self) -> Weaked<R> {
        Weaked(Rc::downgrade(&self.0))
    }
}

impl<R> std::ops::Deref for Shared<R> {
    type Target = RefCell<R>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<R> Clone for Shared<R> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}

impl<R> Shared<R> {
    pub fn new(r: R) -> Self {
        Self(Rc::new(RefCell::new(r)))
    }
}

impl<R> From<R> for Shared<R> {
    fn from(r: R) -> Self {
        Self::new(r)
    }
}

impl<R> From<Rc<RefCell<R>>> for Shared<R> {
    fn from(v: Rc<RefCell<R>>) -> Self {
        Self(v)
    }
}

impl<R> std::fmt::Debug for Shared<R>
where
    R: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

impl<R> PartialEq for Shared<R>
where
    R: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<R> Default for Shared<R>
where
    R: Default,
{
    fn default() -> Self {
        Shared(Rc::new(RefCell::new(R::default())))
    }
}

pub struct Weaked<T>(Weak<RefCell<T>>);

impl<T> Weaked<T> {
    pub fn shared(&self) -> Option<Shared<T>> {
        match self.0.upgrade() {
            Some(r) => Some(Shared::from(r)),
            None => None,
        }
    }

    pub fn shared_unchecked(&self) -> Shared<T> {
        Shared::from(
            self.0
                .upgrade()
                .expect(&format!("shared={} release", std::any::type_name::<T>())),
        )
    }
}

impl<R> std::fmt::Debug for Weaked<R>
where
    R: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.shared() {
            Some(s) => s.fmt(f),
            None => Ok(()),
        }
    }
}

impl<R> PartialEq for Weaked<R>
where
    R: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let o = match other.shared() {
            Some(s) => s,
            None => return false,
        };

        match self.shared() {
            Some(s) => s.eq(&o),
            None => false,
        }
    }
}

impl<R> Default for Weaked<R>
where
    R: Default,
{
    fn default() -> Self {
        Self(Weak::new())
    }
}

impl<R> Clone for Weaked<R> {
    fn clone(&self) -> Self {
        Weaked(self.0.clone())
    }
}

// #[macro_export]
// macro_rules!  {
//     () => { $crate::inner::foo() };
// }
