use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use super::calc_result::CalcResult;
use utils::{Shared, SharedCell, Weaked};

use grammar::{Scope, SharedScope};

pub type SharedStackFrame = Shared<StackFrame>;

#[derive(Clone)]
pub struct SharedStackFrameVec(Shared<Vec<Shared<StackFrame>>>);

impl SharedStackFrameVec {
    pub fn push(&mut self, stack_frame: Shared<StackFrame>) {
        if self.cell().len() > 0 {
            for f in self.cell().iter() {
                if f.cell().enclosing_scope_equal(&*stack_frame.cell()) {
                    stack_frame.cell_mut().set_enclosing_scope_frame(f.weak());
                }
            }

            if stack_frame.cell().enclosing_scope_frame().is_none() {
                stack_frame
                    .cell_mut()
                    .set_enclosing_scope_frame(self.cell().last().unwrap().weak());
            }
        }

        #[cfg(feature = "debug")]
        println!("enter={}", self.cell().len());

        self.cell_mut().push(stack_frame);
    }

    pub fn pop(&mut self) {
        if self.cell().len() > 0 {
            self.cell_mut().pop();
        }

        #[cfg(feature = "debug")]
        println!("pop {}", self.cell().len());
    }

    #[inline]
    pub fn new() -> Self {
        Self(Shared::new(Vec::new()))
    }
}

impl std::ops::Deref for SharedStackFrameVec {
    type Target = Shared<Vec<Shared<StackFrame>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct SharedStackFrameOperator(SharedStackFrameVec);

impl SharedStackFrameOperator {
    pub fn pusher(&mut self) -> SharedStackFrameGuard {
        SharedStackFrameGuard(self.0.clone())
    }

    pub fn push(&mut self, stack_frame: Shared<StackFrame>) {
        self.0.push(stack_frame);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn new() -> Self {
        Self(SharedStackFrameVec::new())
    }
}

impl Deref for SharedStackFrameOperator {
    type Target = RefCell<Vec<SharedStackFrame>>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

pub struct SharedStackFrameGuard(SharedStackFrameVec);

impl SharedStackFrameGuard {
    pub fn push(&mut self, stack_frame: SharedStackFrame) {
        self.0.push(stack_frame);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }
}

impl Drop for SharedStackFrameGuard {
    fn drop(&mut self) {
        self.pop();
    }
}

#[derive(Clone)]
pub struct SharedSharedStackFrameOperator(Rc<RefCell<SharedStackFrameOperator>>);

impl SharedSharedStackFrameOperator {
    #[inline]
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(SharedStackFrameOperator::new())))
    }
}

impl std::ops::Deref for SharedSharedStackFrameOperator {
    type Target = RefCell<SharedStackFrameOperator>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

pub struct StackFrame {
    scope: Option<Weaked<Scope>>,
    enclosing_scope_frame: Option<Weaked<StackFrame>>,
    variables: HashMap<String, CalcResult>,
}

impl StackFrame {
    pub fn scope(&self) -> &Option<Weaked<Scope>> {
        &self.scope
    }

    pub fn set_enclosing_scope_frame(&mut self, enclosing_scope_frame: Weaked<StackFrame>) {
        self.enclosing_scope_frame = Some(enclosing_scope_frame);
    }

    pub fn get_enclosing_scope_frame(&self) -> Option<Weaked<StackFrame>> {
        match self.enclosing_scope_frame {
            Some(ref sf) => Some(sf.clone()),
            None => None,
        }
    }

    pub fn enclosing_scope_frame(&self) -> &Option<Weaked<StackFrame>> {
        &self.enclosing_scope_frame
    }

    pub fn enclosing_scope_equal(&self, other: &StackFrame) -> bool {
        if self.scope.is_none() && other.scope.is_none() {
            return true;
        }
        if self.scope.is_none() || other.scope.is_none() {
            return false;
        }
        self.scope
            .as_ref()
            .unwrap()
            .shared_unchecked()
            .cell()
            .enclosing_scope_equal(
                other
                    .scope
                    .as_ref()
                    .unwrap()
                    .shared_unchecked()
                    .cell()
                    .enclosing_scope(),
            )
    }

    pub fn add_variable(&mut self, name: String, value: CalcResult) {
        self.variables.insert(name, value);
    }

    pub fn update_variable(&mut self, name: &str, value: CalcResult) {
        match self.variables.get_mut(name) {
            Some(v) => *v = value,
            None => {
                panic!("update_variable error: not found");
            }
        }
    }

    pub fn get_variable_value(&self, name: &str) -> Option<CalcResult> {
        match self.variables.get(name) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    pub fn new(scope: Option<Weaked<Scope>>) -> Self {
        Self {
            scope: scope,
            enclosing_scope_frame: None,
            variables: HashMap::new(),
        }
    }
}
