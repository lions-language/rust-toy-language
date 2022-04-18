use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use std::fmt;

use super::ext::*;
use super::{Shared, SharedCell, Symbol};
use utils::Weaked;

pub struct SharedScopeOptionShower<'a>(&'a Option<Shared<Scope>>);

impl<'a> SharedScopeOptionShower<'a> {
    pub fn show_scope_type(&self) {
        println!("{:?}", self.get_scope_type());
    }

    pub fn get_scope_type(&self) -> Option<ScopeType> {
        match self.0 {
            Some(ss) => Some(ss.cell().scope_type.clone()),
            None => None,
        }
    }

    pub fn new(ss: &'a Option<Shared<Scope>>) -> Self {
        Self(ss)
    }
}

#[derive(Debug)]
pub struct SharedScope(Rc<RefCell<Scope>>);

impl Clone for SharedScope {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl SharedScope {
    pub fn new(scope: Scope) -> Self {
        Self(Rc::new(RefCell::new(scope)))
    }
}

impl std::ops::Deref for SharedScope {
    type Target = RefCell<Scope>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug, Clone)]
pub enum ScopeType {
    Block,
    Function,
    Loop,
    Cond,
    Struct,
    Impl,
}

#[derive(Debug)]
pub struct Scope {
    symbols: Vec<Symbol>,
    enclosing_scope: Option<Weaked<Scope>>,
    scope_type: ScopeType,
}

impl Scope {
    pub fn scope_type(&self) -> ScopeType {
        self.scope_type.clone()
    }

    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }

    pub fn enclosing_scope_equal(&self, other: &Option<Weaked<Scope>>) -> bool {
        self.enclosing_scope.as_ref().map_or_else(
            || if other.is_none() { true } else { false },
            |v| {
                if other.is_none() {
                    false
                } else {
                    // self.enclosing_scope.as_ref().unwrap().as_ptr()
                    //     == other.as_ref().unwrap().as_ptr()
                    std::ptr::eq(
                        self.enclosing_scope.as_ref().unwrap(),
                        other.as_ref().unwrap(),
                    )
                }
            },
        )
    }

    pub fn enclosing_scope(&self) -> &Option<Weaked<Scope>> {
        &self.enclosing_scope
    }

    pub fn enclosing_scope_exists(&self) -> bool {
        if let &None = &self.enclosing_scope {
            false
        } else {
            true
        }
    }

    pub fn variable_symbol_exists(&self, name: &str) -> bool {
        for symbol in self.symbols.iter() {
            if symbol.get_name() == name {
                return true;
            }
        }

        false
    }

    pub fn find_variable_symbol(&self, name: &str) -> Option<Symbol> {
        self.symbols.find_clone(name)
    }

    pub fn new(enclosing_scope: Option<Weaked<Scope>>, scope_type: ScopeType) -> Self {
        Self {
            symbols: Vec::new(),
            enclosing_scope: enclosing_scope,
            scope_type: scope_type,
        }
    }
}

// impl fmt::Display for Scope {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         for symbol in self.symbols {
//         }
//     }
// }
