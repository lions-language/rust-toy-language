use std::cell::RefCell;
use std::rc::Rc;

use super::ext::*;
use super::{DataType, FunctionDef, Scope, SharedAstNode, SharedScope, StructDef};

#[derive(Debug, Clone)]
pub enum SymbolObject {
    Variable { data_type: DataType },
    Block,
    Function {},
    Struct {},
    Impl {},
}

#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    enclosing_scope: SharedScope,
    object: SymbolObject,
    ast_node: SharedAstNode,
}

impl Unique for Symbol {
    type T = String;

    fn unique(&self) -> &Self::T {
        &self.name
    }
}

impl Symbol {
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_object(&self) -> &SymbolObject {
        &self.object
    }

    pub fn get_enclosing_scope(&self) -> SharedScope {
        self.enclosing_scope.clone()
    }

    pub fn new_block_symbol(scope: SharedScope, ast_node: SharedAstNode) -> Self {
        Self {
            name: "block".to_string(),
            enclosing_scope: scope,
            object: SymbolObject::Block,
            ast_node: ast_node,
        }
    }

    pub fn new_variable_symbol(
        name: String,
        scope: SharedScope,
        data_type: DataType,
        ast_node: SharedAstNode,
    ) -> Self {
        Self {
            name: name,
            enclosing_scope: scope,
            object: SymbolObject::Variable {
                data_type: data_type,
            },
            ast_node: ast_node,
        }
    }

    pub fn new_function_symbol(
        name: String,
        scope: SharedScope,
        ast_node: SharedAstNode,
        func_def: &FunctionDef,
    ) -> Self {
        Self {
            name: name,
            enclosing_scope: scope,
            object: SymbolObject::Function {},
            ast_node: ast_node,
        }
    }

    pub fn new_impl_symbol(name: String, scope: SharedScope, ast_node: SharedAstNode) -> Self {
        Self {
            name: name,
            enclosing_scope: scope,
            object: SymbolObject::Impl {},
            ast_node: ast_node,
        }
    }

    pub fn new_struct_symbol(
        name: String,
        scope: SharedScope,
        ast_node: SharedAstNode,
        struct_def: &StructDef,
    ) -> Self {
        Self {
            name: name,
            enclosing_scope: scope,
            object: SymbolObject::Struct {},
            ast_node: ast_node,
        }
    }
}

// impl fmt::Display for Symbol {
//     fn fmt(&mut self, f: &mut fmt::Formatter) -> fmt::Result {
//     }
// }
