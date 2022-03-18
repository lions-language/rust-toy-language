#![feature(box_into_inner)]

mod error;
mod parser;

pub use parser::Parser;

pub use error::*;
pub(crate) use lexical::{FromString, Keyword, LexicalParser, Token, TokenReader, Word};

pub use parser::{
    ArrayFinder, AstNode, BlockContext, BreakContext, BuiltInFunc, ContinueContext,
    CustomTypeObjectDeclarationContext, DataType, ExprNode, FuncCall, FuncType, FunctionDef, Ident,
    IfContext, IfStatement, ImplStatement, IntdeclarationContext, OptionBlockContext, PeriodAccess,
    ReturnContext, Scope, ScopeType, SharedAstNode, SharedExpr, SharedScope,
    SharedScopeOptionShower, StructDef, StructInit, Symbol, SymbolObject, TimeUnit, Value,
    WhileContext, WhileStatement,
};
