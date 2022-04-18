use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use educe::Educe;

mod ext;
mod parser;
mod scope;
mod symbol;
mod typ;

pub use ext::*;
pub use parser::Parser;
pub use scope::{Scope, ScopeType, SharedScope, SharedScopeOptionShower};
pub use symbol::{Symbol, SymbolObject};

use try_match::try_match;

pub(crate) use utils::{Shared, SharedCell, Weaked};

#[derive(Debug, Clone)]
pub struct SharedAstNode(Rc<RefCell<AstNode>>);

impl SharedAstNode {
    #[inline]
    pub fn new(ast_node: AstNode) -> Self {
        Self(Rc::new(RefCell::new(ast_node)))
    }

    #[inline]
    pub fn new_expr(expr: SharedExpr) -> Self {
        SharedAstNode::new(AstNode::Expr(expr))
    }

    #[inline]
    pub fn build_format(&self, level: usize, s: &mut String) {
        self.0.as_ref().borrow().build_format(level, s);
    }
}

impl std::ops::Deref for SharedAstNode {
    type Target = RefCell<AstNode>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug)]
pub enum TimeUnit {
    Second,
    Millisecond,
    Microsecond,
}

impl TimeUnit {
    pub fn from_str(s: &str) -> Option<TimeUnit> {
        match s {
            "s" => Some(TimeUnit::Second),
            "ms" => Some(TimeUnit::Millisecond),
            "us" => Some(TimeUnit::Microsecond),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    U32(u32),
    U64(u64),
    Bool(bool),
    TimeLiteral(u64, TimeUnit),
    String(String),
    Unit,
}

impl Value {
    pub fn build_format(&self, level: usize, s: &mut String) {
        use Value::*;

        match self {
            U32(v) => s.push_str(&format!("{}", v)),
            U64(v) => s.push_str(&format!("{}", v)),
            Bool(v) => s.push_str(&format!("{}", v)),
            TimeLiteral(time, time_unit) => {
                s.push_str(&format!("time={:?}, unit={:?}", time, time_unit))
            }
            String(v) => s.push_str(&v),
            Unit => s.push_str("()"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            U32(v) => write!(f, "{}", v),
            U64(v) => write!(f, "{}", v),
            Bool(v) => write!(f, "{}", v),
            TimeLiteral(time, time_unit) => write!(f, "time={:?}, unit={:?}", time, time_unit),
            String(v) => write!(f, "{}", v),
            Unit => write!(f, "()"),
        }
    }
}

pub trait Sign {
    fn to_string(&self) -> String;
}

pub trait FunctionSign {
    fn info<ParamItem, Params, ReturnType>(&self) -> (Params, ReturnType)
    where
        ParamItem: ToString,
        Params: Iterator<Item = ParamItem>,
        ReturnType: ToString;
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_data_types: Vec<Box<DataType>>,
    pub return_data_type: Box<DataType>,
    // assignment in the semantic analysis phase
    pub func_define: Option<Weaked<FunctionDef>>,
}

impl Sign for FunctionType {
    fn to_string(&self) -> String {
        let mut s = String::from("(");
        let mut c = String::from("");
        for dt in self.param_data_types.iter() {
            s.push_str(&c);
            s.push_str(&dt.to_string());
            c = ",".to_string();
        }
        s.push_str(&format!(")->{}", self.return_data_type.to_string()));

        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Unknown(String),
    Unit,
    Int,
    Double,
    Long,
    Bool,
    String,
    Struct(Weaked<StructDef>),
    Function(FunctionType),
}

impl Default for DataType {
    fn default() -> Self {
        DataType::Unknown("default".into())
    }
}

// impl fmt::Debug for DataType {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             v @ DataType::Unknown(_) => v.fmt(f),
//             v @ DataType::Unit => v.fmt(f),
//             v @ DataType::Int => v.fmt(f),
//             v @ DataType::Double => v.fmt(f),
//             v @ DataType::Long => v.fmt(f),
//             v @ DataType::Bool => v.fmt(f),
//             v @ DataType::String => v.fmt(f),
//             DataType::Struct(sd) => f.write_fmt(format_args!("Struct({})", sd.cell().struct_name)),
//         }
//     }
// }

impl DataType {
    pub fn to_string(&self) -> String {
        match self {
            DataType::Unknown(v) => format!("Unknown({})", v),
            DataType::Unit => "Unit".to_string(),
            DataType::Int => "Int".to_string(),
            DataType::Double => "Double".to_string(),
            DataType::Long => "Long".to_string(),
            DataType::Bool => "Bool".to_string(),
            DataType::String => "String".to_string(),
            DataType::Struct(s) => s.shared_unchecked().cell().struct_name.clone(),
            DataType::Function(f) => f.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub value: String,
}

// impl ToString for Ident {
//     fn to_string(&self) -> Self {
//         self.value.into()
//     }
// }

impl Ident {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&self.value)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.value)
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct IntdeclarationContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl IntdeclarationContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct ReturnContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl ReturnContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct BreakContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl BreakContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct ContinueContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl ContinueContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct BlockContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Option<Shared<Scope>>,
}

impl BlockContext {
    pub fn enclosing_scope(&self) -> Option<Weaked<Scope>> {
        self.enclosing_scope.as_ref().map(|s| s.weak())
    }

    pub fn new(scope: Option<Shared<Scope>>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Debug, Clone)]
pub struct OptionBlockContext<'a>(&'a Option<BlockContext>);

impl<'a> OptionBlockContext<'a> {
    pub fn enclosing_scope_clone(&self) -> Option<Weaked<Scope>> {
        match self.0 {
            Some(bc) => bc.enclosing_scope.as_ref().map(|s| s.weak()),
            None => None,
        }
    }

    pub fn new(c: &'a Option<BlockContext>) -> Self {
        Self(c)
    }
}

// #[derive(Educe, Clone)]
// #[educe(Debug)]
#[derive(Clone, Default)]
pub struct VariantDef {
    pub name: String,
    // #[educe(Debug(ignore))]
    pub data_type: DataType,
}

impl fmt::Debug for VariantDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VariantDef")
            .field("name", &self.name)
            .field("data_type", &self.data_type.to_string())
            .finish()
    }
}

impl VariantDef {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&self.data_type.to_string());
        s.push(' ');
        s.push_str(&self.name);
    }
}

impl Unique for VariantDef {
    type T = String;

    fn unique(&self) -> &Self::T {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncNameType {
    Named(String),
    Anonymous(String),
}

impl ToString for FuncNameType {
    fn to_string(&self) -> String {
        match self {
            FuncNameType::Named(n) | FuncNameType::Anonymous(n) => n.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub func_name: FuncNameType,
    pub variant_defs: Vec<VariantDef>,
    pub body: SharedAstNode,
    pub return_data_type: DataType,
    pub context: Option<BlockContext>,
}

impl FunctionDef {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str("func ");
        s.push_str(&format!("{:?}", self.func_name));
    }
}

impl PartialEq for FunctionDef {
    fn eq(&self, other: &Self) -> bool {
        self.func_name.eq(&other.func_name)
    }
}

#[derive(Debug, Clone)]
pub struct MethodDef {
    pub method_type: MethodType,
    pub function_def: FunctionDef,
}

impl MethodDef {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str("func ");
        s.push_str(&format!("{:?}", self.function_def.func_name));
    }
}

#[derive(Debug, Clone)]
pub struct ImplStatement {
    pub name: Ident,
    pub nodes: Vec<SharedAstNode>,
    pub context: Option<BlockContext>,
}

impl ImplStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str("impl ");
        s.push_str(&self.name.value);
    }
}

#[derive(Debug, Clone)]
pub enum MethodType {
    Static,
    Member,
}

#[derive(Educe, Clone, Default)]
#[educe(Debug)]
pub struct StructDef {
    pub struct_name: String,
    pub member_variants: Vec<VariantDef>,
    pub member_method_defines: HashMap<String, FunctionDef>,
    pub static_method_defines: HashMap<String, FunctionDef>,
    #[educe(Debug(ignore))]
    pub context: Option<BlockContext>,
}

impl PartialEq for StructDef {
    fn eq(&self, other: &Self) -> bool {
        self.struct_name.eq(&other.struct_name)
    }
}

impl Unique for StructDef {
    type T = String;

    fn unique(&self) -> &Self::T {
        &self.struct_name
    }
}

impl StructDef {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&format!("struct {}", &self.struct_name));
        s.push_str(" {");
        for member in self.member_variants.iter() {
            s.push_str(&format!(
                "\n  {}: {}",
                &member.name,
                &member.data_type.to_string()
            ));
        }
        s.push_str("\n}");
    }
}

pub struct SharedStructDef(Rc<RefCell<StructDef>>);

impl std::ops::Deref for SharedStructDef {
    type Target = RefCell<StructDef>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug, Clone)]
pub enum BuiltInFunc {
    Println,
    Sleep,
}

#[derive(Debug)]
pub enum FuncClassify {
    BuiltIn(BuiltInFunc),
    Custom,
}

#[derive(Educe)]
#[educe(Debug)]
pub struct FuncCall {
    pub func_type: FuncClassify,
    pub func_name: String,
    pub params: Vec<SharedExpr>,
    #[educe(Debug(ignore))]
    pub enclosing_scope: Option<Shared<Scope>>,
}

impl FuncCall {
    pub fn push_param(&mut self, expr: SharedExpr) {
        self.params.push(expr);
    }

    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&format!("call function: {}", self.func_name));
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct PeriodAccess {
    pub first_expr: SharedExpr,
    pub exprs: Vec<SharedExpr>,
    #[educe(Debug(ignore))]
    pub enclosing_scope: Option<Shared<Scope>>,
}

impl PeriodAccess {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&format!("{:?}", self.exprs));
    }
}

impl Deref for PeriodAccess {
    type Target = Vec<SharedExpr>;

    fn deref(&self) -> &Self::Target {
        &self.exprs
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_expr: SharedExpr,
    pub context: Option<ReturnContext>,
}

impl ReturnStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        let mut expr = String::new();
        self.return_expr.build_format(level, &mut expr);
        s.push_str(&format!("return {}", expr));
    }
}

#[derive(Debug)]
pub struct BreakStatement {
    pub context: Option<BreakContext>,
}

impl BreakStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        let mut expr = String::new();
        s.push_str(&format!("break"));
    }
}

#[derive(Debug)]
pub struct ContinueStatement {
    pub context: Option<ContinueContext>,
}

impl ContinueStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        let mut expr = String::new();
        s.push_str(&format!("continue"));
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct IfContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl IfContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct WhileContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl WhileContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Debug)]
pub struct ConditionBlock {
    pub expr: SharedExpr,
    pub body: SharedAstNode,
}

#[derive(Debug)]
pub struct IfStatement {
    pub if_condition: ConditionBlock,
    pub else_if_conditions: Vec<ConditionBlock>,
    pub else_condition: Option<ConditionBlock>,
    pub context: Option<IfContext>,
}

impl IfStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str("if statement");
    }
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: ConditionBlock,
    pub context: Option<WhileContext>,
}

impl WhileStatement {
    pub fn build_format(&self, level: usize, s: &mut String) {
        s.push_str("while statement");
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct StructInitContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

#[derive(Debug, derive_new::new)]
pub struct StructFieldInit {
    pub field_name: String,
    pub init_expr: SharedExpr,
}

impl Unique for StructFieldInit {
    type T = String;

    fn unique(&self) -> &Self::T {
        &self.field_name
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct StructInit {
    pub ident: Ident,
    pub field_initlist: Vec<StructFieldInit>,
    pub context: Option<StructInitContext>,
}

impl StructInit {
    fn build_format(&self, level: usize, s: &mut String) {
        s.push_str(&format!("init {} struct", self.ident.value));
    }
}

#[derive(Debug)]
pub enum ExprNode {
    Eq { left: SharedExpr, right: SharedExpr },
    Neq { left: SharedExpr, right: SharedExpr },
    Lt { left: SharedExpr, right: SharedExpr },
    Gt { left: SharedExpr, right: SharedExpr },
    Additive { left: SharedExpr, right: SharedExpr },
    Multiplicative { left: SharedExpr, right: SharedExpr },
    Assignment { left: SharedExpr, right: SharedExpr },
    Not { expr: SharedExpr },
    Identifier(Ident),
    StructInit(StructInit),
    Value(Value),
    FuncCall(FuncCall),
    PeriodAccess(PeriodAccess),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
}

impl ExprNode {
    #[inline]
    fn format_binary_expr(
        &self,
        name: &str,
        left_expr: &SharedExpr,
        right_expr: &SharedExpr,
        level: usize,
        s: &mut String,
    ) {
        s.push_str(name);
        s.push('\n');
        left_expr.build_format(level + 1, s);
        s.push('\n');
        right_expr.build_format(level + 1, s);
    }

    #[inline]
    fn format_unary_expr(&self, name: &str, expr: &SharedExpr, level: usize, s: &mut String) {
        s.push_str(name);
        s.push('\n');
        expr.build_format(level + 1, s);
    }

    pub fn build_format(&self, mut level: usize, s: &mut String) {
        use ExprNode::*;

        s.push_str(&"  ".repeat(level));

        match self {
            Eq { left, right } => {
                self.format_binary_expr("==", left, right, level, s);
            }
            Neq { left, right } => {
                self.format_binary_expr("!=", left, right, level, s);
            }
            Lt { left, right } => {
                self.format_binary_expr("<", left, right, level, s);
            }
            Not { expr } => {
                self.format_unary_expr("!", expr, level, s);
            }
            Lt { left, right } => {
                self.format_binary_expr("<", left, right, level, s);
            }
            Gt { left, right } => {
                self.format_binary_expr(">", left, right, level, s);
            }
            Additive { left, right } => {
                self.format_binary_expr("+", left, right, level, s);
            }
            Multiplicative { left, right } => {
                self.format_binary_expr("*", left, right, level, s);
            }
            Assignment { left, right } => {
                self.format_binary_expr("=", left, right, level, s);
            }
            Identifier(ident) => {
                ident.build_format(level + 1, s);
            }
            StructInit(si) => {
                si.build_format(level + 1, s);
            }
            Value(value) => {
                value.build_format(level + 1, s);
            }
            FuncCall(fc) => {
                fc.build_format(level, s);
            }
            PeriodAccess(pa) => {
                pa.build_format(level, s);
            }
            IfStatement(is) => {
                is.build_format(level, s);
            }
            WhileStatement(ws) => {
                ws.build_format(level, s);
            }
        }
    }
}

pub struct Expr {
    pub data_type: DataType,
    pub expr_node: ExprNode,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Expr")
            .field("data_type", &self.data_type.to_string())
            .field("expr_node", &self.expr_node)
            .finish()
        // f.write_fmt(format_args!(
        //     "Expr {{\n\tdata_type: {}\n\texpr_node: {:#?}\n}}",
        //     &self.data_type.to_string(),
        //     self.expr_node
        // ))
    }
}

impl Expr {
    pub fn build_format(&self, mut level: usize, s: &mut String) {
        self.expr_node.build_format(level, s);
    }

    pub fn new_with_name_data_type(name: impl Into<String>, expr_node: ExprNode) -> Self {
        Self {
            data_type: DataType::Unknown(name.into()),
            expr_node: expr_node,
        }
    }

    pub fn new_without_data_type(expr_node: ExprNode) -> Self {
        Self {
            data_type: DataType::default(),
            expr_node: expr_node,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SharedExpr(Rc<RefCell<Expr>>);

impl SharedExpr {
    #[inline]
    pub fn new(expr: Expr) -> Self {
        Self(Rc::new(RefCell::new(expr)))
    }

    pub fn new_with_name_data_type(name: impl Into<String>, expr_node: ExprNode) -> Self {
        SharedExpr::new(Expr::new_with_name_data_type(name, expr_node))
    }

    pub fn new_without_data_type(expr_node: ExprNode) -> Self {
        SharedExpr::new(Expr::new_without_data_type(expr_node))
    }

    pub fn as_ref(&self) -> &Rc<RefCell<Expr>> {
        &self.0
    }

    #[inline]
    pub fn build_format(&self, level: usize, s: &mut String) {
        self.0.as_ref().borrow().build_format(level, s);
    }
}

impl std::ops::Deref for SharedExpr {
    type Target = RefCell<Expr>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Educe, derive_new::new)]
#[educe(Debug)]
pub struct CustomTypeObjectDeclaration {
    pub type_name: String,
    pub object_name: String,
    pub expr: Option<SharedExpr>,
    pub context: Option<CustomTypeObjectDeclarationContext>,
}

#[derive(Educe)]
#[educe(Debug)]
pub struct CustomTypeObjectDeclarationContext {
    #[educe(Debug(ignore))]
    pub enclosing_scope: Shared<Scope>,
}

impl CustomTypeObjectDeclarationContext {
    pub fn new(scope: Shared<Scope>) -> Self {
        Self {
            enclosing_scope: scope,
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub enum AstNode {
    Indeclaration {
        identifier: String,
        expr: Option<SharedExpr>,
        #[educe(Debug(ignore))]
        context: Option<IntdeclarationContext>,
    },
    CustomTypeObjectDeclaration(CustomTypeObjectDeclaration),
    VariantDef(VariantDef),
    FunctionDef(FunctionDef),
    MethodDef(MethodDef),
    MainBlock {
        nodes: Vec<SharedAstNode>,
        context: Option<BlockContext>,
    },
    Block {
        nodes: Vec<SharedAstNode>,
        context: Option<BlockContext>,
    },
    FuncBlock {
        nodes: Vec<SharedAstNode>,
        context: Option<BlockContext>,
    },
    LoopBlock {
        nodes: Vec<SharedAstNode>,
        context: Option<BlockContext>,
    },
    CondBlock {
        nodes: Vec<SharedAstNode>,
        context: Option<BlockContext>,
    },
    StructDef(StructDef),
    ReturnStatement(ReturnStatement),
    BreakStatement(BreakStatement),
    ContinueStatement(ContinueStatement),
    ImplStatement(ImplStatement),
    Expr(SharedExpr),
}

impl AstNode {
    pub fn build_format(&self, mut level: usize, s: &mut String) {
        use AstNode::*;

        s.push_str(&"  ".repeat(level));

        match self {
            Indeclaration {
                identifier,
                expr,
                context,
            } => {
                s.push_str("Indeclaration");
                if let Some(e) = expr {
                    s.push('=');
                    s.push('\n');
                    e.build_format(level + 1, s);
                } else {
                }
            }
            CustomTypeObjectDeclaration(cd) => {
                s.push_str(&format!("{} {}", cd.type_name, cd.object_name));
                if let Some(e) = &cd.expr {
                    s.push('=');
                    s.push('\n');
                    e.build_format(level + 1, s);
                };
            }
            VariantDef(def) => {
                def.build_format(level + 1, s);
            }
            FunctionDef(def) => {
                def.build_format(level + 1, s);
            }
            MethodDef(def) => {
                def.build_format(level + 1, s);
            }
            StructDef(def) => {
                def.build_format(level + 1, s);
            }
            MainBlock { nodes, .. }
            | Block { nodes, .. }
            | FuncBlock { nodes, .. }
            | LoopBlock { nodes, .. }
            | CondBlock { nodes, .. } => {
                for (index, node) in nodes.iter().enumerate() {
                    node.build_format(level, s);
                    if index < nodes.len() - 1 {
                        s.push('\n');
                    }
                }
            }
            ReturnStatement(rs) => {
                rs.build_format(level, s);
            }
            BreakStatement(bs) => {
                bs.build_format(level, s);
            }
            ContinueStatement(cs) => {
                cs.build_format(level, s);
            }
            ImplStatement(is) => {
                is.build_format(level, s);
            }
            Expr(e) => {
                e.build_format(level, s);
            }
        }
    }
}
