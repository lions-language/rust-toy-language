use grammar::{
    ArrayFinder, AstNode, BlockContext, BreakContext, ContinueContext,
    CustomTypeObjectDeclarationContext, DataType, ExprNode, FuncCall, FuncClassify, FunctionDef,
    Ident, IfContext, IfStatement, ImplStatement, IntdeclarationContext, MethodDef, MethodType,
    PeriodAccess, ReturnContext, Scope, ScopeType, SharedAstNode, SharedExpr, SharedScope,
    SharedScopeOptionShower, StructDef, StructInit, Symbol, SymbolObject, Value, WhileContext,
    WhileStatement,
};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use utils::{Shared, SharedCell, Weaked};

use try_match::try_match;

pub trait FunctionDefFinder {
    fn find(&self, name: &str) -> Option<&FunctionDef>;
}

impl FunctionDefFinder for HashMap<String, FunctionDef> {
    fn find(&self, name: &str) -> Option<&FunctionDef> {
        self.get(name)
    }
}

pub struct SemanticDefines {
    pub func_defs: HashMap<String, FunctionDef>,
    pub struct_defs: HashMap<String, Shared<StructDef>>,
}

impl SemanticDefines {
    pub fn find_func_def(&self, name: &str) -> Option<&FunctionDef> {
        self.func_defs.find(name)
    }

    pub fn find_struct_def(&self, name: &str) -> Option<&Shared<StructDef>> {
        self.struct_defs.get(name)
    }

    pub fn add_member_method_to_struct(&mut self, ident: &Ident, function_def: &FunctionDef) {
        let struct_define = match self.struct_defs.get_mut(&ident.value) {
            Some(def) => def,
            None => panic!("{} cannot be found from struct-defs", ident.value),
        };

        struct_define
            .cell_mut()
            .member_method_defines
            .insert(function_def.func_name.to_string(), function_def.clone());
    }

    pub fn add_static_method_to_struct(&mut self, ident: &Ident, function_def: &FunctionDef) {
        let struct_define = match self.struct_defs.get_mut(&ident.value) {
            Some(def) => def,
            None => panic!("{} cannot be found from struct-defs", ident.value),
        };

        struct_define
            .cell_mut()
            .static_method_defines
            .insert(function_def.func_name.to_string(), function_def.clone());
    }

    fn new() -> Self {
        Self {
            func_defs: HashMap::new(),
            struct_defs: HashMap::new(),
        }
    }
}

struct ScopeStack(Vec<Shared<Scope>>);

impl ScopeStack {
    fn last_weak_unwrap(&self) -> Weaked<Scope> {
        self.0.last().as_ref().unwrap().weak()
    }
}

impl std::ops::Deref for ScopeStack {
    type Target = Vec<Shared<Scope>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ScopeStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<Shared<Scope>>> for ScopeStack {
    fn from(s: Vec<Shared<Scope>>) -> Self {
        Self(s)
    }
}

impl ScopeStack {
    fn new() -> Self {
        Self::from(Vec::new())
    }
}

pub struct Semantic {
    pub root_scope: Scope,
    pub defines: SemanticDefines,
    scope_stack: ScopeStack,
}

impl Semantic {
    #[inline]
    fn block_semantic(
        &mut self,
        ast: &SharedAstNode,
        nodes: &mut Vec<SharedAstNode>,
        context: &mut Option<BlockContext>,
        scope_type: ScopeType,
    ) {
        let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));

        #[cfg(feature = "debug")]
        println!(
            "block semantic={:?}, enclosing scope={:?}, scope stack len={}",
            scope_type,
            SharedScopeOptionShower::new(&enclosing_scope).get_scope_type(),
            self.scope_stack.len()
        );

        // println!("block: {:?}", enclosing_scope);

        *context = Some(BlockContext::new(
            enclosing_scope.as_ref().map(|v| v.shared_unchecked()),
        ));

        let scope = Shared::new(Scope::new(enclosing_scope.clone(), scope_type));
        let block_symbol = Symbol::new_block_symbol(scope.weak(), ast.clone());

        self.root_scope.add_symbol(block_symbol);

        self.scope_stack.push(scope.clone());

        for node in nodes.iter_mut() {
            self.semantic(&node.clone());
        }

        self.scope_stack.pop();
    }

    pub fn semantic(&mut self, ast: &SharedAstNode) {
        match &mut *ast.cell_mut() {
            AstNode::MainBlock { nodes, context } => {
                self.block_semantic(ast, nodes, context, ScopeType::Block);
            }
            AstNode::Block { nodes, context } => {
                self.block_semantic(ast, nodes, context, ScopeType::Block);
            }
            AstNode::FuncBlock { nodes, context } => {
                self.block_semantic(ast, nodes, context, ScopeType::Function);
            }
            AstNode::LoopBlock { nodes, context } => {
                self.block_semantic(ast, nodes, context, ScopeType::Loop);
                #[cfg(feature = "debug")]
                println!(
                    "loop block semantic, enclosing scope type={:?}",
                    SharedScopeOptionShower::new(context.as_ref().unwrap().enclosing_scope())
                        .get_scope_type()
                );
            }
            AstNode::CondBlock { nodes, context } => {
                self.block_semantic(ast, nodes, context, ScopeType::Cond);
                #[cfg(feature = "debug")]
                println!(
                    "cond block semantic, enclosing scope type={:?}",
                    SharedScopeOptionShower::new(context.as_ref().unwrap().enclosing_scope())
                        .get_scope_type()
                );
            }
            AstNode::Indeclaration {
                identifier,
                expr,
                context,
            } => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                let variable_symbol = Symbol::new_variable_symbol(
                    identifier.to_string(),
                    cur_scope.clone(),
                    DataType::Int,
                    ast.clone(),
                );
                {
                    cur_scope
                        .shared_unchecked()
                        .cell_mut()
                        .add_symbol(variable_symbol);
                }
                *context = Some(IntdeclarationContext::new(cur_scope.shared_unchecked()));

                if let Some(e) = &expr {
                    self.semantic_expr(e);
                };
            }
            AstNode::CustomTypeObjectDeclaration(cod) => {
                let mut data_type = DataType::Unknown(cod.type_name.to_string());
                if let Some(expr) = &cod.expr {
                    self.semantic_expr(expr);
                    data_type = expr.cell().data_type.clone();
                };

                let cur_scope = self.scope_stack.last_weak_unwrap();
                // let cur_scope = self.scope_stack.last().unwrap().clone();
                let variable_symbol = Symbol::new_variable_symbol(
                    cod.object_name.to_string(),
                    cur_scope.clone(),
                    data_type,
                    ast.clone(),
                );
                {
                    cur_scope
                        .shared_unchecked()
                        .cell_mut()
                        .add_symbol(variable_symbol);
                }
                cod.context = Some(CustomTypeObjectDeclarationContext::new(
                    cur_scope.shared_unchecked(),
                ));
            }
            AstNode::Expr(expr) => {
                self.semantic_expr(expr);
            }
            AstNode::FunctionDef(func_def) => {
                self.semantic_function_def(ast, func_def);

                self.defines
                    .func_defs
                    .insert(func_def.func_name.to_string(), func_def.clone());
            }
            AstNode::StructDef(struct_def) => {
                let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));

                struct_def.context = Some(BlockContext::new(
                    enclosing_scope.as_ref().map(|v| v.shared_unchecked()),
                ));

                let mut scope = Shared::new(Scope::new(enclosing_scope.clone(), ScopeType::Struct));

                let struct_symbol = Symbol::new_struct_symbol(
                    struct_def.struct_name.clone(),
                    scope.weak(),
                    ast.clone(),
                    struct_def,
                );

                // let cur_scope = self.scope_stack.last().unwrap().clone();
                enclosing_scope
                    .unwrap()
                    .shared_unchecked()
                    .cell_mut()
                    .add_symbol(struct_symbol);

                self.semantic_struct_def(struct_def);

                self.defines.struct_defs.insert(
                    struct_def.struct_name.clone(),
                    Shared::<StructDef>::from(struct_def.clone()),
                );
            }
            AstNode::ReturnStatement(rs) => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                rs.context = Some(ReturnContext::new(cur_scope.shared_unchecked()));
            }
            AstNode::BreakStatement(bs) => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                bs.context = Some(BreakContext::new(cur_scope.shared_unchecked()));
            }
            AstNode::ContinueStatement(cs) => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                cs.context = Some(ContinueContext::new(cur_scope.shared_unchecked()));
            }
            AstNode::ImplStatement(is) => {
                self.semantic_impl(ast, is);
            }
            node => {
                unimplemented!("semantic: {:?}", node);
            }
        }
    }

    fn semantic_impl(&mut self, ast: &SharedAstNode, impl_statement: &mut ImplStatement) {
        let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));

        impl_statement.context = Some(BlockContext::new(
            enclosing_scope.as_ref().map(|v| v.shared_unchecked()),
        ));

        let mut scope = Shared::new(Scope::new(enclosing_scope.clone(), ScopeType::Impl));
        let impl_symbol =
            Symbol::new_impl_symbol(impl_statement.name.value.clone(), scope.weak(), ast.clone());

        // let cur_scope = self.scope_stack.last().unwrap().clone();
        enclosing_scope
            .unwrap()
            .shared_unchecked()
            .cell_mut()
            .add_symbol(impl_symbol);

        let struct_define = match self.defines.struct_defs.get(&impl_statement.name.value) {
            Some(def) => def.clone(),
            None => panic!(
                "{} cannot be found from struct-defs in impl statement",
                impl_statement.name.value
            ),
        };

        self.scope_stack.push(scope.clone());

        for node in impl_statement.nodes.iter() {
            match &mut *node.cell_mut() {
                AstNode::MethodDef(md) => {
                    match md.method_type {
                        MethodType::Member => {
                            if md.function_def.variant_defs.len() < 1 {
                                unreachable!(
                                    "Definite presence of member methods (filled in during the syntax analysis phase)");
                            }

                            md.function_def.variant_defs.as_mut_slice()[0].data_type =
                                DataType::Struct(struct_define.weak());
                        }
                        _ => {}
                    }
                    self.semantic_function_def(ast, &mut md.function_def);
                    match md.method_type {
                        MethodType::Member => {
                            self.defines.add_member_method_to_struct(
                                &impl_statement.name,
                                &md.function_def,
                            );
                        }
                        MethodType::Static => {
                            self.defines.add_static_method_to_struct(
                                &impl_statement.name,
                                &md.function_def,
                            );
                        }
                    }
                }
                other => unimplemented!("{:?}", other),
            }
        }

        self.scope_stack.pop();
    }

    fn semantic_struct_def(&mut self, struct_def: &mut StructDef) {
        for member in struct_def.member_variants.iter_mut() {
            self.data_type_semantic(&mut member.data_type);
        }
    }

    fn semantic_function_def(&mut self, ast: &SharedAstNode, func_def: &mut FunctionDef) {
        let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));

        func_def.context = Some(BlockContext::new(
            enclosing_scope.as_ref().map(|v| v.shared_unchecked()),
        ));

        let mut scope = Shared::new(Scope::new(enclosing_scope.clone(), ScopeType::Function));
        for variant in func_def.variant_defs.iter() {
            let variable_symbol = Symbol::new_variable_symbol(
                variant.name.clone(),
                scope.weak(),
                variant.data_type.clone(),
                ast.clone(),
            );
            scope.cell_mut().add_symbol(variable_symbol);
        }

        let function_symbol = Symbol::new_function_symbol(
            func_def.func_name.to_string(),
            scope.weak(),
            ast.clone(),
            func_def,
        );

        // let cur_scope = self.scope_stack.last().unwrap().clone();
        enclosing_scope
            .unwrap()
            .shared_unchecked()
            .cell_mut()
            .add_symbol(function_symbol);

        self.scope_stack.push(scope.clone());

        self.semantic(&func_def.body.clone());

        self.data_type_semantic(&mut func_def.return_data_type);

        self.scope_stack.pop();
    }

    fn data_type_semantic(&mut self, data_type: &mut DataType) {
        match data_type.clone() {
            DataType::Unknown(value) => match self.defines.struct_defs.get(&value) {
                Some(sd) => {
                    *data_type = DataType::Struct(sd.weak());
                    return;
                }
                other => {
                    panic!("{:?}", other);
                }
            },
            _ => {}
        }
    }

    pub fn semantic_expr(&mut self, expr: &SharedExpr) {
        let mut expr_mut = expr.cell_mut();
        match &mut expr_mut.expr_node {
            ExprNode::Identifier(Ident { value }) => {
                let _ = match self.find_variable_symbol(value) {
                    Some(v) => match v.get_object() {
                        SymbolObject::Variable { data_type } => {
                            expr_mut.data_type = data_type.clone();
                        }
                        _ => {
                            panic!("identify must be a varient symbol object");
                        }
                    },
                    None => {
                        panic!("variable=[{}] not found", value);
                    }
                };
            }
            ExprNode::Assignment { .. } => expr_mut.data_type = DataType::Unit,
            ExprNode::FuncCall(func_call) => {
                for param in func_call.params.iter() {
                    self.semantic_expr(param);
                }
                expr_mut.data_type = self.funccall_semantic(&self.defines.func_defs, func_call);
            }
            ExprNode::Not { expr } => {
                self.semantic_expr(expr);
                let expr_data_type = expr.cell().data_type.clone();
                if let DataType::Bool = expr_data_type {
                } else {
                    panic!(
                        "`!` must be followed by a bool type, but when it comes to {:?} types",
                        expr_data_type
                    );
                }
                expr_mut.data_type = DataType::Bool;
            }
            ExprNode::Additive { left, right } => {
                self.semantic_expr(left);
                self.semantic_expr(right);

                let left_data_type = left.cell().data_type.clone();
                let right_data_type = right.cell().data_type.clone();
                if left_data_type != right_data_type {
                    panic!(
                        "left data type={:?} != right data type={:?}",
                        left_data_type, right_data_type
                    );
                }
                expr_mut.data_type = left_data_type;
            }
            ExprNode::Eq { left, right }
            | ExprNode::Neq { left, right }
            | ExprNode::Lt { left, right }
            | ExprNode::Gt { left, right } => {
                self.semantic_expr(left);
                self.semantic_expr(right);

                expr_mut.data_type = DataType::Bool;
            }
            ExprNode::Value(Value::Unit) => {
                expr_mut.data_type = DataType::Unit;
            }
            ExprNode::Value(Value::Bool(_)) => {
                expr_mut.data_type = DataType::Bool;
            }
            ExprNode::Value(Value::U32(_)) => {
                expr_mut.data_type = DataType::Int;
            }
            ExprNode::Value(Value::U64(_)) => {
                expr_mut.data_type = DataType::Long;
            }
            ExprNode::Value(Value::String(_)) => {
                expr_mut.data_type = DataType::String;
            }
            ExprNode::IfStatement(is) => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                is.context = Some(IfContext::new(cur_scope.shared_unchecked()));
                self.semantic_expr(&is.if_condition.expr);
                if let DataType::Bool = is.if_condition.expr.cell().data_type {
                } else {
                    panic!(
                        "expect bool type, but found: {:?}",
                        is.if_condition.expr.cell().data_type
                    );
                }
                self.semantic(&is.if_condition.body);
                if is.else_if_conditions.len() > 0 {
                    for cond in is.else_if_conditions.iter() {
                        self.semantic(&cond.body);
                    }
                }
                if let Some(cond) = &is.else_condition {
                    self.semantic(&cond.body);
                };
            }
            ExprNode::WhileStatement(ws) => {
                let cur_scope = self.scope_stack.last_weak_unwrap();
                ws.context = Some(WhileContext::new(cur_scope.shared_unchecked()));
                self.semantic_expr(&ws.condition.expr);
                if let DataType::Bool = ws.condition.expr.cell().data_type {
                } else {
                    panic!(
                        "expect bool type, but found: {:?}",
                        ws.condition.expr.cell().data_type
                    );
                }
                self.semantic(&ws.condition.body);
            }
            ExprNode::PeriodAccess(pa) => {
                let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));
                pa.enclosing_scope = enclosing_scope.as_ref().map(|v| v.shared_unchecked());

                expr_mut.data_type = self.semantic_period_access(pa);
            }
            ExprNode::StructInit(si) => {
                expr_mut.data_type = self.semantic_struct_init(si);
            }
            node => {
                unimplemented!("semantic expr: {:?}", node);
            }
        }
    }

    fn funccall_semantic(
        &self,
        func_def_finder: &impl FunctionDefFinder,
        func_call: &mut FuncCall,
    ) -> DataType {
        if let FuncClassify::BuiltIn(_) = func_call.func_type {
            return DataType::Unit;
        };

        // TODO: semantic params
        let enclosing_scope = self.scope_stack.last().map_or(None, |v| Some(v.weak()));

        func_call.enclosing_scope = enclosing_scope.as_ref().map(|v| v.shared_unchecked());

        let func_def = match func_def_finder.find(&func_call.func_name) {
            Some(func_def) => func_def.clone(),
            None => {
                panic!("not found func:{}", func_call.func_name);
            }
        };

        if func_def.variant_defs.len() != func_call.params.len() {
            panic!(
                "func_def param_len={} != func_call param_len={}",
                func_def.variant_defs.len(),
                func_call.params.len()
            );
        }

        func_def.return_data_type.clone()
    }

    fn semantic_struct_init(&mut self, si: &mut StructInit) -> DataType {
        let struct_define = match self.defines.struct_defs.get(&si.ident.value) {
            Some(def) => def.clone(),
            None => panic!("{} cannot be found from struct-defs", si.ident.value),
        };

        for member in struct_define.cell().member_variants.iter() {
            match si.field_initlist.find(&member.name) {
                Some(field_init) => {
                    self.semantic_expr(&field_init.init_expr);

                    if field_init.init_expr.cell().data_type != member.data_type {
                        panic!(
                            "member = {} in the structure {} is of type {:?}, but the type of the expression when initialized is {:?}",
                            member.name, struct_define.cell().struct_name,
                            member.data_type, field_init.init_expr.cell().data_type);
                    }
                }
                None => {
                    panic!(
                        "the {} field is defined in the structure {}, but not initialized",
                        struct_define.cell().struct_name,
                        member.name,
                    );
                }
            }
        }

        // let struct_name = struct_define.cell().struct_name.to_string();
        DataType::Struct(struct_define.weak())
    }

    fn semantic_period_access_member(
        &mut self,
        first_expr: SharedExpr,
        first_data_type: &DataType,
        slice: &[SharedExpr],
    ) -> DataType {
        let mut parent_data_type = first_data_type.clone();
        let mut out_data_type = parent_data_type.clone();
        let mut parent_expr = first_expr;

        for expr in slice.iter() {
            let mut expr_mut = expr.cell_mut();
            let dt = match &mut expr_mut.expr_node {
                ExprNode::FuncCall(func_call) => match parent_data_type {
                    DataType::Struct(struct_define) => {
                        for param in func_call.params.iter() {
                            self.semantic_expr(&param.clone());
                        }

                        func_call.push_param(parent_expr.clone());

                        let sd = struct_define.shared_unchecked();
                        let member_method_defines = &sd.cell().member_method_defines;
                        self.funccall_semantic(member_method_defines, func_call)
                    }
                    other => panic!("{:?} cannot be used for period access", other),
                },
                ExprNode::Identifier(Ident { value }) => match parent_data_type {
                    DataType::Struct(struct_define) => {
                        let sd = struct_define.shared_unchecked();
                        let member_variants = &sd.cell().member_variants;
                        match member_variants.find(value) {
                            Some(member) => member.data_type.clone(),
                            None => panic!(
                                "period access: field={} cannot be found from struct={}",
                                value,
                                sd.cell().struct_name
                            ),
                        }
                    }
                    other => panic!("{:?} cannot be used for period access", other),
                },
                other => {
                    panic!("not supported point operation followed by {:?} type", other);
                }
            };

            expr_mut.data_type = dt.clone();
            parent_data_type = dt.clone();
            out_data_type = dt;
            parent_expr = expr.clone();
        }

        out_data_type
    }

    fn semantic_period_access(&mut self, pa: &mut PeriodAccess) -> DataType {
        self.semantic_expr(&pa.first_expr);

        if pa.is_empty() {
            return pa.first_expr.cell().data_type.clone();
        }

        self.semantic_period_access_member(
            pa.first_expr.clone(),
            &pa.first_expr.cell().data_type,
            &pa.as_slice(),
        )
    }

    fn find_variable_value_scope(&self, name: &str) -> Option<Weaked<Scope>> {
        let mut cur_scope = match self.scope_stack.last() {
            Some(s) => s.weak(),
            None => {
                return None;
            }
        };
        loop {
            if cur_scope
                .shared_unchecked()
                .cell()
                .variable_symbol_exists(name)
            {
                return Some(cur_scope.clone());
            } else {
                match cur_scope.shared_unchecked().cell().enclosing_scope() {
                    Some(s) => {
                        cur_scope = s.clone();
                    }
                    None => {
                        return None;
                    }
                }
            }
        }

        None
    }

    fn find_variable_symbol(&self, name: &str) -> Option<Symbol> {
        let scope = match self.find_variable_value_scope(name) {
            Some(s) => s,
            None => return None,
        };

        let shared = scope.shared_unchecked();
        let s = shared.cell();
        s.find_variable_symbol(name)
    }

    pub fn root(self) -> Scope {
        self.root_scope
    }

    pub fn new() -> Self {
        Self {
            root_scope: Scope::new(None, ScopeType::Block),
            scope_stack: ScopeStack::new(),
            defines: SemanticDefines::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::*;
    use grammar::*;
    use lexical::*;

    fn semantic_test(s: impl ToString) -> Semantic {
        let mut parser = FromString::new(s.to_string());
        let tokens = parser.parse().unwrap();
        let token_reader = TokenReader::new(tokens);
        let mut parser = Parser::new(token_reader);
        let mut ast_node = parser.parse().unwrap();
        println!("after parser AST: {:#?}", ast_node);

        let mut sem = Semantic::new();
        sem.semantic(&ast_node);

        println!("after semantic AST: {:#?}", ast_node);

        // let root_scope = sem.root();

        sem
    }

    #[test]
    fn intdeclare_semantic_test() {
        semantic_test("int a = 1".to_string());
    }

    #[test]
    fn function_semantic_test() {
        semantic_test("func print() { int a = 1 }".to_string());
    }

    #[test]
    fn return_statement_semantic_test() {
        semantic_test("func print() { int a = 1; return a; }".to_string());
    }

    #[test]
    fn if_statement_semantic_test() {
        semantic_test("if false {} else if true {} else {}".to_string());
    }

    #[test]
    fn eq_expr_semantic_test() {
        semantic_test("1 == 1".to_string());
    }

    #[test]
    fn while_statement_semantic_test() {
        semantic_test(
            r#"
            int a = 1
            while 1 == 1 {
                if a == 1 {
                    println(1)
                }
            }
        "#
            .to_string(),
        );
    }

    #[test]
    fn struct_def_semantic_test() {
        let sem = semantic_test(
            r#"
            struct S1 {
                f3: int,
            }

            struct S {
                f1: int,
                f2: S1,
                name: string,
            }
        "#
            .to_string(),
        );

        println!("{:?}", sem.defines.struct_defs);
    }

    fn complex_struct_define(s: Option<&str>) -> String {
        String::from(
            r#"
            struct Dog {
                name: string,
                age: int,
            }

            struct Cat {
                name: string,
                age: int,
            }

            struct Master {
                dog: Dog,
                cat: Cat
            }

            Dog d = Dog {
                name: "Mr huang",
                age: 5
            }

            Cat c = Cat {
                name: "Mr miao",
                age: 3,
            }

            Master m = Master {
                dog: Dog {
                    name: "Mr huang",
                    age: 5
                },
                cat: c,
            }

        "#,
        ) + match s {
            Some(s) => s,
            None => "",
        }
    }

    #[test]
    fn struct_init_semantic_test() {
        semantic_test(complex_struct_define(None));
    }

    #[test]
    fn period_access_semantic_test() {
        semantic_test(complex_struct_define(Some(
            r#"
                int dog_age = m.dog.age
                int cat_age = m.cat.age

                int sum_if_dog_age_and_cat_age = dog_age + cat_age
                    "#,
        )));
    }

    fn build_struct_and_func_v1(s: Option<&str>) -> String {
        let mut out = complex_struct_define(Some(
            r#"
            impl Dog {
                func say_self(self) {
                    println(self.name)
                }
            }

            impl Master {
                func get_dog(self) -> Dog {
                    self.dog
                }
            }
        "#,
        ));

        if let Some(v) = s {
            out.push_str(v);
        };

        out
    }

    #[test]
    fn add_func_to_impl_statement_semantic_test() {
        semantic_test(build_struct_and_func_v1(None));
    }

    #[test]
    // #[ignore]
    fn struct_member_funccall_semantic_test() {
        semantic_test(build_struct_and_func_v1(Some(
            r#"
            d.say_self()

            m.get_dog().say_self()
        "#,
        )));
    }
}
