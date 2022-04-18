use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::Write;
use std::time::Duration;

use super::calc_result::{CalcResult, CalcValue, StructValue};
use super::stack_frame::{SharedStackFrame, SharedStackFrameOperator, StackFrame};
use grammar::{
    ArrayFinder, AstNode, BlockContext, BuiltInFunc, DataType, ExprNode, FuncCall, FuncClassify,
    Ident, IfStatement, OptionBlockContext, PeriodAccess, ScopeType, SharedAstNode, SharedExpr,
    SharedScope, SharedScopeOptionShower, StructDef, StructInit, TimeUnit, Value, WhileStatement,
};
use semantic::{FunctionDefFinder, SemanticDefines};

use utils::{Shared, SharedCell, Weaked};

pub struct Calculator {
    // stack: Vec<SharedStackFrame>,
    stack: SharedStackFrameOperator,
    defines: Shared<SemanticDefines>,
}

impl Calculator {
    fn calc_expr(&mut self, expr: &SharedExpr) -> CalcResult {
        match &expr.cell().expr_node {
            ExprNode::Identifier(Ident { value }) => match self.find_variable_value(value) {
                Some(v) => v,
                None => {
                    panic!("variable=[{}] not found", value);
                }
            },
            ExprNode::Value(Value::U32(v)) => CalcResult::Value(CalcValue::U32(*v)),
            ExprNode::Value(Value::U64(v)) => CalcResult::Value(CalcValue::U64(*v)),
            ExprNode::Value(Value::Bool(v)) => CalcResult::Value(CalcValue::Bool(*v)),
            ExprNode::Value(Value::String(v)) => CalcResult::Value(CalcValue::String(v.clone())),
            ExprNode::Assignment { left, right } => {
                match &left.cell().expr_node {
                    ExprNode::Identifier(Ident { value }) => {
                        let stack_frame = match self.find_variable(value) {
                            Some(sf) => sf,
                            None => {
                                panic!("assignment: variable={} not found", value);
                            }
                        };
                        let right_value = self.calc_expr(right);
                        stack_frame
                            .shared_unchecked()
                            .cell_mut()
                            .update_variable(value, right_value);
                    }
                    other => {
                        unimplemented!("{:?}", other);
                    }
                }

                CalcResult::Unit
            }
            ExprNode::FuncCall(func_call) => match &func_call.func_type {
                FuncClassify::Custom => {
                    let defines = self.defines.clone();
                    let func_defs = &defines.cell().func_defs;
                    self.custom_func_call(func_defs, func_call)
                }
                FuncClassify::BuiltIn(builtin_func) => {
                    self.builtin_func_call(builtin_func.clone(), func_call)
                }
            },
            ExprNode::Additive { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                l + r
            }
            ExprNode::Eq { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                CalcResult::Value(CalcValue::Bool(l == r))
            }
            ExprNode::Neq { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                CalcResult::Value(CalcValue::Bool(l != r))
            }
            ExprNode::Lt { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                CalcResult::Value(CalcValue::Bool(l < r))
            }
            ExprNode::Gt { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                CalcResult::Value(CalcValue::Bool(l > r))
            }
            ExprNode::Not { expr } => match self.calc_expr(expr) {
                CalcResult::Value(CalcValue::Bool(b)) => CalcResult::Value(CalcValue::Bool(!b)),
                other => {
                    panic!(
                        "not must be followed by a bool type, but is given as {:?}",
                        other
                    );
                }
            },
            ExprNode::Value(Value::Unit) => CalcResult::Unit,
            ExprNode::IfStatement(if_statement) => self.handle_if(if_statement),
            ExprNode::WhileStatement(while_statement) => self.handle_while(while_statement),
            ExprNode::StructInit(struct_init) => {
                let sd = match &expr.cell().data_type {
                    DataType::Struct(sd) => sd.clone(),
                    other => panic!(
                        "in the initialization of a structure, the data type must be a structure, not {:?}",
                        other),
                };
                self.handle_struct_init(sd, struct_init)
            }
            ExprNode::PeriodAccess(period_access) => self.handle_period_access(period_access),
            _ => {
                unimplemented!("not support {:?}", expr);
            }
        }
    }

    fn calc_block(
        &mut self,
        context: &Option<BlockContext>,
        nodes: &Vec<SharedAstNode>,
    ) -> CalcResult {
        let enclosing_scope = OptionBlockContext::new(context).enclosing_scope_clone();

        #[cfg(feature = "debug")]
        println!(
            "calc block, enclosing scope={:?}",
            SharedScopeOptionShower::new(&enclosing_scope).get_scope_type()
        );

        let stack_frame = SharedStackFrame::new(StackFrame::new(enclosing_scope));
        self.stack.push(stack_frame);

        if nodes.len() > 1 {
            for i in 0..nodes.len() - 1 {
                self.calc(&nodes[i]);
            }
        }

        self.calc(nodes.last().as_ref().unwrap())
    }

    fn calc_func_block(
        &mut self,
        context: &Option<BlockContext>,
        nodes: &Vec<SharedAstNode>,
    ) -> CalcResult {
        let enclosing_scope = OptionBlockContext::new(context).enclosing_scope_clone();
        #[cfg(feature = "debug")]
        println!(
            "calc func block, enclosing scope={:?}",
            SharedScopeOptionShower::new(&enclosing_scope).get_scope_type()
        );

        let stack_frame = SharedStackFrame::new(StackFrame::new(enclosing_scope));
        self.stack.push(stack_frame);

        let mut interrupt_result = None;

        if nodes.len() > 1 {
            for i in 0..nodes.len() - 1 {
                let value = self.calc(&nodes[i]);
                if let CalcResult::Return(return_data) = value {
                    interrupt_result = Some(return_data);
                    break;
                };
            }
        }

        match interrupt_result {
            Some(r) => *r,
            None => {
                let calc_result = self.calc(nodes.last().as_ref().unwrap());
                if let CalcResult::Return(return_data) = calc_result {
                    return *return_data;
                }

                self.stack.pop();

                calc_result
            }
        }
    }

    fn calc_loop_block(
        &mut self,
        context: &Option<BlockContext>,
        nodes: &Vec<SharedAstNode>,
    ) -> CalcResult {
        let enclosing_scope = OptionBlockContext::new(context).enclosing_scope_clone();
        #[cfg(feature = "debug")]
        println!(
            "calc loop block, enclosing scope={:?}",
            SharedScopeOptionShower::new(&enclosing_scope).get_scope_type()
        );

        let stack_frame = SharedStackFrame::new(StackFrame::new(enclosing_scope));
        self.stack.push(stack_frame);

        let mut interrupt_result = None;

        if nodes.len() > 1 {
            for i in 0..nodes.len() - 1 {
                let value = self.calc(&nodes[i]);
                if matches!(&value, CalcResult::Break | CalcResult::Continue) {
                    interrupt_result = Some(value);
                    break;
                }
            }
        }

        match interrupt_result {
            Some(r) => r,
            None => {
                let calc_result = self.calc(nodes.last().as_ref().unwrap());
                if !matches!(&calc_result, CalcResult::Break | CalcResult::Continue) {
                    self.stack.pop();
                }

                calc_result
            }
        }
    }

    fn calc(&mut self, ast_node: &SharedAstNode) -> CalcResult {
        match &*ast_node.cell() {
            AstNode::MainBlock { nodes, context }
            | AstNode::Block { nodes, context }
            | AstNode::CondBlock { nodes, context } => {
                #[cfg(feature = "debug")]
                println!(
                    "calc, scope type={:?}",
                    SharedScopeOptionShower::new(context.as_ref().unwrap().enclosing_scope())
                        .get_scope_type()
                );

                if nodes.len() == 0 {
                    return CalcResult::Unit;
                }

                self.calc_block(context, nodes)
            }
            AstNode::FuncBlock { nodes, context } => {
                #[cfg(feature = "debug")]
                println!(
                    "calc function block, scope type={:?}",
                    SharedScopeOptionShower::new(context.as_ref().unwrap().enclosing_scope())
                        .get_scope_type()
                );

                if nodes.len() == 0 {
                    return CalcResult::Unit;
                }

                self.calc_func_block(context, nodes)
            }
            AstNode::LoopBlock { nodes, context } => {
                #[cfg(feature = "debug")]
                println!(
                    "calc loop block, scope type={:?}",
                    SharedScopeOptionShower::new(context.as_ref().unwrap().enclosing_scope())
                        .get_scope_type()
                );

                if nodes.len() == 0 {
                    return CalcResult::Unit;
                }

                self.calc_loop_block(context, nodes)
            }
            AstNode::Indeclaration {
                identifier,
                expr,
                context,
            } => {
                let value = match expr {
                    Some(e) => self.calc_expr(e),
                    None => CalcResult::Unknown,
                };
                self.stack
                    .cell()
                    .last()
                    .unwrap()
                    .borrow_mut()
                    .add_variable(identifier.to_string(), value);
                // context.as_ref().unwrap().enclosing_scope.as_ref().borrow_mut();

                CalcResult::Unit
            }
            AstNode::CustomTypeObjectDeclaration(ctod) => {
                let value = ctod
                    .expr
                    .as_ref()
                    // Option::map_or
                    .map_or(CalcResult::Unknown, |v| self.calc_expr(v));

                self.stack
                    .cell()
                    .last()
                    .unwrap()
                    .borrow_mut()
                    .add_variable(ctod.object_name.to_string(), value);

                CalcResult::Unit
            }
            AstNode::ImplStatement(_) => CalcResult::Unit,
            AstNode::FunctionDef(_) => CalcResult::Unit,
            AstNode::MethodDef(_) => CalcResult::Unit,
            AstNode::StructDef(_) => CalcResult::Unit,
            AstNode::ReturnStatement(rs) => {
                CalcResult::Return(Box::new(self.calc_expr(&rs.return_expr)))
            }
            AstNode::BreakStatement(bs) => {
                self.handle_break();

                CalcResult::Break
            }
            AstNode::ContinueStatement(cs) => {
                self.handle_continue();

                CalcResult::Continue
            }
            AstNode::Expr(e) => self.calc_expr(e),
            _ => {
                unimplemented!("not support {:?}", ast_node);
            }
        }
    }

    fn find_loop_scope(&self) -> bool {
        match self.stack.cell().last() {
            Some(s) => match s.cell().scope() {
                Some(scope) => match scope.shared_unchecked().cell().scope_type() {
                    ScopeType::Loop => {
                        #[cfg(feature = "debug")]
                        println!("ScopeType::loop");
                        true
                    }
                    other => {
                        #[cfg(feature = "debug")]
                        println!("other {:?}", other);
                        false
                    }
                },
                None => {
                    unreachable!();
                }
            },
            None => {
                unreachable!();
            }
        }
    }

    fn handle_break(&mut self) {
        loop {
            let is_break = self.find_loop_scope();
            self.stack.pop();

            if is_break {
                break;
            }
        }
    }

    fn handle_continue(&mut self) {
        loop {
            let is_break = self.find_loop_scope();

            if is_break {
                break;
            }

            self.stack.pop();
        }
    }

    fn handle_if(&mut self, if_statement: &IfStatement) -> CalcResult {
        let if_condition_calc_result = self.calc_expr(&if_statement.if_condition.expr);
        if if_condition_calc_result.is_true() {
            return self.calc(&if_statement.if_condition.body);
        }

        for cond in if_statement.else_if_conditions.iter() {
            let else_if_condition_calc_result = self.calc_expr(&cond.expr);
            if else_if_condition_calc_result.is_true() {
                return self.calc(&cond.body);
            }
        }

        if let Some(cond) = &if_statement.else_condition {
            return self.calc(&cond.body);
        };

        CalcResult::Unit
    }

    fn handle_while(&mut self, while_statement: &WhileStatement) -> CalcResult {
        let mut condition_calc_result = self.calc_expr(&while_statement.condition.expr);

        let mut calc_result = CalcResult::Unit;

        while condition_calc_result.is_true() {
            condition_calc_result = self.calc_expr(&while_statement.condition.expr);

            #[cfg(feature = "debug")]
            println!("before execute while body");
            calc_result = self.calc(&while_statement.condition.body);

            if let CalcResult::Break = calc_result {
                break;
            };

            if let CalcResult::Continue = calc_result {
                continue;
            };
        }

        calc_result
    }

    fn handle_struct_init(
        &mut self,
        struct_def: Weaked<StructDef>,
        struct_init: &StructInit,
    ) -> CalcResult {
        let mut fields = HashMap::new();
        for field_init in struct_init.field_initlist.iter() {
            let expr_value = self.calc_expr(&field_init.init_expr);
            fields.insert(field_init.field_name.to_string(), Box::new(expr_value));
        }

        StructValue::new(fields, struct_def).into()
    }

    fn handle_period_access(&mut self, period_access: &PeriodAccess) -> CalcResult {
        let mut calc_result = self.calc_expr(&period_access.first_expr);

        for expr in period_access.exprs.iter() {
            match calc_result {
                CalcResult::Value(CalcValue::Struct(struct_value)) => {
                    match &expr.cell().expr_node {
                        fc @ ExprNode::FuncCall(func_call) => {
                            let sd = struct_value.struct_def.shared_unchecked();
                            let member_method_defines = &sd.cell().member_method_defines;
                            calc_result = self.custom_func_call(member_method_defines, func_call);
                        }
                        ExprNode::Identifier(Ident { value }) => {
                            calc_result = Box::into_inner(struct_value.find_member(value));
                        }
                        other => {
                            panic!("not support {:?}", other);
                        }
                    }
                }
                other => {
                    panic!(
                        "period access The first result of an expression may not be = {:?}",
                        other
                    );
                }
            }
        }

        calc_result
    }

    fn builtin_func_call(&mut self, builtin_func: BuiltInFunc, func_call: &FuncCall) -> CalcResult {
        match builtin_func {
            BuiltInFunc::Println => {
                for param in &func_call.params {
                    let value = self.calc_expr(param);
                    std::io::stdout().write(value.to_string().as_bytes());
                    std::io::stdout().write(&[' ' as u8]);
                }
                std::io::stdout().write(&['\n' as u8]);
                std::io::stdout().flush();

                CalcResult::Unit
            }
            BuiltInFunc::Sleep => {
                let param_len = func_call.params.len();
                if param_len != 1 {
                    panic!(
                        "the sleep function expects one argument, but gives {} one argument",
                        param_len
                    );
                }

                let duration = match &func_call.params[0].cell().expr_node {
                    // AstNode::Expr(expr) => match &expr.cell().expr_node {
                    ExprNode::Value(v) => match v {
                        Value::TimeLiteral(time, time_unit) => match time_unit {
                            TimeUnit::Second => Duration::from_secs(*time),
                            TimeUnit::Millisecond => Duration::from_millis(*time),
                            TimeUnit::Microsecond => Duration::from_micros(*time),
                        },
                        Value::U64(time) => Duration::from_secs(*time),
                        Value::U32(time) => Duration::from_secs((*time).into()),
                        other => {
                            panic!("expect time literal, but meet {:?}", other);
                        }
                    },
                    other => {
                        panic!("expect value, but meet {:?}", other);
                    } // },
                      // other => {
                      //     panic!("expect expr, but meet {:?}", other);
                      // }
                };

                std::thread::sleep(duration);

                CalcResult::Unit
            }
            _ => {
                panic!("{} is not builtin func", func_call.func_name);
            }
        }
    }

    fn custom_func_call(
        &mut self,
        finder: &impl FunctionDefFinder,
        func_call: &FuncCall,
    ) -> CalcResult {
        let func_def = match finder.find(&func_call.func_name) {
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

        let mut param_values = Vec::new();
        for param in &func_call.params {
            let value = self.calc_expr(param);
            param_values.push(value);
        }

        // create stack_frame
        let enclosing_scope = match &func_call.enclosing_scope {
            Some(scope) => Some(scope.clone()),
            None => None,
        };

        let stack_frame =
            SharedStackFrame::new(StackFrame::new(enclosing_scope.as_ref().map(|v| v.weak())));
        // self.push_stack_frame(stack_frame);
        let mut pusher = self.stack.pusher();
        pusher.push(stack_frame);

        // bind
        for (i, param) in func_def.variant_defs.iter().enumerate() {
            self.stack
                .cell()
                .last()
                .unwrap()
                .borrow_mut()
                .add_variable(param.name.to_string(), param_values[i].clone());
        }

        // execute funtion
        let value = self.calc(&func_def.body);

        value
    }

    fn find_variable_inner(
        &self,
        stack_frame: Weaked<StackFrame>,
        name: &str,
    ) -> Option<Weaked<StackFrame>> {
        match stack_frame
            .shared_unchecked()
            .cell()
            .get_variable_value(name)
        {
            Some(v) => return Some(stack_frame.clone()),
            None => match stack_frame
                .shared_unchecked()
                .cell()
                .get_enclosing_scope_frame()
            {
                Some(s) => {
                    return self.find_variable_inner(s, name);
                }
                None => return None,
            },
        }
    }

    pub fn find_variable(&self, name: &str) -> Option<Weaked<StackFrame>> {
        let mut sf = match self.stack.cell().last() {
            Some(s) => s.weak(),
            None => return None,
        };
        self.find_variable_inner(sf, name)
    }

    fn find_variable_value_inner(
        &self,
        stack_frame: Weaked<StackFrame>,
        name: &str,
    ) -> Option<CalcResult> {
        match stack_frame
            .shared_unchecked()
            .cell()
            .get_variable_value(name)
        {
            Some(v) => return Some(v),
            None => match stack_frame
                .shared_unchecked()
                .cell()
                .get_enclosing_scope_frame()
            {
                Some(s) => {
                    return self.find_variable_value_inner(s, name);
                }
                None => return None,
            },
        }
    }

    pub fn find_variable_value(&self, name: &str) -> Option<CalcResult> {
        let mut sf = match self.stack.cell().last() {
            Some(s) => s.weak(),
            None => return None,
        };
        self.find_variable_value_inner(sf, name)
    }

    pub fn new(defines: SemanticDefines) -> Self {
        Self {
            // stack: Vec::new(),
            stack: SharedStackFrameOperator::new(),
            defines: Shared::new(defines),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grammar::*;
    use lexical::*;
    use semantic::*;

    fn get_ast(statement: String) -> (SharedAstNode, SemanticDefines) {
        println!("{}", "-".repeat(20));
        let mut parser = FromString::new(statement);
        let tokens = parser.parse().unwrap();
        let token_reader = TokenReader::new(tokens);
        let mut parser = Parser::new(token_reader);
        let ast_node = parser.parse().unwrap();
        println!("after grammer parser: {:#?}", ast_node);
        let mut format_str = String::new();
        ast_node.build_format(0, &mut format_str);
        println!("{}", &format_str);
        println!("{}", "-".repeat(20));

        let mut sem = Semantic::new();
        sem.semantic(&ast_node);

        println!("after semantic parser: {:#?}", ast_node);

        (ast_node, sem.defines)
    }

    fn calculator_with_semantic_test(s: impl ToString) {
        let (ast_node, defines) = get_ast(s.to_string());

        let mut c = Calculator::new(defines);
        c.calc(&ast_node);
    }

    #[test]
    fn calculator_with_semantic_variable_store_test() {
        calculator_with_semantic_test("int a = 1".to_string());
    }

    #[test]
    fn calculator_with_semantic_variable_load_simple_test() {
        calculator_with_semantic_test("int a = 1 a".to_string());
    }

    #[test]
    fn calculator_with_semantic_variable_load_test() {
        calculator_with_semantic_test("int a = 1 { a = 2 a } ".to_string());
    }

    #[test]
    fn calculator_with_semantic_variable_find_simple_test() {
        calculator_with_semantic_test("int a = 1 a".to_string());
    }

    #[test]
    fn calculator_with_semantic_variable_find_test() {
        calculator_with_semantic_test("int a = 1 { a = 2 a } ".to_string());
    }

    #[test]
    fn calculator_with_semantic_func_call_no_param_test() {
        calculator_with_semantic_test("func get() { int a = 1 a } get() ".to_string());
    }

    #[test]
    fn calculator_with_semantic_func_call_with_param_test() {
        calculator_with_semantic_test("func get(int a, int b) { a+b } get(100, 200) ".to_string());
    }

    #[test]
    fn calculator_with_semantic_return_test() {
        calculator_with_semantic_test(
            r#"
            func get(int a, int b) {
                return a+b
            }
            println("-- return_test --:", get(100, 200))
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_return_pro_test() {
        calculator_with_semantic_test(
            r#"
            func max(int a, int b) {
                if a > b {
                    return a
                }

                b
            }
            println("-- return_pro_test --:", max(100, 200))
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_println_test() {
        calculator_with_semantic_test(r#"println("-- println_test --:", 1) "#.to_string());
    }

    #[test]
    fn calculator_with_semantic_println_with_string_test() {
        calculator_with_semantic_test(
            r#"println("-- println_with_string_test --: value =", 1)"#.to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_println_advanced_test() {
        calculator_with_semantic_test(
           r#"
           func get(int a, int b) { return a+b } println("-- println_advanced_test --:", get(100, 200))
           "#.to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_eq_test() {
        calculator_with_semantic_test(
            r#"
            println("-- eq_test --:", 1 == 1)
            println("-- eq_test --:", 1 == 0)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_neq_test() {
        calculator_with_semantic_test(
            r#"
            println("-- neq_test --:", 1 != 1)
            println("-- neq_test --:", 1 != 0)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_if_simple_test() {
        calculator_with_semantic_test(
            r#"
            if true {
                println("-- if_simple_test --:", 1)
            }
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_if_eq_test() {
        calculator_with_semantic_test(
            r#"
            int i = 10;
            if i == 10 {
                println("-- if_eq_test --:", 1)
            }
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_if_expr_test() {
        calculator_with_semantic_test(
            r#"
            int v = if true {
                1
            }

            println("-- if_expr_test --:", v)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_if_else_test() {
        calculator_with_semantic_test(
            r#"
            int v = if false {
                1
            } else if false {
                2
            } else if true {
                3
            } else {
                4
            }

            println("-- if_else_test --:", v)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_if_expr_condition_test() {
        calculator_with_semantic_test(
            r#"
            int v = if 1 == 1 {
                1
            }

            println("-- if_expr_condition_test --:", v)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_sleep_test() {
        calculator_with_semantic_test(r#"sleep(0)"#.to_string());
    }

    #[test]
    fn calculator_with_semantic_not_operator_test() {
        calculator_with_semantic_test(r#"println("-- not_operator_test --:", !false)"#.to_string());
    }

    #[test]
    fn calculator_with_semantic_while_expr_test() {
        calculator_with_semantic_test(
            r#"
            int i = 0
            while i != 10 {
                i = i + 1
            }
            println("-- while_expr_test --:", i)
        "#
            .to_string(),
        );
    }

    #[test]
    // #[ignore]
    fn calculator_with_semantic_break_test() {
        calculator_with_semantic_test(
            r#"
            int i = 0
            while true {
                if i == 10 {
                    break
                }
                i = i + 1
            }
            println("-- break_test --:", i)
        "#
            .to_string(),
        );
    }

    #[test]
    // #[ignore]
    fn calculator_with_semantic_continue_test() {
        calculator_with_semantic_test(
            r#"
            int i = 0
            while true {
                if i < 10 {
                    i = i + 1
                    continue
                }

                break
            }
            println("-- continue --:", i)
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_struct_init_test() {
        calculator_with_semantic_test(
            r#"
            struct Dog {
                name: string,
                age: int
            }

            println("-- struct_init_test --:", Dog{
                name: "huang"
                age: 5,
            })
        "#
            .to_string(),
        );
    }

    #[test]
    fn calculator_with_semantic_custom_type_object_declare_test() {
        calculator_with_semantic_test(
            r#"
            struct Dog {
                name: string,
                age: int
            }

            let dog = Dog{
                name: "Mr huang"
                age: 5,
            };

            println("-- custom_type_object_declare_test --:", dog)
        "#
            .to_string(),
        );
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
    fn calculator_with_semantic_period_access_test() {
        calculator_with_semantic_test(complex_struct_define(Some(
            r#"
                println("-- period_access_test --, d.name:", d.name)
                println("-- period_access_test --, m.cat.name:", m.cat.name)
                    "#,
        )));
    }

    #[test]
    fn calculator_with_semantic_method_call_test() {
        calculator_with_semantic_test(complex_struct_define(Some(
            r#"
                impl Master {
                    func print_master_dog_name(self) {
                        println("-- method_call_test --, self.dog.name:", self.dog.name)
                    }

                    func get_dog(self) -> Dog {
                        return self.dog
                    }
                }

                m.print_master_dog_name()
            "#,
        )));
    }
}
