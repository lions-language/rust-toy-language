use super::{
    AstNode, BreakStatement, BuiltInFunc, ConditionBlock, ContinueStatement,
    CustomTypeObjectDeclaration, DataType, Expr, ExprNode, FuncCall, FuncClassify, FuncNameType,
    FunctionDef, FunctionType, Ident, IfStatement, ImplStatement, MethodDef, MethodType,
    PeriodAccess, ReturnStatement, SharedAstNode, SharedCell, SharedExpr, StructDef,
    StructFieldInit, StructInit, TimeUnit, Value, VariantDef, WhileStatement,
};
use crate::*;
use std::cell::RefCell;
use std::rc::Rc;

macro_rules! binary_operator {
    ($func_name:ident, $next_func_name:ident, $(($token:ident, $node:ident)),*) => {
        fn $func_name(&mut self) -> GrammarResult<SharedExpr> {
            let mut left_node = self.$next_func_name()?;
            match self.token_reader.peek() {
                $(
                Token::$token => {
                    self.token_reader.consume();
                    let right_node = self.$next_func_name()?;
                    Ok(SharedExpr::new_without_data_type(ExprNode::$node {
                        left: left_node,
                        right: right_node
                    }))
                },
                    )*
                _ => {
                    Ok(left_node)
                }
            }
        }
    };
    ($func_name:ident, $next_func_name:ident, $($node:ident),*) => {
        binary_operator!($func_name, $next_func_name, $(($node, $node)),*);
    };
}

macro_rules! unary_prefix_operator {
    ($func_name:ident, $next_func_name:ident, $($token:ident),*) => {
        fn $func_name(&mut self) -> GrammarResult<SharedExpr> {
            match self.token_reader.peek() {
                $(
                Token::$token => {
                    self.token_reader.consume();
                    Ok(SharedExpr::new_without_data_type(ExprNode::$token{
                        expr: self.$func_name()?,
                    }))
                },
                other => {
                    self.$next_func_name()
                }
                )*
            }
        }
    };
}

macro_rules! build_block {
    ($func_name:ident, $block_name:ident) => {
        pub fn $func_name(&mut self) -> GrammarResult<SharedAstNode> {
            let nodes = self.parse_block()?;

            Ok(SharedAstNode::new(AstNode::$block_name {
                nodes: nodes,
                context: None,
            }))
        }
    };
}

pub struct Parser {
    token_reader: TokenReader,
}

impl Parser {
    pub fn parse(&mut self) -> GrammarResult<SharedAstNode> {
        let mut nodes = Vec::<SharedAstNode>::new();

        loop {
            if let Token::EOF = self.token_reader.peek() {
                break;
            };

            nodes.push(self.statement_parse()?);
        }

        Ok(SharedAstNode::new(AstNode::MainBlock {
            nodes: nodes,
            context: None,
        }))
    }

    pub fn statement_parse(&mut self) -> GrammarResult<SharedAstNode> {
        match self.token_reader.peek() {
            Token::SemiColon => {
                self.token_reader.consume();
                Ok(SharedAstNode::new_expr(SharedExpr::new_without_data_type(
                    ExprNode::Value(Value::Unit),
                )))
            }
            Token::Word(Word {
                keyword: Keyword::Int,
                value,
            }) => self.int_declare(),
            Token::Word(Word {
                keyword: Keyword::Func,
                ..
            }) => self.parse_func(),
            Token::Word(Word {
                keyword: Keyword::Struct,
                ..
            }) => self.struct_define(),
            Token::Word(Word {
                keyword: Keyword::Return,
                ..
            }) => self.return_statement(),
            Token::Word(Word {
                keyword: Keyword::Break,
                ..
            }) => self.break_statement(),
            Token::Word(Word {
                keyword: Keyword::Continue,
                ..
            }) => self.continue_statement(),
            Token::Word(Word {
                keyword: Keyword::Impl,
                ..
            }) => self.impl_statement(),
            Token::LBrace => self.block(),
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            }) => {
                let type_name = value;
                match self.token_reader.peek_n(1) {
                    Token::Word(Word {
                        keyword: Keyword::NoKeyword,
                        value,
                    }) => {
                        let object_name = value;

                        // consume class name
                        self.token_reader.consume();
                        // consume object name
                        self.token_reader.consume();

                        self.parse_custom_type_object_declaration(type_name, object_name)
                    }
                    _ => Ok(SharedAstNode::new_expr(self.parse_expr()?)),
                }
            }
            Token::EOF => {
                panic!("expect a statement, but an EOF is reached, the EOF should be checked externally");
            }
            v => Ok(SharedAstNode::new_expr(self.parse_expr()?)),
        }
    }

    fn get_func_type(&self, func_name: &str) -> GrammarResult<FuncClassify> {
        match func_name {
            "println" => Ok(FuncClassify::BuiltIn(BuiltInFunc::Println)),
            "sleep" => Ok(FuncClassify::BuiltIn(BuiltInFunc::Sleep)),
            _ => Ok(FuncClassify::Custom),
        }
    }

    fn parse_custom_type_object_declaration(
        &mut self,
        type_name: String,
        object_name: String,
    ) -> GrammarResult<SharedAstNode> {
        let mut expr = None;

        // read =
        if let Token::Assignment = self.token_reader.peek() {
            self.token_reader.consume();
            expr = Some(self.parse_expr()?);
        }

        Ok(SharedAstNode::new(AstNode::CustomTypeObjectDeclaration(
            CustomTypeObjectDeclaration::new(type_name, object_name, expr, None),
        )))
    }

    fn parse_period_access(&mut self, first_expr: SharedExpr) -> GrammarResult<SharedExpr> {
        let mut exprs: Vec<SharedExpr> = Vec::new();

        while let Token::Period = self.token_reader.peek() {
            // consume `.`
            self.token_reader.consume();

            exprs.push(self.parse_single_expr()?);
        }

        Ok(SharedExpr::new_without_data_type(ExprNode::PeriodAccess(
            PeriodAccess {
                first_expr: first_expr,
                exprs: exprs,
                enclosing_scope: None,
            },
        )))
    }

    fn parse_funcall(&mut self) -> GrammarResult<SharedExpr> {
        let func_name = self
            .peek_nokeyword()
            .expect("before calling this function, it must have been determined that");

        // consume func_name
        self.token_reader.consume();
        // consume `(`
        self.token_reader.consume();

        let mut params: Vec<SharedExpr> = Vec::new();

        let func_type = self.get_func_type(&func_name)?;

        loop {
            match self.token_reader.peek() {
                Token::RParen => {
                    self.token_reader.consume();
                    break;
                }
                Token::EOF => {
                    return Err(format!(
                        "expect right parentheses or other expressions, but arrive at EOF"
                    ));
                }
                Token::Comma => {
                    self.token_reader.consume();
                    continue;
                }
                _ => {
                    params.push(self.parse_expr()?);
                }
            }
        }

        Ok(SharedExpr::new_without_data_type(ExprNode::FuncCall(
            FuncCall {
                func_type: func_type,
                func_name: func_name,
                params: params,
                enclosing_scope: None,
            },
        )))
    }

    fn parse_struct_init(&mut self) -> GrammarResult<SharedExpr> {
        let ident = self.parse_ident()?;

        // consume `{`
        self.token_reader.consume();

        let mut field_initlist: Vec<StructFieldInit> = Vec::new();

        loop {
            match self.token_reader.peek() {
                Token::RBrace => {
                    self.token_reader.consume();
                    break;
                }
                Token::EOF => {
                    return Err(format!(
                        "expect right parentheses or other expressions, but arrive at EOF"
                    ));
                }
                Token::Comma => {
                    self.token_reader.consume();
                    continue;
                }
                _ => {
                    field_initlist.push(self.parse_struct_field_init()?);
                }
            }
        }

        Ok(SharedExpr::new_with_name_data_type(
            ident.value.clone(),
            ExprNode::StructInit(StructInit {
                ident: ident,
                field_initlist: field_initlist,
                context: None,
            }),
        ))
    }

    fn parse_struct_field_init(&mut self) -> GrammarResult<StructFieldInit> {
        let ident = self.parse_ident()?;

        self.expect_token(&Token::Colon)?;

        Ok(StructFieldInit::new(ident.value, self.parse_expr()?))
    }

    fn parse_expr(&mut self) -> GrammarResult<SharedExpr> {
        let expr = self.parse_single_expr()?;

        match self.token_reader.peek() {
            Token::Period => self.parse_period_access(expr),
            _ => Ok(expr),
        }
    }

    fn parse_single_expr(&mut self) -> GrammarResult<SharedExpr> {
        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            }) => match self.token_reader.peek_n(1) {
                Token::LParen => self.parse_funcall(),
                Token::LBrace => self.parse_struct_init(),
                _ => self.assignment(),
            },
            Token::Word(Word {
                keyword: Keyword::If,
                ..
            }) => self.if_statement(),
            Token::Word(Word {
                keyword: Keyword::While,
                ..
            }) => self.while_statement(),
            Token::Word(Word {
                keyword: Keyword::For,
                ..
            }) => self.for_statement(),
            _ => self.assignment(),
        }
    }

    pub fn parse_func_type(&mut self) -> GrammarResult<DataType> {
        // consume `func`
        self.token_reader.consume();

        match self.token_reader.peek() {
            Token::LParen => {
                // params
                self.expect_token(&Token::LParen)?;
                let param_data_types =
                    self.parse_function_param_list(|data_type, _| Box::new(data_type))?;
                self.expect_token(&Token::RParen)?;

                // return
                let return_data_type = self.parse_function_return_type()?;

                Ok(DataType::Function(FunctionType {
                    param_data_types: param_data_types,
                    return_data_type: Box::new(return_data_type),
                    func_define: None,
                }))
            }
            other => Err(format!("expected: ident / lparen, but found: {:?}", other)),
        }
    }

    fn parse_type(&mut self) -> GrammarResult<DataType> {
        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::Int,
                ..
            }) => {
                self.token_reader.consume();
                Ok(DataType::Int)
            }
            Token::Word(Word {
                keyword: Keyword::Double,
                ..
            }) => {
                self.token_reader.consume();
                Ok(DataType::Double)
            }
            Token::Word(Word {
                keyword: Keyword::Long,
                ..
            }) => {
                self.token_reader.consume();
                Ok(DataType::Long)
            }
            Token::Word(Word {
                keyword: Keyword::String,
                ..
            }) => {
                self.token_reader.consume();
                Ok(DataType::String)
            }
            Token::StringLiteral(_) => {
                self.token_reader.consume();
                Ok(DataType::String)
            }
            Token::Word(Word {
                keyword: Keyword::Func,
                ..
            }) => self.parse_func_type(),
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            }) => {
                self.token_reader.consume();
                Ok(DataType::Unknown(value))
            }
            _ => Err(format!(
                "unresolvable type = {:?}",
                self.token_reader.peek()
            )),
        }
    }

    fn peek_nokeyword(&mut self) -> GrammarResult<String> {
        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            }) => Ok(value),
            _ => Err(format!(
                "expect nokeyword, but found {:?}",
                self.token_reader.peek()
            )),
        }
    }

    fn take_nokeyword(&mut self) -> GrammarResult<String> {
        let nokeyword = self.peek_nokeyword()?;

        self.token_reader.consume();

        Ok(nokeyword)
    }

    fn parse_ident(&mut self) -> GrammarResult<Ident> {
        let nokeyword = self.peek_nokeyword()?;

        self.token_reader.consume();

        Ok(Ident { value: nokeyword })
    }

    fn parse_function_param<R, F>(&mut self, f: &F) -> GrammarResult<R>
    where
        F: Fn(DataType, Option<Ident>) -> R,
    {
        let data_type = self.parse_type()?;

        let mut ident = None;
        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                ..
            }) => {
                ident = Some(self.parse_ident()?);
            }
            _ => {}
        }

        Ok(f(data_type, ident))
    }

    fn parse_function_param_variant(&mut self) -> GrammarResult<VariantDef> {
        self.parse_function_param(&|data_type, ident: Option<Ident>| VariantDef {
            data_type: data_type,
            name: ident.unwrap().value,
        })
    }

    fn parse_function_param_list_variant(&mut self) -> GrammarResult<Vec<VariantDef>> {
        self.parse_function_param_list(|data_type, ident| VariantDef {
            data_type: data_type,
            name: ident.unwrap().value,
        })
    }

    fn parse_function_param_list<F, R>(&mut self, f: F) -> GrammarResult<Vec<R>>
    where
        F: Fn(DataType, Option<Ident>) -> R,
    {
        if self.expected(&Token::RParen) {
            // ) => end
            return Ok(Vec::new());
        }

        let mut rs = Vec::new();
        loop {
            let r = self.parse_function_param(&f)?;
            rs.push(r);

            if self.expected(&Token::RParen) {
                break;
            }

            if self.expected(&Token::Comma) {
                self.token_reader.consume();
            } else {
                return Err(format!(
                    "expected: comma / rparen, but found: {:?}",
                    self.token_reader.peek()
                ));
            }
        }

        Ok(rs)
    }

    fn parse_function_return_type(&mut self) -> GrammarResult<DataType> {
        if !self.expected(&Token::Arrow) {
            return Ok(DataType::Unit);
        }

        self.token_reader.consume();
        let t = self.parse_type()?;

        Ok(t)
    }

    fn parse_function_name(&mut self) -> GrammarResult<String> {
        if let Token::Word(Word {
            keyword: Keyword::NoKeyword,
            value,
        }) = self.token_reader.peek()
        {
            self.token_reader.consume();
            Ok(value)
        } else {
            Err(format!(
                "the func keyword should be followed by an identify"
            ))
        }
    }

    pub fn parse_func(&mut self) -> GrammarResult<SharedAstNode> {
        // consume `func`
        self.token_reader.consume();

        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            }) => self.named_function_define(),
            Token::LParen => self.parse_func_lparen(),
            other => Err(format!("expected: ident / lparen, but found: {:?}", other)),
        }
    }

    pub fn parse_func_lparen(&mut self) -> GrammarResult<SharedAstNode> {
        // params
        self.expect_token(&Token::LParen)?;
        let variant_defs = self.parse_function_param_list_variant()?;
        self.expect_token(&Token::RParen)?;

        // return
        let return_data_type = self.parse_function_return_type()?;

        match self.token_reader.peek() {
            Token::LBrace => self.anonymous_function_define(variant_defs, return_data_type),
            _ => self.parse_func_type_declare(variant_defs, return_data_type),
        }
    }

    pub fn parse_func_type_declare(
        &mut self,
        variant_defs: Vec<VariantDef>,
        return_data_type: DataType,
    ) -> GrammarResult<SharedAstNode> {
        todo!();
    }

    pub fn named_function_define(&mut self) -> GrammarResult<SharedAstNode> {
        let func_name = self.parse_function_name()?;
        self.function_define(FuncNameType::Named(func_name))
    }

    pub fn anonymous_function_define(
        &mut self,
        variant_defs: Vec<VariantDef>,
        return_data_type: DataType,
    ) -> GrammarResult<SharedAstNode> {
        // body
        let body = self.func_block()?;

        Ok(self.build_function_def_ast_node(
            FuncNameType::Anonymous(Default::default()),
            variant_defs,
            return_data_type,
            body,
        ))
    }

    #[inline]
    fn build_function_def_ast_node(
        &self,
        func_name: FuncNameType,
        variant_defs: Vec<VariantDef>,
        return_data_type: DataType,
        body: SharedAstNode,
    ) -> SharedAstNode {
        SharedAstNode::new(AstNode::FunctionDef(FunctionDef {
            func_name: func_name,
            variant_defs: variant_defs,
            body: body,
            return_data_type: return_data_type,
            context: None,
        }))
    }

    pub fn function_define(&mut self, func_name: FuncNameType) -> GrammarResult<SharedAstNode> {
        // params
        self.expect_token(&Token::LParen)?;
        let variant_defs = self.parse_function_param_list_variant()?;
        self.expect_token(&Token::RParen)?;

        // return
        let return_data_type = self.parse_function_return_type()?;

        // body
        self.expect_token_unconsume(&Token::LBrace)?;
        let body = self.func_block()?;

        Ok(self.build_function_def_ast_node(func_name, variant_defs, return_data_type, body))
    }

    fn parse_method_param_list(
        &mut self,
        data_type: &DataType,
    ) -> GrammarResult<(MethodType, Vec<VariantDef>)> {
        if self.expected(&Token::RParen) {
            // ) => end
            return Ok((MethodType::Static, Vec::new()));
        }

        let method_type = match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::SelfHim,
                ..
            }) => {
                self.token_reader.consume();

                MethodType::Member
            }
            _ => MethodType::Static,
        };

        let mut variant_defs = Vec::new();

        if let MethodType::Member = method_type {
            variant_defs.push(VariantDef {
                name: "self".into(),
                data_type: data_type.clone(),
            });

            if self.expected(&Token::RParen) {
                // ) => end
                return Ok((method_type, variant_defs));
            }

            if self.expected(&Token::Comma) {
                self.token_reader.consume();
            } else {
                return Err(format!(
                    "expected: comma / rparen, but found: {:?}",
                    self.token_reader.peek()
                ));
            }
        };

        loop {
            let variant_def = self.parse_function_param_variant()?;
            variant_defs.push(variant_def);

            if self.expected(&Token::RParen) {
                break;
            }

            if self.expected(&Token::Comma) {
                self.token_reader.consume();
            } else {
                return Err(format!(
                    "expected: comma / rparen, but found: {:?}",
                    self.token_reader.peek()
                ));
            }
        }

        Ok((method_type, variant_defs))
    }

    pub fn method_define(&mut self, data_type: &DataType) -> GrammarResult<SharedAstNode> {
        self.token_reader.consume();

        let func_name = self.parse_function_name()?;

        // params
        self.expect_token(&Token::LParen)?;
        let (method_type, variant_defs) = self.parse_method_param_list(data_type)?;
        self.expect_token(&Token::RParen)?;

        // return
        let return_data_type = self.parse_function_return_type()?;

        // body
        self.expect_token_unconsume(&Token::LBrace)?;
        let body = self.func_block()?;

        Ok(SharedAstNode::new(AstNode::MethodDef(MethodDef {
            method_type: method_type,
            function_def: FunctionDef {
                func_name: FuncNameType::Named(func_name),
                variant_defs: variant_defs,
                body: body,
                return_data_type: return_data_type,
                context: None,
            },
        })))
    }

    pub fn impl_statement(&mut self) -> GrammarResult<SharedAstNode> {
        // consume impl
        self.token_reader.consume();

        let name = self.parse_ident()?;

        let data_type = DataType::Unknown(name.to_string());

        self.expect_token_unconsume(&Token::LBrace)?;

        let nodes = self.parse_impl_block(&data_type)?;

        Ok(SharedAstNode::new(AstNode::ImplStatement(ImplStatement {
            name: name,
            nodes: nodes,
            context: None,
        })))
    }

    pub fn struct_define(&mut self) -> GrammarResult<SharedAstNode> {
        // comsume struct
        self.token_reader.consume();

        let struct_name = if let Token::Word(Word {
            keyword: Keyword::NoKeyword,
            value,
        }) = self.token_reader.peek()
        {
            self.token_reader.consume();
            value
        } else {
            return Err(format!(
                "the struct keyword should be followed by an identify"
            ));
        };

        // member variants
        self.expect_token(&Token::LBrace)?;
        let member_variants = self.parse_struct_member_list()?;
        self.expect_token(&Token::RBrace)?;

        Ok(SharedAstNode::new(AstNode::StructDef(StructDef {
            struct_name: struct_name,
            member_variants: member_variants,
            member_method_defines: std::collections::HashMap::new(),
            static_method_defines: std::collections::HashMap::new(),
            context: None,
        })))
    }

    pub fn parse_struct_member(&mut self) -> GrammarResult<VariantDef> {
        let ident = self.parse_ident()?;

        self.expect_token(&Token::Colon)?;

        let data_type = self.parse_type()?;

        Ok(VariantDef {
            name: ident.value,
            data_type: data_type,
        })
    }

    pub fn parse_struct_member_list(&mut self) -> GrammarResult<Vec<VariantDef>> {
        let mut member_variants = Vec::new();
        loop {
            let member_variant = self.parse_struct_member()?;
            member_variants.push(member_variant);

            if self.expected(&Token::RBrace) {
                break;
            }

            if self.expected(&Token::Comma) {
                match self.token_reader.peek_n(1) {
                    Token::RBrace => {
                        self.token_reader.consume();
                        break;
                    }
                    _ => {
                        self.token_reader.consume();
                    }
                }
            } else {
                return Err(format!(
                    "expected: comma / rbrace, but found: {:?}",
                    self.token_reader.peek()
                ));
            }
        }

        Ok(member_variants)
    }

    pub fn return_statement(&mut self) -> GrammarResult<SharedAstNode> {
        // consume return
        self.token_reader.consume();

        let return_expr = self.parse_expr()?;

        Ok(SharedAstNode::new(AstNode::ReturnStatement(
            ReturnStatement {
                return_expr: return_expr,
                context: None,
            },
        )))
    }

    pub fn break_statement(&mut self) -> GrammarResult<SharedAstNode> {
        // consume break
        self.token_reader.consume();

        Ok(SharedAstNode::new(AstNode::BreakStatement(
            BreakStatement { context: None },
        )))
    }

    pub fn continue_statement(&mut self) -> GrammarResult<SharedAstNode> {
        // consume continue
        self.token_reader.consume();

        Ok(SharedAstNode::new(AstNode::ContinueStatement(
            ContinueStatement { context: None },
        )))
    }

    pub fn if_statement(&mut self) -> GrammarResult<SharedExpr> {
        // consume if
        self.token_reader.consume();

        let if_condition = ConditionBlock {
            expr: self.parse_expr()?,
            body: self.cond_block()?,
        };

        let mut else_if_conditions = Vec::new();
        let mut else_condition = None;

        loop {
            match self.token_reader.peek() {
                Token::Word(Word {
                    keyword: Keyword::Else,
                    ..
                }) => match self.token_reader.peek_n(1) {
                    Token::Word(Word {
                        keyword: Keyword::If,
                        ..
                    }) => {
                        // else if
                        // consume else
                        self.token_reader.consume();
                        // consume if
                        self.token_reader.consume();

                        else_if_conditions.push(ConditionBlock {
                            expr: self.parse_expr()?,
                            body: self.cond_block()?,
                        });
                    }
                    _ => {
                        // else
                        // consume else
                        self.token_reader.consume();

                        else_condition = Some(ConditionBlock {
                            expr: self.parse_expr()?,
                            body: self.cond_block()?,
                        });

                        break;
                    }
                },
                _ => {
                    // not else if / else
                    break;
                }
            }
        }

        Ok(SharedExpr::new_without_data_type(ExprNode::IfStatement(
            IfStatement {
                if_condition: if_condition,
                else_if_conditions: else_if_conditions,
                else_condition: else_condition,
                context: None,
            },
        )))
    }

    pub fn while_statement(&mut self) -> GrammarResult<SharedExpr> {
        // consume while
        self.token_reader.consume();

        let condition = ConditionBlock {
            expr: self.parse_expr()?,
            body: self.loop_block()?,
        };

        Ok(SharedExpr::new_without_data_type(ExprNode::WhileStatement(
            WhileStatement {
                condition: condition,
                context: None,
            },
        )))
    }

    pub fn for_statement(&mut self) -> GrammarResult<SharedExpr> {
        // consume for
        self.token_reader.consume();

        unimplemented!();
    }

    build_block!(func_block, FuncBlock);

    build_block!(loop_block, LoopBlock);

    build_block!(cond_block, CondBlock);

    pub fn block(&mut self) -> GrammarResult<SharedAstNode> {
        let nodes = self.parse_block()?;

        Ok(SharedAstNode::new(AstNode::Block {
            nodes: nodes,
            context: None,
        }))
    }

    pub fn statement_in_impl_parse(
        &mut self,
        data_type: &DataType,
    ) -> GrammarResult<SharedAstNode> {
        match self.token_reader.peek() {
            Token::Word(Word {
                keyword: Keyword::Func,
                ..
            }) => self.method_define(data_type),
            Token::EOF => {
                panic!("expect a statement, but an EOF is reached, the EOF should be checked externally");
            }
            other => panic!("in impl, the use of {:?} is not allowed.", other),
        }
    }

    pub fn parse_impl_block(&mut self, data_type: &DataType) -> GrammarResult<Vec<SharedAstNode>> {
        // consume `{`
        self.token_reader.consume();

        let mut nodes = Vec::<SharedAstNode>::new();

        loop {
            match self.token_reader.peek() {
                Token::RBrace => {
                    self.token_reader.consume();
                    break;
                }
                Token::EOF => {
                    return Err(format!(
                        "expect right curly brackets or other expressions, but arrive at EOF"
                    ));
                }
                _ => {
                    nodes.push(self.statement_in_impl_parse(&data_type)?);
                }
            }
        }

        Ok(nodes)
    }

    pub fn parse_block(&mut self) -> GrammarResult<Vec<SharedAstNode>> {
        // consume `{`
        self.token_reader.consume();

        let mut nodes = Vec::<SharedAstNode>::new();

        loop {
            match self.token_reader.peek() {
                Token::RBrace => {
                    self.token_reader.consume();
                    break;
                }
                Token::EOF => {
                    return Err(format!(
                        "expect right curly brackets or other expressions, but arrive at EOF"
                    ));
                }
                _ => {
                    nodes.push(self.statement_parse()?);
                }
            }
        }

        Ok(nodes)
    }

    pub fn int_declare(&mut self) -> GrammarResult<SharedAstNode> {
        // read keyword [int]
        let token = self.token_reader.peek();
        if let Token::Word(Word {
            keyword: Keyword::Int,
            value,
        }) = token
        {
            self.token_reader.consume();
            // read identify
            if let Token::Word(Word {
                keyword: Keyword::NoKeyword,
                // value == variant name
                value,
            }) = self.token_reader.peek()
            {
                self.token_reader.consume();
                // read =
                if let Token::Assignment = self.token_reader.peek() {
                    self.token_reader.consume();
                    return Ok(SharedAstNode::new(AstNode::Indeclaration {
                        identifier: value,
                        expr: Some(self.parse_expr()?),
                        context: None,
                    }));
                } else {
                    return Ok(SharedAstNode::new(AstNode::Indeclaration {
                        identifier: value,
                        expr: None,
                        context: None,
                    }));
                }
            } else {
                return Err(format!("variable name expected"));
            };
        } else {
            return Err(format!("keyword [int] expected"));
        }
    }

    /*
     * assignment -> add | add = assignment
     * */
    binary_operator!(assignment, eq, Assignment);
    /*
     * eq -> add (== add)*
     * */
    // binary_operator!(eq, additive, (Eq, Eq), (Neq, Neq), (Lt, Lt), (Gt, Gt));
    binary_operator!(eq, additive, Eq, Neq, Lt, Gt);
    /*
     * add -> mul (+ mul)*
     * */
    binary_operator!(additive, multiplicative, (Plus, Additive));
    /*
     * mul -> not
     * */
    binary_operator!(multiplicative, not, (Mult, Multiplicative));

    // not pri
    unary_prefix_operator!(not, primary, Not);

    fn primary(&mut self) -> GrammarResult<SharedExpr> {
        match self.token_reader.peek() {
            Token::Number(number_str) => {
                self.token_reader.consume();
                let number = match number_str.parse::<u32>() {
                    Ok(n) => n,
                    Err(err) => {
                        return Err(format!(
                            "conversion from Token::Number={} to u64 failed",
                            number_str
                        ))
                    }
                };
                Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                    Value::U32(number),
                )))
            }
            Token::NumberWithSuffix(number_str, suffix) => {
                self.token_reader.consume();
                let number = match number_str.parse::<u64>() {
                    Ok(n) => n,
                    Err(err) => {
                        return Err(format!(
                            "conversion from Token::Number={} to u64 failed",
                            number_str
                        ))
                    }
                };
                match TimeUnit::from_str(&suffix) {
                    Some(unit) => Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                        Value::TimeLiteral(number, unit),
                    ))),
                    None => Err(format!(
                        "unable to convert {} to time units correctly",
                        suffix
                    )),
                }
            }
            Token::Word(Word {
                keyword: Keyword::True,
                ..
            }) => {
                self.token_reader.consume();
                Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                    Value::Bool(true),
                )))
            }
            Token::Word(Word {
                keyword: Keyword::False,
                ..
            }) => {
                self.token_reader.consume();
                Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                    Value::Bool(false),
                )))
            }
            Token::Word(Word {
                keyword: Keyword::NoKeyword,
                value,
            })
            | Token::Word(Word {
                keyword: Keyword::SelfHim,
                value,
            }) => {
                self.token_reader.consume();
                Ok(SharedExpr::new_without_data_type(ExprNode::Identifier(
                    Ident { value: value },
                )))
            }
            Token::StringLiteral(v) => {
                self.token_reader.consume();
                Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                    Value::String(v),
                )))
            }
            t @ Token::LParen => {
                self.token_reader.consume();
                let expr = self.parse_expr()?;
                match self.token_reader.peek() {
                    Token::RParen => {
                        self.token_reader.consume();
                        Ok(expr)
                    }
                    other => Err(format!(
                        "expecting the other half to match {:?}, but found {:?}",
                        t, other
                    )),
                }
            }
            _ => Ok(SharedExpr::new_without_data_type(ExprNode::Value(
                Value::Unit,
            ))), // Token::EOF => {
                 //     return Err(format!("expected primary, but arrive end"));
                 // },
                 // other => {
                 //     return Err(format!("{:?} is not a legal primary", other));
                 // }
        }
    }

    fn expected(&mut self, expected: &Token) -> bool {
        if let Token::EOF = self.token_reader.peek() {
            return false;
        };

        return self.token_reader.peek() == *expected;
    }

    fn expect_token_unconsume(&mut self, expected: &Token) -> GrammarResult<()> {
        if !self.expected(expected) {
            return Err(format!(
                "expected {:?}, found: {:?}",
                expected,
                self.token_reader.peek()
            ));
        }

        Ok(())
    }

    fn expect_token(&mut self, expected: &Token) -> GrammarResult<()> {
        if self.expected(expected) {
            self.token_reader.consume();
        } else {
            return Err(format!(
                "expected {:?}, found: {:?}",
                expected,
                self.token_reader.peek()
            ));
        }

        Ok(())
    }

    pub fn new(token_reader: TokenReader) -> Self {
        Self {
            token_reader: token_reader,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::FromString;

    fn test_string_parser(s: impl Into<String>) {
        let mut parser = FromString::new(s.into());
        let tokens = parser.parse().unwrap();
        println!("{:#?}", tokens);
        let token_reader = TokenReader::new(tokens);
        let mut parser = Parser::new(token_reader);
        let ast_node = parser.parse().unwrap();
        println!("{:#?}", ast_node);
        let mut format_str = String::new();
        ast_node.build_format(0, &mut format_str);
        println!("{}", &format_str);
    }

    #[test]
    fn simple_parser_int_declare_test() {
        test_string_parser("int a = 1 + 2 * 3 + 4".to_string());
    }

    #[test]
    fn simple_parser_expression_test() {
        test_string_parser("a = b = 1 + 2 * 3 + 4".to_string());
    }

    #[test]
    fn simple_parser_parse_test() {
        test_string_parser("a = 1 b = 1".to_string());
    }

    #[test]
    fn simple_parser_block_test() {
        test_string_parser("a = 1 { b = 1 } {}".to_string());
    }

    #[test]
    fn simple_parser_function_define_test() {
        test_string_parser("func f() {}".to_string());
    }

    #[test]
    fn simple_parser_function_define_with_1_param_test() {
        test_string_parser("func f(int a) {}".to_string());
    }

    #[test]
    fn simple_parser_function_define_with_n_param_test() {
        test_string_parser("func f(int a, int b) {}".to_string());
    }

    #[test]
    fn simple_parser_function_call_test() {
        test_string_parser("f(1, 2, a)".to_string());
    }

    #[test]
    fn simple_parser_return_test() {
        test_string_parser("func get() -> int { return 1 }");
    }

    #[test]
    fn simple_parser_println_test() {
        test_string_parser("println(1)");
    }

    #[test]
    fn simple_parser_println_with_string_test() {
        test_string_parser(r#"println("value", 1)"#);
    }

    #[test]
    fn simple_parser_if_test() {
        test_string_parser(
            r#"if 1 {
                println(1)
            } else if 2 {
                println(2)
            } else {
                println(3)
            }

            if 1 {
            }

            if 1 {
            } else {
            }

            if 1 {
            } else if 2 {
            }
            "#,
        );
    }

    #[test]
    fn simple_parser_eq_test() {
        test_string_parser("1 == 1");
    }

    #[test]
    fn simple_parser_while_test() {
        test_string_parser(
            r#"
            while true {
                int a = 1;

                println(a);
            }
        "#,
        );
    }

    #[test]
    fn simple_parser_timeunit_test() {
        test_string_parser(r#"sleep(10s)"#);
    }

    #[test]
    fn simple_parser_not_operator_test() {
        test_string_parser(r#"int a = false; while !a + 1 == 1 {}"#);
    }

    #[test]
    fn simple_parser_expr_with_paren_test() {
        test_string_parser(r#"1 + (2 + 3) * 4"#);
    }

    #[test]
    fn simple_parser_neq_test() {
        test_string_parser(r#"1 != 1"#);
    }

    #[test]
    fn simple_parser_continue_test() {
        test_string_parser(
            r#"
            while true {
                continue
            }
        "#,
        );
    }

    #[test]
    fn simple_parser_struct_def_test() {
        test_string_parser(
            r#"
            struct S1 {
                f1: int,
                f2: bool
            }

            struct S2 {
                f1: int,
                f2: int,
            }
        "#,
        );
    }

    #[test]
    fn simple_parser_period_access_test() {
        test_string_parser(
            r#"
            a.f1().if 1 {
            }

            f2().b.f3().c
        "#,
        );
    }

    #[test]
    fn simple_parser_custom_type_object_declaration() {
        test_string_parser(
            r#"
            Dog d = 1
        "#,
        );
    }

    #[test]
    fn simple_parser_struct_init() {
        test_string_parser(
            r#"
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
                    age: 5,
                },
                cat: c,
            }
        "#,
        );
    }

    fn build_test_struct_v1(s: Option<&str>) -> String {
        let mut out = String::from(
            r#"
            struct Dog {
                name: string,
                age: int
            }

            impl Dog {
                func say_name(self) {
                    println(self.name)
                }

                func type() {
                    println("dog")
                }
            }

            "#,
        );

        if let Some(v) = s {
            out.push_str(v);
        };

        out
    }

    #[test]
    fn simple_parser_impl_statement() {
        test_string_parser(build_test_struct_v1(None));
    }

    #[test]
    fn simple_parser_struct_member_function_call() {
        test_string_parser(build_test_struct_v1(Some(
            r#"
            Dog d = Dog {
                name: "Mr huang",
                age: 5
            }

            d.say_name();
            "#,
        )));
    }

    #[test]
    fn simple_parser_anonymous_function_define() {
        test_string_parser(
            r#"
            func a() {
                func() {
                }
            }
        "#,
        );
    }

    #[test]
    fn simple_parser_function_type_define() {
        test_string_parser(
            r#"
            func a(func(int a, int b) -> int f) {
            }

            func b(func(int, int) -> int f) {
            }
        "#,
        );
    }
}
