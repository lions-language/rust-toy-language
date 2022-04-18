use std::borrow::Borrow;
use std::collections::HashMap;

use grammar::{AstNode, ExprNode, Ident, SharedAstNode, SharedExpr, Value};
use utils::SharedCell;

pub struct IntCalculator {
    variables: HashMap<String, u64>,
}

impl IntCalculator {
    fn calc_expr(&mut self, expr: &SharedExpr) -> u64 {
        match &expr.cell().expr_node {
            ExprNode::Additive { left, right } => {
                let l = self.calc_expr(left);
                let r = self.calc_expr(right);
                l + r
            }
            ExprNode::Value(Value::U64(v)) => *v,
            ExprNode::Value(Value::U32(v)) => *v as u64,
            ExprNode::Identifier(Ident { value }) => match self.variables.get(value) {
                Some(v) => *v,
                None => {
                    panic!("variable=[{}] not found", value);
                }
            },
            _ => {
                unimplemented!();
            }
        }
    }

    fn handle_declare(&mut self, ast_node: &SharedAstNode) {
        match &*ast_node.cell() {
            AstNode::Indeclaration {
                identifier,
                expr,
                context,
            } => match expr {
                Some(e) => {
                    let expr_value = self.calc_expr(e);
                    self.variables.insert(identifier.to_string(), expr_value);
                }
                None => {}
            },
            _ => {
                unimplemented!("not support {:?} declare", ast_node);
            }
        }
    }

    fn calc(&mut self, ast_node: &SharedAstNode) {
        match &*ast_node.cell() {
            AstNode::MainBlock { nodes, .. } => {
                for n in nodes {
                    match &*n.cell() {
                        AstNode::Indeclaration {
                            identifier,
                            expr,
                            context,
                        } => {
                            self.handle_declare(n);
                        }
                        AstNode::Expr(e) => {
                            let value = self.calc_expr(e);
                            // println!("{}", value);
                        }
                        _ => {}
                    }
                }
            }
            _ => {
                unimplemented!("not support {:?}", ast_node);
            }
        }
    }

    pub fn get_variables(&self) -> &HashMap<String, u64> {
        &self.variables
    }

    pub fn new() -> IntCalculator {
        IntCalculator {
            variables: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grammar::*;
    use lexical::*;
    use semantic::*;

    fn get_ast(statement: String) -> SharedAstNode {
        println!("{}", "-".repeat(20));
        let mut parser = FromString::new(statement);
        let tokens = parser.parse().unwrap();
        let token_reader = TokenReader::new(tokens);
        let mut parser = Parser::new(token_reader);
        let ast_node = parser.parse().unwrap();
        println!("{:?}", ast_node);
        let mut format_str = String::new();
        ast_node.build_format(0, &mut format_str);
        println!("{}", &format_str);
        println!("{}", "-".repeat(20));
        ast_node
    }

    #[test]
    fn int_calculator_no_variables_test() {
        let ast_node = get_ast("1 + 2 2 + 2".to_string());

        let mut c = IntCalculator::new();
        c.calc(&ast_node);
    }

    #[test]
    fn int_calculator_int_declare_test() {
        let ast_node = get_ast("int a = 1".to_string());

        let mut c = IntCalculator::new();
        c.calc(&ast_node);
        println!("{:?}", c.get_variables());
    }

    #[test]
    fn int_calculator_with_variables_test() {
        let ast_node = get_ast("int a = 1 a + 1".to_string());

        let mut c = IntCalculator::new();
        c.calc(&ast_node);
    }
}
