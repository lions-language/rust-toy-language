use crate::{Result, Token, Whitespace};

use std::iter::Peekable;
use std::str::Chars;

pub struct FromString {
    content: String,
}

impl FromString {
    pub fn parse(&self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        let mut chars = self.content.chars().peekable();
        loop {
            let token = match self.next_token(&mut chars)? {
                Token::EOF => break,
                t => t,
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn next_token(&self, chars: &mut Peekable<Chars<'_>>) -> Result<Token> {
        match chars.peek() {
            Some(&ch) => match ch {
                ' ' => self.consume_one_and_return(chars, Token::Whitespace(Whitespace::Space)),
                '\n' => self.consume_one_and_return(chars, Token::Whitespace(Whitespace::Newline)),
                '\t' => self.consume_one_and_return(chars, Token::Whitespace(Whitespace::Tab)),
                '\r' => {
                    self.consume_one(chars);
                    if let Some('\n') = chars.peek() {
                        self.consume_one(chars);
                    };
                    Ok(Token::Whitespace(Whitespace::Newline))
                }
                '+' => self.consume_one_and_return(chars, Token::Plus),
                '-' => {
                    self.consume_one(chars);
                    if let Some('>') = chars.peek() {
                        self.consume_one(chars);
                        Ok(Token::Arrow)
                    } else {
                        Ok(Token::Minus)
                    }
                }
                '*' => self.consume_one_and_return(chars, Token::Mult),
                '=' => {
                    self.consume_one(chars);
                    if let Some('=') = chars.peek() {
                        self.consume_one(chars);
                        Ok(Token::Eq)
                    } else {
                        Ok(Token::Assignment)
                    }
                }
                '<' => self.consume_one_and_return(chars, Token::Lt),
                '>' => self.consume_one_and_return(chars, Token::Gt),
                ';' => self.consume_one_and_return(chars, Token::SemiColon),
                '{' => self.consume_one_and_return(chars, Token::LBrace),
                '}' => self.consume_one_and_return(chars, Token::RBrace),
                '(' => self.consume_one_and_return(chars, Token::LParen),
                ')' => self.consume_one_and_return(chars, Token::RParen),
                '.' => self.consume_one_and_return(chars, Token::Period),
                '!' => {
                    self.consume_one(chars);
                    if let Some('=') = chars.peek() {
                        self.consume_one(chars);
                        Ok(Token::Neq)
                    } else {
                        Ok(Token::Not)
                    }
                }
                ch if self.is_identifier_start(ch) => {
                    self.consume_one(chars);
                    let s = self.extract_word(ch, chars);
                    Ok(Token::make_word(s))
                }
                '0'..='9' | '.' => {
                    // before '.'
                    let mut s = self.peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));
                    // '.'
                    if let Some('.') = chars.peek() {
                        s.push('.');
                        self.consume_one(chars);
                    };
                    // after '.'
                    s += &self.peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));

                    if s == "." {
                        // '.' only
                        return Ok(Token::Period);
                    }

                    if let Some(ch) = chars.peek() {
                        if self.is_char(*ch) {
                            let mut suffix = self.peeking_take_while(chars, |ch| self.is_char(ch));
                            return Ok(Token::NumberWithSuffix(s, suffix));
                        }
                    };

                    Ok(Token::Number(s))
                }
                ',' => self.consume_one_and_return(chars, Token::Comma),
                '"' => {
                    self.consume_one(chars);
                    let mut s = self.peeking_take_while(chars, |ch| ch != '"');
                    if let Some('"') = chars.peek() {
                        self.consume_one(chars);
                    } else {
                        return Err(format!("expect \", but meet {:?}", chars.peek()));
                    }

                    Ok(Token::StringLiteral(s))
                }
                ':' => self.consume_one_and_return(chars, Token::Colon),
                ch => {
                    unimplemented!("{}", ch);
                }
            },
            None => Ok(Token::EOF),
        }
    }

    fn peeking_take_while<F: Fn(char) -> bool>(
        &self,
        chars: &mut Peekable<Chars<'_>>,
        f: F,
    ) -> String {
        let mut s = String::new();
        while let Some(&ch) = chars.peek() {
            if !f(ch) {
                break;
            }

            s.push(ch);
            self.consume_one(chars);
        }
        s
    }

    fn extract_word(&self, first_char: char, chars: &mut Peekable<Chars<'_>>) -> String {
        let mut s = first_char.to_string();
        while let Some(&ch) = chars.peek() {
            if !self.is_identifier_part(ch) {
                break;
            }

            self.consume_one(chars);
            s.push(ch);
        }

        s
    }

    #[inline]
    fn is_char(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch)
    }

    #[inline]
    fn is_identifier_start(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
    }

    #[inline]
    fn is_identifier_part(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch)
            || ('A'..='Z').contains(&ch)
            || ('0'..='9').contains(&ch)
            || ch == '_'
    }

    #[inline]
    fn consume_one_and_return(
        &self,
        chars: &mut Peekable<Chars<'_>>,
        token: Token,
    ) -> Result<Token> {
        chars.next();
        Ok(token)
    }

    #[inline]
    fn consume_one(&self, chars: &mut Peekable<Chars<'_>>) {
        chars.next();
    }

    pub fn new(content: String) -> Self {
        Self { content: content }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn simple_from_string_test(s: impl ToString) {
        let parser = FromString::new(s.to_string());
        match parser.parse() {
            Ok(tokens) => {
                println!("tokens: {:#?}", tokens);
            }
            Err(err) => {
                println!("error: {}", err);
            }
        }
    }

    #[test]
    fn simple_from_string_intdeclare_test() {
        simple_from_string_test("int a = 1;".to_string());
    }

    #[test]
    fn simple_from_string_return_test() {
        simple_from_string_test("func get() -> int { return 0; }".to_string());
    }

    #[test]
    fn simple_from_string_for_and_in_test() {
        simple_from_string_test("for i in datas {}".to_string());
    }

    #[test]
    fn simple_from_string_while_test() {
        simple_from_string_test("while true {}".to_string());
    }

    #[test]
    fn simple_from_string_number_with_suffix() {
        simple_from_string_test(r#"1000ms;10s;1000"#);
    }

    #[test]
    fn simple_from_string_string_literal() {
        simple_from_string_test(
            r#"
            "hello"
        "#,
        );
    }

    #[test]
    fn simple_from_string_break() {
        simple_from_string_test(
            r#"
            while true {
                break
            }
        "#,
        );
    }

    #[test]
    fn simple_from_string_continue() {
        simple_from_string_test(
            r#"
            while true {
                continue;
            }
        "#,
        );
    }

    #[test]
    fn simple_from_string_not() {
        simple_from_string_test(r#"!true"#);
    }

    #[test]
    fn simple_from_string_neq() {
        simple_from_string_test(r#"a != 10"#);
    }

    #[test]
    fn simple_from_string_lt() {
        simple_from_string_test(r#"a < 10"#);
    }

    #[test]
    fn simple_from_string_struct() {
        simple_from_string_test(r#"struct S {}"#)
    }

    #[test]
    fn simple_from_string_period_operator() {
        simple_from_string_test(r#"a.b.c; 1.0"#)
    }

    #[test]
    fn simple_from_string_type_of_string() {
        simple_from_string_test(r#"string a = "hello""#)
    }

    #[test]
    fn simple_from_string_impl() {
        simple_from_string_test(r#"impl Dog {}"#)
    }
}
