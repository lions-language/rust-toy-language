use std::slice::Iter;
use std::iter::Peekable;
use crate::Token;

/*
pub struct TokenReader<'a> {
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> TokenReader<'a> {
    pub fn peek(&mut self) -> Option<&&Token> {
        self.tokens.peek()
    }

    pub fn new(tokens: &'a Vec<Token>) -> TokenReader {
        TokenReader{
            tokens: tokens.iter().peekable()
        }
    }
}
*/

