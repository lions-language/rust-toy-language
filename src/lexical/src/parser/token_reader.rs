use crate::Token;

pub struct TokenReader {
    tokens: Vec<Token>,
    index: usize,
}

impl TokenReader {
    #[inline]
    pub fn peek(&self) -> Token {
        self.peek_n(0)
    }

    pub fn peek_n(&self, mut n: usize) -> Token {
        let mut index = self.index;
        loop {
            index += 1;
            match self.tokens.get(index - 1) {
                Some(Token::Whitespace(_)) => {
                    continue;
                },
                None => return Token::EOF,
                non_whitespace /*Some(t)*/ => {
                    if n == 0 {
                        return non_whitespace.unwrap().clone();
                    }
                    n -= 1;
                }
            }
        }
    }

    pub fn consume(&mut self) -> Token {
        loop {
            self.index += 1;
            match self.tokens.get(self.index - 1) {
                Some(Token::Whitespace(_)) => continue,
                None => return Token::EOF,
                token /*Some(t)*/ => {
                    // Options::cloned, if matched None, return None
                    return token.unwrap().clone();
                }
            }
        }
    }

    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            index: 0,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{FromString, TokenReader};

    #[test]
    fn token_reader_test() {
        let mut parser = FromString::new("int a = 1;".to_string());
        let tokens = parser.parse().unwrap();
        let token_reader = TokenReader::new(tokens);
        println!("peek: {:?}", token_reader.peek());
    }
}
