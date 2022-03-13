pub(crate) mod from_string;
use crate::error::Result;
use crate::Token;

pub trait LexicalParser {
    fn parse(&self) -> Result<Vec<Token>>;
}
