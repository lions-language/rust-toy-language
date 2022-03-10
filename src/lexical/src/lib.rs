mod simple;
mod token;
mod error;

pub use token::{Token, Whitespace, Word, Keyword};
pub use simple::from_string::FromString as SimpleFromString;
pub use simple::token_reader::TokenReader as SimpleTokenReader;
pub use error::Result;
