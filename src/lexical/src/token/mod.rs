mod reader;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Whitespace {
    Newline,
    Tab,
    Space,
    SingleLineComment,
    MultiLineComment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    EOF,
    Whitespace(Whitespace),
    // +
    Plus,
    // -
    Minus,
    // *
    Mult,
    // =
    Assignment,
    // !
    Not,
    // ==
    Eq,
    // !=
    Neq,
    // <
    Lt,
    // >
    Gt,
    Word(Word),
    Number(String),
    NumberWithSuffix(String, String),
    StringLiteral(String),
    // ;
    SemiColon,
    // .
    Period,
    // {
    LBrace,
    // }
    RBrace,
    // (
    LParen,
    // )
    RParen,
    // ,
    Comma,
    // ->
    Arrow,
    // :
    Colon,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    NoKeyword,
    Break,
    Continue,
    Double,
    Else,
    False,
    For,
    Func,
    If,
    Impl,
    In,
    Int,
    Long,
    Return,
    String,
    Struct,
    True,
    While,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Word {
    pub keyword: Keyword,
    pub value: String,
}

impl Token {
    pub fn make_word(ident: String) -> Self {
        let keyword = if let Ok(index) = KEYWORDS.binary_search(&ident.as_str()) {
            KEYWORD_INDEX[index]
        } else {
            Keyword::NoKeyword
        };
        Token::Word(Word {
            keyword: keyword,
            value: ident,
        })
    }
}

///// keyword /////
pub const KEYWORD_INDEX: &[Keyword] = &[
    Keyword::Break,
    Keyword::Continue,
    Keyword::Double,
    Keyword::Else,
    Keyword::False,
    Keyword::For,
    Keyword::Func,
    Keyword::If,
    Keyword::Impl,
    Keyword::In,
    Keyword::Int,
    Keyword::Long,
    Keyword::Return,
    Keyword::String,
    Keyword::Struct,
    Keyword::True,
    Keyword::While,
];

pub const KEYWORDS: &[&'static str] = &[
    "break", "continue", "double", "else", "false", "for", "func", "if", "impl", "in", "int",
    "long", "return", "string", "struct", "true", "while",
];
