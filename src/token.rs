use std::rc::Rc;

#[derive(Clone, Hash, Debug)]
pub enum Token {
    Print,
    If,
    Fi,
    For,
    Do,
    Done,
    Plus,
    Minus,
    Asterisk,
    FSlash,
    Percent,
    LT,
    LE,
    EQ,
    GE,
    GT,
    Semicol,
    True,
    False,
    Int(i64),
    Ident(Rc<str>),
}

impl Token {
    pub fn kind(&self) -> &'static str {
        match self {
            Token::Print => "Print",
            Token::If => "If",
            Token::Fi => "Fi",
            Token::For => "For",
            Token::Do => "Do",
            Token::Done => "Done",
            Token::Plus => "Plus",
            Token::Semicol => "Semicol",
            Token::Int(_) => "Int",
            Token::Ident(_) => "Identity",
            Token::Minus => "Minus",
            Token::Asterisk => "Asterisk",
            Token::FSlash => "FSlash",
            Token::Percent => "Percent",
            Token::True => "True",
            Token::False => "False",
            Token::LT => "Less Than",
            Token::LE => "Less Than or Equal To",
            Token::EQ => "Equal To",
            Token::GE => "Greater Than or Equal To",
            Token::GT => "Greater Than",
        }
    }
}
