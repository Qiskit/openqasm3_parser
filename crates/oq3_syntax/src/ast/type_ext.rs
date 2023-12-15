use crate::{ast, SyntaxToken, T};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarTypeKind {
    Bit,
    Bool,
    Int,
    UInt,
    Float,
    Angle,
    Duration,
    Stretch,
    Complex,
    None, // For testing. Remove this
}

impl ast::ScalarType {
    pub fn kind(&self) -> ScalarTypeKind {
        use ScalarTypeKind::*;
        match self.token().kind() {
            T![int] => ScalarTypeKind::Int,
            T![uint] => ScalarTypeKind::UInt,
            T![float] => ScalarTypeKind::Float,
            T![bit] => ScalarTypeKind::Bit,
            T![bool] => ScalarTypeKind::Bool,
            T![angle] => ScalarTypeKind::Angle,
            T![stretch] => Stretch,
            T![complex] => Complex,
            _ => ScalarTypeKind::None, // record an error ?
        }
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }
}
