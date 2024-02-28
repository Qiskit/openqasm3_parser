// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

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
            T![angle] => Angle,
            T![bit] => Bit,
            T![bool] => Bool,
            T![complex] => Complex,
            T![duration] => Duration,
            T![float] => Float,
            T![int] => Int,
            T![stretch] => Stretch,
            T![uint] => UInt,
            _ => None, // record an error ?
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
