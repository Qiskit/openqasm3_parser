// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use crate::{ast, SyntaxToken, T};

// `ScalarTypeKind` includes Qubit because thus far, we only
// use it for def can defcall, which allows mixing scalar and qubits
// in a list of parameters.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScalarTypeKind {
    Angle,
    Bit,
    Bool,
    Complex,
    Duration,
    Float,
    Int,
    None, // Use this to record errors
    Stretch,
    UInt,
    Qubit,
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
            T![qubit] => Qubit,
            _ => None,
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
