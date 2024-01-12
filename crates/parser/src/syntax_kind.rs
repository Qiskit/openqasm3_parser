// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Defines [`SyntaxKind`] -- a fieldless enum of all possible syntactic
//! constructs of the OpenQASM 3 language.

pub mod syntax_kind_enum;

#[allow(unreachable_pub)]
pub use self::syntax_kind_enum::{SyntaxKind, T};

impl From<u16> for SyntaxKind {
    #[inline]
    fn from(d: u16) -> SyntaxKind {
        assert!(d <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    #[inline]
    fn from(k: SyntaxKind) -> u16 {
        k as u16
    }
}

impl SyntaxKind {
    #[inline]
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::WHITESPACE | SyntaxKind::COMMENT)
    }
}
