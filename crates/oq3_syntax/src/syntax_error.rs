// Copyright contributors to the openqasm-parser project

//! See docs for `SyntaxError`.

use std::fmt;

use crate::{TextRange, TextSize};

/// Represents the result of unsuccessful tokenization, parsing
/// or tree validation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxError(String, TextRange);

// FIXME: The fixme comment below is retained from the original r-a code.
// FIXME: there was an unused SyntaxErrorKind previously (before this enum was removed)
// It was introduced in this PR: https://github.com/rust-lang/rust-analyzer/pull/846/files#diff-827da9b03b8f9faa1bade5cdd44d5dafR95
// but it was not removed by a mistake.
//
// So, we need to find a place where to stick validation for attributes in match clauses.
// Code before refactor:
// InvalidMatchInnerAttr => {
//    write!(f, "Inner attributes are only allowed directly after the opening brace of the match expression")
// }

impl SyntaxError {
    pub fn new(message: impl Into<String>, range: TextRange) -> Self {
        Self(message.into(), range)
    }

    // Note that this is meant to convert from `TextSize` to `TextRange`, whatever these mean.
    pub fn new_at_offset(message: impl Into<String>, offset: TextSize) -> Self {
        Self(message.into(), TextRange::empty(offset))
    }

    pub fn range(&self) -> TextRange {
        self.1
    }

    pub fn message(&self) -> &str {
        self.0.as_ref()
    }

    pub fn with_range(mut self, range: TextRange) -> Self {
        self.1 = range;
        self
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
