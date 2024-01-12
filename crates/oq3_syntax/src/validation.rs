// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

mod block;
//mod lexer;
pub use parser::T;

//use rowan::Direction;
use lexer::unescape::{self, unescape_literal, Mode};

use crate::{
    ast::{self, IsString},
    match_ast, AstNode, SyntaxError, SyntaxNode, TextSize,
};

// FIXME: GJL, I think I disabled many of these. Some should be used.
pub(crate) fn validate(root: &SyntaxNode) -> Vec<SyntaxError> {
    // FIXME: fixme note *not* added by GJL.
    // * Add unescape validation of raw string literals and raw byte string literals
    // * Add validation of doc comments are being attached to nodes

    let mut errors = Vec::new();
    for node in root.descendants() {
        match_ast! {
            match node {
                ast::Literal(it) => validate_literal(it, &mut errors),
                // ast::Const(it) => validate_const(it, &mut errors),
                // ast::BlockExpr(it) => block::validate_block_expr(it, &mut errors),
                // ast::FieldExpr(it) => validate_numeric_name(it.name_ref(), &mut errors),
                // ast::RecordExprField(it) => validate_numeric_name(it.name_ref(), &mut errors),
                // ast::RangeExpr(it) => validate_range_expr(it, &mut errors),
                // ast::RefType(it) => validate_trait_object_ref_ty(it, &mut errors),
                // ast::PtrType(it) => validate_trait_object_ptr_ty(it, &mut errors),
                // ast::LetExpr(it) => validate_let_expr(it, &mut errors),
                _ => (),
            }
        }
    }
    errors
}

fn oq3_unescape_error_to_string(err: unescape::EscapeError) -> (&'static str, bool) {
    use unescape::EscapeError as EE;

    #[rustfmt::skip]
    let err_message = match err {
        EE::ZeroChars => {
            "Literal must not be empty"
        }
        EE::MoreThanOneChar => {
            "Literal must be one character long"
        }
        EE::LoneSlash => {
            "Character must be escaped: `\\`"
        }
        EE::InvalidEscape => {
            "Invalid escape"
        }
        EE::BareCarriageReturn | EE::BareCarriageReturnInRawString => {
            "Character must be escaped: `\r`"
        }
        EE::EscapeOnlyChar => {
            "Escape character `\\` must be escaped itself"
        }
        EE::TooShortHexEscape => {
            "ASCII hex escape code must have exactly two digits"
        }
        EE::InvalidCharInHexEscape => {
            "ASCII hex escape code must contain only hex characters"
        }
        EE::OutOfRangeHexEscape => {
            "ASCII hex escape code must be at most 0x7F"
        }
        EE::NoBraceInUnicodeEscape => {
            "Missing `{` to begin the unicode escape"
        }
        EE::InvalidCharInUnicodeEscape => {
            "Unicode escape must contain only hex characters and underscores"
        }
        EE::EmptyUnicodeEscape => {
            "Unicode escape must not be empty"
        }
        EE::UnclosedUnicodeEscape => {
            "Missing `}` to terminate the unicode escape"
        }
        EE::LeadingUnderscoreUnicodeEscape => {
            "Unicode escape code must not begin with an underscore"
        }
        EE::OverlongUnicodeEscape => {
            "Unicode escape code must have at most 6 digits"
        }
        EE::LoneSurrogateUnicodeEscape => {
            "Unicode escape code must not be a surrogate"
        }
        EE::OutOfRangeUnicodeEscape => {
            "Unicode escape code must be at most 0x10FFFF"
        }
        EE::UnicodeEscapeInByte => {
            "Byte literals must not contain unicode escapes"
        }
        EE::NonAsciiCharInByte  => {
            "Byte literals must not contain non-ASCII characters"
        }
        EE::UnskippedWhitespaceWarning => "Whitespace after this escape is not skipped",
        EE::MultipleSkippedLinesWarning => "Multiple lines are skipped by this escape",

    };

    (err_message, err.is_fatal())
}

fn validate_literal(literal: ast::Literal, acc: &mut Vec<SyntaxError>) {
    // FIXME: move this function to outer scope (https://github.com/rust-lang/rust-analyzer/pull/2834#discussion_r366196658)
    fn unquote(text: &str, prefix_len: usize, end_delimiter: char) -> Option<&str> {
        text.rfind(end_delimiter)
            .and_then(|end| text.get(prefix_len..end))
    }

    let token = literal.token();
    let text = token.text();

    // FIXME: lift this lambda refactor to `fn` (https://github.com/rust-lang/rust-analyzer/pull/2834#discussion_r366199205)
    let mut push_err = |prefix_len, off, err: unescape::EscapeError| {
        let off = token.text_range().start() + TextSize::try_from(off + prefix_len).unwrap();
        let (message, is_err) = oq3_unescape_error_to_string(err);
        // FIXME: Emit lexer warnings
        if is_err {
            acc.push(SyntaxError::new_at_offset(message, off));
        }
    };

    match literal.kind() {
        ast::LiteralKind::String(s) => {
            if !s.is_raw() {
                if let Some(without_quotes) = unquote(text, 1, '"') {
                    unescape_literal(without_quotes, Mode::Str, &mut |range, char| {
                        if let Err(err) = char {
                            push_err(1, range.start, err);
                        }
                    });
                }
            }
        }
        ast::LiteralKind::BitString(_s) => {
            if let Some(without_quotes) = unquote(text, 1, '"') {
                unescape_literal(without_quotes, Mode::BitStr, &mut |range, char| {
                    if let Err(err) = char {
                        push_err(1, range.start, err);
                    }
                });
            }
        }
        ast::LiteralKind::Char(_) => {
            if let Some(without_quotes) = unquote(text, 1, '\'') {
                unescape_literal(without_quotes, Mode::Char, &mut |range, char| {
                    if let Err(err) = char {
                        push_err(1, range.start, err);
                    }
                });
            }
        }
        ast::LiteralKind::Byte(_) => {
            if let Some(without_quotes) = unquote(text, 2, '\'') {
                unescape_literal(without_quotes, Mode::Byte, &mut |range, char| {
                    if let Err(err) = char {
                        push_err(2, range.start, err);
                    }
                });
            }
        }
        ast::LiteralKind::IntNumber(_)
        | ast::LiteralKind::FloatNumber(_)
        | ast::LiteralKind::TimingFloatNumber(_)
        | ast::LiteralKind::SimpleFloatNumber(_)
        | ast::LiteralKind::Bool(_) => {}
    }
}

// pub(crate)
fn _validate_block_structure(root: &SyntaxNode) {
    let mut stack = Vec::new();
    for node in root.descendants_with_tokens() {
        match node.kind() {
            T!['{'] => stack.push(node),
            T!['}'] => {
                if let Some(pair) = stack.pop() {
                    assert_eq!(
                        node.parent(),
                        pair.parent(),
                        "\nunpaired curlies:\n{}\n{:#?}\n",
                        root.text(),
                        root,
                    );
                    assert!(
                        node.next_sibling_or_token().is_none()
                            && pair.prev_sibling_or_token().is_none(),
                        "\nfloating curlies at {:?}\nfile:\n{}\nerror:\n{}\n",
                        node,
                        root.text(),
                        node,
                    );
                }
            }
            _ => (),
        }
    }
}
