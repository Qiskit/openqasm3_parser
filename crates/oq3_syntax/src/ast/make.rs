// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! This module contains free-standing functions for creating AST fragments out
//! of smaller pieces.
//!
//! Note that all functions here intended to be stupid constructors, which just
//! assemble a finish node from immediate children. If you want to do something
//! smarter than that, it belongs to the `ext` submodule.
//!
//! GJL: I think these are meant to be used at a higher level, ie HIR. Not used within
//! the oq3_lexer and oq3_parser crates.
//!
//! Keep in mind that `from_text` functions should be kept private. The public
//! API should require to assemble every node piecewise. The trick of
//! `parse(format!())` we use internally is an implementation detail -- long
//! term, it will be replaced with direct tree manipulation.
// use itertools::Itertools;
//use stdx::{format_to};

use crate::{ast, AstNode, SourceFile, SyntaxKind, SyntaxToken}; // utils::is_raw_identifier

/// While the parent module defines basic atomic "constructors", the `ext`
/// module defines shortcuts for common things.
///
/// It's named `ext` rather than `shortcuts` just to keep it short.
pub mod ext {
    // GJL. This is intended to be used for semantic analysis, I think.
}

pub fn expr_loop(block: ast::BlockExpr) -> ast::Expr {
    expr_from_text(&format!("loop {block}"))
}

pub fn expr_prefix(op: SyntaxKind, expr: ast::Expr) -> ast::Expr {
    let token = token(op);
    expr_from_text(&format!("{token}{expr}"))
}
pub fn expr_call(f: ast::Expr, arg_list: ast::ArgList) -> ast::Expr {
    expr_from_text(&format!("{f}{arg_list}"))
}
pub fn expr_paren(expr: ast::Expr) -> ast::Expr {
    expr_from_text(&format!("({expr})"))
}
pub fn expr_assignment(lhs: ast::Expr, rhs: ast::Expr) -> ast::Expr {
    expr_from_text(&format!("{lhs} = {rhs}"))
}
fn expr_from_text(text: &str) -> ast::Expr {
    ast_from_text(&format!("const C: () = {text};"))
}
pub fn expr_stmt(expr: ast::Expr) -> ast::ExprStmt {
    let semi = if expr.is_block_like() { "" } else { ";" };
    ast_from_text(&format!("fn f() {{ {expr}{semi} (); }}"))
}

#[track_caller]
fn ast_from_text<N: AstNode>(text: &str) -> N {
    let parse = SourceFile::parse(text);
    let node = match parse.tree().syntax().descendants().find_map(N::cast) {
        Some(it) => it,
        None => {
            let node = std::any::type_name::<N>();
            panic!("Failed to make ast node `{node}` from text {text}")
        }
    };
    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());
    node
}

pub fn token(kind: SyntaxKind) -> SyntaxToken {
    tokens::SOURCE_FILE
        .tree()
        .syntax()
        .clone_for_update()
        .descendants_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
        .unwrap_or_else(|| panic!("unhandled token: {kind:?}"))
}

pub mod tokens {
    use once_cell::sync::Lazy;

    use crate::{ast, AstNode, Parse, SourceFile, SyntaxKind::*, SyntaxToken};

    pub(super) static SOURCE_FILE: Lazy<Parse<SourceFile>> = Lazy::new(|| {
        SourceFile::parse(
            "const C: <()>::Item = (1 != 1, 2 == 2, 3 < 3, 4 <= 4, 5 > 5, 6 >= 6, !true, *p, &p , &mut p)\n;\n\n",
        )
    });

    pub fn semicolon() -> SyntaxToken {
        SOURCE_FILE
            .tree()
            .syntax()
            .clone_for_update()
            .descendants_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == SEMICOLON)
            .unwrap()
    }

    pub fn single_space() -> SyntaxToken {
        SOURCE_FILE
            .tree()
            .syntax()
            .clone_for_update()
            .descendants_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == WHITESPACE && it.text() == " ")
            .unwrap()
    }

    pub fn whitespace(text: &str) -> SyntaxToken {
        assert!(text.trim().is_empty());
        let sf = SourceFile::parse(text).ok().unwrap();
        sf.syntax()
            .clone_for_update()
            .first_child_or_token()
            .unwrap()
            .into_token()
            .unwrap()
    }

    pub fn doc_comment(text: &str) -> SyntaxToken {
        assert!(!text.trim().is_empty());
        let sf = SourceFile::parse(text).ok().unwrap();
        sf.syntax()
            .first_child_or_token()
            .unwrap()
            .into_token()
            .unwrap()
    }

    pub fn literal(text: &str) -> SyntaxToken {
        assert_eq!(text.trim(), text);
        let lit: ast::Literal = super::ast_from_text(&format!("fn f() {{ let _ = {text}; }}"));
        lit.syntax()
            .first_child_or_token()
            .unwrap()
            .into_token()
            .unwrap()
    }

    pub fn single_newline() -> SyntaxToken {
        let res = SOURCE_FILE
            .tree()
            .syntax()
            .clone_for_update()
            .descendants_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == WHITESPACE && it.text() == "\n")
            .unwrap();
        res.detach();
        res
    }

    pub fn blank_line() -> SyntaxToken {
        SOURCE_FILE
            .tree()
            .syntax()
            .clone_for_update()
            .descendants_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == WHITESPACE && it.text() == "\n\n")
            .unwrap()
    }

    pub struct WsBuilder(SourceFile);

    impl WsBuilder {
        pub fn new(text: &str) -> WsBuilder {
            WsBuilder(SourceFile::parse(text).ok().unwrap())
        }
        pub fn ws(&self) -> SyntaxToken {
            self.0
                .syntax()
                .first_child_or_token()
                .unwrap()
                .into_token()
                .unwrap()
        }
    }
}
