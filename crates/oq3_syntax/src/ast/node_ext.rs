// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Various extension methods to ast Nodes, which are hard to code-generate.
//! Extensions for various expressions live in a sibling `expr_extensions` module.
//!
//! These methods should only do simple, shallow tasks related to the syntax of the node itself.

use rowan::{GreenNodeData, GreenTokenData};
use std::borrow::Cow;

use crate::{
    ast::{self, support, AstNode, SyntaxNode},
    NodeOrToken, TokenText,
};

// This function, `text` is borrowed from r-a (may still be commented out below.)
// There is another method, also called `text` implemented for `SyntaxNode`.
// The present function differs in that it returns the text of the first token
// rather than the text of all tokens in the subtree.
//
// This trait needs a better name.
// Several AstNodes have a single token that is a name.
// This includes, but is not limited to, `Name`.
pub trait HasTextName: AstNode {
    fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }

    // FIXME: is this ok? The name of the function does not seem idiomatic
    // Nor is returning an owned string. but in practice, at the moment, we always,
    // convert text immediately to a `String`.
    fn string(&self) -> String {
        self.text().to_string()
    }
}

impl HasTextName for ast::Param {}
impl HasTextName for ast::Name {}
impl HasTextName for ast::HardwareQubit {}
impl HasTextName for ast::Identifier {}

// This was in the r-a code
// // You didn't really want the name-name, did you?
// impl ast::Name {
//     pub fn text(&self) -> TokenText<'_> {
//         text_of_first_token(self.syntax())
//     }
// }

fn text_of_first_token(node: &SyntaxNode) -> TokenText<'_> {
    fn first_token(green_ref: &GreenNodeData) -> &GreenTokenData {
        green_ref
            .children()
            .next()
            .and_then(NodeOrToken::into_token)
            .unwrap()
    }

    match node.green() {
        Cow::Borrowed(green_ref) => TokenText::borrowed(first_token(green_ref).text()),
        Cow::Owned(green) => TokenText::owned(first_token(&green).to_owned()),
    }
}

// TODO: Implementing something like this would be useful.
// Determining which kind of for iterable we have is done in semantic
// analysis by querying via methods on ast::ForIterable
// We could construct an enum here and expose it to consumers.
// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// pub enum ForIterable {
//     SetExpression(ast::SetExpression),
//     RangeExpression(ast::RangeExpr),
//     Expr(ast::Expr),
// }

// This was carried over from rust. It seems we do not need this any
// longer for disambiguation.
// impl ast::ForStmt {
// //    pub fn iterable(&self) -> Option<ForIterable> {
//     pub fn iterable(&self) -> Option<ast::Expr> {
//         // If the iterable is a BlockExpr, check if the body is missing.
//         // If it is, assume the iterable is the expression that is missing instead.
//         // let token = self.token();
//         // if let Some(t) = ast::SetExpression::cast(token.clone()) {
//         //     return ForIterable::SetExpression(t);
//         // }
//         // if let Some(t) = ast::RangeExpr::cast(token.clone()) {
//         //     return ForIterable::RangeExpression(t);
//         // }
//         // None
//         let mut exprs = support::children(self.syntax());
//         let first = exprs.next();
//         match first {
//             Some(ast::Expr::BlockExpr(_)) => exprs.next().and(first),
//             first => first,
//         }
//     }
// }

impl ast::HasLoopBody for ast::ForStmt {
    fn loop_body(&self) -> Option<ast::BlockExpr> {
        let mut exprs = support::children(self.syntax());
        let first = exprs.next();
        let second = exprs.next();
        second.or(first)
    }
}

impl ast::WhileStmt {
    pub fn condition(&self) -> Option<ast::Expr> {
        // If the condition is a BlockExpr, check if the body is missing.
        // If it is assume the condition is the expression that is missing instead.
        let mut exprs = support::children(self.syntax());
        let first = exprs.next();
        match first {
            Some(ast::Expr::BlockExpr(_)) => exprs.next().and(first),
            first => first,
        }
    }

    // FIXME: need to support both single statement and block.
    // Or, can we / should we collapse the distinction already at this level?
    pub fn body(&self) -> Option<ast::BlockExpr> {
        let mut exprs = support::children(self.syntax());
        exprs.next()
    }
}

// FIXME: This is wrong. Use `body` above for now. But must sort this out.
impl ast::HasLoopBody for ast::WhileStmt {
    fn loop_body(&self) -> Option<ast::BlockExpr> {
        let mut exprs = support::children(self.syntax());
        let first = exprs.next();
        let second = exprs.next();
        second.or(first)
    }
}

impl ast::PragmaStatement {
    fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }

    // return the pragma line omitting the word "pragma"
    pub fn pragma_text(&self) -> String {
        let text = self.text();
        if text.starts_with('#') {
            text[7..].to_string()
        } else {
            text[6..].to_string()
        }
    }
}
