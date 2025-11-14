// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Various extension methods to ast Nodes, which are hard to code-generate.
//! Extensions for various expressions live in a sibling `expr_ext` module.
//!
//! These methods should only do simple, shallow tasks related to the syntax of the node itself.

use rowan::{GreenNodeData, GreenTokenData};
use std::borrow::Cow;

use crate::{
    ast::{self, support, AstNode, SyntaxNode},
    NodeOrToken, TokenText,
};

use super::ForStmt;

// This function, `text` is borrowed from r-a (may still be commented out below.)
// There is another method, also called `text` implemented for `SyntaxNode`.
// The present function differs in that it returns the text of the first token
// rather than the text of all tokens in the subtree.
//
// This trait needs a better name.
// Several AstNodes have a single token that is a name.
// This includes, but is not limited to, `Name`.
pub trait HasTextNode: AstNode {
    fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }

    fn string(&self) -> String {
        self.text().to_string()
    }
}

impl HasTextNode for ast::Param {}
impl HasTextNode for ast::Name {}
impl HasTextNode for ast::HardwareQubit {}
impl HasTextNode for ast::Identifier {}

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

impl ast::AssignmentStmt {
    pub fn identifier(&self) -> Option<ast::Identifier> {
        support::child(&self.syntax)
    }
}

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

    pub fn body(&self) -> Option<ast::BlockExpr> {
        let mut exprs = support::children(self.syntax());
        exprs.next()
    }

    pub fn stmt(&self) -> Option<ast::Stmt> {
        let mut exprs = support::children(self.syntax());
        exprs.next()
    }

    pub fn block_or_stmt(&self) -> BlockOrStmt {
        if let Some(body) = self.body() {
            BlockOrStmt::BlockExpr(body)
        } else if let Some(stmt) = self.stmt() {
            BlockOrStmt::Stmt(stmt)
        } else {
            panic!("Error in oq3_syntax");
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockOrStmt {
    BlockExpr(ast::BlockExpr),
    Stmt(ast::Stmt),
}

impl ForStmt {
    pub fn block_or_stmt(&self) -> BlockOrStmt {
        if let Some(body) = self.body() {
            BlockOrStmt::BlockExpr(body)
        } else if let Some(stmt) = self.stmt() {
            BlockOrStmt::Stmt(stmt)
        } else {
            panic!("Error in oq3_syntax");
        }
    }
}

// FIXME: I mysteriously had to make this public when changing flow control to Stmt
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBranch {
    Block(ast::BlockExpr),
    IfStmt(ast::IfStmt),
}

impl From<ast::BlockExpr> for ElseBranch {
    fn from(block_expr: ast::BlockExpr) -> Self {
        Self::Block(block_expr)
    }
}

impl From<ast::IfStmt> for ElseBranch {
    fn from(if_stmt: ast::IfStmt) -> Self {
        Self::IfStmt(if_stmt)
    }
}

impl ast::IfStmt {
    pub fn condition(&self) -> Option<ast::Expr> {
        // If the condition is a BlockExpr, check if the then body is missing.
        // If it is, assume the condition is the expression that is missing instead.
        let mut exprs = support::children(self.syntax());
        let first = exprs.next();
        match first {
            Some(ast::Expr::BlockExpr(_)) => exprs.next().and(first),
            first => first,
        }
    }

    pub fn then_branch_block(&self) -> Option<ast::BlockExpr> {
        match support::children(self.syntax()).nth(1)? {
            ast::Expr::BlockExpr(block) => Some(block),
            _ => None,
        }
    }

    // Hmm. Not sure why this is not `nth(1)`. (It is equivalent to `nth(0)`.)
    pub fn then_branch_stmt(&self) -> Option<ast::Stmt> {
        support::child(&self.syntax)
    }

    // This is the `if` body, corresponding to the condition evaluating true.
    pub fn true_body_block_or_stmt(&self) -> BlockOrStmt {
        if let Some(body) = self.then_branch_block() {
            BlockOrStmt::BlockExpr(body)
        } else if let Some(stmt) = self.then_branch_stmt() {
            BlockOrStmt::Stmt(stmt)
        } else {
            panic!("Error in oq3_syntax");
        }
    }

    // Return `Some` if the else branch is present and is a curly-delimited block.
    pub fn else_branch_block(&self) -> Option<ast::BlockExpr> {
        match support::children(self.syntax()).nth(2)? {
            ast::Expr::BlockExpr(block) => Some(block),
            _ => None,
        }
    }

    // Return `Some` if the else branch is present and is a single statement.
    pub fn else_branch_stmt(&self) -> Option<ast::Stmt> {
        support::child(&self.syntax)
    }

    // This is the `else` body, corresponding to the condition evaluating false.
    // If there is no `else` body, return `None`.
    pub fn false_body_block_or_stmt(&self) -> Option<BlockOrStmt> {
        if let Some(body) = self.else_branch_block() {
            Some(BlockOrStmt::BlockExpr(body))
        } else {
            self.else_branch_stmt().map(BlockOrStmt::Stmt)
        }
    }

    // FIXME: this may have supported more than what is above.
    // OQ3 appears not to have `elif`-like construct. So this is not useful.
    // pub fn else_branch(&self) -> Option<ElseBranch> {
    //     match support::children(self.syntax()).nth(2)? {
    //         ast::Expr::BlockExpr(block) => Some(ElseBranch::Block(block)),
    //         ast::Expr::IfExpr(elif) => Some(ElseBranch::IfExpr(elif)),
    //         _ => None,
    //     }
    // }
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

impl ast::AnnotationStatement {
    fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }

    pub fn annotation_text(&self) -> String {
        self.text().to_string()
    }
}
