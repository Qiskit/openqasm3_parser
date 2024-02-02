// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Various traits that are implemented by ast nodes.
//!
//! The implementations are usually trivial, and live in generated.rs
//!
//! NOTE!!! This is not support for the rust traits!
//! This cannot be removed for OQ3.

use either::Either;

use crate::ast::{self, support, AstNode};

pub trait HasName: AstNode {
    fn name(&self) -> Option<ast::Name> {
        support::child(self.syntax())
    }
}

pub trait HasLoopBody: AstNode {
    fn loop_body(&self) -> Option<ast::BlockExpr> {
        support::child(self.syntax())
    }
}

pub trait HasArgList: AstNode {
    fn arg_list(&self) -> Option<ast::ArgList> {
        support::child(self.syntax())
    }
}

impl<A: HasName, B: HasName> HasName for Either<A, B> {}
