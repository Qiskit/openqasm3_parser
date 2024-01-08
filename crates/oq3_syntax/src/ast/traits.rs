// Copyright contributors to the openqasm-parser project

//! Various traits that are implemented by ast nodes.
//!
//! The implementations are usually trivial, and live in generated.rs
//!
//! NOTE!!! This is not support for the rust traits!
//! This cannot be removed for OQ3.

use either::Either;

use crate::{
    ast::{self, support, AstChildren, AstNode},
};

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

// pub trait HasGateArgList: AstNode {
//     fn gate_arg_list(&self) -> Option<ast::GateArgList> {
//         support::child(self.syntax())
//     }
// }

pub trait HasModuleItem: AstNode {
    fn items(&self) -> AstChildren<ast::Item> {
        println!("fn items in traits.rs");
        support::children(self.syntax())
    }

    fn statements(&self) -> AstChildren<ast::Stmt> {
        support::children(self.syntax())
    }
}


// pub trait HasAttrs: AstNode {
//     fn attrs(&self) -> AstChildren<ast::Attr> {
//         support::children(self.syntax())
//     }
//     fn has_atom_attr(&self, atom: &str) -> bool {
//         self.attrs().filter_map(|x| x.as_simple_atom()).any(|x| x == atom)
//     }
// }

impl<A: HasName, B: HasName> HasName for Either<A, B> {}
