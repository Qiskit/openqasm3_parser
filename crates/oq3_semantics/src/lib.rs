// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Abstract Semantic Graph (ASG)
//! This crate implements an abstract semantic graph (ASG) for the OpenQASM 3 language.
//! Currently the semantic information encoded in this ASG includes:
//!   All identifiers are resolved to (scoped) symbols.
//!   All expressions are annotated with a type.
//!
//! This kind of structure is often refered to as something like an AST decorated with semantic
//! information, even though it's not really a tree, but rather a directed acyclic graph. We use the
//! acronym ASG here not to be pedantic but rather to have an easy and succinct way to distinguish
//! the output of syntactic analysis from output of semantic analysis.

// This code is littered with FIXME. This doesn't always mean something needs to be FIXED.
// All FIXME's should be turned into external Issues (ie on GH) or just removed if obsolete.

// Organization of API
// We opt for adding one level of hierarchy to the API by using file structure.
// This is useful for example because one might want to manipulate the ASG (in asg.rs),
// but not need to construct the ASG from the output of the parser (in syntax_to_semantics.rs)
// Whether types and symbols should be separate is a bit less clear.
// An alternative is to make these modules private and introduce new modules here in lib.rs
// that only `use` things from the file-level modules.

pub mod asg;
pub mod context;
pub mod semantic_error;
pub mod symbols;
pub mod syntax_to_semantics;
pub mod types;
pub mod validate;

pub use rowan::{TextRange, TextSize};

// mod display;

mod utils;
