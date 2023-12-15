//! Semantic AST
//! This crate implements a semantic AST for the OpenQASM 3 language.
//! Currently the semantic information encoded in this AST includes:
//!    All identifiers are resolved to (scoped) symbols.
//!    All expressions are annotated with a type.

// This code is littered with FIXME. This doesn't always mean something needs to be FIXED.
// All FIXME's should be turned into external Issues (ie on GH) or just removed if obsolete.

// Organization of API
// We opt for adding one level of hierarchy to the API by using file structure.
// This is useful for example because one might want to manipulate the AST (in ast.rs),
// but not need to construct the AST from the output of the parser (in syntax_to_semantics.rs)
// Whether types and symbols should be separate is a bit less clear.
// An alternative is to make these modules private and introduce new modules here in lib.rs
// that only `use` things from the file-level modules.

pub mod types;
pub mod symbols;
pub mod syntax_to_semantics;
pub mod ast;
pub mod context;
pub mod semantic_error;

pub use rowan::{TextRange, TextSize};

// mod display;

mod utils;
