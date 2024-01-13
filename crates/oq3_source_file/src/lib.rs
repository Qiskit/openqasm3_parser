// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Management of source files for OpenQASM 3 parsing and semantic analysis.  The main `struct` here
//! is `SourceFile` which contains the path to a source file and the AST produced by the parser. It
//! also contains a `Vec<SourceFile>` representing source files that are included via
//! `include`. Nested includes are handled naturally this way.
//!
//! `report_error` formats error messages using the external crate `ariadne`.

// mod error_report;
mod api;
mod source_file;

pub use source_file::{ErrorTrait, SourceFile, SourceString, SourceTrait};

pub use api::{
    inner_print_compiler_errors, parse_source_file, parse_source_string, print_compiler_errors,
    report_error,
};
