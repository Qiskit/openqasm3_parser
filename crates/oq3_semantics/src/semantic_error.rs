// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// This is copied and modified from rust-analyzer syntax_error.rs
use oq3_source_file;
use oq3_source_file::ErrorTrait;
use oq3_syntax::AstNode;
use oq3_syntax::SyntaxNode;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

// re-exported in lib.rs from rowan
use crate::TextRange;

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UndefVarError,
    UndefGateError,
    RedeclarationError(String),
    ConstIntegerError, // need a better way to organize this kind of type error
    IncompatibleTypesError,
    IncompatibleDimensionError,
    TooManyIndexes,
    CastError,
    MutateConstError,
    NotInGlobalScopeError,
    IncludeNotInGlobalScopeError,
    ReturnInGlobalScopeError,
    NumGateParamsError,
    NumGateQubitsError,
    // Number of arguments in call to a subroutine def is not equal to number of declared parameters.
    NumDefParamsError,
    FileNotFound,
    PermissionDenied,
    IsADirectory,
    InvalidFilename,
    // Generic IO error. Any io::ErrorKind that we do not translated explicitly
    IOError,
}

impl SemanticErrorKind {
    /// Convert a variant of io::ErrorKind to a variant of SemanticErrorKind
    pub fn from_io_error(io_error: io::ErrorKind) -> SemanticErrorKind {
        match io_error {
            io::ErrorKind::NotFound => SemanticErrorKind::FileNotFound,
            io::ErrorKind::PermissionDenied => SemanticErrorKind::PermissionDenied,
            // These two are unstable features. We can enable them when we bump the MSRV.
            // io::ErrorKind::IsADirectory => SemanticErrorKind::IsADirectory, // Stable in Rust 1.83
            // io::ErrorKind::InvalidFilename => SemanticErrorKind::InvalidFilename, // Stable in Rust 1.87
            _ => SemanticErrorKind::IOError,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SemanticError {
    error_kind: SemanticErrorKind,
    node: SyntaxNode, // Includes span and api functions to retrieve text, etc.
}

/// `SemanticErrorList` stores errors associated with parsing the file in `source_file_path`.
/// The file in `source_file_path` may include more source via the `include` statement.
/// Each such included file is parsed and it gets its own `SemanticErrorList`.
/// The error lists for all files included at the top level of `source_file_path`
/// are stored in `include_errors`.
#[derive(Clone, Debug)]
pub struct SemanticErrorList {
    source_file_path: PathBuf,
    list: Vec<SemanticError>,
    include_errors: Vec<SemanticErrorList>, // Errors when doing `include "code.qasm"`
}

impl std::ops::Deref for SemanticErrorList {
    type Target = Vec<SemanticError>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl ErrorTrait for SemanticError {
    fn message(&self) -> String {
        self.message()
    }

    fn range(&self) -> TextRange {
        self.range()
    }
}

impl SemanticErrorList {
    pub fn new(source_file_path: PathBuf) -> SemanticErrorList {
        SemanticErrorList {
            source_file_path,
            list: Vec::<SemanticError>::new(),
            include_errors: Vec::<SemanticErrorList>::new(),
        }
    }

    /// Push a (newly populated) `SemanticErrorList` into the list of
    /// list of errors found in `include`d files.
    /// This function is called after including a file and collecting errors.
    pub fn push_included(&mut self, new_errors: SemanticErrorList) {
        self.include_errors.push(new_errors);
    }

    pub fn include_errors(&self) -> &Vec<SemanticErrorList> {
        &self.include_errors
    }

    pub fn source_file_path(&self) -> &PathBuf {
        &self.source_file_path
    }

    fn insert_error(&mut self, error: SemanticError) {
        self.list.push(error);
    }

    fn insert_syntax_node(&mut self, error_kind: SemanticErrorKind, node: SyntaxNode) {
        self.insert_error(SemanticError::new(error_kind, node));
    }

    pub fn insert<T>(&mut self, error_kind: SemanticErrorKind, node: &T)
    where
        T: AstNode,
    {
        self.insert_syntax_node(error_kind, node.syntax().clone());
    }

    fn print_included_errors(&self) {
        for errors in &self.include_errors {
            errors.print_errors();
        }
    }

    pub fn print_errors(&self) {
        // If `source_file_path` cannot be read then there are no errors to print.
        // But `print_compiler_errors` would attempt to read the file and panic.
        // So if there are no errors to print, we don't even try.
        if !self.list.is_empty() {
            oq3_source_file::print_compiler_errors(self, &self.source_file_path);
        }
        self.print_included_errors();
    }

    /// Print errors for the case that the top-level code is not associated
    /// with a file. For example it came from a literal string.
    pub fn print_errors_no_file(&self, fake_file_path: &Path, source: &str) {
        // print errors from top level.
        oq3_source_file::inner_print_compiler_errors(self, fake_file_path, source);
        // print (with recursion) errors from included files.
        self.print_included_errors();
    }

    pub fn any_semantic_errors(&self) -> bool {
        if !&self.list.is_empty() {
            return true;
        }
        self.include_errors()
            .iter()
            .any(|inclusion| inclusion.any_semantic_errors())
    }
}

impl SemanticError {
    pub fn new(error_kind: SemanticErrorKind, node: SyntaxNode) -> Self {
        Self { error_kind, node }
    }

    pub fn range(&self) -> TextRange {
        self.node.text_range()
    }

    pub fn kind(&self) -> &SemanticErrorKind {
        &self.error_kind
    }

    pub fn message(&self) -> String {
        format!("{:?}", self.error_kind)
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}: {}, {:?}",
            self.error_kind,
            self.node.text(),
            self.node.text_range()
        )
    }
}
