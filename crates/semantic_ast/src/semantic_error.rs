// This is copied and modified from rust-analyzer syntax_error.rs
use std::fmt;
use std::path::PathBuf;
use oq3_syntax::AstNode;
use oq3_syntax::SyntaxNode;
use source_file::{ErrorTrait};
use source_file;

// re-exported in lib.rs from rowan
use crate::{TextRange};

// #[derive(Debug)]
// pub enum SemanticNote {
//     Error(SemanticErrorKind),
//     Warning(SemanticWarningKind),
// }

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UndefVarError,
    RedeclarationError,
    ConstIntegerError, // need a better way to organize this kind of type error
    IncompatibleTypesError,
    MutateConstError,
}


#[derive(Clone, Debug)]
pub struct SemanticError {
    error_kind: SemanticErrorKind,
    node: SyntaxNode, // Includes span and api functions to retrieve text, etc.
}

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
        SemanticErrorList { source_file_path, list: Vec::<SemanticError>::new() , include_errors: Vec::<SemanticErrorList>::new() }
    }

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

    pub fn insert<T>(&mut self, error_kind: SemanticErrorKind, node: &T) where T: AstNode {
        self.insert_syntax_node(error_kind, node.syntax().clone());
    }

    fn print_included_errors(&self) {
        for errors in &self.include_errors {
            errors.print_errors();
        }
    }

    pub fn print_errors(&self) {
        source_file::print_compiler_errors(self, &self.source_file_path);
        self.print_included_errors();
    }

    /// Print errors for the case that the top-level code is not associated
    /// with a file. For example it came from a literal string.
    pub fn print_errors_no_file(&self, fake_file_path: Option<&str>, source: &str) {
        let file_path = fake_file_path.unwrap_or("no file");
        source_file::inner_print_compiler_errors(self, file_path, source);
        self.print_included_errors();
    }

    pub fn any_semantic_errors(&self) -> bool {
        if ! &self.list.is_empty() {
            return true
        }
        self.include_errors().iter().any(|inclusion| inclusion.any_semantic_errors())
    }

}

impl SemanticError {
    pub fn new(error_kind: SemanticErrorKind, node: SyntaxNode) -> Self {
        Self { error_kind, node }
    }

    pub fn range(&self) -> TextRange {
        self.node.text_range()
    }

    pub fn string(&self) -> String {
        self.node.text().to_string()
    }

    pub fn message(&self) -> String {
        format!("{:?}", self.error_kind)
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}, {:?}",  self.error_kind, self.node.text(), self.node.text_range())
    }
}
