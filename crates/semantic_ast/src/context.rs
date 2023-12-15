use std::path::PathBuf;
use crate::symbols::{SymbolTable, SymbolIdResult, SymbolRecordResult};
use crate::semantic_error::{SemanticErrorKind, SemanticErrorList};
use crate::semantic_error::SemanticErrorKind::*;
use crate::ast;
use oq3_syntax::ast as synast; // Syntactic AST
use synast::{AstNode};
use crate::types::{Type};

#[derive(Clone, Debug)]
pub struct Context {
    pub program: ast::Program,
    pub semantic_errors: SemanticErrorList,
    pub symbol_table: SymbolTable
}

impl Context {
    pub(crate) fn new(file_path: PathBuf) -> Context { // source_file_path: PathBuf) -> Context {
        Context {
            program: ast::Program::new(),
            semantic_errors: SemanticErrorList::new(file_path), // source_file_path),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn push_included(&mut self, errors: SemanticErrorList) {
        self.semantic_errors.push_included(errors);
    }

    pub fn errors(&self) -> &SemanticErrorList {
        &self.semantic_errors
    }

    pub fn program(&self) -> &ast::Program {
        &self.program
    }

    pub fn as_tuple(self) -> (ast::Program, SemanticErrorList, SymbolTable) {
        (self.program, self.semantic_errors, self.symbol_table)
    }

    // fn as_tuple_mut(&mut self) -> (&mut ast::Program, &mut SemanticErrorList, &mut SymbolTable) {
    //     (&mut self.program, &mut self.semantic_errors, &mut self.symbol_table)
    // }

    pub fn insert_error<T>(&mut self, error_kind: SemanticErrorKind, node: &T) where T: AstNode {
        self.semantic_errors.insert(error_kind, node);
    }

    /// Lookup the symbol, returing a SymbolRecordResult. Possibly log a `UndefVarError`.
    pub(crate) fn lookup_symbol<T>(&mut self, name: &str, node: &T)  -> SymbolRecordResult where T: AstNode {
        let symbol_record = self.symbol_table.lookup(name);
        if symbol_record.is_err() {
            self.semantic_errors.insert(UndefVarError, node);
        }
        symbol_record
    }

    /// Bind `name` to new Symbol, returning SymbolIdResult. Possibly log a `RedeclarationError`.
    pub(crate) fn new_binding<T>(&mut self, name: &str, typ: &Type, node: &T) -> SymbolIdResult where T: AstNode {
        let symbol_id_result = self.symbol_table.new_binding(name, typ);
        if symbol_id_result.is_err() {
            self.semantic_errors.insert(RedeclarationError, node);
        }
        symbol_id_result
    }
}

#[macro_export]
macro_rules! with_scope {
    ($val:expr) => { 2 };
    ($ctxt:ident, $scope:path, $($code:stmt);+ $(;)?) => {
        $ctxt.symbol_table.enter_scope($scope);
        $($code)+
        $ctxt.symbol_table.exit_scope();
    };

    ($ctxt:ident, $scope:path, $code:block) => {
        $ctxt.symbol_table.enter_scope($scope);
        $code;
        $ctxt.symbol_table.exit_scope();
    };
}
