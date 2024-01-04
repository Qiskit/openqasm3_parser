// Defines data structures and api for symbols, scope, and symbol tables.

use crate::types::Type;
use hashbrown::HashMap;


// OQ3
// * "The lifetime of each identifier begins when it is declared, and ends
//    at the completion of the scope it was declared in."

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ScopeType {
    /// Top-level
    Global,
    /// Body of `gate` and `def`
    Subroutine,
    /// `cal` and `defcal` blocks
    Calibration,
    /// Control flow blocks
    Local,
}

// This wrapped `usize` serves as
// * A unique label for instances of `Symbol`.
// * An index into `all_symbols: Vec<Symbol>`.
// * The values in `SymbolMap`.
//
// I am assuming that we can clone `SymbolId` willy-nilly
// because it is no more expensive than a reference.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(usize);

impl SymbolId {
    pub fn new() -> SymbolId {
        SymbolId(0)
    }

    /// Post-increment the value, and return the old value.
    pub fn post_increment(&mut self) -> SymbolId {
        let old_val = self.clone();
        self.0 += 1;
        old_val
    }
}

impl Default for SymbolId {
    fn default() -> Self {
        Self::new()
    }
}

// I'd rather keep the implementation and value secret.
// But Python. This is useful for the pyo3 consumer.
impl From<SymbolId> for usize {
    fn from(symid: SymbolId) -> usize {
        symid.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymbolError {
    MissingBinding,
    AlreadyBound,
}

pub type SymbolIdResult = Result<SymbolId, SymbolError>;
pub type SymbolRecordResult<'a> = Result<SymbolRecord<'a>, SymbolError>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol {
    name: String,
    typ: Type,
    //    ast_node: SyntaxNode,
}

pub trait SymbolType {
    /// Return the `Type` of `symbol`, which is `Type::Undefined` if
    /// `self` is an `Option<T>` with value `None`.
    fn symbol_type(&self) -> &Type;
}

impl Symbol {
    fn new<T: ToString>(name: T, typ: &Type) -> Symbol {
        Symbol {
            name: name.to_string(),
            typ: typ.clone(),
        }
        // fn new<T: ToString>(name: T, typ: &Type, ast_node: &SyntaxNode) -> Symbol {
        //     Symbol { name: name.to_string(), typ: typ.clone() }
        //        Symbol { name: name.to_string(), typ: typ.clone(), ast_node: ast_node.clone() }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl SymbolType for Symbol {
    fn symbol_type(&self) -> &Type {
        &self.typ
    }
}

/// A structure for temporarily collecting information about
/// a symbol.
/// * `Symbol` contains `name: String` and the `Type`.
/// * `symbol_id` wraps a `usize` that serves as
///     * a unique label
///     * the index into the `Vec` of all symbols.
///     * the value in `SymbolMap`: `name` -> `symbol_id`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolRecord<'a> {
    symbol: &'a Symbol,
    symbol_id: SymbolId,
    scope_level: usize,
}

impl SymbolRecord<'_> {
    pub fn new(symbol: &Symbol, symbol_id: SymbolId, scope_level: usize) -> SymbolRecord<'_> {
        SymbolRecord {
            symbol,
            symbol_id,
            scope_level,
        }
    }

    pub fn symbol_id(&self) -> SymbolId {
        self.symbol_id.clone()
    }

    pub fn scope_level(&self) -> usize {
        self.scope_level
    }
}

// This trait is a bit heavy weight for what it does.
pub trait SymbolErrorTrait {
    fn to_symbol_id(&self) -> SymbolIdResult;
    fn as_tuple(&self) -> (SymbolIdResult, Type);
}

impl SymbolErrorTrait for SymbolRecordResult<'_> {
    fn to_symbol_id(&self) -> SymbolIdResult {
        self.clone().map(|record| record.symbol_id)
    }

    fn as_tuple(&self) -> (SymbolIdResult, Type) {
        (self.to_symbol_id(), self.symbol_type().clone())
    }
}

impl SymbolType for Option<SymbolRecord<'_>> {
    fn symbol_type(&self) -> &Type {
        match self {
            Some(symbol_record) => symbol_record.symbol_type(),
            None => &Type::Undefined,
        }
    }
}

impl SymbolType for Result<SymbolRecord<'_>, SymbolError> {
    fn symbol_type(&self) -> &Type {
        match self {
            Ok(symbol_record) => symbol_record.symbol_type(),
            Err(_) => &Type::Undefined,
        }
    }
}

impl SymbolType for SymbolRecord<'_> {
    fn symbol_type(&self) -> &Type {
        self.symbol.symbol_type()
    }
}

/// A `SymbolMap` is a map from `names` to `SymbolId` for a single instance
/// of a scope.
/// A `SymbolTable` is a stack of `SymbolMap`s together with a `Vec` mapping
/// `SymbolId as usize` to `Symbol`s.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
struct SymbolMap {
    table: HashMap<String, SymbolId>,
    scope_type: ScopeType,
}

impl SymbolMap {
    fn new(scope_type: ScopeType) -> SymbolMap {
        SymbolMap {
            table: HashMap::<String, SymbolId>::new(),
            scope_type,
        }
    }

    pub fn insert<T: ToString>(&mut self, name: T, sym: SymbolId) {
        self.table.insert(name.to_string(), sym);
    }

    pub fn get_symbol_id(&self, name: &str) -> Option<&SymbolId> {
        self.table.get(name)
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// Re
    pub fn contains_name(&self, name: &str) -> bool {
        self.table.contains_key(name)
    }

    /// Return the `ScopeType` of the `SymbolMap` of the current, or top-most, scope.
    pub fn scope_type(&self) -> ScopeType {
        self.scope_type.clone()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable {
    /// A stack each of whose elements represent a scope mapping `name: String` to `SymbolId`.
    symbol_table_stack: Vec<SymbolMap>,
    /// A list of all `Symbol`s with no explicit scope information. Indices are `SymbolId as usize`.
    all_symbols: Vec<Symbol>,
    /// A counter that is incremented after each new symbol is created.
    symbol_id_counter: SymbolId,
}

#[allow(dead_code)]
impl SymbolTable {
    /// Create a new `SymbolTable` and initialize with the global scope.
    pub fn new() -> SymbolTable {
        let mut symbol_table = SymbolTable {
            symbol_id_counter: SymbolId::new(),
            symbol_table_stack: Vec::<SymbolMap>::new(),
            all_symbols: Vec::<Symbol>::new(),
        };
        symbol_table.enter_scope(ScopeType::Global); // May want to initialize with some global symbols as well
        symbol_table
    }

    pub fn number_of_scopes(&self) -> usize {
        self.symbol_table_stack.len()
    }

    pub fn enter_scope(&mut self, scope_type: ScopeType) {
        if scope_type == ScopeType::Global && self.number_of_scopes() > 0 {
            panic!("The unique global scope must be the first scope.")
        }
        self.symbol_table_stack.push(SymbolMap::new(scope_type))
    }

    pub fn exit_scope(&mut self) {
        self.symbol_table_stack.pop();
    }

    /// If a binding for `name` exists in the current scope, return `None`.
    /// Otherwise, create a new Symbol from `name` and `typ`, bind `name` to
    /// this Symbol in the current scope, and return the Symbol.
    pub fn new_binding(&mut self, name: &str, typ: &Type) -> Result<SymbolId, SymbolError> {
        //    pub fn new_binding(&mut self, name: &str, typ: &Type, ast_node: &SyntaxNode) -> Result<SymbolId, SymbolError> {

        // Can't create a binding if it already exists in the current scope.
        if self.current_scope_contains_name(name) {
            return Err(SymbolError::AlreadyBound);
        }

        // Create new symbol and symbol id.
        //        let symbol = Symbol::new(name, typ, ast_node);
        let symbol = Symbol::new(name, typ);

        // Push the symbol onto list of all symbols (in all scopes). Index
        // to this symbol will be `id_count`.
        self.all_symbols.push(symbol);

        // The "current" SymbolId has not yet been unused.
        // Get the current SymbolId and increment the counter.
        let current_symbol_id = self.symbol_id_counter.post_increment();

        // Map `name` to `symbol_id`.
        self.current_scope_mut()
            .insert(name, current_symbol_id.clone());
        Ok(current_symbol_id)
    }

    // Symbol table for current (latest) scope in stack, mutable ref
    fn current_scope_mut(&mut self) -> &mut SymbolMap {
        self.symbol_table_stack.last_mut().unwrap()
    }

    // Symbol table for current (latest) scope in stack, immutable ref
    fn current_scope(&self) -> &SymbolMap {
        self.symbol_table_stack.last().unwrap()
    }

    /// Return the `ScopeType` of the current, or top-most, scope.
    pub(crate) fn current_scope_type(&self) -> ScopeType {
        self.current_scope().scope_type()
    }

    // Return `true` if `name` is bound in current scope.
    fn current_scope_contains_name(&self, name: &str) -> bool {
        self.current_scope().contains_name(name)
    }

    // /// Look up and return the `SymbolId` that `name` is bound to in the current scope.
    // pub fn lookup_current_scope(&self, name: &str) -> Option<&SymbolId> {
    //     self.current_scope().get(name)
    // }

    /// Return the length (number of bindings) in the current scope.
    pub fn len_current_scope(&self) -> usize {
        self.current_scope().len()
    }

    // FIXME: fix awkward scope numbering
    /// Look up `name` in the stack of symbol tables. Return `SymbolRecord`
    /// if the symbol is found. Otherwise `None`.
    pub fn lookup(&self, name: &str) -> Result<SymbolRecord, SymbolError> {
        for (scope_level_rev, table) in self.symbol_table_stack.iter().rev().enumerate() {
            if let Some(symbol_id) = table.get_symbol_id(name) {
                let symbol = &self.all_symbols[symbol_id.0];
                let scope_level = self.number_of_scopes() - scope_level_rev - 1;
                return Ok(SymbolRecord::new(symbol, symbol_id.clone(), scope_level));
            }
        }
        Err(SymbolError::MissingBinding) // `name` not found in any scope.
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

use std::ops::Index;
impl Index<&SymbolId> for SymbolTable {
    type Output = Symbol;

    // Interface for retrieving `Symbol`s from `all_symbols`
    fn index(&self, symbol_id: &SymbolId) -> &Self::Output {
        &self.all_symbols[symbol_id.0]
    }
}

// FIXME: broken because we have no syntax node here.
// #[test]
// fn test_symbol_table() {
//     use crate::types::{IsConst};

//     let mut table = SymbolTable::new();
//     let name1 = "x";
//     assert!(table.lookup(name1).is_err());
//     let typ = Type::Int(Some(32), IsConst::False);
//     let _symbol = table.new_binding(name1, &typ).unwrap();
//     let symbol_record = table.lookup(name1);
//     assert!(symbol_record.is_ok());
//     assert_eq!(symbol_record.clone().unwrap().symbol_id(), SymbolId(0));
//     assert_eq!(symbol_record.unwrap().scope_level(), 0);

//     table.enter_scope(ScopeType::Subroutine);
//     // First symbol is still in global scope
//     assert_eq!(table.lookup(&name1).unwrap().scope_level(), 0);

//     // Make new binding in current scope.
//     let _symbol_2 = table.new_binding(name1, &typ).unwrap();
//     assert_eq!(table.lookup(&name1).unwrap().scope_level(), 1);

//     table.exit_scope();
//     assert_eq!(table.lookup(&name1).unwrap().scope_level(), 0);
// }
