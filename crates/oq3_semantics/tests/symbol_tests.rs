// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::symbols;
use oq3_semantics::types;

const NUM_BUILTIN_CONSTS: usize = 6;

//
// Test API of symbols and symbol tables
//

#[test]
fn test_symbol_table_create() {
    use symbols::SymbolTable;

    let table = SymbolTable::new();
    assert_eq!(table.len_current_scope(), NUM_BUILTIN_CONSTS);
    let result = table.lookup("x");
    assert!(result.is_err());
}

#[test]
fn test_symbol_table_bind() {
    use symbols::SymbolTable;
    use types::{IsConst, Type};

    let mut table = SymbolTable::new();
    let symbol_name = "x";
    let x = table.new_binding(symbol_name, &Type::Bool(IsConst::False));
    assert!(x.is_ok());
    assert_eq!(table.len_current_scope(), 1 + NUM_BUILTIN_CONSTS);
    let result = table.lookup(symbol_name);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().symbol_id(), x.unwrap());
}
