// Copyright contributors to the openqasm-parser project

use pyo3::prelude::*;
use pyo3::Python;

//use std::fs; // for reading file
use oq3_semantics::asg;
use oq3_semantics::symbols;
use std::path::PathBuf;
//use semantics::syntax_to_semantics as synsem;
// use semantics::syntax_to_semantics::{
//     string_to_semantic
// };

use crate::semanticpy::{Program, SymbolTable};

fn _parse_file(file_path: String) -> (asg::Program, symbols::SymbolTable) {
    // let source_text = fs::read_to_string(file_path.clone())
    //     .expect(format!("Unable to read file {:?}", file_path).as_str());
    let pbuf = PathBuf::from(file_path);
    let result = oq3_semantics::syntax_to_semantics::parse_source_file(&pbuf);
    result.print_errors();
    let context = result.take_context();
    (context.program, context.symbol_table)
}

#[pyfunction]
pub fn parse_file(_py: Python<'_>, path: String) -> (Program, SymbolTable) {
    let (program, symbol_table) = _parse_file(path);
    (Program(program), SymbolTable(symbol_table))
}

#[pymodule]
pub fn parse(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(parse_file))?;
    Ok(())
}
