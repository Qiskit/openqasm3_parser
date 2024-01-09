// Copyright contributors to the openqasm-parser project

//! Python wrappers for semantic AST for the OpenQASM3 language
//! By semantic AST, we mean that all symbols are resolved and all expressions have been
//! assigned types.
//!
//! This is in a sense not true because, in order to cut corners, we use an AST that is built
//! before we know whether the program is semantically correct. To handle errors, the
//! implementation of this AST allows invalid types and unresolved Symbols, expressed via
//! `Option` and `Result` types. However, the AST is validated for semantic correctness before
//! this wrapper has access. So we can assume without checking that it is in fact semantically
//! correct. If these wrappers encounter a semantically incorrect AST, this is manifestation of
//! a bug in the the upstream code, but is not a bug in these wrappers.

use pyo3::prelude::*;
use pyo3::wrap_pymodule;
use pyo3::Python;

mod bytecode;
mod parse;
mod pybytecode;
mod semanticpy;
mod to_pybytecode;

#[pymodule]
fn _qasm3_bytecode(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pymodule!(parse::parse))?;
    m.add_wrapped(wrap_pymodule!(semanticpy::semanticpy))?;
    Ok(())
}
