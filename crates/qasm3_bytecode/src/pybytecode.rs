// Copyright contributors to the openqasm-parser project

use pyo3::prelude::*;
// use pyo3::Python;

#[pyclass(frozen)]
#[derive(Clone)]
pub struct Bytecode {
    #[pyo3(get)]
    opcode: OpCode,
    #[pyo3(get)]
    operands: PyObject,
}

#[allow(unused)]
#[pyclass(frozen)]
#[derive(Clone)]
pub enum OpCode {
    GateCall,
    DeclareGate,
    DeclareQreg,
    BlockStart,
    BlockEnd,
    //    DeclareQubit,
}
