// Copyright contributors to the openqasm-parser project

// use semantics::asg;
use semantics::asg::{GateModifier, TExpr};
use semantics::symbols::SymbolId;

#[derive(Clone)]
pub enum ByteCode {
    GateCall(GateCall),
    DeclareQreg(DeclareQuantum),
}

// Ugh, this is an exact copy of asg::GateCall except SymbolIdResult -> SymbolId
// This is unavoidable in Rust without some fancy approach.
#[derive(Clone)]
pub struct GateCall {
    name: SymbolId,
    params: Option<Vec<TExpr>>,
    qubits: Vec<TExpr>,
    modifier: Option<GateModifier>,
}

#[derive(Clone)]
pub struct DeclareQuantum {
    name: SymbolId,
}
