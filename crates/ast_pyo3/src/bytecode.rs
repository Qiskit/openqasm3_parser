pub enum ByteCode {
    GateCall(GateCall),
    DeclareQreg(DeclareQreg),
}

struct GateCall {
    name: SymbolId,
    
}
