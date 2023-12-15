#!/bin/env sh

# build the library and copy the library and give it the required name.

LIBPARENT=testbytecode
LIBDIR=testbytecode/qasm3_bytecode

[ -d "$LIBPARENT"  ] || mkdir "$LIBPARENT"
[ -d "$LIBDIR"  ] || mkdir "$LIBDIR"

# for release
cargo build --release && cp -a ../../target/release/libqasm3_bytecode.so "$LIBDIR"/_qasm3_bytecode.so

# for debug
# cargo build  && cp -a ../../target/debug/libqasm3_bytecode.so pyo3_examples/qasm3_bytecode/_qasm3_bytecode.so
