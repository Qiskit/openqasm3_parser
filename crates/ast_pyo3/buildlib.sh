#!/bin/env sh

# build the library and copy the library and give it the required name.

# for release
cargo build --release && cp -a ../../target/release/libqasm3_ast.so pyo3_examples/qasm3_ast/_qasm3_ast.so

# for debug
# cargo build  && cp -a ../../target/debug/libqasm3_ast.so pyo3_examples/qasm3_ast/_qasm3_ast.so
