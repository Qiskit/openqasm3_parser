#!/bin/env sh

# Run locally the same checks that are done remotely in CI
# Works on linux, and maybe Mac OS.

cargo fmt --all -- --check && cargo build --verbose && cargo clippy -- -D warnings && cargo test --verbose -- --skip sourcegen_ast --skip sourcegen_ast_nodes
