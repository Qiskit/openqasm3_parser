#!/usr/bin/env sh

# Run locally the same checks that are done remotely in CI
# Works on linux, and maybe Mac OS.

cargo fmt --all -- --check || exit 1
cargo build --release --verbose || exit 1
cargo clippy --all-targets -- -D warnings || exit 1
cargo test --verbose -- --skip sourcegen_ast --skip sourcegen_ast_nodes || exit 1
