#!/bin/env sh

# Copyright contributors to the openqasm-parser project
# SPDX-License-Identifier: Apache-2.0

# See mkgenerated.sh

echo
echo =============== Running sourcegen_ast_nodes once ===============
echo

cd .. && cargo test sourcegen_ast_nodes

# echo
# echo =============== Running sourcegen_ast_nodes twice ===============
# echo

# cargo test sourcegen_ast_nodes

# rustfmt crates/oq3_syntax/src/ast/generated/_new_nodes.rs
# rustfmt crates/parser/src/syntax_kind/_syntax_kind_enum.rs
# rustfmt crates/oq3_syntax/src/ast/generated/_new_tokens.rs
