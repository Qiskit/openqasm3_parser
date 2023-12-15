#!/bin/env sh

# See mkgenerated.sh

echo
echo  =============== Running sourcegen_ast_tokens once ===============
echo

cargo test sourcegen_ast_tokens

# rustfmt crates/oq3_syntax/src/ast/generated/_new_nodes.rs
# rustfmt crates/oq3_parser/src/syntax_kind/_syntax_kind_enum.rs
# rustfmt crates/oq3_syntax/src/ast/generated/_new_tokens.rs

