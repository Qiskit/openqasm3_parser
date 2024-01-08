#!/bin/env sh

# Copyright contributors to the openqasm-parser project

# Remove backups of generated code.
# Also remove the temporary file that generated code is written to.
# The generated files that are actually in use, nodes.rs and generated.rs
# are not touched by this script.

cd ..
rm crates/parser/src/syntax_kind/_syntax_kind_enum.rs
rm crates/parser/src/syntax_kind/syntax_kind_enum.rs.~*

rm crates/oq3_syntax/src/ast/generated/_new_nodes.rs
rm crates/oq3_syntax/src/ast/generated/nodes.rs.~*

# This one is not backed up or copied because it rarely
# changes.
rm crates/oq3_syntax/src/ast/generated/_new_tokens.rs
