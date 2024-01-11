#!/bin/env sh

# Copyright contributors to the openqasm-parser project

# This is not how r-a does generation.
# But I find a boot strapping problem.
# So we write generated code to a temporary file (GJL Aug 2023)

# 1 Update tokens here: crates/oq3_syntax/src/tests/ast_src.rs
# 2 Update tokens in `fn lower` here crates/oq3_syntax/src/tests/sourcegen_ast.rs
# 3 Update grammar here: crates/oq3_syntax/openqasm3.ungram
# This should be enough for codegen.

# But you still need to add the things to the parser grammar
# 4 You might need to update crates/oq3_parser/src/grammar/expressions/atom.rs
#   Or crates/oq3_parser/src/grammar/items.rs (eg. for `gate`)
# Or other grammar files

# Generated files are not given their final names in order not to clobber exisiting generated code
# Here are the temporary filenames and the final filenames
# You have to copy them manually.
# crates/oq3_syntax/src/ast/generated/_new_tokens.rs --> tokens.rs
# crates/oq3_syntax/src/ast/generated/_new_nodes.rs --> nodes.rs
# crates/oq3_parser/src/syntax_kind/_generated.rs --> generated.rs

# Update: Running this script now seems robust. Originally all codegen was done
# by a single test. I split it into two tests and run each of them twice.

# Running the tests always fails at least twice when generating the code.
# If there is really an error, it would fail indefinitely.
# Running three times works.

./mk_syntax_kinds.sh

./mk_syntax_tokens.sh

./mk_nodes.sh
./mk_nodes.sh

# Warning: Do not format till all files are generated.
# Don't know why, but code gen will fail otherwise.

cd ..
rustfmt crates/oq3_parser/src/syntax_kind/_syntax_kind_enum.rs
rustfmt crates/oq3_syntax/src/ast/generated/_new_nodes.rs
rustfmt crates/oq3_syntax/src/ast/generated/_new_tokens.rs
