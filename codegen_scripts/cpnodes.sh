#!/bin/env sh

# Copyright contributors to the openqasm-parser project
# SPDX-License-Identifier: Apache-2.0

# For nodes.rs
# Copy the generated code from the temporary files to which it is written
# to its final location where it will be compiled into the library.

cd .. && cp -a --backup=t  crates/oq3_syntax/src/ast/generated/_new_nodes.rs crates/oq3_syntax/src/ast/generated/nodes.rs && \
  cp -a --backup=t  crates/oq3_syntax/src/ast/generated/_new_tokens.rs crates/oq3_syntax/src/ast/generated/tokens.rs
