#!/bin/env sh

# Copyright contributors to the openqasm-parser project
# SPDX-License-Identifier: Apache-2.0

# Run the tests, but skip the tests that do codegen. They currently
# are breaking the source. Or may break it.
# r-a uses some system to control this that we have not, and may never, set up.

cargo test -- --skip sourcegen_ast --skip sourcegen_ast_nodes
