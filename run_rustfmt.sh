#!/bin/env sh

# Copyright contributors to the openqasm-parser project
# SPDX-License-Identifier: Apache-2.0

# Ideally we want to run rustfmt on all crates
cd crates/source_file && cargo fmt && cd ../..
cd crates/semantics && cargo fmt && cd ../..



