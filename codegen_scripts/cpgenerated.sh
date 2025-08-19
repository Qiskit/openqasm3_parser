#!/usr/bin/env bash

# Copyright contributors to the openqasm-parser project
# SPDX-License-Identifier: Apache-2.0

# For generated.rs
# Copy the generated code from the temporary files to which it is written
# to it's final location where it will be compiled into the library.

cd .. && gcp -a --backup=t crates/oq3_parser/src/syntax_kind/_syntax_kind_enum.rs crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs

