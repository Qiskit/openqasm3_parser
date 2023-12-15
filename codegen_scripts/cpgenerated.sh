#!/bin/env sh

# For generated.rs
# Copy the generated code from the temporary files to which it is written
# to it's final location where it will be compiled into the library.

cd .. && cp -a --backup=t crates/parser/src/syntax_kind/_syntax_kind_enum.rs crates/parser/src/syntax_kind/syntax_kind_enum.rs

