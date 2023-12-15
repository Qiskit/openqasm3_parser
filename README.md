# OpenQASM 3 Parser

This is a parser for the OpenQASM 3 language (OQ3) written in Rust.
In this document, this parser is referred to as `openqasm3_parser`.

We talk about rust ["crates"](https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html).
A rust library crate is more or less the source for a rust library that is developed, built, and installed with the rust package manager [cargo](https://doc.rust-lang.org/cargo/).
This single repository contains more than one separately installable crates. In the future, this repository may also be used to generate other artifacts.

The code does not yet perfectly do everything promised below.

### Crates (roughly one installable library per crate)

* [oq3_lexer](./crates/oq3_lexer) -- This is a lightly modified version of the `rustc` (the rust compiler) lexer.
* [oq3_parser](./crates/oq3_parser) -- This parses OQ3 source as lexed by `oq3_lexer` into a nested sequence of tagged sections of text
according the OQ3 grammar. This is based on the rust-analyzer parser crate. All nodes are represented by an instance
of a single Rust `struct` (with some support `structs`).
* [oq3_syntax](./crates/oq3_syntax) -- This crate transforms the AST produced by `oq3_parser` into an AST implemented
as standard rust data structures: `enum` and `struct` and provides an interface for working with the AST.
The types of these data structures reflect the syntactic elements of OQ3. The rust-analyzer
documentation sometimes refers to this AST by something like "typed AST". This can be confusing. It does not mean that semantic
analysis has been performed and OQ3 types have been assigned to all expressions. Rather it is meant to highlight the distinction:
The AST produced by `oq3_syntax` represents syntactic elements using the rust type system. The lower AST produced by
`oq3_parser` represents syntactic elements via tags.
* [semantic_ast](./crates/semantic_ast) -- Generates an AST for OQ3 with the types of all typed OQ3 elements resolved and
the referent of all identifiers resolved.
* [source_file](./crates/source_file) -- A higher-level interface to the syntactic AST. This sits beetween the syntactic and
semantic ASTs. This crate manages source files, and incuded source files, and diagnostic messages.
* [ast_pyo3](./crates/source_file) A Python interface. This is a bit exploratory. But fixing the approach is a development priority.

#### Supporting crates

* [sourcegen](./crates/sourcegen) -- supports code generation. This is a very small crate that is copied here because the external
crate has a bug.

### Warning !

Do not run `cargo test`. Rather use `./run_tests.sh` or commands found therein. This is because codegen is implemented via
the test system (you read correctly). If possible, we plan to change this to a more conventional approach.

### Design

Code from rust-analyzer has been borrowed and modified for the lower levels of parsing.

The [developer documents for rust-analyzer](https://github.com/rust-lang/rust-analyzer/tree/master/docs/dev) are very relevant as the
structure has not been changed in adapting to OQ3.

* [Syntax](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md)
* [Red/Green trees](https://ericlippert.com/2012/06/08/red-green-trees/) taken from the C# parser.
* Pratt parsing
    * [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
    * [From Pratt to Dijkstra](https://matklad.github.io/2020/04/15/from-pratt-to-dijkstra.html)
    * [Resilient parsing](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html)


## Notes

Some of this code is modified from code in [rust-analyzer](https://github.com/rust-lang/rust-analyzer).
It was taken at [this commit](https://github.com/rust-lang/rust-analyzer/pull/15380):

    commit d398ad3326780598bbf1480014f4c59fbf6461a7
    Merge: 2f2cf21da 6990d0f26
    Author: bors <bors@rust-lang.org>
    Date:   Wed Aug 2 14:28:41 2023 +0000

        Auto merge of #15380 - HKalbasi:mir, r=HKalbasi

        Fix unsized struct problems in mir eval


<!--  LocalWords:  OpenQASM openqasm3 workspace IDEs repo qiskit qasm3 lexing
<!--  LocalWords:  ANTLR untyped AST Qiskit QuantumCircuit oq3 rustc lex enum
<!--  LocalWords:  lossless TokenKind fn lexer's filename codegen ungram bors
<!--  LocalWords:  d398ad3326780598bbf1480014f4c59fbf6461a7 2f2cf21da 6990d0f26
<!--  LocalWords:  HKalbasi mir unsized struct eval lexemes
 -->
