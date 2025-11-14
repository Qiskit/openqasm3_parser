# OpenQASM 3 Parser
[![License](https://img.shields.io/github/license/Qiskit/rustworkx.svg?style=popout-square)](https://opensource.org/licenses/Apache-2.0)
[![Minimum rustc 1.79](https://img.shields.io/badge/rustc-1.79+-blue.svg)](https://rust-lang.github.io/rfcs/2495-min-rust-version.html)
[![oq3_semantics crate](https://img.shields.io/crates/v/oq3_semantics.svg)](https://crates.io/crates/oq3_semantics)


This project provides a compiler front end for OpenQASM 3 language (OQ3).

In this document, this parser is referred to as `openqasm3_parser`.

### Why is this important?

There is a lot of interest in using OpenQASM 3 for a number of purposes. For example, for controlling
hardware and as an exchange medium for quantum circuits. This project aims to provide a performant,
robust front end to compilers and importers. There are no other open source (or public) projects of this
kind.

Differences with the [OpenQASM reference parser](https://github.com/openqasm/openqasm) include

* The parser in `openqasm3_parser` is much more performant.
  A crude test with large source files showed parse time reduced by a factor of 80.
* `openqasm3_parser` performs semantic analysis. In particular diagnostics and error reporting
   are much better.

### Contributing

If you are interested in contributing, browsing the issues is a good place to start.
Some of the issues are [tagged with the label "Looking for assignee"](https://github.com/Qiskit/openqasm3_parser/issues?q=label%3A%22Looking+for+assignee%22+is%3Aopen+sort%3Aupdated-desc).
Issues with the tag include a bit more context than some other issues in order to guide a newcomer. Also, the core developers are not
planning on working on these issues in the immediate future.
However, this does not mean you can't tackle an issue without this tag if it is a better fit.

There are a few more tips in [CONTRIBUTING.md](./CONTRIBUTING.md), as well as some later in this README.

### Crates (roughly one installable library per crate)

The first three crates are based on tools for `rust` and `rust-analyzer`.

* [oq3_lexer](./crates/oq3_lexer) -- A lightly modified version of the `rustc` (the rust compiler) lexer.
* [oq3_parser](./crates/oq3_parser) -- Ingests output of `oq3_lexer` and outputs a concrete syntax tree.
* [oq3_syntax](./crates/oq3_syntax) -- Ingests output of `oq3_parser` and outputs an abstract syntax tree (AST).
The rust-analyzer [documentation](#design) sometimes refers to this AST by something like "typed AST".
This can be confusing. It does not mean that semantic
analysis has been performed and OQ3 types have been assigned to all expressions. It means that the rust type system is
used to encode syntactic elements, in contrast to some lower representations in the same crate.
* [oq3_semantics](./crates/oq3_semantics) -- Performs [semantic analysis](https://en.wikipedia.org/wiki/Compiler#Front_end)
and outputs an [abstract semantic graph (ASG)](https://en.wikipedia.org/wiki/Abstract_semantic_graph)
There are other names for this structure. But "ASG" is convenient.
* [oq3_source_file](./crates/oq3_source_file) -- A higher-level interface to the syntactic AST. This sits beetween the syntactic AST and
semantic ASG. This crate manages the main source file and incuded source files.

<details>
  <summary>What is a rust "crate"</summary>

We talk about rust ["crates"](https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html).
A rust library crate is more or less the source for a rust library that is developed, built, and installed with the rust package manager [cargo](https://doc.rust-lang.org/cargo/).
This single repository contains more than one separately installable crates. In the future, this repository may also be used to generate other artifacts.

</details>

### Using this front end

A reminder: A front end is not of much use unless you have a back end. Examples showing the entry points and how to use them,
can be found in [./crates/oq3_semantics/examples/semdemo.rs](./crates/oq3_semantics/examples/semdemo.rs).

```shell
shell> export QASM3_PATH=./crates/oq3_semantics/examples/qasm/
shell> cargo run --example semdemo -- semantic scratch1.qasm
```

Replace `scratch1.qasm` with some file found in [./crates/oq3_semantics/examples/qasm/](./crates/oq3_semantics/examples/qasm/).

#### Search path

The environment variable `QASM_PATH` is a colon separated list of paths. Note that the name follows the venerable unix tradition of
ending in `PATH` rather than `PATHS`. The code that retrives the paths uses routines `std::path` which may actually handle
path specifications on other platforms.

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

Some of this code is modified from code found in [rust-analyzer](https://github.com/rust-lang/rust-analyzer).
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
