## Prerequisites

This project uses [`just`](https://github.com/casey/just) as a command runner.  

Install it first:

```sh
cargo install just
```

### Verify Installation

Run:

```sh
just --list
```

You should see a list of available recipes (such as `ci`, `sourcegen`, `check_sourcegen` and others).  


## Pull Requests

- All pull requests must pass CI before being merged.  
- Please commit regenerated files when modifying grammar or code generation.  
- Use the provided `just` recipes to ensure consistency.

### Debugging / Developing

Set the environment variable `QASM3_PATH` to the directory containing the QASM 3 example you are working on.
Alternatively, in the examples below, you can use the fully qualified path to `example.qasm`.

Use `semdemo` to see how the parser/analyzer handles your example in `example.qasm`.
You will usually be concerned with two data structures, which we call the AST and the ASG.
In order to test construction of the ASG, the AST must be constructed without errors.

#### The AST

Run
```shell
cargo run --color always --example semdemo -- parse example.qasm
```
If parsing was successful, you should see something like
```
Found 3 stmts
Found 0 parse errors:
[]
```
followed by a representation of the AST.

### The ASG
If running `semdemo` with the option `parse` as above prints no syntax errors, then
you can proceed to the ASG. Run
```shell
cargo run --color always --example semdemo -- semantic example.qasm
```
If there were in fact syntax errors in producing the AST, then this command will
pretty-print those errors, but will not attempt to construct the ASG.
Otherwise the ASG will be constructed, and any *semantic* errors found will be
printed.

### Testing and Continuous Integration (CI)

> [!IMPORTANT]
All pull requests must pass CI. To run the full suite locally, use:

```sh
just ci
```

> Do not run `cargo test` directly — always use `just ci` to match CI.

Note, the github pipeline also verifies that generated sources are up to date:

```sh
just check_sourcegen
```

### Clippy

Clippy is included in the `just ci` recipe. For manual inspection with paging:

```sh
cargo clippy --color always |& less -R
```

### Modifying the ungrammar

An ["ungrammar"](https://docs.rs/ungrammar/latest/ungrammar/) (and [here](https://github.com/rust-analyzer/ungrammar)) describes a concrete syntax tree.

An ungrammar for OpenQASM 3 is
in [./crates/oq3_syntax/openqasm3.ungram](./crates/oq3_syntax/openqasm3.ungram).
For most work, it need not be edited.

If the file is modified, run:

```sh
just sourcegen
```

> This triggers the `build.rs` script in `oq3_parser` with the `sourcegen` feature.

The following three source files may be updated:
* [./crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs](./crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs)
* [./crates/oq3_syntax/src/ast/generated/nodes.rs](./crates/oq3_syntax/src/ast/generated/nodes.rs)
* [./crates/oq3_syntax/src/ast/generated/tokens.rs](./crates/oq3_syntax/src/ast/generated/tokens.rs)

Commit regenerated files along with your changes.

### Style & Formatting

- Code must be formatted with `cargo fmt`.  
- Lints must pass with `cargo clippy` (warnings are treated as errors).  
- Avoid committing debug macros (`dbg!`) — they are denied in CI.  
