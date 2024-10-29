## Pull Requests

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
> Don't run tests with `cargo test`. Rather use [./run_tests.sh](./run_tests.sh).
> Or run the command contained therein:
>
> `cargo test --lib --tests -- --skip sourcegen_ast --skip sourcegen_ast_nodes`

All pull requests must pass a CI check before being merged. You can check if CI will pass locally with
```shell
cargo fmt --all -- --check && cargo build --verbose && cargo clippy -- -D warnings && cargo test --verbose -- --skip sourcegen_ast --skip sourcegen_ast_nodes
```
A script for checking CI locally is [./local_CI.sh](./local_CI.sh)

### Clippy

One of the CI items is `cargo clippy`.
To handle a lot of errors at the command line you can use (for unix-like OS) `cargo clippy --color always &| less -R`.

### Modifying the ungrammar

An ["ungrammar"](https://docs.rs/ungrammar/latest/ungrammar/) (and [here](https://github.com/rust-analyzer/ungrammar)) describes a concrete syntax tree.

An ungrammar for OpenQASM 3 is
in [./crates/oq3_syntax/openqasm3.ungram](./crates/oq3_syntax/openqasm3.ungram).
For most work, it need not be edited.
If the file is modified,
three source files must be regenerated:
* [./crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs](./crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs)
* [./crates/oq3_syntax/src/ast/generated/nodes.rs](./crates/oq3_syntax/src/ast/generated/nodes.rs)
* [./crates/oq3_syntax/src/ast/generated/tokens.rs](./crates/oq3_syntax/src/ast/generated/tokens.rs)

For further information, see [./codegen_scripts/README.md](./codegen_scripts/README.md)
