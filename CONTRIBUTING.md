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

### Continuous Integration (CI)

All pull requests must pass a CI check before being merged. You can check if CI will pass locally with
```shell
cargo fmt --all -- --check && cargo build --verbose && cargo clippy -- -D warnings && cargo test --verbose -- --skip sourcegen_ast --skip sourcegen_ast_nodes
```

### Clippy

One of the CI items is `cargo clippy`.
To handle a lot of errors at the command line you can use (for unix-like OS) `cargo clippy --color always &| less -R`.
