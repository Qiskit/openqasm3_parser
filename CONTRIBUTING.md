## Pull Requests

### Continuous Integration (CI)

All pull requests must pass a CI check before being merged. You can check if CI will pass locally with
```shell
cargo fmt --all -- --check && cargo build --verbose && cargo clippy -- -D warnings && cargo test --verbose -- --skip sourcegen_ast --skip sourcegen_ast_nodes
```

### Clippy

One of the CI items is `cargo clippy`.
To handle a lot of errors at the command line you can use (for unix-like OS) `cargo clippy --color always &| less -R`.


