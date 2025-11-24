- List discovered tests:
  From workspace root

### OpenQASM snippets

To see a list of tests
```sh
cargo test -p pipeline-tests -- --list
```

These tags are at the top of each snippet.
Set the tags according to what is expected.
```
// lex: ok|diag|todo|panic|skip
// parse: ok|diag|todo|panic|skip
// sema: ok|diag|todo|panic|skip
```
