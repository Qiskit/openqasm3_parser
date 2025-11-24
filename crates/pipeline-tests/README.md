- List discovered tests:
  From workspace root

### OpenQASM snippets

To see a list of tests
```sh
cargo test -p pipeline-tests -- --list
```

These tags are at the top of each snippet.
Set the tags according to what is expected.

* ok -   Stage must complete with no diagnostics.
* diag - Stage must complete and produce diagnostics (≥1); snapshot verifies content.
         This is for testing invalid input
* todo - Stage must complete and produce diagnostics (>=1); snapshot verifies content.
         Valid input that produces incorrect diagnostics.
* panic - Stage is expected to panic; harness catches the unwind.
* skip - Do not assert this stage (optionally skip running it).
```
// lex: ok|diag|todo|panic|skip
// parse: ok|diag|todo|panic|skip
// sema: ok|diag|todo|panic|skip
```

For summary status of tags on snippets
```sh
./tools/expect_summary.py
```
