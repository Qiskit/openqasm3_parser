default:
    @just --list

# One-stop CI-ish local run
ci:
    cargo fmt --all -- --check
    cargo build --release --verbose
    cargo test --verbose --lib --tests
    cargo clippy --all-targets -- -D warnings -D clippy::dbg_macro

# Regenerate source files from ungrammar and reformat
sourcegen:
    cargo clean
    cargo build -p oq3_syntax --features sourcegen
    cargo fmt -p oq3_syntax -p oq3_parser

_assert_empty_git_status:
    @if [ -n "$$(git status --porcelain)" ]; then \
        echo "Git working tree has uncommitted changes:"; \
        git --no-pager status --short; \
        echo ""; \
        echo "Diff against HEAD:"; \
        git --no-pager diff; \
    fi

# Check that source files are up to date (assumes a clean working tree)
check_sourcegen: _assert_empty_git_status sourcegen _assert_empty_git_status