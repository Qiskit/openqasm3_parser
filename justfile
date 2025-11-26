# list recipes
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
    @if git status --porcelain | grep .; then \
        echo "Uncommitted changes found"; \
        git --no-pager diff; \
        exit 1; \
    fi

# Check that source files are up to date (assumes a clean working tree)
check_sourcegen: _assert_empty_git_status sourcegen _assert_empty_git_status
