// Copyright contributors to the openqasm-parser project

//! This is the actual "grammar" of the OpenQASM 3 language.
//!
//! Each function in this module and its children corresponds
//! to a production of the formal grammar. Submodules roughly
//! correspond to different *areas* of the grammar. By convention,
//! each submodule starts with `use super::*` import and exports
//! "public" productions via `pub(super)`.
//!
//! Because OQ3 is simpler than Rust, most submodules mentioned above
//! have been removed.
//!
//! See docs for [`Parser`](super::parser::Parser) to learn about API,
//! available to the grammar, and see docs for [`Event`](super::event::Event)
//! to learn how this actually manages to produce parse trees.
//!
//! Code in this module also contains inline tests, which start with
//! `// test name-of-the-test` comment and look like this:
//!
//! Most of the inline tests have been removed and not yet replaced with
//! OQ3 tests.
//!
//! ```
//! // test function_with_zero_parameters
//! // fn foo() {}
//! ```
//!
//! Note: `xtask` is not (yet?) updated for OQ3.
//! After adding a new inline-test, run `cargo test -p xtask` to
//! extract it as a standalone text-fixture into
//! `crates/syntax/test_data/parser/`, and run `cargo test` once to
//! create the "gold" value.
//!
//! Coding convention: rules like `where_clause` always produce either a
//! node or an error, rules like `opt_where_clause` may produce nothing.
//! Non-opt rules typically start with `assert!(p.at(FIRST_TOKEN))`, the
//! caller is responsible for branching on the first token.

mod expressions;
mod items;
mod params;

use crate::{
    parser::{CompletedMarker, Marker, Parser},
    SyntaxKind::{self, *},
    TokenSet, T,
};

pub(crate) mod entry {
    use super::*;

    pub(crate) mod top {
        use super::*;

        pub(crate) fn source_file(p: &mut Parser<'_>) {
            let m = p.start();
            items::source_file_contents(p, false);
            m.complete(p, SOURCE_FILE);
        }

        pub(crate) fn expr(p: &mut Parser<'_>) {
            let m = p.start();
            expressions::expr(p);
            if p.at(EOF) {
                m.abandon(p);
                return;
            }
            while !p.at(EOF) {
                p.bump_any();
            }
            m.complete(p, ERROR);
        }

    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockLike {
    Block,
    NotBlock,
}

// FIXME: flow control expressions removed because flow control are not expressions in OQ3
// This might break something
impl BlockLike {
    fn is_block(self) -> bool {
        self == BlockLike::Block
    }

    fn is_blocklike(kind: SyntaxKind) -> bool {
        matches!(kind, BLOCK_EXPR )
//        matches!(kind, BLOCK_EXPR | IF_EXPR | WHILE_EXPR | FOR_EXPR)
    }
}

fn plain_type(p: &mut Parser<'_>) {
    assert!(p.current().is_type_name());
    let r = p.start();
    p.bump_any();
    r.complete(p, PATH_TYPE);
}

/// Parse the optional return type of a `defcal` or `def` definition.
/// Return `true` if the return type was found, else `false.
fn opt_ret_type(p: &mut Parser<'_>) -> bool {
    if p.at(T![->]) {
        let m = p.start();
        p.bump(T![->]);
        //        types::type_no_bounds(p);
        if p.current().is_type_name() {
            plain_type(p);
        } else {
            p.error("Expected return type after ->");
            m.abandon(p);
            return false;
        }
        m.complete(p, RET_TYPE);
        true
    } else {
        false
    }
}

/// Parse an identifer signifying a name. Attempt recovery
/// on failure.
fn name_r(p: &mut Parser<'_>, recovery: TokenSet) {

    // FIXME: testing. dont know if this belongs
    if p.at(HARDWAREIDENT) {
        let m = p.start();
        p.bump(HARDWAREIDENT);
        m.complete(p, HARDWARE_QUBIT);
        return;
    }

    if p.at(IDENT) {
        let m = p.start();
        p.bump(IDENT);
        m.complete(p, NAME);
    } else {
        p.err_recover("expected a name", recovery);
    }
}

/// Parse an identifer signifying a name. Do not attempt
/// error recovery.
fn name(p: &mut Parser<'_>) {
    name_r(p, TokenSet::EMPTY);
}

/// The `parser` passed this is required to at least consume one token if it returns `true`.
/// If the `parser` returns false, parsing will stop.
fn delimited(
    p: &mut Parser<'_>,
    bra: SyntaxKind,
    ket: SyntaxKind,
    consume_braket: bool,
    delim: SyntaxKind,
    first_set: TokenSet,
    mut parser: impl FnMut(&mut Parser<'_>) -> bool,
) {
    if consume_braket {
        p.bump(bra);
    }
    while !p.at(ket) && !p.at(EOF) {
        if !parser(p) {
            break;
        }
        if !p.at(delim) {
            if p.at_ts(first_set) {
                p.error(format!("expected {:?}", delim));
            } else {
                break;
            }
        } else {
            p.bump(delim);
        }
    }
    if consume_braket {
        p.expect(ket);
    }
}
