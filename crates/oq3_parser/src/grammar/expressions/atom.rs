// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use super::*;

pub(crate) const PATH_FIRST: TokenSet = TokenSet::new(&[IDENT, HARDWAREIDENT, T![:], T![<]]);

pub(crate) const LITERAL_FIRST: TokenSet = TokenSet::new(&[
    BIT_STRING,
    BYTE,
    CHAR,
    FLOAT_NUMBER,
    INT_NUMBER,
    STRING,
    T![true],
    T![false],
]);

// Also for imaginary literals
const TIMING_LITERAL_FIRST: TokenSet = TokenSet::new(&[INT_NUMBER, FLOAT_NUMBER]);

pub(crate) fn literal(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !p.at_ts(LITERAL_FIRST) {
        return None;
    }
    // There is no string literal in the OQ3 grammar.
    if p.at(STRING) {
        p.error("Unexpected string literal");
    }
    let m = p.start();
    // If the first token after the current one is an identifer, then the only
    // syntactically correct construct is a timing or imaginary literal.  Note that `s` is a
    // valid suffix for both a timing literal and a variable identifier, we
    // can't make `s` a keyword token.  This is because, in the parser, I don't
    // know how to make a string either an identifer or "keyword"
    // depending on context. So we parse an identifier and validate in validate.rs.
    if matches!(p.nth(1), IDENT) {
        if !p.at_ts(TIMING_LITERAL_FIRST) {
            p.error("Timing and imaginary literals must begin with an integer or float literal");
        }
        // We don't have access to the text of the identifier here, so we can't distinguish
        // timing literals from imaginary literals. We tag everything TIMING_LITERAL
        // Later in semantic analysis we separate imaginary literals from timing literals.
        let m2 = p.start(); // TIMING_LITERAL
        p.bump_any(); // The numeric literal
        m.complete(p, LITERAL);
        identifier(p); // The time unit suffix, or quasi-suffix.
        return Some(m2.complete(p, TIMING_LITERAL));
    }
    p.bump_any();
    Some(m.complete(p, LITERAL))
}

// E.g. for after the break in `if break {}`, this should not match
pub(super) const ATOM_EXPR_FIRST: TokenSet =
    LITERAL_FIRST.union(PATH_FIRST).union(TokenSet::new(&[
        T!['('],
        T!['{'],
        T!['['],
        T![|],
        T![box],
        T![const],
        T![for],
        T![if],
        T![let],
        T![return],
        T![while],
        T![measure],
        T![inv],
        T![ctrl],
        T![negctrl],
        T![pow],
        T![gphase],
    ]));

pub(super) const EXPR_RECOVERY_SET: TokenSet = TokenSet::new(&[T![')'], T![']']]);

/// This parses most expressions.
/// Probably ~~a bad idea~~ impossible to include parsing of other things here.
pub(super) fn atom_expr(
    p: &mut Parser<'_>,
    _r: Restrictions,
) -> Option<(CompletedMarker, BlockLike)> {
    if let Some(m) = literal(p) {
        return Some((m, BlockLike::NotBlock));
    }
    let la = p.nth(1);
    // Do we need to check `la == T!['(']` ?
    // If the type spec has a width (designator), then `la` will not be `(` in any case.
    if p.current().is_classical_type() {
        let m = cast_expr(p);
        return Some((m, BlockLike::NotBlock));
    }
    let done = match p.current() {
        HARDWAREIDENT => hardware_qubit(p),
        T!['('] => tuple_expr(p),
        // Remove array_expr in order than expressions:index_expr is used
        // Ugh. this is needed for `int[32]` for example. But it prevents array indexing `v[1:3]` from working
        // Need to distinguish these
        T!['['] => array_expr(p),
        T![box] => box_expr(p, None),
        T![measure] => measure_expression(p),
        T![return] => return_expr(p),
        T!['{'] => block_expr(p),
        T![inv] | T![pow] | T![ctrl] | T![negctrl] => modified_gate_call_expr(p),
        T![gphase] => gphase_call_expr(p),
        IDENT if (la == IDENT || la == HARDWAREIDENT) => gate_call_expr(p),
        // This may be wrapped in INDEXED_IDENTIFIER later
        IDENT => identifier(p),
        _ => {
            p.err_and_bump("atom_expr: expected expression");
            return None;
        }
    };
    let blocklike = if BlockLike::is_blocklike(done.kind()) {
        BlockLike::Block
    } else {
        BlockLike::NotBlock
    };
    Some((done, blocklike))
}

// Note: `CAST_EXPR` also parsed in `classical_declaration_stmt` in items.rs
// FIXME. Do we need to worry about precedence here, as in call_expr ?
pub(crate) fn cast_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    type_spec(p);
    p.expect(T!['(']);
    expr(p);
    p.expect(T![')']);
    m.complete(p, CAST_EXPRESSION)
}

fn gphase_call_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T![gphase]));
    let m = p.start();
    p.bump(T![gphase]);
    expressions::expr(p);
    m.complete(p, G_PHASE_CALL_EXPR)
}

fn modified_gate_call_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    loop {
        match p.current() {
            T![inv] => {
                let m1 = p.start();
                p.bump(T![inv]);
                if p.at(T![@]) {
                    p.bump(T![@]);
                } else if p.at(T!['(']) {
                    p.error("Modifier `inv` accepts no parameter. Expecting `@`");
                } else {
                    p.error("Expecting `@`");
                }
                m1.complete(p, INV_MODIFIER);
            }

            T![pow] => {
                let m1 = p.start();
                p.bump(T![pow]);
                if p.at(T!['(']) {
                    let m2 = p.start();
                    p.expect(T!['(']);
                    expressions::expr(p);
                    p.expect(T![')']);
                    m2.complete(p, PAREN_EXPR);
                } else {
                    p.error("expecting argument to pow gate modifier");
                }
                p.expect(T![@]);
                m1.complete(p, POW_MODIFIER);
            }

            T![ctrl] => {
                let m1 = p.start();
                p.bump(T![ctrl]);
                if p.at(T!['(']) {
                    let m2 = p.start();
                    p.expect(T!['(']);
                    expressions::expr(p);
                    p.expect(T![')']);
                    m2.complete(p, PAREN_EXPR);
                }
                p.expect(T![@]);
                m1.complete(p, CTRL_MODIFIER);
            }

            T![negctrl] => {
                let m1 = p.start();
                p.bump(T![negctrl]);
                if p.at(T!['(']) {
                    let m2 = p.start();
                    p.expect(T!['(']);
                    expressions::expr(p);
                    p.expect(T![')']);
                    m2.complete(p, PAREN_EXPR);
                }
                p.expect(T![@]);
                m1.complete(p, NEG_CTRL_MODIFIER);
            }

            _ => break,
        }
    }
    if p.at(T![gphase]) {
        gphase_call_expr(p);
    } else {
        gate_call_expr(p);
    }
    m.complete(p, MODIFIED_GATE_CALL_EXPR)
}

fn gate_call_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    identifier(p); // name of gate
    if p.at(T!['(']) {
        expressions::call_arg_list(p);
    }
    params::arg_list_gate_call_qubits(p);
    m.complete(p, GATE_CALL_EXPR)
}

fn measure_expression(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![measure]);
    match p.current() {
        IDENT | HARDWAREIDENT => {
            let m1 = p.start();
            // Parses elements that can be cast to GateOperand
            params::arg_gate_call_qubit(p, m1);
        }
        _ => {
            p.error("expecting qubit(s) to measure");
        }
    }
    m.complete(p, MEASURE_EXPRESSION)
}

// FIXME: changed the kind to `NAME`
// FIXME: changed it back to IDENTIFIER
pub(crate) fn identifier(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.expect(IDENT);
    m.complete(p, IDENTIFIER)
}

pub(crate) fn hardware_qubit(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(HARDWAREIDENT);
    m.complete(p, HARDWARE_QUBIT)
}

// Parses either a `PAREN_EXPR` or a `TUPLE_EXPR`.
// The `TUPLE_EXPR` is not part of OQ3. At most, it may be
// a quasi-expression. If it does not appear at all, we
// need to log an error here instead of parsing it.
fn tuple_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T!['(']));
    let m = p.start();
    p.expect(T!['(']);

    let mut saw_comma = false;
    let mut saw_expr = false;

    if p.eat(T![,]) {
        p.error("expected expression, found comma instead");
        saw_comma = true;
    }

    while !p.at(EOF) && !p.at(T![')']) {
        saw_expr = true;

        if expr(p).is_none() {
            break;
        }

        if !p.at(T![')']) {
            saw_comma = true;
            p.expect(T![,]);
        }
    }
    p.expect(T![')']);
    m.complete(
        p,
        if saw_expr && !saw_comma {
            PAREN_EXPR
        } else {
            TUPLE_EXPR
        },
    )
}

fn array_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T!['[']));
    let m = p.start();

    let mut n_exprs = 0u32;
    let mut has_semi = false;

    p.bump(T!['[']);
    while !p.at(EOF) && !p.at(T![']']) {
        n_exprs += 1;

        // test array_attrs
        // const A: &[i64] = &[1, #[cfg(test)] 2];
        if expr(p).is_none() {
            break;
        }

        if n_exprs == 1 && p.eat(T![;]) {
            has_semi = true;
            continue;
        }

        if has_semi || !p.at(T![']']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T![']']);
    m.complete(p, ARRAY_EXPR)
}

pub(crate) fn try_block_expr(p: &mut Parser<'_>) {
    if !p.at(T!['{']) {
        p.error("expected a block");
        return;
    }
    let _ = block_expr(p);
}

pub(crate) fn block_expr(p: &mut Parser<'_>) -> CompletedMarker {
    // FIXME: can't use this check in refactor.
    // get this working again.
    // if !p.at(T!['{']) {
    //     p.error("expected a block");
    //     return;
    // }
    assert!(p.at(T!['{']));
    let m = p.start();
    p.bump(T!['{']);
    expr_block_contents(p);
    p.expect(T!['}']);
    m.complete(p, BLOCK_EXPR)
}

fn return_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T![return]));
    let m = p.start();
    p.bump_any(); // return
                  // parse possible returned expression
    if p.at_ts(EXPR_FIRST) {
        expr(p);
    }
    m.complete(p, RETURN_EXPR)
}

fn box_expr(p: &mut Parser<'_>, m: Option<Marker>) -> CompletedMarker {
    assert!(p.at(T![box]));
    let m = m.unwrap_or_else(|| p.start());
    p.bump(T![box]);
    if p.at_ts(EXPR_FIRST) {
        expr(p);
    }
    m.complete(p, BOX_EXPR)
}
