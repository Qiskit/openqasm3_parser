// Copyright contributors to the openqasm-parser project

use super::*;

use crate::grammar;
use crate::grammar::expressions;

pub(crate) const PATH_FIRST: TokenSet = TokenSet::new(&[IDENT, HARDWAREIDENT, T![:], T![<]]);

pub(crate) const LITERAL_FIRST: TokenSet = TokenSet::new(&[
    T![true],
    T![false],
    INT_NUMBER,
    FLOAT_NUMBER,
    SIMPLE_FLOAT_NUMBER,
    BYTE,
    CHAR,
    STRING,
    BIT_STRING,
]);

pub(crate) fn literal(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !p.at_ts(LITERAL_FIRST) {
        return None;
    }
    let m = p.start();
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
    // Do we need to check la == T!['('] ?
    if p.current().is_classical_type() {
        let m = expressions::cast_expr(p);
        return Some((m, BlockLike::NotBlock));
    }
    let done = match p.current() {
        HARDWAREIDENT => hardware_qubit(p),
        T!['('] => tuple_expr(p),
        // Remove array_expr in order than expressions:index_expr is used
        // Ugh. this is needed for `int[32]` for example. But it prevents array indexing `v[1:3]` from working
        // Need to distinguish these
        T!['['] => array_expr(p),
        //        T![if] => if_expr(p),
        T![box] => box_expr(p, None),
        //        T![while] => while_expr(p, None),
        T![measure] => measure_expression(p),
        T![return] => return_expr(p),
        T!['{'] => block_expr(p),
        T![for] => for_expr(p, None),
        // FIXME: This is the simplest gate call. Need to cover
        // `mygate(myparam) q1, q2;` as well.
        //        IDENT if la == IDENT => gate_call_expr(p),
        IDENT if (la == T![=] && p.nth(2) != T![=]) => grammar::items::assignment_statement(p),
        // FIXME: An identifer bound by the user in the program.
        // Need to handle more than identifier.
        // Also `NAME` is probably not correct.
        IDENT => identifier(p),
        _ => {
            p.err_and_bump("expected expression");
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

fn measure_expression(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![measure]);
    match p.current() {
        IDENT | HARDWAREIDENT => {
            items::ident_or_index_expr(p);
        }
        _ => {
            p.error("expecting qubit(s) to measure");
            // m.abandon(p);
            // return;
        }
    }
    m.complete(p, MEASURE_EXPRESSION)
}

// FIXME: changed the kind to `NAME`
// FIXME: changed it back to IDENTIFIER
pub(crate) fn identifier(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    m.complete(p, IDENTIFIER)
}

pub(crate) fn hardware_qubit(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(HARDWAREIDENT);
    m.complete(p, HARDWARE_QUBIT)
}

fn tuple_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T!['(']));
    let m = p.start();
    p.expect(T!['(']);

    let mut saw_comma = false;
    let mut saw_expr = false;

    if p.eat(T![,]) {
        p.error("expected expression");
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

// FIXME: finish this. need to disambiguate from block of statments
// fn set_expr(p: &mut Parser<'_>) -> CompletedMarker {
//     assert!(p.at(T!['{']));
//     if p.at(INT_NUMBER) {
//         p.bump(INT_NUMBER)
//     } else if p.at(T!['}']) {
//         p.bump(T!['}']);
//         m.complete(p, SET_EXPR);
//         return ();
//     }
//     // FIXME find ,
//     p.error("expecting an integer or '}'");
//     // todo eat until ';' maybe,  for recovery
//     m.abandon(p);
//     return ();
//     p.expect(T!['}']);
//     m.complete(p, SET_EXPR)
// }

// test if_expr
// fn foo() {
//     if true {};
//     if true {} else {};
//     if true {} else if false {} else {};
//     if S {};
//     if { true } { } else { };
// }
// moved to items.rs
// fn if_expr(p: &mut Parser<'_>) -> CompletedMarker {
//     assert!(p.at(T![if]));
//     let m = p.start();
//     p.bump(T![if]);
//     expr_no_struct(p);
//     block_expr(p);
//     if p.at(T![else]) {
//         p.bump(T![else]);
//         if p.at(T![if]) {
//             if_expr(p);
//         } else {
//             block_expr(p);
//         }
//     }
//     m.complete(p, IF_STMT)
// }

// test while_expr
// fn foo() {
//     while true {};
//     while let Some(x) = it.next() {};
//     while { true } {};
// }
// fn while_expr(p: &mut Parser<'_>, m: Option<Marker>) -> CompletedMarker {
//     assert!(p.at(T![while]));
//     let m = m.unwrap_or_else(|| p.start());
//     p.bump(T![while]);
//     expr_no_struct(p);
//     block_expr(p);
//     m.complete(p, WHILE_STMT)
// }

// test for_expr
// fn foo() {
//     for x in [] {};
// }
fn for_expr(p: &mut Parser<'_>, m: Option<Marker>) -> CompletedMarker {
    assert!(p.at(T![for]));
    let m = m.unwrap_or_else(|| p.start());
    p.bump(T![for]);
    p.expect(IDENT);
    p.expect(T![in]);
    expr_no_struct(p);
    block_expr(p);
    m.complete(p, FOR_STMT)
}

pub(crate) fn block_expr(p: &mut Parser<'_>) -> CompletedMarker {
    // FIXME: can't use this check in refactor.
    // get this working again.
    // if !p.at(T!['{']) {
    //     p.error("expected a block");
    //     return;
    // }
    let m = p.start();
    p.bump(T!['{']);
    // read until T!['}']
    expr_block_contents(p);
    p.expect(T!['}']);
    m.complete(p, BLOCK_EXPR)
}

// FIXME: This idiom for handling "measure" does not occur
// in the original r-a crates. Need to refactor to avoid
// starting a new Marker.
// test return_expr
// fn foo() {
//     return;
//     return 92;
// }
fn return_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(T![return]));
    let m = p.start();
    p.bump(T![return]);
    if p.at_ts(EXPR_FIRST) {
        expr(p);
    } else if p.at(T![measure]) {
        let m1 = p.start();
        expressions::items::measure_(p, m1);
    }
    m.complete(p, RETURN_EXPR)
}

// test box_expr
// fn foo() {
//     let x = box 1i32;
//     let y = (box 1i32, box 2i32);
//     let z = Foo(box 1i32, box 2i32);
// }
fn box_expr(p: &mut Parser<'_>, m: Option<Marker>) -> CompletedMarker {
    assert!(p.at(T![box]));
    let m = m.unwrap_or_else(|| p.start());
    p.bump(T![box]);
    if p.at_ts(EXPR_FIRST) {
        expr(p);
    }
    m.complete(p, BOX_EXPR)
}
