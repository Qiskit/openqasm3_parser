// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

pub mod atom;

use super::*;

pub(crate) use atom::block_expr;
pub(crate) use atom::try_block_expr;
pub(super) use atom::LITERAL_FIRST;

const EXPR_FIRST: TokenSet = LHS_FIRST;

pub(super) fn expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let r = Restrictions { prefer_stmt: false };
    expr_bp(p, None, r, 1).map(|(m, _)| m)
}

// This inlcudes square brackets
pub(crate) fn range_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let r = Restrictions { prefer_stmt: false };
    let m = p.start();
    assert!(p.at(T!['[']));
    p.bump(T!['[']);
    expr_bp(p, None, r, 1).map(|(m, _)| m);
    if p.at(COLON) {
        p.bump(COLON);
        expr_bp(p, None, r, 1).map(|(m, _)| m);
        if p.at(COLON) {
            p.bump(COLON);
            expr_bp(p, None, r, 1).map(|(m, _)| m);
        }
    } else {
        p.error("Expecting colon in range expression.");
    }
    p.expect(T![']']);
    Some(m.complete(p, RANGE_EXPR))
}

pub(super) fn expr_or_range_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let r = Restrictions { prefer_stmt: false };
    let m = p.start();
    let expr1 = expr_bp(p, None, r, 1).map(|(m, _)| m);
    if p.at(COLON) {
        p.bump(COLON);
        expr_bp(p, None, r, 1).map(|(m, _)| m);
        if p.at(COLON) {
            p.bump(COLON);
            expr_bp(p, None, r, 1).map(|(m, _)| m);
        }
        Some(m.complete(p, RANGE_EXPR))
    } else {
        m.abandon(p);
        expr1
    }
}

pub(super) fn expr_stmt(
    p: &mut Parser<'_>,
    m: Option<Marker>,
) -> Option<(CompletedMarker, BlockLike)> {
    // The restriction `prefer_stmt: true` tells `expr_bp` that we are looking
    // for an expression statement. Some expressions can only occur at the top
    // level of a statement.
    let r = Restrictions { prefer_stmt: true };
    expr_bp(p, m, r, 1)
}

// GJL made public. remove visibility
pub(crate) fn stmt(p: &mut Parser<'_>) {
    if p.eat(T![;]) {
        return;
    }
    if p.at(T![let]) {
        let m = p.start();
        let_stmt(p, m);
        return;
    }
    let m = p.start();
    let m = match items::opt_item(p, m) {
        Ok(()) => return,
        Err(m) => m,
    };
    if p.at(PRAGMA) {
        p.bump_any();
        m.complete(p, PRAGMA_STATEMENT);
        return;
    }
    if p.at(ANNOTATION) {
        p.bump_any();
        // Note this is a single annotation, not an annotated statement.
        m.complete(p, ANNOTATION_STATEMENT);
        return;
    }

    // FIXME: straighten out logic
    if !(p.current().is_classical_type() && (p.nth(1) == T!['('] || p.nth(1) == T!['[']))
        && !p.at_ts(EXPR_FIRST)
    {
        p.err_and_bump("stmt: expected expression, type declaration, or let statement");
        m.abandon(p);
        return;
    };
    if let Some((cm, blocklike)) = expr_stmt(p, Some(m)) {
        if cm.kind() == ASSIGNMENT_STMT {
            // The expression is about to be wrapped in `EXPR_STMT`. But
            // Assignment is really a statement by itself. So we exit before wrapping.
            // The trailing semicolon has already been parsed.
            return;
        }
        if !p.at(T!['}']) {
            // The last completed Marker for `p` was an `EXPR`. We want to wrap it
            // in `EXPR_STMT`, so we create new marker at the beginning of this `EXPR`.
            // Then we parse just a semicolon.
            let m = cm.precede(p);
            if blocklike.is_block() {
                p.eat(T![;]);
            } else if !p.eat(T![;]) {
                p.error("Expecting semicolon terminating statement");
            }
            m.complete(p, EXPR_STMT);
        }
    }

    fn let_stmt(p: &mut Parser<'_>, m: Marker) {
        p.bump(T![let]);
        p.expect(IDENT);
        p.expect(T![=]);
        expressions::expr(p);
        p.expect(T![;]);
        m.complete(p, LET_STMT);
    }
}

// Careful, this reads til } *or* EOF. And this may be called without having read a
// {. In a way, this is an implicit block expression. Or else it is inadvertent.
// YES: inadvertent when adpating code for OQ3.
// This function exists in r-a, but it is not called without having read a `{`
pub(super) fn expr_block_contents(p: &mut Parser<'_>) {
    while !p.at(EOF) && !p.at(T!['}']) {
        stmt(p);
    }
}

#[derive(Clone, Copy)]
struct Restrictions {
    // Set to true in `expr_stmt` to mark top-level expression.
    prefer_stmt: bool,
}

#[derive(Debug)]
enum Associativity {
    Left,
    Right,
}

/// Binding powers of operators for a Pratt parser.
///
/// See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
///
/// Note that Rust doesn't define associativity for some infix operators (e.g. `==` and `..`) and
/// requires parentheses to disambiguate. We just treat them as left associative.
///
/// Returns (binding power : u8, operator : SyntaxKind, associativity : Associativity)
/// Look at canonical example: `+` has bp 10 and `*` has bp 11.
#[rustfmt::skip]
fn current_op(p: &Parser<'_>) -> (u8, SyntaxKind, Associativity) {
    use Associativity::*;
    // It seems that return value is never checked for `NOT_AN_OP`
    // r-a had @ for not an op. But we use triple dot
    const NOT_AN_OP: (u8, SyntaxKind, Associativity) = (0, T![...], Left);
    match p.current() {
        T![|] if p.at(T![||])  => (3,  T![||],  Left),
        T![|] if p.at(T![|=])  => (1,  T![|=],  Right),
        T![|]                  => (6,  T![|],   Left),
        T![>] if p.at(T![>>=]) => (1,  T![>>=], Right),
        T![>] if p.at(T![>>])  => (9,  T![>>],  Left),
        T![>] if p.at(T![>=])  => (5,  T![>=],  Left),
        T![>]                  => (5,  T![>],   Left),
        T![=] if p.at(T![=>])  => NOT_AN_OP,
        T![=] if p.at(T![==])  => (5,  T![==],  Left),
        // r-a had 1 as the bp here. But this attempts to parse
        // `x + y = 3`; as `(x + y) = 3;` which is probably not what the user meant.
        // Putting 12 as the bp instead of 1 parses this as
        // `x + (y = 3)`. In OQ3, this is still illegal, but the user will get a more
        // informative error message. That an assignment statement is not allowed here.
        // This may have unintended consequences and we will need to replace the 12 with 1.
        T![=]                  => (12,  T![=],   Right),
        T![<] if p.at(T![<=])  => (5,  T![<=],  Left),
        T![<] if p.at(T![<<=]) => (1,  T![<<=], Right),
        T![<] if p.at(T![<<])  => (9,  T![<<],  Left),
        T![<]                  => (5,  T![<],   Left),
        T![+] if p.at(T![+=])  => (1,  T![+=],  Right),
        // `++` is the concatenation op and should have some low value for bp.
        T![+] if p.at(T![++])  => (2,  T![++],  Left),
        T![+]                  => (10, T![+],   Left),
        T![^] if p.at(T![^=])  => (1,  T![^=],  Right),
        T![^]                  => (7,  T![^],   Left),
        T![%] if p.at(T![%=])  => (1,  T![%=],  Right),
        T![%]                  => (11, T![%],   Left),
        T![&] if p.at(T![&=])  => (1,  T![&=],  Right),
        T![&] if p.at(T![&&])  => (4,  T![&&],  Left),
        T![&]                  => (8,  T![&],   Left),
        T![/] if p.at(T![/=])  => (1,  T![/=],  Right),
        T![/]                  => (11, T![/],   Left),
        T![*] if p.at(T![*=])  => (1,  T![*=],  Right),
        T![*]                  => (11, T![*],   Left),
        T![.] if p.at(T![..=]) => (2,  T![..=], Left),
        T![.] if p.at(T![..])  => (2,  T![..],  Left),
        T![!] if p.at(T![!=])  => (5,  T![!=],  Left),
        T![-] if p.at(T![-=])  => (1,  T![-=],  Right),
        T![-]                  => (10, T![-],   Left),
        _                      => NOT_AN_OP
    }
}

// Parses expression with binding power of at least bp.
fn expr_bp(
    p: &mut Parser<'_>,
    m: Option<Marker>,
    r: Restrictions,
    bp: u8,
) -> Option<(CompletedMarker, BlockLike)> {
    let m = m.unwrap_or_else(|| p.start());
    #[allow(clippy::nonminimal_bool)]
    if !p.at_ts(EXPR_FIRST) && !(p.current().is_classical_type() && p.nth(1) == T!['(']) {
        p.err_recover("expr_bp: expected expression", atom::EXPR_RECOVERY_SET); // FIXME, remove debug from string
        m.abandon(p);
        return None;
    }
    let lhs_result = lhs(p, r);
    let mut lhs = match lhs_result {
        Some((lhs, blocklike)) => {
            let lhs = lhs.extend_to(p, m);
            if r.prefer_stmt && blocklike.is_block() {
                return Some((lhs, BlockLike::Block));
            }
            lhs
        }
        None => {
            m.abandon(p);
            return None;
        }
    };

    loop {
        let (op_bp, op, associativity) = current_op(p);
        if op_bp < bp {
            break;
        }
        let lhs_kind = lhs.kind();
        let m = lhs.precede(p);
        p.bump(op);

        let op_bp = match associativity {
            Associativity::Left => op_bp + 1,
            Associativity::Right => op_bp,
        };
        // If we enter `expr_bp` from `expr_stmt` we want `prefer_stmt = true`,
        // because we are looking for an expression statement. But when we call
        // it recursively in the following line, we are no longer at the top-level expression
        // in the statement, so we set it to `false`.
        expr_bp(p, None, Restrictions { prefer_stmt: false }, op_bp);
        // If the op is `=` then it looks like an assignment.
        if matches!(op, T![=]) {
            if !r.prefer_stmt {
                // We are not a statement-level expression, rather a subexpression.
                p.error("Assignment statement found where expression expected");
            }
            if matches!(lhs_kind, IDENTIFIER | INDEXED_IDENTIFIER) {
                if r.prefer_stmt {
                    // This only happens if the assignment is in an illegal place.  We
                    // choose not to log an additional syntax error asking for a semicolon
                    p.expect(SEMICOLON);
                }
                lhs = m.complete(p, ASSIGNMENT_STMT);
            } else {
                // If LHS is not an identifier or indexed identifier, parse it as a binary
                // expression, but log error.
                p.error("Illegal LHS in assignment");
                lhs = m.complete(p, BIN_EXPR);
            }
        } else {
            lhs = m.complete(p, BIN_EXPR);
        }
    }
    Some((lhs, BlockLike::NotBlock))
}

const LHS_FIRST: TokenSet =
    atom::ATOM_EXPR_FIRST.union(TokenSet::new(&[T![&], T![*], T![!], T![.], T![-], T![_]]));

// Handles only prefix and postfix expressions?? Not binary infix?
fn lhs(p: &mut Parser<'_>, r: Restrictions) -> Option<(CompletedMarker, BlockLike)> {
    let m;
    // Unary operators.
    let kind = match p.current() {
        T![~] | T![!] | T![-] => {
            m = p.start();
            p.bump_any();
            PREFIX_EXPR
        }
        _ => {
            let (lhs, blocklike) = atom::atom_expr(p, r)?;
            let (cm, block_like) =
                postfix_expr(p, lhs, blocklike, !(r.prefer_stmt && blocklike.is_block()));
            return Some((cm, block_like));
        }
    };
    // parse the interior of the unary expression
    expr_bp(p, None, r, 255);
    let cm = m.complete(p, kind);
    Some((cm, BlockLike::NotBlock))
}

fn postfix_expr(
    p: &mut Parser<'_>,
    mut lhs: CompletedMarker,
    // r-a comment, probably irrelevant to OQ3
    // Calls are disallowed if the type is a block and we prefer statements because the call cannot be disambiguated from a tuple
    // E.g. `while true {break}();` is parsed as
    // `while true {break}; ();`
    mut block_like: BlockLike,
    mut allow_calls: bool,
) -> (CompletedMarker, BlockLike) {
    loop {
        lhs = match p.current() {
            T!['('] if allow_calls => call_expr(p, lhs),
            T!['['] if allow_calls => match lhs.kind() {
                IDENTIFIER => indexed_identifier(p, lhs),
                // The previous token was not `IDENTIFIER`, so we are indexing an expression.
                _ => index_expr(p, lhs),
            },
            _ => break,
        };
        allow_calls = true;
        block_like = BlockLike::NotBlock;
    }
    (lhs, block_like)
}

// Consumes either a function (def) call, or a gate call.
fn call_expr(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    assert!(p.at(T!['(']));
    let m = lhs.precede(p);
    call_arg_list(p);
    // If after consuming `(x,y,..)` we find an identifier, it must be
    // a gate call statement. `expr_stmt` has already begun but will be abandoned.
    // If there is no identifier, it is a function call.
    if matches!(p.current(), IDENT | HARDWAREIDENT) {
        params::arg_list_gate_call_qubits(p);
        return m.complete(p, GATE_CALL_EXPR);
    }
    m.complete(p, CALL_EXPR)
}

// FIXME. Do we need to worry about precedence here, as in call_expr ?
pub(crate) fn cast_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    type_spec(p);
    p.expect(T!['(']);
    expr(p);
    p.expect(T![')']);
    m.complete(p, CAST_EXPRESSION)
}

fn type_name(p: &mut Parser<'_>) {
    if !p.current().is_type_name() {
        p.error("Expected name of type");
    }
    p.bump(p.current());
}

pub(crate) fn type_spec(p: &mut Parser<'_>) -> bool {
    let m = p.start();
    type_name(p);
    if p.at(T!['[']) {
        designator(p);
    }
    m.complete(p, SCALAR_TYPE);
    true
}

pub(crate) fn quantum_type_spec(p: &mut Parser<'_>) -> bool {
    let m = p.start();
    type_name(p);
    if p.at(T!['[']) {
        designator(p);
    }
    m.complete(p, QUBIT_TYPE);
    true
}

fn designator(p: &mut Parser<'_>) -> bool {
    let m = p.start();
    p.eat(T!['[']);
    expr(p);
    p.expect(T![']']);
    m.complete(p, DESIGNATOR);
    true
}

pub(crate) fn var_name(p: &mut Parser<'_>) {
    let m = p.start();
    p.expect(IDENT); // The declared identifier, ie variable name
    m.complete(p, NAME);
}

pub(crate) fn _returns_bool_classical_declaration_stmt(p: &mut Parser<'_>, m: Marker) -> bool {
    p.eat(T![const]); // FIXME. Fix this in ungram and then move this to type_spec
    let mstmt = p.start();
    type_spec(p);
    if p.current() == T!['('] {
        p.expect(T!['(']);
        expr(p);
        p.expect(T![')']);
        m.complete(p, CAST_EXPRESSION);
        if p.at(SEMICOLON) {
            p.expect(SEMICOLON);
            mstmt.complete(p, EXPR_STMT);
        } else {
            mstmt.abandon(p);
        }
        return true;
    } else {
        mstmt.abandon(p);
    }
    var_name(p);
    if p.eat(T![;]) {
        m.complete(p, CLASSICAL_DECLARATION_STATEMENT);
        return true;
    }
    if !p.expect(T![=]) {
        m.abandon(p);
        return false;
    }
    expr(p);
    p.expect(T![;]); // why did I suddenly have to include this?
    m.complete(p, CLASSICAL_DECLARATION_STATEMENT);
    true
}

pub(crate) fn classical_declaration_stmt(p: &mut Parser<'_>, m: Marker) {
    _returns_bool_classical_declaration_stmt(p, m);
}

// This includes a previously parsed expression as the first argument of `INDEX_EXPR`.
// ig `arg1[arg2]` is the expression and only `arg2` is parsed here.
// test index_expr
// fn foo() {
//     x[1][2];
// }
pub(crate) fn index_expr(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    assert!(p.at(T!['[']));
    let m = lhs.precede(p);
    index_operator(p);
    m.complete(p, INDEX_EXPR)
}

pub(crate) fn indexed_identifier(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    assert!(p.at(T!['[']));
    let m = lhs.precede(p);
    while p.at(T!['[']) && !p.at(EOF) {
        index_operator(p);
    }
    m.complete(p, INDEXED_IDENTIFIER)
}

pub(crate) fn set_expression(p: &mut Parser<'_>) {
    assert!(p.at(T!['{']));
    let m = p.start();
    p.bump(T!['{']);
    params::expression_list(p);
    p.bump(T!['}']);
    m.complete(p, SET_EXPRESSION);
}

pub(crate) fn index_operator(p: &mut Parser<'_>) {
    assert!(p.at(T!['[']));
    let m = p.start();
    p.expect(T!['[']);
    if p.at(T!['{']) {
        set_expression(p);
    } else {
        params::expression_list(p);
    }
    p.expect(T![']']);
    m.complete(p, INDEX_OPERATOR);
}

// For function call and gate call
// Cannot enforce no empty parens in gate call here.
pub(crate) fn call_arg_list(p: &mut Parser<'_>) {
    let bra = T!['('];
    let ket = T![')'];
    assert!(p.at(bra));
    let m = p.start();
    let consume_braket = false;
    let m1 = p.start();
    p.bump(bra);
    delimited(
        p,
        bra,
        ket,
        consume_braket,
        T![,],
        EXPR_FIRST,
        |p: &mut Parser<'_>| expr(p).is_some(),
    );
    p.expect(ket);
    m1.complete(p, EXPRESSION_LIST);
    m.complete(p, ARG_LIST);
}
