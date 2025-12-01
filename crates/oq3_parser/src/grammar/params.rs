// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use super::*;

pub(super) fn param_list_gate_params(p: &mut Parser<'_>) {
    // Gate definition parameter list: (p0, p1, ...)
    // - parens: yes
    // - typed: no
    // - terminator: ')'
    _param_list_openqasm(p, DefFlavor::GateParams);
}

pub(super) fn param_list_gate_qubits(p: &mut Parser<'_>) {
    // Gate definition qubit list: q0, q1, ... {
    // - parens: no
    // - typed: no
    // - terminator: '{'
    _param_list_openqasm(p, DefFlavor::GateQubits);
}

pub(super) fn arg_list_gate_call_qubits(p: &mut Parser<'_>) {
    // Gate call qubit list: q0, q1, ...;
    // - parens: no
    // - typed: no
    // - terminator: ';'
    _param_list_openqasm(p, DefFlavor::GateCallQubits);
}

pub(super) fn param_list_def_params(p: &mut Parser<'_>) {
    // Function definition parameter list: (t0 p0, t1 p1, ...)
    // - parens: yes
    // - typed: yes
    // - terminator: ')'
    _param_list_openqasm(p, DefFlavor::DefParams);
}

pub(super) fn scalar_type_list(p: &mut Parser<'_>) {
    // List of scalar types: (t0, t1, ...)
    // - parens: yes
    // - types only
    // - terminator: ')'
    _param_list_openqasm(p, DefFlavor::TypeListFlavor);
}

pub(super) fn param_list_defcal_params(p: &mut Parser<'_>) {
    // DefCal parameter list: (p0, t1 p1, ...)
    // - parens: yes
    // - typed: optional
    // - terminator: ')'
    _param_list_openqasm(p, DefFlavor::DefCalParams);
}

pub(super) fn param_list_defcal_qubits(p: &mut Parser<'_>) {
    // DefCal qubit list: q0, q1, ... {   or   q0, q1, ... -> ...
    // - parens: no
    // - typed: no
    // - terminators: '{' or '->'
    _param_list_openqasm(p, DefFlavor::DefCalQubits);
}

pub(super) fn expression_list(p: &mut Parser<'_>) {
    // General expression list used in bracket contexts: [e0, e1, ...]
    // - parens: no
    // - typed: no
    // - canonical terminator: ']'
    // Note: some callers may wrap this between other delimiters and consume them outside.
    _param_list_openqasm(p, DefFlavor::ExpressionList);
}

pub(super) fn case_value_list(p: &mut Parser<'_>) {
    // Switch case control values: case e0, e1, ... {
    // - parens: no
    // - typed: no
    // - terminator: '{'
    _param_list_openqasm(p, DefFlavor::CaseValues);
}

pub(super) fn array_literal(p: &mut Parser<'_>) {
    // - curlies: yes
    // - typed: no
    // - terminator: '}'
    _param_list_openqasm(p, DefFlavor::ArrayLiteral);
}

// Here and elsewhere "Gate" means gate def, and "GateCall" means gate call.
#[derive(Debug, Clone, Copy)]
enum DefFlavor {
    GateParams,
    GateQubits,
    GateCallQubits,
    DefParams,
    DefCalParams,
    DefCalQubits,
    ExpressionList,
    ArrayLiteral,
    CaseValues,
    TypeListFlavor,
}

// Parse a list of comma-separated items according to the chosen flavor.
// Notes:
// - Trailing commas are allowed: after consuming a comma, if the next token
//   is an end token, the while condition stops the loop before parsing a new item.
// - For '->' handling: if '->' is a compound token, at_ts()/TokenSet may not
//   match; hence the simple array + any() checks here.
fn _param_list_openqasm(p: &mut Parser<'_>, flavor: DefFlavor) {
    use DefFlavor::*;
    let list_marker = p.start();

    // End tokens for each flavor.
    let list_end_tokens = match flavor {
        ExpressionList => [T![']'], T![']']],
        CaseValues => [T!['{'], T!['{']],
        GateParams | DefParams | DefCalParams | TypeListFlavor => [T![')'], T![')']],
        // When no parens are present '{' terminates the gate param list.
        GateQubits => [T!['{'], T!['{']],
        GateCallQubits => [SEMICOLON, SEMICOLON],
        DefCalQubits => [T!['{'], T![->]],
        ArrayLiteral => [T!['}'], T!['}']],
    };

    let mut num_params: usize = 0;

    let need_parens = matches!(
        flavor,
        GateParams | DefParams | DefCalParams | TypeListFlavor
    );
    let need_curlies = matches!(flavor, ArrayLiteral);

    if need_parens {
        p.expect(T!['(']);
    } else if need_curlies {
        p.expect(T!['{']);
    }

    // Parse items until EOF or an end token is seen.
    while !p.at(EOF) && !list_end_tokens.iter().any(|x| p.at(*x)) {
        let m = p.start();

        let inner_array_literal = p.at(T!['{']);
        // Allowed starts for an item: either a type or a first-token of a param/expression,
        // or first token of array literal.
        if !(p.current().is_type() || p.at_ts(PARAM_FIRST) || inner_array_literal) {
            p.error("expected value parameter");
            m.abandon(p);
            break;
        }

        // Dispatch to the appropriate item parser.
        let found_param = match flavor {
            ExpressionList | CaseValues => {
                m.abandon(p);
                expressions::expr_or_range_expr(p);
                true
            }
            GateCallQubits => arg_gate_call_qubit(p, m),
            // These two have different requirements but share this entry point.
            DefParams | DefCalParams => param_typed(p, m),
            TypeListFlavor => scalar_type(p, m),
            // Untyped parameters/qubits.
            GateParams | GateQubits => param_untyped(p, m),
            DefCalQubits => param_untyped_or_hardware_qubit(p, m),
            ArrayLiteral => {
                if inner_array_literal {
                    m.abandon(p);
                    array_literal(p);
                    true
                } else {
                    m.abandon(p);
                    expressions::expr(p);
                    true
                }
            }
        };
        if !found_param {
            break;
        }
        num_params += 1;

        // If the very next token is an end token, stop
        if list_end_tokens.iter().any(|x| p.at(*x)) {
            break;
        }

        // Items must be separated by commas.
        if !p.at(T![,]) {
            if p.at_ts(PARAM_FIRST) {
                p.error("Expected `,`");
            } else {
                break;
            }
        } else {
            // Found the expected comma; consume and continue.
            p.bump(T![,]);
        }
    }

    if num_params < 1 && matches!(flavor, GateParams | ExpressionList | CaseValues) {
        p.error("expected one or more parameters");
    }

    // Some flavors expect closing paren.
    if need_parens {
        p.expect(T![')']);
    } else if matches!(flavor, ArrayLiteral) {
        p.expect(T!['}']);
    }

    // Complete with the correct node kind for this flavor.
    let kind = match flavor {
        GateQubits => PARAM_LIST,
        DefCalQubits => QUBIT_LIST,
        GateCallQubits => QUBIT_LIST,
        ExpressionList | CaseValues => EXPRESSION_LIST,
        DefParams | DefCalParams => TYPED_PARAM_LIST,
        GateParams => PARAM_LIST,
        TypeListFlavor => TYPE_LIST,
        ArrayLiteral => ARRAY_LITERAL,
    };
    list_marker.complete(p, kind);
}

const PATTERN_FIRST: TokenSet = expressions::LITERAL_FIRST
    .union(expressions::atom::PATH_FIRST)
    .union(TokenSet::new(&[
        T![box],
        T![const],
        T!['('],
        T!['['],
        T![&],
        T![_],
        T![-],
        T![.],
    ]));

const TYPE_FIRST: TokenSet = expressions::atom::PATH_FIRST.union(TokenSet::new(&[
    T!['('],
    T!['['],
    T![<],
    T![!],
    T![*],
    T![&],
    T![_],
    T![extern],
]));

const PARAM_FIRST: TokenSet = PATTERN_FIRST.union(TYPE_FIRST);

// TODO: Look again at the r-a code to copy the idioms there.
// We have removed all of the code that can serve as an example.
// In OQ 3, parameters in gate defs don't have type annotations.
fn param_untyped(p: &mut Parser<'_>, m: Marker) -> bool {
    if !p.at(IDENT) {
        p.error("Expected parameter name");
        m.abandon(p);
        return false;
    }
    p.bump(IDENT);
    m.complete(p, PARAM);
    true
}

fn param_untyped_or_hardware_qubit(p: &mut Parser<'_>, m: Marker) -> bool {
    if p.at(IDENT) {
        p.bump(IDENT);
        m.complete(p, PARAM);
        true
    } else if p.at(HARDWAREIDENT) {
        m.abandon(p);
        expressions::atom::hardware_qubit(p);
        true
    } else {
        p.error("Expected parameter name");
        m.abandon(p);
        false
    }
}

fn param_typed(p: &mut Parser<'_>, m: Marker) -> bool {
    expressions::type_spec(p);
    expressions::var_name(p);
    m.complete(p, TYPED_PARAM);
    true
}

fn scalar_type(p: &mut Parser<'_>, m: Marker) -> bool {
    expressions::type_spec(p);
    m.complete(p, SCALAR_TYPE);
    true
}

// These can be cast to GateOperand
pub(crate) fn arg_gate_call_qubit(p: &mut Parser<'_>, m: Marker) -> bool {
    if p.at(HARDWAREIDENT) {
        p.bump(HARDWAREIDENT);
        m.complete(p, HARDWARE_QUBIT);
        return true;
    }

    if !p.at(IDENT) {
        p.error("Expected name in qubit argument");
        m.abandon(p);
        return false;
    }
    //    let mcomp = expressions::atom::identifier(p);
    p.bump(IDENT);
    let mcomp = m.complete(p, IDENTIFIER);
    if p.at(T!['[']) {
        //        expressions::index_expr(p, mcomp);
        expressions::indexed_identifier(p, mcomp);
        return true;
    }
    true
}
