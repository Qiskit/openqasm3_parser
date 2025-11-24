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

// Here and elsewhere "Gate" means gate def, and "GateCall" means gate call.
#[derive(Debug, Clone, Copy)]
enum DefFlavor {
    // Gate definition parameter list: (p0, p1, ...)
    // - parens: yes
    // - typed: no
    // - terminator: ')'
    GateParams,

    // Gate definition qubit list: q0, q1, ... {
    // - parens: no
    // - typed: no
    // - terminator: '{'
    GateQubits,

    // Gate call qubit list: q0, q1, ...;
    // - parens: no
    // - typed: no
    // - terminator: ';'
    GateCallQubits,

    // Function definition parameter list: (t0 p0, t1 p1, ...)
    // - parens: yes
    // - typed: yes
    // - terminator: ')'
    DefParams,

    // DefCal parameter list: (p0, t1 p1, ...)
    // - parens: yes
    // - typed: optional
    // - terminator: ')'
    DefCalParams,

    // DefCal qubit list: q0, q1, ... {   or   q0, q1, ... -> ...
    // - parens: no
    // - typed: no
    // - terminators: '{' or '->'
    DefCalQubits,

    // General expression list (e.g., in index operators): [e0, e1, ...]
    // - parens: no
    // - typed: no
    // - terminator: ']'
    ExpressionList,

    // Switch case control values: case e0, e1, ... {
    // - parens: no
    // - typed: no
    // - terminator: '{'
    CaseValues,
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

    // Only GateParams, DefParams, and DefCalParams open with '(' ... ')'.
    let want_parens = matches!(flavor, GateParams | DefParams | DefCalParams);
    match flavor {
        GateParams | DefParams | DefCalParams => p.bump(T!['(']),
        GateQubits | GateCallQubits | DefCalQubits | ExpressionList | CaseValues => (),
    }

    // End tokens for each flavor.
    let list_end_tokens = match flavor {
        ExpressionList => [T![']'], T![']']],
        CaseValues => [T!['{'], T!['{']],
        GateParams | DefParams | DefCalParams => [T![')'], T![')']],
        // When no parens are present '{' terminates the gate param list.
        GateQubits => [T!['{'], T!['{']],
        GateCallQubits => [SEMICOLON, SEMICOLON],
        DefCalQubits => [T!['{'], T![->]],
    };

    let mut num_params: usize = 0;

    // Parse items until EOF or an end token is seen.
    while !p.at(EOF) && !list_end_tokens.iter().any(|x| p.at(*x)) {
        let m = p.start();

        // Allowed starts for an item: either a type or a first-token of a param/expression.
        if !(p.current().is_type() || p.at_ts(PARAM_FIRST)) {
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
            // Untyped parameters/qubits.
            GateParams | GateQubits | DefCalQubits => param_untyped(p, m),
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

    match flavor {
        GateParams | ExpressionList | CaseValues if num_params < 1 => {
            p.error("expected one or more parameters");
        }
        GateParams | ExpressionList | CaseValues => {}
        GateQubits | GateCallQubits | DefParams | DefCalParams | DefCalQubits => {}
    };

    // Close parens for the paren-using flavors.
    if want_parens {
        p.expect(T![')']);
    }

    // Complete with the correct node kind for this flavor.
    let kind = match flavor {
        GateQubits => PARAM_LIST,
        DefCalQubits => QUBIT_LIST,
        GateCallQubits => QUBIT_LIST,
        ExpressionList | CaseValues => EXPRESSION_LIST,
        DefParams | DefCalParams => TYPED_PARAM_LIST,
        GateParams => PARAM_LIST,
    };
    list_marker.complete(p, kind);
}

// // Parse a list of parameters.
// fn _param_list_openqasm(p: &mut Parser<'_>, flavor: DefFlavor) {
//     use DefFlavor::*;
//     let list_marker = p.start();
//     let want_parens = matches!(flavor, GateParams | DefParams | DefCalParams);
//     match flavor {
//         GateParams | DefParams | DefCalParams => p.bump(T!['(']),
//         GateQubits | GateCallQubits | DefCalQubits | ExpressionList | CaseValues => (),
//     }
//     // FIXME: find implementation that does not require [T![')'], T![')']]
//     // I tried using TokenSet, which should give exactly the same result.
//     // But it does not. May be because -> is a compound token.
//     let list_end_tokens = match flavor {
//         // GateParams | DefParams | DefCalParams => {TokenSet::new(&[T![')']])},
//         // GateQubits => {TokenSet::new(&[T!['{']])},
//         // DefCalQubits => {TokenSet::new(&[T!['{'], T![->]])},
//         ExpressionList => [T![']'], T![']']],
//         CaseValues => [T!['{'], T!['{']],
//         GateParams | DefParams | DefCalParams => [T![')'], T![')']],
//         // When no parens are present `{` terminates the list of parameters.
//         GateQubits => [T!['{'], T!['{']],
//         GateCallQubits => [SEMICOLON, SEMICOLON],
//         DefCalQubits => [T!['{'], T![->]],
//     };
//     let mut num_params: usize = 0;
//     // Would be nice if we could used the following line instead of hacked roll your own two lines down.
//     //  while !p.at(EOF) && !p.at_ts(list_end_tokens) {
//     while !p.at(EOF) && !list_end_tokens.iter().any(|x| p.at(*x)) {
//         let m = p.start();
//         if !(p.current().is_type() || p.at_ts(PARAM_FIRST)) {
//             p.error("expected value parameter");
//             m.abandon(p);
//             break;
//         }
//         let found_param = match flavor {
//             ExpressionList | CaseValues => {
//                 m.abandon(p);
//                 expressions::expr_or_range_expr(p);
//                 true
//             }
//             GateCallQubits => arg_gate_call_qubit(p, m),
//             // FIXME: this can't work. These two variants have different reqs on params
//             DefParams | DefCalParams => param_typed(p, m),
//             // The following is pretty ugly. Probably inefficient as well
//             GateParams | GateQubits | DefCalQubits => param_untyped(p, m),
//         };
//         if !found_param {
//             break;
//         }
//         num_params += 1;
//         // FIXME: This is only needed to support `->` as terminating tokens.
//         // Not for `{`. But I don't know why. prbly because `->` is compound.
//         // FIXME: use at_ts()
//         if list_end_tokens.iter().any(|x| p.at(*x)) {
//             //        if p.at_ts(list_end_tokens) {
//             break;
//         }
//         // Params must be separated by commas.
//         if !p.at(T![,]) {
//             if p.at_ts(PARAM_FIRST) {
//                 p.error("Expected `,`");
//             } else {
//                 break;
//             }
//         } else {
//             // We found the expected comma, so consume it.
//             p.bump(T![,]);
//         }
//     }
//     match flavor {
//         GateParams | ExpressionList | CaseValues if num_params < 1 => {
//             p.error("expected one or more parameters");
//         }
//         GateParams | ExpressionList | CaseValues => {}
//         GateQubits | GateCallQubits | DefParams | DefCalParams | DefCalQubits => {}
//     };
//     // if let Some(m) = param_marker {
//     //     m.abandon(p);
//     // }
//     // Error if we don't find closing paren.
//     if want_parens {
//         p.expect(T![')']);
//     }
//     let kind = match flavor {
//         GateQubits => PARAM_LIST,
//         DefCalQubits => QUBIT_LIST,
//         GateCallQubits => QUBIT_LIST,
//         ExpressionList | CaseValues => EXPRESSION_LIST,
//         DefParams | DefCalParams => TYPED_PARAM_LIST,
//         GateParams => PARAM_LIST,
//     };
//     list_marker.complete(p, kind);
// }

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

fn param_typed(p: &mut Parser<'_>, m: Marker) -> bool {
    expressions::type_spec(p);
    expressions::var_name(p);
    m.complete(p, TYPED_PARAM);
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
