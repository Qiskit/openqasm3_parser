use super::*;

// use self::SyntaxKind::{INT_KW, BIT_KW, QUBIT_KW};
impl SyntaxKind {
    // This is now is_classical_type, written in codegen.
    // pub fn is_scalar_type_name(self) -> bool {
    //     matches!(self,
    //              BIT_KW
    //              | INT_KW
    //              | UINT_KW
    //              | FLOAT_KW
    //              | ANGLE_KW
    //              | BOOL_KW
    //              | DURATION_KW
    //              | STRETCH_KW
    //              | COMPLEX_KW
    //     )
    // }
    pub fn is_classical_type(self) -> bool {
        self.is_scalar_type() || matches!(self, ARRAY_KW)
    }
    pub fn is_type_name(self) -> bool {
        self.is_classical_type() || matches!(self, QUBIT_KW)
    }
}

pub(super) fn param_list_gate_params(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::GateParams, None);
}
pub(super) fn param_list_gate_qubits(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::GateQubits, None);
}

pub(super) fn arg_list_gate_call_qubits(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::GateCallQubits, None);
}

pub(super) fn param_list_def_params(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::DefParams, None);
}
pub(super) fn param_list_defcal_params(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::DefCalParams, None);
}
pub(super) fn param_list_defcal_qubits(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::DefCalQubits, None);
}

// pub(super) fn set_expression(p: &mut Parser<'_>) {
//     _param_list_openqasm(p, DefFlavor::SetExpression, None);
// }

pub(super) fn expression_list(p: &mut Parser<'_>) {
    _param_list_openqasm(p, DefFlavor::ExpressionList, None);
}

// Here and elsewhere "Gate" means gate def, and "GateCall" means gate call.
#[derive(Debug, Clone, Copy)]
enum DefFlavor {
    // Same syntax for: gate def params, function call params, gate call params.
    // But for various reasons, we separate this into GateParams and CallOrGateCallParams
    // One reason: we can disallow here empty parens in gate def.
    GateParams,   // parens,    no type
    // For the moment, following is handled in expressions::arg_list instead.
    GateQubits,   // no parens, no type, '{' terminates
    GateCallQubits,   // no parens, no type, ';' terminates
    DefParams,    // parens,    type
    DefCalParams, // parens,    opt type
    DefCalQubits, // no parens, no type, '{' or '->' terminates
//    SetExpression,
    ExpressionList,
}

// Parse a list of parameters.
// FIXME: m: Option<Marker> is unused, always called with None. Can remove.
fn _param_list_openqasm(p: &mut Parser<'_>, flavor: DefFlavor, m: Option<Marker>) {
    use DefFlavor::*;
    let list_marker = p.start();
    let want_parens = matches!(flavor, GateParams | DefParams | DefCalParams);
    match flavor {
        GateParams | DefParams | DefCalParams => p.bump(T!['(']),
//        SetExpression => p.bump(T!['{']),
        _ => (),
    }
    // FIXME: find implementation that does not require [T![')'], T![')']]
    // I tried using TokenSet, which should give exactly the same result.
    // But it does not. May be because -> is a compound token.
    let list_end_tokens = match flavor {
        // GateParams | DefParams | DefCalParams => {TokenSet::new(&[T![')']])},
        // GateQubits => {TokenSet::new(&[T!['{']])},
        // DefCalQubits => {TokenSet::new(&[T!['{'], T![->]])},
        ExpressionList => [T![']'], T![']']],
        GateParams | DefParams | DefCalParams => [T![')'], T![')']],
        GateQubits => [T!['{'], T!['{']], // When no parens are present `{` terminates the list of parameters.
        GateCallQubits => [SEMICOLON, SEMICOLON], // ugh.
        DefCalQubits => [T!['{'], T![->]],
//        SetExpression => [T!['}'], T!['}']],
    };
    let mut param_marker = m;
    // let mut param_marker = None;
    let mut num_params: usize = 0;
    // Would be nice if we could used the following line instead of hacked roll your own two lines down.
    //  while !p.at(EOF) && !p.at_ts(list_end_tokens) {
    while !p.at(EOF) && !list_end_tokens.iter().any(|x| p.at(*x)) {
        let m = param_marker.take().unwrap_or_else(|| p.start());
        if !(p.current().is_type_name() || p.at_ts(PARAM_FIRST)) { //
            p.error("expected value parameter");
            println!("!!!! Got {:?}", p.current());
            m.abandon(p);
            break;
        }
        let found_param = match flavor {
            ExpressionList => {
                m.abandon(p);
                expressions::expr_or_range_expr(p); true
            },
//            SetExpression => { m.abandon(p); expressions::expr(p); true }
            GateCallQubits => {arg_gate_call_qubit(p, m)}
            DefParams | DefCalParams => {param_typed(p, m)}
            _ => {param_untyped(p, m)}
        };
        if ! found_param {
            break;
        }
        num_params += 1;
        // FIXME: This is only needed to support `->` as terminating tokens.
        // Not for `{`. But I don't know why. prbly because `->` is compound.
        if list_end_tokens.iter().any(|x| p.at(*x)) { // FIXME: use at_ts()
//        if p.at_ts(list_end_tokens) {
            break;
        }
        if !p.at(T![,]) { // Params must be separated by commas.
            if p.at_ts(PARAM_FIRST) {
                p.error("Expected `,`");
            } else {
                break;
            }
        } else { // We found the expected comma, so consume it.
            p.bump(T![,]);
        }
    }
    match flavor {
        GateParams | ExpressionList if num_params < 1 => {
            p.error("expected one or more parameters");
        }
        _ => {}
    };
    if let Some(m) = param_marker {
        m.abandon(p);
    }
    // FIXME: rewrite followig as match statement.
    if want_parens { // Error if we don't find closing paren.
        p.expect(T![')']);
    }
    // if matches!(flavor, SetExpression) {
    //     p.expect(T!['}']);
    // }
    let kind = match flavor {
//        SetExpression => SET_EXPRESSION,
        GateQubits => PARAM_LIST,
        DefCalQubits => QUBIT_LIST,
        GateCallQubits => QUBIT_LIST,
        ExpressionList => EXPRESSION_LIST,
        _ => PARAM_LIST,
    };
    list_marker.complete(p, kind);
    // let is_qubits = matches!(flavor, GateQubits | DefCalQubits);
    // list_marker.complete(p, if is_qubits {QUBIT_LIST} else {PARAM_LIST});
}

const PATTERN_FIRST: TokenSet =
    expressions::LITERAL_FIRST.union(expressions::atom::PATH_FIRST).union(TokenSet::new(&[
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
fn param_untyped(p: &mut Parser<'_>, m: Marker) -> bool { // GJL
    if !p.at(IDENT) {
        p.error("Expected parameter name");
        m.abandon(p);
        return false;
    }
    p.bump(IDENT);
    m.complete(p, PARAM);
    true
}

fn param_typed(p: &mut Parser<'_>, m: Marker) -> bool { // GJL
    let mut success = true;
    if p.current().is_type_name() {
        p.bump_any();
    } else {
        p.error("expected type annotation");
        success = false;
    }
    if !p.at(IDENT) {
        p.error("expected parameter name");
        m.abandon(p);
        return false
    }
    p.bump(IDENT);
    m.complete(p, PARAM);
    success
}

fn arg_gate_call_qubit(p: &mut Parser<'_>, m: Marker) -> bool { // GJL
    if p.at(HARDWAREIDENT) {
        p.bump(HARDWAREIDENT);
        m.complete(p, HARDWARE_QUBIT);
        return true
    }

    if !p.at(IDENT) {
        p.error("Expected parameter name");
        m.abandon(p);
        return false;
    }
//    let mcomp = expressions::atom::identifier(p);
    p.bump(IDENT);
    let mcomp = m.complete(p, IDENTIFIER);
    if p.at(T!['[']) {
//        expressions::index_expr(p, mcomp);
        expressions::indexed_identifer(p, mcomp);
        return true;
    }
    true
}
