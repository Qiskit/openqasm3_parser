// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use super::*;
use crate::grammar::expressions::expr_block_contents;

// This is more or less the entry point for the parser crate.
// Entry point `fn parse` in lib.rs calls grammar::entry::top::source_file.
// And `fn source_file` call items::source_file_contents(p, false).
pub(super) fn source_file_contents(p: &mut Parser<'_>, stop_on_r_curly: bool) {
    while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
        item(p, stop_on_r_curly);
    }
}

pub(super) const ITEM_RECOVERY_SET: TokenSet = TokenSet::new(&[
    T![gate],
    T![def],
    T![defcal],
    T![defcalgrammar],
    T![include],
    T![cal],
    T![reset],
    //    T![measure],
    T![barrier],
    T![const],
    T![let],
    T![OPENQASM],
    T![;],
]);

// This is called in a loop from the top level. From `fn source_file_contents` above.
pub(super) fn item(p: &mut Parser<'_>, stop_on_r_curly: bool) {
    // GJL: Not the correct place for this.
    // But where then ?
    // if false {
    // } else {
    // if p.current() == IDENT && p.nth(1) == T![=] {
    //     assignment_statement(p);
    // } else {
    let m = p.start();
    let m = match opt_item(p, m) {
        Ok(()) => {
            if p.at(T![;]) {
                p.err_and_bump(
                    "expected item, found `;`\n\
                     consider removing this semicolon",
                );
            }
            return;
        }
        Err(m) => m,
    };
    match p.current() {
        T!['}'] if !stop_on_r_curly => {
            m.abandon(p);
            let e = p.start();
            p.error("unmatched `}`");
            p.bump(T!['}']);
            e.complete(p, ERROR);
        }
        EOF | T!['}'] => {
            m.abandon(p);
            p.error("expected an item GJL 2")
        }
        // Parse (semicolon terminated) statements until EOF or '}'.
        // I'm pretty sure there is no open '{' at this point, so this means stop at EOF only.
        // FIXME: for this reason, perhaps recursively call `fn item` instead.
        _ => {
            m.abandon(p);
            expr_block_contents(p);
        }
    }
    //    }
}

/// Try to parse an item, completing `m` in case of success.
pub(super) fn opt_item(p: &mut Parser<'_>, m: Marker) -> Result<(), Marker> {
    // FIXME: instead of following line, we need a function that includes looking for 'const' in classical types.
    // is_classical_type() is generated code that looks for single tokens.
    // This messy interface is error prone.
    let la = p.nth(1);
    if p.current().is_classical_type() && la != T!['('] {
        expressions::classical_declaration_stmt(p, m);
        return Ok(());
    };
    // if p.at(HARDWAREIDENT) {
    //     p.bump(HARDWAREIDENT);
    //     m.complete(p, HARDWARE_QUBIT);
    //     return Ok(())
    // }
    // if p.current().is_classical_type() {
    //     // we should not be handling expressions here, only statements.
    //     if la == T!['('] {
    //         expressions::cast_expr(p, m);
    //     } else {
    //         expressions::classical_declaration_stmt(p, m);
    //     }
    // } else {

    match p.current() {
        T![qubit] => qubit_declaration_stmt(p, m),
        T![const] => expressions::classical_declaration_stmt(p, m),
        IDENT if (la == IDENT || la == HARDWAREIDENT) => gate_call_stmt(p, m),
        IDENT if (la == T![=] && p.nth(2) != T![=]) => assignment_statement_with_marker(p, m),
        T![gate] => gate_definition(p, m),
        T![break] => break_(p, m),
        T![continue] => continue_(p, m),
        T![end] => end_(p, m),
        T![if] => if_stmt(p, m),
        T![while] => while_stmt(p, m),
        T![def] => def_(p, m),
        T![defcal] => defcal_(p, m),
        T![cal] => cal_(p, m),
        T![defcalgrammar] => defcalgrammar_(p, m),
        T![reset] => reset_(p, m),
        // measure is not a statement, does not belong here.
        //            T![measure] => measure_(p, m),
        T![barrier] => barrier_(p, m),
        T![OPENQASM] => version_string(p, m),
        T![include] => include(p, m),
        T![gphase] => gphase_call(p, m),
        // This is already done elsewhere
        //            T![let] => let_stmt(p, m),
        _ => return Err(m),
    }
    Ok(())
}

// expressions::call_expr also handles some occurences of gate calls
// FIXME: params in parens are likely always caught in expressions::call_expr
fn gate_call_stmt(p: &mut Parser<'_>, m: Marker) {
    //    expressions::var_name(p);
    expressions::atom::identifier(p); // name of gate
    assert!(!p.at(T!['(']));
    // This is never true, I hope
    if p.at(T!['(']) {
        expressions::call_arg_list(p);
    }
    params::arg_list_gate_call_qubits(p);
    p.expect(SEMICOLON);
    m.complete(p, GATE_CALL_STMT);
}

fn gphase_call(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![gphase]));
    p.bump(T![gphase]);
    expressions::expr(p);
    p.expect(SEMICOLON);
    m.complete(p, G_PHASE_CALL_STMT);
}

fn if_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![if]));
    //    let m = p.start();
    p.bump(T![if]);
    expressions::expr_no_struct(p);
    expressions::block_expr(p);
    if p.at(T![else]) {
        p.bump(T![else]);
        if p.at(T![if]) {
            let m = p.start();
            if_stmt(p, m);
        } else {
            expressions::block_expr(p);
        }
    }
    m.complete(p, IF_STMT);
}

//fn while_stmt(p: &mut Parser<'_>, m: Option<Marker>) -> CompletedMarker {
fn while_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![while]));
    p.bump(T![while]);
    expressions::expr_no_struct(p);
    expressions::block_expr(p);
    m.complete(p, WHILE_STMT);
}

// Called from atom::atom_expr
// FIXME: return CompletedMarker because this is called from `atom_expr`
// Where functions are called and what they return should be made more uniform.
pub(crate) fn assignment_statement(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    name(p);
    p.bump(T![=]);
    expressions::expr(p);
    p.expect(SEMICOLON);
    m.complete(p, ASSIGNMENT_STMT)
}

// Called from items::opt_item
pub(crate) fn assignment_statement_with_marker(p: &mut Parser<'_>, m: Marker) {
    name(p);
    p.bump(T![=]);
    expressions::expr(p);
    p.expect(SEMICOLON);
    m.complete(p, ASSIGNMENT_STMT);
}

fn qubit_declaration_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![qubit]));
    expressions::quantum_type_spec(p);
    expressions::var_name(p);
    p.expect(SEMICOLON);
    m.complete(p, QUANTUM_DECLARATION_STATEMENT);
}

// unused I hope
// fn opt_const_item(p: &mut Parser<'_>, m: Marker) -> Result<(), Marker> {
//     let la = p.nth(1);
//     match p.current() {
//         T![const] if la == IDENT => konst(p, m),
//         _ => return Err(m),
//     };
//     Ok(())
// }

// We call index_expr, which backtracks. So we have to
// complete the first IDENT.
// An alternative would be to do the logic here and
// avoid backtracking.
//
// I don't know why this kind of thing is evidently used for parsing rust.
// It should be used heavily for OQ3. This could be because expressions
// can be used in so many places in Rust. Eg. `myfunc(arg1, arg2)[ind1]`.
// But in OQ3, a qubit needs to
// Be an identifier or an identifier followed by '[', etc.
pub(crate) fn ident_or_index_expr(p: &mut Parser<'_>) {
    let m = p.start();
    match p.current() {
        IDENT => {
            p.bump(IDENT);
            match p.current() {
                T!['['] => {
                    let newm = m.complete(p, IDENTIFIER);
                    expressions::index_expr(p, newm);
                }
                _ => {
                    // FIXME: m.complete(p, IDENT) is valid, but it should not be
                    // it is a source of bugs!
                    m.complete(p, IDENTIFIER);
                }
            }
        }
        HARDWAREIDENT => {
            p.bump(HARDWAREIDENT);
            m.complete(p, HARDWARE_QUBIT);
        }
        _ => panic!(),
    }
}

fn reset_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![reset]);
    match p.current() {
        IDENT | HARDWAREIDENT => {
            ident_or_index_expr(p);
        }
        _ => {
            p.error("expecting name of qubit to reset");
            m.abandon(p);
            return;
        }
    }
    m.complete(p, RESET);
}

// FIXME: this has underscore, so should be private.
// We should be able to refactor to keep this private.
pub(crate) fn measure_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![measure]);
    match p.current() {
        IDENT | HARDWAREIDENT => {
            let m1 = p.start();
            params::arg_gate_call_qubit(p, m1);
        }
        _ => {
            p.error("expecting qubit(s) to measure");
            m.abandon(p);
            return;
        }
    }
    p.expect(T![;]);
    m.complete(p, MEASURE);
}

fn break_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![break]);
    p.expect(SEMICOLON);
    m.complete(p, BREAK_STMT);
}

fn continue_(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![continue]));
    p.bump(T![continue]);
    p.expect(SEMICOLON);
    m.complete(p, CONTINUE_STMT);
}

fn end_(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![end]));
    p.bump(T![end]);
    p.expect(SEMICOLON);
    m.complete(p, END_STMT);
}

fn gate_definition(p: &mut Parser<'_>, m: Marker) {
    // Read the keyword `gate`
    p.bump(T![gate]);

    // Read the name of the gate. (This records an error message on failure.)
    name_r(p, ITEM_RECOVERY_SET);

    // Read optional gate parameters (not qubit and params)
    if p.at(T!['(']) {
        params::param_list_gate_params(p);
    }
    // Read the list of qubit parameters
    params::param_list_gate_qubits(p);
    // Read the code block.
    expressions::block_expr(p);
    // Mark this attempt at reading an item as complete.
    m.complete(p, GATE);
}

fn defcal_(p: &mut Parser<'_>, m: Marker) {
    // Must read the keyword `gate`
    p.bump(T![defcal]);

    // Read the name of the gate. (This records an error message on failure.)
    name_r(p, ITEM_RECOVERY_SET);

    // Read optional gate parameters (not qubit and params)
    if p.at(T!['(']) {
        params::param_list_defcal_params(p);
    }
    // Read the list of qubit parameters
    params::param_list_defcal_qubits(p);

    opt_ret_type(p);
    // Read the code block.
    expressions::block_expr(p);
    // Mark this attempt at reading an item as complete.
    m.complete(p, DEF_CAL);
}

fn def_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![def]);

    // Read the name of the subroutine. (This records an error message on failure.)
    name_r(p, ITEM_RECOVERY_SET);

    // Read optional gate parameters (not qubit and params)
    if p.at(T!['(']) {
        params::param_list_def_params(p);
    } else {
        p.error("expected function arguments in def");
    }
    opt_ret_type(p);
    // Read the code block.
    expressions::block_expr(p);
    // Mark this attempt at reading an item as complete.
    m.complete(p, DEF);
}

fn filepath_r(p: &mut Parser<'_>, recovery: TokenSet) {
    if p.at(STRING) {
        let m = p.start();
        p.bump(STRING);
        m.complete(p, FILE_PATH);
    } else {
        p.err_recover("expected a path to a file", recovery);
    }
}

// FIXME: Don't know if recovery is useful here.
fn defcalgrammar_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![defcalgrammar]);
    filepath_r(p, ITEM_RECOVERY_SET);
    p.expect(SEMICOLON);
    m.complete(p, DEF_CAL_GRAMMAR);
}

fn include(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![include]);
    filepath_r(p, ITEM_RECOVERY_SET);
    p.expect(SEMICOLON);
    m.complete(p, INCLUDE);
}

// FIXME: block_expr is copied from r-a where it is used, as here, without recovery.
// Should we implement recovery?
fn cal_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![cal]);
    expressions::block_expr(p);
    m.complete(p, CAL);
}

fn version_string(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![OPENQASM]);
    version_(p);
    m.complete(p, VERSION_STRING);
}

fn version_(p: &mut Parser<'_>) -> bool {
    let m = p.start();
    //    if ! p.expect(SIMPLE_FLOAT_NUMBER) && ! p.at(SEMICOLON) {
    if !p.expect(FLOAT_NUMBER) && !p.at(SEMICOLON) {
        p.bump_any(); // FIXME eat til semicolon
    }
    p.expect(SEMICOLON);
    m.complete(p, VERSION);
    true
}

fn barrier_(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![barrier]);
    if !p.at(T![;]) {
        params::param_list_gate_qubits(p);
    }
    p.expect(SEMICOLON);
    m.complete(p, BARRIER);
}
