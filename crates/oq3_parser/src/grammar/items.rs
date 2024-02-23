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
    T![barrier],
    T![const],
    T![let],
    T![OPENQASM],
    T![;],
]);

// This is called in a loop from the top level. From `fn source_file_contents` above.
pub(super) fn item(p: &mut Parser<'_>, stop_on_r_curly: bool) {
    let m = p.start();
    let m = match opt_item(p, m) {
        Ok(()) => {
            if p.at(T![;]) {
                p.err_and_bump(
                    "expected statement, found `;`\n\
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
        }
        // Parse (semicolon terminated) statements until EOF or '}'.
        // I'm pretty sure there is no open '{' at this point, so this means stop at EOF only.
        // FIXME: for this reason, perhaps recursively call `fn item` instead.
        _ => {
            m.abandon(p);
            expr_block_contents(p);
        }
    }
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

    match p.current() {
        T![qubit] => qubit_declaration_stmt(p, m),
        T![const] => expressions::classical_declaration_stmt(p, m),
        T![gate] => gate_definition(p, m),
        T![break] => break_(p, m),
        T![continue] => continue_(p, m),
        T![end] => end_(p, m),
        T![if] => if_stmt(p, m),
        T![while] => while_stmt(p, m),
        T![for] => for_stmt(p, m),
        T![def] => def_(p, m),
        T![defcal] => defcal_(p, m),
        T![cal] => cal_(p, m),
        T![defcalgrammar] => defcalgrammar_(p, m),
        T![reset] => reset_stmt(p, m),
        T![barrier] => barrier_(p, m),
        T![OPENQASM] => version_string(p, m),
        T![include] => include(p, m),
        T![switch] => switch_case_stmt(p, m),
        _ => return Err(m),
    }
    Ok(())
}

fn switch_case_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![switch]));
    p.bump(T![switch]);
    p.expect(T!['(']);
    expressions::expr(p);
    p.expect(T![')']);
    p.expect(T!['{']);
    if !p.at(T![case]) {
        p.error("expecting `case` keyword");
    }
    while p.at(T![case]) {
        let m1 = p.start();
        p.bump(T![case]);
        params::expression_list(p);
        expressions::try_block_expr(p);
        m1.complete(p, CASE_EXPR);
    }
    if p.eat(T![default]) {
        expressions::try_block_expr(p);
    }
    p.expect(T!['}']);
    m.complete(p, SWITCH_CASE_STMT);
}

fn if_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![if]));
    p.bump(T![if]);
    p.expect(T!['(']);
    expressions::expr(p);
    p.expect(T![')']);
    expressions::try_block_expr(p);
    if p.at(T![else]) {
        p.bump(T![else]);
        if p.at(T![if]) {
            let m = p.start();
            if_stmt(p, m);
        } else {
            expressions::try_block_expr(p);
        }
    }
    m.complete(p, IF_STMT);
}

fn while_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![while]));
    p.bump(T![while]);
    p.expect(T!['(']);
    expressions::expr(p);
    p.expect(T![')']);
    expressions::try_block_expr(p);
    m.complete(p, WHILE_STMT);
}

fn for_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![for]));
    p.bump(T![for]);
    // Type specifier of the loop variable.
    expressions::type_spec(p);
    // The loop variable.
    name(p);
    p.expect(T![in]);
    // The "iterator"
    let m1 = p.start();
    if p.at(T!['{']) {
        expressions::set_expression(p);
    } else if p.at(T!['[']) {
        expressions::range_expr(p);
    } else {
        expressions::expr(p);
    }
    m1.complete(p, FOR_ITERABLE);
    // The body of the for loop
    if p.at(T!['{']) {
        expressions::block_expr(p);
    } else {
        expressions::stmt(p);
    }
    m.complete(p, FOR_STMT);
}

fn qubit_declaration_stmt(p: &mut Parser<'_>, m: Marker) {
    assert!(p.at(T![qubit]));
    expressions::quantum_type_spec(p);
    expressions::var_name(p);
    p.expect(SEMICOLON);
    m.complete(p, QUANTUM_DECLARATION_STATEMENT);
}

fn reset_stmt(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![reset]);
    match p.current() {
        IDENT | HARDWAREIDENT => {
            let m1 = p.start();
            // Parses elements that can be cast to GateOperand
            params::arg_gate_call_qubit(p, m1);
        }
        _ => {
            p.error("expecting name of qubit or register to reset");
            m.abandon(p);
            return;
        }
    }
    p.expect(SEMICOLON);
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
    expressions::try_block_expr(p);
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
    expressions::try_block_expr(p);
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
    expressions::try_block_expr(p);
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
    expressions::try_block_expr(p);
    m.complete(p, CAL);
}

fn version_string(p: &mut Parser<'_>, m: Marker) {
    p.bump(T![OPENQASM]);
    version_(p);
    m.complete(p, VERSION_STRING);
}

fn version_(p: &mut Parser<'_>) -> bool {
    let m = p.start();
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
        params::arg_list_gate_call_qubits(p);
    }
    p.expect(SEMICOLON);
    m.complete(p, BARRIER);
}
