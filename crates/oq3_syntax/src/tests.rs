// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

mod ast_src;
mod sourcegen_ast;

use crate::ast;
use crate::ast::HasTextName; // for methods: text(), string()
                             //use oq3_syntax::ast;
                             // use std::{
                             //    fs,
                             //    path::{Path, PathBuf},
                             // };
                             // use ast::HasName;
                             //use expect_test::expect_file;
                             // use rayon::prelude::*;
                             // Adds complication from rust-analyzer
                             // use test_utils::{bench, bench_fixture, project_root};
                             //use crate::{ast, AstNode, SourceFile, SyntaxError};
use crate::SourceFile;

// fn collect_stmts(code: &str) -> (usize, Vec<ast::Stmt>){
//     let parse = SourceFile::parse(code);
//     let file : SourceFile = parse.tree();
//     let stmts = file.statements().collect::<Vec<_>>();
//     return (parse.errors.len(), stmts)
// }

#[test]
fn parse_measure_1_test() {
    let code = r##"
measure q;
    "##;
    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_measure_err1_test() {
    let code = r##"
measure;
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_barrier_1_test() {
    let code = r##"
barrier q;
    "##;
    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_barrier_2_test() {
    let code = r##"
barrier;
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_gate_def_test() {
    let code = r##"
gate h q {
   U(π/2, 0, π, q);
   gphase(-π/4);
}
    "##;
    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_gate_call_test() {
    let code = r##"
h q;
    "##;
    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_gate_call_err1_test() {
    let code = r##"
h q; ()
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_let_test() {
    let code = r##"
def myfunc() {
   let x = q;
}
    "##;
    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_qasm_test() {
    let code = r##"
gate chpase(x) a, b {
 CX a, b;
}
    "##;

    let parse = SourceFile::parse(code);
    assert!(parse.ok().is_ok());
}

#[test]
fn parse_qasm_err1_test() {
    let code = r##"
gate chpase() a, b {
 CX a, b;
}
    "##;

    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_qasm_err2_test() {
    let code = r##"
gate chpase(x) a b {
 CX a, b;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_qasm_err3_test() {
    let code = r##"
gate chpase() a b {
 CX a, b;
}
    "##;

    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 2);
}

#[test]
fn parse_qasm_defcal_2_test() {
    let code = r##"
defcal xmeasure(int a, int b) q, p {
   1 + 1;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_qasm_defcal_err2_test() {
    let code = r##"
defcal xmeasure(int a, b) q, p -> bit {
   1 + 1;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_qasm_defcal_err1_test() {
    let code = r##"
defcal xmeasure(int a, int b) q, p -> {
   1 + 1;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn parse_qasm_defcal_test() {
    let code = r##"
defcal xmeasure(int a, int b) q, p -> bit {
   1 + 1;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_qasm_def_test() {
    let code = r##"
def xmeasure(int q, int q2) -> bit {
    h q;
    return measure q;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_qasm_def2_test() {
    let code = r##"
def xmeasure(q) -> bit {
    h q;
    return measure q;
}
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 1);
}

#[test]
fn with_details_test() {
    use crate::ast;
    use ast::HasName;

    let code = r##"
defcal xmeasure(int a, int b) q, p -> bit {
   1 + 1;
}
   "##;

    // Get tree and list of errors
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);

    // Get just the tree, as a SyntaxNode
    let file: SourceFile = parse.tree();

    let mut defcal = None;
    for stmt in file.statements() {
        if let ast::Stmt::DefCal(f) = stmt {
            defcal = Some(f)
        }
    }
    let defcal: ast::DefCal = defcal.unwrap();
    let name: Option<ast::Name> = defcal.name();
    let name = name.unwrap();
    assert_eq!(name.text(), "xmeasure");
}

#[test]
fn variable_declaration() {
    let code = r##"
int x;
   "##;
    let parse = SourceFile::parse(code);
    assert!(parse.errors().is_empty());
    let mut stmts = parse.tree().statements();
    let decl = match stmts.next() {
        Some(ast::Stmt::ClassicalDeclarationStatement(s)) => s,
        _ => unreachable!(),
    };
    let scalar_type = decl.scalar_type().unwrap();
    assert_eq!(scalar_type.kind(), ast::ScalarTypeKind::Int);
    assert!(scalar_type.designator().is_none());
}

#[test]
fn parse_cast_expr_test_1() {
    let code = r##"
int(x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_cast_expr_test_2() {
    let code = r##"
uint(x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_cast_expr_test_3() {
    let code = r##"
float(x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_cast_expr_test_4() {
    let code = r##"
int[32](x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_cast_expr_test_5() {
    let code = r##"
z + int(x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

#[test]
fn parse_cast_expr_test_6() {
    let code = r##"
int(x) + z;
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}

// Issue #208 and associated PR
#[test]
fn parse_cast_expr_test_7() {
    let code = r##"
z + int[32](x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 0);
}
