// Copyright contributors to the openqasm-parser project

#[cfg(not(feature = "in-rust-tree"))]
mod ast_src;
#[cfg(not(feature = "in-rust-tree"))]
mod sourcegen_ast;

use crate::ast;
use crate::ast::{HasModuleItem}; // for file.items()
use crate::ast::{HasTextName}; // for methods: text(), string()
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
use crate::{SourceFile};

// fn collect_items(code: &str) -> (usize, Vec<ast::Item>){
//     let parse = SourceFile::parse(code);
//     let file : SourceFile = parse.tree();
//     let items = file.items().collect::<Vec<_>>();
//     return (parse.errors.len(), items)
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
    // eprintln!("{:#?}", parse.syntax_node());
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
    // eprintln!("{:#?}", parse.syntax_node());
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
    use crate::{ast};
    use ast::{HasModuleItem, HasName};

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
    for item in file.items() {
        match item {
            ast::Item::DefCal(f) => defcal = Some(f),
            _ => (),
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
//    let file: SourceFile = parse.tree();
    let mut items = parse.tree().items();
    let decl = match items.next() {
        Some(ast::Item::ClassicalDeclarationStatement(s)) => s,
        _ => unreachable!(),
    };
    // println!("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS {:?}", decl);
    // println!("scalar type {:?}", decl.scalar_type().unwrap());
    let scalar_type = decl.scalar_type().unwrap();
    assert_eq!(scalar_type.kind(), ast::ScalarTypeKind::Int);
    assert!(scalar_type.designator().is_none());
    // println!(" name {:?}", scalar_type);
    // println!(" syntax {:?}", scalar_type.kind());
    // println!(" token {}", scalar_type.token());
    // println!(" token {:?}", scalar_type.token());

//        Some(ast::Item::ClassicalDeclarationStatement(s)) => Some(s),
//    assert_eq!(0, 1);
    // for item in items {
    //     match item {
    //         ast::Item::ClassicalDeclarationStatement(s) => decl = Some(s),
    //         _ => unreachable!(),
    //     }
    // }
//    return item
//    let ast::Item::ClassicalDeclarationStatement(_)> =
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

// FIXME: Should be zero errors!
#[test]
fn parse_cast_expr_test_7() {
    let code = r##"
z + int[32](x);
    "##;
    let parse = SourceFile::parse(code);
    assert_eq!(parse.errors.len(), 2);
}

