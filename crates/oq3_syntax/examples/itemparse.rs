// Copyright contributors to the openqasm-parser project

use oq3_syntax::SourceFile;
use oq3_syntax::ast;
use ast::{HasModuleItem, HasName};

#[allow(dead_code)]
fn parse_some_code() {
    let code = r##"
int q;

OPENQASM 3.1;

include "stdgates.qasm";

defcalgrammar "openpulse";

gate mygate q1, q2 {
  x q1;
}

defcal xmeasure(int a, int b) q, p -> bit {
   1 + 1;
}

def myfunc(int a, int b) -> int {
   x = 3;
}

cal {
  x + y;
}

   "##;
    parse_print_items(code);
}

fn try_int_def() {
    let code = r##"
int[64] x = 142;
"##;
    parse_print_items(code);
}

//use parser::syntax_kind::SyntaxKind;
fn main () {
    //    parts_testing();
    try_int_def();
//    parse_some_code();
}

fn print_item(item: ast::Item) {
    match item {
        ast::Item::Gate(gate) => {print_gate(gate)},
        ast::Item::Def(def) => {print_def(def)},
        ast::Item::DefCal(defcal) => {print_defcal(defcal)},
        ast::Item::DefCalGrammar(defcg) => {print_defcalgrammar(defcg)},
        ast::Item::Cal(cal) => {print_cal(cal)},
        ast::Item::VersionString(version_string) => {print_version_string(version_string)},
        ast::Item::Include(include) => {print_include(include)},
        ast::Item::ClassicalDeclarationStatement(type_decl) => {print_type_declaration_statement(type_decl)},
        _ => {println!("unhandled item: {:?}", item)},
    }
}

fn parse_print_items(code: &str) {
    use oq3_syntax::AstNode;
    let parse = SourceFile::parse(&code);
    let file: SourceFile = parse.tree();
    println!("Found {} items", file.items().collect::<Vec<_>>().len());
    for item in file.items() {
        print!("desc {}: ", item.syntax().descendants().collect::<Vec<_>>().len());
        print_item(item.clone());
        println!("");
//        println!("{:?}", item.syntax().descendants().collect::<Vec<_>>());
        for d in item.syntax().descendants().collect::<Vec<_>>() {
//        for d in item.children().collect::<Vec<_>>() {
            //            print_item(d.clone());
            println!(" {}", d);
//            println!(" {:?}", d);
        }
        println!();
    }
}

//
// Printing of each item
//

#[allow(dead_code)]
fn parts_testing() {
    let code = r##"
gate mygate q {
  h x;
}
    "##;
    // let parse = SourceFile::parse(&code);
    // let file: SourceFile = parse.tree();
    let file: SourceFile = SourceFile::parse(&code).tree();
    let mut gateitem = None;
    for item in file.items() {
        match item {
            ast::Item::Gate(gate) => {
                gateitem = Some(gate);
                break;
            },
            _ => (),
        }
    }
    println!("{}", test_gate_def(gateitem.unwrap(), ("mygate", "q")));
}

#[allow(dead_code)]
fn test_gate_def(gate: ast::Gate, (name, qubit_list) : (&str, &str)) -> bool {
    return format!("{}", gate.name().unwrap()) == name
        && format!("{}", gate.qubit_params().unwrap()) == qubit_list;
}

#[allow(dead_code)]
fn print_type_declaration_statement(type_decl: ast::ClassicalDeclarationStatement) {
    println!("Type declaration");
    print!(" scalar_type ");
    let scalar_type = type_decl.scalar_type().unwrap();
    println!(" name {:?}", scalar_type);
    println!(" syntax {:?}", scalar_type.kind());
    println!(" token {}", scalar_type.token());
    println!(" token {:?}", scalar_type.token());
    // let width = type_spec.expr();
    // print!(" width ");
    // if !width.is_none() {
    //     println!("'{}'", width.unwrap());
    // } else {
    //     println!("none");
    // }
    // if ! type_decl.type_spec().is_none() {
    //     println!("{:?}", type_decl.type_spec().unwrap());
    // }
    // else {
    //     println!(" none");
    // }
    print!(" initial value: ");
    if ! type_decl.expr().is_none() {
        print!("{}", type_decl.expr().unwrap());
    }
    else {
        print!(" none");
    }


    // println!("Type declaration: declared_var.expr '{:?}'", type_decl.declared_var().unwrap().expr());
    // println!(" eq_token {:?}", type_decl.eq_token());
    // println!("        expr '{:?}'", type_decl.expr());
//    println!("Stub unhandled item: {:?}", type_decl);
}

fn print_gate(gate: ast::Gate) {
    println!("Gate\ngate name: '{}'", gate.name().unwrap());
    if !gate.angle_params().is_none() {
        println!("parameters: '{}'", gate.angle_params().unwrap());
    }
    println!("qubits: '{}'", gate.qubit_params().unwrap());
    print!("body: '{}'", gate.body().unwrap());
}

fn print_defcal(defcal: ast::DefCal) {
    println!("DefCal\ndefcal name: '{}'", defcal.name().unwrap());
    if !defcal.param_list().is_none() {
        println!("parameters: '{}'", defcal.param_list().unwrap());
    }
    println!("qubits: '{}'", defcal.qubit_list().unwrap());
    if !defcal.ret_type().is_none() {
        println!("return type: '{}'", defcal.ret_type().unwrap());
    }
    print!("body: '{}'", defcal.body().unwrap());
}

fn print_def(def: ast::Def) {
    println!("Def\ndef name: '{}'", def.name().unwrap());
    if !def.param_list().is_none() {
        println!("parameters: '{}'", def.param_list().unwrap());
    }
    if !def.ret_type().is_none() {
        println!("return type: '{}'", def.ret_type().unwrap());
    }
    print!("body: '{}'", def.body().unwrap());
}

fn print_defcalgrammar(defcg: ast::DefCalGrammar) {
    println!("DefCalgrammar\ndefcalgrammar token: '{}'", defcg.defcalgrammar_token().unwrap());
    if !defcg.file().is_none() {
        print!("file: '{}'", defcg.file().unwrap());
    } else {
        print!("file: NONE");
    }
}

fn print_cal(cal: ast::Cal) {
    println!("Cal\ncal token: '{}'", cal.cal_token().unwrap());
    if !cal.body().is_none() {
        print!("body: '{}'", cal.body().unwrap());
    } else {
        print!("body: NONE");
    }
}

fn print_version_string(version_string: ast::VersionString) {
    println!("VersionString\n openqasm_token: '{}'", version_string.OPENQASM_token().unwrap());
    if !version_string.version().is_none() {
        print!("version: '{}'", version_string.version().unwrap());
    } else {
        print!("version: NONE");
    }
}

fn print_include(include: ast::Include) {
    println!("Include\ninclude token: '{}'", include.include_token().unwrap());
    if !include.file().is_none() {
        print!("file: '{}'", include.file().unwrap());
    } else {
        print!("file: NONE");
    }
}
