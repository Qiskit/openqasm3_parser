// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use ast::HasName;
use oq3_syntax::ast;
use oq3_syntax::SourceFile;

#[allow(dead_code)]
fn parse_some_code() {
    let code = r#"
int q;

OPENQASM 3.1;

include "stdgates.inc";

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

   "#;
    parse_print_stmts(code);
}

fn try_int_def() {
    let code = r##"
int[64] x = 142;
"##;
    parse_print_stmts(code);
}

//use parser::syntax_kind::SyntaxKind;
fn main() {
    //    parts_testing();
    try_int_def();
    //    parse_some_code();
}

fn print_stmt(stmt: ast::Stmt) {
    match stmt {
        ast::Stmt::Gate(gate) => print_gate(gate),
        ast::Stmt::Def(def) => print_def(def),
        ast::Stmt::DefCal(defcal) => print_defcal(defcal),
        ast::Stmt::DefCalGrammar(defcg) => print_defcalgrammar(defcg),
        ast::Stmt::Cal(cal) => print_cal(cal),
        ast::Stmt::VersionString(version_string) => print_version_string(version_string),
        ast::Stmt::Include(include) => print_include(include),
        ast::Stmt::ClassicalDeclarationStatement(type_decl) => {
            print_type_declaration_statement(type_decl)
        }
        _ => {
            println!("unhandled stmt: {:?}", stmt)
        }
    }
}

fn parse_print_stmts(code: &str) {
    use oq3_syntax::AstNode;
    let parse = SourceFile::parse(code);
    let file: SourceFile = parse.tree();
    println!(
        "Found {} stmts",
        file.statements().collect::<Vec<_>>().len()
    );
    for stmt in file.statements() {
        print!(
            "desc {}: ",
            stmt.syntax().descendants().collect::<Vec<_>>().len()
        );
        print_stmt(stmt.clone());
        println!();
        for d in stmt.syntax().descendants().collect::<Vec<_>>() {
            println!(" {}", d);
        }
        println!();
    }
}

//
// Printing of each stmt
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
    let file: SourceFile = SourceFile::parse(code).tree();
    let mut gatestmt = None;
    for stmt in file.statements() {
        if let ast::Stmt::Gate(gate) = stmt {
            gatestmt = Some(gate);
            break;
        };
    }
    println!("{}", test_gate_def(gatestmt.unwrap(), ("mygate", "q")));
}

#[allow(dead_code)]
fn test_gate_def(gate: ast::Gate, (name, qubit_list): (&str, &str)) -> bool {
    format!("{}", gate.name().unwrap()) == name
        && format!("{}", gate.qubit_params().unwrap()) == qubit_list
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
    print!(" initial value: ");
    if type_decl.expr().is_some() {
        print!("{}", type_decl.expr().unwrap());
    } else {
        print!(" none");
    }
}

fn print_gate(gate: ast::Gate) {
    println!("Gate\ngate name: '{}'", gate.name().unwrap());
    if gate.angle_params().is_some() {
        println!("parameters: '{}'", gate.angle_params().unwrap());
    }
    println!("qubits: '{}'", gate.qubit_params().unwrap());
    print!("body: '{}'", gate.body().unwrap());
}

fn print_defcal(defcal: ast::DefCal) {
    println!("DefCal\ndefcal name: '{}'", defcal.name().unwrap());
    if defcal.param_list().is_some() {
        println!("parameters: '{}'", defcal.param_list().unwrap());
    }
    println!("qubits: '{}'", defcal.qubit_list().unwrap());
    if defcal.return_signature().is_some() {
        println!("return type: '{}'", defcal.return_signature().unwrap());
    }
    print!("body: '{}'", defcal.body().unwrap());
}

fn print_def(def: ast::Def) {
    println!("Def\ndef name: '{}'", def.name().unwrap());
    if def.typed_param_list().is_some() {
        println!("parameters: '{}'", def.typed_param_list().unwrap());
    }
    if def.return_signature().is_some() {
        println!("return type: '{}'", def.return_signature().unwrap());
    }
    print!("body: '{}'", def.body().unwrap());
}

fn print_defcalgrammar(defcg: ast::DefCalGrammar) {
    println!(
        "DefCalgrammar\ndefcalgrammar token: '{}'",
        defcg.defcalgrammar_token().unwrap()
    );
    if defcg.file().is_some() {
        print!("file: '{}'", defcg.file().unwrap());
    } else {
        print!("file: NONE");
    }
}

fn print_cal(cal: ast::Cal) {
    println!("Cal\ncal token: '{}'", cal.cal_token().unwrap());
    if cal.body().is_some() {
        print!("body: '{}'", cal.body().unwrap());
    } else {
        print!("body: NONE");
    }
}

fn print_version_string(version_string: ast::VersionString) {
    println!(
        "VersionString\n openqasm_token: '{}'",
        version_string.OPENQASM_token().unwrap()
    );
    if version_string.version().is_some() {
        print!("version: '{}'", version_string.version().unwrap());
    } else {
        print!("version: NONE");
    }
}

fn print_include(include: ast::Include) {
    println!(
        "Include\ninclude token: '{}'",
        include.include_token().unwrap()
    );
    if include.file().is_some() {
        print!("file: '{}'", include.file().unwrap());
    } else {
        print!("file: NONE");
    }
}
