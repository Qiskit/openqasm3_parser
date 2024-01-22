// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::asg;
use oq3_semantics::semantic_error::SemanticErrorList;
use oq3_semantics::symbols::{SymbolTable, SymbolType};
use oq3_semantics::syntax_to_semantics::parse_source_string;
use oq3_semantics::types::{ArrayDims, IsConst, Type};

fn parse_string(code: &str) -> (asg::Program, SemanticErrorList, SymbolTable) {
    parse_source_string(code, None, None::<&[&std::path::Path]>)
        .take_context()
        .as_tuple()
}

#[test]
fn test_from_string_one_qubit_decl() {
    let code = r#"
qubit q;
"#;
    let (program, errors, symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let stmt = program.first();
    assert!(matches!(stmt, Some(asg::Stmt::DeclareQuantum(_))));
    let qdecl = match stmt {
        Some(asg::Stmt::DeclareQuantum(qdecl)) => qdecl,
        _ => unreachable!(),
    };
    let varname_id = qdecl.name().clone().unwrap();
    assert_eq!(varname_id, symbol_table.lookup("q").unwrap().symbol_id());
    assert_eq!(&Type::Qubit, (symbol_table[&varname_id]).symbol_type());
    //    assert_eq!(&Type::Qubit(None), (*symbol_table.lookup_from_symbol_id(varname_id)).symbol_type());
}

#[test]
fn test_from_string_if_stmt() {
    let code = r##"
if (true) {
   1;
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let if_stmt = match program.first() {
        Some(asg::Stmt::If(if_stmt)) => if_stmt,
        _ => unreachable!(),
    };

    let cond_expr = if_stmt.condition().expression();
    let bool_literal = match cond_expr {
        asg::Expr::Literal(asg::Literal::Bool(literal)) => literal,
        _ => unreachable!(),
    };
    assert!(*bool_literal.value());

    let cond_ty = if_stmt.condition().get_type();
    assert_eq!(cond_ty, &Type::Bool(IsConst::True));
    assert!(if_stmt.else_branch().is_none());
}

#[test]
fn test_from_string_while_stmt() {
    let code = r##"
while (false) {
  1;
  2;
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let while_stmt = match program.first() {
        Some(asg::Stmt::While(while_stmt)) => while_stmt,
        _ => unreachable!(),
    };

    let cond_expr = while_stmt.condition().expression();
    let bool_literal = match cond_expr {
        asg::Expr::Literal(asg::Literal::Bool(literal)) => literal,
        _ => unreachable!(),
    };
    assert!(!(*bool_literal.value()));
    let cond_ty = while_stmt.condition().get_type();
    assert_eq!(cond_ty, &Type::Bool(IsConst::True));
    let body_statements = while_stmt.loop_body().statements();
    let inner = body_statements
        .iter()
        .map(|x| match x {
            asg::Stmt::ExprStmt(expr) => {
                let lit = expr.expression();
                match lit {
                    asg::Expr::Literal(asg::Literal::Int(v)) => Some(v),
                    _ => None,
                }
                .unwrap()
                .value()
            }
            _ => unreachable!(),
        })
        .collect::<Vec<_>>();
    assert_eq!(inner, vec![&1u128, &2u128]);
}

#[test]
fn test_indexed_identifier() {
    let code = r##"
x[1];
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_gate_decl_1() {
    let code = r##"
gate mygate(x, y) q, p, r {
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_gate_decl_2() {
    let code = r##"
gate mygate q, p, r {
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_gate_decl_3() {
    let code = r##"
gate bell q0, q1 {
  h q0;
  cx q0, q1;
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 2); // h and cx undefined
    assert_eq!(program.len(), 1);
}

#[test]
fn test_gate_call() {
    let code = r##"
mygate(x, y) q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 5);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_bit_string_literal() {
    let code = r#"
bit[4] b = "1001";
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_from_string_qubit_register_decl() {
    let code = r#"
qubit[3] q;
"#;
    let (program, errors, symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let stmt = program.first();
    assert!(matches!(stmt, Some(asg::Stmt::DeclareQuantum(_))));
    let qdecl = match stmt {
        Some(asg::Stmt::DeclareQuantum(qdecl)) => qdecl,
        _ => unreachable!(),
    };
    let varname_id = qdecl.name().clone().unwrap();
    assert_eq!(varname_id, symbol_table.lookup("q").unwrap().symbol_id());
    assert_eq!(
        &Type::QubitArray(ArrayDims::D1(3)),
        symbol_table[&varname_id].symbol_type()
    );
}

#[test]
fn test_from_string_measure() {
    let code = r#"
qubit q;
measure q;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}

// Issue #42
#[test]
fn test_from_string_measure_assign() {
    let code = r#"
qubit q;
bit c;
c = measure q;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_measure_indexed() {
    let code = r#"
qubit[2] q;
measure q[1];
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_measure_hardware() {
    let code = r#"
measure $0;
measure $42;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_gate_call_indexed() {
    let code = r#"
gate h q {}
qubit[2] q;
h q[1];
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_barrier() {
    let code = r#"
qubit[5] q;
barrier q;
barrier $0, $1;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_assign_indexed() {
    let code = r#"
qubit q;
bit[2] c;
c[0] = measure q;
bit k;
k = measure q;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    let assignment = match &program[2] {
        asg::Stmt::Assignment(assignment) => assignment,
        _ => unreachable!(),
    };
    assignment.rvalue().expression();
    matches!(
        assignment.rvalue().expression(),
        asg::Expr::MeasureExpression(_)
    );

    let assignment = match &program[4] {
        asg::Stmt::Assignment(assignment) => assignment,
        _ => unreachable!(),
    };
    assignment.rvalue().expression();
    matches!(
        assignment.rvalue().expression(),
        asg::Expr::MeasureExpression(_)
    );
    assert!(errors.is_empty());
    assert_eq!(program.len(), 5);
}

#[test]
fn test_from_string_assign_indexed_2() {
    let code = r#"
OPENQASM 3.0;

bit[2] out;
out[0] = measure $0;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}
