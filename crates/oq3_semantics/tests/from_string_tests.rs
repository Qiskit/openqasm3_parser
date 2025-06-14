// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::asg;
use oq3_semantics::semantic_error::{SemanticErrorKind, SemanticErrorList};
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

// FIXME: The small test harness we use here only counts semantic errors.
// but we need to detect syntax errors here.
// #[test]
// fn test_from_string_if_condition_parens() {
//     let code = r##"
// if x == 1 {
//    true;
// }
// "##;
//     let (program, errors, _symbol_table) = parse_string(code);
//     assert_eq!(errors.len(), 1);
//     assert_eq!(program.len(), 0);
// }

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
fn test_from_string_while_stmt_scope() {
    let code = r##"
while (false) {
  int x = 1;
}
x = 2;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(!errors.is_empty() && matches!(&errors[0].kind(), SemanticErrorKind::UndefVarError));
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_if_stmt_scope() {
    let code = r##"
if (false) {
  int x = 1;
}
x = 2;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::UndefVarError
    ));
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_if_stmt_scope_2() {
    let code = r##"
if (false) {
  int x = 1;
}
int x = 2;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
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
fn test_from_string_inv_gate_call() {
    let code = r#"
gate h q {}
qubit q;
inv @ h q;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_mult_mod_gate_call() {
    let code = r#"
gate h q {}
qubit q;
ctrl(3) @ inv @ h q;
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

fn expr_from_expr_stmt(stmt: &asg::Stmt) -> asg::Expr {
    match stmt {
        asg::Stmt::ExprStmt(texpr) => texpr.expression().clone(),
        _ => unreachable!(),
    }
}

fn literal_value(stmt: &asg::Stmt) -> Option<String> {
    match expr_from_expr_stmt(stmt) {
        asg::Expr::Literal(lit) => match lit {
            asg::Literal::Float(float) | asg::Literal::ImaginaryFloat(float) => {
                Some(float.value().to_string())
            }
            asg::Literal::Int(int) | asg::Literal::ImaginaryInt(int) => {
                if *int.sign() {
                    Some(format!("{}", int.value()))
                } else {
                    Some(format!("-{}", int.value()))
                }
            }
            _ => None,
        },
        _ => None,
    }
}

#[test]
fn test_from_string_unary_minus() {
    let code = r##"
- a;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    let expr = expr_from_expr_stmt(&program[0]);
    assert!(matches!(&expr, asg::Expr::UnaryExpr(_)));
}

#[test]
fn test_from_string_pos_lit_float() {
    let code = r##"
1.23;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "1.23");
}

#[test]
fn test_from_string_neg_lit_float() {
    let code = r##"
-1.23;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-1.23");
}

#[test]
fn test_from_string_pos_lit_int() {
    let code = r##"
123;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "123");
}

#[test]
fn test_from_string_neg_lit_int() {
    let code = r##"
-123;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-123");
}

#[test]
fn test_from_string_neg_lit_int_im() {
    let code = r##"
-1 im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-1");
}

#[test]
fn test_from_string_neg_lit_float_im() {
    let code = r##"
-10.1 im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-10.1");
}

#[test]
fn test_from_string_neg_spc_lit_int() {
    let code = r##"
- 123;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-123");
}

#[test]
fn test_from_string_neg_spc_lit_float() {
    let code = r##"
-  1.23;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
    let expr = literal_value(&program[0]).unwrap();
    assert_eq!(expr, "-1.23");
}

// PR #91
#[test]
fn test_from_string_bin_expr_no_spc() {
    let code = r##"
a-1.23;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 1);
    assert!(matches!(
        expr_from_expr_stmt(&program[0]),
        asg::Expr::BinaryExpr(_)
    ));
}

#[test]
fn test_from_string_switch() {
    let code = r##"
int c = 1;
switch(c) {
   case 1 {
    1;
   }
   case 2 {
    2;
   }
   default {
    3;
   }
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    assert!(matches!(&program[1], asg::Stmt::SwitchCaseStmt(_)));
}

#[test]
fn test_from_string_switch_scope() {
    let code = r##"
int c = 1;
switch(c) {
   case 1 {
      int x = 1;
    }
   case 2 {
      int x = 2;
    }
}
int x = 3;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 3);
    assert!(matches!(&program[1], asg::Stmt::SwitchCaseStmt(_)));
}

#[test]
fn test_from_string_cast_width() {
    let code = r##"
float x;
int[32](x);
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    assert!(matches!(&program[1], asg::Stmt::ExprStmt(_)));
}

#[test]
fn test_from_string_alias_stmt() {
    let code = r##"
qubit[10] q;
let r = q[0:3] ++ q[5:9];
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    assert!(matches!(&program[1], asg::Stmt::Alias(_)));
}

#[test]
fn test_from_string_duration_decl() {
    let code = r##"
duration x = 100 ns;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
    let stmt = &program[0];
    let decl = match stmt {
        asg::Stmt::DeclareClassical(decl) => decl,
        _ => unreachable!(),
    };
    let tlit = match decl.initializer().unwrap().expression() {
        asg::Expr::Literal(asg::Literal::TimingIntLiteral(x)) => x,
        _ => unreachable!(),
    };
    assert_eq!(*tlit.value(), 100);
    assert_eq!(tlit.time_unit(), &asg::TimeUnit::NanoSecond);
}

#[test]
fn test_from_string_delay_1() {
    let code = r##"
qubit q;
delay[100ms] q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_delay_2() {
    let code = r##"
qubit q;
int x;
delay[x] q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_def_1() {
    let code = r##"
gate h q {}
def xmeasure(qubit q) -> bit { h q; return measure q; }
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_def_2() {
    let code = r##"
gate h q {}
gate rz(theta) q {}

def pmeasure(angle[32] theta, qubit q) -> bit {
  rz(theta) q;
  h q;
  return measure q;
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 3);
}

// Issue #183
#[test]
fn test_from_string_def_3() {
    let code = r##"
gate cx p, q {}

def xcheck(qubit[4] d, qubit a) -> bit {
  reset a;
  for int i in [0: 3] {cx d[i], a;}
  return measure a;
}
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_stdgates() {
    let code = r##"
include "stdgates.inc";
qubit q;
h q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
}

#[test]
fn test_from_string_stdgates_2() {
    let code = r##"
gate h q {}
include "stdgates.inc";
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_from_string_check_assign_types_1() {
    let code = r##"
bit[3] b;
qubit[2] q;
b = measure q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleDimensionError
    ));
    assert_eq!(program.len(), 3);
}

#[test]
fn test_from_string_check_assign_types_2() {
    let code = r##"
float b;
qubit[2] q;
b = measure q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
    assert_eq!(program.len(), 3);
}

// Bug
#[test]
fn test_from_string_check_assign_types_3() {
    let code = r##"
float b[2];
qubit[2] q;
b[1] = measure q;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
}

#[test]
fn test_from_string_check_assign_types_4() {
    let code = r##"
float x0;
x0 = 1;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
}

#[test]
fn test_from_string_check_assign_types_5() {
    let code = r##"
uint x0;
x0 = 1;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
}

#[test]
fn test_from_string_check_assign_types_6() {
    let code = r##"
uint x0;
x0 = -1;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(&errors[0].kind(), SemanticErrorKind::CastError));
}

#[test]
fn test_from_string_check_assign_types_7() {
    let code = r##"
int x0;
x0 = 2.0;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

#[test]
fn test_from_string_check_assign_types_8() {
    let code = r##"
float xx = 3;
int yy;
yy = xx;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

#[test]
fn test_from_string_imaginary_int_literal_1() {
    let code = r##"
10 im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    let stmt = &program[0];
    match stmt {
        asg::Stmt::ExprStmt(texpr) => match texpr.expression() {
            asg::Expr::Literal(asg::Literal::ImaginaryInt(_)) => (),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
}

#[test]
fn test_from_string_imaginary_int_literal_2() {
    let code = r##"
10im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    let stmt = &program[0];
    match stmt {
        asg::Stmt::ExprStmt(texpr) => match texpr.expression() {
            asg::Expr::Literal(asg::Literal::ImaginaryInt(_)) => (),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
}

#[test]
fn test_from_string_imaginary_float_literal_1() {
    let code = r##"
12.3 im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    let stmt = &program[0];
    match stmt {
        asg::Stmt::ExprStmt(texpr) => match texpr.expression() {
            asg::Expr::Literal(asg::Literal::ImaginaryFloat(_)) => (),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
}

// Issue #200
#[test]
fn test_from_string_init_bit_with_measure() {
    let code = r##"
bit mid = measure $1;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

// Issue #203
#[test]
fn test_from_string_declaration_type_check_1() {
    let code = r##"
qubit q;
float a = q;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 2);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

// Issue #203
#[test]
fn test_from_string_declaration_type_check_2() {
    let code = r##"
float c = 2.1;
int d = c;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 2);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

// Issue #203
#[test]
fn test_from_string_declaration_type_check_3() {
    let code = r##"
float c = 2.1;
int d = c;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 2);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

// at least it's not python
// Check that the classical declaration statment `stmt` casts its RHS
// to type `expected_type`.
fn _check_cast_type(stmt: &asg::Stmt, expected_type: &Type) {
    match stmt {
        asg::Stmt::DeclareClassical(decl) => match decl.initializer() {
            Some(texpr) => {
                match texpr.expression() {
                    asg::Expr::Cast(cast) => {
                        assert!(cast.get_type() == expected_type)
                    }
                    _ => unreachable!(),
                };
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

// Issue #203
#[test]
fn test_from_string_declaration_type_check_4() {
    let code = r##"
int a = 2;
float b = a;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    _check_cast_type(&program[1], &Type::Float(None, IsConst::False));
}

#[test]
fn test_from_string_declaration_type_check_5() {
    let code = r##"
int a = 2;
float[64] b = a;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    _check_cast_type(&program[1], &Type::Float(Some(64), IsConst::False));
}

#[test]
fn test_from_string_declaration_type_check_6() {
    let code = r##"
float[32] a;
float[64] b = a;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    _check_cast_type(&program[1], &Type::Float(Some(64), IsConst::False));
}

#[test]
fn test_from_string_declaration_type_check_7() {
    let code = r##"
float[32] a;
const float[64] b = a;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 2);
    _check_cast_type(&program[1], &Type::Float(Some(64), IsConst::True));
}

#[test]
fn test_from_string_declaration_type_check_8() {
    let code = r##"
float a;
float[64] b = a;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

#[test]
fn test_from_string_declaration_type_check_9() {
    let code = r##"
duration a = 2.0;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        &errors[0].kind(),
        SemanticErrorKind::IncompatibleTypesError
    ));
}

// Issue #208
#[test]
fn test_from_string_declaration_type_check_10() {
    let code = r##"
complex[float[64]] c1 = 1.0 + 2.0im;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

// Issue #208
#[test]
fn test_from_string_declaration_type_check_11() {
    let code = r##"
const int[64] i1 = 4;
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

// Issue #208
#[test]
fn test_from_string_declaration_type_check_12() {
    let code = r##"
const bit[8] b2 = "0010_1010";
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_13() {
    let code = r##"
float[64] c1 = 1.0 + 2.0im;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_14() {
    let code = r##"
float c1 = 1.0 + 2.0im;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_15() {
    let code = r##"
int c1 = 1.0 + 2.0im;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_16() {
    let code = r##"
int[8] c1 = 1.0 + 2.0im;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_17() {
    let code = r##"
uint a = 1;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
}

#[test]
fn test_from_string_declaration_type_check_18() {
    let code = r##"
uint[64] a = 22;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
}

#[test]
fn test_from_string_declaration_type_check_19() {
    let code = r##"
uint[64] a = -1;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}

#[test]
fn test_from_string_declaration_type_check_20() {
    let code = r##"
uint a = -99;
"##;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
}
