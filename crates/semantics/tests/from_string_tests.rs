use semantics::asg;
use semantics::semantic_error::SemanticErrorList;
use semantics::symbols::{SymbolTable, SymbolType};
use semantics::syntax_to_semantics::parse_source_string;
use semantics::types::{IsConst, Type};

fn parse_string(code: &str) -> (asg::Program, SemanticErrorList, SymbolTable) {
    parse_source_string(code, None).take_context().as_tuple()
}

#[test]
fn test_from_string_one_qubit_decl() {
    let code = r##"
qubit q;
"##;
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
    assert_eq!(*bool_literal.value(), true);

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
    assert_eq!(*bool_literal.value(), false);
    let cond_ty = while_stmt.condition().get_type();
    assert_eq!(cond_ty, &Type::Bool(IsConst::True));
    let body_statements = while_stmt.loop_body().statements();
    match body_statements[1] {
        _ => (),
    };
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
    assert_eq!(errors.len(), 4);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_bit_string_literal() {
    let code = r##"
bit[4] b = "1001";
"##;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

// #[test]
// fn test_include() {
//     let code = r##"
// include "somefile.qasm";
// "##;
//     let (program, errors, symbol_table) = parse_string(code);
//     assert_eq!(errors.len(), 0);
//     assert_eq!(program.len(), 1);
// }

// #[test]
// fn test_from_string_qubit_register_decl() {
//     let code = r##"
// qubit[3] q;
// "##;
//     let (program, errors, symbol_table) = parse_string(code);
//     assert!(errors.is_empty());
//     assert_eq!(program.len(), 1);
//     let stmt = program.first();
//     assert!(matches!(stmt, Some(asg::Stmt::DeclareQuantum(_))));
//     let qdecl = match stmt {
//         Some(asg::Stmt::DeclareQuantum(qdecl)) => qdecl,
//         _ => unreachable!(),
//     };
//     let varname_id = qdecl.name().clone().unwrap();
//     assert_eq!(varname_id, symbol_table.lookup("q").unwrap().symbol_id());
//     assert_eq!(&Type::Qubit(Some(3)), (symbol_table[varname_id]).symbol_type());
// }
