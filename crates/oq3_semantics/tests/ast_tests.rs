// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::asg;
use oq3_semantics::symbols;
use oq3_semantics::types;

//
// TExpr
//

#[test]
fn test_texpr_bool_literal() {
    use asg::BoolLiteral;
    use types::{IsConst, Type};

    let bool_value = true;
    let literal = BoolLiteral::new(bool_value);
    let texpr = literal.clone().to_texpr();
    assert_eq!(texpr.expression(), &literal.to_expr());
    assert_eq!(texpr.get_type(), &Type::Bool(IsConst::True));
}

#[test]
fn test_texpr_int_literal() {
    use asg::IntLiteral;
    use types::{IsConst, Type};

    let literal = IntLiteral::new(1_u32);
    let texpr = literal.clone().to_texpr();
    assert_eq!(texpr.expression(), &literal.to_expr());
    assert_eq!(texpr.get_type(), &Type::UInt(Some(128), IsConst::True));
}

//
// Structs representing expressions in ASG
//

//
// Literal
//

#[test]
fn test_int_literal() {
    use asg::IntLiteral;

    let int_value = 42;
    let literal = IntLiteral::new(int_value as u32);
    assert_eq!(*literal.value(), int_value as u128);
}

#[test]
fn test_bitstring_literal() {
    use asg::BitStringLiteral;
    //    use types::{Type, IsConst};

    let bit_string_value = "10110";
    let literal = BitStringLiteral::new(bit_string_value.to_string());
    assert_eq!(literal.value(), bit_string_value);
    let texpr = literal.clone().to_texpr();
    assert_eq!(texpr.expression(), &literal.clone().to_expr());
    //    assert_eq!(texpr.get_type(), &Type::Bit(Some(5), IsConst::True));
    assert_eq!(texpr.expression(), &literal.to_expr());
}

//
// Cast
//

#[test]
fn test_cast() {
    use asg::{Cast, IntLiteral};
    use types::{IsConst, Type};

    let typ = Type::Int(Some(32), IsConst::True);
    let literal = IntLiteral::new(1_u64).to_texpr();
    let cast = Cast::new(literal.clone(), typ.clone());
    assert_eq!(cast.get_type(), &typ);
    assert_eq!(cast.operand(), &literal);
}

#[test]
fn test_declaration() {
    //    use asg::{ClassicalDeclaration};
    use symbols::SymbolTable;
    use types::{IsConst, Type};

    let mut table = SymbolTable::new();
    let x = table.new_binding("x", &Type::Bool(IsConst::False));
    assert!(x.is_ok());
    assert_eq!(table.len_current_scope(), 1);
    let result = table.lookup("x");
    assert!(result.is_ok());
    assert_eq!(result.unwrap().symbol_id(), x.unwrap());
}

#[test]
fn test_ident() {
    //    use asg::{Identifier};
    // use symbols::{SymbolTable};
    // let table = SymbolTable::new();
}

#[test]
fn test_binary_expr_add() {
    // use asg::{BinaryExpr, Identifier};
    // use asg::{BinaryOp::*};

    //    let expr = BinaryExpr::new(Add,
}

#[test]
fn test_annotation() {
    let a = asg::Annotation::new("ann".to_string(), Some("details".to_string()));
    let b = asg::Annotation::new("ann".to_string(), None);

    assert_eq!(a.body(), Some("details"));
    assert_eq!(b.body(), None);
}
