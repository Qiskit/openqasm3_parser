// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::types;

//
// Types
//

#[test]
fn test_int_type_const() {
    use types::{IsConst, Type};

    let typ = Type::Int(Some(32), IsConst::True);
    assert_eq!(typ.width(), Some(32));
    assert_eq!(typ.is_scalar(), true);
    assert_eq!(typ.is_const(), true);
    assert_eq!(typ.is_quantum(), false);
}

#[test]
fn test_int_type_not_const() {
    use types::{IsConst, Type};

    let typ = Type::Int(Some(32), IsConst::False);
    assert_eq!(typ.width(), Some(32));
    assert_eq!(typ.is_scalar(), true);
    assert_eq!(typ.is_const(), false);
    assert_eq!(typ.is_quantum(), false);
}

#[test]
fn test_int_type_no_width() {
    use types::{IsConst, Type};

    let typ = Type::Int(None, IsConst::False); // No width
    assert!(typ.width().is_none());
    assert_eq!(typ.is_scalar(), true);
    assert_eq!(typ.is_const(), false);
    assert_eq!(typ.is_quantum(), false);
}

#[test]
fn test_qubit_type_single_qubit() {
    use types::Type;

    let typ = Type::Qubit;
    assert!(typ.width().is_none());
    assert_eq!(typ.is_scalar(), false);
    assert_eq!(typ.is_const(), true);
    assert_eq!(typ.is_quantum(), true);
}

// #[test]
// fn test_qubit_type_qubit_register() {
//     use types::{Type};

//     let width = 100;
//     let typ = Type::Qubit(Some(width));
//     assert_eq!(typ.width(), Some(width));
//     assert_eq!(typ.is_scalar(), false);
//     assert_eq!(typ.is_const(), false);
//     assert_eq!(typ.is_quantum(), true);
// }
