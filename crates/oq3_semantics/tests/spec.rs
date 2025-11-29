use oq3_semantics::asg;
use oq3_semantics::semantic_error::SemanticErrorList;
use oq3_semantics::symbols::SymbolTable;
use oq3_semantics::syntax_to_semantics::parse_source_string;

// This file tests code in the written OpenQASM 3 spec.
//
// The commit from which the example code was taken is written above each test.
//
// Tests that cause the parser to panic are commented out.
//
// Tests of unimplemented features are marked "Not supported". Note that if possible
// these tests are designed to pass. The

fn parse_string(code: &str) -> (asg::Program, SemanticErrorList, SymbolTable) {
    parse_source_string(code, None).take_context().as_tuple()
}

//
// comments.rst
//

// 17dedf
#[test]
fn test_spec_comments_1() {
    let code = r#"
// A comment line

/*
A comment block
*/
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert!(program.is_empty());
}

// 17dedf
// NOTE! Two problems here. One: In this version of the spec,
// the file is stdgates.qasm rather than the correct stdgates.inc
// Two: This parser panics rather than gracefully recording an error
// if an included file is not found.
// There is a PR open to fix the spec https://github.com/openqasm/openqasm/pull/523
// In the meantime, I took the liberty of changing the code in the spec and testing that
// instead.
// Note that neither of the non-comment lines is translated to a statement, so the
// program is still empty.
#[test]
fn test_spec_comments_2() {
    let code = r#"
// First non-comment is a version string
OPENQASM 3.0;

include "stdgates.inc";

// Rest of QASM program
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert!(program.is_empty());
}

//
// types.rst
//

// 17dedf
#[test]
fn test_spec_types_1() {
    let code = r#"
qubit q0;
qubit q1;
qubit q2;

// and to declare a set of classical variables

int[32] a;
float[32] b = 5.5;
bit[3] c;
bool my_bool = false;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 7);
}

// 17dedf
// Not supported
// Issue #211
// #[test]
// fn test_spec_types_2() {
//     let code = r#"
// // Valid statements

// include "stdgates.inc";

// qubit[5] q1;
// const uint SIZE = 4;
// uint runtime_u = 2;
// qubit[SIZE] q2;  // Declare a 4-qubit register.

// x q1[0];
// z q2[SIZE - 2];  // The index operand is of type `const uint`.

// // Validity is implementation-defined.

// x q1[runtime_u];
// // Indexing with a value with a non-`const` type (`uint`, in this case) is
// // not guaranteed to be supported.
// "#;
//     let (_program, errors, _symbol_table) = parse_string(code);
//     assert_eq!(errors.len(), 0);
// }

// 17dedf
#[test]
fn test_spec_types_3() {
    let code = r#"
// Declare a qubit
qubit gamma;
// Declare a qubit with a Unicode name
qubit γ;
// Declare a qubit register with 20 qubits
qubit[20] qubit_array;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 3);
}

// 17dedf
#[test]
fn test_spec_types_4() {
    let code = r#"
// Not in spec code block
include "stdgates.inc";

// CNOT gate between physical qubits 0 and 1
CX $0, $1;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 1);
}

// 17dedf
// Not supported in the semantic analysis.
// Issue #210
#[test]
fn test_spec_types_5() {
    let code = r#"
defcal h $0 { }
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 1);
    assert_eq!(program.len(), 1);
    let stmt = program.first();
    assert!(matches!(stmt, Some(asg::Stmt::NullStmt)));
}

// 17dedf
#[test]
fn test_spec_types_6() {
    let code = r#"
// Declare a register of 20 bits
bit[20] bit_array;
// Declare and assign a register of bits with decimal value of 15
bit[8] name = "00001111";
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}

// 17dedf
#[test]
fn test_spec_types_7() {
    let code = r#"
// Declare a 32-bit unsigned integer
uint[32] my_uint = 10;
// Declare a 16 bit signed integer
int[16] my_int;
my_int = int[16](my_uint);
// Declare a machine-sized integer
int my_machine_int;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 4);
}

// 17dedf
#[test]
fn test_spec_types_8() {
    let code = r#"
   // Declare a single-precision 32-bit float
   float[32] my_float = π;
   // Declare a machine-precision float.
   float my_machine_float = 2.3;
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
    assert_eq!(program.len(), 2);
}

#[test]
fn test_spec_types_float_leading_dot() {
    let code = r#"
   float my_machine_float = .1e3;
   float w = .2;
   float x = .1234_5678;
   float y = .1e-3;
   float z = .1e+3;
  //
"#;
    let (_program, errors, _symbol_table) = parse_string(code);
    assert!(errors.is_empty());
}
