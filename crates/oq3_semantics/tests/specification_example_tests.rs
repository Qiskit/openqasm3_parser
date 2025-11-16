// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_semantics::asg;
use oq3_semantics::semantic_error::SemanticErrorList;
use oq3_semantics::symbols::SymbolTable;
use oq3_semantics::syntax_to_semantics::parse_source_string;
//use oq3_semantics::types::{ArrayDims, IsConst, Type};

fn parse_string(code: &str) -> (asg::Program, SemanticErrorList, SymbolTable) {
    parse_source_string(code, None).take_context().as_tuple()
}

#[test]
fn test_subroutines_1() {
    let code = r#"
include "stdgates.inc";
def xmeasure(qubit q) -> bit { h q; return measure q; }
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_subroutines_2() {
    let code = r#"
    include "stdgates.inc";
    def pmeasure(angle[32] theta, qubit q) -> bit {
      rz(theta) q;
      h q;
      return measure q;
   }
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}

#[test]
fn test_subroutines_3() {
    let code = r#"
   include "stdgates.inc";
   def xcheck(qubit[4] d, qubit a) -> bit {
     reset a;
     for int i in [0: 3] cx d[i], a;
     return measure a;
   }
"#;
    let (program, errors, _symbol_table) = parse_string(code);
    assert_eq!(errors.len(), 0);
    assert_eq!(program.len(), 1);
}
