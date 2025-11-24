// lex: ok
// parse: ok
// sema: panic

include "stdlib.qasm";

gate test_gate(theta) a, b {
  reset a;
  barrier b;
  gphase(-theta/2);
  CX a, b;
  barrier;
}
