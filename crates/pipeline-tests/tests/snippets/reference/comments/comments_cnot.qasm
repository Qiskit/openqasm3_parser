// lex: ok
// parse: ok
// sema: diag

OPENQASM 3.0;
// Line comment before include
include "stdgates.inc"; // Inline comment
/* Block comment before declaration */
qubit[2] q; /* Inline block comment */

// Comment before gate
h q[0]; // Gate with comment
/* Multi-line block comment
   spanning multiple lines */
cnot q[0], q[1];
