// lex: ok
// parse: ok
// sema: panic

include "stdgates.inc";
if (x == a) {
  for uint i in [0:2:4] x[i] += 1;
}
else CX x[0], x[1];
