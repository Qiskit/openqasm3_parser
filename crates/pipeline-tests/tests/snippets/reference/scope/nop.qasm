// lex: ok
// parse: todo
// sema: skip

nop;
nop $0;
nop $1, $2;
nop q, q[0],;
box {
  nop $0;
}
gate x q {
  nop q;
}
