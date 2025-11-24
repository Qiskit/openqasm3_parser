// lex: ok
// parse: todo
// sema: skip

bit[2] a;
bit[2] b;
creg b[2];
qubit[3] q;
int[10] x = 12;
a[0] = b[1];
x += int[10](a[1]);
measure q[1] -> a[0];
a = measure q[1:2];
measure q[0];
b = a == 0;
