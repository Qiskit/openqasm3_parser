// lex: ok
// parse: todo
// sema: todo

bit[2] a;
creg b[2];
qubit[5] q1;
qreg q2[7];
let q = q1 ++ q2;
let c = a[{0,1}] ++ b[1:2];
let qq = q1[{1,3,4}];
let qqq = qq ++ q2[1:2:6];
let d = c;
let e = d[1];
