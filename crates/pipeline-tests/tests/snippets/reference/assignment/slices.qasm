// lex: ok
// parse: ok
// sema: skip

array[uint[16], 2, 2] a = {{1, 2}, {3, 4}};
array[uint[16], 2, 4] b = {{1, 2, 3, 4}, {5, 6, 7, 8}};
// Various forms of testing that assignments can be made to indexed
// identifiers, and from indexed identifiers.
a = b[0:1][0:1];
a[0:1] = b[0:1][0:1];
a[0] = b[0][0:1];
a[0][0] = b[0][0];
a[0][0:1] = b[1][1:2:3];
a[0:1][0] = b[0:1][0];
a[0:1][0:1] = b[0:1][0:1];
