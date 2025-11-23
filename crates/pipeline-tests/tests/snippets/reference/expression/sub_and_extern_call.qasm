// lex: ok
// parse: ok
// sema: panic

bit x = sub_call(10, "01", q1[0], q2);
int[2] y = extern_call(0.5, 10dt);
ambiguous_call(pi);
