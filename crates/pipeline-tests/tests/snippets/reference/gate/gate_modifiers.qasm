// lex: ok
// parse: ok
// sema: ok

qubit q;
gate g q {}
ctrl(2) @ g q;
negctrl(3) @ g q;
pow(-1./2.) @ g q;
inv @ g q;
