// lex: ok
// parse: ok
// sema: panic

// beware the tab character in dur4 = 8 ns below
  int[10] x;
  int[10] y;
  uint[32] z = 0xFa_1F;
  uint[32] z = 0XFa_1F;
  uint[16] z = 0o12_34;
  uint[16] z = 0b1001_1001;
  uint[16] z = 0B1001_1001;
  uint x;
  qubit[6] q1;
  qubit q2;
  bit[4] b1="0100";
  bit[8] b2="1001_0100";
  bit b2 = "1";
  bool m=true;
  bool n=bool(b2);
  bool o=false;
  const float[64] c = 5.5e3;
  const float[64] d=5;
  float[32] f = .1e+3;
  duration dur = 1000dt;
  duration dur2 = dur + 200ns;
  duration dur3 = 10 ms;
  duration dur4 = 8	us;
  duration dur5 = 1s;
  stretch s;
