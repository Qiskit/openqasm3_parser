// lex: ok
// parse: todo
// sema: todo

def test_sub1(int[5] i, qubit[2] q1, qreg q2[5]) -> int[10] {
  int[10] result;
  if (result == 2) return 1 + result;
  return result;
}
def test_sub2(int[5] i, bit[2] b, creg c[3]) {
  for int[5] j in {2, 3}
    i += j;
  return i+1;
}
def returns_a_measure(qubit q) {
  return measure q;
}
