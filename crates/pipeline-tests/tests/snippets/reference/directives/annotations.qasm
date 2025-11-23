// lex: ok
// parse: ok
// sema: diag

@bind [2:3]
input uint[16] x;

@rename other
output float[64] var;

@hello world
int[8] x;

@outer
def fn() {
  @inner word1
  uint[16] x;
  @inner word2
  return;
}

@first
@second @not_third
uint[16] x;

@binds tightly
x = 1; x = 2;
