// lex: ok
// parse: ok
// sema: panic

while (i < 10) {
  for uint j in {1, 4, 6} reset q[j];
  if (i == 8) break;
  else continue;
}
