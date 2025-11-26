// lex: ok
// parse: todo
// sema: skip

def test_array_1(mutable array[uint[16], 4, 2] a) {}
def test_array_2(readonly array[uint[16], 4, 2] a) {}
def test_array_3(mutable array[uint[16], #dim=2] a) {}
def test_array_4(readonly array[uint[16], #dim=2*n] a) {}
def test_array_5(readonly array[int[8], #dim=1] a, mutable array[complex[float[64]], #dim=3] b, readonly array[complex[float[64]], 2, 2] c) -> int[8] {}
