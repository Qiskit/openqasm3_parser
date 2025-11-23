// lex: ok
// parse: todo
// sema: skip

switch (i) {
  case 0 {
    x $0;
  }
  case 1, 2 {
    x $0;
    z $1;
  }
  case 3, {
  }
  default {
    cx $0, $1;
  }
}

switch (i + j) {
  default {
    switch (2 * k) {
      case 0 {
        x $0;
      }
      default {
        z $0;
      }
    }
  }
}
