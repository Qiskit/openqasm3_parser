// lex: ok
// parse: ok
// sema: ok

include "stdgates.inc";

int i = 1;
int j = 2;
int k = 3;

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
