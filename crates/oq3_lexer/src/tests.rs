// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use super::*;

use expect_test::{expect, Expect};
use std::fmt::Write;

fn check_lexing(src: &str, expect: Expect) {
    let actual: String = tokenize(src).fold(String::new(), |mut output, token| {
        let _ = writeln!(output, "{token:?}");
        output
    });
    expect.assert_eq(&actual)
}

#[test]
fn smoke_test() {
    check_lexing(
        "/* my source file */ fn main() { println!(\"zebra\"); }\n",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 20 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: OpenBrace, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 7 }
            Token { kind: Bang, len: 1 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 7 }, len: 7 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Semi, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: CloseBrace, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn comment_flavors() {
    check_lexing(
        r"
// line
//// line as well
/// outer doc line
//! inner doc line
/* block */
/**/
/*** also block */
/** outer doc block */
/*! inner doc block */
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 17 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 22 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 22 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn nested_block_comments() {
    check_lexing(
        r#"/* /* */ */"a""#,
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 3 }, len: 3 }
        "#]],
    )
}

// Why does this fail? GJL
// #[test]
// fn characters() {
//     check_lexing(
//         "'a' ' ' '\\n'",
//         expect![[r#"
//             Token { kind: Literal { kind: Char { terminated: true }, suffix_start: 3 }, len: 3 }
//             Token { kind: Whitespace, len: 1 }
//             Token { kind: Literal { kind: Char { terminated: true }, suffix_start: 3 }, len: 3 }
//             Token { kind: Whitespace, len: 1 }
//             Token { kind: Literal { kind: Char { terminated: true }, suffix_start: 4 }, len: 4 }
//         "#]],
//     );
// }

#[test]
fn literal_suffixes() {
    check_lexing(
        r#"
"a"
1234
0b101
0xABC
1.0
1.0e10
2us
3ns
4ms
6dt
5s
6dta
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 3 }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 4 }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Binary, empty_int: false }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Hexadecimal, empty_int: false }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false }, suffix_start: 3 }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false }, suffix_start: 6 }, len: 6 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false }, suffix_start: 1 }, len: 1 }
            Token { kind: Ident, len: 3 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}
