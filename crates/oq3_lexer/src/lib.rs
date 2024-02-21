// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Low-level OpenQASM3 lexer.
//!
//! This is largely copied from `rustc_lexer`, the lexer for the rust compiler.
//! `rustc_lexer` is avaiable as a crate, and rust-analyzer uses this crate for lexing.
//!
//! The main entity of this crate is the [`TokenKind`] enum which represents common
//! lexeme types.
#![deny(rustc::untranslatable_diagnostic)]
#![deny(rustc::diagnostic_outside_of_impl)]

mod cursor;
pub mod unescape;

#[cfg(test)]
mod tests;

pub use crate::cursor::Cursor;

use self::LiteralKind::*;
use self::TokenKind::*;
// Will likely need this:
// use crate::cursor::EOF_CHAR;
use unicode_properties::UnicodeEmoji;

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    fn new(kind: TokenKind, len: u32) -> Token {
        Token { kind, len }
    }
}

/// Enum representing common lexeme types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "// comment"
    LineComment,

    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment {
        terminated: bool,
    },

    /// Any whitespace character sequence.
    Whitespace,

    //    ClassicalTypeName,
    /// "ident" or "continue"
    ///
    /// At this step, keywords are also considered identifiers.
    Ident,

    HardwareIdent, // OQ3

    /// Like the above, but containing invalid unicode codepoints.
    InvalidIdent,

    Pragma,

    Annotation,

    /// Needed for OpenQASM 3 ?
    /// An unknown prefix, like `foo#`, `foo'`, `foo"`.
    ///
    /// Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token). In Rust 2021 and later, reserved
    /// prefixes are reported as errors; in earlier editions, they result in a
    /// (allowed by default) lint, and are treated as regular identifier
    /// tokens.
    /// UnknownPrefix,

    /// Examples: `1.0e-40`, `1000dt`, `1000ms`. Here `dt` and `ms` are suffixes.
    /// OpenQASM3 relevance? -> Note that `_` is an invalid
    /// suffix, but may be present here on string and float literals. Users of
    /// this type will need to check for and reject that case.
    ///
    /// See [LiteralKind] for more details.
    Literal {
        kind: LiteralKind,
        suffix_start: u32,
    },

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "#"
    Pound,
    /// "~"
    Tilde,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "&"
    And,
    /// "|"
    Or,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "^"
    Caret,
    /// "%"
    Percent,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

/// Enum representing the literal types supported by the lexer.
///
/// Note that the suffix is *not* considered when deciding the `LiteralKind` in
/// this type. This means that float literals like `1f32` are classified by this
/// type as `Int`. (Compare against `rustc_ast::token::LitKind` and
/// `rustc_ast::ast::LitKind`).
///
/// Following this same idea, timing literals are also just Int or Float at this stage.
/// But the length of the suffix will be used later to classify them. (GJL Aug 2023)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// "12_u8", "0o100", "0b120i99", "1f32".
    Int {
        base: Base,
        empty_int: bool,
    },
    /// "12.34f32", "1e3", but not "1f32".
    Float {
        base: Base,
        empty_exponent: bool,
    },
    /// "b'a'", "b'\\'", "b'''", "b';"
    Byte {
        terminated: bool,
    },
    /// ""abc"", ""abc"
    Str {
        terminated: bool,
    },
    /// "10011" "100_11"
    BitStr {
        terminated: bool,
        consecutive_underscores: bool,
    },
    /// Int Timing literal
    TimingInt {
        base: Base,
        empty_int: bool,
    },
    /// Float Timing literal
    TimingFloat {
        base: Base,
        empty_exponent: bool,
    },
    SimpleFloat,
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0b".
    Binary = 2,
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0x".
    Hexadecimal = 16,
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    //    c == '_' || c == '$' || unicode_xid::UnicodeXID::is_xid_start(c)
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

// GJL OQ3
// pub fn is_hardware_id_start(c: char) -> bool {
//     // This is XID_Start OR '_' (which formally is not a XID_Start).
// //    c == '_' || c == '$' || unicode_xid::UnicodeXID::is_xid_start(c)
//     c == '$'
// }

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

/// The passed string is lexically an identifier.
pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Token {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };
        let token_kind = match first_char {
            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_unknown_prefix(),

            // Numeric literal.
            c @ '0'..='9' => {
                // n.b. floating point literals must have an integer part. e.g. .123 is not valid
                let literal_kind = self.number(c);
                let suffix_start = self.pos_within_token();
                // Eat suffix, and return true if it is a timing suffix.
                if self.timing_suffix() {
                    match literal_kind {
                        Float {
                            base: baseval,
                            empty_exponent: emptyval,
                        } => TokenKind::Literal {
                            kind: TimingFloat {
                                base: baseval,
                                empty_exponent: emptyval,
                            },
                            suffix_start,
                        },
                        Int {
                            base: baseval,
                            empty_int: emptyval,
                        } => TokenKind::Literal {
                            kind: TimingInt {
                                base: baseval,
                                empty_int: emptyval,
                            },
                            suffix_start,
                        },
                        _ => {
                            // This is unreachable
                            TokenKind::Literal {
                                kind: literal_kind,
                                suffix_start,
                            }
                        }
                    }
                } else {
                    TokenKind::Literal {
                        kind: literal_kind,
                        suffix_start,
                    }
                }
            }

            '#' => {
                if self.first() == 'p' {
                    self.bump();
                    if self.first() == 'r' {
                        self.bump();
                        if self.first() == 'a' {
                            self.bump();
                            if self.first() == 'g' {
                                self.bump();
                                if self.first() == 'm' {
                                    self.bump();
                                    if self.first() == 'a' {
                                        self.eat_while(|c| c != '\n');
                                        let res = Token::new(Pragma, self.pos_within_token());
                                        self.reset_pos_within_token();
                                        return res;
                                    }
                                }
                            }
                        }
                    }
                }
                InvalidIdent
            }

            '@' => {
                if is_id_start(self.first()) {
                    self.eat_while(|c| c != '\n');
                    Annotation
                } else {
                    At
                }
            }

            '$' => self.hardware_ident(),
            // One-symbol tokens.
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '=' => Eq,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '&' => And,
            '|' => Or,
            '+' => Plus,
            '*' => Star,
            '^' => Caret,
            '%' => Percent,

            // OQ3 unfortunately follows Python in allowing either ' or " for the same kind of string
            // Lifetime or character literal.
            // '\'' => self.lifetime_or_char(),

            // String literal.
            '"' => {
                let (terminated, only_ones_and_zeros, consecutive_underscores) =
                    self.double_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_literal_suffix();
                }
                let kind = match only_ones_and_zeros {
                    true => BitStr {
                        terminated,
                        consecutive_underscores,
                    },
                    false => Str { terminated },
                };
                Literal { kind, suffix_start }
            }
            // OQ3 can error on this as well.
            // Identifier starting with an emoji. Only lexed for graceful error recovery.
            c if !c.is_ascii() && c.is_emoji_char() => self.fake_ident_or_unknown_prefix(),
            _ => Unknown,
        };
        let res = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        res
    }

    fn line_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '/');
        self.bump();

        self.eat_while(|c| c != '\n');
        LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();

        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                _ => (),
            }
        }

        BlockComment {
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Whitespace
    }

    fn ident_or_unknown_prefix(&mut self) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));

        if self.prev() == 'p' && self.first() == 'r' {
            self.bump();
            if self.first() == 'a' {
                self.bump();
                if self.first() == 'g' {
                    self.bump();
                    if self.first() == 'm' {
                        self.bump();
                        if self.first() == 'a' {
                            self.eat_while(|c| c != '\n');
                            return Pragma;
                        }
                    }
                }
            }
        }

        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.

        match self.first() {
            // '#' | '"' | '\'' => UnknownPrefix,
            c if !c.is_ascii() && c.is_emoji_char() => self.fake_ident_or_unknown_prefix(),
            _ => Ident,
        }
    }

    fn hardware_ident(&mut self) -> TokenKind {
        // debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        match self.first() {
            c if !c.is_ascii() && c.is_emoji_char() => {
                self.eat_while(is_id_continue);
                self.fake_ident_or_unknown_prefix()
            }
            _ => {
                if !self.eat_decimal_digits() {
                    Dollar
                } else {
                    HardwareIdent
                }
            }
        }
    }

    // FIXME. I think this is r-a cruft.
    fn fake_ident_or_unknown_prefix(&mut self) -> TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c)
                || (!c.is_ascii() && c.is_emoji_char())
                || c == '\u{200d}'
        });
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        InvalidIdent
        // match self.first() {
        //     // '#' | '"' | '\'' => UnknownPrefix,
        //     _ => InvalidIdent,
        // }
    }

    // In OQ3, whitespace *is* allowed between an imaginary literal and `im`. So to be consistent, these
    // must each be a separate token. Furthermore, if we continue with the way this lexer works for rust,
    // `im` will not be recognized as reserved at this stage, just as `for`, `if`, etc. are not. The distinction
    // between ordinary identifiers and keywords is made first at the level of the parser.
    //
    fn number(&mut self, first_digit: char) -> LiteralKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    if !self.eat_hexadecimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                // Not a base prefix; consume additional digits.
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                // Also not a base prefix; nothing more to do here.
                '.' | 'e' | 'E' => {}

                // Just a 0.
                _ => {
                    return Int {
                        base,
                        empty_int: false,
                    }
                }
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        };

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            // '.' if self.second() != '.' && !is_id_start(self.second()) => {
            //     // might have stuff after the ., and if it does, it needs to start
            //     // with a number
            // Above applies in Rust, not OQ3
            '.' => {
                self.bump();
                // n.b. example of `empty_exponent` : 3.4e; This is a syntax error
                let mut empty_exponent = false;
                // preferred to is_digit(10), in rust lexer
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                Float {
                    base,
                    empty_exponent,
                }
            }
            _ => Int {
                base,
                empty_int: false,
            },
        }
    }

    // OQ3 will use this. But comment out for now.
    // Or, may not hand single-quoted strings this way
    // fn single_quoted_string(&mut self) -> bool {
    //     debug_assert!(self.prev() == '\'');
    //     // Check if it's a one-symbol literal.
    //     if self.second() == '\'' && self.first() != '\\' {
    //         self.bump();
    //         self.bump();
    //         return true;
    //     }

    //     // Literal has more than one symbol.

    //     // Parse until either quotes are terminated or error is detected.
    //     loop {
    //         match self.first() {
    //             // Quotes are terminated, finish parsing.
    //             '\'' => {
    //                 self.bump();
    //                 return true;
    //             }
    //             // Probably beginning of the comment, which we don't want to include
    //             // to the error report.
    //             '/' => break,
    //             // Newline without following '\'' means unclosed quote, stop parsing.
    //             '\n' if self.second() != '\'' => break,
    //             // FIXME: EOF_CHAR became unused after some edit. why?
    //             // End of file, stop parsing.
    //             _EOF_CHAR if self.is_eof() => break,
    //             // Escaped slash is considered one character, so bump twice.
    //             '\\' => {
    //                 self.bump();
    //                 self.bump();
    //             }
    //             // Skip the character.
    //             _ => {
    //                 self.bump();
    //             }
    //         }
    //     }
    //     // String was not terminated.
    //     false
    // }

    /// Eats double-quoted string and returns (terminated, only_ones_and_zeros)
    /// terminated - true if the string is terminated (with double quote)
    /// only_ones_and_zeros - true if string is a bit string literal
    /// Note that only_ones_and_zeros allows underscores.
    fn double_quoted_string(&mut self) -> (bool, bool, bool) {
        debug_assert!(self.prev() == '"');
        let mut only_ones_and_zeros = true;
        let mut terminated = false;
        let mut consecutive_underscores = false;
        let mut count_newlines = 0;
        let mut prev_char = '\0';
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    terminated = true;
                    if count_newlines > 0 {
                        only_ones_and_zeros = false;
                    }
                    return (terminated, only_ones_and_zeros, consecutive_underscores);
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character.
                    only_ones_and_zeros = false;
                    self.bump();
                }
                '\n' => {
                    count_newlines += 1;
                    if count_newlines > 1 {
                        only_ones_and_zeros = false;
                    }
                }
                '_' => {
                    if prev_char == '_' {
                        consecutive_underscores = true;
                    }
                }
                '0' | '1' => (),
                _ => {
                    only_ones_and_zeros = false;
                }
            }
            prev_char = c;
        }
        // This will skip the case that an unterminated bitstring is the last
        // characters in a file. Probably not too common.
        if count_newlines > 0 && !(count_newlines == 1 && prev_char == '\n') {
            only_ones_and_zeros = false;
        }
        // End of file reached.
        (terminated, only_ones_and_zeros, consecutive_underscores)
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }

    // Eats the suffix of the literal, e.g. "u8".
    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }

    // Eat a timing suffix if found and returns true.
    // Otherwise eat suffix if present and return false.
    fn timing_suffix(&mut self) -> bool {
        let mut timing = false;
        if self.first() == 's' {
            self.bump();
            timing = true;
        } else {
            // TODO: greek mu is encoded in more than one way. We only get one here.
            for (f, s) in [('d', 't'), ('n', 's'), ('u', 's'), ('m', 's'), ('µ', 's')] {
                if self.first() == f && self.second() == s {
                    self.bump();
                    self.bump();
                    timing = true;
                }
            }
        }
        if timing {
            if is_id_continue(self.first()) {
                self.eat_while(is_id_continue);
                return false;
            }
            return true;
        }
        self.eat_literal_suffix();
        false
    }

    // Eats the identifier. Note: succeeds on `_`, which isn't a valid
    // identifier.
    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_id_continue);
    }
}
