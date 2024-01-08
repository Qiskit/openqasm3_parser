// Copyright contributors to the openqasm-parser project

//! Lexing `&str` into a sequence of OpenQASM 3 tokens.
//!
//! Note that `str` does *not* refer to a string in the target language.
//! Note that strictly speaking the parser in this crate is not required to work
//! on tokens which originated from text. Macros, eg, can synthesize tokens out
//! of thin air. So, ideally, lexer should be an orthogonal crate. It is however
//! convenient to include a text-based lexer here!
//!
//! Note that these tokens, unlike the tokens we feed into the parser, do
//! include info about comments and whitespace.

use std::ops;

use crate::{
    SyntaxKind::{self, *},
    T,
};

pub struct LexedStr<'a> {
    text: &'a str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexError>,
}

struct LexError {
    msg: String,
    token: u32,
}

impl<'a> LexedStr<'a> {
    pub fn new(text: &'a str) -> LexedStr<'a> {
        let mut conv = Converter::new(text);

        for token in lexer::tokenize(&text[conv.offset..]) {
            let token_text = &text[conv.offset..][..token.len as usize];
            conv.extend_token(&token.kind, token_text);
        }

        conv.finalize_with_eof()
    }

    pub fn as_str(&self) -> &str {
        self.text
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self, i: usize) -> SyntaxKind {
        assert!(i < self.len());
        self.kind[i]
    }

    pub fn text(&self, i: usize) -> &str {
        self.range_text(i..i + 1)
    }

    // The text corresponding to a range.
    pub fn range_text(&self, r: ops::Range<usize>) -> &str {
        assert!(r.start < r.end && r.end <= self.len());
        let lo = self.start[r.start] as usize;
        let hi = self.start[r.end] as usize;
        &self.text[lo..hi]
    }

    // A range corresponding to piece of text.
    // Naming is hard.
    pub fn text_range(&self, i: usize) -> ops::Range<usize> {
        assert!(i < self.len());
        let lo = self.start[i] as usize;
        let hi = self.start[i + 1] as usize;
        lo..hi
    }
    pub fn text_start(&self, i: usize) -> usize {
        assert!(i <= self.len());
        self.start[i] as usize
    }
    pub fn text_len(&self, i: usize) -> usize {
        assert!(i < self.len());
        let r = self.text_range(i);
        r.end - r.start
    }

    pub fn error(&self, i: usize) -> Option<&str> {
        assert!(i < self.len());
        let err = self.error.binary_search_by_key(&(i as u32), |i| i.token).ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error.iter().map(|it| (it.token as usize, it.msg.as_str()))
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kind.push(kind);
        self.start.push(offset as u32);
    }
}

struct Converter<'a> {
    res: LexedStr<'a>,
    offset: usize,
}

impl<'a> Converter<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            res: LexedStr { text, kind: Vec::new(), start: Vec::new(), error: Vec::new() },
            offset: 0,
        }
    }

    fn finalize_with_eof(mut self) -> LexedStr<'a> {
        self.res.push(EOF, self.offset);
        self.res
    }

    fn push(&mut self, kind: SyntaxKind, len: usize, err: Option<&str>) {
        self.res.push(kind, self.offset);
        self.offset += len;

        if let Some(err) = err {
            let token = self.res.len() as u32;
            let msg = err.to_string();
            self.res.error.push(LexError { msg, token });
        }
    }


    fn extend_token(&mut self, kind: &lexer::TokenKind, token_text: &str) {
        let (err, syntax_kind, text_len) = inner_extend_token(kind, token_text);
        let err = if err.is_empty() { None } else { Some(err) };
        self.push(syntax_kind, text_len, err);
    }

    }

fn extend_literal_func(len: usize, kind: &lexer::LiteralKind) ->
    (&str, SyntaxKind, usize) {
        let mut err = "";
        let syntax_kind = match *kind {
            lexer::LiteralKind::Int { empty_int, base: _ } => {
                if empty_int {
                    err = "Missing digits after the integer base prefix";
                }
                INT_NUMBER
            }
            lexer::LiteralKind::Float { empty_exponent, base: _ } => {
                if empty_exponent {
                    err = "Missing digits after the exponent symbol";
                }
                FLOAT_NUMBER
            }
            lexer::LiteralKind::TimingInt{empty_int, base} => {
                if empty_int {
                    err = "Missing digits after the integer base prefix";
                }
                if base != lexer::Base::Decimal {
                    err = "Base of timing integer literal is not decimal";
                }
                TIMING_INT_NUMBER
            }
            lexer::LiteralKind::TimingFloat{empty_exponent, base} => {
                if empty_exponent {
                    err = "Missing digits after the exponent symbol";
                }
                if base != lexer::Base::Decimal {
                    err = "Base of timing integer literal is not decimal";
                }
                TIMING_FLOAT_NUMBER
            }
            lexer::LiteralKind::SimpleFloat => {
                SIMPLE_FLOAT_NUMBER
            }
            lexer::LiteralKind::Byte { terminated } => {
                if !terminated {
                    err = "Missing trailing `'` symbol to terminate the byte literal";
                }
                BYTE
            }
            lexer::LiteralKind::Str { terminated } => {
                if !terminated {
                    err = "Missing trailing `\"` symbol to terminate the string literal";
                }
                STRING
            }
            lexer::LiteralKind::BitStr { terminated, consecutive_underscores } => {
                // FIXME. Both errors at once are possible but not handled.
                if !terminated {
                    if !consecutive_underscores {
                        err = "Missing trailing `\"` symbol to terminate the bitstring literal";
                    }
                } else if consecutive_underscores {
                    err = "Consecutive underscores not allowed in bitstring literal";

                }
                BIT_STRING
            }
        };
        (err, syntax_kind, len)
    }

fn inner_extend_token<'a>(kind: &'a lexer::TokenKind, token_text: &str) ->
    (&'a str, SyntaxKind, usize)
{
    // A note on an intended tradeoff:
    // We drop some useful information here (see patterns with double dots `..`)
    // Storing that info in `SyntaxKind` is not possible due to its layout requirements of
    // being `u16` that come from `rowan::SyntaxKind`.
    let mut err = "";

    let syntax_kind = {
        match kind {
            lexer::TokenKind::LineComment => COMMENT,
            lexer::TokenKind::BlockComment { terminated } => {
                if !terminated {
                    err = "Missing trailing `*/` symbols to terminate the block comment";
                }
                COMMENT
            }

            lexer::TokenKind::Whitespace => WHITESPACE,
            lexer::TokenKind::Ident if token_text == "_" => UNDERSCORE,

            // If it looks like an identifer, look first if it is a keyword.
            lexer::TokenKind::Ident => {
                SyntaxKind::from_keyword(token_text).unwrap_or(SyntaxKind::from_scalar_type(token_text).unwrap_or(IDENT))
            }

            // um, this does not look correct
            lexer::TokenKind::HardwareIdent => {
                SyntaxKind::from_keyword(token_text).unwrap_or(HARDWAREIDENT)
            }

            lexer::TokenKind::InvalidIdent => {
                err = "Ident contains invalid characters";
                IDENT
            }

            lexer::TokenKind::Literal { kind, .. } => {
//                    self.extend_literal(token_text.len(), kind);
                return extend_literal_func(token_text.len(), kind)
            }

            lexer::TokenKind::Semi => T![;],
            lexer::TokenKind::Comma => T![,],
            lexer::TokenKind::Dot => T![.],
            lexer::TokenKind::OpenParen => T!['('],
            lexer::TokenKind::CloseParen => T![')'],
            lexer::TokenKind::OpenBrace => T!['{'],
            lexer::TokenKind::CloseBrace => T!['}'],
            lexer::TokenKind::OpenBracket => T!['['],
            lexer::TokenKind::CloseBracket => T![']'],
            lexer::TokenKind::At => T![@],
            lexer::TokenKind::Pound => T![#],
            lexer::TokenKind::Tilde => T![~],
            lexer::TokenKind::Question => T![?],
            lexer::TokenKind::Colon => T![:],
            lexer::TokenKind::Dollar => T![$],
            lexer::TokenKind::Eq => T![=],
            lexer::TokenKind::Bang => T![!],
            lexer::TokenKind::Lt => T![<],
            lexer::TokenKind::Gt => T![>],
            lexer::TokenKind::Minus => T![-],
            lexer::TokenKind::And => T![&],
            lexer::TokenKind::Or => T![|],
            lexer::TokenKind::Plus => T![+],
            lexer::TokenKind::Star => T![*],
            lexer::TokenKind::Slash => T![/],
            lexer::TokenKind::Caret => T![^],
            lexer::TokenKind::Percent => T![%],
            lexer::TokenKind::Unknown => ERROR,
            lexer::TokenKind::Eof => EOF,
        }
    };
    (err, syntax_kind, token_text.len())
}
