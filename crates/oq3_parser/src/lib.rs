// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Parser for OpenQASM 3

mod lexed_str;
mod token_set;

// Temp make pub for debugging
mod event;
mod grammar;
mod input;
mod output;
mod parser;
mod shortcuts;
pub mod syntax_kind;

// FIXME
// #[cfg(test)]
// mod tests;

pub(crate) use token_set::TokenSet;

pub use crate::{
    input::Input,
    lexed_str::LexedStr,
    output::{Output, Step},
    shortcuts::StrStep,
    syntax_kind::SyntaxKind,
};

/// GJL FIXME comments
/// Parse the whole of the input as a given syntactic construct.
/// [`TopEntryPoint::parse`] makes a guarantee that
///   * all input is consumed
///   * the result is a valid tree (there's one root node)
#[derive(Debug)]
pub enum TopEntryPoint {
    SourceFile,
    //    Type,
    Expr,
}

impl TopEntryPoint {
    pub fn parse(&self, input: &Input) -> Output {
        let entry_point: fn(&'_ mut parser::Parser<'_>) = match self {
            TopEntryPoint::SourceFile => grammar::entry::top::source_file,
            //            TopEntryPoint::Type => grammar::entry::top::type_,
            // This entry point is unused.
            TopEntryPoint::Expr => grammar::entry::top::expr,
        };
        let mut p = parser::Parser::new(input);
        entry_point(&mut p);
        // move `events` out of the parser `p`.
        let events = p.finish();
        let res = event::process(events);

        if cfg!(debug_assertions) {
            let mut depth = 0;
            let mut first = true;
            for step in res.iter() {
                assert!(depth > 0 || first);
                first = false;
                match step {
                    Step::Enter { .. } => depth += 1,
                    Step::Exit => depth -= 1,
                    Step::FloatSplit {
                        ends_in_dot: has_pseudo_dot,
                    } => depth -= 1 + !has_pseudo_dot as usize,
                    Step::Token { .. } | Step::Error { .. } => (),
                }
            }
            assert!(!first, "no tree at all");
            assert_eq!(depth, 0, "unbalanced tree");
        }

        res
    }
}
