// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Lexing, bridging to parser (which does the actual parsing) and
//! incremental reparsing.

use rowan::TextRange;

use crate::{syntax_node::GreenNode, SyntaxError, SyntaxTreeBuilder};

/// This function is sort of obsolete. We can likely replace all uses by `parse_text_check_lex`.
pub fn parse_text(openqasm_code_text: &str) -> (GreenNode, Vec<SyntaxError>) {
    let lexed = oq3_parser::LexedStr::new(openqasm_code_text);
    let parser_input = lexed.to_input();
    let parser_output = oq3_parser::TopEntryPoint::SourceFile.parse(&parser_input);
    let (node, errors, _eof) = build_tree(lexed, parser_output);
    (node, errors)
}

/// Lex `openqasm_code_text`. If there are no lexing errors, parse the result
/// returning the AST as `Option<GreenNode>`, as well as errors.
/// If lexing errors do occur, do no parsing, but rather, return the lexing errors.
pub fn parse_text_check_lex(openqasm_code_text: &str) -> (Option<GreenNode>, Vec<SyntaxError>) {
    let lexed = oq3_parser::LexedStr::new(openqasm_code_text);
    if lexed.errors_len() > 0 {
        return (None, just_errors(lexed));
    }
    let parser_input = lexed.to_input();
    let parser_output = oq3_parser::TopEntryPoint::SourceFile.parse(&parser_input);
    let (node, errors, _eof) = build_tree(lexed, parser_output);
    (Some(node), errors)
}

/// The lexer stores error messages on encountering lexing errors.
/// Here we iterate over the lexing errors,"converting" them to parsing
/// errors: `SyntaxError`. This is for convenience in handling and reporting
/// these errors. We only actually parse the lexed text if there were no lexer
/// errors. So when the semantic analyzer gets the syntax tree, any associated
/// errors are either all from the lexer, or all from the parser.
fn just_errors(lexed: oq3_parser::LexedStr<'_>) -> Vec<SyntaxError> {
    let mut errors = Vec::<SyntaxError>::new();
    for (i, err) in lexed.errors() {
        let text_range = lexed.text_range(i);
        let text_range = TextRange::new(
            text_range.start.try_into().unwrap(),
            text_range.end.try_into().unwrap(),
        );
        errors.push(SyntaxError::new(err, text_range))
    }
    errors
}

pub(crate) fn build_tree(
    lexed: oq3_parser::LexedStr<'_>,
    parser_output: oq3_parser::Output,
) -> (GreenNode, Vec<SyntaxError>, bool) {
    let mut builder = SyntaxTreeBuilder::default();

    let is_eof = lexed.intersperse_trivia(&parser_output, &mut |step| match step {
        oq3_parser::StrStep::Token { kind, text } => builder.token(kind, text),
        oq3_parser::StrStep::Enter { kind } => builder.start_node(kind),
        oq3_parser::StrStep::Exit => builder.finish_node(),
        oq3_parser::StrStep::Error { msg, pos } => {
            builder.error(msg.to_string(), pos.try_into().unwrap())
        }
    });

    let (node, mut errors) = builder.finish_raw();
    for (i, err) in lexed.errors() {
        let text_range = lexed.text_range(i);
        let text_range = TextRange::new(
            text_range.start.try_into().unwrap(),
            text_range.end.try_into().unwrap(),
        );
        errors.push(SyntaxError::new(err, text_range))
    }

    (node, errors, is_eof)
}
