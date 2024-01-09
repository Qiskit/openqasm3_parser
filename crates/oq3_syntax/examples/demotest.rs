// Copyright contributors to the openqasm-parser project

use std::fs;
use std::path::PathBuf;
use clap::{Parser, Subcommand};

use rowan::NodeOrToken; // TODO: this can be accessed from a higher level
use lexer::{tokenize, Token};
use parser::SyntaxKind;
use oq3_syntax::{ast, parse_text, SourceFile, GreenNode};

#[derive(Parser)]
#[command(name = "demotest")]
#[command(about = "Demo of parser that parses and prints tokens or trees to stdout.")]
#[command(long_about = "
Demo of parser that parses and prints tokens or trees to stdout.

Commands are `lex`, `parse`, and `parse-green`.
`lex` prints a stream of tokens. `parse` prints the red tree. `parse-green` prints the green tree.
")]
struct Cli {
    #[command(subcommand)]
    /// This is the Cli command doc
    command: Option<Commands>,
}

// `value_name` expects bare word, not flag.
#[derive(Subcommand)]
enum Commands {
    /// Parse file to `SyntaxNode`
    Parse {
        #[arg(value_name = "FILENAME")]
        /// file name to read
        filename: String,
    },

    /// Parse file to `GreenNode`
    ParseGreen {
        #[arg(value_name = "FILENAME")]
        filename: String,
    },

    /// Lex file to `Token`s
    Lex {
        #[arg(value_name = "FILENAME")]
        filename: String,
    },
}

fn main() {
    use oq3_syntax::ast::HasModuleItem;
    let cli = Cli::parse();

    // You can check for the existence of subcommands, and if found use their
    // matches just as you would the top level cmd
    match &cli.command {
        Some(Commands::Parse { filename }) => {
            let parsed_source = SourceFile::parse(&read_example_source(filename));
            let parse_tree: SourceFile = parsed_source.tree();
            println!("Found {} items", parse_tree.items().collect::<Vec<_>>().len());
            let syntax_errors = parsed_source.errors();
            println!("Found {} parse errors:\n{:?}\n", syntax_errors.len(), syntax_errors);
            print_tree(parse_tree);
        }

        Some(Commands::ParseGreen { filename }) => {
            let (green_node, syntax_errors) = parse_text(&read_example_source(filename));
            println!("{:?}", green_node);
            println!("{:?}", green_node.kind());
            print_node_or_token(green_node, 0);
            println!("\nFound {} parse errors:\n{:?}", syntax_errors.len(), syntax_errors);
        }

        Some(Commands::Lex { filename }) => {
            let tokens: Vec<Token> = tokenize(&read_example_source(filename)).collect();
            for tok in tokens {
                println!("{:?}", tok);
            }
        }
        None => {
            // FIXME should print usage here.
            println!("Commands are parse, parse-green, and lex")
        }
    }
}

/// Construct the fqpn of an example from a filename.
fn example_path(example: &str) -> PathBuf {
    return ["crates", "oq3_syntax", "examples", "oq3_source", example]
        .iter().collect();
}

fn read_example_source(file_name: &str) -> String {
    let file_path = example_path(file_name);
    let contents = fs::read_to_string(file_path.clone())
        .expect(format!("Unable to read file {:?}", file_path).as_str());
    return contents;
}

fn print_tree(file: SourceFile) {
    use ast::{AstNode};
    for item in file.syntax().descendants() {
        println!("{:?}", item);
    }
}

fn print_node_or_token(item: GreenNode, depth: usize) {
    let spcs = " ".repeat(depth);
    for (_i, child) in item.children().enumerate() {
//        println!("{}{}: {} : {:?}", spcs, i, child, child);
        match child {
            NodeOrToken::Node(node) => {
                print_node_or_token(node.to_owned(), depth + 1);
            },
            NodeOrToken::Token(token) => {
                let sk = SyntaxKind::from(token.kind().0);
//                let sk = token.kind().0;
                println!("{}  {:?} {:?}", spcs, sk, token.text());
            },
        };
    }
    println!("{}<", spcs);
}

