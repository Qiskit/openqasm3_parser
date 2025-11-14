#[cfg(feature = "sourcegen")]
mod sourcegen {

    use std::fs;

    use std::{
        collections::{BTreeSet, HashSet},
        fmt::Write,
    };

    use itertools::Itertools;
    use proc_macro2::{Ident, Punct, Spacing, Span};
    use quote::{format_ident, quote};
    use ungrammar::{Grammar, Rule};

    use std::{
        fmt, mem,
        path::{Path, PathBuf},
    };

    use xshell::{cmd, Shell};

    // I think list_rust_files and list_files are only used to support
    // extracting tests from comments.

    #[allow(unused)]
    pub fn list_rust_files(dir: &Path) -> Vec<PathBuf> {
        let mut res = list_files(dir);
        res.retain(|it| {
            it.file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .ends_with(".rs")
        });
        res
    }

    #[allow(unused)]
    pub fn list_files(dir: &Path) -> Vec<PathBuf> {
        let mut res = Vec::new();
        let mut work = vec![dir.to_path_buf()];
        while let Some(dir) = work.pop() {
            for entry in dir.read_dir().unwrap() {
                let entry = entry.unwrap();
                let file_type = entry.file_type().unwrap();
                let path = entry.path();
                let is_hidden = path
                    .file_name()
                    .unwrap_or_default()
                    .to_str()
                    .unwrap_or_default()
                    .starts_with('.');
                if !is_hidden {
                    if file_type.is_dir() {
                        work.push(path);
                    } else if file_type.is_file() {
                        res.push(path);
                    }
                }
            }
        }
        res
    }

    #[derive(Clone)]
    pub struct CommentBlock {
        pub id: String,
        pub line: usize,
        pub contents: Vec<String>,
        is_doc: bool,
    }

    impl CommentBlock {
        #[allow(unused)]
        pub fn extract(tag: &str, text: &str) -> Vec<CommentBlock> {
            assert!(tag.starts_with(char::is_uppercase));

            let tag = format!("{tag}:");
            let mut blocks = CommentBlock::extract_untagged(text);
            blocks.retain_mut(|block| {
                let first = block.contents.remove(0);
                let Some(id) = first.strip_prefix(&tag) else {
                    return false;
                };

                if block.is_doc {
                    panic!("Use plain (non-doc) comments with tags like {tag}:\n    {first}");
                }

                block.id = id.trim().to_string();
                true
            });
            blocks
        }

        #[allow(unused)]
        pub fn extract_untagged(text: &str) -> Vec<CommentBlock> {
            let mut res = Vec::new();

            let lines = text.lines().map(str::trim_start);

            let dummy_block = CommentBlock {
                id: String::new(),
                line: 0,
                contents: Vec::new(),
                is_doc: false,
            };
            let mut block = dummy_block.clone();
            for (line_num, line) in lines.enumerate() {
                match line.strip_prefix("//") {
                    Some(mut contents) => {
                        if let Some('/' | '!') = contents.chars().next() {
                            contents = &contents[1..];
                            block.is_doc = true;
                        }
                        if let Some(' ') = contents.chars().next() {
                            contents = &contents[1..];
                        }
                        block.contents.push(contents.to_string());
                    }
                    None => {
                        if !block.contents.is_empty() {
                            let block = mem::replace(&mut block, dummy_block.clone());
                            res.push(block);
                        }
                        block.line = line_num + 2;
                    }
                }
            }
            if !block.contents.is_empty() {
                res.push(block);
            }
            res
        }
    }

    #[derive(Debug)]
    pub struct Location {
        pub file: PathBuf,
        pub line: usize,
    }

    // FIXME: URL for r-a is hardcoded here.
    // My guess is:
    // This is meant to display the location in the code of a test failure.
    // The tests are extracted and written and run elsewhere, so the location in
    // source is recorded so that the dev can find them.
    impl fmt::Display for Location {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let path = self
                .file
                .strip_prefix(project_root())
                .unwrap()
                .display()
                .to_string();
            let path = path.replace('\\', "/");
            let name = self.file.file_name().unwrap();
            write!(
                f,
                "https://github.com/rust-lang/rust-analyzer/blob/master/{}#L{}[{}]",
                path,
                self.line,
                name.to_str().unwrap()
            )
        }
    }

    #[allow(unused)]
    fn ensure_rustfmt(sh: &Shell) {
        let version = cmd!(sh, "rustup run stable rustfmt --version")
            .read()
            .unwrap_or_default();
        if !version.contains("stable") {
            panic!(
                "Failed to run rustfmt from toolchain 'stable'. \
                    Please run `rustup component add rustfmt --toolchain stable` to install it.",
            );
        }
    }

    #[allow(unused)]
    pub fn reformat(text: String) -> String {
        let sh = Shell::new().unwrap();
        ensure_rustfmt(&sh);
        let rustfmt_toml = project_root().join("rustfmt.toml");
        let mut stdout = cmd!(
            sh,
            "rustup run stable rustfmt --config-path {rustfmt_toml} --config fn_single_line=true"
        )
        .stdin(text)
        .read()
        .unwrap();
        if !stdout.ends_with('\n') {
            stdout.push('\n');
        }
        stdout
    }

    #[allow(unused)]
    pub fn add_preamble(generator: &'static str, mut text: String) -> String {
        let preamble = format!("//! Generated by `{generator}`, do not edit by hand.\n\n");
        text.insert_str(0, &preamble);
        text
    }

    /// Checks that the `file` has the specified `contents`. If that is not the
    /// case, updates the file and then fails the test.
    #[allow(unused)]
    pub fn verify_file_contents(file: &std::path::Path, expected: &str) {
        let current = std::fs::read_to_string(file)
            .unwrap_or_else(|_| panic!("Missing file: {}", file.display()));
        assert_eq!(
            normalize_newlines(&current),
            normalize_newlines(expected),
            "Generated file out of date.\n\
            Run: `just regen` to refresh, then commit."
        );
    }

    #[allow(unused)]
    pub fn write_file_contents(file: &std::path::Path, content: &str) {
        if let Some(parent) = file.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let changed = std::fs::read_to_string(file).ok().as_deref() != Some(content);
        if changed {
            std::fs::write(file, content).expect("failed to write generated file");
        }
    }

    #[allow(unused)]
    fn normalize_newlines(s: &str) -> String {
        s.replace("\r\n", "\n")
    }

    pub fn project_root() -> PathBuf {
        let dir = env!("CARGO_MANIFEST_DIR");
        let res = PathBuf::from(dir)
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_owned();
        assert!(res.join("dummy_triagebot.toml").exists()); // in rust-analyzer just "triagebot.toml".
        res
    }

    pub(crate) struct KindsSrc<'a> {
        pub(crate) punct: &'a [(&'a str, &'a str)],
        pub(crate) keywords: &'a [&'a str],
        pub(crate) literals: &'a [&'a str],
        pub(crate) scalar_types: &'a [&'a str],
        pub(crate) tokens: &'a [&'a str],
        pub(crate) nodes: &'a [&'a str],
    }

    pub(crate) const KINDS_SRC: KindsSrc<'_> = KindsSrc {
        punct: &[
            ("!", "BANG"),
            ("!=", "NEQ"),
            ("#", "POUND"),
            ("$", "DOLLAR"),
            ("%", "PERCENT"),
            ("%=", "PERCENTEQ"),
            ("&", "AMP"),
            ("&&", "AMP2"),
            ("&=", "AMPEQ"),
            ("(", "L_PAREN"),
            (")", "R_PAREN"),
            ("*", "STAR"),
            ("*=", "STAREQ"),
            ("+", "PLUS"),
            ("++", "DOUBLE_PLUS"),
            ("+=", "PLUSEQ"),
            (",", "COMMA"),
            ("-", "MINUS"),
            ("-=", "MINUSEQ"),
            ("->", "THIN_ARROW"),
            (".", "DOT"),
            ("..", "DOT2"),
            ("...", "DOT3"),
            ("..=", "DOT2EQ"),
            ("/", "SLASH"),
            ("/=", "SLASHEQ"),
            (":", "COLON"),
            ("::", "COLON2"),
            (";", "SEMICOLON"),
            ("<", "L_ANGLE"),
            ("<<", "SHL"),
            ("<<=", "SHLEQ"),
            ("<=", "LTEQ"),
            ("=", "EQ"),
            ("==", "EQ2"),
            ("=>", "FAT_ARROW"),
            (">", "R_ANGLE"),
            (">=", "GTEQ"),
            (">>", "SHR"),
            (">>=", "SHREQ"),
            ("?", "QUESTION"),
            ("@", "AT"),
            ("[", "L_BRACK"),
            ("]", "R_BRACK"),
            ("^", "CARET"),
            ("^=", "CARETEQ"),
            ("_", "UNDERSCORE"),
            ("{", "L_CURLY"),
            ("|", "PIPE"),
            ("|=", "PIPEEQ"),
            ("||", "PIPE2"),
            ("}", "R_CURLY"),
            ("~", "TILDE"),
        ],
        keywords: &[
            "OPENQASM",
            "barrier",
            "box",
            "cal",
            "const",
            "def",
            "defcal",
            "defcalgrammar",
            "delay",
            "extern",
            "gate",
            "gphase", // This is a slight hack because a `gphase` call has unique syntax.
            "include",
            "let",
            "measure",
            "pragma",
            "reset",
            // Flow control
            "break",
            "case",
            "continue",
            "default",
            "else",
            "end",
            "for",
            "if",
            "in",
            "return",
            "switch",
            "while",
            // Types
            "array",
            "creg",
            "input",
            "mutable",
            "output",
            "qreg",
            "qubit",
            "readonly",
            "void",
            // Gate modifiers
            "ctrl",
            "inv",
            "negctrl",
            "pow",
            // I suppose these are literals
            "false",
            "true",
        ],
        // GJL: try introducing scalar_types to help parse var declarations. May not be useful
        // sourcegen_ast.rs can convert these to upper snake case.
        scalar_types: &[
            "angle", "bit", "bool", "complex", "duration", "float", "int", "stretch", "uint",
        ],
        // These are already upper snake case.
        literals: &[
            "BIT_STRING",
            "BYTE",
            "CHAR",
            "FLOAT_NUMBER",
            "INT_NUMBER",
            "STRING",
        ],
        tokens: &[
            "ANNOTATION",
            "COMMENT",
            "ERROR",
            "HARDWAREIDENT",
            "IDENT",
            "PRAGMA",
            "WHITESPACE",
        ],
        nodes: &[
            "ANNOTATION_STATEMENT",
            "BARRIER",
            "CAL",
            "CONST",
            "DEF",
            "DEF_CAL",
            "DEF_CAL_GRAMMAR",
            "DELAY_STMT",
            "GATE",
            "MEASURE",
            "PRAGMA_STATEMENT",
            "RESET",
            "SOURCE_FILE",
            "TIMING_LITERAL",
            // atoms
            "ARRAY_EXPR",
            "BLOCK_EXPR",
            "BOX_EXPR",
            "BREAK_STMT",
            "CASE_EXPR",
            "CONTINUE_STMT",
            "END_STMT",
            "FOR_ITERABLE",
            "FOR_STMT",
            "IF_STMT",
            "LET_STMT",
            "PAREN_EXPR",
            "RETURN_EXPR",
            "STMT_LIST",
            "SWITCH_CASE_STMT",
            "TUPLE_EXPR",
            "WHILE_STMT",
            // postfix
            "CALL_EXPR",
            "CAST_EXPRESSION",
            "GATE_CALL_EXPR",
            "G_PHASE_CALL_EXPR",
            "INDEX_EXPR",
            "MODIFIED_GATE_CALL_EXPR",
            // unary
            "ARG_LIST",
            "BIN_EXPR",
            "DECLARATION",
            "EXPR_STMT",
            "FILE_PATH",
            "INCLUDE",
            "LITERAL",
            "NAME",
            "PARAM",
            "PARAM_LIST",
            "PREFIX_EXPR",
            "QUBIT_LIST",
            "RANGE_EXPR",
            "TYPE",
            "TYPED_PARAM",
            "TYPED_PARAM_LIST",
            "VERSION",
            "VERSION_STRING",
            // From ANTLR grammar
            "ALIAS_DECLARATION_STATEMENT",
            "ARRAY_LITERAL",
            "ARRAY_TYPE",
            "ASSIGNMENT_STMT",
            "CLASSICAL_DECLARATION_STATEMENT",
            "DESIGNATOR",
            "EXPRESSION_LIST",
            "GATE_OPERAND",
            "HARDWARE_QUBIT",
            "IDENTIFIER",
            "INDEXED_IDENTIFIER",
            "INDEX_KIND",
            "INDEX_OPERATOR",
            "I_O_DECLARATION_STATEMENT",
            "MEASURE_EXPRESSION",
            "OLD_STYLE_DECLARATION_STATEMENT",
            "QUANTUM_DECLARATION_STATEMENT",
            "QUBIT_TYPE",
            "RETURN_SIGNATURE",
            "SCALAR_TYPE",
            "SET_EXPRESSION",
            // Gate modifiers
            "CTRL_MODIFIER",
            "INV_MODIFIER",
            "MODIFIER",
            "NEG_CTRL_MODIFIER",
            "POW_MODIFIER",
        ],
    };

    #[derive(Default, Debug)]
    pub(crate) struct AstSrc {
        pub(crate) tokens: Vec<String>,
        pub(crate) nodes: Vec<AstNodeSrc>,
        pub(crate) enums: Vec<AstEnumSrc>,
    }

    #[derive(Debug)]
    pub(crate) struct AstNodeSrc {
        pub(crate) doc: Vec<String>,
        pub(crate) name: String,
        pub(crate) traits: Vec<String>,
        pub(crate) fields: Vec<Field>,
    }

    #[derive(Debug, Eq, PartialEq)]
    pub(crate) enum Field {
        Token(String),
        Node {
            name: String,
            ty: String,
            cardinality: Cardinality,
        },
    }

    #[derive(Debug, Eq, PartialEq)]
    pub(crate) enum Cardinality {
        Optional,
        Many,
    }

    #[derive(Debug)]
    pub(crate) struct AstEnumSrc {
        pub(crate) doc: Vec<String>,
        pub(crate) name: String,
        pub(crate) traits: Vec<String>,
        pub(crate) variants: Vec<String>,
    }

    pub fn write_syntax_kinds_enum() {
        let syntax_kinds = generate_syntax_kinds(KINDS_SRC);
        let syntax_kinds_file =
            project_root().join("crates/oq3_parser/src/syntax_kind/syntax_kind_enum.rs");
        write_file_contents(syntax_kinds_file.as_path(), &syntax_kinds);
    }

    /// Read the ungrammar from openqasm3.ungram, lower to the AST, and return the result.
    fn _generate_ast() -> AstSrc {
        let grammar = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/openqasm3.ungram"))
            .parse()
            .unwrap();
        lower(&grammar)
    }

    /// Generate the code destined for nodes.rs, but write to temp file _new_nodes.rs.
    /// The code depends on the AST generated from openqasm3.ungram.
    pub fn sourcegen_ast_tokens() {
        let ast = _generate_ast();

        let ast_tokens = generate_tokens(&ast);
        let ast_tokens_file = project_root().join("crates/oq3_syntax/src/ast/generated/tokens.rs");
        write_file_contents(ast_tokens_file.as_path(), &ast_tokens);
    }

    /// Generate the code destined for nodes.rs, but write to temp file _new_nodes.rs.
    /// The code depends on the AST generated from openqasm3.ungram as well as the ast ingredients
    /// in KINDS_SRC.
    pub fn sourcegen_ast_nodes() {
        let ast = _generate_ast();

        let ast_nodes = generate_nodes(KINDS_SRC, &ast);
        let ast_nodes_file = project_root().join("crates/oq3_syntax/src/ast/generated/nodes.rs");
        write_file_contents(ast_nodes_file.as_path(), &ast_nodes);
    }

    fn generate_tokens(grammar: &AstSrc) -> String {
        let tokens = grammar.tokens.iter().map(|token| {
            let name = format_ident!("{}", token);
            let kind = format_ident!("{}", to_upper_snake_case(token));
            quote! {
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #name {
                    pub(crate) syntax: SyntaxToken,
                }
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(&self.syntax, f)
                    }
                }
                impl AstToken for #name {
                    fn can_cast(kind: SyntaxKind) -> bool { kind == #kind }
                    fn cast(syntax: SyntaxToken) -> Option<Self> {
                        if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                    }
                    fn syntax(&self) -> &SyntaxToken { &self.syntax }
                }
            }
        });

        add_preamble(
            "sourcegen_ast",
            quote! {
                use crate::{SyntaxKind::{self, *}, SyntaxToken, ast::AstToken};
                #(#tokens)*
            }
            .to_string(),
            // sourcegen::reformat( FIXME
            //     quote! {
            //         use crate::{SyntaxKind::{self, *}, SyntaxToken, ast::AstToken};
            //         #(#tokens)*
            //     }
            //     .to_string(),
            // ),
        )
        .replace("#[derive", "\n#[derive")
    }

    fn generate_nodes(kinds: KindsSrc<'_>, grammar: &AstSrc) -> String {
        let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
            .nodes
            .iter()
            .map(|node| {
                let name = format_ident!("{}", node.name);
                let kind = format_ident!("{}", to_upper_snake_case(&node.name));
                let traits = node
                    .traits
                    .iter()
                    .filter(|trait_name| {
                        // Loops have two expressions so this might collide, therefore manual impl it
                        node.name != "ForExpr" && node.name != "WhileExpr"
                            || trait_name.as_str() != "HasLoopBody"
                    })
                    .map(|trait_name| {
                        let trait_name = format_ident!("{}", trait_name);
                        quote!(impl ast::#trait_name for #name {})
                    });

                let methods = node.fields.iter().map(|field| {
                    let method_name = field.method_name();
                    let ty = field.ty();

                    if field.is_many() {
                        quote! {
                            pub fn #method_name(&self) -> AstChildren<#ty> {
                                support::children(&self.syntax)
                            }
                        }
                    } else if let Some(token_kind) = field.token_kind() {
                        quote! {
                            pub fn #method_name(&self) -> Option<#ty> {
                                support::token(&self.syntax, #token_kind)
                            }
                        }
                    } else {
                        quote! {
                            pub fn #method_name(&self) -> Option<#ty> {
                                support::child(&self.syntax)
                            }
                        }
                    }
                });
                (
                    quote! {
                        #[pretty_doc_comment_placeholder_workaround]
                        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                        pub struct #name {
                            pub(crate) syntax: SyntaxNode,
                        }

                        #(#traits)*

                        impl #name {
                            #(#methods)*
                        }
                    },
                    quote! {
                        impl AstNode for #name {
                            fn can_cast(kind: SyntaxKind) -> bool {
                                kind == #kind
                            }
                            fn cast(syntax: SyntaxNode) -> Option<Self> {
                                if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                            }
                            fn syntax(&self) -> &SyntaxNode { &self.syntax }
                        }
                    },
                )
            })
            .unzip();

        let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
            .enums
            .iter()
            .map(|en| {
                let variants: Vec<_> = en
                    .variants
                    .iter()
                    .map(|var| format_ident!("{}", var))
                    .collect();
                let name = format_ident!("{}", en.name);
                let kinds: Vec<_> = variants
                    .iter()
                    .map(|name| format_ident!("{}", to_upper_snake_case(&name.to_string())))
                    .collect();
                let traits = en.traits.iter().map(|trait_name| {
                    let trait_name = format_ident!("{}", trait_name);
                    quote!(impl ast::#trait_name for #name {})
                });

                let ast_node = if en.name == "DisableStmt" {
                    // No longer want to skip Stmt
                    quote! {}
                } else {
                    quote! {
                        impl AstNode for #name {
                            fn can_cast(kind: SyntaxKind) -> bool {
                                matches!(kind, #(#kinds)|*)
                            }
                            fn cast(syntax: SyntaxNode) -> Option<Self> {
                                let res = match syntax.kind() {
                                    #(
                                    #kinds => #name::#variants(#variants { syntax }),
                                    )*
                                    _ => return None,
                                };
                                Some(res)
                            }
                            fn syntax(&self) -> &SyntaxNode {
                                match self {
                                    #(
                                    #name::#variants(it) => &it.syntax,
                                    )*
                                }
                            }
                        }
                    }
                };

                (
                    quote! {
                        #[pretty_doc_comment_placeholder_workaround]
                        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                        pub enum #name {
                            #(#variants(#variants),)*
                        }

                        #(#traits)*
                    },
                    quote! {
                        #(
                            impl From<#variants> for #name {
                                fn from(node: #variants) -> #name {
                                    #name::#variants(node)
                                }
                            }
                        )*
                        #ast_node
                    },
                )
            })
            .unzip();

        let (any_node_defs, any_node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
            .nodes
            .iter()
            .flat_map(|node| node.traits.iter().map(move |t| (t, node)))
            .into_group_map()
            .into_iter()
            .sorted_by_key(|(k, _)| *k)
            .map(|(trait_name, nodes)| {
                let name = format_ident!("Any{}", trait_name);
                let trait_name = format_ident!("{}", trait_name);
                let kinds: Vec<_> = nodes
                    .iter()
                    .map(|name| format_ident!("{}", to_upper_snake_case(&name.name.to_string())))
                    .collect();

                (
                    quote! {
                        #[pretty_doc_comment_placeholder_workaround]
                        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                        pub struct #name {
                            pub(crate) syntax: SyntaxNode,
                        }
                        impl ast::#trait_name for #name {}
                    },
                    quote! {
                        impl #name {
                            #[inline]
                            pub fn new<T: ast::#trait_name>(node: T) -> #name {
                                #name {
                                    syntax: node.syntax().clone()
                                }
                            }
                        }
                        impl AstNode for #name {
                            fn can_cast(kind: SyntaxKind) -> bool {
                                matches!(kind, #(#kinds)|*)
                            }
                            fn cast(syntax: SyntaxNode) -> Option<Self> {
                                Self::can_cast(syntax.kind()).then_some(#name { syntax })
                            }
                            fn syntax(&self) -> &SyntaxNode {
                                &self.syntax
                            }
                        }
                    },
                )
            })
            .unzip();

        let enum_names = grammar.enums.iter().map(|it| &it.name);
        let node_names = grammar.nodes.iter().map(|it| &it.name);

        let display_impls = enum_names
            .chain(node_names.clone())
            .map(|it| format_ident!("{}", it))
            .map(|name| {
                quote! {
                    impl std::fmt::Display for #name {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            std::fmt::Display::fmt(self.syntax(), f)
                        }
                    }
                }
            });

        let defined_nodes: HashSet<_> = node_names.collect();

        for node in kinds
            .nodes
            .iter()
            .map(|kind| to_pascal_case(kind))
            .filter(|name| !defined_nodes.iter().any(|&it| it == name))
        {
            drop(node)
            // FIXME: restore this (not a GJL FIXME !)
            // eprintln!("Warning: node {} not defined in ast source", node);
        }

        let ast = quote! {
            #![allow(non_snake_case)]
            use crate::{
                SyntaxNode, SyntaxToken, SyntaxKind::{self, *},
                ast::{self, AstNode, AstChildren, support},
                T,
            };

            #(#node_defs)*
            #(#enum_defs)*
            #(#any_node_defs)*
            #(#node_boilerplate_impls)*
            #(#enum_boilerplate_impls)*
            #(#any_node_boilerplate_impls)*
            #(#display_impls)*
        };

        let ast = ast.to_string().replace("T ! [", "T![");

        let mut res = String::with_capacity(ast.len() * 2);

        let mut docs = grammar
            .nodes
            .iter()
            .map(|it| &it.doc)
            .chain(grammar.enums.iter().map(|it| &it.doc));

        for chunk in ast.split("# [pretty_doc_comment_placeholder_workaround] ") {
            res.push_str(chunk);
            if let Some(doc) = docs.next() {
                write_doc_comment(doc, &mut res);
            }
        }

        //    let res = sourcegen::add_preamble("sourcegen_ast", sourcegen::reformat(res)); FIXME
        let res = add_preamble("sourcegen_ast", res);
        res.replace("#[derive", "\n#[derive")
    }

    fn write_doc_comment(contents: &[String], dest: &mut String) {
        for line in contents {
            writeln!(dest, "///{line}").unwrap();
        }
    }

    fn generate_syntax_kinds(grammar: KindsSrc<'_>) -> String {
        let (single_byte_tokens_values, single_byte_tokens): (Vec<_>, Vec<_>) = grammar
            .punct
            .iter()
            .filter(|(token, _name)| token.len() == 1)
            .map(|(token, name)| (token.chars().next().unwrap(), format_ident!("{}", name)))
            .unzip();

        let punctuation_values = grammar.punct.iter().map(|(token, _name)| {
            if "{}[]()".contains(token) {
                let c = token.chars().next().unwrap();
                quote! { #c }
            } else if *token == "_" {
                // `_` is not punctuation in proc_macro2; emit it as an identifier.
                let ident = Ident::new("_", Span::call_site());
                quote! { #ident }
            } else {
                let cs = token.chars().map(|c| Punct::new(c, Spacing::Joint));
                quote! { #(#cs)* }
            }
        });
        let punctuation = grammar
            .punct
            .iter()
            .map(|(_token, name)| format_ident!("{}", name))
            .collect::<Vec<_>>();

        let upper_snake = |&name| match name {
            "Self" => format_ident!("SELF_TYPE_KW"),
            name => format_ident!("{}_KW", to_upper_snake_case(name)),
        };
        let full_keywords_values = grammar.keywords;
        let full_keywords = full_keywords_values.iter().map(upper_snake);

        // let contextual_keywords_values = &grammar.contextual_keywords;
        // let contextual_keywords = contextual_keywords_values.iter().map(upper_snake);

        let all_keywords_values = grammar.keywords.to_vec();
        // .iter()
        // //        .chain(grammar.contextual_keywords.iter())
        // .copied()
        // .collect::<Vec<_>>();
        let all_keywords_idents = all_keywords_values.iter().map(|kw| format_ident!("{}", kw));
        let all_keywords = all_keywords_values
            .iter()
            .map(upper_snake)
            .collect::<Vec<_>>();

        let scalar_types_values = grammar.scalar_types.iter().collect::<Vec<_>>();
        let scalar_types_idents = scalar_types_values.iter().map(|ty| format_ident!("{}", ty));
        let scalar_types = scalar_types_values
            .iter()
            .map(|ty| format_ident!("{}_TY", to_upper_snake_case(ty)))
            .collect::<Vec<_>>();
        //        grammar.scalar_types.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

        let literals = grammar
            .literals
            .iter()
            .map(|name| format_ident!("{}", name))
            .collect::<Vec<_>>();

        let tokens = grammar
            .tokens
            .iter()
            .map(|name| format_ident!("{}", name))
            .collect::<Vec<_>>();

        let nodes = grammar
            .nodes
            .iter()
            .map(|name| format_ident!("{}", name))
            .collect::<Vec<_>>();

        // FIXME: find out how to insert a plain old comment in the quoted thing.
        // Usual double slash quotes are ommited. trip slash quotes are converted to some kind of doc macro.
        let ast = quote! {
            #![allow(bad_style, missing_docs, unreachable_pub)]
            /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            #[repr(u16)]
            pub enum SyntaxKind {
                // Technical SyntaxKinds: they appear temporally during parsing,
                // but never end up in the final tree
                #[doc(hidden)]
                TOMBSTONE,
                #[doc(hidden)]
                EOF,
                /// punctuation
                #(#punctuation,)*
                /// all_keywords
                #(#all_keywords,)*
                /// literals
                #(#literals,)*
                /// scalar_types
                #(#scalar_types,)*
                /// tokens
                #(#tokens,)*
                /// nodes
                #(#nodes,)*

                // Technical kind so that we can cast from u16 safely
                #[doc(hidden)]
                __LAST,
            }
            use self::SyntaxKind::*;

            impl SyntaxKind {
                pub fn is_keyword(self) -> bool {
                    matches!(self, #(#all_keywords)|*)
                }

                pub fn is_punct(self) -> bool {
                    matches!(self, #(#punctuation)|*)
                }

                pub fn is_literal(self) -> bool {
                    matches!(self, #(#literals)|*)
                }

                pub fn is_scalar_type(self) -> bool {
                    matches!(self, #(#scalar_types)|*)
                }

                pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
                    let kw = match ident {
                        #(#full_keywords_values => #full_keywords,)*
                        _ => return None,
                    };
                    Some(kw)
                }

                pub fn from_scalar_type(type_name: &str) -> Option<SyntaxKind> {
                    let ty = match type_name {
                        #(#scalar_types_values => #scalar_types,)*
                        _ => return None,
                    };
                    Some(ty)
                }

                pub fn from_char(c: char) -> Option<SyntaxKind> {
                    let tok = match c {
                        #(#single_byte_tokens_values => #single_byte_tokens,)*
                        _ => return None,
                    };
                    Some(tok)
                }
            }

            #[macro_export]
            macro_rules! T {
                #([#punctuation_values] => { $crate::SyntaxKind::#punctuation };)*
                #([#all_keywords_idents] => { $crate::SyntaxKind::#all_keywords };)*
                #([#scalar_types_idents] => { $crate::SyntaxKind::#scalar_types };)*
                [ident] => { $crate::SyntaxKind::IDENT };
            }
            pub use T;
        };

        //    sourcegen::add_preamble("sourcegen_ast", sourcegen::reformat(ast.to_string())) // FIXME
        add_preamble("sourcegen_ast", ast.to_string())
    }

    fn to_upper_snake_case(s: &str) -> String {
        let mut buf = String::with_capacity(s.len());
        let mut prev = false;
        for c in s.chars() {
            if c.is_ascii_uppercase() && prev {
                buf.push('_')
            }
            prev = true;

            buf.push(c.to_ascii_uppercase());
        }
        buf
    }

    fn to_lower_snake_case(s: &str) -> String {
        let mut buf = String::with_capacity(s.len());
        let mut prev = false;
        for c in s.chars() {
            if c.is_ascii_uppercase() && prev {
                buf.push('_')
            }
            prev = true;

            buf.push(c.to_ascii_lowercase());
        }
        buf
    }

    fn to_pascal_case(s: &str) -> String {
        let mut buf = String::with_capacity(s.len());
        let mut prev_is_underscore = true;
        for c in s.chars() {
            if c == '_' {
                prev_is_underscore = true;
            } else if prev_is_underscore {
                buf.push(c.to_ascii_uppercase());
                prev_is_underscore = false;
            } else {
                buf.push(c.to_ascii_lowercase());
            }
        }
        buf
    }

    fn pluralize(s: &str) -> String {
        format!("{s}s")
    }

    impl Field {
        fn is_many(&self) -> bool {
            matches!(
                self,
                Field::Node {
                    cardinality: Cardinality::Many,
                    ..
                }
            )
        }
        fn token_kind(&self) -> Option<proc_macro2::TokenStream> {
            match self {
                Field::Token(token) => {
                    let token: proc_macro2::TokenStream = token.parse().unwrap();
                    Some(quote! { T![#token] })
                }
                _ => None,
            }
        }
        fn method_name(&self) -> proc_macro2::Ident {
            match self {
                Field::Token(name) => {
                    let name = match name.as_str() {
                        ";" => "semicolon",
                        "->" => "thin_arrow",
                        "'{'" => "l_curly",
                        "'}'" => "r_curly",
                        "'('" => "l_paren",
                        "')'" => "r_paren",
                        "'['" => "l_brack",
                        "']'" => "r_brack",
                        "<" => "l_angle",
                        ">" => "r_angle",
                        "=" => "eq",
                        "!" => "excl",
                        "*" => "star",
                        "&" => "amp",
                        "-" => "minus",
                        "_" => "underscore",
                        "." => "dot",
                        ".." => "dotdot",
                        "..." => "dotdotdot",
                        "..=" => "dotdoteq",
                        "=>" => "fat_arrow",
                        "@" => "at",
                        ":" => "colon",
                        "::" => "coloncolon",
                        "#" => "pound",
                        "?" => "question_mark",
                        "," => "comma",
                        "|" => "pipe",
                        "~" => "tilde",
                        "++" => "double_plus",
                        _ => name,
                    };
                    format_ident!("{}_token", name)
                }
                Field::Node { name, .. } => {
                    if name == "type" {
                        format_ident!("ty")
                    } else {
                        format_ident!("{}", name)
                    }
                }
            }
        }
        fn ty(&self) -> proc_macro2::Ident {
            match self {
                Field::Token(_) => format_ident!("SyntaxToken"),
                Field::Node { ty, .. } => format_ident!("{}", ty),
            }
        }
    }

    fn lower(grammar: &Grammar) -> AstSrc {
        let mut res = AstSrc {
            tokens: "Whitespace Comment String IntNumber FloatNumber Char Byte Ident BitString"
                .split_ascii_whitespace()
                .map(|it| it.to_string())
                .collect::<Vec<_>>(),
            ..Default::default()
        };

        let nodes = grammar.iter().collect::<Vec<_>>();

        for &node in &nodes {
            let name = grammar[node].name.clone();
            let rule = &grammar[node].rule;
            match lower_enum(grammar, rule) {
                Some(variants) => {
                    let enum_src = AstEnumSrc {
                        doc: Vec::new(),
                        name,
                        traits: Vec::new(),
                        variants,
                    };
                    res.enums.push(enum_src);
                }
                None => {
                    let mut fields = Vec::new();
                    lower_rule(&mut fields, grammar, None, rule);
                    res.nodes.push(AstNodeSrc {
                        doc: Vec::new(),
                        name,
                        traits: Vec::new(),
                        fields,
                    });
                }
            }
        }

        deduplicate_fields(&mut res);
        extract_enums(&mut res);
        extract_struct_traits(&mut res);
        extract_enum_traits(&mut res);
        res
    }

    fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<String>> {
        let alternatives = match rule {
            Rule::Alt(it) => it,
            _ => return None,
        };
        let mut variants = Vec::new();
        for alternative in alternatives {
            match alternative {
                Rule::Node(it) => variants.push(grammar[*it].name.clone()),
                Rule::Token(it) if grammar[*it].name == ";" => (),
                _ => return None,
            }
        }
        Some(variants)
    }

    // I copied comments from `enum Rule` to the match arms below.
    // acc -- accumulate the lowered code
    // grammar -- ??
    // label -- used to name the accessor created by lowering. At the top-level calls, `label` is `None`.
    //     If a Rule::Label is encountered, the label and inner rule are extracted and `lower_rule`
    //     is called again, passing the inner rule and the extracted label.
    // rule -- the rule to lower.
    fn lower_rule(acc: &mut Vec<Field>, grammar: &Grammar, label: Option<&String>, rule: &Rule) {
        if lower_comma_list(acc, grammar, label, rule) {
            return;
        }
        match rule {
            // Node(Node) A node, like `A`.
            Rule::Node(node) => {
                let ty = grammar[*node].name.clone();
                let name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
                let field = Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Optional,
                };
                acc.push(field);
            }
            // Token(Token), A token, like `'struct'`.
            Rule::Token(token) => {
                assert!(label.is_none());
                let mut name = grammar[*token].name.clone();
                // Prevent writing rules for hardware identifiers like this:
                // pub fn hardwareident_token(&self) -> Option<SyntaxToken> {
                //     support::token(&self.syntax, T![hardwareident])
                // }
                if name != "int_number" && name != "string" && name != "hardwareident" {
                    if "[]{}()".contains(&name) {
                        name = format!("'{name}'");
                    }
                    let field = Field::Token(name);
                    acc.push(field);
                }
            }
            // Rep(Box<Rule>), A repeated rule, like `A*`.
            Rule::Rep(inner) => {
                if let Rule::Node(node) = &**inner {
                    let ty = grammar[*node].name.clone();
                    let name = label
                        .cloned()
                        .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
                    let field = Field::Node {
                        name,
                        ty,
                        cardinality: Cardinality::Many,
                    };
                    acc.push(field);
                    return;
                }
                // I added a bit of diagnostic info here.
                // Note that the restriction on inner is not part of the grammar of ungrammar, but rather
                // what is implemented here.
                if let Rule::Seq(_rules) = &**inner {
                    panic!("Inner rule in Rule::Rep (repeated with '*') must be a Node, got a Seq.\nNode{rule:?}\nInner: {inner:?}");
                }
                // FIXME: This desperately needs better diagnostics.
                // There is no clue about which rule is causing the error.
                panic!("unhandled Rule::Rep (repeated with '*'): {rule:?}\nInner: {inner:?}")
            }
            // Labeled { label: String, rule: Box<Rule>}
            //     A labeled rule, like `a:B` (`"a"` is the label, `B` is the rule).
            Rule::Labeled { label: l, rule } => {
                assert!(label.is_none());
                // This is a list of labels that are used in openqasm3.ungram (modified from list in in rust-analyzer)
                // There are typically functions with the same name as the label that are defined in files named
                // *_ext.rs.
                let manually_implemented = matches!(
                    l.as_str(),
                    "lhs"
                        | "rhs"
                        | "then_branch"
                        | "else_branch"
                        | "start"
                        | "end"
                        | "op"
                        | "concat"
                        | "index"
                        | "base"
                        | "value"
                        | "iterable"
                        | "condition"
                        | "angle_params"
                        | "qubit_params"
                        | "thelabel"
                );
                if manually_implemented {
                    return;
                }
                lower_rule(acc, grammar, Some(l), rule);
            }
            // Seq(Vec<Rule>) : A sequence of rules, like `'while' '(' Expr ')' Stmt`.
            // Alt(Vec<Rule>) : An alternative between many rules, like `'+' | '-' | '*' | '/'`.
            Rule::Seq(rules) | Rule::Alt(rules) => {
                for rule in rules {
                    lower_rule(acc, grammar, label, rule)
                }
            }
            // Opt(Box<Rule>), An optional rule, like `A?`.
            Rule::Opt(rule) => lower_rule(acc, grammar, label, rule),
        }
    }

    // (T (',' T)* ','?)
    fn lower_comma_list(
        acc: &mut Vec<Field>,
        grammar: &Grammar,
        label: Option<&String>,
        rule: &Rule,
    ) -> bool {
        let rule = match rule {
            Rule::Seq(it) => it,
            _ => return false,
        };
        let (node, repeat, trailing_comma) = match rule.as_slice() {
            [Rule::Node(node), Rule::Rep(repeat), Rule::Opt(trailing_comma)] => {
                (node, repeat, trailing_comma)
            }
            _ => return false,
        };
        let repeat = match &**repeat {
            Rule::Seq(it) => it,
            _ => return false,
        };
        match repeat.as_slice() {
            [comma, Rule::Node(n)] if comma == &**trailing_comma && n == node => (),
            _ => return false,
        }
        let ty = grammar[*node].name.clone();
        let name = label
            .cloned()
            .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
        let field = Field::Node {
            name,
            ty,
            cardinality: Cardinality::Many,
        };
        acc.push(field);
        true
    }

    fn deduplicate_fields(ast: &mut AstSrc) {
        for node in &mut ast.nodes {
            let mut i = 0;
            'outer: while i < node.fields.len() {
                for j in 0..i {
                    let f1 = &node.fields[i];
                    let f2 = &node.fields[j];
                    if f1 == f2 {
                        node.fields.remove(i);
                        continue 'outer;
                    }
                }
                i += 1;
            }
        }
    }

    fn extract_enums(ast: &mut AstSrc) {
        for node in &mut ast.nodes {
            for enm in &ast.enums {
                let mut to_remove = Vec::new();
                for (i, field) in node.fields.iter().enumerate() {
                    let ty = field.ty().to_string();
                    if enm.variants.iter().any(|it| it == &ty) {
                        to_remove.push(i);
                    }
                }
                if to_remove.len() == enm.variants.len() {
                    node.remove_field(to_remove);
                    let ty = enm.name.clone();
                    let name = to_lower_snake_case(&ty);
                    node.fields.push(Field::Node {
                        name,
                        ty,
                        cardinality: Cardinality::Optional,
                    });
                }
            }
        }
    }

    fn extract_struct_traits(ast: &mut AstSrc) {
        let traits: &[(&str, &[&str])] = &[
            ("HasAttrs", &["attrs"]),
            ("HasName", &["name"]),
            //        ("HasTypeBounds", &["type_bound_list", "colon_token"]),
            //        ("HasModuleItem", &["items"]), No longer needed now that Item is removed
            ("HasLoopBody", &["label", "loop_body"]),
            ("HasArgList", &["arg_list"]),
        ];

        for node in &mut ast.nodes {
            for (name, methods) in traits {
                extract_struct_trait(node, name, methods);
            }
        }
    }

    fn extract_struct_trait(node: &mut AstNodeSrc, trait_name: &str, methods: &[&str]) {
        let mut to_remove = Vec::new();
        for (i, field) in node.fields.iter().enumerate() {
            let method_name = field.method_name().to_string();
            if methods.iter().any(|&it| it == method_name) {
                to_remove.push(i);
            }
        }
        if to_remove.len() == methods.len() {
            node.traits.push(trait_name.to_string());
            node.remove_field(to_remove);
        }
    }

    fn extract_enum_traits(ast: &mut AstSrc) {
        for enm in &mut ast.enums {
            if enm.name == "DisableStmt" {
                // No longer want to skip Stmt
                continue;
            }
            let nodes = &ast.nodes;
            let mut variant_traits = enm
                .variants
                .iter()
                .map(|var| nodes.iter().find(|it| &it.name == var).unwrap())
                .map(|node| node.traits.iter().cloned().collect::<BTreeSet<_>>());

            let mut enum_traits = match variant_traits.next() {
                Some(it) => it,
                None => continue,
            };
            for traits in variant_traits {
                enum_traits = enum_traits.intersection(&traits).cloned().collect();
            }
            enm.traits = enum_traits.into_iter().collect();
        }
    }

    impl AstNodeSrc {
        fn remove_field(&mut self, to_remove: Vec<usize>) {
            to_remove.into_iter().rev().for_each(|idx| {
                self.fields.remove(idx);
            });
        }
    }

    pub(super) fn main() {
        // Make builds reproducible across clean checkouts
        let _ = fs::create_dir_all(project_root().join("crates/oq3_parser/src/syntax_kind"));
        let _ = fs::create_dir_all(project_root().join("crates/oq3_syntax/src/ast/generated"));

        write_syntax_kinds_enum();
        sourcegen_ast_tokens();
        sourcegen_ast_nodes();
    }
}

fn main() {
    #[cfg(feature = "sourcegen")]
    sourcegen::main();
}
