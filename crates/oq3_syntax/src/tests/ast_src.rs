// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Defines input for code generation process.

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
        ("++", "DOUBLE_PLUS"),
        (";", "SEMICOLON"),
        (",", "COMMA"),
        ("(", "L_PAREN"),
        (")", "R_PAREN"),
        ("{", "L_CURLY"),
        ("}", "R_CURLY"),
        ("[", "L_BRACK"),
        ("]", "R_BRACK"),
        ("<", "L_ANGLE"),
        (">", "R_ANGLE"),
        ("@", "AT"),
        ("#", "POUND"),
        ("~", "TILDE"),
        ("?", "QUESTION"),
        ("$", "DOLLAR"),
        ("&", "AMP"),
        ("|", "PIPE"),
        ("+", "PLUS"),
        ("*", "STAR"),
        ("/", "SLASH"),
        ("^", "CARET"),
        ("%", "PERCENT"),
        ("_", "UNDERSCORE"),
        (".", "DOT"),
        ("..", "DOT2"),
        ("...", "DOT3"),
        ("..=", "DOT2EQ"),
        (":", "COLON"),
        ("::", "COLON2"),
        ("=", "EQ"),
        ("==", "EQ2"),
        ("=>", "FAT_ARROW"),
        ("!", "BANG"),
        ("!=", "NEQ"),
        ("-", "MINUS"),
        ("->", "THIN_ARROW"),
        ("<=", "LTEQ"),
        (">=", "GTEQ"),
        ("+=", "PLUSEQ"),
        ("-=", "MINUSEQ"),
        ("|=", "PIPEEQ"),
        ("&=", "AMPEQ"),
        ("^=", "CARETEQ"),
        ("/=", "SLASHEQ"),
        ("*=", "STAREQ"),
        ("%=", "PERCENTEQ"),
        ("&&", "AMP2"),
        ("||", "PIPE2"),
        ("<<", "SHL"),
        (">>", "SHR"),
        ("<<=", "SHLEQ"),
        (">>=", "SHREQ"),
    ],
    keywords: &[
        "OPENQASM",
        "include",
        "pragma",
        "def",
        "defcalgrammar",
        "cal",
        "defcal",
        "gate",
        "delay",
        "reset",
        "measure",
        "let",
        "box",
        "extern",
        "const",
        "barrier",
        "gphase", // This is a slight hack because a `gphase` call has unique syntax.
        // Flow control
        "if",
        "else",
        "for",
        "in",
        "while",
        "continue",
        "return",
        "break",
        "end",
        "switch",
        "case",
        "default",
        // Types
        "input",
        "output",
        "readonly",
        "mutable",
        "qreg",
        "creg",
        "qubit",
        "void",
        "array",
        // Gate modifiers
        "ctrl",
        "negctrl",
        "inv",
        "pow",
        // I suppose these are literals
        "false",
        "true",
    ],
    // GJL: try introducing scalar_types to help parse var declarations. May not be useful
    // sourcegen_ast.rs can convert these to upper snake case.
    scalar_types: &[
        "float", "int", "uint", "complex", "bool", "bit", "duration", "stretch", "angle",
    ],
    // These are already upper snake case.
    literals: &[
        "INT_NUMBER",
        "FLOAT_NUMBER",
        "CHAR",
        "BYTE",
        "STRING",
        "BIT_STRING",
    ],
    tokens: &[
        "ERROR",
        "IDENT",
        "HARDWAREIDENT",
        "WHITESPACE",
        "COMMENT",
        "PRAGMA",
        "ANNOTATION",
    ],
    nodes: &[
        "SOURCE_FILE",
        "GATE",
        "DEF_CAL",
        "DELAY_STMT",
        "CAL",
        "DEF_CAL_GRAMMAR",
        "MEASURE",
        "BARRIER",
        "DEF",
        "RESET",
        "CONST",
        "PRAGMA_STATEMENT",
        "ANNOTATION_STATEMENT",
        "TIMING_LITERAL",
        // atoms
        "TUPLE_EXPR",
        "ARRAY_EXPR",
        "PAREN_EXPR",
        "IF_STMT",
        "WHILE_STMT",
        "FOR_STMT",
        "FOR_ITERABLE",
        "END_STMT",
        "CONTINUE_STMT",
        "BREAK_STMT",
        "BLOCK_EXPR",
        "STMT_LIST",
        "RETURN_EXPR",
        "LET_STMT",
        "BOX_EXPR",
        "SWITCH_CASE_STMT",
        "CASE_EXPR",
        // postfix
        "CALL_EXPR",
        "GATE_CALL_EXPR",
        "MODIFIED_GATE_CALL_EXPR",
        "G_PHASE_CALL_EXPR",
        "CAST_EXPRESSION",
        "INDEX_EXPR",
        // unary
        "PREFIX_EXPR",
        "RANGE_EXPR",
        "BIN_EXPR",
        "LITERAL",
        "NAME",
        "EXPR_STMT",
        "TYPE",
        "PARAM_LIST",
        "TYPED_PARAM_LIST",
        "QUBIT_LIST",
        "FILE_PATH",
        "PARAM",
        "TYPED_PARAM",
        "ARG_LIST",
        "VERSION",
        "VERSION_STRING",
        "INCLUDE",
        "DECLARATION",
        // From ANTLR grammar
        "DESIGNATOR",
        "SCALAR_TYPE",
        "ARRAY_TYPE",
        "QUBIT_TYPE",
        "EXPRESSION_LIST",
        "RETURN_SIGNATURE",
        "SET_EXPRESSION",
        "ALIAS_DECLARATION_STATEMENT",
        "INDEX_OPERATOR",
        "INDEX_KIND",
        "INDEXED_IDENTIFIER",
        "IDENTIFIER",
        "ARRAY_LITERAL",
        "HARDWARE_QUBIT",
        "CLASSICAL_DECLARATION_STATEMENT",
        "ASSIGNMENT_STMT",
        "I_O_DECLARATION_STATEMENT",
        "GATE_OPERAND",
        "MEASURE_EXPRESSION",
        "OLD_STYLE_DECLARATION_STATEMENT",
        "QUANTUM_DECLARATION_STATEMENT",
        // Gate modifiers
        "INV_MODIFIER",
        "POW_MODIFIER",
        "CTRL_MODIFIER",
        "NEG_CTRL_MODIFIER",
        "MODIFIER",
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
