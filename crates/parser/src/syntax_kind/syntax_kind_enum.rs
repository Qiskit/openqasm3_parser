// Copyright contributors to the openqasm-parser project

//! Generated by `sourcegen_ast`, do not edit by hand.

#![allow(bad_style, missing_docs, unreachable_pub)]
#[doc = r" The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`."]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    #[doc(hidden)]
    TOMBSTONE,
    #[doc(hidden)]
    EOF,
    #[doc = r" punctuation"]
    DOUBLE_PLUS,
    SEMICOLON,
    COMMA,
    L_PAREN,
    R_PAREN,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,
    L_ANGLE,
    R_ANGLE,
    AT,
    POUND,
    TILDE,
    QUESTION,
    DOLLAR,
    AMP,
    PIPE,
    PLUS,
    STAR,
    SLASH,
    CARET,
    PERCENT,
    UNDERSCORE,
    DOT,
    DOT2,
    DOT3,
    DOT2EQ,
    COLON,
    COLON2,
    EQ,
    EQ2,
    FAT_ARROW,
    BANG,
    NEQ,
    MINUS,
    THIN_ARROW,
    LTEQ,
    GTEQ,
    PLUSEQ,
    MINUSEQ,
    PIPEEQ,
    AMPEQ,
    CARETEQ,
    SLASHEQ,
    STAREQ,
    PERCENTEQ,
    AMP2,
    PIPE2,
    SHL,
    SHR,
    SHLEQ,
    SHREQ,
    #[doc = r" all_keywords"]
    O_P_E_N_Q_A_S_M_KW,
    INCLUDE_KW,
    DEF_KW,
    DEFCALGRAMMAR_KW,
    CAL_KW,
    DEFCAL_KW,
    GATE_KW,
    DELAY_KW,
    RESET_KW,
    MEASURE_KW,
    PRAGMA_KW,
    END_KW,
    LET_KW,
    BOX_KW,
    EXTERN_KW,
    CONST_KW,
    BARRIER_KW,
    GPHASE_KW,
    IF_KW,
    ELSE_KW,
    FOR_KW,
    IN_KW,
    WHILE_KW,
    CONTINUE_KW,
    RETURN_KW,
    BREAK_KW,
    INPUT_KW,
    OUTPUT_KW,
    READONLY_KW,
    MUTABLE_KW,
    QREG_KW,
    CREG_KW,
    QUBIT_KW,
    VOID_KW,
    ARRAY_KW,
    FALSE_KW,
    TRUE_KW,
    #[doc = r" literals"]
    INT_NUMBER,
    FLOAT_NUMBER,
    SIMPLE_FLOAT_NUMBER,
    CHAR,
    BYTE,
    STRING,
    BIT_STRING,
    TIMING_FLOAT_NUMBER,
    TIMING_INT_NUMBER,
    #[doc = r" scalar_types"]
    FLOAT_TY,
    INT_TY,
    UINT_TY,
    COMPLEX_TY,
    BOOL_TY,
    BIT_TY,
    DURATION_TY,
    STRETCH_TY,
    ANGLE_TY,
    #[doc = r" tokens"]
    ERROR,
    IDENT,
    HARDWAREIDENT,
    WHITESPACE,
    COMMENT,
    #[doc = r" nodes"]
    SOURCE_FILE,
    GATE,
    DEF_CAL,
    CAL,
    DEF_CAL_GRAMMAR,
    MEASURE,
    BARRIER,
    DEF,
    RESET,
    RET_TYPE,
    CONST,
    PAREN_TYPE,
    PATH_TYPE,
    SLICE_TYPE,
    TUPLE_EXPR,
    ARRAY_EXPR,
    PAREN_EXPR,
    PATH_EXPR,
    IF_STMT,
    WHILE_STMT,
    FOR_STMT,
    END_STMT,
    CONTINUE_STMT,
    BREAK_STMT,
    LABEL,
    BLOCK_EXPR,
    STMT_LIST,
    RETURN_EXPR,
    LET_STMT,
    ALIAS_EXPR,
    CONCATENATION_EXPR,
    BOX_EXPR,
    CALL_EXPR,
    CAST_EXPRESSION,
    GATE_CALL_STMT,
    G_PHASE_CALL_STMT,
    INDEX_EXPR,
    PREFIX_EXPR,
    RANGE_EXPR,
    BIN_EXPR,
    SET_NUM,
    EXTERN_ITEM,
    ITEM_LIST,
    PATH,
    PATH_SEGMENT,
    LITERAL,
    NAME,
    NAME_REF,
    EXPR_STMT,
    TYPE_SPEC,
    TYPE_ARG,
    TYPE,
    NEW_TYPE,
    DECLARED_VAR,
    TYPE_DECLARATION_STMT,
    RETURN_TYPE_ARG,
    CONST_PARAM,
    CONST_ARG,
    PARAM_LIST,
    QUBIT_LIST,
    FILE_PATH,
    PARAM,
    ARG_LIST,
    GATE_ARG_LIST,
    VERSION,
    VERSION_STRING,
    INCLUDE,
    DECLARATION,
    DESIGNATOR,
    SCALAR_TYPE,
    SCALAR_TYPE_NAME,
    ARRAY_TYPE,
    QUBIT_TYPE,
    EXPRESSION_LIST,
    RETURN_SIGNATURE,
    INT_NUM,
    ALIAS_EXPRESSION,
    SET_EXPRESSION,
    ALIAS_DECLARATION_STATEMENT,
    INDEX_OPERATOR,
    INDEX_KIND,
    INDEXED_IDENTIFIER,
    IDENTIFIER,
    ARRAY_LITERAL,
    HARDWARE_QUBIT,
    CLASSICAL_DECLARATION_STATEMENT,
    ASSIGNMENT_STMT,
    DECLARATION_EXPRESSION,
    CONST_DECLARATION_STATEMENT,
    I_O_DECLARATION_STATEMENT,
    GATE_OPERAND,
    MEASURE_EXPRESSION,
    OLD_STYLE_DECLARATION_STATEMENT,
    QUANTUM_DECLARATION_STATEMENT,
    #[doc(hidden)]
    __LAST,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            O_P_E_N_Q_A_S_M_KW
                | INCLUDE_KW
                | DEF_KW
                | DEFCALGRAMMAR_KW
                | CAL_KW
                | DEFCAL_KW
                | GATE_KW
                | DELAY_KW
                | RESET_KW
                | MEASURE_KW
                | PRAGMA_KW
                | END_KW
                | LET_KW
                | BOX_KW
                | EXTERN_KW
                | CONST_KW
                | BARRIER_KW
                | GPHASE_KW
                | IF_KW
                | ELSE_KW
                | FOR_KW
                | IN_KW
                | WHILE_KW
                | CONTINUE_KW
                | RETURN_KW
                | BREAK_KW
                | INPUT_KW
                | OUTPUT_KW
                | READONLY_KW
                | MUTABLE_KW
                | QREG_KW
                | CREG_KW
                | QUBIT_KW
                | VOID_KW
                | ARRAY_KW
                | FALSE_KW
                | TRUE_KW
        )
    }
    pub fn is_punct(self) -> bool {
        matches!(
            self,
            DOUBLE_PLUS
                | SEMICOLON
                | COMMA
                | L_PAREN
                | R_PAREN
                | L_CURLY
                | R_CURLY
                | L_BRACK
                | R_BRACK
                | L_ANGLE
                | R_ANGLE
                | AT
                | POUND
                | TILDE
                | QUESTION
                | DOLLAR
                | AMP
                | PIPE
                | PLUS
                | STAR
                | SLASH
                | CARET
                | PERCENT
                | UNDERSCORE
                | DOT
                | DOT2
                | DOT3
                | DOT2EQ
                | COLON
                | COLON2
                | EQ
                | EQ2
                | FAT_ARROW
                | BANG
                | NEQ
                | MINUS
                | THIN_ARROW
                | LTEQ
                | GTEQ
                | PLUSEQ
                | MINUSEQ
                | PIPEEQ
                | AMPEQ
                | CARETEQ
                | SLASHEQ
                | STAREQ
                | PERCENTEQ
                | AMP2
                | PIPE2
                | SHL
                | SHR
                | SHLEQ
                | SHREQ
        )
    }
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            INT_NUMBER
                | FLOAT_NUMBER
                | SIMPLE_FLOAT_NUMBER
                | CHAR
                | BYTE
                | STRING
                | BIT_STRING
                | TIMING_FLOAT_NUMBER
                | TIMING_INT_NUMBER
        )
    }
    pub fn is_scalar_type(self) -> bool {
        matches!(
            self,
            FLOAT_TY
                | INT_TY
                | UINT_TY
                | COMPLEX_TY
                | BOOL_TY
                | BIT_TY
                | DURATION_TY
                | STRETCH_TY
                | ANGLE_TY
        )
    }
    pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
        let kw = match ident {
            "OPENQASM" => O_P_E_N_Q_A_S_M_KW,
            "include" => INCLUDE_KW,
            "def" => DEF_KW,
            "defcalgrammar" => DEFCALGRAMMAR_KW,
            "cal" => CAL_KW,
            "defcal" => DEFCAL_KW,
            "gate" => GATE_KW,
            "delay" => DELAY_KW,
            "reset" => RESET_KW,
            "measure" => MEASURE_KW,
            "pragma" => PRAGMA_KW,
            "end" => END_KW,
            "let" => LET_KW,
            "box" => BOX_KW,
            "extern" => EXTERN_KW,
            "const" => CONST_KW,
            "barrier" => BARRIER_KW,
            "gphase" => GPHASE_KW,
            "if" => IF_KW,
            "else" => ELSE_KW,
            "for" => FOR_KW,
            "in" => IN_KW,
            "while" => WHILE_KW,
            "continue" => CONTINUE_KW,
            "return" => RETURN_KW,
            "break" => BREAK_KW,
            "input" => INPUT_KW,
            "output" => OUTPUT_KW,
            "readonly" => READONLY_KW,
            "mutable" => MUTABLE_KW,
            "qreg" => QREG_KW,
            "creg" => CREG_KW,
            "qubit" => QUBIT_KW,
            "void" => VOID_KW,
            "array" => ARRAY_KW,
            "false" => FALSE_KW,
            "true" => TRUE_KW,
            _ => return None,
        };
        Some(kw)
    }
    pub fn from_scalar_type(type_name: &str) -> Option<SyntaxKind> {
        let ty = match type_name {
            "float" => FLOAT_TY,
            "int" => INT_TY,
            "uint" => UINT_TY,
            "complex" => COMPLEX_TY,
            "bool" => BOOL_TY,
            "bit" => BIT_TY,
            "duration" => DURATION_TY,
            "stretch" => STRETCH_TY,
            "angle" => ANGLE_TY,
            _ => return None,
        };
        Some(ty)
    }
    pub fn from_char(c: char) -> Option<SyntaxKind> {
        let tok = match c {
            ';' => SEMICOLON,
            ',' => COMMA,
            '(' => L_PAREN,
            ')' => R_PAREN,
            '{' => L_CURLY,
            '}' => R_CURLY,
            '[' => L_BRACK,
            ']' => R_BRACK,
            '<' => L_ANGLE,
            '>' => R_ANGLE,
            '@' => AT,
            '#' => POUND,
            '~' => TILDE,
            '?' => QUESTION,
            '$' => DOLLAR,
            '&' => AMP,
            '|' => PIPE,
            '+' => PLUS,
            '*' => STAR,
            '/' => SLASH,
            '^' => CARET,
            '%' => PERCENT,
            '_' => UNDERSCORE,
            '.' => DOT,
            ':' => COLON,
            '=' => EQ,
            '!' => BANG,
            '-' => MINUS,
            _ => return None,
        };
        Some(tok)
    }
}
#[macro_export]
macro_rules ! T { [++] => { $ crate :: SyntaxKind :: DOUBLE_PLUS } ; [;] => { $ crate :: SyntaxKind :: SEMICOLON } ; [,] => { $ crate :: SyntaxKind :: COMMA } ; ['('] => { $ crate :: SyntaxKind :: L_PAREN } ; [')'] => { $ crate :: SyntaxKind :: R_PAREN } ; ['{'] => { $ crate :: SyntaxKind :: L_CURLY } ; ['}'] => { $ crate :: SyntaxKind :: R_CURLY } ; ['['] => { $ crate :: SyntaxKind :: L_BRACK } ; [']'] => { $ crate :: SyntaxKind :: R_BRACK } ; [<] => { $ crate :: SyntaxKind :: L_ANGLE } ; [>] => { $ crate :: SyntaxKind :: R_ANGLE } ; [@] => { $ crate :: SyntaxKind :: AT } ; [#] => { $ crate :: SyntaxKind :: POUND } ; [~] => { $ crate :: SyntaxKind :: TILDE } ; [?] => { $ crate :: SyntaxKind :: QUESTION } ; [$] => { $ crate :: SyntaxKind :: DOLLAR } ; [&] => { $ crate :: SyntaxKind :: AMP } ; [|] => { $ crate :: SyntaxKind :: PIPE } ; [+] => { $ crate :: SyntaxKind :: PLUS } ; [*] => { $ crate :: SyntaxKind :: STAR } ; [/] => { $ crate :: SyntaxKind :: SLASH } ; [^] => { $ crate :: SyntaxKind :: CARET } ; [%] => { $ crate :: SyntaxKind :: PERCENT } ; [_] => { $ crate :: SyntaxKind :: UNDERSCORE } ; [.] => { $ crate :: SyntaxKind :: DOT } ; [..] => { $ crate :: SyntaxKind :: DOT2 } ; [...] => { $ crate :: SyntaxKind :: DOT3 } ; [..=] => { $ crate :: SyntaxKind :: DOT2EQ } ; [:] => { $ crate :: SyntaxKind :: COLON } ; [::] => { $ crate :: SyntaxKind :: COLON2 } ; [=] => { $ crate :: SyntaxKind :: EQ } ; [==] => { $ crate :: SyntaxKind :: EQ2 } ; [=>] => { $ crate :: SyntaxKind :: FAT_ARROW } ; [!] => { $ crate :: SyntaxKind :: BANG } ; [!=] => { $ crate :: SyntaxKind :: NEQ } ; [-] => { $ crate :: SyntaxKind :: MINUS } ; [->] => { $ crate :: SyntaxKind :: THIN_ARROW } ; [<=] => { $ crate :: SyntaxKind :: LTEQ } ; [>=] => { $ crate :: SyntaxKind :: GTEQ } ; [+=] => { $ crate :: SyntaxKind :: PLUSEQ } ; [-=] => { $ crate :: SyntaxKind :: MINUSEQ } ; [|=] => { $ crate :: SyntaxKind :: PIPEEQ } ; [&=] => { $ crate :: SyntaxKind :: AMPEQ } ; [^=] => { $ crate :: SyntaxKind :: CARETEQ } ; [/=] => { $ crate :: SyntaxKind :: SLASHEQ } ; [*=] => { $ crate :: SyntaxKind :: STAREQ } ; [%=] => { $ crate :: SyntaxKind :: PERCENTEQ } ; [&&] => { $ crate :: SyntaxKind :: AMP2 } ; [||] => { $ crate :: SyntaxKind :: PIPE2 } ; [<<] => { $ crate :: SyntaxKind :: SHL } ; [>>] => { $ crate :: SyntaxKind :: SHR } ; [<<=] => { $ crate :: SyntaxKind :: SHLEQ } ; [>>=] => { $ crate :: SyntaxKind :: SHREQ } ; [OPENQASM] => { $ crate :: SyntaxKind :: O_P_E_N_Q_A_S_M_KW } ; [include] => { $ crate :: SyntaxKind :: INCLUDE_KW } ; [def] => { $ crate :: SyntaxKind :: DEF_KW } ; [defcalgrammar] => { $ crate :: SyntaxKind :: DEFCALGRAMMAR_KW } ; [cal] => { $ crate :: SyntaxKind :: CAL_KW } ; [defcal] => { $ crate :: SyntaxKind :: DEFCAL_KW } ; [gate] => { $ crate :: SyntaxKind :: GATE_KW } ; [delay] => { $ crate :: SyntaxKind :: DELAY_KW } ; [reset] => { $ crate :: SyntaxKind :: RESET_KW } ; [measure] => { $ crate :: SyntaxKind :: MEASURE_KW } ; [pragma] => { $ crate :: SyntaxKind :: PRAGMA_KW } ; [end] => { $ crate :: SyntaxKind :: END_KW } ; [let] => { $ crate :: SyntaxKind :: LET_KW } ; [box] => { $ crate :: SyntaxKind :: BOX_KW } ; [extern] => { $ crate :: SyntaxKind :: EXTERN_KW } ; [const] => { $ crate :: SyntaxKind :: CONST_KW } ; [barrier] => { $ crate :: SyntaxKind :: BARRIER_KW } ; [gphase] => { $ crate :: SyntaxKind :: GPHASE_KW } ; [if] => { $ crate :: SyntaxKind :: IF_KW } ; [else] => { $ crate :: SyntaxKind :: ELSE_KW } ; [for] => { $ crate :: SyntaxKind :: FOR_KW } ; [in] => { $ crate :: SyntaxKind :: IN_KW } ; [while] => { $ crate :: SyntaxKind :: WHILE_KW } ; [continue] => { $ crate :: SyntaxKind :: CONTINUE_KW } ; [return] => { $ crate :: SyntaxKind :: RETURN_KW } ; [break] => { $ crate :: SyntaxKind :: BREAK_KW } ; [input] => { $ crate :: SyntaxKind :: INPUT_KW } ; [output] => { $ crate :: SyntaxKind :: OUTPUT_KW } ; [readonly] => { $ crate :: SyntaxKind :: READONLY_KW } ; [mutable] => { $ crate :: SyntaxKind :: MUTABLE_KW } ; [qreg] => { $ crate :: SyntaxKind :: QREG_KW } ; [creg] => { $ crate :: SyntaxKind :: CREG_KW } ; [qubit] => { $ crate :: SyntaxKind :: QUBIT_KW } ; [void] => { $ crate :: SyntaxKind :: VOID_KW } ; [array] => { $ crate :: SyntaxKind :: ARRAY_KW } ; [false] => { $ crate :: SyntaxKind :: FALSE_KW } ; [true] => { $ crate :: SyntaxKind :: TRUE_KW } ; [float] => { $ crate :: SyntaxKind :: FLOAT_TY } ; [int] => { $ crate :: SyntaxKind :: INT_TY } ; [uint] => { $ crate :: SyntaxKind :: UINT_TY } ; [complex] => { $ crate :: SyntaxKind :: COMPLEX_TY } ; [bool] => { $ crate :: SyntaxKind :: BOOL_TY } ; [bit] => { $ crate :: SyntaxKind :: BIT_TY } ; [duration] => { $ crate :: SyntaxKind :: DURATION_TY } ; [stretch] => { $ crate :: SyntaxKind :: STRETCH_TY } ; [angle] => { $ crate :: SyntaxKind :: ANGLE_TY } ; [ident] => { $ crate :: SyntaxKind :: IDENT } ; }
pub use T;
