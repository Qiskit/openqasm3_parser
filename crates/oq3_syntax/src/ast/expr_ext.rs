// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Various extension methods to ast Expr Nodes, which are hard to code-generate.
//!
//! These methods should only do simple, shallow tasks related to the syntax of the node itself.

use crate::{
    ast::{
        self,
        operators::{ArithOp, BinaryOp, CmpOp, LogicOp, Ordering},
        support, AstChildren, AstNode,
    },
    AstToken,
    SyntaxKind::*,
    SyntaxToken, T,
};

// FIXME: In OQ3 the control flow things are statements, not expressions
// So, this function may no longer do what it needs to do.
impl ast::Expr {
    pub fn is_block_like(&self) -> bool {
        matches!(
            self,
            // ast::Expr::IfExpr(_)
            //     | ast::Expr::ForExpr(_)
            //     | ast::Expr::WhileExpr(_)
                | ast::Expr::BlockExpr(_)
        )
    }
}

// FIXME: I mysteriously had to make this public when changing flow control to Stmt
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBranch {
    Block(ast::BlockExpr),
    IfStmt(ast::IfStmt),
}

impl From<ast::BlockExpr> for ElseBranch {
    fn from(block_expr: ast::BlockExpr) -> Self {
        Self::Block(block_expr)
    }
}

impl From<ast::IfStmt> for ElseBranch {
    fn from(if_stmt: ast::IfStmt) -> Self {
        Self::IfStmt(if_stmt)
    }
}

// FIXME: Other flow control, such as WhileStmt is in node_ext.rs
// This belongs there. But do we jettison all the extra structs above, or
// fix them and carry them along?
impl ast::IfStmt {
    pub fn condition(&self) -> Option<ast::Expr> {
        // If the condition is a BlockExpr, check if the then body is missing.
        // If it is assume the condition is the expression that is missing instead.
        let mut exprs = support::children(self.syntax());
        let first = exprs.next();
        match first {
            Some(ast::Expr::BlockExpr(_)) => exprs.next().and(first),
            first => first,
        }
    }

    pub fn then_branch(&self) -> Option<ast::BlockExpr> {
        match support::children(self.syntax()).nth(1)? {
            ast::Expr::BlockExpr(block) => Some(block),
            _ => None,
        }
    }

    // FIXME: this may have supported more than what is below.
    // pub fn else_branch(&self) -> Option<ElseBranch> {
    //     match support::children(self.syntax()).nth(2)? {
    //         ast::Expr::BlockExpr(block) => Some(ElseBranch::Block(block)),
    //         ast::Expr::IfExpr(elif) => Some(ElseBranch::IfExpr(elif)),
    //         _ => None,
    //     }
    // }

    pub fn else_branch(&self) -> Option<ast::BlockExpr> {
        match support::children(self.syntax()).nth(2)? {
            ast::Expr::BlockExpr(block) => Some(block),
            _ => None,
        }
    }
}

// FIXME: These tests broke now that IfExpr is a Stmt (Item really)
// But they should be fixed.
// #[test]
// fn if_block_condition() {
//     let parse = ast::SourceFile::parse(
//         r#"
//         def test() {
//             if { true } { "if" }
//             else if { false } { "first elif" }
//             else if true { "second elif" }
//             else if (true) { "third elif" }
//             else { "else" }
//         }
//         "#,
//     );
//     let if_ = parse.tree().syntax().descendants().find_map(ast::IfExpr::cast).unwrap();
//     assert_eq!(if_.then_branch().unwrap().syntax().text(), r#"{ "if" }"#);
//     let elif = match if_.else_branch().unwrap() {
//         ElseBranch::IfExpr(elif) => elif,
//         ElseBranch::Block(_) => panic!("should be `else if`"),
//     };
//     assert_eq!(elif.then_branch().unwrap().syntax().text(), r#"{ "first elif" }"#);
//     let elif = match elif.else_branch().unwrap() {
//         ElseBranch::IfExpr(elif) => elif,
//         ElseBranch::Block(_) => panic!("should be `else if`"),
//     };
//     assert_eq!(elif.then_branch().unwrap().syntax().text(), r#"{ "second elif" }"#);
//     let elif = match elif.else_branch().unwrap() {
//         ElseBranch::IfExpr(elif) => elif,
//         ElseBranch::Block(_) => panic!("should be `else if`"),
//     };
//     assert_eq!(elif.then_branch().unwrap().syntax().text(), r#"{ "third elif" }"#);
//     let else_ = match elif.else_branch().unwrap() {
//         ElseBranch::Block(else_) => else_,
//         ElseBranch::IfExpr(_) => panic!("should be `else`"),
//     };
//     assert_eq!(else_.syntax().text(), r#"{ "else" }"#);
// }

// #[test]
// fn if_condition_with_if_inside() {
//     let parse = ast::SourceFile::parse(
//         r#"
//         fn test() {
//             if if true { true } else { false } { "if" }
//             else { "else" }
//         }
//         "#,
//     );
//     let if_ = parse.tree().syntax().descendants().find_map(ast::IfExpr::cast).unwrap();
//     assert_eq!(if_.then_branch().unwrap().syntax().text(), r#"{ "if" }"#);
//     let else_ = match if_.else_branch().unwrap() {
//         ElseBranch::Block(else_) => else_,
//         ElseBranch::IfExpr(_) => panic!("should be `else`"),
//     };
//     assert_eq!(else_.syntax().text(), r#"{ "else" }"#);
// }

// FIXME: use fn lhs instead. This was for testing GJL
// impl ast::BinExpr {
//     pub fn get_lhs(&self) -> Option<ast::Expr> {
//         let c1 = self.syntax().first_child();
//         match c1 {
//             Some(c1) => ast::Expr::cast(c1),
//             None => None
//         }
//     }
// }

impl ast::BinExpr {
    pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOp)> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| {
            #[rustfmt::skip]
            let bin_op = match c.kind() {
                T![||] => BinaryOp::LogicOp(LogicOp::Or),
                T![&&] => BinaryOp::LogicOp(LogicOp::And),

                T![==] => BinaryOp::CmpOp(CmpOp::Eq { negated: false }),
                T![!=] => BinaryOp::CmpOp(CmpOp::Eq { negated: true }),
                T![<=] => BinaryOp::CmpOp(CmpOp::Ord { ordering: Ordering::Less,    strict: false }),
                T![>=] => BinaryOp::CmpOp(CmpOp::Ord { ordering: Ordering::Greater, strict: false }),
                T![<]  => BinaryOp::CmpOp(CmpOp::Ord { ordering: Ordering::Less,    strict: true }),
                T![>]  => BinaryOp::CmpOp(CmpOp::Ord { ordering: Ordering::Greater, strict: true }),

                T![+]  => BinaryOp::ArithOp(ArithOp::Add),
                T![*]  => BinaryOp::ArithOp(ArithOp::Mul),
                T![-]  => BinaryOp::ArithOp(ArithOp::Sub),
                T![/]  => BinaryOp::ArithOp(ArithOp::Div),
                T![%]  => BinaryOp::ArithOp(ArithOp::Rem),
                T![<<] => BinaryOp::ArithOp(ArithOp::Shl),
                T![>>] => BinaryOp::ArithOp(ArithOp::Shr),
                T![^]  => BinaryOp::ArithOp(ArithOp::BitXor),
                T![|]  => BinaryOp::ArithOp(ArithOp::BitOr),
                T![&]  => BinaryOp::ArithOp(ArithOp::BitAnd),

                T![=]   => BinaryOp::Assignment { op: None },
                T![+=]  => BinaryOp::Assignment { op: Some(ArithOp::Add) },
                T![*=]  => BinaryOp::Assignment { op: Some(ArithOp::Mul) },
                T![-=]  => BinaryOp::Assignment { op: Some(ArithOp::Sub) },
                T![/=]  => BinaryOp::Assignment { op: Some(ArithOp::Div) },
                T![%=]  => BinaryOp::Assignment { op: Some(ArithOp::Rem) },
                T![<<=] => BinaryOp::Assignment { op: Some(ArithOp::Shl) },
                T![>>=] => BinaryOp::Assignment { op: Some(ArithOp::Shr) },
                T![^=]  => BinaryOp::Assignment { op: Some(ArithOp::BitXor) },
                T![|=]  => BinaryOp::Assignment { op: Some(ArithOp::BitOr) },
                T![&=]  => BinaryOp::Assignment { op: Some(ArithOp::BitAnd) },

                _ => return None,
            };
            Some((c, bin_op))
        })
    }

    pub fn op_kind(&self) -> Option<BinaryOp> {
        self.op_details().map(|t| t.1)
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.op_details().map(|t| t.0)
    }

    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }

    pub fn rhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn sub_exprs(&self) -> (Option<ast::Expr>, Option<ast::Expr>) {
        let mut children = support::children(self.syntax());
        let first = children.next();
        let second = children.next();
        (first, second)
    }
}

// FIXME: The r-a implementations always return nodes for the all tokens, eg COLONs.
// Maybe include this later. For now, we only need expressions.
impl ast::RangeExpr {
    /// Return the start, step, and stop expressions as a three-tuple
    pub fn start_step_stop(&self) -> (Option<ast::Expr>, Option<ast::Expr>, Option<ast::Expr>) {
        let mut children = support::children(self.syntax());
        let first = children.next();
        let second = children.next();
        let third = children.next();
        if third.is_none() {
            // start:stop
            (first, third, second)
        } else {
            // start:step:stop
            (first, second, third)
        }
    }
}

impl ast::IndexExpr {
    pub fn base(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    pub fn index(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

pub enum ArrayExprKind {
    Repeat {
        initializer: Option<ast::Expr>,
        repeat: Option<ast::Expr>,
    },
    ElementList(AstChildren<ast::Expr>),
}

impl ast::ArrayExpr {
    pub fn kind(&self) -> ArrayExprKind {
        if self.is_repeat() {
            ArrayExprKind::Repeat {
                initializer: support::children(self.syntax()).next(),
                repeat: support::children(self.syntax()).nth(1),
            }
        } else {
            ArrayExprKind::ElementList(support::children(self.syntax()))
        }
    }

    fn is_repeat(&self) -> bool {
        self.semicolon_token().is_some()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    String(ast::String),
    BitString(ast::BitString),
    IntNumber(ast::IntNumber),
    FloatNumber(ast::FloatNumber),
    SimpleFloatNumber(ast::SimpleFloatNumber),
    TimingFloatNumber(ast::TimingFloatNumber),
    Char(ast::Char),
    Byte(ast::Byte),
    Bool(bool),
}

// only for OQ3
// This is not for use in the expression tree.
// Literal strings in OQ3 occur only in a few contexts.
impl ast::FilePath {
    pub fn token(&self) -> SyntaxToken {
        self.syntax()
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }

    // returns ast::String
    pub fn string(&self) -> Option<ast::String> {
        ast::String::cast(self.token())
    }

    // FIXME: there is a better idiomatic way
    pub fn to_string(&self) -> Option<String> {
        Some(self.string()?.value()?.to_string())
    }
}

impl ast::Version {
    pub fn token(&self) -> SyntaxToken {
        self.syntax()
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }

    pub fn version(&self) -> Option<ast::FloatNumber> {
        let token = self.token();
        if let Some(t) = ast::FloatNumber::cast(token) {
            return Some(t);
        }
        None
    }
}

impl ast::Literal {
    pub fn token(&self) -> SyntaxToken {
        self.syntax()
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }

    pub fn kind(&self) -> LiteralKind {
        let token = self.token();
        if let Some(t) = ast::IntNumber::cast(token.clone()) {
            return LiteralKind::IntNumber(t);
        }
        if let Some(t) = ast::FloatNumber::cast(token.clone()) {
            return LiteralKind::FloatNumber(t);
        }
        if let Some(t) = ast::SimpleFloatNumber::cast(token.clone()) {
            return LiteralKind::SimpleFloatNumber(t);
        }
        if let Some(t) = ast::TimingFloatNumber::cast(token.clone()) {
            return LiteralKind::TimingFloatNumber(t);
        }
        if let Some(t) = ast::String::cast(token.clone()) {
            return LiteralKind::String(t);
        }
        if let Some(t) = ast::BitString::cast(token.clone()) {
            return LiteralKind::BitString(t);
        }
        if let Some(t) = ast::Char::cast(token.clone()) {
            return LiteralKind::Char(t);
        }
        if let Some(t) = ast::Byte::cast(token.clone()) {
            return LiteralKind::Byte(t);
        }

        match token.kind() {
            T![true] => LiteralKind::Bool(true),
            T![false] => LiteralKind::Bool(false),
            _ => unreachable!(),
        }
    }
}

impl ast::BlockExpr {
    /// false if the block is an intrinsic part of the syntax and can't be
    /// replaced with arbitrary expression.
    ///
    /// ```not_rust
    /// fn foo() { not_stand_alone }
    /// const FOO: () = { stand_alone };
    /// ```
    pub fn is_standalone(&self) -> bool {
        let parent = match self.syntax().parent() {
            Some(it) => it,
            None => return true,
        };
        match parent.kind() {
            FOR_STMT | IF_STMT => parent
                .children()
                .find(|it| ast::Expr::can_cast(it.kind()))
                .map_or(true, |it| it == *self.syntax()),
            WHILE_STMT => false,
            _ => true,
        }
    }
}

impl ast::ArgList {
    pub fn altargs(&self) -> Option<ast::Name> {
        let mut children = support::children(self.syntax());
        //        children.next();
        children.next()
    }
}

// ? is this used?
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub  enum IndexedIdentifier {
//     Param(ast::Param),
//     IndexExpr(ast::IndexExpr),
// }

impl ast::IndexedIdentifier {
    pub fn identifier(&self) -> Option<ast::Identifier> {
        support::child(&self.syntax)
    }
}

// impl ast::AssignmentStmt {
//     pub fn indexed_identifier(&self) -> Option<ast::IndexedIdentifier> {
//         support::child(&self.syntax)
//     }
// }

impl ast::AssignmentStmt {
    pub fn rhs(&self) -> Option<ast::Expr> {
        let mut children: AstChildren<ast::Expr> = support::children(self.syntax());
        // If there is one child Expr, then the lhs is a Name, rhs is the Expr.
        // If there are two child Expr's, the lhs is an Expr, and so is the rhs.
        let expr1 = children.next();
        let expr2 = children.next();
        if expr2.is_some() {
            expr2
        } else {
            expr1
        }
    }
}

impl ast::GateCallStmt {
    // This may be redundant
    pub fn name(&self) -> Option<ast::Name> {
        support::child(&self.syntax)
    }

    pub fn identifier(&self) -> Option<ast::Identifier> {
        let maybe_id = support::children(self.syntax()).next();
        match maybe_id {
            Some(ast::Expr::Identifier(ident)) => Some(ident),
            _ => None,
        }
    }
}

// Angles are optional, but they come before qubits.
// So we need a bit of logic.
//
// Another solution would be to duplicate the types in the ungram file
// that generate types in nodes.rs. And in expressions.rs, etc. change
// the kinds, and add some kinds to ast_source.rs.
impl ast::Gate {
    fn angles_and_or_qubits(&self) -> (Option<ast::ParamList>, Option<ast::ParamList>) {
        let mut children = support::children(self.syntax());
        let qubits_or_angles = children.next();
        let qubits_or_none = children.next();
        (qubits_or_angles, qubits_or_none)
    }

    pub fn angle_params(&self) -> Option<ast::ParamList> {
        let (qubits_or_angles, qubits_or_none) = self.angles_and_or_qubits();
        if qubits_or_none.is_none() {
            qubits_or_none
        } else {
            qubits_or_angles
        }
    }

    pub fn qubit_params(&self) -> Option<ast::ParamList> {
        let (qubits_or_angles, qubits_or_none) = self.angles_and_or_qubits();
        if qubits_or_none.is_none() {
            qubits_or_angles
        } else {
            qubits_or_none
        }
    }
}
