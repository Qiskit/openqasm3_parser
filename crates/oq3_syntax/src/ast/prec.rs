// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! Precedence representation.

use crate::{
    ast::{self, Expr, HasArgList},
    match_ast, AstNode, SyntaxNode,
};

impl Expr {
    // Implementation is based on
    // - https://doc.rust-lang.org/reference/expressions.html#expression-precedence
    // - https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    // - rustc source, including, but not limited to
    //   - https://github.com/rust-lang/rust/blob/b6852428a8ea9728369b64b9964cad8e258403d3/compiler/rustc_ast/src/util/parser.rs#L296

    /// Returns `true` if `self` would need to be wrapped in parentheses given that its parent is `parent`.
    pub fn needs_parens_in(&self, parent: SyntaxNode) -> bool {
        match_ast! {
            match parent {
                ast::Expr(e) => self.needs_parens_in_expr(&e),
                ast::Stmt(e) =>      self.needs_parens_in_stmt(Some(&e)),
                ast::BlockExpr(_) => self.needs_parens_in_stmt(None),
                ast::ArgList(_) => false,
                _ => false,
            }
        }
    }

    fn needs_parens_in_expr(&self, parent: &Expr) -> bool {
        // Special-case block weirdness
        if parent.child_is_followed_by_a_block() {
            use Expr::*;
            match self {
                // Cases like `if return {}` (need parens or else `{}` is returned, instead of being `if`'s body)
                ReturnExpr(e) if e.expr().is_none() => return true,
                //                BreakExpr(e) if e.expr().is_none() => return true, oq3 has no expr after break
                // Same but with `..{}`
                // FIXME: Verify that branch for RangeExpr is un-needed for OQ3
                //                RangeExpr(e) if matches!(e.end(), Some(BlockExpr(..))) => return true,
                _ => {}
            }
        }

        // Special-case `return.f()`
        if self.is_ret_like_with_no_value() && parent.is_postfix() {
            return false;
        }

        if self.is_paren_like()
            || parent.is_paren_like()
            || self.is_prefix() && (parent.is_prefix() || !self.is_ordered_before(parent))
            || self.is_postfix() && (parent.is_postfix() || self.is_ordered_before(parent))
        {
            return false;
        }

        let (left, right, inv) = match self.is_ordered_before(parent) {
            true => (self, parent, false),
            false => (parent, self, true),
        };

        let (_, left_right_bp) = left.binding_power();
        let (right_left_bp, _) = right.binding_power();

        (left_right_bp < right_left_bp) ^ inv
    }

    fn needs_parens_in_stmt(&self, _stmt: Option<&ast::Stmt>) -> bool {
        use Expr::*;

        // Prevent false-positives in cases like `fn x() -> u8 { ({ 0 } + 1) }`,
        // `{ { 0 } + 1 }` won't parse -- `{ 0 }` would be parsed as a self-contained stmt,
        // leaving `+ 1` as a parse error.
        let mut innermost = self.clone();
        loop {
            let next = match &innermost {
                BinExpr(e) => e.lhs(),
                CallExpr(e) => e.expr(),
                CastExpression(e) => e.expr(),
                IndexExpr(e) => e.expr(),
                IndexedIdentifier(_) => break,
                _ => break,
            };

            if let Some(next) = next {
                innermost = next;
                if !innermost.requires_semi_to_be_stmt() {
                    return true;
                }
            } else {
                break;
            }
        }
        false
    }

    // FIXME: LetExpr renamed to LetStmt has been moved to enum Stmt
    /// Returns left and right so-called "binding powers" of this expression.
    fn binding_power(&self) -> (u8, u8) {
        use ast::{ArithOp::*, BinaryOp::*, Expr::*, LogicOp::*};

        match self {
            // (0, 0)   -- paren-like/nullary
            // (0, N)   -- prefix
            // (N, 0)   -- postfix
            // (N, N)   -- infix, requires parens
            // (N, N+1) -- infix, left to right associative
            // (N+1, N) -- infix, right to left associative
            // N is odd
            //
            ReturnExpr(_) => (0, 1),
            RangeExpr(_) => (5, 5),
            BinExpr(e) => {
                // Return a dummy value if we don't know the op
                let Some(op) = e.op_kind() else { return (0, 0) };
                match op {
                    Assignment { .. } => (4, 3),
                    //
                    // Ranges are here in order :)
                    //
                    LogicOp(op) => match op {
                        Or => (7, 8),
                        And => (9, 10),
                    },
                    CmpOp(_) => (11, 11),
                    ArithOp(op) => match op {
                        BitOr => (13, 14),
                        BitXor => (15, 16),
                        BitAnd => (17, 18),
                        Shl | Shr => (19, 20),
                        Add | Sub => (21, 22),
                        Mul | Div | Rem => (23, 24),
                    },
                }
            }
            ArrayLiteral(_) => (0, 0), // These need to be checked
            MeasureExpression(_) => (0, 0),
            BoxExpr(_) | PrefixExpr(_) => (0, 27),
            GPhaseCallExpr(_)
            | GateCallExpr(_)
            | ModifiedGateCallExpr(_)
            | CallExpr(_)
            | CastExpression(_)
            | IndexExpr(_)
            | IndexedIdentifier(_) => (29, 0),
            ArrayExpr(_) | Literal(_) | ParenExpr(_) | Identifier(_) | HardwareQubit(_)
            | BlockExpr(_) => (0, 0),
        }
    }

    fn is_paren_like(&self) -> bool {
        matches!(self.binding_power(), (0, 0))
    }

    fn is_prefix(&self) -> bool {
        matches!(self.binding_power(), (0, 1..))
    }

    fn is_postfix(&self) -> bool {
        matches!(self.binding_power(), (1.., 0))
    }

    /// Returns `true` if this expression can't be a standalone statement.
    fn requires_semi_to_be_stmt(&self) -> bool {
        use Expr::*;
        !matches!(self, BlockExpr(..))
    }

    /// Returns true if self is one of `return`, `break`, `continue` or `yield` with **no associated value**.
    fn is_ret_like_with_no_value(&self) -> bool {
        use Expr::*;
        match self {
            ReturnExpr(e) => e.expr().is_none(),
            _ => false,
        }
    }

    fn is_ordered_before(&self, other: &Expr) -> bool {
        use Expr::*;

        return order(self) < order(other);

        // FIXME: LetExpr renamed to LetStmt has been moved to enum Stmt
        /// Returns text range that can be used to compare two expression for order (which goes first).
        fn order(this: &Expr) -> rowan::TextSize {
            // For non-paren-like operators: get the operator itself
            let token = match this {
                RangeExpr(_) => None,
                PrefixExpr(e) => e.op_token(),
                BinExpr(e) => e.op_token(),
                BoxExpr(e) => e.box_token(),
                CallExpr(e) => e.arg_list().and_then(|args| args.l_paren_token()),
                // FIXME: next line is quick fix, likely wrong
                GPhaseCallExpr(_) => None,
                GateCallExpr(_) => None,
                ModifiedGateCallExpr(_) => None,
                CastExpression(e) => e.l_paren_token(),
                //                IndexExpr(e) => e.l_brack_token(),
                // The bracket in IndexExpr is now absorbed in IndexOperator
                IndexExpr(_e) => todo!(),
                IndexedIdentifier(_e) => todo!(),
                ReturnExpr(e) => e.return_token(),
                ArrayLiteral(_) => todo!(),
                MeasureExpression(_) => todo!(),
                ArrayExpr(_) | Literal(_) | ParenExpr(_) | Identifier(_) | HardwareQubit(_)
                | BlockExpr(_) => None,
            };
            token
                .map(|t| t.text_range())
                .unwrap_or_else(|| this.syntax().text_range())
                .start()
        }
    }

    fn child_is_followed_by_a_block(&self) -> bool {
        use Expr::*;

        match self {
            ArrayExpr(_)
            | BlockExpr(_)
                | CallExpr(_)
                | GPhaseCallExpr(_)
                | GateCallExpr(_)
                | ModifiedGateCallExpr(_)
            | CastExpression(_)
            | IndexExpr(_)
            | IndexedIdentifier(_)
            | Literal(_)
            | Identifier(_)
            | HardwareQubit(_)
            | ParenExpr(_) => false,

            // For BinExpr and RangeExpr this is technically wrong -- the child can be on the left...
            PrefixExpr(_) | BinExpr(_) | RangeExpr(_) | BoxExpr(_) | ReturnExpr(_) => self
                .syntax()
                .parent()
                .and_then(Expr::cast)
                .map(|e| e.child_is_followed_by_a_block())
                .unwrap_or(false),
            ArrayLiteral(_) => todo!(),
            MeasureExpression(_) => todo!(),
        }
    }
}
