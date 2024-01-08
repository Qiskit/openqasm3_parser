// Copyright contributors to the openqasm-parser project

use crate::asg::{Expr, Program, Stmt, TExpr};
use crate::symbols::{SymbolIdResult, SymbolTable};



// Thus far, everything below is meant to apply any function
// FnMut(&SymbolIdResult) to all possible places in the ASG.

// FIXME: use more traits to generalize this code to
// walk for more things than just SymbolIdResult.
// I tried to do this, but was stopped by lack of sealed traits.
// Tried all the tricks I could find online. No luck.

// This struct is used to apply `func` to all `SymbolIdResult` in the ASG.
// We intend that T : FnMut(&SymbolIdResult), but the bound is not needed here.
// We want the `FnMut` in the trait bound so that the compiler
// knows the function at compile time and can optimize.
struct SymContext<'a, T> {
    // : FnMut(&SymbolIdResult)> {
    func: T,
    symtab: &'a SymbolTable,
}

trait WalkSymbols<T> {
    fn walk_symbols(&self, context: &mut SymContext<T>);
}

// Trick to emulate an alias for a trait bound
// This is like:
// type SymTrait = FnMut(&SymbolIdResult);
trait SymTrait: FnMut(&SymbolIdResult) {}
impl<T> SymTrait for T where T: FnMut(&SymbolIdResult) {}

impl<T: SymTrait> WalkSymbols<T> for Program {
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        self.stmts().walk_symbols(context);
    }
}

impl<V, T: SymTrait> WalkSymbols<T> for &Vec<V>
where
    V: WalkSymbols<T>,
{
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        self.iter().for_each(|s| s.walk_symbols(context));
    }
}

impl<V, T: SymTrait> WalkSymbols<T> for Option<V>
where
    V: WalkSymbols<T>,
{
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        if self.is_some() {
            self.as_ref().unwrap().walk_symbols(context);
        }
    }
}

impl<V, T: SymTrait> WalkSymbols<T> for Box<V>
where
    V: WalkSymbols<T>,
{
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        self.as_ref().walk_symbols(context);
    }
}

impl<T: SymTrait> WalkSymbols<T> for TExpr {
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        self.expression().walk_symbols(context);
    }
}

// The impl's for Stmt and Expr are the only two that actually apply
// the function
impl<T: SymTrait> WalkSymbols<T> for Stmt {
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        match self {
            Stmt::DeclareClassical(decl) => {
                (context.func)(decl.name());
                decl.initializer().walk_symbols(context);
            }
            Stmt::DeclareQuantum(decl) => {
                (context.func)(decl.name());
            }
            Stmt::Assignment(assign) => {
                assign.rvalue().walk_symbols(context);
            }
            _ => (),
        }
    }
}

impl<T: SymTrait> WalkSymbols<T> for Expr {
    fn walk_symbols(&self, context: &mut SymContext<T>) {
        if let Expr::Identifier(ident) = self {
            (context.func)(ident.symbol())
        }
    }
}

pub fn count_symbol_errors(program: &Program, symtab: &SymbolTable) -> usize {
    let mut okcount = 0;
    let mut context = SymContext {
        func: |x: &SymbolIdResult| {
            if x.is_err() {
                okcount += 1
            }
        },
        symtab,
    };
    program.walk_symbols(&mut context);
    okcount
}
