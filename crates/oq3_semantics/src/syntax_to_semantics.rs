// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// Build the ASG defined in asg.rs from the syntactic AST
// constructed in the crate oq3_syntax.
//
// In the filename syntax_to_semantics.rs, "syntax" is a noun and "semantics" is a noun.
// I'd rather have syntax_to_semantic.rs, but that would make the latter an adjective.
// And syntactic_to_semantic.rs doesn't sound right.

use std;
use std::mem::replace;
use std::path::PathBuf;

use crate::asg;
use crate::types;
use crate::types::{ArrayDims, IsConst, Type};

use crate::context::Context;
use crate::semantic_error::{SemanticErrorKind::*, SemanticErrorList};
use crate::symbols::{ScopeType, SymbolErrorTrait, SymbolIdResult, SymbolTable};
use oq3_source_file::{SourceFile, SourceString, SourceTrait};
use oq3_syntax::ast as synast; // Syntactic AST

use crate::with_scope;

use crate::utils::type_name_of; // for debugging

// traits
use synast::{HasArgList, HasModuleItem, HasName, HasTextName};

pub struct ParseResult<T: SourceTrait> {
    syntax_result: T, // syntax tree and errors
    context: Context, // semantic asg and errors
    have_syntax_errors: bool,
}

impl<T: SourceTrait> ParseResult<T> {
    pub fn any_syntax_errors(&self) -> bool {
        self.have_syntax_errors
    }

    pub fn any_semantic_errors(&self) -> bool {
        self.context.errors().any_semantic_errors()
    }

    pub fn any_errors(&self) -> bool {
        self.any_syntax_errors() || self.any_semantic_errors()
    }

    pub fn take_context(self) -> Context {
        self.context
    }

    pub fn program(&self) -> &asg::Program {
        self.context.program()
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        self.context.symbol_table()
    }

    pub fn syntax_result(&self) -> &T {
        &self.syntax_result
    }
}

impl ParseResult<SourceFile> {
    /// Prints any errors recorded during lexing, parsing, and analysis.
    /// This reads the source again into a string. It might be
    /// more efficient to call the method `any_errors` first.
    pub fn print_errors(&self) {
        self.syntax_result.print_syntax_errors();
        self.context.errors().print_errors();
    }
}

impl ParseResult<SourceString> {
    pub fn print_errors(&self) {
        let synres = &self.syntax_result();
        synres.print_syntax_errors();
        self.context
            .errors()
            .print_errors_no_file(synres.fake_file_path(), synres.source());
    }
}

/// Parse string containing source to semantic ASG.
/// Fake file name is used for printing diagnostics.
pub fn parse_source_string<T: ToString>(
    source: T,
    fake_file_path: Option<&str>,
) -> ParseResult<SourceString> {
    let parsed_source = oq3_source_file::parse_source_string(source, fake_file_path);
    analyze_source(parsed_source)
}

/// Parse source file to semantic ASG
pub fn parse_source_file(file_path: &PathBuf) -> ParseResult<SourceFile> {
    let parsed_source = oq3_source_file::parse_source_file(file_path);
    analyze_source(parsed_source)
}

fn analyze_source<T: SourceTrait>(parsed_source: T) -> ParseResult<T> {
    let file_path = parsed_source.file_path();
    let context = Context::new(file_path.clone());
    if parsed_source.any_parse_errors() {
        // on syntax errors, do not continue with semantic analysis.
        return ParseResult {
            syntax_result: parsed_source,
            context,
            have_syntax_errors: true,
        };
    }
    let errors = SemanticErrorList::new(file_path);
    let (mut context, errors) = syntax_to_semantic(&parsed_source, context, errors);
    let _ = replace(&mut context.semantic_errors, errors);
    ParseResult {
        syntax_result: parsed_source,
        context: context.clone(),
        have_syntax_errors: false,
    }
}

// FIXME: it is probably possible to encapsulate the manipulations of (context, errors).
pub fn syntax_to_semantic<T: SourceTrait>(
    parsed_source: &T,
    mut context: Context,
    errors: SemanticErrorList,
) -> (Context, SemanticErrorList) {
    let parse_tree = parsed_source.syntax_ast().tree();
    let mut included_iter = parsed_source.included().iter();
    let save_errors = replace(&mut context.semantic_errors, errors);
    for parse_item in parse_tree.statements() {
        let stmt = match parse_item {
            // Include does not go in the ASG, instead it is evaluated.
            // So we include the parsed code, collect errors, and return `None`.
            synast::Stmt::Item(synast::Item::Include(include)) => {
                // Get SourceFile object with syntax AST for the next included file.
                let included_parsed_source = included_iter.next().unwrap();
                // Empty list for possible semantic errors in the included file.
                let mut errors_in_included =
                    SemanticErrorList::new(included_parsed_source.file_path().clone());
                // The following path is likely never taken
                if context.symbol_table().current_scope_type() != ScopeType::Global {
                    context.insert_error(IncludeNotInGlobalScopeError, &include);
                }
                // Call this function recursively passing the new, empty, storage for errors.
                // Note that `errors_in_included` will be swapped into `context` upon entering `syntax_to_semantic`.
                (context, errors_in_included) =
                    syntax_to_semantic(included_parsed_source, context, errors_in_included);
                // Just before exiting the previous call, `errors_in_included` and `errors` are swapped again in `context`.
                // Push the newly-populated list of errors onto the list of included errors in `context`, which now
                // holds `errors`, the list passed in the current call to this `syntax_to_semantic`. And `errors`
                // corresponds to the source in which `include` was encountered.
                context.push_included(errors_in_included);
                // Return `None` because have evaluated (and removed)the `include` statement.
                None
            }

            synast::Stmt::Item(item) => from_item(item, &mut context),

            synast::Stmt::ExprStmt(expr_stmt) => from_expr_stmt(expr_stmt, &mut context),
        };
        if let Some(stmt) = stmt {
            context.program.insert_stmt(stmt)
        }
    }

    // Upon entering the current call, the errors in `context` were swapped out and replaced with `errors`.
    // This `errors` was also passed in, in the current call.
    // Now retrieve `errors` from `context` and restore `saved_errors` to `context`.
    // And return `errors`, which was populated during the current call.
    // Suppose the current call to `syntax_to_semantic` was made while processing an `include` statement....
    let errors = replace(&mut context.semantic_errors, save_errors);
    (context, errors)
}

fn from_expr_stmt(expr_stmt: synast::ExprStmt, context: &mut Context) -> Option<asg::Stmt> {
    let expr = from_expr(expr_stmt.expr().unwrap(), context);
    expr.map_or_else(
        || panic!("expr::ExprStmt is None"),
        |ex| Some(asg::Stmt::ExprStmt(ex)),
    )
}

fn from_expr(expr: synast::Expr, context: &mut Context) -> Option<asg::TExpr> {
    match expr {
        synast::Expr::ParenExpr(paren_expr) => from_expr(paren_expr.expr().unwrap(), context),
        synast::Expr::BinExpr(bin_expr) => {
            let synast_op = bin_expr.op_kind().unwrap();
            let left_syn = bin_expr.lhs().unwrap();
            let right_syn = bin_expr.rhs().unwrap();

            let op = from_binary_op(synast_op);
            let left = from_expr(left_syn, context).unwrap();
            let right = from_expr(right_syn, context).unwrap();

            Some(asg::BinaryExpr::new_texpr_with_cast(op, left, right))
        }

        synast::Expr::Literal(ref literal) => from_literal(literal),

        synast::Expr::Identifier(identifier) => {
            let (astidentifier, typ) = ast_identifier(&identifier, context);
            Some(astidentifier.to_texpr(typ))
        }

        synast::Expr::HardwareQubit(hwq) => Some(ast_hardware_qubit(&hwq).to_texpr()),

        synast::Expr::RangeExpr(range_expr) => {
            let (start, step, stop) = range_expr.start_step_stop();
            let start = from_expr(start.unwrap(), context).unwrap();
            let stop = from_expr(stop.unwrap(), context).unwrap();
            let step = step.and_then(|step| from_expr(step, context));
            let range = asg::Range::new(start, step, stop);
            Some(range.to_texpr(Type::Range))
        }

        synast::Expr::IndexExpr(index_expr) => {
            let expr = from_expr(index_expr.expr().unwrap(), context);
            let index = from_index_operator(index_expr.index_operator().unwrap(), context);
            Some(asg::IndexExpression::new(expr.unwrap(), index).to_texpr())
        }

        synast::Expr::IndexedIdentifier(indexed_identifier) => {
            let (indexed_identifier, _typ) = ast_indexed_identifier(&indexed_identifier, context);
            Some(indexed_identifier.to_texpr())
        }

        synast::Expr::MeasureExpression(ref measure_expr) => {
            let gate_operand = measure_expr.gate_operand().unwrap(); // FIXME: check this
            let gate_operand_asg = from_gate_operand(gate_operand, context);
            Some(asg::MeasureExpression::new(gate_operand_asg).to_texpr())
        }

        // Everything else is not yet implemented
        _ => {
            println!("Expression not supported {:?}", expr);
            None
        }
    }
}

fn from_gate_operand(gate_operand: synast::GateOperand, context: &mut Context) -> asg::TExpr {
    match gate_operand {
        synast::GateOperand::HardwareQubit(ref hwq) => {
            asg::GateOperand::HardwareQubit(ast_hardware_qubit(hwq)).to_texpr(Type::HardwareQubit)
        }
        synast::GateOperand::Identifier(identifier) => {
            let (astidentifier, typ) = ast_identifier(&identifier, context);
            asg::GateOperand::Identifier(astidentifier).to_texpr(typ)
        }
        synast::GateOperand::IndexedIdentifier(indexed_identifier) => {
            let (indexed_identifier, typ) = ast_indexed_identifier(&indexed_identifier, context);
            asg::GateOperand::IndexedIdentifier(indexed_identifier).to_texpr(typ)
        }
    }
}

fn from_index_operator(
    index_op: synast::IndexOperator,
    context: &mut Context,
) -> asg::IndexOperator {
    match index_op.index_kind().unwrap() {
        synast::IndexKind::SetExpression(set_expression) => {
            asg::IndexOperator::SetExpression(from_set_expression(set_expression, context))
        }

        synast::IndexKind::ExpressionList(expression_list) => {
            asg::IndexOperator::ExpressionList(from_expression_list(expression_list, context))
        }
    }
}

fn from_set_expression(
    set_expression: synast::SetExpression,
    context: &mut Context,
) -> asg::SetExpression {
    asg::SetExpression::new(inner_expression_list(
        set_expression.expression_list().unwrap(),
        context,
    ))
}

fn from_expression_list(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> asg::ExpressionList {
    asg::ExpressionList::new(inner_expression_list(expression_list, context))
}

#[allow(clippy::filter_map_identity)]
fn inner_expression_list(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> Vec<asg::TExpr> {
    expression_list
        .exprs()
        .map(|x| from_expr(x, context))
        .filter_map(|x| x) // filter None and unwrap
        .collect::<Vec<_>>()
}

fn from_binary_op(synast_op: synast::BinaryOp) -> asg::BinaryOp {
    match synast_op {
        synast::BinaryOp::ArithOp(arith_op) => {
            use asg::BinaryOp::ArithOp;
            use synast::ArithOp::*;
            match arith_op {
                Add => ArithOp(asg::ArithOp::Add),
                Mul => ArithOp(asg::ArithOp::Mul),
                Sub => ArithOp(asg::ArithOp::Sub),
                Div => ArithOp(asg::ArithOp::Div),
                // variant synast::ArithOp::Mod does not exist
                // Mod => asg::BinaryOp::Mod,
                // Shl => Shl,
                // Shr => Shr,
                _ => todo!(),
            }
        }
        synast::BinaryOp::CmpOp(cmp_op) => {
            use asg::BinaryOp::CmpOp;
            use synast::CmpOp::*;
            match cmp_op {
                Eq { negated: false } => CmpOp(asg::CmpOp::Eq),
                _ => todo!(),
            }
        }
        _ => {
            todo!()
        }
    }
}

fn from_literal(literal: &synast::Literal) -> Option<asg::TExpr> {
    let literal_texpr = match literal.kind() {
        synast::LiteralKind::Bool(bool_val) => asg::BoolLiteral::new(bool_val).to_texpr(),

        synast::LiteralKind::IntNumber(int_num) => {
            let num = int_num.value_u128().unwrap(); // fn value_u128 is kind of a hack
            asg::IntLiteral::new(num).to_texpr()
        }

        synast::LiteralKind::FloatNumber(float_num) => {
            let num = float_num.value().unwrap();
            let float = format!("{num}");
            asg::FloatLiteral::new(float).to_texpr()
        }

        synast::LiteralKind::BitString(bit_string) => {
            asg::BitStringLiteral::new(bit_string.str()?).to_texpr()
        }

        _ => todo!(), // error. can/should be caught at syntax level, obviously
    };
    Some(literal_texpr)
}

fn from_item(item: synast::Item, context: &mut Context) -> Option<asg::Stmt> {
    match item {
        synast::Item::IfStmt(if_stmt) => {
            let condition = from_expr(if_stmt.condition().unwrap(), context);
            let then_branch = from_block_expr(if_stmt.then_branch().unwrap(), context);
            let else_branch = if_stmt.else_branch().map(|ex| from_block_expr(ex, context));
            Some(asg::If::new(condition.unwrap(), then_branch, else_branch).to_stmt())
        }

        synast::Item::WhileStmt(while_stmt) => {
            let condition = from_expr(while_stmt.condition().unwrap(), context);
            let loop_body = from_block_expr(while_stmt.body().unwrap(), context);
            Some(asg::While::new(condition.unwrap(), loop_body).to_stmt())
        }

        synast::Item::ClassicalDeclarationStatement(type_decl) => {
            Some(from_classical_declaration_statement(&type_decl, context))
        }

        synast::Item::QuantumDeclarationStatement(q_decl) => {
            let qubit_type = q_decl.qubit_type().unwrap();
            let width = match qubit_type.designator().and_then(|x| x.expr()) {
                Some(synast::Expr::Literal(ref literal)) => {
                    match literal.kind() {
                        synast::LiteralKind::IntNumber(int_num) => {
                            Some(int_num.value().unwrap() as u32)
                        }
                        _ => todo!(), // error. can/should be caught at syntax level, obviously
                    }
                }
                None => None,
                _ => todo!(), // only literals supported
            };
            let typ = match width {
                Some(width) => Type::QubitArray(ArrayDims::D1(width as usize)),
                None => Type::Qubit,
            };
            let name_str = q_decl.name().unwrap().string();
            let symbol_id = context.new_binding(name_str.as_ref(), &typ, &q_decl);
            let q_decl_ast = asg::DeclareQuantum::new(symbol_id);
            Some(asg::Stmt::DeclareQuantum(q_decl_ast))
        }

        synast::Item::AssignmentStmt(assignment_stmt) => {
            from_assignment_stmt(&assignment_stmt, context)
        }

        synast::Item::BreakStmt(_) => Some(asg::Stmt::Break),

        synast::Item::ContinueStmt(_) => Some(asg::Stmt::Continue),

        synast::Item::EndStmt(_) => Some(asg::Stmt::End),

        // Gate definition
        synast::Item::Gate(gate) => {
            let name_node = gate.name().unwrap();
            let gate_name_symbol_id =
                context.new_binding(name_node.string().as_ref(), &Type::Gate, &name_node);

            // Here are three ways to manage the context.

            // Make some bindings and push and pop the scope automatically.
            // Clumsy. There must be a better way.
            // Alternative is the doing in manually as in commented out code below.
            // let mut params = None;
            // let mut qubits = None;
            // with_scope(context, ScopeType::Subroutine,
            //            |cxt| {
            //                params = Some(bind_parameter_list(gate.angle_params(), &Type::Angle(None, IsConst::True), cxt));
            //                qubits = bind_parameter_list(gate.qubit_params(), &Type::Qubit, cxt);
            //            }
            // );
            // let params = params.unwrap();
            // let qubits = qubits.unwrap();

            // Manage the scope manually. This is more readable.

            // context.symbol_table.enter_scope(ScopeType::Subroutine);
            // let params = bind_parameter_list(gate.angle_params(), &Type::Angle(None, IsConst::True), context);
            // let qubits = bind_parameter_list(gate.qubit_params(), &Type::Qubit, context).unwrap();
            // let block = from_block_expr(gate.body().unwrap(), context);
            // context.symbol_table.exit_scope();

            // This might be kind of fragile. Currently, we should be able to handle
            //    1. a sequnce of semicolon-separated items.
            // or 2. a single block. But the block has a scope of course.
            with_scope!(context,  ScopeType::Subroutine,
                          let params = bind_parameter_list(gate.angle_params(), &Type::Angle(None, IsConst::True), context);
                          let qubits = bind_parameter_list(gate.qubit_params(), &Type::Qubit, context).unwrap();
                          let block = from_block_expr(gate.body().unwrap(), context);
            );

            Some(asg::GateDeclaration::new(gate_name_symbol_id, params, qubits, block).to_stmt())
        }

        synast::Item::GateCallStmt(gate_call) => {
            // Warning, I think map overlooks None. which can cause a bug, in the present case.
            // Because None means a coding error upstream. Better to blow up here.
            let gate_operands = gate_call
                .qubit_list()
                .unwrap()
                .gate_operands()
                .map(|qubit| from_gate_operand(qubit, context))
                .collect();

            let param_list = gate_call
                .arg_list()
                .map(|ex| inner_expression_list(ex.expression_list().unwrap(), context));
            let gate_id = gate_call.identifier();
            // FIXME: make sure we are efficient with strings
            let gate_name = gate_call.identifier().unwrap().text().to_string();
            let (symbol_result, _typ) = context
                .lookup_symbol(gate_name.as_ref(), &gate_id.unwrap())
                .as_tuple();
            Some(asg::Stmt::GateCall(asg::GateCall::new(
                symbol_result,
                param_list,
                gate_operands,
                None,
            )))
        }

        synast::Item::Include(include) => {
            if context.symbol_table().current_scope_type() != ScopeType::Global {
                context.insert_error(IncludeNotInGlobalScopeError, &include);
                None
            } else {
                // `include`s are read and included when parsing to AST
                unreachable!()
            }
        }

        synast::Item::VersionString(version_string) => {
            let version = version_string.version().unwrap().version().unwrap();
            let _ = version.split_into_parts();
            None
        }

        synast::Item::GPhaseCallStmt(gphase) => {
            let synarg = gphase.arg().unwrap();
            let arg = from_expr(synarg, context).unwrap();
            Some(asg::Stmt::GPhaseCall(asg::GPhaseCall::new(arg)))
        }

        _ => None,
    }
}

fn from_block_expr(block_synast: synast::BlockExpr, context: &mut Context) -> asg::Block {
    let mut block = asg::Block::new();

    for parse_item in block_synast.statements() {
        let stmt = match parse_item {
            synast::Stmt::Item(item) => from_item(item, context),
            synast::Stmt::ExprStmt(expr_stmt) => from_expr_stmt(expr_stmt, context),
        };
        if let Some(stmt) = stmt {
            block.insert_stmt(stmt)
        }
    }
    block
}

fn from_classical_declaration_statement(
    type_decl: &synast::ClassicalDeclarationStatement,
    context: &mut Context,
) -> asg::Stmt {
    let scalar_type = type_decl.scalar_type().unwrap();
    let isconst = type_decl.const_token().is_some();

    // We only support literal integer designators at the moment.
    let width = match scalar_type.designator().and_then(|desg| desg.expr()) {
        Some(synast::Expr::Literal(ref literal)) => {
            match literal.kind() {
                synast::LiteralKind::IntNumber(int_num) => Some(int_num.value().unwrap() as u32),
                _ => {
                    // FIXME: This error should be done when validating syntax. Before the semantic analysis
                    context.insert_error(ConstIntegerError, literal);
                    None // FIXME. This should be something signifying an invalid type
                         // `None` signifies a valid type.
                }
            }
        }
        Some(expr) => panic!("Unsupported designator type: {:?}", type_name_of(expr)),
        None => None,
    };

    let typ = match scalar_type.kind() {
        synast::ScalarTypeKind::Int => Type::Int(width, isconst.into()),
        synast::ScalarTypeKind::Float => Type::Float(width, isconst.into()),
        synast::ScalarTypeKind::Angle => Type::Angle(width, isconst.into()),
        synast::ScalarTypeKind::Bit => match width {
            Some(width) => Type::BitArray(ArrayDims::D1(width as usize), isconst.into()),
            None => Type::Bit(isconst.into()),
        },
        synast::ScalarTypeKind::Bool => Type::Bool(isconst.into()),
        _ => todo!(),
    };

    let name_str = type_decl.name().unwrap().string();
    let initializer = type_decl
        .expr()
        .and_then(|initializer| from_expr(initializer, context));

    // FIXME: This error and several others can and should be moved to a subsequent pass.
    // However, we would lose the information in `text_range` unless we do something to preserve it.
    let symbol_id = context.new_binding(name_str.as_ref(), &typ, type_decl);
    if let Some(ref initializer) = initializer {
        if !types::can_cast_loose(&typ, initializer.get_type()) {
            context.insert_error(IncompatibleTypesError, type_decl);
        }
    }
    asg::DeclareClassical::new(symbol_id, initializer).to_stmt()
}

// FIXME: In oq3_syntax we have both Name and Identifier. We only need one, I think.
// This will be changed.

fn from_assignment_stmt(
    assignment_stmt: &synast::AssignmentStmt,
    context: &mut Context,
) -> Option<asg::Stmt> {
    let nameb = assignment_stmt.name(); // LHS of assignment
    let name = nameb.as_ref().unwrap();
    let name_str = name.string();
    let expr = from_expr(assignment_stmt.expr().unwrap(), context); // rhs of `=` operator

    let (symbol_id, typ) = context.lookup_symbol(name_str.as_str(), name).as_tuple();
    let is_mutating_const = symbol_id.is_ok() && typ.is_const();
    let lvalue = asg::LValue::Identifier(symbol_id);
    let stmt_asg = Some(asg::Assignment::new(lvalue, expr.unwrap()).to_stmt());
    if is_mutating_const {
        context.insert_error(MutateConstError, assignment_stmt);
    }
    stmt_asg
}

//
// These functions (fn ast_ ...) convert oq3_syntax::ast (alias synast) structs to ast structs.
// These ast structs are not yet wrapped in `enum Stmt` or `enum Expr`, etc.
// These functions exist because some of these ast structs will be wrapped into the
// ast tree in different ways. So their construction is abstracted out.
//

fn ast_hardware_qubit(hwq: &synast::HardwareQubit) -> asg::HardwareQubit {
    asg::HardwareQubit::new(hwq.string())
}

fn ast_identifier(
    identifier: &synast::Identifier,
    context: &mut Context,
) -> (asg::Identifier, Type) {
    let name_str = identifier.string();
    let (symbol_id, typ) = context
        .lookup_symbol(name_str.as_str(), identifier)
        .as_tuple();
    (asg::Identifier::new(name_str, symbol_id), typ)
}

fn ast_indexed_identifier(
    indexed_identifier: &synast::IndexedIdentifier,
    context: &mut Context,
) -> (asg::IndexedIdentifier, Type) {
    let identifier = indexed_identifier.identifier().unwrap().string();
    let (symbol_id, typ) = context
        .lookup_symbol(identifier.as_ref(), indexed_identifier)
        .as_tuple();
    let indexes = indexed_identifier
        .index_operators()
        .map(|index| from_index_operator(index, context))
        .collect();
    (asg::IndexedIdentifier::new(symbol_id, indexes), typ)
}

// Bind all parameter names to new symbols. Assume they all have common type `typ`.
// Log RedeclarationError when it occurs.
fn bind_parameter_list(
    inparam_list: Option<synast::ParamList>,
    typ: &Type,
    context: &mut Context,
) -> Option<Vec<SymbolIdResult>> {
    inparam_list.map(|param_list| {
        param_list
            .params()
            .map(|param| context.new_binding(param.text().as_ref(), typ, &param))
            .collect()
    })
}

// This works, but using it is pretty clumsy.
// fn with_scope<F>(context: &mut Context, scope: ScopeType, func: F) where
//     F: FnOnce(&mut Context)
// {
//     context.symbol_table.enter_scope(scope);
//     let res = func(context);
//     context.symbol_table.exit_scope();
//     res // but there is no return value
// }
