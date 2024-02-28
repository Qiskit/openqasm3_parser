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
use std::path::Path;

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
use synast::{HasArgList, HasName, HasTextName};

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
pub fn parse_source_string<T, P>(
    source: T,
    fake_file_path: Option<&str>,
    search_path_list: Option<&[P]>,
) -> ParseResult<SourceString>
where
    T: AsRef<str>,
    P: AsRef<Path>,
{
    let parsed_source =
        oq3_source_file::parse_source_string(source, fake_file_path, search_path_list);
    analyze_source(parsed_source)
}

/// Parse source file to semantic ASG
pub fn parse_source_file<T, P>(
    file_path: T,
    search_path_list: Option<&[P]>,
) -> ParseResult<SourceFile>
where
    T: AsRef<Path>,
    P: AsRef<Path>,
{
    let parsed_source = oq3_source_file::parse_source_file(file_path, search_path_list);
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

pub fn syntax_to_semantic<T: SourceTrait>(
    parsed_source: &T,
    mut context: Context,
    errors: SemanticErrorList,
) -> (Context, SemanticErrorList) {
    let mut included_iter = parsed_source.included().iter();
    let save_errors = replace(&mut context.semantic_errors, errors);
    for parse_stmt in parsed_source.syntax_ast().tree().statements() {
        let stmt = match parse_stmt {
            // Include does not go in the ASG, instead it is evaluated.
            // So we include the parsed code, collect errors, and return `None`.
            // It would be convenient to treat include in `from_stmt` as we do all other statements.
            // But the arm below for Stmt::Include manipulates context and depends on
            // included_iter in ways that no other match arm does.
            // It is probably possible to encapsulate the manipulations of (context, errors).
            // But I have not made much of an attempt to do so.
            synast::Stmt::Include(include) => {
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

            // Everything other than `include` only needs `context`.
            stmt => from_stmt(stmt, &mut context),
        };
        if let Some(stmt) = stmt {
            if context.annotations_is_empty() {
                context.program.insert_stmt(stmt);
            } else {
                let anstmt = asg::AnnotatedStmt::new(stmt, context.take_annotations()).to_stmt();
                context.program.insert_stmt(anstmt);
            }
        }
    }

    // Upon entering the current call, the errors in `context` were swapped out and replaced with `errors`.
    // This `errors` was also passed in, in the current call.
    // Now retrieve `errors` from `context` and restore `saved_errors` to `context`.
    // And return `errors`, which was populated during the current call.
    // Suppose the current call to `syntax_to_semantic` was made while processing an `include` statement...
    let errors = replace(&mut context.semantic_errors, save_errors);
    (context, errors)
}

// Main entry point for converting statements in AST to ASG
fn from_stmt(stmt: synast::Stmt, context: &mut Context) -> Option<asg::Stmt> {
    match stmt {
        synast::Stmt::IfStmt(if_stmt) => {
            let condition = from_expr(if_stmt.condition(), context);
            with_scope!(context,  ScopeType::Local,
                        let then_branch = from_block_expr(if_stmt.then_branch().unwrap(), context);
            );
            with_scope!(context,  ScopeType::Local,
                        let else_branch = if_stmt.else_branch().map(|ex| from_block_expr(ex, context));
            );
            Some(asg::If::new(condition.unwrap(), then_branch, else_branch).to_stmt())
        }

        synast::Stmt::WhileStmt(while_stmt) => {
            let condition = from_expr(while_stmt.condition(), context);
            with_scope!(context,  ScopeType::Local,
                        let loop_body = from_block_expr(while_stmt.body().unwrap(), context);
            );
            Some(asg::While::new(condition.unwrap(), loop_body).to_stmt())
        }

        synast::Stmt::ForStmt(for_stmt) => {
            let loop_var = for_stmt.loop_var().unwrap();
            let ty = from_scalar_type(&for_stmt.scalar_type().unwrap(), false, context);
            let iterable_ast = for_stmt.for_iterable().unwrap();
            let iterable = if let Some(set_expression) = iterable_ast.set_expression() {
                asg::ForIterable::SetExpression(from_set_expression(set_expression, context))
            } else if let Some(range_expression) = iterable_ast.range_expr() {
                asg::ForIterable::RangeExpression(from_range_expression(range_expression, context))
            } else if let Some(expression) = iterable_ast.for_iterable_expr() {
                asg::ForIterable::Expr(from_expr(Some(expression), context).unwrap())
            } else {
                // It would be nice to use an enum on the other side.
                // This error should be caught before semantic analysis. Eg in validation of the AST
                unreachable!() // probably is reachable
            };
            with_scope!(context,  ScopeType::Local,
                        let loop_var_symbol_id = context.new_binding(loop_var.string().as_ref(), &ty, &loop_var);
                        let loop_body = from_block_expr(for_stmt.body().unwrap(), context);
            );
            Some(asg::ForStmt::new(loop_var_symbol_id, iterable, loop_body).to_stmt())
        }

        // Note: The outer curlies do not entail a new scope. But the inner curlies do entail a
        // new scope, one for each case.
        synast::Stmt::SwitchCaseStmt(switch_case_stmt) => {
            let control = from_expr(switch_case_stmt.control(), context);
            let case_exprs = switch_case_stmt.case_exprs().map(|case_expr| {
                let int_exprs = inner_expression_list(case_expr.expression_list().unwrap(), context);
                with_scope!(context,  ScopeType::Local,
                            let statements = statement_list_from_block(case_expr.block_expr().unwrap(), context);
                );
                asg::CaseExpr::new(int_exprs, statements)
            }).collect::<Vec<_>>();
            with_scope!(context,  ScopeType::Local,
                        let default_statements = switch_case_stmt.default_block().map(|block| statement_list_from_block(block, context));
            );
            Some(
                asg::SwitchCaseStmt::new(control.unwrap(), case_exprs, default_statements)
                    .to_stmt(),
            )
        }

        synast::Stmt::ClassicalDeclarationStatement(type_decl) => {
            Some(from_classical_declaration_statement(&type_decl, context))
        }

        synast::Stmt::QuantumDeclarationStatement(q_decl) => {
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

        synast::Stmt::AssignmentStmt(assignment_stmt) => {
            from_assignment_stmt(&assignment_stmt, context)
        }

        synast::Stmt::BreakStmt(_) => Some(asg::Stmt::Break),

        synast::Stmt::ContinueStmt(_) => Some(asg::Stmt::Continue),

        synast::Stmt::EndStmt(_) => Some(asg::Stmt::End),

        // Gate definition
        synast::Stmt::Gate(gate) => {
            let name_node = gate.name().unwrap();
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
            //    1. a sequnce of semicolon-separated stmts.
            // or 2. a single block. But the block has a scope of course.
            with_scope!(context,  ScopeType::Subroutine,
                          let params = bind_parameter_list(gate.angle_params(), &Type::Angle(None, IsConst::True), context);
                          let qubits = bind_parameter_list(gate.qubit_params(), &Type::Qubit, context).unwrap();
                          let block = from_block_expr(gate.body().unwrap(), context);
            );
            let num_params = match params {
                Some(ref params) => params.len(),
                None => 0,
            };
            let gate_name_symbol_id = context.new_binding(
                name_node.string().as_ref(),
                &Type::Gate(
                    num_params.try_into().unwrap(),
                    qubits.len().try_into().unwrap(),
                ),
                &name_node,
            );

            Some(asg::GateDeclaration::new(gate_name_symbol_id, params, qubits, block).to_stmt())
        }

        synast::Stmt::Barrier(barrier) => {
            let gate_operands = barrier.qubit_list().map(|operands| {
                operands
                    .gate_operands()
                    .map(|qubit| from_gate_operand(qubit, context))
                    .collect()
            });
            Some(asg::Stmt::Barrier(asg::Barrier::new(gate_operands)))
        }

        synast::Stmt::DelayStmt(delay_stmt) => {
            let gate_operands = delay_stmt.qubit_list().map(|operands| {
                operands
                    .gate_operands()
                    .map(|qubit| from_gate_operand(qubit, context))
                    .collect()
            });
            let duration = from_expr(delay_stmt.designator().unwrap().expr(), context).unwrap();
            if !matches!(duration.get_type(), Type::Duration(_)) {
                context.insert_error(IncompatibleTypesError, &delay_stmt.designator().unwrap());
            }
            Some(asg::Stmt::Delay(asg::DelayStmt::new(
                duration,
                gate_operands.unwrap(),
            )))
        }

        synast::Stmt::Reset(reset) => {
            let gate_operand = reset.gate_operand().unwrap(); // FIXME: check this
            let gate_operand_asg = from_gate_operand(gate_operand, context);
            Some(asg::Reset::new(gate_operand_asg).to_stmt())
        }

        synast::Stmt::Include(include) => {
            if context.symbol_table().current_scope_type() != ScopeType::Global {
                context.insert_error(IncludeNotInGlobalScopeError, &include);
                None
            } else {
                // `include`s are read and included when parsing to AST
                unreachable!()
            }
        }

        synast::Stmt::ExprStmt(expr_stmt) => from_expr_stmt(expr_stmt, context),

        synast::Stmt::VersionString(version_string) => {
            let version = version_string.version().unwrap().version().unwrap();
            let _ = version.split_into_parts();
            None
        }

        synast::Stmt::PragmaStatement(pragma) => {
            Some(asg::Pragma::new(pragma.pragma_text()).to_stmt())
        }

        // Annotations are accumulated and attached to the following statement.
        // So we return None here.
        // It would be more convenient to return the annotation and handle
        // attaching, clearing etc. in one spot. But we have to return a Stmt.
        // And asg::Annotation is not a Stmt.
        synast::Stmt::AnnotationStatement(annotation) => {
            context.push_annotation(asg::Annotation::new(annotation.annotation_text()));
            None
        }

        synast::Stmt::AliasDeclarationStatement(alias_stmt) => {
            let name_str = alias_stmt.name().unwrap().string();
            let rhs = from_expr(alias_stmt.expr(), context).unwrap();
            // Bind the name to the RHS, giving it the same type as the RHS.
            let symbol_id = context.new_binding(name_str.as_ref(), rhs.get_type(), &alias_stmt);
            Some(asg::Alias::new(symbol_id, rhs).to_stmt())
        }

        _ => None,
    }
}

fn from_expr_stmt(expr_stmt: synast::ExprStmt, context: &mut Context) -> Option<asg::Stmt> {
    use synast::Expr::{GPhaseCallExpr, GateCallExpr, ModifiedGateCallExpr};
    // At present, three expressions, those for gate calls, are handled specially. In oq3_syntax, gate calls
    // are expressions wrapped in `ExprStmt`. But in the ASG, gate calls are are variant of `Stmt`. All
    // other `synast::ExprStmt` are translated to `asg::ExprStmt`.
    match expr_stmt.expr() {
        Some(GateCallExpr(gate_call)) => {
            from_gate_call_expr(gate_call, Vec::<asg::GateModifier>::new(), context)
        }
        Some(ModifiedGateCallExpr(mod_gate_call)) => {
            let modifiers = mod_gate_call
                .modifiers()
                .map(|modifier| match modifier {
                    synast::Modifier::InvModifier(_) => asg::GateModifier::Inv,

                    synast::Modifier::PowModifier(pow_mod) => {
                        let exponent =
                            from_paren_expr(pow_mod.paren_expr().unwrap(), context).unwrap();
                        asg::GateModifier::Pow(exponent)
                    }

                    synast::Modifier::CtrlModifier(ctrl_mod) => {
                        let exponent = ctrl_mod
                            .paren_expr()
                            .and_then(|x| from_paren_expr(x, context));
                        asg::GateModifier::Ctrl(exponent)
                    }

                    synast::Modifier::NegCtrlModifier(neg_ctrl_mod) => {
                        let exponent = neg_ctrl_mod
                            .paren_expr()
                            .and_then(|x| from_paren_expr(x, context));
                        asg::GateModifier::NegCtrl(exponent)
                    }
                })
                .collect();

            // `synast::ModifiedGateCallExpr` may wrap either gate call or gphase call,
            // which is treated separately.
            if let Some(gate_call) = mod_gate_call.gate_call_expr() {
                from_gate_call_expr(gate_call, modifiers, context)
            } else {
                let gphase = mod_gate_call.g_phase_call_expr().unwrap();
                let arg = from_expr(gphase.arg(), context).unwrap();
                Some(asg::Stmt::ModifiedGPhaseCall(asg::ModifiedGPhaseCall::new(
                    arg, modifiers,
                )))
            }
        }

        Some(GPhaseCallExpr(gphase)) => {
            let arg = from_expr(gphase.arg(), context).unwrap();
            Some(asg::Stmt::GPhaseCall(asg::GPhaseCall::new(arg)))
        }

        syn_expr => {
            let expr = from_expr(syn_expr, context);
            expr.map_or_else(
                || panic!("expr::ExprStmt is None. Expression not implemented in the ASG."),
                |ex| Some(asg::Stmt::ExprStmt(ex)),
            )
        }
    }
}

fn from_paren_expr(paren_expr: synast::ParenExpr, context: &mut Context) -> Option<asg::TExpr> {
    from_expr(paren_expr.expr(), context)
}

fn from_expr(expr_maybe: Option<synast::Expr>, context: &mut Context) -> Option<asg::TExpr> {
    let expr = expr_maybe?;
    match expr {
        // FIXME: Ugh. could clean up logic here
        synast::Expr::PrefixExpr(prefix_expr) => {
            match prefix_expr.op_kind() {
                Some(synast::UnaryOp::Neg) => {
                    match prefix_expr.expr() {
                        Some(synast::Expr::Literal(ref literal)) => {
                            match literal.kind() {
                                synast::LiteralKind::FloatNumber(float_num) => {
                                    let num = float_num.value().unwrap();
                                    let float = format!("-{num}");
                                    Some(asg::FloatLiteral::new(float).to_texpr())
                                }
                                synast::LiteralKind::IntNumber(int_num) => {
                                    let num = int_num.value_u128().unwrap(); // fn value_u128 is kind of a hack
                                    Some(asg::IntLiteral::new(num, false).to_texpr())
                                    // `false` means negative
                                }
                                _ => todo!(),
                            }
                        }

                        Some(synexpr) => Some(
                            asg::UnaryExpr::new(
                                asg::UnaryOp::Minus,
                                from_expr(Some(synexpr), context).unwrap(),
                            )
                            .to_texpr(),
                        ),

                        _ => todo!(),
                    }
                }
                _ => todo!(),
            }
        }

        synast::Expr::ParenExpr(paren_expr) => from_paren_expr(paren_expr, context),

        synast::Expr::BinExpr(bin_expr) => {
            let synast_op = bin_expr.op_kind().unwrap();
            let left_syn = bin_expr.lhs();
            let right_syn = bin_expr.rhs();

            let op = from_binary_op(synast_op);
            let left = from_expr(left_syn, context).unwrap();
            let right = from_expr(right_syn, context).unwrap();

            Some(asg::BinaryExpr::new_texpr_with_cast(op, left, right))
        }

        synast::Expr::Literal(ref literal) => from_literal(literal),

        synast::Expr::TimingLiteral(ref timing_literal) => {
            let time_unit = match timing_literal.time_unit().unwrap() {
                synast::TimeUnit::Second => asg::TimeUnit::Second,
                synast::TimeUnit::MilliSecond => asg::TimeUnit::MilliSecond,
                synast::TimeUnit::MicroSecond => asg::TimeUnit::MicroSecond,
                synast::TimeUnit::NanoSecond => asg::TimeUnit::NanoSecond,
                synast::TimeUnit::Cycle => asg::TimeUnit::Cycle,
            };
            match timing_literal.literal().unwrap().kind() {
                synast::LiteralKind::IntNumber(int_num) => {
                    let num = int_num.value_u128().unwrap();
                    Some(asg::TimingIntLiteral::new(num, true, time_unit).to_texpr())
                }
                synast::LiteralKind::FloatNumber(float_num) => {
                    let num = float_num.value().unwrap();
                    Some(asg::TimingFloatLiteral::new(num, true, time_unit).to_texpr())
                }

                _ => panic!("You have found a bug in oq3_syntax or oq3_parser"),
            }
        }

        synast::Expr::Identifier(identifier) => {
            let (astidentifier, typ) = ast_identifier(&identifier, context);
            Some(astidentifier.to_texpr(typ))
        }

        synast::Expr::HardwareQubit(hwq) => Some(ast_hardware_qubit(&hwq).to_texpr()),

        // Range expressions are not allowed everywhere. Maybe remove from expr tree
        synast::Expr::RangeExpr(range_expr) => {
            Some(from_range_expression(range_expr, context).to_texpr())
        }

        synast::Expr::IndexExpr(index_expr) => {
            let expr = from_expr(index_expr.expr(), context);
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

        synast::Expr::ReturnExpr(ref return_expr) => {
            let expr_asg = from_expr(return_expr.expr(), context);
            if context.symbol_table().current_scope_type() == ScopeType::Global {
                context.insert_error(ReturnInGlobalScopeError, &expr);
            }
            Some(asg::ReturnExpression::new(expr_asg).to_texpr())
        }

        synast::Expr::CastExpression(cast) => {
            let typ = from_scalar_type(&cast.scalar_type().unwrap(), true, context);
            let expr = from_expr(cast.expr(), context);
            Some(asg::Cast::new(expr.unwrap(), typ).to_texpr())
        }

        // Everything else is not yet implemented
        _ => {
            println!("Expression not supported {:?}", expr);
            None
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

fn from_range_expression(
    range_expr: synast::RangeExpr,
    context: &mut Context,
) -> asg::RangeExpression {
    let (start, step, stop) = range_expr.start_step_stop();
    let start = from_expr(start, context).unwrap();
    let stop = from_expr(stop, context).unwrap();
    let step = from_expr(step, context);
    asg::RangeExpression::new(start, step, stop)
}

fn from_gate_call_expr(
    gate_call_expr: synast::GateCallExpr,
    modifiers: Vec<asg::GateModifier>,
    context: &mut Context,
) -> Option<asg::Stmt> {
    // Warning, I think map overlooks None. This can cause a bug in the present case.
    // Because None means a coding error upstream. Better to blow up here.
    let gate_operands: Vec<_> = gate_call_expr
        .qubit_list()
        .unwrap()
        .gate_operands()
        .map(|qubit| from_gate_operand(qubit, context))
        .collect();

    let param_list = gate_call_expr
        .arg_list()
        .map(|ex| inner_expression_list(ex.expression_list().unwrap(), context));
    let num_params = match param_list {
        Some(ref params) => params.len(),
        None => 0,
    };
    let gate_id = gate_call_expr.identifier();
    // FIXME: make sure we are efficient with strings
    let gate_name = gate_call_expr.identifier().unwrap().text().to_string();
    let (symbol_result, gate_type) = context
        .lookup_gate_symbol(gate_name.as_ref(), gate_id.as_ref().unwrap())
        .as_tuple();
    if matches!(gate_type, Type::Gate(_, _)) {
        let (def_num_params, def_num_qubits) = match gate_type {
            Type::Gate(np, nq) => (np, nq),
            _ => (0, 0),
        };
        if def_num_params != num_params.try_into().unwrap() {
            if num_params != 0 {
                // If num params is mismatched, locate error at list of params supplied.
                context.insert_error(NumGateParamsError, &gate_call_expr.arg_list().unwrap());
            } else {
                // If no params are supplied, but some are expected, locate error at gate name.
                context.insert_error(NumGateParamsError, &gate_id.unwrap());
            }
        }
        let num_qubits: usize = gate_operands.len();
        if def_num_qubits != num_qubits.try_into().unwrap() {
            if num_qubits == 0 {
                // This probably can't happen because no qubit args is not recognized syntactially
                // as a gate call.
                context.insert_error(NumGateQubitsError, &gate_call_expr);
            } else {
                context.insert_error(NumGateQubitsError, &gate_call_expr.qubit_list().unwrap());
            };
        }
    } else if symbol_result.is_ok() {
        // If symbol_result.is_err then we have already logged UndefGateError.
        context.insert_error(IncompatibleTypesError, &gate_id.unwrap());
    }
    Some(asg::Stmt::GateCall(asg::GateCall::new(
        symbol_result,
        param_list,
        gate_operands,
        modifiers,
    )))
}

fn from_gate_operand(gate_operand: synast::GateOperand, context: &mut Context) -> asg::TExpr {
    match gate_operand {
        synast::GateOperand::HardwareQubit(ref hwq) => {
            asg::GateOperand::HardwareQubit(ast_hardware_qubit(hwq)).to_texpr(Type::HardwareQubit)
        }
        synast::GateOperand::Identifier(ref identifier) => {
            let (astidentifier, typ) = ast_identifier(identifier, context);
            if !matches!(typ, Type::Qubit | Type::HardwareQubit | Type::QubitArray(_)) {
                context.insert_error(IncompatibleTypesError, &gate_operand);
            }
            asg::GateOperand::Identifier(astidentifier).to_texpr(typ)
        }
        synast::GateOperand::IndexedIdentifier(ref indexed_identifier) => {
            let (indexed_identifier, typ) = ast_indexed_identifier(indexed_identifier, context);
            if !matches!(typ, Type::QubitArray(_)) {
                context.insert_error(IncompatibleTypesError, &gate_operand);
            }
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

fn from_expression_list(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> asg::ExpressionList {
    asg::ExpressionList::new(inner_expression_list(expression_list, context))
}

// Return a Vec of TExpr.
fn inner_expression_list(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> Vec<asg::TExpr> {
    expression_list
        .exprs()
        .filter_map(|x| from_expr(Some(x), context))
        .collect()
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
        synast::BinaryOp::ConcatenationOp => asg::BinaryOp::ConcatenationOp,
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
            asg::IntLiteral::new(num, true).to_texpr() // `true` means positive literal.
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

// Convert a block of statements in the AST to a list of ASG statements
// We don't convert to asg::Block, because these lists of statements go into
// other block-like structures as well.
fn statement_list_from_block(block: synast::BlockExpr, context: &mut Context) -> Vec<asg::Stmt> {
    block
        .statements()
        .filter_map(|syn_stmt| from_stmt(syn_stmt, context))
        .collect::<Vec<_>>()
}

fn from_block_expr(block_synast: synast::BlockExpr, context: &mut Context) -> asg::Block {
    asg::Block::new(statement_list_from_block(block_synast, context))
}

// Convert AST scalar type to a `types::Type`
fn from_scalar_type(
    scalar_type: &synast::ScalarType,
    isconst: bool,
    context: &mut Context,
) -> Type {
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
    match scalar_type.kind() {
        synast::ScalarTypeKind::Int => Type::Int(width, isconst.into()),
        synast::ScalarTypeKind::Float => Type::Float(width, isconst.into()),
        synast::ScalarTypeKind::Angle => Type::Angle(width, isconst.into()),
        synast::ScalarTypeKind::Bit => match width {
            Some(width) => Type::BitArray(ArrayDims::D1(width as usize), isconst.into()),
            None => Type::Bit(isconst.into()),
        },
        synast::ScalarTypeKind::Bool => Type::Bool(isconst.into()),
        synast::ScalarTypeKind::Duration => Type::Duration(isconst.into()),
        _ => todo!(),
    }
}

fn from_classical_declaration_statement(
    type_decl: &synast::ClassicalDeclarationStatement,
    context: &mut Context,
) -> asg::Stmt {
    if type_decl.array_type().is_some() {
        panic!("Array types are not supported yet in the ASG");
    }
    let scalar_type = type_decl.scalar_type().unwrap();
    let typ = from_scalar_type(&scalar_type, type_decl.const_token().is_some(), context);

    let name_str = type_decl.name().unwrap().string();
    let initializer = from_expr(type_decl.expr(), context);
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

// FIXME: Refactor this. It was done in a hurry.
fn from_assignment_stmt(
    assignment_stmt: &synast::AssignmentStmt,
    context: &mut Context,
) -> Option<asg::Stmt> {
    let nameb = assignment_stmt.identifier(); // LHS of assignment
    if nameb.is_some() {
        let name = nameb.as_ref().unwrap();
        let name_str = name.string();
        let expr = from_expr(assignment_stmt.rhs(), context); // rhs of `=` operator

        let (symbol_id, typ) = context.lookup_symbol(name_str.as_str(), name).as_tuple();
        let is_mutating_const = symbol_id.is_ok() && typ.is_const();
        let lvalue = asg::LValue::Identifier(symbol_id);
        let stmt_asg = Some(asg::Assignment::new(lvalue, expr.unwrap()).to_stmt());
        if is_mutating_const {
            context.insert_error(MutateConstError, assignment_stmt);
        }
        return stmt_asg;
    }
    let indexed_identifier_ast = assignment_stmt.indexed_identifier();
    let (indexed_identifier, _typ) =
        ast_indexed_identifier(&indexed_identifier_ast.unwrap(), context);
    let expr = from_expr(assignment_stmt.rhs(), context); // rhs of `=` operator
                                                          //    let is_mutating_const = symbol_id.is_ok() && typ.is_const();
    let lvalue = asg::LValue::IndexedIdentifier(indexed_identifier);
    Some(asg::Assignment::new(lvalue, expr.unwrap()).to_stmt())
}

//
// These functions (fn ast_ ...) convert oq3_syntax::ast (alias synast) structs to ast structs.
// These ast structs are not yet wrapped in `enum Stmt` or `enum Expr`, etc.
// These functions exist because some of these ast structs will be wrapped into the
// ast tree in different ways. So their construction is abstracted out.
//
// It seems like it would be a good idea to use these functions as much as possible. However,
// Sometimes construction of the object on the one hand and wrapping it in a type and in an
// `enum` variant on the other are not so easily separated into two parts. See for example
// asg::BinaryExpr.new_texpr_with_cast.

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
