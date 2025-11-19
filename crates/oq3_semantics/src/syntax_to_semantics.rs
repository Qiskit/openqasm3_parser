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
use std::path::{Path, PathBuf};

use crate::asg;
use crate::types;
use crate::types::{ArrayDims, IsConst, Type};

use crate::context::Context;
use crate::semantic_error::{
    SemanticErrorKind::{self, *},
    SemanticErrorList,
};
use crate::symbols::{ScopeType, SymbolErrorTrait, SymbolIdResult, SymbolTable};
use oq3_source_file::{SourceFile, SourceString, SourceTrait};

use oq3_syntax::ast as synast; // Syntactic AST

use crate::with_scope;

use crate::utils::type_name_of; // for debugging

// traits
use synast::{HasArgList, HasName, HasTextNode};

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

    pub fn semantic_errors(&self) -> &SemanticErrorList {
        self.context.errors()
    }

    pub fn num_syntax_errors(&self) -> usize {
        self.syntax_result.num_syntax_errors()
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
pub fn parse_source_string_with_path_search<T, P>(
    source: T,
    fake_file_path: Option<&str>,
    search_path_list: Option<&[P]>,
) -> ParseResult<SourceString>
where
    T: AsRef<str>,
    P: AsRef<Path>,
{
    let parsed_source: SourceString =
        oq3_source_file::parse_source_string(source, fake_file_path, search_path_list);
    analyze_source(parsed_source)
}

/// Parse source file `file_path` and analyze results to semantic ASG.
/// If not absolute, `file_path` will be resolved using `search_path_list`.
/// We don't bother to resolve early because we will still need `search_path_list`
/// till the end of sematic analysis: It is used to resolve included source files.
pub fn parse_source_file_with_search<T, P>(
    file_path: T,
    search_path_list: Option<&[P]>,
) -> ParseResult<SourceFile>
where
    T: AsRef<Path>,
    P: AsRef<Path>,
{
    let parsed_source: SourceFile =
        oq3_source_file::parse_source_file_with_search(file_path, search_path_list);
    analyze_source(parsed_source)
}

/// Use an empty `search_path_list`.
pub fn parse_source_string<T>(source: T, fake_file_path: Option<&str>) -> ParseResult<SourceString>
where
    T: AsRef<str>,
{
    let search_path_list = None::<&[PathBuf]>;
    let parsed_source: SourceString =
        oq3_source_file::parse_source_string(source, fake_file_path, search_path_list);
    analyze_source(parsed_source)
}

/// Use an empty `search_path_list`.
pub fn parse_source_file<T>(file_path: T) -> ParseResult<SourceFile>
where
    T: AsRef<Path>,
{
    let parsed_source: SourceFile = oq3_source_file::parse_source_file(file_path); // search_path_list);
    analyze_source(parsed_source)
}

/// Perform semantic analysis on the output of the parser.
/// But only if no parser or lexer errors were recorded.
fn analyze_source<T: SourceTrait>(parsed_source: T) -> ParseResult<T> {
    let file_path = parsed_source.file_path();
    let context = Context::new(file_path.to_path_buf());
    if parsed_source.have_syntax_errors() {
        // on syntax errors, do not continue with semantic analysis.
        return ParseResult {
            syntax_result: parsed_source,
            context,
            have_syntax_errors: true,
        };
    }
    let errors = SemanticErrorList::new(file_path.to_path_buf());
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
    // Move `errors` into `context.semantic_errors`. Save the previous value of `context.semantic_errors`
    // as `save_errors`.
    let save_errors = replace(&mut context.semantic_errors, errors);
    let statements = parsed_source.syntax_ast().unwrap().tree().statements();
    for parse_stmt in statements {
        let stmt = match parse_stmt {
            // Include does not go in the ASG, instead it is evaluated.
            // So we include the parsed code, collect errors, and return `None`.
            // It would be convenient to treat include in `from_stmt` as we do all other statements.
            // But the arm below for Stmt::Include manipulates context and depends on
            // included_iter in ways that no other match arm does.
            // It is probably possible to encapsulate the manipulations of (context, errors).
            // But I have not made much of an attempt to do so.
            synast::Stmt::Include(include) => {
                let file: synast::FilePath = include.file().unwrap();
                let file_path = file.to_string().unwrap();
                if file_path == "stdgates.inc" {
                    // We do not use a file for standard library, but rather create the symbols.
                    context.standard_library_gates(&include);
                } else {
                    let included_parsed_source = included_iter.next().unwrap();
                    // Allocate an empty list for possible semantic errors in the included source file.
                    let mut errors_in_included =
                        SemanticErrorList::new(included_parsed_source.file_path().to_path_buf());
                    // The following path is likely never taken
                    if context.symbol_table().current_scope_type() != ScopeType::Global {
                        context.insert_error(IncludeNotInGlobalScopeError, &include);
                    }

                    // If there was no error when trying to read the included code, then perform semantic analysis
                    // on the parsed contents. If there *was* an error-- the read failed --then we insert a semantic
                    // error for the failed read.
                    match included_parsed_source.include_error() {
                        None => {
                            // Call the present function recursively, passing the new, empty, storage for errors.
                            // Note that `errors_in_included` will be swapped into `context` upon entering `syntax_to_semantic`.
                            (context, errors_in_included) = syntax_to_semantic(
                                included_parsed_source,
                                context,
                                errors_in_included,
                            );
                        }
                        Some(include_error) => {
                            // We include the syntax element as usual, because its single field `SyntaxNode`
                            // includes the range in the source text corresponding to the pathname causing the error.
                            let filename_included = include_error.include.file().unwrap();

                            // We are inserting this semantic error into the list of errors for the source file that we
                            // failed to read. We should really insert it into the list of errors for the parent-- the
                            // source code that contains the failed include statement. But that would be a bit more complicated.
                            context.insert_error(
                                // Convert the io::ErrorKind to a SemanticErrorKind
                                SemanticErrorKind::from_io_error(include_error.error),
                                &filename_included,
                            );
                        }
                    }
                    // Just before exiting the previous call, `errors_in_included` and `errors` are swapped again in `context`.
                    // Push the newly-populated list of errors onto the list of included errors in `context`, which now
                    // holds `errors`, the list passed in the current call to this `syntax_to_semantic`. And `errors`
                    // corresponds to the source in which `include` was encountered.
                    context.push_errors_from_included_file(errors_in_included);
                    // Return `None` because have evaluated (and removed)the `include` statement.
                }
                None
            } // end `synast::Stmt::Include(include) => {`

            // Everything other than `include` only needs `context`.
            stmt => stmt_to_asg_stmt(stmt, &mut context),
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

    // TODO: wtf?
    // Upon entering the current call, the errors in `context` were swapped out and replaced with `errors`.
    // This `errors` was also passed in, in the current call.
    // Now retrieve `errors` from `context` and restore `saved_errors` to `context`.
    // And return `errors`, which was populated during the current call.
    // Suppose the current call to `syntax_to_semantic` was made while processing an `include` statement...
    let errors = replace(&mut context.semantic_errors, save_errors);
    (context, errors)
}

// Main entry point for converting statements in AST to ASG
fn stmt_to_asg_stmt(stmt: synast::Stmt, context: &mut Context) -> Option<asg::Stmt> {
    match stmt {
        synast::Stmt::IfStmt(if_stmt) => {
            let condition = expr_to_asg_texpr(if_stmt.condition(), context);
            with_scope!(context,  ScopeType::Local,
                        let then_branch = block_or_stmt_to_asg_type(if_stmt.true_body_block_or_stmt(), context);
            );
            with_scope!(context,  ScopeType::Local,
                        let else_branch = if_stmt.false_body_block_or_stmt().map(|bors| block_or_stmt_to_asg_type(bors, context));
            );
            Some(asg::If::new(condition.unwrap(), then_branch, else_branch).to_stmt())
        }

        synast::Stmt::WhileStmt(while_stmt) => {
            let condition = expr_to_asg_texpr(while_stmt.condition(), context);
            with_scope!(context,  ScopeType::Local,
                        let loop_body = block_or_stmt_to_asg_type(while_stmt.block_or_stmt(), context);
            );
            Some(asg::While::new(condition.unwrap(), loop_body).to_stmt())
        }

        synast::Stmt::ForStmt(for_stmt) => {
            let loop_var = for_stmt.loop_var().unwrap();
            let ty = scalar_type_to_type(&for_stmt.scalar_type().unwrap(), false, context);
            let iterable_ast = for_stmt.for_iterable().unwrap();
            let iterable = if let Some(set_expression) = iterable_ast.set_expression() {
                asg::ForIterable::SetExpression(set_expression_to_asg_type(set_expression, context))
            } else if let Some(range_expression) = iterable_ast.range_expr() {
                asg::ForIterable::RangeExpression(range_expression_to_asg_type(range_expression, context))
            } else if let Some(expression) = iterable_ast.for_iterable_expr() {
                asg::ForIterable::Expr(expr_to_asg_texpr(Some(expression), context).unwrap())
            } else {
                // It would be nice to use an enum on the other side.
                // This error should be caught before semantic analysis. Eg in validation of the AST
                unreachable!() // probably is reachable
            };
            with_scope!(context,  ScopeType::Local,
                        let loop_var_symbol_id = context.new_binding(loop_var.string().as_ref(), &ty, &loop_var);
                        let loop_body = block_or_stmt_to_asg_type(for_stmt.block_or_stmt(), context);
            );
            Some(asg::ForStmt::new(loop_var_symbol_id, iterable, loop_body).to_stmt())
        }

        // Note: The outer curlies do not entail a new scope. But the inner curlies do entail a
        // new scope, one for each case.
        synast::Stmt::SwitchCaseStmt(switch_case_stmt) => {
            let control = expr_to_asg_texpr(switch_case_stmt.control(), context);
            let case_exprs = switch_case_stmt.case_exprs().map(|case_expr| {
                let int_exprs = expression_list_to_asg_texpr(case_expr.expression_list().unwrap(), context);
                with_scope!(context,  ScopeType::Local,
                            let statements = block_expr_to_asg_stmt_list(case_expr.block_expr().unwrap(), context);
                );
                asg::CaseExpr::new(int_exprs, statements)
            }).collect::<Vec<_>>();
            with_scope!(context,  ScopeType::Local,
                        let default_statements = switch_case_stmt.default_block().map(|block| block_expr_to_asg_stmt_list(block, context));
            );
            Some(
                asg::SwitchCaseStmt::new(control.unwrap(), case_exprs, default_statements)
                    .to_stmt(),
            )
        }

        synast::Stmt::ClassicalDeclarationStatement(type_decl) => {
            Some(classical_declaration_statement_to_asg_stmt(&type_decl, context))
        }

        synast::Stmt::IODeclarationStatement(type_decl) => {
            Some(io_declaration_statement_to_asg_stmt(&type_decl, context))
        }

        synast::Stmt::QuantumDeclarationStatement(q_decl) => {
            if !context.symbol_table().in_global_scope() {
                context.insert_error(NotInGlobalScopeError, &q_decl);
            }
            let name_str = if let Some(name_str) = q_decl.name() {
                name_str.string()
            } else {
                let hw_qubit = q_decl.hardware_qubit().unwrap();
                return Some(asg::DeclareHardwareQubit::new(hardware_qubit_to_asg_type(&hw_qubit)).to_stmt());
            };
            let qubit_type = q_decl.qubit_type().unwrap();
            let width = designator_to_asg(qubit_type.designator().as_ref(), context);
            let typ = match width {
                Some(width) => Type::QubitArray(ArrayDims::D1(width as usize)),
                None => Type::Qubit,
            };
            let symbol_id = context.new_binding(name_str.as_ref(), &typ, &q_decl);
            Some(asg::DeclareQuantum::new(symbol_id).to_stmt())
        }

        synast::Stmt::AssignmentStmt(assignment_stmt) => {
            assignment_stmt_to_asg_stmt(&assignment_stmt, context)
        }

        synast::Stmt::BreakStmt(_) => Some(asg::Stmt::Break),

        synast::Stmt::ContinueStmt(_) => Some(asg::Stmt::Continue),

        synast::Stmt::EndStmt(_) => Some(asg::Stmt::End),

        // Gate definition
        synast::Stmt::Gate(gate) => {
            if !context.symbol_table().in_global_scope() {
                // It would be better to use gate.gate_token(), that is, the word "gate" for the
                // location of the error. But that is of type `SyntaxToken`, not `AstNode` as
                // required. There is probably a solution.
                context.insert_error(NotInGlobalScopeError, &gate.name().unwrap());
            }
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
                        let block = block_expr_to_asg_type(gate.body().unwrap(), context);
            );
            let num_params = match params {
                Some(ref params) => params.len(),
                None => 0,
            };
            let gate_name_symbol_id = context.new_binding(
                name_node.string().as_ref(),
                &Type::Gate(
                    num_params,
                    qubits.len(),
                ),
                &name_node,
            );
            Some(asg::GateDefinition::new(gate_name_symbol_id, params, qubits, block).to_stmt())
        }

        synast::Stmt::Def(def_stmt) => {
            let name_node = def_stmt.name().unwrap();
            if !context.symbol_table().in_global_scope() {
                context.insert_error(NotInGlobalScopeError, &name_node);
            }
            with_scope!(context,  ScopeType::Subroutine,
                        let params = bind_typed_parameter_list(def_stmt.typed_param_list(), context);
                        let block = block_expr_to_asg_type(def_stmt.body().unwrap(), context);
            );
            // FIXME: Should we let subroutines have a parameterized type?
            // This would be very convenient for matching signatures.
            // let num_params = match params {
            //     Some(ref params) => params.len(),
            //     None => 0,
            // };
            let num_params = match params {
                Some(ref params) => params.len(),
                None => 0,
            };
            let return_type = def_stmt.return_signature()
                .and_then(|x| x.scalar_type())
                .map(|x| scalar_type_to_type(&x, true, context));
            let return_type = match return_type {
                Some(typ) => typ,
                _ => Type::Void
            };
            let def_name_symbol_id = context.new_binding(
                name_node.string().as_ref(),
                &Type::SubroutineDef(
                    types::SubroutineDef{ num_params, return_type: Box::new(return_type.clone()) }
                ),
                &name_node,
            );
            Some(asg::DefStmt::new(def_name_symbol_id, params.unwrap(), block, return_type).to_stmt())
        }

        synast::Stmt::Barrier(barrier) => {
            let gate_operands = qubit_list_to_asg_texpr(barrier.qubit_list(), context);
            Some(asg::Stmt::Barrier(asg::Barrier::new(Some(gate_operands))))
        }

        synast::Stmt::DelayStmt(delay_stmt) => {
            let gate_operands = qubit_list_to_asg_texpr(delay_stmt.qubit_list(), context);
            let duration = expr_to_asg_texpr(delay_stmt.designator().unwrap().expr(), context).unwrap();
            if !matches!(duration.get_type(), Type::Duration(_)) {
                context.insert_error(IncompatibleTypesError, &delay_stmt.designator().unwrap());
            }
            Some(asg::Stmt::Delay(asg::DelayStmt::new(
                duration,
                gate_operands,
            )))
        }

        synast::Stmt::Reset(reset) => {
            let gate_operand = reset.gate_operand().unwrap(); // FIXME: check this
            let gate_operand_asg = gate_operand_to_asg_texpr(gate_operand, context);
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

        synast::Stmt::ExprStmt(expr_stmt) => expr_stmt_to_asg_stmt(expr_stmt, context),

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
            let rhs = expr_to_asg_texpr(alias_stmt.expr(), context).unwrap();
            // Bind the name to the RHS, giving it the same type as the RHS.
            let symbol_id = context.new_binding(name_str.as_ref(), rhs.get_type(), &alias_stmt);
            Some(asg::Alias::new(symbol_id, rhs).to_stmt())
        }

        synast::Stmt::Cal(_)
            | synast::Stmt::DefCal(_)
            | synast::Stmt::DefCalGrammar(_)
            // The following two should probably be removed from the syntax parser.
            | synast::Stmt::LetStmt(_)
            | synast::Stmt::Measure(_)
            => panic!("Unsupported statement {stmt}"),
    }
}

fn expr_stmt_to_asg_stmt(expr_stmt: synast::ExprStmt, context: &mut Context) -> Option<asg::Stmt> {
    use synast::Expr::{GPhaseCallExpr, GateCallExpr, ModifiedGateCallExpr};
    // At present, three expressions, those for gate calls, are handled specially. In oq3_syntax, gate calls
    // are expressions wrapped in `ExprStmt`. But in the ASG, gate calls are are variant of `Stmt`. All
    // other `synast::ExprStmt` are translated to `asg::ExprStmt`.
    match expr_stmt.expr() {
        Some(GateCallExpr(gate_call)) => {
            gate_call_expr_to_asg_stmt(gate_call, Vec::<asg::GateModifier>::new(), context)
        }
        Some(ModifiedGateCallExpr(mod_gate_call)) => {
            let modifiers = mod_gate_call
                .modifiers()
                .map(|modifier| match modifier {
                    synast::Modifier::InvModifier(_) => asg::GateModifier::Inv,

                    synast::Modifier::PowModifier(pow_mod) => {
                        let exponent =
                            paren_expr_to_asg_texpr(pow_mod.paren_expr().unwrap(), context)
                                .unwrap();
                        asg::GateModifier::Pow(exponent)
                    }

                    synast::Modifier::CtrlModifier(ctrl_mod) => {
                        let exponent = ctrl_mod
                            .paren_expr()
                            .and_then(|x| paren_expr_to_asg_texpr(x, context));
                        asg::GateModifier::Ctrl(exponent)
                    }

                    synast::Modifier::NegCtrlModifier(neg_ctrl_mod) => {
                        let exponent = neg_ctrl_mod
                            .paren_expr()
                            .and_then(|x| paren_expr_to_asg_texpr(x, context));
                        asg::GateModifier::NegCtrl(exponent)
                    }
                })
                .collect();

            // `synast::ModifiedGateCallExpr` may wrap either gate call or gphase call,
            // which is treated separately.
            if let Some(gate_call) = mod_gate_call.gate_call_expr() {
                gate_call_expr_to_asg_stmt(gate_call, modifiers, context)
            } else {
                let gphase = mod_gate_call.g_phase_call_expr().unwrap();
                let arg = expr_to_asg_texpr(gphase.arg(), context).unwrap();
                Some(asg::Stmt::ModifiedGPhaseCall(asg::ModifiedGPhaseCall::new(
                    arg, modifiers,
                )))
            }
        }

        Some(GPhaseCallExpr(gphase)) => {
            let arg = expr_to_asg_texpr(gphase.arg(), context).unwrap();
            Some(asg::Stmt::GPhaseCall(asg::GPhaseCall::new(arg)))
        }

        syn_expr => {
            let expr = expr_to_asg_texpr(syn_expr, context);
            expr.map_or_else(
                || panic!("expr::ExprStmt is None. Expression not implemented in the ASG."),
                |ex| Some(asg::Stmt::ExprStmt(ex)),
            )
        }
    }
}

fn paren_expr_to_asg_texpr(
    paren_expr: synast::ParenExpr,
    context: &mut Context,
) -> Option<asg::TExpr> {
    expr_to_asg_texpr(paren_expr.expr(), context)
}

fn negative_float_number_to_asg_type(f: synast::FloatNumber) -> asg::FloatLiteral {
    let num = f.value().unwrap();
    let float = format!("-{num}");
    asg::FloatLiteral::new(float)
}

fn negative_int_to_asg_type(n: synast::IntNumber) -> asg::IntLiteral {
    let num = n.value_u128().unwrap(); // fn value_u128 is kind of a hack
    asg::IntLiteral::new(num, false) // `false` means negative
}

fn expr_to_asg_texpr(
    expr_maybe: Option<synast::Expr>,
    context: &mut Context,
) -> Option<asg::TExpr> {
    let expr = expr_maybe?;
    match expr {
        // FIXME: Ugh. could clean up logic here
        // It is convenient to rewrite literals wrapped in unary minus as literals
        // with negative values.
        synast::Expr::PrefixExpr(prefix_expr) => match prefix_expr.op_kind() {
            Some(synast::UnaryOp::Neg) => match prefix_expr.expr() {
                Some(synast::Expr::Literal(ref literal)) => Some(match literal.kind() {
                    synast::LiteralKind::FloatNumber(f) => {
                        negative_float_number_to_asg_type(f).to_texpr()
                    }
                    synast::LiteralKind::IntNumber(n) => negative_int_to_asg_type(n).to_texpr(),
                    _ => {
                        panic!("Only integers and floats are supported as operands to unary minus.")
                    }
                }),

                Some(synast::Expr::TimingLiteral(ref timing_literal)) => {
                    match timing_literal.time_unit().unwrap() {
                        synast::TimeUnit::Imaginary => {
                            Some(match timing_literal.literal().unwrap().kind() {
                                synast::LiteralKind::FloatNumber(f) => {
                                    negative_float_number_to_asg_type(f).to_imaginary_texpr()
                                }
                                synast::LiteralKind::IntNumber(n) => {
                                    negative_int_to_asg_type(n).to_imaginary_texpr()
                                }
                                _ => panic!("You have found a bug in oq3_syntax or oq3_parser"),
                            })
                        }
                        _ => {
                            panic!("Only floats are supported as operands to unary minus.")
                        }
                    }
                }

                Some(synexpr) => Some(
                    asg::UnaryExpr::new(
                        asg::UnaryOp::Minus,
                        expr_to_asg_texpr(Some(synexpr), context).unwrap(),
                    )
                    .to_texpr(),
                ),

                None => {
                    panic!("You have found a bug in oq3_parser. No operand to unary minus found.")
                }
            },
            Some(op) => {
                panic!("Unary operators other than minus are not supported. Found '{op:?}.'")
            }
            _ => panic!("You have found a bug in oq3_parser. No operand to unary operator found."),
        },

        synast::Expr::ParenExpr(paren_expr) => paren_expr_to_asg_texpr(paren_expr, context),

        synast::Expr::BinExpr(bin_expr) => {
            let synast_op = bin_expr.op_kind().unwrap();
            let left_syn = bin_expr.lhs();
            let right_syn = bin_expr.rhs();

            let op = binary_op_to_asg_type(synast_op);
            let left = expr_to_asg_texpr(left_syn, context).unwrap();
            let right = expr_to_asg_texpr(right_syn, context).unwrap();
            // There are no binary ops that accept quantum operands.
            if left.get_type().is_quantum() {
                // Generate the ast node again, for the borrow checker. But we are already
                // on the sad path.
                context.insert_error(IncompatibleTypesError, &bin_expr.lhs().unwrap());
            }
            if right.get_type().is_quantum() {
                context.insert_error(IncompatibleTypesError, &bin_expr.rhs().unwrap());
            }
            Some(asg::BinaryExpr::new_texpr_with_cast(op, left, right))
        }

        synast::Expr::Literal(ref literal) => literal_to_asg_texpr(literal),

        // We also handle imaginary literals here along with timing literals.
        // This makes no sense on the level of semantics. But at all eariler points,
        // from lexing to here, it is trivial to treat the imaginary suffix `im` as
        // if it were a timing suffix. We could try to fix this, but it would add
        // code paths and complexity from lexing on up.
        synast::Expr::TimingLiteral(ref timing_literal) => {
            let ast_time_unit = timing_literal.time_unit().unwrap();
            if matches!(ast_time_unit, synast::TimeUnit::Imaginary) {
                return match timing_literal.literal().unwrap().kind() {
                    synast::LiteralKind::IntNumber(int_num) => {
                        let num = int_num.value_u128().unwrap();
                        Some(asg::IntLiteral::new(num, true).to_imaginary_texpr())
                    }
                    synast::LiteralKind::FloatNumber(float_num) => {
                        let num = float_num.value().unwrap();
                        Some(asg::FloatLiteral::new(num).to_imaginary_texpr())
                    }
                    _ => panic!("You have found a bug in oq3_syntax or oq3_parser"),
                };
            }
            let time_unit = match ast_time_unit {
                synast::TimeUnit::Second => asg::TimeUnit::Second,
                synast::TimeUnit::MilliSecond => asg::TimeUnit::MilliSecond,
                synast::TimeUnit::MicroSecond => asg::TimeUnit::MicroSecond,
                synast::TimeUnit::NanoSecond => asg::TimeUnit::NanoSecond,
                synast::TimeUnit::Cycle => asg::TimeUnit::Cycle,
                // Imaginary was handled above.
                synast::TimeUnit::Imaginary => unreachable!(),
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
            let (sym, typ) = lookup_identifier(&identifier, context);
            Some(asg::TExpr::new(asg::Expr::Identifier(sym), typ))
        }

        synast::Expr::HardwareQubit(hwq) => Some(hardware_qubit_to_asg_type(&hwq).to_texpr()),

        // Range expressions are not allowed everywhere. Maybe remove from expr tree
        synast::Expr::RangeExpr(range_expr) => {
            Some(range_expression_to_asg_type(range_expr, context).to_texpr())
        }

        synast::Expr::IndexExpr(index_expr) => {
            let expr = expr_to_asg_texpr(index_expr.expr(), context);
            let index = index_operator_to_asg_type(index_expr.index_operator().unwrap(), context);
            Some(asg::IndexExpression::new(expr.unwrap(), index).to_texpr())
        }

        synast::Expr::IndexedIdentifier(indexed_identifier) => {
            let (indexed_identifier, _typ) =
                indexed_identifier_to_asg_type(&indexed_identifier, context);
            Some(indexed_identifier.to_texpr())
        }

        synast::Expr::MeasureExpression(ref measure_expr) => {
            let gate_operand = measure_expr.gate_operand().unwrap(); // FIXME: check this
            let gate_operand_asg = gate_operand_to_asg_texpr(gate_operand, context);
            Some(asg::MeasureExpression::new(gate_operand_asg).to_texpr())
        }

        synast::Expr::ReturnExpr(ref return_expr) => {
            let expr_asg = expr_to_asg_texpr(return_expr.expr(), context);
            if context.symbol_table().current_scope_type() == ScopeType::Global {
                context.insert_error(ReturnInGlobalScopeError, &expr);
            }
            Some(asg::ReturnExpression::new(expr_asg).to_texpr())
        }

        synast::Expr::CastExpression(cast) => {
            let typ = scalar_type_to_type(&cast.scalar_type().unwrap(), true, context);
            let expr = expr_to_asg_texpr(cast.expr(), context);
            Some(asg::Cast::new(expr.unwrap(), typ).to_texpr())
        }

        synast::Expr::CallExpr(call_expr) => Some(call_expr_to_asg_texpr(call_expr, context)),

        // Followng may be a parser error. But I think we will need to support BlockExpr anywhere here.
        synast::Expr::BlockExpr(_) => panic!("BlockExpr not supported."),

        synast::Expr::ArrayExpr(_) => panic!("ArrayExpr not supported {expr:?}"),
        synast::Expr::ArrayLiteral(_) => panic!("ArrayLiteral not supported {expr:?}"),
        synast::Expr::BoxExpr(_) => panic!("BoxExpr not supported {expr:?}"),
        synast::Expr::GateCallExpr(_)
        | synast::Expr::GPhaseCallExpr(_)
        | synast::Expr::ModifiedGateCallExpr(_) => panic!("You have found a bug in oq3_parser."),
    }
}

fn set_expression_to_asg_type(
    set_expression: synast::SetExpression,
    context: &mut Context,
) -> asg::SetExpression {
    asg::SetExpression::new(expression_list_to_asg_texpr(
        set_expression.expression_list().unwrap(),
        context,
    ))
}

fn range_expression_to_asg_type(
    range_expr: synast::RangeExpr,
    context: &mut Context,
) -> asg::RangeExpression {
    let (start, step, stop) = range_expr.start_step_stop();
    let start = expr_to_asg_texpr(start, context).unwrap();
    let stop = expr_to_asg_texpr(stop, context).unwrap();
    let step = expr_to_asg_texpr(step, context);
    asg::RangeExpression::new(start, step, stop)
}

fn gate_call_expr_to_asg_stmt(
    gate_call_expr: synast::GateCallExpr,
    modifiers: Vec<asg::GateModifier>,
    context: &mut Context,
) -> Option<asg::Stmt> {
    let gate_operands: Vec<_> = qubit_list_to_asg_texpr(gate_call_expr.qubit_list(), context);
    let param_list = gate_call_expr
        .arg_list()
        .map(|ex| expression_list_to_asg_texpr(ex.expression_list().unwrap(), context));
    let num_params = match param_list {
        Some(ref params) => params.len(),
        None => 0,
    };
    let gate_id = gate_call_expr.identifier();
    // FIXME: make sure we are efficient with strings
    let gate_name = gate_call_expr.identifier().unwrap().string();
    let (symbol_result, gate_type) = context
        .lookup_gate_symbol(gate_name.as_ref(), gate_id.as_ref().unwrap())
        .as_tuple();
    // FIXME: I think we can use if-let with a pattern here. But
    // arm with (0, 0) looks like it can never be taken.
    if matches!(gate_type, Type::Gate(_, _)) {
        let (def_num_params, def_num_qubits) = match gate_type {
            Type::Gate(np, nq) => (np, nq),
            _ => (0, 0),
        };
        if def_num_params != num_params {
            if num_params != 0 {
                // If num params is mismatched, locate error at list of params supplied.
                context.insert_error(NumGateParamsError, &gate_call_expr.arg_list().unwrap());
            } else {
                // If no params are supplied, but some are expected, locate error at gate name.
                context.insert_error(NumGateParamsError, &gate_id.unwrap());
            }
        }
        let num_qubits: usize = gate_operands.len();
        if def_num_qubits != num_qubits {
            if num_qubits == 0 {
                // This probably can't happen because if there are no qubit args,
                // then the call is not recognized syntactially as a gate call.
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

fn call_expr_to_asg_texpr(call_expr: synast::CallExpr, context: &mut Context) -> asg::TExpr {
    let param_list = call_expr
        .arg_list()
        .map(|ex| expression_list_to_asg_texpr(ex.expression_list().unwrap(), context));
    let subroutine_id = call_expr.identifier();
    let subroutine_name = call_expr.identifier().unwrap().string();
    let (symbol_result, call_type) = context
        .lookup_symbol(subroutine_name.as_ref(), subroutine_id.as_ref().unwrap())
        .as_tuple();
    let def_type = match call_type {
        Type::SubroutineDef(def_type) => def_type,
        _ => panic!("programming error: expected Type::Def variant"),
    };
    let expected_num_params = def_type.num_params;
    // number of params actually passed in call.
    let num_params = match param_list {
        Some(ref params) => params.len(),
        None => 0,
    };
    if expected_num_params != num_params {
        context.insert_error(NumDefParamsError, &call_expr.arg_list().unwrap());
    }
    let typ = def_type.return_type;
    asg::SubroutineCall::new(symbol_result, param_list).to_texpr(*typ)
}

fn gate_operand_to_asg_texpr(
    gate_operand: synast::GateOperand,
    context: &mut Context,
) -> asg::TExpr {
    match gate_operand {
        synast::GateOperand::HardwareQubit(ref hwq) => {
            asg::GateOperand::HardwareQubit(hardware_qubit_to_asg_type(hwq))
                .to_texpr(Type::HardwareQubit)
        }
        synast::GateOperand::Identifier(ref identifier) => {
            let (sym, typ) = lookup_identifier(identifier, context);
            if !matches!(typ, Type::Qubit | Type::HardwareQubit | Type::QubitArray(_)) {
                context.insert_error(IncompatibleTypesError, &gate_operand);
            }
            asg::GateOperand::Identifier(sym).to_texpr(typ)
        }
        synast::GateOperand::IndexedIdentifier(ref indexed_identifier) => {
            let (indexed_identifier, typ) =
                indexed_identifier_to_asg_type(indexed_identifier, context);
            if !matches!(typ, Type::QubitArray(_)) {
                context.insert_error(IncompatibleTypesError, &gate_operand);
            }
            asg::GateOperand::IndexedIdentifier(indexed_identifier).to_texpr(typ)
        }
    }
}

fn get_ast_designator_expression(arg: Option<&synast::Designator>) -> Option<synast::Expr> {
    arg.and_then(|desg| desg.expr())
    // NOTE: Other uses of this pattern above have certain error checking,
    // but because it was not standardized, it is not included
    // in this helper function
}

fn index_operator_to_asg_type(
    index_op: synast::IndexOperator,
    context: &mut Context,
) -> asg::IndexOperator {
    match index_op.index_kind().unwrap() {
        synast::IndexKind::SetExpression(set_expression) => {
            asg::IndexOperator::SetExpression(set_expression_to_asg_type(set_expression, context))
        }

        synast::IndexKind::ExpressionList(expression_list) => asg::IndexOperator::ExpressionList(
            expression_list_to_asg_type(expression_list, context),
        ),
    }
}

fn expression_list_to_asg_type(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> asg::ExpressionList {
    asg::ExpressionList::new(expression_list_to_asg_texpr(expression_list, context))
}

fn qubit_list_to_asg_texpr(
    qubit_list: Option<synast::QubitList>,
    context: &mut Context,
) -> Vec<asg::TExpr> {
    // Warning, I think map overlooks None. This can cause a bug in the present case.
    // Because None means a coding error upstream. Better to blow up here.
    qubit_list
        .unwrap()
        .gate_operands()
        .map(|qubit| gate_operand_to_asg_texpr(qubit, context))
        .collect()
}

// Return a Vec of TExpr.  There is no reason to return an iterator, because if it were an
// iterator, then at every call site this would be collected immediately.
fn expression_list_to_asg_texpr(
    expression_list: synast::ExpressionList,
    context: &mut Context,
) -> Vec<asg::TExpr> {
    expression_list
        .exprs()
        .filter_map(|x| expr_to_asg_texpr(Some(x), context))
        .collect()
}

fn binary_op_to_asg_type(synast_op: synast::BinaryOp) -> asg::BinaryOp {
    match synast_op {
        synast::BinaryOp::ArithOp(arith_op) => {
            use asg::BinaryOp::ArithOp;
            use synast::ArithOp::*;
            match arith_op {
                Add => ArithOp(asg::ArithOp::Add),
                Mul => ArithOp(asg::ArithOp::Mul),
                Sub => ArithOp(asg::ArithOp::Sub),
                Div => ArithOp(asg::ArithOp::Div),
                Rem | Shl | Shr | BitOr | BitXor | BitAnd => panic!("Unsupported binary operator"),
            }
        }
        synast::BinaryOp::CmpOp(cmp_op) => {
            use asg::BinaryOp::CmpOp;
            use synast::CmpOp::*;
            match cmp_op {
                Eq { negated: false } => CmpOp(asg::CmpOp::Eq),
                Eq { negated: true } => CmpOp(asg::CmpOp::Neq),
                Ord { .. } => {
                    panic!("Comparision operators other than `=` and `!=` are not supported.")
                }
            }
        }
        synast::BinaryOp::ConcatenationOp => asg::BinaryOp::ConcatenationOp,
        synast::BinaryOp::LogicOp(_) => panic!("Binary logic operators unsupported."),
        synast::BinaryOp::Assignment { .. } => panic!("Unsupported binary operator"),
    }
}

fn literal_to_asg_texpr(literal: &synast::Literal) -> Option<asg::TExpr> {
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

        synast::LiteralKind::Byte(_)
        | synast::LiteralKind::Char(_)
        | synast::LiteralKind::String(_) => todo!(),
    };
    Some(literal_texpr)
}

// Convert a block of statements in the AST to a list of ASG statements
// We don't convert to asg::Block, because these lists of statements go into
// other block-like structures as well.
fn block_expr_to_asg_stmt_list(block: synast::BlockExpr, context: &mut Context) -> Vec<asg::Stmt> {
    block
        .statements()
        .filter_map(|syn_stmt| stmt_to_asg_stmt(syn_stmt, context))
        .collect::<Vec<_>>()
}

fn block_expr_to_asg_type(block_synast: synast::BlockExpr, context: &mut Context) -> asg::Block {
    asg::Block::new(block_expr_to_asg_stmt_list(block_synast, context))
}

// If the body of the for loop is a single statement, with no curlies,
// we wrap it in a `Block`. The result is the same as if there were
// as single statement in a pair of `{` `}`.
fn block_or_stmt_to_asg_type(val: oq3_syntax::BlockOrStmt, context: &mut Context) -> asg::Block {
    match val {
        oq3_syntax::BlockOrStmt::BlockExpr(body) => block_expr_to_asg_type(body, context),
        oq3_syntax::BlockOrStmt::Stmt(stmt) => {
            asg::Block::new(vec![stmt_to_asg_stmt(stmt, context).unwrap()])
        }
    }
}

// Convert AST scalar type to a `types::Type`
fn scalar_type_to_type(
    scalar_type: &synast::ScalarType,
    isconst: bool,
    context: &mut Context,
) -> Type {
    // If the scalar type is `complex`, then scalar_type.scalar_type() will return
    // the base type, which is a float type. If scalar_type.scalar_type() is `None`,
    // then it is a simple scalar type and the designator is in scalar_type.designator.
    // Note that this is the point where we throw away the token `float`, which is
    // superfluous.
    // In OQ3 source, `complex` types have a different syntax from other scalar types.
    // Eg, we write `int[32]`, but we don't write `complex[32]`, but rather `complex[float[32]]`.
    // However `Type::Complex` has exactly the same form as other scalar types. In this case
    // `width` is understood to be the width of each of real and imaginary components.
    let designator = if let Some(float_type) = scalar_type.scalar_type() {
        // complex
        float_type.designator()
    } else {
        // not complex
        scalar_type.designator()
    };
    let width = designator_to_asg(designator.as_ref(), context);
    match scalar_type.kind() {
        synast::ScalarTypeKind::Angle => Type::Angle(width, isconst.into()),
        synast::ScalarTypeKind::Bit => match width {
            Some(width) => Type::BitArray(ArrayDims::D1(width as usize), isconst.into()),
            None => Type::Bit(isconst.into()),
        },
        synast::ScalarTypeKind::Bool => Type::Bool(isconst.into()),
        synast::ScalarTypeKind::Complex => Type::Complex(width, isconst.into()),
        synast::ScalarTypeKind::Duration => Type::Duration(isconst.into()),
        synast::ScalarTypeKind::Float => Type::Float(width, isconst.into()),
        synast::ScalarTypeKind::Int => Type::Int(width, isconst.into()),
        synast::ScalarTypeKind::Stretch => Type::Stretch(isconst.into()),
        synast::ScalarTypeKind::UInt => Type::UInt(width, isconst.into()),
        // The spec says a qubit register is equivalent to a 1D qubit array.
        // Trying to maintain a non-existent distinction would lead to bad confusion.
        // This is the point where we eliminate the distinction. (Code is repeated elsewhere.)
        synast::ScalarTypeKind::Qubit => match width {
            Some(width) => Type::QubitArray(ArrayDims::D1(width as usize)),
            None => Type::Qubit,
        },
        synast::ScalarTypeKind::None => panic!("You have found a bug in oq3_parser"),
    }
}

fn designator_to_asg(
    designator: Option<&synast::Designator>,
    context: &mut Context,
) -> Option<u32> {
    //  Get the width of the type.
    match get_ast_designator_expression(designator) {
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
        Some(synast::Expr::Identifier(identifier)) => {
            let (sym, typ) = lookup_identifier(&identifier, context);
            if typ.is_const() {
                let const_value = context.get_const_value(sym.unwrap());
                let width = match u32::try_from(const_value.unwrap()) {
                    Ok(width) => width,
                    Err(_) => {
                        context.insert_error(InvalidDesignatorError, &identifier);
                        // It's not clear what value to substitute for the width if we don't have a valid one.
                        // We choose zero.
                        0
                    }
                };
                Some(width)
            } else {
                None
            }
        }
        Some(expr) => panic!("Unsupported designator type: {:?}", type_name_of(expr)),
        None => None,
    }
}

// Return `true` if the literal
fn can_cast_literal(lhs_type: &Type, init_type: &Type, literal: &asg::Literal) -> bool {
    if matches!(lhs_type, &Type::UInt(..)) {
        if let asg::Literal::Int(intlit) = literal {
            return *intlit.sign();
        }
    }
    types::can_cast_literal(lhs_type, init_type)
}

fn classical_declaration_statement_to_asg_stmt(
    type_decl: &synast::ClassicalDeclarationStatement,
    context: &mut Context,
) -> asg::Stmt {
    if type_decl.array_type().is_some() {
        if !context.symbol_table().in_global_scope() {
            context.insert_error(NotInGlobalScopeError, type_decl);
        }
        panic!("Array types are not supported yet in the ASG");
    }
    let scalar_type = type_decl.scalar_type().unwrap();
    let lhs_type = scalar_type_to_type(&scalar_type, type_decl.const_token().is_some(), context);
    let name_str = type_decl.name().unwrap().string();
    let initializer = expr_to_asg_texpr(type_decl.expr(), context);
    let symbol_id = context.new_binding(name_str.as_ref(), &lhs_type, type_decl);
    // If there is an initializer, check that types are compatible and if there is
    // an implicit cast, make it explicit.
    if let Some(initializer) = initializer {
        let init_type = initializer.get_type();
        // TODO: Find what we want here. Equal up to base type, constness.
        // Also literals are probably treated differently.
        // Is this in the spec, or somewhat up to the implementation?
        if types::equal_up_to_constness(&lhs_type, init_type) {
            return asg::DeclareClassical::new(symbol_id, Some(initializer)).to_stmt();
        }
        // From this point, we need to cast, if possible.
        // So, we either cast, or record an error saying types are incompatible.
        if let asg::Expr::Literal(literal) = initializer.expression() {
            if can_cast_literal(&lhs_type, init_type, literal) {
                let new_initializer =
                    asg::Cast::new(initializer.clone(), lhs_type.clone()).to_texpr();
                return declare_classical_helper(symbol_id, Some(new_initializer), context);
            } else {
                context.insert_error(IncompatibleTypesError, type_decl);
                return declare_classical_helper(symbol_id, Some(initializer), context);
            }
        }
        let promoted_type = types::promote_types_not_equal(&lhs_type, init_type);
        let new_initializer = if types::equal_up_to_constness(&promoted_type, &lhs_type) {
            // Very often `promoted_type` is correct. But it may be off by constness.
            // We cast to exactly the type of the lhs, by cloning it.
            // The important thing is to filter out cases where casting is either
            // not necessary, or not allowed.
            asg::Cast::new(initializer.clone(), lhs_type.clone()).to_texpr()
        } else {
            // Either the type can't be promoted,
            // or promote_types says to promote lhs to rhs, which is allowed
            // for some binary expressions, but not for assignment because the
            // type of the lhs is fixed.
            if promoted_type == Type::Void || &promoted_type == init_type {
                context.insert_error(IncompatibleTypesError, type_decl);
            }
            initializer
        };
        return declare_classical_helper(symbol_id, Some(new_initializer), context);
    }
    declare_classical_helper(symbol_id, initializer, context)
}

fn declare_classical_helper(
    symbol_id: SymbolIdResult,
    initializer: Option<asg::TExpr>,
    context: &mut Context,
) -> asg::Stmt {
    if let Some(initializer) = &initializer {
        if initializer.get_type().is_const() {
            context.insert_const_value(symbol_id.clone().unwrap(), initializer.clone());
        }
    }
    asg::DeclareClassical::new(symbol_id, initializer).to_stmt()
}

fn io_declaration_statement_to_asg_stmt(
    type_decl: &synast::IODeclarationStatement,
    context: &mut Context,
) -> asg::Stmt {
    if type_decl.array_type().is_some() {
        panic!("Array types are not supported yet in the ASG");
    }
    let scalar_type = type_decl.scalar_type().unwrap();
    // Assume that input / ouput variables are not constant.
    let typ = scalar_type_to_type(&scalar_type, false, context);
    let name_str = type_decl.name().unwrap().string();
    let symbol_id = context.new_binding(name_str.as_ref(), &typ, &type_decl.name().unwrap());
    if type_decl.input_token().is_some() {
        asg::InputDeclaration::new(symbol_id).to_stmt()
    } else {
        asg::OutputDeclaration::new(symbol_id).to_stmt()
    }
}

// FIXME: Refactor this. It was done in a hurry.
fn assignment_stmt_to_asg_stmt(
    assignment_stmt: &synast::AssignmentStmt,
    context: &mut Context,
) -> Option<asg::Stmt> {
    let nameb = assignment_stmt.identifier(); // LHS of assignment
                                              // LHS is an identifier
    if let Some(name) = &nameb {
        let name_str = name.string();
        let mut expr = expr_to_asg_texpr(assignment_stmt.rhs(), context).unwrap(); // rhs of `=` operator

        let (symbol_id, symbol_type) = context.lookup_symbol(name_str.as_str(), name).as_tuple();
        let symbol_ok = symbol_id.is_ok();
        let is_mutating_const = symbol_ok && symbol_type.is_const();
        let lvalue = asg::LValue::Identifier(symbol_id);
        let expr_type = expr.get_type();
        // Check that types match, but only if lhs has been declared, in which case
        // recording a type error would be redundant.
        if symbol_ok && expr_type != &symbol_type {
            if expr_type.equal_up_to_dims(&symbol_type) {
                context.insert_error(IncompatibleDimensionError, assignment_stmt);
            } else if let asg::Expr::Literal(asg::Literal::Int(intlit)) = expr.expression() {
                // Cast positive integer literal to UInt
                if matches!(symbol_type, Type::UInt(..)) {
                    if *intlit.sign() {
                        // FIXME: We are casting to unsigned if the int literal is positive.
                        // But we are not checking the width.
                        expr = asg::Cast::new(expr, symbol_type).to_texpr()
                    } else {
                        // We call this cast error. Not a great name
                        // In Rust, `let x: u32 = -1;` gives "cannot apply unary operator `-` to type `u32`"
                        // But we have combined `-` with the literal already during parsing (I think?).
                        // In Julia, `x::UInt = -1` throws `InexactError`.
                        context.insert_error(CastError, assignment_stmt);
                    }
                }
            } else {
                let promoted_type = types::promote_types(&symbol_type, expr_type);
                if promoted_type == symbol_type {
                    expr = asg::Cast::new(expr, promoted_type).to_texpr()
                } else {
                    context.insert_error(IncompatibleTypesError, assignment_stmt);
                }
            }
        }
        let stmt_asg = Some(asg::Assignment::new(lvalue, expr).to_stmt());
        if is_mutating_const {
            context.insert_error(MutateConstError, assignment_stmt);
        }
        return stmt_asg;
    }
    // LHS is *not* an identifier, rather an indexed identifier
    let indexed_identifier_ast = assignment_stmt.indexed_identifier().unwrap();
    let (indexed_identifier, typ) =
        indexed_identifier_to_asg_type(&indexed_identifier_ast, context);
    // Examine number of indexing operators. Eg. `d[1][2]` has two operations.
    // We only check expressions with a single indexing op for now.
    if indexed_identifier.indexes().len() == 1 {
        let index = &indexed_identifier.indexes()[0];
        if index.num_dims() > typ.num_dims() {
            context.insert_error(TooManyIndexes, &indexed_identifier_ast);
        }
        // let mut num_out_dims = typ.num_dims();
        // match index {
        //     asg::IndexOperator::SetExpression(_) => { num_out_dims = 1;}
        //     asg::IndexOperator::ExpressionList(elist) => {
        //         for expr in &elist.expressions {
        //             dbg!(expr);
        //         }
        //     }
        // }
    }
    let expr = expr_to_asg_texpr(assignment_stmt.rhs(), context).unwrap(); // rhs of `=` operator
    let lvalue = asg::LValue::IndexedIdentifier(indexed_identifier);
    Some(asg::Assignment::new(lvalue, expr).to_stmt())
}

//
// These functions (fn ast_ ...) convert oq3_syntax::ast (alias synast) structs to ast structs.
// These ast structs are not yet wrapped in `enum Stmt` or `enum Expr`, etc.
// These functions exist because some of these ast structs will be wrapped into the
// ast tree in different ways. So their construction is factored out.
//
// It seems like it would be a good idea to use these functions as much as possible. However,
// Sometimes construction of the object on the one hand and wrapping it in a type and in an
// `enum` variant on the other are not so easily separated into two parts. See for example
// asg::BinaryExpr.new_texpr_with_cast.
//

fn hardware_qubit_to_asg_type(hwq: &synast::HardwareQubit) -> asg::HardwareQubit {
    asg::HardwareQubit::new(hwq.string())
}

fn lookup_identifier(
    identifier: &synast::Identifier,
    context: &mut Context,
) -> (SymbolIdResult, Type) {
    let name_str = identifier.string();
    let (symbol_id, typ) = context
        .lookup_symbol(name_str.as_str(), identifier)
        .as_tuple();
    (symbol_id, typ)
}

fn indexed_identifier_to_asg_type(
    indexed_identifier: &synast::IndexedIdentifier,
    context: &mut Context,
) -> (asg::IndexedIdentifier, Type) {
    let identifier = indexed_identifier.identifier().unwrap().string();
    let (symbol_id, typ) = context
        .lookup_symbol(identifier.as_ref(), indexed_identifier)
        .as_tuple();
    let indexes = indexed_identifier
        .index_operators()
        .map(|index| index_operator_to_asg_type(index, context))
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

fn bind_typed_parameter_list(
    inparam_list: Option<synast::TypedParamList>,
    context: &mut Context,
) -> Option<Vec<SymbolIdResult>> {
    inparam_list.map(|param_list| {
        param_list
            .typed_params()
            .map(|param| {
                let typ = scalar_type_to_type(&param.scalar_type().unwrap(), false, context);
                let namestr = param.name().unwrap().string();
                context.new_binding(namestr.as_ref(), &typ, &param)
            })
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
