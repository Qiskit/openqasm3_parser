// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// The definition of the abstract semantic graph (ASG) as well as the API for using it.
// Construction of this typed ASG from syntactic AST is in syntax_to_semantics.rs

// SymbolIdResult can represent a valid symbol symbol, via a symbol id, or a semantic error.
// We need to insert SymbolIdResult everywhere a symbol is needed in the semantic tree.
// Because, although this ASG can assume synactic correctness, we need to track
// semantic errors and continue to build the semantic ASG.

use crate::symbols::SymbolIdResult; // SymbolIdResult = Result<SymbolId, SymbolError>
use crate::types;
use crate::types::{ArrayDims, IsConst, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub version: Option<OpenQASMVersion>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpenQASMVersion {
    major: usize,
    minor: usize,
}

impl OpenQASMVersion {
    pub fn new(major: usize, minor: usize) -> OpenQASMVersion {
        OpenQASMVersion { major, minor }
    }

    pub fn major(&self) -> usize {
        self.major
    }

    pub fn minor(&self) -> usize {
        self.minor
    }
}

impl Program {
    pub fn new() -> Program {
        Program {
            version: None,
            stmts: Vec::<Stmt>::new(),
        }
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }

    pub fn insert_stmt(&mut self, stmt: Stmt) {
        let _ = &self.stmts.push(stmt);
    }

    // FIXME: This should probably log a semantic error rather than panic.
    // The check should be done when checking syntax.
    // This check may be overly restrictive, as one might want to manipulate
    // or construct a `Program` in a way other than sequentially translating
    // statements.
    pub fn set_version(&mut self, version: OpenQASMVersion) {
        if self.version.is_some() {
            panic!("OpenQASM version cannot be set more than once");
        }
        self.version = Some(version);
    }

    pub fn version(&self) -> Option<&OpenQASMVersion> {
        self.version.as_ref()
    }

    // FIXME: must exist idiomatic rust for managing these modes
    /// Print the ASG using the pretty print `Debug` trait.
    pub fn print_asg_debug_pretty(&self) {
        for stmt in self.iter() {
            println!("{stmt:#?}");
        }
    }

    /// Print the ASG using the `Debug` trait.
    pub fn print_asg_debug(&self) {
        for stmt in self.iter() {
            println!("{stmt:?}\n");
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

// One way to forward methods for Iterator to Program statements.
impl std::ops::Deref for Program {
    type Target = Vec<Stmt>;

    fn deref(&self) -> &Self::Target {
        &self.stmts
    }
}

// Variants have the form XXX(XXX)
// The outer name XXX is the name of the variant
// The inner name XXX is the name of the struct or enum expressing the structure of the expression.
// If you try to import both symbols, one will clobber the other, of course.
// This is a confusing, but not uncommon, idiom.
// https://github.com/rust-lang/lang-team/issues/122#issuecomment-964459769
// > The bottom line is this: we all agree that it is a common, and annoying,
// > pattern in Rust today to have to make a struct for every enum variant and
// > then just have the enum wrap those structs. This gives you the ability to
// > have a "type for an enum variant", but is annoying and inconvenient.
//
// Note the variant Ident(Ident)
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    BinaryExpr(Box<BinaryExpr>),
    UnaryExpr(Box<UnaryExpr>),
    Literal(Literal),
    Cast(Box<Cast>),
    Identifier(SymbolIdResult),
    HardwareQubit(HardwareQubit),
    IndexExpression(Box<IndexExpression>),
    IndexedIdentifier(IndexedIdentifier),
    GateOperand(GateOperand),
    Return(Box<ReturnExpression>),
    SubroutineCall(SubroutineCall),
    MeasureExpression(Box<MeasureExpression>),
    SetExpression(SetExpression),
    RangeExpression(Box<RangeExpression>),
}

/// Typed expression implemented by tagging an `Expr` with a `Type`.
///
/// This solution to AST typing is under the heading
/// "Separate IR where nodes are decorated with types"
/// on this page: http://blog.ezyang.com/2013/05/the-ast-typing-problem/
/// The page includes a link to a discussion of advantages and disadvantages of this approach.
/// In order to gracefully handle errors, we frequently use `Option<TExpr>`. It would
/// be nice to have another ASG that can only represent semantically valid programs.
/// (or more precisely, the class of representable invalid programs is much smaller.)
/// But that would increase complexity. The link above and several like it discuss this problem.
/// Here is a link that sketches several solutions in Rust: https://lukasatkinson.de/dump/2023-09-02-ast-phases/
#[derive(Clone, Debug, PartialEq)]
pub struct TExpr {
    expression: Expr,
    ty: Type,
}

impl TExpr {
    pub fn new(expression: Expr, ty: Type) -> TExpr {
        TExpr { expression, ty }
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn expression(&self) -> &Expr {
        &self.expression
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Alias(Box<Alias>),
    AnnotatedStmt(Box<AnnotatedStmt>),
    Assignment(Assignment),
    Barrier(Barrier),
    Block(Block),
    Box, // stub
    Break,
    Cal, // stub
    Continue,
    DeclareClassical(Box<DeclareClassical>),
    DeclareQuantum(DeclareQuantum),
    DeclareHardwareQubit(DeclareHardwareQubit),
    DefStmt(DefStmt),
    DefCal, // stub
    Delay(DelayStmt),
    End,
    ExprStmt(TExpr),
    Extern, // stub
    ForStmt(ForStmt),
    GPhaseCall(GPhaseCall),
    GateCall(GateCall), // A statement because a gate call does not return anything
    GateDefinition(GateDefinition),
    InputDeclaration(InputDeclaration),
    OutputDeclaration(OutputDeclaration),
    If(If),
    Include(Include),
    ModifiedGPhaseCall(ModifiedGPhaseCall),
    NullStmt,            // for testing
    OldStyleDeclaration, // stub
    Pragma(Pragma),
    Reset(Reset),
    SwitchCaseStmt(SwitchCaseStmt),
    While(While),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Include {
    file_path: String,
}

impl Include {
    pub fn new<T: ToString>(file_path: T) -> Include {
        Include {
            file_path: file_path.to_string(),
        }
    }

    pub fn file_path(&self) -> &str {
        self.file_path.as_str()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::Include(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    annotation_text: String,
}

impl Annotation {
    pub fn new<T: ToString>(annotation_text: T) -> Annotation {
        Annotation {
            annotation_text: annotation_text.to_string(),
        }
    }

    pub fn annotation_text(&self) -> &str {
        self.annotation_text.as_ref()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnnotatedStmt {
    stmt: Stmt,
    annotations: Vec<Annotation>,
}

impl AnnotatedStmt {
    pub fn new(stmt: Stmt, annotations: Vec<Annotation>) -> AnnotatedStmt {
        if matches!(stmt, Stmt::AnnotatedStmt(..)) {
            panic!("Annotation of annotated statement is not allowed.");
        }
        AnnotatedStmt { stmt, annotations }
    }

    pub fn statement(&self) -> &Stmt {
        &self.stmt
    }

    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::AnnotatedStmt(Box::new(self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexExpression {
    expr: TExpr,
    index: IndexOperator,
}

impl IndexExpression {
    pub fn new(expr: TExpr, index: IndexOperator) -> IndexExpression {
        IndexExpression { expr, index }
    }

    pub fn to_expr(self) -> Expr {
        Expr::IndexExpression(Box::new(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::ToDo)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnExpression {
    value: Option<TExpr>,
}

impl ReturnExpression {
    pub fn new(value: Option<TExpr>) -> ReturnExpression {
        ReturnExpression { value }
    }

    pub fn to_texpr(self) -> TExpr {
        let return_type = match self.value() {
            Some(expr) => expr.get_type().clone(),
            None => Type::Void,
        };
        TExpr::new(Expr::Return(Box::new(self)), return_type)
    }

    pub fn value(&self) -> Option<&TExpr> {
        self.value.as_ref()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexedIdentifier {
    identifier: SymbolIdResult,
    indexes: Vec<IndexOperator>,
}

impl IndexedIdentifier {
    pub fn new(identifier: SymbolIdResult, indexes: Vec<IndexOperator>) -> IndexedIdentifier {
        IndexedIdentifier {
            identifier,
            indexes,
        }
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(Expr::IndexedIdentifier(self), Type::ToDo)
    }

    pub fn identifier(&self) -> &SymbolIdResult {
        &self.identifier
    }

    pub fn indexes(&self) -> &[IndexOperator] {
        &self.indexes
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IndexOperator {
    SetExpression(SetExpression),
    ExpressionList(ExpressionList),
}

impl IndexOperator {
    pub fn num_dims(&self) -> usize {
        match self {
            IndexOperator::SetExpression(_) => 1,
            IndexOperator::ExpressionList(elist) => elist.len(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionList {
    pub expressions: Vec<TExpr>,
}

// The rules regarding hardware qubits are not clear
#[derive(Clone, Debug, PartialEq)]
pub struct HardwareQubit {
    identifier: String,
}

impl HardwareQubit {
    pub fn new<T: ToString>(identifier: T) -> HardwareQubit {
        HardwareQubit {
            identifier: identifier.to_string(),
        }
    }

    pub fn identifier(&self) -> &str {
        &self.identifier
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(Expr::HardwareQubit(self), Type::HardwareQubit)
    }
}

impl ExpressionList {
    pub fn new(expressions: Vec<TExpr>) -> ExpressionList {
        ExpressionList { expressions }
    }

    pub fn len(&self) -> usize {
        self.expressions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.expressions.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LValue {
    Identifier(SymbolIdResult),
    IndexedIdentifier(IndexedIdentifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetExpression {
    expressions: Vec<TExpr>,
}

impl SetExpression {
    pub fn new(expressions: Vec<TExpr>) -> SetExpression {
        SetExpression { expressions }
    }

    pub fn expressions(&self) -> &[TExpr] {
        &self.expressions
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    lvalue: LValue,
    rvalue: TExpr,
}

impl Assignment {
    pub fn new(lvalue: LValue, rvalue: TExpr) -> Assignment {
        Assignment { lvalue, rvalue }
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::Assignment(self)
    }

    pub fn lvalue(&self) -> &LValue {
        &self.lvalue
    }

    pub fn rvalue(&self) -> &TExpr {
        &self.rvalue
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclareClassical {
    name: SymbolIdResult, // The name and type can be retrieved from SymbolId
    initializer: Option<TExpr>,
}

impl DeclareClassical {
    pub fn new(name: SymbolIdResult, initializer: Option<TExpr>) -> DeclareClassical {
        DeclareClassical { name, initializer }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn initializer(&self) -> Option<&TExpr> {
        self.initializer.as_ref()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::DeclareClassical(Box::new(self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputDeclaration {
    name: SymbolIdResult, // The name and type can be retrieved from SymbolId
}

impl InputDeclaration {
    pub fn new(name: SymbolIdResult) -> InputDeclaration {
        InputDeclaration { name }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::InputDeclaration(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct OutputDeclaration {
    name: SymbolIdResult, // The name and type can be retrieved from SymbolId
}

impl OutputDeclaration {
    pub fn new(name: SymbolIdResult) -> OutputDeclaration {
        OutputDeclaration { name }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::OutputDeclaration(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclareQuantum {
    name: SymbolIdResult,
}

impl DeclareQuantum {
    pub fn new(name: SymbolIdResult) -> DeclareQuantum {
        DeclareQuantum { name }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::DeclareQuantum(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclareHardwareQubit {
    name: HardwareQubit,
}

impl DeclareHardwareQubit {
    pub fn new(name: HardwareQubit) -> DeclareHardwareQubit {
        DeclareHardwareQubit { name }
    }

    pub fn name(&self) -> &HardwareQubit {
        &self.name
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::DeclareHardwareQubit(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    name: SymbolIdResult, // The name and type can be retrieved from SymbolId
    rhs: TExpr,
}

impl Alias {
    pub fn new(name: SymbolIdResult, rhs: TExpr) -> Alias {
        Alias { name, rhs }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn rhs(&self) -> &TExpr {
        &self.rhs
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::Alias(Box::new(self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Block {
        Block { statements }
    }

    pub fn statements(&self) -> &[Stmt] {
        &self.statements
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateDefinition {
    name: SymbolIdResult,
    params: Option<Vec<SymbolIdResult>>,
    qubits: Vec<SymbolIdResult>,
    block: Block,
}

impl GateDefinition {
    pub fn new(
        name: SymbolIdResult,
        params: Option<Vec<SymbolIdResult>>,
        qubits: Vec<SymbolIdResult>,
        block: Block,
    ) -> GateDefinition {
        GateDefinition {
            name,
            params,
            qubits,
            block,
        }
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::GateDefinition(self)
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn params(&self) -> Option<&[SymbolIdResult]> {
        self.params.as_deref()
    }

    pub fn num_params(&self) -> usize {
        self.params.as_ref().map_or(0, Vec::len)
    }

    pub fn qubits(&self) -> &[SymbolIdResult] {
        &self.qubits
    }

    pub fn block(&self) -> &Block {
        &self.block
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DefStmt {
    name: SymbolIdResult,
    params: Vec<SymbolIdResult>,
    block: Block,
    return_type: Type,
}

impl DefStmt {
    pub fn new(
        name: SymbolIdResult,
        params: Vec<SymbolIdResult>,
        block: Block,
        return_type: Type,
    ) -> DefStmt {
        DefStmt {
            name,
            params,
            block,
            return_type,
        }
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::DefStmt(self)
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn params(&self) -> &[SymbolIdResult] {
        self.params.as_ref()
    }

    pub fn block(&self) -> &Block {
        &self.block
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MeasureExpression {
    operand: TExpr,
}

impl MeasureExpression {
    pub fn new(operand: TExpr) -> MeasureExpression {
        MeasureExpression { operand }
    }

    pub fn operand(&self) -> &TExpr {
        &self.operand
    }

    pub fn to_texpr(self) -> TExpr {
        let out_type = match self.operand.get_type() {
            Type::Qubit | Type::HardwareQubit => Type::Bit(IsConst::False),
            Type::QubitArray(dims) => Type::BitArray(dims.clone(), IsConst::False),
            // syntax_to_semantics.from_gate_operand logs an error for non-quantum types.
            // Here, we give type `Undefined` to these illegal operands.
            _ => Type::Undefined,
        };
        TExpr::new(Expr::MeasureExpression(Box::new(self)), out_type)
    }
}

// qubits == None represents `barrier;`
#[derive(Clone, Debug, PartialEq)]
pub struct Barrier {
    qubits: Option<Vec<TExpr>>,
}

impl Barrier {
    pub fn new(qubits: Option<Vec<TExpr>>) -> Barrier {
        Barrier { qubits }
    }

    pub fn qubits(&self) -> Option<&[TExpr]> {
        self.qubits.as_deref()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelayStmt {
    duration: TExpr,
    qubits: Vec<TExpr>,
}

impl DelayStmt {
    pub fn new(duration: TExpr, qubits: Vec<TExpr>) -> DelayStmt {
        DelayStmt { duration, qubits }
    }

    pub fn duration(&self) -> &TExpr {
        &self.duration
    }

    pub fn qubits(&self) -> &[TExpr] {
        self.qubits.as_ref()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SubroutineCall {
    name: SymbolIdResult,
    params: Option<Vec<TExpr>>,
}

impl SubroutineCall {
    pub fn new(name: SymbolIdResult, params: Option<Vec<TExpr>>) -> SubroutineCall {
        SubroutineCall { name, params }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn params(&self) -> Option<&[TExpr]> {
        self.params.as_deref()
    }

    pub fn to_expr(self) -> Expr {
        Expr::SubroutineCall(self)
    }

    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(self.to_expr(), typ)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateCall {
    name: SymbolIdResult,
    params: Option<Vec<TExpr>>,
    qubits: Vec<TExpr>,
    modifiers: Vec<GateModifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateModifier {
    Inv,
    Pow(TExpr),
    Ctrl(Option<TExpr>),
    NegCtrl(Option<TExpr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateOperand {
    Identifier(SymbolIdResult),
    HardwareQubit(HardwareQubit),
    IndexedIdentifier(IndexedIdentifier),
}

impl GateOperand {
    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(Expr::GateOperand(self), typ)
    }
}

impl GateCall {
    pub fn new(
        name: SymbolIdResult,
        params: Option<Vec<TExpr>>,
        qubits: Vec<TExpr>,
        modifiers: Vec<GateModifier>,
    ) -> GateCall {
        GateCall {
            name,
            params,
            qubits,
            modifiers,
        }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn qubits(&self) -> &[TExpr] {
        &self.qubits
    }

    pub fn params(&self) -> Option<&[TExpr]> {
        self.params.as_deref()
    }

    pub fn modifiers(&self) -> &[GateModifier] {
        &self.modifiers
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GPhaseCall {
    arg: TExpr,
}

impl GPhaseCall {
    pub fn new(arg: TExpr) -> GPhaseCall {
        GPhaseCall { arg }
    }
    pub fn arg(&self) -> &TExpr {
        &self.arg
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModifiedGPhaseCall {
    arg: TExpr,
    modifiers: Vec<GateModifier>,
}

impl ModifiedGPhaseCall {
    pub fn new(arg: TExpr, modifiers: Vec<GateModifier>) -> ModifiedGPhaseCall {
        ModifiedGPhaseCall { arg, modifiers }
    }
    pub fn arg(&self) -> &TExpr {
        &self.arg
    }
    pub fn modifiers(&self) -> &[GateModifier] {
        &self.modifiers
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Reset {
    gate_operand: TExpr,
}

impl Reset {
    pub fn new(gate_operand: TExpr) -> Reset {
        Reset { gate_operand }
    }

    pub fn gate_operand(&self) -> &TExpr {
        &self.gate_operand
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::Reset(self)
    }
}

// This is one of several examples where instead of `enum Literal`
// we could do something like `struct Literal<T>`.
// However, if the dispatch must occur at runtime (In Rust called "dynamic",
// but that can mean something else as well in other languages), then tagged-union dispatch (ie `enum`)
// is likely much faster.
//
// If I understand correctly, if dynamic dispatch is needed, then
// the keyword `dyn` is required.
//
// The function syntax_to_semantics::from_literal matches the variant of the enum
// `oq3_syntax::ast::LiteralKind`. So, in the match arms, the variant of `enum Literal` (below)
// is known and the code therein can be devirtualized, inline, optimized, etc.
// In this case `struct Literal<T>` might be an advantage because there would be no dynamic dispatch.
// However when consuming the semantic ASG at later stage, dynamic dispatch may be required.

// BitStringLiteral and ArrayLiteral have data of size that can't be known at compile time.
// What effect does this have on the size of the enum.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(BoolLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    ImaginaryInt(IntLiteral),
    ImaginaryFloat(FloatLiteral),
    BitString(BitStringLiteral),
    TimingIntLiteral(TimingIntLiteral),
    TimingFloatLiteral(TimingFloatLiteral),
    Array, // stub
}

#[derive(Clone, Debug, PartialEq)]
pub struct Cast {
    operand: TExpr,
    typ: Type,
}

impl Cast {
    pub fn new(operand: TExpr, typ: Type) -> Cast {
        Cast { operand, typ }
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    pub fn operand(&self) -> &TExpr {
        &self.operand
    }

    pub fn to_expr(self) -> Expr {
        Expr::Cast(Box::new(self))
    }

    pub fn to_texpr(self) -> TExpr {
        let typ = self.get_type().clone();
        TExpr::new(self.to_expr(), typ)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoolLiteral {
    value: bool,
}

impl BoolLiteral {
    pub fn new<T>(value: T) -> BoolLiteral
    where
        T: Into<bool>,
    {
        let x: bool = value.into();
        BoolLiteral { value: x }
    }

    pub fn value(&self) -> &bool {
        &self.value
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::Bool(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Bool(IsConst::True))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IntLiteral {
    value: u128,
    sign: bool,
}

impl IntLiteral {
    pub fn new<T>(value: T, sign: bool) -> IntLiteral
    where
        T: Into<u128>,
    {
        let x: u128 = value.into();
        IntLiteral { value: x, sign }
    }

    pub fn value(&self) -> &u128 {
        &self.value
    }

    pub fn sign(&self) -> &bool {
        &self.sign
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::Int(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Int(Some(128), IsConst::True))
    }

    pub fn to_imaginary_expr(self) -> Expr {
        Expr::Literal(Literal::ImaginaryInt(self))
    }

    pub fn to_imaginary_texpr(self) -> TExpr {
        TExpr::new(self.to_imaginary_expr(), Type::Int(Some(64), IsConst::True))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TimeUnit {
    Second,
    MilliSecond,
    MicroSecond,
    NanoSecond,
    Cycle,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TimingIntLiteral {
    value: u128,
    sign: bool,
    time_unit: TimeUnit,
}

impl TimingIntLiteral {
    pub fn new<T>(value: T, sign: bool, time_unit: TimeUnit) -> TimingIntLiteral
    where
        T: Into<u128>,
    {
        let x: u128 = value.into();
        TimingIntLiteral {
            value: x,
            sign,
            time_unit,
        }
    }

    pub fn value(&self) -> &u128 {
        &self.value
    }

    pub fn sign(&self) -> &bool {
        &self.sign
    }

    pub fn time_unit(&self) -> &TimeUnit {
        &self.time_unit
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::TimingIntLiteral(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Duration(IsConst::True))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TimingFloatLiteral {
    value: f64,
    sign: bool,
    time_unit: TimeUnit,
}

impl TimingFloatLiteral {
    pub fn new<T>(value: T, sign: bool, time_unit: TimeUnit) -> TimingFloatLiteral
    where
        T: Into<f64>,
    {
        //        let x: String = format!("{}", value.into());
        let x = value.into();
        TimingFloatLiteral {
            value: x,
            sign,
            time_unit,
        }
    }

    pub fn value(&self) -> &f64 {
        &self.value
    }

    pub fn sign(&self) -> &bool {
        &self.sign
    }

    pub fn time_unit(&self) -> &TimeUnit {
        &self.time_unit
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::TimingFloatLiteral(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Duration(IsConst::True))
    }
}

// FIXME: Floats are ok-ish here, but broken in lexer/parser/syntax
// This is float as a string.
// For example, trait Eq is implemented here, but not for actual f64.
#[derive(Clone, Debug, PartialEq)]
pub struct FloatLiteral {
    value: String,
}

impl FloatLiteral {
    // pub fn new<T>(value: T) -> FloatLiteral where T: Into<String> {
    //     FloatLiteral { value: value.into() }
    // }

    // This is a bit more clear.
    pub fn new<T: ToString>(value: T) -> FloatLiteral {
        FloatLiteral {
            value: value.to_string(),
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::Float(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Float(Some(64), IsConst::True))
    }

    pub fn to_imaginary_expr(self) -> Expr {
        Expr::Literal(Literal::ImaginaryFloat(self))
    }

    pub fn to_imaginary_texpr(self) -> TExpr {
        TExpr::new(
            self.to_imaginary_expr(),
            Type::Complex(Some(64), IsConst::True),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BitStringLiteral {
    value: String,
}

impl BitStringLiteral {
    pub fn new<T: ToString>(value: T) -> BitStringLiteral {
        BitStringLiteral {
            value: value.to_string(),
        }
    }

    pub fn value(&self) -> &str {
        &(self.value)
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::BitString(self))
    }

    pub fn to_texpr(self) -> TExpr {
        // Don't count underscores when counting bits.
        let width: usize = self
            .value
            .chars()
            .filter(|c| *c == '0' || *c == '1')
            .count();
        TExpr::new(
            self.to_expr(),
            Type::BitArray(ArrayDims::D1(width), IsConst::True),
        )
    }
}

// String literal appears in restricted contexts. It is not in the expression tree.
#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral {
    value: String,
}

impl StringLiteral {
    pub fn new<T: ToString>(value: T) -> StringLiteral {
        StringLiteral {
            value: value.to_string(),
        }
    }

    pub fn value(&self) -> &str {
        &(self.value)
    }
}

// Unary Expressions

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNot,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    op: UnaryOp,
    operand: TExpr,
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, operand: TExpr) -> UnaryExpr {
        UnaryExpr { op, operand }
    }

    pub fn operand(&self) -> &TExpr {
        &self.operand
    }

    pub fn op(&self) -> &UnaryOp {
        &self.op
    }

    pub fn to_texpr(self) -> TExpr {
        match self.op() {
            // Logical not returns bool
            UnaryOp::Not => TExpr::new(Expr::UnaryExpr(Box::new(self)), Type::Bool(IsConst::False)),
            // Assume Minus and BitNot return the same type a operand.
            UnaryOp::Minus | UnaryOp::BitNot => {
                let ty = self.operand.get_type().clone();
                TExpr::new(Expr::UnaryExpr(Box::new(self)), ty)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub(crate) op: BinaryOp,
    pub(crate) left: TExpr,
    pub(crate) right: TExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    ArithOp(ArithOp),
    CmpOp(CmpOp),
    ConcatenationOp,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Rem,
    Shl,
    Shr,
    BitXOr,
    BitAnd,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CmpOp {
    Eq,
    Neq,
}

impl BinaryExpr {
    pub fn new(op: BinaryOp, left: TExpr, right: TExpr) -> BinaryExpr {
        BinaryExpr { op, left, right }
    }
    pub fn op(&self) -> &BinaryOp {
        &self.op
    }
    pub fn left(&self) -> &TExpr {
        &self.left
    }
    pub fn right(&self) -> &TExpr {
        &self.right
    }

    pub fn to_expr(self) -> Expr {
        Expr::BinaryExpr(Box::new(self))
    }

    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(self.to_expr(), typ)
    }

    // Determine:
    // 1 If either the lhs or rhs needs to be cast, and to which type
    // 2 The return type
    // Construct a `BinaryExpr` with possible appropriate casts and types.
    pub fn new_texpr_with_cast(op: BinaryOp, left: TExpr, right: TExpr) -> TExpr {
        let left_type = left.get_type();
        let right_type = right.get_type();
        match &op {
            BinaryOp::ArithOp(arith_op) => {
                let promoted_type = implicit_cast_type(arith_op, left_type, right_type);
                let new_left = if &promoted_type == left_type {
                    left
                } else {
                    Cast::new(left, promoted_type.clone()).to_texpr()
                };
                let new_right = if &promoted_type == right_type {
                    right
                } else {
                    Cast::new(right, promoted_type.clone()).to_texpr()
                };
                BinaryExpr::new(op, new_left, new_right).to_texpr(promoted_type)
            }
            BinaryOp::CmpOp(_) | BinaryOp::ConcatenationOp => {
                BinaryExpr::new(op, left, right).to_texpr(Type::ToDo)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RangeExpression {
    start: TExpr,
    step: Option<TExpr>,
    stop: TExpr,
}

impl RangeExpression {
    pub fn new(start: TExpr, step: Option<TExpr>, stop: TExpr) -> RangeExpression {
        RangeExpression { start, step, stop }
    }

    pub fn start(&self) -> &TExpr {
        &self.start
    }

    pub fn step(&self) -> Option<&TExpr> {
        self.step.as_ref()
    }

    pub fn stop(&self) -> &TExpr {
        &self.stop
    }

    pub fn to_expr(self) -> Expr {
        Expr::RangeExpression(Box::new(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::Range)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    condition: TExpr,
    then_branch: Block,
    else_branch: Option<Block>,
}

impl If {
    pub fn new(condition: TExpr, then_branch: Block, else_branch: Option<Block>) -> If {
        If {
            condition,
            then_branch,
            else_branch,
        }
    }

    pub fn condition(&self) -> &TExpr {
        &self.condition
    }

    pub fn then_branch(&self) -> &Block {
        &self.then_branch
    }

    pub fn else_branch(&self) -> Option<&Block> {
        self.else_branch.as_ref()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::If(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    condition: TExpr,
    loop_body: Block,
}

impl While {
    pub fn new(condition: TExpr, loop_body: Block) -> While {
        While {
            condition,
            loop_body,
        }
    }

    pub fn condition(&self) -> &TExpr {
        &self.condition
    }

    pub fn loop_body(&self) -> &Block {
        &self.loop_body
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::While(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForIterable {
    SetExpression(SetExpression),
    RangeExpression(RangeExpression),
    Expr(TExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStmt {
    loop_var: SymbolIdResult,
    iterable: ForIterable,
    loop_body: Block,
}

impl ForStmt {
    pub fn new(loop_var: SymbolIdResult, iterable: ForIterable, loop_body: Block) -> ForStmt {
        ForStmt {
            loop_var,
            iterable,
            loop_body,
        }
    }

    pub fn loop_var(&self) -> &SymbolIdResult {
        &self.loop_var
    }

    pub fn iterable(&self) -> &ForIterable {
        &self.iterable
    }

    pub fn loop_body(&self) -> &Block {
        &self.loop_body
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::ForStmt(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchCaseStmt {
    control: TExpr,
    cases: Vec<CaseExpr>,
    default_block: Option<Vec<Stmt>>,
}

impl SwitchCaseStmt {
    pub fn new(
        control: TExpr,
        cases: Vec<CaseExpr>,
        default_block: Option<Vec<Stmt>>,
    ) -> SwitchCaseStmt {
        SwitchCaseStmt {
            control,
            cases,
            default_block,
        }
    }

    pub fn control(&self) -> &TExpr {
        &self.control
    }

    pub fn cases(&self) -> &[CaseExpr] {
        &self.cases
    }

    pub fn default_block(&self) -> Option<&[Stmt]> {
        self.default_block.as_deref()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::SwitchCaseStmt(self)
    }
}

// Not in `enum Expr`
#[derive(Clone, Debug, PartialEq)]
pub struct CaseExpr {
    control_values: Vec<TExpr>, // Const integer expressions
    statements: Vec<Stmt>,
}

impl CaseExpr {
    pub fn new(control_values: Vec<TExpr>, statements: Vec<Stmt>) -> CaseExpr {
        CaseExpr {
            control_values,
            statements,
        }
    }

    pub fn control_values(&self) -> &[TExpr] {
        &self.control_values
    }

    pub fn statements(&self) -> &[Stmt] {
        &self.statements
    }
}

// Might make sense for this, and others to be like `struct Pragma(String)`.
#[derive(Clone, Debug, PartialEq)]
pub struct Pragma {
    pragma_text: String,
}

impl Pragma {
    pub fn new<T: ToString>(pragma: T) -> Pragma {
        Pragma {
            pragma_text: pragma.to_string(),
        }
    }

    /// Return the pragma line omitting the directive "pragma" or "#pragma"
    pub fn pragma_text(&self) -> &str {
        self.pragma_text.as_ref()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::Pragma(self)
    }
}

// Return a common type for implicit casting
// Values of both types `ty1` and `ty2` should be cast to this
// type and passed as operands to `op`.
pub fn implicit_cast_type(op: &ArithOp, ty1: &Type, ty2: &Type) -> Type {
    use ArithOp::*;
    match op {
        Add | Sub | Mul => types::promote_types(ty1, ty2),

        Div => {
            if matches!(ty1, Type::Float(..)) || matches!(ty2, Type::Float(..)) {
                types::promote_types(ty1, ty2)
            } else {
                Type::Float(None, IsConst::False)
            }
        }

        Mod | Rem | Shl | Shr | BitXOr | BitAnd => todo!(),
    }
}
