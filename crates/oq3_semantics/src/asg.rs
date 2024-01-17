// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// The definition of the abstract semantic graph (ASG) as well as the API for using it.
// Construction of this typed ASG from syntactic AST is in string_to_semantic.rs.

// SymbolIdResult can represent a valid symbol symbol, via a symbol id, or a semantic error.
// We need to insert SymbolIdResult everywhere a symbol is needed in the semantic tree.
// Because, although this ASG can assume synactic correctness, we need to track
// semantic errors and continue to build the semantic ASG.

use crate::symbols::SymbolIdResult; // SymbolIdResult = Result<SymbolId, SymbolError>
use crate::types;
use crate::types::{ArrayDims, IsConst, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Program {
    pub version: Option<OpenQASMVersion>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }

    pub fn insert_stmt(&mut self, stmt: Stmt) {
        let _ = &self.stmts.push(stmt);
    }

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

    pub fn version(&self) -> &Option<OpenQASMVersion> {
        &self.version
    }

    // FIXME: must exist idiomatic rust for managing these modes
    /// Print the ASG using the pretty print `Debug` trait.
    pub fn print_asg_debug_pretty(&self) {
        for stmt in self.iter() {
            println!("{:#?}", stmt);
        }
    }

    /// Print the ASG using the `Debug` trait.
    pub fn print_asg_debug(&self) {
        for stmt in self.iter() {
            println!("{:?}", stmt);
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

// One way to forward methods for Iterator to Program items.
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    ArraySlice(ArraySlice),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    Literal(Literal),
    Cast(Cast),
    Identifier(Identifier),
    HardwareQubit(HardwareQubit),
    IndexExpression(IndexExpression),
    IndexedIdentifier(IndexedIdentifier),
    GateOperand(GateOperand),
    // FIXME: for Range and similiar, it is easiest to shove everything in expression tree and give it a useless type.
    // But we need to handle these in a consistent way. Is there any situation where the "type" of Range is meaningful or useful?
    // For example, in syntax_to_semantics, have a routine that handles out-of-tree expressions.
    Range(Range),
    Call,    // stub function (def) call
    Set,     // stub
    MeasureExpression(MeasureExpression),
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TExpr {
    expression: Expr,
    ty: Type,
}

impl TExpr {
    pub fn new(expression: Expr, ty: Type) -> TExpr {
        TExpr { expression, ty }
    }

    pub fn get_type(&self) -> &Type {
        &(self.ty)
    }

    pub fn expression(&self) -> &Expr {
        &(self.expression)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    Alias, // stub
    AnnotatedStmt(AnnotatedStmt),
    Assignment(Assignment),
    Barrier, // stub
    Block(Block),
    Box, // stub
    Break,
    Cal, // stub
    Continue,
    DeclareClassical(DeclareClassical),
    Def,    // stub
    DefCal, // stub
    Delay,  // stub
    End,
    ExprStmt(TExpr),
    Extern, // stub
    For,    // stub
    GateDeclaration(GateDeclaration),
    GateCall(GateCall), // A statement because a gate call does not return anything
    GPhaseCall(GPhaseCall),
    IODeclaration, // stub
    If(If),
    Include(Include),
    NullStmt,            // for testing
    OldStyleDeclaration, // stub
    Pragma(Pragma),
    DeclareQuantum(DeclareQuantum),
    Reset,  // stub
    Return, // stub
    While(While),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Annotation {
    kind: String,
    body: Option<String>,
}

impl Annotation {
    pub fn new<T: ToString>(kind: T, body: Option<String>) -> Annotation {
        Annotation {
            kind: kind.to_string(),
            body,
        }
    }

    pub fn kind(&self) -> &str {
        self.kind.as_ref()
    }

    // Various ways to do this. But the troglodyte way, with `match` is clear.
    pub fn body(&self) -> Option<&str> {
        Some(self.body.as_ref()?)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AnnotatedStmt {
    stmt: Box<Stmt>,
    annotations: Vec<Annotation>,
}

impl AnnotatedStmt {
    // For convenience, we violate the prinicple of making semantic errors
    // unexpressible.
    pub fn new(stmt: Stmt, annotations: Vec<Annotation>) -> AnnotatedStmt {
        if matches!(stmt, Stmt::AnnotatedStmt(..)) {
            panic!("Annotation of annotated statement is not allowed.");
        }
        AnnotatedStmt {
            stmt: Box::new(stmt),
            annotations,
        }
    }

    // stmt or statement. Should be uniform.
    pub fn statement(&self) -> &Stmt {
        &self.stmt
    }

    pub fn annotations(&self) -> &Vec<Annotation> {
        &self.annotations
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexExpression {
    expr: Box<TExpr>,
    index: IndexOperator,
}

impl IndexExpression {
    pub fn new(expr: TExpr, index: IndexOperator) -> IndexExpression {
        IndexExpression {
            expr: Box::new(expr),
            index,
        }
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(Expr::IndexExpression(self), Type::ToDo)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IndexOperator {
    SetExpression(SetExpression),
    ExpressionList(ExpressionList),
}

// FIXME: probably want an interface on this.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionList {
    pub expressions: Vec<TExpr>,
}

// The rules regarding hardware qubits are not clear
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HardwareQubit {
    identifier: String,
}

impl HardwareQubit {
    pub fn new<T: ToString>(identifier: T) -> HardwareQubit {
        HardwareQubit {
            identifier: identifier.to_string(),
        }
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(Expr::HardwareQubit(self), Type::HardwareQubit)
    }
}

impl ExpressionList {
    pub fn new(expressions: Vec<TExpr>) -> ExpressionList {
        ExpressionList { expressions }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LValue {
    Identifier(SymbolIdResult),
    ArraySlice(ArraySlice),
    RegisterSlice(RegisterSlice),
}

// For example `expr` in `v[expr]`, or `1:3` in `v[1:3]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArraySliceIndex {
    Expr(TExpr),
    Range(TExpr),
}

// FIXME: Is this the slice, or just the index ???
// Same form as ArraySliceIndex, but they have different semantics.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterSlice {
    Expr(TExpr),
    Range(Range),
}

// example: v[3:4]. Includes multidimensional index
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArraySlice {
    name: SymbolIdResult,
    pub indices: Vec<ArraySliceIndex>,
}

impl ArraySlice {
    pub fn new(name: SymbolIdResult, indices: Vec<ArraySliceIndex>) -> ArraySlice {
        ArraySlice { name, indices }
    }

    pub fn to_texpr(self, base_type: Type) -> TExpr {
        TExpr::new(Expr::ArraySlice(self), base_type)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DeclareClassical {
    name: SymbolIdResult, // The name and type can be retrieved from SymbolId
    initializer: Option<Box<TExpr>>,
}

impl DeclareClassical {
    pub fn new(name: SymbolIdResult, initializer: Option<TExpr>) -> DeclareClassical {
        DeclareClassical {
            name,
            initializer: initializer.map(Box::new),
        }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn initializer(&self) -> &Option<Box<TExpr>> {
        &self.initializer
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::DeclareClassical(self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block {
    statements: Vec<Stmt>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            statements: Vec::<Stmt>::new(),
        }
    }

    pub fn insert_stmt(&mut self, stmt: Stmt) {
        self.statements.push(stmt);
    }

    pub fn statements(&self) -> &Vec<Stmt> {
        &self.statements
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

// #[test]
// fn test_construct_block() {
//     let mut block = Block::new();
//     let id = make_ident_expr("x", false);
//     let assign = Stmt::Assignment(Assignment { name: id, rhs: make_int_lit_expr(1) });
//     block.insert_stmt(assign);
// }

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GateDeclaration {
    name: SymbolIdResult,
    params: Option<Vec<SymbolIdResult>>,
    qubits: Vec<SymbolIdResult>,
    block: Block,
}

impl GateDeclaration {
    pub fn new(
        name: SymbolIdResult,
        params: Option<Vec<SymbolIdResult>>,
        qubits: Vec<SymbolIdResult>,
        block: Block,
    ) -> GateDeclaration {
        GateDeclaration {
            name,
            params,
            qubits,
            block,
        }
    }

    pub fn new_empty_qubit_list() -> Vec<SymbolIdResult> {
        Vec::<SymbolIdResult>::new()
    }

    pub fn new_empty_param_list() -> Vec<SymbolIdResult> {
        Vec::<SymbolIdResult>::new()
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::GateDeclaration(self)
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn params(&self) -> &Option<Vec<SymbolIdResult>> {
        &self.params
    }

    pub fn qubits(&self) -> &Vec<SymbolIdResult> {
        &self.qubits
    }

    pub fn block(&self) -> &Block {
        &self.block
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MeasureExpression {
    operand: Box<TExpr>,
}

impl MeasureExpression {
    pub fn new(operand: TExpr) -> MeasureExpression {
        MeasureExpression { operand: Box::new(operand) }
    }

    // FIXME: type may not be correct here.
    // This assumes a single qubit is measured.
    pub fn to_texpr(self) -> TExpr {
        TExpr::new(Expr::MeasureExpression(self), Type::Bit(IsConst::False))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GateCall {
    name: SymbolIdResult,
    params: Option<Vec<TExpr>>,
    qubits: Vec<TExpr>,
    modifier: Option<GateModifier>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GateModifier {
    Inv,
    Pow(TExpr),
    Ctrl(Option<TExpr>),
    NegCtrl(Option<TExpr>),
}

// Following naming in ref parser instead
// We ~~will~~ should try to use the distinction between "parameter", which appears in the signature,
// and "argument", which appears in the call expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum QubitArgument {
    Identifier(SymbolIdResult),
    ArraySlice(ArraySlice),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GateOperand {
    Identifier(Identifier),
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
        modifier: Option<GateModifier>,
    ) -> GateCall {
        GateCall {
            name,
            params,
            qubits,
            modifier,
        }
    }

    pub fn name(&self) -> &SymbolIdResult {
        &self.name
    }

    pub fn qubits(&self) -> &Vec<TExpr> {
        &self.qubits
    }

    pub fn params(&self) -> &Option<Vec<TExpr>> {
        &self.params
    }

    pub fn modifier(&self) -> &Option<GateModifier> {
        &self.modifier
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    Bool(BoolLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    BitString(BitStringLiteral),
    Array, // stub
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cast {
    operand: Box<TExpr>,
    typ: Type,
}

impl Cast {
    pub fn new(operand: TExpr, typ: Type) -> Cast {
        Cast {
            operand: Box::new(operand),
            typ,
        }
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    pub fn operand(&self) -> &TExpr {
        self.operand.as_ref()
    }

    pub fn to_expr(self) -> Expr {
        Expr::Cast(self)
    }

    pub fn to_texpr(self) -> TExpr {
        let typ = self.get_type().clone();
        TExpr::new(self.to_expr(), typ)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntLiteral {
    value: u128,
}

impl IntLiteral {
    pub fn new<T>(value: T) -> IntLiteral
    where
        T: Into<u128>,
    {
        let x: u128 = value.into();
        IntLiteral { value: x }
    }

    pub fn value(&self) -> &u128 {
        &self.value
    }

    pub fn to_expr(self) -> Expr {
        Expr::Literal(Literal::Int(self))
    }

    pub fn to_texpr(self) -> TExpr {
        TExpr::new(self.to_expr(), Type::UInt(Some(128), IsConst::True))
    }
}

// FIXME: Floats are ok-ish here, but broken in lexer/parser/syntax
// This is float as a string.
// For example, trait Eq is implemented here, but not for actual f64.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
        let width: usize = self.value.len();
        TExpr::new(
            self.to_expr(),
            Type::BitArray(ArrayDims::D1(width), IsConst::True),
        )
    }
}

// String literal appears in restricted contexts. It is not in the expression tree.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNot,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnaryExpr {
    op: UnaryOp,
    operand: Box<TExpr>,
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, operand: TExpr) -> UnaryExpr {
        UnaryExpr {
            op,
            operand: Box::new(operand),
        }
    }

    pub fn operand(&self) -> &TExpr {
        &self.operand
    }

    pub fn op(&self) -> &UnaryOp {
        &self.op
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub(crate) op: BinaryOp,
    pub(crate) left: Box<TExpr>,
    pub(crate) right: Box<TExpr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    ArithOp(ArithOp),
    CmpOp(CmpOp),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
}

// FIXME: ?? Code would be easier, maybe more performant since we
// tag fields with pub(crate).
impl BinaryExpr {
    pub fn new(op: BinaryOp, left: TExpr, right: TExpr) -> BinaryExpr {
        BinaryExpr {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
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
        Expr::BinaryExpr(self)
    }

    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(self.to_expr(), typ)
    }

    pub fn new_texpr_with_cast(op: BinaryOp, left: TExpr, right: TExpr) -> TExpr {
        let left_type = left.get_type();
        let right_type = right.get_type();
        let promoted_type = types::promote_types(left_type, right_type);
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
}

// FIXME: Elsewhere, we use `name` for a SymbolIdResult. The actual string can be looked up
// from the SymbolId. But here we use `name` for the string. This is not uniform
// and potentially confusing. I think we will have to ditch the approach below.
// The name of the identifer is stored in both `name` and as a field in `symbol_id`.
// But this is only true if `symbol_id` is not Err. In case of a semantic error,
// the symbol_id may not be resolved, which we represent by a value of `Err(SymbolError)`.
// This happens when attempting to look up the binding of an identifier that is in
// fact not bound in any visible scope.
// We carry the name in `name` as well in order to facilitate diagnosing the semantic
// error. But we have not concrete plan for this. So the field `name` below may be removed.
// And in that case, the variant `Ident(Ident)` in `enum Expr` above could be replaced with
// `Ident(SymbolId)`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    name: String,
    symbol: SymbolIdResult,
}

impl Identifier {
    pub fn new<T: ToString>(name: T, symbol: SymbolIdResult) -> Identifier {
        Identifier {
            name: name.to_string(),
            symbol,
        }
    }

    pub fn to_expr(self) -> Expr {
        Expr::Identifier(self)
    }

    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(self.to_expr(), typ)
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn symbol(&self) -> &SymbolIdResult {
        &self.symbol
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Range {
    start: Box<TExpr>,
    step: Box<Option<TExpr>>,
    stop: Box<TExpr>,
}

impl Range {
    pub fn new(start: TExpr, step: Option<TExpr>, stop: TExpr) -> Range {
        Range {
            start: Box::new(start),
            step: Box::new(step),
            stop: Box::new(stop),
        }
    }

    pub fn start(&self) -> &TExpr {
        &self.start
    }

    pub fn step(&self) -> Option<&TExpr> {
        self.step.as_ref().as_ref()
    }

    pub fn stop(&self) -> &TExpr {
        &self.stop
    }

    pub fn to_texpr(self, typ: Type) -> TExpr {
        TExpr::new(Expr::Range(self), typ)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    pub fn else_branch(&self) -> &Option<Block> {
        &self.else_branch
    }

    pub fn to_stmt(self) -> Stmt {
        Stmt::If(self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

// Might make sense for this, and others to be like `struct Pragma(String)`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pragma {
    pragma: String,
}

impl Pragma {
    pub fn new<T: ToString>(pragma: T) -> Pragma {
        Pragma {
            pragma: pragma.to_string(),
        }
    }

    pub fn pragma(&self) -> &str {
        self.pragma.as_ref()
    }
}
