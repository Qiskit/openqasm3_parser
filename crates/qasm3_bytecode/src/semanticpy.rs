// Copyright contributors to the openqasm-parser project

use pyo3::prelude::*;
use pyo3::types::PyList;
use pyo3::Python;

use semantics::{asg, symbols, types};
use symbols::SymbolType; // trait for getting symbol type

#[derive(Clone)]
#[pyclass]
pub enum TypeName {
    Bit,
    Qubit,
    Int,
    UInt,
    Float,
    Angle,
    Complex,
    Bool,
    Duration,
    Stretch,
    // Arrays
    BitArray,
    QubitArray,
    IntArray,
    UIntArray,
    FloatArray,
    AngleArray,
    ComplexArray,
    BoolArray,
    DurationArray,
    // Other
    Gate,  // <-- this type seems anomalous
    Range, // temporary placeholder, perhaps
    Void,
    Invalid,
    // Undefined means a type that is erroneously non-existent. This is not the same as unknown.
    // The prototypical application is trying to resolve an unbound identifier.
    Undefined,
    NotImplemented,
    // Void, do we need this?
}

#[pyclass]
#[derive(Clone)]
pub struct Type(types::Type);

#[pymethods]
impl Type {
    pub fn type_name(&self) -> TypeName {
        type At = types::Type;
        type Tn = TypeName;
        match self.0 {
            At::Bool(..) => Tn::Bool,
            At::Bit(..) => Tn::Bit,
            At::Qubit => Tn::Qubit,
            At::Int(..) => Tn::Int,
            At::UInt(..) => Tn::UInt,
            At::Float(..) => Tn::Float,
            At::Angle(..) => Tn::Angle,
            At::Gate => Tn::Gate,
            At::QubitArray(..) => Tn::QubitArray,
            _ => TypeName::NotImplemented, // TODO: all other types
        }
    }

    pub fn is_const(&self) -> bool {
        self.0.is_const()
    }

    pub fn dims(&self, py: Python<'_>) -> PyObject {
        self.0.dims().into_py(py)
    }
}

// Errors with threading problem here
// #[pyclass]
// #[derive(Clone)]
// pub struct Context(syntax_to_semantics::Context);
// #[pymethods]
// impl Context {
//     pub fn program(&self, py: Python<'_>) -> PyObject {
//         Program(self.0.program()).into_py(py)
//     }
// }

#[pyclass]
#[derive(Clone)]
pub struct TExpr(asg::TExpr);

#[pymethods]
impl TExpr {
    //impl TExpr<'_> {
    pub fn get_type(&self) -> Type {
        Type(self.0.get_type().clone())
    }

    pub fn expr(&self, py: Python<'_>) -> PyObject {
        Expr(self.0.expression()).into_py(py)
    }
}

#[pyclass]
#[derive(Clone, Debug)]
pub struct SymbolId(symbols::SymbolId);

#[pyclass]
#[derive(Clone)]
pub struct DeclareClassical(asg::DeclareClassical);

#[pymethods]
impl DeclareClassical {
    pub fn name(&self) -> SymbolId {
        SymbolId(self.0.name().as_ref().unwrap().clone())
    }

    pub fn initializer(&self, py: Python<'_>) -> PyObject {
        match self.0.initializer() {
            Some(expr) => Expr(expr.as_ref().expression()).into_py(py),
            None => py.None(),
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct DeclareQuantum(asg::DeclareQuantum);

#[pymethods]
impl DeclareQuantum {
    pub fn name(&self) -> SymbolId {
        SymbolId(self.0.name().as_ref().unwrap().clone())
    }

    pub fn name_symbol(&self, symbol_table: SymbolTable) -> Symbol {
        symbol_table.get_symbol(self.name())
    }

    // pub fn as_tuple(&self, symbol_table: SymbolTable, py: Python<'_>) -> PyObject {
    //     let symbol_id = self.0.name().as_ref().unwrap();
    //     let symbol = &symbol_table.0[symbol_id];
    //     let name = symbol.name();
    //     let ty = symbol.symbol_type();

    //     // match ty {
    //     //     types::Type::Qubit => py.None(),
    //     // }
    //     py.None()
    // }
}

#[pyclass]
#[derive(Clone)]
pub struct GateCall(asg::GateCall);

#[pymethods]
impl GateCall {
    pub fn name(&self) -> SymbolId {
        SymbolId(self.0.name().as_ref().unwrap().clone())
    }

    pub fn qubits(&self, py: Python<'_>) -> Py<PyList> {
        let elements: Vec<_> = self
            .0
            .qubits()
            .iter()
            .map(|x| TExpr(x.clone()).into_py(py))
            .collect();
        PyList::new(py, elements).into()
    }
}
// wrapper exists only allow `impl IntoPy for Stmt`
#[derive(Clone)]
pub struct Stmt(asg::Stmt);

impl IntoPy<PyObject> for Stmt {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self.0 {
            asg::Stmt::DeclareQuantum(qdecl) => DeclareQuantum(qdecl).into_py(py),
            asg::Stmt::DeclareClassical(cdecl) => DeclareClassical(cdecl).into_py(py),
            asg::Stmt::GateCall(gate_call) => GateCall(gate_call).into_py(py),
            _ => py.None(),
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct Program(pub asg::Program);

#[pymethods]
impl Program {
    pub fn trystatements(&self, py: Python<'_>) -> Py<PyList> {
        let elements: Vec<i32> = vec![0, 1, 2, 3, 4, 5];
        PyList::new(py, elements).into()
    }

    pub fn statements(&self, py: Python<'_>) -> Py<PyList> {
        let elements: Vec<_> = self
            .0
            .stmts
            .iter()
            .map(|x| Stmt(x.clone()).into_py(py))
            .collect();
        PyList::new(py, elements).into()
    }
}

#[pyclass]
#[derive(Clone)]
pub struct BoolLiteral(asg::BoolLiteral);

#[pymethods]
impl BoolLiteral {
    pub fn value(&self) -> bool {
        *self.0.value()
    }
}

// Dont think we need this
// #[pyclass]
#[derive(Clone)]
pub struct Expr<'a>(&'a asg::Expr);

impl IntoPy<PyObject> for Expr<'_> {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self.0 {
            asg::Expr::Literal(asg::Literal::Bool(bool_literal)) => {
                BoolLiteral(bool_literal.clone()).into_py(py)
            }
            asg::Expr::GateOperand(asg::GateOperand::Identifier(ident)) => {
                //                dbg!(ident.clone());
                SymbolId(ident.clone().symbol().as_ref().unwrap().clone()).into_py(py)
                //                py.None()
            }
            _ => py.None(),
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct Symbol(pub symbols::Symbol);

use pyo3::class::basic::CompareOp;
use pyo3::exceptions::PyTypeError;

#[pymethods]
impl SymbolId {
    // This is supposed to work, but does not.
    // So we have to use richcmp
    // fn __eq__(&self, other: &Self) -> bool {
    //     self.0 == other.0
    // }

    // FIXME: use the appropriate error, rather than the first one I find.
    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        match op {
            CompareOp::Eq => Ok(self.0 == other.0),
            CompareOp::Ne => Ok(self.0 != other.0),
            _ => Err(PyTypeError::new_err(format!(
                "Operation {op:?} not supported"
            ))),
        }
    }
}

#[pymethods]
impl Symbol {
    pub fn name(&self, py: Python<'_>) -> PyObject {
        self.0.name().into_py(py)
    }

    // can we remove _py ?
    pub fn symbol_type(&self, _py: Python<'_>) -> Type {
        Type(self.0.symbol_type().clone())
    }

    // Benchmark:
    // 156ns for as_tuple.
    // 190ns for calling `name` and `symbol_type` separately from Python
    pub fn as_tuple(&self, py: Python<'_>) -> (PyObject, Type) {
        (self.name(py), self.symbol_type(py))
    }
}

#[pyclass]
#[derive(Clone)]
pub struct SymbolTable(pub symbols::SymbolTable);

#[pymethods]
impl SymbolTable {
    //    #[pyo3(name = "__item__")]
    pub fn get_symbol(&self, symbol_id: SymbolId) -> Symbol {
        Symbol(self.0[&symbol_id.0].clone())
    }
}

//
// Testing during development
//

// pub fn get_a_qdecl() -> Stmt {
//     use semantics::symbols::{SymbolTable};
//     use semantics::types::{Type, IsConst};
//     let mut table = SymbolTable::new();
//     let x = table.new_binding("q", &Type::Bool(IsConst::False));
//     let qd = asg::DeclareQuantum::new(x);
//     Stmt(asg::Stmt::DeclareQuantum(qd))
// }

// #[pyfunction]
// pub fn try_convert(py: Python<'_>) -> PyObject {
//     let stmt = get_a_qdecl();
//     stmt.into_py(py)
// }

#[pyfunction]
pub fn check_enum(x: TypeName, py: Python<'_>) -> PyObject {
    match x {
        TypeName::Int => 1i32,
        _ => 0i32,
    }
    .into_py(py)
}

// #[pyfunction]
// pub fn cdeclaration(py: Python<'_>) -> (PyObject, SymbolTable) {
//     use semantics::symbols::{SymbolTable};
//     use semantics::types::{Type, IsConst};
//     let mut table = SymbolTable::new();
//     let x = table.new_binding("x", &Type::Bool(IsConst::False));
//     let initializer = Some(asg::BoolLiteral::new(false).to_texpr());
//     let decl = asg::DeclareClassical::new(x, initializer).to_stmt();
//     (Stmt(decl).into_py(py), SymbolTable(table))
// }

#[pymodule]
pub fn semanticpy(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<TypeName>()?;
    m.add_class::<Symbol>()?;
    m.add_class::<SymbolId>()?;
    m.add_class::<GateCall>()?;
    // m.add_wrapped(wrap_pyfunction!(try_convert))?;
    // m.add_wrapped(wrap_pyfunction!(cdeclaration))?;
    m.add_wrapped(wrap_pyfunction!(check_enum))?;
    Ok(())
}
