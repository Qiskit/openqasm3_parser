// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// Defines the data structures representing the types used to annotate types of expressions in
// the typed ASG.
// This file should include all code that manipulates the types. In particular type promotion
// is implemented here. Casting is not implemented here because it involves not only the types,
// but the typed AST as well.

// Tuple fields (Option<u32>, bool) are (width, is_const).
// width == None, means no width specified. Spec sometimes says "machine" int, etc.
//
// Problem: We will later want to extend this to array types.
// Arrays up to seven dimensions are allowed. But specifying the
// type and width and dimensions as follows greatly increases the size of `enum Type`:
// ArrayInt(u32, (d1,...,d7))
//
// Widths are currently Option<u32>. This may not be the most efficient or performant way
// to handle absence of widths. For example Int(u32,bool) and IntM(bool) for "machine" int.
// But the current implementation is faster and less complex to code.

use boolenum::BoolEnum;

#[derive(BoolEnum, Clone, Debug, PartialEq, Eq, Hash)]
pub enum IsConst {
    True,
    False,
}

// I think semantics of these should be cleared up before implementing
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IOType {
    Input,
    Output,
    Neither,
}

// From the OQ3 Spec:
// The supported [base types for arrays] include various sizes of ``bit``,
// ``int``, ``uint``, ``float``, ``complex``, and ``angle``, as well as
// ``bool`` and ``duration``

/// Bit width of primitive classical types
type Width = Option<u32>;

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    // Scalar types
    Bit(IsConst),
    Qubit,
    HardwareQubit,

    Int(Width, IsConst),
    UInt(Width, IsConst),
    Float(Width, IsConst),
    Angle(Width, IsConst),
    Complex(Width, IsConst), // width is for one component.
    Bool(IsConst),           // bool field is is_const
    Duration(IsConst),
    Stretch(IsConst),
    // Arrays
    BitArray(ArrayDims, IsConst),
    QubitArray(ArrayDims),
    IntArray(ArrayDims),
    UIntArray(ArrayDims),
    FloatArray(ArrayDims),
    AngleArray(ArrayDims),
    ComplexArray(ArrayDims),
    BoolArray(ArrayDims),
    DurationArray(ArrayDims),

    // Other
    Gate(usize, usize),           // (num classical args, num quantum args)
    SubroutineDef(SubroutineDef), // number of parameters ("arguments")
    Range,
    Set,
    Void,
    ToDo, // not yet implemented
    // Undefined means a type that is erroneously non-existent. This is not the same as unknown.
    // The prototypical application is trying to resolve an unbound identifier.
    Undefined,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SubroutineDef {
    pub num_params: usize,
    pub return_type: Box<Type>,
}

// wow. Is there a less boiler-plated way?
// Return `true` if `ty1 == ty2` except that the `is_const`
// property is allowed to differ.
pub(crate) fn equal_up_to_constness(ty1: &Type, ty2: &Type) -> bool {
    use Type::*;
    // FIXME: Make sure we can remove following. Looks inefficient
    if ty1 == ty2 {
        return true;
    }
    match (ty1, ty2) {
        (Bit(_), Bit(_)) => true,
        (Duration(_), Duration(_)) => true,
        (Bool(_), Bool(_)) => true,
        (Stretch(_), Stretch(_)) => true,
        (Int(w1, _), Int(w2, _)) => w1 == w2,
        (UInt(w1, _), UInt(w2, _)) => w1 == w2,
        (Float(w1, _), Float(w2, _)) => w1 == w2,
        (Complex(w1, _), Complex(w2, _)) => w1 == w2,
        (Angle(w1, _), Angle(w2, _)) => w1 == w2,
        (BitArray(dims1, _), BitArray(dims2, _)) => dims1 == dims2,
        _ => false,
    }
}

// Are the base types of the scalars equal?
// (That is modulo width anc constness?)
// Returns `false` for all array types. Not sure what
// we need from arrays.
fn equal_base_type(ty1: &Type, ty2: &Type) -> bool {
    use Type::*;
    matches!(
        (ty1, ty2),
        (Bit(_), Bit(_))
            | (Duration(_), Duration(_))
            | (Bool(_), Bool(_))
            | (Stretch(_), Stretch(_))
            | (Int(..), Int(..))
            | (UInt(..), UInt(..))
            | (Float(..), Float(..))
            | (Complex(..), Complex(..))
            | (Angle(..), Angle(..))
    )
}

// OQ3 supports arrays with number of dims up to seven.
// Probably exists a much better way to represent dims... [usize, N]
// Could use Box for higher dimensional arrays, or...
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArrayDims {
    D1(usize),
    D2(usize, usize),
    D3(usize, usize, usize),
}

impl ArrayDims {
    pub fn num_dims(&self) -> usize {
        match self {
            ArrayDims::D1(..) => 1,
            ArrayDims::D2(..) => 2,
            ArrayDims::D3(..) => 3,
        }
    }

    pub fn dims(&self) -> Vec<usize> {
        match self {
            ArrayDims::D1(a) => vec![*a],
            ArrayDims::D2(a, b) => vec![*a, *b],
            ArrayDims::D3(a, b, c) => vec![*a, *b, *c],
        }
    }
}

impl Type {
    /// Return true if the type is a classical type and is neither an array type
    /// nor an array reference type.
    pub fn is_scalar(&self) -> bool {
        use Type::*;
        matches!(
            self,
            Bit(..)
                | Int(..)
                | UInt(..)
                | Float(..)
                | Angle(..)
                | Complex(..)
                | Bool(..)
                | Duration(..)
                | Stretch(..)
        )
    }

    /// Return the `Some(width)` if the type has a width `width`. Otherwise return `None`.
    /// The width of scalar types that support bit width is the bit width if present.
    /// The width of registers is the length of the register.
    pub fn width(&self) -> Width {
        use Type::*;
        match self {
            Int(w, _) | UInt(w, _) | Float(w, _) | Angle(w, _) | Complex(w, _) => *w,
            //            Bit(w, _) | Int(w, _) | UInt(w, _)  | Float(w, _)  | Angle(w, _)  | Complex(w, _) | Qubit(w) => *w,
            _ => None,
        }
    }

    // FIXME: I think types should be `const` by default.
    // We can't rebind qubit names or gatenames.
    // We have changed it to `true` for now.
    // Two ways to implement is_const. We choose second one.
    // 1. Return Some(true) or Some(false) for types that can be const, and None otherwise.
    // 2. Return true if type can be const and is const, otherwise false.
    //
    // The second choice emphasizes that, as in C, the "type modifier" `const` is an attribute of the type.
    // And two types that differ only in an attribute are different types.
    // `is_const` returns true if the type has this attribute. (or perhaps if the type has this attribute and its value
    // is `true`.)
    /// Return `true` if the type has the attribute `const`.
    pub fn is_const(&self) -> bool {
        use Type::*;
        match self {
            Bit(c)
            | Int(_, c)
            | UInt(_, c)
            | Float(_, c)
            | Angle(_, c)
            | Complex(_, c)
            | Bool(c)
            | Duration(c)
            | Stretch(c)
            | BitArray(_, c) => matches!(*c, IsConst::True),
            _ => true,
        }
    }

    /// Return `true` if the type is a qubit or qubit register.
    pub fn is_quantum(&self) -> bool {
        matches!(
            self,
            Type::Qubit | Type::QubitArray(..) | Type::HardwareQubit
        )
    }

    pub fn dims(&self) -> Option<Vec<usize>> {
        use Type::*;
        match self {
            QubitArray(dims) | IntArray(dims) | BitArray(dims, _) => Some(dims.dims()),
            _ => None,
        }
    }

    pub fn num_dims(&self) -> usize {
        use Type::*;
        match self {
            QubitArray(dims) | IntArray(dims) | BitArray(dims, _) => dims.num_dims(),
            _ => 0,
        }
    }

    // FIXME: Not finished
    /// Return `true` if the types have the same base type.
    /// The number of dimensions and dimensions may differ.
    pub fn equal_up_to_shape(&self, other: &Type) -> bool {
        use Type::*;
        if self == other {
            return true;
        }
        if matches!(self, BitArray(_, _)) && matches!(other, BitArray(_, _)) {
            return true;
        }
        if matches!(self, QubitArray(_)) && matches!(other, QubitArray(_)) {
            return true;
        }
        false
    }

    // FIXME: Not finished
    /// Return `true` if the types have the same base type and the same shape.
    /// The dimensions of each axis may differ.
    pub fn equal_up_to_dims(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }
        if self.num_dims() != other.num_dims() {
            return false;
        }
        self.equal_up_to_shape(other)
    }
}

#[test]
fn test_type_enum1() {
    let t = Type::Bit(IsConst::False);
    assert!(!t.is_const());
    assert!(t.width().is_none());
    assert!(!t.is_quantum());
    assert!(t.is_scalar());
}

#[test]
fn test_type_enum2() {
    let t = Type::Qubit;
    assert!(t.is_const());
    assert!(t.width().is_none());
    assert!(t.is_quantum());
    assert!(!t.is_scalar());
}

//
// Promotion
//

// `const` is less than non-`const`.
// (why does this look like the opposite of correct definition?)
fn promote_constness(ty1: &Type, ty2: &Type) -> IsConst {
    IsConst::from(ty1.is_const() && ty2.is_const())
}

// Return greater of the `Width`s of the types.
// The width `None` is the greatest width.
fn promote_width(ty1: &Type, ty2: &Type) -> Width {
    match (ty1.width(), ty2.width()) {
        (Some(width1), Some(width2)) => Some(std::cmp::max(width1, width2)),
        (Some(_), None) | (None, Some(_)) | (None, None) => None,
    }
}

pub fn promote_types_not_equal(ty1: &Type, ty2: &Type) -> Type {
    let typ = promote_type_width(ty1, ty2);
    if typ != Type::Void {
        return typ;
    }
    promote_base_type(ty1, ty2)
}

// promotion suitable for some binary operations, eg +, -, *
pub fn promote_types(ty1: &Type, ty2: &Type) -> Type {
    if equal_up_to_constness(ty1, ty2) {
        return ty1.clone();
    }
    let typ = promote_type_width(ty1, ty2);
    if typ != Type::Void {
        return typ;
    }
    promote_base_type(ty1, ty2)
}

/// Promotes the width of two types if they belong to the same type category.
///
/// This function checks whether the two types belong to the same category
/// (integers, unsigned integers, or floating-point numbers) and promotes them
/// to the one with the wider width. If the types are of different categories,
/// such as an integer and a float, the function returns `Type::Void`.
///
/// # Parameters
///
/// - `ty1`: A reference to the first type.
/// - `ty2`: A reference to the second type.
///
/// # Returns
///
/// Returns the type with the promoted width if `ty1` and `ty2` are of the same
/// category. If not, returns `Type::Void`.
fn promote_type_width(ty1: &Type, ty2: &Type) -> Type {
    use Type::*;
    let isconst = promote_constness(ty1, ty2);
    match (ty1, ty2) {
        (Int(..), Int(..)) => Int(promote_width(ty1, ty2), isconst),
        (UInt(..), UInt(..)) => UInt(promote_width(ty1, ty2), isconst),
        (Float(..), Float(..)) => Float(promote_width(ty1, ty2), isconst),
        _ => Void,
    }
}

/// Given two mathematical types, if one type is an (algebraic) extension of other,
/// promote to the larger type. If this is not possible, return `Void`.
/// TODO: Check OQ3 semantics on this. For example, we could promote a very wide integer
/// type to a narrow floating point type.
fn promote_base_type(ty1: &Type, ty2: &Type) -> Type {
    use Type::*;
    match (ty1, ty2) {
        (Int(..), Float(..)) => ty2.clone(),
        (UInt(..), Float(..)) => ty2.clone(),
        (Float(..), Complex(..)) => ty2.clone(),
        (Int(..), Complex(..)) => ty2.clone(),
        (UInt(..), Complex(..)) => ty2.clone(),
        (Float(..), Int(..))
        | (Float(..), UInt(..))
        | (Complex(..), Float(..))
        | (Complex(..), Int(..))
        | (Complex(..), UInt(..)) => promote_base_type(ty2, ty1),
        _ => Void,
    }
}

/// Can the literal type be cast to type `ty1` for assignment?
/// The width of a literal is a fiction that is not in the spec.
/// So when casting, the width does not matter.
///
/// We currently have `128` for the width of a literal `Int`.
/// and the literal parsed into a Rust `u128` plus a bool for
/// a sign. We need to check, outside of the semantics whether
/// the value of type `u128` can be cast to the lhs type.
/// For that, we need the value. `can_cast_literal` does not
/// know the value.
pub fn can_cast_literal(ty1: &Type, ty_lit: &Type) -> bool {
    use Type::*;
    if equal_base_type(ty1, ty_lit) {
        return true;
    }
    match (ty1, ty_lit) {
        (Float(..), Int(..)) => true,
        (Float(..), UInt(..)) => true,
        (Complex(..), Float(..)) => true,
        (Complex(..), Int(..)) => true,
        (Complex(..), UInt(..)) => true,

        // Listing these explicitly is slower, but
        // might be better for maintaining and debugging.
        (Int(..), Float(..)) => false,
        (UInt(..), Float(..)) => false,
        (Int(..), Complex(..)) => false,
        (UInt(..), Complex(..)) => false,
        _ => false,
    }
}
