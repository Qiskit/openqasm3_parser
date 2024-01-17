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
    Gate,  // <-- this type seems anomalous
    Range, // temporary placeholder, perhaps
    Void,
    ToDo, // not yet implemented
    // Undefined means a type that is erroneously non-existent. This is not the same as unknown.
    // The prototypical application is trying to resolve an unbound identifier.
    Undefined,
    // Void, do we need this?
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
    pub fn num_dims(&self) -> i32 {
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
        matches!(self, Type::Qubit | Type::QubitArray(..))
    }

    pub fn dims(&self) -> Option<Vec<usize>> {
        use Type::*;
        match self {
            QubitArray(dims) | IntArray(dims) => Some(dims.dims()),
            _ => None,
        }
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

fn promote_constness(ty1: &Type, ty2: &Type) -> IsConst {
    IsConst::from(ty1.is_const() && ty2.is_const())
}

fn promote_width(ty1: &Type, ty2: &Type) -> Width {
    match (ty1.width(), ty2.width()) {
        (Some(width1), Some(width2)) => Some(std::cmp::max(width1, width2)),
        (Some(_), None) | (None, Some(_)) | (None, None) => None,
    }
}

// promotion suitable for some binary operations, eg +, -, *, /
pub fn promote_types(ty1: &Type, ty2: &Type) -> Type {
    use Type::*;
    if ty1 == ty2 {
        return ty1.clone();
    }
    let isconst = promote_constness(ty1, ty2);
    match (ty1, ty2) {
        (Int(..), Int(..)) => Int(promote_width(ty1, ty2), isconst),
        (UInt(..), UInt(..)) => UInt(promote_width(ty1, ty2), isconst),
        (Int(..), Float(..)) => ty2.clone(),
        (Float(..), Int(..)) => ty1.clone(),
        _ => Void,
    }
}

// FIXME: always returns false
// Return `true` if `ty1 == ty2` except that the `is_const`
// property is allowed to differ.
fn equal_up_to_constness(_ty1: &Type, _ty2: &Type) -> bool {
    false
    //     match (ty1, ty2) {
    // //        (Bit(w1, _), Bit(w2, _)) => w1 == w2,
    //         _ => false,
    //     }
}

// Return `true` of  `from_type` can be cast to `to_type`.
// Warning: It is assumed that `equal_up_to_constness(from_type, to_type)`
// returns `false`. If not, `can_cast_strict` may give incorrect results.
// Unused at the moment
// fn can_cast_strict(from_type: &Type, to_type: &Type) -> bool {
//     use Type::*;
//     !matches!((from_type, to_type), (Bit(..), Bit(..)))
// }

// FIXME: This in unfinished and permissive.
/// Return `true` if `from_type` is equal to `to_type` up to constness,
/// or if `from_type` can be cast to `to_type`. What "can be cast" means
/// is perhaps a bit vague at the moment.
pub(crate) fn can_cast_loose(from_type: &Type, to_type: &Type) -> bool {
    use Type::*;
    equal_up_to_constness(from_type, to_type) || !matches!((from_type, to_type), (Bit(..), Bit(..)))
}
