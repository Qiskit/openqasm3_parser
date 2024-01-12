// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

// Test validating types of arguments to binary ops. That is test if they are legal.
// And compute the common type that they must be promoted to.
// You can run this with `cargo run --example promote`
#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Type {
    Int { konst: bool, size: u32 },
    UInt { konst: bool, size: u32 },
    Float { konst: bool, size: u32 },
    Angle { konst: bool, size: u32 },
    Complex { konst: bool, size: u32 },
    // We could define the following to allow complex numbers outside the spec.
    // Complex {konst: bool, size: u32, rtype: xxx},
    Qubit,
    Bottom,
}

#[derive(Clone, Debug)]
enum BinOp {
    Add,
    Multiply,
}

use crate::Type::{Angle, Bottom, Complex, Float, Int, UInt};

/// Pairs of types int, uint, float, and complex, can be arguments of several binary ops.
fn _validate_basic_pairs(x: Type, y: Type) -> bool {
    match (x, y) {
        (
            UInt { konst: _, size: _ }
            | Int { konst: _, size: _ }
            | Float { konst: _, size: _ }
            | Complex { konst: _, size: _ },
            UInt { konst: _, size: _ }
            | Int { konst: _, size: _ }
            | Float { konst: _, size: _ }
            | Complex { konst: _, size: _ },
        ) => true,

        (_, _) => false,
    }
}

/// Return `true` if `x + y` is defined (not a type error) in OQ3.
fn valid_args_add(x: Type, y: Type) -> bool {
    if _validate_basic_pairs(x, y) {
        return true;
    }
    match (x, y) {
        // "Addition + and subtraction - by other angles of the same size"
        (Angle { konst: _, size: sx }, Angle { konst: _, size: sy }) => sx == sy,
        (_, _) => false,
    }
}

// In some places in the spec, it says that angles must have the same sizes in some binary ops.
// In another place, it says that angle sizes will be converted as needed.
// Don't know how to reconcile this. We choose the former rule here.
/// Return `true` if `x * y` is defined (not a type error) in OQ3.
fn valid_args_multiply(x: Type, y: Type) -> bool {
    if _validate_basic_pairs(x, y) {
        return true;
    }
    match (x, y) {
        // "Multiplication * and division / by unsigned integers of the same size."
        (UInt { konst: _, size: sx }, Angle { konst: _, size: sy }) => sx == sy,

        (_, _) => false,
    }
}

/// Promote types x, y to an output type.
/// Note that conversion of values of these types occurs elsewhere.
/// But the information here is neccessary to know what to convert to.
/// You call both promote_type(x, y) and promote_type(y, x) for application to a
/// commutative operation.
/// We don't handle missing type information here. I have not thought about what
/// To do. It may depend on the context. Or the value of the data attached to the
/// type, if that value is available at compile time.
fn promote_type(x: Type, y: Type) -> Type {
    if x == y {
        return x;
    }
    match (x, y) {
        // With two integer types, return the wider one.
        (
            Int {
                konst: kx,
                size: sx,
            },
            Int {
                konst: ky,
                size: sy,
            },
        ) => Int {
            konst: kx && ky,
            size: std::cmp::max(sx, sy),
        },

        // If one is float and the other an integer type, return float
        (
            Float {
                konst: kx,
                size: sx,
            },
            Int { konst: ky, size: _ } | UInt { konst: ky, size: _ },
        ) => Float {
            konst: kx && ky,
            size: sx,
        },

        (
            Angle {
                konst: kx,
                size: sx,
            },
            UInt {
                konst: ky,
                size: sy,
            },
        ) if sx == sy => Angle {
            konst: kx && ky,
            size: sx,
        },

        (_, _) => Bottom, // No rule found for promotion
    }
}

fn _one_call(op: BinOp, type1: Type, type2: Type) -> Option<Type> {
    match op {
        BinOp::Add => {
            if !valid_args_add(type1, type2) {
                return None;
            }
        }
        BinOp::Multiply => {
            if !valid_args_multiply(type1, type2) {
                return None;
            }
        }
    }
    let mut otype = promote_type(type1, type2);
    if otype == Bottom {
        otype = promote_type(type2, type1);
    }
    assert!(otype != Bottom);
    return Some(otype);
}

// Analyze a vector of triples (Binary op, type of first arg, type of second arg).
// Return None if the operation is not defined by the OQ3 spec.
// Otherwise return the common type that both args should be cast to
// before performing the operation.
//
// The output looks something like this:
// op: Add, t1: Int { konst: true, size: 16 }, t2: Int { konst: true, size: 16 } promoted: Int { konst: true, size: 16 }
// op: Add, t1: Int { konst: true, size: 16 }, t2: Int { konst: false, size: 16 } promoted: Int { konst: false, size: 16 }
// op: Add, t1: Int { konst: true, size: 16 }, t2: Int { konst: false, size: 32 } promoted: Int { konst: false, size: 32 }
// op: Add, t1: Int { konst: true, size: 64 }, t2: Float { konst: false, size: 32 } promoted: Float { konst: false, size: 32 }
// op: Add, t1: UInt { konst: true, size: 64 }, t2: Angle { konst: false, size: 32 } Not allowed
// op: Multiply, t1: UInt { konst: true, size: 64 }, t2: Angle { konst: false, size: 32 } Not allowed
// op: Multiply, t1: UInt { konst: true, size: 64 }, t2: Angle { konst: false, size: 64 } promoted: Angle { konst: false, size: 64 }
fn try_promotions_for_call() {
    let v = [
        (
            BinOp::Add,
            Int {
                konst: true,
                size: 16,
            },
            Int {
                konst: true,
                size: 16,
            },
        ),
        (
            BinOp::Add,
            Int {
                konst: true,
                size: 16,
            },
            Int {
                konst: false,
                size: 16,
            },
        ),
        (
            BinOp::Add,
            Int {
                konst: true,
                size: 16,
            },
            Int {
                konst: false,
                size: 32,
            },
        ),
        (
            BinOp::Add,
            Int {
                konst: true,
                size: 64,
            },
            Float {
                konst: false,
                size: 32,
            },
        ),
        (
            BinOp::Add,
            UInt {
                konst: true,
                size: 64,
            },
            Angle {
                konst: false,
                size: 32,
            },
        ),
        (
            BinOp::Multiply,
            UInt {
                konst: true,
                size: 64,
            },
            Angle {
                konst: false,
                size: 32,
            },
        ),
        (
            BinOp::Multiply,
            UInt {
                konst: true,
                size: 64,
            },
            Angle {
                konst: false,
                size: 64,
            },
        ),
    ];
    for (op, type1, type2) in v {
        print!("op: {:?}, t1: {:?}, t2: {:?}", op.clone(), type1, type2);
        match _one_call(op, type1, type2) {
            Some(t) => println!(" promoted: {:?}", t),
            _ => println!(" Not allowed"),
        }
    }
}

fn main() {
    try_promotions_for_call();
}
