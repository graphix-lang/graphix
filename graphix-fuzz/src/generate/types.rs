//! The generator's type vocabulary ([`GenType`]) and everything
//! type-directed that doesn't recurse through expressions: random type
//! selection, literal construction, and annotation rendering.

use crate::mutate::Rng;

#[derive(Debug, Clone, PartialEq)]
pub enum GenType {
    I64,
    F64,
    U8,
    Bool,
    Str,
    Tuple(Vec<GenType>),
    Array(Box<GenType>),
}

impl GenType {
    /// The graphix type-annotation text for this type (`Array<i64>`,
    /// `(i64, string)`, ...), used for `let x: T = ...`.
    pub fn render(&self) -> String {
        match self {
            GenType::I64 => "i64".into(),
            GenType::F64 => "f64".into(),
            GenType::U8 => "u8".into(),
            GenType::Bool => "bool".into(),
            GenType::Str => "string".into(),
            GenType::Tuple(elems) => {
                let parts: Vec<_> = elems.iter().map(|e| e.render()).collect();
                format!("({})", parts.join(", "))
            }
            GenType::Array(elem) => format!("Array<{}>", elem.render()),
        }
    }
}

pub(super) fn numeric_type(rng: &mut Rng) -> GenType {
    match rng.below(3) {
        0 => GenType::I64,
        1 => GenType::F64,
        _ => GenType::U8,
    }
}

pub(super) fn random_type(rng: &mut Rng, depth: usize) -> GenType {
    if depth == 0 {
        return match rng.below(5) {
            0 => GenType::I64,
            1 => GenType::F64,
            2 => GenType::U8,
            3 => GenType::Bool,
            _ => GenType::Str,
        };
    }
    match rng.below(8) {
        0 | 1 => GenType::I64,
        2 => GenType::F64,
        3 => GenType::U8,
        4 => GenType::Bool,
        5 => GenType::Str,
        6 => {
            let n = 2 + rng.below(2);
            GenType::Tuple((0..n).map(|_| random_type(rng, depth - 1)).collect())
        }
        _ => GenType::Array(Box::new(random_type(rng, depth - 1))),
    }
}

pub(super) fn literal(rng: &mut Rng, ty: &GenType) -> String {
    match ty {
        GenType::I64 => {
            let v = [0i64, 1, -1, 2, 42, 100, -100, 7][rng.below(8)];
            format!("i64:{v}")
        }
        GenType::U8 => format!("u8:{}", [0u8, 1, 2, 100, 255][rng.below(5)]),
        GenType::F64 => {
            let v = ["0.0", "1.0", "-1.0", "3.14", "2.5", "0.1"][rng.below(6)];
            format!("f64:{v}")
        }
        GenType::Bool => {
            if rng.below(2) == 0 {
                "true".into()
            } else {
                "false".into()
            }
        }
        GenType::Str => {
            let v = ["a", "hello", "", "xyz", "graphix"][rng.below(5)];
            format!("\"{v}\"")
        }
        GenType::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| literal(rng, e)).collect();
            format!("({})", parts.join(", "))
        }
        GenType::Array(elem) => {
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| literal(rng, elem)).collect();
            format!("[{}]", parts.join(", "))
        }
    }
}
