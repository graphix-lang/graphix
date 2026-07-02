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
    /// A lambda with fully annotated params/return — callable at
    /// exactly these types. Never produced by `random_type` (fn VALUES
    /// inside composites are deferred); enters scope only through
    /// lambda bindings.
    Fn {
        params: Vec<GenType>,
        ret: Box<GenType>,
    },
    /// An explicitly-polymorphic numeric lambda
    /// (`'a: Number |x: 'a, y: 'a| -> 'a x + y`) — per-call-site
    /// monomorphizing, callable with all args at any one numeric type
    /// (its body is built from params with `+ - *` and neg only, so
    /// the result type equals the argument type). Two call sites at
    /// distinct numeric types = a monomorphization pair, the audit's
    /// bug-2 shape.
    PolyFn {
        arity: usize,
    },
    /// A name deliberately bound to something OUTSIDE the typed
    /// vocabulary (a rec lambda, a bare wide-tvar lambda). The entry
    /// exists to MASK any binding the name shadowed — without it a
    /// stale entry would offer the dead earlier type to later
    /// references. Matches nothing; produced by no `random_type`.
    Opaque,
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
            // fn-type annotations name their positional params.
            GenType::Fn { params, ret } => {
                let parts: Vec<_> = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| format!("a{i}: {}", p.render()))
                    .collect();
                format!("fn({}) -> {}", parts.join(", "), ret.render())
            }
            GenType::PolyFn { .. } | GenType::Opaque => {
                unreachable!("poly/opaque bindings are never annotated")
            }
        }
    }

    pub(super) fn is_numeric(&self) -> bool {
        matches!(self, GenType::I64 | GenType::F64 | GenType::U8)
    }

    pub(super) fn is_scalar(&self) -> bool {
        matches!(
            self,
            GenType::I64 | GenType::F64 | GenType::U8 | GenType::Bool | GenType::Str
        )
    }
}

pub(super) fn scalar_type(rng: &mut Rng) -> GenType {
    match rng.below(5) {
        0 => GenType::I64,
        1 => GenType::F64,
        2 => GenType::U8,
        3 => GenType::Bool,
        _ => GenType::Str,
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
        return scalar_type(rng);
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
        GenType::Fn { .. } | GenType::PolyFn { .. } | GenType::Opaque => {
            unreachable!("fn/opaque types have no literal form")
        }
    }
}
