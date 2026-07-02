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
    /// Fields kept SORTED by name at construction, so structural
    /// equality (the vocabulary-matching relation) matches graphix's
    /// field-order-insensitive structs.
    Struct(Vec<(String, GenType)>),
    /// A tag union (`` [`A(i64), `B] ``). Only produced by dedicated
    /// let emission (a bare variant literal's type is its single tag —
    /// the union needs the annotation); consumed as pass-through data
    /// until select patterns land.
    Variant(Vec<(String, Vec<GenType>)>),
    /// String-keyed map, built from a small key pool so accesses
    /// mostly hit.
    Map(Box<GenType>),
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
    /// (its body is built from params with `+ - *` only, so the result
    /// type follows the argument type). Two call sites at distinct
    /// numeric types = a monomorphization pair, the audit's bug-2
    /// shape.
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
            GenType::Struct(fields) => {
                let parts: Vec<_> =
                    fields.iter().map(|(f, t)| format!("{f}: {}", t.render())).collect();
                format!("{{{}}}", parts.join(", "))
            }
            GenType::Variant(tags) => {
                let parts: Vec<_> = tags
                    .iter()
                    .map(|(tag, args)| {
                        if args.is_empty() {
                            format!("`{tag}")
                        } else {
                            let a: Vec<_> = args.iter().map(|t| t.render()).collect();
                            format!("`{tag}({})", a.join(", "))
                        }
                    })
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            GenType::Map(v) => format!("Map<string, {}>", v.render()),
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

/// Struct field names. Deliberately overlaps nothing with binding
/// names (`v<N>`) — field-vs-binding confusion is exercised through
/// select patterns later, not here.
pub(super) const FIELDS: &[&str] = &["a", "b", "c", "x", "y", "n"];

/// Map keys — small pool so generated accesses mostly hit.
pub(super) const KEYS: &[&str] = &["k0", "k1", "k2", "a", "b"];

/// Variant tags.
pub(super) const TAGS: &[&str] = &["A", "B", "C", "Some", "Nil"];

pub(super) fn random_struct(rng: &mut Rng, depth: usize) -> GenType {
    let n = 1 + rng.below(3);
    let mut fields: Vec<(String, GenType)> = Vec::new();
    for _ in 0..n {
        let f = FIELDS[rng.below(FIELDS.len())];
        if !fields.iter().any(|(g, _)| g == f) {
            fields.push((f.to_string(), random_type(rng, depth)));
        }
    }
    fields.sort_by(|a, b| a.0.cmp(&b.0));
    GenType::Struct(fields)
}

/// A random tag union: 2-3 distinct tags, each with 0-2 payload types.
pub(super) fn random_variant(rng: &mut Rng, depth: usize) -> GenType {
    let n = 2 + rng.below(2);
    let mut tags: Vec<(String, Vec<GenType>)> = Vec::new();
    for _ in 0..n {
        let t = TAGS[rng.below(TAGS.len())];
        if !tags.iter().any(|(u, _)| u == t) {
            let nargs = rng.below(3);
            tags.push((
                t.to_string(),
                (0..nargs).map(|_| random_type(rng, depth)).collect(),
            ));
        }
    }
    tags.sort_by(|a, b| a.0.cmp(&b.0));
    GenType::Variant(tags)
}

pub(super) fn random_type(rng: &mut Rng, depth: usize) -> GenType {
    if depth == 0 {
        return scalar_type(rng);
    }
    match rng.below(10) {
        0 | 1 => GenType::I64,
        2 => GenType::F64,
        3 => GenType::U8,
        4 => GenType::Bool,
        5 => GenType::Str,
        6 => {
            let n = 2 + rng.below(2);
            GenType::Tuple((0..n).map(|_| random_type(rng, depth - 1)).collect())
        }
        7 => random_struct(rng, depth - 1),
        8 => GenType::Map(Box::new(random_type(rng, depth - 1))),
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
        GenType::Struct(fields) => {
            let parts: Vec<_> =
                fields.iter().map(|(f, t)| format!("{f}: {}", literal(rng, t))).collect();
            format!("{{ {} }}", parts.join(", "))
        }
        GenType::Variant(tags) => {
            let (tag, args) = &tags[rng.below(tags.len())];
            if args.is_empty() {
                format!("`{tag}")
            } else {
                let parts: Vec<_> = args.iter().map(|t| literal(rng, t)).collect();
                format!("`{tag}({})", parts.join(", "))
            }
        }
        GenType::Map(v) => {
            let n = 1 + rng.below(3);
            let mut keys: Vec<&str> = Vec::new();
            for _ in 0..n {
                let k = KEYS[rng.below(KEYS.len())];
                if !keys.contains(&k) {
                    keys.push(k);
                }
            }
            let parts: Vec<_> =
                keys.iter().map(|k| format!("\"{k}\" => {}", literal(rng, v))).collect();
            format!("{{{}}}", parts.join(", "))
        }
        GenType::Fn { .. } | GenType::PolyFn { .. } | GenType::Opaque => {
            unreachable!("fn/opaque types have no literal form")
        }
    }
}
