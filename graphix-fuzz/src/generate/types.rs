//! The generator's type vocabulary ([`GenType`]) and everything
//! type-directed that doesn't recurse through expressions: random type
//! selection, literal construction, and annotation rendering.

use crate::mutate::Rng;

/// Every graphix numeric type. The fixed-width ten are the JIT's
/// register-scalar set; the variable-width four (`v`/`z`) are valid
/// language that de-fuses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumTy {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    V32,
    V64,
    Z32,
    Z64,
}

pub(super) const NUM_TYS: &[NumTy] = &[
    NumTy::I8,
    NumTy::I16,
    NumTy::I32,
    NumTy::I64,
    NumTy::U8,
    NumTy::U16,
    NumTy::U32,
    NumTy::U64,
    NumTy::F32,
    NumTy::F64,
    NumTy::V32,
    NumTy::V64,
    NumTy::Z32,
    NumTy::Z64,
];

impl NumTy {
    pub fn render(self) -> &'static str {
        match self {
            NumTy::I8 => "i8",
            NumTy::I16 => "i16",
            NumTy::I32 => "i32",
            NumTy::I64 => "i64",
            NumTy::U8 => "u8",
            NumTy::U16 => "u16",
            NumTy::U32 => "u32",
            NumTy::U64 => "u64",
            NumTy::F32 => "f32",
            NumTy::F64 => "f64",
            NumTy::V32 => "v32",
            NumTy::V64 => "v64",
            NumTy::Z32 => "z32",
            NumTy::Z64 => "z64",
        }
    }

    pub(super) fn is_float(self) -> bool {
        matches!(self, NumTy::F32 | NumTy::F64)
    }

    /// Signed (negation is legal): signed ints, zigzag varints, floats.
    pub(super) fn is_signed(self) -> bool {
        matches!(
            self,
            NumTy::I8
                | NumTy::I16
                | NumTy::I32
                | NumTy::I64
                | NumTy::Z32
                | NumTy::Z64
                | NumTy::F32
                | NumTy::F64
        )
    }

    /// `v`/`z` varints are alternate wire encodings of a fixed-width
    /// type; casts and fits follow the fixed-width twin.
    fn range_twin(self) -> NumTy {
        match self {
            NumTy::V32 => NumTy::U32,
            NumTy::V64 => NumTy::U64,
            NumTy::Z32 => NumTy::I32,
            NumTy::Z64 => NumTy::I64,
            t => t,
        }
    }

    /// Every value of `self` casts to `other` losslessly. Floats count
    /// integer embeddings only up to their exact-integer range.
    pub(super) fn fits_in(self, other: NumTy) -> bool {
        fn int_bits(t: NumTy) -> Option<(u32, bool)> {
            Some(match t {
                NumTy::I8 => (8, true),
                NumTy::I16 => (16, true),
                NumTy::I32 => (32, true),
                NumTy::I64 => (64, true),
                NumTy::U8 => (8, false),
                NumTy::U16 => (16, false),
                NumTy::U32 => (32, false),
                NumTy::U64 => (64, false),
                NumTy::F32 | NumTy::F64 => return None,
                NumTy::V32 | NumTy::V64 | NumTy::Z32 | NumTy::Z64 => unreachable!(),
            })
        }
        let (a, b) = (self.range_twin(), other.range_twin());
        match (int_bits(a), int_bits(b)) {
            (Some((ab, asig)), Some((bb, bsig))) => match (asig, bsig) {
                (false, false) | (true, true) => ab <= bb,
                (false, true) => ab < bb,
                (true, false) => false,
            },
            (Some((ab, asig)), None) => {
                let mant = if b == NumTy::F32 { 24 } else { 53 };
                if asig { ab - 1 <= mant } else { ab <= mant }
            }
            (None, None) => a == NumTy::F32 || b == NumTy::F64,
            (None, Some(_)) => false,
        }
    }

    /// A literal of this type from a per-type pool that includes the
    /// boundary values.
    pub(super) fn literal(self, rng: &mut Rng) -> String {
        let v: &str = match self {
            NumTy::I8 => {
                ["0", "1", "-1", "7", "100", "-100", "127", "-128"][rng.below(8)]
            }
            NumTy::I16 => {
                ["0", "1", "-1", "7", "1000", "-1000", "32767", "-32768"][rng.below(8)]
            }
            NumTy::I32 => {
                ["0", "1", "-1", "42", "100000", "-100000", "2147483647", "-2147483648"]
                    [rng.below(8)]
            }
            NumTy::I64 => ["0", "1", "-1", "2", "42", "100", "-100", "7"][rng.below(8)],
            NumTy::U8 => ["0", "1", "2", "100", "255"][rng.below(5)],
            NumTy::U16 => ["0", "1", "2", "1000", "65535"][rng.below(5)],
            NumTy::U32 => ["0", "1", "7", "100000", "4294967295"][rng.below(5)],
            NumTy::U64 => {
                ["0", "1", "7", "1000000", "18446744073709551615"][rng.below(5)]
            }
            NumTy::F32 => ["0.0", "1.0", "-1.0", "3.5", "0.25", "-2.5"][rng.below(6)],
            NumTy::F64 => ["0.0", "1.0", "-1.0", "3.14", "2.5", "0.1"][rng.below(6)],
            NumTy::V32 => ["0", "1", "7", "1000", "4294967295"][rng.below(5)],
            NumTy::V64 => ["0", "1", "7", "1000000"][rng.below(4)],
            NumTy::Z32 => ["0", "1", "-1", "7", "-1000"][rng.below(5)],
            NumTy::Z64 => ["0", "1", "-1", "42", "-1000000"][rng.below(5)],
        };
        format!("{}:{v}", self.render())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenType {
    Num(NumTy),
    Bool,
    Str,
    Tuple(Vec<GenType>),
    Array(Box<GenType>),
    /// The native List: produced as `[<..>]` literals / `list::from_array`
    /// / `cons`, consumed by list HOFs and list-slice pattern selects.
    List(Box<GenType>),
    /// Fields kept sorted by name, so structural equality matches
    /// graphix's field-order-insensitive structs.
    Struct(Vec<(String, GenType)>),
    /// A tag union (`` [`A(i64), `B] ``). Only produced by dedicated let
    /// emission: a bare variant literal's type is its single tag.
    Variant(Vec<(String, Vec<GenType>)>),
    /// String-keyed map over a small key pool so accesses mostly hit.
    Map(Box<GenType>),
    /// An option (`[T, null]`).
    Nullable(Box<GenType>),
    /// A lambda with fully annotated params/return. Never produced by
    /// `random_type`; enters scope only through lambda bindings.
    Fn {
        params: Vec<GenType>,
        ret: Box<GenType>,
    },
    /// An explicitly-polymorphic numeric lambda, callable with all args
    /// at any one numeric type; the result type follows the argument type.
    PolyFn {
        arity: usize,
    },
    /// A reference `&T` with a scalar inner type. Never produced by
    /// `random_type`; introduced by dedicated statements, ref-typed
    /// params, and `&literal` leaves.
    Ref(Box<GenType>),
    /// A generated module's abstract type, identified by its module.
    /// Produced only by `<module>::mk(i64)`; the only structural consumer
    /// is `<module>::un(T) -> i64`. Never produced by `random_type`.
    Abstract {
        module: String,
    },
    /// A name bound to something outside the typed vocabulary (a rec
    /// lambda, a bare wide-tvar lambda). The entry masks any binding the
    /// name shadowed. Matches nothing.
    Opaque,
}

/// The dominant trio, named for template code.
pub(super) const I64: GenType = GenType::Num(NumTy::I64);
pub(super) const F64: GenType = GenType::Num(NumTy::F64);
pub(super) const U8: GenType = GenType::Num(NumTy::U8);

impl GenType {
    /// The graphix type-annotation text for this type.
    pub fn render(&self) -> String {
        match self {
            GenType::Num(n) => n.render().into(),
            GenType::Bool => "bool".into(),
            GenType::Str => "string".into(),
            GenType::Tuple(elems) => {
                let parts: Vec<_> = elems.iter().map(|e| e.render()).collect();
                format!("({})", parts.join(", "))
            }
            GenType::Array(elem) => format!("Array<{}>", elem.render()),
            GenType::List(elem) => format!("List<{}>", elem.render()),
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
            GenType::Nullable(t) => format!("[{}, null]", t.render()),
            GenType::Abstract { module } => format!("{module}::T"),
            GenType::Ref(t) => format!("&{}", t.render()),
            // fn-type annotations name their positional params
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
        matches!(self, GenType::Num(_))
    }

    /// Whether the type contains an option anywhere. Such a binding must
    /// be annotated: an unannotated `let v = null` infers `null`, not
    /// the union.
    pub(super) fn contains_nullable(&self) -> bool {
        match self {
            GenType::Nullable(_) => true,
            GenType::Tuple(es) => es.iter().any(|e| e.contains_nullable()),
            GenType::Struct(fs) => fs.iter().any(|(_, t)| t.contains_nullable()),
            GenType::Variant(ts) => {
                ts.iter().any(|(_, args)| args.iter().any(|t| t.contains_nullable()))
            }
            GenType::Array(e) | GenType::List(e) | GenType::Map(e) => {
                e.contains_nullable()
            }
            GenType::Ref(t) => t.contains_nullable(),
            GenType::Num(_)
            | GenType::Bool
            | GenType::Str
            | GenType::Fn { .. }
            | GenType::PolyFn { .. }
            | GenType::Abstract { .. }
            | GenType::Opaque => false,
        }
    }

    pub(super) fn is_scalar(&self) -> bool {
        matches!(self, GenType::Num(_) | GenType::Bool | GenType::Str)
    }

    /// A literal-free generated body of this type infers exactly this
    /// type: no Variant and no Nullable anywhere. Gates unannotated-return
    /// impls, whose signature must match the inferred type exactly.
    pub(super) fn infers_exact(&self) -> bool {
        match self {
            GenType::Num(_) | GenType::Bool | GenType::Str | GenType::Abstract { .. } => {
                true
            }
            GenType::Variant(_)
            | GenType::Nullable(_)
            | GenType::Ref(_)
            | GenType::Fn { .. }
            | GenType::PolyFn { .. }
            | GenType::Opaque => false,
            GenType::Tuple(es) => es.iter().all(|e| e.infers_exact()),
            GenType::Struct(fs) => fs.iter().all(|(_, t)| t.infers_exact()),
            GenType::Array(e) | GenType::List(e) | GenType::Map(e) => e.infers_exact(),
        }
    }
}

/// A random numeric type: the dominant trio at 60%, the rest of the
/// family at 40%.
pub(super) fn numeric_type(rng: &mut Rng) -> GenType {
    GenType::Num(num_ty(rng))
}

pub(super) fn num_ty(rng: &mut Rng) -> NumTy {
    match rng.below(10) {
        0 | 1 => NumTy::I64,
        2 | 3 => NumTy::F64,
        4 | 5 => NumTy::U8,
        _ => NUM_TYS[rng.below(NUM_TYS.len())],
    }
}

pub(super) fn scalar_type(rng: &mut Rng) -> GenType {
    match rng.below(5) {
        0..=2 => numeric_type(rng),
        3 => GenType::Bool,
        _ => GenType::Str,
    }
}

/// Struct field names; disjoint from binding names (`v<N>`).
pub(super) const FIELDS: &[&str] = &["a", "b", "c", "x", "y", "n"];

/// Map keys: a small pool so generated accesses mostly hit.
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
/// Two tags minimum: `gen_pattern` counts a variant pattern refutable
/// on that premise, and a single-tag union makes the final arm dead.
pub(super) fn random_variant(rng: &mut Rng, depth: usize) -> GenType {
    let n = 2 + rng.below(2);
    let mut tags: Vec<(String, Vec<GenType>)> = Vec::new();
    while tags.len() < n {
        let t = TAGS[rng.below(TAGS.len())];
        if !tags.iter().any(|(u, _)| u == t) {
            let nargs = rng.below(3);
            tags.push((
                t.to_string(),
                (0..nargs).map(|_| random_type(rng, depth)).collect(),
            ));
        }
    }
    // same-tag arity case: a variant's identity is (tag, arity), so
    // `` [`A, `A(i64)] `` is a legal union
    if rng.below(8) == 0 {
        let (t, args) = tags[rng.below(tags.len())].clone();
        let mut arity = args.len();
        while arity == args.len() {
            arity = rng.below(3);
        }
        tags.push((t, (0..arity).map(|_| random_type(rng, depth)).collect()));
    }
    tags.sort_by(|a, b| a.0.cmp(&b.0));
    GenType::Variant(tags)
}

pub(super) fn random_type(rng: &mut Rng, depth: usize) -> GenType {
    if depth == 0 {
        return scalar_type(rng);
    }
    match rng.below(12) {
        0 | 1 => I64,
        2 => F64,
        3 => U8,
        4 => GenType::Bool,
        5 => GenType::Str,
        6 => {
            let n = 2 + rng.below(2);
            GenType::Tuple((0..n).map(|_| random_type(rng, depth - 1)).collect())
        }
        7 => random_struct(rng, depth - 1),
        8 => GenType::Map(Box::new(random_type(rng, depth - 1))),
        9 => GenType::Nullable(Box::new(scalar_type(rng))),
        10 => GenType::List(Box::new(random_type(rng, depth - 1))),
        _ => GenType::Array(Box::new(random_type(rng, depth - 1))),
    }
}

pub(super) fn literal(rng: &mut Rng, ty: &GenType) -> String {
    match ty {
        GenType::Num(n) => n.literal(rng),
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
        GenType::List(elem) => {
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| literal(rng, elem)).collect();
            // nullable-bearing elements must be annotated: a `null` part
            // infers bare null, not the union
            if elem.contains_nullable() {
                format!(
                    "{{ let mtl: List<{}> = [<{}>]; mtl }}",
                    elem.render(),
                    parts.join(", ")
                )
            } else {
                format!("[<{}>]", parts.join(", "))
            }
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
        GenType::Nullable(t) => {
            if rng.below(3) == 0 {
                "null".into()
            } else {
                literal(rng, t)
            }
        }
        // a ref to a literal
        GenType::Ref(t) => format!("&{}", literal(rng, t)),
        // no literal form; the constructor over an i64 literal is the
        // closed leaf
        GenType::Abstract { module } => {
            format!("{module}::mk({})", literal(rng, &GenType::Num(NumTy::I64)))
        }
        GenType::Fn { .. } | GenType::PolyFn { .. } | GenType::Opaque => {
            unreachable!("fn/opaque types have no literal form")
        }
    }
}
