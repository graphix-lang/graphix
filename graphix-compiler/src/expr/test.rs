use super::*;
use crate::{
    expr::parser::parse_one,
    format_with_flags,
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, TypeRef},
};
use bytes::Bytes;
use chrono::prelude::*;
use enumflags2::BitFlags;
use netidx_value::PBytes;
use netidx_value::Typ;
use parser::RESERVED;
use poolshark::local::LPooled;
use prop::option;
use proptest::{collection, prelude::*};
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::{iter, time::Duration};

const SLEN: usize = 16;

fn datetime() -> impl Strategy<Value = DateTime<Utc>> {
    (
        DateTime::<Utc>::MIN_UTC.timestamp()..DateTime::<Utc>::MAX_UTC.timestamp(),
        0..1_000_000_000u32,
    )
        .prop_map(|(s, ns)| Utc.timestamp_opt(s, ns).unwrap())
}

fn duration() -> impl Strategy<Value = Duration> {
    (any::<u64>(), 0..1_000_000_000u32).prop_map(|(s, ns)| Duration::new(s, ns))
}

fn pbytes() -> impl Strategy<Value = PBytes> {
    any::<Vec<u8>>().prop_map(|b| PBytes::from(Bytes::from(b)))
}

fn arcstr() -> impl Strategy<Value = ArcStr> {
    any::<String>().prop_map(ArcStr::from)
}

/// `#[name]` / `#[name(arg, ..)]`. The args are leaves rather than full
/// expressions: an arg prints through `Display for Expr`, so a decorated
/// one would put a comment line inside the brackets, and the recursion
/// would have to be threaded through every caller of `decorations`.
fn attr() -> impl Strategy<Value = Attr> {
    (random_fname(), collection::vec(prop_oneof![constant(), reference()], 0..3))
        .prop_map(|(name, args)| Attr { name, args: Arc::from_iter(args) })
}

/// The `//` comment lines and `#[..]` attributes an expression can carry.
/// Comment text is any run short of a newline that does not open with `/`
/// (that would read back as a `///` doc comment). `None` when there are
/// none of either, as the parser leaves an undecorated expression.
fn decorations() -> impl Strategy<Value = Option<Box<Decorations>>> {
    let comment = "[ a-zA-Z0-9_,.!?*-]{0,24}".prop_map(ArcStr::from);
    (collection::vec(comment, 0..3), collection::vec(attr(), 0..2)).prop_map(
        |(comments, attrs)| {
            if comments.is_empty() && attrs.is_empty() {
                None
            } else {
                Some(Box::new(Decorations {
                    comments: Arc::from_iter(comments),
                    attrs: Arc::from_iter(attrs),
                }))
            }
        },
    )
}

/// `inner` with decorations above it. Legal ONLY where the parser
/// captures decorations — an expression position (a block item, a call
/// argument, the top level) or one of the heads that hand them to the
/// expression below (a select arm, an impl method, a struct field).
/// Decorating an operand (`1 + <decorated>`) would be an interior comment,
/// a parse error by design.
fn decorated(inner: impl Strategy<Value = Expr>) -> impl Strategy<Value = Expr> {
    (inner, decorations()).prop_map(|(mut e, dec)| {
        e.dec = dec;
        e
    })
}

fn value() -> impl Strategy<Value = Value> {
    let leaf = prop_oneof![
        any::<i8>().prop_map(Value::I8),
        any::<u8>().prop_map(Value::U8),
        any::<i16>().prop_map(Value::I16),
        any::<u16>().prop_map(Value::U16),
        any::<u32>().prop_map(Value::U32),
        any::<u32>().prop_map(Value::V32),
        any::<i32>().prop_map(Value::I32),
        any::<i32>().prop_map(Value::Z32),
        any::<u64>().prop_map(Value::U64),
        any::<u64>().prop_map(Value::V64),
        any::<i64>().prop_map(Value::I64),
        any::<i64>().prop_map(Value::Z64),
        any::<f32>().prop_map(Value::F32),
        any::<f64>().prop_map(Value::F64),
        any::<[u8; 16]>().prop_map(|a| Value::Decimal(Arc::new(Decimal::deserialize(a)))),
        datetime().prop_map(|d| Value::DateTime(Arc::new(d))),
        duration().prop_map(|d| Value::Duration(Arc::new(d))),
        arcstr().prop_map(Value::String),
        pbytes().prop_map(Value::Bytes),
        Just(Value::Bool(true)),
        Just(Value::Bool(false)),
        Just(Value::Null),
    ];
    leaf.prop_recursive(1, 1, 1, |inner| {
        prop_oneof![inner.clone().prop_map(|v| Value::Error(v.into())),]
    })
}

fn random_modpart() -> impl Strategy<Value = String> {
    collection::vec(prop_oneof![Just(b'_'), b'a'..=b'z', b'0'..=b'9'], 1..=SLEN - 1)
        .prop_map(|mut v| unsafe {
            if v[0] == b'_' {
                v[0] = b'a';
            }
            if v[0] >= b'0' && v[0] <= b'9' {
                v[0] += 49;
            }
            String::from_utf8_unchecked(v)
        })
        .prop_filter("Filter reserved words", |s| !RESERVED.contains(s.as_str()))
}

fn typart() -> impl Strategy<Value = ArcStr> {
    collection::vec(prop_oneof![Just(b'_'), b'a'..=b'z', b'0'..=b'9'], 1..=SLEN - 1)
        .prop_map(|mut v| unsafe {
            if v[0] == b'_' {
                v[0] = b'A';
            }
            if v[0] >= b'0' && v[0] <= b'9' {
                v[0] += 17;
            }
            if v[0] >= 97 {
                v[0] -= 32;
            }
            ArcStr::from(String::from_utf8_unchecked(v))
        })
        .prop_filter("Filter reserved words", |s| !RESERVED.contains(s.as_str()))
}

fn valid_fname() -> impl Strategy<Value = ArcStr> {
    prop_oneof![
        Just(ArcStr::from("all")),
        Just(ArcStr::from("sum")),
        Just(ArcStr::from("product")),
        Just(ArcStr::from("divide")),
        Just(ArcStr::from("mean")),
        Just(ArcStr::from("min")),
        Just(ArcStr::from("max")),
        Just(ArcStr::from("and")),
        Just(ArcStr::from("or")),
        Just(ArcStr::from("not")),
        Just(ArcStr::from("cmp")),
        Just(ArcStr::from("filter")),
        Just(ArcStr::from("isa")),
        Just(ArcStr::from("eval")),
        Just(ArcStr::from("count")),
        Just(ArcStr::from("sample")),
        Just(ArcStr::from("join")),
        Just(ArcStr::from("concat")),
        Just(ArcStr::from("navigate")),
        Just(ArcStr::from("confirm")),
        Just(ArcStr::from("load")),
        Just(ArcStr::from("get")),
        Just(ArcStr::from("store")),
        Just(ArcStr::from("set")),
        Just(ArcStr::from("selected")),
        Just(ArcStr::from("throwsstuff")),
        Just(ArcStr::from("anything")),
        Just(ArcStr::from("letstuff")),
        Just(ArcStr::from("recval")),
        Just(ArcStr::from("castval")),
        Just(ArcStr::from("withval")),
        Just(ArcStr::from("catchit")),
        Just(ArcStr::from("valn")),
        Just(ArcStr::from("used")),
        Just(ArcStr::from("mods")),
        Just(ArcStr::from("sigs")),
        Just(ArcStr::from("as_thing")),
        Just(ArcStr::from("if_thing")),
        // type keywords are legal BINDING names (2026-08-18) — mix
        // them into every name pool so the round trip hunts for
        // positions we missed
        Just(ArcStr::from("duration")),
        Just(ArcStr::from("string")),
        Just(ArcStr::from("i64")),
        Just(ArcStr::from("bool")),
        Just(ArcStr::from("datetime")),
        Just(ArcStr::from("f64")),
        Just(ArcStr::from("decimal")),
    ]
}

fn random_fname() -> impl Strategy<Value = ArcStr> {
    prop_oneof![random_modpart().prop_map(ArcStr::from), valid_fname()]
}

/// Struct FIELD names may be reserved words (2026-08-18) — mix keywords
/// in so the round trip exercises the relaxed grammar. Binding and type
/// names stay `random_fname`.
fn field_name() -> impl Strategy<Value = ArcStr> {
    prop_oneof![
        random_fname(),
        random_fname(),
        random_fname(),
        Just(ArcStr::from("duration")),
        Just(ArcStr::from("string")),
        Just(ArcStr::from("i64")),
        Just(ArcStr::from("bool")),
        Just(ArcStr::from("datetime")),
        Just(ArcStr::from("select")),
        Just(ArcStr::from("cast")),
        // field-only: `bytes` can't bind (base64-payload ambiguity)
        Just(ArcStr::from("bytes")),
    ]
}

fn tvar() -> impl Strategy<Value = TVar> {
    random_fname().prop_map(|n| TVar::empty_named(n))
}

/// Optional keyword ROOT (`self`/`package`/a `super` chain) — legal
/// leading-only, in expression, connect, sandbox, and type paths alike.
fn path_lead() -> impl Strategy<Value = Vec<String>> {
    prop_oneof![
        6 => Just(Vec::new()),
        1 => Just(vec!["self".to_string()]),
        1 => Just(vec!["package".to_string()]),
        1 => (1..3usize).prop_map(|n| vec!["super".to_string(); n]),
    ]
}

fn random_modpath() -> impl Strategy<Value = ModPath> {
    // `self` is special inside use groups (the enclosing prefix), so a
    // literal `self` segment would print/reparse asymmetrically —
    // keyword roots come only from `path_lead`.
    let segs = collection::vec(
        random_modpart().prop_filter("self is special in use", |s| s != "self"),
        (1, 5),
    );
    (path_lead(), segs)
        .prop_map(|(lead, segs)| ModPath::from_iter(lead.into_iter().chain(segs)))
}

fn typath() -> impl Strategy<Value = ModPath> {
    (path_lead(), collection::vec(random_modpart(), (0, 4)), typart()).prop_map(
        |(lead, path, typ)| {
            ModPath(
                ModPath::from_iter(lead.into_iter().chain(path)).0.append(typ.as_str()),
            )
        },
    )
}

fn valid_modpath() -> impl Strategy<Value = ModPath> {
    prop_oneof![
        Just(ModPath::from_iter(["all"])),
        Just(ModPath::from_iter(["sum"])),
        Just(ModPath::from_iter(["product"])),
        Just(ModPath::from_iter(["divide"])),
        Just(ModPath::from_iter(["mean"])),
        Just(ModPath::from_iter(["min"])),
        Just(ModPath::from_iter(["max"])),
        Just(ModPath::from_iter(["and"])),
        Just(ModPath::from_iter(["or"])),
        Just(ModPath::from_iter(["not"])),
        Just(ModPath::from_iter(["cmp"])),
        Just(ModPath::from_iter(["filter"])),
        Just(ModPath::from_iter(["isa"])),
        Just(ModPath::from_iter(["eval"])),
        Just(ModPath::from_iter(["count"])),
        Just(ModPath::from_iter(["sample"])),
        Just(ModPath::from_iter(["str", "join"])),
        Just(ModPath::from_iter(["str", "concat"])),
        Just(ModPath::from_iter(["navigate"])),
        Just(ModPath::from_iter(["confirm"])),
        Just(ModPath::from_iter(["load"])),
        Just(ModPath::from_iter(["get"])),
        Just(ModPath::from_iter(["store"])),
        Just(ModPath::from_iter(["set"])),
    ]
}

fn modpath() -> impl Strategy<Value = ModPath> {
    prop_oneof![random_modpath(), valid_modpath()]
}

fn constant() -> impl Strategy<Value = Expr> {
    value().prop_map(|v| ExprKind::Constant(v).to_expr_nopos())
}

fn reference() -> impl Strategy<Value = Expr> {
    modpath().prop_map(|name| ExprKind::Ref { name }.to_expr_nopos())
}

fn typ() -> impl Strategy<Value = Typ> {
    prop_oneof![
        Just(Typ::I8),
        Just(Typ::U8),
        Just(Typ::I16),
        Just(Typ::U16),
        Just(Typ::U32),
        Just(Typ::V32),
        Just(Typ::I32),
        Just(Typ::Z32),
        Just(Typ::U64),
        Just(Typ::V64),
        Just(Typ::I64),
        Just(Typ::Z64),
        Just(Typ::F32),
        Just(Typ::F64),
        Just(Typ::Decimal),
        Just(Typ::DateTime),
        Just(Typ::Duration),
        Just(Typ::Bool),
        Just(Typ::String),
        Just(Typ::Bytes),
        Just(Typ::Error),
        Just(Typ::Array),
        Just(Typ::Null),
    ]
}

fn typexp() -> impl Strategy<Value = Type> {
    let leaf = prop_oneof![
        Just(Type::Bottom),
        Just(Type::Any),
        collection::vec(typ(), (0, 10)).prop_map(|mut prims| {
            prims.sort();
            prims.dedup();
            Type::Primitive(BitFlags::from_iter(prims))
        }),
        tvar().prop_map(Type::TVar),
    ];
    leaf.prop_recursive(5, 20, 10, |inner| {
        prop_oneof![
            collection::vec(inner.clone(), (2, 20)).prop_map(|t| Type::Set(Arc::from(t))),
            collection::vec(inner.clone(), (2, 20))
                .prop_map(|t| Type::Tuple(Arc::from(t))),
            (typart(), collection::vec(inner.clone(), (0, 20)))
                .prop_map(|(tag, typs)| Type::Variant(tag, Arc::from_iter(typs))),
            collection::vec((field_name(), inner.clone()), (1, 20)).prop_map(|mut t| {
                t.sort_by_key(|(n, _)| n.clone());
                t.dedup_by_key(|(n, _)| n.clone());
                Type::Struct(Arc::from(t))
            }),
            inner.clone().prop_map(|t| Type::Array(Arc::new(t))),
            inner.clone().prop_map(|t| Type::Array(Arc::new(t))),
            inner.clone().prop_map(|t| Type::List(Arc::new(t))),
            inner.clone().prop_map(|t| Type::ByRef(Arc::new(t))),
            (typath(), collection::vec(inner.clone(), (0, 8))).prop_map(
                |(name, params)| {
                    Type::Ref(TypeRef::synthetic(
                        ModPath::root(),
                        name,
                        Arc::from(params),
                    ))
                }
            ),
            (
                collection::vec(
                    (
                        option::of(random_fname()),
                        random_fname(),
                        any::<bool>(),
                        inner.clone(),
                    ),
                    (1, 10),
                ),
                option::of(inner.clone()),
                inner.clone(),
                collection::vec((random_fname(), inner.clone()), (0, 4)),
                option::of(inner.clone())
            )
                .prop_map(|(mut args, vargs, rtype, constraints, throws)| {
                    args.sort_by(|(k0, _, _, _), (k1, _, _, _)| k1.cmp(k0));
                    let args =
                        args.into_iter().map(|(label, pos_name, optional, typ)| {
                            let kind = match label {
                                Some(n) => {
                                    FnArgKind::Labeled { name: n, has_default: optional }
                                }
                                None => FnArgKind::Positional { name: Some(pos_name) },
                            };
                            FnArgType { kind, typ }
                        });
                    let explicit_throws = throws.is_some();
                    let throws = throws.unwrap_or(Type::Bottom);
                    let ft = FnType {
                        args: Arc::from_iter(args),
                        vargs,
                        rtype,
                        throws,
                        explicit_throws,
                        // one quantifier per NAME, like the parser
                        // (`fn<'a: A, 'a: B>` is one variable, two conjuncts)
                        quantifiers: {
                            let mut names: Vec<ArcStr> = Vec::new();
                            for (a, _) in constraints.iter() {
                                if !names.contains(a) {
                                    names.push(a.clone());
                                }
                            }
                            Arc::from_iter(names)
                        },
                        ..Default::default()
                    };
                    // Mirror the parser: quantifier constraints seed
                    // CELLS after aliasing same-named signature
                    // leaves onto the quantifier tvars (phase C — the
                    // cells are the only store). Orphan quantifiers
                    // (names not reachable from the signature) are
                    // invisible to `constraint_view` on BOTH sides of
                    // the round trip, so equality still holds.
                    {
                        let mut known: ahash::AHashMap<ArcStr, TVar> =
                            ahash::AHashMap::default();
                        let pairs: Vec<(TVar, Type)> = constraints
                            .into_iter()
                            .map(|(a, t)| (TVar::empty_named(a), t))
                            .collect();
                        for (tv, _) in pairs.iter() {
                            known.insert(tv.name.clone(), tv.clone());
                        }
                        ft.alias_tvars(&mut known);
                        for (tv, tc) in pairs {
                            // The parser aliases the constraint TYPE's
                            // interior too (typexp.rs fntype builder):
                            // a same-named tvar inside the conjunct IS
                            // the quantifier (one name, one cell per
                            // scope). Without this the generator minted
                            // a distinct interior cell the printed text
                            // can't express, and reparse aliased it —
                            // view mismatch (trip3 at 24k cases).
                            tc.alias_tvars(&mut known);
                            tv.add_cell_constraint(tc);
                        }
                    }
                    Type::Fn(Arc::new(ft))
                })
        ]
    })
}

fn structure_pattern() -> impl Strategy<Value = StructurePattern> {
    let leaf = prop_oneof![
        value().prop_map(|v| StructurePattern::Literal(v)),
        option::of(random_fname()).prop_map(|name| match name {
            None => StructurePattern::Ignore,
            Some(name) => StructurePattern::Bind(name),
        }),
    ];
    leaf.prop_recursive(5, 20, 10, |inner| {
        prop_oneof![
            (option::of(random_fname()), collection::vec(inner.clone(), (0, 10)))
                .prop_map(|(all, b)| {
                    StructurePattern::Slice { list: false, all, binds: Arc::from_iter(b) }
                }),
            (option::of(random_fname()), collection::vec(inner.clone(), (0, 10)))
                .prop_map(|(all, b)| {
                    StructurePattern::Slice { list: true, all, binds: Arc::from_iter(b) }
                }),
            (option::of(random_fname()), collection::vec(inner.clone(), (2, 10)))
                .prop_map(|(all, b)| {
                    StructurePattern::Tuple { all, binds: Arc::from_iter(b) }
                }),
            (
                option::of(random_fname()),
                typart(),
                collection::vec(inner.clone(), (0, 10))
            )
                .prop_map(|(all, tag, b)| {
                    StructurePattern::Variant { all, tag, binds: Arc::from_iter(b) }
                }),
            (option::of(random_fname()), typart(), inner.clone()).prop_map(
                |(all, name, b)| {
                    StructurePattern::Abstract {
                        all,
                        name: ModPath::from([name]),
                        bind: Arc::new(b),
                    }
                }
            ),
            (
                option::of(random_fname()),
                collection::vec((field_name(), inner.clone()), (1, 10)),
                any::<bool>()
            )
                .prop_map(|(all, mut b, exhaustive)| {
                    b.sort_by_key(|(f, _)| f.clone());
                    b.dedup_by_key(|(f, _)| f.clone());
                    StructurePattern::Struct { all, exhaustive, binds: Arc::from_iter(b) }
                }),
            (
                option::of(random_fname()),
                collection::vec(inner.clone(), (1, 10)),
                option::of(random_fname())
            )
                .prop_map(|(all, p, tail)| StructurePattern::SlicePrefix {
                    list: false,
                    all,
                    prefix: Arc::from_iter(p),
                    tail
                }),
            (
                option::of(random_fname()),
                collection::vec(inner.clone(), (1, 10)),
                option::of(random_fname())
            )
                .prop_map(|(all, p, tail)| StructurePattern::SlicePrefix {
                    list: true,
                    all,
                    prefix: Arc::from_iter(p),
                    tail
                }),
            (
                option::of(random_fname()),
                option::of(random_fname()),
                collection::vec(inner.clone(), (1, 10))
            )
                .prop_map(|(all, head, s)| StructurePattern::SliceSuffix {
                    all,
                    head,
                    suffix: Arc::from_iter(s)
                }),
        ]
    })
}

fn pattern() -> impl Strategy<Value = Pattern> {
    (option::of(typexp()), structure_pattern()).prop_map(
        |(type_predicate, structure_predicate)| Pattern {
            type_predicate,
            structure_predicate,
            guard: None,
        },
    )
}

fn build_pattern(arg: Expr, arms: Vec<(Option<Expr>, Pattern, Expr)>) -> Expr {
    let arms = arms.into_iter().map(|(guard, mut pat, expr)| {
        pat.guard = guard;
        (pat, expr)
    });
    ExprKind::Select(SelectExpr { arg: Arc::new(arg), arms: Arc::from_iter(arms) })
        .to_expr_nopos()
}

/// One use item across the whole grammar space: optional keyword lead
/// (`self`/`package`/`super` chain), plain segments, an optional final
/// glob, an optional rename (never on a glob — the parser refuses it).
fn use_item() -> impl Strategy<Value = UseItem> {
    let lead = prop_oneof![
        4 => Just(Vec::new()),
        1 => Just(vec![ArcStr::from("self")]),
        1 => Just(vec![ArcStr::from("package")]),
        1 => (1..3usize).prop_map(|n| vec![ArcStr::from("super"); n]),
    ];
    let glob = prop_oneof![4 => Just(false), 1 => Just(true)];
    // the TERMINAL segment (and a rename target) may be a type name —
    // uppercase; interior segments are modules (lowercase)
    let upper = prop_oneof![
        Just(ArcStr::from("Client")),
        Just(ArcStr::from("T0")),
        Just(ArcStr::from("StyleSheet")),
    ];
    let leaf = prop_oneof![3 => random_fname(), 1 => upper.clone()];
    let rename = prop_oneof![3 => random_fname(), 1 => upper];
    (lead, collection::vec(random_fname(), 0..3), leaf, glob, option::of(rename))
        .prop_map(|(lead, segs, leaf, glob, rename)| {
            let mut parts = lead;
            parts.extend(segs);
            parts.push(leaf);
            if glob {
                parts.push(ArcStr::from("*"));
            }
            UseItem {
                path: ModPath::from_iter(parts),
                rename: if glob { None } else { rename },
            }
        })
}

fn reexport() -> impl Strategy<Value = bool> {
    prop_oneof![7 => Just(false), 1 => Just(true)]
}

fn usestmt() -> impl Strategy<Value = Expr> {
    (reexport(), collection::vec(use_item(), 1..4)).prop_map(|(reexport, names)| {
        ExprKind::Use { reexport, names: Arc::from_iter(names) }.to_expr_nopos()
    })
}

fn typedef() -> impl Strategy<Value = Expr> {
    let body = prop_oneof![
        typexp().prop_map(TypeDefBody::Alias),
        option::of(typexp()).prop_map(TypeDefBody::Abstract),
    ];
    (typart(), collection::vec((tvar(), option::of(typexp())), 0..4), body).prop_map(
        |(name, params, body)| {
            let params = Arc::from_iter(params.into_iter());
            ExprKind::TypeDef(TypeDefExpr { name, params, body }).to_expr_nopos()
        },
    )
}

/// A trait method signature: `fn(self, x: T, ..) -> R` — the receiver
/// first, typed by the `self` variable.
fn trait_method_sig() -> impl Strategy<Value = Arc<FnType>> {
    (collection::vec((random_fname(), typexp()), 0..3), typexp()).prop_map(
        |(args, rtype)| {
            let recv = FnArgType {
                kind: FnArgKind::Positional { name: Some(ArcStr::from("self")) },
                typ: Type::TVar(TVar::empty_named(ArcStr::from("self"))),
            };
            let args = iter::once(recv).chain(args.into_iter().map(|(n, typ)| {
                FnArgType { kind: FnArgKind::Positional { name: Some(n) }, typ }
            }));
            let ft = FnType {
                args: Arc::from_iter(args),
                vargs: None,
                rtype,
                throws: Type::Bottom,
                explicit_throws: false,
                ..Default::default()
            };
            // mirror the parser: same-named leaves of one signature share
            // one cell (see typexp()'s fn-type arm)
            ft.alias_tvars(&mut ahash::AHashMap::default());
            Arc::new(ft)
        },
    )
}

macro_rules! trait_decl {
    ($inner:expr) => {
        (
            typart(),
            collection::vec(
                (
                    random_fname(),
                    trait_method_sig(),
                    option::of($inner),
                    option::of(arcstr()),
                ),
                0..4,
            ),
        )
            .prop_map(|(name, methods)| {
                let mut seen: ahash::AHashSet<ArcStr> = ahash::AHashSet::default();
                let methods = methods
                    .into_iter()
                    .filter(|(n, _, _, _)| seen.insert(n.clone()))
                    .map(|(name, typ, default, doc)| TraitMethod {
                        doc: Doc(doc),
                        name,
                        typ,
                        self_index: 0,
                        default,
                    });
                ExprKind::Trait(Arc::new(TraitExpr {
                    name,
                    methods: Arc::from_iter(methods),
                }))
                .to_expr_nopos()
            })
    };
}

macro_rules! impl_decl {
    ($inner:expr) => {
        (
            collection::vec((tvar(), collection::vec(typexp(), 0..3)), 0..3),
            typath(),
            typexp(),
            collection::vec(
                (random_fname(), option::of(typexp()), $inner, decorations()),
                0..3,
            ),
        )
            .prop_map(|(params, trait_name, target, methods)| {
                let mut seen: ahash::AHashSet<ArcStr> = ahash::AHashSet::default();
                let params: Vec<(TVar, Vec<Type>)> = params
                    .into_iter()
                    .filter(|(tv, _)| seen.insert(tv.name.clone()))
                    .collect();
                let constraints = params
                    .iter()
                    .flat_map(|(tv, bs)| bs.iter().map(move |b| (tv.clone(), b.clone())));
                let mut seen: ahash::AHashSet<ArcStr> = ahash::AHashSet::default();
                let methods = methods
                    .into_iter()
                    .filter(|(n, _, _, _)| seen.insert(n.clone()))
                    .map(|(name, typ, value, dec)| {
                        let mut m = ExprKind::Bind(Arc::new(BindExpr {
                            rec: false,
                            pattern: StructurePattern::Bind(name),
                            typ,
                            value,
                        }))
                        .to_expr_nopos();
                        m.dec = dec;
                        m
                    });
                ExprKind::Impl(Arc::new(ImplExpr {
                    trait_name,
                    constraints: Arc::from_iter(constraints),
                    params: Arc::from_iter(params.iter().map(|(tv, _)| tv.clone())),
                    target,
                    methods: Arc::from_iter(methods),
                }))
                .to_expr_nopos()
            })
    };
}

macro_rules! structref {
    ($inner:expr) => {
        ($inner, field_name()).prop_map(|(source, field)| {
            ExprKind::StructRef { source: Arc::new(source), field }.to_expr_nopos()
        })
    };
}

macro_rules! tupleref {
    ($inner:expr) => {
        ($inner, any::<usize>()).prop_map(|(source, field)| {
            ExprKind::TupleRef { source: Arc::new(source), field }.to_expr_nopos()
        })
    };
}

macro_rules! bind {
    ($inner:expr) => {
        ($inner, any::<bool>(), structure_pattern(), option::of(typexp())).prop_map(
            |(value, rec, p, typ)| {
                ExprKind::Bind(Arc::new(BindExpr { rec, pattern: p, value, typ }))
                    .to_expr_nopos()
            },
        )
    };
}

macro_rules! qop {
    ($inner:expr) => {
        ($inner, any::<bool>()).prop_map(|(e, qop)| match &e.kind {
            ExprKind::Do { .. }
            | ExprKind::Select { .. }
            | ExprKind::TypeCast { .. }
            | ExprKind::Ref { .. }
            | ExprKind::Any { .. }
            | ExprKind::Apply { .. }
            | ExprKind::ArrayRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::StructRef { .. } => {
                if qop {
                    ExprKind::Qop(Arc::new(e)).to_expr_nopos()
                } else {
                    ExprKind::OrNever(Arc::new(e)).to_expr_nopos()
                }
            }
            _ => e,
        })
    };
}

macro_rules! catch_stmt {
    ($inner:expr) => {
        (random_fname(), option::of(typexp()), $inner).prop_map(
            |(bind, constraint, handler)| {
                ExprKind::Catch(Arc::new(CatchExpr {
                    bind,
                    constraint,
                    handler: Arc::new(handler),
                }))
                .to_expr_nopos()
            },
        )
    };
}

macro_rules! arrayslice {
    ($inner:expr) => {
        ($inner, option::of($inner), option::of($inner)).prop_map(
            |(source, start, end)| {
                ExprKind::ArraySlice {
                    source: Arc::new(source),
                    start: start.map(Arc::new),
                    end: end.map(Arc::new),
                }
                .to_expr_nopos()
            },
        )
    };
}

macro_rules! apply {
    ($inner:expr, $concat:literal) => {
        ($inner, collection::vec((option::of(random_fname()), $inner), (0, 10))).prop_map(
            |(f, mut args)| {
                args.sort_unstable_by(|(n0, _), (n1, _)| n1.cmp(n0));
                ExprKind::Apply(ApplyExpr {
                    function: Arc::new(f),
                    args: Arc::from(args),
                })
                .to_expr_nopos()
            },
        )
    };
}

macro_rules! any {
    ($inner:expr) => {
        collection::vec($inner, (0, 10))
            .prop_map(|args| ExprKind::Any { args: Arc::from(args) }.to_expr_nopos())
    };
}

macro_rules! do_block {
    ($inner:expr) => {
        (
            collection::vec(
                decorated(prop_oneof![
                    typedef(),
                    usestmt(),
                    catch_stmt!($inner.clone()),
                    trait_decl!($inner.clone()),
                    impl_decl!($inner.clone()),
                    $inner
                ]),
                (2, 10),
            ),
            any::<bool>(),
        )
            .prop_map(|(e, nop)| {
                if nop {
                    ExprKind::Do {
                        exprs: Arc::from_iter(
                            e.into_iter()
                                .chain(iter::once(ExprKind::NoOp.to_expr_nopos())),
                        ),
                    }
                    .to_expr_nopos()
                } else {
                    ExprKind::Do { exprs: Arc::from(e) }.to_expr_nopos()
                }
            })
    };
}

macro_rules! lambda {
    ($inner:expr) => {
        (
            collection::vec(
                (
                    any::<bool>(),
                    random_fname(),
                    structure_pattern(),
                    option::of(typexp()),
                    option::of($inner),
                ),
                (0, 10),
            ),
            option::of(option::of(typexp())),
            option::of(typexp()),
            collection::vec((random_fname(), typexp()), (0, 4)),
            option::of(typexp()),
            option::of(random_fname()),
            $inner,
        )
            .prop_map(
                |(mut args, vargs, rtype, constraints, throws, builtin, body)| {
                    args.sort_unstable_by(|(k0, _, _, _, _), (k1, _, _, _, _)| {
                        k1.cmp(k0)
                    });
                    let args = args.into_iter().map(
                        |(labeled, name, pattern, constraint, default)| {
                            let pattern = if labeled {
                                StructurePattern::Bind(name)
                            } else {
                                pattern
                            };
                            Arg {
                                labeled: labeled.then_some(default),
                                pattern,
                                constraint,
                                pos: Default::default(),
                            }
                        },
                    );
                    let constraints = Arc::from_iter(
                        constraints.into_iter().map(|(a, t)| (TVar::empty_named(a), t)),
                    );
                    ExprKind::Lambda(Arc::new(LambdaExpr {
                        args: Arc::from_iter(args),
                        vargs,
                        rtype,
                        constraints,
                        throws,
                        body: match builtin {
                            None => Either::Left(body),
                            Some(name) => Either::Right(name),
                        },
                    }))
                    .to_expr_nopos()
                },
            )
    };
}

macro_rules! select {
    ($inner:expr) => {
        (
            $inner,
            collection::vec((option::of($inner), pattern(), decorated($inner)), (1, 10)),
        )
            .prop_map(|(arg, arms)| build_pattern(arg, arms))
    };
}

macro_rules! structure {
    ($inner:expr) => {
        collection::vec((field_name(), decorated($inner)), (1, 10)).prop_map(|mut a| {
            a.sort_by_key(|(n, _)| n.clone());
            a.dedup_by_key(|(n, _)| n.clone());
            ExprKind::Struct(StructExpr { args: Arc::from_iter(a) }).to_expr_nopos()
        })
    };
}

macro_rules! variant {
    ($inner:expr) => {
        (typart(), collection::vec($inner, (0, 10))).prop_map(|(tag, a)| {
            ExprKind::Variant { tag, args: Arc::from_iter(a) }.to_expr_nopos()
        })
    };
}

macro_rules! construct {
    ($inner:expr) => {
        (typart(), $inner).prop_map(|(name, a)| {
            ExprKind::Construct { name: ModPath::from([name]), arg: Arc::new(a) }
                .to_expr_nopos()
        })
    };
}

macro_rules! connect {
    ($inner:expr) => {
        ($inner, any::<bool>(), modpath()).prop_map(|(e, deref, n)| {
            ExprKind::Connect { name: n, value: Arc::new(e), deref }.to_expr_nopos()
        })
    };
}

macro_rules! arrayref {
    ($inner:expr) => {
        ($inner, $inner).prop_map(|(source, i)| {
            ExprKind::ArrayRef { source: Arc::new(source), i: Arc::new(i) }
                .to_expr_nopos()
        })
    };
}

macro_rules! mapref {
    ($inner:expr) => {
        ($inner, $inner).prop_map(|(source, key)| {
            ExprKind::MapRef { source: Arc::new(source), key: Arc::new(key) }
                .to_expr_nopos()
        })
    };
}

macro_rules! typecast {
    ($inner:expr) => {
        ($inner, typexp()).prop_map(|(expr, typ)| {
            ExprKind::TypeCast { expr: Arc::new(expr), typ }.to_expr_nopos()
        })
    };
}

macro_rules! array {
    ($inner:expr) => {
        collection::vec($inner, (0, 10))
            .prop_map(|a| { ExprKind::Array { args: Arc::from_iter(a) } }.to_expr_nopos())
    };
}

macro_rules! list_lit {
    ($inner:expr) => {
        collection::vec($inner, (0, 10))
            .prop_map(|a| { ExprKind::List { args: Arc::from_iter(a) } }.to_expr_nopos())
    };
}

macro_rules! map {
    ($inner:expr) => {
        collection::vec(($inner, $inner), (0, 10))
            .prop_map(|a| { ExprKind::Map { args: Arc::from_iter(a) } }.to_expr_nopos())
    };
}

macro_rules! tuple {
    ($inner:expr) => {
        collection::vec($inner, (2, 10))
            .prop_map(|a| { ExprKind::Tuple { args: Arc::from_iter(a) } }.to_expr_nopos())
    };
}

macro_rules! binop {
    ($inner:expr, $op:ident) => {
        ($inner, $inner).prop_map(|(e0, e1)| {
            ExprKind::$op { lhs: Arc::new(e0), rhs: Arc::new(e1) }.to_expr_nopos()
        })
    };
}

macro_rules! structwith {
    ($inner:expr) => {
        ($inner, collection::vec((field_name(), decorated($inner)), (1, 10))).prop_map(
            |(source, mut replace)| {
                let source = Arc::new(source);
                replace.sort_by_key(|(f, _)| f.clone());
                replace.dedup_by_key(|(f, _)| f.clone());
                ExprKind::StructWith(StructWithExpr {
                    source,
                    replace: Arc::from_iter(replace),
                })
                .to_expr_nopos()
            },
        )
    };
}

macro_rules! byref {
    ($inner:expr) => {
        $inner
            .prop_map(|e| ExprKind::ByRef(Arc::new(e)).to_expr_nopos())
            .prop_map(add_parens)
    };
}

macro_rules! neg {
    ($inner:expr) => {
        $inner
            .prop_map(|e| ExprKind::Neg(Arc::new(e)).to_expr_nopos())
            .prop_map(add_parens)
    };
}

macro_rules! deref {
    ($inner:expr) => {
        $inner
            .prop_map(|e| ExprKind::Deref(Arc::new(e)).to_expr_nopos())
            .prop_map(add_parens)
    };
}

fn module_sigitem() -> impl Strategy<Value = SigItem> {
    prop_oneof![
        (random_fname(), typexp(), option::of(arcstr())).prop_map(|(name, typ, doc)| {
            SigItem {
                kind: SigKind::Bind(BindSig { name, typ }),
                doc: Doc(doc),
                pos: Default::default(),
                ori: None,
            }
        }),
        (typedef(), option::of(arcstr())).prop_map(
            |(mut td, doc)| match std::mem::replace(&mut td.kind, ExprKind::NoOp,) {
                ExprKind::TypeDef(td) => SigItem {
                    kind: SigKind::TypeDef(td),
                    doc: Doc(doc),
                    pos: Default::default(),
                    ori: None,
                },
                _ => unreachable!(),
            }
        ),
        (reexport(), collection::vec(use_item(), 1..4), option::of(arcstr())).prop_map(
            |(reexport, paths, doc)| SigItem {
                kind: SigKind::Use { reexport, names: Arc::from_iter(paths) },
                doc: Doc(doc),
                pos: Default::default(),
                ori: None,
            }
        ),
        (random_fname(), option::of(arcstr())).prop_map(|(name, doc)| SigItem {
            kind: SigKind::Module(name),
            doc: Doc(doc),
            pos: Default::default(),
            ori: None,
        }),
        (trait_decl!(constant()), option::of(arcstr())).prop_map(|(mut t, doc)| {
            match std::mem::replace(&mut t.kind, ExprKind::NoOp) {
                ExprKind::Trait(t) => SigItem {
                    kind: SigKind::Trait(t),
                    doc: Doc(doc),
                    pos: Default::default(),
                    ori: None,
                },
                _ => unreachable!(),
            }
        }),
        (impl_decl!(constant()), option::of(arcstr())).prop_map(|(mut i, doc)| {
            match std::mem::replace(&mut i.kind, ExprKind::NoOp) {
                ExprKind::Impl(i) => SigItem {
                    kind: SigKind::Impl(Arc::new(ImplExpr {
                        methods: Arc::from_iter([]),
                        ..(*i).clone()
                    })),
                    doc: Doc(doc),
                    pos: Default::default(),
                    ori: None,
                },
                _ => unreachable!(),
            }
        })
    ]
}

fn check_trait(t0: &TraitExpr, t1: &TraitExpr) -> bool {
    dbg!(t0.name == t1.name)
        && dbg!(t0.methods.len() == t1.methods.len())
        && t0.methods.iter().zip(t1.methods.iter()).all(|(m0, m1)| {
            dbg!(m0.name == m1.name)
                && dbg!(m0.doc == m1.doc)
                && dbg!(m0.self_index == m1.self_index)
                && dbg!(check_type(&Type::Fn(m0.typ.clone()), &Type::Fn(m1.typ.clone())))
                && match (&m0.default, &m1.default) {
                    (Some(d0), Some(d1)) => dbg!(check(d0, d1)),
                    (None, None) => true,
                    _ => false,
                }
        })
}

fn check_impl(i0: &ImplExpr, i1: &ImplExpr) -> bool {
    dbg!(i0.trait_name == i1.trait_name)
        && dbg!(i0.params.len() == i1.params.len())
        && i0.params.iter().zip(i1.params.iter()).all(|(a, b)| a.name == b.name)
        && dbg!(i0.constraints.len() == i1.constraints.len())
        && i0
            .constraints
            .iter()
            .zip(i1.constraints.iter())
            .all(|((a, ta), (b, tb))| a.name == b.name && check_type(ta, tb))
        && dbg!(check_type(&i0.target, &i1.target))
        && dbg!(i0.methods.len() == i1.methods.len())
        && i0.methods.iter().zip(i1.methods.iter()).all(|(a, b)| check(a, b))
}

fn module_sandbox() -> impl Strategy<Value = Sandbox> {
    prop_oneof![
        Just(Sandbox::Unrestricted),
        (collection::vec(modpath(), (1, 10)))
            .prop_map(|l| Sandbox::Blacklist(Arc::from(l))),
        (collection::vec(modpath(), (1, 10)))
            .prop_map(|l| Sandbox::Whitelist(Arc::from(l)))
    ]
}

macro_rules! dynamic_module {
    ($inner:expr) => {
        (
            random_fname(),
            module_sandbox(),
            collection::vec(module_sigitem(), (1, 10)),
            $inner,
        )
            .prop_map(|(name, sandbox, sig, source)| {
                ExprKind::Module {
                    name,
                    value: ModuleKind::Dynamic {
                        sandbox,
                        sig: Sig { items: Arc::from(sig), toplevel: false },
                        source: Arc::new(source),
                    },
                }
                .to_expr_nopos()
            })
    };
}

fn module() -> impl Strategy<Value = Expr> {
    (random_fname()).prop_map(|name| {
        ExprKind::Module { name, value: ModuleKind::Unresolved { from_interface: false } }
            .to_expr_nopos()
    })
}

/// Returns the precedence of an expression if it's a binary operator.
/// Higher values bind tighter. Returns None for non-binary-op expressions.
fn binop_precedence(e: &ExprKind) -> Option<u8> {
    use parser::arithexp::precedence;
    let op = match e {
        ExprKind::Or { .. } => "||",
        ExprKind::And { .. } => "&&",
        ExprKind::Eq { .. } => "==",
        ExprKind::Ne { .. } => "!=",
        ExprKind::Lt { .. } => "<",
        ExprKind::Gt { .. } => ">",
        ExprKind::Lte { .. } => "<=",
        ExprKind::Gte { .. } => ">=",
        ExprKind::Add { .. } => "+",
        ExprKind::CheckedAdd { .. } => "+?",
        ExprKind::Sub { .. } => "-",
        ExprKind::CheckedSub { .. } => "-?",
        ExprKind::Mul { .. } => "*",
        ExprKind::CheckedMul { .. } => "*?",
        ExprKind::Div { .. } => "/",
        ExprKind::CheckedDiv { .. } => "/?",
        ExprKind::Mod { .. } => "%",
        ExprKind::CheckedMod { .. } => "%?",
        ExprKind::Sample { .. } => "~",
        _ => return None,
    };
    Some(precedence(op).0)
}

/// Prefix-unary operators (`!`, `*`, `&`, `-`) bind tighter than every binary
/// operator; this is the `parent_prec` they pass to `maybe_paren_lhs`.
const UNARY_PREC: u8 = 255;

fn paren(child: Expr) -> Expr {
    ExprKind::ExplicitParens(Arc::new(child)).to_expr_nopos()
}

/// Some children need parens regardless of left/right position. `Connect`
/// (`name <- value`) binds looser than every operator, so it always needs them
/// as an operand. Postfix `Qop` (`e?`) re-binds onto a prefix-unary parent's
/// result (`*x?` is `(*x)?`, not `*(x?)`), so it needs them under a prefix
/// unary only — under a binary operator it re-parses correctly without them.
/// Returns `None` to defer to ordinary binary-operator precedence.
fn loose_needs_parens(child: &ExprKind, parent_prec: u8) -> Option<bool> {
    match child {
        ExprKind::Connect { .. } => Some(true),
        ExprKind::Qop(_) => Some(parent_prec == UNARY_PREC),
        _ => None,
    }
}

/// Wraps a left child in ExplicitParens if it has lower precedence than the parent.
fn maybe_paren_lhs(child: Expr, parent_prec: u8) -> Expr {
    let needs = loose_needs_parens(&child.kind, parent_prec).unwrap_or_else(|| {
        binop_precedence(&child.kind).is_some_and(|p| p < parent_prec)
    });
    if needs { paren(child) } else { child }
}

/// Wraps a right child in ExplicitParens if it has lower or equal precedence than the parent.
/// Equal precedence needs parens on the right because all operators are left-associative:
/// `a - b - c` parses as `(a - b) - c`, so `Sub(a, Sub(b, c))` must print as `a - (b - c)`.
fn maybe_paren_rhs(child: Expr, parent_prec: u8) -> Expr {
    let needs = loose_needs_parens(&child.kind, parent_prec).unwrap_or_else(|| {
        binop_precedence(&child.kind).is_some_and(|p| p <= parent_prec)
    });
    if needs { paren(child) } else { child }
}

/// Recursively adds ExplicitParens where needed to make the expression tree
/// consistent with precedence rules. This ensures the round-trip test works
/// for randomly generated expressions.
fn add_parens(mut e: Expr) -> Expr {
    use parser::arithexp::precedence;
    macro_rules! fix_binop {
        ($op:literal, $ctor:ident, $lhs:expr, $rhs:expr) => {{
            let prec = precedence($op).0;
            let lhs =
                Arc::new(maybe_paren_lhs(add_parens(Arc::unwrap_or_clone($lhs)), prec));
            let rhs =
                Arc::new(maybe_paren_rhs(add_parens(Arc::unwrap_or_clone($rhs)), prec));
            ExprKind::$ctor { lhs, rhs }
        }};
    }
    let kind = match std::mem::replace(&mut e.kind, ExprKind::NoOp) {
        ExprKind::Or { lhs, rhs } => fix_binop!("||", Or, lhs, rhs),
        ExprKind::And { lhs, rhs } => fix_binop!("&&", And, lhs, rhs),
        ExprKind::Eq { lhs, rhs } => fix_binop!("==", Eq, lhs, rhs),
        ExprKind::Ne { lhs, rhs } => fix_binop!("!=", Ne, lhs, rhs),
        ExprKind::Lt { lhs, rhs } => fix_binop!("<", Lt, lhs, rhs),
        ExprKind::Gt { lhs, rhs } => fix_binop!(">", Gt, lhs, rhs),
        ExprKind::Lte { lhs, rhs } => fix_binop!("<=", Lte, lhs, rhs),
        ExprKind::Gte { lhs, rhs } => fix_binop!(">=", Gte, lhs, rhs),
        ExprKind::Add { lhs, rhs } => fix_binop!("+", Add, lhs, rhs),
        ExprKind::CheckedAdd { lhs, rhs } => fix_binop!("+?", CheckedAdd, lhs, rhs),
        ExprKind::Sub { lhs, rhs } => fix_binop!("-", Sub, lhs, rhs),
        ExprKind::CheckedSub { lhs, rhs } => fix_binop!("-?", CheckedSub, lhs, rhs),
        ExprKind::Mul { lhs, rhs } => fix_binop!("*", Mul, lhs, rhs),
        ExprKind::CheckedMul { lhs, rhs } => fix_binop!("*?", CheckedMul, lhs, rhs),
        ExprKind::Div { lhs, rhs } => fix_binop!("/", Div, lhs, rhs),
        ExprKind::CheckedDiv { lhs, rhs } => fix_binop!("/?", CheckedDiv, lhs, rhs),
        ExprKind::Mod { lhs, rhs } => fix_binop!("%", Mod, lhs, rhs),
        ExprKind::CheckedMod { lhs, rhs } => fix_binop!("%?", CheckedMod, lhs, rhs),
        ExprKind::Sample { lhs, rhs } => fix_binop!("~", Sample, lhs, rhs),
        ExprKind::Not { expr } => ExprKind::Not {
            expr: Arc::new(maybe_paren_lhs(Arc::unwrap_or_clone(expr), 255)),
        },
        ExprKind::Deref(e) => {
            ExprKind::Deref(Arc::new(maybe_paren_lhs(Arc::unwrap_or_clone(e), 255)))
        }
        ExprKind::ByRef(e) => {
            ExprKind::ByRef(Arc::new(maybe_paren_lhs(Arc::unwrap_or_clone(e), 255)))
        }
        ExprKind::Neg(e) => {
            let inner = Arc::unwrap_or_clone(e);
            match &inner.kind {
                // `-5` re-parses as the literal Constant(-5), not Neg(5), so
                // a constant operand must be parenthesized to stay a Neg.
                ExprKind::Constant(_) => ExprKind::Neg(Arc::new(
                    ExprKind::ExplicitParens(Arc::new(inner)).to_expr_nopos(),
                )),
                _ => ExprKind::Neg(Arc::new(maybe_paren_lhs(inner, 255))),
            }
        }
        // For non-binop expressions, just return as-is
        other => other,
    };
    Expr { kind, id: e.id, ori: e.ori.clone(), pos: e.pos, dec: e.dec.take() }
}

fn arithexpr() -> impl Strategy<Value = Expr> {
    let leaf = prop_oneof![constant(), reference()];
    leaf.prop_recursive(5, 20, 10, |inner| {
        prop_oneof![
            select!(inner.clone().prop_map(add_parens)),
            do_block!(inner.clone().prop_map(add_parens)),
            any!(inner.clone().prop_map(add_parens)),
            apply!(inner.clone().prop_map(add_parens), false),
            typecast!(inner.clone().prop_map(add_parens)),
            arrayref!(inner.clone().prop_map(add_parens)),
            arrayslice!(inner.clone().prop_map(add_parens)),
            structref!(inner.clone().prop_map(add_parens)),
            tupleref!(inner.clone().prop_map(add_parens)),
            mapref!(inner.clone().prop_map(add_parens)),
            tuple!(inner.clone().prop_map(add_parens)),
            structure!(inner.clone().prop_map(add_parens)),
            structwith!(inner.clone().prop_map(add_parens)),
            variant!(inner.clone().prop_map(add_parens)),
            construct!(inner.clone().prop_map(add_parens)),
            byref!(inner.clone().prop_map(add_parens)),
            deref!(inner.clone().prop_map(add_parens)),
            neg!(inner.clone().prop_map(add_parens)),
            binop!(inner.clone().prop_map(add_parens), Eq),
            binop!(inner.clone().prop_map(add_parens), Ne),
            binop!(inner.clone().prop_map(add_parens), Lt),
            binop!(inner.clone().prop_map(add_parens), Gt),
            binop!(inner.clone().prop_map(add_parens), Gte),
            binop!(inner.clone().prop_map(add_parens), Lte),
            binop!(inner.clone().prop_map(add_parens), And),
            binop!(inner.clone().prop_map(add_parens), Or),
            inner
                .clone()
                .prop_map(add_parens)
                .prop_map(|e0| ExprKind::Not { expr: Arc::new(e0) }.to_expr_nopos()),
            binop!(inner.clone().prop_map(add_parens), Add),
            binop!(inner.clone().prop_map(add_parens), CheckedAdd),
            binop!(inner.clone().prop_map(add_parens), Sub),
            binop!(inner.clone().prop_map(add_parens), CheckedSub),
            binop!(inner.clone().prop_map(add_parens), Mul),
            binop!(inner.clone().prop_map(add_parens), CheckedMul),
            binop!(inner.clone().prop_map(add_parens), Div),
            binop!(inner.clone().prop_map(add_parens), CheckedDiv),
            binop!(inner.clone().prop_map(add_parens), Mod),
            binop!(inner.clone().prop_map(add_parens), CheckedMod),
            binop!(inner.clone().prop_map(add_parens), Sample)
        ]
    })
    .prop_map(add_parens)
}

fn expr() -> impl Strategy<Value = Expr> {
    decorated(undecorated_expr())
}

fn undecorated_expr() -> impl Strategy<Value = Expr> {
    let leaf = prop_oneof![
        constant(),
        reference(),
        usestmt(),
        typedef(),
        module(),
        trait_decl!(constant()),
        impl_decl!(constant())
    ];
    leaf.prop_recursive(5, 100, 25, |inner| {
        prop_oneof![
            dynamic_module!(inner.clone()),
            arrayref!(inner.clone()),
            arrayslice!(inner.clone()),
            mapref!(inner.clone()),
            qop!(inner.clone()),
            arithexpr(),
            byref!(inner.clone()),
            deref!(inner.clone()),
            structref!(inner.clone()),
            tupleref!(inner.clone()),
            any!(inner.clone()),
            apply!(inner.clone(), false),
            typecast!(inner.clone()),
            do_block!(inner.clone()),
            lambda!(inner.clone()),
            bind!(inner.clone()),
            connect!(inner.clone()),
            select!(inner.clone()),
            array!(inner.clone()),
            list_lit!(inner.clone()),
            map!(inner.clone()),
            tuple!(inner.clone()),
            variant!(inner.clone()),
            construct!(inner.clone()),
            structure!(inner.clone()),
            structwith!(inner.clone()),
        ]
    })
}

fn acc_strings<'a>(args: impl IntoIterator<Item = &'a Expr> + 'a) -> Arc<[Expr]> {
    let mut v: LPooled<Vec<Expr>> = LPooled::take();
    for s in args {
        let s = s.clone();
        match s.kind {
            ExprKind::Constant(Value::String(ref c1)) => match v.last_mut() {
                None => v.push(s),
                Some(e0) => match &mut e0.kind {
                    ExprKind::Constant(Value::String(c0))
                        if c1.len() > 0 && c0.len() > 0 =>
                    {
                        let mut st = String::new();
                        st.push_str(&*c0);
                        st.push_str(&*c1);
                        *c0 = ArcStr::from(st);
                    }
                    _ => v.push(s),
                },
            },
            _ => v.push(s),
        }
    }
    Arc::from_iter(v.drain(..))
}

fn check_type(t0: &Type, t1: &Type) -> bool {
    dbg!(dbg!(&t0).normalize()) == dbg!(dbg!(&t1).normalize())
}

fn check_type_opt(t0: &Option<Type>, t1: &Option<Type>) -> bool {
    match (t0, t1) {
        (Some(t0), Some(t1)) => check_type(&t0, &t1),
        (None, None) => true,
        (_, _) => false,
    }
}

fn check_structure_pattern(pat0: &StructurePattern, pat1: &StructurePattern) -> bool {
    match (pat0, pat1) {
        (
            StructurePattern::Literal(Value::Array(a)),
            StructurePattern::Slice { list: false, all: None, binds },
        )
        | (
            StructurePattern::Slice { list: false, all: None, binds },
            StructurePattern::Literal(Value::Array(a)),
        ) => {
            binds.iter().all(|n| match n {
                StructurePattern::Literal(_) => true,
                _ => false,
            }) && {
                let binds = binds
                    .iter()
                    .filter_map(|n| match n {
                        StructurePattern::Literal(l) => Some(l),
                        _ => None,
                    })
                    .collect::<SmallVec<[&Value; 16]>>();
                binds.len() == a.len()
                    && binds.iter().zip(a.iter()).all(|(v0, v1)| *v0 == v1)
            }
        }
        (StructurePattern::Bind(n0), StructurePattern::Bind(n1)) => n0 == n1,
        (StructurePattern::Ignore, StructurePattern::Ignore) => true,
        (StructurePattern::Literal(v0), StructurePattern::Literal(v1)) => {
            v0.approx_eq(v1)
        }
        (
            StructurePattern::Slice { list: l0, all: a0, binds: p0 },
            StructurePattern::Slice { list: l1, all: a1, binds: p1 },
        ) => {
            l0 == l1
                && a0 == a1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .all(|(p0, p1)| check_structure_pattern(p0, p1))
        }
        (
            StructurePattern::SlicePrefix { list: l0, all: a0, prefix: p0, tail: t0 },
            StructurePattern::SlicePrefix { list: l1, all: a1, prefix: p1, tail: t1 },
        ) => {
            l0 == l1
                && a0 == a1
                && t0 == t1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .all(|(p0, p1)| check_structure_pattern(p0, p1))
        }
        (
            StructurePattern::SliceSuffix { all: a0, head: h0, suffix: p0 },
            StructurePattern::SliceSuffix { all: a1, head: h1, suffix: p1 },
        ) => {
            a0 == a1
                && h0 == h1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .all(|(p0, p1)| check_structure_pattern(p0, p1))
        }
        (
            StructurePattern::Tuple { all: a0, binds: p0 },
            StructurePattern::Tuple { all: a1, binds: p1 },
        ) => {
            a0 == a1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .all(|(p0, p1)| check_structure_pattern(p0, p1))
        }
        (
            StructurePattern::Abstract { all: a0, name: n0, bind: b0 },
            StructurePattern::Abstract { all: a1, name: n1, bind: b1 },
        ) => a0 == a1 && n0 == n1 && check_structure_pattern(b0, b1),
        (
            StructurePattern::Variant { all: a0, tag: t0, binds: p0 },
            StructurePattern::Variant { all: a1, tag: t1, binds: p1 },
        ) => {
            a0 == a1
                && t0 == t1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .all(|(p0, p1)| check_structure_pattern(p0, p1))
        }
        (
            StructurePattern::Struct { exhaustive: e0, all: a0, binds: p0 },
            StructurePattern::Struct { exhaustive: e1, all: a1, binds: p1 },
        ) => {
            e0 == e1
                && a0 == a1
                && p0.len() == p1.len()
                && p0.iter().zip(p1.iter()).all(|((f0, p0), (f1, p1))| {
                    f0 == f1 && check_structure_pattern(p0, p1)
                })
        }
        (_, _) => false,
    }
}

fn check_pattern(pat0: &Pattern, pat1: &Pattern) -> bool {
    dbg!(check_type_opt(&pat0.type_predicate, &pat1.type_predicate))
        && check_structure_pattern(&pat0.structure_predicate, &pat1.structure_predicate)
        && dbg!(match (&pat0.guard, &pat1.guard) {
            (Some(g0), Some(g1)) => check(g0, g1),
            (None, None) => true,
            (_, _) => false,
        })
}

fn check_args(args0: &[Arg], args1: &[Arg]) -> bool {
    args0.iter().zip(args1.iter()).fold(true, |r, (a0, a1)| {
        r && dbg!(check_structure_pattern(&a0.pattern, &a1.pattern))
            && dbg!(check_type_opt(&a0.constraint, &a1.constraint))
            && dbg!(match (&a0.labeled, &a1.labeled) {
                (None, None) | (Some(None), Some(None)) => true,
                (Some(Some(d0)), Some(Some(d1))) => check(d0, d1),
                (_, _) => false,
            })
    })
}

fn check_opt(s0: &Option<Arc<Expr>>, s1: &Option<Arc<Expr>>) -> bool {
    match (s0, s1) {
        (None, None) => true,
        (Some(_), None) | (None, Some(_)) => false,
        (Some(e0), Some(e1)) => check(e0, e1),
    }
}

fn check_typedef(td0: &TypeDefExpr, td1: &TypeDefExpr) -> bool {
    let TypeDefExpr { name: name0, params: p0, body: body0 } = td0;
    let TypeDefExpr { name: name1, params: p1, body: body1 } = td1;
    dbg!(name0 == name1)
        && dbg!(
            p0.len() == p1.len()
                && p0.iter().zip(p1.iter()).all(|((t0, c0), (t1, c1))| {
                    t0 == t1
                        && match (c0.as_ref(), c1.as_ref()) {
                            (Some(c0), Some(c1)) => check_type(c0, c1),
                            (None, None) => true,
                            _ => false,
                        }
                })
        )
        && dbg!(match (body0, body1) {
            (TypeDefBody::Alias(t0), TypeDefBody::Alias(t1)) => check_type(t0, t1),
            (TypeDefBody::Abstract(Some(t0)), TypeDefBody::Abstract(Some(t1))) => {
                check_type(t0, t1)
            }
            (TypeDefBody::Abstract(None), TypeDefBody::Abstract(None)) => true,
            _ => false,
        })
}

fn check_module_sig(s0: &[SigItem], s1: &[SigItem]) -> bool {
    s0.len() == s1.len()
        && s0.iter().zip(s1.iter()).all(|(s0, s1)| match (s0, s1) {
            (
                SigItem {
                    kind: SigKind::Bind(BindSig { name: n0, typ: t0 }),
                    doc: d0,
                    ..
                },
                SigItem {
                    kind: SigKind::Bind(BindSig { name: n1, typ: t1 }),
                    doc: d1,
                    ..
                },
            ) => n0 == n1 && check_type(t0, t1) && d0 == d1,
            (
                SigItem { kind: SigKind::TypeDef(td0), doc: d0, .. },
                SigItem { kind: SigKind::TypeDef(td1), doc: d1, .. },
            ) => check_typedef(td0, td1) && d0 == d1,
            (
                SigItem {
                    kind: SigKind::Use { reexport: r0, names: path0 },
                    doc: d0,
                    ..
                },
                SigItem {
                    kind: SigKind::Use { reexport: r1, names: path1 },
                    doc: d1,
                    ..
                },
            ) => r0 == r1 && path0 == path1 && d0 == d1,
            (
                SigItem { kind: SigKind::Module(n0), doc: d0, .. },
                SigItem { kind: SigKind::Module(n1), doc: d1, .. },
            ) => n0 == n1 && d0 == d1,
            (
                SigItem { kind: SigKind::Trait(t0), doc: d0, .. },
                SigItem { kind: SigKind::Trait(t1), doc: d1, .. },
            ) => check_trait(t0, t1) && d0 == d1,
            (
                SigItem { kind: SigKind::Impl(i0), doc: d0, .. },
                SigItem { kind: SigKind::Impl(i1), doc: d1, .. },
            ) => check_impl(i0, i1) && d0 == d1,
            (_, _) => false,
        })
}

fn check_dec(d0: &Option<Box<Decorations>>, d1: &Option<Box<Decorations>>) -> bool {
    match (d0, d1) {
        (None, None) => true,
        (Some(d0), Some(d1)) => {
            dbg!(d0.comments == d1.comments)
                && dbg!(d0.attrs.len() == d1.attrs.len())
                && d0.attrs.iter().zip(d1.attrs.iter()).all(|(a0, a1)| {
                    a0.name == a1.name
                        && a0.args.len() == a1.args.len()
                        && a0
                            .args
                            .iter()
                            .zip(a1.args.iter())
                            .all(|(e0, e1)| check(e0, e1))
                })
        }
        _ => dbg!(false),
    }
}

fn check(s0: &Expr, s1: &Expr) -> bool {
    if !check_dec(&s0.dec, &s1.dec) {
        return false;
    }
    match (&s0.kind, &s1.kind) {
        (ExprKind::ExplicitParens(e0), ExprKind::ExplicitParens(e1)) => check(e0, e1),
        (ExprKind::Constant(v0), ExprKind::Constant(v1)) => v0.approx_eq(v1),
        (ExprKind::Array { args: a0 }, ExprKind::Array { args: a1 })
        | (ExprKind::List { args: a0 }, ExprKind::List { args: a1 })
        | (ExprKind::Tuple { args: a0 }, ExprKind::Tuple { args: a1 }) => {
            a0.len() == a1.len() && a0.iter().zip(a1.iter()).all(|(e0, e1)| check(e0, e1))
        }
        (
            ExprKind::Construct { name: n0, arg: a0 },
            ExprKind::Construct { name: n1, arg: a1 },
        ) => n0 == n1 && check(a0, a1),
        (
            ExprKind::Variant { tag: t0, args: a0 },
            ExprKind::Variant { tag: t1, args: a1 },
        ) => {
            t0 == t1
                && a0.len() == a1.len()
                && a0.iter().zip(a1.iter()).all(|(e0, e1)| check(e0, e1))
        }
        (ExprKind::Map { args: a0 }, ExprKind::Map { args: a1 }) => {
            a0.len() == a1.len()
                && a0
                    .iter()
                    .zip(a1.iter())
                    .all(|((k0, v0), (k1, v1))| check(k0, k1) && check(v0, v1))
        }
        (
            ExprKind::MapRef { source: s0, key: k0 },
            ExprKind::MapRef { source: s1, key: k1 },
        ) => check(s0, s1) && check(k0, k1),
        (
            ExprKind::Struct(StructExpr { args: a0 }),
            ExprKind::Struct(StructExpr { args: a1 }),
        ) => {
            a0.len() == a1.len()
                && a0
                    .iter()
                    .zip(a1.iter())
                    .all(|((n0, e0), (n1, e1))| n0 == n1 && check(e0, e1))
        }
        (
            ExprKind::StructWith(StructWithExpr { source: s0, replace: r0 }),
            ExprKind::StructWith(StructWithExpr { source: s1, replace: r1 }),
        ) => {
            check(s0, s1)
                && r0.len() == r1.len()
                && r0
                    .iter()
                    .zip(r1.iter())
                    .all(|((n0, e0), (n1, e1))| n0 == n1 && check(e0, e1))
        }
        (
            ExprKind::ArrayRef { source: s0, i: i0 },
            ExprKind::ArrayRef { source: s1, i: i1 },
        ) => check(s0, s1) && check(i0, i1),
        (
            ExprKind::ArraySlice { source: s0, start: st0, end: e0 },
            ExprKind::ArraySlice { source: s1, start: st1, end: e1 },
        ) => check(s0, s1) && check_opt(st0, st1) && check_opt(e0, e1),
        (
            ExprKind::TupleRef { source: s0, field: f0 },
            ExprKind::TupleRef { source: s1, field: f1 },
        ) => check(s0, s1) && f0 == f1,
        (
            ExprKind::StructRef { source: s0, field: f0 },
            ExprKind::StructRef { source: s1, field: f1 },
        ) => check(s0, s1) && f0 == f1,
        (
            ExprKind::StringInterpolate { args: a0 },
            ExprKind::Constant(Value::String(c1)),
        ) => match &acc_strings(a0.iter())[..] {
            [Expr { kind: ExprKind::Constant(Value::String(c0)), .. }] => {
                dbg!(c0 == c1)
            }
            _ => false,
        },
        (
            ExprKind::StringInterpolate { args: a0 },
            ExprKind::StringInterpolate { args: a1 },
        ) => {
            let srs0 = acc_strings(a0.iter());
            let srs1 = acc_strings(a1.iter());
            dbg!(
                srs0.iter().zip(srs1.iter()).fold(true, |r, (s0, s1)| r && check(s0, s1))
            )
        }
        (
            ExprKind::Apply(ApplyExpr { args: srs0, function: f0 }),
            ExprKind::Apply(ApplyExpr { args: srs1, function: f1 }),
        ) if check(f0, f1) && srs0.len() == srs1.len() => {
            dbg!(
                srs0.iter()
                    .zip(srs1.iter())
                    .fold(true, |r, ((n0, s0), (n1, s1))| r && n0 == n1 && check(s0, s1))
            )
        }
        (
            ExprKind::Add { lhs: lhs0, rhs: rhs0 },
            ExprKind::Add { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::CheckedAdd { lhs: lhs0, rhs: rhs0 },
            ExprKind::CheckedAdd { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Sub { lhs: lhs0, rhs: rhs0 },
            ExprKind::Sub { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::CheckedSub { lhs: lhs0, rhs: rhs0 },
            ExprKind::CheckedSub { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Mul { lhs: lhs0, rhs: rhs0 },
            ExprKind::Mul { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::CheckedMul { lhs: lhs0, rhs: rhs0 },
            ExprKind::CheckedMul { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Div { lhs: lhs0, rhs: rhs0 },
            ExprKind::Div { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::CheckedDiv { lhs: lhs0, rhs: rhs0 },
            ExprKind::CheckedDiv { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Mod { lhs: lhs0, rhs: rhs0 },
            ExprKind::Mod { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::CheckedMod { lhs: lhs0, rhs: rhs0 },
            ExprKind::CheckedMod { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Eq { lhs: lhs0, rhs: rhs0 },
            ExprKind::Eq { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Ne { lhs: lhs0, rhs: rhs0 },
            ExprKind::Ne { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Lt { lhs: lhs0, rhs: rhs0 },
            ExprKind::Lt { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Gt { lhs: lhs0, rhs: rhs0 },
            ExprKind::Gt { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Lte { lhs: lhs0, rhs: rhs0 },
            ExprKind::Lte { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Gte { lhs: lhs0, rhs: rhs0 },
            ExprKind::Gte { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::And { lhs: lhs0, rhs: rhs0 },
            ExprKind::And { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (
            ExprKind::Or { lhs: lhs0, rhs: rhs0 },
            ExprKind::Or { lhs: lhs1, rhs: rhs1 },
        ) => dbg!(dbg!(check(lhs0, lhs1)) && dbg!(check(rhs0, rhs1))),
        (ExprKind::Not { expr: expr0 }, ExprKind::Not { expr: expr1 }) => {
            dbg!(check(expr0, expr1))
        }
        (
            ExprKind::Module {
                name: name0,
                value: ModuleKind::Unresolved { from_interface: fi0 },
            },
            ExprKind::Module {
                name: name1,
                value: ModuleKind::Unresolved { from_interface: fi1 },
            },
        ) => dbg!(name0 == name1) && fi0 == fi1,
        (
            ExprKind::Module {
                name: name0,
                value: ModuleKind::Dynamic { sandbox: sb0, sig: si0, source: sr0 },
            },
            ExprKind::Module {
                name: name1,
                value: ModuleKind::Dynamic { sandbox: sb1, sig: si1, source: sr1 },
            },
        ) => {
            dbg!(name0 == name1)
                && dbg!(sb0 == sb1)
                && dbg!(check_module_sig(si0, si1))
                && dbg!(check(sr0, sr1))
        }
        (ExprKind::Do { exprs: exprs0 }, ExprKind::Do { exprs: exprs1 }) => {
            exprs0.len() == exprs1.len()
                && exprs0.iter().zip(exprs1.iter()).all(|(v0, v1)| check(v0, v1))
        }
        (
            ExprKind::Use { reexport: r0, names: names0 },
            ExprKind::Use { reexport: r1, names: names1 },
        ) => {
            dbg!(r0 == r1 && names0 == names1)
        }
        (ExprKind::Bind(b0), ExprKind::Bind(b1)) => {
            let BindExpr { rec: r0, pattern: p0, value: value0, typ: typ0 } = &**b0;
            let BindExpr { rec: r1, pattern: p1, value: value1, typ: typ1 } = &**b1;
            dbg!(
                dbg!(r0 == r1)
                    && dbg!(check_structure_pattern(p0, p1))
                    && dbg!(check_type_opt(typ0, typ1))
                    && dbg!(check(value0, value1))
            )
        }
        (
            ExprKind::Connect { name: name0, value: value0, deref: d0 },
            ExprKind::Connect { name: name1, value: value1, deref: d1 },
        ) => dbg!(dbg!(d0 == d1) && dbg!(name0 == name1) && dbg!(check(value0, value1))),
        (ExprKind::Qop(e0), ExprKind::Qop(e1)) => check(e0, e1),
        (ExprKind::OrNever(e0), ExprKind::OrNever(e1)) => check(e0, e1),
        (ExprKind::Catch(c0), ExprKind::Catch(c1)) => {
            let CatchExpr { bind: b0, constraint: c0, handler: h0 } = &**c0;
            let CatchExpr { bind: b1, constraint: c1, handler: h1 } = &**c1;
            b0 == b1 && check_type_opt(c0, c1) && check(h0, h1)
        }
        (ExprKind::Ref { name: name0 }, ExprKind::Ref { name: name1 }) => {
            dbg!(name0 == name1)
        }
        (ExprKind::Lambda(l0), ExprKind::Lambda(l1)) => match (&**l0, &**l1) {
            (
                LambdaExpr {
                    args: args0,
                    vargs: vargs0,
                    rtype: rtype0,
                    constraints: constraints0,
                    throws: throws0,
                    body: Either::Left(body0),
                },
                LambdaExpr {
                    args: args1,
                    vargs: vargs1,
                    rtype: rtype1,
                    constraints: constraints1,
                    throws: throws1,
                    body: Either::Left(body1),
                },
            ) => dbg!(
                dbg!(check_args(args0, args1))
                    && dbg!(match (vargs0, vargs1) {
                        (Some(t0), Some(t1)) => check_type_opt(t0, t1),
                        (None, None) => true,
                        _ => false,
                    })
                    && dbg!(check_type_opt(rtype0, rtype1))
                    && dbg!(
                        constraints0
                            .iter()
                            .zip(constraints1.iter())
                            .all(|((tv0, tc0), (tv1, tc1))| tv0.name == tv1.name
                                && check_type(&tc0, &tc1))
                    )
                    && dbg!(check_type_opt(throws0, throws1))
                    && dbg!(check(body0, body1))
            ),
            (
                LambdaExpr {
                    args: args0,
                    vargs: vargs0,
                    rtype: rtype0,
                    constraints: constraints0,
                    throws: throws0,
                    body: Either::Right(b0),
                },
                LambdaExpr {
                    args: args1,
                    vargs: vargs1,
                    rtype: rtype1,
                    constraints: constraints1,
                    throws: throws1,
                    body: Either::Right(b1),
                },
            ) => dbg!(
                dbg!(check_args(args0, args1))
                    && dbg!(match (vargs0, vargs1) {
                        (Some(t0), Some(t1)) => check_type_opt(t0, t1),
                        (None, None) => true,
                        _ => false,
                    })
                    && dbg!(check_type_opt(rtype0, rtype1))
                    && dbg!(
                        constraints0
                            .iter()
                            .zip(constraints1.iter())
                            .all(|((tv0, tc0), (tv1, tc1))| tv0.name == tv1.name
                                && check_type(&tc0, &tc1))
                    )
                    && dbg!(check_type_opt(throws0, throws1))
                    && dbg!(b0 == b1)
            ),
            (_, _) => false,
        },
        (
            ExprKind::Select(SelectExpr { arg: arg0, arms: arms0 }),
            ExprKind::Select(SelectExpr { arg: arg1, arms: arms1 }),
        ) => {
            dbg!(
                dbg!(check(arg0, arg1))
                    && dbg!(arms0.len() == arms1.len())
                    && dbg!(
                        arms0
                            .iter()
                            .zip(arms1.iter())
                            .all(|((pat0, b0), (pat1, b1))| check(b0, b1)
                                && dbg!(check_pattern(pat0, pat1)))
                    )
            )
        }
        (ExprKind::TypeDef(td0), ExprKind::TypeDef(td1)) => check_typedef(td0, td1),
        (ExprKind::Trait(t0), ExprKind::Trait(t1)) => check_trait(t0, t1),
        (ExprKind::Impl(i0), ExprKind::Impl(i1)) => check_impl(i0, i1),
        (
            ExprKind::TypeCast { expr: expr0, typ: typ0 },
            ExprKind::TypeCast { expr: expr1, typ: typ1 },
        ) => dbg!(check(expr0, expr1)) && dbg!(check_type(&typ0, &typ1)),
        (ExprKind::Any { args: a0 }, ExprKind::Any { args: a1 }) => {
            a0.len() == a1.len() && a0.iter().zip(a1.iter()).all(|(a0, a1)| check(a0, a1))
        }
        (ExprKind::ByRef(e0), ExprKind::ByRef(e1)) => check(e0, e1),
        (ExprKind::Deref(e0), ExprKind::Deref(e1)) => check(e0, e1),
        (ExprKind::Neg(e0), ExprKind::Neg(e1)) => check(e0, e1),
        (
            ExprKind::Sample { lhs: l0, rhs: r0 },
            ExprKind::Sample { lhs: l1, rhs: r1 },
        ) => check(l0, l1) && check(r0, r1),
        (ExprKind::NoOp, ExprKind::NoOp) => true,
        (_, _) => false,
    }
}

proptest! {
    #[test]
    fn expr_round_trip0(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip1(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip2(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip3(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip4(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip5(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip6(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_round_trip7(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string()));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip0(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip1(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip2(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip3(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip4(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip5(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip6(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }

    #[test]
    fn expr_pp_round_trip7(s in expr()) {
        let s = dbg!(s);
        let st = dbg!(format_with_flags(BitFlags::empty(), || s.to_string_pretty(80)));
        let e = dbg!(parse_one(st.as_str()).unwrap());
        assert!(check(&s, &e))
    }
}

mod tree_sitter_compat {
    use super::*;

    fn find_tree_error(node: tree_sitter::Node, source: &str) -> Option<String> {
        if node.is_error() {
            return Some(format!(
                "ERROR at {}:{}: {:?}",
                node.start_position().row,
                node.start_position().column,
                &source[node.byte_range()]
            ));
        }
        if node.is_missing() {
            return Some(format!(
                "MISSING {} at {}:{}",
                node.kind(),
                node.start_position().row,
                node.start_position().column
            ));
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(err) = find_tree_error(child, source) {
                return Some(err);
            }
        }
        None
    }

    /// The editor query files are part of the grammar's contract: a
    /// stale node name doesn't degrade one rule, it makes the whole
    /// query fail to compile and the editor shows NO colors at all.
    #[test]
    fn queries_compile() {
        const QUERIES: [(&str, &str); 3] = [
            (
                "highlights.scm",
                include_str!("../../../ide/tree-sitter-graphix/queries/highlights.scm"),
            ),
            (
                "locals.scm",
                include_str!("../../../ide/tree-sitter-graphix/queries/locals.scm"),
            ),
            (
                "indents.scm",
                include_str!("../../../ide/tree-sitter-graphix/queries/indents.scm"),
            ),
        ];
        let lang: tree_sitter::Language = tree_sitter_graphix::LANGUAGE.into();
        for (name, src) in QUERIES {
            if let Err(e) = tree_sitter::Query::new(&lang, src) {
                panic!("{name} does not compile against the grammar: {e}")
            }
        }
        // The emacs mode embeds its queries in elisp instead of loading
        // the files above (its captures are font-lock faces), so it rots
        // separately and needs the same gate.
        let el = include_str!("../../../ide/editors/emacs/graphix-mode.el");
        let queries = emacs_queries(el);
        assert!(queries.len() > 10, "found only {} emacs queries", queries.len());
        for (feature, src) in queries {
            if let Err(e) = tree_sitter::Query::new(&lang, &src) {
                panic!("graphix-mode.el: the {feature} query does not compile: {e}")
            }
        }
    }

    /// The tree-sitter queries embedded in the emacs mode: each is the
    /// form following a `:feature 'name` keyword — a quoted list, or a
    /// string where the elisp reader would choke on a query operator.
    fn emacs_queries(src: &str) -> Vec<(String, String)> {
        let mut res = vec![];
        let mut rest = src;
        while let Some(i) = rest.find(":feature '") {
            rest = &rest[i + ":feature '".len()..];
            let end = rest.find(|c: char| !c.is_alphanumeric() && c != '-').unwrap();
            let feature = rest[..end].to_string();
            rest = &rest[end..];
            // skip whitespace and comment lines to the query form
            loop {
                rest = rest.trim_start();
                if rest.starts_with(";;") {
                    rest = &rest[rest.find('\n').unwrap()..];
                } else {
                    break;
                }
            }
            let (body, len) = match rest.as_bytes()[0] {
                b'\'' => {
                    let (b, n) = balanced(&rest[1..]);
                    (b.to_string(), n + 1)
                }
                b'"' => {
                    let n = rest[1..].find('"').unwrap();
                    (rest[1..1 + n].to_string(), n + 2)
                }
                c => panic!("{feature}: unexpected query form starting {:?}", c as char),
            };
            res.push((feature, body));
            rest = &rest[len..];
        }
        res
    }

    /// The text of the parenthesized form at the head of `s`, and how
    /// many bytes of `s` it spans. Elisp strings can hold parens.
    fn balanced(s: &str) -> (&str, usize) {
        let b = s.as_bytes();
        assert_eq!(b[0], b'(');
        let (mut depth, mut instr, mut i) = (0, false, 0);
        while i < b.len() {
            match b[i] {
                b'\\' if instr => i += 1,
                b'"' => instr = !instr,
                b'(' if !instr => depth += 1,
                b')' if !instr => {
                    depth -= 1;
                    if depth == 0 {
                        return (&s[..=i], i + 1);
                    }
                }
                _ => (),
            }
            i += 1;
        }
        panic!("unbalanced form: {}", &s[..s.len().min(80)])
    }

    fn assert_ts_parses(source: &str) {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_graphix::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        if let Some(err) = find_tree_error(tree.root_node(), source) {
            panic!(
                "tree-sitter: {err}\n\nSource:\n{source}\n\nTree:\n{}",
                tree.root_node().to_sexp()
            );
        }
    }

    /// Attributes at every position the graphix parser captures a
    /// decoration, and the shapes it admits: bare, args, args that are
    /// themselves expressions. The proptest lane below generates
    /// attributes too, but only over the expressions it builds — this
    /// pins the syntax itself against the grammar.
    #[test]
    fn ts_attributes_parse() {
        const SRCS: [&str; 6] = [
            "#[sync]\nlet f = |x| x + 1",
            "#[foo(1, \"two\", a::b)]\nlet f = 3",
            "let f = |n| select n {\n  // above the pattern\n  #[native]\n  0 => 0,\n  k => k\n}",
            "type Counter = Abstract<i64>;\nimpl Show for Counter {\n  #[sync]\n  let show = |c| \"x\"\n}",
            "let s = {\n  #[native]\n  a: 1,\n  // and a comment\n  #[sync]\n  b: 2\n}",
            "{\n  #[sync]\n  let a = 1;\n  #[async]\n  a + 1\n}",
        ];
        for src in SRCS {
            assert_ts_parses(src);
        }
    }

    /// An attribute is a node of its own, so an editor can color it —
    /// if it were swallowed by the expression's extent there would be
    /// nothing to match on.
    #[test]
    fn ts_attribute_is_a_node() {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_graphix::LANGUAGE.into()).unwrap();
        let src = "#[foo(1)]\nlet f = 3";
        let tree = parser.parse(src, None).unwrap();
        let sexp = tree.root_node().to_sexp();
        assert!(sexp.contains("attribute"), "no attribute node in {sexp}");
    }

    proptest! {
        #[test]
        fn ts_expr0(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string());
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_expr1(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string());
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_expr2(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string());
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_expr3(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string());
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_pp0(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string_pretty(80));
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_pp1(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string_pretty(80));
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_pp2(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string_pretty(80));
            assert_ts_parses(&st);
        }

        #[test]
        fn ts_pp3(s in expr()) {
            let st = format_with_flags(BitFlags::empty(), || s.to_string_pretty(80));
            assert_ts_parses(&st);
        }
    }
}
