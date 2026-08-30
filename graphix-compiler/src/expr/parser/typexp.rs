use super::{
    csep, fname, grow::grow, ident, not_prefix, path_root, sep_by_tok, sep_by1_tok,
    spaces, spaces1, spfldname, spstring, sptoken, typname,
};
use crate::{
    expr::{Expr, ExprKind, ModPath, TypeDefBody, TypeDefExpr},
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, TypeRef},
};
use ahash::AHashSet;
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, look_ahead,
    not_followed_by, optional,
    parser::char::{alpha_num, string},
    position, sep_by1,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use netidx_core::utils::Either;
use netidx_value::Typ;
use poolshark::local::LPooled;
use triomphe::Arc;

pub(super) fn typath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        spaces().with(path_root()),
        sep_by1(spaces().with(choice((fname(), typname()))), string("::")),
    )
        .then(
            |(mut root, mut parts): (LPooled<Vec<ArcStr>>, LPooled<Vec<ArcStr>>)| {
                if parts.len() == 0 {
                    unexpected_any("empty type path").left()
                } else {
                    match parts.last().unwrap().chars().next() {
                        None => unexpected_any("empty name").left(),
                        Some(c) if c.is_lowercase() => {
                            unexpected_any("type names must be capitalized").left()
                        }
                        Some(_) => {
                            root.extend(parts.drain(..));
                            value(ModPath::from(root.drain(..))).right()
                        }
                    }
                }
            },
        )
}

fn typeprim<I>() -> impl Parser<I, Output = Typ>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        string("string").map(|_| Typ::String),
        string("error").map(|_| Typ::Error),
        string("array").map(|_| Typ::Array),
        string("null").map(|_| Typ::Null),
        attempt(string("i8")).map(|_| Typ::I8),
        attempt(string("i16")).map(|_| Typ::I16),
        attempt(string("i32")).map(|_| Typ::I32),
        string("i64").map(|_| Typ::I64),
        attempt(string("u8")).map(|_| Typ::U8),
        attempt(string("u16")).map(|_| Typ::U16),
        attempt(string("u32")).map(|_| Typ::U32),
        string("u64").map(|_| Typ::U64),
        attempt(string("v32")).map(|_| Typ::V32),
        string("v64").map(|_| Typ::V64),
        attempt(string("z32")).map(|_| Typ::Z32),
        string("z64").map(|_| Typ::Z64),
        attempt(string("f32")).map(|_| Typ::F32),
        string("f64").map(|_| Typ::F64),
        attempt(string("decimal")).map(|_| Typ::Decimal),
        attempt(string("datetime")).map(|_| Typ::DateTime),
        string("duration").map(|_| Typ::Duration),
        attempt(string("bytes")).map(|_| Typ::Bytes),
        string("bool").map(|_| Typ::Bool),
    ))
    .skip(not_prefix())
}

/// A type variable's bound: one type, or a `+`-joined conjunction of
/// traits (`'a: Read + Write`). Every member becomes its own conjunct
/// on the variable's cell.
pub(super) fn bound<I>() -> impl Parser<I, Output = LPooled<Vec<Type>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    sep_by1(typ(), attempt(spaces().with(token('+'))))
}

/// Flatten `(tvar, bounds)` pairs into one `(tvar, type)` pair per
/// conjunct, the shape every constraint consumer takes.
pub(super) fn flatten_bounds(
    mut cs: LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>,
) -> LPooled<Vec<(TVar, Type)>> {
    let mut out: LPooled<Vec<(TVar, Type)>> = LPooled::take();
    for (tv, mut bs) in cs.drain(..) {
        for b in bs.drain(..) {
            out.push((tv.clone(), b));
        }
    }
    out
}

fn fnconstraints<I>() -> impl Parser<I, Output = LPooled<Vec<(TVar, Type)>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces()
        .with(optional(between(
            token('<'),
            sptoken('>'),
            sep_by1_tok(
                (spaces().with(tvar()).skip(sptoken(':')), bound()),
                csep(),
                token('>'),
            ),
        )))
        .map(|cs: Option<LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>>| {
            flatten_bounds(cs.unwrap_or_else(LPooled::take))
        })
}

fn fnlabeled<I>() -> impl Parser<I, Output = FnArgType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((string("?#").map(|_| true), string("#").map(|_| false))).then(|optional| {
        (fname().skip(sptoken(':')), typ()).map(move |(name, typ)| FnArgType {
            kind: FnArgKind::Labeled { name: name.into(), has_default: optional },
            typ,
        })
    })
}

fn fnpositional<I>() -> impl Parser<I, Output = FnArgType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(string("self").skip(not_prefix()).skip(not_followed_by(sptoken(':'))))
            .with(optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
            .map(|arg| FnArgType {
                kind: FnArgKind::Positional { name: Some(literal!("self")) },
                typ: match arg {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                },
            }),
        (fname().skip(sptoken(':')), typ()).map(|(name, typ)| FnArgType {
            kind: FnArgKind::Positional { name: Some(name.into()) },
            typ,
        }),
    ))
}

/// The receiver type of a trait method signature: the type variable
/// spelled `self`. Same-named occurrences in one signature alias to one
/// cell like any quantifier, and the trait declaration constrains it.
pub(crate) fn self_tvar() -> Type {
    Type::TVar(TVar::empty_named(literal!("self")))
}

fn fnargs<I>() -> impl Parser<I, Output = LPooled<Vec<Either<FnArgType, Type>>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(between(
        token('('),
        sptoken(')'),
        sep_by_tok(
            spaces().then(|_| {
                choice((
                    string("@args:").with(typ()).map(|e| Either::Right(e)),
                    fnlabeled().map(Either::Left),
                    fnpositional().map(Either::Left),
                ))
            }),
            csep(),
            attempt(sptoken(')')),
        ),
    ))
}

pub(super) fn fntype<I>() -> impl Parser<I, Output = FnType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(string("fn").skip(not_followed_by(choice((token('_'), alpha_num())))))
        .with((
            fnconstraints(),
            fnargs(),
            spstring("->").with(typ()),
            optional(
                attempt(spaces1().with(string("throws"))).with(spaces1()).with(typ()),
            ),
        ))
        .then(|(constraints, mut args, rtype, throws)| {
            let vargs = match args.pop() {
                None => None,
                Some(Either::Right(t)) => Some(t),
                Some(Either::Left(t)) => {
                    args.push(Either::Left(t));
                    None
                }
            };
            if !args.iter().all(|a| a.is_left()) {
                return unexpected_any("vargs must appear once at the end of the args")
                    .left();
            }
            let args = Arc::from_iter(args.drain(..).map(|t| match t {
                Either::Left(t) => t,
                Either::Right(_) => unreachable!(),
            }));
            let mut anon = false;
            for a in args.iter() {
                if anon && a.is_labeled() {
                    return unexpected_any(
                        "anonymous args must appear after labeled args",
                    )
                    .left();
                }
                anon |= a.is_positional();
            }
            let explicit_throws = throws.is_some();
            let throws = throws.unwrap_or(Type::Bottom);
            let ft = FnType {
                args,
                vargs,
                rtype,
                throws,
                explicit_throws,
                quantifiers: quantifier_names(constraints.iter().map(|(tv, _)| tv)),
                ..Default::default()
            };
            // Quantifier constraints seed CELLS (phase C — the cells
            // are the only store). Alias the signature's same-named
            // tvars to the quantifier tvars FIRST so the conjunct
            // lands in the one cell every occurrence shares; the
            // constraint types' own tvars go through the same map.
            {
                let mut known: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
                for (tv, _) in constraints.iter() {
                    known.insert(tv.name.clone(), tv.clone());
                }
                ft.alias_tvars(&mut known);
                for (tv, tc) in constraints.iter() {
                    tc.alias_tvars(&mut known);
                    tv.add_cell_constraint(tc.clone());
                }
            }
            value(ft).right()
        })
}

/// The declared quantifier names of a signature, in source order,
/// deduplicated: a `+`-bound variable appears once per conjunct in the
/// constraint list but is one quantifier.
pub(crate) fn quantifier_names<'a>(tvs: impl Iterator<Item = &'a TVar>) -> Arc<[ArcStr]> {
    let mut names: LPooled<Vec<ArcStr>> = LPooled::take();
    for tv in tvs {
        if !names.contains(&tv.name) {
            names.push(tv.name.clone());
        }
    }
    Arc::from_iter(names.drain(..))
}

pub(super) fn tvar<I>() -> impl Parser<I, Output = TVar>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    token('\'').with(fname()).map(TVar::empty_named)
}

fn varianttyp<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        token('`').with(ident(true)),
        optional(attempt(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(typ(), csep(), token(')')),
        ))),
    )
        .map(|(tag, typs): (ArcStr, Option<LPooled<Vec<Type>>>)| {
            let mut t = match typs {
                None => LPooled::take(),
                Some(v) => v,
            };
            Type::Variant(tag.clone(), Arc::from_iter(t.drain(..)))
        })
}

fn structtyp<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('{'),
        sptoken('}'),
        sep_by1_tok((spfldname().skip(sptoken(':')), typ()), csep(), token('}')),
    )
    .then(|mut exps: LPooled<Vec<(ArcStr, Type)>>| {
        let s = exps.iter().map(|(n, _)| n).collect::<LPooled<AHashSet<_>>>();
        if s.len() < exps.len() {
            return unexpected_any("struct field names must be unique").left();
        }
        drop(s);
        exps.sort_by_key(|(n, _)| n.clone());
        value(Type::Struct(Arc::from_iter(exps.drain(..)))).right()
    })
}

fn tupletyp<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(token('('), sptoken(')'), sep_by1_tok(typ(), csep(), token(')'))).map(
        |mut exps: LPooled<Vec<Type>>| {
            if exps.len() == 1 {
                exps.pop().unwrap()
            } else {
                Type::Tuple(Arc::from_iter(exps.drain(..)))
            }
        },
    )
}

pub(super) fn typref<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        typath(),
        look_ahead(optional(attempt(sptoken('<')))).then(|o| match o {
            None => value(None).left(),
            Some(_) => between(
                sptoken('<'),
                sptoken('>'),
                sep_by1_tok(typ(), csep(), token('>')),
            )
            .map(Some)
            .right(),
        }),
    )
        .map(
            |(pos, n, params): (SourcePosition, ModPath, Option<LPooled<Vec<Type>>>)| {
                let params = params
                    .map(|mut a| Arc::from_iter(a.drain(..)))
                    .unwrap_or_else(|| Arc::from_iter([]));
                Type::Ref(TypeRef::new(
                    ModPath::root(),
                    n,
                    params,
                    Some(pos),
                    Some(crate::expr::get_origin()),
                ))
            },
        )
}

parser! {
    pub(super) fn typ[I]()(I) -> Type
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(spaces().with(choice((
            token('&').with(typ()).map(|t| Type::ByRef(Arc::new(t))),
            token('_').map(|_| Type::Bottom),
            between(
                token('['),
                sptoken(']'),
                sep_by_tok(typ(), csep(), attempt(sptoken(']'))),
            )
                .map(|mut ts: LPooled<Vec<Type>>| Type::flatten_set(ts.drain(..))),
            tupletyp(),
            structtyp(),
            varianttyp(),
            fntype().map(|f| Type::Fn(Arc::new(f))),
            attempt(string("Array").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Array(Arc::new(t))),
            attempt(string("List").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::List(Arc::new(t))),
            attempt(string("Any").skip(not_prefix())).map(|_| Type::Any),
            attempt(string("Map").skip(not_prefix())).with(between(
                sptoken('<'), sptoken('>'),
                (typ().skip(sptoken(',')), typ())
            )).map(|(k, v)| Type::Map { key: Arc::new(k), value: Arc::new(v) }),
            attempt(string("Error").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Error(Arc::new(t))),
            attempt(string("Abstract").skip(not_prefix())).then(|_| {
                unexpected_any("Abstract<..> is legal only as the whole body of a type definition")
            }),
            attempt(typeprim()).map(|typ| Type::Primitive(typ.into())),
            attempt(string("self").skip(not_prefix()).skip(not_followed_by(string("::"))))
                .with(optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
                .map(|arg| match arg {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                }),
            attempt(string("'_").skip(not_prefix())).map(|_| Type::Hole),
            (tvar(), optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
                .map(|(tv, arg)| match arg {
                    None => Type::TVar(tv),
                    Some(arg) => Type::App(Arc::new(Type::TVar(tv)), Arc::new(arg)),
                }),
            typref(),
        ))))
    }
}

pub(super) fn typedef<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("type").skip(spaces1())).with(typname()),
        spaces().with(optional(between(
            token('<'),
            sptoken('>'),
            sep_by1_tok(
                (
                    spaces().with(tvar()),
                    spaces().then(|_| optional(token(':').with(typ()))),
                ),
                csep(),
                token('>'),
            ),
        ))),
        spaces().with(optional(
            attempt(token('=').skip(not_followed_by(token('>')))).with(choice((
                attempt(spaces().with(string("Abstract")).skip(not_prefix()))
                    .with(between(sptoken('<'), sptoken('>'), typ()))
                    .map(|rep| TypeDefBody::Abstract(Some(rep))),
                typ().map(TypeDefBody::Alias),
            ))),
        )),
    )
        .map(|(pos, name, params, body)| {
            let params = params
                .map(|mut ps: LPooled<Vec<(TVar, Option<Type>)>>| {
                    Arc::from_iter(ps.drain(..))
                })
                .unwrap_or_else(|| Arc::<[(TVar, Option<Type>)]>::from_iter([]));
            let body = body.unwrap_or(TypeDefBody::Abstract(None));
            ExprKind::TypeDef(TypeDefExpr { name, params, body }).to_expr(pos)
        })
}
