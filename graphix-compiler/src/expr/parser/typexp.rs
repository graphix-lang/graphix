use super::{
    csep, fldname, fname,
    grow::{grow, refuse},
    ident,
    lambdaexp::{LABELED_FIRST, labeled_first},
    not_prefix, path_root, sep_by_tok, sep_by1_tok, sort_unique, spaces, spaces1,
    spstring, sptoken, typname,
};
use crate::{
    expr::{
        ModPath, Name, TypeDefBody, TypeDefExpr, WrittenAt, get_origin,
    },
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, TypeRef},
};
use ahash::AHashMap;
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, look_ahead,
    not_followed_by, optional,
    parser::char::string,
    position, sep_by1,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use netidx_core::utils::Either;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::str::FromStr;
use triomphe::Arc;

pub(super) fn typath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        spaces().with(path_root()),
        sep_by1(spaces().with(choice((fname(), typname()))), string("::")),
    )
        .then(
            |(mut root, mut parts): (LPooled<Vec<ArcStr>>, LPooled<Vec<ArcStr>>)| {
                let capitalized = parts
                    .last()
                    .and_then(|p| p.chars().next())
                    .is_some_and(char::is_uppercase);
                if capitalized {
                    root.extend(parts.drain(..));
                    value(ModPath::from(root.drain(..))).right()
                } else {
                    unexpected_any("type names must be capitalized").left()
                }
            },
        )
}

/// A primitive type's name. `map` and `abstract` name no type here.
fn typeprim<I>() -> impl Parser<I, Output = Typ>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(false).then(|s| match Typ::from_str(&s) {
        Ok(t) if t.name() == s && !matches!(t, Typ::Map | Typ::Abstract) => {
            value(t).left()
        }
        _ => unexpected_any("a primitive type").right(),
    })
}

/// A type variable's bound: one type, or a `+`-joined conjunction of
/// traits (`'a: Read + Write`), one conjunct per member.
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
    cs: impl IntoIterator<Item = (TVar, LPooled<Vec<Type>>)>,
) -> LPooled<Vec<(TVar, Type)>> {
    let mut out: LPooled<Vec<(TVar, Type)>> = LPooled::take();
    for (tv, mut bs) in cs {
        out.extend(bs.drain(..).map(|b| (tv.clone(), b)));
    }
    out
}

/// `'a: A + B`: a type variable and its bound.
pub(super) fn tvar_bound<I>() -> impl Parser<I, Output = (TVar, LPooled<Vec<Type>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (spaces().with(tvar()).skip(sptoken(':')), bound())
}

/// `'a`, or `'a: A + B`.
pub(super) fn tvar_opt_bound<I>()
-> impl Parser<I, Output = (TVar, Option<LPooled<Vec<Type>>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (spaces().with(tvar()), optional(attempt(sptoken(':')).with(bound())))
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
            sep_by1_tok(tvar_bound(), csep(), token('>')),
        )))
        .map(|cs: Option<LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>>| match cs {
            Some(mut cs) => flatten_bounds(cs.drain(..)),
            None => LPooled::take(),
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
/// spelled `self`.
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
            spaces().with(choice((
                string("@args:").with(typ()).map(|e| Either::Right(e)),
                fnlabeled().map(Either::Left),
                fnpositional().map(Either::Left),
            ))),
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
    attempt(string("fn").skip(not_prefix()))
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
            if !labeled_first(args.iter().map(|a| a.is_labeled())) {
                return unexpected_any(LABELED_FIRST).left();
            }
            let explicit_throws = throws.is_some();
            let throws = throws.unwrap_or(Type::Bottom);
            let ft = FnType { args, vargs, rtype, throws, explicit_throws, ..Default::default() };
            value(declared_fn_type(ft, &constraints)).right()
        })
}

/// `ft` declaring `constraints`: its quantifiers are their names in source
/// order, and every same-named tvar of the signature and of a conjunct is
/// the quantifier's, whose cell holds the conjuncts.
pub(crate) fn declared_fn_type(mut ft: FnType, constraints: &[(TVar, Type)]) -> FnType {
    ft.quantifiers = quantifier_names(constraints.iter().map(|(tv, _)| tv));
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    for (tv, _) in constraints.iter() {
        known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
    }
    ft.alias_tvars(&mut known);
    for (tv, tc) in constraints.iter() {
        // a conjunct may name its own quantifier (`'a: [i64, Array<'a>]`), so
        // the cell holds a type holding the cell: a walk over cell
        // constraints guards the cycle
        tc.alias_tvars(&mut known);
        known[&tv.name].add_cell_constraint(tc.clone());
    }
    ft
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
        position(),
        token('`').with(ident(true)),
        optional(attempt(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(typ(), csep(), token(')')),
        ))),
    )
        .map(|(pos, tag, typs): (_, ArcStr, Option<LPooled<Vec<Type>>>)| {
            let mut t = match typs {
                None => LPooled::take(),
                Some(v) => v,
            };
            Type::Variant(tag.clone(), Arc::from_iter(t.drain(..)), WrittenAt(pos))
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
        sep_by1_tok(
            (spaces().with(position()), fldname().skip(sptoken(':')), typ()),
            csep(),
            token('}'),
        ),
    )
    .and(position())
    .then(|(mut exps, end): (LPooled<Vec<(SourcePosition, ArcStr, Type)>>, _)| {
        if !sort_unique(&mut exps, |(_, n, _)| n) {
            return refuse(end, "struct field names must be unique").left();
        }
        let fields = exps.drain(..).map(|(pos, n, t)| (n, t, WrittenAt(pos)));
        value(Type::Struct(Arc::from_iter(fields))).right()
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
                    Some(get_origin()),
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

pub(super) fn typedef<I>() -> impl Parser<I, Output = TypeDefExpr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let params = (
        between(token('<'), sptoken('>'), sep_by1_tok(tvar_opt_bound(), csep(), token('>'))),
        position(),
    )
        .then(|(mut ps, end): (LPooled<Vec<(TVar, Option<LPooled<Vec<Type>>>)>>, _)| {
            let one = |b: &Option<LPooled<Vec<Type>>>| b.as_ref().is_none_or(|b| b.len() == 1);
            if !ps.iter().all(|(_, b)| one(b)) {
                return refuse(end, "a type parameter's bound is one type").right();
            }
            let ps = ps.drain(..).map(|(tv, b)| (tv, b.and_then(|mut b| b.pop())));
            value(Arc::<[(TVar, Option<Type>)]>::from_iter(ps)).left()
        });
    (
        attempt(string("type").skip(spaces1())).with((position(), typname())),
        spaces().with(optional(params)),
        spaces().with(optional(
            attempt(token('=').skip(not_followed_by(token('>')))).with(choice((
                attempt(spaces().with(string("Abstract")).skip(not_prefix()))
                    .with(between(sptoken('<'), sptoken('>'), typ()))
                    .map(|rep| TypeDefBody::Abstract(Some(rep))),
                typ().map(TypeDefBody::Alias),
            ))),
        )),
    )
        .map(|((at, name), params, body)| {
            let name = Name::written(name, at);
            let params = params.unwrap_or_else(|| Arc::from_iter([]));
            let body = body.unwrap_or(TypeDefBody::Abstract(None));
            TypeDefExpr { name, params, body }
        })
}
