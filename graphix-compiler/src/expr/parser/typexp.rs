use super::{
    csep, fname, ident, sep_by1_tok, sep_by_tok, spaces, spaces1, spfname, spstring,
    sptoken, typname,
};
use crate::{
    expr::{Expr, ExprKind, ModPath, TypeDefExpr},
    typ::{FnArgType, FnType, TVar, Type},
};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, look_ahead, not_followed_by, optional,
    parser::char::{alpha_num, string},
    position, sep_by1,
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use fxhash::FxHashSet;
use netidx::{publisher::Typ, utils::Either};
use parking_lot::RwLock;
use poolshark::local::LPooled;
use triomphe::Arc;

fn sptypname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(typname())
}

pub(super) fn typath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    sep_by1(spaces().with(choice((fname(), typname()))), string("::")).then(
        |mut parts: LPooled<Vec<ArcStr>>| {
            if parts.len() == 0 {
                unexpected_any("empty type path").left()
            } else {
                match parts.last().unwrap().chars().next() {
                    None => unexpected_any("empty name").left(),
                    Some(c) if c.is_lowercase() => {
                        unexpected_any("type names must be capitalized").left()
                    }
                    Some(_) => value(ModPath::from(parts.drain(..))).right(),
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
        attempt(string("i8")).map(|_| Typ::I8),
        attempt(string("i16")).map(|_| Typ::I16),
        attempt(string("i32")).map(|_| Typ::I32),
        attempt(string("i64")).map(|_| Typ::I64),
        attempt(string("u8")).map(|_| Typ::U8),
        attempt(string("u16")).map(|_| Typ::U16),
        attempt(string("u32")).map(|_| Typ::U32),
        attempt(string("u64")).map(|_| Typ::U64),
        attempt(string("v32")).map(|_| Typ::V32),
        attempt(string("v64")).map(|_| Typ::V64),
        attempt(string("z32")).map(|_| Typ::Z32),
        attempt(string("z64")).map(|_| Typ::Z64),
        attempt(string("f32")).map(|_| Typ::F32),
        attempt(string("f64")).map(|_| Typ::F64),
        attempt(string("decimal")).map(|_| Typ::Decimal),
        attempt(string("datetime")).map(|_| Typ::DateTime),
        attempt(string("duration")).map(|_| Typ::Duration),
        attempt(string("bytes")).map(|_| Typ::Bytes),
        attempt(string("bool")).map(|_| Typ::Bool),
        attempt(string("string")).map(|_| Typ::String),
        attempt(string("error")).map(|_| Typ::Error),
        attempt(string("array")).map(|_| Typ::Array),
        attempt(string("null")).map(|_| Typ::Null),
    ))
    .skip(not_followed_by(choice((alpha_num(), token('_')))))
}

fn fnconstraints<I>() -> impl Parser<I, Output = Arc<RwLock<LPooled<Vec<(TVar, Type)>>>>>
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
                (spaces().with(tvar()).skip(sptoken(':')), typ()),
                csep(),
                token('>'),
            ),
        )))
        .map(|cs: Option<LPooled<Vec<(TVar, Type)>>>| match cs {
            Some(cs) => Arc::new(RwLock::new(cs)),
            None => Arc::new(RwLock::new(LPooled::take())),
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
            label: Some((name.into(), optional)),
            typ,
        })
    })
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
                    typ().map(|typ| Either::Left(FnArgType { label: None, typ })),
                ))
            }),
            csep(),
            token(')'),
        ),
    ))
}

pub(super) fn fntype<I>() -> impl Parser<I, Output = FnType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(string("fn"))
        .with((
            fnconstraints(),
            fnargs(),
            spstring("->").with(typ()),
            spaces1().with(optional(string("throws").with(spaces1()).with(typ()))),
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
                if anon && a.label.is_some() {
                    return unexpected_any(
                        "anonymous args must appear after labeled args",
                    )
                    .left();
                }
                anon |= a.label.is_none();
            }
            let throws = throws.unwrap_or(Type::Bottom);
            value(FnType { args, vargs, rtype, constraints, throws }).right()
        })
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
        spaces().with(optional(between(
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
        sep_by1_tok((spfname().skip(sptoken(':')), typ()), csep(), token('}')),
    )
    .then(|mut exps: LPooled<Vec<(ArcStr, Type)>>| {
        let s = exps.iter().map(|(n, _)| n).collect::<LPooled<FxHashSet<_>>>();
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
        .map(|(n, params): (ModPath, Option<LPooled<Vec<Type>>>)| {
            let params = params
                .map(|mut a| Arc::from_iter(a.drain(..)))
                .unwrap_or_else(|| Arc::from_iter([]));
            Type::Ref { scope: ModPath::root(), name: n, params }
        })
}

parser! {
    pub(super) fn typ[I]()(I) -> Type
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        spaces().with(choice((
            token('&').with(typ()).map(|t| Type::ByRef(Arc::new(t))),
            token('_').map(|_| Type::Bottom),
            between(token('['), sptoken(']'), sep_by_tok(typ(), csep(), token(']')))
                .map(|mut ts: LPooled<Vec<Type>>| Type::flatten_set(ts.drain(..))),
            tupletyp(),
            structtyp(),
            varianttyp(),
            fntype().map(|f| Type::Fn(Arc::new(f))),
            attempt(string("Array")).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Array(Arc::new(t))),
            attempt(string("Any")).map(|_| Type::Any),
            attempt(string("Map")).with(between(
                sptoken('<'), sptoken('>'),
                (typ().skip(sptoken(',')), typ())
            )).map(|(k, v)| Type::Map { key: Arc::new(k), value: Arc::new(v) }),
            attempt(string("Error")).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Error(Arc::new(t))),
            typeprim().map(|typ| Type::Primitive(typ.into())),
            tvar().map(|tv| Type::TVar(tv)),
            typref(),
        )))
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
        attempt(string("type")).with(sptypname()),
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
        sptoken('=').with(typ()),
    )
        .map(|(pos, name, params, typ)| {
            let params = params
                .map(|mut ps: LPooled<Vec<(TVar, Option<Type>)>>| {
                    Arc::from_iter(ps.drain(..))
                })
                .unwrap_or_else(|| Arc::<[(TVar, Option<Type>)]>::from(Vec::new()));
            ExprKind::TypeDef(TypeDefExpr { name, params, typ }).to_expr(pos)
        })
}
