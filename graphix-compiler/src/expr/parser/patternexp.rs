use crate::{
    expr::{
        parser::{
            csep, expr, fname, sep_by1_tok, sep_by_tok, spaces, spaces1, spstring,
            sptoken, typ, typname,
        },
        Expr, Pattern, StructurePattern,
    },
    typ::Type,
};
use arcstr::{literal, ArcStr};
use combine::{
    attempt, between, choice, not_followed_by, optional,
    parser::char::string,
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use fxhash::FxHashSet;
use netidx::utils::Either;
use netidx_value::parser::{value as parse_value, VAL_ESC, VAL_MUST_ESC};
use poolshark::local::LPooled;
use triomphe::Arc;

pub(super) fn slice_pattern<I>(
    all: Option<ArcStr>,
) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    macro_rules! all_left {
        ($pats:expr) => {{
            let mut err = false;
            let pats: Arc<[StructurePattern]> =
                Arc::from_iter($pats.drain(..).map(|s| match s {
                    Either::Left(s) => s,
                    Either::Right(_) => {
                        err = true;
                        StructurePattern::Ignore
                    }
                }));
            if err {
                return unexpected_any("invalid pattern").left();
            }
            pats
        }};
    }
    between(
        token('['),
        sptoken(']'),
        sep_by_tok(
            spaces().with(choice((
                string("..").map(|_| Either::Right(None)),
                attempt(fname().skip(spstring(".."))).map(|n| Either::Right(Some(n))),
                structure_pattern().map(|p| Either::Left(p)),
            ))),
            csep(),
            token(']'),
        ),
    )
    .then(move |mut pats: LPooled<Vec<Either<StructurePattern, Option<ArcStr>>>>| {
        let all = all.clone();
        if pats.len() == 0 {
            value(StructurePattern::Slice { all, binds: Arc::from_iter([]) }).right()
        } else if pats.len() == 1 {
            match pats.pop().unwrap() {
                Either::Left(s) => {
                    value(StructurePattern::Slice { all, binds: Arc::from_iter([s]) })
                        .right()
                }
                Either::Right(_) => unexpected_any("invalid singular range match").left(),
            }
        } else {
            match (&pats[0], &pats[pats.len() - 1]) {
                (Either::Right(_), Either::Right(_)) => {
                    unexpected_any("invalid pattern").left()
                }
                (Either::Right(_), Either::Left(_)) => {
                    let head = pats.remove(0).right().unwrap();
                    let suffix = all_left!(pats);
                    value(StructurePattern::SliceSuffix { all, head, suffix }).right()
                }
                (Either::Left(_), Either::Right(_)) => {
                    let tail = pats.pop().unwrap().right().unwrap();
                    let prefix = all_left!(pats);
                    value(StructurePattern::SlicePrefix { all, tail, prefix }).right()
                }
                (Either::Left(_), Either::Left(_)) => {
                    value(StructurePattern::Slice { all, binds: all_left!(pats) }).right()
                }
            }
        }
    })
}

fn tuple_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('('),
        sptoken(')'),
        sep_by1_tok(structure_pattern(), csep(), token(')')),
    )
    .then(move |mut binds: LPooled<Vec<StructurePattern>>| {
        if binds.len() < 2 {
            unexpected_any("tuples must have at least 2 elements").left()
        } else {
            let all = all.clone();
            value(StructurePattern::Tuple { all, binds: Arc::from_iter(binds.drain(..)) })
                .right()
        }
    })
}

fn variant_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        token('`').with(typname()).skip(spaces()),
        optional(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(structure_pattern(), csep(), token(')')),
        )),
    )
        .map(
            move |(tag, binds): (ArcStr, Option<LPooled<Vec<StructurePattern>>>)| {
                let all = all.clone();
                let mut binds = match binds {
                    None => LPooled::take(),
                    Some(a) => a,
                };
                StructurePattern::Variant {
                    all,
                    tag,
                    binds: Arc::from_iter(binds.drain(..)),
                }
            },
        )
}

pub(super) fn struct_pattern<I>(
    all: Option<ArcStr>,
) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('{'),
        sptoken('}'),
        spaces().with(sep_by1_tok(
            choice((
                string("..").map(|_| (literal!(""), StructurePattern::Ignore, false)),
                fname()
                    .skip(spaces())
                    .then(|name| {
                        optional(token(':').with(structure_pattern()))
                            .map(move |pat| (name.clone(), pat))
                    })
                    .map(|(name, pat)| match pat {
                        Some(pat) => (name, pat, true),
                        None => {
                            let pat = StructurePattern::Bind(name.clone());
                            (name, pat, true)
                        }
                    }),
            )),
            csep(),
            token('}'),
        )),
    )
    .then(move |mut binds: LPooled<Vec<(ArcStr, StructurePattern, bool)>>| {
        let mut exhaustive = true;
        binds.retain(|(_, _, ex)| {
            exhaustive &= *ex;
            *ex
        });
        binds.sort_by_key(|(s, _, _)| s.clone());
        let s = binds.iter().map(|(s, _, _)| s).collect::<LPooled<FxHashSet<_>>>();
        if s.len() < binds.len() {
            unexpected_any("struct fields must be unique").left()
        } else {
            drop(s);
            let all = all.clone();
            let binds = Arc::from_iter(binds.drain(..).map(|(s, p, _)| (s, p)));
            value(StructurePattern::Struct { all, exhaustive, binds }).right()
        }
    })
}

fn underbar_pattern<I>(all: bool) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    token('_').then(move |_| {
        if all {
            unexpected_any("all patterns are not supported by _").left()
        } else {
            value(StructurePattern::Ignore).right()
        }
    })
}

fn bind_pattern<I>(all: bool) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    fname().then(move |name| {
        if all {
            unexpected_any("all patterns are not supported by bind").left()
        } else {
            value(StructurePattern::Bind(name)).right()
        }
    })
}

fn literal_pattern<I>(all: bool) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(parse_value(&VAL_MUST_ESC, &VAL_ESC)).skip(not_followed_by(token('_'))).then(
        move |v| {
            if all {
                unexpected_any("all patterns are not supported by literals").left()
            } else {
                value(StructurePattern::Literal(v)).right()
            }
        },
    )
}

fn all_pattern<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    fname().skip(sptoken('@')).skip(spaces())
}

parser! {
    pub(crate) fn structure_pattern[I]()(I) -> StructurePattern
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        spaces().with(optional(attempt(all_pattern()))).then(|all| choice((
            slice_pattern(all.clone()),
            tuple_pattern(all.clone()),
            struct_pattern(all.clone()),
            variant_pattern(all.clone()),
            underbar_pattern(all.is_some()),
            literal_pattern(all.is_some()),
            bind_pattern(all.is_some()),
        )))
    }
}

pub(crate) fn pattern<I>() -> impl Parser<I, Output = Pattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        optional(attempt(typ().skip(spaces1()).skip(string("as")).skip(spaces1()))),
        structure_pattern(),
        optional(attempt(spaces1().with(string("if")).with(spaces1()).with(expr()))),
    )
        .map(
            |(type_predicate, structure_predicate, guard): (
                Option<Type>,
                StructurePattern,
                Option<Expr>,
            )| { Pattern { type_predicate, structure_predicate, guard } },
        )
}
