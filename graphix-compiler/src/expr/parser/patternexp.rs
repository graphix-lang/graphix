use crate::{
    expr::{
        parser::{
            csep, expr, fname, sep_by1_tok, sep_by_tok, spaces, spstring, sptoken,
            typexp, typname,
        },
        Expr, Pattern, StructurePattern,
    },
    typ::Type,
};
use arcstr::{literal, ArcStr};
use combine::{
    attempt, between, choice, not_followed_by, optional,
    parser::char::{space, string},
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use fxhash::FxHashSet;
use netidx::utils::Either;
use netidx_value::parser::{value as parse_value, VAL_ESC, VAL_MUST_ESC};
use poolshark::local::LPooled;
use triomphe::Arc;

fn slice_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
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
                fname().skip(spstring("..")).map(|n| Either::Right(Some(n))),
                structure_pattern().map(|p| Either::Left(p)),
            ))),
            csep(),
            sptoken(']'),
        ),
    )
    .then(|mut pats: LPooled<Vec<Either<StructurePattern, Option<ArcStr>>>>| {
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('('),
        sptoken(')'),
        sep_by1_tok(structure_pattern(), csep(), sptoken(')')),
    )
    .then(|mut binds: LPooled<Vec<StructurePattern>>| {
        if binds.len() < 2 {
            unexpected_any("tuples must have at least 2 elements").left()
        } else {
            value(StructurePattern::Tuple { all, binds: Arc::from_iter(binds.drain(..)) })
                .right()
        }
    })
}

fn variant_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        token('`').with(typname()).skip(spaces()),
        optional(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(structure_pattern(), csep(), sptoken(')')),
        )),
    )
        .map(|(tag, binds): (ArcStr, Option<LPooled<Vec<StructurePattern>>>)| {
            let mut binds = match binds {
                None => LPooled::take(),
                Some(a) => a,
            };
            StructurePattern::Variant { all, tag, binds: Arc::from_iter(binds.drain(..)) }
        })
}

fn struct_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('{'),
        sptoken('}'),
        sep_by1_tok(
            spaces().with(choice((
                string("..").map(|_| (literal!(""), StructurePattern::Ignore, false)),
                attempt(fname().with(not_followed_by(sptoken(':')))).map(|s| {
                    let p = StructurePattern::Bind(s.clone());
                    (s, p, true)
                }),
                (fname().skip(sptoken(':')), structure_pattern())
                    .map(|(s, p)| (s, p, true)),
            ))),
            csep(),
            sptoken('}'),
        ),
    )
    .then(|mut binds: LPooled<Vec<(ArcStr, StructurePattern, bool)>>| {
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
            let binds = Arc::from_iter(binds.drain(..).map(|(s, p, _)| (s, p)));
            value(StructurePattern::Struct { all, exhaustive, binds }).right()
        }
    })
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
            slice_pattern(all),
            tuple_pattern(all),
            struct_pattern(all),
            variant_pattern(all),
            token('_').then(|_| {
                if all.is_some() {
                    unexpected_any("all patterns are not supported by _").left()
                } else {
                    value(StructurePattern::Ignore).right()
                }
            }),
            fname().map(|name| {
                if all.is_some() {
                    unexpected_any("all patterns are not supported by bind").left()
                } else {
                    value(StructurePattern::Bind(name)).right()
                }
            }),
            parse_value(&VAL_MUST_ESC, &VAL_ESC)
                .then(|v| {
                    if all.is_some() {
                        unexpected_any("all patterns are not supported by literals").left()
                    } else {
                        value(StructurePattern::Literal(v)).right()
                    }
                }),
        )))
    }
}

pub(crate) fn pattern<I>() -> impl Parser<I, Output = Pattern>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        optional(attempt(typexp().skip(space().with(spstring("as "))))),
        structure_pattern(),
        optional(attempt(space().with(spstring("if").with(space()).with(expr())))),
    )
        .map(
            |(type_predicate, structure_predicate, guard): (
                Option<Type>,
                StructurePattern,
                Option<Expr>,
            )| { Pattern { type_predicate, structure_predicate, guard } },
        )
}
