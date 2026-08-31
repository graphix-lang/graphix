use crate::{
    expr::{
        Expr, Pattern, StructurePattern,
        parser::{
            RESERVED_BINDING, csep, expr, fldname, fname, ident, sep_by_tok, sep_by1_tok,
            spaces, spaces1, spstring, sptoken, typ,
        },
    },
    typ::Type,
};
use ahash::AHashSet;
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, optional,
    parser::char::string,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use netidx_core::utils::Either;
use netidx_value::parser::{VAL_ESC, VAL_MUST_ESC, value as parse_value};
use poolshark::local::LPooled;
use triomphe::Arc;

use super::{grow::grow, not_prefix};

/// Shared post-processing for slice-shaped patterns: classify the
/// element/rest mix into Slice / SlicePrefix / SliceSuffix. `list`
/// selects the native-list flavor; the SUFFIX form is refused there —
/// a list's tail is O(1), its front is an O(n) walk
/// (`design/list_native.md`).
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
            attempt(sptoken(']')),
        ),
    )
    .then(move |mut pats: LPooled<Vec<Either<StructurePattern, Option<ArcStr>>>>| {
        let all = all.clone();
        if pats.len() == 0 {
            value(StructurePattern::Slice { list: false, all, binds: Arc::from_iter([]) })
                .right()
        } else if pats.len() == 1 {
            match pats.pop().unwrap() {
                Either::Left(s) => value(StructurePattern::Slice {
                    list: false,
                    all,
                    binds: Arc::from_iter([s]),
                })
                .right(),
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
                    value(StructurePattern::SlicePrefix {
                        list: false,
                        all,
                        tail,
                        prefix,
                    })
                    .right()
                }
                (Either::Left(_), Either::Left(_)) => value(StructurePattern::Slice {
                    list: false,
                    all,
                    binds: all_left!(pats),
                })
                .right(),
            }
        }
    })
}

/// The native-list pattern `[<..>]` — the list-flavored slice grammar.
pub(super) fn list_slice_pattern<I>(
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
        attempt(combine::parser::char::string("[<")),
        spstring(">]"),
        sep_by_tok(
            spaces().with(choice((
                string("..").map(|_| Either::Right(None)),
                attempt(fname().skip(spstring(".."))).map(|n| Either::Right(Some(n))),
                structure_pattern().map(|p| Either::Left(p)),
            ))),
            csep(),
            attempt(spstring(">]")),
        ),
    )
    .then(move |mut pats: LPooled<Vec<Either<StructurePattern, Option<ArcStr>>>>| {
        let all = all.clone();
        if pats.len() == 0 {
            value(StructurePattern::Slice { list: true, all, binds: Arc::from_iter([]) })
                .right()
        } else if pats.len() == 1 {
            match pats.pop().unwrap() {
                Either::Left(s) => value(StructurePattern::Slice {
                    list: true,
                    all,
                    binds: Arc::from_iter([s]),
                })
                .right(),
                Either::Right(_) => unexpected_any("invalid singular range match").left(),
            }
        } else {
            match (&pats[0], &pats[pats.len() - 1]) {
                (Either::Right(_), _) => unexpected_any(
                    "list patterns have no suffix form (the tail is O(1), the front is not)",
                )
                .left(),
                (Either::Left(_), Either::Right(_)) => {
                    let tail = pats.pop().unwrap().right().unwrap();
                    let prefix = all_left!(pats);
                    value(StructurePattern::SlicePrefix { list: true, all, tail, prefix })
                        .right()
                }
                (Either::Left(_), Either::Left(_)) => {
                    value(StructurePattern::Slice {
                        list: true,
                        all,
                        binds: all_left!(pats),
                    })
                    .right()
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
        token('`').with(ident(true)),
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

fn abstract_pattern<I>(all: Option<ArcStr>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        attempt(super::typexp::typath().skip(spaces()).skip(token('('))),
        structure_pattern(),
        sptoken(')'),
    )
        .map(move |(name, bind, _)| StructurePattern::Abstract {
            all: all.clone(),
            name,
            bind: Arc::new(bind),
        })
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
                fldname()
                    .skip(spaces())
                    .then(|name| {
                        optional(token(':').with(structure_pattern()))
                            .map(move |pat| (name.clone(), pat))
                    })
                    .then(|(name, pat)| match pat {
                        Some(pat) => value((name, pat, true)).left(),
                        None if RESERVED_BINDING.contains(&name.as_str()) => unexpected_any(
                            "a reserved word field needs the explicit `name: pattern` form",
                        )
                        .right(),
                        None => {
                            let pat = StructurePattern::Bind(name.clone());
                            value((name, pat, true)).left()
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
        let s = binds.iter().map(|(s, _, _)| s).collect::<LPooled<AHashSet<_>>>();
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
    attempt(parse_value(&VAL_MUST_ESC, &VAL_ESC)).skip(not_prefix()).then(move |v| {
        if all {
            unexpected_any("all patterns are not supported by literals").left()
        } else {
            value(StructurePattern::Literal(v)).right()
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
        grow(spaces().with(optional(attempt(all_pattern()))).then(|all| choice((
            list_slice_pattern(all.clone()),
            slice_pattern(all.clone()),
            tuple_pattern(all.clone()),
            struct_pattern(all.clone()),
            variant_pattern(all.clone()),
            abstract_pattern(all.clone()),
            underbar_pattern(all.is_some()),
            literal_pattern(all.is_some()),
            bind_pattern(all.is_some()),
        ))))
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
