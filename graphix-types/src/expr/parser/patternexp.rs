use crate::{
    expr::{
        Expr, ExprKind, Name, Pattern, StructurePattern, WrittenAt,
        parser::{
            csep, expr, fldname,
            grow::{grow, refuse},
            ident, interpolated, is_reserved_binding, name, not_prefix, raw_string,
            sep_by_tok, sep_by1_tok, spaces, spaces1, spstring, sptoken, typ,
        },
    },
    typ::Type,
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice,
    error::StreamError,
    many, optional,
    parser::char::string,
    position,
    stream::{Range, StreamErrorFor, position::SourcePosition},
    token, value,
};
use netidx_value::{
    Value,
    parser::{VAL_ESC, VAL_MUST_ESC, value as parse_value},
};
use poolshark::local::LPooled;
use triomphe::Arc;

/// One element of a slice pattern: a pattern, or the rest `..` / `name..`.
enum SliceItem {
    Pat(StructurePattern),
    Rest(Option<Name>),
}

/// Classify a slice-shaped pattern's element/rest mix into Slice /
/// SlicePrefix / SliceSuffix. `list` selects the native-list flavor
/// `[<..>]`, which refuses the suffix form (a list's front is an O(n) walk).
pub(super) fn slice_pattern<I>(
    list: bool,
    all: Option<Name>,
) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let (open, close) = if list { ("[<", ">]") } else { ("[", "]") };
    let item = spaces().with(choice((
        string("..").map(|_| SliceItem::Rest(None)),
        attempt(name().skip(spstring(".."))).map(|n| SliceItem::Rest(Some(n))),
        structure_pattern_or().map(SliceItem::Pat),
    )));
    (
        between(
            attempt(string(open)),
            spstring(close),
            sep_by_tok(item, csep(), attempt(spstring(close))),
        ),
        position(),
    )
        .then(move |(mut items, end): (LPooled<Vec<SliceItem>>, _)| {
            let mut rest: Option<(usize, Option<Name>)> = None;
            let mut pats: LPooled<Vec<StructurePattern>> = LPooled::take();
            for (i, item) in items.drain(..).enumerate() {
                match item {
                    SliceItem::Pat(p) => pats.push(p),
                    SliceItem::Rest(n) if rest.is_none() => rest = Some((i, n)),
                    SliceItem::Rest(_) => {
                        return refuse(end, "a slice pattern has one rest (`..`)").right();
                    }
                }
            }
            let n = pats.len();
            let pats = Arc::from_iter(pats.drain(..));
            let all = all.clone();
            let pat = match rest {
                None => StructurePattern::Slice { list, all, binds: pats },
                Some((0, _)) if n == 0 => {
                    return refuse(end, "a rest (`..`) alone is not a slice pattern").right();
                }
                Some((0, _)) if list => {
                    return refuse(
                        end,
                        "list patterns have no suffix form (the tail is O(1), the front is not)",
                    )
                    .right();
                }
                Some((0, head)) => StructurePattern::SliceSuffix { all, head, suffix: pats },
                Some((i, tail)) if i == n => {
                    StructurePattern::SlicePrefix { list, all, tail, prefix: pats }
                }
                Some(_) => {
                    return refuse(end, "a slice pattern's rest (`..`) is first or last")
                        .right();
                }
            };
            value(pat).left()
        })
}

fn tuple_pattern<I>(all: Option<Name>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        between(
            token('('),
            sptoken(')'),
            sep_by1_tok(structure_pattern_or(), csep(), token(')')),
        ),
        position(),
    )
        .then(move |(mut binds, end): (LPooled<Vec<StructurePattern>>, _)| {
            if binds.len() < 2 {
                refuse(end, "tuples must have at least 2 elements").right()
            } else {
                let all = all.clone();
                let binds = Arc::from_iter(binds.drain(..));
                value(StructurePattern::Tuple { all, binds }).left()
            }
        })
}

fn variant_pattern<I>(all: Option<Name>) -> impl Parser<I, Output = StructurePattern>
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
            sep_by1_tok(structure_pattern_or(), csep(), token(')')),
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

fn abstract_pattern<I>(all: Option<Name>) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        attempt(super::typexp::typath().skip(spaces()).skip(token('('))),
        structure_pattern_or(),
        sptoken(')'),
    )
        .map(move |(name, bind, _)| StructurePattern::Abstract {
            all: all.clone(),
            name,
            bind: Arc::new(bind),
        })
}

pub(super) fn struct_pattern<I>(
    all: Option<Name>,
) -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    /// One entry between the braces.
    enum Field {
        /// `name: pattern`, or the shorthand `name` binding the field
        Named(ArcStr, StructurePattern, WrittenAt),
        /// `..`: the pattern is not exhaustive
        Rest,
    }
    let field = choice((
        string("..").map(|_| Field::Rest),
        (position(), fldname().skip(spaces()), optional(token(':').with(structure_pattern_or())))
            .and_then(|(pos, name, pat)| {
                let at = WrittenAt(pos);
                match pat {
                    Some(pat) => Ok(Field::Named(name, pat, at)),
                    None if is_reserved_binding(&name) => {
                        Err(StreamErrorFor::<I>::message_static_message(
                            "a reserved word field needs the explicit `name: pattern` form",
                        ))
                    }
                    None => {
                        let bind = StructurePattern::Bind(Name::written(name.clone(), pos));
                        Ok(Field::Named(name, bind, at))
                    }
                }
            }),
    ));
    (
        between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by1_tok(field, csep(), token('}'))),
        ),
        position(),
    )
        .then(move |(mut fields, end): (LPooled<Vec<Field>>, _)| {
            let exhaustive = !fields.iter().any(|f| matches!(f, Field::Rest));
            let mut binds: LPooled<Vec<(ArcStr, StructurePattern, WrittenAt)>> = fields
                .drain(..)
                .filter_map(|f| match f {
                    Field::Named(n, p, at) => Some((n, p, at)),
                    Field::Rest => None,
                })
                .collect();
            if super::sort_unique(&mut binds, |(n, _, _)| n) {
                let all = all.clone();
                let binds = Arc::from_iter(binds.drain(..));
                value(StructurePattern::Struct { all, exhaustive, binds }).left()
            } else {
                refuse(end, "struct fields must be unique").right()
            }
        })
}

/// A string pattern, lexed as an expression's string is.
fn string_pattern<I>() -> impl Parser<I, Output = Value>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (choice((raw_string(), interpolated())), position()).then(|(e, end): (Expr, _)| {
        match &e.kind {
            ExprKind::Constant(v @ Value::String(_)) => value(v.clone()).left(),
            _ => refuse(end, "a string pattern cannot interpolate").right(),
        }
    })
}

fn literal_pattern<I>() -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((string_pattern(), attempt(parse_value(&VAL_MUST_ESC, &VAL_ESC))))
        .skip(not_prefix())
        .map(StructurePattern::Literal)
}

fn all_pattern<I>() -> impl Parser<I, Output = Name>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    name().skip(sptoken('@')).skip(spaces())
}

parser! {
    pub(crate) fn structure_pattern[I]()(I) -> StructurePattern
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(spaces().with(optional(attempt(all_pattern()))).then(|all| {
            let captures = all.is_some();
            (
                choice((
                    slice_pattern(true, all.clone()),
                    slice_pattern(false, all.clone()),
                    tuple_pattern(all.clone()),
                    struct_pattern(all.clone()),
                    variant_pattern(all.clone()),
                    abstract_pattern(all.clone()),
                    token('_').map(|_| StructurePattern::Ignore),
                    literal_pattern(),
                    name().map(StructurePattern::Bind),
                )),
                position(),
            )
                .then(move |(pat, end)| match pat {
                    StructurePattern::Ignore
                    | StructurePattern::Literal(_)
                    | StructurePattern::Bind(_)
                        if captures =>
                    {
                        refuse(end, "a capture `name@` takes a structure pattern").right()
                    }
                    pat => value(pat).left(),
                })
        }))
    }
}

/// One or more `|`-separated alternatives. Legal in select arms and every
/// bracketed element position, not at the top level of `let` or lambda
/// params. `|` binds loosest; an `@`-capture is per-alternative; flat.
pub(super) fn structure_pattern_or<I>() -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (structure_pattern(), many(attempt(sptoken('|').with(structure_pattern())))).map(
        |(first, rest): (StructurePattern, Vec<StructurePattern>)| {
            if rest.is_empty() {
                first
            } else {
                StructurePattern::Or(Arc::from_iter(
                    std::iter::once(first).chain(rest.into_iter()),
                ))
            }
        },
    )
}

pub(crate) fn pattern<I>() -> impl Parser<I, Output = Pattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        optional(attempt(typ().skip(spaces1()).skip(string("as")).skip(spaces1()))),
        structure_pattern_or(),
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
