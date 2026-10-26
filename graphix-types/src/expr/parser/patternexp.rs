use crate::{
    expr::{
        Expr, ExprKind, Name, Pattern, StructurePattern, WrittenAt, WrittenForm,
        parser::{
            ONE_TUPLE, csep, expr, fldname, fname_noting,
            grow::{grow, refusal, refuse},
            ident, interpolated, is_reserved_binding, name, not_prefix, raw_string,
            sep_by_tok, sep_by1_tok, spaces, spaces1, spstring, sptoken, typ,
        },
    },
    typ::Type,
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, many, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
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
                refuse(end, ONE_TUPLE).right()
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
        // CR claude for claude: [bug/medium] The type path's WrittenPath is discarded
        // here and in `construct` in parser/mod.rs. Neither StructurePattern::Abstract
        // nor ExprKind::Construct records a TypeRefSite when it resolves the typedef.
        // So LSP references on an abstract type `T` miss every `T(v)` and pattern
        // `T(p)`, and go-to-definition there returns nothing. Keep the WrittenPath in
        // both nodes, as ImplExpr keeps trait_at, and push a TypeRefSite where they
        // resolve the typedef under `env.ide.is_lsp()`. probe:
        // design/review-2026-10-26/repro/t-parser-b-abstract-ctor-refs.py
        // (t-parser-b-04)
        .map(move |((name, _), bind, _)| StructurePattern::Abstract {
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
        (
            position(),
            fldname().skip(spaces()),
            optional(token(':').with(structure_pattern_or())),
        )
            .and_then(|(pos, name, pat)| {
                let at = WrittenAt(pos);
                match pat {
                    Some(pat) => Ok(Field::Named(name, pat, at)),
                    None if is_reserved_binding(&name) => Err(refusal::<I>(
                        pos,
                        "a reserved word field needs the explicit `name: pattern` form",
                    )),
                    None => {
                        let bind =
                            StructurePattern::Bind(Name::written(name.clone(), pos));
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
            // one rest, and it ends the fields
            let rests = fields.iter().filter(|f| matches!(f, Field::Rest)).count();
            if rests > 1 || (rests == 1 && !matches!(fields.last(), Some(Field::Rest))) {
                return refuse(end, "`..` must be a struct pattern's last field").right();
            }
            let exhaustive = rests == 0;
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

/// A string pattern, lexed as an expression's string is, and how it was
/// delimited.
fn string_pattern<I>() -> impl Parser<I, Output = (Value, WrittenForm)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (choice((raw_string(), interpolated())), position()).then(|(e, end): (Expr, _)| {
        match &e.kind {
            ExprKind::Constant(v @ Value::String(_)) => {
                value((v.clone(), WrittenForm(e.str_form))).left()
            }
            _ => refuse(end, "a string pattern cannot interpolate").right(),
        }
    })
}

// CR claude for claude: [bug/low] A pattern literal gets the float range note
// (`finite`) but not the integer one. `select 1 { u8:300 => 0, _ => 1 }` is refused
// with 'Expected whitespaces or =>' and no reason, while `let a = u8:300` says "`300`
// is out of the range of u8". Put int_range_note behind the literal choice here as
// `literal` does. A `duration:` literal past its range
// (duration:99999999999999999999.s) is likewise refused with no reason. probe:
// design/review-2026-10-26/repro/x-rerun-4-pattern-int-range-note.gx (x-rerun-4-06)
fn literal_pattern<I>() -> impl Parser<I, Output = StructurePattern>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        choice((
            string_pattern(),
            attempt(parse_value(&VAL_MUST_ESC, &VAL_ESC))
                .map(|v| (v, WrittenForm::default())),
        )),
    )
        .skip(not_prefix())
        .and_then(|(pos, (v, form))| {
            super::finite::<I>(pos, v).map(|v| StructurePattern::Literal(v, form))
        })
}

fn all_pattern<I>() -> impl Parser<I, Output = Name>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    // a probe at every pattern: a reserved word here is a literal's
    (position(), fname_noting(false))
        .map(|(pos, n)| Name::written(n, pos))
        .skip(sptoken('@'))
        .skip(spaces())
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
                    | StructurePattern::Literal(..)
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
        position(),
        optional(attempt(typ().skip(spaces1()).skip(string("as")).skip(spaces1()))),
        structure_pattern_or(),
        position(),
        optional(attempt(spaces1().with(string("if")).with(spaces1()).with(expr()))),
    )
        .map(
            |(pos, type_predicate, structure_predicate, end, guard): (
                SourcePosition,
                Option<Type>,
                StructurePattern,
                SourcePosition,
                Option<Expr>,
            )| Pattern {
                type_predicate,
                structure_predicate,
                guard,
                pos: WrittenAt(pos),
                end: WrittenAt(end),
            },
        )
}
