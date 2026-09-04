use crate::{
    expr::{
        Attr, BindExpr, CatchExpr, Decorations, Doc, Expr, ExprKind, ModPath, Origin,
        ParserContext, Pattern, SelectExpr, Sig, SigItem, StructExpr, StructWithExpr,
        set_origin,
    },
    typ::{FnType, Type},
};
use ahash::AHashSet;
use arcstr::{ArcStr, literal};
use combine::{
    EasyParser, ParseError, Parser, RangeStream, attempt, between, choice, count_min_max,
    eof, look_ahead, many, many1, none_of, not_followed_by, optional,
    parser::token::produce,
    parser::{
        char::{space, string},
        combinator::recognize,
        range::{take_while, take_while1},
    },
    position, satisfy, sep_by1,
    stream::{
        Range,
        position::{self, SourcePosition},
    },
    token, unexpected_any, value,
};
use compact_str::CompactString;
use escaping::Escape;
use netidx_core::path::Path;
use netidx_value::Value;
use netidx_value::parser::{
    VAL_ESC, VAL_MUST_ESC, not_prefix, sep_by_tok, sep_by1_tok, value as parse_value,
};
use poolshark::local::LPooled;
use std::sync::LazyLock;
use triomphe::Arc;

mod grow;
use grow::grow;
pub use grow::{DEFAULT_MAX_NESTING, max_nesting, set_max_nesting};

mod interpolateexp;
use interpolateexp::interpolated;

mod modexp;
use modexp::{module, sig_item, use_module};

mod typexp;
pub(crate) use typexp::quantifier_names;
use typexp::{fntype, typ, typedef};

mod traitexp;
use traitexp::{impl_decl, trait_decl};

mod lambdaexp;
use lambdaexp::{apply_args, lambda};

mod arrayexp;
use arrayexp::{array, array_index_suffix, list_lit};

pub(crate) mod arithexp;
use arithexp::arith;

#[cfg(test)]
mod test;

mod patternexp;
use patternexp::{pattern, structure_pattern};

pub(super) fn escape_generic(c: char) -> bool {
    c.is_control()
}

pub const GRAPHIX_MUST_ESC: [char; 4] = ['"', '\\', '[', ']'];
pub static GRAPHIX_ESC: LazyLock<Escape> = LazyLock::new(|| {
    Escape::new(
        '\\',
        &['"', '\\', '[', ']', '\n', '\r', '\t', '\0'],
        &[('\n', "n"), ('\r', "r"), ('\t', "t"), ('\0', "0")],
        Some(escape_generic),
    )
    .unwrap()
});
/// The primitive TYPE-NAME keywords legal as binding names (2026-08-18)
/// — reserved-ness protects the places where they mean a type (type
/// expressions, typed literals like `duration:1.s`, `Type as`
/// patterns), and every such place is disambiguated by position or by
/// the `:`/`as` that must follow. Control keywords, literals, and the
/// expression forms stay reserved everywhere. `bytes` is the one
/// primitive that CANNOT bind: its literal payload is base64, whose
/// alphabet overlaps identifiers and admits short/empty payloads, so an
/// annotated bind (`let bytes: T = v`) is genuinely ambiguous with a
/// refutable literal-pattern let — the 32k round-trip hunt's find. It
/// remains a legal FIELD name (fields never meet the literal grammar).
/// NB `bytes` must stay in RESERVED even though it can't bind — it is
/// still a type name.
pub static TYPE_KEYWORDS: LazyLock<AHashSet<&str>> = LazyLock::new(|| {
    AHashSet::from_iter([
        "i8", "u8", "i16", "u16", "i32", "u32", "v32", "z32", "i64", "u64", "v64", "z64",
        "f32", "f64", "decimal", "datetime", "duration", "bool", "string",
    ])
});

pub static RESERVED: LazyLock<AHashSet<&str>> = LazyLock::new(|| {
    AHashSet::from_iter(
        [
            "true", "false", "ok", "null", "mod", "let", "select", "type", "fn", "cast",
            "never", "bytes", "if", "_", "?", "Array", "Map", "List", "any", "Any",
            "use", "rec", "catch", "try", "self", "super", "package", "pub", "trait",
            "impl", "seq", "until",
        ]
        .into_iter()
        .chain(TYPE_KEYWORDS.iter().copied()),
    )
});

/// The path-root keywords (design/module_system.md): legal only as the
/// LEADING segment(s) of a path — `self::x`, `super::super::x`,
/// `package::a::b` — and refused everywhere else an identifier could
/// appear (they are in [`RESERVED`]). `super` may repeat as a prefix;
/// `self` and `package` may not.
pub static PATH_KEYWORDS: LazyLock<AHashSet<&str>> =
    LazyLock::new(|| AHashSet::from_iter(["self", "super", "package"]));

/// The reserved words that BEGIN a construct — what a statement or
/// expression parser probes for first, so their refusal as a name is
/// routine and reports nothing.
pub static CONSTRUCT_KEYWORDS: LazyLock<AHashSet<&str>> = LazyLock::new(|| {
    AHashSet::from_iter([
        "mod", "let", "select", "type", "fn", "cast", "never", "if", "use", "rec",
        "catch", "try", "pub", "trait", "impl", "seq", "until",
    ])
});

/// The words refused in BINDING positions (`let`, params, labeled args,
/// pattern binds, module/val names): everything reserved except the
/// type-name keywords.
pub static RESERVED_BINDING: LazyLock<AHashSet<&str>> = LazyLock::new(|| {
    RESERVED.iter().copied().filter(|s| !TYPE_KEYWORDS.contains(s)).collect()
});

// sep_by1 but a separator terminator is allowed and mapped to an output value
pub fn sep_by1_tok_exp<I, O, OC, F, EP, SP, TP>(
    p: EP,
    sep: SP,
    term: TP,
    f: F,
) -> impl Parser<I, Output = OC>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    OC: Extend<O> + Default,
    SP: Parser<I>,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
    F: Fn(I::Position) -> O,
{
    sep_by1((position(), choice((look_ahead(term).map(|_| None::<O>), p.map(Some)))), sep)
        .map(move |mut e: LPooled<Vec<(_, Option<O>)>>| {
            let mut res = OC::default();
            res.extend(e.drain(..).map(|(pos, e)| match e {
                Some(e) => e,
                None => f(pos),
            }));
            res
        })
}

// Whitespace ONLY — `//` comments are never skipped. They are captured by
// `leading_decorations()`, at the `expr()` entry and ahead of the three
// non-expression heads that hand them to the expression below (a select
// arm's pattern, an impl method, a struct field's name — `decorate`), so
// a comment anywhere else (interior, trailing, dangling) is a parse
// error, which makes "every comment is preserved in the AST" structural.
fn spaces<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    combine::parser::char::spaces()
}

// Parse one own-line `//` comment line: its text (everything after `//` up to
// the newline) is kept verbatim so it round-trips. `///` is left untouched
// (handled by `doc_comment` in interface files; a syntax error in `.gx`).
// Trailing whitespace and blank lines after the line are skipped.
fn comment_line<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(
        (position(), string("//"), optional(attempt(token('/'))))
            .then(|(pos, _, doc)| match doc {
                Some(_) => {
                    grow::note_reason(
                        pos,
                        None,
                        compact_str::CompactString::const_new(
                            "`///` is a doc comment, legal only in a .gxi interface \
                             file; a .gx file comments with `//`",
                        ),
                    );
                    unexpected_any("doc comment").left()
                }
                None => value(()).right(),
            })
            .with(many::<String, _, _>(none_of(['\n']))),
    )
    .skip(combine::parser::char::spaces())
    .map(|s: String| ArcStr::from(s.as_str()))
}

// Capture the run of own-line `//` comment lines directly above an expression.
// The `.gxi` `sig_item` path uses this to tolerate `//` notes above a
// declaration; `.gx` expressions capture comments AND attributes via
// `leading_decorations`.
fn leading_comments<I>() -> impl Parser<I, Output = LPooled<Vec<ArcStr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    combine::parser::char::spaces().with(many(comment_line()))
}

// Parse a single `#[name]` or `#[name(arg, ...)]` attribute. The args are
// full expressions (so `#[foo(1 + 2, "x")]` is legal). An attribute is only
// ever consumed by `leading_decorations`, so it is legal exactly where a
// comment is. The leading `attempt(string("#["))` makes the branch
// backtrack cleanly when there is no attribute, so it never collides with a
// labeled call arg `#name` (which is `#` immediately followed by an ident).
fn attribute<I>() -> impl Parser<I, Output = Attr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        attempt(string("#[")).with(spaces().with(fname())),
        spaces().with(optional(between(
            token('('),
            sptoken(')'),
            sep_by_tok(expr(), csep(), attempt(sptoken(')'))),
        ))),
    )
        .skip(sptoken(']'))
        .map(|(name, args): (ArcStr, Option<LPooled<Vec<Expr>>>)| {
            let mut args = args.unwrap_or_else(LPooled::take);
            Attr { name, args: Arc::from_iter(args.drain(..)) }
        })
}

// Capture the run of own-line `//` comments and `#[..]` attributes directly
// above an expression (or one of the heads `decorate` names), returning them
// as two flat lists (comments, attrs). They may interleave in the source; the
// relative order between a comment and an attribute is not retained (each
// printer emits comments then attrs in a fixed order), which is fine because
// `Decorations` is invisible to `Expr` equality. `leading_comments` itself is
// kept for the `.gxi` `sig_item` path.
fn leading_decorations<I>() -> impl Parser<I, Output = Leading>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    enum Dec {
        Comment(ArcStr),
        Attr(Attr),
    }
    combine::parser::char::spaces()
        .with(many::<LPooled<Vec<Dec>>, _, _>(choice((
            comment_line().map(Dec::Comment),
            attribute().skip(combine::parser::char::spaces()).map(Dec::Attr),
        ))))
        .map(|mut items: LPooled<Vec<Dec>>| {
            let mut comments: LPooled<Vec<ArcStr>> = LPooled::take();
            let mut attrs: LPooled<Vec<Attr>> = LPooled::take();
            for d in items.drain(..) {
                match d {
                    Dec::Comment(c) => comments.push(c),
                    Dec::Attr(a) => attrs.push(a),
                }
            }
            (comments, attrs)
        })
}

/// The comments and attributes `leading_decorations` captured, in
/// source order within each list.
type Leading = (LPooled<Vec<ArcStr>>, LPooled<Vec<Attr>>);

/// Give `e` the decorations captured directly above it. The capture
/// point need not be the expression itself: what sits above a select
/// arm's pattern, an impl method, or a struct field's name belongs to
/// the expression that follows it — the arm's body, the method's
/// binding, the field's value — ahead of anything that expression
/// captured for itself, and the printers put it back above the pattern
/// or the name.
fn decorate(mut e: Expr, (mut comments, mut attrs): Leading) -> Expr {
    if comments.is_empty() && attrs.is_empty() {
        return e;
    }
    if let Some(own) = e.dec.take() {
        let Decorations { comments: c, attrs: a } = *own;
        comments.extend(c.iter().cloned());
        attrs.extend(a.iter().cloned());
    }
    e.dec = Some(Box::new(Decorations {
        comments: Arc::from_iter(comments.drain(..)),
        attrs: Arc::from_iter(attrs.drain(..)),
    }));
    e
}

fn spaces1<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    space().with(spaces())
}

fn doc_comment<I>() -> impl Parser<I, Output = Doc>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    combine::parser::char::spaces()
        .with(many(
            string("///")
                .with(many(none_of(['\n'])))
                .skip(combine::parser::char::spaces()),
        ))
        .map(|lines: LPooled<Vec<String>>| {
            if lines.len() == 0 {
                Doc(None)
            } else {
                Doc(Some(ArcStr::from(lines.join("\n"))))
            }
        })
}

fn spstring<'a, I>(s: &'static str) -> impl Parser<I, Output = &'a str>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(string(s))
}

fn ident<I>(cap: bool) -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    recognize((
        take_while1(move |c: char| c.is_alphabetic() && cap == c.is_uppercase()),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))
    .map(|s: CompactString| ArcStr::from(s.as_str()))
}

fn fname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), ident(false)).then(|(pos, s): (SourcePosition, ArcStr)| {
        if RESERVED_BINDING.contains(&s.as_str()) {
            // A construct keyword (`select`, `let`, `mod`, …) is refused
            // here whenever an alternative probes a statement's first
            // token as a name, which is ordinary parsing, not a mistake
            // worth a note; the words that never begin a construct are
            // the ones a program meant as names.
            if !CONSTRUCT_KEYWORDS.contains(&s.as_str()) {
                grow::note_reason(
                    pos,
                    Some(s.chars().count()),
                    compact_str::format_compact!(
                        "`{s}` is a reserved word and cannot be used as a name"
                    ),
                );
            }
            unexpected_any("can't use keyword as a function or variable name").left()
        } else {
            value(s).right()
        }
    })
}

fn spfname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(fname())
}

/// A struct FIELD name: any lowercase-initial identifier, reserved words
/// included. Reserved-ness protects bindings and type names; a field is
/// neither, and mirrors of external data want `duration`/`string`/`bool`
/// as fields. A keyword field must use the explicit `name: …` form —
/// shorthand refers to a binding, which a keyword cannot name — enforced
/// by the callers that accept shorthand.
fn fldname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(false)
}

fn spfldname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(fldname())
}

fn typname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(true).then(|s| {
        if RESERVED.contains(&s.as_str()) {
            unexpected_any("can't use keyword as a type name").left()
        } else {
            value(s).right()
        }
    })
}

/// A path's optional keyword ROOT (design/module_system.md): `self::`,
/// `package::`, or a chain of `super::`s. Yields the keyword segments
/// consumed (empty when the path starts with an ordinary name). Each
/// alternative is attempted WITH its following `::`, so an identifier
/// that merely starts with a keyword (`packaged`) backtracks cleanly
/// to `fname` — which itself refuses the bare keywords, keeping them
/// leading-only.
fn path_root<I>() -> impl Parser<I, Output = LPooled<Vec<ArcStr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(string("package").with(string("::"))).map(|_| {
            let mut v: LPooled<Vec<ArcStr>> = LPooled::take();
            v.push(literal!("package"));
            v
        }),
        attempt(string("self").with(string("::"))).map(|_| {
            let mut v: LPooled<Vec<ArcStr>> = LPooled::take();
            v.push(literal!("self"));
            v
        }),
        many1::<LPooled<Vec<_>>, _, _>(
            attempt(string("super").with(string("::"))).map(|_| literal!("super")),
        ),
        produce(|| LPooled::take()),
    ))
}

pub(crate) fn modpath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (path_root(), sep_by1(fname(), string("::"))).map(
        |(mut root, mut v): (LPooled<Vec<ArcStr>>, LPooled<Vec<ArcStr>>)| {
            root.extend(v.drain(..));
            ModPath(Path::from_iter(root.drain(..)))
        },
    )
}

fn spmodpath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(modpath())
}

fn csep<I>() -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces().with(token(','))).skip(spaces())
}

fn semisep<I>() -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces().with(token(';'))).skip(spaces())
}

fn sptoken<I>(t: char) -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(token(t))
}

fn do_block<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(
            token('{'),
            sptoken('}'),
            sep_by1_tok_exp(expr(), semisep(), token('}'), |pos| {
                ExprKind::NoOp.to_expr(pos)
            }),
        ),
    )
        .then(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            if args.len() < 2 {
                unexpected_any("do must contain at least 2 expressions").left()
            } else {
                let exprs = Arc::from_iter(args.drain(..));
                value(ExprKind::Do { exprs }.to_expr(pos)).right()
            }
        })
}

fn ref_pexp<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        between(attempt(sptoken('(')), sptoken(')'), expr()),
        spaces().with(qop(reference())),
    ))
}

/// `never<T>(args…)` / `never(args…)`: the value that never arrives.
fn never_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("never").skip(not_prefix())),
        optional(attempt(between(sptoken('<'), sptoken('>'), typ()))),
        between(
            sptoken('('),
            sptoken(')'),
            sep_by_tok(expr(), csep(), attempt(sptoken(')'))),
        ),
    )
        .map(|(pos, _, typ, mut args): (_, _, Option<Type>, LPooled<Vec<Expr>>)| {
            ExprKind::Never { typ, args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

fn any<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("any").skip(not_prefix())).with(between(
            token('('),
            sptoken(')'),
            sep_by_tok(expr(), csep(), attempt(sptoken(')'))),
        )),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            ExprKind::Any { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

pub(super) fn letbind<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("let").skip(spaces1()))
            .with((
                optional(attempt(string("rec").with(spaces1()))),
                structure_pattern().skip(
                    optional(attempt(spaces().with(token('|')))).then(|t| match t {
                        Some(_) => {
                            unexpected_any("or-patterns are only legal in select arms")
                                .left()
                        }
                        None => value(()).right(),
                    }),
                ),
                spaces().with(optional(token(':').with(typ()))),
            ))
            .skip(sptoken('=')),
        expr(),
    )
        .map(|(pos, (rec, pattern, typ), value)| {
            let rec = rec.is_some();
            ExprKind::Bind(Arc::new(BindExpr { rec, pattern, typ, value })).to_expr(pos)
        })
}

fn connect<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), optional(token('*')), spmodpath().skip(spstring("<-")), expr()).map(
        |(pos, deref, name, e)| {
            ExprKind::Connect { name, value: Arc::new(e), deref: deref.is_some() }
                .to_expr(pos)
        },
    )
}

fn literal<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    // `parse_value` is netidx's own recursive-descent value parser —
    // `[[[…]]]` is a valid nested Value literal, and its recursion is
    // outside this crate, so it neither counts against `max_nesting`
    // nor claims segments of its own. `grow` gives it headroom at the
    // boundary; bounding it properly needs the same treatment in
    // netidx-value.
    // A quoted string is `interpolated()`'s alone: parsed here it
    // would consume the whole literal before the refusal, and the
    // failure would be reported past it.
    attempt(
        grow((
            position(),
            not_followed_by(token('"')),
            parse_value(&VAL_MUST_ESC, &VAL_ESC).skip(not_prefix()),
        ))
        .map(|(pos, _, v)| ExprKind::Constant(v).to_expr(pos)),
    )
    .or(grow(duration_unit_note()))
}

/// A diagnostic arm behind the literal parser: a `duration:` literal
/// whose unit is not one of netidx's names its unit, since the value
/// parser accepts the longest unit prefix (`min` parses as `m` plus
/// `in`) and the failure lands on the letters after it.
fn duration_unit_note<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    const UNITS: [&str; 9] = ["ns", "us", "ms", "s", "m", "h", "d", "M", "y"];
    (
        attempt(string("duration:")),
        many1::<CompactString, _, _>(satisfy(|c: char| {
            c.is_ascii_digit() || c == '.' || c == '-' || c == '+'
        })),
        position(),
        many1::<CompactString, _, _>(satisfy(|c: char| c.is_alphabetic())),
    )
        .then(|(_, _, pos, unit): (_, CompactString, _, CompactString)| {
            if !UNITS.contains(&unit.as_str()) {
                grow::note_reason(
                    pos,
                    None,
                    compact_str::format_compact!(
                        "`{unit}` is not a duration unit; the units are ns, us, ms, \
                         s, m, h, d, M and y (`duration:30.m`)"
                    ),
                );
            }
            unexpected_any("duration literal").map(|_: ()| unreachable!())
        })
}

/// A value path: `x`, `m::x`, `Trait::m` (an uppercase interior
/// segment names a trait, whose methods are reached like a module's
/// items), or the bare receiver name `self` of an impl method.
fn valpath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(string("self").skip(not_prefix()).skip(not_followed_by(string("::"))))
            .map(|_| ModPath::from([literal!("self")])),
        (path_root(), sep_by1(choice((fname(), typname())), string("::"))).then(
            |(mut root, mut v): (LPooled<Vec<ArcStr>>, LPooled<Vec<ArcStr>>)| {
                let terminal_is_value = v
                    .last()
                    .and_then(|s| s.chars().next())
                    .map(|c| c.is_lowercase())
                    .unwrap_or(false);
                if !terminal_is_value {
                    return unexpected_any("expected a value name").left();
                }
                root.extend(v.drain(..));
                value(ModPath(Path::from_iter(root.drain(..)))).right()
            },
        ),
    ))
}

fn reference<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), valpath()).map(|(pos, name)| ExprKind::Ref { name }.to_expr(pos))
}

fn qop<I, P>(p: P) -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I, Output = Expr>,
{
    enum Op {
        Qop,
        OrNever,
    }
    (
        position(),
        p,
        optional(attempt(spaces().with(choice((
            token('?').map(|_| Op::Qop),
            token('$').map(|_| Op::OrNever),
        ))))),
    )
        .map(|(pos, e, qop)| match qop {
            None => e,
            Some(Op::Qop) => ExprKind::Qop(Arc::new(e)).to_expr(pos),
            Some(Op::OrNever) => ExprKind::OrNever(Arc::new(e)).to_expr(pos),
        })
}

/// Rust-style raw strings: `r"…"`, `r#"…"#`, `r##"…"##`, … — NO
/// escapes at all (that is the point: every string is representable by
/// choosing enough hashes; the old `r'…'` form's `\'` escape made the
/// two-character sequence `\'` itself unrepresentable). The content
/// ends at the FIRST `"` followed by the opener's hash count. No
/// interpolation, no newline stripping — verbatim.
fn raw_string<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt((token('r'), many::<String, _, _>(token('#')), token('"'))))
        .then(|(pos, (_, hashes, _)): (_, (_, String, _))| {
            let n = hashes.len();
            (
                many::<String, _, _>(choice((
                    satisfy(|c| c != '"'),
                    attempt(
                        token('"').skip(not_followed_by(
                            count_min_max::<Vec<char>, _, _>(n, n, token('#'))
                                .map(|_| "raw string terminator"),
                        )),
                    ),
                ))),
                token('"'),
                count_min_max::<Vec<char>, _, _>(n, n, token('#')),
            )
                .map(move |(s, _, _): (String, _, _)| (pos, s))
        })
        .map(|(pos, s)| ExprKind::Constant(Value::String(s.into())).to_expr(pos))
}

fn until_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    // `attempt` covers the leading spaces so a non-until body item
    // (after `{` or `;`) backtracks into `expr()`. `until` is not in
    // `expr()`, so a comment above it is a parse error — same as a
    // comment above `let` if decorations had not already run.
    attempt(
        spaces().with(
            (position(), string("until").skip(not_prefix()).with(spaces1()).with(expr()))
                .map(|(pos, e)| ExprKind::Until(Arc::new(e)).to_expr(pos)),
        ),
    )
}

fn seq_body_item<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((until_expr(), expr()))
}

pub(super) fn seq<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("seq").skip(not_prefix())),
        spaces(),
        optional(attempt(
            not_followed_by(token('{'))
                .with(choice((between(token('('), sptoken(')'), expr()), reference()))),
        )),
        between(
            sptoken('{'),
            sptoken('}'),
            sep_by1_tok_exp(seq_body_item(), semisep(), token('}'), |pos| {
                ExprKind::NoOp.to_expr(pos)
            }),
        ),
    )
        .then(
            |(pos, _, _, trigger, mut body): (
                _,
                _,
                _,
                Option<Expr>,
                LPooled<Vec<Expr>>,
            )| {
                if body.is_empty()
                    || (body.len() == 1 && matches!(body[0].kind, ExprKind::NoOp))
                {
                    unexpected_any("a seq block must contain at least one step").left()
                } else {
                    let body = Arc::from_iter(body.drain(..));
                    value(
                        ExprKind::Seq { trigger: trigger.map(Arc::new), body }
                            .to_expr(pos),
                    )
                    .right()
                }
            },
        )
}

fn select<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("select").with(not_prefix())).with(spaces1()).with((
            expr(),
            between(
                sptoken('{'),
                sptoken('}'),
                spaces().with(sep_by1_tok(
                    (leading_decorations(), pattern(), spstring("=>").with(expr()))
                        .map(|(dec, pat, body)| (pat, decorate(body, dec))),
                    csep(),
                    token('}'),
                )),
            ),
        )),
    )
        .map(|(pos, (arg, mut arms)): (_, (Expr, LPooled<Vec<(Pattern, Expr)>>))| {
            ExprKind::Select(SelectExpr {
                arg: Arc::new(arg),
                arms: Arc::from_iter(arms.drain(..)),
            })
            .to_expr(pos)
        })
}

fn cast<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("cast").skip(not_prefix())).with(between(
            sptoken('<'),
            sptoken('>'),
            typ(),
        )),
        between(sptoken('('), sptoken(')'), expr()),
    )
        .map(|(pos, typ, e)| ExprKind::TypeCast { expr: Arc::new(e), typ }.to_expr(pos))
}

/// The `name: value, name, ..` field list of a struct literal or a
/// functional update: names unique (a reserved word needs the explicit
/// form — it cannot be a reference), sorted by name; decorations above
/// a field attach to its value; a shorthand's reference is minted at
/// the field's own position.
fn struct_fields<I>() -> impl Parser<I, Output = LPooled<Vec<(ArcStr, Expr)>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let field = (
        leading_decorations(),
        position(),
        fldname(),
        spaces().with(optional(token(':').with(expr()))),
    )
        .then(|(dec, pos, name, v): (Leading, _, ArcStr, Option<Expr>)| {
            let v = match v {
                Some(v) => v,
                None if RESERVED_BINDING.contains(&name.as_str()) => {
                    return unexpected_any(
                        "a reserved word field needs the explicit `name: value` form",
                    )
                    .left();
                }
                None => {
                    ExprKind::Ref { name: ModPath::from([name.clone()]) }.to_expr(pos)
                }
            };
            value((name, decorate(v, dec))).right()
        });
    sep_by1_tok(field, csep(), token('}')).then(
        |mut fields: LPooled<Vec<(ArcStr, Expr)>>| {
            let names = fields.iter().map(|(n, _)| n).collect::<LPooled<AHashSet<_>>>();
            if names.len() < fields.len() {
                return unexpected_any("struct fields must be unique").left();
            }
            drop(names);
            fields.sort_by_key(|(n, _)| n.clone());
            value(fields).right()
        },
    )
}

fn structure<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), between(token('{'), sptoken('}'), spaces().with(struct_fields()))).map(
        |(pos, mut fields): (_, LPooled<Vec<(ArcStr, Expr)>>)| {
            ExprKind::Struct(StructExpr { args: Arc::from_iter(fields.drain(..)) })
                .to_expr(pos)
        },
    )
}

fn map<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(
            token('{'),
            sptoken('}'),
            sep_by_tok(
                (expr(), spstring("=>").with(expr())),
                csep(),
                attempt(sptoken('}')),
            ),
        ),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<(Expr, Expr)>>)| {
            ExprKind::Map { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

fn variant<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        token('`').with(ident(true)),
        spaces().with(optional(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(expr(), csep(), token(')')),
        ))),
    )
        .map(|(pos, tag, args): (_, ArcStr, Option<LPooled<Vec<Expr>>>)| {
            let mut args = match args {
                None => LPooled::take(),
                Some(a) => a,
            };
            ExprKind::Variant { tag, args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

/// `T(v)` — a constructor call of the abstract type at the capitalized
/// path `T`. A capitalized last segment is what tells it from a call
/// (bindings can't be capitalized).
fn construct<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(typexp::typath().skip(spaces()).skip(token('('))),
        expr(),
        sptoken(')'),
    )
        .map(|(pos, name, arg, _)| {
            ExprKind::Construct { name, arg: Arc::new(arg) }.to_expr(pos)
        })
}

fn structwith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(
            token('{'),
            sptoken('}'),
            (
                ref_pexp().skip(space()).skip(spstring("with")).skip(space()),
                struct_fields(),
            ),
        ),
    )
        .map(
            |(pos, (source, mut fields)): (_, (Expr, LPooled<Vec<(ArcStr, Expr)>>))| {
                ExprKind::StructWith(StructWithExpr {
                    source: Arc::new(source),
                    replace: Arc::from_iter(fields.drain(..)),
                })
                .to_expr(pos)
            },
        )
}

fn catch_stmt<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position().skip(attempt(string("catch").skip(not_prefix()))),
        between(
            sptoken('('),
            sptoken(')'),
            (spfname(), spaces().with(optional(token(':').with(typ())))),
        ),
        expr(),
    )
        .map(|(pos, (bind, constraint), handler)| {
            ExprKind::Catch(Arc::new(CatchExpr {
                bind,
                constraint,
                handler: Arc::new(handler),
            }))
            .to_expr(pos)
        })
}

/// try/catch was removed from the language (2026-08-06,
/// design/catch.md); `try` stays reserved so old code gets a
/// direction instead of a confusing generic parse failure.
fn try_removed<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(string("try").skip(space())).then(|_| {
        unexpected_any("try/catch was removed; install a handler with `catch(e) expr` covering the rest of its enclosing block")
    })
}

fn byref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('&').with(expr()))
        .map(|(pos, expr)| ExprKind::ByRef(Arc::new(expr)).to_expr(pos))
}

fn deref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('*').with(expr()))
        .map(|(pos, expr)| ExprKind::Deref(Arc::new(expr)).to_expr(pos))
}

parser! {
    fn expr[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow((
            leading_decorations(),
            choice((
                module(),
                use_module(),
                catch_stmt(),
                try_removed(),
                typedef(),
                trait_decl(),
                impl_decl(),
                letbind(),
                attempt(lambda()),
                attempt(connect()),
                attempt(arith()),
                byref(),
                qop(deref()),
                qop((position(), between(token('('), sptoken(')'), expr())).map(|(pos, e)| {
                    ExprKind::ExplicitParens(Arc::new(e)).to_expr(pos)
                })),
                attempt(literal()),
                qop(reference()),
            )),
        )
            .map(|(dec, e): (Leading, Expr)| decorate(e, dec)))
    }
}

/// Parse one or more expressions
///
/// followed by (optional) whitespace and then eof. At least one
/// expression is required otherwise this function will fail.
pub fn parse(ori: Origin) -> anyhow::Result<Arc<[Expr]>> {
    let ori = Arc::new(ori);
    set_origin(ori.clone());
    let mut r: LPooled<Vec<Expr>> = grow::parsing(&ori.text, || {
        sep_by1_tok_exp(expr(), semisep(), eof(), |pos| ExprKind::NoOp.to_expr(pos))
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(&*ori.text))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e).context(ParserContext { ori: ori.clone(), pos })
    })?;
    Ok(Arc::from_iter(r.drain(..)))
}

/// Parse one or more signature expressions
///
/// followed by (optional) whitespace and then eof. At least one
/// expression is required otherwise this function will fail.
pub fn parse_sig(ori: Origin) -> anyhow::Result<Sig> {
    let ori = Arc::new(ori);
    set_origin(ori.clone());
    let mut r: LPooled<Vec<SigItem>> = grow::parsing(&ori.text, || {
        sep_by1_tok(sig_item(), semisep(), eof())
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(&*ori.text))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e).context(ParserContext { ori: ori.clone(), pos })
    })?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)) })
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    grow::parsing(s, || {
        expr()
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(s))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e)
            .context(ParserContext { ori: Arc::new(Origin::from_str(s)), pos })
    })
}

#[cfg(test)]
pub fn test_parse_mapref(s: &str) -> anyhow::Result<Expr> {
    arithexp::arith_term()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(&*s))
        .map(|(r, _)| r)
        .map_err(|e| {
            anyhow::anyhow!("{e}").context(ParserContext {
                ori: Arc::new(Origin::from_str(s)),
                pos: e.position,
            })
        })
}

/// Parse one fntype expression
pub fn parse_fn_type(s: &str) -> anyhow::Result<FnType> {
    grow::parsing(s, || {
        fntype()
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(s))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e)
            .context(ParserContext { ori: Arc::new(Origin::from_str(s)), pos })
    })
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    grow::parsing(s, || {
        typ()
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(s))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e)
            .context(ParserContext { ori: Arc::new(Origin::from_str(s)), pos })
    })
}

pub(super) fn parse_modpath(s: &str) -> anyhow::Result<ModPath> {
    grow::parsing(s, || {
        modpath()
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(s))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e)
            .context(ParserContext { ori: Arc::new(Origin::from_str(s)), pos })
    })
}
