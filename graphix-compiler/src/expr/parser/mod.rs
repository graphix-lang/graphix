use crate::{
    expr::{
        Attr, BindExpr, CatchExpr, Decorations, Doc, Expr, ExprKind, ModPath, Name,
        Origin, OriginScope, ParserContext, Pattern, SelectExpr, SeqTrigger, Sig,
        SigItem, StrForm, StructExpr, StructWithExpr, TryWithExpr,
    },
    profile::{self, Phase},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use arcstr::{ArcStr, literal};
use combine::{
    EasyParser, ParseError, Parser, RangeStream, attempt, between, choice, count_min_max,
    easy, eof, look_ahead, many, many1, none_of, not_followed_by, optional,
    parser::{
        char::{space, string},
        combinator::recognize,
        range::{take_while, take_while1},
        token::produce,
    },
    position, satisfy, sep_by1,
    stream::{
        Range,
        position::{self, SourcePosition},
    },
    token, unexpected_any, value,
    error::StreamError,
    stream::StreamErrorFor,
};
use compact_str::{CompactString, format_compact};
use escaping::Escape;
use netidx_core::path::Path;
use netidx_value::{
    Value,
    parser::{
        VAL_ESC, VAL_MUST_ESC, not_prefix, sep_by_tok, value as parse_value,
    },
};
use poolshark::local::LPooled;
use smallvec::SmallVec;
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
#[cfg(test)]
pub(crate) use typexp::declared_fn_type;
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

pub const GRAPHIX_MUST_ESC: [char; 4] = ['"', '\\', '[', ']'];
pub static GRAPHIX_ESC: LazyLock<Escape> = LazyLock::new(|| {
    const NAMED: [(char, &str); 4] = [('\n', "n"), ('\r', "r"), ('\t', "t"), ('\0', "0")];
    let esc: SmallVec<[char; 8]> =
        GRAPHIX_MUST_ESC.into_iter().chain(NAMED.map(|(c, _)| c)).collect();
    Escape::new('\\', &esc, &NAMED, Some(char::is_control)).unwrap()
});

/// How a reserved word may be used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keyword {
    /// begins a construct: refusing it as a name is routine and reports
    /// nothing
    Construct,
    /// never a name
    Reserved,
    /// a primitive type name, legal as a binding name: every place it
    /// means a type is disambiguated by position or a following `:`/`as`
    Type,
}

/// `bytes` is not a [`Keyword::Type`]: `let bytes: T = v` is ambiguous
/// with a base64 literal pattern.
static KEYWORDS: LazyLock<AHashMap<&str, Keyword>> = LazyLock::new(|| {
    use Keyword::*;
    let construct = [
        "mod", "let", "select", "type", "fn", "cast", "never", "if", "use", "rec",
        "catch", "try", "pub", "trait", "impl", "seq", "seqq", "until",
    ];
    let reserved = [
        "true", "false", "ok", "null", "bytes", "Array", "Map", "List", "any", "Any",
        "self", "super", "package", "abort",
    ];
    let typ = [
        "i8", "u8", "i16", "u16", "i32", "u32", "v32", "z32", "i64", "u64", "v64", "z64",
        "f32", "f64", "decimal", "datetime", "duration", "bool", "string",
    ];
    construct
        .map(|w| (w, Construct))
        .into_iter()
        .chain(reserved.map(|w| (w, Reserved)))
        .chain(typ.map(|w| (w, Type)))
        .collect()
});

/// A reserved word: never a type name, and a name only if a type keyword.
pub fn is_reserved(s: &str) -> bool {
    KEYWORDS.contains_key(s)
}

/// A word refused in BINDING positions (`let`, params, labeled args,
/// pattern binds, module/val names): every reserved word but the
/// primitive type names.
pub fn is_reserved_binding(s: &str) -> bool {
    matches!(KEYWORDS.get(s), Some(Keyword::Construct | Keyword::Reserved))
}

/// A letter that can begin a value name: any but an uppercase one, which
/// begins a type name, so a caseless script names values.
fn is_value_initial(c: char) -> bool {
    c.is_alphabetic() && !c.is_uppercase()
}

/// A terminator ahead, past any whitespace; consumes nothing.
fn ahead<I, P>(term: P) -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I>,
{
    look_ahead(attempt(spaces().with(term))).map(|_| ())
}

/// One or more `p` separated by `sep`, a trailing `sep` before `term`
/// allowed.
fn sep_by1_tok<I, O, OC, EP, SP, TP>(p: EP, sep: SP, term: TP) -> impl Parser<I, Output = OC>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    OC: Extend<O> + Default,
    SP: Parser<I>,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
{
    sep_by1(choice((ahead(term).map(|_| None::<O>), p.map(Some))), sep).and_then(
        |mut items: LPooled<Vec<Option<O>>>| match items.first() {
            Some(Some(_)) => {
                let mut res = OC::default();
                res.extend(items.drain(..).flatten());
                Ok(res)
            }
            _ => Err(<StreamErrorFor<I>>::message_static_message("expected an item")),
        },
    )
}

/// `sep_by1` of statements, an empty one standing for what `f` makes of
/// its position: a list may end with its separator, and be empty.
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
    sep_by1((position(), choice((ahead(term).map(|_| None::<O>), p.map(Some)))), sep)
        .map(move |mut e: LPooled<Vec<(_, Option<O>)>>| {
            let mut res = OC::default();
            res.extend(e.drain(..).map(|(pos, e)| match e {
                Some(e) => e,
                None => f(pos),
            }));
            res
        })
}

// Whitespace only: `//` comments are never skipped, so a comment anywhere
// `leading_decorations()` does not run is a parse error.
fn spaces<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    combine::parser::char::spaces()
}

/// The rest of the line.
fn line_text<I>() -> impl Parser<I, Output = CompactString>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    many(none_of(['\n']))
}

// One own-line `//` comment line, text kept verbatim. `///` is left for
// `doc_comment`.
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
                        CompactString::const_new(
                            "`///` is a doc comment, legal only in a .gxi interface \
                             file; a .gx file comments with `//`",
                        ),
                    );
                    unexpected_any("doc comment").left()
                }
                None => value(()).right(),
            })
            .with(line_text()),
    )
    .skip(spaces())
    .map(|s| ArcStr::from(s.as_str()))
}

fn leading_comments<I>() -> impl Parser<I, Output = LPooled<Vec<ArcStr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(many(comment_line()))
}

// `#[name]` or `#[name(arg, ...)]`; the args are full expressions. The
// `attempt` on `#[` keeps a labeled call arg `#name` from colliding.
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

// The own-line `//` comments and `#[..]` attributes directly above an
// expression, as two flat lists; their relative interleaving is not kept.
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
    spaces()
        .with(many::<LPooled<Vec<Dec>>, _, _>(choice((
            comment_line().map(Dec::Comment),
            attribute().skip(spaces()).map(Dec::Attr),
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

/// Give `e` the decorations captured directly above it, ahead of any it
/// captured for itself. What sits above a select arm's pattern, an impl
/// method or a struct field name belongs to the expression that follows.
fn decorate(mut e: Expr, (mut comments, mut attrs): Leading) -> Expr {
    if comments.is_empty() && attrs.is_empty() {
        return e;
    }
    if let Some(own) = e.dec.take() {
        comments.extend(own.comments.iter().cloned());
        attrs.extend(own.attrs.iter().cloned());
    }
    e.dec = Some(Arc::new(Decorations {
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
    /// Doc lines joined by newlines.
    #[derive(Default)]
    struct Lines(Option<String>);
    impl Extend<CompactString> for Lines {
        fn extend<T: IntoIterator<Item = CompactString>>(&mut self, lines: T) {
            for l in lines {
                match &mut self.0 {
                    None => self.0 = Some(String::from(l.as_str())),
                    Some(s) => {
                        s.push('\n');
                        s.push_str(&l)
                    }
                }
            }
        }
    }
    spaces()
        .with(many(string("///").with(line_text()).skip(spaces())))
        .map(|Lines(doc)| Doc(doc.map(ArcStr::from)))
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
        take_while1(move |c: char| match cap {
            true => c.is_uppercase(),
            false => is_value_initial(c),
        }),
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
        if is_reserved_binding(&s) {
            // Probing a statement's first token as a name is ordinary
            // parsing; only words that never begin a construct earn a note.
            if KEYWORDS.get(s.as_str()) != Some(&Keyword::Construct) {
                grow::note_reason(
                    pos,
                    Some(s.chars().count()),
                    format_compact!("`{s}` is a reserved word and cannot be used as a name"),
                );
            }
            unexpected_any("can't use keyword as a function or variable name").left()
        } else {
            value(s).right()
        }
    })
}

/// A binding name and where it stands.
fn name<I>() -> impl Parser<I, Output = Name>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), fname()).map(|(pos, name)| Name::written(name, pos))
}

fn spname<I>() -> impl Parser<I, Output = Name>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(name())
}

fn fldname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(false)
}

fn typname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(true).then(|s| {
        if is_reserved(&s) {
            unexpected_any("can't use keyword as a type name").left()
        } else {
            value(s).right()
        }
    })
}

/// A path's optional keyword root: `self::`, `package::`, or a chain of
/// `super::`s. Yields the keyword segments consumed. Each alternative is
/// attempted with its `::` so an identifier like `packaged` backtracks.
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

/// `let [rec] pattern[: type] = value`, with the value read by `value`:
/// the full `expr()` for a binding, the head form for a seq trigger.
fn letbind_with<I, P>(val: P) -> impl Parser<I, Output = BindExpr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I, Output = Expr>,
{
    attempt(string("let").skip(spaces1()))
        .with((
            optional(attempt(string("rec").with(spaces1()))).map(|r| r.is_some()),
            structure_pattern().skip(optional(attempt(spaces().with(token('|')))).then(
                |t| match t {
                    Some(_) => {
                        unexpected_any("or-patterns are only legal in select arms").left()
                    }
                    None => value(()).right(),
                },
            )),
            spaces().with(optional(token(':').with(typ()))),
        ))
        .skip(sptoken('='))
        .and(val)
        .map(|((rec, pattern, typ), value)| BindExpr { rec, pattern, typ, value })
}

pub(super) fn letbind<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), letbind_with(expr()))
        .map(|(pos, b)| ExprKind::Bind(Arc::new(b)).to_expr(pos))
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
    // An integer in front of `..` is a slice bound: the value parser
    // would read `1.` as a float and leave `.n` to be a field access.
    let slice_bound =
        attempt((position(), arrayexp::idx().skip(look_ahead(string("..")))))
            .map(|(pos, v)| ExprKind::Constant(v).to_expr(pos));
    // `parse_value` recurses outside this crate; `grow` gives it headroom
    // at the boundary. A quoted string is `interpolated()`'s alone, so its
    // failure is reported inside it rather than past it.
    slice_bound
        .or(attempt(
            grow((
                position(),
                not_followed_by(token('"')),
                parse_value(&VAL_MUST_ESC, &VAL_ESC).skip(not_prefix()),
            ))
            .map(|(pos, _, v)| ExprKind::Constant(v).to_expr(pos)),
        ))
        .or(grow(duration_unit_note()))
}

/// A diagnostic arm behind the literal parser: a `duration:` literal with
/// an unknown unit names the unit, since the value parser accepts the
/// longest known prefix and fails after it.
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
                let terminal_is_value =
                    v.last().and_then(|s| s.chars().next()).is_some_and(is_value_initial);
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

/// Rust-style raw strings: `r"…"`, `r#"…"#`, `r##"…"##`, … No escapes,
/// no interpolation, no newline stripping; the content ends at the first
/// `"` followed by the opener's hash count.
fn raw_string<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt((token('r'), many::<CompactString, _, _>(token('#')), token('"'))))
        .then(|(pos, (_, hashes, _)): (_, (_, CompactString, _))| {
            let n = hashes.len();
            (
                many::<CompactString, _, _>(choice((
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
                .map(move |(s, _, _): (CompactString, _, _)| (pos, s))
        })
        .map(|(pos, s)| {
            ExprKind::Constant(Value::String(ArcStr::from(s.as_str())))
                .to_expr(pos)
                .written_as(StrForm::Raw)
        })
}

fn until_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt(string("until").skip(not_prefix()).with(spaces1())).with(expr()))
        .map(|(pos, e)| ExprKind::Until(Arc::new(e)).to_expr(pos))
}

/// A seq statement: an expression, or `until e`, with what stands above it.
fn seq_body_item<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (leading_decorations(), choice((until_expr(), expr())), position())
        .map(|(dec, e, end): (Leading, Expr, _)| decorate(e.ending(end), dec))
}

/// A brace-delimited seq statement list (the body of `seq`, `try`
/// and `with`). Empty positions come back as `NoOp`.
fn seq_stmts<I>() -> impl Parser<I, Output = LPooled<Vec<Expr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        sptoken('{'),
        sptoken('}'),
        sep_by1_tok_exp(seq_body_item(), semisep(), token('}'), |pos| {
            ExprKind::NoOp.to_expr(pos).ending(pos)
        }),
    )
}

/// `abort(e)` or `flush(e)`, after a `;` unless it opens the head.
/// `flush` is not a reserved word, so a trigger that calls a function of
/// that name is parenthesized.
fn seq_clause<I>(name: &'static str, first: bool) -> impl Parser<I, Output = Arc<Expr>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let semi = move || match first {
        true => value(()).left(),
        false => spaces().with(token(';')).map(|_| ()).right(),
    };
    attempt((semi(), spaces(), string(name), spaces(), token('(')))
        .with(expr())
        .skip(sptoken(')'))
        .map(Arc::new)
}

/// A seq's trigger: an operator expression, bare (a `{` ahead is the
/// body), or `let pattern = ` one.
fn seq_trigger<I>() -> impl Parser<I, Output = SeqTrigger>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let clause = attempt(
        choice((string("abort"), string("flush"))).skip(spaces()).skip(token('(')),
    );
    attempt(spaces().skip(not_followed_by(token('{'))).skip(not_followed_by(clause)).with(
        choice((
            (letbind_with(arithexp::arith(false)), position()).then(|(b, pos)| match b.rec {
                true => grow::refuse(pos, "a seq trigger's `let` cannot be `rec`").right(),
                false => value(SeqTrigger::Bind(Arc::new(b))).left(),
            }),
            arithexp::arith(false).map(|e| SeqTrigger::Expr(Arc::new(e))),
        )),
    ))
}

/// The head of a seq, in order: `[trigger][; abort(e)][; flush(e)]`.
fn seq_head<I>(
    queued: bool,
) -> impl Parser<I, Output = (Option<SeqTrigger>, Option<Arc<Expr>>, Option<Arc<Expr>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    optional(seq_trigger()).then(move |trigger| {
        let first = trigger.is_none();
        optional(seq_clause("abort", first)).then(move |abort| {
            let trigger = trigger.clone();
            let flush = seq_clause("flush", first && abort.is_none());
            (optional(flush), position(), optional(attempt(sptoken(';'))), position()).then(
                move |(flush, at_flush, semi, at_semi)| {
                    if semi.is_some() {
                        grow::refuse(at_semi, "a seq head is `[trigger][; abort(..)][; flush(..)]`")
                            .right()
                    } else if flush.is_some() && !queued {
                        grow::refuse(at_flush, "`flush(..)` is legal only in a seqq head")
                            .right()
                    } else {
                        value((trigger.clone(), abort.clone(), flush)).left()
                    }
                },
            )
        })
    })
}

pub(super) fn seq<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        choice((
            attempt(string("seqq").skip(not_prefix())).map(|_| true),
            attempt(string("seq").skip(not_prefix())).map(|_| false),
        ))
        .then(|queued| seq_head(queued).map(move |head| (queued, head))),
        seq_stmts(),
        position(),
    )
        .then(|(pos, (queued, (trigger, abort, flush)), mut body, end)| {
            if body.iter().all(|e: &Expr| matches!(e.kind, ExprKind::NoOp)) {
                grow::refuse(end, "a seq block must contain at least one step").right()
            } else {
                let body = Arc::from_iter(body.drain(..));
                value(ExprKind::Seq { queued, trigger, abort, flush, body }.to_expr(pos))
                    .left()
            }
        })
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

/// Sort `items` by name; false when a name repeats.
fn sort_unique<T>(items: &mut [T], name: impl Fn(&T) -> &str) -> bool {
    items.sort_by(|a, b| name(a).cmp(name(b)));
    items.windows(2).all(|w| name(&w[0]) != name(&w[1]))
}

/// The `name: value, name, ..` field list of a struct literal or a
/// functional update: names unique, sorted by name; decorations above a
/// field attach to its value.
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
        position(),
        spaces().with(optional(token(':').with(expr()))),
    )
        .then(|(dec, pos, name, end, v): (Leading, _, ArcStr, _, Option<Expr>)| {
            let v = match v {
                Some(v) => v,
                // routine: a block's first statement may begin with a keyword
                None if is_reserved_binding(&name) => {
                    return unexpected_any(
                        "a reserved word field needs the explicit `name: value` form",
                    )
                    .left();
                }
                None => {
                    let name = ModPath::from([name.clone()]);
                    ExprKind::Ref { name }.to_expr(pos).ending(end)
                }
            };
            value((name, decorate(v, dec))).right()
        });
    (sep_by1_tok(field, csep(), token('}')), position()).and_then(
        |(mut fields, end): (LPooled<Vec<(ArcStr, Expr)>>, _)| {
            match sort_unique(&mut fields, |(n, _)| n) {
                true => Ok(fields),
                false => Err(grow::refusal::<I>(end, "struct fields must be unique")),
            }
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

/// A struct literal, else a form that shares a first item parsed once: the
/// empty map `{}`, a map literal, a functional update `{ s with f: v }`,
/// or a block.
pub(super) fn brace<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    /// What follows the first item.
    enum Tail {
        Map(Expr, LPooled<Vec<(Expr, Expr)>>),
        With(LPooled<Vec<(ArcStr, Expr)>>),
        Block(LPooled<Vec<Expr>>),
        One,
    }
    let entry = || (expr(), spstring("=>").with(expr()));
    let tail = choice((
        attempt(spstring("=>"))
            .with((
                expr(),
                optional(csep().with(sep_by_tok(entry(), csep(), attempt(sptoken('}'))))),
            ))
            .map(|(v, rest)| Tail::Map(v, rest.unwrap_or_else(LPooled::take))),
        attempt(spaces1().with(string("with")).skip(space()))
            .with(struct_fields())
            .map(Tail::With),
        semisep()
            .with(sep_by1_tok_exp(expr(), semisep(), token('}'), |pos| {
                ExprKind::NoOp.to_expr(pos).ending(pos)
            }))
            .map(Tail::Block),
        produce(|| Tail::One),
    ));
    /// The source of a functional update: a name, a `?`/`$` chain on one,
    /// or anything parenthesized.
    fn with_source(e: &Expr) -> Option<Expr> {
        fn chain_on_name(e: &Expr) -> bool {
            match &e.kind {
                ExprKind::Ref { .. } => true,
                ExprKind::Qop(s) | ExprKind::OrNever(s) => chain_on_name(s),
                _ => false,
            }
        }
        match &e.kind {
            _ if e.dec.is_some() => None,
            ExprKind::ExplicitParens(e) => Some((**e).clone()),
            _ if chain_on_name(e) => Some(e.clone()),
            _ => None,
        }
    }
    choice((
        attempt(structure()),
        (
            position(),
            token('{').skip(spaces()).with(optional((expr(), tail))),
            position(),
            sptoken('}'),
        )
            .then(|(pos, body, end, _)| {
                let kind = match body {
                    None => ExprKind::Map { args: Arc::from_iter([]) },
                    Some((k, Tail::Map(v, mut rest))) => ExprKind::Map {
                        args: Arc::from_iter(std::iter::once((k, v)).chain(rest.drain(..))),
                    },
                    Some((source, Tail::With(mut fields))) => match with_source(&source) {
                        None => {
                            return grow::refuse(
                                end,
                                "a functional update's source is a name or parenthesized",
                            )
                            .right();
                        }
                        Some(source) => ExprKind::StructWith(StructWithExpr {
                            source: Arc::new(source),
                            replace: Arc::from_iter(fields.drain(..)),
                        }),
                    },
                    Some((first, Tail::Block(mut rest))) => ExprKind::Do {
                        exprs: Arc::from_iter(std::iter::once(first).chain(rest.drain(..))),
                    },
                    Some((_, Tail::One)) => {
                        return grow::refuse(end, "a block must contain at least 2 expressions")
                            .right();
                    }
                };
                value(kind.to_expr(pos)).left()
            }),
    ))
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
/// path `T`; the capitalized last segment is what tells it from a call.
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
            (spname(), spaces().with(optional(token(':').with(typ())))),
        ),
        expr(),
    )
        .map(|(pos, (bind, constraint), handler)| {
            ExprKind::Catch(Arc::new(CatchExpr {
                bind,
                constraint,
                handler: Arc::new(handler),
                seq_abort: None,
                seq_capture: None,
                seq_manual: None,
                seq_pc: None,
            }))
            .to_expr(pos)
        })
}

/// `try { stmts } with(e[: T]) { stmts }` — a seq statement. Parsed
/// wherever an expression is; the compiler refuses it outside seq
/// statement position.
fn try_with<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    fn empty(b: &[Expr]) -> bool {
        b.iter().all(|e| matches!(e.kind, ExprKind::NoOp))
    }
    let block = |what: &'static str| {
        (spaces().with(seq_stmts()), position()).then(move |(b, end)| match empty(&b) {
            true => grow::refuse(end, what).right(),
            false => value(b).left(),
        })
    };
    (
        position().skip(attempt(string("try").skip(not_prefix()))),
        block("a try body must contain at least one step"),
        spaces().skip(string("with").skip(not_prefix())),
        between(
            sptoken('('),
            sptoken(')'),
            (
                spaces().with(choice((
                    attempt(
                        (position(), token('_'))
                            .skip(look_ahead(choice((sptoken(')'), sptoken(':'))))),
                    )
                    .map(|(pos, _)| Name::written(literal!("_"), pos)),
                    name(),
                ))),
                spaces().with(optional(token(':').with(typ()))),
            ),
        ),
        block("a with body must contain at least one step"),
    )
        .map(
            |(pos, mut body, _, (bind, constraint), mut handler): (
                _,
                LPooled<Vec<Expr>>,
                _,
                _,
                LPooled<Vec<Expr>>,
            )| {
                ExprKind::TryWith(Arc::new(TryWithExpr {
                    body: Arc::from_iter(body.drain(..)),
                    bind,
                    constraint,
                    handler: Arc::from_iter(handler.drain(..)),
                }))
                .to_expr(pos)
            },
        )
}

/// `&|x| ..`: the one reference `arith` cannot read, a lambda's.
fn byref_lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('&').with(lambda()))
        .map(|(pos, expr)| ExprKind::ByRef(Arc::new(expr)).to_expr(pos))
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
                try_with(),
                (position(), typedef()).map(|(pos, td)| ExprKind::TypeDef(td).to_expr(pos)),
                (position(), trait_decl(false))
                    .map(|(pos, t)| ExprKind::Trait(Arc::new(t)).to_expr(pos)),
                (position(), impl_decl()).map(|(pos, i)| ExprKind::Impl(Arc::new(i)).to_expr(pos)),
                letbind(),
                attempt(lambda()),
                attempt(connect()),
                attempt(arith(true)),
                byref_lambda(),
            )),
            position(),
        )
            .map(|(dec, e, end): (Leading, Expr, _)| decorate(e.ending(end), dec)))
    }
}

/// Run `p` over all of `text`, trailing whitespace allowed; a failure is
/// reported against `ori()`.
fn parse_all<'a, T, P>(
    text: &'a str,
    ori: impl FnOnce() -> Arc<Origin>,
    p: P,
) -> anyhow::Result<T>
where
    P: Parser<easy::Stream<position::Stream<&'a str, SourcePosition>>, Output = T>,
{
    let _profile = profile::phase(Phase::Parse);
    grow::parsing(text, || {
        p.skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(text))
            .map(|(r, _)| r)
            .map_err(|e| {
                grow::note_error_pos(e.position);
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e).context(ParserContext { ori: ori(), pos })
    })
}

/// Parse the expressions of a file.
pub fn parse(ori: Origin) -> anyhow::Result<Arc<[Expr]>> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let items = sep_by1_tok_exp(expr(), semisep(), eof(), |pos| {
        ExprKind::NoOp.to_expr(pos).ending(pos)
    });
    let mut r: LPooled<Vec<Expr>> = parse_all(&ori.text, || ori.clone(), items)?;
    Ok(Arc::from_iter(r.drain(..)))
}

/// Parse the items of an interface file, which may have none.
pub fn parse_sig(ori: Origin) -> anyhow::Result<Sig> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let items = sep_by_tok(sig_item(), semisep(), attempt(spaces().with(eof())));
    let mut r: LPooled<Vec<SigItem>> = parse_all(&ori.text, || ori.clone(), items)?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)) })
}

fn text_origin(s: &str) -> impl FnOnce() -> Arc<Origin> + '_ {
    move || Arc::new(Origin::unspecified(s))
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    parse_all(s, text_origin(s), expr())
}

#[cfg(test)]
pub fn test_parse_mapref(s: &str) -> anyhow::Result<Expr> {
    parse_all(s, text_origin(s), arithexp::arith_term(true))
}

/// Parse one fntype expression
pub fn parse_fn_type(s: &str) -> anyhow::Result<FnType> {
    parse_all(s, text_origin(s), fntype())
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    parse_all(s, text_origin(s), typ())
}

pub(super) fn parse_path(s: &str) -> anyhow::Result<ModPath> {
    parse_all(s, text_origin(s), modpath())
}
