use crate::{
    expr::{
        Attr, BindExpr, CatchExpr, CatchRole, Comments, Decorations, Doc, Eol, Expr,
        ExprKind, ModPath, ModuleKind, Name, Origin, OriginScope, ParserContext, Pattern,
        SelectExpr, SeqExpr, SeqKind, SeqTrigger, Sig, SigItem, StrForm, StructExpr,
        StructWithExpr, TraitMethod, TryWithExpr, WrittenAt,
    },
    profile::{self, Phase},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use arcstr::{ArcStr, literal};
use combine::{
    EasyParser, ParseError, Parser, RangeStream, StreamOnce, attempt, between, choice,
    count_min_max, easy, eof,
    error::StreamError,
    look_ahead, many, many1, none_of, not_followed_by, optional,
    parser::{
        char::{space, string},
        combinator::recognize,
        range::{take_while, take_while1},
        token::produce,
    },
    position, satisfy, sep_by1, skip_many,
    stream::StreamErrorFor,
    stream::{
        Range,
        position::{self, Positioner, RangePositioner, SourcePosition},
    },
    token, unexpected_any, value,
};
use compact_str::{CompactString, format_compact};
use escaping::Escape;
use netidx_core::path::Path;
use netidx_value::{
    Value,
    parser::{
        VAL_ESC, VAL_MUST_ESC, duration_of, not_prefix, sep_by_tok, value as parse_value,
    },
};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::LazyLock;
use triomphe::Arc;

mod grow;
use grow::grow;
pub use grow::{DEFAULT_MAX_NESTING, ParseFailure, max_nesting, set_max_nesting};

mod interpolateexp;
use interpolateexp::interpolated;

mod modexp;
use modexp::{module, sig_item, use_module};

mod typexp;
use typexp::{fntype, typ, typedef};

mod traitexp;
use traitexp::{impl_decl, trait_decl};

mod lambdaexp;
use lambdaexp::{any_lambda, apply_args, lambda_ahead};

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
    // other control characters print raw: the string parser reads them
    // verbatim and knows no `\u{..}` escape
    Escape::new('\\', &esc, &NAMED, None).unwrap()
});

/// The escape for strings nested in a value literal (`error:"..."`):
/// netidx's, whose generic `\u{..}` arm its own parser cannot read back,
/// so other control characters print raw.
pub static VALUE_ESC: LazyLock<Escape> = LazyLock::new(|| {
    const NAMED: [(char, &str); 4] = [('\n', "n"), ('\r', "r"), ('\t', "t"), ('\0', "0")];
    Escape::new('\\', &['\\', '"', '\n', '\r', '\0', '\t'], &NAMED, None).unwrap()
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
        "catch", "try", "pub", "trait", "impl", "seq", "seqq", "until", "live", "mut",
    ];
    // the compiler-known type and bound names among them: a user type of
    // one of those names would be unnameable, or silently the builtin
    let reserved = [
        "true",
        "false",
        "null",
        "bytes",
        "Array",
        "Map",
        "List",
        "any",
        "Any",
        "self",
        "super",
        "package",
        "abort",
        "Error",
        "Abstract",
        "Concrete",
        "Data",
        "Function",
        "Singleton",
        "OneNumber",
        "NoError",
        "Discernible",
        "Ordered",
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
fn sep_by1_tok<I, O, OC, EP, SP, TP>(
    p: EP,
    sep: SP,
    term: TP,
) -> impl Parser<I, Output = OC>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    OC: Extend<O> + Default,
    SP: Parser<I>,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
{
    sep_by1_tok_trailing(p, sep, term).map(|(items, _)| items)
}

/// [`sep_by1_tok`], and whether the list ended in its separator.
pub(super) fn sep_by1_tok_trailing<I, O, OC, EP, SP, TP>(
    p: EP,
    sep: SP,
    term: TP,
) -> impl Parser<I, Output = (OC, bool)>
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
                let trailing = matches!(items.last(), Some(None));
                let mut res = OC::default();
                res.extend(items.drain(..).flatten());
                Ok((res, trailing))
            }
            _ => Err(<StreamErrorFor<I>>::message_static_message("expected an item")),
        },
    )
}

/// Graphix has no 1-tuple: `(x,)` is refused, not read as `(x)`.
pub(super) const ONE_TUPLE: &str = "tuples must have at least 2 elements";

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
    many(none_of(['\r', '\n']))
}

// One own-line `//` comment line, text kept verbatim. `///` is left for
// `doc_comment`; `docs` says whether one is legal here.
fn comment_line<I>(docs: bool) -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(
        (
            position(),
            string("//"),
            optional(attempt(token('/').skip(not_followed_by(token('/'))))),
        )
            .then(move |(pos, _, doc)| match doc {
                None if !grow::starts_line(pos) => {
                    grow::note_reason(
                        pos,
                        None,
                        CompactString::const_new(
                            "a comment stands on a line of its own, above what it \
                             describes, or ends an item's line after its separator",
                        ),
                    );
                    unexpected_any("trailing comment").left()
                }
                Some(_) if docs => unexpected_any("doc comment").left(),
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

fn leading_comments<I>(docs: bool) -> impl Parser<I, Output = LPooled<Vec<ArcStr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(many(comment_line(docs)))
}

/// Whether a `//` comment stands anywhere in `e`, an attribute's
/// arguments included: none can in what prints on one line.
pub(super) fn holds_comment(e: &Expr) -> bool {
    e.fold(false, &mut |c, e| {
        c || e.dec.as_ref().is_some_and(|d| {
            !d.comments.is_empty()
                || d.eol.is_some()
                || !d.below.is_empty()
                || d.attrs.iter().any(|a| a.args.iter().any(holds_comment))
        }) || match &e.kind {
            ExprKind::Trait(t) => {
                t.methods.iter().any(|m| !m.comments.lines().is_empty())
            }
            ExprKind::Module { value, .. } => match &**value {
                ModuleKind::Dynamic { sig, .. } => {
                    sig.items.iter().any(|si| !si.comments.lines().is_empty())
                }
                _ => false,
            },
            _ => false,
        }
    })
}

// `#[name]` or `#[name(arg, ...)]`; the args are full expressions, which
// print on the attribute's line. The `attempt` on `#[` keeps a labeled
// call arg `#name` from colliding.
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
        position(),
    )
        .skip(sptoken(']'))
        .then(|(name, args, end): (ArcStr, Option<LPooled<Vec<Expr>>>, _)| {
            let mut args = args.unwrap_or_else(LPooled::take);
            match args.iter().any(holds_comment) {
                true => grow::refuse(end, "a comment cannot stand inside an attribute")
                    .right(),
                false => {
                    value(Attr { name, args: Arc::from_iter(args.drain(..)) }).left()
                }
            }
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
            comment_line(false).map(Dec::Comment),
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
    let own = e.dec.take().map(|d| (*d).clone()).unwrap_or_default();
    comments.extend(own.comments.iter().cloned());
    attrs.extend(own.attrs.iter().cloned());
    e.dec = Some(Arc::new(Decorations {
        comments: Arc::from_iter(comments.drain(..)),
        attrs: Arc::from_iter(attrs.drain(..)),
        ..own
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
    let line = attempt((
        position(),
        look_ahead(string("///").skip(not_followed_by(token('/')))),
    ))
    .then(|(pos, _)| {
        if grow::starts_line(pos) {
            return string("///").with(line_text()).left();
        }
        grow::note_reason(
            pos,
            None,
            CompactString::const_new(
                "a doc comment stands on a line of its own, above what it describes",
            ),
        );
        unexpected_any("trailing doc comment").right()
    });
    spaces().with(many(line.skip(spaces()))).and(position()).then(|(Lines(doc), pos)| {
        match doc {
            None => value(Doc(None)).left(),
            Some(doc) => optional(attempt(string("//")))
                .then(move |c| match c {
                    None => value(Doc(Some(ArcStr::from(doc.as_str())))).left(),
                    Some(_) => {
                        grow::note_reason(
                            pos,
                            None,
                            CompactString::const_new(
                                "a `//` comment goes above the `///` doc lines",
                            ),
                        );
                        unexpected_any("comment below the doc").right()
                    }
                })
                .right(),
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
    fname_noting(true)
}

/// [`fname`]; a refused reserved word is noted only when `note`, since a
/// type path's segment is never a binding and a pattern head probes one.
pub(super) fn fname_noting<I>(note: bool) -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), ident(false)).then(move |(pos, s): (SourcePosition, ArcStr)| {
        if is_reserved_binding(&s) {
            // Probing a statement's first token as a name is ordinary
            // parsing; only words that never begin a construct earn a note.
            if note && KEYWORDS.get(s.as_str()) != Some(&Keyword::Construct) {
                grow::note_reason(
                    pos,
                    Some(s.chars().count()),
                    format_compact!(
                        "`{s}` is a reserved word and cannot be used as a name"
                    ),
                );
            }
            grow::because("can't use keyword as a function or variable name").left()
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
            grow::because("can't use keyword as a type name").left()
        } else {
            value(s).right()
        }
    })
}

/// A path's optional keyword root: `self::`, `package::`, or a chain of
/// `super::`s. Yields the keyword segments consumed, each where it was
/// written. Each alternative is attempted with its `::` so an identifier
/// like `packaged` backtracks.
fn path_root<I>() -> impl Parser<I, Output = LPooled<Vec<Name>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let one = |pos, name| {
        let mut v: LPooled<Vec<Name>> = LPooled::take();
        v.push(Name::written(name, pos));
        v
    };
    choice((
        attempt((position(), string("package").with(string("::"))))
            .map(move |(pos, _)| one(pos, literal!("package"))),
        attempt((position(), string("self").with(string("::"))))
            .map(move |(pos, _)| one(pos, literal!("self"))),
        many1::<LPooled<Vec<_>>, _, _>(
            attempt((position(), string("super").with(string("::"))))
                .map(|(pos, _)| Name::written(literal!("super"), pos)),
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
        |(mut root, mut v): (LPooled<Vec<Name>>, LPooled<Vec<ArcStr>>)| {
            ModPath(Path::from_iter(root.drain(..).map(|n| n.name).chain(v.drain(..))))
        },
    )
}

fn csep<I>() -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    separator(',', COMMA_WHY)
}

/// `sep` between two items. Where what follows an item could start another
/// one, the reason names the missing separator; where a list meets a `;`,
/// it names the list left open; either for a failure right there.
fn separator_tok<I>(sep: char, why: &'static str) -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let starts_item = |c: char| {
        c.is_alphanumeric()
            || matches!(
                c,
                '_' | '"' | '\'' | '`' | '(' | '[' | '{' | '#' | '@' | '&' | '*' | '!'
            )
    };
    let reason = move |c: char| match c {
        ';' if sep == ',' => Some("the list is not closed before this `;`"),
        c if starts_item(c) => Some(why),
        _ => None,
    };
    choice((
        attempt(spaces().with(token(sep))),
        attempt(
            spaces()
                .with((position(), look_ahead(satisfy(move |c| reason(c).is_some()))))
                .then(move |(pos, c)| {
                    let why = reason(c).expect("satisfied");
                    grow::note_reason(pos, Some(1), CompactString::const_new(why));
                    grow::note_furthest(pos);
                    satisfy(|_| false)
                }),
        ),
    ))
}

/// [`separator`], then the space after it.
fn separator<I>(sep: char, why: &'static str) -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    separator_tok(sep, why).skip(spaces())
}

/// [`separator`], keeping the comment that ends its line.
fn separator_eol<I>(
    sep: char,
    why: &'static str,
) -> impl Parser<I, Output = Option<ArcStr>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    separator_tok(sep, why).with(eol_comment()).skip(spaces())
}

const COMMA_WHY: &str = "a `,` separates this item from the one before";
const SEMI_WHY: &str = "a `;` separates this statement from the one before";

/// A `//` comment ending a line after code, its text kept verbatim. One
/// that starts its line stands below, and a `///` is a doc.
fn eol_comment<I>() -> impl Parser<I, Output = Option<ArcStr>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    optional(attempt(
        skip_many(satisfy(|c| c == ' ' || c == '\t'))
            .with((
                position(),
                string("//"),
                optional(attempt(token('/').skip(not_followed_by(token('/'))))),
            ))
            .then(|(pos, _, doc)| match doc.is_none() && !grow::starts_line(pos) {
                true => line_text().left(),
                false => unexpected_any("comment").right(),
            }),
    ))
    .map(|t: Option<CompactString>| t.map(|t| ArcStr::from(t.as_str())))
}

/// The own-line comments after a list's last item, above what closes it;
/// `docs` where a `///` line is an item's doc, which none follows.
fn below_comments<I>(docs: bool) -> impl Parser<I, Output = LPooled<Vec<ArcStr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    many(attempt(spaces().with(comment_line(docs))))
}

/// A list item that takes the comment ending its line, after its
/// separator.
pub(super) trait Commentable {
    fn take_eol(&mut self, eol: ArcStr);
}

/// A list item whose comments its last expression carries.
pub(super) trait ExprItem {
    fn last_expr(&mut self) -> &mut Expr;
}

impl ExprItem for Expr {
    fn last_expr(&mut self) -> &mut Expr {
        self
    }
}

impl<K> ExprItem for (K, Expr) {
    fn last_expr(&mut self) -> &mut Expr {
        &mut self.1
    }
}

impl<T: ExprItem> Commentable for T {
    fn take_eol(&mut self, eol: ArcStr) {
        trail(self.last_expr(), Some(eol), &[])
    }
}

impl Commentable for SigItem {
    fn take_eol(&mut self, eol: ArcStr) {
        self.eol = Eol::new(Some(eol))
    }
}

impl Commentable for TraitMethod {
    fn take_eol(&mut self, eol: ArcStr) {
        self.eol = Eol::new(Some(eol))
    }
}

/// Give `e` the comment ending its line and the lines below it.
pub(super) fn trail(e: &mut Expr, eol: Option<ArcStr>, below: &[ArcStr]) {
    if eol.is_none() && below.is_empty() {
        return;
    }
    let mut d = e.dec.take().map(|d| (*d).clone()).unwrap_or_default();
    if eol.is_some() {
        d.eol = eol;
    }
    if !below.is_empty() {
        d.below = d.below.iter().chain(below.iter()).cloned().collect();
    }
    e.dec = Some(Arc::new(d));
}

/// What sits in a list's place: an item, or the end, with the comments
/// above it.
enum Slot<O> {
    Item(O),
    End(LPooled<Vec<ArcStr>>),
}

/// A list [`commented`] read: each slot with where it starts, `None` for
/// the empty one a trailing separator or an empty list leaves, and the
/// own-line comments below the last item, which the list's kind places.
struct Listed<O> {
    slots: LPooled<Vec<(SourcePosition, Option<O>)>>,
    below: LPooled<Vec<ArcStr>>,
}

/// `p` separated by `sep` (a `;` or `,`, see [`separator`]) up to `term`,
/// which is not consumed; a trailing separator leaves an empty slot, as
/// does a list with nothing in it. Each item takes the comment ending
/// its line, after its separator; `docs` as for [`below_comments`].
fn commented<I, O, EP, TP, PF, TF>(
    p: PF,
    sep: char,
    why: &'static str,
    term: TF,
    docs: bool,
) -> impl Parser<I, Output = Listed<O>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    O: Commentable,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
    PF: Fn() -> EP,
    TF: Fn() -> TP,
{
    (slot(p(), term(), docs), list_rest(slot(p(), term(), docs), sep, why, docs))
        .map(|(first, rest)| rest.after(first))
}

/// A list's place before `term`: an item, or the end with the comments
/// above it.
fn slot<I, O, EP, TP>(
    p: EP,
    term: TP,
    docs: bool,
) -> impl Parser<I, Output = (SourcePosition, Slot<O>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
{
    (
        position(),
        choice((
            attempt(below_comments(docs).skip(ahead(term))).map(Slot::End),
            p.map(Slot::Item),
        )),
    )
}

/// What follows a [`commented`] list's first slot: each separator, with
/// the comment ending the line it is on, and the slot after it; then the
/// comment ending the last item's line and the own-line comments below.
struct ListRest<O> {
    rest: LPooled<Vec<(Option<ArcStr>, (SourcePosition, Slot<O>))>>,
    eol: Option<ArcStr>,
    below: LPooled<Vec<ArcStr>>,
}

fn list_rest<I, O, SP>(
    slot: SP,
    sep: char,
    why: &'static str,
    docs: bool,
) -> impl Parser<I, Output = ListRest<O>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    SP: Parser<I, Output = (SourcePosition, Slot<O>)>,
{
    (
        many::<LPooled<Vec<_>>, _, _>((separator_eol(sep, why), slot)),
        eol_comment(),
        below_comments(docs),
    )
        .map(|(rest, eol, below)| ListRest { rest, eol, below })
}

impl<O: Commentable> ListRest<O> {
    /// The list whose first slot is `first`.
    fn after(mut self, first: (SourcePosition, Slot<O>)) -> Listed<O> {
        let mut listed = Listed { slots: LPooled::take(), below: LPooled::take() };
        // only an item stands before a separator: the empty slot ends the list
        let take_eol = |slots: &mut LPooled<Vec<(SourcePosition, Option<O>)>>,
                        eol: Option<ArcStr>| {
            if let (Some(eol), Some((_, Some(o)))) = (eol, slots.last_mut()) {
                o.take_eol(eol)
            }
        };
        for (eol, (pos, slot)) in
            std::iter::once((None, first)).chain(self.rest.drain(..))
        {
            take_eol(&mut listed.slots, eol);
            match slot {
                Slot::Item(o) => listed.slots.push((pos, Some(o))),
                Slot::End(mut lines) => {
                    listed.below.extend(lines.drain(..));
                    listed.slots.push((pos, None))
                }
            }
        }
        take_eol(&mut listed.slots, self.eol);
        listed.below.extend(self.below.drain(..));
        listed
    }
}

/// [`commented`] statements: an empty slot is an empty statement, and
/// the last statement keeps the comments below it.
pub(super) fn statements<I, TP, TF>(
    term: TF,
) -> impl Parser<I, Output = LPooled<Vec<Expr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    TP: Parser<I>,
    TF: Fn() -> TP,
{
    commented(expr, ';', SEMI_WHY, term, false).map(|mut l: Listed<Expr>| {
        let mut res: LPooled<Vec<Expr>> = LPooled::take();
        res.extend(l.slots.drain(..).map(|(pos, e)| {
            e.unwrap_or_else(|| ExprKind::NoOp.to_expr(pos).ending(pos))
        }));
        if let Some(last) = res.last_mut() {
            trail(last, None, &l.below)
        }
        res
    })
}

/// [`commented`] `,` items, read as [`comma_listed`] reads them.
pub(super) fn comma_items<I, O, EP, TP, PF, TF>(
    p: PF,
    term: TF,
    empty: bool,
) -> impl Parser<I, Output = (LPooled<Vec<O>>, bool)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    O: ExprItem,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
    PF: Fn() -> EP,
    TF: Fn() -> TP,
{
    commented(p, ',', COMMA_WHY, term, false).and_then(move |l: Listed<O>| {
        comma_listed(l, empty).map_err(<StreamErrorFor<I>>::message_static_message)
    })
}

/// The items of a `,` list and whether it ended in its separator; at
/// least one unless `empty`. The last item keeps the comments below it.
fn comma_listed<O: ExprItem>(
    mut l: Listed<O>,
    empty: bool,
) -> Result<(LPooled<Vec<O>>, bool), &'static str> {
    let trailing = l.slots.len() > 1 && matches!(l.slots.last(), Some((_, None)));
    if !empty && !matches!(l.slots.first(), Some((_, Some(_)))) {
        return Err("expected an item");
    }
    let mut res: LPooled<Vec<O>> = LPooled::take();
    res.extend(l.slots.drain(..).filter_map(|(_, o)| o));
    match res.last_mut() {
        Some(o) => trail(o.last_expr(), None, &l.below),
        None if l.below.is_empty() => (),
        None => return Err("a comment here has no item to describe"),
    }
    Ok((res, trailing))
}

/// [`commented`] `;` items of an interface or a trait, which has no empty
/// item, and the comments below the last.
pub(super) fn decl_items<I, O, EP, TP, PF, TF>(
    p: PF,
    term: TF,
    docs: bool,
) -> impl Parser<I, Output = (LPooled<Vec<O>>, Comments)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    O: Commentable,
    EP: Parser<I, Output = O>,
    TP: Parser<I>,
    PF: Fn() -> EP,
    TF: Fn() -> TP,
{
    commented(p, ';', SEMI_WHY, term, docs).map(|mut l: Listed<O>| {
        let mut res: LPooled<Vec<O>> = LPooled::take();
        res.extend(l.slots.drain(..).filter_map(|(_, o)| o));
        (res, Comments::of(l.below.drain(..)))
    })
}

fn sptoken<I>(t: char) -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(token(t))
}

/// The arguments of `any(..)` or `never(..)`, none or more.
fn call_args<I>() -> impl Parser<I, Output = LPooled<Vec<Expr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    comma_items(expr, || attempt(sptoken(')')), true).map(|(args, _)| args)
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
        between(sptoken('('), sptoken(')'), call_args()),
    )
        .map(|(pos, _, typ, mut args): (_, _, Option<Type>, LPooled<Vec<Expr>>)| {
            ExprKind::Never {
                typ: typ.map(Arc::new),
                args: Arc::from_iter(args.drain(..)),
            }
            .to_expr(pos)
        })
}

/// `live(e)`: in a seq step, `e`'s variables read live.
fn live_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("live").skip(not_prefix())),
        between(sptoken('('), sptoken(')'), expr()),
    )
        .map(|(pos, _, e)| ExprKind::Live(Arc::new(e)).to_expr(pos))
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
            call_args(),
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
                        grow::because("or-patterns are only legal in select arms").left()
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
    (
        position(),
        optional(token('*')),
        spaces().with(position()),
        connect_target(),
        expr(),
    )
        .map(|(pos, deref, at, name, e)| {
            ExprKind::Connect {
                name,
                at: WrittenAt(at),
                value: Arc::new(e),
                deref: deref.is_some(),
            }
            .to_expr(pos)
        })
}

/// A connect's target and its `<-`. Every expression is probed as a
/// connect first, so a reserved word is noted as a misused name only
/// once the `<-` shows it is one: `(1, false]` is no misuse.
fn connect_target<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        spaces().with(path_root()),
        sep_by1((position(), ident(false)), string("::")),
        spstring("<-").with(position()),
    )
        .then(
            |(mut root, mut segs, arrow): (
                LPooled<Vec<Name>>,
                LPooled<Vec<(SourcePosition, ArcStr)>>,
                SourcePosition,
            )| {
                if let Some((pos, s)) = segs.iter().find(|(_, s)| is_reserved_binding(s))
                {
                    // explains a failure up to the arrow, where the
                    // expression parse of the word stops
                    let span = (arrow.line == pos.line)
                        .then(|| usize::try_from(arrow.column - pos.column).unwrap_or(0));
                    grow::note_reason(
                        *pos,
                        span,
                        format_compact!(
                            "`{s}` is a reserved word and cannot be used as a name"
                        ),
                    );
                    return grow::because(
                        "can't use keyword as a function or variable name",
                    )
                    .left();
                }
                let path =
                    root.drain(..).map(|n| n.name).chain(segs.drain(..).map(|(_, s)| s));
                value(ModPath(Path::from_iter(path))).right()
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
            .and_then(|(pos, _, v)| {
                finite::<I>(pos, v).map(|v| ExprKind::Constant(v).to_expr(pos))
            }),
        ))
        .or(attempt(int_range_note()))
        .or(grow(duration_unit_note()))
}

/// A diagnostic arm behind the literal parser: an integer literal out of
/// its type's range names the type, since the value parser fails without
/// a reason.
fn int_range_note<I, O>() -> impl Parser<I, Output = O>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let typ = many1::<CompactString, _, _>(satisfy(|c: char| c.is_ascii_alphanumeric()))
        .skip(token(':'));
    let int = recognize::<CompactString, _, _>((
        optional(token('-')),
        many1::<CompactString, _, _>(satisfy(|c: char| c.is_ascii_digit())),
    ));
    (position(), optional(attempt(typ)), int, not_followed_by(token('.'))).then(
        |(pos, typ, int, _): (_, Option<CompactString>, CompactString, ())| {
            let name = typ.as_deref().unwrap_or("i64");
            let fits = match name {
                "i8" => int.parse::<i8>().is_ok(),
                "i16" => int.parse::<i16>().is_ok(),
                "i32" | "z32" => int.parse::<i32>().is_ok(),
                "i64" | "z64" => int.parse::<i64>().is_ok(),
                "u8" => int.parse::<u8>().is_ok(),
                "u16" => int.parse::<u16>().is_ok(),
                "u32" | "v32" => int.parse::<u32>().is_ok(),
                "u64" | "v64" => int.parse::<u64>().is_ok(),
                _ => true,
            };
            if !fits {
                let len = typ.as_ref().map_or(0, |t| t.len() + 1) + int.len();
                grow::note_reason(
                    pos,
                    Some(len),
                    format_compact!("`{int}` is out of the range of {name}"),
                );
            }
            unexpected_any("integer literal").map(|_: ()| -> O { unreachable!() })
        },
    )
}

/// `v`, unless it is a float literal out of its type's range, which
/// would read as an infinity no literal can write.
fn finite<I>(pos: SourcePosition, v: Value) -> Result<Value, StreamErrorFor<I>>
where
    I: StreamOnce,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    fn holds_infinity(v: &Value) -> bool {
        match v {
            Value::F64(f) => !f.is_finite(),
            Value::F32(f) => !f.is_finite(),
            Value::Error(e) => holds_infinity(e),
            Value::Array(a) => a.iter().any(holds_infinity),
            _ => false,
        }
    }
    match holds_infinity(&v) {
        true => Err(grow::refusal::<I>(pos, "a float literal out of its type's range")),
        false => Ok(v),
    }
}

/// A diagnostic arm behind the literal parser: a `duration:` literal with
/// an unknown unit names the unit, since the value parser accepts the
/// longest known prefix and fails after it, and one no duration holds
/// says so, since the value parser fails without a reason.
fn duration_unit_note<I, O>() -> impl Parser<I, Output = O>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    const UNITS: [&str; 9] = ["ns", "us", "ms", "s", "m", "h", "d", "M", "y"];
    (
        attempt(string("duration:")),
        position(),
        many1::<CompactString, _, _>(satisfy(|c: char| {
            c.is_ascii_digit() || c == '.' || c == '-' || c == '+'
        })),
        position(),
        many1::<CompactString, _, _>(satisfy(|c: char| c.is_alphabetic())),
    )
        .then(|(_, start, n, pos, unit): (_, _, CompactString, _, CompactString)| {
            if !UNITS.contains(&unit.as_str()) {
                grow::note_reason(
                    pos,
                    None,
                    compact_str::format_compact!(
                        "`{unit}` is not a duration unit; the units are ns, us, ms, \
                         s, m, h, d, M and y (`duration:30.m`)"
                    ),
                );
            } else if n.parse::<f64>().is_ok() && duration_of(&n, &unit).is_none() {
                grow::note_reason(
                    start,
                    Some(n.len() + unit.len()),
                    compact_str::format_compact!(
                        "`{n}{unit}` is negative or longer than the longest duration"
                    ),
                );
            }
            unexpected_any("duration literal").map(|_: ()| -> O { unreachable!() })
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
            |(mut root, mut v): (LPooled<Vec<Name>>, LPooled<Vec<ArcStr>>)| {
                let terminal_is_value =
                    v.last().and_then(|s| s.chars().next()).is_some_and(is_value_initial);
                if !terminal_is_value {
                    return grow::because("expected a value name").left();
                }
                let path = root.drain(..).map(|n| n.name).chain(v.drain(..));
                value(ModPath(Path::from_iter(path))).right()
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
    (
        position(),
        attempt((token('r'), many::<CompactString, _, _>(token('#')), token('"'))),
    )
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
    (position(), attempt(string("until").skip(not_prefix()).with(spaces())).with(expr()))
        .map(|(pos, e)| ExprKind::Until(Arc::new(e)).to_expr(pos))
}

/// A brace-delimited seq statement list (the body of `seq`, `try`
/// and `with`). Empty positions come back as `NoOp`.
fn seq_stmts<I>() -> impl Parser<I, Output = LPooled<Vec<Expr>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(sptoken('{'), sptoken('}'), statements(|| token('}')))
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
    attempt(
        spaces().skip(not_followed_by(token('{'))).skip(not_followed_by(clause)).with(
            choice((
                (letbind_with(arithexp::arith(false)), position()).then(
                    |(b, pos)| match b.rec {
                        true => grow::refuse(
                            pos,
                            "a seq trigger cannot be rec: it has no self to recurse on",
                        )
                        .right(),
                        false => value(SeqTrigger::Bind(Arc::new(b))).left(),
                    },
                ),
                arithexp::arith(false).map(|e| SeqTrigger::Expr(Arc::new(e))),
            )),
        ),
    )
}

/// The head of a seq, in order: `[trigger][; abort(e)][; flush(e)]`.
fn seq_head<I>(
    queued: bool,
) -> impl Parser<I, Output = (SeqKind, Option<SeqTrigger>, Option<Arc<Expr>>)>
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
            (optional(flush), position(), optional(attempt(sptoken(';'))), position())
                .then(move |(flush, at_flush, semi, at_semi)| {
                    if semi.is_some() {
                        grow::refuse(
                            at_semi,
                            "a seq head is `[trigger][; abort(..)][; flush(..)]`",
                        )
                        .right()
                    } else if flush.is_some() && !queued {
                        grow::refuse(
                            at_flush,
                            "flush empties a queue, and a seq has none: write seqq, or abort(..)",
                        )
                            .right()
                    } else {
                        let kind =
                            if queued { SeqKind::Queued { flush } } else { SeqKind::Plain };
                        value((kind, trigger.clone(), abort.clone())).left()
                    }
                })
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
        .then(seq_head),
        seq_stmts(),
        position(),
    )
        .then(|(pos, (kind, trigger, abort), mut body, end)| {
            if body.iter().all(|e: &Expr| matches!(e.kind, ExprKind::NoOp)) {
                grow::refuse(end, "a seq block must contain at least one step").right()
            } else {
                let body = Arc::from_iter(body.drain(..));
                let seq = SeqExpr { kind, trigger, abort, body };
                value(ExprKind::Seq(Arc::new(seq)).to_expr(pos)).left()
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
        attempt(string("select").with(not_prefix())).with(spaces()).with((
            expr(),
            between(
                sptoken('{'),
                sptoken('}'),
                spaces().with(
                    comma_items(
                        || {
                            (
                                leading_decorations(),
                                pattern(),
                                spstring("=>").with(expr()),
                            )
                                .map(|(dec, pat, body)| (pat, decorate(body, dec)))
                        },
                        || token('}'),
                        false,
                    )
                    .map(|(arms, _)| arms),
                ),
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
        .map(|(pos, typ, e)| {
            ExprKind::TypeCast { expr: Arc::new(e), typ: Arc::new(typ) }.to_expr(pos)
        })
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
    let field = || {
        (
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
                    const WHY: &str =
                        "a reserved word field needs the explicit `name: value` form";
                    let span = Some(name.chars().count());
                    grow::note_reason(pos, span, CompactString::const_new(WHY));
                    grow::note_furthest(pos);
                    return grow::because(WHY).left();
                }
                None => {
                    let name = ModPath::from([name.clone()]);
                    ExprKind::Ref { name }.to_expr(pos).ending(end)
                }
            };
            value((name, decorate(v, dec))).right()
        })
    };
    (comma_items(field, || token('}'), false).map(|(fields, _)| fields), position())
        .and_then(
            |(mut fields, end): (LPooled<Vec<(ArcStr, Expr)>>, _)| match sort_unique(
                &mut fields,
                |(n, _)| n,
            ) {
                true => Ok(fields),
                false => Err(grow::refusal::<I>(end, "struct fields must be unique")),
            },
        )
}

/// A struct literal's start: its first field's name and what may follow
/// a field's name.
fn struct_head<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let comment = || (string("//"), skip_many(satisfy(|c| c != '\n')), spaces());
    (
        token('{'),
        leading_decorations(),
        fldname(),
        spaces(),
        skip_many(attempt(comment())),
    )
        .with(satisfy(|c| matches!(c, ':' | ',' | '}')))
        .map(|_| ())
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

/// Whether `e` is a name or a `?`/`$` chain on one, which a functional
/// update reads as its source without parentheses.
pub(crate) fn chain_on_name(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Ref { .. } => true,
        ExprKind::Qop { arg: s, .. } | ExprKind::OrNever(s) => chain_on_name(s),
        _ => false,
    }
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
        Map(Expr, ListRest<(Expr, Expr)>),
        With(LPooled<Vec<(ArcStr, Expr)>>),
        /// The comment ending the first statement's line, and the rest.
        Block(Option<ArcStr>, LPooled<Vec<Expr>>),
        One,
    }
    let entry = || (expr(), spstring("=>").with(expr()));
    let tail = choice((
        attempt(spstring("=>"))
            .with((
                expr(),
                list_rest(
                    slot(entry(), attempt(sptoken('}')), false),
                    ',',
                    COMMA_WHY,
                    false,
                ),
            ))
            .map(|(v, rest)| Tail::Map(v, rest)),
        attempt(spaces1().with(string("with")).skip(space()))
            .with(struct_fields())
            .map(Tail::With),
        (separator_eol(';', SEMI_WHY), statements(|| token('}')))
            .map(|(eol, rest)| Tail::Block(eol, rest)),
        produce(|| Tail::One),
    ));
    /// The source of a functional update: a name, a `?`/`$` chain on one,
    /// or anything parenthesized.
    fn with_source(e: &Expr) -> Option<Expr> {
        match &e.kind {
            _ if e.dec.is_some() => None,
            ExprKind::ExplicitParens(e) => Some((**e).clone()),
            _ if chain_on_name(e) => Some(e.clone()),
            _ => None,
        }
    }
    choice((
        attempt(look_ahead(struct_head()).with(structure())),
        (
            position(),
            token('{').skip(spaces()).with(optional((expr(), tail))),
            position(),
            sptoken('}'),
        )
            .then(|(pos, body, end, _)| {
                let kind = match body {
                    None => ExprKind::Map { args: Arc::from_iter([]) },
                    Some((k, Tail::Map(v, rest))) => {
                        let first = (k.pos, Slot::Item((k, v)));
                        match comma_listed(rest.after(first), false) {
                            Ok((mut args, _)) => {
                                ExprKind::Map { args: Arc::from_iter(args.drain(..)) }
                            }
                            Err(why) => return grow::refuse(end, why).right(),
                        }
                    }
                    Some((source, Tail::With(mut fields))) => {
                        match with_source(&source) {
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
                        }
                    }
                    Some((mut first, Tail::Block(eol, mut rest))) => {
                        trail(&mut first, eol, &[]);
                        ExprKind::Block {
                            exprs: Arc::from_iter(
                                std::iter::once(first).chain(rest.drain(..)),
                            ),
                        }
                    }
                    Some((_, Tail::One)) => {
                        return grow::refuse(
                            end,
                            "a block must contain at least 2 expressions",
                        )
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
        // the spaces after the tag belong to a payload, when one follows
        optional(attempt(spaces().skip(look_ahead(token('(')))).with(between(
            token('('),
            sptoken(')'),
            comma_items(expr, || token(')'), false).map(|(args, _)| args),
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
        .map(|(pos, (name, _), arg, _)| {
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
                role: CatchRole::User,
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
                    attempt((position(), token('_').skip(not_prefix())))
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
                until_expr(),
                (position(), typedef())
                    .map(|(pos, td)| ExprKind::TypeDef(Arc::new(td)).to_expr(pos)),
                (position(), trait_decl(false))
                    .map(|(pos, t)| ExprKind::Trait(Arc::new(t)).to_expr(pos)),
                (position(), impl_decl()).map(|(pos, i)| ExprKind::Impl(Arc::new(i)).to_expr(pos)),
                letbind(),
                lambda_ahead().with(any_lambda()),
                attempt(connect()),
                arith(true),
            )),
            position(),
        )
            .map(|(dec, e, end): (Leading, Expr, _)| decorate(e.ending(end), dec)))
    }
}

/// Run the parser `$p` builds over all of `$text`, trailing whitespace
/// allowed: first over the plain stream, whose errors cost nothing, and
/// only when that fails again under `easy`, whose errors make the
/// message (the passes share the grammar, so they agree). A failure is
/// reported against `$ori()`.
macro_rules! parse_all {
    ($text:expr, $ori:expr, $p:expr) => {{
        let text: &str = $text;
        let text = text.strip_prefix(BOM).unwrap_or(text);
        let mut lf: LPooled<String> = LPooled::take();
        let text = crlf_as_lf(text, &mut lf);
        let _profile = profile::phase(Phase::Parse);
        let quick = grow::parsing(text, || {
            ($p).skip(spaces())
                .skip(eof())
                .parse(position::Stream::with_positioner(text, Lines::default()))
                .map(|(r, _)| r)
        });
        match quick {
            Ok(r) => Ok(r),
            Err(_) => explain(text, $ori, $p),
        }
    }};
}
#[cfg(test)]
use parse_all;

/// Parse `text` under `easy` for a failure's message.
fn explain<'a, T, P>(
    text: &'a str,
    ori: impl FnOnce() -> Arc<Origin>,
    p: P,
) -> anyhow::Result<T>
where
    P: Parser<easy::Stream<position::Stream<&'a str, Lines>>, Output = T>,
{
    grow::parsing(text, || {
        p.skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::with_positioner(text, Lines::default()))
            .map(|(r, _)| r)
            .map_err(|mut e| {
                grow::note_error_pos(e.position);
                // a whitespace token is never what went wrong
                let blank = |err: &easy::Error<char, &str>| {
                    matches!(err, easy::Error::Unexpected(easy::Info::Token(c)) if c.is_whitespace())
                };
                if e.errors.iter().any(|err| !blank(err)) {
                    e.errors.retain(|err| !blank(err));
                }
                e
            })
    })
    .map_err(|e| {
        let pos = e.pos;
        anyhow::Error::msg(e).context(ParserContext { ori: ori(), pos })
    })
}

/// The byte-order mark every parse skips.
const BOM: char = '\u{feff}';

/// `text` with every CRLF line ending read as LF, as rustc reads source,
/// so a file means the same whatever its checkout's line endings. Only a
/// line ending ever moves: lines and columns are unchanged.
fn crlf_as_lf<'a>(text: &'a str, buf: &'a mut LPooled<String>) -> &'a str {
    if !text.contains("\r\n") {
        return text;
    }
    for (i, line) in text.split("\r\n").enumerate() {
        if i > 0 {
            buf.push('\n');
        }
        buf.push_str(line);
    }
    buf.as_str()
}

/// A source position as an editor counts lines: a line ends at "\r\n",
/// "\r" or "\n".
#[derive(Debug, Clone, Copy, Default)]
pub struct Lines {
    at: SourcePosition,
    after_cr: bool,
}

impl Positioner<char> for Lines {
    type Position = SourcePosition;
    type Checkpoint = Self;

    fn position(&self) -> SourcePosition {
        self.at
    }

    fn update(&mut self, c: &char) {
        match c {
            '\n' if self.after_cr => (),
            '\r' | '\n' => self.at = SourcePosition { line: self.at.line + 1, column: 1 },
            _ => self.at.column += 1,
        }
        self.after_cr = *c == '\r';
    }

    fn checkpoint(&self) -> Self {
        *self
    }

    fn reset(&mut self, checkpoint: Self) {
        *self = checkpoint
    }
}

impl<'a> RangePositioner<char, &'a str> for Lines {
    fn update_range(&mut self, range: &&'a str) {
        for c in range.chars() {
            self.update(&c)
        }
    }
}

/// A text's lines as [`Lines`] numbers them, each without its break;
/// what follows the last break, empty or not, is a line.
pub fn lines(text: &str) -> impl Iterator<Item = &str> {
    let mut rest = Some(text);
    std::iter::from_fn(move || {
        let t = rest?;
        Some(match t.find(['\r', '\n']) {
            None => {
                rest = None;
                t
            }
            Some(i) => {
                let brk = if t[i..].starts_with("\r\n") { 2 } else { 1 };
                rest = Some(&t[i + brk..]);
                &t[..i]
            }
        })
    })
}

/// The length of the byte-order mark and then the `#!` line a program
/// text may start with, the line's newline excluded; 0 when there is
/// neither. Every reader of a program skips them.
pub fn shebang_len(text: &str) -> usize {
    let bom = if text.starts_with(BOM) { BOM.len_utf8() } else { 0 };
    let rest = &text[bom..];
    bom + if rest.starts_with("#!") { lines(rest).next().map_or(0, str::len) } else { 0 }
}

/// `text` without its byte-order mark and `#!` line, the newline kept so
/// lines number alike.
pub fn without_shebang(text: &ArcStr) -> ArcStr {
    match shebang_len(text) {
        0 => text.clone(),
        n => ArcStr::from(&text[n..]),
    }
}

/// Parse the expressions of a file.
pub fn parse(ori: Origin) -> anyhow::Result<Arc<[Expr]>> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let mut r: LPooled<Vec<Expr>> =
        parse_all!(&ori.text, || ori.clone(), statements(eof))?;
    Ok(Arc::from_iter(r.drain(..)))
}

/// Parse the items of an interface file, which may have none.
// CR claude for eric: [bug/low] An interface takes no comment at the end of an item's
// line and none below its last item. A comment-only file does not parse at all: a .gxi
// holding only a license header is "Unexpected end of input", and a .gx stub holding
// only comments fails `commented` with "a comment here has no item to describe",
// although an empty file parses. CLAUDE.md says comments are legal below a list's last
// item or a file's, and a .gx ending in a comment formats. Parse interface items with
// `commented` (eol/below on `SigItem`) and let an empty list keep its comments, or
// narrow the rule to .gx. probe:
// design/review-2026-10-26/repro/t-format-resolver-comment-only-file.sh
// (t-format-resolver-03)
// 2026-10-26 claude: The for-claude twins (t-parser-b-02, t-parser-a-04,
// t-parser-b-03, t-parser-a-03) were fixed by taking 'the rule covers every list':
// interface items and trait methods take an eol comment (`Eol`) and the lines below
// the last (`Sig::below`, `TraitExpr::below`), impl bodies parse as statements, and a
// comment-only .gx or .gxi parses; CLAUDE.md and the book's comment section say so. If
// you rule the rule narrower, those XCRs come back to CR. tests-parser-01's test is
// now a_comment_below_the_last_impl_method_rides_it.
// CR claude for eric: [bug/medium] Interface items are parsed with `sep_by_tok`, not
// `commented`, so an interface file ending in a comment, or a `val` followed by an
// end-of-line comment, is a parse error: the module's interface fails to load.
// CLAUDE.md allows a comment below a file's last item and after a list item's
// separator. Trait methods, impl methods (statements), map entries and never/any
// arguments refuse both forms too, and a comment-only .gx is refused where an empty one
// loads. Decide whether the rule covers every list. If so, route these through
// `commented` and give `SigItem`/`TraitMethod` eol/below. If not, narrow the documented
// rule. probe: design/review-2026-10-26/repro/x-formatter-list-comments-missing.sh
// (x-formatter-03)
// 2026-10-26 claude: The for-claude twins (t-parser-b-02, t-parser-a-04, t-parser-b-03,
// t-parser-a-03) were fixed by taking 'the rule covers every list': interface items and
// trait methods take an eol comment (`Eol`) and the lines below the last (`Sig::below`,
// `TraitExpr::below`), impl bodies parse as statements, and a comment-only .gx or .gxi
// parses; CLAUDE.md and the book's comment section say so. If you rule the rule
// narrower, those XCRs come back to CR. tests-parser-01's test is now
// a_comment_below_the_last_impl_method_rides_it.
pub fn parse_sig(ori: Origin) -> anyhow::Result<Sig> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let (mut r, below): (LPooled<Vec<SigItem>>, Comments) = parse_all!(
        &ori.text,
        || ori.clone(),
        decl_items(sig_item, || attempt(spaces().with(eof())), true)
    )?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)), below })
}

fn text_origin(s: &str) -> impl FnOnce() -> Arc<Origin> + '_ {
    move || Arc::new(Origin::unspecified(s))
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    parse_all!(s, text_origin(s), expr())
}

/// Parse one fntype expression
pub fn parse_fn_type(s: &str) -> anyhow::Result<FnType> {
    parse_all!(s, text_origin(s), fntype())
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    parse_all!(s, text_origin(s), typ())
}

pub(super) fn parse_path(s: &str) -> anyhow::Result<ModPath> {
    parse_all!(s, text_origin(s), modpath())
}
