use crate::{
    expr::{
        Attr, BindExpr, Decorations, Doc, Expr, ExprKind, ModPath, Origin, ParserContext,
        Pattern, SelectExpr, Sig, SigItem, StructExpr, StructWithExpr, TryCatchExpr,
        set_origin,
    },
    typ::{FnType, Type},
};
use ahash::AHashSet;
use arcstr::{ArcStr, literal};
use combine::{
    EasyParser, ParseError, Parser, RangeStream, attempt, between, choice, eof,
    look_ahead, many, none_of, not_followed_by, one_of, optional,
    parser::{
        char::{space, string},
        combinator::recognize,
        range::{take_while, take_while1},
    },
    position, sep_by1,
    stream::{
        Range,
        position::{self, SourcePosition},
    },
    token, unexpected_any, value,
};
use compact_str::CompactString;
use escaping::Escape;
use netidx::{path::Path, publisher::Value};
use netidx_value::parser::{
    VAL_ESC, VAL_MUST_ESC, escaped_string, not_prefix, sep_by_tok, sep_by1_tok,
    value as parse_value,
};
use poolshark::local::LPooled;
use std::sync::LazyLock;
use triomphe::Arc;

mod interpolateexp;
use interpolateexp::interpolated;

mod modexp;
use modexp::{module, sig_item, use_module};

mod typexp;
use typexp::{fntype, typ, typedef};

mod lambdaexp;
use lambdaexp::{apply_args, lambda};

mod arrayexp;
use arrayexp::{array, array_index_suffix};

pub(crate) mod arithexp;
use arithexp::arith;

#[cfg(test)]
mod test;

mod patternexp;
use patternexp::{pattern, structure_pattern};

fn escape_generic(c: char) -> bool {
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
pub const RESERVED: LazyLock<AHashSet<&str>> = LazyLock::new(|| {
    AHashSet::from_iter([
        "true", "false", "ok", "null", "mod", "let", "select", "type", "fn", "cast",
        "if", "i8", "u8", "i16", "u16", "u32", "v32", "i32", "z32", "u64", "v64", "i64",
        "z64", "f32", "f64", "decimal", "datetime", "duration", "bool", "string",
        "bytes", "null", "_", "?", "fn", "Array", "Map", "any", "Any", "use", "rec",
        "catch", "try", "sync", "for", "mut",
    ])
});

// sep_by1 but a separator terminator is allowed and mapped to an output value
pub fn sep_by1_tok_exp<I, O, OC, F, EP, SP, TP>(
    p: EP,
    sep: SP,
    term: TP,
    f: F,
) -> impl Parser<I, Output = OC>
where
    I: RangeStream<Token = char>,
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

// Whitespace ONLY — `//` comments are no longer skipped here. They are
// captured exclusively by `leading_comments()` at the `expr()` entry, so a
// comment anywhere else (interior, trailing, dangling) is a parse error:
// a comment is legal only on its own line directly above an expression,
// which makes "every comment is preserved in the AST" structural.
fn spaces<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(
        string("//")
            .with(not_followed_by(token('/')))
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
// full expressions (so `#[foo(1 + 2, "x")]` is legal). Like `leading_comments`,
// an attribute is only ever consumed by `leading_decorations` at the `expr()`
// entry, so it is legal exactly where a comment is — on its own line directly
// above an expression. The leading `attempt(string("#["))` makes the branch
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
            sep_by_tok(expr(), csep(), token(')')),
        ))),
    )
        .skip(sptoken(']'))
        .map(|(name, args): (ArcStr, Option<LPooled<Vec<Expr>>>)| {
            let mut args = args.unwrap_or_else(LPooled::take);
            Attr { name, args: Arc::from_iter(args.drain(..)) }
        })
}

// Capture the run of own-line `//` comments and `#[..]` attributes directly
// above an expression, returning them as two flat lists (comments, attrs).
// They may interleave in the source; the relative order between a comment and
// an attribute is not retained (each printer emits comments then attrs in a
// fixed order), which is fine because `Decorations` is invisible to `Expr`
// equality. This replaces `leading_comments` at the `expr()` entry;
// `leading_comments` itself is kept for the `.gxi` `sig_item` path.
fn leading_decorations<I>()
-> impl Parser<I, Output = (LPooled<Vec<ArcStr>>, LPooled<Vec<Attr>>)>
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

fn spaces1<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    space().with(spaces())
}

fn doc_comment<I>() -> impl Parser<I, Output = Doc>
where
    I: RangeStream<Token = char>,
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(string(s))
}

fn ident<I>(cap: bool) -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char>,
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(false).then(|s| {
        if RESERVED.contains(&s.as_str()) {
            unexpected_any("can't use keyword as a function or variable name").left()
        } else {
            value(s).right()
        }
    })
}

fn spfname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(fname())
}

fn typname<I>() -> impl Parser<I, Output = ArcStr>
where
    I: RangeStream<Token = char>,
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

pub(crate) fn modpath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    sep_by1(fname(), string("::"))
        .map(|mut v: LPooled<Vec<ArcStr>>| ModPath(Path::from_iter(v.drain(..))))
}

fn spmodpath<I>() -> impl Parser<I, Output = ModPath>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(modpath())
}

fn csep<I>() -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces().with(token(','))).skip(spaces())
}

fn semisep<I>() -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces().with(token(';'))).skip(spaces())
}

fn sptoken<I>(t: char) -> impl Parser<I, Output = char>
where
    I: RangeStream<Token = char>,
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

/// A `{ e; e; ... }` block accepting ONE OR MORE exprs, for positions
/// where a block is syntactically REQUIRED (sync-block bodies, for
/// bodies) and the `{x}` / struct-literal / map-postfix ambiguities
/// can't arise. Produces `Do` (possibly of one expr — only these
/// positions parse that shape back, and the printer prints for-bodies
/// and sync blocks with braces always, so round-trips hold).
fn block1<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(
            sptoken('{'),
            sptoken('}'),
            sep_by1_tok_exp(expr(), semisep(), token('}'), |pos| {
                ExprKind::NoOp.to_expr(pos)
            }),
        ),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            let exprs = Arc::from_iter(args.drain(..));
            ExprKind::Do { exprs }.to_expr(pos)
        })
}

/// `sync { e; e; ... }` — the sync-subset block (sequential
/// semantics; design/sync_subset.md).
fn sync_block<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position().skip(attempt(string("sync").with(not_prefix()))),
        spaces().with(between(
            sptoken('{'),
            sptoken('}'),
            sep_by1_tok_exp(expr(), semisep(), token('}'), |pos| {
                ExprKind::NoOp.to_expr(pos)
            }),
        )),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            let exprs = Arc::from_iter(args.drain(..));
            ExprKind::SyncBlock { exprs }.to_expr(pos)
        })
}

/// `for <pattern> in <iter> { body }` — legal only inside sync blocks
/// (enforced at compile). `in` is contextual, not reserved (labeled
/// args like `#in` stay valid).
fn for_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position().skip(attempt(string("for").with(not_prefix()))),
        spaces1().with(structure_pattern()),
        spaces().skip(string("in").skip(not_prefix())).with(expr()),
        block1(),
    )
        .map(|(pos, pattern, iter, body)| {
            ExprKind::For { pattern, iter: Arc::new(iter), body: Arc::new(body) }
                .to_expr(pos)
        })
}

/// `name = value` — rebind a `let mut` local (sync blocks only;
/// enforced at compile). The lookahead rejects `==` and `=>`.
fn assign<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(
            spmodpath()
                .skip(spaces())
                .skip(token('='))
                .skip(not_followed_by(one_of("=>".chars()))),
        ),
        expr(),
    )
        .map(|(pos, name, value)| {
            ExprKind::Assign { name, value: Arc::new(value) }.to_expr(pos)
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
            sep_by_tok(expr(), csep(), token(')')),
        )),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            ExprKind::Any { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

fn letbind<I>() -> impl Parser<I, Output = Expr>
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
                optional(attempt(string("mut").with(spaces1()))),
                structure_pattern(),
                spaces().with(optional(token(':').with(typ()))),
            ))
            .skip(sptoken('=')),
        expr(),
    )
        .map(|(pos, (rec, mut_, pattern, typ), value)| {
            let rec = rec.is_some();
            let mut_ = mut_.is_some();
            ExprKind::Bind(Arc::new(BindExpr { rec, mut_, pattern, typ, value }))
                .to_expr(pos)
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
    (position(), parse_value(&VAL_MUST_ESC, &VAL_ESC).skip(not_prefix())).then(
        |(pos, v)| match v {
            Value::String(_) => {
                unexpected_any("parse error in string interpolation").left()
            }
            v => value(ExprKind::Constant(v).to_expr(pos)).right(),
        },
    )
}

fn reference<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), modpath()).map(|(pos, name)| ExprKind::Ref { name }.to_expr(pos))
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

fn raw_string<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    static MUST_ESC: [char; 2] = ['\\', '\''];
    static ESC: LazyLock<Escape> =
        LazyLock::new(|| Escape::new('\\', &MUST_ESC, &[], None).unwrap());
    (
        position(),
        between(attempt(string("r\'")), token('\''), escaped_string(&MUST_ESC, &ESC)),
    )
        .map(|(pos, s): (_, String)| {
            ExprKind::Constant(Value::String(s.into())).to_expr(pos)
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
                    (pattern(), spstring("=>").with(expr())),
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

fn structure<I>() -> impl Parser<I, Output = Expr>
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
            spaces().with(sep_by1_tok(
                (fname(), spaces().with(optional(token(':').with(expr())))),
                csep(),
                token('}'),
            )),
        ),
    )
        .then(|(pos, mut exprs): (_, LPooled<Vec<(ArcStr, Option<Expr>)>>)| {
            let s = exprs.iter().map(|(n, _)| n).collect::<LPooled<AHashSet<_>>>();
            if s.len() < exprs.len() {
                return unexpected_any("struct fields must be unique").left();
            }
            drop(s);
            exprs.sort_by_key(|(n, _)| n.clone());
            let args = exprs.drain(..).map(|(n, e)| match e {
                Some(e) => (n, e),
                None => {
                    let e = ExprKind::Ref { name: [n.clone()].into() }.to_expr(pos);
                    (n, e)
                }
            });
            value(
                ExprKind::Struct(StructExpr { args: Arc::from_iter(args) }).to_expr(pos),
            )
            .right()
        })
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
            sep_by_tok((expr(), spstring("=>").with(expr())), csep(), token('}')),
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
                sep_by1_tok(
                    (spfname(), spaces().with(optional(token(':').with(expr())))),
                    csep(),
                    token('}'),
                ),
            ),
        ),
    )
        .then(
            |(pos, (source, mut exprs)): (
                _,
                (Expr, LPooled<Vec<(ArcStr, Option<Expr>)>>),
            )| {
                let s = exprs.iter().map(|(n, _)| n).collect::<LPooled<AHashSet<_>>>();
                if s.len() < exprs.len() {
                    return unexpected_any("struct fields must be unique").left();
                }
                drop(s);
                exprs.sort_by_key(|(n, _)| n.clone());
                let exprs = exprs.drain(..).map(|(name, e)| match e {
                    Some(e) => (name, e),
                    None => {
                        let e = ExprKind::Ref { name: ModPath::from([name.clone()]) }
                            .to_expr(pos);
                        (name, e)
                    }
                });
                let e = ExprKind::StructWith(StructWithExpr {
                    source: Arc::new(source),
                    replace: Arc::from_iter(exprs),
                })
                .to_expr(pos);
                value(e).right()
            },
        )
}

fn try_catch<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position().skip(attempt(string("try").skip(space()))),
        sep_by1_tok(expr(), semisep(), attempt(string("catch"))),
        spstring("catch").with(between(
            sptoken('('),
            sptoken(')'),
            (spfname(), spaces().with(optional(token(':').with(typ())))),
        )),
        spstring("=>").with(expr()),
    )
        .map(
            |(pos, mut exprs, (bind, constraint), handler): (
                _,
                LPooled<Vec<Expr>>,
                _,
                _,
            )| {
                ExprKind::TryCatch(Arc::new(TryCatchExpr {
                    bind,
                    constraint,
                    exprs: Arc::from_iter(exprs.drain(..)),
                    handler: Arc::new(handler),
                }))
                .to_expr(pos)
            },
        )
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
        (
            leading_decorations(),
            choice((
                module(),
                use_module(),
                try_catch(),
                typedef(),
                sync_block(),
                for_expr(),
                letbind(),
                attempt(lambda()),
                attempt(connect()),
                assign(),
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
            .map(
                |((comments, attrs), mut e): (
                    (LPooled<Vec<ArcStr>>, LPooled<Vec<Attr>>),
                    Expr,
                )| {
                    if !comments.is_empty() || !attrs.is_empty() {
                        e.dec = Some(Box::new(Decorations {
                            comments: comments.iter().cloned().collect(),
                            attrs: attrs.iter().cloned().collect(),
                            trailing: Arc::from_iter(std::iter::empty::<ArcStr>()),
                        }));
                    }
                    e
                },
            )
    }
}

/// Parse one or more expressions
///
/// followed by (optional) whitespace and then eof. At least one
/// expression is required otherwise this function will fail.
pub fn parse(ori: Origin) -> anyhow::Result<Arc<[Expr]>> {
    let ori = Arc::new(ori);
    set_origin(ori.clone());
    let mut r: LPooled<Vec<Expr>> =
        sep_by1_tok_exp(expr(), semisep(), eof(), |pos| ExprKind::NoOp.to_expr(pos))
            .skip(spaces())
            .skip(eof())
            .easy_parse(position::Stream::new(&*ori.text))
            .map(|(r, _)| r)
            .map_err(|e| {
                let pc = ParserContext { ori: ori.clone(), pos: e.position };
                anyhow::Error::msg(format!("{e}")).context(pc)
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
    let mut r: LPooled<Vec<SigItem>> = sep_by1_tok(sig_item(), semisep(), eof())
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(&*ori.text))
        .map(|(r, _)| r)
        .map_err(|e| {
            anyhow::anyhow!("{e}")
                .context(ParserContext { ori: ori.clone(), pos: e.position })
        })?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)) })
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    expr()
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
    fntype()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| {
            anyhow::anyhow!("{e}").context(ParserContext {
                ori: Arc::new(Origin::from_str(s)),
                pos: e.position,
            })
        })
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    typ()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| {
            anyhow::anyhow!("{e}").context(ParserContext {
                ori: Arc::new(Origin::from_str(s)),
                pos: e.position,
            })
        })
}

pub(super) fn parse_modpath(s: &str) -> anyhow::Result<ModPath> {
    modpath()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| {
            anyhow::anyhow!("{e}").context(ParserContext {
                ori: Arc::new(Origin::from_str(s)),
                pos: e.position,
            })
        })
}
