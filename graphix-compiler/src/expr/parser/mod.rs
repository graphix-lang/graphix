use crate::{
    expr::{
        set_origin, BindExpr, Doc, Expr, ExprKind, ModPath, Origin, Pattern, SelectExpr,
        Sig, SigItem, StructExpr, StructWithExpr, TryCatchExpr,
    },
    typ::{FnType, Type},
};
use arcstr::{literal, ArcStr};
use combine::{
    attempt, between, choice, eof, look_ahead, many, none_of, not_followed_by, optional,
    parser::{
        char::{alpha_num, space, string},
        combinator::recognize,
        range::{take_while, take_while1},
    },
    position, sep_by1, skip_many,
    stream::{
        position::{self, SourcePosition},
        Range,
    },
    token, unexpected_any, value, EasyParser, ParseError, Parser, RangeStream,
};
use compact_str::CompactString;
use escaping::Escape;
use fxhash::FxHashSet;
use netidx::{path::Path, publisher::Value};
use netidx_value::parser::{
    escaped_string, int, sep_by1_tok, sep_by_tok, value as parse_value, VAL_ESC,
    VAL_MUST_ESC,
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
use lambdaexp::{apply, lambda};

mod arrayexp;
use arrayexp::{array, arrayref};

mod arithexp;
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
pub const RESERVED: LazyLock<FxHashSet<&str>> = LazyLock::new(|| {
    FxHashSet::from_iter([
        "true", "false", "ok", "null", "mod", "let", "select", "pub", "type", "fn",
        "cast", "if", "i8", "u8", "i16", "u16", "u32", "v32", "i32", "z32", "u64", "v64",
        "i64", "z64", "f32", "f64", "decimal", "datetime", "duration", "bool", "string",
        "bytes", "result", "null", "_", "?", "fn", "Array", "Map", "any", "Any", "use",
        "rec", "catch", "try",
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

fn spaces<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    combine::parser::char::spaces().with(skip_many(
        attempt(string("//").with(not_followed_by(token('/'))))
            .with(skip_many(none_of(['\n'])))
            .with(combine::parser::char::spaces()),
    ))
}

fn spaces1<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    space().with(spaces())
}

fn not_prefix<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    not_followed_by(choice((token('_'), alpha_num())))
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

fn structref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), ref_pexp().skip(sptoken('.')), spfname()).map(|(pos, source, field)| {
        ExprKind::StructRef { source: Arc::new(source), field }.to_expr(pos)
    })
}

fn tupleref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), ref_pexp().skip(sptoken('.')), int::<_, usize>()).map(
        |(pos, source, field)| {
            ExprKind::TupleRef { source: Arc::new(source), field }.to_expr(pos)
        },
    )
}

fn mapref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), ref_pexp(), between(sptoken('{'), sptoken('}'), expr())).map(
        |(pos, source, key)| {
            ExprKind::MapRef { source: Arc::new(source), key: Arc::new(key) }.to_expr(pos)
        },
    )
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
            sptoken('('),
            sptoken(')'),
            sep_by_tok(expr(), csep(), sptoken(')')),
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
                optional(attempt(string("rec")).with(spaces1())),
                structure_pattern(),
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
    (position(), parse_value(&VAL_MUST_ESC, &VAL_ESC).skip(not_prefix())).then(
        |(pos, v)| match v {
            Value::String(_) => {
                unexpected_any("parse error in st ring interpolation").left()
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
        attempt(string("select")).with(spaces1()).with((
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
        attempt(string("cast")).with(between(sptoken('<'), sptoken('>'), typ())),
        between(sptoken('('), sptoken(')'), expr()),
    )
        .map(|(pos, typ, e)| ExprKind::TypeCast { expr: Arc::new(e), typ }.to_expr(pos))
}

fn tuple<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(token('('), sptoken(')'), sep_by1_tok(expr(), csep(), token(')'))),
    )
        .then(|(pos, mut exprs): (_, LPooled<Vec<Expr>>)| {
            if exprs.len() < 2 {
                unexpected_any("tuples must have at least 2 elements").left()
            } else {
                value(
                    ExprKind::Tuple { args: Arc::from_iter(exprs.drain(..)) }
                        .to_expr(pos),
                )
                .right()
            }
        })
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
            let s = exprs.iter().map(|(n, _)| n).collect::<LPooled<FxHashSet<_>>>();
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
                let s = exprs.iter().map(|(n, _)| n).collect::<LPooled<FxHashSet<_>>>();
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
        spaces().with(choice((
            choice((
                module(),
                use_module(),
                try_catch(),
                typedef(),
                letbind(),
                attempt(connect()),
                attempt(arith()),
                raw_string(),
                array(),
                byref(),
                variant(),
                qop(select()),
                qop(cast()),
                qop(any()),
                interpolated(),
                qop(deref()),
            )),
            attempt(qop(mapref())),
            attempt(qop(arrayref())),
            attempt(qop(tupleref())),
            attempt(qop(structref())),
            attempt(qop(apply())),
            attempt(tuple()),
            attempt(map()),
            attempt(structure()),
            attempt(structwith()),
            qop(do_block()),
            attempt(lambda()),
            between(token('('), sptoken(')'), expr()),
            attempt(literal()),
            qop(reference())
        )))
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
            .map_err(|e| anyhow::anyhow!(format!("{}", e)))?;
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
        .map_err(|e| anyhow::anyhow!(format!("{}", e)))?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)) })
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    expr()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(&*s))
        .map(|(r, _)| r)
        .map_err(|e| anyhow::anyhow!(format!("{e}")))
}

#[cfg(test)]
pub fn test_parse_mapref(s: &str) -> anyhow::Result<Expr> {
    mapref()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(&*s))
        .map(|(r, _)| r)
        .map_err(|e| anyhow::anyhow!(format!("{e}")))
}

/// Parse one fntype expression
pub fn parse_fn_type(s: &str) -> anyhow::Result<FnType> {
    fntype()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| anyhow::anyhow!(format!("{e}")))
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    typ()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| anyhow::anyhow!(format!("{e}")))
}

pub(super) fn parse_modpath(s: &str) -> anyhow::Result<ModPath> {
    modpath()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(s))
        .map(|(r, _)| r)
        .map_err(|e| anyhow::anyhow!(format!("{e}")))
}
