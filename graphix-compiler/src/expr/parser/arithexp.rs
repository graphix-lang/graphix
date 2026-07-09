use crate::expr::{
    ApplyExpr, Expr, ExprKind,
    parser::{
        any, apply_args, array, array_index_suffix, cast, csep, do_block, expr,
        interpolated, literal, map, raw_string, reference, select, spaces, spfname,
        sptoken, structure, structwith, variant,
    },
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, many, not_followed_by,
    optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token,
};
use netidx::utils::Either;
use netidx_value::parser::{int, sep_by1_tok};
use poolshark::local::LPooled;
use triomphe::Arc;

fn byref_arith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('&').with(arith_term()))
        .map(|(pos, expr)| ExprKind::ByRef(Arc::new(expr)).to_expr(pos))
}

fn deref_arith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('*').with(arith_term()))
        .map(|(pos, expr)| ExprKind::Deref(Arc::new(expr)).to_expr(pos))
}

// Unary minus. Tried AFTER `literal()` in the `arith_term` choice so a
// signed numeric literal (`-2.5`, `-2`) is consumed as a `Constant`; only
// a non-literal operand (`-x`, `-(a + b)`, `-f(x)`) becomes a `Neg` node.
fn neg_arith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('-').with(arith_term()))
        .map(|(pos, expr)| ExprKind::Neg(Arc::new(expr)).to_expr(pos))
}

/// A postfix operator applied to a primary in `arith_term`'s postfix loop.
enum Post {
    Field(ArcStr),                                     // `.name`  -> StructRef
    Index(usize),                                      // `.0`     -> TupleRef
    Array(Either<(Option<Expr>, Option<Expr>), Expr>), // `[i]`/`[a..b]`
    Key(Expr),                                         // `{k}`    -> MapRef
    Call(Vec<(Option<ArcStr>, Expr)>),                 // `(args)` -> Apply
}

enum QopSuffix {
    Qop,
    OrNever,
}

// One postfix operator. Each alternative is `attempt`-wrapped so a partial
// parse (e.g. the `{` of a map-key access that turns out to be a block)
// backtracks and the postfix loop ends cleanly rather than hard-failing.
fn postfix_op<I>() -> impl Parser<I, Output = Post>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(sptoken('.').with(choice((
            attempt(int::<_, usize>()).map(Post::Index),
            spfname().map(Post::Field),
        )))),
        attempt(array_index_suffix()).map(Post::Array),
        // TIGHT brace: `m{"k"}` is a map access, `m {"k"}` is not — the
        // whitespace disambiguates a postfix key from a BLOCK following
        // an expression (`for v in a { .. }`, `select a { .. }`,
        // sync-subset blocks). Map accesses have always been written
        // tight; a space here now ends the postfix chain.
        attempt(between(token('{'), sptoken('}'), expr())).map(Post::Key),
        attempt(apply_args()).map(Post::Call),
    ))
}

// The optional trailing `?`/`$` error operator. Mirrors the tail of the old
// `qop` combinator; applied once to the whole postfix expression.
fn qop_suffix<I>() -> impl Parser<I, Output = QopSuffix>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces().with(choice((
        token('?').map(|_| QopSuffix::Qop),
        token('$').map(|_| QopSuffix::OrNever),
    ))))
}

fn apply_post(pos: SourcePosition, src: Expr, op: Post) -> Expr {
    let source = Arc::new(src);
    match op {
        Post::Field(field) => ExprKind::StructRef { source, field }.to_expr(pos),
        Post::Index(field) => ExprKind::TupleRef { source, field }.to_expr(pos),
        Post::Array(Either::Right(i)) => {
            ExprKind::ArrayRef { source, i: Arc::new(i) }.to_expr(pos)
        }
        Post::Array(Either::Left((start, end))) => ExprKind::ArraySlice {
            source,
            start: start.map(Arc::new),
            end: end.map(Arc::new),
        }
        .to_expr(pos),
        Post::Key(key) => ExprKind::MapRef { source, key: Arc::new(key) }.to_expr(pos),
        Post::Call(args) => {
            ExprKind::Apply(ApplyExpr { function: source, args: Arc::from(args) })
                .to_expr(pos)
        }
    }
}

// `( e )` or `( a, b, .. )`. Yields the bare inner expr plus a marker:
// `Some(())` for a single parenthesized expr — it becomes `ExplicitParens` if
// no postfix follows, else its parens are stripped and it is used directly as
// the postfix source (matching the old `ref_pexp`/`apply_pexp` `between`).
// `None` marks a tuple (>= 2 elements).
fn paren_group<I>() -> impl Parser<I, Output = (Expr, Option<()>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(token('('), sptoken(')'), sep_by1_tok(expr(), csep(), token(')'))),
    )
        .map(|(pos, mut exprs): (_, LPooled<Vec<Expr>>)| {
            if exprs.len() == 1 {
                (exprs.drain(..).next().unwrap(), Some(()))
            } else {
                (
                    ExprKind::Tuple { args: Arc::from_iter(exprs.drain(..)) }
                        .to_expr(pos),
                    None,
                )
            }
        })
}

// A primary expression — everything that can begin a postfix expression. The
// prefix-operator forms (`!`, `&`, `*`, unary `-`) recurse into `arith_term`,
// so they bind looser than the postfix operators. `paren_group` carries the
// "is a single `( e )`" marker; every other primary is `None`.
fn primary<I>() -> impl Parser<I, Output = (Expr, Option<()>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        (position(), token('!').with(arith_term()))
            .map(|(pos, e)| (ExprKind::Not { expr: Arc::new(e) }.to_expr(pos), None)),
        raw_string().map(|e| (e, None)),
        array().map(|e| (e, None)),
        byref_arith().map(|e| (e, None)),
        deref_arith().map(|e| (e, None)),
        select().map(|e| (e, None)),
        variant().map(|e| (e, None)),
        cast().map(|e| (e, None)),
        any().map(|e| (e, None)),
        interpolated().map(|e| (e, None)),
        (position(), token('!').with(arith()))
            .map(|(pos, e)| (ExprKind::Not { expr: Arc::new(e) }.to_expr(pos), None)),
        attempt(map()).map(|e| (e, None)),
        attempt(structure()).map(|e| (e, None)),
        attempt(structwith()).map(|e| (e, None)),
        do_block().map(|e| (e, None)),
        paren_group(),
        attempt(literal()).map(|e| (e, None)),
        neg_arith().map(|e| (e, None)),
        reference().map(|e| (e, None)),
    ))
}

parser! {
    pub(crate) fn arith_term[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        spaces()
            .with(
                (
                    position(),
                    primary(),
                    many::<LPooled<Vec<Post>>, _, _>(postfix_op()),
                    optional(qop_suffix()),
                )
                    .map(|(pos, (base, paren), mut ops, qop)| {
                        let folded = if ops.is_empty() {
                            match paren {
                                Some(()) => {
                                    ExprKind::ExplicitParens(Arc::new(base)).to_expr(pos)
                                }
                                None => base,
                            }
                        } else {
                            ops.drain(..).fold(base, |acc, op| apply_post(pos, acc, op))
                        };
                        match qop {
                            None => folded,
                            Some(QopSuffix::Qop) => {
                                ExprKind::Qop(Arc::new(folded)).to_expr(pos)
                            }
                            Some(QopSuffix::OrNever) => {
                                ExprKind::OrNever(Arc::new(folded)).to_expr(pos)
                            }
                        }
                    }),
            )
            .skip(spaces())
    }
}

fn mke(lhs: Expr, op: &'static str, rhs: Expr) -> Expr {
    macro_rules! mk {
        ($ctor:ident) => {{
            let pos = lhs.pos;
            ExprKind::$ctor { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }};
    }
    match op {
        "+" => mk!(Add),
        "+?" => mk!(CheckedAdd),
        "-" => mk!(Sub),
        "-?" => mk!(CheckedSub),
        "*" => mk!(Mul),
        "*?" => mk!(CheckedMul),
        "/" => mk!(Div),
        "/?" => mk!(CheckedDiv),
        "%" => mk!(Mod),
        "%?" => mk!(CheckedMod),
        "==" => mk!(Eq),
        "!=" => mk!(Ne),
        ">" => mk!(Gt),
        "<" => mk!(Lt),
        ">=" => mk!(Gte),
        "<=" => mk!(Lte),
        "&&" => mk!(And),
        "||" => mk!(Or),
        "~" => mk!(Sample),
        _ => unreachable!(),
    }
}

/// Returns (precedence, left_associative) for an operator.
/// Higher precedence binds tighter.
pub(crate) fn precedence(op: &str) -> (u8, bool) {
    match op {
        "~" => (0, true),
        "||" => (1, true),
        "&&" => (2, true),
        "==" | "!=" => (3, true),
        "<" | ">" | "<=" | ">=" => (4, true),
        "+" | "+?" | "-" | "-?" => (5, true),
        "/" | "/?" | "%" | "%?" => (6, true),
        "*" | "*?" => (7, true),
        _ => unreachable!(),
    }
}

/// Shunting-yard algorithm to build an expression tree respecting precedence.
/// Thank you Djikstra.
fn shunting_yard(first: Expr, mut rest: LPooled<Vec<(&'static str, Expr)>>) -> Expr {
    let mut output: LPooled<Vec<Expr>> = LPooled::take();
    let mut ops: LPooled<Vec<&'static str>> = LPooled::take();
    output.push(first);
    for (op, expr) in rest.drain(..) {
        let (prec, left_assoc) = precedence(op);
        while let Some(&top) = ops.last() {
            let (top_prec, _) = precedence(top);
            if top_prec > prec || (top_prec == prec && left_assoc) {
                let rhs = output.pop().unwrap();
                let lhs = output.pop().unwrap();
                output.push(mke(lhs, ops.pop().unwrap(), rhs));
            } else {
                break;
            }
        }
        ops.push(op);
        output.push(expr);
    }
    while let Some(op) = ops.pop() {
        let rhs = output.pop().unwrap();
        let lhs = output.pop().unwrap();
        output.push(mke(lhs, op, rhs));
    }
    output.pop().unwrap()
}

parser! {
    pub(crate) fn arith[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        (
            arith_term(),
            many((
                attempt(spaces().with(choice((
                    attempt(string("==")),
                    attempt(string("!=")),
                    attempt(string(">=")),
                    attempt(string("<=")),
                    attempt(string("&&")),
                    attempt(string("||")),
                    string(">"),
                    // `<` must not swallow the `<-` of a connect: with unary
                    // minus, `a <- b` would otherwise read as `a < (-b)`.
                    // `a < -b` (space before `-`) still parses as less-than.
                    attempt(string("<").skip(not_followed_by(token('-')))),
                    attempt(string("+?")),
                    attempt(string("+")),
                    attempt(string("-?")),
                    attempt(string("-")),
                    attempt(string("*?")),
                    attempt(string("*")),
                    attempt(string("/?")),
                    attempt(string("/")),
                    attempt(string("%?")),
                    attempt(string("%")),
                    string("~"),
                )))),
                arith_term(),
            )),
        ).map(|(e, exprs): (Expr, LPooled<Vec<(&'static str, Expr)>>)| {
            if exprs.is_empty() {
                e
            } else {
                shunting_yard(e, exprs)
            }
        })
    }
}
