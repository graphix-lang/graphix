use crate::expr::{
    ApplyExpr, BinOp, Expr, ExprKind, Name,
    parser::{
        any, apply_args, array, array_index_suffix,
        arrayexp::Index,
        brace, cast, construct, csep, expr, fldname,
        grow::{grow, max_nesting, note_refused},
        interpolated, list_lit, literal, never_expr, raw_string, reference, select,
        sep_by1_tok, seq, spaces, sptoken, variant,
    },
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice,
    error::StreamError,
    many, not_followed_by,
    parser::char::string,
    position, satisfy,
    stream::{Range, StreamErrorFor, position::SourcePosition},
    token, unexpected_any,
};
use netidx_value::parser::int;
use poolshark::local::LPooled;
use triomphe::Arc;

/// A prefix operator: `&e`, `*e`, `-e`, `!e`. The operand is an
/// `arith_term`, so a prefix binds looser than the postfix operators.
fn prefix<I>(
    op: char,
    key: bool,
    mk: fn(Arc<Expr>) -> ExprKind,
) -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token(op).with(arith_term(key)))
        .map(move |(pos, e)| mk(Arc::new(e)).to_expr(pos))
}

/// A postfix operator applied to a primary in `arith_term`'s postfix loop.
enum Post {
    Field(Name),                                // `.name`  -> StructRef
    Index(usize),                               // `.0`     -> TupleRef
    Array(Index),                               // `[i]`/`[a..b]`
    Key(Expr),                                  // `{k}`    -> MapRef
    Call(LPooled<Vec<(Option<ArcStr>, Expr)>>), // `(args)` -> Apply
    Qop,                                        // `?`      -> Qop
    OrNever,                                    // `$`      -> OrNever
}

// Each alternative is `attempt`-wrapped so a partial parse (the `{` of a
// map access that is really a block) ends the postfix loop cleanly.
// `key` admits `{k}`, written against its source like a call's `(`; the
// head of a `seq` refuses it so that its body is not read as a map access
// of the trigger.
fn postfix_op<I>(key: bool) -> impl Parser<I, Output = Post>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(
            sptoken('.').with(choice((
                attempt(int::<_, usize>()).map(Post::Index),
                spaces()
                    .with((position(), fldname()))
                    .map(|(pos, n)| Post::Field(Name::written(n, pos))),
            ))),
        ),
        attempt(array_index_suffix()).map(Post::Array),
        attempt(combine::value(()).then(move |()| match key {
            true => between(token('{'), sptoken('}'), expr()).map(Post::Key).left(),
            false => unexpected_any("a seq trigger takes no map access").right(),
        })),
        attempt(apply_args()).map(Post::Call),
        // `?`/`$` chain innermost first: `x?$` takes the errors off `x`,
        // then the null
        attempt(spaces().with(choice((
            token('?').map(|_| Post::Qop),
            token('$').map(|_| Post::OrNever),
        )))),
    ))
}

fn apply_post(pos: SourcePosition, src: Expr, op: Post) -> Expr {
    match op {
        Post::Field(field) => {
            ExprKind::StructRef { source: Arc::new(src), field }.to_expr(pos)
        }
        Post::Index(field) => {
            ExprKind::TupleRef { source: Arc::new(src), field }.to_expr(pos)
        }
        Post::Array(Index::At(i)) => {
            ExprKind::ArrayRef { source: Arc::new(src), i: Arc::new(i) }.to_expr(pos)
        }
        Post::Array(Index::Slice(start, end)) => ExprKind::ArraySlice {
            source: Arc::new(src),
            start: start.map(Arc::new),
            end: end.map(Arc::new),
        }
        .to_expr(pos),
        Post::Key(key) => {
            ExprKind::MapRef { source: Arc::new(src), key: Arc::new(key) }.to_expr(pos)
        }
        Post::Call(mut args) => ExprKind::Apply(ApplyExpr {
            function: Arc::new(src),
            args: Arc::from_iter(args.drain(..)),
        })
        .to_expr(pos),
        Post::Qop => ExprKind::Qop(Arc::new(src)).to_expr(pos),
        Post::OrNever => ExprKind::OrNever(Arc::new(src)).to_expr(pos),
    }
}

/// A single parenthesized expr: `ExplicitParens` if no postfix follows,
/// else the bare postfix source.
struct Parenthesized;

fn paren_group<I>() -> impl Parser<I, Output = (Expr, Option<Parenthesized>)>
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
                (exprs.drain(..).next().unwrap(), Some(Parenthesized))
            } else {
                (
                    ExprKind::Tuple { args: Arc::from_iter(exprs.drain(..)) }
                        .to_expr(pos),
                    None,
                )
            }
        })
}

fn primary<I>(key: bool) -> impl Parser<I, Output = (Expr, Option<Parenthesized>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        prefix('!', key, |expr| ExprKind::Not { expr }).map(|e| (e, None)),
        raw_string().map(|e| (e, None)),
        list_lit().map(|e| (e, None)),
        array().map(|e| (e, None)),
        prefix('&', key, ExprKind::ByRef).map(|e| (e, None)),
        prefix('*', key, ExprKind::Deref).map(|e| (e, None)),
        select().map(|e| (e, None)),
        seq().map(|e| (e, None)),
        variant().map(|e| (e, None)),
        cast().map(|e| (e, None)),
        never_expr().map(|e| (e, None)),
        any().map(|e| (e, None)),
        interpolated().map(|e| (e, None)),
        brace().map(|e| (e, None)),
        paren_group(),
        attempt(literal()).map(|e| (e, None)),
        // after `literal()`, so that a signed numeric literal is a constant
        prefix('-', key, ExprKind::Neg).map(|e| (e, None)),
        construct().map(|e| (e, None)),
        reference().map(|e| (e, None)),
    ))
}

parser! {
    pub(crate) fn arith_term[I](key: bool)(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(spaces()
            .with(
                (
                    position(),
                    primary(*key),
                    position(),
                    many::<LPooled<Vec<(Post, SourcePosition)>>, _, _>((
                        postfix_op(*key),
                        position(),
                    )),
                    position(),
                )
                    .and_then(|(pos, (base, paren), end, mut ops, chain_end)| {
                        let base = base.ending(end);
                        // The iterative postfix loop escapes `grow`'s depth
                        // counter, but the fold builds an N-deep AST.
                        if ops.len() > max_nesting() {
                            note_refused(chain_end);
                            return Err(<StreamErrorFor<I>>::message_static_message(
                                "expression nesting too deep",
                            ));
                        }
                        // `?`/`$` print their operand bare, so a
                        // parenthesized one keeps its parens
                        let base = match (paren, ops.first()) {
                            (Some(Parenthesized), None | Some((Post::Qop | Post::OrNever, _))) => {
                                ExprKind::ExplicitParens(Arc::new(base)).to_expr(pos).ending(end)
                            }
                            _ => base,
                        };
                        Ok(ops
                            .drain(..)
                            .fold(base, |acc, (op, end)| apply_post(pos, acc, op).ending(end)))
                    }),
            ))
    }
}

/// Shunting-yard: build the tree the operators' precedence says.
fn shunting_yard(first: Expr, mut rest: LPooled<Vec<(BinOp, Expr)>>) -> Expr {
    fn reduce(output: &mut Vec<Expr>, op: BinOp) {
        let rhs = output.pop().unwrap();
        let lhs = output.pop().unwrap();
        let (pos, end) = (lhs.pos, rhs.end.0);
        output.push(op.build(Arc::new(lhs), Arc::new(rhs)).to_expr(pos).ending(end));
    }
    let mut output: LPooled<Vec<Expr>> = LPooled::take();
    let mut ops: LPooled<Vec<BinOp>> = LPooled::take();
    output.push(first);
    for (op, expr) in rest.drain(..) {
        while let Some(&top) = ops.last()
            && top.precedence() >= op.precedence()
        {
            ops.pop();
            reduce(&mut output, top);
        }
        ops.push(op);
        output.push(expr);
    }
    while let Some(op) = ops.pop() {
        reduce(&mut output, op);
    }
    output.pop().unwrap()
}

/// A binary operator token.
fn binop<I>() -> impl Parser<I, Output = BinOp>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice(BinOp::ALL.map(|op| {
        attempt(
            string(op.token())
                .skip(not_followed_by(satisfy(move |c| op.not_before() == Some(c))))
                .map(move |_| op),
        )
    }))
}

parser! {
    pub(crate) fn arith[I](key: bool)(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow((
            arith_term(*key),
            many((attempt(spaces().with(binop())), arith_term(*key))),
            position(),
        ).and_then(|(e, exprs, end): (Expr, LPooled<Vec<(BinOp, Expr)>>, _)| {
            // The iterative operator chain builds one AST level per operator.
            if exprs.len() > max_nesting() {
                note_refused(end);
                return Err(<StreamErrorFor<I>>::message_static_message(
                    "expression nesting too deep",
                ));
            }
            Ok(if exprs.is_empty() {
                e
            } else {
                shunting_yard(e, exprs)
            })
        }))
    }
}
