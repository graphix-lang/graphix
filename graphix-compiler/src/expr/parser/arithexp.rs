use crate::expr::{
    parser::{
        any, apply, array, arrayref, cast, do_block, interpolated, literal, map, mapref,
        qop, raw_string, reference, select, spaces, sptoken, structref, structure,
        structwith, tuple, tupleref, variant,
    },
    Expr, ExprKind,
};
use combine::{
    attempt, between, chainl1, choice,
    parser::char::string,
    position,
    stream::{position::SourcePosition, Range},
    token, ParseError, Parser, RangeStream,
};
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

parser! {
    pub(crate) fn arith_term[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        spaces()
            .with(choice((
                qop(deref_arith()),
                raw_string(),
                array(),
                byref_arith(),
                qop(select()),
                variant(),
                qop(cast()),
                qop(any()),
                interpolated(),
                (position(), token('!').with(arith()))
                    .map(|(pos, expr)| ExprKind::Not { expr: Arc::new(expr) }.to_expr(pos)),
                attempt(tuple()),
                attempt(map()),
                attempt(structwith()),
                attempt(structure()),
                qop(do_block()),
                attempt(qop(arrayref())),
                attempt(qop(tupleref())),
                attempt(qop(structref())),
                attempt(qop(mapref())),
                attempt(qop(apply())),
                between(token('('), sptoken(')'), spaces().with(arith())),
                attempt(literal()),
                qop(reference()),
            )))
            .skip(spaces())
    }
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

/*
fn oplist<I>() -> impl Parser<I, Output = (LPooled<Vec<(Expr, &'static str)>>, Expr)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        many1((
            arith_term(),
            choice((
                string("+"),
                string("-"),
                string("*"),
                string("/"),
                string("%"),
                string("=="),
                string("!="),
                string(">="),
                string("<="),
                string(">"),
                string("<"),
                string("&&"),
                string("||"),
                string("~"),
            )),
        )),
        arith_term(),
    )
}
*/

pub(super) fn infix_arith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    chainl1(
        arith_term(),
        choice((
            attempt(string("==")),
            attempt(string("!=")),
            attempt(string(">=")),
            attempt(string("<=")),
            attempt(string("&&")),
            attempt(string("||")),
            string(">"),
            string("<"),
            string("+"),
            string("-"),
            string("*"),
            string("/"),
            string("%"),
            string("~"),
        ))
        .map(|op: &str| match op {
            "+" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Add { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "-" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Sub { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "*" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Mul { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "/" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Div { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "%" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Mod { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "==" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Eq { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "!=" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Ne { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            ">" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Gt { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "<" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Lt { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            ">=" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Gte { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "<=" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Lte { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "&&" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::And { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "||" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Or { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            "~" => |lhs: Expr, rhs: Expr| {
                let pos = lhs.pos;
                ExprKind::Sample { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
            },
            _ => unreachable!(),
        }),
    )
}

parser! {
    pub(crate) fn arith[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        choice((
            attempt(infix_arith()),
            (position(), token('!').with(arith_term()))
                .map(|(pos, expr)| ExprKind::Not { expr: Arc::new(expr) }.to_expr(pos)),
            between(token('('), sptoken(')'), spaces().with(arith())),
        ))
    }
}
