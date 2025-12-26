use crate::expr::{
    parser::{
        any, apply, array, arrayref, cast, do_block, interpolated, literal, map, mapref,
        qop, raw_string, reference, select, spaces, sptoken, structref, structure,
        structwith, tuple, tupleref, variant,
    },
    Expr, ExprKind,
};
use combine::{
    attempt, between, choice, many,
    parser::char::string,
    position,
    stream::{position::SourcePosition, Range},
    token, ParseError, Parser, RangeStream,
};
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

parser! {
    pub(crate) fn arith_term[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        spaces()
            .with(choice((
                raw_string(),
                array(),
                byref_arith(),
                qop(deref_arith()),
                qop(select()),
                variant(),
                qop(cast()),
                qop(any()),
                interpolated(),
                (position(), token('!').with(arith()))
                    .map(|(pos, expr)| ExprKind::Not { expr: Arc::new(expr) }.to_expr(pos)),
                attempt(tuple()),
                attempt(map()),
                attempt(structure()),
                attempt(structwith()),
                qop(do_block()),
                attempt(qop(mapref())),
                attempt(qop(arrayref())),
                attempt(qop(tupleref())),
                attempt(qop(structref())),
                attempt(qop(apply())),
                between(token('('), sptoken(')'), spaces().with(arith())),
                attempt(literal()),
                qop(reference()),
            )))
            .skip(spaces())
    }
}

fn mke(lhs: Expr, op: &'static str, rhs: Expr) -> Expr {
    match op {
        "+" => {
            let pos = lhs.pos;
            ExprKind::Add { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "-" => {
            let pos = lhs.pos;
            ExprKind::Sub { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "*" => {
            let pos = lhs.pos;
            ExprKind::Mul { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "/" => {
            let pos = lhs.pos;
            ExprKind::Div { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "%" => {
            let pos = lhs.pos;
            ExprKind::Mod { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "==" => {
            let pos = lhs.pos;
            ExprKind::Eq { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "!=" => {
            let pos = lhs.pos;
            ExprKind::Ne { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        ">" => {
            let pos = lhs.pos;
            ExprKind::Gt { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "<" => {
            let pos = lhs.pos;
            ExprKind::Lt { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        ">=" => {
            let pos = lhs.pos;
            ExprKind::Gte { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "<=" => {
            let pos = lhs.pos;
            ExprKind::Lte { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "&&" => {
            let pos = lhs.pos;
            ExprKind::And { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "||" => {
            let pos = lhs.pos;
            ExprKind::Or { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        "~" => {
            let pos = lhs.pos;
            ExprKind::Sample { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
        }
        _ => unreachable!(),
    }
}

/// Returns (precedence, left_associative) for an operator.
/// Higher precedence binds tighter.
fn precedence(op: &str) -> (u8, bool) {
    match op {
        "||" => (1, true),
        "&&" => (2, true),
        "==" | "!=" => (3, true),
        "<" | ">" | "<=" | ">=" => (4, true),
        "+" | "-" => (5, true),
        "/" | "%" => (6, true),
        "*" => (7, true),
        "~" => (8, true),
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

fn infix_arith<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
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
                string("<"),
                string("+"),
                string("-"),
                string("*"),
                string("/"),
                string("%"),
                string("~"),
            )))),
            arith_term(),
        )),
    )
        .map(
            |(e, exprs): (Expr, LPooled<Vec<(&'static str, Expr)>>)| {
                if exprs.is_empty() {
                    e
                } else {
                    shunting_yard(e, exprs)
                }
            },
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
