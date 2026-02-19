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
                (position(), token('!').with(arith_term()))
                    .map(|(pos, expr)| ExprKind::Not { expr: Arc::new(expr) }.to_expr(pos)),
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
                (position(), between(token('('), sptoken(')'), spaces().with(arith()))).map(|(pos, e)| {
                    ExprKind::ExplicitParens(Arc::new(e)).to_expr(pos)
                }),
                attempt(literal()),
                qop(reference()),
            )))
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
        "-" => mk!(Sub),
        "*" => mk!(Mul),
        "/" => mk!(Div),
        "%" => mk!(Mod),
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
        "+" | "-" => (5, true),
        "/" | "%" => (6, true),
        "*" => (7, true),
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
        ).map(|(e, exprs): (Expr, LPooled<Vec<(&'static str, Expr)>>)| {
            if exprs.is_empty() {
                e
            } else {
                shunting_yard(e, exprs)
            }
        })
    }
}
