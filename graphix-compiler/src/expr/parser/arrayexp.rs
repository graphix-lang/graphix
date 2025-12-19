use crate::expr::{
    parser::{csep, expr, ref_pexp, sep_by_tok, spaces, spstring, sptoken},
    Expr, ExprKind,
};
use combine::{
    attempt, between, choice, look_ahead, many1, optional,
    parser::char::digit,
    position,
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use compact_str::CompactString;
use netidx::{publisher::Value, utils::Either};
use poolshark::local::LPooled;
use triomphe::Arc;

pub(super) fn array<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(token('['), sptoken(']'), sep_by_tok(expr(), csep(), sptoken(']'))),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            ExprKind::Array { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

pub(super) fn idx<I>() -> impl Parser<I, Output = Value>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (optional(token('-')), many1(digit())).then(
        |(minus, idx): (Option<char>, CompactString)| {
            let idx = match idx.parse::<i64>() {
                Ok(i) => i,
                Err(_) => return unexpected_any("expected int").left(),
            };
            if minus.is_some() {
                value(Value::I64(-idx)).right()
            } else {
                value(Value::I64(idx)).right()
            }
        },
    )
}

pub(super) fn arrayref<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        ref_pexp(),
        between(
            token('['),
            sptoken(']'),
            (position(), spaces()).then(|(pos, _)| {
                choice((
                    attempt(idx().skip(look_ahead(sptoken(']')))).map(move |idx| {
                        Either::Right(ExprKind::Constant(idx).to_expr(pos))
                    }),
                    attempt(
                        (
                            optional(idx()).skip(spstring("..")),
                            spaces().with(optional(idx())),
                        )
                            .skip(look_ahead(sptoken(']'))),
                    )
                    .map(move |(start, end)| {
                        let start = start.map(|e| ExprKind::Constant(e).to_expr(pos));
                        let end = end.map(|e| ExprKind::Constant(e).to_expr(pos));
                        Either::Left((start, end))
                    }),
                    attempt((
                        optional(attempt(expr())).skip(spstring("..")),
                        optional(attempt(expr())),
                    ))
                    .map(|(start, end)| Either::Left((start, end))),
                    attempt(expr()).map(|e| Either::Right(e)),
                ))
            }),
        ),
    )
        .map(|(pos, a, args)| match args {
            Either::Left((start, end)) => ExprKind::ArraySlice {
                source: Arc::new(a),
                start: start.map(Arc::new),
                end: end.map(Arc::new),
            }
            .to_expr(pos),
            Either::Right(i) => {
                ExprKind::ArrayRef { source: Arc::new(a), i: Arc::new(i) }.to_expr(pos)
            }
        })
}
