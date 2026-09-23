use crate::expr::{
    Expr, ExprKind,
    parser::{csep, expr, sep_by_tok, spaces, spstring, sptoken},
};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, look_ahead, many1,
    optional,
    parser::char::{digit, string},
    position,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use compact_str::CompactString;
use netidx_core::utils::Either;
use netidx_value::Value;
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
        between(
            token('['),
            sptoken(']'),
            sep_by_tok(expr(), csep(), attempt(sptoken(']'))),
        ),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            ExprKind::Array { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
        })
}

/// A native list literal `[<e1, e2, ...>]`. The two-char open is
/// attempt-wrapped so a plain array literal's `[` backtracks cleanly.
pub(super) fn list_lit<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        between(
            attempt(string("[<")),
            spstring(">]"),
            sep_by_tok(expr(), csep(), attempt(spstring(">]"))),
        ),
    )
        .map(|(pos, mut args): (_, LPooled<Vec<Expr>>)| {
            ExprKind::List { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
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

/// A constant with its span.
fn at<I, P>(value: P) -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I, Output = Value>,
{
    (position(), value, position())
        .map(|(pos, v, end)| ExprKind::Constant(v).to_expr(pos).ending(end))
}

// CR claude for eric: [perf] For a non-literal index the third arm parses the
// whole expression, fails at the missing `..`, and the fourth parses it again:
// `a[a[…a[0]…]]` 16 deep takes 4.4s to `graphix fmt` (probe, doubling per
// level). Parse `optional(expr())` once and branch on `..`.
// CR claude for eric: [readability] `Either` with Left = slice and Right =
// index (here and in arithexp's Post::Array) needs this doc to be read; a
// two-variant enum names the cases.
/// The `[ idx ]` / `[ start..end ]` postfix suffix. `Right(e)` is a
/// single-index `ArrayRef`; `Left((start, end))` is an `ArraySlice`.
pub(super) fn array_index_suffix<I>()
-> impl Parser<I, Output = Either<(Option<Expr>, Option<Expr>), Expr>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('['),
        sptoken(']'),
        spaces().with(choice((
            attempt(at(idx()).skip(look_ahead(sptoken(']')))).map(Either::Right),
            attempt(
                (
                    optional(at(idx())).skip(spstring("..")),
                    spaces().with(optional(at(idx()))),
                )
                    .skip(look_ahead(sptoken(']'))),
            )
            .map(Either::Left),
            attempt((
                optional(attempt(expr())).skip(spstring("..")),
                optional(attempt(expr())),
            ))
            .map(|(start, end)| Either::Left((start, end))),
            attempt(expr()).map(|e| Either::Right(e)),
        ))),
    )
}
