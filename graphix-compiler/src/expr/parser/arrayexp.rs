use crate::expr::{
    Expr, ExprKind,
    parser::{csep, expr, sep_by_tok, spaces, spstring, sptoken},
};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, many1, optional,
    error::StreamError,
    parser::char::{digit, string},
    position,
    stream::{Range, StreamErrorFor, position::SourcePosition},
    token, unexpected_any, value,
};
use compact_str::CompactString;
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

/// What `[..]` after an expression selects.
pub(super) enum Index {
    /// `[i]`
    At(Expr),
    /// `[start..end]`, either bound optional
    Slice(Option<Expr>, Option<Expr>),
}

/// The `[ idx ]` / `[ start..end ]` postfix suffix.
pub(super) fn array_index_suffix<I>() -> impl Parser<I, Output = Index>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('['),
        sptoken(']'),
        (spaces().with(optional(expr())), optional(spstring("..").with(optional(expr())))),
    )
    .and_then(|(start, range)| match (start, range) {
        (Some(i), None) => Ok(Index::At(i)),
        (start, Some(end)) => Ok(Index::Slice(start, end)),
        (None, None) => Err(StreamErrorFor::<I>::message_static_message("expected an index")),
    })
}
