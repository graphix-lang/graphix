use crate::expr::{
    Expr, ExprKind,
    parser::{comma_items, expr, grow::because, spaces, spstring, sptoken},
};
use combine::{
    ParseError, Parser, RangeStream, attempt, between,
    error::StreamError,
    many1, optional,
    parser::char::{digit, string},
    parser::combinator::recognize,
    position,
    stream::{Range, StreamErrorFor, position::SourcePosition},
    token, value,
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
            comma_items(expr, || attempt(sptoken(']')), true).map(|(args, _)| args),
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
            comma_items(expr, || attempt(spstring(">]")), true).map(|(args, _)| args),
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
    // the sign is part of the literal: i64::MIN's magnitude is no i64
    recognize::<CompactString, _, _>((
        optional(token('-')),
        many1::<CompactString, _, _>(digit()),
    ))
    .then(|idx: CompactString| match idx.parse::<i64>() {
        Ok(i) => value(Value::I64(i)).right(),
        Err(_) => because("expected int").left(),
    })
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
        (
            spaces().with(optional(expr())),
            optional(attempt(spstring("..")).with(spaces()).with(optional(expr()))),
        ),
    )
    .and_then(|(start, range)| match (start, range) {
        (Some(i), None) => Ok(Index::At(i)),
        (start, Some(end)) => Ok(Index::Slice(start, end)),
        (None, None) => {
            Err(StreamErrorFor::<I>::message_static_message("expected an index"))
        }
    })
}
