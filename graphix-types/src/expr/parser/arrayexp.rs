use super::{
    comma_items, expr,
    input::{Input, R},
};
use crate::expr::{Expr, ExprKind};
use netidx_value::Value;
use poolshark::local::LPooled;
use triomphe::Arc;

/// The items of a list literal before what `close` reads, none or more,
/// and what closes it.
fn items<T>(i: &mut Input, close: impl Fn(&mut Input) -> R<T>) -> R<LPooled<Vec<Expr>>> {
    let term = |i: &mut Input| i.attempt(&close).map(drop);
    let (args, _) = comma_items(i, &expr, &term, true)?;
    close(i)?;
    Ok(args)
}

pub(super) fn array(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.token('[')?;
    let mut args = items(i, |i| i.sptoken(']'))?;
    Ok(ExprKind::Array { args: Arc::from_iter(args.drain(..)) }.to_expr(pos))
}

/// A native list literal `[<e1, e2, ...>]`. The two-char open is
/// attempted so a plain array literal's `[` backtracks cleanly.
pub(super) fn list_lit(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.attempt(|i| i.string("[<"))?;
    let mut args = items(i, |i| i.spstring(">]"))?;
    Ok(ExprKind::List { args: Arc::from_iter(args.drain(..)) }.to_expr(pos))
}

pub(super) fn idx(i: &mut Input) -> R<Value> {
    // the sign is part of the literal: i64::MIN's magnitude is no i64
    let m = i.mark();
    i.optional(|i| i.token('-'))?;
    i.take_while1(|c| c.is_ascii_digit())?;
    match i.since(&m).parse::<i64>() {
        Ok(n) => Ok(Value::I64(n)),
        Err(_) => Err(i.because("expected int")),
    }
}

/// The `[ idx ]` / `[ start..end ]` postfix suffix.
pub(super) fn array_index_suffix(i: &mut Input) -> R<Index> {
    let start = i.mark();
    i.token('[')?;
    i.spaces();
    let first = i.optional(expr)?;
    let range = i.optional(|i| {
        i.attempt(|i| i.spstring(".."))?;
        i.spaces();
        i.optional(expr)
    })?;
    i.sptoken(']')?;
    match (first, range) {
        (Some(e), None) => Ok(Index::At(e)),
        (first, Some(end)) => Ok(Index::Slice(first, end)),
        (None, None) => Err(i.because_at(start, "expected an index")),
    }
}

/// What `[..]` after an expression selects.
pub(super) enum Index {
    /// `[i]`
    At(Expr),
    /// `[start..end]`, either bound optional
    Slice(Option<Expr>, Option<Expr>),
}
