use super::{
    super::ONE_TUPLE,
    super::arithexp::{Parenthesized, Post, apply_post, shunting_yard},
    any, arrayexp, brace, cast, choice, comma_items, expr, fldname,
    input::{Input, R},
    interpolateexp::interpolated,
    lambdaexp::{any_lambda, apply_args, lambda_ahead},
    literal, live_expr, never_expr, not_prefix, raw_string, reference, select, seq,
    variant,
};
use crate::{
    expr::{BinOp, Expr, ExprKind, Name},
    typ::Mutability,
};
use combine::stream::position::SourcePosition;
use netidx_value::parser::int;
use poolshark::local::LPooled;
use triomphe::Arc;

/// A prefix operator: `&e`, `*e`, `-e`, `!e`. The operand is an
/// `arith_term`, so a prefix binds looser than the postfix operators.
fn prefix(i: &mut Input, op: char, key: bool, mk: fn(Arc<Expr>) -> ExprKind) -> R<Expr> {
    let pos = i.pos();
    i.token(op)?;
    let e = arith_term(i, key)?;
    Ok(mk(Arc::new(e)).to_expr(pos))
}

/// `&e` or `&mut e`.
fn byref(i: &mut Input, key: bool) -> R<Expr> {
    let pos = i.pos();
    i.token('&')?;
    let m = mutability(i)?;
    let e = arith_term(i, key)?;
    Ok(ExprKind::ByRef(m, Arc::new(e)).to_expr(pos))
}

/// The `mut` after a `&`, and the space after it.
pub(super) fn mutability(i: &mut Input) -> R<Mutability> {
    let m = i.optional(|i| {
        i.attempt(|i| {
            i.string("mut")?;
            not_prefix(i)?;
            Ok(i.spaces())
        })
    })?;
    Ok(match m {
        Some(_) => Mutability::Mut,
        None => Mutability::Shared,
    })
}

// Each alternative is attempted so a partial parse (the `{` of a map
// access that is really a block) ends the postfix loop cleanly. `key`
// admits `{k}`, written against its source like a call's `(`; the head of a
// `seq` refuses it so that its body is not read as a map access of the
// trigger.
fn postfix_op(i: &mut Input, key: bool) -> R<Post> {
    choice!(
        i,
        |i| i.attempt(|i| {
            i.sptoken('.')?;
            choice!(
                i,
                |i| i.attempt(|i| super::bridge!(i, int::<_, usize>())).map(Post::Index),
                |i| {
                    i.spaces();
                    let pos = i.pos();
                    fldname(i).map(|n| Post::Field(Name::written(n, pos)))
                }
            )
        }),
        // a `[` or `(` right after a term starts an index or a call and
        // nothing else: a failure past it is that suffix's to report
        |i| {
            i.attempt(|i| i.look_ahead(|i| i.token('[')))?;
            arrayexp::array_index_suffix(i).map(Post::Array)
        },
        |i| i.attempt(|i| match key {
            true => {
                i.token('{')?;
                let k = expr(i)?;
                i.sptoken('}')?;
                Ok(Post::Key(k))
            }
            false => Err(i.unexpected("a seq trigger takes no map access")),
        }),
        |i| {
            i.attempt(|i| i.look_ahead(|i| i.token('(')))?;
            apply_args(i).map(Post::Call)
        },
        // `?`/`$` chain innermost first: `x?$` takes the errors off `x`,
        // then the null
        |i| i.attempt(|i| {
            i.spaces();
            choice!(i, |i| i.token('?').map(|_| Post::Qop), |i| i
                .token('$')
                .map(|_| Post::OrNever))
        })
    )
}

fn paren_group(i: &mut Input) -> R<(Expr, Option<Parenthesized>)> {
    let pos = i.pos();
    i.token('(')?;
    let (mut exprs, trailing) =
        comma_items(i, &expr, &|i| i.token(')').map(drop), false)?;
    i.sptoken(')')?;
    let end = i.pos();
    Ok(match exprs.len() {
        1 if trailing => return Err(i.refuse(end, ONE_TUPLE)),
        1 => (exprs.pop().unwrap(), Some(Parenthesized)),
        _ => {
            (ExprKind::Tuple { args: Arc::from_iter(exprs.drain(..)) }.to_expr(pos), None)
        }
    })
}

/// Where a literal may start, consuming nothing: never at a letter that
/// begins no keyword value (`true`) or typed literal's prefix (`i64:`),
/// so most references skip the value parser.
fn literal_start(i: &mut Input) -> R<()> {
    const LITERAL_LETTERS: &str = "abdefinutvz";
    let printed = i.printed_data;
    i.look_ahead(|i| {
        i.satisfy(|c| {
            !c.is_alphabetic() || LITERAL_LETTERS.contains(c) || (c == 'N' && printed)
        })
    })
    .map(drop)
}

fn primary(i: &mut Input, key: bool) -> R<(Expr, Option<Parenthesized>)> {
    let bare = |e: Expr| (e, None);
    choice!(
        i,
        |i| prefix(i, '!', key, |expr| ExprKind::Not { expr }).map(bare),
        |i| raw_string(i).map(bare),
        |i| arrayexp::list_lit(i).map(bare),
        |i| arrayexp::array(i).map(bare),
        |i| byref(i, key).map(bare),
        |i| prefix(i, '*', key, ExprKind::Deref).map(bare),
        |i| select(i).map(bare),
        |i| seq(i).map(bare),
        |i| variant(i).map(bare),
        |i| cast(i).map(bare),
        |i| never_expr(i).map(bare),
        |i| live_expr(i).map(bare),
        |i| any(i).map(bare),
        |i| interpolated(i).map(bare),
        |i| brace(i).map(bare),
        paren_group,
        |i| i
            .attempt(|i| {
                literal_start(i)?;
                literal(i)
            })
            .map(bare),
        // after `literal()`, so that a signed numeric literal is a constant
        |i| prefix(i, '-', key, ExprKind::Neg).map(bare),
        |i| super::construct(i).map(bare),
        |i| reference(i).map(bare)
    )
}

pub(super) fn arith_term(i: &mut Input, key: bool) -> R<Expr> {
    i.knot(|i| {
        i.spaces();
        let pos = i.pos();
        let (base, paren) = primary(i, key)?;
        let end = i.pos();
        let mut ops: LPooled<Vec<(Post, SourcePosition)>> = LPooled::take();
        i.many(
            |i| {
                let op = postfix_op(i, key)?;
                Ok((op, i.pos()))
            },
            |op| ops.push(op),
        )?;
        let chain_end = i.pos();
        let base = base.ending(end);
        // `?`/`$` print their operand bare, so a parenthesized one keeps
        // its parens
        let keeps_parens = matches!(
            (&paren, ops.first()),
            (Some(Parenthesized), None | Some((Post::Qop | Post::OrNever, _)))
        );
        i.fold_fits(ops.len(), chain_end)?;
        let base = match keeps_parens {
            true => ExprKind::ExplicitParens(Arc::new(base)).to_expr(pos).ending(end),
            false => base,
        };
        Ok(ops
            .drain(..)
            .fold(base, |acc, (op, end)| apply_post(pos, acc, op).ending(end)))
    })
}

/// A binary operator token.
fn binop(i: &mut Input) -> R<BinOp> {
    let mut best = None;
    for op in BinOp::ALL {
        let r = i.alt(&mut best, |i| {
            i.attempt(|i| {
                i.string(op.token())?;
                i.not_followed_by(|i| i.satisfy(|c| op.not_before() == Some(c)))
            })
        })?;
        if r.is_some() {
            return Ok(op);
        }
    }
    Err(best.unwrap())
}

/// An operator's right operand: a term, or a lambda, which is the last
/// operand of its chain since its body takes the rest.
fn operand(i: &mut Input, key: bool) -> R<Expr> {
    choice!(
        i,
        |i| {
            lambda_ahead(i)?;
            any_lambda(i)
        },
        |i| arith_term(i, key)
    )
}

pub(super) fn arith(i: &mut Input, key: bool) -> R<Expr> {
    i.knot(|i| {
        let e = arith_term(i, key)?;
        let mut rest: LPooled<Vec<(BinOp, Expr)>> = LPooled::take();
        i.many(
            |i| {
                let op = i.attempt(|i| {
                    i.spaces();
                    binop(i)
                })?;
                Ok((op, operand(i, key)?))
            },
            |o| rest.push(o),
        )?;
        let end = i.pos();
        i.fold_fits(rest.len(), end)?;
        Ok(match rest.is_empty() {
            true => e,
            false => shunting_yard(e, rest),
        })
    })
}
