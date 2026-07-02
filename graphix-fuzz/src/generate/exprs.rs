//! The expression core: `gen_typed` emits an expression of exactly the
//! requested [`GenType`], recursing through operators, collections, and
//! in-scope references.

use super::{
    GenCtx,
    types::{self, GenType},
};
use crate::mutate::Rng;

pub(super) fn pick<'a>(rng: &mut Rng, xs: &[&'a str]) -> &'a str {
    xs[rng.below(xs.len())]
}

/// A call to a visible lambda producing `ty`, if one is in scope: a
/// typed lambda whose return type is `ty` (args generated recursively
/// at its param types), or — for numeric `ty` — a poly lambda with all
/// args at `ty` (its numeric body makes the result type follow the
/// arguments, so each distinct arg type is a distinct monomorphization).
fn try_call(ctx: &GenCtx, rng: &mut Rng, ty: &GenType, depth: usize) -> Option<String> {
    let typed = ctx.fns_returning(ty);
    let polys = if ty.is_numeric() { ctx.poly_fns() } else { Vec::new() };
    if typed.is_empty() && polys.is_empty() {
        return None;
    }
    let n = rng.below(typed.len() + polys.len());
    let (name, param_tys) = if n < typed.len() {
        let (name, params) = &typed[n];
        (name.to_string(), params.clone())
    } else {
        let (name, arity) = polys[n - typed.len()];
        (name.to_string(), vec![ty.clone(); arity])
    };
    let args: Vec<_> = param_tys.iter().map(|p| gen_typed(ctx, rng, p, depth)).collect();
    Some(format!("{name}({})", args.join(", ")))
}

/// Emit an expression of exactly `ty`, recursing up to `depth`.
pub(super) fn gen_typed(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> String {
    // Base case: a literal, or an in-scope ref of this type. Bias toward
    // refs when available (they create dataflow).
    let recurse = depth > 0 && rng.below(3) != 0;
    if !recurse {
        let vars = ctx.vars_of(ty);
        if !vars.is_empty() && rng.below(2) == 0 {
            return vars[rng.below(vars.len())].to_string();
        }
        return types::literal(rng, ty);
    }
    let d = depth - 1;
    if ty.is_scalar() && rng.below(5) == 0 {
        if let Some(call) = try_call(ctx, rng, ty, d) {
            return call;
        }
    }
    match ty {
        GenType::I64 | GenType::F64 | GenType::U8 => {
            // Unary minus (a real `Neg` node, exercising `ineg`/`fneg`). Only
            // for signed/float — `-u8` is a compile error (Part B's
            // signed/float/decimal constraint). Parenthesize the operand so
            // `-(i64:5)` stays a `Neg` rather than re-parsing as a negative
            // literal.
            if !matches!(ty, GenType::U8) && rng.below(6) == 0 {
                return format!("(-({}))", gen_typed(ctx, rng, ty, d));
            }
            // Bias toward +/-/* over //% — a generated `/0` or `%0` drops to
            // bottom (Timeout in all modes), which is slow to check. Div/mod
            // are still ~25% of ops, so the div0/overflow paths get exercised.
            let op = pick(rng, &["+", "+", "-", "-", "*", "*", "/", "%"]);
            format!(
                "({} {} {})",
                gen_typed(ctx, rng, ty, d),
                op,
                gen_typed(ctx, rng, ty, d)
            )
        }
        GenType::Bool => match rng.below(3) {
            0 => {
                let nt = types::numeric_type(rng);
                let op = pick(rng, &["<", ">", "<=", ">=", "==", "!="]);
                format!(
                    "({} {} {})",
                    gen_typed(ctx, rng, &nt, d),
                    op,
                    gen_typed(ctx, rng, &nt, d)
                )
            }
            1 => {
                let op = pick(rng, &["&&", "||"]);
                format!(
                    "({} {} {})",
                    gen_typed(ctx, rng, &GenType::Bool, d),
                    op,
                    gen_typed(ctx, rng, &GenType::Bool, d)
                )
            }
            _ => format!("(!{})", gen_typed(ctx, rng, &GenType::Bool, d)),
        },
        GenType::Str => types::literal(rng, ty),
        GenType::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| gen_typed(ctx, rng, e, d)).collect();
            format!("({})", parts.join(", "))
        }
        GenType::Array(elem) => {
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| gen_typed(ctx, rng, elem, d)).collect();
            format!("[{}]", parts.join(", "))
        }
        GenType::Fn { .. } | GenType::PolyFn { .. } | GenType::Opaque => {
            unreachable!("gen_typed is never asked for a fn/opaque type")
        }
    }
}

/// Maybe wrap an expression of `ty` in a `select` over a numeric
/// scrutinee with a literal arm + catch-all (both arms produce `ty`).
pub(super) fn maybe_select(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    if depth == 0 || rng.below(4) != 0 {
        return None;
    }
    let scrut = gen_typed(ctx, rng, &GenType::I64, depth - 1);
    let lit = [0i64, 1, 2, 42][rng.below(4)];
    let arm = gen_typed(ctx, rng, ty, depth - 1);
    let dflt = gen_typed(ctx, rng, ty, depth - 1);
    Some(format!("select {scrut} {{ {lit} => {arm}, _ => {dflt} }}"))
}
