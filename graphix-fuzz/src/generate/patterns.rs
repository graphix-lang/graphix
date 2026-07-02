//! Select generation with patterns derived FROM the scrutinee's
//! [`GenType`] — never free-form, so exhaustiveness and dead-arm
//! validity hold by construction:
//!
//! ```text
//! select scrut {
//!   [0-2 GUARDED arms]            // first ⇒ can never be dead
//!   [0-1 unguarded refutable arm] // nothing unguarded before it
//!   [final irrefutable arm]       // one refutable arm never exhausts
//! }
//! ```
//!
//! or, for a variant scrutinee, full-coverage mode: every tag exactly
//! once, unguarded, with IRREFUTABLE payload patterns (a literal
//! payload would leave a coverage gap) and NO trailing wildcard (which
//! would be a dead arm).

use super::{
    GenCtx, exprs,
    types::{self, GenType},
};
use crate::mutate::Rng;

/// A generated pattern over one type: its text, whether it can fail to
/// match, and (via `inner`) the names it binds.
struct Pat {
    text: String,
    refutable: bool,
}

/// A collision-biased bind name for a pattern position — arm-binding
/// shadowing of OUTER names (the #167 class) is deliberate, but two
/// binds within one arm's pattern must be distinct (`mark` is the arm's
/// scope start; everything pushed above it is this pattern's binds).
fn bind_name(inner: &mut GenCtx, rng: &mut Rng, mark: usize) -> String {
    let mut n = if rng.below(10) < 3 && !inner.collision_pool.is_empty() {
        inner.collision_pool[rng.below(inner.collision_pool.len())].clone()
    } else {
        inner.fresh()
    };
    while inner.vars[mark..].iter().any(|(m, _)| *m == n) {
        n = inner.fresh();
    }
    n
}

fn gen_pattern(
    inner: &mut GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
    force_irrefutable: bool,
    mark: usize,
) -> Pat {
    // Leaf choices shared by every type: `_`, or a bind.
    let leaf = |inner: &mut GenCtx, rng: &mut Rng| {
        if rng.below(4) == 0 {
            Pat { text: "_".into(), refutable: false }
        } else {
            let n = bind_name(inner, rng, mark);
            inner.push(n.clone(), ty.clone());
            Pat { text: n, refutable: false }
        }
    };
    match ty {
        // Scalar literal leaves are the refutable base case (f64
        // excluded — float-equality patterns are legal but a value
        // match is vanishingly unlikely, all noise).
        GenType::I64 | GenType::U8 | GenType::Bool | GenType::Str => {
            if !force_irrefutable && rng.below(2) == 0 {
                Pat { text: types::literal(rng, ty), refutable: true }
            } else {
                leaf(inner, rng)
            }
        }
        GenType::F64 | GenType::Map(_) | GenType::Nullable(_) => leaf(inner, rng),
        GenType::Tuple(elems) if depth > 0 => {
            let parts: Vec<Pat> = elems
                .iter()
                .map(|e| gen_pattern(inner, rng, e, depth - 1, force_irrefutable, mark))
                .collect();
            let refutable = parts.iter().any(|p| p.refutable);
            let text: Vec<_> = parts.into_iter().map(|p| p.text).collect();
            Pat { text: format!("({})", text.join(", ")), refutable }
        }
        GenType::Struct(fields) if depth > 0 => {
            let parts: Vec<(String, Pat)> = fields
                .iter()
                .map(|(f, t)| {
                    (
                        f.clone(),
                        gen_pattern(inner, rng, t, depth - 1, force_irrefutable, mark),
                    )
                })
                .collect();
            let refutable = parts.iter().any(|(_, p)| p.refutable);
            let text: Vec<_> =
                parts.into_iter().map(|(f, p)| format!("{f}: {}", p.text)).collect();
            Pat { text: format!("{{{}}}", text.join(", ")), refutable }
        }
        GenType::Variant(tags) if !force_irrefutable => {
            let (tag, args) = &tags[rng.below(tags.len())];
            let text = if args.is_empty() {
                format!("`{tag}")
            } else {
                let parts: Vec<_> = args
                    .iter()
                    .map(|t| {
                        gen_pattern(inner, rng, t, depth.saturating_sub(1), false, mark)
                            .text
                    })
                    .collect();
                format!("`{tag}({})", parts.join(", "))
            };
            // Refutable even as the only tag pattern — our unions have
            // ≥2 tags.
            Pat { text, refutable: true }
        }
        GenType::Array(e) if !force_irrefutable && depth > 0 => {
            // Exactly one slice arm is ever emitted per select (the
            // caller's structure), so subsumption between slice arms
            // can't arise; each of these shapes leaves lengths
            // uncovered, so a final bind-all is never dead after one.
            match rng.below(3) {
                0 => Pat { text: "[]".into(), refutable: true },
                1 => {
                    let p = gen_pattern(inner, rng, e, depth - 1, true, mark);
                    Pat { text: format!("[{}]", p.text), refutable: true }
                }
                _ => {
                    let p = gen_pattern(inner, rng, e, depth - 1, true, mark);
                    let tl = bind_name(inner, rng, mark);
                    inner.push(tl.clone(), ty.clone());
                    Pat { text: format!("[{}, {tl}..]", p.text), refutable: true }
                }
            }
        }
        _ => leaf(inner, rng),
    }
}

/// One arm: pattern (+ optional guard) over `scrut_ty`, body of `ty`,
/// generated in a scope clone so arm binds don't leak.
fn gen_arm(
    ctx: &GenCtx,
    rng: &mut Rng,
    scrut_ty: &GenType,
    ty: &GenType,
    depth: usize,
    force_irrefutable: bool,
    guarded: bool,
) -> (String, bool) {
    let mut inner = ctx.clone();
    let mark = inner.mark();
    let pat = gen_pattern(&mut inner, rng, scrut_ty, 2, force_irrefutable, mark);
    let guard = if guarded {
        format!(" if {}", exprs::gen_typed(&inner, rng, &GenType::Bool, 1))
    } else {
        String::new()
    };
    let body = exprs::gen_typed(&inner, rng, ty, depth);
    (format!("{}{guard} => {body}", pat.text), pat.refutable)
}

/// Maybe wrap an expression of `ty` in a `select`. The scrutinee is a
/// visible composite/variant/scalar binding when one exists (that's
/// where destructuring bites), else a fresh numeric expression.
pub(super) fn maybe_select(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    if depth == 0 || rng.below(4) != 0 {
        return None;
    }
    let d = depth - 1;
    // Scrutinee: prefer a var whose type has pattern structure.
    let structured: Vec<(&str, &GenType)> = ctx
        .visible_entries()
        .into_iter()
        .filter(|(_, t)| {
            matches!(
                t,
                GenType::Tuple(_)
                    | GenType::Struct(_)
                    | GenType::Variant(_)
                    | GenType::Array(_)
                    | GenType::Nullable(_)
                    | GenType::I64
                    | GenType::U8
                    | GenType::Bool
                    | GenType::Str
            )
        })
        .collect();
    let (scrut, scrut_ty) = if !structured.is_empty() && rng.below(10) < 7 {
        let (n, t) = structured[rng.below(structured.len())];
        (n.to_string(), t.clone())
    } else {
        (exprs::gen_typed(ctx, rng, &GenType::I64, d), GenType::I64)
    };
    let mut arms: Vec<String> = Vec::new();
    // Nullable coverage mode: null arm + value type-match arm cover
    // the whole union — no wildcard (it would be a dead arm).
    if let GenType::Nullable(t) = &scrut_ty {
        if t.is_scalar() && rng.below(2) == 0 {
            let null_body = exprs::gen_typed(ctx, rng, ty, d);
            let mut inner = ctx.clone();
            let mark = inner.mark();
            let n = bind_name(&mut inner, rng, mark);
            inner.push(n.clone(), (**t).clone());
            let val_body = exprs::gen_typed(&inner, rng, ty, d);
            return Some(format!(
                "select {scrut} {{ null as _ => {null_body}, {} as {n} => {val_body} }}",
                t.render()
            ));
        }
    }
    // Variant full-coverage mode: every tag once, no wildcard.
    if let GenType::Variant(tags) = &scrut_ty {
        if rng.below(2) == 0 {
            for _ in 0..rng.below(2) {
                let (a, _) = gen_arm(ctx, rng, &scrut_ty, ty, d, false, true);
                arms.push(a);
            }
            for (tag, args) in tags {
                let mut inner = ctx.clone();
                let mark = inner.mark();
                let text = if args.is_empty() {
                    format!("`{tag}")
                } else {
                    let parts: Vec<_> = args
                        .iter()
                        .map(|t| gen_pattern(&mut inner, rng, t, 1, true, mark).text)
                        .collect();
                    format!("`{tag}({})", parts.join(", "))
                };
                let body = exprs::gen_typed(&inner, rng, ty, d);
                arms.push(format!("{text} => {body}"));
            }
            return Some(format!("select {scrut} {{ {} }}", arms.join(", ")));
        }
    }
    // General mode: guarded arms first, then ≤1 unguarded refutable,
    // then the irrefutable final.
    for _ in 0..rng.below(3) {
        let (a, _) = gen_arm(ctx, rng, &scrut_ty, ty, d, false, true);
        arms.push(a);
    }
    if rng.below(2) == 0 {
        let (a, refutable) = gen_arm(ctx, rng, &scrut_ty, ty, d, false, false);
        if refutable {
            arms.push(a);
        }
    }
    let (fin, _) = gen_arm(ctx, rng, &scrut_ty, ty, d, true, false);
    arms.push(fin);
    Some(format!("select {scrut} {{ {} }}", arms.join(", ")))
}
