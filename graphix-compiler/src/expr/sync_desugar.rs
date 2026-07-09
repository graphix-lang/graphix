//! The sync-subset desugar (design/sync_subset.md): a `sync { … }`
//! block is FUNCTIONALIZED into the existing expression vocabulary —
//! no new node types, no new evaluation regime.
//!
//! - `let mut x = e`   → `let x = e` (an ordinary binding; `mut` only
//!   licenses later assignment)
//! - `x = e`           → `let x = e` (sequential rebinding IS
//!   shadowing)
//! - `for p in it { body }` with assigned set `{x1..xk}` →
//!   `let (x1,..,xk) = array::fold(it, (x1,..,xk), |(x1,..,xk), p| {
//!        body'; (x1,..,xk) })`
//!   — a `for` loop is a fold whose accumulator is the tuple of the
//!   mut locals its body assigns. An effect-only body (no assigns)
//!   folds a `null` accumulator through for its effects.
//! - a `select` in statement position whose arms assign → the select
//!   yields the updated values (each arm's assigns become shadow-lets
//!   with the arm evaluating to the assigned tuple; arms that don't
//!   assign a variable yield its current value), bound by
//!   `let (x1,..,xk) = select … { … }`.
//!
//! The elaboration ladder falls out with NO effect analysis here: a
//! sync callback makes the fold a pure fold (the JIT's scaffold loop);
//! an async callback makes it an impure fold (MapQ/FoldQ's per-slot
//! machinery — slots, re-evaluation, taint). The desugar is the single
//! specification; the two elaborations are the existing evaluators.
//!
//! v1 restrictions (each a clear compile error, not a silent wrong
//! answer): assignment targets a `let mut` of the CURRENT sync block
//! (no cross-block assignment through a nested `sync`); `for` iterates
//! Arrays; a `for` pattern may not shadow an in-scope mut; assignment
//! is a STATEMENT (block statement or select-arm body), not a value.

use super::{BindExpr, Expr, ExprKind, ModPath, SelectExpr, pattern::StructurePattern};
use crate::bailat;
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::CompactString;
use netidx::utils::Either;
use poolshark::local::LPooled;
use triomphe::Arc;

/// Desugar one `sync { exprs }` body into an ordinary expression.
/// Nested `sync` blocks are left in place — the compiler's `SyncBlock`
/// arm desugars them when it reaches them (each block is its own
/// mut-scope).
pub fn desugar_sync_block(spec: &Expr, exprs: &[Expr]) -> Result<Expr> {
    let mut muts: LPooled<Vec<ArcStr>> = LPooled::take();
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    let last = exprs.len() - 1;
    for (i, e) in exprs.iter().enumerate() {
        if i == last {
            // The tail is a VALUE, not a statement: assigns in tail
            // position would be dead (nothing reads the rebind), so
            // they are rejected by the backstop (an `Assign` that
            // survives desugar fails to compile).
            out.push(e.clone());
        } else {
            desugar_stmt(e, &mut muts, &mut out)?;
        }
    }
    if out.len() == 1 {
        Ok(out.drain(..).next().unwrap())
    } else {
        Ok(ExprKind::Do { exprs: Arc::from_iter(out.drain(..)) }.to_expr(spec.pos))
    }
}

/// Desugar one statement, appending the rewritten statement(s) to
/// `out` and updating the mut-scope.
fn desugar_stmt(
    e: &Expr,
    muts: &mut LPooled<Vec<ArcStr>>,
    out: &mut LPooled<Vec<Expr>>,
) -> Result<()> {
    match &e.kind {
        ExprKind::Bind(b) if b.mut_ => {
            let Some(name) = b.pattern.single_bind() else {
                bailat!(e, "`let mut` requires a single-name pattern")
            };
            let name = name.clone();
            out.push(
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: b.rec,
                    mut_: false,
                    pattern: b.pattern.clone(),
                    typ: b.typ.clone(),
                    value: b.value.clone(),
                }))
                .to_expr(e.pos),
            );
            if !muts.contains(&name) {
                muts.push(name);
            }
            Ok(())
        }
        ExprKind::Assign { name, value } => {
            let x = assign_target(e, name, muts)?;
            out.push(bind_single(e, &x, value.clone()));
            Ok(())
        }
        ExprKind::For { .. } | ExprKind::Select(_) => {
            let (rewritten, assigned) = yielding_form(e, muts)?;
            bind_assigned(e, &assigned, rewritten, out);
            Ok(())
        }
        _ => {
            // Any other statement passes through untouched; an Assign
            // buried in an unsupported position survives to the
            // compiler's backstop error.
            out.push(e.clone());
            Ok(())
        }
    }
}

/// Rewrite a statement-position expression into one that EVALUATES to
/// the updated values of the mut locals it assigns, returning the
/// rewritten expression and the (sorted, deduped) assigned set. An
/// empty set means the expression is effect-only and unchanged apart
/// from interior rewrites.
fn yielding_form(
    e: &Expr,
    muts: &mut LPooled<Vec<ArcStr>>,
) -> Result<(Expr, Vec<ArcStr>)> {
    match &e.kind {
        ExprKind::Assign { name, value } => {
            let x = assign_target(e, name, muts)?;
            Ok(((**value).clone(), vec![x]))
        }
        ExprKind::For { pattern, iter, body } => {
            for n in pattern_binds(pattern) {
                if muts.contains(&n) {
                    bailat!(
                        e,
                        "for pattern binds `{n}`, shadowing a `let mut` of the \
                         enclosing sync block — rename one of them"
                    )
                }
            }
            let (body, assigned) = body_yielding(body, muts)?;
            let fold_cb_args: Arc<[super::Arg]> = Arc::from_iter([
                super::Arg {
                    labeled: None,
                    pattern: acc_pattern(&assigned),
                    constraint: None,
                    pos: e.pos,
                },
                super::Arg {
                    labeled: None,
                    pattern: pattern.clone(),
                    constraint: None,
                    pos: e.pos,
                },
            ]);
            let cb = ExprKind::Lambda(Arc::new(super::LambdaExpr {
                args: fold_cb_args,
                vargs: None,
                rtype: None,
                constraints: Arc::from_iter(std::iter::empty()),
                throws: None,
                body: Either::Left(body),
            }))
            .to_expr(e.pos);
            let init = acc_value(e, &assigned);
            let fold = ExprKind::Apply(super::ApplyExpr {
                function: Arc::new(
                    ExprKind::Ref { name: ModPath::from_iter(["array", "fold"]) }
                        .to_expr(e.pos),
                ),
                args: Arc::from_iter([
                    (None, (**iter).clone()),
                    (None, init),
                    (None, cb),
                ]),
            })
            .to_expr(e.pos);
            Ok((fold, assigned))
        }
        ExprKind::Select(sel) => {
            // Collect the union of arm-assigned sets first, then
            // rewrite each arm to yield that full set.
            let mut assigned: Vec<ArcStr> = Vec::new();
            let mut probe: LPooled<Vec<ArcStr>> = LPooled::take();
            for n in muts.iter() {
                probe.push(n.clone());
            }
            for (_, arm) in sel.arms.iter() {
                let (_, a) = arm_yielding(arm, &mut probe, &[])?;
                for n in a {
                    if !assigned.contains(&n) {
                        assigned.push(n);
                    }
                }
            }
            assigned.sort();
            if assigned.is_empty() {
                return Ok((e.clone(), assigned));
            }
            let mut arms: LPooled<Vec<(super::Pattern, Expr)>> = LPooled::take();
            for (pat, arm) in sel.arms.iter() {
                let (arm, _) = arm_yielding(arm, muts, &assigned)?;
                arms.push((pat.clone(), arm));
            }
            let sel = ExprKind::Select(SelectExpr {
                arg: sel.arg.clone(),
                arms: Arc::from_iter(arms.drain(..)),
            })
            .to_expr(e.pos);
            Ok((sel, assigned))
        }
        _ => Ok((e.clone(), Vec::new())),
    }
}

/// Rewrite a select ARM body so it evaluates to the tuple of `yield_set`
/// (single value for one name): interior assigns become shadow-lets;
/// nested for/select statements recurse through `yielding_form`. When
/// `yield_set` is empty this is a PROBE — only the assigned set is
/// computed.
fn arm_yielding(
    arm: &Expr,
    muts: &mut LPooled<Vec<ArcStr>>,
    yield_set: &[ArcStr],
) -> Result<(Expr, Vec<ArcStr>)> {
    let mut assigned: Vec<ArcStr> = Vec::new();
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    let stmts: &[Expr] = match &arm.kind {
        ExprKind::Do { exprs } => exprs,
        _ => std::slice::from_ref(arm),
    };
    for s in stmts.iter() {
        match &s.kind {
            ExprKind::Assign { .. } | ExprKind::For { .. } | ExprKind::Select(_) => {
                let (rewritten, a) = yielding_form(s, muts)?;
                bind_assigned(s, &a, rewritten, &mut out);
                for n in a {
                    if !assigned.contains(&n) {
                        assigned.push(n);
                    }
                }
            }
            _ => out.push(s.clone()),
        }
    }
    if yield_set.is_empty() {
        // Probe mode: assigned set only; the rewritten arm is unused.
        return Ok((arm.clone(), assigned));
    }
    // The arm evaluates to the yield set: names the arm assigned refer
    // to the shadow-lets; the rest to the enclosing (current) values.
    // If the arm's ORIGINAL body was a lone non-assigning expression it
    // still evaluates (for its value-discard) before the yield.
    let tail = acc_ref(arm, yield_set);
    out.push(tail);
    let body = if out.len() == 1 {
        out.drain(..).next().unwrap()
    } else {
        ExprKind::Do { exprs: Arc::from_iter(out.drain(..)) }.to_expr(arm.pos)
    };
    Ok((body, assigned))
}

/// Rewrite a for BODY (a `Do` from `block1`) so it evaluates to the
/// accumulator tuple, returning the assigned set.
fn body_yielding(
    body: &Expr,
    muts: &mut LPooled<Vec<ArcStr>>,
) -> Result<(Expr, Vec<ArcStr>)> {
    let stmts: &[Expr] = match &body.kind {
        ExprKind::Do { exprs } => exprs,
        _ => std::slice::from_ref(body),
    };
    // Probe pass: the union of assigned names across all statements.
    let mut assigned: Vec<ArcStr> = Vec::new();
    {
        let mut probe: LPooled<Vec<ArcStr>> = LPooled::take();
        for n in muts.iter() {
            probe.push(n.clone());
        }
        for s in stmts.iter() {
            let (_, a) = match &s.kind {
                ExprKind::Assign { .. } | ExprKind::For { .. } | ExprKind::Select(_) => {
                    yielding_form(s, &mut probe)?
                }
                _ => (s.clone(), Vec::new()),
            };
            for n in a {
                if !assigned.contains(&n) {
                    assigned.push(n);
                }
            }
        }
    }
    assigned.sort();
    // Rewrite pass: assigns bind shadows, tail yields the accumulator.
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    for s in stmts.iter() {
        match &s.kind {
            ExprKind::Assign { .. } | ExprKind::For { .. } | ExprKind::Select(_) => {
                let (rewritten, a) = yielding_form(s, muts)?;
                bind_assigned(s, &a, rewritten, &mut out);
            }
            _ => out.push(s.clone()),
        }
    }
    out.push(acc_ref(body, &assigned));
    let body = if out.len() == 1 {
        out.drain(..).next().unwrap()
    } else {
        ExprKind::Do { exprs: Arc::from_iter(out.drain(..)) }.to_expr(body.pos)
    };
    Ok((body, assigned))
}

fn assign_target(
    e: &Expr,
    name: &ModPath,
    muts: &LPooled<Vec<ArcStr>>,
) -> Result<ArcStr> {
    let mut parts = netidx::path::Path::parts(&**name);
    let (Some(x), None) = (parts.next(), parts.next()) else {
        bailat!(e, "assignment target must be a local `let mut` name")
    };
    let x = ArcStr::from(x);
    if !muts.contains(&x) {
        bailat!(
            e,
            "`{x}` is not a `let mut` of the enclosing sync block — assignment \
             requires one"
        )
    }
    Ok(x)
}

fn pattern_binds(p: &StructurePattern) -> Vec<ArcStr> {
    let mut out = Vec::new();
    p.with_names(&mut |n: &ArcStr| {
        if !out.contains(n) {
            out.push(n.clone())
        }
    });
    out
}

/// `let x = value` for one name.
fn bind_single(at: &Expr, x: &ArcStr, value: Arc<Expr>) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr {
        rec: false,
        mut_: false,
        pattern: StructurePattern::Bind(x.clone()),
        typ: None,
        value: (*value).clone(),
    }))
    .to_expr(at.pos)
}

/// Bind the assigned set from `value`: nothing (effect-only), `let x =
/// value`, or — for multiple names — a temp bind plus one tuple-index
/// accessor per name. Accessors instead of a destructuring `let (..)`
/// because single binds and TupleRef are in the fused vocabulary while
/// pattern lets are not; a destructuring let here would leave every
/// multi-mut loop with node-walk residue.
fn bind_assigned(
    at: &Expr,
    assigned: &[ArcStr],
    value: Expr,
    out: &mut LPooled<Vec<Expr>>,
) {
    match assigned {
        [] => out.push(value),
        [x] => {
            out.push(
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: false,
                    mut_: false,
                    pattern: StructurePattern::Bind(x.clone()),
                    typ: None,
                    value,
                }))
                .to_expr(at.pos),
            );
        }
        xs => {
            let mut tmp = CompactString::const_new("__acc");
            for x in xs {
                tmp.push('_');
                tmp.push_str(x);
            }
            let tmp: ArcStr = tmp.as_str().into();
            out.push(
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: false,
                    mut_: false,
                    pattern: StructurePattern::Bind(tmp.clone()),
                    typ: None,
                    value,
                }))
                .to_expr(at.pos),
            );
            for (i, x) in xs.iter().enumerate() {
                let field = ExprKind::TupleRef {
                    source: Arc::new(
                        ExprKind::Ref { name: ModPath::from_iter([tmp.as_str()]) }
                            .to_expr(at.pos),
                    ),
                    field: i,
                }
                .to_expr(at.pos);
                out.push(bind_single(at, x, Arc::new(field)));
            }
        }
    }
}

/// The accumulator PATTERN for the assigned set: a single bind, or a
/// tuple of binds; `_` when the set is empty (effect-only fold).
fn acc_pattern(assigned: &[ArcStr]) -> StructurePattern {
    match assigned {
        [] => StructurePattern::Ignore,
        [x] => StructurePattern::Bind(x.clone()),
        xs => StructurePattern::Tuple {
            all: None,
            binds: Arc::from_iter(xs.iter().map(|x| StructurePattern::Bind(x.clone()))),
        },
    }
}

/// The accumulator VALUE for the assigned set: a single ref, a tuple
/// of refs, or `null` when empty.
fn acc_value(at: &Expr, assigned: &[ArcStr]) -> Expr {
    acc_ref(at, assigned)
}

fn acc_ref(at: &Expr, assigned: &[ArcStr]) -> Expr {
    match assigned {
        [] => ExprKind::Constant(netidx_value::Value::Null).to_expr(at.pos),
        [x] => ExprKind::Ref { name: ModPath::from_iter([x.as_str()]) }.to_expr(at.pos),
        xs => ExprKind::Tuple {
            args: Arc::from_iter(xs.iter().map(|x| {
                ExprKind::Ref { name: ModPath::from_iter([x.as_str()]) }.to_expr(at.pos)
            })),
        }
        .to_expr(at.pos),
    }
}
