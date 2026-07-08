//! Lambda-binding emission: typed and poly (unannotated) lambdas,
//! monomorphization call-site pairs, the shadowed-lambda-name template,
//! and guaranteed-terminating `let rec` skeletons. Each emitter returns
//! the statements it produced; the caller owns the statement list.

use super::{
    GenCfg, GenCtx, GenStats, chance, exprs,
    types::{self, GenType, I64},
};
use crate::mutate::Rng;

/// Two DISTINCT numeric types for a monomorphization pair — any
/// combination from the full 14-type family (i16-vs-u64 pairs are
/// exactly the sign/zero-extension narrowing surface).
fn distinct_numeric_pair(rng: &mut Rng) -> (GenType, GenType) {
    let a = types::num_ty(rng);
    let mut b = types::num_ty(rng);
    while b == a {
        b = types::num_ty(rng);
    }
    (GenType::Num(a), GenType::Num(b))
}

/// Distinct parameter names, collision-pool-biased (a param that
/// collides with an outer binding or another lambda's local is the
/// point), but unique within one param list (`|x, x|` is an error).
fn param_names(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    arity: usize,
) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for _ in 0..arity {
        let mut n = ctx.name_for_bind(rng, cfg);
        while names.contains(&n) {
            n = ctx.fresh();
        }
        names.push(n);
    }
    names
}

/// The body of a typed lambda: an expression of the return type, or —
/// with `p_body_block` — a block with a collision-prone local (the
/// audit's bug-3 shape: a lambda-local aliasing a transitively-called
/// function's binding). Params are already in scope.
fn lambda_body(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
    ret: &GenType,
) -> String {
    if chance(rng, cfg.p_body_block) {
        let lty = types::scalar_type(rng);
        let lval = exprs::gen_typed(ctx, rng, &lty, 2);
        let lname = ctx.name_for_bind(rng, cfg);
        if ctx.in_collision_pool(&lname) {
            stats.collision_local = true;
        }
        let mark = ctx.mark();
        ctx.push(lname.clone(), lty);
        let tail = exprs::gen_typed(ctx, rng, ret, 2);
        ctx.truncate(mark);
        format!("{{ let {lname} = {lval}; {tail} }}")
    } else {
        exprs::gen_typed(ctx, rng, ret, 2)
    }
}

/// A typed lambda binding (`let f = |x: i64, s: string| -> i64 body`).
/// Params may shadow outer names; the body sees params + everything
/// outer, so it captures naturally.
pub(super) fn gen_typed_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> String {
    let arity = 1 + rng.below(3);
    let params: Vec<GenType> = (0..arity).map(|_| types::scalar_type(rng)).collect();
    let ret = types::scalar_type(rng);
    let names = param_names(ctx, rng, cfg, arity);
    let mark = ctx.mark();
    for (n, t) in names.iter().zip(params.iter()) {
        ctx.push(n.clone(), t.clone());
    }
    let body = lambda_body(ctx, rng, cfg, stats, &ret);
    ctx.truncate(mark);
    let sig: Vec<_> = names
        .iter()
        .zip(params.iter())
        .map(|(n, t)| format!("{n}: {}", t.render()))
        .collect();
    let name = ctx.name_for_bind(rng, cfg);
    if matches!(
        ctx.visible_type(&name),
        Some(GenType::Fn { .. } | GenType::PolyFn { .. })
    ) {
        stats.lambda_rebind = true;
    }
    let stmt = format!("let {name} = |{}| -> {} {body}", sig.join(", "), ret.render());
    ctx.push(name, GenType::Fn { params, ret: Box::new(ret) });
    stmt
}

/// A polymorphic lambda binding in the EXPLICIT constraint form
/// (`let g = 'a: Number |x: 'a, y: 'a| -> 'a x + y`) plus — with
/// `p_mono_pair` — two immediate call-site bindings at DISTINCT numeric
/// types: the audit's bug-2 shape (two monomorphizations of one lambda
/// in one region). The explicit form is load-bearing: a BARE
/// unannotated lambda's params share one widening tvar across all call
/// sites, so its results poison any annotated context ("f64 does not
/// contain '_N: Number") — see `gen_bare_lambda` for that shape. The
/// body is params-only `+ - *`/neg so the result type follows the args.
pub(super) fn gen_poly_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    let arity = 1 + rng.below(2);
    let names = param_names(ctx, rng, cfg, arity);
    let body = poly_body(rng, &names);
    let name = ctx.name_for_bind(rng, cfg);
    if matches!(
        ctx.visible_type(&name),
        Some(GenType::Fn { .. } | GenType::PolyFn { .. })
    ) {
        stats.lambda_rebind = true;
    }
    let sig: Vec<_> = names.iter().map(|n| format!("{n}: 'a")).collect();
    let mut stmts =
        vec![format!("let {name} = 'a: Number |{}| -> 'a {body}", sig.join(", "))];
    ctx.push(name.clone(), GenType::PolyFn { arity });
    if chance(rng, cfg.p_mono_pair) {
        stats.mono_pair = true;
        let (ta, tb) = distinct_numeric_pair(rng);
        for ty in [ta, tb] {
            let args: Vec<_> =
                (0..arity).map(|_| exprs::gen_typed(ctx, rng, &ty, 1)).collect();
            let cname = ctx.fresh();
            stmts.push(format!(
                "let {cname}: {} = {name}({})",
                ty.render(),
                args.join(", ")
            ));
            ctx.push(cname, ty);
        }
    }
    stmts
}

/// A BARE unannotated lambda (`let f = |a| a + a`) with two unannotated
/// call sites at distinct numeric types. This is the everyday user
/// shape AND the wide-tvar JIT-blocker class: the params share one
/// WIDENING tvar, so the call results are Number-wide — legal only in
/// unannotated contexts, and infectiously so (an op over a wide value
/// is wide too). The results therefore never enter the typed
/// vocabulary: the bindings exist purely to create the two
/// instantiation sites.
pub(super) fn gen_bare_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
) -> Vec<String> {
    let arity = 1 + rng.below(2);
    let names = param_names(ctx, rng, cfg, arity);
    let body = poly_body(rng, &names);
    let f = ctx.name_for_bind(rng, cfg);
    let mut stmts = vec![format!("let {f} = |{}| {body}", names.join(", "))];
    // f stays out of the callable vocabulary (a `try_call` in an
    // annotated context would reject) but must MASK whatever it
    // shadowed — a stale entry would offer the dead type to later refs.
    ctx.push(f.clone(), GenType::Opaque);
    let (ta, tb) = distinct_numeric_pair(rng);
    for ty in [ta, tb] {
        let args: Vec<_> = (0..arity).map(|_| types::literal(rng, &ty)).collect();
        let cname = ctx.fresh();
        stmts.push(format!("let {cname} = {f}({})", args.join(", ")));
    }
    stmts
}

/// A literal-free numeric body over exactly the params: combined with
/// `+ - *` only. A literal or division would pin or complicate the
/// type; unary neg constrains its operand to `[Real, Sint]`, which a
/// `Number` tvar (u8 included) does not fit.
fn poly_body(rng: &mut Rng, params: &[String]) -> String {
    let mut acc = params[rng.below(params.len())].clone();
    let n = 1 + rng.below(3);
    for _ in 0..n {
        let op = ["+", "-", "*"][rng.below(3)];
        let rhs = &params[rng.below(params.len())];
        acc = format!("({acc} {op} {rhs})");
    }
    acc
}

/// The audit bug-1 template, emitted whole: bind a lambda `f`, bind a
/// wrapper that CALLS `f`, REBIND `f`, then call the wrapper — correct
/// resolution must use the wrapper's captured (first) `f`, not the
/// name.
pub(super) fn gen_shadowed_lambda_template(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.lambda_rebind = true;
    let ty = types::numeric_type(rng);
    let t = ty.render();
    let f = ctx.name_for_bind(rng, cfg);
    let mut stmts = Vec::new();
    let p0 = ctx.fresh();
    stmts.push(format!(
        "let {f} = |{p0}: {t}| -> {t} ({p0} {} {})",
        ["+", "-", "*"][rng.below(3)],
        types::literal(rng, &ty)
    ));
    ctx.push(
        f.clone(),
        GenType::Fn { params: vec![ty.clone()], ret: Box::new(ty.clone()) },
    );
    let g = ctx.fresh();
    let p1 = ctx.fresh();
    stmts.push(format!(
        "let {g} = |{p1}: {t}| -> {t} ({f}({p1}) {} {})",
        ["+", "*"][rng.below(2)],
        types::literal(rng, &ty)
    ));
    ctx.push(
        g.clone(),
        GenType::Fn { params: vec![ty.clone()], ret: Box::new(ty.clone()) },
    );
    // The rebind of f: another lambda (same signature) or a plain value.
    if rng.below(2) == 0 {
        let p2 = ctx.fresh();
        stmts.push(format!(
            "let {f} = |{p2}: {t}| -> {t} ({p2} {} {})",
            ["-", "*"][rng.below(2)],
            types::literal(rng, &ty)
        ));
        ctx.push(f, GenType::Fn { params: vec![ty.clone()], ret: Box::new(ty.clone()) });
    } else {
        stmts.push(format!("let {f} = {}", types::literal(rng, &ty)));
        ctx.push(f, ty.clone());
    }
    let call = ctx.fresh();
    stmts.push(format!("let {call} = {g}({})", exprs::gen_typed(ctx, rng, &ty, 1)));
    ctx.push(call, ty);
    stmts
}

/// A lambda whose select merges an ok arm with an `error(...)` arm —
/// its return type is the union `[T, Error<E>]` — plus a call-site
/// binding taking either arm, consumed by one of the three legal error
/// consumers. This is the soak-jul06c B5 shape (the error arm's payload
/// pointer marshalled through a return frozen as Scalar(I64) — a class
/// only reachable when a lambda RETURN carries the error union), which
/// generation could never produce before: `error()` was not in the
/// vocabulary at all.
pub(super) fn gen_error_arm_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.error_lambda = true;
    let f = ctx.name_for_bind(rng, cfg);
    // Mask any shadowed binding; the union return keeps f out of the
    // callable vocabulary (try_call would use the bare ok type).
    ctx.push(f.clone(), GenType::Opaque);
    let n = ctx.fresh();
    let ret = types::scalar_type(rng);
    let mark = ctx.mark();
    ctx.push(n.clone(), I64);
    let ok = exprs::gen_typed(ctx, rng, &ret, 1);
    ctx.truncate(mark);
    let pty = types::scalar_type(rng);
    let payload = types::literal(rng, &pty);
    let lam = format!(
        "let {f} = |{n}: i64| select {n} {{ i64:0 => {ok}, _ => error({payload}) }}"
    );
    // Call so the ok arm or the ERROR arm is taken, then consume the
    // union with `$`, an error-arm select, or `?` under try.
    let arg = if rng.below(2) == 0 { "i64:0" } else { "i64:1" };
    let dflt = exprs::gen_typed(ctx, rng, &ret, 1);
    let consume = match rng.below(3) {
        0 => format!("{f}({arg})$"),
        1 => format!(
            "select {f}({arg}) {{ error as _ => {dflt}, {} as x => x }}",
            ret.render()
        ),
        _ => format!("(try ({f}({arg}))? catch(e) => {dflt})"),
    };
    let call = ctx.fresh();
    let stmts = vec![lam, format!("let {call} = {consume}")];
    ctx.push(call, ret);
    stmts
}

/// Reference statements: bind `let r = &<target>` (a visible scalar
/// binding, else a fresh literal — the `&24.0` GUI idiom), then
/// sometimes store the ref in a tuple (field-projection deref,
/// `*(p.0)`, is the one composite read refs support — an
/// `Array<&T>` element deref is a runtime error) or write through it
/// (`*r <- <literal>` — LITERAL RHS only: a self-reading RHS re-fires
/// every cycle and never quiesces, probed 2026-07-08). Reads (`*r` at
/// scalar positions, `&` args to ref-param fns) are organic
/// vocabulary from the binding alone.
pub(super) fn gen_ref_stmts(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.ref_op = true;
    let inner = types::scalar_type(rng);
    let rty = GenType::Ref(Box::new(inner.clone()));
    let mut stmts = Vec::new();
    let tgts = ctx.vars_of(&inner);
    let (val, var_target) = if !tgts.is_empty() && rng.below(3) != 0 {
        (format!("&{}", tgts[rng.below(tgts.len())]), true)
    } else {
        (format!("&{}", types::literal(rng, &inner)), false)
    };
    let r = ctx.name_for_bind(rng, cfg);
    stmts.push(format!("let {r} = {val}"));
    ctx.push(r.clone(), rty.clone());
    match rng.below(4) {
        0 => {
            let other = exprs::gen_typed(ctx, rng, &inner, 1);
            let t = ctx.fresh();
            stmts.push(format!("let {t} = ({r}, {other})"));
            ctx.push(t, GenType::Tuple(vec![rty, inner]));
        }
        // Write-through requires a VARIABLE target (`*(&lit) <- v`
        // has no binding to write).
        1 if var_target => {
            stmts.push(format!("*{r} <- {}", types::literal(rng, &inner)));
        }
        // Array of refs — element deref (`*(a[0]$)`) is legal since
        // Deref::typecheck0 derefs TVars (the 2026-07-08 fix Eric's
        // question found); try_accessor reads it back.
        2 => {
            let arr = ctx.fresh();
            stmts.push(format!("let {arr} = [{r}, {r}]"));
            ctx.push(arr, GenType::Array(Box::new(rty)));
        }
        _ => {}
    }
    stmts
}

/// A guaranteed-terminating `let rec` plus a call-site binding. The
/// base arm's `<= 0` guard terminates any argument sign; call args are
/// small literals (mutation perturbs them toward the edges later —
/// runaway variants are the mutation campaign's job, not generation's).
pub(super) fn gen_rec_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.rec = true;
    let f = ctx.name_for_bind(rng, cfg);
    // Inside its own body (and after it) `f` IS the rec lambda; mask
    // any shadowed binding BEFORE generating the base expression, or
    // the base could reference `f` at the dead outer type (or worse,
    // `try_call` could emit an unboundedly-recursive base-arm call).
    ctx.push(f.clone(), GenType::Opaque);
    let n = ctx.fresh();
    let m = ctx.fresh();
    let mark = ctx.mark();
    ctx.push(m.clone(), I64);
    let base = exprs::gen_typed(ctx, rng, &I64, 1);
    ctx.truncate(mark);
    let (sig, stmt_args, step) = match rng.below(4) {
        // Non-tail: n + f(n - 1).
        0 => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(12)),
            format!("({m} + {f}({m} - i64:1))"),
        ),
        // Tail loop with an accumulator.
        1 => {
            let acc = ctx.fresh();
            (
                format!("|{n}: i64, {acc}: i64| -> i64"),
                format!("{}, i64:0", 1 + rng.below(12)),
                format!("{f}({m} - i64:1, {acc} + {m})"),
            )
        }
        // Pure tail (no accumulator).
        2 => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(12)),
            format!("{f}({m} - i64:1)"),
        ),
        // Double recursion (fib-style) — keep the argument small.
        _ => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(10)),
            format!("({f}({m} - i64:1) + {f}({m} - i64:2))"),
        ),
    };
    // The tail-loop variant's acc param leaks from sig construction into
    // scope only within the body string; body references are textual, so
    // no ctx entry is needed beyond what the arms already read.
    let rec = format!(
        "let rec {f} = {sig} select {n} {{ {m} if {m} <= i64:0 => {base}, {m} => {step} }}"
    );
    // The rec lambda itself is not registered as callable vocabulary —
    // its shape is pinned; random extra call sites add little and risk
    // deep double-recursion blowups. The single call site:
    let call = ctx.fresh();
    let stmts = vec![rec, format!("let {call} = {f}(i64:{stmt_args})")];
    ctx.push(call, I64);
    stmts
}
