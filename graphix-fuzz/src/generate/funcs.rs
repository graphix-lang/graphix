//! Lambda-binding emission: typed and poly (unannotated) lambdas,
//! monomorphization call-site pairs, the shadowed-lambda-name template,
//! and guaranteed-terminating `let rec` skeletons. Each emitter returns
//! the statements it produced; the caller owns the statement list.

use super::{
    GenCfg, GenCtx, GenStats, chance, exprs,
    types::{self, GenType, I64, Label},
};
use crate::mutate::Rng;

/// Two distinct numeric types for a monomorphization pair, from the
/// full 14-type family.
fn distinct_numeric_pair(rng: &mut Rng) -> (GenType, GenType) {
    let a = types::num_ty(rng);
    let mut b = types::num_ty(rng);
    while b == a {
        b = types::num_ty(rng);
    }
    (GenType::Num(a), GenType::Num(b))
}

/// Distinct parameter names, collision-pool-biased but unique within
/// one param list (`|x, x|` is an error).
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

/// The body of a typed lambda: an expression of the return type, or,
/// with `p_body_block`, a block with a collision-prone local. Params are
/// already in scope.
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

/// A typed lambda binding (`let f = |x: i64, s: string| -> i64 body`),
/// with `p_labeled` labeled params first (`|#a: i64, #b: string = "x",
/// x: i64|`, then possibly no positionals) and then usually a call of it
/// that supplies or omits each default. Params may shadow outer names;
/// the body sees params + everything outer, so it captures naturally. A
/// default sees only the outer scope.
pub(super) fn gen_typed_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    let nlabels = if chance(rng, cfg.p_labeled) { 1 + rng.below(3) } else { 0 };
    let arity = if nlabels > 0 { rng.below(3) } else { 1 + rng.below(3) };
    let params: Vec<GenType> = (0..arity).map(|_| types::scalar_type(rng)).collect();
    let ret = types::scalar_type(rng);
    let names = param_names(ctx, rng, cfg, arity);
    let mut labeled: Vec<(Label, Option<String>)> = Vec::new();
    for name in param_names_excluding(ctx, rng, cfg, nlabels, &names) {
        let ty = types::scalar_type(rng);
        // half the defaults a literal, half an expression over the outer
        // scope
        let default = chance(rng, 0.5).then(|| match chance(rng, 0.5) {
            true => types::literal(rng, &ty),
            false => exprs::gen_typed(ctx, rng, &ty, 1),
        });
        labeled.push((Label { name, ty, optional: default.is_some() }, default));
    }
    if !labeled.is_empty() {
        stats.labeled_fn = true;
    }
    let mark = ctx.mark();
    for (l, _) in &labeled {
        ctx.push(l.name.clone(), l.ty.clone());
    }
    for (n, t) in names.iter().zip(params.iter()) {
        ctx.push(n.clone(), t.clone());
    }
    let body = lambda_body(ctx, rng, cfg, stats, &ret);
    ctx.truncate(mark);
    let sig: Vec<_> = labeled
        .iter()
        .map(|(l, d)| match d {
            Some(d) => format!("#{}: {} = {d}", l.name, l.ty.render()),
            None => format!("#{}: {}", l.name, l.ty.render()),
        })
        .chain(
            names.iter().zip(params.iter()).map(|(n, t)| format!("{n}: {}", t.render())),
        )
        .collect();
    let labels: Vec<Label> = labeled.into_iter().map(|(l, _)| l).collect();
    let name = ctx.name_for_bind(rng, cfg);
    if matches!(
        ctx.visible_type(&name),
        Some(GenType::Fn { .. } | GenType::PolyFn { .. })
    ) {
        stats.lambda_rebind = true;
    }
    let mut stmts =
        vec![format!("let {name} = |{}| -> {} {body}", sig.join(", "), ret.render())];
    let call = !labels.is_empty() && chance(rng, 0.7);
    let fty = GenType::Fn {
        labels: labels.clone(),
        params: params.clone(),
        ret: Box::new(ret.clone()),
    };
    // bound before the call's arguments: they must see `name` as the lambda
    ctx.push(name.clone(), fty);
    if call {
        let args = exprs::call_args(ctx, rng, &labels, &params, 1);
        let c = ctx.fresh();
        stmts.push(format!("let {c}: {} = {name}({args})", ret.render()));
        ctx.push(c, ret);
    }
    stmts
}

/// `n` names for params, distinct from each other and from `taken`.
fn param_names_excluding(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    n: usize,
    taken: &[String],
) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for _ in 0..n {
        let mut name = ctx.name_for_bind(rng, cfg);
        while names.contains(&name) || taken.contains(&name) {
            name = ctx.fresh();
        }
        names.push(name);
    }
    names
}

/// A labeled lambda passed as a value: a wrapper taking a function of a
/// VIEW of its type and calling it, then the wrapper applied to it. The
/// view keeps every required label and each optional one dropped, kept
/// optional or made required (`fntyp.rs::align` admits all three); the
/// wrapper's call supplies what the view requires and each optional
/// label of the view at even odds. The wrapper stays out of the callable
/// vocabulary (a function-typed argument has no generated value).
pub(super) fn gen_labeled_hof(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    let labeled: Vec<(String, Vec<Label>, Vec<GenType>, GenType)> = ctx
        .visible_entries()
        .into_iter()
        .filter_map(|(n, t)| match t {
            GenType::Fn { labels, params, ret } if !labels.is_empty() => {
                Some((n.to_string(), labels.clone(), params.clone(), (**ret).clone()))
            }
            _ => None,
        })
        .collect();
    if labeled.is_empty() {
        return Vec::new();
    }
    stats.labeled_hof = true;
    let (f, labels, params, ret) = labeled[rng.below(labeled.len())].clone();
    let view: Vec<Label> = labels
        .into_iter()
        .filter_map(|l| match (l.optional, rng.below(3)) {
            (false, _) => Some(l),
            (true, 0) => None,
            (true, 1) => Some(l),
            (true, _) => Some(Label { optional: false, ..l }),
        })
        .collect();
    let fty = GenType::Fn {
        labels: view.clone(),
        params: params.clone(),
        ret: Box::new(ret.clone()),
    };
    let h = ctx.fresh();
    // the wrapper must not shadow the function it is applied to
    let mut w = ctx.name_for_bind(rng, cfg);
    while w == f {
        w = ctx.fresh();
    }
    let mark = ctx.mark();
    ctx.push(h.clone(), GenType::Opaque);
    let call = exprs::call_args(ctx, rng, &view, &params, 1);
    ctx.truncate(mark);
    let r = ctx.fresh();
    let stmts = vec![
        format!("let {w} = |{h}: {}| -> {} {h}({call})", fty.render(), ret.render()),
        format!("let {r} = {w}({f})"),
    ];
    // w masks whatever it shadowed and is never called by the generator
    ctx.push(w, GenType::Opaque);
    ctx.push(r, ret);
    stmts
}

/// A polymorphic lambda binding in the explicit constraint form
/// (`let g = 'a: Number |x: 'a, y: 'a| -> 'a x + y`) plus, with
/// `p_mono_pair`, two immediate call-site bindings at distinct numeric
/// types. The explicit form is load-bearing: a bare unannotated lambda's
/// params share one widening tvar, so its results poison any annotated
/// context (see `gen_bare_lambda`). The body is params-only `+ - *` so
/// the result type follows the args.
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

/// A bare unannotated lambda (`let f = |a| a + a`) with two unannotated
/// call sites at distinct numeric types. The params share one widening
/// tvar, so the results are Number-wide and never enter the typed
/// vocabulary; the bindings exist to create the two instantiation sites.
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
    // f stays out of the callable vocabulary but must mask whatever it
    // shadowed
    ctx.push(f.clone(), GenType::Opaque);
    let (ta, tb) = distinct_numeric_pair(rng);
    for ty in [ta, tb] {
        let args: Vec<_> = (0..arity).map(|_| types::literal(rng, &ty)).collect();
        let cname = ctx.fresh();
        stmts.push(format!("let {cname} = {f}({})", args.join(", ")));
    }
    stmts
}

/// A literal-free numeric body over exactly the params, combined with
/// `+ - *` only: a literal or division would pin the type, and unary neg
/// constrains its operand to `[Real, Sint]`, which `Number` does not fit.
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

/// The shadowed-lambda template: bind a lambda `f`, bind a wrapper that
/// calls `f`, rebind `f`, then call the wrapper. Resolution must use the
/// wrapper's captured first `f`, not the name.
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
        GenType::Fn {
            labels: Vec::new(),
            params: vec![ty.clone()],
            ret: Box::new(ty.clone()),
        },
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
        GenType::Fn {
            labels: Vec::new(),
            params: vec![ty.clone()],
            ret: Box::new(ty.clone()),
        },
    );
    if rng.below(2) == 0 {
        let p2 = ctx.fresh();
        stmts.push(format!(
            "let {f} = |{p2}: {t}| -> {t} ({p2} {} {})",
            ["-", "*"][rng.below(2)],
            types::literal(rng, &ty)
        ));
        ctx.push(
            f,
            GenType::Fn {
                labels: Vec::new(),
                params: vec![ty.clone()],
                ret: Box::new(ty.clone()),
            },
        );
    } else {
        stmts.push(format!("let {f} = {}", types::literal(rng, &ty)));
        ctx.push(f, ty.clone());
    }
    let call = ctx.fresh();
    stmts.push(format!("let {call} = {g}({})", exprs::gen_typed(ctx, rng, &ty, 1)));
    ctx.push(call, ty);
    stmts
}

/// A lambda whose select merges an ok arm with an `error(...)` arm, so
/// its return type is the union `[T, Error<E>]`, plus a call-site
/// binding consumed by one of the three legal error consumers.
pub(super) fn gen_error_arm_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.error_lambda = true;
    let f = ctx.name_for_bind(rng, cfg);
    // mask any shadowed binding; the union return keeps f out of the
    // callable vocabulary
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
    // call so the ok arm or the error arm is taken, then consume the union
    let arg = if rng.below(2) == 0 { "i64:0" } else { "i64:1" };
    let dflt = exprs::gen_typed(ctx, rng, &ret, 1);
    let consume = match rng.below(3) {
        0 => format!("{f}({arg})$"),
        1 => format!(
            "select {f}({arg}) {{ error as _ => {dflt}, {} as x => x }}",
            ret.render()
        ),
        _ => format!("{{ catch(e) {dflt}; ({f}({arg}))? }}"),
    };
    let call = ctx.fresh();
    let stmts = vec![lam, format!("let {call} = {consume}")];
    ctx.push(call, ret);
    stmts
}

/// Reference statements: `let r = &<target>` (a visible scalar binding,
/// else a fresh literal), then sometimes store the ref in a tuple or
/// write through it with a literal RHS (a self-reading RHS re-fires
/// every cycle and never quiesces). Reads are organic vocabulary from
/// the binding alone.
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
        // write-through requires a variable target
        1 if var_target => {
            stmts.push(format!("*{r} <- {}", types::literal(rng, &inner)));
        }
        // array of refs; try_accessor reads it back
        2 => {
            let arr = ctx.fresh();
            stmts.push(format!("let {arr} = [{r}, {r}]"));
            ctx.push(arr, GenType::Array(Box::new(rty)));
        }
        _ => {}
    }
    stmts
}

/// A guaranteed-terminating `let rec` plus a call-site binding: the
/// base arm's `<= 0` guard terminates any argument sign, and call args
/// are small literals (runaway variants are mutation's job).
pub(super) fn gen_rec_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.rec = true;
    let f = ctx.name_for_bind(rng, cfg);
    // Inside its own body `f` is the rec lambda: mask any shadowed
    // binding before generating the base expression, or the base could
    // reference `f` at the dead outer type.
    ctx.push(f.clone(), GenType::Opaque);
    let n = ctx.fresh();
    let m = ctx.fresh();
    let mark = ctx.mark();
    ctx.push(m.clone(), I64);
    let base = exprs::gen_typed(ctx, rng, &I64, 1);
    ctx.truncate(mark);
    let (sig, stmt_args, step) = match rng.below(4) {
        // non-tail
        0 => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(12)),
            format!("({m} + {f}({m} - i64:1))"),
        ),
        // tail loop with an accumulator
        1 => {
            let acc = ctx.fresh();
            (
                format!("|{n}: i64, {acc}: i64| -> i64"),
                format!("{}, i64:0", 1 + rng.below(12)),
                format!("{f}({m} - i64:1, {acc} + {m})"),
            )
        }
        // pure tail
        2 => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(12)),
            format!("{f}({m} - i64:1)"),
        ),
        // double recursion: keep the argument small
        _ => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(10)),
            format!("({f}({m} - i64:1) + {f}({m} - i64:2))"),
        ),
    };
    let rec = format!(
        "let rec {f} = {sig} select {n} {{ {m} if {m} <= i64:0 => {base}, {m} => {step} }}"
    );
    // the rec lambda itself is not callable vocabulary: random extra call
    // sites risk deep double-recursion blowups
    let call = ctx.fresh();
    let stmts = vec![rec, format!("let {call} = {f}(i64:{stmt_args})")];
    ctx.push(call, I64);
    stmts
}
