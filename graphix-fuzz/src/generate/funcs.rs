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
    let params: Vec<GenType> = (0..arity).map(|_| signature_type(rng)).collect();
    let ret = signature_type(rng);
    let names = param_names_excluding(ctx, rng, cfg, arity, &[]);
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
        ctx.visible_entry(&name),
        Some(super::Entry::Val(GenType::Fn { .. }) | super::Entry::Poly { .. })
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

/// A typed lambda's positional parameter or return type: a scalar, now
/// and then a union of runtime forms.
fn signature_type(rng: &mut Rng) -> GenType {
    match rng.below(16) {
        0 => types::random_union(rng, 1),
        _ => types::scalar_type(rng),
    }
}

/// A union of runtime forms that flows: built at an annotated let, passed
/// through a function that takes and returns it, stored in a struct, and
/// read by a second function's type-test select. Both functions enter the
/// callable vocabulary, so later calls pass and read unions too.
pub(super) fn gen_union_flow(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.union_flow = true;
    let u = types::random_union(rng, 1);
    let GenType::Union(members) = &u else { unreachable!() };
    let t = u.render();
    let member = rng.pick(members);
    let built = exprs::gen_typed(ctx, rng, member, 2);
    let v = ctx.name_for_bind(rng, cfg);
    let mut stmts = vec![format!("let {v}: {t} = {built}")];
    ctx.push(v.clone(), u.clone());
    let (f, p, k, g, q) =
        (ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh());
    let other = exprs::gen_typed(ctx, rng, &u, 1);
    stmts.push(format!(
        "let {f} = |{p}: {t}, {k}: i64| -> {t} select {k} {{ i64:0 => {other}, _ => {p} }}"
    ));
    let mut inner = ctx.clone();
    inner.push(q.clone(), u.clone());
    let read = super::patterns::union_select(&inner, rng, &q, members, &I64, 2);
    stmts.push(format!("let {g} = |{q}: {t}| -> i64 {read}"));
    let fn_ty = |params: Vec<GenType>, ret: GenType| GenType::Fn {
        labels: Vec::new(),
        params,
        ret: Box::new(ret),
    };
    ctx.push(f.clone(), fn_ty(vec![u.clone(), I64], u.clone()));
    ctx.push(g.clone(), fn_ty(vec![u.clone()], I64));
    let n = exprs::gen_typed(ctx, rng, &I64, 1);
    let (w, r) = (ctx.fresh(), ctx.fresh());
    stmts.push(format!("let {w}: {t} = {f}({v}, {n})"));
    let carried = match rng.below(2) {
        0 => w.clone(),
        _ => {
            let s = ctx.fresh();
            stmts.push(format!("let {s} = {{ n: {n}, x: {w} }}"));
            ctx.push(
                s.clone(),
                GenType::Struct(vec![("n".into(), I64), ("x".into(), u.clone())]),
            );
            format!("{s}.x")
        }
    };
    stmts.push(format!("let {r} = {g}({carried})"));
    ctx.push(w, u);
    ctx.push(r, I64);
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
        .visible_values()
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
    ctx.push_entry(h.clone(), super::Entry::Opaque);
    let call = exprs::call_args(ctx, rng, &view, &params, 1);
    ctx.truncate(mark);
    let r = ctx.fresh();
    let stmts = vec![
        format!("let {w} = |{h}: {}| -> {} {h}({call})", fty.render(), ret.render()),
        format!("let {r} = {w}({f})"),
    ];
    // w masks whatever it shadowed and is never called by the generator
    ctx.push_entry(w, super::Entry::Opaque);
    ctx.push(r, ret);
    stmts
}

/// A polymorphic lambda binding in the explicit constraint form
/// (`let g = 'a: Number |x: 'a, y: 'a| -> 'a x + y`) plus, with
/// `p_mono_pair`, two immediate call-site bindings at distinct numeric
/// types. The body is params-only `+ - *` so the result type follows the
/// args.
pub(super) fn gen_poly_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> Vec<String> {
    let arity = 1 + rng.below(2);
    let names = param_names_excluding(ctx, rng, cfg, arity, &[]);
    let body = poly_body(rng, &names);
    let name = ctx.name_for_bind(rng, cfg);
    if matches!(
        ctx.visible_entry(&name),
        Some(super::Entry::Val(GenType::Fn { .. }) | super::Entry::Poly { .. })
    ) {
        stats.lambda_rebind = true;
    }
    let sig: Vec<_> = names.iter().map(|n| format!("{n}: 'a")).collect();
    let mut stmts =
        vec![format!("let {name} = 'a: Number |{}| -> 'a {body}", sig.join(", "))];
    ctx.push_entry(name.clone(), super::Entry::Poly { arity });
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
/// call sites at distinct numeric types: each result has its arguments'
/// type, and the lambda is callable vocabulary like an explicit one.
pub(super) fn gen_bare_lambda(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
) -> Vec<String> {
    let arity = 1 + rng.below(2);
    let names = param_names_excluding(ctx, rng, cfg, arity, &[]);
    let body = poly_body(rng, &names);
    let f = ctx.name_for_bind(rng, cfg);
    let mut stmts = vec![format!("let {f} = |{}| {body}", names.join(", "))];
    ctx.push_entry(f.clone(), super::Entry::Poly { arity });
    let (ta, tb) = distinct_numeric_pair(rng);
    // the two sites: calls, or (one param) the lambda passed as a value,
    // which instantiates per use only because the binding is generalized
    let values = arity == 1 && chance(rng, 0.5);
    for ty in [ta, tb] {
        let cname = ctx.fresh();
        if values {
            let xs: Vec<_> = (0..2).map(|_| types::literal(rng, &ty)).collect();
            stmts.push(format!("let {cname} = array::map([{}], {f})", xs.join(", ")));
            ctx.push(cname, GenType::Array(Box::new(ty)));
        } else {
            let args: Vec<_> = (0..arity).map(|_| types::literal(rng, &ty)).collect();
            stmts.push(format!("let {cname} = {f}({})", args.join(", ")));
            ctx.push(cname, ty);
        }
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
    ctx.push_entry(f.clone(), super::Entry::Opaque);
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
    let consume =
        exprs::consume_fallible(ctx, rng, &format!("{f}({arg})"), &ret, "error", 1);
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
    let mut stmts = Vec::new();
    let tgts = ctx.vars_of(&inner);
    let places = places_of(ctx, &inner);
    let arrays = ctx.vars_of(&GenType::Array(Box::new(inner.clone())));
    let (val, var_target) = if !arrays.is_empty() && rng.below(4) == 0 {
        // a moving reference: its key is a variable written below
        let (arr, key) = (arrays[rng.below(arrays.len())].to_string(), ctx.fresh());
        stmts.push(format!("let {key} = i64:0"));
        stmts.push(format!("{key} <- i64:-1"));
        (format!("&mut {arr}[{key}]"), true)
    } else if !places.is_empty() && rng.below(2) == 0 {
        (format!("&mut {}", places[rng.below(places.len())]), true)
    } else if !tgts.is_empty() && rng.below(3) != 0 {
        (format!("&mut {}", tgts[rng.below(tgts.len())]), true)
    } else {
        (format!("&{}", types::literal(rng, &inner)), false)
    };
    let rty = GenType::Ref { inner: Box::new(inner.clone()), mutable: var_target };
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
        // write-through requires a variable target, here or in a callee
        1 if var_target && rng.below(2) == 0 => {
            stmts.push(format!("*{r} <- {}", types::literal(rng, &inner)));
        }
        1 if var_target => {
            let w = ctx.fresh();
            let (t, lit) = (inner.render(), types::literal(rng, &inner));
            if rng.below(2) == 0 {
                stmts.push(format!("let {w} = |p: &mut {t}| *p <- {lit}"));
                stmts.push(format!("{w}({r})"));
            } else {
                // an optional writable argument, given and omitted
                stmts.push(format!(
                    "let {w} = |#p: [&mut {t}, null] = null| \
                     select p {{ null => null, q => *q <- {lit} }}"
                ));
                stmts.push(format!("{w}(#p: {r})"));
                stmts.push(format!("{w}()"));
            }
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

/// Places of type `ty` inside visible composite bindings: a struct field,
/// a tuple element, an array element or a map entry (a write through the
/// reference patches the root; a missing element or key addresses
/// nothing).
fn places_of(ctx: &GenCtx, ty: &GenType) -> Vec<String> {
    let mut out = Vec::new();
    for (name, t) in ctx.visible_values() {
        if name.contains("::") {
            continue;
        }
        match t {
            GenType::Struct(fs) => out.extend(
                fs.iter().filter(|(_, ft)| ft == ty).map(|(f, _)| format!("{name}.{f}")),
            ),
            GenType::Tuple(es) => out.extend(
                es.iter()
                    .enumerate()
                    .filter(|(_, e)| *e == ty)
                    .map(|(i, _)| format!("{name}.{i}")),
            ),
            GenType::Array(e) if **e == *ty => {
                out.push(format!("{name}[0]"));
                out.push(format!("{name}[1]"));
            }
            GenType::Map { key, val } if **val == *ty && **key == GenType::Str => {
                out.push(format!("{name}{{\"k0\"}}"));
                out.push(format!("{name}{{\"a\"}}"));
            }
            _ => (),
        }
    }
    out
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
    ctx.push_entry(f.clone(), super::Entry::Opaque);
    let n = ctx.fresh();
    let m = ctx.fresh();
    let shape = rng.below(4);
    // the tail loop's accumulator, which the base arm returns
    let acc = (shape == 1).then(|| ctx.fresh());
    let mark = ctx.mark();
    ctx.push(m.clone(), I64);
    if let Some(acc) = &acc {
        ctx.push(acc.clone(), I64);
    }
    let base = exprs::gen_typed(ctx, rng, &I64, 1);
    let base = match &acc {
        Some(acc) => format!("({base} + {acc})"),
        None => base,
    };
    ctx.truncate(mark);
    let (sig, stmt_args, step) = match shape {
        // non-tail
        0 => (
            format!("|{n}: i64| -> i64"),
            format!("{}", 1 + rng.below(12)),
            format!("({m} + {f}({m} - i64:1))"),
        ),
        // tail loop with an accumulator
        1 => {
            let acc = acc.as_ref().expect("the tail loop's accumulator");
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
    // a tail shape whose base calls nothing (a call may hold per-call
    // memory, which no loop shares) and reads no reference (references
    // de-fuse) asserts its loop half the time: a JIT that stops building
    // it refuses (a body that cannot fuse is dropped unrecorded)
    // CR claude for claude: [bug/low] This scan misses consume_fallible's catch form `{
    // catch(e) h; (a op b)? }`, which the base can draw through checked arithmetic. The
    // tail-loop gate counts a catch as stateful, so both builds refuse the assertion
    // and the whole program is lost as a both-reject. Generate the base with
    // ctx.no_catch set, or refuse the attribute when the base contains `catch(`. probe:
    // design/review-2026-10-26/repro/fuzz-gen-a-tailrec-catch-base.gx (--expand
    // refuses, --check passes) (fuzz-gen-a-04)
    let calls_or_derefs = base.contains("::")
        || base.as_bytes().windows(2).any(|w| {
            ((w[0].is_ascii_alphanumeric() || w[0] == b'_') && w[1] == b'(')
                || (w[0] == b'*' && w[1] != b' ')
        });
    let attr = match (shape, !calls_or_derefs && chance(rng, 0.5)) {
        (1 | 2, true) => "#[tail_recursive] ",
        _ => "",
    };
    let rec = format!(
        "{attr}let rec {f} = {sig} select {n} {{ {m} if {m} <= i64:0 => {base}, {m} => {step} }}"
    );
    // the rec lambda itself is not callable vocabulary: random extra call
    // sites risk deep double-recursion blowups
    let call = ctx.fresh();
    let stmts = vec![rec, format!("let {call} = {f}(i64:{stmt_args})")];
    ctx.push(call, I64);
    stmts
}

/// The members a kind union draws from: its type, a value of it over an
/// `i64` `{n}` and a string `{s}`, and its type-test arm reading an `i64`.
const KIND_MEMBERS: [(&str, &str, &str); 9] = [
    ("Array<i64>", "[{n}, {n}]", "Array<i64> as a => array::len(a)"),
    ("(i64, i64)", "({n}, {n})", "(i64, i64) as (x, y) => x + y"),
    ("List<i64>", "[<{n}>]", "List<i64> as l => list::len(l) + i64:100"),
    ("{a: i64, b: i64}", "{a: {n}, b: {n}}", "{a: i64, b: i64} as {a, b} => a * b"),
    ("`A(i64, i64)", "`A({n}, {n})", "`A(x, y) => x - y"),
    ("string", "{s}", "string as t => str::len(t)"),
    ("`S", "`S", "`S => i64:7"),
    ("i64", "{n}", "i64 as k => k"),
    ("f64", "cast<f64>({n})$", "f64 as f => cast<i64>(f)$"),
];

/// A union of members of different runtime forms (composite kinds, a
/// string beside a bare variant, two numeric types), annotated at its
/// let and read by one type-test arm per member, compared with `==`, and
/// a map's key.
fn kind_union(ctx: &mut GenCtx, rng: &mut Rng, n: &str, s: &str) -> Vec<String> {
    let mut picked: Vec<usize> = (0..KIND_MEMBERS.len()).collect();
    rng.shuffle(&mut picked);
    picked.truncate(2 + rng.below(KIND_MEMBERS.len() - 1));
    let member = |i: usize| KIND_MEMBERS[i];
    let value = |i: usize| member(i).1.replace("{n}", n).replace("{s}", s);
    let ty = format!(
        "[{}]",
        picked.iter().map(|&i| member(i).0).collect::<Vec<_>>().join(", ")
    );
    let arms = picked.iter().map(|&i| member(i).2).collect::<Vec<_>>().join(", ");
    let (u, w, r, e, m) =
        (ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh());
    let (a, b) = (picked[rng.below(picked.len())], picked[rng.below(picked.len())]);
    let stmts = vec![
        format!("let {u}: {ty} = {}", value(a)),
        format!("let {w}: {ty} = {}", value(b)),
        format!("let {r} = select {u} {{ {arms} }}"),
        format!("let {e} = {u} == {w}"),
        // CR claude for claude: [bug/medium] A map key must be `Ordered`, which refuses
        // a type holding two numeric types. KIND_MEMBERS has both `i64` and `f64`, so
        // every union that picks both is refused here. That is 39 of the 40 rejects in
        // `gen-check 2000 6` (98% compile rate). The same rule is broken by exprs.rs's
        // annotated `<`/`>=` over a `types::random_union` int-beside-float pair. Fix:
        // add a `GenType::ordered()` predicate and use it at both sites. Here, key the
        // map only when the members are not both numeric; otherwise keep `==` alone.
        // probe: design/review-2026-10-26/repro/fuzz-gen-b-kind-union-mapkey.gx
        // (fuzz-gen-b-02)
        format!("let {m} = map::len({{{u} => i64:1, {w} => i64:2}})"),
    ];
    ctx.push(r, I64);
    ctx.push(e, GenType::Bool);
    ctx.push(m, I64);
    stmts
}

/// Results that reach a let and a typed parameter before they are
/// consumed: a checked product bound as it is, and a `Result` built by a
/// select, annotated, and passed to a function that takes it.
fn result_lets(ctx: &mut GenCtx, n: &str, s: &str) -> Vec<String> {
    let (c, cr, f, p, v, r) =
        (ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh(), ctx.fresh());
    let stmts = vec![
        format!("let {c} = {n} *? {n}"),
        format!("let {cr} = select {c} {{ error as _ => i64:0, x => x }}"),
        format!(
            "let {f} = |{p}: Result<i64, `E(string)>| -> i64 \
             select {p} {{ error as _ => i64:-1, x => x }}"
        ),
        format!(
            "let {v}: Result<i64, `E(string)> = \
             select ({n} % i64:2) {{ i64:0 => error(`E({s})), _ => {n} }}"
        ),
        format!("let {r} = {f}({v})"),
    ];
    ctx.push(cr, I64);
    ctx.push(r, I64);
    stmts
}

/// A value of a shape `random_type` never draws, built and consumed in
/// place, its consumer bound as an `i64`: a primitive union read by a
/// type test, an option of a composite, a variant nested in a struct, a
/// recursive typedef with a `let rec` builder and consumer, a union of
/// runtime forms, or Results bound before they are consumed. Each crosses
/// a kernel as an opaque value.
pub(super) fn gen_shape_template(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    stats: &mut GenStats,
) -> Vec<String> {
    stats.shape = true;
    let n = exprs::gen_typed(ctx, rng, &I64, 1);
    let s = exprs::gen_typed(ctx, rng, &GenType::Str, 1);
    match rng.below(6) {
        4 => {
            stats.kind_union = true;
            return kind_union(ctx, rng, &n, &s);
        }
        5 => {
            stats.result_let = true;
            return result_lets(ctx, &n, &s);
        }
        _ => (),
    }
    let v = ctx.fresh();
    let r = ctx.fresh();
    let stmts = match rng.below(4) {
        0 => {
            let init = if rng.below(2) == 0 { n } else { s };
            vec![
                format!("let {v}: [i64, string] = {init}"),
                format!(
                    "let {r} = select {v} {{ i64 as x => x, string as t => str::len(t) }}"
                ),
            ]
        }
        1 => {
            let init = if rng.below(3) == 0 {
                "null".to_string()
            } else {
                format!("({n}, {s})")
            };
            vec![
                format!("let {v}: [(i64, string), null] = {init}"),
                format!(
                    "let {r} = select {v} {{ null as _ => i64:-1, (x, t) => x + str::len(t) }}"
                ),
            ]
        }
        2 => {
            let tag =
                if rng.below(2) == 0 { format!("`A({n})") } else { "`B".to_string() };
            vec![
                format!(
                    "let {v}: {{ k: [`A(i64), `B], w: string }} = {{ k: {tag}, w: {s} }}"
                ),
                format!(
                    "let {r} = select {v}.k {{ `A(x) => x + str::len({v}.w), `B => str::len({v}.w) }}"
                ),
            ]
        }
        _ => {
            let t = format!("L{}", ctx.fresh());
            let build = ctx.fresh();
            let sum = ctx.fresh();
            let k = rng.below(8);
            vec![
                format!("type {t} = [`Cons(i64, {t}), `Nil]"),
                format!(
                    "let rec {build} = |m: i64, x: i64| -> {t} select m {{ i64:0 => `Nil, m => `Cons(x, {build}(m - i64:1, x)) }}"
                ),
                format!(
                    "let rec {sum} = |l: {t}| -> i64 select l {{ `Nil => i64:0, `Cons(h, tl) => h + {sum}(tl) }}"
                ),
                format!("let {v} = {build}(i64:{k}, {n})"),
                format!("let {r} = {sum}({v})"),
            ]
        }
    };
    ctx.push(r, I64);
    stmts
}

/// Sibling calls of one `queuefn` wrapper, each its own `let`, the
/// trigger a credit for fewer than all of them: the calls reach one ORDERED
/// builtin, so the first in program order run and the rest stay queued
/// whatever forks around them. The queued ones print, so a reordered call
/// shows as another left queued.
pub(super) fn gen_queuefn_siblings(ctx: &mut GenCtx, rng: &mut Rng) -> Vec<String> {
    let (qf, x) = (ctx.fresh(), ctx.fresh());
    let n = 2 + rng.below(3);
    // CR claude for claude: [test-gap/medium] The template assumes `#trigger:
    // i64:{credits}` releases calls 1..credits-1 and types them as live i64 values. A
    // trigger fire born with the queuefn is no credit, so with a constant trigger only
    // call 0 ever produces. The calls pushed as I64 are bottom forever, and every
    // expression generated over them is vacuous (`graphix-fuzz gen 150 1` draws credits
    // = 3, whose v4 and v5 feed a select and a division). Fix: push only call 0 as a
    // value and the rest as queued (Opaque, printed), or give the trigger fires after
    // birth that the model counts. probe:
    // design/review-2026-10-26/repro/x-rerun-3-fuzz-queuefn-credits.gx (x-rerun-3-03)
    // CR claude for claude: [bug/medium] A trigger fire born with the queuefn releases
    // nothing, so the constant `#trigger: i64:{credits}` gives exactly one release
    // whatever `credits` is. With credits >= 2, calls 1..credits-1 are pushed as live
    // I64 vocabulary but stay bottom forever, and every later expression that reads
    // them is blanked. Take credits = 1, or drive the trigger from a binding written
    // credits - 1 times after birth. probe:
    // design/review-2026-10-26/repro/fuzz-gen-a-queuefn-credits.gx (fuzz-gen-a-01)
    let credits = 1 + rng.below(n - 1);
    let body = exprs::gen_typed(ctx, rng, &I64, 1);
    let mut stmts = vec![format!(
        "let {qf} = queuefn(#trigger: i64:{credits}, |{x}: i64| -> i64 {x} + {body})"
    )];
    let calls: Vec<(String, String)> = (0..n)
        .map(|_| (ctx.fresh(), format!("{qf}({})", exprs::gen_typed(ctx, rng, &I64, 1))))
        .collect();
    ctx.push_entry(qf, super::Entry::Opaque);
    for (i, (c, call)) in calls.into_iter().enumerate() {
        stmts.push(format!("let {c} = {call}"));
        match i < credits {
            true => ctx.push(c, I64),
            false => {
                stmts.push(format!("println(\"[{c}]\")"));
                ctx.push_entry(c, super::Entry::Opaque);
            }
        }
    }
    stmts
}
