//! The expression core: `gen_typed` emits an expression of exactly the
//! requested [`GenType`], recursing through operators, collections, and
//! in-scope references.

use super::{
    GenCtx,
    types::{self, GenType, I64, NUM_TYS, NumTy},
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

/// An accessor over a visible composite producing `ty`: a struct field
/// read, a tuple index, a bounds-checked array index / slice, a map key
/// lookup, or a numeric cast — each fallible one consumed by `$`
/// (bounds misses and narrowing overflows drop to bottom; kept to a
/// small budgeted fraction because a bottom program burns the campaign
/// timeout in every mode).
fn try_accessor(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    let mut cands: Vec<String> = Vec::new();
    let reffed = GenType::Ref(Box::new(ty.clone()));
    for (name, t) in ctx.visible_entries() {
        match t {
            // Deref a visible `&ty` binding.
            GenType::Ref(inner) => {
                if **inner == *ty {
                    cands.push(format!("*{name}"));
                }
            }
            GenType::Struct(fields) => {
                for (f, ft) in fields {
                    if ft == ty {
                        cands.push(format!("{name}.{f}"));
                    }
                    // Field-projection deref (`*(p.f)`) statically
                    // resolves — the one composite read refs support.
                    if *ft == reffed {
                        cands.push(format!("*({name}.{f})"));
                    }
                }
            }
            GenType::Tuple(elems) => {
                for (i, et) in elems.iter().enumerate() {
                    if et == ty {
                        cands.push(format!("{name}.{i}"));
                    }
                    if *et == reffed {
                        cands.push(format!("*({name}.{i})"));
                    }
                }
            }
            GenType::Array(e) => {
                if **e == *ty {
                    // Literal arrays are 1-3 long: 0 and -1 always hit;
                    // 1 / -4 sometimes / always miss (the bottom budget).
                    let idx = match rng.below(10) {
                        0..=5 => "0",
                        6..=7 => "-1",
                        8 => "1",
                        _ => "-4",
                    };
                    cands.push(format!("{name}[{idx}]$"));
                }
                // Element deref over an array of refs (ref arrays are
                // built 2 long, so 0/-1 always hit).
                if **e == reffed {
                    let idx = if rng.below(2) == 0 { "0" } else { "-1" };
                    cands.push(format!("*({name}[{idx}]$)"));
                }
                if ty == t {
                    let slice = if rng.below(2) == 0 { "..1" } else { "1.." };
                    cands.push(format!("{name}[{slice}]$"));
                }
            }
            GenType::Map(v) => {
                if **v == *ty {
                    let k = types::KEYS[rng.below(types::KEYS.len())];
                    cands.push(format!("{name}{{\"{k}\"}}$"));
                }
            }
            // A visible option unwrapped to its value type via a
            // two-arm type-match select. (`?` is error-only — it
            // rejects [T, null], so there is no try/catch consumer.)
            GenType::Nullable(t) => {
                if **t == *ty && ty.is_scalar() {
                    let dflt = gen_typed(ctx, rng, ty, depth);
                    cands.push(format!(
                        "select {name} {{ null as _ => {dflt}, {} as n => n }}",
                        ty.render()
                    ));
                }
            }
            GenType::Num(_)
            | GenType::Bool
            | GenType::Str
            | GenType::Variant(_)
            | GenType::Fn { .. }
            | GenType::PolyFn { .. }
            | GenType::Abstract { .. }
            | GenType::Opaque => {}
        }
    }
    // Numeric casts: mostly widening (lossless per `fits_in` — never
    // fails); the rest arbitrary, exercising the narrowing/overflow
    // error path at a budgeted rate.
    if let GenType::Num(t) = ty
        && rng.below(2) == 0
    {
        let fit: Vec<NumTy> =
            NUM_TYS.iter().copied().filter(|s| s != t && s.fits_in(*t)).collect();
        let any: Vec<NumTy> = NUM_TYS.iter().copied().filter(|s| s != t).collect();
        let src = if rng.below(10) < 8 && !fit.is_empty() {
            fit[rng.below(fit.len())]
        } else {
            any[rng.below(any.len())]
        };
        cands.push(format!(
            "cast<{}>({})$",
            ty.render(),
            gen_typed(ctx, rng, &GenType::Num(src), depth)
        ));
    }
    if cands.is_empty() {
        return None;
    }
    Some(cands.swap_remove(rng.below(cands.len())))
}

/// A callback parameter name: collision-pool-biased like every binding
/// (an HOF-callback local aliasing another function's binding is the
/// audit's bug-3 surface), unique within the param list.
fn callback_param(inner: &mut GenCtx, rng: &mut Rng, taken: &[String]) -> String {
    let mut n = if rng.below(10) < 3 && !inner.collision_pool.is_empty() {
        inner.collision_pool[rng.below(inner.collision_pool.len())].clone()
    } else {
        inner.fresh()
    };
    while taken.contains(&n) {
        n = inner.fresh();
    }
    n
}

/// The callback argument text + scope entries for element type `d_ty`:
/// either one param bound to the whole element, or — for a 2-3 tuple
/// element — a destructuring `|(k, v)|` pattern. `taken` holds sibling
/// param names already claimed (fold's accumulator).
fn callback_binder(
    inner: &mut GenCtx,
    rng: &mut Rng,
    d_ty: &GenType,
    taken: &[String],
) -> String {
    if let GenType::Tuple(elems) = d_ty {
        if elems.len() <= 3 && rng.below(2) == 0 {
            let mut names: Vec<String> = taken.to_vec();
            for _ in 0..elems.len() {
                let n = callback_param(inner, rng, &names);
                names.push(n);
            }
            let names = &names[taken.len()..];
            for (n, t) in names.iter().zip(elems.iter()) {
                inner.push(n.clone(), t.clone());
            }
            return format!("({})", names.join(", "));
        }
    }
    let n = callback_param(inner, rng, taken);
    inner.push(n.clone(), d_ty.clone());
    n
}

/// An array HOF producing `ty`: `map`/`filter`/`flat_map`/`init` for
/// array targets, `fold` for scalar targets. Callbacks are generated in
/// a CLONED scope (params + everything outer visible — captures come
/// free, and nested HOFs arise naturally through the body's own
/// `gen_typed` recursion).
fn try_hof(ctx: &GenCtx, rng: &mut Rng, ty: &GenType, depth: usize) -> Option<String> {
    if depth == 0 {
        return None;
    }
    let d = depth - 1;
    match ty {
        GenType::Array(e) => match rng.below(4) {
            0 => {
                // map: element type = ours (50%) or a random scalar.
                let d_ty = if rng.below(2) == 0 {
                    (**e).clone()
                } else {
                    types::scalar_type(rng)
                };
                let src = gen_typed(ctx, rng, &GenType::Array(Box::new(d_ty.clone())), d);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, &d_ty, &[]);
                let body = gen_typed(&inner, rng, e, d.min(2));
                Some(format!("array::map({src}, |{binder}| {body})"))
            }
            1 => {
                let src = gen_typed(ctx, rng, ty, d);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, e, &[]);
                let body = gen_typed(&inner, rng, &GenType::Bool, d.min(2));
                Some(format!("array::filter({src}, |{binder}| {body})"))
            }
            // flat_map's callback returns ['b, Array<'b>]. The checker
            // binds 'b to whatever the body IS without backtracking, so
            // an Array-typed body lands on the first union member and
            // the result comes out one Array deeper than intended —
            // only a scalar element body ('b = e, unambiguous) is safe.
            2 if e.is_scalar() => {
                let d_ty = types::scalar_type(rng);
                let src = gen_typed(ctx, rng, &GenType::Array(Box::new(d_ty.clone())), d);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, &d_ty, &[]);
                let body = gen_typed(&inner, rng, e, d.min(2));
                Some(format!("array::flat_map({src}, |{binder}| {body})"))
            }
            _ => {
                let n = 1 + rng.below(4);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, &I64, &[]);
                let body = gen_typed(&inner, rng, e, d.min(2));
                Some(format!("array::init({n}, |{binder}| {body})"))
            }
        },
        // find: the union return `[e, null]` IS the Nullable type. The
        // empty/no-match answer (Null) is the soak-jul06c B7 seam
        // (MapQ's empty-input shortcut vs the JIT find loop).
        GenType::Nullable(e) if e.is_scalar() => {
            let src = gen_typed(ctx, rng, &GenType::Array(Box::new((**e).clone())), d);
            let mut inner = ctx.clone();
            let binder = callback_binder(&mut inner, rng, e, &[]);
            let body = gen_typed(&inner, rng, &GenType::Bool, d.min(2));
            Some(format!("array::find({src}, |{binder}| {body})"))
        }
        _ if ty.is_scalar() => {
            let d_ty =
                if rng.below(2) == 0 { ty.clone() } else { types::random_type(rng, 1) };
            let src = gen_typed(ctx, rng, &GenType::Array(Box::new(d_ty.clone())), d);
            let init = gen_typed(ctx, rng, ty, d.min(2));
            let mut inner = ctx.clone();
            let acc = callback_param(&mut inner, rng, &[]);
            inner.push(acc.clone(), ty.clone());
            let binder = callback_binder(&mut inner, rng, &d_ty, &[acc.clone()]);
            let body = if *ty == I64 && rng.below(8) == 0 {
                // A terminating tail-recursive `let rec` INSIDE the
                // callback: the per-slot pred lazy-binds at runtime, a
                // dispatch path with its own resolution/marking pipeline
                // (soak-jul06c B8 — the rec-in-HOF-slot depth-guard trip
                // was unreachable while recs only appeared at statement
                // level).
                let lp = inner.fresh();
                let ln = inner.fresh();
                let la = inner.fresh();
                let depth = 200 + rng.below(200);
                let combine = gen_typed(&inner, rng, ty, 1);
                format!(
                    "{{ let rec {lp} = |{ln}: i64, {la}: i64| -> i64 \
                     select {ln} {{ i64:0 => {la}, _ => {lp}({ln} - i64:1, {la} + {ln}) }}; \
                     ({lp}(i64:{depth}, i64:0) * i64:0) + {combine} }}"
                )
            } else {
                gen_typed(&inner, rng, ty, d.min(2))
            };
            Some(format!("array::fold({src}, {init}, |{acc}, {binder}| {body})"))
        }
        _ => None,
    }
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
    // Any type — a composite-returning lambda (module interfaces carry
    // them since the composite-iface stage) is only exercised if calls
    // generate at composite-typed positions too.
    if rng.below(5) == 0 {
        if let Some(call) = try_call(ctx, rng, ty, d) {
            return call;
        }
    }
    if rng.below(4) == 0 {
        if let Some(acc) = try_accessor(ctx, rng, ty, d) {
            return acc;
        }
    }
    let hof_odds = if matches!(ty, GenType::Array(_)) { 4 } else { 8 };
    if rng.below(hof_odds) == 0 {
        if let Some(hof) = try_hof(ctx, rng, ty, d) {
            return hof;
        }
    }
    if rng.below(8) == 0 {
        if let Some(b) = try_str_builtin(ctx, rng, ty, d) {
            return b;
        }
    }
    match ty {
        GenType::Num(n) => {
            // Checked arithmetic, consumed by one of the three legal
            // forms: `$` (error -> bottom), a type-match select with an
            // error arm, or `?` under try/catch.
            if rng.below(8) == 0 {
                let op = pick(rng, &["+?", "-?", "*?", "/?", "%?"]);
                let a = gen_typed(ctx, rng, ty, d);
                let b = gen_typed(ctx, rng, ty, d);
                let dflt = gen_typed(ctx, rng, ty, d);
                return match rng.below(3) {
                    0 => format!("({a} {op} {b})$"),
                    1 => format!(
                        "select ({a} {op} {b}) {{ error as _ => {dflt}, {} as n => n }}",
                        ty.render()
                    ),
                    _ => format!("(try (({a} {op} {b}))? catch(e) => {dflt})"),
                };
            }
            // Unary minus (a real `Neg` node, exercising `ineg`/`fneg`). Only
            // for signed/float — `-` on an unsigned type is a compile error
            // (Part B's signed/float/decimal constraint). Parenthesize the
            // operand so `-(i64:5)` stays a `Neg` rather than re-parsing as a
            // negative literal.
            if n.is_signed() && rng.below(6) == 0 {
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
        GenType::Str => match rng.below(3) {
            // Computed interpolation: 1-2 [expr] parts, mostly scalar
            // but occasionally COMPOSITE (array/tuple/struct/map/
            // nullable — the type-directed TVal formatting walk, whose
            // union/fallback paths type-prefixed nested numerics until
            // soak-jul06c B4), occasionally with escaped literal
            // brackets around them.
            0 => {
                let n = 1 + rng.below(2);
                let parts: Vec<_> = (0..n)
                    .map(|_| {
                        let t = if rng.below(4) == 0 {
                            types::random_type(rng, 1)
                        } else {
                            types::scalar_type(rng)
                        };
                        format!("[{}]", gen_typed(ctx, rng, &t, d.min(1)))
                    })
                    .collect();
                let (pre, post) =
                    if rng.below(5) == 0 { ("\\[", "\\]") } else { ("", "") };
                format!("\"{pre}{}{post}\"", parts.join("-"))
            }
            _ => types::literal(rng, ty),
        },
        GenType::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| gen_typed(ctx, rng, e, d)).collect();
            format!("({})", parts.join(", "))
        }
        GenType::Array(elem) => {
            // Occasionally EMPTY (annotated via a block binding — a bare
            // `[]` doesn't infer): the zero-length path through HOFs,
            // slices, and folds (array::find([]) was soak-jul06c B7).
            if rng.below(10) == 0 {
                return format!("{{ let mt: {} = []; mt }}", ty.render());
            }
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| gen_typed(ctx, rng, elem, d)).collect();
            format!("[{}]", parts.join(", "))
        }
        GenType::Struct(fields) => {
            // Functional update over a visible same-shaped struct, or a
            // fresh literal with recursive field values.
            let sources = ctx.vars_of(ty);
            if !sources.is_empty() && rng.below(3) == 0 {
                let src = sources[rng.below(sources.len())];
                let (f, ft) = &fields[rng.below(fields.len())];
                let v = gen_typed(ctx, rng, ft, d);
                format!("{{ {src} with {f}: {v} }}")
            } else {
                let parts: Vec<_> = fields
                    .iter()
                    .map(|(f, t)| format!("{f}: {}", gen_typed(ctx, rng, t, d)))
                    .collect();
                format!("{{ {} }}", parts.join(", "))
            }
        }
        GenType::Variant(tags) => {
            let (tag, args) = &tags[rng.below(tags.len())];
            if args.is_empty() {
                format!("`{tag}")
            } else {
                let parts: Vec<_> =
                    args.iter().map(|t| gen_typed(ctx, rng, t, d)).collect();
                format!("`{tag}({})", parts.join(", "))
            }
        }
        GenType::Map(v) => {
            let n = 1 + rng.below(3);
            let mut keys: Vec<&str> = Vec::new();
            for _ in 0..n {
                let k = types::KEYS[rng.below(types::KEYS.len())];
                if !keys.contains(&k) {
                    keys.push(k);
                }
            }
            let parts: Vec<_> = keys
                .iter()
                .map(|k| format!("\"{k}\" => {}", gen_typed(ctx, rng, v, d)))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        GenType::Nullable(t) => {
            if rng.below(3) == 0 {
                "null".into()
            } else {
                gen_typed(ctx, rng, t, d)
            }
        }
        // A reference: a visible `&T` binding, `&` of a visible
        // T-typed binding, or `&(<expr>)` / `&<literal>` (all probed
        // working, incl. `&24.0`-style literal refs).
        GenType::Ref(inner) => {
            let ref_vars = ctx.vars_of(ty);
            if !ref_vars.is_empty() && rng.below(2) == 0 {
                return ref_vars[rng.below(ref_vars.len())].to_string();
            }
            let tgt_vars = ctx.vars_of(inner);
            if !tgt_vars.is_empty() && rng.below(2) == 0 {
                return format!("&{}", tgt_vars[rng.below(tgt_vars.len())]);
            }
            format!("&({})", gen_typed(ctx, rng, inner, d.min(1)))
        }
        // An abstract T: a T-typed binding, or the constructor over a
        // recursive i64 (the only producers — `un` consumption arises
        // organically through `try_call` at i64 positions).
        GenType::Abstract { module } => {
            let vars = ctx.vars_of(ty);
            if !vars.is_empty() && rng.below(2) == 0 {
                vars[rng.below(vars.len())].to_string()
            } else {
                format!("{module}::mk({})", gen_typed(ctx, rng, &I64, d.min(1)))
            }
        }
        GenType::Fn { .. } | GenType::PolyFn { .. } | GenType::Opaque => {
            unreachable!("gen_typed is never asked for a fn/opaque type")
        }
    }
}

/// A `str::` builtin call producing `ty`: length, predicates with
/// labeled args, and string transforms (the labeled-arg + DynCall
/// surface).
fn try_str_builtin(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    let d = depth.min(1);
    match ty {
        GenType::Num(NumTy::I64) => {
            Some(format!("str::len({})", gen_typed(ctx, rng, &GenType::Str, d)))
        }
        GenType::Bool => {
            let (f, lbl) =
                [("contains", "part"), ("starts_with", "pfx"), ("ends_with", "sfx")]
                    [rng.below(3)];
            let needle = types::literal(rng, &GenType::Str);
            let s = gen_typed(ctx, rng, &GenType::Str, d);
            Some(format!("str::{f}(#{lbl}: {needle}, {s})"))
        }
        GenType::Str => match rng.below(5) {
            0 => {
                let f = pick(rng, &["to_upper", "to_lower", "trim"]);
                Some(format!("str::{f}({})", gen_typed(ctx, rng, &GenType::Str, d)))
            }
            // sub: labeled args + a Result return consumed by `$`.
            // Occasionally a labeled arg is itself `$`-consumed checked
            // arith that never fires (div0 → error → `$` drops it) — a
            // builtin invoked with a PERMANENTLY missing labeled arg
            // must not fire at all (soak-jul06c B6: the node-walk fired
            // str::sub with len=None and returned its SubError).
            4 => {
                let start = rng.below(3);
                let len = if rng.below(6) == 0 {
                    "(i64:0 /? i64:0)$".to_string()
                } else {
                    format!("i64:{}", rng.below(4))
                };
                let s = gen_typed(ctx, rng, &GenType::Str, d);
                Some(format!("str::sub(#start: i64:{start}, #len: {len}, {s})$"))
            }
            1 => {
                let pat = types::literal(rng, &GenType::Str);
                let rep = types::literal(rng, &GenType::Str);
                let s = gen_typed(ctx, rng, &GenType::Str, d);
                Some(format!("str::replace(#pat: {pat}, #rep: {rep}, {s})"))
            }
            2 => Some(format!(
                "str::concat({}, {})",
                gen_typed(ctx, rng, &GenType::Str, d),
                gen_typed(ctx, rng, &GenType::Str, d)
            )),
            _ => {
                let sep = types::literal(rng, &GenType::Str);
                let arr = gen_typed(ctx, rng, &GenType::Array(Box::new(GenType::Str)), d);
                Some(format!("str::join(#sep: {sep}, {arr})"))
            }
        },
        _ => None,
    }
}
