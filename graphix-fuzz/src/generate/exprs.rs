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

/// A call to a visible lambda producing `ty`: a typed lambda returning
/// `ty`, or, for numeric `ty`, a poly lambda with all args at `ty`.
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

/// An accessor over a visible composite producing `ty`: a struct field,
/// a tuple index, a bounds-checked array index / slice, a map lookup, or
/// a numeric cast, each fallible one consumed by `$`. Misses are kept to
/// a small fraction because a bottom program burns the campaign timeout.
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
                    // field-projection deref is the one composite read
                    // refs support
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
                    // literal arrays are 1-3 long: 0 and -1 always hit
                    let idx = match rng.below(10) {
                        0..=5 => "0",
                        6..=7 => "-1",
                        8 => "1",
                        _ => "-4",
                    };
                    cands.push(format!("{name}[{idx}]$"));
                    // narrow-int index
                    if rng.below(8) == 0 {
                        let nt = pick(rng, &["u8", "i16", "u32"]);
                        cands.push(format!("{name}[{nt}:1]$"));
                    }
                }
                // ref arrays are built 2 long, so 0/-1 always hit
                if **e == reffed {
                    let idx = if rng.below(2) == 0 { "0" } else { "-1" };
                    cands.push(format!("*({name}[{idx}]$)"));
                }
                if ty == t {
                    let slice = if rng.below(2) == 0 { "..1" } else { "1.." };
                    cands.push(format!("{name}[{slice}]$"));
                    // narrow-int slice bound
                    if rng.below(8) == 0 {
                        let nt = pick(rng, &["u8", "i16", "u32"]);
                        cands.push(format!("{name}[{nt}:1..]$"));
                    }
                }
            }
            GenType::Map(v) => {
                if **v == *ty {
                    let k = types::KEYS[rng.below(types::KEYS.len())];
                    cands.push(format!("{name}{{\"{k}\"}}$"));
                }
            }
            // a visible list read through the pattern ladder
            GenType::List(e) => {
                if **e == *ty && ty.is_scalar() {
                    let dflt = gen_typed(ctx, rng, ty, depth);
                    cands.push(format!(
                        "select {name} {{ [<h0, ..>] => h0, [<>] => {dflt} }}"
                    ));
                }
                if ty == t {
                    // a bound head, never `_`: a wildcard's inferred
                    // predicate is Any, which poisons the completed tail type
                    cands.push(format!(
                        "select {name} {{ [<>] => {name}, [<h1, tl0..>] => tl0 }}"
                    ));
                }
            }
            // an option unwrapped via a two-arm type-match select (`?`
            // is error-only)
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
    // numeric casts: mostly lossless widening, the rest arbitrary
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

/// A callback parameter name, collision-pool-biased, unique within the
/// param list.
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

/// The three pieces of a generated HOF call: the source collection, the
/// callback's parameter and the callback's body.
struct HofParts {
    src: String,
    binder: String,
    body: String,
}

/// A map-shaped source's element type: ours (50%) or a random scalar.
fn map_source_elem(rng: &mut Rng, e: &GenType) -> GenType {
    if rng.below(2) == 0 { e.clone() } else { types::scalar_type(rng) }
}

/// A source of `Array<d_ty>` and a callback from `d_ty` to `e`.
fn map_parts(
    ctx: &GenCtx,
    rng: &mut Rng,
    e: &GenType,
    d_ty: GenType,
    d: usize,
) -> HofParts {
    let src = gen_typed(ctx, rng, &GenType::Array(Box::new(d_ty.clone())), d);
    let mut inner = ctx.clone();
    let binder = callback_binder(&mut inner, rng, &d_ty, &[]);
    let body = gen_typed(&inner, rng, e, d.min(2));
    HofParts { src, binder, body }
}

/// A source of `ty` (an `Array<e>`) and a bool predicate over `e`.
fn filter_parts(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    e: &GenType,
    d: usize,
) -> HofParts {
    let src = gen_typed(ctx, rng, ty, d);
    let mut inner = ctx.clone();
    let binder = callback_binder(&mut inner, rng, e, &[]);
    let body = gen_typed(&inner, rng, &GenType::Bool, d.min(2));
    HofParts { src, binder, body }
}

/// An array HOF producing `ty`: `map`/`filter`/`flat_map`/`init` for
/// array targets, `fold` for scalar targets. Callbacks are generated in
/// a cloned scope, so captures and nested HOFs arise naturally.
fn try_hof(ctx: &GenCtx, rng: &mut Rng, ty: &GenType, depth: usize) -> Option<String> {
    if depth == 0 {
        return None;
    }
    let d = depth - 1;
    match ty {
        GenType::Array(e) => match rng.below(6) {
            0 => {
                let d_ty = map_source_elem(rng, e);
                let HofParts { src, binder, body } = map_parts(ctx, rng, e, d_ty, d);
                // a third of draws take the trait road: `Collection::map`
                // dispatches through the constructor trait
                Some(if rng.below(3) == 0 {
                    format!("Collection::map({src}, |{binder}| {body})")
                } else {
                    format!("array::map({src}, |{binder}| {body})")
                })
            }
            1 => {
                let HofParts { src, binder, body } = filter_parts(ctx, rng, ty, e, d);
                Some(if rng.below(3) == 0 {
                    format!("Collection::filter({src}, |{binder}| {body})")
                } else {
                    format!("array::filter({src}, |{binder}| {body})")
                })
            }
            // list HOFs, roundtrip-wrapped so the target type stays Array
            4 => {
                let d_ty = map_source_elem(rng, e);
                let HofParts { src, binder, body } = map_parts(ctx, rng, e, d_ty, d);
                Some(format!(
                    "list::to_array(list::map(list::from_array({src}), |{binder}| {body}))"
                ))
            }
            5 => {
                let HofParts { src, binder, body } = filter_parts(ctx, rng, ty, e, d);
                Some(format!(
                    "list::to_array(list::filter(list::from_array({src}), |{binder}| {body}))"
                ))
            }
            // flat_map's callback returns ['b, Array<'b>] and the checker
            // binds 'b to the body without backtracking, so only a scalar
            // element body is unambiguous
            2 if e.is_scalar() => {
                let d_ty = types::scalar_type(rng);
                let HofParts { src, binder, body } = map_parts(ctx, rng, e, d_ty, d);
                Some(format!("array::flat_map({src}, |{binder}| {body})"))
            }
            _ => {
                // occasionally an over-limit count (> MAX_ARRAY_INIT_LEN):
                // bottom on both engines, fast to evaluate
                let n = if rng.below(24) == 0 {
                    pick(rng, &["16777217", "99999999"]).to_string()
                } else {
                    (1 + rng.below(4)).to_string()
                };
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, &I64, &[]);
                let body = gen_typed(&inner, rng, e, d.min(2));
                Some(format!("array::init({n}, |{binder}| {body})"))
            }
        },
        // a List target: literal / from_array / cons / the list HOFs
        GenType::List(e) => match rng.below(5) {
            0 => {
                let src =
                    gen_typed(ctx, rng, &GenType::Array(Box::new((**e).clone())), d);
                Some(format!("list::from_array({src})"))
            }
            1 => {
                let h = gen_typed(ctx, rng, e, d.min(2));
                let t = gen_typed(ctx, rng, ty, d.min(2));
                Some(format!("list::cons({h}, {t})"))
            }
            2 => {
                let d_ty = if rng.below(2) == 0 {
                    (**e).clone()
                } else {
                    types::scalar_type(rng)
                };
                let src = gen_typed(ctx, rng, &GenType::List(Box::new(d_ty.clone())), d);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, &d_ty, &[]);
                let body = gen_typed(&inner, rng, e, d.min(2));
                Some(format!("list::map({src}, |{binder}| {body})"))
            }
            3 => {
                let src = gen_typed(ctx, rng, ty, d);
                let mut inner = ctx.clone();
                let binder = callback_binder(&mut inner, rng, e, &[]);
                let body = gen_typed(&inner, rng, &GenType::Bool, d.min(2));
                Some(format!("list::filter({src}, |{binder}| {body})"))
            }
            _ => None,
        },
        // find: the union return `[e, null]` is the Nullable type
        GenType::Nullable(e) if e.is_scalar() => {
            let src = gen_typed(ctx, rng, &GenType::Array(Box::new((**e).clone())), d);
            let mut inner = ctx.clone();
            let binder = callback_binder(&mut inner, rng, e, &[]);
            let body = gen_typed(&inner, rng, &GenType::Bool, d.min(2));
            if rng.below(3) == 0 {
                // the list twin
                Some(format!("list::find(list::from_array({src}), |{binder}| {body})"))
            } else if rng.below(3) == 0 {
                Some(format!("Collection::find({src}, |{binder}| {body})"))
            } else {
                Some(format!("array::find({src}, |{binder}| {body})"))
            }
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
                // a terminating tail-recursive `let rec` inside the
                // callback: the per-slot pred lazy-binds at runtime
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
            if rng.below(4) == 0 {
                // the list twin
                Some(format!(
                    "list::fold(list::from_array({src}), {init}, |{acc}, {binder}| {body})"
                ))
            } else if rng.below(3) == 0 {
                Some(format!("Collection::fold({src}, {init}, |{acc}, {binder}| {body})"))
            } else {
                Some(format!("array::fold({src}, {init}, |{acc}, {binder}| {body})"))
            }
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
    // base case: a literal, or an in-scope ref (preferred: dataflow)
    let recurse = depth > 0 && rng.below(3) != 0;
    if !recurse {
        let vars = ctx.vars_of(ty);
        if !vars.is_empty() && rng.below(2) == 0 {
            return vars[rng.below(vars.len())].to_string();
        }
        return types::literal(rng, ty);
    }
    let d = depth - 1;
    // any type: composite-returning lambdas are only exercised if calls
    // generate at composite-typed positions too
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
    if rng.below(10) == 0 {
        if let Some(b) = try_map_builtin(ctx, rng, ty, d) {
            return b;
        }
    }
    match ty {
        GenType::Num(n) => {
            // checked arithmetic, consumed by `$`, a type-match select
            // with an error arm, or `?` under a catch
            if rng.below(8) == 0 {
                let op = pick(rng, &["+?", "-?", "*?", "/?", "%?"]);
                let a = gen_typed(ctx, rng, ty, d);
                let b = gen_typed(ctx, rng, ty, d);
                let dflt = gen_typed(ctx, rng, ty, d);
                return match rng.below(4) {
                    0 => format!("({a} {op} {b})$"),
                    1 => format!(
                        "select ({a} {op} {b}) {{ error as _ => {dflt}, {} as n => n }}",
                        ty.render()
                    ),
                    // the bare-wildcard form is the only one that reaches
                    // the kernel's result-union predicate lowering
                    2 => format!(
                        "select ({a} {op} {b}) {{ {} as n => n, _ => {dflt} }}",
                        ty.render()
                    ),
                    _ => format!("{{ catch(e) {dflt}; (({a} {op} {b}))? }}"),
                };
            }
            // unary minus: signed/float only; parenthesized so `-(i64:5)`
            // stays a `Neg` rather than a negative literal
            if n.is_signed() && rng.below(6) == 0 {
                return format!("(-({}))", gen_typed(ctx, rng, ty, d));
            }
            // bias toward +/-/* : a generated `/0` or `%0` bottoms and is
            // slow to check
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
            // computed interpolation: 1-2 [expr] parts, occasionally
            // composite, occasionally inside escaped literal brackets
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
            // occasionally empty (annotated: a bare `[]` doesn't infer)
            if rng.below(10) == 0 {
                return format!("{{ let mt: {} = []; mt }}", ty.render());
            }
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| gen_typed(ctx, rng, elem, d)).collect();
            format!("[{}]", parts.join(", "))
        }
        GenType::List(elem) => {
            // occasionally empty (annotated: a bare `[<>]` leaves the
            // element cell free)
            if rng.below(10) == 0 {
                return format!("{{ let mt: {} = [<>]; mt }}", ty.render());
            }
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| gen_typed(ctx, rng, elem, d)).collect();
            // nullable-bearing elements must be annotated: a `null` part
            // infers the bare null, not the union
            if ty.contains_nullable() {
                format!("{{ let mt: {} = [<{}>]; mt }}", ty.render(), parts.join(", "))
            } else {
                format!("[<{}>]", parts.join(", "))
            }
        }
        GenType::Struct(fields) => {
            // functional update over a visible same-shaped struct, or a
            // fresh literal
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
        // a visible `&T` binding, `&` of a visible T-typed binding, or
        // `&(<expr>)`
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
        // an abstract T: a T-typed binding, or the constructor over an i64
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

/// A `map::` builtin call producing `ty`. Keys come from the shared
/// pool so gets/removes mostly hit.
fn try_map_builtin(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    let d = depth.min(1);
    match ty {
        GenType::Num(NumTy::I64) => {
            let elem = types::scalar_type(rng);
            let m = gen_typed(ctx, rng, &GenType::Map(Box::new(elem)), d);
            Some(format!("map::len({m})"))
        }
        GenType::Map(e) => {
            let m = gen_typed(ctx, rng, ty, d);
            let k = types::KEYS[rng.below(types::KEYS.len())];
            match rng.below(3) {
                0 => {
                    let v = gen_typed(ctx, rng, e, d);
                    Some(format!("map::insert({m}, \"{k}\", {v})"))
                }
                1 => Some(format!("map::remove({m}, \"{k}\")")),
                _ => Some(format!("map::filter({m}, |kv| str::len(kv.0) > i64:1)")),
            }
        }
        _ => None,
    }
}

/// A `str::` builtin call producing `ty`: length, predicates with
/// labeled args, and string transforms.
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
            // sometimes a regex match; patterns from a valid pool plus one
            // malformed (the `$`-consumed ReError path)
            if rng.below(4) == 0 {
                let pat = pick(rng, &["a+", "[a-z]+", "x|y", "^g", "[0-9]", "(("]);
                let s = gen_typed(ctx, rng, &GenType::Str, d);
                // raw string: `[...]` in a plain literal is interpolation
                return Some(format!("re::is_match(#pat: r\"{pat}\", {s})$"));
            }
            let (f, lbl) =
                [("contains", "part"), ("starts_with", "pfx"), ("ends_with", "sfx")]
                    [rng.below(3)];
            let needle = types::literal(rng, &GenType::Str);
            let s = gen_typed(ctx, rng, &GenType::Str, d);
            Some(format!("str::{f}(#{lbl}: {needle}, {s})"))
        }
        GenType::Str => match rng.below(6) {
            // sprintf, valid and malformed formats; the Result return is
            // `$`-consumed either way
            5 => Some(match rng.below(4) {
                0 => format!("str::sprintf(\"%d\", i64:{})$", rng.below(100)),
                1 => format!(
                    "str::sprintf(\"%s\", {})$",
                    types::literal(rng, &GenType::Str)
                ),
                2 => "str::sprintf(\"%d\")$".to_string(),
                _ => "str::sprintf(\"%q\", i64:1)$".to_string(),
            }),
            0 => {
                let f = pick(rng, &["to_upper", "to_lower", "trim"]);
                Some(format!("str::{f}({})", gen_typed(ctx, rng, &GenType::Str, d)))
            }
            // sub: labeled args + a Result return consumed by `$`.
            // Occasionally a labeled arg is `$`-consumed checked arith
            // that never fires: the builtin must not fire at all then.
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
        GenType::Array(e) if **e == GenType::Str => {
            let pat = pick(rng, &["a", ",", "[0-9]"]);
            let src = gen_typed(ctx, rng, &GenType::Str, d);
            Some(format!("re::split(#pat: r\"{pat}\", {src})$"))
        }
        _ => None,
    }
}
