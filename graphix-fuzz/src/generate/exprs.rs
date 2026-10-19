//! The expression core: `gen_typed` emits an expression of exactly the
//! requested [`GenType`], recursing through operators, collections, and
//! in-scope references.

use super::{
    GenCtx, chance,
    types::{self, GenType, I64, Label, NUM_TYS, NumTy},
};
use crate::mutate::Rng;

/// A call to a visible lambda producing `ty`: a typed lambda returning
/// `ty`, or, for numeric `ty`, a poly lambda with all args at `ty`.
fn try_call(ctx: &GenCtx, rng: &mut Rng, ty: &GenType, depth: usize) -> Option<String> {
    let typed = ctx.fns_returning(ty);
    let polys = if ty.is_numeric() { ctx.poly_fns() } else { Vec::new() };
    if typed.is_empty() && polys.is_empty() {
        return None;
    }
    let n = rng.below(typed.len() + polys.len());
    let (name, labels, param_tys) = if n < typed.len() {
        let (name, labels, params) = &typed[n];
        (name.to_string(), labels.clone(), params.clone())
    } else {
        let (name, arity) = polys[n - typed.len()];
        (name.to_string(), Vec::new(), vec![ty.clone(); arity])
    };
    Some(format!("{name}({})", call_args(ctx, rng, &labels, &param_tys, depth)))
}

/// A value of `ty` whose type is exactly `ty`: a literal can be narrower
/// (all nulls, or all non-null, of a nullable element; one variant of a
/// union), which a callback typed from `ty` or a sibling argument then
/// contradicts, so such a `ty` is pinned by an annotation.
pub(super) fn gen_pinned(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> String {
    let v = gen_typed(ctx, rng, ty, depth);
    let narrows = |e: &GenType| e.contains_nullable() || matches!(e, GenType::Variant(_));
    let wide = match ty {
        GenType::Array(e) | GenType::List(e) | GenType::Map { val: e, .. } => narrows(e),
        t => narrows(t),
    };
    match wide {
        true => format!("{{ let mt: {} = {v}; mt }}", ty.render()),
        false => v,
    }
}

/// A call's arguments: every required label and each optional one at
/// even odds, the labeled ones in random order, then the positionals.
pub(super) fn call_args(
    ctx: &GenCtx,
    rng: &mut Rng,
    labels: &[Label],
    params: &[GenType],
    depth: usize,
) -> String {
    let mut given: Vec<&Label> =
        labels.iter().filter(|l| !l.optional || chance(rng, 0.5)).collect();
    for i in (1..given.len()).rev() {
        given.swap(i, rng.below(i + 1));
    }
    let mut args: Vec<String> = given
        .into_iter()
        .map(|l| format!("#{}: {}", l.name, gen_typed(ctx, rng, &l.ty, depth)))
        .collect();
    args.extend(params.iter().map(|p| gen_typed(ctx, rng, p, depth)));
    args.join(", ")
}

/// An accessor over a visible composite producing `ty`: a struct field,
/// a tuple index, a bounds-checked array index / slice, a map lookup, or
/// a numeric cast, each fallible one consumed by `$`. Misses are kept to
/// a small fraction because one bottom blanks every reader of it.
fn try_accessor(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    let mut cands: Vec<String> = Vec::new();
    for (name, t) in ctx.visible_values() {
        match t {
            GenType::Ref { inner, .. } => {
                if **inner == *ty {
                    cands.push(format!("*{name}"));
                }
            }
            GenType::Struct(fields) => {
                for (f, ft) in fields {
                    if ft == ty {
                        cands.push(format!("{name}.{f}"));
                    }
                }
            }
            GenType::Tuple(elems) => {
                for (i, et) in elems.iter().enumerate() {
                    if et == ty {
                        cands.push(format!("{name}.{i}"));
                    }
                    if refers_to(et, ty) {
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
                        let nt = *rng.pick(&["u8", "i16", "u32"]);
                        cands.push(format!("{name}[{nt}:1]$"));
                    }
                }
                // ref arrays are built 2 long, so 0/-1 always hit
                if refers_to(e, ty) {
                    let idx = if rng.below(2) == 0 { "0" } else { "-1" };
                    cands.push(format!("*({name}[{idx}]$)"));
                }
                if ty == t {
                    let slice = *rng.pick(&["..1", "1..", "-1..", "..-1", "-3..-1"]);
                    cands.push(format!("{name}[{slice}]$"));
                    // narrow-int slice bound
                    if rng.below(8) == 0 {
                        let nt = *rng.pick(&["u8", "i16", "u32"]);
                        cands.push(format!("{name}[{nt}:1..]$"));
                    }
                }
            }
            GenType::Map { key, val } => {
                if **val == *ty {
                    let k = types::key_literal(rng, key);
                    cands.push(format!("{name}{{{k}}}$"));
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
            // an option unwrapped: a two-arm type-match select, `$`, or a
            // `?` whose null a catch takes
            GenType::Nullable(t) => {
                if **t == *ty && ty.is_scalar() {
                    cands.push(consume_fallible(ctx, rng, name, ty, "null", depth));
                }
            }
            GenType::Num(_)
            | GenType::Bool
            | GenType::Str
            | GenType::Variant(_)
            | GenType::Fn { .. }
            | GenType::Dur
            | GenType::Time
            | GenType::Bytes
            | GenType::Abstract { .. } => {}
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
                let n = inner.name_avoiding(rng, |_, n| names.iter().any(|t| t == n));
                names.push(n);
            }
            let names = &names[taken.len()..];
            for (n, t) in names.iter().zip(elems.iter()) {
                inner.push(n.clone(), t.clone());
            }
            return format!("({})", names.join(", "));
        }
    }
    let n = inner.name_avoiding(rng, |_, n| taken.iter().any(|t| t == n));
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

/// A source `coll(d_ty)` (an Array or a List) and a callback from `d_ty`
/// to `e`.
fn map_parts(
    ctx: &GenCtx,
    rng: &mut Rng,
    e: &GenType,
    coll: fn(Box<GenType>) -> GenType,
    d_ty: GenType,
    d: usize,
) -> HofParts {
    let src = gen_pinned(ctx, rng, &coll(Box::new(d_ty.clone())), d);
    let mut inner = ctx.clone();
    let binder = callback_binder(&mut inner, rng, &d_ty, &[]);
    let body = gen_typed(&inner, rng, e, d.min(2));
    HofParts { src, binder, body }
}

/// A fold callback's scope: `ctx` with an accumulator of `acc_ty` and a
/// binder for elements of `d_ty` after it, and the callback's parameter
/// list.
fn fold_params(
    ctx: &GenCtx,
    rng: &mut Rng,
    acc_ty: &GenType,
    d_ty: &GenType,
) -> (GenCtx, String) {
    let mut inner = ctx.clone();
    let acc = inner.name_avoiding(rng, |_, _| false);
    inner.push(acc.clone(), acc_ty.clone());
    let binder = callback_binder(&mut inner, rng, d_ty, &[acc.clone()]);
    (inner, format!("{acc}, {binder}"))
}

/// A source of `ty` (an Array or a List of `e`) and a bool predicate over
/// `e`.
fn filter_parts(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    e: &GenType,
    d: usize,
) -> HofParts {
    let src = gen_pinned(ctx, rng, ty, d);
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
        GenType::Array(e) => match rng.below(7) {
            0 => {
                let d_ty = map_source_elem(rng, e);
                let HofParts { src, binder, body } =
                    map_parts(ctx, rng, e, GenType::Array, d_ty, d);
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
                let HofParts { src, binder, body } =
                    map_parts(ctx, rng, e, GenType::Array, d_ty, d);
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
            // flat_map's callback returns the collection it splices
            2 => {
                let d_ty = map_source_elem(rng, e);
                let HofParts { src, binder, body } = map_parts(
                    ctx,
                    rng,
                    &GenType::Array(e.clone()),
                    GenType::Array,
                    d_ty,
                    d,
                );
                Some(format!("array::flat_map({src}, |{binder}| {body})"))
            }
            // filter_map keeps the payloads its callback does not null
            3 if e.is_scalar() => {
                let d_ty = map_source_elem(rng, e);
                let nullable = GenType::Nullable(e.clone());
                let HofParts { src, binder, body } =
                    map_parts(ctx, rng, &nullable, GenType::Array, d_ty, d);
                Some(format!("array::filter_map({src}, |{binder}| {body})"))
            }
            _ => {
                // occasionally an over-limit count (> MAX_ARRAY_INIT_LEN):
                // bottom on both engines, fast to evaluate
                let n = if rng.below(24) == 0 {
                    rng.pick(&["16777217", "99999999"]).to_string()
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
                let d_ty = map_source_elem(rng, e);
                let HofParts { src, binder, body } =
                    map_parts(ctx, rng, e, GenType::List, d_ty, d);
                Some(format!("list::map({src}, |{binder}| {body})"))
            }
            3 => {
                let HofParts { src, binder, body } = filter_parts(ctx, rng, ty, e, d);
                Some(format!("list::filter({src}, |{binder}| {body})"))
            }
            // filter_map keeps the payloads its callback does not null;
            // flat_map splices the list its callback returns
            _ => {
                let d_ty = map_source_elem(rng, e);
                let (f, ret) = match e.is_scalar() && rng.below(2) == 0 {
                    true => ("filter_map", GenType::Nullable(e.clone())),
                    false => ("flat_map", ty.clone()),
                };
                let HofParts { src, binder, body } =
                    map_parts(ctx, rng, &ret, GenType::List, d_ty, d);
                Some(format!("list::{f}({src}, |{binder}| {body})"))
            }
        },
        // find_map: the first payload its callback does not null
        GenType::Nullable(e) if e.is_scalar() && rng.below(3) == 0 => {
            let d_ty = map_source_elem(rng, e);
            let HofParts { src, binder, body } =
                map_parts(ctx, rng, ty, GenType::Array, d_ty, d);
            Some(if rng.below(3) == 0 {
                // the list twin
                format!("list::find_map(list::from_array({src}), |{binder}| {body})")
            } else {
                format!("array::find_map({src}, |{binder}| {body})")
            })
        }
        // find: the union return `[e, null]` is the Nullable type
        GenType::Nullable(e) if e.is_scalar() => {
            let src_ty = GenType::Array(e.clone());
            let HofParts { src, binder, body } = filter_parts(ctx, rng, &src_ty, e, d);
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
            let src = gen_pinned(ctx, rng, &GenType::Array(Box::new(d_ty.clone())), d);
            let init = gen_typed(ctx, rng, ty, d.min(2));
            let (mut inner, params) = fold_params(ctx, rng, ty, &d_ty);
            let body = if *ty == I64 && rng.below(8) == 0 {
                // a terminating tail-recursive `let rec` inside the
                // callback: the per-slot pred lazy-binds at runtime
                let lp = inner.fresh();
                let ln = inner.fresh();
                let la = inner.fresh();
                let depth = 200 + rng.below(200);
                let combine = gen_typed(&inner, rng, ty, 1);
                let attr = if chance(rng, 0.5) { "#[tail_recursive] " } else { "" };
                format!(
                    "{{ {attr}let rec {lp} = |{ln}: i64, {la}: i64| -> i64 \
                     select {ln} {{ i64:0 => {la}, _ => {lp}({ln} - i64:1, {la} + {ln}) }}; \
                     {lp}(i64:{depth}, i64:0) + {combine} }}"
                )
            } else {
                gen_typed(&inner, rng, ty, d.min(2))
            };
            if rng.below(4) == 0 {
                // the list twin
                Some(format!(
                    "list::fold(list::from_array({src}), {init}, |{params}| {body})"
                ))
            } else if rng.below(3) == 0 {
                Some(format!("Collection::fold({src}, {init}, |{params}| {body})"))
            } else {
                Some(format!("array::fold({src}, {init}, |{params}| {body})"))
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
    if ctx.codecs && rng.below(24) == 0 {
        if let Some(b) = try_format_round_trip(ctx, rng, ty, d) {
            return b;
        }
    }
    match ty {
        GenType::Num(n) => {
            // checked arithmetic, consumed by `$`, a type-match select
            // with an error arm, or `?` under a catch
            if rng.below(8) == 0 {
                let op = *rng.pick(&["+?", "-?", "*?", "/?", "%?"]);
                let a = gen_typed(ctx, rng, ty, d);
                let b = gen_typed(ctx, rng, ty, d);
                return consume_fallible(
                    ctx,
                    rng,
                    &format!("({a} {op} {b})"),
                    ty,
                    "error",
                    d,
                );
            }
            // unary minus: signed/float only; parenthesized so `-(i64:5)`
            // stays a `Neg` rather than a negative literal
            if n.is_signed() && rng.below(6) == 0 {
                return format!("(-({}))", gen_typed(ctx, rng, ty, d));
            }
            // bias toward +/-/* : a generated `/0` or `%0` bottoms, which
            // blanks every reader of it
            let op = *rng.pick(&["+", "+", "-", "-", "*", "*", "/", "%"]);
            format!(
                "({} {} {})",
                gen_typed(ctx, rng, ty, d),
                op,
                gen_typed(ctx, rng, ty, d)
            )
        }
        GenType::Bool => match rng.below(3) {
            0 if rng.below(8) == 0
                && let Some(c) = ref_comparison(ctx, rng) =>
            {
                c
            }
            0 => {
                let op = *rng.pick(&["<", ">", "<=", ">=", "==", "!="]);
                let nt = match rng.below(12) {
                    0..4 => types::random_type(rng, 1),
                    4 => types::random_variant(rng, 0),
                    _ => types::numeric_type(rng),
                };
                if nt.is_numeric() {
                    let (a, b) =
                        (gen_typed(ctx, rng, &nt, d), gen_typed(ctx, rng, &nt, d));
                    format!("({a} {op} {b})")
                } else {
                    // annotated operands: a bare `null` or tag is not the union
                    let t = nt.render();
                    let (a, b) =
                        (gen_typed(ctx, rng, &nt, d), gen_typed(ctx, rng, &nt, d));
                    format!("({{ let l: {t} = {a}; l }} {op} {{ let r: {t} = {b}; r }})")
                }
            }
            1 => {
                let op = *rng.pick(&["&&", "||"]);
                format!(
                    "({} {} {})",
                    gen_typed(ctx, rng, &GenType::Bool, d),
                    op,
                    gen_typed(ctx, rng, &GenType::Bool, d)
                )
            }
            _ => format!("(!{})", gen_typed(ctx, rng, &GenType::Bool, d)),
        },
        // the pure time arithmetic, its failures consumed
        GenType::Dur => match rng.below(4) {
            0 => {
                let (a, b) = (gen_typed(ctx, rng, ty, d), gen_typed(ctx, rng, ty, d));
                consume_fallible(
                    ctx,
                    rng,
                    &format!("sys::time::add_dur({a}, {b})"),
                    ty,
                    "error",
                    d,
                )
            }
            1 => {
                let a = gen_typed(ctx, rng, ty, d);
                let by = gen_typed(ctx, rng, &types::F64, d);
                consume_fallible(
                    ctx,
                    rng,
                    &format!("sys::time::scale({a}, {by})"),
                    ty,
                    "error",
                    d,
                )
            }
            2 => {
                let (a, b) = (gen_typed(ctx, rng, ty, d), gen_typed(ctx, rng, ty, d));
                format!("sys::time::sub_dur({a}, {b})")
            }
            _ => types::literal(rng, ty),
        },
        GenType::Time => match rng.below(3) {
            0 => {
                let (t, u) =
                    (gen_typed(ctx, rng, ty, d), gen_typed(ctx, rng, &GenType::Dur, d));
                let f = rng.pick(&["add", "sub"]);
                format!("sys::time::{f}({t}, {u})")
            }
            _ => types::literal(rng, ty),
        },
        GenType::Bytes => match rng.below(3) {
            0 => {
                format!("buffer::from_string({})", gen_typed(ctx, rng, &GenType::Str, d))
            }
            1 => {
                let (a, b) = (gen_typed(ctx, rng, ty, d), gen_typed(ctx, rng, ty, d));
                format!("buffer::concat({a}, {b})")
            }
            _ => types::literal(rng, ty),
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
        GenType::Map { key, val } => {
            let parts: Vec<_> = types::distinct_keys(rng, key)
                .iter()
                .map(|k| format!("{k} => {}", gen_typed(ctx, rng, val, d)))
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
        GenType::Ref { inner, mutable } => {
            let ref_vars = ctx.vars_of(ty);
            if !ref_vars.is_empty() && rng.below(2) == 0 {
                return ref_vars[rng.below(ref_vars.len())].to_string();
            }
            let amp = if *mutable { "&mut " } else { "&" };
            let tgt_vars = ctx.vars_of(inner);
            if !tgt_vars.is_empty() && rng.below(2) == 0 {
                return format!("{amp}{}", tgt_vars[rng.below(tgt_vars.len())]);
            }
            format!("{amp}({})", gen_typed(ctx, rng, inner, d.min(1)))
        }
        // an abstract T: a T-typed binding, or the constructor over an i64
        GenType::Abstract { module, boxed } => {
            let vars = ctx.vars_of(ty);
            if !vars.is_empty() && rng.below(2) == 0 {
                vars[rng.below(vars.len())].to_string()
            } else {
                let mk = if *boxed { "mkb" } else { "mk" };
                format!("{module}::{mk}({})", gen_typed(ctx, rng, &I64, d.min(1)))
            }
        }
        GenType::Fn { .. } => {
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
        GenType::Num(NumTy::I64) if rng.below(2) == 0 => {
            let elem = types::scalar_type(rng);
            let mty =
                GenType::Map { key: Box::new(types::map_key(rng)), val: Box::new(elem) };
            let m = gen_typed(ctx, rng, &mty, d);
            Some(format!("map::len({m})"))
        }
        // a fold over the (key, value) pairs, now and then: every scalar
        // target reaches here
        t if t.is_scalar() && rng.below(8) == 0 => {
            let elem = types::scalar_type(rng);
            let key = types::map_key(rng);
            let mty =
                GenType::Map { key: Box::new(key.clone()), val: Box::new(elem.clone()) };
            let m = gen_typed(ctx, rng, &mty, d);
            let init = gen_typed(ctx, rng, t, d);
            let (inner, params) =
                fold_params(ctx, rng, t, &GenType::Tuple(vec![key, elem]));
            let body = gen_typed(&inner, rng, t, d);
            Some(format!("map::fold({m}, {init}, |{params}| {body})"))
        }
        GenType::Map { key, val: e } => {
            let m = gen_pinned(ctx, rng, ty, d);
            let k = types::key_literal(rng, key);
            let kv = GenType::Tuple(vec![(**key).clone(), (**e).clone()]);
            match rng.below(6) {
                0 => {
                    let v = gen_typed(ctx, rng, e, d);
                    Some(format!("map::insert({m}, {k}, {v})"))
                }
                1 => Some(format!("map::remove({m}, {k})")),
                2 if **key == GenType::Str => {
                    Some(format!("map::filter({m}, |kv| str::len(kv.0) > i64:1)"))
                }
                2 => Some(format!("map::filter({m}, |kv| kv.0 != {k})")),
                3 => {
                    let mut inner = ctx.clone();
                    let binder = callback_binder(&mut inner, rng, &kv, &[]);
                    let body = gen_typed(&inner, rng, &kv, d);
                    Some(format!("map::map({m}, |{binder}| {body})"))
                }
                4 => {
                    let mut inner = ctx.clone();
                    let binder = callback_binder(&mut inner, rng, &kv, &[]);
                    let body =
                        gen_typed(&inner, rng, &GenType::Nullable(Box::new(kv)), d);
                    Some(format!("map::filter_map({m}, |{binder}| {body})"))
                }
                // both over the same small key pool, so their keys overlap
                _ => {
                    let other = gen_typed(ctx, rng, ty, d);
                    Some(format!("map::union({m}, {other})"))
                }
            }
        }
        _ => None,
    }
}

/// A value of `ty` from `x`, which may instead be `absent` (`error` or
/// `null`): `$`, a type-match select with a fallback arm, or a `?` under
/// a catch, whose handler's value goes nowhere (the block is bottom when
/// `x` is absent).
pub(super) fn consume_fallible(
    ctx: &GenCtx,
    rng: &mut Rng,
    x: &str,
    ty: &GenType,
    absent: &str,
    depth: usize,
) -> String {
    let t = ty.render();
    match rng.below(4) {
        0 => format!("{x}$"),
        1 => {
            let dflt = gen_typed(ctx, rng, ty, depth);
            format!("select {x} {{ {absent} as _ => {dflt}, {t} as n => n }}")
        }
        // the bare-wildcard form is the only one that reaches the
        // kernel's result-union predicate lowering
        2 => {
            let dflt = gen_typed(ctx, rng, ty, depth);
            format!("select {x} {{ {t} as n => n, _ => {dflt} }}")
        }
        _ if ctx.no_catch => format!("{x}$"),
        _ => {
            let handler = gen_typed(ctx, rng, ty, depth);
            format!("{{ catch(e) {handler}; {x}? }}")
        }
    }
}

/// Whether `r` is a reference, of either kind, to a `ty`.
fn refers_to(r: &GenType, ty: &GenType) -> bool {
    matches!(r, GenType::Ref { inner, .. } if **inner == *ty)
}

/// `==`/`!=` over two visible references of one type, compared by what
/// they name.
fn ref_comparison(ctx: &GenCtx, rng: &mut Rng) -> Option<String> {
    let refs: Vec<(&str, &GenType)> = ctx
        .visible_values()
        .into_iter()
        .filter(|(_, t)| matches!(t, GenType::Ref { .. }))
        .collect();
    if refs.is_empty() {
        return None;
    }
    let (a, ta) = refs[rng.below(refs.len())];
    let same: Vec<&str> =
        refs.iter().filter(|(_, t)| *t == ta).map(|(n, _)| *n).collect();
    let b = same[rng.below(same.len())];
    let op = *rng.pick(&["==", "!="]);
    Some(format!("({a} {op} {b})"))
}

/// A format a value round-trips through.
#[derive(Clone, Copy, PartialEq)]
enum Format {
    Json,
    Toml,
    Pack,
}

impl Format {
    /// Whether a value of `ty` round-trips through this format; `field`
    /// when it is a struct field, where toml writes a null as an absent
    /// key (anywhere else toml has no null).
    fn holds(self, ty: &GenType, field: bool) -> bool {
        use Format::*;
        let toml = self == Toml;
        match ty {
            GenType::Num(n) if toml => {
                !matches!(n, NumTy::Decimal | NumTy::U64 | NumTy::V64)
            }
            GenType::Num(_) | GenType::Bool | GenType::Str | GenType::Time => true,
            GenType::Dur | GenType::Bytes => !toml,
            GenType::Nullable(t) => (field || !toml) && self.holds(t, false),
            GenType::Tuple(ts) => ts.iter().all(|t| self.holds(t, false)),
            GenType::Array(t) | GenType::List(t) => self.holds(t, false),
            GenType::Map { key, val } => {
                let key_ok = match self {
                    Json => !matches!(**key, GenType::Tuple(_)),
                    Toml => **key == GenType::Str,
                    Pack => true,
                };
                key_ok && self.holds(key, false) && self.holds(val, false)
            }
            GenType::Struct(fs) => fs.iter().all(|(_, t)| self.holds(t, true)),
            GenType::Variant(vs) => {
                vs.iter().all(|(_, ts)| ts.iter().all(|t| self.holds(t, false)))
            }
            GenType::Fn { .. } | GenType::Ref { .. } | GenType::Abstract { .. } => false,
        }
    }
}

/// A value of `ty` through a format and its async, type-directed reader:
/// json or pack for any data the format holds, toml for a struct (its
/// documents are tables). The reader's target is the annotation, so the
/// imaged cast target meets the cold/warm comparison.
fn try_format_round_trip(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    let f = match (rng.below(3), ty) {
        (0, GenType::Struct(_)) => Format::Toml,
        (0 | 1, _) => Format::Json,
        _ => Format::Pack,
    };
    if !f.holds(ty, false) {
        return None;
    }
    let v = gen_typed(ctx, rng, ty, depth.min(1));
    let t = ty.render();
    let fmt = match f {
        Format::Toml => "toml::read(toml::write_str",
        Format::Json => "json::read(json::write_str",
        Format::Pack => "pack::read(pack::write_bytes",
    };
    Some(format!("{{ let r: {t} = {fmt}({v})$)$; r }}"))
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
                let pat = *rng.pick(&["a+", "[a-z]+", "x|y", "^g", "[0-9]", "(("]);
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
                let f = *rng.pick(&["to_upper", "to_lower", "trim"]);
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
            let pat = *rng.pick(&["a", ",", "[0-9]"]);
            let src = gen_typed(ctx, rng, &GenType::Str, d);
            Some(format!("re::split(#pat: r\"{pat}\", {src})$"))
        }
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Each format holds what it round-trips, as the codecs answer: toml
    /// has no duration, bytes, wide integer or decimal, and a null only as
    /// a struct field; json keys are scalars.
    #[test]
    fn formats_hold_what_round_trips() {
        use Format::*;
        use GenType::*;
        let b = |t: GenType| Box::new(t);
        let opt = Nullable(b(I64));
        let field = |t: GenType| Struct(vec![("x".into(), t)]);
        for f in [Json, Pack] {
            for t in [List(b(I64)), Dur, Time, Bytes, Array(b(opt.clone()))] {
                assert!(f.holds(&t, false), "{t:?}");
            }
        }
        let tuple_key = Map { key: b(Tuple(vec![I64, Str])), val: b(I64) };
        assert!(Pack.holds(&tuple_key, false) && !Json.holds(&tuple_key, false));
        assert!(Json.holds(&Map { key: b(I64), val: b(Str) }, false));
        assert!(Toml.holds(&field(opt.clone()), false));
        assert!(Toml.holds(&field(List(b(I64))), false));
        for t in [
            Dur,
            Bytes,
            Num(NumTy::Decimal),
            Num(NumTy::U64),
            Array(b(opt.clone())),
            Tuple(vec![I64, opt.clone()]),
        ] {
            assert!(!Toml.holds(&field(t.clone()), false), "{t:?}");
        }
    }
}
