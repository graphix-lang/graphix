//! Module emission: a generated module is a pair of wrapper file
//! sections (`m{i}.gx` + usually `m{i}.gxi`) plus registrations into
//! the main scope, where public lambdas enter the callable vocabulary
//! under their absolute path (`m0::f1`). The module's own scope is
//! self-contained: a root-mounted module cannot see main bindings.

use super::{
    GenCfg, GenCtx, GenStats, chance, exprs,
    types::{self, GenType, I64, NumTy},
};
use crate::mutate::Rng;

pub(super) struct GenModule {
    /// Wrapper file sections: `m{i}.gx` and, unless the bare-module
    /// variant fired, `m{i}.gxi`.
    pub files: Vec<(String, String)>,
    /// Statements for the main body.
    pub stmts: Vec<String>,
}

/// A sibling module's public (`m<j>::…`) does not resolve bare inside a
/// later module. One road is drawn per module: 0 keeps the plain
/// spelling and emits a `use super::{m0, …};` header, 1 rewrites
/// references to `super::m<j>::…` inline, 2 to `package::m<j>::…`.
fn sibling_qualified(n: &str) -> bool {
    n.strip_prefix('m').and_then(|r| r.split_once("::")).is_some_and(|(digits, _)| {
        !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit())
    })
}

/// Walk the abstract-type module fields nested in a vocabulary entry's
/// type. Roads 1/2 prefix them; registration back into main strips the
/// prefix so the main vocabulary stays plain.
fn map_abstract(t: &mut GenType, f: &impl Fn(&mut String)) {
    match t {
        GenType::Abstract { module } => f(module),
        GenType::Tuple(ts) => ts.iter_mut().for_each(|t| map_abstract(t, f)),
        GenType::Array(t)
        | GenType::List(t)
        | GenType::Map(t)
        | GenType::Nullable(t)
        | GenType::Ref(t) => map_abstract(t, f),
        GenType::Struct(fs) => fs.iter_mut().for_each(|(_, t)| map_abstract(t, f)),
        GenType::Variant(vs) => vs
            .iter_mut()
            .for_each(|(_, ts)| ts.iter_mut().for_each(|t| map_abstract(t, f))),
        GenType::Fn { params, ret } => {
            params.iter_mut().for_each(|t| map_abstract(t, f));
            map_abstract(ret, f)
        }
        GenType::Num(_)
        | GenType::Bool
        | GenType::Str
        | GenType::PolyFn { .. }
        | GenType::Opaque => (),
    }
}

/// Emit one module. Public lambdas are registered in `ctx` under
/// `m{idx}::name`; an abstract-type round-trip (when present) binds an
/// i64 in the main scope via `stmts`.
pub(super) fn gen_module(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
    idx: usize,
) -> GenModule {
    stats.module = true;
    let mname = format!("m{idx}");
    // The module's own scope: params, locals, and earlier modules'
    // path-qualified publics (`mod m0;` is declared before `mod m1;`).
    // Main-body bindings are dropped; names still come from the shared
    // collision pool.
    let mut inner = ctx.clone();
    inner.vars.retain(|(n, _)| n.contains("::"));
    // decided up front: unannotated-return impls are legal only when the
    // interface states the full type
    let has_gxi = !chance(rng, cfg.p_bare_module);
    let mut gx = String::new();
    let mut gxi = String::new();
    // The cross-module road (see `sibling_qualified`). Road 0's `use`
    // header goes into the gxi when present: its imports apply to the impl.
    let road = if idx > 0 { rng.below(3) } else { 0 };
    if road != 0 {
        let prefix = if road == 1 { "super" } else { "package" };
        for (n, t) in inner.vars.iter_mut() {
            if sibling_qualified(n) {
                *n = format!("{prefix}::{n}");
            }
            map_abstract(t, &|module| {
                if !module.contains("::") {
                    *module = format!("{prefix}::{module}");
                }
            });
        }
        stats.use_vocab = true;
    } else if idx > 0 {
        let list: Vec<String> = (0..idx).map(|j| format!("m{j}")).collect();
        let header = format!("use super::{{{}}};\n", list.join(", "));
        if has_gxi {
            gxi.push_str(&header)
        } else {
            gx.push_str(&header)
        }
        stats.use_vocab = true;
    }
    let mut public: Vec<(String, GenType)> = Vec::new();
    // optional exported constant, emitted first so later fn bodies can
    // reference it
    let konst = if chance(rng, 0.5) {
        let k = format!("k{idx}");
        let kty = if chance(rng, 0.3) {
            types::random_type(rng, 1)
        } else {
            types::scalar_type(rng)
        };
        let kv = exprs::gen_typed(&inner, rng, &kty, 1);
        gx.push_str(&format!("let {k}: {t} = {kv};\n", t = kty.render()));
        gxi.push_str(&format!("val {k}: {};\n", kty.render()));
        inner.push(k.clone(), kty.clone());
        ctx.push(format!("{mname}::{k}"), kty.clone());
        stats.iface_const = true;
        Some((k, kty))
    } else {
        None
    };
    // optional private helper, used by the first public lambda and
    // absent from the interface
    let helper = if chance(rng, 0.5) {
        let h = format!("h{idx}");
        let ht = types::numeric_type(rng);
        let p = inner.fresh();
        let body = {
            let m = inner.mark();
            inner.push(p.clone(), ht.clone());
            let b = exprs::gen_typed(&inner, rng, &ht, 2);
            inner.truncate(m);
            b
        };
        gx.push_str(&format!("let {h} = |{p}: {t}| -> {t} {body};\n", t = ht.render()));
        Some((h, ht))
    } else {
        None
    };
    // An interface-boundary type: scalars keep the majority; composites
    // and the occasional tag union cross too.
    let iface_type = |rng: &mut Rng, stats: &mut GenStats| match rng.below(10) {
        0..=4 => types::scalar_type(rng),
        5..=8 => {
            let t = types::random_type(rng, 2);
            if !t.is_scalar() {
                stats.composite_iface = true;
            }
            t
        }
        _ => {
            stats.composite_iface = true;
            types::random_variant(rng, 1)
        }
    };
    // cross-module wiring (later modules): pick an earlier module's
    // public fn, pin f0's return type to the callee's, splice the call
    // into f0's body
    let cross = if idx > 0 && chance(rng, 0.6) {
        let cands: Vec<(String, Vec<GenType>, GenType)> = inner
            .visible_entries()
            .into_iter()
            .filter(|(n, _)| n.contains("::"))
            .filter_map(|(n, t)| match t {
                GenType::Fn { params, ret } => {
                    Some((n.to_string(), params.clone(), (**ret).clone()))
                }
                _ => None,
            })
            .collect();
        (!cands.is_empty()).then(|| cands[rng.below(cands.len())].clone())
    } else {
        None
    };
    let nfns = 1 + rng.below(3);
    for i in 0..nfns {
        let fname = format!("f{i}");
        let arity = 1 + rng.below(2);
        // params only, never returns: `&T` across the interface
        let params: Vec<GenType> = (0..arity)
            .map(|_| {
                if chance(rng, 0.15) {
                    stats.ref_op = true;
                    GenType::Ref(Box::new(types::scalar_type(rng)))
                } else {
                    iface_type(rng, stats)
                }
            })
            .collect();
        let ret = match (i, &cross) {
            (0, Some((_, _, rty))) => rty.clone(),
            _ => iface_type(rng, stats),
        };
        let names: Vec<String> = (0..arity).map(|_| inner.fresh()).collect();
        let m = inner.mark();
        for (n, t) in names.iter().zip(params.iter()) {
            inner.push(n.clone(), t.clone());
        }
        let mut body = exprs::gen_typed(&inner, rng, &ret, 2);
        if i == 0
            && let Some((callee, ptys, _)) = &cross
        {
            let args: Vec<String> =
                ptys.iter().map(|t| exprs::gen_typed(&inner, rng, t, 1)).collect();
            let call = format!("{callee}({})", args.join(", "));
            // merge with the organic body where `+` is defined
            body = if ret.is_numeric() { format!("({call} + ({body}))") } else { call };
        }
        if i == 0
            && let Some((h, ht)) = &helper
            && *ht == ret
        {
            let arg = exprs::gen_typed(&inner, rng, ht, 1);
            body = format!("{h}({arg}) + ({body})");
        }
        inner.truncate(m);
        let sig: Vec<String> = names
            .iter()
            .zip(params.iter())
            .map(|(n, t)| format!("{n}: {}", t.render()))
            .collect();
        // Unannotated return: the impl's return type is inferred and
        // checked against the interface. Only where inference reproduces
        // the type exactly: the signature must match, it never narrows.
        if has_gxi && ret.infers_exact() && chance(rng, 0.3) {
            stats.unannotated_ret = true;
            gx.push_str(&format!("let {fname} = |{}| {body};\n", sig.join(", ")));
        } else {
            gx.push_str(&format!(
                "let {fname} = |{}| -> {} {body};\n",
                sig.join(", "),
                ret.render()
            ));
        }
        let fty = GenType::Fn { params: params.clone(), ret: Box::new(ret.clone()) };
        if chance(rng, 0.15) {
            gxi.push_str("/// generated\n");
        }
        gxi.push_str(&format!("val {fname}: {};\n", fty.render()));
        inner.push(fname.clone(), fty.clone());
        public.push((fname, fty));
    }
    // Optional abstract type: `type T;` in the interface, a hidden
    // concrete def in the impl, constructor + accessor registered as main
    // vocabulary over `GenType::Abstract`, so T values flow first-class.
    // In the bare-module variant `m<i>::T` is a public newtype.
    let mut stmts = Vec::new();
    if chance(rng, cfg.p_abstract) {
        let concrete = match rng.below(3) {
            0 => I64,
            1 => GenType::Tuple(vec![I64, I64]),
            _ => GenType::Array(Box::new(I64)),
        };
        // the nominal faces: the constructor `T(..)`, the payload `.0`,
        // the pattern `T(p)`
        let (mk_body, un_body) = match &concrete {
            GenType::Num(NumTy::I64) => ("T(x)".to_string(), "t.0".to_string()),
            GenType::Tuple(_) => {
                ("T((x, x + i64:1))".to_string(), "{ let T((a, _)) = t; a }".to_string())
            }
            _ => ("T([x, x])".to_string(), "t.0[0]$".to_string()),
        };
        gxi.push_str("type T;\nval mk: fn(x: i64) -> T;\nval un: fn(t: T) -> i64;\n");
        gx.push_str(&format!(
            "type T = Abstract<{}>;\nlet mk = |x: i64| -> T {mk_body};\nlet un = |t: T| -> i64 {un_body};\n",
            concrete.render()
        ));
        let aty = GenType::Abstract { module: mname.clone() };
        // A trait over T: declared in the interface (with a default the
        // impl may override), implemented for T, and a bounded generic
        // `via`. Both enter the main vocabulary as T -> i64 fns.
        if chance(rng, 0.6) {
            let override_default = chance(rng, 0.5);
            let decl = "trait Tr { val tv: fn(self) -> i64; val tw: fn(self) -> i64 = |s| tv(s) + i64:1 };\n";
            let (sig_impl, body_impl) = if override_default {
                (
                    "impl Tr for T;\nval via: fn<'a: Tr>(x: 'a) -> i64;\n",
                    format!(
                        "impl Tr for T {{ let tv = |t| {un_body}; let tw = |t| {un_body} * i64:2 }};\nlet via = 'a: Tr |x: 'a| Tr::tw(x);\n"
                    ),
                )
            } else {
                (
                    "impl Tr for T;\nval via: fn<'a: Tr>(x: 'a) -> i64;\n",
                    format!(
                        "impl Tr for T {{ let tv = |t| {un_body} }};\nlet via = 'a: Tr |x: 'a| Tr::tw(x);\n"
                    ),
                )
            };
            if has_gxi {
                gxi.push_str(decl);
                gxi.push_str(sig_impl);
            } else {
                gx.push_str(decl);
                gx.push_str(
                    "let via: fn<'a: Tr>(x: 'a) -> i64 = 'a: Tr |x: 'a| Tr::tw(x);\n",
                );
            }
            if has_gxi {
                gx.push_str(&body_impl);
            } else {
                gx.push_str(
                    &body_impl.replace("let via = 'a: Tr |x: 'a| Tr::tw(x);\n", ""),
                );
            }
            ctx.push(
                format!("{mname}::Tr::tv"),
                GenType::Fn { params: vec![aty.clone()], ret: Box::new(I64) },
            );
            ctx.push(
                format!("{mname}::via"),
                GenType::Fn { params: vec![aty.clone()], ret: Box::new(I64) },
            );
            stats.trait_call = true;
            // union dispatch: a second abstract implementing the same
            // trait, and a fn whose self is the union of the two
            if chance(rng, 0.5) {
                let decl = "type T2;\nval mk2: fn(x: i64) -> T2;\nimpl Tr for T2;\nval both: fn(v: [T, T2]) -> i64;\n";
                let body = "type T2 = Abstract<i64>;\nlet mk2 = |x: i64| -> T2 T2(x);\nimpl Tr for T2 { let tv = |t| t.0 };\nlet both = |v: [T, T2]| -> i64 Tr::tv(v);\n";
                if has_gxi {
                    gxi.push_str(decl);
                }
                gx.push_str(body);
                // `both` is callable with either member; register at T and
                // seed one T2-routed call as a statement
                ctx.push(
                    format!("{mname}::both"),
                    GenType::Fn { params: vec![aty.clone()], ret: Box::new(I64) },
                );
                let arg = exprs::gen_typed(ctx, rng, &I64, 1);
                let w = ctx.fresh();
                stmts.push(format!("let {w} = {mname}::both({mname}::mk2({arg}))"));
                ctx.push(w, I64);
                stats.trait_union = true;
            }
            // a trait-bounded HOF over Array<'a: Tr>
            if chance(rng, 0.5) {
                let decl = "val tsum: fn<'a: Tr>(xs: Array<'a>) -> i64;\n";
                let body = "let tsum = 'a: Tr |xs: Array<'a>| -> i64 array::fold(xs, i64:0, |acc, x| acc + Tr::tv(x));\n";
                if has_gxi {
                    gxi.push_str(decl);
                    gx.push_str(body);
                } else {
                    gx.push_str(
                        "let tsum: fn<'a: Tr>(xs: Array<'a>) -> i64 = 'a: Tr |xs: Array<'a>| -> i64 array::fold(xs, i64:0, |acc, x| acc + Tr::tv(x));\n",
                    );
                }
                ctx.push(
                    format!("{mname}::tsum"),
                    GenType::Fn {
                        params: vec![GenType::Array(Box::new(aty.clone()))],
                        ret: Box::new(I64),
                    },
                );
                stats.bounded_hof = true;
            }
        }
        // The core traits: an `Eq` whose answer differs from the
        // structural one (payload parity), an `Ord` to match, and a
        // `Display` with its own spelling, reached bare, in arrays, in
        // maps and in interpolation.
        if chance(rng, 0.5) {
            let decls = "impl Eq for T;\nimpl Ord for T;\nimpl Display for T;\nval teq: fn(a: T, b: T) -> bool;\nval teqa: fn(a: T, b: T) -> bool;\nval tshow: fn(t: T) -> string;\nval tmap: fn(a: T, b: T) -> i64;\n";
            let impls = "impl Eq for T { let eq = |a, b| un(a) % i64:2 == un(b) % i64:2 };\nimpl Ord for T { let cmp = |a, b| select (un(a) % i64:2, un(b) % i64:2) { (x, y) if x < y => `Less, (x, y) if x > y => `Greater, _ => `Equal } };\nimpl Display for T { let fmt = |t| \"T<[un(t)]>\" };\n";
            let fns = "let teq = |a: T, b: T| -> bool a == b && (a <= b || a > b);\nlet teqa = |a: T, b: T| -> bool [a, a] == [b, a];\nlet tshow = |t: T| -> string \"[t]|[(t, i64:1)]\";\nlet tmap = |a: T, b: T| -> i64 { let m = {a => i64:1, b => i64:2}; map::len(m) + map::len(map::insert(m, a, i64:3)) };\n";
            if has_gxi {
                gxi.push_str(decls);
            }
            gx.push_str(impls);
            gx.push_str(fns);
            let f2 = GenType::Fn {
                params: vec![aty.clone(), aty.clone()],
                ret: Box::new(GenType::Bool),
            };
            ctx.push(format!("{mname}::teq"), f2.clone());
            ctx.push(format!("{mname}::teqa"), f2);
            ctx.push(
                format!("{mname}::tmap"),
                GenType::Fn {
                    params: vec![aty.clone(), aty.clone()],
                    ret: Box::new(I64),
                },
            );
            ctx.push(
                format!("{mname}::tshow"),
                GenType::Fn { params: vec![aty.clone()], ret: Box::new(GenType::Str) },
            );
            stats.core_trait = true;
        }
        // a Collection-generic fn (`|c: Collection|`), registered at
        // several constructor types so call sites dispatch across them
        if chance(rng, 0.5) {
            let decl = "val csize: fn(c: Collection) -> i64;\n";
            let body = "let csize = |c: Collection| Collection::fold(c, i64:0, |acc, x| acc + i64:1);\n";
            if has_gxi {
                gxi.push_str(decl);
            }
            gx.push_str(body);
            for pty in [
                GenType::Array(Box::new(I64)),
                GenType::Map(Box::new(I64)),
                GenType::Array(Box::new(GenType::Str)),
            ] {
                ctx.push(
                    format!("{mname}::csize"),
                    GenType::Fn { params: vec![pty], ret: Box::new(I64) },
                );
            }
            stats.collection_generic = true;
        }
        ctx.push(
            format!("{mname}::mk"),
            GenType::Fn { params: vec![I64], ret: Box::new(aty.clone()) },
        );
        ctx.push(
            format!("{mname}::un"),
            GenType::Fn { params: vec![aty.clone()], ret: Box::new(I64) },
        );
        // seed one T-typed binding; production and consumption are
        // organic vocabulary from here
        let arg = exprs::gen_typed(ctx, rng, &I64, 1);
        let v = ctx.fresh();
        stmts.push(format!("let {v} = {mname}::mk({arg})"));
        ctx.push(v, aty.clone());
        // sometimes store T in a composite so accessors read it back out
        if chance(rng, 0.5) {
            let t_expr = exprs::gen_typed(ctx, rng, &aty, 1);
            let n_expr = exprs::gen_typed(ctx, rng, &I64, 1);
            let v2 = ctx.fresh();
            stmts.push(format!("let {v2} = ({t_expr}, {n_expr})"));
            ctx.push(v2, GenType::Tuple(vec![aty, I64]));
        }
        stats.abstract_value = true;
    }
    // textual, but exact: `m<j>::` for an earlier j can only appear
    // through a vocabulary reference
    if (0..idx).any(|j| gx.contains(&format!("m{j}::"))) {
        stats.cross_module_call = true;
    }
    // Register the public lambdas in main under their absolute paths,
    // road prefixes stripped.
    let plain = |fty: &GenType| {
        let mut fty = fty.clone();
        map_abstract(&mut fty, &|module| {
            for p in ["super::", "package::"] {
                if let Some(r) = module.strip_prefix(p) {
                    *module = r.to_string();
                }
            }
        });
        fty
    };
    for (fname, fty) in &public {
        ctx.push(format!("{mname}::{fname}"), plain(fty));
    }
    // main-scope import vocabulary: a plain item import, a rename (`f0`
    // collides across modules), and for the first module a glob
    if chance(rng, 0.4) {
        stats.use_vocab = true;
        match rng.below(3) {
            0 if konst.is_some() => {
                let (k, kty) = konst.clone().unwrap();
                stmts.push(format!("use {mname}::{k}"));
                ctx.push(k, kty);
            }
            1 if idx == 0 => {
                stmts.push(format!("use {mname}::*"));
                for (fname, fty) in &public {
                    ctx.push(fname.clone(), plain(fty));
                }
                if let Some((k, kty)) = &konst {
                    ctx.push(k.clone(), kty.clone());
                }
            }
            _ => {
                let (fname, fty) = &public[rng.below(public.len())];
                let alias = ctx.fresh();
                stmts.push(format!("use {mname}::{fname} as {alias}"));
                ctx.push(alias, plain(fty));
            }
        }
    }
    // a trailing expression statement
    if chance(rng, 0.15) {
        let t = types::scalar_type(rng);
        let e = exprs::gen_typed(&inner, rng, &t, 1);
        gx.push_str(&format!("{e}\n"));
    }
    let mut files = vec![(format!("{mname}.gx"), gx.trim_end().to_string())];
    if has_gxi {
        files.insert(0, (format!("{mname}.gxi"), gxi.trim_end().to_string()));
    }
    GenModule { files, stmts }
}

/// Emit a dynamic module: a raw-string-literal source compiled at
/// runtime against a declared sig inside a sandbox, consumed through
/// the status gate. Every outcome (load, sig mismatch, sandbox
/// violation, syntax error) is deterministic at Exact strength, so
/// negative variants exercise the error arm as first-class outcomes.
pub(super) fn gen_dynamic_module(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    _cfg: &GenCfg,
    stats: &mut GenStats,
    idx: usize,
) -> Vec<String> {
    stats.dynamic_module = true;
    let dname = format!("d{idx}");
    let mut inner = ctx.clone();
    inner.truncate(0);
    let mut src = String::new();
    // optional hidden binding, used through capture, never in the sig
    if chance(rng, 0.4) {
        let h = inner.fresh();
        let v = exprs::gen_typed(&inner, rng, &I64, 1);
        src.push_str(&format!("let {h} = {v};\n"));
        inner.push(h, I64);
    }
    let nfns = 1 + rng.below(2);
    let mut sig = String::new();
    let mut public: Vec<(String, GenType)> = Vec::new();
    for i in 0..nfns {
        let fname = format!("g{i}");
        let arity = 1 + rng.below(2);
        let params: Vec<GenType> = (0..arity).map(|_| types::scalar_type(rng)).collect();
        let ret = types::scalar_type(rng);
        let names: Vec<String> = (0..arity).map(|_| inner.fresh()).collect();
        let m = inner.mark();
        for (n, t) in names.iter().zip(params.iter()) {
            inner.push(n.clone(), t.clone());
        }
        let body = exprs::gen_typed(&inner, rng, &ret, 2);
        inner.truncate(m);
        let sigp: Vec<String> = names
            .iter()
            .zip(params.iter())
            .map(|(n, t)| format!("{n}: {}", t.render()))
            .collect();
        src.push_str(&format!(
            "let {fname} = |{}| -> {} {body};\n",
            sigp.join(", "),
            ret.render()
        ));
        let fty = GenType::Fn { params, ret: Box::new(ret) };
        sig.push_str(&format!("val {fname}: {}; ", fty.render()));
        public.push((fname, fty));
    }
    // negative variant: the sig demands a val the source never defines
    let sig_mismatch = chance(rng, 0.12);
    if sig_mismatch {
        sig.push_str("val absent: fn(x: i64) -> i64; ");
    }
    // a narrow sandbox turns array::/str:: reaches into deterministic
    // error-arm outcomes, so both whitelists are healthy to emit
    let whitelist = if chance(rng, 0.25) { "[core]" } else { "[core, array, str]" };
    // Counted-hash raw strings are verbatim (a generated body can contain
    // both `\\` and `\[`); one hash suffices unless the source contains `"#`.
    let src = src.trim_end().trim_end_matches(';');
    let hashes = if src.contains("\"#") { "##" } else { "#" };
    let status = ctx.fresh();
    let mut stmts = vec![format!(
        "let {status} = mod {dname} dynamic {{ sandbox whitelist {whitelist}; \
         sig {{ {} }}; source r{hashes}\"{src}\"{hashes} }}",
        sig.trim_end().trim_end_matches(';')
    )];
    // consume one public fn through the status gate
    let (fname, fty) = &public[rng.below(public.len())];
    let GenType::Fn { params, ret } = fty else { unreachable!() };
    let args: Vec<String> =
        params.iter().map(|t| exprs::gen_typed(ctx, rng, t, 1)).collect();
    let fallback = exprs::gen_typed(ctx, rng, ret, 1);
    let v = ctx.fresh();
    stmts.push(format!(
        "let {v} = select {status} {{ error as _ => {fallback}, null as _ => \
         {dname}::{fname}({}) }}",
        args.join(", ")
    ));
    ctx.push(v, (**ret).clone());
    stmts
}
