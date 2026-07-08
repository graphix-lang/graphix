//! Module emission — submodules with `.gxi` interfaces, the bread and
//! butter of larger programs and a surface generation never touched:
//! interface types CONSTRAIN impl inference (the instantiation path),
//! abstract types flow through fusion's abstract registry, and
//! visibility gates which bindings the main body may reach.
//!
//! A generated module is a pair of wrapper file sections (`m{i}.gx` +
//! usually `m{i}.gxi`) plus registrations into the MAIN scope: public
//! lambdas enter the callable vocabulary under their absolute path
//! (`m0::f1`), so `try_call` composes module calls into arbitrary
//! expressions for free. The module's own scope is self-contained (no
//! captures of main bindings — a root-mounted module can't see them).

use super::{
    GenCfg, GenCtx, GenStats, chance, exprs,
    types::{self, GenType, I64, NumTy},
};
use crate::mutate::Rng;

pub(super) struct GenModule {
    /// Wrapper file sections: `(name, text)` — `m{i}.gx` and, unless
    /// the bare-module variant fired, `m{i}.gxi`.
    pub files: Vec<(String, String)>,
    /// Statements for the MAIN body (abstract-type round-trips bind a
    /// value the main vocabulary can use).
    pub stmts: Vec<String>,
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
    // The module's own scope: params, locals, and EARLIER modules'
    // path-qualified publics — `mod m0;` is declared before `mod m1;`
    // in the compile text, so m1's body calling `m0::f` resolves
    // (probed both directions 2026-07-08: forward works, backward is a
    // compile error). Main-body bindings are dropped (a root-mounted
    // module can't see them); names still come from the SHARED
    // collision pool (a module-private binding aliasing a main-body
    // name is exactly the name-vs-identity surface).
    let mut inner = ctx.clone();
    inner.vars.retain(|(n, _)| n.contains("::"));
    // Decided up front: unannotated-return impls are legal only when
    // the interface states the full type (a bare module registers the
    // vocabulary from the declared type, which a body's inference may
    // not reproduce exactly).
    let has_gxi = !chance(rng, cfg.p_bare_module);
    let mut gx = String::new();
    let mut gxi = String::new();
    let mut public: Vec<(String, GenType)> = Vec::new();
    // Optional exported CONSTANT — a non-fn `val`. Emitted FIRST so
    // later fn bodies can reference it organically; registered in MAIN
    // under its path. A cross-region read of exactly this shape found
    // the dead-eliminated-module deadlock (2026-07-08).
    if chance(rng, 0.5) {
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
        ctx.push(format!("{mname}::{k}"), kty);
        stats.iface_const = true;
    }
    // Optional private helper: used by the first public lambda, absent
    // from the interface — the visibility surface.
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
    // An interface-boundary type: scalars keep the majority, but
    // composites (tuple/struct/array/map/nullable) and the occasional
    // tag union cross too — a marshalled composite param/return is the
    // instantiation + ABI surface a scalar-only interface never touched.
    // Bare variant literals and `null` coerce at the annotated call
    // site exactly like the annotated-let flow (probed 2026-07-08).
    let mut iface_type = |rng: &mut Rng, stats: &mut GenStats| match rng.below(10) {
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
    // Cross-module wiring (later modules): pick an EARLIER module's
    // public fn, pin f0's return type to the callee's, and splice the
    // call into f0's body. Organic draws alone leave the shape under
    // the presence gate (~0.6% — it needs ≥2 modules in one program
    // AND `try_call` landing on the import).
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
    // 1-3 public typed lambdas.
    let nfns = 1 + rng.below(3);
    for i in 0..nfns {
        let fname = format!("f{i}");
        let arity = 1 + rng.below(2);
        let params: Vec<GenType> =
            (0..arity).map(|_| iface_type(rng, stats)).collect();
        let ret = match (i, &cross) {
            (0, Some((_, _, rty))) => rty.clone(),
            _ => iface_type(rng, stats),
        };
        let names: Vec<String> = (0..arity).map(|_| inner.fresh()).collect();
        let m = inner.mark();
        for (n, t) in names.iter().zip(params.iter()) {
            inner.push(n.clone(), t.clone());
        }
        // Earlier PUBLIC module fns are callable from later bodies
        // (registered plain-named in the module scope).
        let mut body = exprs::gen_typed(&inner, rng, &ret, 2);
        if i == 0
            && let Some((callee, ptys, _)) = &cross
        {
            let args: Vec<String> =
                ptys.iter().map(|t| exprs::gen_typed(&inner, rng, t, 1)).collect();
            let call = format!("{callee}({})", args.join(", "));
            // Merge with the organic body where `+` is defined;
            // otherwise the call IS the body.
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
        // Unannotated RETURN: the impl's return type is inferred from
        // the body and checked against the interface's declared type —
        // the signature-match seam annotated impls never exercise.
        // Only where inference reproduces the type EXACTLY (the
        // signature must MATCH the inferred type, it never narrows it:
        // a bare tag infers its single tag, not the union — probed
        // 2026-07-08, "signature mismatch" is a hard error).
        if has_gxi && ret.infers_exact() && chance(rng, 0.3) {
            stats.unannotated_ret = true;
            gx.push_str(&format!(
                "let {fname} = |{}| {body};\n",
                sig.join(", ")
            ));
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
    // concrete def in the impl, constructor + accessor — the
    // abstract-registry surface fusion resolves through
    // `resolve_abstract`/`freeze_for_abi`. `mk`/`un` register as MAIN
    // vocabulary typed over `GenType::Abstract`, so T values flow
    // FIRST-CLASS from here: through lets, tuples/arrays, select
    // binds, later modules' bodies, and (via cross-module wiring
    // pinning a later f0's return to `mk`'s) other interfaces —
    // `m1.gxi` declaring `-> m0::T` resolves (probed 2026-07-08). In
    // the bare-module variant the same paths hold with `m<i>::T`
    // resolving as a transparent alias.
    let mut stmts = Vec::new();
    if chance(rng, cfg.p_abstract) {
        let concrete = match rng.below(3) {
            0 => I64,
            1 => GenType::Tuple(vec![I64, I64]),
            _ => GenType::Array(Box::new(I64)),
        };
        let (mk_body, un_body) = match &concrete {
            GenType::Num(NumTy::I64) => ("x".to_string(), "t".to_string()),
            GenType::Tuple(_) => ("(x, x + i64:1)".to_string(), "t.0".to_string()),
            _ => ("[x, x]".to_string(), "t[0]$".to_string()),
        };
        gxi.push_str("type T;\nval mk: fn(x: i64) -> T;\nval un: fn(t: T) -> i64;\n");
        gx.push_str(&format!(
            "type T = {};\nlet mk = |x: i64| -> T {mk_body};\nlet un = |t: T| -> i64 {un_body};\n",
            concrete.render()
        ));
        let aty = GenType::Abstract { module: mname.clone() };
        ctx.push(
            format!("{mname}::mk"),
            GenType::Fn { params: vec![I64], ret: Box::new(aty.clone()) },
        );
        ctx.push(
            format!("{mname}::un"),
            GenType::Fn { params: vec![aty.clone()], ret: Box::new(I64) },
        );
        // Seed one T-typed binding; production and consumption are
        // organic vocabulary from here (`un` via try_call at i64
        // positions, `mk` via the Abstract arm of gen_typed).
        let arg = exprs::gen_typed(ctx, rng, &I64, 1);
        let v = ctx.fresh();
        stmts.push(format!("let {v} = {mname}::mk({arg})"));
        ctx.push(v, aty.clone());
        // Sometimes store T in a composite so accessors read it back
        // out (the opaque-value-in-composite marshal surface).
        if chance(rng, 0.5) {
            let t_expr = exprs::gen_typed(ctx, rng, &aty, 1);
            let n_expr = exprs::gen_typed(ctx, rng, &I64, 1);
            let v2 = ctx.fresh();
            stmts.push(format!("let {v2} = ({t_expr}, {n_expr})"));
            ctx.push(v2, GenType::Tuple(vec![aty, I64]));
        }
        stats.abstract_value = true;
    }
    // Cross-module call presence — textual, but exact: `m<j>::` for an
    // earlier j can only appear in this module's body through a
    // vocabulary reference.
    if (0..idx).any(|j| gx.contains(&format!("m{j}::"))) {
        stats.cross_module_call = true;
    }
    // Register the public lambdas in MAIN under their absolute paths.
    // With the bare-module variant (no .gxi) everything is public —
    // same registrations, no interface to constrain the types.
    for (fname, fty) in public {
        ctx.push(format!("{mname}::{fname}"), fty);
    }
    let mut files = vec![(format!("{mname}.gx"), gx.trim_end().to_string())];
    if has_gxi {
        files.insert(0, (format!("{mname}.gxi"), gxi.trim_end().to_string()));
    }
    GenModule { files, stmts }
}

/// Emit a DYNAMIC module: a raw-string-literal source compiled at
/// runtime against a declared sig inside a sandbox, consumed through
/// the status gate (`select status { error => fallback, null =>
/// d0::f(...) }` — the hand-seed discipline). Every outcome is
/// deterministic at EXACT oracle strength — load, sig mismatch,
/// sandbox violation, and source syntax errors all settle the same
/// way in both modes (probed 2026-07-07) — so no netidx plumbing and
/// no tier demotion is needed. Negative variants (a sig val the
/// source doesn't define; a core-only sandbox under a vocabulary that
/// may reach array::/str::) exercise the error arm as a first-class
/// deterministic outcome.
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
    // Optional hidden binding, used through capture — never in the sig.
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
    // Negative variant: the sig demands a val the source never defines
    // — the load must take the error arm, identically in both modes.
    let sig_mismatch = chance(rng, 0.12);
    if sig_mismatch {
        sig.push_str("val absent: fn(x: i64) -> i64; ");
    }
    // The generated bodies may reach array::/str:: through gen_typed's
    // vocabulary; a narrow sandbox turns those loads into deterministic
    // error-arm outcomes rather than invalid programs, so both
    // whitelists are healthy to emit.
    let whitelist = if chance(rng, 0.25) { "[core]" } else { "[core, array, str]" };
    // Raw-string-escape the source: `\\` and `\'` are the only escapes
    // a raw string recognizes, and a generated body can contain BOTH
    // characters (the escaped-bracket string-interp shape emits `\[`,
    // which an unescaped raw string rejects as a parse error).
    // Backslashes first, so the quote escape's own backslash isn't
    // doubled.
    let src =
        src.trim_end().trim_end_matches(';').replace('\\', "\\\\").replace('\'', "\\'");
    let status = ctx.fresh();
    let mut stmts = vec![format!(
        "let {status} = mod {dname} dynamic {{ sandbox whitelist {whitelist}; \
         sig {{ {} }}; source r'{src}' }}",
        sig.trim_end().trim_end_matches(';')
    )];
    // Consume ONE public fn through the status gate; the result enters
    // the main vocabulary.
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
