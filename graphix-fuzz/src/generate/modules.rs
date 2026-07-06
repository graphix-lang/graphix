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
    types::{self, GenType},
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
    // The module's own scope: params and locals only. Names come from
    // the SHARED collision pool (a module-private binding aliasing a
    // main-body name is exactly the name-vs-identity surface).
    let mut inner = ctx.clone();
    inner.truncate(0);
    let mut gx = String::new();
    let mut gxi = String::new();
    let mut public: Vec<(String, GenType)> = Vec::new();
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
    // 1-3 public typed lambdas over scalars.
    let nfns = 1 + rng.below(3);
    for i in 0..nfns {
        let fname = format!("f{i}");
        let arity = 1 + rng.below(2);
        let params: Vec<GenType> = (0..arity).map(|_| types::scalar_type(rng)).collect();
        let ret = types::scalar_type(rng);
        let names: Vec<String> = (0..arity).map(|_| inner.fresh()).collect();
        let m = inner.mark();
        for (n, t) in names.iter().zip(params.iter()) {
            inner.push(n.clone(), t.clone());
        }
        // Earlier PUBLIC module fns are callable from later bodies
        // (registered plain-named in the module scope).
        let mut body = exprs::gen_typed(&inner, rng, &ret, 2);
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
        gx.push_str(&format!(
            "let {fname} = |{}| -> {} {body};\n",
            sig.join(", "),
            ret.render()
        ));
        let fty = GenType::Fn { params: params.clone(), ret: Box::new(ret.clone()) };
        if chance(rng, 0.15) {
            gxi.push_str("/// generated\n");
        }
        gxi.push_str(&format!("val {fname}: {};\n", fty.render()));
        inner.push(fname.clone(), fty.clone());
        public.push((fname, fty));
    }
    // Optional abstract type round-trip: `type T;` in the interface, a
    // hidden concrete def in the impl, constructor + accessor — the
    // abstract-registry surface fusion resolves through
    // `resolve_abstract`/`freeze_for_abi`.
    let mut stmts = Vec::new();
    if chance(rng, cfg.p_abstract) {
        let concrete = match rng.below(3) {
            0 => GenType::I64,
            1 => GenType::Tuple(vec![GenType::I64, GenType::I64]),
            _ => GenType::Array(Box::new(GenType::I64)),
        };
        let (mk_body, un_body) = match &concrete {
            GenType::I64 => ("x".to_string(), "t".to_string()),
            GenType::Tuple(_) => ("(x, x + i64:1)".to_string(), "t.0".to_string()),
            _ => ("[x, x]".to_string(), "t[0]$".to_string()),
        };
        gxi.push_str("type T;\nval mk: fn(x: i64) -> T;\nval un: fn(t: T) -> i64;\n");
        gx.push_str(&format!(
            "type T = {};\nlet mk = |x: i64| -> T {mk_body};\nlet un = |t: T| -> i64 {un_body};\n",
            concrete.render()
        ));
        let arg = exprs::gen_typed(ctx, rng, &GenType::I64, 1);
        let v = ctx.fresh();
        stmts.push(format!("let {v} = {mname}::un({mname}::mk({arg}))"));
        ctx.push(v, GenType::I64);
    }
    // Register the public lambdas in MAIN under their absolute paths.
    // With the bare-module variant (no .gxi) everything is public —
    // same registrations, no interface to constrain the types.
    for (fname, fty) in public {
        ctx.push(format!("{mname}::{fname}"), fty);
    }
    let mut files = vec![(format!("{mname}.gx"), gx.trim_end().to_string())];
    if !chance(rng, cfg.p_bare_module) {
        files.insert(0, (format!("{mname}.gxi"), gxi.trim_end().to_string()));
    }
    GenModule { files, stmts }
}
