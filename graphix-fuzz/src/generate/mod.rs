//! Type-directed program generation from scratch. `gen_typed(ctx, ty,
//! …)` emits a graphix expression of type `ty` from in-scope bindings
//! and literals, so every program typechecks by construction. Programs
//! are emitted as text. Bindings deliberately rebind visible names and
//! collide with names from other scopes; only pure, deterministic
//! constructs are generated so the oracle's comparison is sound.

mod exprs;
mod funcs;
mod modules;
mod patterns;
pub mod reactive;
pub mod twin;
mod types;

pub use types::GenType;

use crate::mutate::Rng;

/// Feature probabilities and limits for one generation profile. All
/// randomness flows through the seeded [`Rng`], so a given (cfg, seed)
/// pair always produces the same program text.
#[derive(Debug, Clone)]
pub struct GenCfg {
    /// A new binding reuses a VISIBLE name — a rebind/shadow.
    pub p_shadow: f64,
    /// A new binding reuses a name from the collision pool (any name
    /// ever used for a binding in the program, visible or not) —
    /// creates same-name locals across unrelated scopes.
    pub p_collision: f64,
    /// A let carries an explicit type annotation.
    pub p_annotate: f64,
    /// A statement slot emits a lambda binding instead of a value let.
    pub p_lambda: f64,
    /// A lambda is poly (explicit `'a: Number` constraint form) rather
    /// than fully typed.
    pub p_poly: f64,
    /// A statement slot emits the bare-unannotated-lambda template (wide
    /// shared tvar, two unannotated call sites).
    pub p_bare: f64,
    /// A poly lambda is immediately called at two distinct numeric types.
    pub p_mono_pair: f64,
    /// A typed lambda's body is a block with a collision-prone local.
    pub p_body_block: f64,
    /// A statement slot emits a terminating `let rec` + call.
    pub p_rec: f64,
    /// A statement slot emits the whole shadowed-lambda-name template.
    pub p_lambda_shadow_template: f64,
    /// A value let binds a TAG-UNION variant (always annotated — a
    /// bare variant literal's type is its single tag).
    pub p_variant: f64,
    /// A statement slot emits the error-arm-lambda template (a select
    /// merging an ok arm with `error(...)` as a lambda's return) plus a
    /// consumed call.
    pub p_error_lambda: f64,
    /// A statement slot emits a `catch(e) <handler>` installation
    /// covering the rest of the block.
    pub p_catch: f64,
    /// A statement slot emits a module: wrapper file sections whose
    /// public lambdas enter the callable vocabulary as `m{i}::f`.
    pub p_module: f64,
    /// A generated module carries an abstract-type round-trip
    /// (`type T;` in the interface, hidden concrete def in the impl).
    pub p_abstract: f64,
    /// A generated module omits its `.gxi` entirely (bare module —
    /// everything public).
    pub p_bare_module: f64,
    /// A statement slot emits a dynamic module (raw-string source
    /// compiled at runtime against a sig, consumed through the status gate).
    pub p_dynmod: f64,
    /// A statement slot emits a REFERENCE group (`let r = &v`, tuple
    /// storage, `*r <- lit` write-through).
    pub p_ref: f64,
    /// Statement slots per program: 0..=max_lets (template slots may
    /// emit several statements).
    pub max_lets: usize,
    /// Depth passed to `random_type` for value-let and tail types.
    pub type_depth: usize,
    /// A statement slot embeds a whole generated subprogram as a typed
    /// block value (`let v: T = { …; tail: T }`); 50/50 the inner block
    /// shares the outer scope vs generating closed.
    pub p_subprogram: f64,
    /// Remaining nesting budget for subprogram slots (decremented per
    /// level; 0 disables the arm).
    pub subprogram_depth: usize,
}

impl Default for GenCfg {
    fn default() -> Self {
        GenCfg {
            p_shadow: 0.25,
            p_collision: 0.15,
            p_annotate: 0.3,
            p_lambda: 0.25,
            p_poly: 0.4,
            p_bare: 0.05,
            p_mono_pair: 0.5,
            p_body_block: 0.4,
            p_rec: 0.06,
            p_lambda_shadow_template: 0.05,
            p_variant: 0.08,
            p_error_lambda: 0.06,
            p_catch: 0.07,
            p_module: 0.12,
            p_abstract: 0.4,
            p_bare_module: 0.2,
            p_dynmod: 0.08,
            p_ref: 0.1,
            max_lets: 6,
            type_depth: 2,
            p_subprogram: 0.10,
            subprogram_depth: 2,
        }
    }
}

/// The big profile: 4x the statement slots, more modules per program,
/// one level deeper types.
pub fn big_cfg() -> GenCfg {
    GenCfg { max_lets: 24, p_module: 0.2, type_depth: 3, ..GenCfg::default() }
}

/// Which bug-class shapes one generated program contains; the
/// shape-presence gate asserts each stays reachable at a healthy rate.
#[derive(Debug, Default, Clone, Copy)]
pub struct GenStats {
    /// Subprogram (nested typed block) slots emitted.
    pub subprograms: usize,
    /// A lambda name was rebound (organically or via the template).
    pub lambda_rebind: bool,
    /// A poly lambda got call sites at two distinct numeric types.
    pub mono_pair: bool,
    /// A lambda-body local reused a name bound elsewhere in the program.
    pub collision_local: bool,
    /// A `let rec` was emitted.
    pub rec: bool,
    /// The error-arm-lambda template was emitted.
    pub error_lambda: bool,
    /// A module (wrapper file sections) was emitted.
    pub module: bool,
    /// A dynamic module was emitted.
    pub dynamic_module: bool,
    /// A module interface carried a composite (non-scalar) param or
    /// return type.
    pub composite_iface: bool,
    /// A module body called an EARLIER module's public fn.
    pub cross_module_call: bool,
    /// An abstract T entered the vocabulary as a first-class value.
    pub abstract_value: bool,
    /// A module declared a trait, implemented it for its abstract T,
    /// and exported a trait-bounded generic — both callable from MAIN.
    pub trait_call: bool,
    /// A module implements `Eq`/`Display` for its abstract type and
    /// compares/prints it.
    pub core_trait: bool,
    /// A module declared a second abstract implementing the same trait
    /// and exported a union-self fn.
    pub trait_union: bool,
    /// A trait-bounded HOF over `Array<'a: Tr>`.
    pub bounded_hof: bool,
    /// A Collection-generic fn (`|c: Collection|`) registered at
    /// several constructor types.
    pub collection_generic: bool,
    /// A module exported a non-fn `val` constant.
    pub iface_const: bool,
    /// A module fn impl carried NO return annotation (interface-checked
    /// inference).
    pub unannotated_ret: bool,
    /// A reference shape was emitted (`&`/`*`/ref param/`*r <-`).
    pub ref_op: bool,
    /// A `use super::{…}` header, an inline `super::`/`package::`
    /// spelling, a main-scope item import, a rename, or a module glob.
    pub use_vocab: bool,
}

pub(crate) fn chance(rng: &mut Rng, p: f64) -> bool {
    (rng.below(1000) as f64) < p * 1000.0
}

#[derive(Clone)]
pub(crate) struct GenCtx {
    /// In-scope bindings in declaration order. Lookups scan in reverse
    /// and take the first hit per name, so a name rebound at a different
    /// type never produces a reference at the dead earlier type.
    vars: Vec<(String, GenType)>,
    /// Every name ever used for a binding, visible or not: the pool
    /// targeted collisions draw from.
    collision_pool: Vec<String>,
    next: usize,
}

fn is_type_keyword(name: &str) -> bool {
    matches!(name, "v32" | "v64")
}

impl GenCtx {
    fn new() -> Self {
        GenCtx { vars: Vec::new(), collision_pool: Vec::new(), next: 0 }
    }

    fn fresh(&mut self) -> String {
        loop {
            let n = format!("v{}", self.next);
            self.next += 1;
            if !is_type_keyword(&n) {
                return n;
            }
        }
    }

    /// Choose the name for a new binding: a visible name (shadow), a
    /// collision-pool name, or a fresh one. The caller pushes the
    /// binding AFTER generating its RHS, so the RHS sees the old
    /// binding (`let x = x + 1` works and is generated on purpose).
    fn name_for_bind(&mut self, rng: &mut Rng, cfg: &GenCfg) -> String {
        if chance(rng, cfg.p_shadow) {
            let names = self.visible_names();
            if !names.is_empty() {
                return names[rng.below(names.len())].to_string();
            }
        }
        if chance(rng, cfg.p_collision) && !self.collision_pool.is_empty() {
            return self.collision_pool[rng.below(self.collision_pool.len())].clone();
        }
        self.fresh()
    }

    fn push(&mut self, name: String, ty: GenType) {
        // path-qualified module callables are reference-only vocabulary
        if !name.contains("::") && !self.collision_pool.contains(&name) {
            self.collision_pool.push(name.clone());
        }
        self.vars.push((name, ty));
    }

    /// Distinct visible names, innermost first: shadow candidates, so
    /// path-qualified module callables are excluded.
    fn visible_names(&self) -> Vec<&str> {
        let mut out: Vec<&str> = Vec::new();
        for (n, _) in self.vars.iter().rev() {
            if !n.contains("::") && !out.contains(&n.as_str()) {
                out.push(n.as_str());
            }
        }
        out
    }

    /// The type `name` currently resolves to, if bound.
    fn visible_type(&self, name: &str) -> Option<&GenType> {
        self.vars.iter().rev().find(|(n, _)| n == name).map(|(_, t)| t)
    }

    fn in_collision_pool(&self, name: &str) -> bool {
        self.collision_pool.iter().any(|n| n == name)
    }

    /// Scope bracket for lambda bodies / blocks / arms: bindings pushed
    /// after `mark()` are dropped by `truncate(mark)`. The collision
    /// pool keeps them on purpose.
    fn mark(&self) -> usize {
        self.vars.len()
    }

    fn truncate(&mut self, mark: usize) {
        self.vars.truncate(mark);
    }

    /// The visible bindings (last binding wins per name), innermost first.
    fn visible_entries(&self) -> Vec<(&str, &GenType)> {
        let mut seen: Vec<&str> = Vec::new();
        let mut out = Vec::new();
        for (n, t) in self.vars.iter().rev() {
            if seen.contains(&n.as_str()) {
                continue;
            }
            seen.push(n.as_str());
            out.push((n.as_str(), t));
        }
        out
    }

    /// Visible bindings of type `ty`.
    fn vars_of(&self, ty: &GenType) -> Vec<&str> {
        self.visible_entries()
            .into_iter()
            .filter_map(|(n, t)| (t == ty).then_some(n))
            .collect()
    }

    /// Visible typed lambdas returning `ty` (a lambda name shadowed by
    /// a value is NOT callable).
    fn fns_returning(&self, ty: &GenType) -> Vec<(&str, Vec<GenType>)> {
        self.visible_entries()
            .into_iter()
            .filter_map(|(n, t)| match t {
                GenType::Fn { params, ret } if **ret == *ty => Some((n, params.clone())),
                _ => None,
            })
            .collect()
    }

    /// Visible poly lambdas.
    fn poly_fns(&self) -> Vec<(&str, usize)> {
        self.visible_entries()
            .into_iter()
            .filter_map(|(n, t)| match t {
                GenType::PolyFn { arity } => Some((n, *arity)),
                _ => None,
            })
            .collect()
    }
}

/// Generate one complete program: ~15% draw the big profile
/// ([`big_cfg`]), the rest the default.
pub fn gen_program(rng: &mut Rng) -> String {
    let cfg = if chance(rng, 0.15) { big_cfg() } else { GenCfg::default() };
    gen_program_stats(&cfg, rng).0
}

/// Generate one complete program: a run of statement slots whose values
/// reference (and rebind) earlier bindings, plus a tail expression. Also
/// reports which bug-class shapes the program contains.
pub fn gen_program_stats(cfg: &GenCfg, rng: &mut Rng) -> (String, GenStats) {
    let mut ctx = GenCtx::new();
    let mut stats = GenStats::default();
    let mut files: Vec<(String, String)> = Vec::new();
    let stmts = gen_slots(&mut ctx, rng, cfg, &mut stats, Some(&mut files));
    let tail_ty = types::random_type(rng, cfg.type_depth);
    let tail = patterns::maybe_select(&ctx, rng, &tail_ty, 3)
        .unwrap_or_else(|| exprs::gen_typed(&ctx, rng, &tail_ty, 3));
    let prog = if stmts.is_empty() {
        tail
    } else {
        format!("{{ {}; {} }}", stmts.join("; "), tail)
    };
    let prog = if files.is_empty() { prog } else { crate::files::render(&prog, &files) };
    (prog, stats)
}

/// Geometric slot draw: P(stop) = 1/(mean+1) per step, capped, so long
/// dataflow chains appear organically without a separate profile.
pub(crate) fn geo_slots(rng: &mut Rng, mean: usize, cap: usize) -> usize {
    let mut n = 0;
    while n < cap && rng.below(mean + 1) != 0 {
        n += 1;
    }
    n
}

/// One run of statement slots. `files` present = top level (module arms
/// enabled; `mod` statements only parse at the program's top level);
/// `None` = a nested subprogram block.
fn gen_slots(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
    mut files: Option<&mut Vec<(String, String)>>,
) -> Vec<String> {
    let mut stmts = Vec::new();
    let mut nmodules = 0usize;
    let mut ndynmods = 0usize;
    let nslots = geo_slots(rng, (cfg.max_lets / 2).max(1), (cfg.max_lets * 4).min(48));
    for _ in 0..nslots {
        if files.is_some() && chance(rng, cfg.p_module) {
            let m = modules::gen_module(ctx, rng, cfg, stats, nmodules);
            nmodules += 1;
            files.as_deref_mut().unwrap().extend(m.files);
            stmts.extend(m.stmts);
        } else if files.is_some() && chance(rng, cfg.p_dynmod) {
            let n = ndynmods;
            ndynmods += 1;
            stmts.extend(modules::gen_dynamic_module(ctx, rng, cfg, stats, n));
        } else if cfg.subprogram_depth > 0 && chance(rng, cfg.p_subprogram) {
            stmts.push(gen_subprogram_stmt(ctx, rng, cfg, stats));
        } else if chance(rng, cfg.p_ref) {
            stmts.extend(funcs::gen_ref_stmts(ctx, rng, cfg, stats));
        } else if chance(rng, cfg.p_rec) {
            stmts.extend(funcs::gen_rec_lambda(ctx, rng, cfg, stats));
        } else if chance(rng, cfg.p_error_lambda) {
            stmts.extend(funcs::gen_error_arm_lambda(ctx, rng, cfg, stats));
        } else if chance(rng, cfg.p_catch) {
            let n = stmts.len();
            let acc = format!("cerr{n}");
            stmts.push(format!("let {acc}: Error<Any> = never()"));
            let handler = if rng.below(2) == 0 {
                format!("{acc} <- e")
            } else {
                format!("{{ let m = e; {acc} <- m }}")
            };
            stmts.push(format!("catch(e) {handler}"));
        } else if chance(rng, cfg.p_lambda_shadow_template) {
            stmts.extend(funcs::gen_shadowed_lambda_template(ctx, rng, cfg, stats));
        } else if chance(rng, cfg.p_bare) {
            stmts.extend(funcs::gen_bare_lambda(ctx, rng, cfg));
        } else if chance(rng, cfg.p_lambda) {
            if chance(rng, cfg.p_poly) {
                stmts.extend(funcs::gen_poly_lambda(ctx, rng, cfg, stats));
            } else {
                stmts.push(funcs::gen_typed_lambda(ctx, rng, cfg, stats));
            }
        } else {
            let variant = chance(rng, cfg.p_variant);
            let ty = if variant {
                types::random_variant(rng, 1)
            } else {
                types::random_type(rng, cfg.type_depth)
            };
            let val = patterns::maybe_select(ctx, rng, &ty, 3)
                .unwrap_or_else(|| exprs::gen_typed(ctx, rng, &ty, 3));
            let name = ctx.name_for_bind(rng, cfg);
            let must = variant || ty.contains_nullable();
            let stmt = if must || chance(rng, cfg.p_annotate) {
                format!("let {name}: {} = {val}", ty.render())
            } else {
                format!("let {name} = {val}")
            };
            stmts.push(stmt);
            ctx.push(name, ty);
        }
    }
    stmts
}

/// A subprogram slot: a nested generated block bound as a typed value,
/// either sharing the outer scope or generated closed in a fresh ctx.
/// Module arms are disabled inside and the nesting budget decrements.
fn gen_subprogram_stmt(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
) -> String {
    stats.subprograms += 1;
    let ty = types::random_type(rng, cfg.type_depth);
    let mut inner_cfg = cfg.clone();
    inner_cfg.subprogram_depth = cfg.subprogram_depth - 1;
    inner_cfg.max_lets = (cfg.max_lets / 2).max(2);
    let block = if chance(rng, 0.5) {
        let mark = ctx.mark();
        let b = gen_block(ctx, rng, &inner_cfg, stats, &ty);
        ctx.truncate(mark);
        b
    } else {
        let mut inner = GenCtx::new();
        inner.next = ctx.next;
        let b = gen_block(&mut inner, rng, &inner_cfg, stats, &ty);
        ctx.next = inner.next;
        b
    };
    let name = ctx.name_for_bind(rng, cfg);
    let stmt = format!("let {name}: {} = {block}", ty.render());
    ctx.push(name, ty);
    stmt
}

/// A nested block with a required tail type. Zero slots degenerates to a
/// bare typed expression (a block needs two or more elements).
fn gen_block(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    cfg: &GenCfg,
    stats: &mut GenStats,
    tail_ty: &GenType,
) -> String {
    let stmts = gen_slots(ctx, rng, cfg, stats, None);
    let tail = patterns::maybe_select(ctx, rng, tail_ty, 3)
        .unwrap_or_else(|| exprs::gen_typed(ctx, rng, tail_ty, 3));
    if stmts.is_empty() { tail } else { format!("{{ {}; {} }}", stmts.join("; "), tail) }
}

#[cfg(test)]
mod test {
    use super::{types::I64, *};

    /// Same seed → byte-identical program stream.
    #[test]
    fn determinism() {
        let mut a = Rng::new(0xfeed);
        let mut b = Rng::new(0xfeed);
        for _ in 0..50 {
            assert_eq!(gen_program(&mut a), gen_program(&mut b));
        }
    }

    /// The let-bound names of a generated program, in order, extracted
    /// from the text (names are always `v<digits>` or pool reuses).
    fn let_names(prog: &str) -> Vec<&str> {
        prog.split("let ")
            .skip(1)
            .filter_map(|rest| {
                let end = rest.find([' ', ':'])?;
                Some(&rest[..end])
            })
            .collect()
    }

    /// A healthy fraction of default-profile programs embed a nested
    /// typed block.
    #[test]
    fn subprogram_presence() {
        let mut rng = Rng::new(0xabcd);
        let cfg = GenCfg::default();
        let mut n = 0usize;
        for _ in 0..300 {
            let (_, stats) = gen_program_stats(&cfg, &mut rng);
            n += stats.subprograms;
        }
        assert!(n > 30, "subprogram slots over 300 programs: {n}");
    }

    /// A healthy fraction of default-profile programs contain a rebind.
    #[test]
    fn shadow_presence() {
        let mut rng = Rng::new(1);
        let mut with_rebind = 0;
        const N: usize = 300;
        for _ in 0..N {
            let p = gen_program(&mut rng);
            let names = let_names(&p);
            let mut seen: Vec<&str> = Vec::new();
            let rebind = names.iter().any(|n| {
                if seen.contains(n) {
                    true
                } else {
                    seen.push(n);
                    false
                }
            });
            if rebind {
                with_rebind += 1;
            }
        }
        assert!(
            with_rebind * 100 / N >= 15,
            "only {with_rebind}/{N} programs contain a rebind"
        );
    }

    /// Every tracked bug shape is reachable at a rate a campaign will
    /// exercise.
    #[test]
    fn audit_bug_shapes_reachable() {
        let cfg = GenCfg::default();
        let mut rng = Rng::new(3);
        let (mut rebind, mut mono, mut collision, mut rec, mut errl, mut module) =
            (0, 0, 0, 0, 0, 0);
        let (mut dynmod, mut comp_iface, mut xmod, mut abst) = (0, 0, 0, 0);
        let mut trait_call = 0;
        let mut core_trait = 0;
        let mut trait_union = 0;
        let mut bounded_hof = 0;
        let mut collection_generic = 0;
        let (mut konst, mut unret, mut refop) = (0, 0, 0);
        const N: usize = 500;
        for _ in 0..N {
            let (_, s) = gen_program_stats(&cfg, &mut rng);
            rebind += s.lambda_rebind as usize;
            mono += s.mono_pair as usize;
            collision += s.collision_local as usize;
            rec += s.rec as usize;
            errl += s.error_lambda as usize;
            module += s.module as usize;
            dynmod += s.dynamic_module as usize;
            comp_iface += s.composite_iface as usize;
            xmod += s.cross_module_call as usize;
            abst += s.abstract_value as usize;
            trait_call += s.trait_call as usize;
            core_trait += s.core_trait as usize;
            trait_union += s.trait_union as usize;
            bounded_hof += s.bounded_hof as usize;
            collection_generic += s.collection_generic as usize;
            konst += s.iface_const as usize;
            unret += s.unannotated_ret as usize;
            refop += s.ref_op as usize;
        }
        for (what, n) in [
            ("lambda rebind (bug 1)", rebind),
            ("monomorphization pair (bug 2)", mono),
            ("collision local (bug 3)", collision),
            ("let rec", rec),
            ("error-arm lambda (B5 shape)", errl),
            ("module with interface", module),
            ("dynamic module", dynmod),
            ("composite interface type", comp_iface),
            ("cross-module call", xmod),
            ("first-class abstract value", abst),
            ("trait declared+implemented+called", trait_call),
            ("core Eq/Display implemented+used", core_trait),
            ("trait union dispatch exported", trait_union),
            ("trait-bounded HOF exported", bounded_hof),
            ("Collection-generic fn exported", collection_generic),
            ("exported constant", konst),
            ("unannotated-return impl", unret),
            ("reference op", refop),
        ] {
            assert!(n * 100 >= N, "{what}: only {n}/{N} programs (<1%)");
        }
    }

    /// A name rebound at a different type is never referenced at its
    /// dead earlier type.
    #[test]
    fn vars_of_last_binding_wins() {
        let mut ctx = GenCtx::new();
        ctx.push("v0".into(), I64);
        ctx.push("v1".into(), I64);
        ctx.push("v0".into(), GenType::Bool);
        assert_eq!(ctx.vars_of(&I64), vec!["v1"]);
        assert_eq!(ctx.vars_of(&GenType::Bool), vec!["v0"]);
        assert_eq!(ctx.visible_names(), vec!["v0", "v1"]);
    }
}
