//! Source C — type-directed program generation from scratch.
//!
//! Unlike Source A (mutating existing seeds), this builds valid programs
//! out of nothing, reaching shapes no fixture contains — "test cases no
//! one would think of." It's purely mechanical (no API), so it can run
//! for as long as you let it.
//!
//! The generator is TYPE-DIRECTED: `gen_typed(ctx, ty, …)` emits a graphix
//! expression *of type `ty`*, recursively, choosing only constructions
//! that produce `ty` from in-scope bindings + literals. This guarantees
//! type-correctness by construction (the killer constraint — a random
//! parse-valid graphix program typechecks ~never). It emits TEXT (not the
//! AST), tracking types as it goes; the oracle compiles from text.
//!
//! `let`-bindings introduce scoped variables that later expressions
//! reference — producing the internal data dependencies that stress
//! fusion's region/dataflow analysis (fusion *is* dependency-subgraph
//! analysis). Bindings deliberately REBIND in-scope names ([`GenCfg`]'s
//! `p_shadow`) and collide with names used in other scopes
//! (`p_collision`) — the 2026-07 audit's bug classes were all
//! name-vs-identity confusions the fresh-names-only V1 could never reach.
//! Only pure, deterministic constructs are generated (no
//! rand/time/net/fs), so the oracle's comparison is sound.
//!
//! Current vocabulary: i64/f64/u8/bool/string scalars, arithmetic,
//! comparison, boolean ops, tuples, arrays, `select`, and shadowed/
//! colliding rebinds. Growing per the fuzzer-V2 plan: lambdas +
//! monomorphization, composites + accessors, HOF callbacks, real select
//! patterns, error ops, reactive programs with injection schedules.

mod exprs;
mod funcs;
mod patterns;
pub mod reactive;
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
    /// A lambda is poly (explicit `'a: Number` constraint form —
    /// per-call-site monomorphization) rather than fully typed.
    pub p_poly: f64,
    /// A statement slot emits the bare-unannotated-lambda template
    /// (wide shared tvar, two unannotated call sites — the everyday
    /// user shape and the wide-Number JIT-blocker class).
    pub p_bare: f64,
    /// A poly lambda is immediately called at two distinct numeric
    /// types (the audit's bug-2 shape).
    pub p_mono_pair: f64,
    /// A typed lambda's body is a block with a collision-prone local
    /// (the audit's bug-3 shape).
    pub p_body_block: f64,
    /// A statement slot emits a terminating `let rec` + call.
    pub p_rec: f64,
    /// A statement slot emits the whole shadowed-lambda-name template
    /// (the audit's bug-1 shape).
    pub p_lambda_shadow_template: f64,
    /// A value let binds a TAG-UNION variant (always annotated — a
    /// bare variant literal's type is its single tag).
    pub p_variant: f64,
    /// A statement slot emits the error-arm-lambda template (a select
    /// merging an ok arm with `error(...)` as a lambda's return — the
    /// soak-jul06c B5 shape) plus a consumed call.
    pub p_error_lambda: f64,
    /// Statement slots per program: 0..=max_lets (template slots may
    /// emit several statements).
    pub max_lets: usize,
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
            max_lets: 6,
        }
    }
}

/// Which bug-class shapes one generated program contains — the
/// shape-presence gate asserts each stays reachable at a healthy rate
/// (a probability-weights bug silently disabling a shape is exactly
/// the failure mode this fuzzer exists to catch in the compiler).
#[derive(Debug, Default, Clone, Copy)]
pub struct GenStats {
    /// A lambda name was rebound (organically or via the template) —
    /// audit bug-1 shape.
    pub lambda_rebind: bool,
    /// A poly lambda got call sites at two distinct numeric types —
    /// audit bug-2 shape.
    pub mono_pair: bool,
    /// A lambda-body local reused a name bound elsewhere in the
    /// program — audit bug-3 shape.
    pub collision_local: bool,
    /// A `let rec` was emitted.
    pub rec: bool,
    /// The error-arm-lambda template was emitted (B5 shape).
    pub error_lambda: bool,
}

pub(crate) fn chance(rng: &mut Rng, p: f64) -> bool {
    (rng.below(1000) as f64) < p * 1000.0
}

#[derive(Clone)]
pub(crate) struct GenCtx {
    /// In-scope bindings in declaration order (inner scopes at the
    /// tail). Shadowing = a later entry with the same name; lookups
    /// scan in REVERSE and count only the first hit per name
    /// (last-binding-wins), so a name rebound at a different type never
    /// produces a reference to the dead earlier type.
    vars: Vec<(String, GenType)>,
    /// Every name ever used for a binding, visible or not — the pool
    /// `name_for_bind` draws targeted collisions from.
    collision_pool: Vec<String>,
    next: usize,
}

impl GenCtx {
    fn new() -> Self {
        GenCtx { vars: Vec::new(), collision_pool: Vec::new(), next: 0 }
    }

    fn fresh(&mut self) -> String {
        let n = format!("v{}", self.next);
        self.next += 1;
        n
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
        if !self.collision_pool.contains(&name) {
            self.collision_pool.push(name.clone());
        }
        self.vars.push((name, ty));
    }

    /// Distinct visible names, innermost first.
    fn visible_names(&self) -> Vec<&str> {
        let mut out: Vec<&str> = Vec::new();
        for (n, _) in self.vars.iter().rev() {
            if !out.contains(&n.as_str()) {
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
    /// pool deliberately keeps them — out-of-scope names are what
    /// targeted collisions are made of.
    fn mark(&self) -> usize {
        self.vars.len()
    }

    fn truncate(&mut self, mark: usize) {
        self.vars.truncate(mark);
    }

    /// The visible bindings (last-binding-wins per name), innermost
    /// first — the single lookup all the typed queries filter over.
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

/// Generate one complete program with the default profile.
pub fn gen_program(rng: &mut Rng) -> String {
    gen_program_stats(&GenCfg::default(), rng).0
}

/// Generate one complete program (a graphix expression): a run of
/// statement slots — value lets, lambda bindings, rec skeletons,
/// bug-shape templates — whose values reference (and rebind) earlier
/// bindings, plus a tail expression. Also reports which bug-class
/// shapes the program contains.
pub fn gen_program_stats(cfg: &GenCfg, rng: &mut Rng) -> (String, GenStats) {
    let mut ctx = GenCtx::new();
    let mut stats = GenStats::default();
    let mut stmts = Vec::new();
    let nslots = rng.below(cfg.max_lets + 1);
    for _ in 0..nslots {
        if chance(rng, cfg.p_rec) {
            stmts.extend(funcs::gen_rec_lambda(&mut ctx, rng, cfg, &mut stats));
        } else if chance(rng, cfg.p_error_lambda) {
            stmts.extend(funcs::gen_error_arm_lambda(&mut ctx, rng, cfg, &mut stats));
        } else if chance(rng, cfg.p_lambda_shadow_template) {
            stmts.extend(funcs::gen_shadowed_lambda_template(
                &mut ctx, rng, cfg, &mut stats,
            ));
        } else if chance(rng, cfg.p_bare) {
            stmts.extend(funcs::gen_bare_lambda(&mut ctx, rng, cfg));
        } else if chance(rng, cfg.p_lambda) {
            if chance(rng, cfg.p_poly) {
                stmts.extend(funcs::gen_poly_lambda(&mut ctx, rng, cfg, &mut stats));
            } else {
                stmts.push(funcs::gen_typed_lambda(&mut ctx, rng, cfg, &mut stats));
            }
        } else {
            let variant = chance(rng, cfg.p_variant);
            let ty = if variant {
                types::random_variant(rng, 1)
            } else {
                types::random_type(rng, 2)
            };
            let val = patterns::maybe_select(&ctx, rng, &ty, 3)
                .unwrap_or_else(|| exprs::gen_typed(&ctx, rng, &ty, 3));
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
    let tail_ty = types::random_type(rng, 2);
    let tail = patterns::maybe_select(&ctx, rng, &tail_ty, 3)
        .unwrap_or_else(|| exprs::gen_typed(&ctx, rng, &tail_ty, 3));
    let prog = if stmts.is_empty() {
        tail
    } else {
        format!("{{ {}; {} }}", stmts.join("; "), tail)
    };
    (prog, stats)
}

#[cfg(test)]
mod test {
    use super::*;

    /// Same seed → byte-identical program stream. The whole
    /// replay/minimize/regress pipeline rests on this.
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

    /// Shape presence: with the default profile a healthy fraction of
    /// programs contain a REBIND (the same name bound twice). Catches a
    /// weights bug silently disabling the feature.
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

    /// The Phase-1.2 acceptance gate: the generator can EXPRESS all
    /// three 2026-07 audit bug shapes (each was hand-found because the
    /// fresh-names-only V1 could not reach it), at a rate a campaign
    /// will actually exercise.
    #[test]
    fn audit_bug_shapes_reachable() {
        let cfg = GenCfg::default();
        let mut rng = Rng::new(3);
        let (mut rebind, mut mono, mut collision, mut rec, mut errl) = (0, 0, 0, 0, 0);
        const N: usize = 500;
        for _ in 0..N {
            let (_, s) = gen_program_stats(&cfg, &mut rng);
            rebind += s.lambda_rebind as usize;
            mono += s.mono_pair as usize;
            collision += s.collision_local as usize;
            rec += s.rec as usize;
            errl += s.error_lambda as usize;
        }
        for (what, n) in [
            ("lambda rebind (bug 1)", rebind),
            ("monomorphization pair (bug 2)", mono),
            ("collision local (bug 3)", collision),
            ("let rec", rec),
            ("error-arm lambda (B5 shape)", errl),
        ] {
            assert!(n * 100 >= N, "{what}: only {n}/{N} programs (<1%)");
        }
    }

    /// Rebinds must be sound: a name rebound at a different type must
    /// never be referenced at its dead earlier type. Purely structural
    /// check here (vars_of last-binding-wins); the oracle's compile
    /// step is the end-to-end guard.
    #[test]
    fn vars_of_last_binding_wins() {
        let mut ctx = GenCtx::new();
        ctx.push("v0".into(), GenType::I64);
        ctx.push("v1".into(), GenType::I64);
        ctx.push("v0".into(), GenType::Bool);
        assert_eq!(ctx.vars_of(&GenType::I64), vec!["v1"]);
        assert_eq!(ctx.vars_of(&GenType::Bool), vec!["v0"]);
        assert_eq!(ctx.visible_names(), vec!["v0", "v1"]);
    }
}
