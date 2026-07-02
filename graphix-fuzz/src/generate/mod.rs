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
    /// Bindings per program: 0..=max_lets.
    pub max_lets: usize,
}

impl Default for GenCfg {
    fn default() -> Self {
        GenCfg { p_shadow: 0.25, p_collision: 0.15, p_annotate: 0.3, max_lets: 6 }
    }
}

pub(crate) fn chance(rng: &mut Rng, p: f64) -> bool {
    (rng.below(1000) as f64) < p * 1000.0
}

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

    /// Visible bindings of type `ty` (last-binding-wins per name).
    fn vars_of(&self, ty: &GenType) -> Vec<&str> {
        let mut seen: Vec<&str> = Vec::new();
        let mut out = Vec::new();
        for (n, t) in self.vars.iter().rev() {
            if seen.contains(&n.as_str()) {
                continue;
            }
            seen.push(n.as_str());
            if t == ty {
                out.push(n.as_str());
            }
        }
        out
    }
}

/// Generate one complete program with the default profile.
pub fn gen_program(rng: &mut Rng) -> String {
    gen_program_cfg(&GenCfg::default(), rng)
}

/// Generate one complete program (a graphix expression): a run of
/// `let`-bindings whose values reference (and rebind) earlier bindings,
/// plus a tail expression.
pub fn gen_program_cfg(cfg: &GenCfg, rng: &mut Rng) -> String {
    let mut ctx = GenCtx::new();
    let mut stmts = Vec::new();
    let nlets = rng.below(cfg.max_lets + 1);
    for _ in 0..nlets {
        let ty = types::random_type(rng, 2);
        let val = exprs::maybe_select(&ctx, rng, &ty, 3)
            .unwrap_or_else(|| exprs::gen_typed(&ctx, rng, &ty, 3));
        let name = ctx.name_for_bind(rng, cfg);
        let stmt = if chance(rng, cfg.p_annotate) {
            format!("let {name}: {} = {val}", ty.render())
        } else {
            format!("let {name} = {val}")
        };
        stmts.push(stmt);
        ctx.push(name, ty);
    }
    let tail_ty = types::random_type(rng, 2);
    let tail = exprs::maybe_select(&ctx, rng, &tail_ty, 3)
        .unwrap_or_else(|| exprs::gen_typed(&ctx, rng, &tail_ty, 3));
    if stmts.is_empty() { tail } else { format!("{{ {}; {} }}", stmts.join("; "), tail) }
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
