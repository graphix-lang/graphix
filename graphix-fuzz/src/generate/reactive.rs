//! Reactive program generation (fuzzer-v2 Phase 3.2) — multi-cycle,
//! injection-driven programs built from quiescing-by-construction
//! templates, paired with a generated [`Schedule`].
//!
//! Every template quiesces on its own (bounded counters stop at a
//! literal limit; accumulators only move when an injected input
//! fires) EXCEPT the deliberate runaway (a few percent), which the
//! schedule's trace budget cuts deterministically. The injected
//! inputs are pushed into the [`GenCtx`] vocabulary first, so the
//! whole Phase-1 sync surface (typed lets, selects, calls) can
//! reference them — and `name_for_bind` can SHADOW them, which is a
//! deliberate shape (the injection still hits the root decl; later
//! body uses see the shadow, identically in both modes).

use netidx::publisher::Value;

use super::{GenCfg, GenCtx, GenType, chance, exprs, types};
use crate::mutate::Rng;
use crate::schedule::Schedule;

/// Which reactive shapes one generated program contains — the
/// shape-presence gate for this module.
#[derive(Debug, Default, Clone, Copy)]
pub struct ReactiveStats {
    pub inputs: usize,
    pub epochs: usize,
    pub counters: usize,
    pub accumulators: usize,
    pub cross_cycle: usize,
    pub runaway: bool,
}

/// Generate one reactive WRAPPER (schedule header + body) with the
/// default profile.
pub fn gen_reactive_program(rng: &mut Rng) -> String {
    gen_reactive_stats(&GenCfg::default(), rng).0
}

pub fn gen_reactive_stats(_cfg: &GenCfg, rng: &mut Rng) -> (String, ReactiveStats) {
    let mut ctx = GenCtx::new();
    let mut stats = ReactiveStats::default();
    let mut stmts: Vec<String> = Vec::new();
    // Inputs: 1–2, weighted toward i64 (the widest template
    // vocabulary). Registered in the ctx BEFORE anything else so every
    // later expression can consume them.
    let n_inputs = 1 + rng.below(2);
    stats.inputs = n_inputs;
    let mut inputs: Vec<(String, GenType)> = Vec::new();
    for i in 0..n_inputs {
        let ty = match rng.below(10) {
            0..=5 => GenType::I64,
            6..=7 => GenType::F64,
            _ => GenType::Bool,
        };
        let name = format!("in{i}");
        ctx.push(name.clone(), ty.clone());
        inputs.push((name, ty));
    }
    // Reactive statements: 1–3 templates, each binding results into
    // the ctx. Sync lets from the Phase-1 vocabulary interleave.
    // `live` collects the i64 results that fire on EVERY injection of
    // their input (accumulators, sample chains) — the tail is biased
    // through one of them so injections stay observable at the traced
    // result (the epochs-advance health metric's whole subject).
    // `runaways` are forced into the tail: an unobserved runaway spins
    // in cycles that never emit a traced value, so the ACTIVE-cycle
    // budget never trips and the program burns its whole wall-clock
    // backstop; an observed one is cut by the cap deterministically.
    let mut live: Vec<String> = Vec::new();
    let mut runaways: Vec<String> = Vec::new();
    let n_templates = 1 + rng.below(3);
    for _ in 0..n_templates {
        if chance(rng, 0.25) {
            // A plain sync let over the enriched vocabulary.
            let ty = types::random_type(rng, 2);
            let val = exprs::gen_typed(&ctx, rng, &ty, 2);
            let name = ctx.fresh();
            stmts.push(format!("let {name}: {} = {val}", ty.render()));
            ctx.push(name, ty);
            continue;
        }
        match rng.below(10) {
            0 | 1 => counter(&mut ctx, rng, &mut stmts, &mut stats),
            2..=4 => {
                accumulator(&mut ctx, rng, &inputs, &mut stmts, &mut stats, &mut live)
            }
            5..=8 => cross_cycle(&mut ctx, rng, &inputs, &mut stmts, &mut stats),
            _ => {
                if chance(rng, 0.30) {
                    runaway(&mut ctx, &mut stmts, &mut stats, &mut runaways);
                } else {
                    sample_chain(&mut ctx, rng, &inputs, &mut stmts, &mut live);
                }
            }
        }
    }
    // If nothing input-driven landed, add one scalar accumulator so
    // injections are observable.
    if live.is_empty() {
        accumulator(&mut ctx, rng, &inputs, &mut stmts, &mut stats, &mut live);
    }
    // Tail: start from an input-driven result, mix in other visible
    // scalars, and force every runaway in.
    let tail = {
        let i64s: Vec<String> =
            ctx.vars_of(&GenType::I64).into_iter().map(|s| s.to_string()).collect();
        let mut t = live[rng.below(live.len())].clone();
        for _ in 0..rng.below(3) {
            let n = &i64s[rng.below(i64s.len())];
            let op = ["+", "-", "*"][rng.below(3)];
            t = format!("({t} {op} {n})");
        }
        for r in &runaways {
            t = format!("({t} + {r})");
        }
        t
    };
    let body = if stmts.is_empty() {
        tail
    } else {
        format!("{{ {}; {} }}", stmts.join("; "), tail)
    };
    // The schedule: 1–4 epochs, each injecting a non-empty subset of
    // the inputs. Values stay mild here — pushing them toward edges is
    // the M3 mutation's job.
    let n_epochs = 1 + rng.below(4);
    stats.epochs = n_epochs;
    let mut epochs: Vec<Vec<(String, Value)>> = Vec::with_capacity(n_epochs);
    for _ in 0..n_epochs {
        let mut ep = Vec::new();
        for (name, ty) in &inputs {
            if !ep.is_empty() && !chance(rng, 0.7) {
                continue;
            }
            ep.push((name.clone(), injection_value(rng, ty)));
        }
        epochs.push(ep);
    }
    // Coverage post-pass: the DRIVER derives the input decls from the
    // schedule, so an input the body references but no epoch injects
    // would be undeclared — a guaranteed compile reject. Every input
    // lands in at least one epoch.
    for (name, ty) in &inputs {
        if !epochs.iter().any(|ep| ep.iter().any(|(n, _)| n == name)) {
            let i = rng.below(epochs.len());
            epochs[i].push((name.clone(), injection_value(rng, ty)));
        }
    }
    let sched = Schedule { epochs, ..Schedule::default() };
    (sched.render(&body), stats)
}

fn injection_value(rng: &mut Rng, ty: &GenType) -> Value {
    match ty {
        GenType::I64 => Value::I64([-3, -1, 0, 1, 2, 3, 5, 7, 12, 100][rng.below(10)]),
        GenType::F64 => {
            Value::F64([-2.25, -1.0, 0.0, 0.5, 1.5, 3.0, 10.25][rng.below(7)])
        }
        GenType::Bool => Value::Bool(rng.below(2) == 0),
        other => unreachable!("no injection literal for {other:?}"),
    }
}

/// A self-clocked bounded counter — moves without any injection,
/// quiesces at a literal limit.
fn counter(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
) {
    let c = ctx.fresh();
    let x = ctx.fresh();
    let k = 1 + rng.below(6);
    stmts.push(format!("let {c} = i64:0"));
    stmts.push(format!(
        "select {c} {{ {x} if {x} < i64:{k} => {c} <- ({x} ~ {c}) + i64:1, _ => never() }}"
    ));
    ctx.push(c, GenType::I64);
    st.counters += 1;
}

/// An input-gated accumulator through the connect lift — scalar,
/// array (the sliding-window idiom), string, or struct, by the
/// input's type and the roll.
fn accumulator(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
    live: &mut Vec<String>,
) {
    let (input, ity) = &inputs[rng.below(inputs.len())];
    st.accumulators += 1;
    match ity {
        GenType::I64 => match rng.below(4) {
            0 => {
                let a = ctx.fresh();
                stmts.push(format!("let {a} = i64:0"));
                stmts.push(format!("{a} <- {input} ~ ({a} + {input})"));
                live.push(a.clone());
                ctx.push(a, GenType::I64);
            }
            1 => {
                let d = ctx.fresh();
                let l = ctx.fresh();
                stmts.push(format!("let {d}: Array<i64> = []"));
                stmts.push(format!("{d} <- {input} ~ array::push({d}, {input})"));
                stmts.push(format!("let {l} = array::len({d})"));
                live.push(l.clone());
                ctx.push(d, GenType::Array(Box::new(GenType::I64)));
                ctx.push(l, GenType::I64);
            }
            2 => {
                let s = ctx.fresh();
                let l = ctx.fresh();
                stmts.push(format!("let {s} = \"\""));
                stmts.push(format!("{s} <- {input} ~ \"[{s}]x[{input}]\""));
                stmts.push(format!("let {l} = str::len({s})"));
                live.push(l.clone());
                ctx.push(s, GenType::Str);
                ctx.push(l, GenType::I64);
            }
            _ => {
                let t = ctx.fresh();
                let m = ctx.fresh();
                stmts.push(format!("let {t} = {{ n: i64:0, last: i64:0 }}"));
                stmts.push(format!(
                    "{t} <- {input} ~ {{ {t} with n: {t}.n + i64:1, last: {input} }}"
                ));
                stmts.push(format!("let {m} = {t}.n * i64:100 + {t}.last"));
                live.push(m.clone());
                ctx.push(m, GenType::I64);
            }
        },
        GenType::F64 => {
            // The f64 accumulator itself can't join the i64 tail, so a
            // count over the same input carries the observable pulse.
            let a = ctx.fresh();
            let m = ctx.fresh();
            stmts.push(format!("let {a} = f64:0.0"));
            stmts.push(format!("{a} <- {input} ~ ({a} + {input})"));
            stmts.push(format!("let {m} = count({input})"));
            live.push(m.clone());
            ctx.push(a, GenType::F64);
            ctx.push(m, GenType::I64);
        }
        GenType::Bool => {
            // Count the true injections.
            let a = ctx.fresh();
            stmts.push(format!("let {a} = i64:0"));
            stmts.push(format!(
                "{a} <- {input} ~ (select {input} {{ true => {a} + i64:1, false => {a} }})"
            ));
            live.push(a.clone());
            ctx.push(a, GenType::I64);
        }
        other => unreachable!("no accumulator for input type {other:?}"),
    }
}

/// A cross-cycle builtin over an injected stream.
fn cross_cycle(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
) {
    let (input, ity) = &inputs[rng.below(inputs.len())];
    st.cross_cycle += 1;
    let b = ctx.fresh();
    let (expr, ty) = match ity {
        GenType::I64 => match rng.below(7) {
            0 => (format!("count({input})"), GenType::I64),
            1 => (format!("sum({input})"), GenType::I64),
            2 => (format!("uniq({input})"), GenType::I64),
            3 => (format!("once({input})"), GenType::I64),
            4 => (format!("take(#n: i64:{}, {input})", 1 + rng.below(3)), GenType::I64),
            5 => (format!("skip(#n: i64:{}, {input})", 1 + rng.below(3)), GenType::I64),
            _ => {
                let x = ctx.fresh();
                // `filter(v, pred)` — value first, predicate second.
                (
                    format!("filter({input}, |{x}: i64| {x} > i64:{})", rng.below(5)),
                    GenType::I64,
                )
            }
        },
        GenType::F64 => match rng.below(3) {
            0 => (format!("count({input})"), GenType::I64),
            1 => (format!("uniq({input})"), GenType::F64),
            _ => (format!("once({input})"), GenType::F64),
        },
        GenType::Bool => match rng.below(2) {
            0 => (format!("count({input})"), GenType::I64),
            _ => (format!("uniq({input})"), GenType::Bool),
        },
        other => unreachable!("no cross-cycle template for {other:?}"),
    };
    stmts.push(format!("let {b} = {expr}"));
    ctx.push(b, ty);
}

/// A `~` chain: a sync value sampled on the input's events.
fn sample_chain(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    live: &mut Vec<String>,
) {
    let (input, _) = &inputs[rng.below(inputs.len())];
    let t = ctx.fresh();
    let val = exprs::gen_typed(ctx, rng, &GenType::I64, 2);
    stmts.push(format!("let {t} = {input} ~ ({val})"));
    live.push(t.clone());
    ctx.push(t, GenType::I64);
}

/// The deliberate runaway — never quiesces; the schedule's cycle
/// budget cuts it deterministically in both modes.
fn runaway(
    ctx: &mut GenCtx,
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
    runaways: &mut Vec<String>,
) {
    let r = ctx.fresh();
    stmts.push(format!("let {r} = i64:0"));
    stmts.push(format!("{r} <- {r} + i64:1"));
    runaways.push(r.clone());
    ctx.push(r, GenType::I64);
    st.runaway = true;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn determinism() {
        let mut a = Rng::new(0xabcd);
        let mut b = Rng::new(0xabcd);
        for _ in 0..50 {
            assert_eq!(gen_reactive_program(&mut a), gen_reactive_program(&mut b));
        }
    }

    /// Every generated wrapper parses back through the schedule format
    /// (the generator renders through Schedule, so a failure here is a
    /// format drift).
    #[test]
    fn wrappers_parse() {
        let mut rng = Rng::new(7);
        for _ in 0..200 {
            let w = gen_reactive_program(&mut rng);
            Schedule::parse(&w).expect("generated wrapper must parse");
        }
    }

    /// Shape presence over the default profile.
    #[test]
    fn shape_presence() {
        let cfg = GenCfg::default();
        let mut rng = Rng::new(3);
        let (mut acc, mut cc, mut ctr, mut run) = (0, 0, 0, 0);
        const N: usize = 300;
        for _ in 0..N {
            let (_, st) = gen_reactive_stats(&cfg, &mut rng);
            acc += (st.accumulators > 0) as usize;
            cc += (st.cross_cycle > 0) as usize;
            ctr += (st.counters > 0) as usize;
            run += st.runaway as usize;
        }
        assert!(acc * 100 / N >= 25, "accumulators in only {acc}/{N}");
        assert!(cc * 100 / N >= 30, "cross-cycle in only {cc}/{N}");
        assert!(ctr * 100 / N >= 10, "counters in only {ctr}/{N}");
        assert!(run * 100 / N >= 1, "runaways in only {run}/{N}");
        assert!(run * 100 / N <= 15, "runaways in {run}/{N} — too hot");
    }
}
