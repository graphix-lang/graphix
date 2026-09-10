//! Reactive program generation: multi-cycle, injection-driven programs
//! built from quiescing-by-construction templates, paired with a
//! generated [`Schedule`]. Every template quiesces on its own except
//! the deliberate runaway, which the trace budget cuts. The injected
//! inputs enter the [`GenCtx`] vocabulary first, so the sync surface
//! can reference (and shadow) them.

use netidx::publisher::Value;

use super::{
    GenCfg, GenCtx, GenType, chance, exprs, types,
    types::{F64, I64, NumTy},
};
use crate::mutate::Rng;
use crate::schedule::Schedule;

/// Which reactive shapes one generated program contains.
#[derive(Debug, Default, Clone, Copy)]
pub struct ReactiveStats {
    pub inputs: usize,
    pub epochs: usize,
    pub counters: usize,
    pub accumulators: usize,
    pub cross_cycle: usize,
    pub runaway: bool,
    pub dyn_reload: bool,
    pub slept_arms: usize,
    /// Sync subprogram slots embedded.
    pub subprograms: usize,
    /// Block-valued bindings whose body connects to an outer target.
    pub nested_connects: usize,
    /// `seq` ceremonies (busy-drop) emitted.
    pub seqs: usize,
    /// `seqq` ceremonies (queued) emitted.
    pub seqqs: usize,
    /// Ceremonies whose trigger is a burst, so a second trigger lands
    /// while the first run is busy.
    pub burst_ceremonies: usize,
    /// Ceremony steps by kind, in [`STEP_KINDS`] order.
    pub steps: [usize; STEP_KINDS.len()],
}

/// The statement kinds a ceremony body draws from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepKind {
    /// `let x = e`: call-free, completes at entry.
    Let,
    /// `let x = h(e)`: an issued call to a generated lambda.
    Call,
    /// `acc <- e`: a generated connect to an outer accumulator.
    Connect,
    /// `do { let y = e; acc <- e }`: several statements as one arm.
    Do,
    /// `until cond`: a wait on a bool level.
    Until,
    /// `let t = try { bad(e)? } with(e) { e }`: the error branch taken
    /// on even arguments.
    Try,
    /// `let w = bad(e)?`: an abort path into the enclosing catch.
    Abort,
    /// `println("..")`: a waitable effect whose output the oracle compares.
    Print,
}

pub const STEP_KINDS: [StepKind; 8] = [
    StepKind::Let,
    StepKind::Call,
    StepKind::Connect,
    StepKind::Do,
    StepKind::Until,
    StepKind::Try,
    StepKind::Abort,
    StepKind::Print,
];

/// Generate one reactive wrapper (schedule header + body) with the
/// default profile. A slice of the lane is metamorphic twin programs.
pub fn gen_reactive_program(rng: &mut Rng) -> String {
    if rng.below(100) < 15 {
        return super::twin::gen_twin_program(rng);
    }
    gen_reactive_stats(&GenCfg::default(), rng).0
}

pub fn gen_reactive_stats(cfg: &GenCfg, rng: &mut Rng) -> (String, ReactiveStats) {
    // Runaways are generated without inputs or epochs: where an
    // injection lands in a free-running program's cycle stream is
    // wall-clock timing, but a single-burst runaway is cap-cut
    // deterministically from the Compiled anchor.
    if chance(rng, 0.03) {
        return gen_runaway_burst(rng);
    }
    let mut ctx = GenCtx::new();
    let mut stats = ReactiveStats::default();
    let mut stmts: Vec<String> = Vec::new();
    // inputs: 1–2, weighted toward i64, registered before anything else
    let n_inputs = 1 + rng.below(2);
    stats.inputs = n_inputs;
    let mut inputs: Vec<(String, GenType)> = Vec::new();
    for i in 0..n_inputs {
        let ty = match rng.below(10) {
            0..=5 => I64,
            6..=7 => F64,
            _ => GenType::Bool,
        };
        let name = format!("in{i}");
        ctx.push(name.clone(), ty.clone());
        inputs.push((name, ty));
    }
    // `fires_per_injection` collects the i64 results that fire on every injection of
    // their input; the tail is biased through one so injections stay
    // observable. Runaways are forced into the tail: an unobserved one
    // spins in cycles that never emit, so the active-cycle budget never
    // trips.
    let mut fires_per_injection: Vec<String> = Vec::new();
    let mut ndyn = 0usize;
    let n_templates = 1 + rng.below(3);
    for _ in 0..n_templates {
        if chance(rng, 0.25) {
            if cfg.subprogram_depth > 0 && chance(rng, 0.4) {
                // A composed sync subprogram slot. The outer binding never
                // shadows: the live/tail machinery references bindings by
                // name, so a shadow at a different type breaks the tail's typing.
                let mut sub_cfg = cfg.clone();
                sub_cfg.p_shadow = 0.0;
                sub_cfg.p_collision = 0.0;
                let mut gs = super::GenStats::default();
                stmts.push(super::gen_subprogram_stmt(&mut ctx, rng, &sub_cfg, &mut gs));
                stats.subprograms += gs.subprograms;
            } else {
                // a plain sync let over the enriched vocabulary
                let ty = types::random_type(rng, 2);
                let val = exprs::gen_typed(&ctx, rng, &ty, 2);
                let name = ctx.fresh();
                stmts.push(format!("let {name}: {} = {val}", ty.render()));
                ctx.push(name, ty);
            }
            continue;
        }
        match rng.below(17) {
            0 | 1 => counter(&mut ctx, rng, &mut stmts, &mut stats),
            2..=4 => accumulator(
                &mut ctx,
                rng,
                &inputs,
                &mut stmts,
                &mut stats,
                &mut fires_per_injection,
            ),
            5..=7 => cross_cycle(&mut ctx, rng, &inputs, &mut stmts, &mut stats),
            8 => {
                sample_chain(&mut ctx, rng, &inputs, &mut stmts, &mut fires_per_injection)
            }
            9 | 10 => slept_arm(&mut ctx, rng, &inputs, &mut stmts, &mut stats),
            11 | 12 => nested_connect(
                &mut ctx,
                rng,
                &inputs,
                &mut stmts,
                &mut stats,
                &mut fires_per_injection,
            ),
            13 => dyn_reload(&mut ctx, rng, &inputs, &mut stmts, &mut stats, &mut ndyn),
            _ => ceremony(
                &mut ctx,
                rng,
                &inputs,
                &mut stmts,
                &mut stats,
                &mut fires_per_injection,
            ),
        }
    }
    // if nothing input-driven landed, add one scalar accumulator
    if fires_per_injection.is_empty() {
        accumulator(
            &mut ctx,
            rng,
            &inputs,
            &mut stmts,
            &mut stats,
            &mut fires_per_injection,
        );
    }
    // tail: start from an input-driven result, mix in other visible
    // scalars, force every runaway in
    let tail = {
        let i64s: Vec<String> =
            ctx.vars_of(&I64).into_iter().map(|s| s.to_string()).collect();
        let mut t = fires_per_injection[rng.below(fires_per_injection.len())].clone();
        if !i64s.is_empty() {
            for _ in 0..rng.below(3) {
                let n = &i64s[rng.below(i64s.len())];
                let op = ["+", "-", "*"][rng.below(3)];
                t = format!("({t} {op} {n})");
            }
        }
        t
    };
    let body = if stmts.is_empty() {
        tail
    } else {
        format!("{{ {}; {} }}", stmts.join("; "), tail)
    };
    // The schedule: a geometric epoch draw (mean 4, tail to 12), each
    // epoch injecting a non-empty subset of the inputs. Values stay
    // mild; edges are mutation's job. Budgets scale with the schedule.
    let n_epochs = 1 + super::geo_slots(rng, 3, 11);
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
    // the driver derives the input decls from the schedule, so every
    // input must land in at least one epoch
    for (name, ty) in &inputs {
        if !epochs.iter().any(|ep| ep.iter().any(|(n, _)| n == name)) {
            let i = rng.below(epochs.len());
            epochs[i].push((name.clone(), injection_value(rng, ty)));
        }
    }
    let sched = Schedule {
        epochs,
        max_cycles: crate::trace::MAX_CYCLES + 16 * n_epochs as u64,
        max_events: crate::trace::MAX_EVENTS + 128 * n_epochs,
    };
    (sched.render(&body), stats)
}

fn injection_value(rng: &mut Rng, ty: &GenType) -> Value {
    match ty {
        GenType::Num(NumTy::I64) => {
            Value::I64([-3, -1, 0, 1, 2, 3, 5, 7, 12, 100][rng.below(10)])
        }
        GenType::Num(NumTy::F64) => {
            Value::F64([-2.25, -1.0, 0.0, 0.5, 1.5, 3.0, 10.25][rng.below(7)])
        }
        GenType::Bool => Value::Bool(rng.below(2) == 0),
        other => unreachable!("no injection literal for {other:?}"),
    }
}

/// A self-clocked bounded counter: moves without any injection,
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
    ctx.push(c, I64);
    st.counters += 1;
}

/// A block-valued binding whose body connects to an outer target. The
/// target rides `fires_per_injection`: it accumulates on every injection of its input.
fn nested_connect(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
    fires_per_injection: &mut Vec<String>,
) {
    let i64s: Vec<&(String, GenType)> =
        inputs.iter().filter(|(_, t)| *t == I64).collect();
    if i64s.is_empty() {
        return accumulator(ctx, rng, inputs, stmts, st, fires_per_injection);
    }
    let (input, _) = i64s[rng.below(i64s.len())];
    st.nested_connects += 1;
    let t = ctx.fresh();
    stmts.push(format!("let {t} = i64:0"));
    ctx.push(t.clone(), I64);
    let b = ctx.fresh();
    let mark = ctx.mark();
    let inner = ctx.fresh();
    let iv = exprs::gen_typed(ctx, rng, &I64, 2);
    stmts.push(format!(
        "let {b}: i64 = {{ let {inner}: i64 = {iv};          {t} <- {input} ~ ({t} + {inner}); ({inner} + i64:1) }}"
    ));
    ctx.truncate(mark);
    ctx.push(b, I64);
    fires_per_injection.push(t);
}

fn accumulator(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
    fires_per_injection: &mut Vec<String>,
) {
    let (input, ity) = &inputs[rng.below(inputs.len())];
    st.accumulators += 1;
    match ity {
        GenType::Num(NumTy::I64) => match rng.below(4) {
            0 => {
                let a = ctx.fresh();
                stmts.push(format!("let {a} = i64:0"));
                stmts.push(format!("{a} <- {input} ~ ({a} + {input})"));
                fires_per_injection.push(a.clone());
                ctx.push(a, I64);
            }
            1 => {
                let d = ctx.fresh();
                let l = ctx.fresh();
                stmts.push(format!("let {d}: Array<i64> = []"));
                stmts.push(format!("{d} <- {input} ~ array::push({d}, {input})"));
                stmts.push(format!("let {l} = array::len({d})"));
                fires_per_injection.push(l.clone());
                ctx.push(d, GenType::Array(Box::new(I64)));
                ctx.push(l, I64);
            }
            2 => {
                let s = ctx.fresh();
                let l = ctx.fresh();
                stmts.push(format!("let {s} = \"\""));
                stmts.push(format!("{s} <- {input} ~ \"[{s}]x[{input}]\""));
                stmts.push(format!("let {l} = str::len({s})"));
                fires_per_injection.push(l.clone());
                ctx.push(s, GenType::Str);
                ctx.push(l, I64);
            }
            _ => {
                let t = ctx.fresh();
                let m = ctx.fresh();
                stmts.push(format!("let {t} = {{ n: i64:0, last: i64:0 }}"));
                stmts.push(format!(
                    "{t} <- {input} ~ {{ {t} with n: {t}.n + i64:1, last: {input} }}"
                ));
                stmts.push(format!("let {m} = {t}.n * i64:100 + {t}.last"));
                fires_per_injection.push(m.clone());
                ctx.push(m, I64);
            }
        },
        GenType::Num(NumTy::F64) => {
            // the f64 accumulator can't join the i64 tail, so a count over
            // the same input carries the observable pulse
            let a = ctx.fresh();
            let m = ctx.fresh();
            stmts.push(format!("let {a} = f64:0.0"));
            stmts.push(format!("{a} <- {input} ~ ({a} + {input})"));
            stmts.push(format!("let {m} = count({input})"));
            fires_per_injection.push(m.clone());
            ctx.push(a, F64);
            ctx.push(m, I64);
        }
        GenType::Bool => {
            // count the true injections
            let a = ctx.fresh();
            stmts.push(format!("let {a} = i64:0"));
            stmts.push(format!(
                "{a} <- {input} ~ (select {input} {{ true => {a} + i64:1, false => {a} }})"
            ));
            fires_per_injection.push(a.clone());
            ctx.push(a, I64);
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
        GenType::Num(NumTy::I64) => match rng.below(7) {
            0 => (format!("count({input})"), I64),
            1 => (format!("sum({input})"), I64),
            2 => (format!("uniq({input})"), I64),
            3 => (format!("once({input})"), I64),
            4 => (format!("take(#n: i64:{}, {input})", 1 + rng.below(3)), I64),
            5 => (format!("skip(#n: i64:{}, {input})", 1 + rng.below(3)), I64),
            _ => {
                let x = ctx.fresh();
                (format!("filter({input}, |{x}: i64| {x} > i64:{})", rng.below(5)), I64)
            }
        },
        GenType::Num(NumTy::F64) => match rng.below(3) {
            0 => (format!("count({input})"), I64),
            1 => (format!("uniq({input})"), F64),
            _ => (format!("once({input})"), F64),
        },
        GenType::Bool => match rng.below(2) {
            0 => (format!("count({input})"), I64),
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
    fires_per_injection: &mut Vec<String>,
) {
    let (input, _) = &inputs[rng.below(inputs.len())];
    let t = ctx.fresh();
    let val = exprs::gen_typed(ctx, rng, &I64, 2);
    stmts.push(format!("let {t} = {input} ~ ({val})"));
    fires_per_injection.push(t.clone());
    ctx.push(t, I64);
}

/// A toggling node-walked select: the scrutinee is an injected input's
/// parity, so epochs flip which arm is live, and the `once(...)` arm is
/// async, which keeps the select on the node-walk. The live arm's
/// builtin variant carries a per-epoch bottoming arg.
fn slept_arm(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
) {
    let Some((input, _)) = inputs.iter().find(|(_, t)| *t == I64) else {
        return;
    };
    st.slept_arms += 1;
    // the live arm: a locally-defined lambda call, a sync builtin over a
    // bottoming arg, or a mix through the enriched vocabulary
    let live_arm = match rng.below(3) {
        0 => {
            let h = ctx.fresh();
            let x = ctx.fresh();
            let body = exprs::gen_typed(ctx, rng, &I64, 1);
            stmts.push(format!(
                "let {h} = |{x}: i64| -> i64 {{ let y = {x} * i64:2; y + ({body}) }}"
            ));
            format!("{h}({input})")
        }
        1 => format!(
            "array::len(array::window(#n: i64:{}, [{input}], i64:100 / ({input} % i64:3)))",
            1 + rng.below(3)
        ),
        _ => {
            let v = exprs::gen_typed(ctx, rng, &I64, 2);
            format!("({v}) + {input}")
        }
    };
    let s = ctx.fresh();
    stmts.push(format!(
        "let {s} = select ({input} % i64:2) {{ i64:0 => {live_arm}, _ => once({input}) }}"
    ));
    // not pushed into `fires_per_injection`: the select fires only on its fires_per_injection-arm epochs
    ctx.push(s, I64);
}

/// A hot-reloading dynamic module: the source is selected from an array
/// of raw-string variants by an injected index, so each epoch can swap
/// the implementation. `srcs[in % n]$` is total for any injected i64.
/// Not pushed into `fires_per_injection`: two injections can select the same source,
/// and an unchanged source need not re-fire downstream.
fn dyn_reload(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    stats: &mut ReactiveStats,
    ndyn: &mut usize,
) {
    let Some((iname, _)) = inputs.iter().find(|(_, t)| *t == I64) else {
        return;
    };
    stats.dyn_reload = true;
    let dname = format!("dr{ndyn}");
    *ndyn += 1;
    let n_srcs = 2 + rng.below(2);
    let mut inner = ctx.clone();
    inner.truncate(0);
    let srcs: Vec<String> = (0..n_srcs)
        .map(|_| {
            let p = inner.fresh();
            let m = inner.mark();
            inner.push(p.clone(), I64);
            let body = exprs::gen_typed(&inner, rng, &I64, 1);
            inner.truncate(m);
            if chance(rng, 0.5) {
                // internal block-level computation, so a reload swap
                // deletes a module-body kernel with live slots
                let k = 1 + rng.below(5) as i64;
                format!(
                    "r#\"let f = |{p}: i64| -> i64 {body}; \
                     let q = f(i64:{k}) * i64:5; \
                     let z = array::len([q, q + i64:1, f(i64:{k} + i64:2)]); \
                     let w = q + z\"#"
                )
            } else {
                format!("r#\"let f = |{p}: i64| -> i64 {body}\"#")
            }
        })
        .collect();
    let srcs_name = ctx.fresh();
    stmts.push(format!("let {srcs_name} = [{}]", srcs.join(", ")));
    let status = ctx.fresh();
    stmts.push(format!(
        "let {status} = mod {dname} dynamic {{ sandbox whitelist [core];          sig {{ val f: fn(x: i64) -> i64 }};          source {srcs_name}[{iname} % i64:{n_srcs}]$ }}"
    ));
    let arg = exprs::gen_typed(ctx, rng, &I64, 1);
    let v = ctx.fresh();
    stmts.push(format!(
        "let {v} = select {status} {{ error as _ => i64:-1, null as _ =>          {dname}::f({arg}) }}"
    ));
    ctx.push(v, I64);
}

/// A `seq`/`seqq` ceremony. The trigger is an injected input (one run
/// per epoch) or a burst counter restarted by an injection, whose two
/// triggers land a cycle apart so `seq` drops the second and `seqq`
/// queues it. The body is a run of [`StepKind`]s over the enriched
/// vocabulary plus the trigger and earlier step lets; every run is
/// observable through the accumulator its connects write, a run
/// counter and the last block value, all present from init. A body
/// that can stall (an `until` on a level nothing in the program is
/// guaranteed to flip) or abort keeps its cells out of
/// `fires_per_injection`.
fn ceremony(
    ctx: &mut GenCtx,
    rng: &mut Rng,
    inputs: &[(String, GenType)],
    stmts: &mut Vec<String>,
    st: &mut ReactiveStats,
    fires_per_injection: &mut Vec<String>,
) {
    let (input, ity) = &inputs[rng.below(inputs.len())];
    let burst = chance(rng, 0.5);
    let (trigger, trigger_ty, burst_counter) = if burst {
        st.burst_ceremonies += 1;
        let b = ctx.fresh();
        let req = ctx.fresh();
        stmts.push(format!("let {b} = {input} ~ i64:0"));
        stmts.push(format!(
            "{b} <- select {b} {{ s if s < i64:3 => s + i64:1, _ => never() }}"
        ));
        stmts.push(format!(
            "let {req} = select {b} {{ i64:1 | i64:2 => {b}, _ => never() }}"
        ));
        ctx.push(b.clone(), I64);
        (req, I64, Some(b))
    } else {
        (input.clone(), ity.clone(), None)
    };
    let h = ctx.fresh();
    let x = ctx.fresh();
    let hbody = {
        let mark = ctx.mark();
        ctx.push(x.clone(), I64);
        let e = exprs::gen_typed(ctx, rng, &I64, 1);
        ctx.truncate(mark);
        e
    };
    stmts.push(format!(
        "let {h} = |{x}: i64| -> i64 {x} * i64:{} + ({hbody})",
        1 + rng.below(4)
    ));
    ctx.push(h.clone(), GenType::Fn { params: vec![I64], ret: Box::new(I64) });
    let bad = ctx.fresh();
    stmts.push(format!(
        "let {bad} = |{x}: i64| -> [i64, Error<`Oops>] \
         select ({x} % i64:2) {{ i64:0 => error(`Oops), _ => {x} }}"
    ));
    let cerr = ctx.fresh();
    let nerr = ctx.fresh();
    stmts.push(format!("let {cerr}: Error<Any> = never()"));
    stmts.push(format!("catch(e) {cerr} <- e"));
    stmts.push(format!("let {nerr} = i64:0"));
    stmts.push(format!("{nerr} <- {cerr} ~ ({nerr} + i64:1)"));
    ctx.push(nerr, I64);
    let acc = ctx.fresh();
    stmts.push(format!("let {acc} = i64:0"));
    ctx.push(acc.clone(), I64);
    let bool_inputs: Vec<&str> = inputs
        .iter()
        .filter_map(|(n, t)| (*t == GenType::Bool).then_some(n.as_str()))
        .collect();
    let mut body: Vec<String> = Vec::new();
    let mut reliable = true;
    let mark = ctx.mark();
    ctx.push(trigger.clone(), trigger_ty);
    ctx.no_catch = true;
    let n_steps = 1 + rng.below(4);
    for i in 0..n_steps {
        let kind = STEP_KINDS[rng.below(STEP_KINDS.len())];
        st.steps[STEP_KINDS.iter().position(|k| *k == kind).unwrap()] += 1;
        match kind {
            StepKind::Let => {
                let v = ctx.fresh();
                let e = exprs::gen_typed(ctx, rng, &I64, 2);
                body.push(format!("let {v} = {e}"));
                ctx.push(v, I64);
            }
            StepKind::Call => {
                let v = ctx.fresh();
                let e = exprs::gen_typed(ctx, rng, &I64, 1);
                body.push(format!("let {v} = {h}({e})"));
                ctx.push(v, I64);
            }
            StepKind::Connect => {
                let e = exprs::gen_typed(ctx, rng, &I64, 2);
                body.push(format!("{acc} <- {e}"));
            }
            StepKind::Do => {
                let y = ctx.fresh();
                let m = ctx.mark();
                let e0 = exprs::gen_typed(ctx, rng, &I64, 1);
                ctx.push(y.clone(), I64);
                let e1 = exprs::gen_typed(ctx, rng, &I64, 1);
                ctx.truncate(m);
                body.push(format!("do {{ let {y} = {e0}; {acc} <- {e1} }}"));
            }
            StepKind::Until => {
                let cond = match (&burst_counter, bool_inputs.is_empty()) {
                    (Some(b), _) if chance(rng, 0.6) => format!("({b} >= i64:3)"),
                    (_, false) if chance(rng, 0.7) => {
                        reliable = false;
                        bool_inputs[rng.below(bool_inputs.len())].to_string()
                    }
                    _ => {
                        reliable = false;
                        exprs::gen_typed(ctx, rng, &GenType::Bool, 1)
                    }
                };
                body.push(format!("until {cond}"));
            }
            StepKind::Try => {
                let t = ctx.fresh();
                let e = exprs::gen_typed(ctx, rng, &I64, 1);
                let with = if chance(rng, 0.8) {
                    exprs::gen_typed(ctx, rng, &I64, 1)
                } else {
                    reliable = false;
                    "e?".to_string()
                };
                body.push(format!(
                    "let {t} = try {{ {bad}({e})? }} with(e) {{ {with} }}"
                ));
                ctx.push(t, I64);
            }
            StepKind::Abort => {
                reliable = false;
                let w = ctx.fresh();
                let e = exprs::gen_typed(ctx, rng, &I64, 1);
                body.push(format!("let {w} = {bad}({e})?"));
                ctx.push(w, I64);
            }
            StepKind::Print => {
                let vars = ctx.vars_of(&I64);
                let v = vars[rng.below(vars.len())];
                body.push(format!("println(\"s{i} [{v}]\")"));
            }
        }
    }
    let tail = exprs::gen_typed(ctx, rng, &I64, 2);
    body.push(tail);
    ctx.no_catch = false;
    ctx.truncate(mark);
    let queued = chance(rng, 0.5);
    if queued {
        st.seqqs += 1;
    } else {
        st.seqs += 1;
    }
    let done = ctx.fresh();
    stmts.push(format!(
        "let {done} = {} {trigger} {{ {} }}",
        if queued { "seqq" } else { "seq" },
        body.join("; ")
    ));
    let runs = ctx.fresh();
    let last = ctx.fresh();
    stmts.push(format!("let {runs} = i64:0"));
    stmts.push(format!("{runs} <- {done} ~ ({runs} + i64:1)"));
    stmts.push(format!("let {last} = i64:0"));
    stmts.push(format!("{last} <- {done}"));
    if reliable {
        fires_per_injection.push(runs.clone());
    }
    ctx.push(runs, I64);
    ctx.push(last, I64);
}

/// The deliberate runaway: an input-free single burst (no schedule)
/// that never quiesces; the trace's cycle budget cuts it deterministically.
fn gen_runaway_burst(rng: &mut Rng) -> (String, ReactiveStats) {
    let mut ctx = GenCtx::new();
    let mut stats = ReactiveStats::default();
    let mut stmts: Vec<String> = Vec::new();
    let r = ctx.fresh();
    stmts.push(format!("let {r} = i64:0"));
    stmts.push(format!("{r} <- {r} + i64:1"));
    ctx.push(r.clone(), I64);
    stats.runaway = true;
    if chance(rng, 0.5) {
        counter(&mut ctx, rng, &mut stmts, &mut stats);
    }
    let val = exprs::gen_typed(&ctx, rng, &I64, 2);
    let body = format!("{{ {}; ({r} + ({val})) }}", stmts.join("; "));
    (Schedule::default().render(&body), stats)
}

#[cfg(test)]
mod test {
    use super::*;

    /// Subprogram slots and nested connects both appear at the default
    /// profile.
    #[test]
    fn reactive_composition_presence() {
        let mut rng = Rng::new(0x5150);
        let (mut nsub, mut nconn) = (0usize, 0usize);
        for _ in 0..400 {
            let (_, st) = gen_reactive_stats(&GenCfg::default(), &mut rng);
            nsub += st.subprograms;
            nconn += st.nested_connects;
        }
        assert!(nsub > 10, "subprogram slots over 400 programs: {nsub}");
        assert!(nconn > 15, "nested connects over 400 programs: {nconn}");
    }

    #[test]
    fn determinism() {
        let mut a = Rng::new(0xabcd);
        let mut b = Rng::new(0xabcd);
        for _ in 0..50 {
            assert_eq!(gen_reactive_program(&mut a), gen_reactive_program(&mut b));
        }
    }

    /// Every generated wrapper parses back through the schedule format.
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
        let (mut acc, mut cc, mut ctr, mut run, mut slept) = (0, 0, 0, 0, 0);
        const N: usize = 300;
        for _ in 0..N {
            let (_, st) = gen_reactive_stats(&cfg, &mut rng);
            acc += (st.accumulators > 0) as usize;
            cc += (st.cross_cycle > 0) as usize;
            ctr += (st.counters > 0) as usize;
            run += st.runaway as usize;
            slept += (st.slept_arms > 0) as usize;
        }
        assert!(acc * 100 / N >= 25, "accumulators in only {acc}/{N}");
        assert!(cc * 100 / N >= 25, "cross-cycle in only {cc}/{N}");
        assert!(ctr * 100 / N >= 10, "counters in only {ctr}/{N}");
        assert!(run * 100 / N >= 1, "runaways in only {run}/{N}");
        assert!(run * 100 / N <= 15, "runaways in {run}/{N} — too hot");
        assert!(slept * 100 / N >= 8, "slept-arm selects in only {slept}/{N}");
    }

    /// Both ceremony forms, both trigger shapes and every step kind
    /// appear at the default profile.
    #[test]
    fn ceremony_presence() {
        let mut rng = Rng::new(0x5e9);
        let mut sum = ReactiveStats::default();
        const N: usize = 400;
        for _ in 0..N {
            let (_, st) = gen_reactive_stats(&GenCfg::default(), &mut rng);
            sum.seqs += st.seqs;
            sum.seqqs += st.seqqs;
            sum.burst_ceremonies += st.burst_ceremonies;
            for (a, b) in sum.steps.iter_mut().zip(st.steps) {
                *a += b;
            }
        }
        assert!(sum.seqs * 100 / N >= 8, "seq in only {}/{N}", sum.seqs);
        assert!(sum.seqqs * 100 / N >= 8, "seqq in only {}/{N}", sum.seqqs);
        assert!(
            sum.burst_ceremonies * 100 / N >= 8,
            "bursts: {}/{N}",
            sum.burst_ceremonies
        );
        for (kind, n) in STEP_KINDS.iter().zip(sum.steps) {
            assert!(n * 100 / N >= 3, "{kind:?} steps: only {n} over {N} programs");
        }
    }
}
