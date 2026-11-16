// x-leaks: what the runtime holds across dynamic-module reloads and
// compile-and-drop inputs, by registry. A graphix-rt integration test.
//
// Run: copy to graphix-rt/tests/review_x_leaks_1.rs, then
//   cargo test -p graphix-rt --test review_x_leaks_1 -- --include-ignored --nocapture
// Results at HEAD e1be2fbf2 (a "<-- GREW" line is a count that rose between
// the 20th and the 3020th step; rss_kb is the whole test process's):
//
// reload_exported_fn: no counted registry grows (env, lexical, names, Lives,
//   block types, rt, EnvStats, fusion.retained), yet RssAnon rises ~120 B per
//   reload with an exported function val (none without it): the LambdaIds
//   Weak links (x-leaks-reload-exported-fn.gx).
// repl_inputs (compile-and-drop, distinct text each time):
//   { type C = Abstract<i64>; C(i).0 }   env.abstract_reps 21 -> 3020,
//                                        env.lexical.block_names 21 -> 3020
//   { type C = {a: i64}; ... }           env.lexical.block_names 21 -> 3020
//   { let st = mod q dynamic {..}; q::v } Debug of env.block_types 1211 -> 151163
//                                        chars (BlockRegistry::module_loads)
//   { let g = |x| x + i; g(i) }          nothing (control)
// hof_calls: root `ap`, inputs `ap(|x| x + i, 1)`:
//   fusion.retained 176 -> 20076, jit_committed 86016 -> 3989504
// failed_compiles: the same text with an impl of a root trait, compiled while
//   the first compile lives: refused "conflicting implementation: T is already
//   implemented for C" (x-leaks-repl-same-line-conflict.sh).
// failed_fusion_records: FusionStats::failed 126 -> 18120 entries,
//   rss_kb 5032 -> 26108 over 3000 dropped inputs with an unfusable lambda
//   and a 4000-byte string (x-leaks-repl-fusion-failure-pins-source.sh).

use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{CompExp, GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

fn rss_kb() -> usize {
    let s = std::fs::read_to_string("/proc/self/status").unwrap();
    s.lines()
        .find(|l| l.starts_with("RssAnon:"))
        .and_then(|l| l.split_whitespace().nth(1))
        .and_then(|n| n.parse().ok())
        .unwrap_or(0)
}

async fn counts(gx: &GXHandle<NoExt>) -> Result<Vec<(&'static str, usize)>> {
    let s = gx.env_stats().await?;
    let f = gx.fusion_stats().await?;
    let mut v = gx
        .with_ctx(|ctx| {
            let cx = &ctx.cx;
            let env = &cx.env;
            macro_rules! nested {
                ($m:expr) => {
                    (&$m).into_iter().map(|(_, names)| names.len()).sum::<usize>()
                };
            }
            vec![
                ("lambda_defs", cx.lambda_defs.len()),
                ("bind_to_lambda", cx.bind_to_lambda.len()),
                ("fn_forward_resolutions", cx.fn_forward_resolutions.len()),
                ("builtin_bindings", cx.builtin_bindings.len()),
                ("connect_targets", cx.connect_targets.len()),
                ("batch_connect_targets", cx.batch_connect_targets.len()),
                ("env.by_id", env.by_id.len()),
                ("env.byref_chain", env.byref_chain.len()),
                ("env.names", env.names.len()),
                ("env.abstract_reps", env.abstract_reps.len()),
                ("env.trait_defs", env.trait_defs.len()),
                ("env.trait_methods", env.trait_methods.len()),
                ("env.impls", nested!(*env.impls)),
                ("env.kept_traits", env.kept_traits.len()),
                ("env.kept_impls", env.kept_impls.len()),
                ("env.poly_binds", env.poly_binds.len()),
                ("lex.binds", nested!(env.lexical.binds)),
                ("lex.typedefs", nested!(env.lexical.typedefs)),
                ("lex.traits", nested!(env.lexical.traits)),
                ("lex.modules", env.lexical.modules.len()),
                ("lex.block_names", nested!(env.lexical.block_names)),
                ("env.unsettled", env.unsettled.len()),
                ("rt.store", ctx.rt.store().len()),
                ("dbg.lives", format!("{:?}", ctx.cx.control.lives()).len()),
                ("dbg.rt", format!("{:?}", ctx.rt).len()),
                ("dbg.block_types", format!("{:?}", env.block_types).len()),
                ("dbg.names", format!("{:?}", env.names).len()),
                ("dbg.lexical", format!("{:?}", env.lexical).len()),
            ]
        })
        .await?;
    v.extend([
        ("stats.store_len", s.store_len),
        ("stats.aliases_len", s.aliases_len),
        ("stats.ref_var_keys", s.ref_var_keys),
        ("stats.ref_var_total", s.ref_var_total),
        ("stats.lowerings_len", s.lowerings_len),
        ("fusion.retained", f.retained),
        ("rss_kb", rss_kb()),
    ]);
    Ok(v)
}

fn report(what: &str, a: &[(&'static str, usize)], b: &[(&'static str, usize)]) {
    println!("=== {what}");
    for ((n, x), (_, y)) in a.iter().zip(b.iter()) {
        let mark = if y > x { "  <-- GREW" } else { "" };
        println!("{n:28} {x:>10} {y:>10}{mark}");
    }
}

async fn answer(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    live: &CompExp<NoExt>,
    want: i64,
) -> Result<()> {
    let found = tokio::time::timeout(Duration::from_secs(20), async {
        while let Some(mut batch) = rx.recv().await {
            for e in batch.drain(..) {
                if let GXEvent::Updated(id, v) = e
                    && id == live.id
                    && v == Value::I64(want)
                {
                    return Some(());
                }
            }
        }
        None
    });
    found.await?.context("no answer")
}

async fn reloads(sig: &str, body: &str, fusion: bool) -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(1000);
    let mut b = GXConfig::builder(ctx, tx);
    if !fusion {
        b = b.flags(graphix_compiler::CFlag::FusionDisabled.into());
    }
    let gx = b.build()?.start().await?;
    let program = gx
        .compile(ArcStr::from(format!(
            "let n: i64 = 0;\nlet st = mod m dynamic {{ sig {{ {sig} }}; source \"{body}\" }};\nm::v"
        )))
        .await?;
    let live = program.exprs.last().context("value")?;
    let env = gx.get_env().await?;
    let mut n =
        gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["n"])).await?;
    let mut early = vec![];
    for i in 1..=3020i64 {
        n.set(Value::I64(i))?;
        answer(&mut rx, live, i).await?;
        if i == 20 {
            early = counts(&gx).await?;
        }
    }
    let late = counts(&gx).await?;
    report(&format!("reloads fusion={fusion} sig=[{sig}] body=[{body}] 20 vs 3020"), &early, &late);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn reload_exported_fn() -> Result<()> {
    reloads("val v: i64; val f: fn(x: i64) -> i64", "let v = [n]; let f = |x| x + v", false).await?;
    reloads("val v: i64; val f: fn(x: i64) -> i64", "let v = [n]; let f = |x| x + v", true).await?;
    reloads("val v: i64", "let v = [n]", false).await?;
    reloads("val v: i64", "let f = |x| x + 1; let v = f([n])", false).await
}

async fn compiled_and_dropped(what: &str, text: impl Fn(i64) -> String) -> Result<()> {
    compiled_and_dropped_after(what, None, text).await
}

async fn compiled_and_dropped_after(
    what: &str,
    root: Option<&str>,
    text: impl Fn(i64) -> String,
) -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let _root = match root {
        Some(r) => Some(gx.compile(ArcStr::from(r)).await?),
        None => None,
    };
    let mut early = vec![];
    let mut t = std::time::Instant::now();
    for i in 0..3020i64 {
        drop(gx.compile(ArcStr::from(text(i))).await?);
        if i == 20 {
            early = counts(&gx).await?;
            let f = gx.fusion_stats().await?;
            early.push(("jit_committed", f.jit_committed));
            early.push(("fused", f.fused));
            t = std::time::Instant::now();
        }
        if i == 520 {
            println!("{what}: 500 compiles at the start took {:?}", t.elapsed());
        }
        if i == 2520 {
            t = std::time::Instant::now();
        }
    }
    println!("{what}: 500 compiles at the end took {:?}", t.elapsed());
    let mut late = counts(&gx).await?;
    let f = gx.fusion_stats().await?;
    late.push(("jit_committed", f.jit_committed));
    late.push(("fused", f.fused));
    report(what, &early, &late);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn hof_calls() -> Result<()> {
    compiled_and_dropped_after(
        "root hof called with fresh lambdas",
        Some("let ap = |f: fn(x: i64) -> i64, x: i64| f(x)"),
        |i| format!("ap(|x| x + {i}, 1)"),
    )
    .await?;
    compiled_and_dropped_after(
        "root fn variable written with fresh lambdas",
        Some("let h: fn(x: i64) -> i64 = |x| x"),
        |i| format!("{{ h <- |x| x + {i}; 1 }}"),
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[ignore]
async fn repl_inputs() -> Result<()> {
    compiled_and_dropped("block abstract type, distinct text", |i| {
        format!("{{ type C = Abstract<i64>; C({i}).0 }}")
    })
    .await?;
    compiled_and_dropped("block alias type, distinct text", |i| {
        format!("{{ type C = {{a: i64}}; let c: C = {{a: {i}}}; c.a }}")
    })
    .await?;
    compiled_and_dropped("dynamic module, distinct text", |i| {
        format!("{{ let st = mod q dynamic {{ sig {{ val v: i64 }}; source \"let v = {i}\" }}; q::v }}")
    })
    .await?;
    compiled_and_dropped("dynamic module, same text", |_| {
        literal!("{ let st = mod q dynamic { sig { val v: i64 }; source \"let v = 1\" }; q::v }").to_string()
    })
    .await?;
    compiled_and_dropped("lambda block, distinct text", |i| {
        format!("{{ let g = |x| x + {i}; g({i}) }}")
    })
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn failed_compiles() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let _root = gx.compile(literal!("trait T { val m: fn(self) -> i64 }")).await?;
    let text = literal!("{ type C = Abstract<i64>; impl T for C { let m = |c| c.0 }; T::m(C(1)) }");
    let _first = gx.compile(text.clone()).await?;
    let mut early = vec![];
    for i in 0..3020i64 {
        let r = gx.compile(text.clone()).await;
        assert!(r.is_err(), "the second compile of the same text was accepted");
        if i == 0 {
            println!("second compile: {:#}", r.err().unwrap());
        }
        if i == 20 {
            early = counts(&gx).await?;
        }
    }
    let late = counts(&gx).await?;
    report("same text with a core impl compiled again while the first lives (fails)", &early, &late);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn failed_fusion_records() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let pad = "x".repeat(4000);
    let mut early = (0, 0);
    for i in 0..3020i64 {
        let text = format!(
            "{{ let pad = \"{pad}\"; let f = |k: i64| {{ let s = 0; s <- k; s + 1 }}; f({i}) }}"
        );
        drop(gx.compile(ArcStr::from(text)).await?);
        if i == 20 {
            early = (gx.fusion_stats().await?.failed.len(), rss_kb());
        }
    }
    let late = (gx.fusion_stats().await?.failed.len(), rss_kb());
    println!("FusionStats::failed len, rss_kb: after 20 inputs {early:?}, after 3020 {late:?}");
    Ok(())
}
