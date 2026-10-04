// `#[parallel]` and `#[serial]` (design/parallel_eval.md §7), and how a
// collection's growth runs under `Auto` (§8).

use anyhow::{Result, bail};
use graphix_compiler::{BitFlags, CFlag, ParMode};
use graphix_package_core::testing::init_with_flags_and_setup;
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

/// What a run made.
struct Ran {
    /// The result's updates.
    values: Vec<Value>,
    /// Forks of evaluation.
    forks: u64,
    /// Runs of compile tasks building instances.
    build_forks: u64,
}

/// Run `code` (as `let result = {code}`) node-walked under `mode` until it
/// is quiet for 300ms.
async fn run_par(code: &str, mode: ParMode) -> Result<Ran> {
    run_with(code, mode, CFlag::FusionDisabled.into()).await
}

/// [`run_par`] fused: a kernel's loops fork as chunks of its slots.
async fn run_fused(code: &str, mode: ParMode) -> Result<Ran> {
    run_with(code, mode, BitFlags::empty()).await
}

/// Run `code` compiled with `flags` under `mode` until it is quiet for
/// 300ms. Under `Auto` the run waits for the fork threshold's
/// calibration first: nothing forks until it.
async fn run_with(code: &str, mode: ParMode, flags: BitFlags<CFlag>) -> Result<Ran> {
    if mode == ParMode::Auto {
        while graphix_compiler::cost::calibration().is_none() {
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
    }
    let (tx, mut rx) = mpsc::channel(10);
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(format!(
            "let result = {code}"
        ))),
    )]);
    let ctx = init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![graphix_compiler::expr::VfsResolver::new(tbl)],
        flags,
        move |ctx| ctx.control.set_par_mode(mode),
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let mut values = Vec::new();
    let deadline = tokio::time::sleep(std::time::Duration::from_secs(20));
    tokio::pin!(deadline);
    loop {
        let quiet = tokio::time::sleep(std::time::Duration::from_millis(300));
        tokio::pin!(quiet);
        tokio::select! {
            _ = &mut deadline => bail!("the program did not quiesce"),
            _ = &mut quiet => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => values.extend(batch.drain(..).filter_map(|e| match e {
                    GXEvent::Updated(id, v) if id == eid => Some(v),
                    _ => None,
                })),
            }
        }
    }
    let control = ctx.rt.control();
    let (forks, build_forks) = (control.forks(), control.build_forks());
    ctx.shutdown().await;
    Ok(Ran { values, forks, build_forks })
}

/// The compile error `code` gets.
async fn refusal(code: &str) -> String {
    match run_par(code, ParMode::Auto).await {
        Ok(_) => panic!("compiled: {code}"),
        Err(e) => format!("{e:#}"),
    }
}

/// A map recomputed on each of 30 ticks, with `attr` above it.
fn ticking_map(attr: &str) -> String {
    format!(
        r#"{{
            let n = 0;
            n <- select sys::time::timer(duration:5.ms, 30) ~ n {{
                k if k < 30 => k + 1,
                _ => never()
            }};
            let xs = array::init(8, |i| i + n);
            {attr}
            array::fold(array::map(xs, |x| x * 2), 0, |a, b| a + b)
        }}"#
    )
}

// Whether `Auto` forks depends on timing and on the pool, which the
// tests in this process share: `cost::tests` pins those decisions.
#[tokio::test(flavor = "current_thread")]
async fn parallel_forks_a_cheap_map() -> Result<()> {
    let ran = run_par(&ticking_map(""), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    let ran = run_par(&ticking_map("#[parallel]"), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(ran.forks > 0, "#[parallel] forked nothing");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn parallel_forks_a_fused_map() -> Result<()> {
    let ran = run_fused(&ticking_map("#[parallel]"), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(ran.forks > 0, "#[parallel] forked no kernel loop");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn parallel_on_a_definition_forks_its_instances() -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select sys::time::timer(duration:5.ms, 30) ~ n {
            k if k < 30 => k + 1,
            _ => never()
        };
        #[parallel]
        let double = |xs: Array<i64>| array::map(xs, |x| x * 2);
        array::fold(double(array::init(8, |i| i + n)), 0, |a, b| a + b)
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(ran.forks > 0, "#[parallel] on the definition forked nothing");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn serial_reaches_callees() -> Result<()> {
    let code = |attr: &str| {
        format!(
            r#"{{
                let n = 0;
                n <- select sys::time::timer(duration:5.ms, 30) ~ n {{
                    k if k < 30 => k + 1,
                    _ => never()
                }};
                let double = |xs: Array<i64>| #[parallel] array::map(xs, |x| x * 2);
                {attr}
                array::fold(double(array::init(8, |i| i + n)), 0, |a, b| a + b)
            }}"#
        )
    };
    let ran = run_par(&code(""), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(ran.forks > 0, "the callee's #[parallel] forked nothing");
    let ran = run_par(&code("#[serial]"), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert_eq!(ran.forks, 0, "#[serial] let a callee fork");
    let ran = run_fused(&code(""), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(ran.forks > 0, "the callee's #[parallel] forked no kernel loop");
    let ran = run_fused(&code("#[serial]"), ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert_eq!(ran.forks, 0, "#[serial] let a callee's kernel loop fork");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn first_cycle_growth_agrees() -> Result<()> {
    let code = r#"{
        let rec f = |d: i64, v: i64| -> i64 select d {
            0 => v,
            d => (f(d - 1, v + 1) + f(d - 1, v * 3)) % 1000003
        };
        array::fold(array::map(array::init(8, |i| i), |x| f(9, x)), 0, |a, b| a + b)
    }"#;
    let serial = run_par(code, ParMode::Off).await?;
    assert_eq!(run_par(code, ParMode::Auto).await?.values, serial.values);
    let ran = run_par(code, ParMode::Force).await?;
    assert_eq!(ran.values, serial.values);
    assert!(ran.forks > 0 && ran.build_forks > 0, "a forced growth ran in order");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn fold_growth_builds_in_tasks() -> Result<()> {
    let code = r#"{
        let xs = #[serial] array::init(200, |i| i);
        array::fold(xs, 0, |a, b| {
            let c = select (a + b) % 4 {
                0 => a / 2,
                1 => b * 5 + 1,
                n => n + a
            };
            let d = select (b, c) {
                (b, c) if b > c => b - c,
                (b, c) => c - b
            };
            (a + b + c + d) % 65537
        })
    }"#;
    let serial = run_par(code, ParMode::Off).await?;
    assert_eq!(serial.build_forks, 0);
    assert_eq!(run_par(code, ParMode::Auto).await?.values, serial.values);
    let ran = run_par(code, ParMode::Force).await?;
    assert_eq!(ran.values, serial.values);
    assert!(ran.build_forks > 0, "the fold built its slots in order");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn parallel_names_the_dependency() {
    let e = refusal(
        r#"#[parallel] {
            let a = 1;
            let b = a + 1;
            b
        }"#,
    )
    .await;
    assert!(e.contains("statement 2 reads `a`"), "{e}");
}

#[tokio::test(flavor = "current_thread")]
async fn parallel_needs_something_to_fork() {
    let e = refusal("#[parallel]\n42").await;
    assert!(e.contains("#[parallel] has nothing to run in parallel"), "{e}");
}

#[tokio::test(flavor = "current_thread")]
async fn fork_attribute_arguments() {
    for (code, msg) in [
        ("#[parallel(0)]\n[1, 2]", "positive integer literal"),
        ("#[parallel(x)]\n[1, 2]", "positive integer literal"),
        ("#[serial(1)]\n[1, 2]", "takes no arguments"),
        ("#[parallel]\n#[serial]\n[1, 2]", "at most one of"),
    ] {
        let e = refusal(code).await;
        assert!(e.contains(msg), "{code}: {e}");
    }
}
