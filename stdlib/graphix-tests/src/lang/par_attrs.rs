// `#[parallel]` and `#[serial]` (design/parallel_eval.md §7).

use anyhow::{Result, bail};
use graphix_compiler::{CFlag, ParMode};
use graphix_package_core::testing::init_with_flags_and_setup;
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

/// Run `code` (as `let result = {code}`) node-walked under `mode` until it
/// is quiet for 300ms: the result's updates and the forks the runtime
/// made. A kernel does not fork.
async fn run_par(code: &str, mode: ParMode) -> Result<(Vec<Value>, u64)> {
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
        CFlag::FusionDisabled.into(),
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
    let forks = ctx.rt.control().forks();
    ctx.shutdown().await;
    Ok((values, forks))
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

#[tokio::test(flavor = "current_thread")]
async fn parallel_forks_where_auto_would_not() -> Result<()> {
    let (values, forks) = run_par(&ticking_map(""), ParMode::Auto).await?;
    assert_eq!(values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert_eq!(forks, 0, "the cost model forked a cheap map");
    let (values, forks) = run_par(&ticking_map("#[parallel]"), ParMode::Auto).await?;
    assert_eq!(values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(forks > 0, "#[parallel] forked nothing");
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
    let (values, forks) = run_par(code, ParMode::Auto).await?;
    assert_eq!(values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(forks > 0, "#[parallel] on the definition forked nothing");
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
    let (values, forks) = run_par(&code(""), ParMode::Auto).await?;
    assert_eq!(values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert!(forks > 0, "the callee's #[parallel] forked nothing");
    let (values, forks) = run_par(&code("#[serial]"), ParMode::Auto).await?;
    assert_eq!(values.last(), Some(&Value::I64(2 * (28 + 8 * 30))));
    assert_eq!(forks, 0, "#[serial] let a callee fork");
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
