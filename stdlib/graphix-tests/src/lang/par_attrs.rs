// `#[parallel]` and `#[serial]` (design/parallel_eval.md §7), and how a
// collection's growth runs under `Auto` (§8).

use super::dense_deltas::{Delta, run_delta_with};
use anyhow::Result;
use graphix_compiler::ParMode;
use graphix_package_core::testing::Mode;
use netidx::publisher::Value;
use std::time::Duration;

/// Run `code` (as `let result = {code}`) node-walked under `par` until it
/// is quiet for 300ms.
async fn run_par(code: &str, par: ParMode) -> Result<Delta> {
    run_with(code, par, Mode::Interp).await
}

/// [`run_par`] fused: a kernel's loops fork as chunks of its slots.
async fn run_fused(code: &str, par: ParMode) -> Result<Delta> {
    run_with(code, par, Mode::Jit).await
}

/// Run `code` built as `mode` builds it, forking under `par`, until it is
/// quiet for 300ms. Under `Auto` the run waits for the fork threshold's
/// calibration first: nothing forks until it.
async fn run_with(code: &str, par: ParMode, mode: Mode) -> Result<Delta> {
    if par == ParMode::Auto {
        while graphix_compiler::cost::calibration().is_none() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    }
    run_delta_with(
        code,
        mode,
        move |ctx| ctx.control.set_par_mode(par),
        Duration::from_millis(300),
    )
    .await
}

/// The compile error `code` gets; a refused program forks nothing, so it
/// needs no calibration.
async fn refusal(code: &str) -> String {
    match run_par(code, ParMode::Off).await {
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

// Parentheses around the definition are grouping: the attribute still
// reaches its body.
#[tokio::test(flavor = "current_thread")]
async fn parallel_on_a_parenthesized_definition_forks() -> Result<()> {
    let code = r#"{
        let g = |x: i64| x * 2;
        let h = #[parallel] (|x: i64| (g(x), g(x + 1)));
        let (a, b) = h(3);
        a + b
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(14)));
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

// #[parallel] counts only its own fork points: the map inside the callee
// does not make the call parallel.
#[tokio::test(flavor = "current_thread")]
async fn parallel_stops_at_callees() {
    let e = refusal(
        "{ let double = |xs: Array<i64>| array::map(xs, |x| x * 2); #[parallel] double([1, 2, 3]) }",
    )
    .await;
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

// A constant beside a call is nothing to run in parallel.
#[tokio::test(flavor = "current_thread")]
async fn parallel_block_needs_two_that_work() {
    let e = refusal(
        "{ let g = |x: i64| x * 2; let n = 3; #[parallel] { let k = 1; let a = g(n); a + k } }",
    )
    .await;
    assert!(e.contains("#[parallel] has nothing to run in parallel"), "{e}");
}

// A reference publishes its cell: the read through it is a later run.
#[tokio::test(flavor = "current_thread")]
async fn parallel_reference_publishes() {
    let e = refusal(
        "{ let n = 1; let holder = 0; let r = &holder; #[parallel] { r <- &(n * 10); let v = *r; v } }",
    )
    .await;
    assert!(e.contains("reads through a reference"), "{e}");
}

// Nothing under a seq or a #[serial] forks.
#[tokio::test(flavor = "current_thread")]
async fn parallel_where_nothing_forks() {
    let e = refusal(
        "{ let g = |x: i64| x * 2; let n = 3; seq { let v = #[parallel] (g(n), g(n + 1)); v } }",
    )
    .await;
    assert!(e.contains("#[parallel] is not allowed inside a seq"), "{e}");
    let e = refusal(
        "{ let g = |x: i64| x * 2; let n = 3; #[serial] { let v = #[parallel] (g(n), g(n + 1)); v } }",
    )
    .await;
    assert!(e.contains("#[parallel] under #[serial] never forks"), "{e}");
}

// A fork attribute leaves the expression's other attributes in force: a
// definition assertion beside it is checked, and #[native] on a body a
// fork governs is checked on the body.
#[tokio::test(flavor = "current_thread")]
async fn fork_keeps_the_other_attributes() {
    let e = refusal("{ let g = #[serial] #[tail_recursive] |x: i64| x + 1; g(1) }").await;
    assert!(e.contains("#[tail_recursive]"), "{e}");
    let e =
        refusal("{ #[serial] let f = #[tail_recursive] (|x: i64| x + 1); f(1) }").await;
    assert!(e.contains("#[tail_recursive]"), "{e}");
}

// A block of self-calls is planned with its instance in the graph: the
// recursive calls are not opaque, a self-call's reads of the activation's
// own lets are the new activation's, so the two lets fork.
#[tokio::test(flavor = "current_thread")]
async fn self_calls_in_a_block_fork() -> Result<()> {
    let code = r#"{
        let rec fib = |n: i64| -> i64 select n < 2 {
            true => n,
            false => #[parallel] { let a = fib(n - 1); let b = fib(n - 2); a + b }
        };
        fib(10)
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(55)));
    assert!(ran.forks > 0, "the block of self-calls ran in order");
    Ok(())
}

// An activation's fork points are decided at its bind, its call to the
// next activation a self-call: the update after an argument changes
// works nothing out at the chain's operators. Asked there, each operator
// walked every activation below it, cubic in the depth.
#[tokio::test(flavor = "current_thread")]
async fn activations_decide_their_fork_points_at_bind() -> Result<()> {
    let code = r#"{
        let rec f = |n: i64, s: string| -> i64 select n {
            0 => 0,
            n => select str::contains(#part: "b[n]", s) { true => 2, false => 1 } + f(n - 1, s)
        };
        let s = "aaaa";
        s <- sys::time::timer(duration:5.ms, false) ~ str::concat(s, "b7");
        f(300, s)
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(ran.values, [Value::I64(300), Value::I64(301)]);
    assert_eq!(ran.verdicts, 0, "a fork point was decided at an update");
    Ok(())
}

// A verdict that rests on a call never bound is asked again only after
// something binds, not at every update.
#[tokio::test(flavor = "current_thread")]
async fn an_open_verdict_waits_for_a_bind() -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select sys::time::timer(duration:1.ms, 40) ~ n {
            k if k < 40 => k + 1,
            _ => never()
        };
        let fs = [|x: i64| x * 2];
        let xs = array::init(4, |i| i + n);
        array::fold(array::map(xs, |x| select x { -1 => (fs[0]$)(x), x => x * 3 + x * 5 }), 0, |a, b| a + b)
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(8 * (6 + 4 * 40))));
    assert!(ran.verdicts <= 4, "{} verdicts in 40 cycles", ran.verdicts);
    Ok(())
}

// A verdict that rests on a dynamic call is decided whether or not the
// call is bound, since bound it is as opaque: a null callback leaves
// nothing open for each cycle's rebuild to ask again.
#[tokio::test(flavor = "current_thread")]
async fn an_unbound_dynamic_call_leaves_no_verdict_open() -> Result<()> {
    let code = r#"{
        let f = |x: i64| x + 1;
        let cb: [fn(x: i64) -> i64, null] = null;
        let n = 0;
        n <- select sys::time::timer(duration:1.ms, 40) ~ n {
            k if k < 40 => k + 1,
            _ => never()
        };
        let grow = array::init(n, |i| i * 2);
        ({ let y = n * 2; f(y) }, select cb { null => 0, g => g(n) }, array::len(grow))
    }"#;
    let ran = run_par(code, ParMode::Auto).await?;
    assert_eq!(
        ran.values.last(),
        Some(&Value::Array([81i64, 0, 40].map(Value::I64).into()))
    );
    assert!(ran.verdicts <= 4, "{} verdicts in 40 cycles", ran.verdicts);
    Ok(())
}

// Fields, arguments and operands that reach one ordered queuefn wrapper
// do not fork, so #[parallel] over them has nothing to run in parallel.
#[tokio::test(flavor = "current_thread")]
async fn parallel_refuses_ordered_siblings() {
    for e in [
        "(qf(1), qf(2), qf(3))",
        "g(qf(1), qf(2))",
        "qf(1) + qf(2)",
        "array::map([1, 2, 3], |x| qf(x))",
    ] {
        let code = format!(
            "{{ let qf = queuefn(#trigger: 1, |x: i64| -> i64 x); \
             let g = |a: i64, b: i64| a + b; #[parallel] {e} }}"
        );
        let err = refusal(&code).await;
        assert!(err.contains("#[parallel] has nothing to run in parallel"), "{e}: {err}");
    }
}

// A block forks only a run of two statements that are work, decorated
// or nested in what is.
#[tokio::test(flavor = "current_thread")]
async fn parallel_refuses_a_block_of_constants() {
    for e in ["{ let a = 1; let b = 2; a + b }", "(t, { let a = 1; let b = 2; a + b })"] {
        let code = format!("{{ let t = 3; #[parallel] {e} }}");
        let err = refusal(&code).await;
        assert!(err.contains("#[parallel] has nothing to run in parallel"), "{e}: {err}");
    }
}

// A call through a bound queuefn wrapper is ordered at run time too: a
// dynamic call's written argument and the default its bind compiles both
// call the wrapper, so forking keeps the serial queue order.
#[tokio::test(flavor = "current_thread")]
async fn bound_queuefn_calls_stay_ordered() -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select n { x if x < 12 => x + 1, _ => never() };
        let args: Array<i64> = [];
        let q = queuefn(#trigger: n, |x: i64| -> i64 { args <- x ~ array::push(args, x); x });
        let f = |#d: i64 = q(1000 + n), a: i64, b: i64| -> i64 a + b + d;
        let g = select n { x if x < 0 => f, _ => f };
        let s = g(q(n), 2);
        select n { 12 => args, _ => never() }
    }"#;
    let serial = run_par(code, ParMode::Off).await?;
    for _ in 0..4 {
        let forked = run_par(code, ParMode::Force).await?;
        assert_eq!(forked.values, serial.values);
    }
    Ok(())
}

// A reference publishes its cell, and a moving place its path, in the
// cycle it fires: a later sibling reading it through `*r` is not
// independent of it, as a call's argument or a tuple's field.
#[tokio::test(flavor = "current_thread")]
async fn a_reference_and_its_reader_stay_ordered() -> Result<()> {
    for code in [
        r#"{
            let i = array::iter([1, 2, 3, 4, 5, 6]);
            let held: [&i64, null] = null;
            let pair = |r: &i64, v: i64| (r, v);
            let t = pair(&(i * 10), select held { null as _ => -1, r => *r });
            held <- once(t.0);
            t.1
        }"#,
        r#"{
            let a = [10, 20, 30, 40];
            let i = 0;
            i <- select i { 6 => never(), _ => i + 1 };
            let r = &a[0];
            let t = ({ r <- &a[i % 4]; i }, *r);
            t.1
        }"#,
    ] {
        let serial = run_par(code, ParMode::Off).await?;
        assert!(serial.values.len() > 3, "{code}");
        for _ in 0..3 {
            assert_eq!(
                run_par(code, ParMode::Force).await?.values,
                serial.values,
                "{code}"
            );
        }
    }
    let err = refusal(
        r#"{
            let a = [10, 20, 30, 40];
            let i = array::iter([0, 1, 2, 3]);
            let held: [&i64, null] = null;
            let t = #[parallel] (&a[i], select held { null as _ => -1, r => *r });
            held <- t.0;
            t.1
        }"#,
    )
    .await;
    assert!(err.contains("#[parallel] has nothing to run in parallel"), "{err}");
    Ok(())
}

// A block's runs fork: here nothing but the block's statements can.
#[tokio::test(flavor = "current_thread")]
async fn block_runs_fork() -> Result<()> {
    // CR claude for claude: [test-gap/medium] Under Force, `ran.forks` includes the
    // forks of the stdlib root's init cycles: 73 before the fixture compiles. So
    // `assert!(ran.forks > 0)` here, and the `forks > 0` half of
    // first_cycle_growth_agrees, pass even if a block's runs never fork (a fixture of
    // `42` already reports 73). `dense_deltas::run_delta_with` should read forks and
    // build_forks just before `compile_result` and return the differences, as `run!`
    // does with forks_before. probe:
    // design/review-2026-10-26/repro/tests-lang-rest-a-par-attrs-force-fork-baseline.rs
    // (tests-lang-rest-a-02)
    let ran = run_par("{ let a = 1; let b = 2; b }", ParMode::Force).await?;
    assert_eq!(ran.values.last(), Some(&Value::I64(2)));
    assert!(ran.forks > 0, "the block's run ran in order");
    Ok(())
}

/// The forks a program `{ mod m; m::b }` makes under `Force`, `m`'s body
/// `module` behind an interface (a module without one is a block), and
/// its value.
async fn module_forks(module: &'static str) -> Result<(u64, Option<Value>)> {
    use graphix_package_core::testing::{
        compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    let (ctx, mut rx) = fixture_runtime(
        [
            ("/test.gx", result_source("{ mod m; m::b }")),
            ("/test/m.gxi", arcstr::literal!("val b: i64;").into()),
            ("/test/m.gx", arcstr::ArcStr::from(module).into()),
        ],
        &crate::TEST_REGISTER,
        Mode::Interp,
        |ctx| ctx.control.set_par_mode(ParMode::Force),
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(20);
    let id = res.exprs[0].id;
    let quiet = Duration::from_millis(300);
    let values = updates_until_quiet(&ctx.rt, &mut rx, id, quiet, deadline).await?;
    let forks = ctx.rt.control().forks();
    ctx.shutdown().await;
    Ok((forks, values.last().cloned()))
}

// A module's body runs as a block's does: under Force its eight
// independent statements fork (one flat fork) where one statement cannot.
#[tokio::test(flavor = "current_thread")]
async fn module_runs_fork() -> Result<()> {
    let (one, v1) = module_forks("let b = 2").await?;
    let eight = "let a1 = 1;\nlet a2 = 1;\nlet a3 = 1;\nlet a4 = 1;\n\
                 let a5 = 1;\nlet a6 = 1;\nlet a7 = 1;\nlet b = 2";
    let (many, v2) = module_forks(eight).await?;
    assert_eq!((v1, v2), (Some(Value::I64(2)), Some(Value::I64(2))));
    assert!(
        many > one,
        "the module's statements ran in order: {many} forks, {one} alone"
    );
    Ok(())
}

// A reference made in a callee's body publishes its cell in the cycle it
// fires, so a later sibling that reads through a reference to it is not
// independent of the call.
const CALLEE_REF: &str = r#"{
    let n = 0;
    n <- select n { k if k < 4 => k + 1, _ => never() };
    let mk = || -> &i64 &(n * 10);
    let saved: [&i64, null] = null;
    let pair = (mk(), select saved { null as _ => -1, r => *r });
    saved <- once(pair.0);
    pair.1
}"#;

#[tokio::test(flavor = "current_thread")]
async fn callee_reference_is_a_publication() -> Result<()> {
    let serial = run_par(CALLEE_REF, ParMode::Off).await?;
    let forked = run_par(CALLEE_REF, ParMode::Force).await?;
    assert_eq!(forked.values, serial.values);
    let parallel = CALLEE_REF.replace("let pair = (", "let pair = #[parallel] (");
    let err = refusal(&parallel).await;
    assert!(err.contains("#[parallel] has nothing to run in parallel"), "{err}");
    Ok(())
}

// A call with no static target may call something else at its next bind,
// so siblings that make one are not independent: once `h` calls the
// queuefn wrapper the queue still sees the fields' calls in order, whether
// `h` was a lambda or a builtin before.
#[tokio::test(flavor = "current_thread")]
async fn rebound_dynamic_calls_stay_ordered() -> Result<()> {
    for g1 in ["|x: i64| -> i64 x", "once"] {
        let code = format!(
            r#"{{
        let n = 0;
        n <- select n {{ k if k < 8 => k + 1, _ => never() }};
        let got: Array<i64> = [];
        let q = queuefn(#trigger: n, |x: i64| -> i64 {{ got <- x ~ array::push(got, x); x }});
        let g1 = {g1};
        let g2 = |x: i64| -> i64 q(x);
        let h = select n {{ k if k < 3 => g1, _ => g2 }};
        let t = (h(n * 10 + 1), h(n * 10 + 2), h(n * 10 + 3), h(n * 10 + 4));
        select n {{ 8 => got, _ => never() }}
    }}"#
        );
        let serial = run_par(&code, ParMode::Off).await?;
        for _ in 0..4 {
            let forked = run_par(&code, ParMode::Force).await?;
            assert_eq!(forked.values, serial.values, "g1 = {g1}");
        }
    }
    Ok(())
}
