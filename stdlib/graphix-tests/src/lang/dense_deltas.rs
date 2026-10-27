// Dense delivery (design/dense_delivery.md): bottom is a production and
// a builtin arg never rides its previous value. Do not adjust these
// expectations without a ruling.

use anyhow::Result;
use graphix_package_core::{
    PrintSink,
    testing::{
        FuseExpect, Mode, TestState, as_i64s, check_fixture, compile_result,
        fixture_runtime, result_source, updates_until_quiet, verdicts_agree,
    },
};
use netidx::publisher::Value;
use std::time::Duration;
use tokio::time::Instant;

/// What a run made.
pub(super) struct Delta {
    /// Every update of the result.
    pub values: Vec<Value>,
    /// The captured print output.
    pub out: String,
    /// Forks of evaluation.
    pub forks: u64,
    /// Runs of compile tasks building instances.
    pub build_forks: u64,
    /// Fork-point verdicts worked out at an update, past the analysis.
    pub verdicts: u64,
    /// Fused-kernel and JIT invocations of the fixture's own program.
    pub invocations: (u64, u64),
    /// Prune boundaries walked and skipped.
    pub boundaries: (u64, u64),
    /// The regions the build fused.
    pub regions: usize,
}

/// Run `code` (wrapped as `let result = {code}`) in `mode` until the
/// runtime settles (else, under a standing source, until no event arrives
/// for 700ms), collecting every update of the result and the captured
/// print output.
pub(super) async fn run_delta(code: &str, mode: Mode) -> Result<(Vec<Value>, String)> {
    let d = run_delta_with(code, mode, |_| (), Duration::from_millis(700)).await?;
    Ok((d.values, d.out))
}

/// [`run_delta`] with `setup` run over the state before the runtime
/// starts, until the runtime settles or no event arrives for `quiet`.
pub(super) async fn run_delta_with(
    code: &str,
    mode: Mode,
    setup: impl FnOnce(&mut TestState),
    quiet: Duration,
) -> Result<Delta> {
    let sink = PrintSink::default();
    let seeded = sink.clone();
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        move |ctx| {
            ctx.libstate.set(seeded);
            setup(ctx)
        },
    )
    .await?;
    let checked = match mode {
        Mode::Interp => Some(check_fixture(&ctx).await),
        _ => None,
    };
    let control = ctx.rt.control();
    control.reset_invocations();
    // the stdlib root's own cycles fork too: count the fixture's
    let (forks_before, build_forks_before) = (control.forks(), control.build_forks());
    let built = compile_result(&ctx).await;
    if let Some(checked) = checked {
        verdicts_agree(checked, built.as_ref().map(|_| ()))?;
    }
    let res = built?;
    let regions = ctx.fusion_stats().await.map(|s| s.fused).unwrap_or(0);
    let deadline = Instant::now() + Duration::from_secs(20);
    let values =
        updates_until_quiet(&ctx.rt, &mut rx, res.exprs[0].id, quiet, deadline).await?;
    let out = sink.take();
    let forks = control.forks() - forks_before;
    let build_forks = control.build_forks() - build_forks_before;
    let (verdicts, invocations) = (control.verdicts(), control.invocations());
    let boundaries = control.boundaries();
    ctx.shutdown().await;
    Ok(Delta {
        values,
        out,
        forks,
        build_forks,
        verdicts,
        invocations,
        boundaries,
        regions,
    })
}

/// [`run_delta`] for a fixture that holds on both engines: in the jit
/// modes, `expect` says whether the program fuses, so a jit run that
/// node-walks everything cannot pass for a kernel's.
pub(super) async fn run_delta_fused(
    code: &str,
    mode: Mode,
    expect: FuseExpect,
) -> Result<(Vec<Value>, String)> {
    run_delta_fused_with(code, mode, |_| (), expect).await
}

/// [`run_delta_fused`] over a context `setup` prepares.
pub(super) async fn run_delta_fused_with(
    code: &str,
    mode: Mode,
    setup: impl FnOnce(&mut TestState),
    expect: FuseExpect,
) -> Result<(Vec<Value>, String)> {
    let d = run_delta_with(code, mode, setup, Duration::from_millis(700)).await?;
    #[cfg(debug_assertions)]
    if matches!(mode, Mode::Jit | Mode::JitPar) {
        graphix_package_core::testing::check_fuse_expectation(
            graphix_package_core::testing::FuseObserved {
                regions: Some(d.regions),
                runs: d.invocations,
            },
            expect,
        )
    }
    #[cfg(not(debug_assertions))]
    let _ = expect;
    Ok((d.values, d.out))
}

// A constant print message fires once in both engines.
const PRINT_CONST_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 5 => n <- (x ~ n) + 1, _ => never() };
  let r = { println("A"); n };
  r
}"#;

async fn print_const_once(mode: Mode) -> Result<()> {
    let (values, out) = run_delta_fused(PRINT_CONST_ONCE, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 1, 2, 3, 4, 5]);
    assert_eq!(out, "A\n");
    Ok(())
}

modes!(print_const_once);

// A callback's print fires once per element, not per kernel invocation.
// The slots print unordered.
const PRINT_HOF_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 3 => n <- (x ~ n) + 1, _ => never() };
  let a = [i64:1, i64:2];
  let r = { let m = array::map(a, |x| { println("@P[x]"); x * i64:2 }); (n, array::len(m)) };
  println("@R[r]");
  i64:0
}"#;

async fn print_hof_once(mode: Mode) -> Result<()> {
    let (values, out) = run_delta_fused(PRINT_HOF_ONCE, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0]);
    let mut lines: Vec<&str> = out.lines().collect();
    lines[..2].sort();
    assert_eq!(
        lines,
        ["@P1", "@P2", "@R(0, 2)", "@R(1, 2)", "@R(2, 2)", "@R(3, 2)"],
        "{out}"
    );
    Ok(())
}

modes!(print_hof_once);

// A bottomed builtin arg bottoms the invocation: epochs give
// [1, 9] and the bottoming third epoch emits nothing.
const BOTTOM_PROPAGATES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 2 => n + 1, _ => never() };
  let in0 = select ep { 2 => i64:1, _ => i64:0 };
  let in1 = select ep { 1 => true, _ => false };
  let v0 = i64:1 - in0;
  select in1 { true => i64:9, _ => max(in0 * i64:10, i64:1 / v0) }
}"#;

async fn builtin_bottom_propagates(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(BOTTOM_PROPAGATES, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 9]);
    Ok(())
}

modes!(builtin_bottom_propagates);

// A dynamic call whose function is briefly bottom: the argument that
// moved in the window reaches the instance when the same function
// returns, which fires the call: [0, 14].
const CALLEE_BACK_FROM_BOTTOM: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 4 => n + 1, _ => never() };
  let g = |n: i64| n * 2;
  let cb: [fn(n: i64) -> i64, null] = select ep { 2 | 3 => null, _ => g };
  let x = 0;
  x <- select ep { 2 => 7, _ => never() };
  let f = |cb: [fn(n: i64) -> i64, null], x| cb$(x);
  f(cb, x)
}"#;

async fn callee_back_from_bottom(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(CALLEE_BACK_FROM_BOTTOM, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 14]);
    Ok(())
}

modes!(callee_back_from_bottom);

// An argument that went bottom while the function was bottom is bottom
// when the function returns: the call stays bottom, never `[null]`.
const CALLEE_BACK_ARG_STILL_BOTTOM: &str = r#"{
  let c = array::iter([0, 1, 2, 3]);
  let k = 5;
  k <- select c { 0 => 0, _ => never() };
  let y = select k { 0 => never(), k => k };
  let f = |b: i64| [b];
  let h = select c { 1 => never(), _ => f };
  h(y)
}"#;

async fn callee_back_arg_still_bottom(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(CALLEE_BACK_ARG_STILL_BOTTOM, mode).await?;
    assert_eq!(values, vec![Value::Array([Value::I64(5)].into())]);
    Ok(())
}

modes!(callee_back_arg_still_bottom);

// A callee the returning function's instance binds fresh, its function
// value having changed while the function was bottom, is owed the fires
// its body's inputs had meanwhile.
const CALLEE_BACK_FRESH_INNER_CALLEE: &str = r#"{
  let a = 0;
  let which = 0;
  let gd = select which { 0 => |x| x - 100, _ => |x| a ~ x };
  let n = 0;
  n <- select n { k if k < 8 => k + 1, _ => never() };
  a <- select n { 4 => 7, _ => never() };
  which <- select n { 4 => 1, _ => never() };
  let h = |x| gd(x);
  let f = select n { k if (k > 2) && (k < 7) => never(), _ => h };
  f(1)
}"#;

async fn callee_back_fresh_inner_callee(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(CALLEE_BACK_FRESH_INNER_CALLEE, mode).await?;
    assert_eq!(values.last(), Some(&Value::I64(1)), "{values:?}");
    Ok(())
}

modes!(callee_back_fresh_inner_callee);

// A stateful callee whose function value is bottom for a while, or a
// collection slot whose source is, pauses as the select-arm twin's body
// does: its `<-` state survives, and the input that fired meanwhile is
// caught up once at the wake.
fn callee_pauses_program(call: &str) -> String {
    format!(
        r#"{{
  let ep = 0;
  ep <- select ep {{ n if n < 6 => n + 1, _ => never() }};
  let in0 = 1;
  in0 <- select ep {{ 2 => 2, 4 => 3, _ => never() }};
  let in1 = 0;
  in1 <- select ep {{ 1 => 1, 3 => 0, _ => never() }};
  let f = |x: i64| -> i64 {{ let c = 0; c <- x ~ c + 1; x + c * 1000 }};
  {call}
}}"#
    )
}

async fn callee_back_from_bottom_pauses(mode: Mode) -> Result<()> {
    for call in [
        "let cb: [fn(x: i64) -> i64, null] = select in1 { 0 => f, _ => null };\n  cb$(in0)",
        "select in1 { 0 => f(in0), _ => never() }",
        "array::map(select in1 { 0 => [0], _ => never() }, |_| f(in0))[0]$",
        "array::fold(select in1 { 0 => [0], _ => never() }, 0, |acc, _| acc + f(in0))",
    ] {
        let (values, _) =
            run_delta_fused(&callee_pauses_program(call), mode, FuseExpect::Jit).await?;
        assert_eq!(as_i64s(&values), vec![1, 1001, 1002, 2003, 3003], "{call}");
    }
    Ok(())
}

modes!(callee_back_from_bottom_pauses);

// A write through a moving reference lands once, where the reference
// points when its value fires: a retarget writes nothing already
// written, and a write after its arm wakes goes where the reference
// points now.
const MOVING_REFERENCE_WRITES_ONCE: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 6 => n + 1, _ => never() };
  let a = [1, 2, 3];
  let i = 0;
  let ra = &mut a[i];
  let x = 1;
  let y = 2;
  let c = false;
  let rx = select c { false => &mut x, true => &mut y };
  let v = never();
  v <- select ep { 1 => 99, _ => never() };
  *ra <- v;
  *rx <- v;
  i <- select ep { 3 => 2, _ => never() };
  c <- select ep { 3 => true, _ => never() };
  let rows = [1, 2, 3];
  let in0 = 0;
  in0 <- select ep { 1 => 1, 2 => 0, 4 => 1, _ => never() };
  let in1 = 0;
  in1 <- select ep { 3 => 2, _ => never() };
  let in2 = 0;
  in2 <- select ep { 5 => 7, _ => never() };
  let r = &mut rows[in1];
  let o = select in0 { 1 => { *r <- in2; -1 }, _ => *r + in2 };
  select ep { 6 => (a, x, y, rows, o), _ => never() }
}"#;

async fn moving_reference_writes_once(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(MOVING_REFERENCE_WRITES_ONCE, mode, FuseExpect::Jit).await?;
    let last = values.last().map(|v| v.to_string());
    assert_eq!(
        last.as_deref(),
        Some("[[i64:99, i64:2, i64:3], i64:99, i64:2, [i64:1, i64:2, i64:7], i64:-1]")
    );
    Ok(())
}

modes!(moving_reference_writes_once);

// An any whose source went bottom while its arm slept is bottom at the
// wake: [10, 5, -1] and nothing at the wake.
const ANY_WAKES_BOTTOM: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 5 => n + 1, _ => never() };
  let in0 = 0;
  in0 <- select ep { 3 => 1, 5 => 0, _ => never() };
  let in1 = 1;
  in1 <- select ep { 2 => 2, 4 => 0, _ => never() };
  let v0 = 10 / in1;
  select in0 { 0 => any(v0, never<i64>()), _ => -1 }
}"#;

async fn any_wakes_bottom(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(ANY_WAKES_BOTTOM, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![10, 5, -1]);
    Ok(())
}

modes!(any_wakes_bottom);

// A uniq holds nothing across a bottom: the value it emitted before the
// bottom is emitted again after it.
const UNIQ_ACROSS_BOTTOM: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 4 => n + 1, _ => never() };
  let in1 = 1;
  in1 <- select ep { 2 => 2, 3 => 0, 4 => 2, _ => never() };
  uniq(10 / in1)
}"#;

async fn uniq_across_bottom(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(UNIQ_ACROSS_BOTTOM, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![10, 5, 5]);
    Ok(())
}

modes!(uniq_across_bottom);

// A window with no arm (here a bottom scrutinee) pauses the selected
// arm, which resumes with its state: presses goes on from where it
// stood and entries, sampled on the scrutinee's delivery, counts the
// re-entry.
const NO_ARM_WINDOW_PAUSES: &str = r#"{
  let ev = 0;
  ev <- select ev { k if k < 8 => k + 1, _ => never() };
  let mode: [`Detail, `List] = `List;
  let sel: [i64, null] = 1;
  mode <- select ev { 2 => `Detail, 5 => `List, _ => never() };
  sel <- select ev { 5 => null, 6 => 1, _ => never() };
  let entries = 0;
  select mode {
    `List => select sel$ {
      i => {
        let presses = 0;
        presses <- ev ~ presses + 1;
        entries <- (i ~ entries) + 1;
        (i, entries, presses)
      }
    },
    `Detail => never()
  }
}"#;

async fn no_arm_window_pauses(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(NO_ARM_WINDOW_PAUSES, mode, FuseExpect::Jit).await?;
    let shown: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(
        shown,
        [
            "[i64:1, i64:0, i64:0]",
            "[i64:1, i64:1, i64:1]",
            "[i64:1, i64:1, i64:2]",
            "[i64:1, i64:1, i64:3]",
            "[i64:1, i64:2, i64:4]",
            "[i64:1, i64:2, i64:5]"
        ]
    );
    Ok(())
}

modes!(no_arm_window_pauses);

// An untaken guarded arm's binds leave no value behind: a closure made
// in the arm reads only what the arm matched, and fires once.
const UNTAKEN_GUARD_BINDS_NOTHING: &str = r#"{
  let n = 0;
  n <- select n { k if k < 6 => k + 1, _ => never() };
  let f: fn(u: i64) -> i64 = never();
  f <- select (n, n == 2) { (k, b) if b => |u: i64| k * 100 + u, _ => never() };
  (f(n), f(0), count(f(0)))
}"#;

async fn untaken_guard_binds_nothing(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(UNTAKEN_GUARD_BINDS_NOTHING, mode, FuseExpect::Jit).await?;
    let last = values.last().map(|v| v.to_string());
    assert_eq!(last.as_deref(), Some("[i64:206, i64:200, i64:1]"));
    Ok(())
}

modes!(untaken_guard_binds_nothing);
