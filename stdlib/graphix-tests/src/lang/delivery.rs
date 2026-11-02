// Deliveries to a binding (`ExecCtx::deliver`): a fire is stored as the
// event holds it, stamped this cycle; a quiet production stands in the
// store with the stamp of the last fire, so a wake owes no fire for it;
// a binding's fire schedules the roots that read it.

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::expr::VfsEntry;
use graphix_package_core::testing::{
    Collect, HANG_GUARD, Mode, compile_result, data, fixture_runtime, result_source,
    updates_until,
};
use netidx::publisher::Value;
use std::time::Duration;
use tokio::time::Instant;

const QUIET: Collect = Collect::Quiet(Duration::from_millis(700));

fn shown(values: Vec<Value>) -> Vec<String> {
    values.into_iter().map(|v| data(v).to_string().replace("i64:", "")).collect()
}

/// `/test.gx` (`let result = {code}`) beside `files`, run in `mode` until
/// the runtime settles: every update of the result.
pub(super) async fn result_updates(
    code: &str,
    files: &[(&str, &str)],
    mode: Mode,
) -> Result<Vec<String>> {
    let files = files.iter().map(|(p, c)| (*p, VfsEntry::from(ArcStr::from(*c))));
    let all = std::iter::once(("/test.gx", result_source(code))).chain(files);
    let (ctx, mut rx) = fixture_runtime(all, &crate::TEST_REGISTER, mode, |_| ()).await?;
    let res = compile_result(&ctx).await?;
    let deadline = Instant::now() + HANG_GUARD;
    let values =
        updates_until(&ctx.rt, &mut rx, res.exprs[0].id, QUIET, deadline).await?;
    ctx.shutdown().await;
    Ok(shown(values))
}

/// Each of `inputs` compiled as a root of its own, in order, as REPL
/// inputs are: every update of the input at `probe` until the runtime
/// settles.
async fn input_updates(
    files: &[(&str, &str)],
    inputs: &[&str],
    probe: usize,
    mode: Mode,
) -> Result<Vec<String>> {
    let files = files.iter().map(|(p, c)| (*p, VfsEntry::from(ArcStr::from(*c))));
    let (ctx, mut rx) =
        fixture_runtime(files, &crate::TEST_REGISTER, mode, |_| ()).await?;
    let mut held = Vec::with_capacity(inputs.len());
    for input in inputs {
        held.push(ctx.rt.compile(ArcStr::from(*input)).await?);
    }
    let deadline = Instant::now() + HANG_GUARD;
    let id = held[probe].exprs[0].id;
    let values = updates_until(&ctx.rt, &mut rx, id, QUIET, deadline).await?;
    drop(held);
    ctx.shutdown().await;
    Ok(shown(values))
}

/// Written last, so every root before it reads `n` from its first value.
const DRIVER: &str = "n <- select n { k if k < 3 => k + 1, _ => never() }";

// A let's fire schedules a root that reads it and nothing else.
async fn let_fire_schedules_its_readers(mode: Mode) -> Result<()> {
    let got =
        input_updates(&[], &["let n = 0", "let m = n + 0", "m * 10", DRIVER], 2, mode)
            .await?;
    assert_eq!(got, ["0", "10", "20", "30"], "{mode:?}");
    Ok(())
}

modes!(let_fire_schedules_its_readers);

// A reference cell's fire schedules a root that dereferences it, and the
// cell stands from its birth for a reader born later.
async fn reference_cell_fire_schedules_its_readers(mode: Mode) -> Result<()> {
    let inputs = ["let n = 0", "let r = &(n + 0)", "*r * 10", DRIVER];
    let got = input_updates(&[], &inputs, 2, mode).await?;
    assert_eq!(got, ["0", "10", "20", "30"], "{mode:?}");
    Ok(())
}

modes!(reference_cell_fire_schedules_its_readers);

const MODULE_V: &[(&str, &str)] =
    &[("/a.gx", "use super::n; let v = n + 0"), ("/a.gxi", "val v: i64")];

// The val of a module with an interface fires with its body's binding,
// and schedules a root that reads it.
async fn module_val_fire_schedules_its_readers(mode: Mode) -> Result<()> {
    let got =
        input_updates(MODULE_V, &["let n = 0", "mod a", "a::v * 10", DRIVER], 2, mode)
            .await?;
    assert_eq!(got, ["0", "10", "20", "30"], "{mode:?}");
    Ok(())
}

modes!(module_val_fire_schedules_its_readers);

// The seq machine enters a step in the cycle its predecessor completed:
// three steps read `n` in the cycle one does.
async fn seq_steps_enter_in_the_cycle_their_predecessor_completed(
    mode: Mode,
) -> Result<()> {
    let code = |body: &str| {
        format!(
            "{{
  let n = 0;
  n <- select n {{ k if k < 4 => k + 1, _ => never() }};
  let t = select n {{ 1 => 1, _ => never() }};
  seq let k = t {{ {body} }}
}}"
        )
    };
    let one = result_updates(&code("(k, n)"), &[], mode).await?;
    assert_eq!(one, ["[1, 2]"], "{mode:?}");
    let three = code("let a = k + 1; let b = a + 1; (b, n)");
    assert_eq!(result_updates(&three, &[], mode).await?, ["[3, 2]"], "{mode:?}");
    Ok(())
}

modes!(seq_steps_enter_in_the_cycle_their_predecessor_completed);

// A guard's fire re-matches the arm over a stale scrutinee: `x` is bound
// stale, so `count(x)` counts its birth alone.
async fn guard_rematch_binds_stale(mode: Mode) -> Result<()> {
    let got = result_updates(
        "{
  let n = 0;
  n <- select n { k if k < 3 => k + 1, _ => never() };
  let in1 = 0;
  in1 <- select n { 1 => 1, _ => never() };
  let s: [`A(i64), `B] = `A(0);
  select s { `A(x) if in1 >= 0 => count(x) + in1 * 10, _ => -1 }
}",
        &[],
        mode,
    )
    .await?;
    assert_eq!(got, ["1", "11"], "{mode:?}");
    Ok(())
}

modes!(guard_rematch_binds_stale);

/// `g` takes `|v| count(X)` while the arm of `in2` that calls it sleeps;
/// the arm wakes (n = 5) and binds that callee fresh, owed only the fires
/// of `X` that landed while it slept. `A` is the select making `X`.
const OWED: &str = "{
  let n = 0;
  n <- select n { k if k < 7 => (k ~ n) + 1, _ => never() };
  let in0 = 0;
  in0 <- select n { 3 => 1, 4 => 0, _ => never() };
  let in1 = 0;
  in1 <- select n { 3 => 1, _ => never() };
  let in2 = 1;
  in2 <- select n { 1 => 0, 5 => 1, _ => never() };
  let in3 = 0;
  in3 <- select n { 2 => 1, _ => never() };
  let g: fn(v: i64) -> i64 = |v| v + 100;
  let a = A;
  let b = select in2 { 0 => -7, _ => g(5) };
  (a, b)
}";

// A guard's re-match over a stale scrutinee (n = 3) rebinds `x` stale,
// which is no fire: the callee bound at the wake counts none.
async fn stale_rebind_is_owed_no_fire(mode: Mode) -> Result<()> {
    let a = "select s {
    `A(x) if in1 >= 0 => {
      g <- select in3 { 0 => never(), _ => |v| count(x) };
      count(x) + in1
    },
    _ => -1
  }";
    let code = OWED
        .replace("let a = A;", &format!("let s: [`A(i64), `B] = `A(0); let a = {a};"));
    let got = result_updates(&code, &[], mode).await?;
    assert_eq!(got, ["[1, 105]", "[1, -7]", "[2, -7]"], "{mode:?}");
    Ok(())
}

modes!(stale_rebind_is_owed_no_fire);

// The val of a module with an interface republishes quietly when its arm
// wakes (n = 4), which is no fire: the callee bound at the other arm's
// wake counts none, as over a module with no interface.
async fn module_quiet_republish_is_owed_no_fire(mode: Mode) -> Result<()> {
    let a = "select in0 {
    0 => {
      mod a;
      g <- select in3 { 0 => never(), _ => |v| count(a::x) };
      count(a::x)
    },
    _ => -1
  }";
    let code = OWED.replace("let a = A;", &format!("let a = {a};"));
    let expected = ["[1, 105]", "[1, -7]", "[-1, -7]", "[1, -7]"];
    let bare = result_updates(&code, &[("/test/a.gx", "let x = 0")], mode).await?;
    assert_eq!(bare, expected, "no interface, {mode:?}");
    let files = [("/test/a.gx", "let x = 0"), ("/test/a.gxi", "val x: i64")];
    let got = result_updates(&code, &files, mode).await?;
    assert_eq!(got, expected, "{mode:?}");
    Ok(())
}

modes!(module_quiet_republish_is_owed_no_fire);
