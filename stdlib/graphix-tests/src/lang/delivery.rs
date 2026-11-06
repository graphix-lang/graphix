// Deliveries to a binding (`ExecCtx::deliver`): a fire is stored as the
// event holds it, stamped this cycle; a quiet production stands in the
// store with the stamp of the last fire, so a wake owes no fire for it;
// a binding's fire schedules the roots that read it.

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::expr::VfsEntry;
use graphix_package_core::testing::{
    Collect, HANG_GUARD, Mode, collect, compile_result, data, fixture_runtime,
    result_source, updates_until,
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

/// `w` is made after `r`'s statement ran, and `r` reads it through the
/// function `h` holds: BEFORE or AFTER places `r`.
const PASSED: &str = "{
  let n = 0;
  n <- select n { k if k < 3 => k + 1, _ => never() };
  let h: fn(x: i64) -> i64 = never();
  BEFORE
  let w = n * 10;
  AFTER
  h <- |x: i64| -> i64 x + w;
  r
}";

// A fire reaches a reader the cycle had passed, one that sits before it,
// in the next cycle, as a write does; a reader after it reads it in the
// cycle it is made, as it always did.
async fn a_fire_reaches_a_passed_reader_next_cycle(mode: Mode) -> Result<()> {
    let before = PASSED.replace("BEFORE", "let r = h(1);").replace("AFTER", "");
    let got = result_updates(&before, &[], mode).await?;
    assert_eq!(got, ["1", "11", "21", "31"], "before, {mode:?}");
    let after = PASSED.replace("BEFORE", "").replace("AFTER", "let r = h(1);");
    let got = result_updates(&after, &[], mode).await?;
    assert_eq!(got, ["11", "21", "31"], "after, {mode:?}");
    Ok(())
}

modes!(a_fire_reaches_a_passed_reader_next_cycle);

/// `r`'s second arm reads `w` through `h`, and `w` is made after `r`'s
/// statement ran, once, at `n == FIRES`; at `n == ASLEEP` the arm sleeps.
const PASSED_ASLEEP: &str = "{
  let n = 0;
  n <- select n { k if k < 5 => k + 1, _ => never() };
  let h: fn(x: i64) -> i64 = never();
  let log: Array<(i64, i64)> = [];
  let r = select n {
    ASLEEP => -1,
    _ => { let v = h(1); log <- v ~ array::push(log, (n, v)); v }
  };
  let w = uniq(select n { k if k < FIRES => 0, _ => 10 });
  h <- |x: i64| -> i64 x + w;
  select n { 5 => n ~ log, _ => never() }
}";

// `w`'s one fire reaches the passed reader next cycle: there if its arm is
// awake, else at the arm's wake, once, as a fire made while it slept does,
// whether the arm falls asleep as the fire arrives or slept already.
async fn a_late_fire_reaches_a_sleeping_arm_at_its_wake(mode: Mode) -> Result<()> {
    for (asleep, fires, want) in [
        ("100", "1", "[[1, 1], [2, 11]]"),
        ("2", "1", "[[1, 1], [3, 11]]"),
        ("2 | 3", "2", "[[1, 1], [4, 11]]"),
    ] {
        let code = PASSED_ASLEEP.replace("ASLEEP", asleep).replace("FIRES", fires);
        let got = result_updates(&code, &[], mode).await?;
        assert_eq!(got, [want], "asleep at {asleep}, {mode:?}");
    }
    Ok(())
}

modes!(a_late_fire_reaches_a_sleeping_arm_at_its_wake);

// The same in a module's body: a val the interface declares is the
// binding's one variable, and the arm reading it owes the late fire the
// same with the val declared or not.
async fn a_late_fire_to_a_val_reaches_a_sleeping_arm_at_its_wake(
    mode: Mode,
) -> Result<()> {
    let body = PASSED_ASLEEP
        .replace("ASLEEP", "2")
        .replace("FIRES", "1")
        .replace("select n { 5 =>", "let out = select n { 5 =>");
    let body = body.trim().trim_start_matches('{').trim_end_matches('}');
    for gxi in ["val out: Array<(i64, i64)>", "val w: i64;\nval out: Array<(i64, i64)>"] {
        let files = [("/test/m.gx", body), ("/test/m.gxi", gxi)];
        let got = result_updates("{ mod m; m::out }", &files, mode).await?;
        assert_eq!(got, ["[[1, 1], [3, 11]]"], "{gxi}, {mode:?}");
    }
    Ok(())
}

modes!(a_late_fire_to_a_val_reaches_a_sleeping_arm_at_its_wake);

// A late value that feeds its producer back is a self-loop, as `x <- x + 1`
// is: `r` reads `w` through `h` a cycle late, and each new `r` makes a new
// `w`, one step a cycle, never settling. `c` counts the cycles.
const LATE_LOOP: &str = "{
  let c = 0;
  c <- select c { k if k < 8 => k + 1, _ => never() };
  let h: fn(x: i64) -> i64 = |x: i64| -> i64 x;
  let r = h(1);
  let w = r + 1;
  h <- |x: i64| -> i64 x + w;
  let seen = [];
  seen <- r ~ array::push(seen, [c, r, w]);
  select c { 8 => c ~ seen, _ => never() }
}";

async fn a_late_value_closing_a_loop_steps_a_cycle_at_a_time(mode: Mode) -> Result<()> {
    let got =
        collect(LATE_LOOP, &crate::TEST_REGISTER, mode, Collect::First(1), HANG_GUARD)
            .await?;
    let got = shown(got);
    assert_eq!(
        got,
        ["[[0, 1, 2], [1, 3, 4], [2, 5, 6], [3, 7, 8], [4, 9, 10], [5, 11, 12], \
             [6, 13, 14], [7, 15, 16]]"],
        "{mode:?}"
    );
    Ok(())
}

modes!(a_late_value_closing_a_loop_steps_a_cycle_at_a_time);

// An earlier input reads a later one's binding through a function value:
// each fire of `w` reaches it in the next cycle.
async fn a_fire_reaches_an_earlier_input_next_cycle(mode: Mode) -> Result<()> {
    let inputs = [
        "let n = 0",
        "let hs: Array<fn(x: i64) -> i64> = []",
        "select hs { [f, ..] => f(1), _ => 0 }",
        "let w = n * 10",
        "hs <- [|x: i64| -> i64 x + w]",
        DRIVER,
    ];
    let got = input_updates(&[], &inputs, 2, mode).await?;
    assert_eq!(got, ["0", "1", "11", "21", "31"], "{mode:?}");
    Ok(())
}

modes!(a_fire_reaches_an_earlier_input_next_cycle);

/// `r` reads, through the function `g` holds, a binding `BIND` makes after
/// `r`'s statement ran: a formal or a pattern bind, delivered in the walk.
const PASSED_WALK: &str = "{
  let n = 0;
  n <- select n { k if k < 3 => k + 1, _ => never() };
  let g: fn(y: i64) -> i64 = never();
  let r = g(0);
  BIND;
  r
}";

// A fire delivered in the walk (a formal, a pattern bind) reaches a reader
// it had passed next cycle, as one delivered to its roots does.
async fn a_walk_fire_reaches_a_passed_reader_next_cycle(mode: Mode) -> Result<()> {
    for bind in [
        "let f = |x: i64| -> i64 { g <- |y: i64| -> i64 y + x; x }; let s = f(n * 10)",
        "let s = select n * 10 { v => { g <- |y: i64| -> i64 y + v; v } }",
    ] {
        let code = PASSED_WALK.replace("BIND", bind);
        let got = result_updates(&code, &[], mode).await?;
        assert_eq!(got, ["0", "10", "20", "30"], "{bind}, {mode:?}");
    }
    Ok(())
}

modes!(a_walk_fire_reaches_a_passed_reader_next_cycle);

// A fire delivered in the walk reaches a reader in a later input in its own
// cycle, as one delivered to its roots does.
async fn a_walk_fire_reaches_a_later_input(mode: Mode) -> Result<()> {
    let inputs = [
        "let n = 0",
        "let mk = |x: i64| |y: i64| -> i64 y + x",
        "let g = mk(n * 10)",
        "g(1)",
        DRIVER,
    ];
    let got = input_updates(&[], &inputs, 3, mode).await?;
    assert_eq!(got, ["1", "11", "21", "31"], "{mode:?}");
    Ok(())
}

modes!(a_walk_fire_reaches_a_later_input);

/// `a` reads, through the function `h` comes to hold, a binding `mk`'s
/// call fires after `a`'s statement ran, in the same run of the block:
/// forked or not, it reads the fire next cycle.
const PASSED_CALLEE: &str = "{
  let n = 0;
  n <- select n { k if k < 3 => k + 1, _ => never() };
  let mk = MK;
  let h: fn(y: i64) -> i64 = |y: i64| -> i64 y;
  let a = h(1);
  let g = mk(n * 10);
  h <- g;
  a
}";

async fn a_callee_fire_reaches_a_passed_reader_forked_or_not(mode: Mode) -> Result<()> {
    for mk in [
        "|x: i64| |y: i64| -> i64 y + x",
        "|x: i64| { let z = x + 0; |y: i64| -> i64 y + z }",
        "|x: i64| { let r = &x; |y: i64| -> i64 y + *r }",
    ] {
        let code = PASSED_CALLEE.replace("MK", mk);
        let got = result_updates(&code, &[], mode).await?;
        assert_eq!(got, ["1", "1", "11", "21", "31"], "{mk}, {mode:?}");
    }
    Ok(())
}

modes!(a_callee_fire_reaches_a_passed_reader_forked_or_not);

/// `MAKE` makes `rr`'s target and fires it in the walk; `READ` reads it
/// through `rr` before and after `MAKE`, in one statement or in three.
const PASSED_DEREF: &str = "{
  let n = 0;
  n <- select n { k if k < 3 => k + 1, _ => never() };
  let rr: [&i64, null] = null;
  let mk = |u: i64| -> i64 { let v = u * 10; rr <- &v; v };
  READ
}";

// A dereference the walk passed inside its own statement, before the
// nested block or the call that fires its target, reads that fire next
// cycle, as one in a statement of its own does; one after it reads it in
// its cycle.
async fn a_fire_reaches_a_passed_dereference_in_its_statement(mode: Mode) -> Result<()> {
    for make in ["{ let inner = n * 10; rr <- &inner; inner }", "mk(n)"] {
        for read in [
            "(*(rr$), MAKE, *(rr$))",
            "{ let a = *(rr$); let b = MAKE; let c = *(rr$); (a, b, c) }",
        ] {
            let code = PASSED_DEREF.replace("READ", read).replace("MAKE", make);
            let got = result_updates(&code, &[], mode).await?;
            assert_eq!(
                got,
                ["[0, 10, 10]", "[10, 20, 20]", "[20, 30, 30]", "[30, 30, 30]"],
                "{read} over {make}, {mode:?}"
            );
        }
    }
    Ok(())
}

modes!(a_fire_reaches_a_passed_dereference_in_its_statement);

// A write through a reference that moves after the walk passed the write
// lands at the new target from the next cycle on.
const PASSED_REFERENCE: &str = "{
  let a = 0;
  let b = 0;
  let n = 0;
  n <- select n { k if k < 5 => k + 1, _ => never() };
  let f: fn(x: i64) -> i64 = never();
  let r = f(n);
  let p = select n { k if k < 3 => &mut a, _ => &mut b };
  f <- |x: i64| -> i64 { *p <- x * 10; x };
  (r ~ a, r ~ b)
}";

async fn a_moved_reference_reaches_a_passed_write_next_cycle(mode: Mode) -> Result<()> {
    let got = result_updates(PASSED_REFERENCE, &[], mode).await?;
    assert_eq!(got, ["[0, 0]", "[10, 0]", "[20, 0]", "[30, 0]", "[30, 40]"], "{mode:?}");
    Ok(())
}

modes!(a_moved_reference_reaches_a_passed_write_next_cycle);

// A dynamic module loads after `a`'s statement ran: `a` reads, through the
// function `h` takes from it, the module's `d`, made in the walk, a cycle
// late.
const LATER_MODULE: &str = r#"{
  let h: fn(x: i64) -> i64 = |x: i64| -> i64 x;
  let a = h(1);
  let status = mod foo dynamic {
    sandbox whitelist [core];
    sig { val g: fn(x: i64) -> i64 };
    source "let c = 0; c <- select c { k if k < 3 => k + 1, _ => never() };
            let d = c * 10; let g = |x: i64| -> i64 x + d"
  };
  h <- select status { error as e => never(dbg(e)), null as _ => foo::g };
  a
}"#;

async fn a_fire_in_a_later_module_reaches_a_passed_reader_next_cycle(
    mode: Mode,
) -> Result<()> {
    let got = result_updates(LATER_MODULE, &[], mode).await?;
    assert_eq!(got, ["1", "1", "11", "21", "31"], "{mode:?}");
    Ok(())
}

modes!(a_fire_in_a_later_module_reaches_a_passed_reader_next_cycle);
