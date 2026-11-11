// A wake fires nothing for anyone. A woken arm recomputes from the world
// as it stands; a value it recomputes from an input that changed while it
// slept is tagged moved, and the binding that stands it (a `let`, a
// formal, a pattern bind, a reference's cell) gives every reader the new
// value with no fire: inside the arm, and outside it through a closure or
// a reference that left it. No `count`, `<-` or other event consumer acts
// on it. Each program runs in every mode.

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use graphix_package_core::testing::{Collect, Mode, TestState, data};
use std::time::Duration;

const MODES: [Mode; 5] =
    [Mode::Interp, Mode::Unpruned, Mode::Jit, Mode::Par, Mode::JitPar];

/// `code`'s updates in `mode`, each update's data form printed without
/// its integer type prefixes, and the regions the build fused.
async fn updates(code: &str, mode: Mode) -> Result<(Vec<String>, usize)> {
    let setup = |ctx: &mut TestState| ctx.control.set_prune_mode(mode.prune());
    let d = run_delta_with(code, mode, setup, Collect::Quiet(Duration::from_millis(300)))
        .await?;
    let got =
        d.values.into_iter().map(|v| data(v).to_string().replace("i64:", "")).collect();
    Ok((got, d.regions))
}

/// `code`'s updates are `expected` in every mode.
async fn agrees(code: impl AsRef<str>, expected: &[impl AsRef<str>]) -> Result<()> {
    let expected: Vec<&str> = expected.iter().map(|s| s.as_ref()).collect();
    for mode in MODES {
        let (got, _) = updates(code.as_ref(), mode).await?;
        assert_eq!(got, expected, "{mode:?}");
    }
    Ok(())
}

/// [`agrees`], and the fused modes fused a region.
async fn fused_agrees(code: &str, expected: &[&str]) -> Result<()> {
    for mode in MODES {
        let (got, regions) = updates(code, mode).await?;
        assert_eq!(got, expected, "{mode:?}");
        if matches!(mode, Mode::Jit | Mode::JitPar) {
            assert!(regions > 0, "{mode:?} fused nothing")
        }
    }
    Ok(())
}

/// A clock of ten ticks, `n` counting the even ones and `sel` flipping at
/// clk 1 and 5 (landing at 2 and 6): arm 0 of a select over `sel` sleeps
/// from clk 2 while `n` goes 1, 2, 3, and wakes at clk 6.
const CLOCK: &str = "let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);";

/// The closure `mk(n)` returns in the arm that sleeps, held by a reader
/// that never does: its body `body` reads `mk`'s formal `x`, which the
/// wake takes from 1 to 3 at clk 6; `defs` are statements before it.
/// Each tick gives `(clk, n, sel, v, count(v))`.
fn closure(defs: &str, body: &str) -> String {
    format!(
        "{{
  {CLOCK}
  {defs}
  let mk = |x: i64| |y: i64| {{ let z = y; {body} }};
  let g = select sel {{ 0 => mk(n), _ => mk(n + 100) }};
  let h = once(g);
  let k = 0;
  let v = select k {{ 0 => h(1), _ => -1 }};
  clk ~ (clk, n, sel, v, count(v))
}}"
    )
}

/// [`CLOCK`]'s `n` and `sel` at each tick, the value `X` an arm-0 formal
/// over `n` holds, and `FIRES`, the fires of that formal a reader outside
/// has counted: one at the root's birth, then clk 1, 7 and 9. The wake at
/// clk 6 is none.
const N: [i64; 10] = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5];
const SEL: [i64; 10] = [0, 0, 1, 1, 1, 1, 0, 0, 0, 0];
const X: [i64; 10] = [0, 1, 1, 1, 1, 1, 3, 4, 4, 5];
const FIRES: [i64; 10] = [1, 2, 2, 2, 2, 2, 2, 3, 3, 4];

/// [`closure`]'s ticks for a body computing `f` of the formal: the reader
/// has `f(3)` at clk 6, and `count` still reads 2 there.
fn formal(f: impl Fn(i64) -> i64) -> Vec<String> {
    (0..10)
        .map(|i| format!("[{i}, {}, {}, {}, {}]", N[i], SEL[i], f(X[i]), FIRES[i]))
        .collect()
}

/// A reference to an arm's value, which the wake moves from 2 to 4 at
/// clk 6 with no fire, read as `read` by a reader that never sleeps:
/// `(clk, v, count(v))`.
fn reference(read: &str) -> String {
    format!(
        "{{
  {CLOCK}
  let r = select sel {{ 0 => &(n + 1), _ => &(n + 100) }};
  let holder = once(r);
  let k = 0;
  let v = select k {{ 0 => {read}, _ => -1 }};
  clk ~ (clk, v, count(v))
}}"
    )
}

#[tokio::test(flavor = "multi_thread")]
async fn a_bare_read_of_a_moved_formal_stands() -> Result<()> {
    agrees(closure("", "x"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn an_operator_over_a_moved_formal_stands() -> Result<()> {
    agrees(closure("", "x + 0"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_kernel_over_a_moved_formal_stands() -> Result<()> {
    agrees(closure("", "#[native] (x + 0)"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_select_over_a_moved_formal_stands() -> Result<()> {
    agrees(closure("", "select x { v => v }"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_lambda_argument_moved_stands() -> Result<()> {
    agrees(closure("let id = |v: i64| v;", "id(x)"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_builtin_argument_moved_stands() -> Result<()> {
    agrees(closure("", "max(x, 0)"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_tuple_over_a_moved_formal_stands() -> Result<()> {
    agrees(closure("", "(x, 0).0"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_let_over_a_moved_operator_stands() -> Result<()> {
    agrees(closure("", "let w = x + 0; w"), &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_collection_over_a_moved_source_stands() -> Result<()> {
    agrees(
        closure("", "array::fold(array::map([x], |e| e + 0), 0, |a, e| a + e)"),
        &formal(|x| x),
    )
    .await
}

// A fold's init reaches its accumulator chain: 3 + 1 + 2.
#[tokio::test(flavor = "multi_thread")]
async fn a_fold_over_a_moved_init_stands() -> Result<()> {
    agrees(closure("", "array::fold([1, 2], x, |a, e| a + e)"), &formal(|x| x + 3)).await
}

// A fire is the event, never a compare of values: `x * 0` stays 0 and
// `count` ticks at clk 7 and 9, where `x` fires, and not at the wake.
#[tokio::test(flavor = "multi_thread")]
async fn a_product_of_a_moved_formal_fires_only_with_the_formal() -> Result<()> {
    agrees(closure("", "x * 0"), &formal(|_| 0)).await
}

// The new value re-routes a select over the formal, in the node-walk and
// in a kernel alike: the arm it enters gives its value with no fire.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_scrutinee_routes_its_select_quietly() -> Result<()> {
    let expected = formal(|x| if x == 1 { 10 } else { 20 });
    agrees(closure("", "select x { 1 => 10, _ => 20 }"), &expected).await?;
    agrees(closure("", "#[native] select x { 1 => 10, _ => 20 }"), &expected).await
}

// And it resizes a collection whose length is the formal: a resize is no
// fire of its own.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_length_resizes_its_collection_quietly() -> Result<()> {
    let expected = formal(|x| x);
    agrees(closure("", "array::len(array::init(x, |i| i))"), &expected).await?;
    agrees(closure("", "#[native] array::len(array::init(x, |i| i))"), &expected).await
}

const REFERENCE: [&str; 10] = [
    "[0, 1, 1]",
    "[1, 2, 2]",
    "[2, 2, 2]",
    "[3, 2, 2]",
    "[4, 2, 2]",
    "[5, 2, 2]",
    "[6, 4, 2]",
    "[7, 5, 3]",
    "[8, 5, 3]",
    "[9, 6, 4]",
];

#[tokio::test(flavor = "multi_thread")]
async fn a_bare_read_of_a_moved_reference_stands() -> Result<()> {
    agrees(reference("*holder"), &REFERENCE).await
}

#[tokio::test(flavor = "multi_thread")]
async fn an_operator_over_a_moved_reference_stands() -> Result<()> {
    agrees(reference("*holder + 0"), &REFERENCE).await
}

// The kernel's select takes the capture's new value at clk 6 (30) with no
// fire, and fires with its argument at clk 9 (38).
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_capture_routes_a_kernel_quietly() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  let e = filter(clk, |c| c % 2 == 0);
  n <- select e { c if c >= 6 => never(), c => c ~ (n + 1) };
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| #[native] select x { 3 => 30 + y, v => v * 1000 + y };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let yy = 0;
  yy <- filter(clk, |c| c == 8);
  let v = select k { 0 => h(yy), _ => -1 };
  clk ~ (clk, v, count(v))
}";
    fused_agrees(
        code,
        &[
            "[0, 0, 1]",
            "[1, 1000, 2]",
            "[2, 1000, 2]",
            "[3, 1000, 2]",
            "[4, 1000, 2]",
            "[5, 1000, 2]",
            "[6, 30, 2]",
            "[7, 30, 2]",
            "[8, 30, 2]",
            "[9, 38, 3]",
        ],
    )
    .await
}

// `n` stops at 3 while the arm sleeps; the wake gives the reader the
// capture's 3 with no fire: the select takes `3 => 30`, `count` does not
// tick and `w <- v` writes nothing, so `w` keeps 1.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_capture_routes_a_select_and_writes_nothing() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  let e = filter(clk, |c| c % 2 == 0);
  n <- select e { c if c >= 6 => never(), c => c ~ (n + 1) };
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; select x { 3 => 30, v => v } };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  let w = 0;
  w <- v;
  clk ~ (clk, v, count(v), w)
}";
    agrees(
        code,
        &[
            "[0, 0, 1, 0]",
            "[1, 1, 2, 0]",
            "[2, 1, 2, 1]",
            "[3, 1, 2, 1]",
            "[4, 1, 2, 1]",
            "[5, 1, 2, 1]",
            "[6, 30, 2, 1]",
            "[7, 30, 2, 1]",
            "[8, 30, 2, 1]",
            "[9, 30, 2, 1]",
        ],
    )
    .await
}

// A bare `*h`, an operator over it and a `let` all read 3 at clk 6, and a
// `count` over it does not tick.
#[tokio::test(flavor = "multi_thread")]
async fn every_reader_of_a_moved_reference_agrees() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let k = 0;
  let g = select sel { 0 => { let z = n * 1; &z }, _ => { let z = n + 100; &z } };
  let h = once(g);
  let d = clk ~ select k { 0 => *h, _ => -1 };
  let o = clk ~ select k { 0 => (*h) + 0, _ => -1 };
  let r = select k { 0 => *h, _ => -1 };
  let l = clk ~ r;
  let f = clk ~ count(select k { 0 => *h, _ => -1 });
  (clk, n, sel, d, o, l, f)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 0, 0, 1]",
            "[1, 1, 0, 1, 1, 1, 2]",
            "[2, 1, 1, 1, 1, 1, 2]",
            "[3, 2, 1, 1, 1, 1, 2]",
            "[4, 2, 1, 1, 1, 1, 2]",
            "[5, 3, 1, 1, 1, 1, 2]",
            "[6, 3, 0, 3, 3, 3, 2]",
            "[7, 4, 0, 4, 4, 4, 3]",
            "[8, 4, 0, 4, 4, 4, 3]",
            "[9, 5, 0, 5, 5, 5, 4]",
            "[9, 5, 1, 5, 5, 5, 4]",
        ],
    )
    .await
}

// A collection in the arm rebuilt from a moved input, read through a
// reference: the map and the fold outside take the new elements and
// neither fires.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_array_stands_through_a_reference() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let g = select sel {
    0 => { let a = array::init(2, |i| i + n); &a },
    _ => { let a = array::init(2, |i| i + n + 100); &a }
  };
  let h = once(g);
  let m = array::map(*h, |x| x * 2);
  let fm = count(m);
  let s = array::fold(m, 0, |acc, x| acc + x);
  clk ~ (clk, n, sel, m, fm, s, count(s))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, [0, 2], 1, 2, 1]",
            "[1, 1, 0, [2, 4], 2, 6, 2]",
            "[2, 1, 1, [2, 4], 2, 6, 2]",
            "[3, 2, 1, [2, 4], 2, 6, 2]",
            "[4, 2, 1, [2, 4], 2, 6, 2]",
            "[5, 3, 1, [2, 4], 2, 6, 2]",
            "[6, 3, 0, [6, 8], 2, 14, 2]",
            "[7, 4, 0, [8, 10], 3, 18, 3]",
            "[8, 4, 0, [8, 10], 3, 18, 3]",
            "[9, 5, 0, [10, 12], 4, 22, 4]",
        ],
    )
    .await
}

// Readers above the select read 3 at clk 7, all of them alike, and their
// `count` does not tick for it.
#[tokio::test(flavor = "multi_thread")]
async fn readers_the_walk_passed_take_the_value_a_cycle_late() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let k = 0;
  let hh: fn(y: i64) -> i64 = never();
  let d = clk ~ select k { 0 => hh(1), _ => -1 };
  let o = clk ~ select k { 0 => hh(1) + 0, _ => -1 };
  let f = clk ~ count(select k { 0 => hh(1), _ => -1 });
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  hh <- once(g);
  (clk, n, sel, d, o, f)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 0, 1]",
            "[1, 1, 0, 0, 0, 1]",
            "[2, 1, 1, 1, 1, 2]",
            "[3, 2, 1, 1, 1, 2]",
            "[4, 2, 1, 1, 1, 2]",
            "[5, 3, 1, 1, 1, 2]",
            "[6, 3, 0, 1, 1, 2]",
            "[7, 4, 0, 3, 3, 2]",
            "[8, 4, 0, 4, 4, 3]",
            "[9, 5, 0, 4, 4, 3]",
            "[9, 5, 1, 4, 4, 3]",
        ],
    )
    .await
}

// `count(n)` in the arm consumed no event while it slept: its value did not
// move, its `let` stands as it was and nothing outside fires.
#[tokio::test(flavor = "multi_thread")]
async fn a_count_that_missed_its_events_stands_silent() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let g = select sel { 0 => { let c = count(n); &c }, _ => { let c = count(n) + 100; &c } };
  let h = once(g);
  let k = 0;
  let d = clk ~ select k { 0 => *h, _ => -1 };
  let f = clk ~ count(select k { 0 => *h, _ => -1 });
  (clk, n, sel, d, f)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 1, 1]",
            "[1, 1, 0, 2, 2]",
            "[2, 1, 1, 2, 2]",
            "[3, 2, 1, 2, 2]",
            "[4, 2, 1, 2, 2]",
            "[5, 3, 1, 2, 2]",
            "[6, 3, 0, 2, 2]",
            "[7, 4, 0, 3, 3]",
            "[8, 4, 0, 3, 3]",
            "[9, 5, 0, 4, 4]",
            "[9, 5, 1, 4, 4]",
        ],
    )
    .await
}

// `n` is written 1 over 1 while the arm sleeps: nothing is kept for an arm
// that does not run, so the capture stands at the wake and `count` does
// not tick.
#[tokio::test(flavor = "multi_thread")]
async fn a_write_of_an_equal_value_asleep_is_no_fire_at_the_wake() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (select clk { c if c < 6 => 1, _ => n + 1 });
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let d = clk ~ select k { 0 => h(1), _ => -1 };
  let f = clk ~ count(select k { 0 => h(1), _ => -1 });
  (clk, n, sel, d, f)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 1]",
            "[1, 1, 0, 1, 2]",
            "[2, 1, 1, 1, 2]",
            "[3, 1, 1, 1, 2]",
            "[4, 1, 1, 1, 2]",
            "[5, 1, 1, 1, 2]",
            "[6, 1, 0, 1, 2]",
            "[7, 2, 0, 2, 3]",
            "[8, 2, 0, 2, 3]",
            "[9, 3, 0, 3, 4]",
            "[9, 3, 1, 3, 4]",
        ],
    )
    .await
}

// `x` moved at the outer arm's wake; an arm inside it that wakes later
// binds a callee reading `x` and is owed no fire: it slept through the
// event with the arm around it.
#[tokio::test(flavor = "multi_thread")]
async fn a_reader_inside_the_woken_arm_is_owed_nothing() -> Result<()> {
    let code = "{
  let n = 0;
  n <- select n { k if k < 9 => (k ~ n) + 1, _ => never() };
  let in0 = 0;
  in0 <- select n { 3 => 1, 4 => 0, _ => never() };
  let in1 = 0;
  in1 <- select n { 3 => 1, _ => never() };
  let in2 = 1;
  in2 <- select n { 1 => 0, 6 => 1, _ => never() };
  let in3 = 0;
  in3 <- select n { 2 => 1, _ => never() };
  let g: fn(v: i64) -> i64 = |v| v + 100;
  let a = select in0 {
    0 => {
      let x = in1 + 0;
      g <- select in3 { 0 => never(), _ => |v| count(x) };
      let b = select in2 { 0 => -7, _ => g(5) };
      (count(x), b)
    },
    _ => (in1 * 0 - 1, -1)
  };
  a
}";
    agrees(code, &["[1, 105]", "[1, -7]", "[-1, -1]", "[1, -7]"]).await
}

// The same callee bound at the wake of an arm outside the one that moved
// `x` is owed nothing either: its `count(x)` is new and `x` stands, so the
// call has no value and the pair no fifth update.
#[tokio::test(flavor = "multi_thread")]
async fn a_reader_asleep_outside_is_owed_nothing() -> Result<()> {
    let code = "{
  let n = 0;
  n <- select n { k if k < 9 => (k ~ n) + 1, _ => never() };
  let in0 = 0;
  in0 <- select n { 3 => 1, 4 => 0, _ => never() };
  let in1 = 0;
  in1 <- select n { 3 => 1, _ => never() };
  let in2 = 1;
  in2 <- select n { 1 => 0, 6 => 1, _ => never() };
  let in3 = 0;
  in3 <- select n { 2 => 1, _ => never() };
  let g: fn(v: i64) -> i64 = |v| v + 100;
  let a = select in0 {
    0 => {
      let x = in1 + 0;
      g <- select in3 { 0 => never(), _ => |v| count(x) };
      count(x)
    },
    _ => in1 * 0 - 1
  };
  let b = select in2 { 0 => -7, _ => g(5) };
  (a, b)
}";
    agrees(code, &["[1, 105]", "[1, -7]", "[-1, -7]", "[1, -7]"]).await
}

// `v` fired at ep 3 with no reader in the inner arm awake, and the outer
// wake stands `v` moved at ep 5. Nothing is kept for the inner arm: its
// `t`, written 99, keeps the write over an initializer that stands at 8,
// and a `t` nothing writes takes the 8.
#[tokio::test(flavor = "multi_thread")]
async fn a_written_let_keeps_its_write_over_a_moved_initializer() -> Result<()> {
    let code = "{
  let ep = 0;
  ep <- select ep { n if n < 7 => n + 1, _ => never() };
  let in0 = 0;
  in0 <- select ep { 3 => 1, 4 => 0, _ => never() };
  let in1 = 0;
  in1 <- select ep { 2 => 7, 3 => 8, _ => never() };
  let in3 = 1;
  in3 <- select ep { 1 => 0, 4 => 1, _ => never() };
  let f = |c, a, d, e| select c {
    0 => {
      let v = a * 1;
      select d { 0 => -5, _ => { let t = v + 0; WRITE t } }
    },
    _ => a * 0 - 1
  };
  f(in0, in1, in3, 0)
}";
    let written = code.replace("WRITE", "t <- e ~ 99;");
    agrees(written, &["0", "99", "-5", "-1", "99"]).await?;
    agrees(code.replace("WRITE", ""), &["0", "-5", "-1", "8"]).await
}

// `*r` sits above `z` in the woken arm: it takes 7 a cycle late as a
// standing value, so neither `count(*r)` nor `count(d)` ticks, and `o`
// holds the 7 with no fire: nothing updates past clk 6, and a sample of
// `o` on the clock reads 7 from clk 7 with `count(o)` still 4.
#[tokio::test(flavor = "multi_thread")]
async fn a_passed_reader_inside_the_woken_arm_takes_it_standing() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(12, |i| i));
  let n = 0;
  n <- select clk { 3 => 7, _ => never() };
  let sel = 0;
  sel <- select clk { 1 => 1, 5 => 0, _ => never() };
  let o = select sel {
    0 => {
      let r: &i64 = never();
      let cnt = count(*r);
      let d = *r;
      let e = count(d);
      let z = n + 0;
      r <- &z;
      (d, cnt, e)
    },
    _ => (n * 0 - 5, 0, 0)
  };
  OUT
}";
    let updates =
        ["[[0, 1, 1], 1]", "[[-5, 0, 0], 2]", "[[-5, 0, 0], 3]", "[[0, 1, 1], 4]"];
    agrees(code.replace("OUT", "(o, count(o))"), &updates).await?;
    let sampled = ticks(12, |c| match c {
        0 | 1 => "[[0, 1, 1], 1]",
        2 | 3 => "[[-5, 0, 0], 2]",
        4 | 5 => "[[-5, 0, 0], 3]",
        6 => "[[0, 1, 1], 4]",
        _ => "[[7, 1, 1], 4]",
    });
    agrees(code.replace("OUT", "clk ~ (clk, (o, count(o)))"), &sampled).await
}

// A function variable written while its call is awake binds its new
// callee as an entry: the argument stands, so the callee's `count(x)` has
// no value and neither has the call.
#[tokio::test(flavor = "multi_thread")]
async fn a_rebound_callee_is_entered_over_its_standing_argument() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(6, |i| i));
  let x = 5;
  let f: fn(x: i64) -> i64 = |x: i64| count(x);
  f <- filter(clk, |c| c == 2) ~ |x: i64| count(x) * 1000 + x;
  let r = f(x);
  let c = count(r);
  (clk, r, c)
}";
    agrees(code, &["[0, 1, 1]", "[1, 1, 1]", "[2, 1, 1]"]).await
}

// A handler that left the modal's arm reads the key that opened the modal
// at the wake (acted 2 at clk 5) and does not act on it: n_acted ticks at
// the next key, clk 7.
#[tokio::test(flavor = "multi_thread")]
async fn an_escaped_handler_does_not_act_at_the_wake() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let key = 0;
  key <- select clk { 1 => 1, 3 => 2, 6 => 2, _ => never() };
  let screen = 1;
  screen <- select key { 2 => key ~ 1, 1 => key ~ 0, _ => never() };
  let mk = |k: i64| |u: i64| k;
  let g = select screen { 0 => mk(key ~ 9), _ => mk(key) };
  let held = once(g);
  let acted = held(0);
  let n_acted = count(acted);
  let inside = select screen { 0 => key ~ -1, _ => count(key) };
  clk ~ (clk, key, screen, acted, n_acted, inside)
}";
    agrees(
        code,
        &[
            "[0, 0, 1, 0, 1, 1]",
            "[1, 0, 1, 0, 1, 1]",
            "[2, 1, 1, 1, 2, 2]",
            "[4, 2, 0, 1, 2, -1]",
            "[5, 2, 1, 2, 2, 2]",
            "[6, 2, 1, 2, 2, 2]",
            "[7, 2, 1, 2, 3, 3]",
            "[8, 2, 1, 2, 3, 3]",
            "[9, 2, 1, 2, 3, 3]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_pattern_bind_of_a_woken_arm_fires_outside() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let g = select (sel, n) { (0, x) => |y: i64| x, (_, x) => |y: i64| x + 100 };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 1]",
            "[1, 1, 0, 1, 2]",
            "[2, 1, 1, 1, 2]",
            "[3, 2, 1, 1, 2]",
            "[4, 2, 1, 1, 2]",
            "[5, 3, 1, 1, 2]",
            "[6, 3, 0, 3, 3]",
            "[7, 4, 0, 4, 4]",
            "[8, 4, 0, 4, 4]",
            "[9, 5, 0, 5, 5]",
        ],
    )
    .await
}

// A reader whose own arm wakes in the cycle of the stand reads the new
// value as a reader awake does, with no fire: the `count` inside it does
// not tick.
#[tokio::test(flavor = "multi_thread")]
async fn a_reader_waking_in_the_same_cycle_reads_it_standing() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  let h = once(g);
  let v = select sel { 0 => (h(1), count(h(1))), _ => (-1, -1) };
  clk ~ (clk, n, sel, v)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, [0, 1]]",
            "[1, 1, 0, [1, 2]]",
            "[2, 1, 1, [-1, -1]]",
            "[3, 2, 1, [-1, -1]]",
            "[4, 2, 1, [-1, -1]]",
            "[5, 3, 1, [-1, -1]]",
            "[6, 3, 0, [3, 2]]",
            "[7, 4, 0, [4, 3]]",
            "[8, 4, 0, [4, 3]]",
            "[9, 5, 0, [5, 4]]",
        ],
    )
    .await
}

// A kernel inside the woken arm ORs the bit into its result as it does
// bottomness, so the formal it feeds gives the reader outside the new
// value, with no fire.
#[tokio::test(flavor = "multi_thread")]
async fn a_kernel_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(#[native] (n * 2 + 1)), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 1, 1]",
            "[1, 1, 0, 3, 2]",
            "[2, 1, 1, 3, 2]",
            "[3, 2, 1, 3, 2]",
            "[4, 2, 1, 3, 2]",
            "[5, 3, 1, 3, 2]",
            "[6, 3, 0, 7, 2]",
            "[7, 4, 0, 9, 3]",
            "[8, 4, 0, 9, 3]",
            "[9, 5, 0, 11, 4]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_kernel_select_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(#[native] select n { 3 => max(n, 30), v => v * 2 }), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 1]",
            "[1, 1, 0, 2, 2]",
            "[2, 1, 1, 2, 2]",
            "[3, 2, 1, 2, 2]",
            "[4, 2, 1, 2, 2]",
            "[5, 3, 1, 2, 2]",
            "[6, 3, 0, 30, 2]",
            "[7, 4, 0, 8, 3]",
            "[8, 4, 0, 8, 3]",
            "[9, 5, 0, 10, 4]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_kernel_loop_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(#[native] array::fold(array::map([n, n], |e| e + 1), 0, |a, e| a + e)), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 2, 1]",
            "[1, 1, 0, 4, 2]",
            "[2, 1, 1, 4, 2]",
            "[3, 2, 1, 4, 2]",
            "[4, 2, 1, 4, 2]",
            "[5, 3, 1, 4, 2]",
            "[6, 3, 0, 8, 2]",
            "[7, 4, 0, 10, 3]",
            "[8, 4, 0, 10, 3]",
            "[9, 5, 0, 12, 4]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_kernel_call_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(#[native] { let f = |a: i64| a * 3; f(n) }), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 1]",
            "[1, 1, 0, 3, 2]",
            "[2, 1, 1, 3, 2]",
            "[3, 2, 1, 3, 2]",
            "[4, 2, 1, 3, 2]",
            "[5, 3, 1, 3, 2]",
            "[6, 3, 0, 9, 2]",
            "[7, 4, 0, 12, 3]",
            "[8, 4, 0, 12, 3]",
            "[9, 5, 0, 15, 4]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_collection_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(array::fold(array::map([n, n], |e| e + 1), 0, |a, e| a + e)), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 2, 1]",
            "[1, 1, 0, 4, 2]",
            "[2, 1, 1, 4, 2]",
            "[3, 2, 1, 4, 2]",
            "[4, 2, 1, 4, 2]",
            "[5, 3, 1, 4, 2]",
            "[6, 3, 0, 8, 2]",
            "[7, 4, 0, 10, 3]",
            "[8, 4, 0, 10, 3]",
            "[9, 5, 0, 12, 4]",
        ],
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_stateless_builtin_in_the_woken_arm_carries_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(max(n, 2)), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let v = select k { 0 => h(1), _ => -1 };
  clk ~ (clk, n, sel, v, count(v))
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 2, 1]",
            "[1, 1, 0, 2, 2]",
            "[2, 1, 1, 2, 2]",
            "[3, 2, 1, 2, 2]",
            "[4, 2, 1, 2, 2]",
            "[5, 3, 1, 2, 2]",
            "[6, 3, 0, 3, 2]",
            "[7, 4, 0, 4, 3]",
            "[8, 4, 0, 4, 3]",
            "[9, 5, 0, 5, 4]",
        ],
    )
    .await
}

const ESCAPED: [&str; 10] = [
    "[1, 1, 0, 1, 1, 1]",
    "[2, 1, 1, 1, 1, 1]",
    "[3, 2, 1, 1, 1, 1]",
    "[4, 2, 1, 1, 1, 1]",
    "[5, 3, 1, 1, 1, 1]",
    "[6, 3, 0, 3, 3, 1]",
    "[7, 4, 0, 4, 4, 2]",
    "[8, 4, 0, 4, 4, 2]",
    "[9, 5, 0, 5, 5, 3]",
    "[9, 5, 1, 5, 5, 3]",
];

/// A value the arm publishes by `publish` (statements ending in the
/// select `s`) and a reader that never sleeps reads as `read`, bare,
/// under an operator and under a `count`: `(clk, n, sel, d, o, f)`.
fn escaped(publish: &str, read: &str) -> String {
    format!(
        "{{
  {CLOCK}
  {publish}
  let k = 0;
  let d = clk ~ select k {{ 0 => {read}, _ => -1 }};
  let o = clk ~ select k {{ 0 => {read} + 0, _ => -1 }};
  let f = clk ~ count(select k {{ 0 => {read}, _ => -1 }});
  (clk, n, sel, d, o, f)
}}"
    )
}

// A closure the arm writes to a variable outside it.
#[tokio::test(flavor = "multi_thread")]
async fn a_closure_connected_out_of_the_arm_stands() -> Result<()> {
    let publish = "let out: fn(y: i64) -> i64 = never();
  let s = select sel {
    0 => { let x = n * 1; out <- filter(clk, |c| c == 0) ~ |y: i64| x; 0 },
    _ => n * 0 + 1
  };";
    agrees(escaped(publish, "out(1)"), &ESCAPED).await
}

// A reference the arm writes to a variable outside it.
#[tokio::test(flavor = "multi_thread")]
async fn a_reference_connected_out_of_the_arm_stands() -> Result<()> {
    let publish = "let out: &i64 = never();
  let s = select sel {
    0 => { let x = n * 1; out <- filter(clk, |c| c == 0) ~ &x; 0 },
    _ => n * 0 + 1
  };";
    agrees(escaped(publish, "*out"), &ESCAPED).await
}

// A closure a helper the arm calls writes out.
#[tokio::test(flavor = "multi_thread")]
async fn a_closure_a_helper_connects_out_stands() -> Result<()> {
    let publish = "let out: fn(y: i64) -> i64 = never();
  let publish = |t: i64, f: fn(y: i64) -> i64| { out <- t ~ f; t };
  let s = select sel {
    0 => { let x = n * 1; publish(filter(clk, |c| c == 0), |y: i64| x) },
    _ => n * 0 + 1
  };";
    agrees(escaped(publish, "out(1)"), &ESCAPED).await
}

// A generic function's select arms make the closure: the same value, and
// no fire.
#[tokio::test(flavor = "multi_thread")]
async fn a_generic_callee_passes_the_move() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let pick = |s: i64, mk: fn(x: i64) -> 'a, v: i64| -> 'a select s { 0 => mk(v), _ => mk(v + 100) };
  let mk = |x: i64| |y: i64| { let z = y; x };
  let h = once(pick(sel, mk, n));
  let k = 0;
  let d = clk ~ select k { 0 => h(1), _ => -1 };
  let o = clk ~ select k { 0 => h(1) + 0, _ => -1 };
  let f = clk ~ count(select k { 0 => h(1), _ => -1 });
  (clk, n, sel, d, o, f)
}";
    agrees(
        code,
        &[
            "[0, 0, 0, 0, 0, 1]",
            "[1, 1, 0, 1, 1, 2]",
            "[2, 1, 1, 1, 1, 2]",
            "[3, 2, 1, 1, 1, 2]",
            "[4, 2, 1, 1, 1, 2]",
            "[5, 3, 1, 1, 1, 2]",
            "[6, 3, 0, 3, 3, 2]",
            "[7, 4, 0, 4, 4, 3]",
            "[8, 4, 0, 4, 4, 3]",
            "[9, 5, 0, 5, 5, 4]",
            "[9, 5, 1, 5, 5, 4]",
        ],
    )
    .await
}

// The outer arm resumes twice (n 5 and n 7) while the arm inside it
// sleeps: `x` moved at the first resume, and the inner arm, waking at n 9,
// is owed nothing for it, as after one resume.
#[tokio::test(flavor = "multi_thread")]
async fn a_reader_inside_an_arm_that_resumed_twice_is_owed_nothing() -> Result<()> {
    let code = "{
  let n = 0;
  n <- select n { k if k < 12 => (k ~ n) + 1, _ => never() };
  let in0 = 0;
  in0 <- select n { 3 => 1, 4 => 0, 5 => 1, 6 => 0, _ => never() };
  let in1 = 0;
  in1 <- select n { 3 => 1, _ => never() };
  let in2 = 1;
  in2 <- select n { 1 => 0, 8 => 1, _ => never() };
  let in3 = 0;
  in3 <- select n { 2 => 1, _ => never() };
  let g: fn(v: i64) -> i64 = |v| v + 100;
  let a = select in0 {
    0 => {
      let x = in1 + 0;
      g <- select in3 { 0 => never(), _ => |v| count(x) };
      let b = select in2 { 0 => -7, _ => g(5) };
      (count(x), b)
    },
    _ => (in1 * 0 - 1, -1)
  };
  a
}";
    agrees(code, &["[1, 105]", "[1, -7]", "[-1, -1]", "[1, -7]", "[-1, -1]", "[1, -7]"])
        .await
}

/// A clock of fourteen ticks: `n` goes 0 to 7 at clk 4, and arm 0 of a
/// select over `sel` sleeps from clk 2 and wakes at clk 6.
const LATE_CLOCK: &str = "let clk = array::iter(array::init(14, |i| i));
  let n = 0;
  n <- select clk { 3 => 7, _ => never() };
  let sel = 0;
  sel <- select clk { 1 => 1, 5 => 0, _ => never() };";

/// `[clk, v(clk)]` for each of [`LATE_CLOCK`]'s ticks.
fn late_ticks(v: impl Fn(i64) -> String) -> Vec<String> {
    (0..14).map(|c| format!("[{c}, {}]", v(c))).collect()
}

/// The closure arm 0 makes over its `let z = n + 0`, called by a reader
/// outside it whose own select over `y` leaves arm 0 at clk 4 and returns
/// at clk 8: `other` is that select's other arm.
fn two_selects(other: &str) -> String {
    format!(
        "{{
  {LATE_CLOCK}
  let y = 0;
  y <- select clk {{ 3 => 1, 7 => 0, _ => never() }};
  let g = select sel {{
    0 => {{ let z = n + 0; |q: i64| select q {{ 0 => count(z), _ => {other} }} }},
    _ => {{ let u = n * 0; |q: i64| u + q }}
  }};
  let h = once(g);
  clk ~ (clk, h(y))
}}"
    )
}

// `z` moves to 7 at clk 6 for the reader's awake arm, which reads it with
// no fire, and its sleeping arm's `count(z)` does not tick at clk 8.
#[tokio::test(flavor = "multi_thread")]
async fn a_stand_another_arm_took_is_not_raised_again() -> Result<()> {
    let ticks = late_ticks(|c| match c {
        0..4 | 8.. => "1".into(),
        4 | 5 => "1000".into(),
        _ => "1007".into(),
    });
    agrees(two_selects("z + 1000"), &ticks).await
}

// No arm of the reader's select read `z` at clk 6: the arm that slept
// through the stand is owed nothing for it, and its `count(z)` does not
// tick when it wakes at clk 8.
#[tokio::test(flavor = "multi_thread")]
async fn a_stand_no_arm_read_is_owed_to_none() -> Result<()> {
    let ticks = late_ticks(|c| match c {
        0..4 | 8.. => "1".into(),
        _ => "1001".into(),
    });
    agrees(two_selects("q + 1000"), &ticks).await
}

/// A reference to arm 0's `let z = n + 0`, read outside as `(v, count(v),
/// count(v + 0))`; the arm sleeps again from clk 8 and wakes at clk 10,
/// and `n` is written by `writes`.
fn two_wakes(writes: &str) -> String {
    format!(
        "{{
  let clk = array::iter(array::init(14, |i| i));
  let n = 0;
  n <- select clk {{ {writes}, _ => never() }};
  let sel = 0;
  sel <- select clk {{ 1 => 1, 5 => 0, 7 => 1, 9 => 0, _ => never() }};
  let g = select sel {{ 0 => {{ let z = n + 0; &z }}, _ => {{ let z = n + 100; &z }} }};
  let h = once(g);
  let v = *h;
  clk ~ (clk, (v, count(v), count(v + 0)))
}}"
    )
}

// The first wake stands 7 with no fire. The second slept through no fire
// of `n`: nothing moved.
#[tokio::test(flavor = "multi_thread")]
async fn a_wake_that_slept_through_no_fire_stands_silent() -> Result<()> {
    let ticks = late_ticks(|c| match c {
        0..6 => "[0, 1, 1]".into(),
        _ => "[7, 1, 1]".into(),
    });
    agrees(two_wakes("3 => 7"), &ticks).await
}

// `n` fires again while the arm sleeps the second time: the second wake
// stands 9, and `count` ticks at neither.
#[tokio::test(flavor = "multi_thread")]
async fn each_wake_stands_what_it_slept_through() -> Result<()> {
    let ticks = late_ticks(|c| match c {
        0..6 => "[0, 1, 1]".into(),
        6..10 => "[7, 1, 1]".into(),
        _ => "[9, 1, 1]".into(),
    });
    agrees(two_wakes("3 => 7, 8 => 9"), &ticks).await
}

// The scrutinee fires at clk 4 while the guarded arm sleeps, and the
// guard's flip wakes the arm at clk 6: its pattern bind stands at a value
// the arm slept through, and the closure over it gives 7 outside with no
// fire.
#[tokio::test(flavor = "multi_thread")]
async fn a_pattern_bind_of_a_guard_woken_arm_stands() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(14, |i| i));
  let n = 0;
  n <- select clk { 3 => 7, _ => never() };
  let flag = true;
  flag <- select clk { 1 => false, 5 => true, _ => never() };
  let g = select n { x if flag => |q: i64| x + q, w => { let u = w * 0; |q: i64| u + q - 1 } };
  let h = once(g);
  let v = h(0);
  clk ~ (clk, (v, count(v)))
}";
    let ticks = late_ticks(|c| match c {
        0..6 => "[0, 1]".into(),
        _ => "[7, 1]".into(),
    });
    agrees(code, &ticks).await
}

// `d` reads `z` from above it in the woken arm and takes 7 a cycle late,
// standing: `count(*r)` inside does not tick, and `d`, which left the arm
// by reference, gives the reader outside 7 at clk 7 with no fire.
#[tokio::test(flavor = "multi_thread")]
async fn a_let_over_a_passed_reader_stands_outside_a_cycle_late() -> Result<()> {
    let code = format!(
        "{{
  {LATE_CLOCK}
  let hd: &i64 = never();
  let o = select sel {{
    0 => {{
      let r: &i64 = never();
      let cnt = count(*r);
      let d = *r;
      hd <- once(&d);
      let z = n + 0;
      r <- &z;
      (d, cnt)
    }},
    _ => (n * 0 - 5, 0)
  }};
  let od = *hd;
  clk ~ (clk, (o, od, count(od)))
}}"
    );
    let ticks = late_ticks(|c| match c {
        0 | 1 | 6 => "[[0, 1], 0, 1]".into(),
        2..6 => "[[-5, 0], 0, 1]".into(),
        _ => "[[7, 1], 7, 1]".into(),
    });
    agrees(code, &ticks).await
}

/// A value `expr` computes in the arm that sleeps, from `n` and from `k`
/// and `m`, which never fire: it stands in `mk`'s formal, which a closure
/// held by a reader that never sleeps reads. Each tick gives `(clk, n,
/// sel, v, count(v))`.
fn in_arm(expr: &str) -> String {
    format!(
        "{{
  {CLOCK}
  let k = 0;
  let m = 7;
  let mk = |x| |y: i64| {{ let z = y; x }};
  let g = select sel {{ 0 => mk({expr}), _ => mk({expr}) }};
  let h = once(g);
  let v = select k {{ 0 => h(1), _ => -1 }};
  clk ~ (clk, n, sel, v, count(v))
}}"
    )
}

/// `expr` in the woken arm gives `expected` node-walked and as a kernel.
async fn in_arm_agrees(expr: &str, expected: &[String]) -> Result<()> {
    agrees(in_arm(expr), expected).await?;
    let native = in_arm(&format!("#[native] {expr}"));
    let expected: Vec<&str> = expected.iter().map(|s| s.as_str()).collect();
    fused_agrees(&native, &expected).await
}

/// [`in_arm`]'s ticks for a value that stays `v` and never fires again.
fn silent(v: &str) -> Vec<String> {
    (0..10).map(|i| format!("[{i}, {}, {}, {v}, 1]", N[i], SEL[i])).collect()
}

// The scrutinee and both arms stand still: the guard alone read what the
// arm slept through, and a select moved when a guard it consulted did.
#[tokio::test(flavor = "multi_thread")]
async fn a_guard_that_moved_moves_its_select() -> Result<()> {
    let expr = "select k { x if n > 2 => 10, _ => 20 }";
    in_arm_agrees(expr, &formal(|x| if x > 2 { 10 } else { 20 })).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_guard_that_moved_moves_a_select_in_a_callback() -> Result<()> {
    let expr =
        "array::fold([1, 2, 3], 0, |a, e| a + select e { y if y > n => 1, _ => 0 })";
    in_arm_agrees(expr, &formal(|x| (3 - x).max(0))).await
}

// A tail loop's selects sit on its spine, where a return carries what
// they consumed: the depth `n` decides moved, and so did the answer.
#[tokio::test(flavor = "multi_thread")]
async fn a_tail_loop_over_a_moved_scrutinee_moves() -> Result<()> {
    let expr = "{
    let rec go = |i: i64, acc: i64| -> i64 select i { 0 => acc, _ => go(i - 1, acc + 1) };
    go(n, 0)
  }";
    in_arm_agrees(expr, &formal(|x| x)).await
}

#[tokio::test(flavor = "multi_thread")]
async fn a_tail_loop_whose_guard_moved_moves() -> Result<()> {
    let expr = "{
    let rec go = |i: i64| -> i64 select i { j if j > n => 100, _ => go(i + 1) };
    go(0)
  }";
    in_arm_agrees(expr, &formal(|_| 100)).await
}

// `dbg` gives its argument back: the value moved with it, and the event
// the arm slept through is not printed.
#[tokio::test(flavor = "multi_thread")]
async fn an_io_call_passes_its_moved_argument_on() -> Result<()> {
    in_arm_agrees("dbg(n + 1)", &formal(|x| x + 1)).await
}

// A map's result holds what its callback made of each element, and a fold
// what its callback made of the chain: a source that moved under a
// callback that reads no element moved neither, as its fire fires neither.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_source_no_callback_reads_moves_nothing() -> Result<()> {
    in_arm_agrees("array::map([n, m], |e| 1)", &silent("[1, 1]")).await?;
    in_arm_agrees("array::fold([n, m], 0, |a, e| a + 1)", &silent("2")).await?;
    in_arm_agrees("array::len(array::map([n, m], |e| k))", &silent("2")).await
}

// `clk + n` fires at the wake, so nothing over it moved: a fold whose
// callback reads no element stays as it stood.
#[tokio::test(flavor = "multi_thread")]
async fn a_fire_over_a_moved_input_moves_nothing() -> Result<()> {
    let expr = "{ let s = clk + n; array::fold([s, m], 0, |a, e| a + 1) }";
    in_arm_agrees(expr, &silent("2")).await
}

// A filter's result holds its source's elements, so it moved with them.
#[tokio::test(flavor = "multi_thread")]
async fn a_moved_source_moves_what_holds_its_elements() -> Result<()> {
    let ticks: Vec<String> = formal(|x| x)
        .into_iter()
        .map(|t| {
            let mut f: Vec<&str> = t.trim_matches(['[', ']']).split(", ").collect();
            let v = format!("[{}, 7]", f[3]);
            f[3] = &v;
            format!("[{}]", f.join(", "))
        })
        .collect();
    in_arm_agrees("array::filter([n, m], |e| true)", &ticks).await
}

/// [`in_arm`]'s ticks for a value the wake leaves bottom at clk 6, where
/// the tick is missing: `v` is the value at each `n` the arm sees.
fn bottomed(v: impl Fn(i64) -> i64) -> Vec<String> {
    (0..10)
        .filter(|i| *i != 6)
        .map(|i| format!("[{i}, {}, {}, {}, {}]", N[i], SEL[i], v(X[i]), FIRES[i]))
        .collect()
}

// A select the wake leaves with no arm, its scrutinee bottom or a
// consulted guard's channel bottom over what the arm slept through, moved
// to bottom: the reader outside loses the value at the wake, as it does
// where an operator bottoms.
#[tokio::test(flavor = "multi_thread")]
async fn a_select_a_wake_leaves_bottom_moves() -> Result<()> {
    in_arm_agrees("10 / (n - 3)", &bottomed(|x| 10 / (x - 3))).await?;
    let scrutinee = "select 10 / (n - 3) { 0 => 1, x => x }";
    in_arm_agrees(scrutinee, &bottomed(|x| 10 / (x - 3))).await?;
    let guard = "select k { x if 10 / (n - 3) > 0 => 1, _ => 0 }";
    in_arm_agrees(guard, &bottomed(|x| (10 / (x - 3) > 0) as i64)).await
}

/// `[clk, v(clk)]` for each of `n` ticks.
fn ticks(n: i64, v: impl Fn(i64) -> &'static str) -> Vec<String> {
    (0..n).map(|c| format!("[{c}, {}]", v(c))).collect()
}

/// A select over `s2` whose arm 0 holds `let w = a + 0` and gives a
/// reference to it.
const INNER: &str =
    "select s2 { 0 => { let w = a + 0; &w }, _ => { let w = a + 200; &w } }";

/// [`INNER`]'s reference out of `arm`, arm 0 of a select over `s0`, read
/// outside as `(v, count(v))`. The switches are written by `writes`, which
/// take `a` to 1.
fn nested(writes: &str, arm: &str) -> String {
    format!(
        "{{
  let clk = array::iter(array::init(13, |i| i));
  let (a, s0, s1, s2) = (0, 0, 0, 0);
  {writes}
  let g = select s0 {{ 0 => {arm}, _ => {{ let w = a + 100; &w }} }};
  let h = once(g);
  let v = *h;
  clk ~ (clk, (v, count(v)))
}}"
    )
}

// An arm with nothing to pause is slept when it is next entered. The
// outer arm sleeps over clk 2 and 3 while `a` lands at clk 2, and is
// entered again at clk 4, in the cycle the select inside leaves the arm
// that holds `w`. That arm stopped running with the outer arm, at clk 2,
// so its wake at clk 6 slept through `a` and `w` moves outside, to 1 with
// no fire.
#[tokio::test(flavor = "multi_thread")]
async fn an_arm_slept_at_its_entry_stopped_at_its_deselect() -> Result<()> {
    let writes = "a <- select clk { 1 => 1, _ => never() };
  s0 <- select clk { 1 => 1, 3 => 0, _ => never() };
  s2 <- select clk { 3 => 1, 5 => 0, _ => never() };";
    let moved = ticks(13, |c| if c < 6 { "[0, 1]" } else { "[1, 1]" });
    agrees(nested(writes, INNER), &moved).await?;
    // an arm holding a `count` sleeps at its deselect
    let held = format!("{{ let c = count(s2); {INNER} }}");
    agrees(nested(writes, &held), &moved).await
}

// The arm slept at its entry holds the select behind a call and in a
// collection's slot: their sleep is the arm's, as of the arm's deselect.
#[tokio::test(flavor = "multi_thread")]
async fn a_late_sleep_reaches_a_select_through_a_call_and_a_slot() -> Result<()> {
    let writes = "a <- select clk { 1 => 1, _ => never() };
  s0 <- select clk { 1 => 1, 3 => 0, _ => never() };
  s2 <- select clk { 3 => 1, 5 => 0, _ => never() };";
    let moved = ticks(13, |c| if c < 6 { "[0, 1]" } else { "[1, 1]" });
    let called = format!("{writes}\n  let f = |s2: i64| {INNER};");
    agrees(nested(&called, "f(s2)"), &moved).await?;
    let slot = "array::map([0], |i| select s2 {
      0 => { let w = a + i; &w },
      _ => { let w = a + 200; &w }
    })[0]$";
    agrees(nested(writes, slot), &moved).await
}

// The middle select leaves its arm at clk 2, `a` lands at clk 3 and the
// outer arm sleeps over clk 4 and 5. Slept with the outer arm at its
// entry at clk 6, the middle arm stopped at clk 2, not at clk 4: entered
// at clk 8 as the innermost select leaves the arm holding `w`, that arm
// wakes at clk 10 having slept through `a`.
#[tokio::test(flavor = "multi_thread")]
async fn an_arm_slept_with_its_select_stopped_at_its_own_deselect() -> Result<()> {
    let writes = "a <- select clk { 2 => 1, _ => never() };
  s0 <- select clk { 3 => 1, 5 => 0, _ => never() };
  s1 <- select clk { 1 => 1, 7 => 0, _ => never() };
  s2 <- select clk { 7 => 1, 9 => 0, _ => never() };";
    let mid = format!("select s1 {{ 0 => {INNER}, _ => {{ let w = a + 300; &w }} }}");
    let moved = ticks(13, |c| if c < 10 { "[0, 1]" } else { "[1, 1]" });
    agrees(nested(writes, &mid), &moved).await?;
    let held = format!("{{ let c = count(s1); {mid} }}");
    agrees(nested(writes, &held), &moved).await
}

/// `a` lands at clk 1 while the region that holds `let w = a + 0` sleeps;
/// the region wakes at clk 3, where `w` stands moved, and, with `again`,
/// sleeps at clk 4 and wakes once more at clk 5, where `w` stands again
/// as it is. `s1` is 1 from `away` to clk 5; `rest` reads `w`.
fn stood_again(again: bool, away: i64, rest: &str) -> String {
    let again = if again { "3 => 1, 4 => 0," } else { "" };
    format!(
        "{{
  let clk = array::iter(array::init(9, |i| i));
  let (a, s0, s1) = (0, 0, 0);
  a <- select clk {{ 0 => 1, _ => never() }};
  s0 <- select clk {{ 0 => 1, 2 => 0, {again} _ => never() }};
  s1 <- select clk {{ {} => 1, 4 => 0, _ => never() }};
  {rest}
}}",
        away - 1
    )
}

/// A region holding `let w = a + 0` beside a select over `s1` whose arm 0
/// reads it into `u`, a reference to which is read outside as `(v,
/// count(v))`.
const BESIDE: &str = "let g = select s0 {
    0 => {
      let w = a + 0;
      let c = count(s1);
      select s1 { 0 => { let u = w + 1; &u }, _ => { let u = w + 100; &u } }
    },
    _ => { let u = a + 500; &u }
  };
  let h = once(g);
  let v = *h;
  clk ~ (clk, (v, count(v)))";

// A `let` a wake stands again is delivered quietly, and keeps the stamp
// of the wake that moved it. The arm nested beside it slept through that
// one and wakes in the cycle of the second: it reads `w` moved, and `u`
// moves outside, to 2 with no fire, as where the region woke once.
#[tokio::test(flavor = "multi_thread")]
async fn a_let_stood_again_moved_for_the_arm_that_slept_through_it() -> Result<()> {
    let moved = ticks(9, |c| if c < 5 { "[1, 1]" } else { "[2, 1]" });
    agrees(stood_again(true, 1, BESIDE), &moved).await?;
    agrees(stood_again(false, 1, BESIDE), &moved).await
}

// `w` fires at clk 1 with its region awake, in the cycle the nested arm
// is left. The region sleeps over clk 2 and 3 and wakes at clk 4, where
// `w` stands again with the stamp of that fire and the nested arm wakes
// having slept through it: `u` moves outside, to 2 with no fire, as where
// the region never slept.
#[tokio::test(flavor = "multi_thread")]
async fn a_let_stood_again_keeps_the_stamp_of_its_fire() -> Result<()> {
    let program = |region: &str| {
        format!(
            "{{
  let clk = array::iter(array::init(9, |i| i));
  let (a, s0, s1) = (0, 0, 0);
  a <- select clk {{ 0 => 1, _ => never() }};
  s0 <- select clk {{ {region} _ => never() }};
  s1 <- select clk {{ 0 => 1, 3 => 0, _ => never() }};
  {BESIDE}
}}"
        )
    };
    let moved = ticks(9, |c| if c < 4 { "[1, 1]" } else { "[2, 1]" });
    agrees(program("1 => 1, 3 => 0,"), &moved).await?;
    agrees(program(""), &moved).await
}

// The same through a closure, for an arm of another select.
#[tokio::test(flavor = "multi_thread")]
async fn a_let_stood_again_moved_for_another_selects_arm() -> Result<()> {
    let rest = "let g0 = select s0 {
    0 => { let w = a + 0; |y: i64| w },
    _ => { let w = 100; |y: i64| w }
  };
  let h0 = once(g0);
  let g1 = select s1 {
    0 => { let u = h0(1) + 1; |y: i64| u },
    _ => { let u = 100; |y: i64| u }
  };
  let h1 = once(g1);
  let v = h1(1);
  clk ~ (clk, (h0(1), v, count(v)))";
    let moved = ticks(9, |c| match c {
        0..3 => "[0, 1, 1]",
        3 | 4 => "[1, 1, 1]",
        _ => "[1, 2, 1]",
    });
    agrees(stood_again(true, 3, rest), &moved).await?;
    agrees(stood_again(false, 3, rest), &moved).await
}

// The waking arm's reference moved to `w` while it slept, a target it
// learns as it runs: it is owed nothing for `w`, and its `count` keeps 1,
// whether or not `w` stands again in that cycle.
#[tokio::test(flavor = "multi_thread")]
async fn a_target_learned_at_a_wake_is_no_fire() -> Result<()> {
    let rest = "let k = 0;
  k <- select clk { 2 => 1, _ => never() };
  let g0 = select s0 { 0 => { let w = a + 0; &w }, _ => { let w = a + 500; &w } };
  let x = 7;
  let refs = [&x, once(g0)];
  let g1 = select s1 { 0 => { let r = refs[k]$; count(*r) }, _ => k - 100 };
  clk ~ (clk, g1)";
    let quiet = ticks(9, |c| if c == 3 || c == 4 { "-99" } else { "1" });
    agrees(stood_again(true, 3, rest), &quiet).await?;
    agrees(stood_again(false, 3, rest), &quiet).await
}

// A seq step's call is a fresh call, owed each capture that stands. It
// runs at clk 5, the cycle the region's second wake stands `w` again as
// it is: the callee's `count` takes `w`'s fire and the run completes, as
// where the region woke once and `w` only stands.
#[tokio::test(flavor = "multi_thread")]
async fn a_let_stood_again_is_owed_to_a_steps_call() -> Result<()> {
    let rest = "let g0 = select s0 {
    0 => { let w = a + 0; let c = count(s0); |y: i64| count(w) + y },
    _ => { let w = 100; |y: i64| count(w) + y }
  };
  let h0 = once(g0);
  let r = seq filter(clk, |c| c == 4) { let x = h0(10); x };
  let got = 0;
  got <- r;
  clk ~ (clk, got)";
    let owed = ticks(9, |c| if c < 8 { "0" } else { "11" });
    agrees(stood_again(true, 3, rest), &owed).await?;
    agrees(stood_again(false, 3, rest), &owed).await
}
