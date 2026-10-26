// Tests for by-reference operations

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, is, prints, refused},
};
use netidx::publisher::Value;

// A reference is its own runtime form: under `Any` no number test takes
// it, a cast refuses it, and it prints as `&ref`.
run!(any_holds_no_reference_number, r#"
{
  let id = |p: Any| -> u64 select p { u64 as n => n, _ => u64:0 };
  let x = 41;
  let a: Any = &x;
  "[id(&x)] [id(u64:7)] [a] [cast<u64>(a)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s
    == r#"0 7 &ref error:`InvalidCast("can't cast &ref to u64: an abstract value has no primitive form")"#);
    FuseExpect::None);

const BYREF_DEREF: &str = r#"
{
  let a = 42;
  let x = &a;
  *x
}
"#;

run!(byref_deref, BYREF_DEREF, is(Value::I64(42)); FuseExpect::None);

const BYREF_TUPLE: &str = r#"
{
  let r = &(1, 2);
  let t = *r;
  t.0 + t.1
}
"#;

run!(byref_tuple, BYREF_TUPLE, is(Value::I64(3)); FuseExpect::Jit);

const BYREF_PATTERN: &str = r#"
{
  let r = &42;
  select r {
    &i64 as v => *v
  }
}
"#;

run!(byref_pattern, BYREF_PATTERN, is(Value::I64(42)); FuseExpect::None);

const CONNECT_DEREF0: &str = r#"
{
  let v = 41;
  let r = &mut v;
  *r <- *r + 1;
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref0, CONNECT_DEREF0, prints("[i64:41, i64:42]"); FuseExpect::None);

const CONNECT_DEREF1: &str = r#"
{
  let f = |x: &mut i64| *x <- *x + 1;
  let v = 41;
  f(&mut v);
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref1, CONNECT_DEREF1, prints("[i64:41, i64:42]"); FuseExpect::None);

// Refs are first-class runtime values, so a ref read back out of a
// container derefs like any other (`*(a[0]$)` over `Array<&i64>`).
const DEREF_FROM_ARRAY: &str = r#"
{
  let v = 42;
  let a = [&v];
  *(a[0]$)
}
"#;

run!(deref_from_array, DEREF_FROM_ARRAY, is(Value::I64(42)); FuseExpect::None);

const DEREF_FROM_TUPLE_FIELD: &str = r#"
{
  let v = 7;
  let p = (&v, 1);
  let s = { x: &v };
  *(p.0) + *(s.x)
}
"#;

run!(deref_from_tuple_field, DEREF_FROM_TUPLE_FIELD, is(Value::I64(14)); FuseExpect::None);

// Place references (design/place_references.md).

// Reads and writes through every accessor kind; the root value is
// rebuilt along the path.
const PLACE_READ_WRITE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [10, 20, 30];
  let s = { x: 5, tags: ["p", "q"] };
  let t = (7, 8);
  let m = {"k" => 9};
  let ra = &mut a[1];
  let rs = &mut s.x;
  let rn = &mut s.tags[0];
  let rt = &mut t.1;
  let rm = &mut m{"k"};
  let before = once((*ra, *rs, *rn, *rt, *rm));
  let t1 = select step { 2 => null, _ => never() };
  *ra <- t1 ~ 21;
  *rs <- t1 ~ 6;
  *rn <- t1 ~ "P";
  *rt <- t1 ~ 88;
  *rm <- t1 ~ 99;
  let t2 = select step { 5 => null, _ => never() };
  t2 ~ (before, a, s, t, m{"k"}$, (*ra, *rs, *rn, *rt, *rm))
}
"#;

run!(place_read_write, PLACE_READ_WRITE, prints(r#"[[i64:20, i64:5, "p", i64:8, i64:9], [i64:10, i64:21, i64:30], [["tags", ["P", "q"]], ["x", i64:6]], [i64:7, i64:88], i64:99, [i64:21, i64:6, "P", i64:88, i64:99]]"#); FuseExpect::Jit);

// A moving reference points where its key says when it fires; two
// writes to one root in one cycle both land; a write past an array's
// end is dropped and the root is untouched.
const PLACE_MOVE_SIBLINGS_BAD: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [1, 2, 3];
  let i = 0;
  let r = &mut a[i];
  let r0 = &mut a[0];
  let r2 = &mut a[2];
  let bad = &mut a[7];
  let first = once(*r);
  let t1 = select step { 2 => null, _ => never() };
  i <- t1 ~ 2;
  let t2 = select step { 5 => null, _ => never() };
  *r <- t2 ~ 30;
  let tm = select step { 7 => null, _ => never() };
  let mid = tm ~ a;
  let t3 = select step { 8 => null, _ => never() };
  *r0 <- t3 ~ 100;
  *r2 <- t3 ~ 300;
  *bad <- t3 ~ 9;
  let t4 = select step { 11 => null, _ => never() };
  t4 ~ (first, mid, a, *r)
}
"#;

run!(place_move_siblings_bad, PLACE_MOVE_SIBLINGS_BAD, prints("[i64:1, [i64:1, i64:2, i64:30], [i64:100, i64:2, i64:300], i64:300]"); FuseExpect::Jit);

// A write to a key the map lacks inserts it; a write through a path
// below a missing key has no entry to patch and is dropped.
const PLACE_MISSING_KEY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let m = {"a" => {x: 1, y: 2}};
  let ins = &mut m{"b"};
  let below = &mut m{"c"}.x;
  let t1 = select step { 2 => null, _ => never() };
  *ins <- t1 ~ {x: 3, y: 4};
  *below <- t1 ~ 5;
  let t2 = select step { 5 => null, _ => never() };
  t2 ~ m
}
"#;

run!(place_missing_key, PLACE_MISSING_KEY, prints(r#"{"a" => [["x", i64:1], ["y", i64:2]], "b" => [["x", i64:3], ["y", i64:4]]}"#); FuseExpect::Jit);

// A lambda over `&State` reaches an editor held in an array through a
// place reference passed as its argument.
const PLACE_THROUGH_PARAM: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  type State = { value: string, cursor: i64 };
  let vals: Array<State> = [{ value: "a", cursor: 1 }, { value: "b", cursor: 1 }];
  let bump = |st: &mut State, t: Any| -> null {
    let s = t ~ *st;
    *st <- { value: "[s.value]!", cursor: s.cursor + 1 };
    null
  };
  let t1 = select step { 2 => null, _ => never() };
  let go = t1 ~ 1;
  bump(&mut vals[go], go);
  let t2 = select step { 5 => null, _ => never() };
  t2 ~ vals
}
"#;

run!(place_through_param, PLACE_THROUGH_PARAM, prints(r#"[[["cursor", i64:1], ["value", "a"]], [["cursor", i64:2], ["value", "b!"]]]"#); FuseExpect::Jit);

// A reference that became null dereferences to bottom, never to the
// old target: the late read finds nothing and the deadline wins.
const DEREF_NULL_REF_IS_BOTTOM: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 10 => s + 1, _ => never() };
  let x = 10;
  let r: [&i64, null] = &x;
  r <- null;
  let t1 = select step { 3 => null, _ => never() };
  let late = t1 ~ *(r$);
  let t2 = select step { 6 => null, _ => never() };
  any(late, t2 ~ -1)
}
"#;

run!(deref_null_ref_is_bottom, DEREF_NULL_REF_IS_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
}; FuseExpect::Jit);

// A fired address fires the dereference even when the new target is
// standing: switching from &x to &y delivers y.
const DEREF_FIRES_ON_ADDRESS: &str = r#"
{
  let x = 10;
  let y = 20;
  let choose = false;
  choose <- true;
  let r = select choose { false => &x, true => &y };
  array::group(*r, |n, _| n == 2)
}
"#;

run!(deref_fires_on_address, DEREF_FIRES_ON_ADDRESS, prints("[i64:10, i64:20]"); FuseExpect::None);

// A key move a selected reader saw is read stale by an arm woken after it:
// the woken arm's counter does not tick.
// XCR claude for claude: [test-gap/medium] This test only asserts that the woken arm
// does not tick. It also passes if a woken Deref never fires, and nothing pins the
// positive half: Deref::update's `wake::owed` read of a new target written while the
// arm slept. Deleting that read fails no test. Add a twin. The other arm reads `r`
// without dereferencing it (`r ~ -1`), so no arm tracks the new target `y`; `y` is
// written during the sleep, and the woken arm's `count(*r)` must reach 2, while staying
// at 1 when `y` is never written. probe:
// design/review-2026-10-26/repro/tests-lang-rest-b-deref-stamp-catchup.gx
// (tests-lang-rest-b-02)
// 2026-10-26 claude: Twin added:
// lang::byref::woken_deref_catches_up_a_target_written_asleep runs the probe's program
// in every mode and asserts the woken count is 2 when y is written during the sleep and
// 1 when it never is; it fails in all four modes with Deref::update's wake::owed read
// removed.
async fn woken_deref_reads_a_seen_move_stale(mode: Mode) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(
        r#"{
    let n = 0;
    n <- select n { k if k < 8 => k + 1, _ => never() };
    let a = [10, 20];
    let i = 0;
    i <- select n { 4 => 1, _ => never() };
    let r = &a[i];
    let c = ((n >= 2) && (n < 4)) || (n >= 6);
    select c {
      true => { let cnt = 0; cnt <- (*r) ~ cnt + 1; cnt },
      false => *r + 100
    }
}"#,
        mode,
    )
    .await?;
    assert_eq!(values.last(), Some(&Value::I64(0)), "{values:?}");
    Ok(())
}

modes!(woken_deref_reads_a_seen_move_stale);

// A woken arm's dereference catches up a target that moved and was written
// while it slept, which no selected arm tracked (the other arm reads `r`
// without dereferencing it): the woken count ticks for the write, and does
// not when the target was never written.
async fn woken_deref_catches_up_a_target_written_asleep(mode: Mode) -> Result<()> {
    const PROG: &str = r#"{
    let n = 0;
    n <- select n { k if k < 8 => k + 1, _ => never() };
    let x = 1;
    let y = 2;
    y <- select n { WRITE => 20, _ => never() };
    let which = 0;
    which <- select n { 4 => 1, _ => never() };
    let r = select which { 0 => &x, _ => &y };
    let c = (n < 2) || (n >= 6);
    select c {
      true => count(*r),
      false => r ~ -1
    }
}"#;
    for (write, ticks) in [("3", 2), ("300", 1)] {
        let (values, _) =
            super::dense_deltas::run_delta(&PROG.replace("WRITE", write), mode).await?;
        assert_eq!(
            values.last(),
            Some(&Value::I64(ticks)),
            "write at {write}: {values:?}"
        );
    }
    Ok(())
}

modes!(woken_deref_catches_up_a_target_written_asleep);

// A place's address is evaluated once: a key with an effect runs it
// once per fire (`n` fires at its binding and once more, not twice
// more), and the read sees that one evaluation.
// XCR claude for claude: [risk/low] This fixture, deref_null_ref_is_bottom and
// place_root_bottom_mirror race a 20 ms timer against the runtime's next cycle (or a
// 200 ms window). Under load the timer can win, and the test fails with the code right:
// count 1, or the old target read. Sample on a step counter as the other place fixtures
// do (`select step { 5 => null, _ => never() }`), so the read derives from the write.
// Found by reading; not run. (tests-lang-rest-b-08)
// 2026-10-26 claude: The three now derive their reads from the writes:
// deref_null_ref_is_bottom and place_key_evaluated_once sample on a step counter, and
// place_root_bottom_mirror writes b at a step and waits for GXHandle::wait_settled,
// then drains the events, in place of a 200 ms window. deref_null_ref_is_bottom now
// fuses (step counter), so its FuseExpect is Jit. No pin beyond the fixtures: nothing
// was wrong in the code under test.
const PLACE_KEY_EVALUATED_ONCE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 5 => s + 1, _ => never() };
  let a = [10, 20];
  let n = 0;
  let r = &a[{ n <- a ~ n + 1; 0 }];
  let t = select step { 3 => null, _ => never() };
  (t ~ count(n), t ~ *r)
}
"#;

run!(place_key_evaluated_once, PLACE_KEY_EVALUATED_ONCE, prints("[i64:2, i64:10]"); FuseExpect::Jit);

// An undetermined key is a bottom reference: a write through it does not
// happen, as no operation with a bottom input does; when the key returns
// the reference retargets, and nothing written meanwhile lands there
// (tests-lang-rest-b-03).
// XCR claude for claude: [test-gap/medium] This and the other byref fixtures pin only
// one side of the rule: a write whose reference is BOTTOM is dropped. The other side,
// 'a write made before the reference first arrives waits for it and lands at its first
// target' (ConnectDeref::pending), has no pin, and removing `pending` fails nothing.
// Add a fixture where the reference is absent, not bottom, at the write, e.g. `let w =
// sys::time::after_idle(duration:50.ms, &mut x); *w <- 42` (x becomes 42). Set it
// beside the `k$`-over-null form, which must leave x alone. probe:
// design/review-2026-10-26/repro/tests-lang-rest-b-write-before-ref-arrives.gx
// (tests-lang-rest-b-03)
// 2026-10-26 claude: Pin: lang::byref::write_before_reference_arrives writes 42
// through an after_idle reference (absent at the write: lands at x) and through k$
// over a null k (bottom at the write: y stays 1), and reads both once x is 42. It
// fails in all four modes with ConnectDeref's pending memory removed. The event-RHS
// case is x-engine-firing-04, still open.
const PLACE_BOTTOM_KEY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [10, 20];
  let k: [i64, null] = 0;
  let r = &mut a[k$];
  let t1 = select step { 2 => null, _ => never() };
  k <- t1 ~ null;
  let t2 = select step { 5 => null, _ => never() };
  *r <- t2 ~ 99;
  let t3 = select step { 8 => null, _ => never() };
  let t4 = select step { 11 => null, _ => never() };
  k <- t4 ~ 1;
  let t5 = select step { 16 => null, _ => never() };
  let seen = array::group(*r, |n, _| n == 2);
  (t3 ~ a, t5 ~ a, t5 ~ seen)
}
"#;

run!(place_bottom_key, PLACE_BOTTOM_KEY, prints("[[i64:10, i64:20], [i64:10, i64:20], [i64:10, i64:20]]"); FuseExpect::Jit);

// A write made before its reference first arrives waits for it and lands
// at its first target (`x`); one made while its reference is bottom does
// not happen, and the reference arriving later writes nothing (`y`).
const WRITE_BEFORE_REFERENCE_ARRIVES: &str = r#"
{
  let x = 1;
  let y = 1;
  let w = sys::time::after_idle(duration:50.ms, &mut x);
  *w <- 42;
  let k: [&mut i64, null] = null;
  k <- &mut y;
  let wk = k$;
  *wk <- 42;
  let landed = select x { 42 => null, _ => never() };
  (landed ~ x, landed ~ y)
}
"#;

run!(write_before_reference_arrives, WRITE_BEFORE_REFERENCE_ARRIVES, prints("[i64:42, i64:1]"); FuseExpect::Jit);

// A place the root no longer has is bottom, not its last value: a
// connect sampling it then writes nothing.
const PLACE_REMOVED_ELEMENT: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [10, 20];
  let r = &a[1];
  let t1 = select step { 2 => null, _ => never() };
  a <- t1 ~ [5];
  let t2 = select step { 5 => null, _ => never() };
  let obs: [i64, null] = null;
  obs <- t2 ~ *r;
  let t3 = select step { 8 => null, _ => never() };
  a <- t3 ~ [6, 7];
  let seen = array::group(*r, |n, _| n == 2);
  let t4 = select step { 11 => null, _ => never() };
  (t4 ~ seen, t4 ~ obs)
}
"#;

run!(place_removed_element, PLACE_REMOVED_ELEMENT, prints("[[i64:20, i64:7], null]"); FuseExpect::Jit);

// The referent type is the container's element type: an Error-valued
// field is referenced as such.
const PLACE_ERROR_FIELD: &str = r#"
{
  let a = {x: error(`E)};
  let r = &a.x;
  let expected: &Error<`E> = r;
  select *expected { error as e => `Caught }
}
"#;

run!(place_error_field, PLACE_ERROR_FIELD, prints("\"Caught\""); FuseExpect::Jit);

// Parentheses are transparent in a place, and a place through a
// dereferenced place reference composes the paths: the write reaches
// the root.
const PLACE_THROUGH_DEREF: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = {p: {x: 10, y: 1}};
  let r: &mut {x: i64, y: i64} = &mut a.p;
  let s = &mut (*r).x;
  let t1 = select step { 2 => null, _ => never() };
  *s <- t1 ~ 20;
  let t2 = select step { 5 => null, _ => never() };
  (t2 ~ a.p.x, t2 ~ *s)
}
"#;

run!(place_through_deref, PLACE_THROUGH_DEREF, prints("[i64:20, i64:20]"); FuseExpect::Jit);

// A place through a dereference whose reference went bottom has no
// address: it reads nothing and a write through it does not happen; when
// the reference returns the place does, and a later write lands there.
const PLACE_THROUGH_BOTTOM_DEREF: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [10, 20];
  let r: [&mut Array<i64>, null] = &mut a;
  let s = &mut (*r$)[0];
  let t1 = select step { 2 => null, _ => never() };
  r <- t1 ~ null;
  let t2 = select step { 5 => null, _ => never() };
  *s <- t2 ~ 99;
  let obs: [i64, null] = null;
  obs <- t2 ~ *s;
  let t3 = select step { 8 => null, _ => never() };
  r <- t3 ~ &mut a;
  let tm = select step { 10 => null, _ => never() };
  let t4 = select step { 11 => null, _ => never() };
  *s <- t4 ~ 7;
  let t5 = select step { 16 => null, _ => never() };
  (t3 ~ a, tm ~ a, t5 ~ obs, t5 ~ a)
}
"#;

run!(place_through_bottom_deref, PLACE_THROUGH_BOTTOM_DEREF, prints("[[i64:10, i64:20], [i64:10, i64:20], null, [i64:7, i64:20]]"); FuseExpect::Jit);

// A place indexed past i64::MAX addresses nothing, never from the end:
// its read is bottom and the sample banks.
const PLACE_INDEX_U64_ABOVE_I64_MAX: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let a = [10, 20, 30];
  let r = &a[u64:18446744073709551615];
  let t1 = select step { 2 => null, _ => never() };
  let obs: [i64, null] = null;
  obs <- t1 ~ *r;
  let t2 = select step { 5 => null, _ => never() };
  t2 ~ obs
}
"#;

run!(place_index_u64_above_i64_max, PLACE_INDEX_U64_ABOVE_I64_MAX, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; FuseExpect::Jit);

// A place's index is an integer, as in an access.
const PLACE_INDEX_IS_AN_INTEGER: &str = r#"
{
  let a = [10];
  let r = &a["0"];
  *r
}
"#;

run!(place_index_is_an_integer, PLACE_INDEX_IS_AN_INTEGER, |v: Result<&Value>| {
    matches!(v, Err(e) if format!("{e:#}").contains("Int does not contain string"))
}; FuseExpect::None);

/// A composed place's cell, which an embedder reads, holds its element.
#[tokio::test(flavor = "current_thread")]
async fn place_through_deref_mirror() -> Result<()> {
    use graphix_compiler::Rt;
    use graphix_package_core::testing::{
        compile_result, fixture_runtime, next_update, result_source,
    };
    let src = result_source("{ let a = {p: {x: 10}}; let r = &a.p; &(*r).x }");
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", src)], crate::TEST_REGISTER, Mode::Jit, |_| ())
            .await?;
    // the graph lives while its cell is read: a deleted reference forgets it
    let res = compile_result(&ctx).await?;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(5);
    let v = next_update(&mut rx, res.exprs[0].id, deadline).await?;
    let Some(id) = graphix_compiler::reference::cell(&v) else {
        anyhow::bail!("expected a reference, got {v}")
    };
    let mirror = ctx.rt.with_ctx(move |ctx| ctx.rt.store_value(&id)).await?;
    assert_eq!(mirror, Some(Value::I64(10)));
    Ok(())
}

// A reference to an expression publishes in the cycle its expression
// fires, bottoms included: it reads as a reference to a `let` of the
// expression does.
const BYREF_EXPR: &str = r#"{
  let n = array::iter([0, 1, 2, 3]);
  let v = select n { 2 => never(), k => k * 10 };
  let r = &(v + 1);
  *r
}"#;

const BYREF_LET: &str = r#"{
  let n = array::iter([0, 1, 2, 3]);
  let v = select n { 2 => never(), k => k * 10 };
  let w = v + 1;
  let r = &w;
  *r
}"#;

async fn byref_expr_same_cycle(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    use graphix_package_core::testing::as_i64s;
    let (expr, _) = run_delta(BYREF_EXPR, mode).await?;
    let (bind, _) = run_delta(BYREF_LET, mode).await?;
    assert_eq!(as_i64s(&expr), as_i64s(&bind));
    assert_eq!(as_i64s(&expr), vec![1, 11, 31]);
    Ok(())
}

modes!(byref_expr_same_cycle);

// A dereference whose address moved to a binding that has never
// delivered is bottom, not the previous referent's value.
const DEREF_MOVED_TO_UNDELIVERED: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let x = 1;
  let y = sys::time::after_idle(duration:10.s, 2);
  let sel = false;
  sel <- select step { 2 => null, _ => never() } ~ true;
  let r = select sel { false => &x, true => &y };
  let late = select step { 6 => null, _ => never() } ~ *r;
  any(late, select step { 12 => null, _ => never() } ~ -1)
}
"#;

run!(deref_moved_to_undelivered, DEREF_MOVED_TO_UNDELIVERED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
}; FuseExpect::Jit);

// A place reaches an abstract value's payload where its definition is
// visible, and an error's; writes rebuild them.
const PLACE_PAYLOAD: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  type C = Abstract<i64>;
  let c = C(5);
  let e: Error<i64> = error(1);
  let rc = &mut c.0;
  let re = &mut e.0;
  let before = once((*rc, *re));
  let t1 = select step { 2 => null, _ => never() };
  *rc <- t1 ~ 7;
  *re <- t1 ~ 8;
  let t2 = select step { 6 => null, _ => never() };
  t2 ~ (before, c.0, e.0, *rc, *re)
}
"#;

run!(place_payload, PLACE_PAYLOAD, prints("[[i64:5, i64:1], i64:7, i64:8, i64:7, i64:8]"); FuseExpect::Jit);

/// A place whose root goes bottom sets its cell, which an embedder
/// reads, bottom.
#[tokio::test(flavor = "current_thread")]
async fn place_root_bottom_mirror() -> Result<()> {
    use graphix_compiler::Rt;
    use graphix_rt::GXEvent;
    let (tx, mut rx) = tokio::sync::mpsc::channel(64);
    let ctx = crate::init(tx).await?;
    let compiled = ctx
        .rt
        .compile(arcstr::literal!(
            "{ let step = 0; \
             step <- select step { s if s < 3 => s + 1, _ => never() }; \
             let b = 1; b <- select step { 2 => 0, _ => never() }; \
             let a = [10 / b]; &a[0] }"
        ))
        .await?;
    let eid = compiled.exprs[0].id;
    ctx.rt.wait_settled().await?;
    let mut cell = None;
    while let Ok(mut batch) = rx.try_recv() {
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e
                && id == eid
            {
                cell = graphix_compiler::reference::cell(&v);
            }
        }
    }
    let Some(id) = cell else { anyhow::bail!("no reference produced") };
    let mirror = ctx.rt.with_ctx(move |ctx| ctx.rt.store_value(&id)).await?;
    ctx.shutdown().await;
    assert_eq!(mirror, None);
    Ok(())
}

// A reference prints as an opaque token: its id is the session's (a warm
// start relocates it), never something a program can read.
const BYREF_PRINTS_OPAQUE: &str = r#"
{
  let x = 1;
  let r = &x;
  let s = {a: 2, r: r};
  "[r] [s]"
}
"#;

run!(byref_prints_opaque, BYREF_PRINTS_OPAQUE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "&ref {a: 2, r: &ref}")
}; FuseExpect::None);

// `==` and `!=` compare references by what they point to: a binding, or
// a place's root and path. Each `&x` mints a cell of its own, so its
// value can't say this; a reference made by `&mut` over a value is its
// own place.
const BYREF_EQ_COMPARES_TARGETS: &str = r#"
{
  let x = 1;
  let y = 2;
  let a = [1, 2];
  let s = {f: 1, g: 2};
  let r = &x;
  let f = |p: &i64| p;
  let eq = |p, q| p == q;
  let t1 = {n: 1, r: &x};
  let t2 = {n: 1, r: &x};
  [
    &x == &x, r == &x, f(&x) == r, &x == &y, &x != &y, &a[0] == &a[0],
    &a[0] == &a[1], &s.f == &s.g, eq(&x, &x), eq(&x, &y), [&x, &y] == [&x, &y],
    t1 == t2, &mut 0 == &mut 0, &a[-1] == &a[1]
  ]
}
"#;

run!(byref_eq_compares_targets, BYREF_EQ_COMPARES_TARGETS, |v: Result<&Value>| {
    let want = [true, true, true, false, true, true, false, false, true, false, true, true, false, false];
    match v {
        Ok(Value::Array(a)) => a.iter().map(|v| *v == Value::Bool(true)).eq(want),
        _ => false,
    }
}; FuseExpect::Jit);

// A moving reference compares by where it points now: `&a[i]` names
// `a[0]` until `i` moves.
async fn byref_eq_follows_a_moving_place(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let code = r#"{
  let n = 0;
  n <- select n { k if k < 4 => k + 1, _ => never() };
  let a = [1, 2];
  let i = 0;
  i <- select n { 2 => 1, _ => never() };
  n ~ (&a[i] == &a[0])
}"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values, [true, true, true, false, false].map(Value::Bool));
    Ok(())
}

modes!(byref_eq_follows_a_moving_place);

// References have no order: a reference's value is its cell, numbered in
// whatever order compiling made it. The orderings, sorting, `min`/`max`
// and map keys refuse them, a generic definition's at the call.
run!(
    byref_lt_refused,
    "{ let x = 1; let y = 2; &x < &y }",
    refused("no order");
    FuseExpect::None
);
run!(
    byref_sort_refused,
    "{ let x = 1; array::len(array::sort([&x])) }",
    refused("no order");
    FuseExpect::None
);
run!(
    byref_map_key_refused,
    "{ let x = 1; map::len({&x => 1}) }",
    refused("no order");
    FuseExpect::None
);
run!(
    byref_generic_lt_refused,
    "{ let x = 1; let lt = |a, b| a < b; lt(&x, &x) }",
    refused("no order");
    FuseExpect::None
);
run!(
    byref_min_refused,
    "{ let x = 1; *min(&x, &x) }",
    refused("no order");
    FuseExpect::None
);
run!(
    byref_map_insert_refused,
    "{ let x = 1; map::len(map::insert({}, &x, 1)) }",
    refused("no order");
    FuseExpect::None
);

// `uniq` compares as `==` does: a second reference to `x` is no change.
const BYREF_UNIQ_COMPARES_TARGETS: &str = r#"
{
  let x = 1;
  let y = 2;
  let r = array::iter([&x, &x, &y]);
  let seen = array::group(uniq(r), |n, _| n == 2);
  array::map(seen, |p| *p)
}
"#;

run!(byref_uniq_compares_targets, BYREF_UNIQ_COMPARES_TARGETS, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if &**a == &[Value::I64(1), Value::I64(2)])
}; FuseExpect::None);

// A moving place held by a variable that does not fire again writes where
// its key names at the write, as a read through it reads there.
async fn held_moving_place_writes_where_its_key_names(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let code = r#"{
  let a = [10, 20, 30, 40];
  let i = 0;
  let step = array::iter([1, 2, 3, 4]);
  i <- select step { 2 => 2, _ => never() };
  let held = once(&mut a[i]);
  *held <- select step { 4 => 99, _ => never() };
  (i, *held, a)
}"#;
    let (values, _) = run_delta(code, mode).await?;
    let last = values.last().map(|v| format!("{v}"));
    assert_eq!(
        last.as_deref(),
        Some("[i64:2, i64:99, [i64:10, i64:20, i64:99, i64:40]]")
    );
    Ok(())
}

modes!(held_moving_place_writes_where_its_key_names);

// A write through a reference to a value lands in the reference's own
// cell: `&mut e` is a fresh variable holding `e`.
const WRITE_THROUGH_A_VALUE_REFERENCE: &str = r#"
{
    let r0 = &mut "top";
    *r0 <- "hello";
    let f = |#row: &mut string = &mut never(), #path: &mut string = &mut never(), x: string| -> string {
        *row <- x;
        *path <- "[*row]/leaf";
        x
    };
    let p: string = never();
    f(#path: &mut p, "a");
    (*r0, p)
}
"#;

run!(write_through_a_value_reference, WRITE_THROUGH_A_VALUE_REFERENCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::from("hello"), Value::from("a/leaf")])
}; FuseExpect::None);

// A moving place reference held where nothing re-fires it (`once`)
// still re-fires its readers when its key moves, with the new element.
const MOVING_REFERENCE_HELD: &str = r#"{
    let a = [1, 2, 3];
    let i = 0;
    let r = once(&a[i]);
    i <- once(r) ~ 2;
    *r * 10 + *r
}"#;

async fn moving_reference_held(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    use graphix_package_core::testing::as_i64s;
    let (values, _) = run_delta(MOVING_REFERENCE_HELD, mode).await?;
    assert_eq!(as_i64s(&values), vec![11, 33]);
    Ok(())
}

modes!(moving_reference_held);

// A binding written only through a writable reference is a `<-` target:
// an arm's wake keeps its last write, and a call through it dispatches.
const WRITTEN_THROUGH_A_REFERENCE: &str = r#"{
    let n = array::iter([0, 1, 2, 3, 4]);
    let bump = |c: &mut i64, ev: i64| -> i64 { *c <- ev ~ *c + 1; 0 };
    let viaref = select n % 2 {
        0 => { let x = 10; let ignored = bump(&mut x, n); x },
        _ => -1
    };
    let f = |x: i64| -> i64 x + 1;
    let fr = &mut f;
    *fr <- |x: i64| -> i64 x + 100;
    (array::group(viaref, |i, _| i == 5), f(1))
}"#;

run!(written_through_a_reference, WRITTEN_THROUGH_A_REFERENCE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            a.len() == 2
                && a[0]
                    == Value::Array(
                        [10, -1, 11, -1, 12].map(Value::I64).into_iter().collect(),
                    )
                && a[1] == Value::I64(101)
        }
        _ => false,
    }
}; FuseExpect::Jit);

// A reborrow names what its reference names: a write through `&mut *r`,
// here or in a callee, lands in `s`.
run!(
    reborrow_writes_the_referent,
    r#"{
    let s = 1;
    let r = &mut s;
    let p = &mut *r;
    *p <- 5;
    let bump = |c: &mut i64| { *c <- c ~ (*c + 10); null };
    let k = r ~ bump(&mut *r);
    array::group(s, |i, _| i == 3)
}"#,
    prints("[i64:1, i64:5, i64:11]"); FuseExpect::None
);

// A place reference fires when its address moves, never at a write of
// its root that leaves the address where it was.
async fn place_reference_fires_on_a_move(mode: Mode) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(
        r#"{
    let n = 0;
    n <- select n { k if k < 4 => k + 1, _ => never() };
    let a = [1, 2, 3];
    a <- n ~ [n, 2, 3];
    let s = {a: 1, b: 2};
    s <- n ~ {a: n, b: 2};
    let i = 0;
    i <- select n { 3 => 2, _ => never() };
    (count(&a[1]), count(&s.b), count(&a[i]))
}"#,
        mode,
    )
    .await?;
    let last = values.last().map(|v| format!("{v}"));
    assert_eq!(last.as_deref(), Some("[i64:1, i64:1, i64:2]"), "{values:?}");
    Ok(())
}

modes!(place_reference_fires_on_a_move);

// A typedef naming a reference is that reference as a union member too.
run!(write_through_aliased_reference_union, r#"{
  let step = 0;
  step <- select step { s if s < 5 => s + 1, _ => never() };
  type Cell = &mut i64;
  let a = 1;
  let r: [Cell, &mut i64] = &mut a;
  *r <- 10;
  let t = select step { 3 => null, _ => never() };
  t ~ a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(10))); FuseExpect::Jit);

// A rank-2 parameter's quantifier is generic, not a binding an earlier
// compile generalized: a writable reference to it is allowed.
run!(mut_ref_to_a_rank2_parameter, r#"{
  let app = |g: fn<'b: Any>(y: 'b) -> 'b| { let h = &mut g; (*h)(1) };
  app(|x| x)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::None);

// So is a lambda literal's parameter checked against a rank-2 formal:
// its type is the quantifier itself.
run!(mut_ref_to_a_rank2_argument_parameter, r#"{
  let app = |g: fn<'b: Any>(y: 'b) -> 'b| g(1);
  app(|y| { let r = &mut y; y })
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// A reference never equals an array of numbers, its root's bind id
// among them: its comparison form holds the root as a reference.
run!(reference_never_equals_an_array, r#"{
  let x = 1;
  let a: [&i64, Array<u64>] = &x;
  let base = cast<u64>(4611686018427387904)$;
  array::len(array::filter(array::init(100000, |k| k), |k| {
    let b: [&i64, Array<u64>] = [base + cast<u64>(k)$];
    a == b
  }))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::Jit);

// A write that lands while a `let` sleeps is re-raised at its wake to the
// readers that slept with it, not to one outside that read it live
// through an escaped `&mut`: the count outside sees the write once.
run!(
    reraise_skips_a_reader_outside,
    r#"{
  let n = 0;
  n <- select n { k if k < 6 => k + 1, _ => never() };
  let sel = select n { 2 => false, 3 => false, 4 => false, _ => true };
  let holder: [&mut i64, null] = null;
  let inner = select sel {
    true => { let x = 0; holder <- &mut x; x },
    false => -1
  };
  let h = holder$;
  *h <- select n { 3 => n ~ 100, _ => never() };
  let c = count(*h);
  select n { 6 => (inner, c), _ => never() }
}"#,
    prints("[i64:100, i64:2]");
    FuseExpect::Jit
);

// A reference outlives the `&` that minted it, here a collection slot
// deleted at c = 3: reads and writes through it keep reaching `x`.
run!(
    reference_outlives_its_site,
    r#"{
  let c = 0;
  c <- select c { k if k < 6 => k + 1, _ => never() };
  let x = 7;
  let keep: [&mut i64, null] = null;
  let rs = array::map(select c { k if k < 3 => [0], _ => [] }, |_| &mut x);
  keep <- select rs { [r] => r, _ => never() };
  let w = keep$;
  *w <- c ~ c * 1000;
  let seen = array::group(c ~ (x, *w), |n, _| n == 6);
  seen
}"#,
    prints("[[i64:7, i64:7], [i64:1000, i64:1000], [i64:2000, i64:2000], [i64:3000, i64:3000], [i64:4000, i64:4000], [i64:5000, i64:5000]]");
    FuseExpect::Jit
);

// A reference keeps the binding it names alive past the binding's own
// deletion: the slot's `y` goes at c = 3, the writes through `w` land on.
run!(
    reference_keeps_its_binding,
    r#"{
  let c = 0;
  c <- select c { k if k < 6 => k + 1, _ => never() };
  let keep: [&mut i64, null] = null;
  let rs = array::map(select c { k if k < 3 => [0], _ => [] }, |_| {
    let y = 5;
    keep <- &mut y;
    y
  });
  let w = keep$;
  *w <- c ~ c * 10;
  let seen = array::group(c ~ *w, |n, _| n == 6);
  seen
}"#,
    prints("[i64:5, i64:10, i64:20, i64:30, i64:40, i64:50]");
    FuseExpect::Jit
);

// A write through a moving reference, and a comparison of one, reach the
// place the reference names this cycle, so neither runs beside the
// statement that moves it.
run!(
    a_write_through_a_moving_reference_follows_its_move,
    r#"{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let x = array::iter([0, 1, 2]);
  let a = [10, 20, 30];
  let holder: [&mut i64, null] = null;
  let r = &mut a[x];
  select holder { null => null, p => { *p <- x ~ 100 + x; null } };
  holder <- r;
  let t = select step { 8 => null, _ => never() };
  t ~ a
}"#,
    prints("[i64:10, i64:101, i64:102]");
    FuseExpect::Jit
);

run!(
    a_comparison_of_a_moving_reference_follows_its_move,
    r#"{
  let step = 0;
  step <- select step { s if s < 20 => s + 1, _ => never() };
  let x = array::iter([0, 1, 2]);
  let a = [10, 20, 30];
  let holder: [&i64, null] = null;
  let out: Array<bool> = [];
  let r = &a[x];
  out <- select holder { null => never(), p => x ~ array::push(out, p == &a[1]) };
  holder <- r;
  let t = select step { 8 => null, _ => never() };
  t ~ out
}"#,
    prints("[true, false]");
    FuseExpect::Jit
);
