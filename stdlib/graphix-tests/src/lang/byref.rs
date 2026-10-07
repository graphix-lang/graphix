// Tests for by-reference operations

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode},
};
use netidx::publisher::Value;

const BYREF_DEREF: &str = r#"
{
  let a = 42;
  let x = &a;
  *x
}
"#;

run!(byref_deref, BYREF_DEREF, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; FuseExpect::Jit);

const BYREF_TUPLE: &str = r#"
{
  let r = &(1, 2);
  let t = *r;
  t.0 + t.1
}
"#;

run!(byref_tuple, BYREF_TUPLE, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; FuseExpect::Jit);

const BYREF_PATTERN: &str = r#"
{
  let r = &42;
  select r {
    &i64 as v => *v
  }
}
"#;

run!(byref_pattern, BYREF_PATTERN, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; FuseExpect::None);

const CONNECT_DEREF0: &str = r#"
{
  let v = 41;
  let r = &mut v;
  *r <- *r + 1;
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref0, CONNECT_DEREF0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

const CONNECT_DEREF1: &str = r#"
{
  let f = |x: &mut i64| *x <- *x + 1;
  let v = 41;
  f(&mut v);
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref1, CONNECT_DEREF1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// Refs are first-class runtime values, so a ref read back out of a
// container derefs like any other (`*(a[0]$)` over `Array<&i64>`).
const DEREF_FROM_ARRAY: &str = r#"
{
  let v = 42;
  let a = [&v];
  *(a[0]$)
}
"#;

run!(deref_from_array, DEREF_FROM_ARRAY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(42))
); FuseExpect::Jit);

const DEREF_FROM_TUPLE_FIELD: &str = r#"
{
  let v = 7;
  let p = (&v, 1);
  let s = { x: &v };
  *(p.0) + *(s.x)
}
"#;

run!(deref_from_tuple_field, DEREF_FROM_TUPLE_FIELD, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(14))
); FuseExpect::Jit);

// Place references (design/place_references.md).

// Reads and writes through every accessor kind; the root value is
// rebuilt along the path.
const PLACE_READ_WRITE: &str = r#"
{
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
  let t1 = sys::time::timer(duration:0.05s, false);
  *ra <- t1 ~ 21;
  *rs <- t1 ~ 6;
  *rn <- t1 ~ "P";
  *rt <- t1 ~ 88;
  *rm <- t1 ~ 99;
  let t2 = sys::time::timer(duration:0.2s, false);
  t2 ~ (before, a, s, t, m{"k"}$, (*ra, *rs, *rn, *rt, *rm))
}
"#;

run!(place_read_write, PLACE_READ_WRITE, |v: Result<&Value>| {
    format!("{}", v.unwrap())
        == r#"[[i64:20, i64:5, "p", i64:8, i64:9], [i64:10, i64:21, i64:30], [["tags", ["P", "q"]], ["x", i64:6]], [i64:7, i64:88], i64:99, [i64:21, i64:6, "P", i64:88, i64:99]]"#
}; FuseExpect::Jit);

// A moving reference points where its key says when it fires; two
// writes to one root in one cycle both land; a write into a missing
// place is dropped and the root is untouched.
const PLACE_MOVE_SIBLINGS_BAD: &str = r#"
{
  let a = [1, 2, 3];
  let i = 0;
  let r = &mut a[i];
  let r0 = &mut a[0];
  let r2 = &mut a[2];
  let bad = &mut a[7];
  let first = once(*r);
  let t1 = sys::time::timer(duration:0.05s, false);
  i <- t1 ~ 2;
  let t2 = sys::time::timer(duration:0.12s, false);
  *r <- t2 ~ 30;
  let t3 = sys::time::timer(duration:0.2s, false);
  *r0 <- t3 ~ 100;
  *r2 <- t3 ~ 300;
  *bad <- t3 ~ 9;
  let t4 = sys::time::timer(duration:0.3s, false);
  t4 ~ (first, a, *r)
}
"#;

run!(place_move_siblings_bad, PLACE_MOVE_SIBLINGS_BAD, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:1, [i64:100, i64:2, i64:300], i64:300]"
}; FuseExpect::Jit);

// A lambda over `&State` reaches an editor held in an array through a
// place reference passed as its argument.
const PLACE_THROUGH_PARAM: &str = r#"
{
  type State = { value: string, cursor: i64 };
  let vals: Array<State> = [{ value: "a", cursor: 1 }, { value: "b", cursor: 1 }];
  let bump = |st: &mut State, t: Any| -> null {
    let s = t ~ *st;
    *st <- { value: "[s.value]!", cursor: s.cursor + 1 };
    null
  };
  let t1 = sys::time::timer(duration:0.05s, false);
  let go = t1 ~ 1;
  bump(&mut vals[go], go);
  let t2 = sys::time::timer(duration:0.2s, false);
  t2 ~ vals
}
"#;

run!(place_through_param, PLACE_THROUGH_PARAM, |v: Result<&Value>| {
    format!("{}", v.unwrap())
        == r#"[[["cursor", i64:1], ["value", "a"]], [["cursor", i64:2], ["value", "b!"]]]"#
}; FuseExpect::Jit);

// A reference that became null dereferences to bottom, never to the
// old target: the late read finds nothing and the deadline wins.
const DEREF_NULL_REF_IS_BOTTOM: &str = r#"
{
  let x = 10;
  let r: [&i64, null] = &x;
  r <- null;
  let late = sys::time::timer(duration:0.02s, false) ~ *(r$);
  any(late, sys::time::timer(duration:0.1s, false) ~ -1)
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

run!(deref_fires_on_address, DEREF_FIRES_ON_ADDRESS, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:10, i64:20]"
}; FuseExpect::Jit);

// A place's address is evaluated once: a key with an effect runs it
// once per fire (`n` fires at its binding and once more, not twice
// more), and the read sees that one evaluation.
const PLACE_KEY_EVALUATED_ONCE: &str = r#"
{
  let a = [10, 20];
  let n = 0;
  let r = &a[{ n <- a ~ n + 1; 0 }];
  let t = sys::time::timer(duration:0.02s, false);
  (t ~ count(n), t ~ *r)
}
"#;

run!(place_key_evaluated_once, PLACE_KEY_EVALUATED_ONCE, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:2, i64:10]"
}; FuseExpect::Jit);

// An undetermined key is a bottom reference: a write through it lands
// nowhere; when the key returns the reference retargets (and, as at
// every retarget, the pending write lands there).
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
  let seen = array::group(*r, |n, _| n == 3);
  (t3 ~ a, t5 ~ a, t5 ~ seen)
}
"#;

run!(place_bottom_key, PLACE_BOTTOM_KEY, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:10, i64:20], [i64:10, i64:99], [i64:10, i64:20, i64:99]]"
}; FuseExpect::Jit);

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

run!(place_removed_element, PLACE_REMOVED_ELEMENT, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:20, i64:7], null]"
}; FuseExpect::Jit);

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

run!(place_error_field, PLACE_ERROR_FIELD, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "\"Caught\""
}; FuseExpect::Jit);

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

run!(place_through_deref, PLACE_THROUGH_DEREF, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:20, i64:20]"
}; FuseExpect::Jit);

// A place through a dereference whose reference went bottom has no
// address: it reads nothing and a write through it lands nowhere; when
// the reference returns, the place does (and, as at every retarget,
// the pending write lands there).
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
  let t4 = select step { 11 => null, _ => never() };
  *s <- t4 ~ 7;
  let t5 = select step { 16 => null, _ => never() };
  (t3 ~ a, t5 ~ obs, t5 ~ a)
}
"#;

run!(place_through_bottom_deref, PLACE_THROUGH_BOTTOM_DEREF, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:10, i64:20], null, [i64:7, i64:20]]"
}; FuseExpect::Jit);

// A place indexed past i64::MAX addresses nothing, never from the end:
// its read is bottom and the sample banks.
const PLACE_INDEX_U64_ABOVE_I64_MAX: &str = r#"
{
  let a = [10, 20, 30];
  let r = &a[u64:18446744073709551615];
  let t1 = sys::time::timer(duration:0.02s, false);
  let obs: [i64, null] = null;
  obs <- t1 ~ *r;
  let t2 = sys::time::timer(duration:0.05s, false);
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
#[tokio::test(flavor = "multi_thread")]
async fn place_through_deref_mirror() -> Result<()> {
    use graphix_compiler::Rt;
    let (v, ctx) = graphix_package_core::testing::eval(
        "{ let a = {p: {x: 10}}; let r = &a.p; &(*r).x }",
        crate::TEST_REGISTER,
    )
    .await?;
    let id = match v {
        Value::U64(id) => graphix_compiler::BindId::from(id),
        v => anyhow::bail!("expected a reference, got {v}"),
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
    use super::dense_deltas::{as_i64s, run_delta};
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

run!(place_payload, PLACE_PAYLOAD, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:5, i64:1], i64:7, i64:8, i64:7, i64:8]"
}; FuseExpect::Jit);

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
            "{ let b = 1; b <- sys::time::timer(duration:0.02s, false) ~ 0; \
             let a = [10 / b]; &a[0] }"
        ))
        .await?;
    let eid = compiled.exprs[0].id;
    let mut cell = None;
    let deadline = tokio::time::sleep(std::time::Duration::from_millis(200));
    tokio::pin!(deadline);
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => anyhow::bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, Value::U64(c)) = e && id == eid {
                        cell = Some(graphix_compiler::BindId::from(c));
                    }
                }
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
}; FuseExpect::Jit);

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
    t1 == t2, &mut 0 == &mut 0
  ]
}
"#;

run!(byref_eq_compares_targets, BYREF_EQ_COMPARES_TARGETS, |v: Result<&Value>| {
    let want = [true, true, true, false, true, true, false, false, true, false, true, true, false];
    match v {
        Ok(Value::Array(a)) => a.iter().map(|v| *v == Value::Bool(true)).eq(want),
        _ => false,
    }
}; FuseExpect::Jit);

// References have no order: a reference's value is its cell, numbered in
// whatever order compiling made it. The orderings, sorting, `min`/`max`
// and map keys refuse them, a generic definition's at the call.
#[tokio::test(flavor = "current_thread")]
async fn byref_ordering_refused() -> Result<()> {
    for src in [
        "{ let x = 1; let y = 2; &x < &y }",
        "{ let x = 1; array::len(array::sort([&x])) }",
        "{ let x = 1; map::len({&x => 1}) }",
        "{ let x = 1; let lt = |a, b| a < b; lt(&x, &x) }",
        "{ let x = 1; *min(&x, &x) }",
        "{ let x = 1; map::len(map::insert({}, &x, 1)) }",
    ] {
        match graphix_package_core::testing::eval(src, crate::TEST_REGISTER).await {
            Err(e) => {
                let msg = format!("{e:#}");
                assert!(msg.contains("no order"), "{src}: {msg}")
            }
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

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
}; FuseExpect::Jit);
