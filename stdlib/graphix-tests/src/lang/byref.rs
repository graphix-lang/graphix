// Tests for by-reference operations

use anyhow::Result;
use graphix_package_core::run;
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::None);

const CONNECT_DEREF0: &str = r#"
{
  let v = 41;
  let r = &v;
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
}; graphix_package_core::testing::FuseExpect::Jit);

const CONNECT_DEREF1: &str = r#"
{
  let f = |x: &i64| *x <- *x + 1;
  let v = 41;
  f(&v);
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref1, CONNECT_DEREF1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
); graphix_package_core::testing::FuseExpect::Jit);

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
); graphix_package_core::testing::FuseExpect::Jit);

// Place references (design/place_references.md).

// Reads and writes through every accessor kind; the root value is
// rebuilt along the path.
const PLACE_READ_WRITE: &str = r#"
{
  let a = [10, 20, 30];
  let s = { x: 5, tags: ["p", "q"] };
  let t = (7, 8);
  let m = {"k" => 9};
  let ra = &a[1];
  let rs = &s.x;
  let rn = &s.tags[0];
  let rt = &t.1;
  let rm = &m{"k"};
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
}; graphix_package_core::testing::FuseExpect::Jit);

// A moving reference points where its key says when it fires; two
// writes to one root in one cycle both land; a write into a missing
// place is dropped and the root is untouched.
const PLACE_MOVE_SIBLINGS_BAD: &str = r#"
{
  let a = [1, 2, 3];
  let i = 0;
  let r = &a[i];
  let r0 = &a[0];
  let r2 = &a[2];
  let bad = &a[7];
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
}; graphix_package_core::testing::FuseExpect::Jit);

// A lambda over `&State` reaches an editor held in an array through a
// place reference passed as its argument.
const PLACE_THROUGH_PARAM: &str = r#"
{
  type State = { value: string, cursor: i64 };
  let vals: Array<State> = [{ value: "a", cursor: 1 }, { value: "b", cursor: 1 }];
  let bump = |st: &State, t: Any| -> null {
    let s = t ~ *st;
    *st <- { value: "[s.value]!", cursor: s.cursor + 1 };
    null
  };
  let t1 = sys::time::timer(duration:0.05s, false);
  let go = t1 ~ 1;
  bump(&vals[go], go);
  let t2 = sys::time::timer(duration:0.2s, false);
  t2 ~ vals
}
"#;

run!(place_through_param, PLACE_THROUGH_PARAM, |v: Result<&Value>| {
    format!("{}", v.unwrap())
        == r#"[[["cursor", i64:1], ["value", "a"]], [["cursor", i64:2], ["value", "b!"]]]"#
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// An undetermined key is a bottom reference: a write through it lands
// nowhere; when the key returns the reference retargets (and, as at
// every retarget, the pending write lands there).
const PLACE_BOTTOM_KEY: &str = r#"
{
  let a = [10, 20];
  let k: [i64, null] = 0;
  let r = &a[k$];
  let t1 = sys::time::timer(duration:0.02s, false);
  k <- t1 ~ null;
  let t2 = sys::time::timer(duration:0.05s, false);
  *r <- t2 ~ 99;
  let t3 = sys::time::timer(duration:0.08s, false);
  let t4 = sys::time::timer(duration:0.11s, false);
  k <- t4 ~ 1;
  let t5 = sys::time::timer(duration:0.16s, false);
  let seen = array::group(*r, |n, _| n == 3);
  (t3 ~ a, t5 ~ a, t5 ~ seen)
}
"#;

run!(place_bottom_key, PLACE_BOTTOM_KEY, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:10, i64:20], [i64:10, i64:99], [i64:10, i64:20, i64:99]]"
}; graphix_package_core::testing::FuseExpect::Jit);

// A place the root no longer has is bottom, not its last value: a
// connect sampling it then writes nothing.
const PLACE_REMOVED_ELEMENT: &str = r#"
{
  let a = [10, 20];
  let r = &a[1];
  let t1 = sys::time::timer(duration:0.02s, false);
  a <- t1 ~ [5];
  let t2 = sys::time::timer(duration:0.05s, false);
  let obs: [i64, null] = null;
  obs <- t2 ~ *r;
  let t3 = sys::time::timer(duration:0.08s, false);
  a <- t3 ~ [6, 7];
  let seen = array::group(*r, |n, _| n == 2);
  let t4 = sys::time::timer(duration:0.11s, false);
  (t4 ~ seen, t4 ~ obs)
}
"#;

run!(place_removed_element, PLACE_REMOVED_ELEMENT, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:20, i64:7], null]"
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// Parentheses are transparent in a place, and a place through a
// dereferenced place reference composes the paths: the write reaches
// the root.
const PLACE_THROUGH_DEREF: &str = r#"
{
  let a = {p: {x: 10, y: 1}};
  let r: &{x: i64, y: i64} = &a.p;
  let s = &(*r).x;
  let t1 = sys::time::timer(duration:0.02s, false);
  *s <- t1 ~ 20;
  let t2 = sys::time::timer(duration:0.05s, false);
  (t2 ~ a.p.x, t2 ~ *s)
}
"#;

run!(place_through_deref, PLACE_THROUGH_DEREF, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:20, i64:20]"
}; graphix_package_core::testing::FuseExpect::Jit);

// A place through a dereference whose reference went bottom has no
// address: it reads nothing and a write through it lands nowhere; when
// the reference returns, the place does (and, as at every retarget,
// the pending write lands there).
const PLACE_THROUGH_BOTTOM_DEREF: &str = r#"
{
  let a = [10, 20];
  let r: [&Array<i64>, null] = &a;
  let s = &(*r$)[0];
  let t1 = sys::time::timer(duration:0.02s, false);
  r <- t1 ~ null;
  let t2 = sys::time::timer(duration:0.05s, false);
  *s <- t2 ~ 99;
  let obs: [i64, null] = null;
  obs <- t2 ~ *s;
  let t3 = sys::time::timer(duration:0.08s, false);
  r <- t3 ~ &a;
  let t4 = sys::time::timer(duration:0.11s, false);
  *s <- t4 ~ 7;
  let t5 = sys::time::timer(duration:0.16s, false);
  (t3 ~ a, t5 ~ obs, t5 ~ a)
}
"#;

run!(place_through_bottom_deref, PLACE_THROUGH_BOTTOM_DEREF, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:10, i64:20], null, [i64:7, i64:20]]"
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::None);

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
