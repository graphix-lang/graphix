// Basic language feature tests: bindings, arithmetic, scoping

use crate::init;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use graphix_package_core::run;
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

#[tokio::test(flavor = "current_thread")]
async fn bind_ref_arith() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let gx = ctx.rt;
    let e = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  v
}
"#;
    let e = gx.compile(ArcStr::from(e)).await?;
    let eid = e.exprs[0].id;
    match rx.recv().await {
        None => bail!("runtime died"),
        Some(mut ev) => {
            for e in ev.drain(..) {
                match e {
                    GXEvent::Env(_) | GXEvent::Diagnostic(_, _) => (),
                    GXEvent::Updated(id, v) => {
                        assert_eq!(id, eid);
                        assert_eq!(v, Value::I64(1))
                    }
                }
            }
        }
    }
    Ok(())
}

const MOD0: &str = r#"
{
  let v = 8;
  v % 10
}
"#;

run!(mod0, MOD0, |v: Result<&Value>| match v {
    Ok(&Value::I64(8)) => true,
    _ => false,
});

const SCOPE: &str = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  let x = {
     let v = 42;
     v * 2
  };
  v + x
}
"#;

run!(scope, SCOPE, |v: Result<&Value>| match v {
    Ok(&Value::I64(85)) => true,
    _ => false,
});

const CORE_USE: &str = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  let x = {
     let v = 42;
     once(v * 2)
  };
  [v, x]
}
"#;

run!(core_use, CORE_USE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &**a == &[Value::I64(1), Value::I64(84)] => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const NAME_MODPATH: &str = r#"
{
  let z = "baz";
  str::join(#sep:", ", "foo", "bar", z)
}
"#;

run!(name_modpath, NAME_MODPATH, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "foo, bar, baz",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const STATIC_SCOPE: &str = r#"
{
  let f = |x| x + y;
  let y = 10;
  f(10)
}
"#;

run!(static_scope, STATIC_SCOPE, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const UNDEFINED: &str = r#"
{
  let y = 10;
  let z = x + y;
  let x = 10;
  z
}
"#;

run!(undefined, UNDEFINED, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// A sync variadic builtin called with no positional args has no data
// inputs — the node can never fire (#216: pre-fix this was a silent
// bottom, and in a fused kernel the dead pending DynCall bottomed the
// WHOLE kernel — a value divergence vs the node-walk). Now a compile
// error pointing the user at never().
const DEAD_VARIADIC_ZERO_ARGS: &str = r#"
{
  let v = str::concat();
  v
}
"#;

run!(dead_variadic_zero_args, DEAD_VARIADIC_ZERO_ARGS, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// Labeled args are config, not data — `join(#sep: ",")` with zero
// varargs is just as dead as `concat()`.
const DEAD_VARIADIC_LABELED_ONLY: &str = r#"
{
  let v = str::join(#sep: ",");
  v
}
"#;

run!(dead_variadic_labeled_only, DEAD_VARIADIC_LABELED_ONLY, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// never() is the sanctioned way to write a value that never arrives —
// it must stay legal (it's declared Async, the "later, autonomously,
// or never" contract, so the dead-variadic check exempts it). The
// binding never fires; the block's tail still does.
const NEVER_ZERO_ARGS_OK: &str = r#"
{
  let v = never();
  42
}
"#;

run!(never_zero_args_ok, NEVER_ZERO_ARGS_OK, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ANY0: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  any(z, x, y)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(any0, ANY0, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ANY1: &str = r#"
{
  let x = 1;
  let y = "[x] + 1";
  let z = [y, y];
  any(z, x, y)
}
"#;

// ASPIRE: Jit (currently None) — blocked on: array construction and builtin selection (any) should fuse
run!(any1, ANY1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s0), Value::String(s1)] => {
            &**s0 == "1 + 1" && s0 == s1
        }
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const OR_NEVER: &str = r#"
{
    let a = [error("foo"), 42];
    array::iter(a)$
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(or_never, OR_NEVER, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const WRAP_OVERFLOW: &str = r#"
{
    let a = i64:9223372036854775807 + i64:1;
    let b = u8:255 + u8:1;
    let c = i64:-9223372036854775808 - i64:1;
    let d = i64:3037000500 * i64:3037000500;
    select (a, b, c, d) {
        (i64:-9223372036854775808, u8:0, i64:9223372036854775807, i64:-9223372036709301616) => true,
        _ => false
    }
}
"#;

// Unchecked integer arith WRAPS on overflow in BOTH modes (the JIT's
// iadd/isub/imul always did; the node-walk gained the wrapping fast
// path 2026-07-04 — it previously errored to bottom, which stalled tail
// loops forever). Checked `+?` keeps its catchable ArithError.
run!(wrap_overflow, WRAP_OVERFLOW, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);
