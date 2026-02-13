use crate::test::{REGISTER, ROOT};
// Basic language feature tests: bindings, arithmetic, scoping

use crate::run;
use crate::test::init;
use anyhow::{bail, Result};
use arcstr::ArcStr;
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
                    GXEvent::Env(_) => (),
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
});

const NAME_MODPATH: &str = r#"
{
  let z = "baz";
  str::join(#sep:", ", "foo", "bar", z)
}
"#;

run!(name_modpath, NAME_MODPATH, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "foo, bar, baz",
    _ => false,
});

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
});

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
});

const ANY0: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  any(z, x, y)
}
"#;

run!(any0, ANY0, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const ANY1: &str = r#"
{
  let x = 1;
  let y = "[x] + 1";
  let z = [y, y];
  any(z, x, y)
}
"#;

run!(any1, ANY1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s0), Value::String(s1)] => {
            &**s0 == "1 + 1" && s0 == s1
        }
        _ => false,
    },
    _ => false,
});

const OR_NEVER: &str = r#"
{
    let a = [error("foo"), 42];
    array::iter(a)$
}
"#;

run!(or_never, OR_NEVER, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});
