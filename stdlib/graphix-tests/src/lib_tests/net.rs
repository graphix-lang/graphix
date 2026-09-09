use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const NET_PUB_SUB: &str = r#"
{
  sys::net::publish("/local/foo", 42);
  let v: i64 = sys::net::subscribe("/local/foo")?;
  v
}
"#;

run!(net_pub_sub, NET_PUB_SUB, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const NET_WRITE0: &str = r#"
{
  let p = "/local/foo";
  let x = 42;
  sys::net::publish(#on_write:|v| x <- cast<i64>(v)?, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write0, NET_WRITE0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(43)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const NET_WRITE1: &str = r#"
{
  let p = "/local/foo";
  let x = 42;
  sys::net::publish(#on_write:|v: string| x <- cast<i64>(v)?, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write1, NET_WRITE1, |v: Result<&Value>| {
    // the i64 write is cast to string and `cast<i64>` in the callback
    // converts it back
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(43)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const NET_LIST: &str = r#"
{
  sys::net::publish("/local/foo", 42);
  sys::net::publish("/local/bar", 42);
  sys::net::list("/local")
}
"#;

run!(net_list, NET_LIST, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => {
                let mut a = [s0, s1];
                a.sort();
                a[0] == "/local/bar" && a[1] == "/local/foo"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const NET_LIST_TABLE: &str = r#"
{
  sys::net::publish("/local/t/0/foo", 42);
  sys::net::publish("/local/t/0/bar", 42);
  sys::net::publish("/local/t/1/foo", 42);
  sys::net::publish("/local/t/1/bar", 42);
  let t = dbg(sys::net::list_table("/local/t"))?;
  (array::sort(t.columns) == ["bar", "foo"])
  && (array::sort(t.rows) == ["/local/t/0", "/local/t/1"])
}
"#;

run!(net_list_table, NET_LIST_TABLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const NET_RPC0: &str = r#"
{
  let get_val = "/local/get_val";
  let set_val = "/local/set_val";
  let v: Any = never();
  sys::net::rpc(
    #path:get_val,
    #doc:"get the value",
    #spec:null,
    #f:|a: null| a ~ v);
  sys::net::rpc(
    #path:set_val,
    #doc:"set the value",
    #spec:{val: {default: null, doc: "The value"}},
    #f:|args: {val: Any}| {
      v <- args.val;
      args.val ~ null
    });
  let r: null = sys::net::call(set_val, {val: 42})?;
  let r2: i64 = sys::net::call(get_val, r)?;
  r2
}
"#;

run!(net_rpc0, NET_RPC0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// A re-woken arm's subscribe re-establishes from the present path (the
// path is a binding). The matcher wants a delivery, a sleep marker (-1),
// then a delivery of a newer value after the rewake.
const NET_SUB_REWAKE: &str = r#"
{
  let x = i64:0;
  let p = "/local/wakesub";
  sys::net::publish(p, x);
  let t = sys::time::timer(duration:0.15s, true);
  x <- t ~ x + i64:1;
  let flip = select x % i64:2 { i64:0 => `On, _ => `Off };
  let got = select flip {
    `On => { let v: i64 = sys::net::subscribe(p)$; v },
    `Off => i64:-1
  };
  array::group(got, |n, _| n >= i64:6)
}
"#;

run!(net_subscribe_arm_rewake, NET_SUB_REWAKE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            let vals: Vec<i64> = a
                .iter()
                .filter_map(|v| match v {
                    Value::I64(n) => Some(*n),
                    _ => None,
                })
                .collect();
            let mut first: Option<i64> = None;
            let mut slept = false;
            let mut resub = false;
            for v in vals {
                match (first, slept) {
                    (None, _) if v >= 0 => first = Some(v),
                    (Some(_), false) if v == -1 => slept = true,
                    (Some(f), true) if v > f => resub = true,
                    _ => (),
                }
            }
            resub
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// The publish twin: a re-woken arm republishes from the present
// path/value; the observer subscription rides netidx's durable
// resubscribe across the unpublish window.
const NET_PUB_REWAKE: &str = r#"
{
  let x = i64:0;
  let p = "/local/wakepub";
  let t = sys::time::timer(duration:0.15s, true);
  x <- t ~ x + i64:1;
  let flip = select x % i64:2 { i64:0 => `On, _ => `Off };
  select flip { `On => sys::net::publish(p, x), `Off => null };
  let s: i64 = sys::net::subscribe(p)$;
  array::group(s, |n, _| n >= i64:3)
}
"#;

run!(net_publish_arm_rewake, NET_PUB_REWAKE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            let vals: Vec<i64> = a
                .iter()
                .filter_map(|v| match v {
                    Value::I64(n) => Some(*n),
                    _ => None,
                })
                .collect();
            vals.first().is_some_and(|f| vals.iter().any(|v| v > f))
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);
