// CR claude for claude: [test-gap] These pins, and the other sys::net uses under
// stdlib/graphix-tests, cover successful deliveries only. No test fails if any of these
// breaks: a write's select arm sleeping and waking, a publisher going away under a
// subscriber, an rpc server failing or its path missing, a written value or call
// argument failing its cast, a second subscriber joining a path, updates arriving
// faster than a cycle, publish's path changing while its value is bottom, or rpc
// replies arriving out of order. graphix-fuzz marks every program naming sys::net as
// oracle_tier Excluded (graphix-fuzz/src/lib.rs:798), which never records a divergence,
// so these pins are the only check sys::net has. Add one per case with its fix.
// (sys-net-19)
// 2026-10-07 claude: pinned with their fixes: a write's arm sleeping and waking
// (net_write_after_wake), a refused cast (net_write_refused_cast), a second
// subscriber (net_second_subscriber), a burst (net_burst_in_order) and a path
// moving while the value is bottom (net_publish_path_moves_while_bottom). Still
// unpinned: a publisher going away, an rpc server failing or missing, and rpc
// replies out of order (sys-net-09, Eric's).
use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
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
}; FuseExpect::None);

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
}; FuseExpect::Jit);

const NET_WRITE1: &str = r#"
{
  let p = "/local/foo";
  let x = 42;
  sys::net::publish(#on_write:|v: string| x <- str::len(v), p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write1, NET_WRITE1, |v: Result<&Value>| {
    // the i64 write reaches the callback cast to its string type
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(n)] => *n == "i64:43".len() as i64 || *n == 2,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

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
}; FuseExpect::None);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A written value the on_write callback cannot take answers the writer and
// never reaches the callback.
run!(net_write_refused_cast, r#"
{
  let p = "/local/pin/refuse";
  let x = 0;
  sys::net::publish(#on_write: |v: i64| x <- v, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s ~ "abc"));
  sys::net::write(p, sys::time::after_idle(duration:50.ms, once(s)) ~ 5);
  array::group(s, |n, _| n == 2)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if &a[..] == [Value::I64(0), Value::I64(5)])
}; FuseExpect::Jit);

// A write in an arm that sleeps writes again after its wake, to the path
// as it stands.
run!(net_write_after_wake, r#"
{
  let x = 0;
  let log: Array<i64> = [];
  let p = "/local/pin/wakewrite";
  sys::net::publish(#on_write: |v: i64| log <- v ~ array::push(log, v), p, 0);
  x <- sys::time::timer(duration:20.ms, true) ~ x + 1;
  let flip = select x % 3 { 0 => `Off, _ => `On };
  select flip { `On => sys::net::write(p, x * 100), `Off => null };
  select array::len(log) { 4 => log, _ => never() }
}
"#, |v: Result<&Value>| {
    let want = [Value::I64(100), Value::I64(200), Value::I64(400), Value::I64(500)];
    matches!(v, Ok(Value::Array(a)) if &a[..] == want)
}; FuseExpect::Jit);

// A second subscriber of a path gets its value without the first one's
// firing again.
run!(net_second_subscriber, r#"
{
  sys::net::publish("/local/pin/dup", 42);
  let a: i64 = sys::net::subscribe("/local/pin/dup")$;
  let b: i64 = sys::net::subscribe(sys::time::timer(duration:50.ms, false) ~ "/local/pin/dup")$;
  let na = count(a);
  sys::time::after_idle(duration:150.ms, b) ~ (na, b)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if &a[..] == [Value::I64(1), Value::I64(42)])
}; FuseExpect::None);

// Every update of a burst reaches the subscriber, in order.
run!(net_burst_in_order, r#"
{
  let x = 0;
  sys::net::publish("/local/pin/burst", x);
  let s: i64 = sys::net::subscribe("/local/pin/burst")$;
  let go = false;
  go <- once(s) ~ true;
  x <- select (go, x) { (true, n) if n < 200 => n + 1, _ => never() };
  let c = count(s);
  select s { 200 => c, _ => never() }
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(201)))
}; FuseExpect::Jit);

// A path that moves while the published value is bottom takes the next
// value.
run!(net_publish_path_moves_while_bottom, r#"
{
  let p = "/local/pin/pa";
  let src: [i64, null] = 1;
  src <- sys::time::timer(duration:30.ms, false) ~ null;
  p <- sys::time::timer(duration:60.ms, false) ~ "/local/pin/pb";
  src <- sys::time::timer(duration:90.ms, false) ~ 2;
  sys::net::publish(p, src$);
  let pb: i64 = sys::net::subscribe("/local/pin/pb")$;
  pb
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; FuseExpect::Jit);
