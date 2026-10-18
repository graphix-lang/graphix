use anyhow::{Result, anyhow};
use arcstr::literal;
use graphix_package_core::{
    NetConfig, run,
    testing::{self, Collect, FuseExpect, Mode, collect},
};
use netidx::subscriber::Value;
use poolshark::local::LPooled;
use std::{fmt::Write, sync::Arc, time::Duration};

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

// A publication carries the data an abstract value's `Cast` writes, and
// a subscriber reads it back through `from_value`, which refuses what the
// type does not hold: here a plain array that is not sorted.
const NET_ABSTRACT_THROUGH_CAST: &str = r#"
{
  type Sorted = Abstract<Array<i64>>;
  impl Cast for Sorted {
    let from_value = |v| select cast<Array<i64>>(v) {
      error as e => e,
      a => select array::sort(a) == a {
        true => Sorted(a),
        false => error(`InvalidCast("not sorted"))
      }
    }
  };
  sys::net::publish("/local/sorted", Sorted([1, 2]));
  sys::net::publish("/local/plain", [2, 1]);
  let good: Result<Sorted, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/sorted");
  let bad: Result<Sorted, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/plain");
  "[good] [bad]"
}
"#;

run!(net_abstract_through_cast, NET_ABSTRACT_THROUGH_CAST, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"Sorted([1, 2]) error:`InvalidCast("can't cast [2, 1] to Sorted: not sorted")"#)
}; FuseExpect::Jit);

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
}; FuseExpect::None);

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
            // the written i64 casts to "43"
            [Value::I64(42), Value::I64(2)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::None);

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
}; FuseExpect::None);

// An argument a call omits takes the spec's default.
run!(net_rpc_defaults, r#"{
  let p = "/local/rpc_defaults";
  sys::net::rpc(
    #path: p,
    #doc: "add",
    #spec: {a: {default: 1, doc: "a"}, b: {default: 2, doc: "b"}},
    #f: |args: {a: i64, b: i64}| args.a + args.b);
  let r: i64 = sys::net::call(p, {a: 10})?;
  r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(12))); FuseExpect::None);

// A call whose arguments fail the cast is answered with the error and
// never reaches the handler; the next call is served.
run!(net_rpc_refused_cast_answered, r#"{
  let p = "/local/rpc_cast";
  sys::net::rpc(
    #path: p,
    #doc: "add",
    #spec: {a: {default: 1, doc: "a"}, b: {default: 2, doc: "b"}},
    #f: |args: {a: i64, b: i64}| args.a + args.b);
  let bad: [i64, Error<[`RpcError(string), `InvalidCast(string)]>] =
    sys::net::call(p, {a: "x", b: 1});
  let good: i64 = sys::net::call(bad ~ p, {a: 3, b: 4})?;
  (is_err(bad), good)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::Bool(true), Value::I64(7)])
}; FuseExpect::Jit);

// A changed #doc republishes the rpc at its path: the old one frees the
// path when it is dropped, so each republish succeeds and the call after
// it is answered.
run!(net_rpc_republish_answers, r#"{
  let p = "/local/rpc_republish";
  let t = sys::time::timer(duration:60.ms, true);
  let k = 0;
  k <- t ~ k + 1;
  let r = sys::net::rpc(#path: p, #doc: "doc [k]", #spec: {x: {default: 0, doc: "x"}},
    #f: |a: {x: i64}| a.x + k);
  let fails = 0;
  fails <- r ~ fails + 1;
  let res: i64 = sys::net::call(p, sys::time::after_idle(duration:25.ms, t) ~ {x: 100})$;
  let answers = array::group(res, |n, _| n >= 6);
  (array::len(answers), answers ~ fails)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(6), Value::I64(0)])
}; FuseExpect::None);

// A reply to a call the client moved on from lands nowhere: the value
// settles on the latest call's answer.
run!(net_rpc_latest_call_wins, r#"{
  sys::net::rpc(#path: "/local/rpc_slow", #doc: "slow", #spec: null,
    #f: |a: null| sys::time::after_idle(duration:300.ms, a ~ 1));
  sys::net::rpc(#path: "/local/rpc_fast", #doc: "fast", #spec: null,
    #f: |a: null| a ~ 2);
  let p = "/local/rpc_slow";
  p <- sys::time::timer(duration:50.ms, false) ~ "/local/rpc_fast";
  let r: i64 = sys::net::call(p, null)?;
  sys::time::after_idle(duration:600.ms, r)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::None);

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

// The publish twin: a sleeping arm unpublishes and a re-woken one
// republishes from the present value; the observer sees each window it
// catches as an error after a value, each value even and past the last.
// Its resubscribe backs off at random, so it may miss a window.
const NET_PUB_REWAKE: &str = r#"
{
  let x = i64:0;
  let p = "/local/wakepub";
  let t = sys::time::timer(duration:0.15s, true);
  x <- t ~ x + i64:1;
  let flip = select x % i64:2 { i64:0 => `On, _ => `Off };
  select flip { `On => sys::net::publish(p, x), `Off => null };
  let s: Result<i64, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe(p);
  array::group(s, |n, _| n >= i64:4)
}
"#;

run!(net_publish_arm_rewake, NET_PUB_REWAKE, |v: Result<&Value>| {
    let gone = r#"error:["SubscribeError", "unsubscribed"]"#;
    let Ok(Value::Array(a)) = v else { return false };
    let mut last = -1;
    a.chunks(2).all(|w| match w {
        [Value::I64(n), e] if *n > last && n % 2 == 0 && e.to_string() == gone => {
            last = *n;
            true
        }
        _ => false,
    })
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
}; FuseExpect::None);

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

// A reader that wakes gets the path's value though NetState held no Dval of
// it while it slept: the rpc client the call keeps does, so netidx still
// streams the path and sends a new reader nothing.
run!(net_rewoken_reader_of_a_held_path, r#"
{
  sys::net::rpc(#path: "/local/pin/held", #doc: "d", #spec: {x: {default: 0, doc: "x"}}, #f: |a: {x: i64}| a.x)$;
  let c: i64 = sys::net::call("/local/pin/held", sys::time::timer(duration:50.ms, false) ~ {x: 5})$;
  let on = true;
  on <- sys::time::timer(duration:150.ms, true) ~ !on;
  let b = select on {
    true => { let x: Array<string> = sys::net::subscribe("/local/pin/held")$; x },
    false => ["off"]
  };
  let seen = count(select b { [s] if s == "off" => never(), x => x });
  sys::time::timer(duration:700.ms, false) ~ (c, seen)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[0] == Value::I64(5) && matches!(a[1], Value::I64(n) if n >= 2))
}; FuseExpect::Jit);

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
}; FuseExpect::None);

// A subscriber sees its publisher go away as an error, and the value again
// when it comes back.
run!(net_publisher_gone_and_back, r#"
{
  let paths = ["/local/pin/gone"];
  paths <- sys::time::timer(duration:200.ms, false) ~ [];
  paths <- sys::time::timer(duration:700.ms, false) ~ ["/local/pin/gone"];
  array::map(paths, |p| sys::net::publish(p, 1));
  let v: Result<i64, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/pin/gone");
  array::group(v, |n, _| n == 3)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"[i64:1, error:["SubscribeError", "unsubscribed"], i64:1]"#)
}; FuseExpect::Jit);

// A handler that raises answers its caller with the error.
run!(net_rpc_handler_raises, r#"{
  sys::net::rpc(#path: "/local/pin/raise", #doc: "raise", #spec: null,
    #f: |a: null| { let e: Result<i64, `Boom> = error(`Boom); a ~ e? });
  let r: Result<i64, [`RpcError(string), `InvalidCast(string)]> =
    sys::net::call("/local/pin/raise", null);
  r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if matches!(&**e, Value::Array(a)
        if a[0] == Value::from("RpcError")
            && matches!(&a[1], Value::String(m) if m.contains("Boom"))))
}; FuseExpect::None);

const NET_RPC_MISSING: &str = r#"{
  let r: Result<i64, [`RpcError(string), `InvalidCast(string)]> =
    sys::net::call("/local/pin/missing", null);
  r
}"#;

/// netidx's rpc client gives up on an unpublished procedure after 10 s.
#[tokio::test]
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
async fn net_rpc_missing_path() -> Result<()> {
    for mode in Mode::ALL {
        let first = Collect::First(1);
        let v = collect(
            NET_RPC_MISSING,
            &crate::TEST_REGISTER,
            mode,
            first,
            Duration::from_secs(20),
        )
        .await?;
        assert_eq!(
            v[0].to_string(),
            r#"error:["RpcError", "timeout subscribing to procedure"]"#,
            "{mode:?}"
        );
    }
    Ok(())
}

// A subscription that moves to another path and back gets the first
// path's value again.
const NET_RESUBSCRIBE: &str = r#"
{
  sys::net::publish("/local/resub/a", "a");
  sys::net::publish("/local/resub/b", "b");
  let path = "/local/resub/a";
  let n = 0;
  n <- sys::time::timer(duration:100.ms, 2) ~ n + 1;
  path <- n ~ select n { 1 => "/local/resub/b", _ => "/local/resub/a" };
  let v: string = sys::net::subscribe(path)?;
  array::group(v, |n, _| n == 3)
}
"#;

run!(net_resubscribe, NET_RESUBSCRIBE, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["a", "b", "a"]"#)
}; FuseExpect::None);

// A subscription moving between busy paths never answers with a value of
// the path it left.
const NET_NO_STALE_VALUE: &str = r#"
{
  type R = Result<string, [`InvalidCast(string), `SubscribeError(string)]>;
  let tick = 0;
  tick <- sys::time::timer(duration:2.ms, true) ~ tick + 1;
  let pubs = array::init(4, |k| sys::net::publish("/local/stale/k[k]", "k[k]:[tick]"));
  let held: Array<R> = array::init(4, |k| sys::net::subscribe("/local/stale/k[k]"));
  let j = 0;
  j <- sys::time::timer(duration:10.ms, 100) ~ j + 1;
  let path = "/local/stale/k0";
  path <- j ~ "/local/stale/k[j % 4]";
  let r: R = sys::net::subscribe(path);
  let got = r$ ~ (str::basename(path)$, r$);
  let wrong = 0;
  let good = 0;
  select got {
    (k, s) if str::starts_with(#pfx: "[k]:", s) => good <- got ~ good + 1,
    _ => wrong <- got ~ wrong + 1
  };
  sys::time::timer(duration:1.5s, false) ~ (wrong, good)
}
"#;

/// The race needs the pump running beside the runtime, which `run!`'s
/// single-threaded runtimes never do.
#[tokio::test(flavor = "multi_thread")]
async fn net_no_stale_value() -> Result<()> {
    for mode in Mode::ALL {
        let first = Collect::First(1);
        let v = collect(
            NET_NO_STALE_VALUE,
            &crate::TEST_REGISTER,
            mode,
            first,
            Duration::from_secs(10),
        )
        .await?;
        match &v[..] {
            [Value::Array(a)] => match &a[..] {
                // hundreds of good deliveries over the 100 moves
                [Value::I64(0), Value::I64(good)] if *good >= 20 => (),
                _ => panic!(
                    "values of the path left, or too few of the path taken, {mode:?}: {v:?}"
                ),
            },
            _ => panic!("{mode:?}: {v:?}"),
        }
    }
    Ok(())
}

fn write_error_program(path: &str) -> LPooled<String> {
    let mut code: LPooled<String> = LPooled::take();
    write!(
        code,
        r#"
    {{
        let result = never();
        catch(e) result <- select (e.0).error {{ `WriteError(message) => message }};
        sys::net::write("{path}", 1)?;
        result
    }}
    "#
    )
    .unwrap();
    code
}

run!(net_write_invalid_path_error_tag, write_error_program("relative/path"), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("invalid path") && s.contains("relative/path"))
}, timeout: 2; FuseExpect::Jit);

#[tokio::test]
async fn net_write_config_error_tag() -> Result<()> {
    let (value, ctx) = testing::eval_with_setup(
        &write_error_program("/local/unavailable"),
        &crate::TEST_REGISTER,
        |ctx| {
            ctx.libstate.set(NetConfig::Load {
                load: Arc::new(|| {
                    Err(anyhow!("config unavailable").context("test loader"))
                }),
                bind: None,
            });
        },
    )
    .await?;
    assert_eq!(value, Value::String(literal!("test loader: config unavailable")));
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn net_lists_reject_relative_paths() -> Result<()> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    for operation in ["list", "list_table"] {
        let loads = Arc::new(AtomicUsize::new(0));
        let setup_loads = loads.clone();
        let mut code: LPooled<String> = LPooled::take();
        write!(
            code,
            r#"
        {{
            let trigger = 0;
            trigger <- sys::time::after_idle(duration:10.ms, 1);
            let message = never();
            catch(e) message <- select (e.0).error {{ `ListError(text) => text }};
            sys::net::{operation}(#update: trigger, "relative/path")?;
            array::group(message, |n, _| n == 2)
        }}
        "#
        )?;
        let (value, ctx) =
            testing::eval_with_setup(&code, &crate::TEST_REGISTER, |ctx| {
                ctx.libstate.set(NetConfig::Load {
                    load: Arc::new(move || {
                        setup_loads.fetch_add(1, Ordering::SeqCst);
                        Err(anyhow!("unexpected loader use"))
                    }),
                    bind: None,
                });
            })
            .await?;
        let Value::Array(messages) = value else { panic!("expected two errors") };
        assert_eq!(messages.len(), 2);
        for message in messages.iter() {
            assert!(
                matches!(message, Value::String(s) if s.starts_with("invalid path") && s.contains("relative/path")),
                "{operation}: {message}"
            );
        }
        assert_eq!(loads.load(Ordering::SeqCst), 0);
        ctx.shutdown().await;
    }
    Ok(())
}

async fn request_restarts_after_wake(call: &str) -> Result<()> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    let loads = Arc::new(AtomicUsize::new(0));
    let setup_loads = loads.clone();
    let mut code: LPooled<String> = LPooled::take();
    write!(
        code,
        r#"{{
        let path = "/local/restart";
        let args = null;
        let trigger = null;
        let on = true;
        on <- sys::time::after_idle(duration:100.ms, false);
        on <- sys::time::after_idle(duration:200.ms, true);
        let errors = 0;
        catch(e) errors <- e ~ errors + 1;
        let response: Any = select on {{
            true => {call},
            false => never()
        }};
        sys::time::after_idle(duration:400.ms, null) ~ errors
    }}"#
    )?;
    let (value, ctx) = testing::eval_with_setup(&code, &crate::TEST_REGISTER, |ctx| {
        ctx.libstate.set(NetConfig::Load {
            load: Arc::new(move || {
                setup_loads.fetch_add(1, Ordering::SeqCst);
                Err(anyhow!("restart probe"))
            }),
            bind: None,
        });
    })
    .await?;
    ctx.shutdown().await;
    assert_eq!(value, Value::I64(2), "{call}");
    assert_eq!(loads.load(Ordering::SeqCst), 2, "{call}");
    Ok(())
}

#[tokio::test]
async fn net_call_restarts_after_wake() -> Result<()> {
    request_restarts_after_wake(
        "{ let answer: i64 = sys::net::call(path, args)?; answer }",
    )
    .await
}

#[tokio::test]
async fn net_list_restarts_after_wake() -> Result<()> {
    request_restarts_after_wake("sys::net::list(#update:trigger, path)?").await
}

#[tokio::test]
async fn net_list_table_restarts_after_wake() -> Result<()> {
    request_restarts_after_wake("sys::net::list_table(#update:trigger, path)?").await
}

run!(net_write_standing_after_wake, r#"{
    let path = "/local/standing_write";
    let value = 42;
    let writes = 0;
    sys::net::publish(#on_write: |v: i64| writes <- v ~ writes + 1, path, 0);
    let ready: i64 = once(sys::net::subscribe(path)?);
    let on = ready ~ true;
    on <- sys::time::after_idle(duration:100.ms, ready) ~ false;
    on <- sys::time::after_idle(duration:200.ms, ready) ~ true;
    select on {
        true => sys::net::write(path, value),
        false => never()
    };
    sys::time::after_idle(duration:400.ms, ready) ~ writes
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::None);
