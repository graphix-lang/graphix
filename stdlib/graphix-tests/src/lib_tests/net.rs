use anyhow::{Result, anyhow};
use arcstr::literal;
use graphix_package_core::{
    NetConfig, run,
    testing::{self, Collect, FuseExpect, Mode, collect, is, prints},
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
  sys::net::publish(#on_write:|v: i64| x <- v, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write0, NET_WRITE0, prints("[i64:42, i64:43]"); FuseExpect::None);

// Values that fire before the path is known are written, in order, when
// it arrives.
const NET_WRITE_QUEUED: &str = r#"
{
  let got: Array<i64> = [];
  sys::net::publish(#on_write: |v: i64| got <- v ~ array::push(got, v), "/local/wq", 0);
  let v = array::iter([1, 2, 3]);
  let p: string = never();
  p <- select v { 3 => "/local/wq", _ => never() };
  sys::net::write(p, v);
  select array::len(got) { 3 => got, _ => never() }
}
"#;

run!(net_write_queued, NET_WRITE_QUEUED, prints("[i64:1, i64:2, i64:3]"); FuseExpect::Jit);

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

// A polled listing whose resolver reports nothing changed answers no list,
// and the next tick lists again: a path published after such a poll is seen.
const NET_LIST_POLLED: &str = r#"
{
  sys::net::publish("/local/poll/a", 1);
  sys::net::publish(sys::time::after_idle(duration:700.ms, "/local/poll/b"), 2);
  let tick = sys::time::timer(duration:100.ms, true);
  let n = array::len(sys::net::list(#update: tick, "/local/poll")?);
  sys::time::after_idle(duration:2.s, null) ~ n
}
"#;

run!(net_list_polled_sees_a_later_path, NET_LIST_POLLED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
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
}; FuseExpect::None);

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
  let r: null = sys::net::call(set_val, {val: 42}).1?;
  let r2: i64 = sys::net::call(get_val, r).1?;
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
  let r: i64 = sys::net::call(p, {a: 10}).1?;
  r
}"#, is(Value::I64(12)); FuseExpect::None);

// A call whose arguments fail the cast is answered with the cast's message
// and never reaches the handler; the next call is served.
run!(net_rpc_refused_cast_answered, r#"{
  let p = "/local/rpc_cast";
  sys::net::rpc(
    #path: p,
    #doc: "add",
    #spec: {a: {default: 1, doc: "a"}, b: {default: 2, doc: "b"}},
    #f: |args: {a: i64, b: i64}| args.a + args.b);
  let bad: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call(p, {a: "x", b: 1}).1;
  let good: i64 = sys::net::call(bad ~ p, {a: 3, b: 4}).1?;
  (bad, good)
}"#, |v: Result<&Value>| {
    let bad = r#"error:["RpcError", "can't cast \"x\" to i64: no primitive conversion"]"#;
    matches!(v, Ok(Value::Array(a))
        if a.len() == 2 && a[0].to_string() == bad && a[1] == Value::I64(7))
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
  let res: i64 = sys::net::call(p, sys::time::after_idle(duration:25.ms, t) ~ {x: 100}).1$;
  let answers = array::group(res, |n, _| n >= 6);
  (array::len(answers), answers ~ fails)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(6), Value::I64(0)])
}; FuseExpect::None);

// Two calls in flight from one site answer in the order they complete,
// each with its id: the slow call's handler answers only once the fast
// call's answer is in.
run!(net_rpc_calls_answer_as_they_complete, r#"{
  let release = never<null>();
  let slow_seen = never<null>();
  sys::net::rpc(#path: "/local/ids/slow", #doc: "slow", #spec: null,
    #f: |a: null| { slow_seen <- a; release ~ 1 });
  sys::net::rpc(#path: "/local/ids/fast", #doc: "fast", #spec: null, #f: |a: null| a ~ 2);
  let path = "/local/ids/slow";
  path <- slow_seen ~ "/local/ids/fast";
  let id = sys::net::call_id(path);
  let slow = once(id);
  let (got, r) = sys::net::call(#id: id, path, null);
  let v: i64 = r$;
  release <- select v { 2 => v ~ null, _ => never() };
  array::group((got == slow, v), |n, _| n == 2)
}"#, prints("[[false, i64:2], [true, i64:1]]"); FuseExpect::Jit);

// A call under an id still in flight replaces it: the old call answers
// `Aborted, the new one answers under the same id.
run!(net_rpc_reused_id_replaces, r#"{
  let held_seen = never<null>();
  sys::net::rpc(#path: "/local/ids/held", #doc: "held", #spec: null,
    #f: |a: null| { held_seen <- a; never<i64>() });
  sys::net::rpc(#path: "/local/ids/two", #doc: "two", #spec: null, #f: |a: null| a ~ 2);
  let id = sys::net::call_id(null);
  let path = "/local/ids/held";
  path <- held_seen ~ "/local/ids/two";
  let (got, r) = sys::net::call(#id: id, path, null);
  let v: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> = r;
  array::sort(array::group((got == id, "[v]"), |n, _| n == 2))
}"#, prints(r#"[[true, "2"], [true, "error:`Aborted"]]"#); FuseExpect::None);

// abort_call answers `Aborted, and the reply the server sends after it
// is ignored: the next call is made once the server has replied, and its
// answer is the next one.
run!(net_rpc_abort_call, r#"{
  let release = never<null>();
  let late_seen = never<null>();
  let replied = never<i64>();
  sys::net::rpc(#path: "/local/ids/late", #doc: "late", #spec: null,
    #f: |a: null| { late_seen <- a; let v = release ~ 1; replied <- v; v });
  sys::net::rpc(#path: "/local/ids/next", #doc: "next", #spec: null, #f: |a: null| a ~ 2);
  let path = "/local/ids/late";
  path <- replied ~ "/local/ids/next";
  let id = sys::net::call_id(path);
  let first = once(id);
  sys::net::abort_call(late_seen ~ first);
  let (got, r) = sys::net::call(#id: id, path, null);
  let v: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> = r;
  release <- select v { error as _ => v ~ null, _ => never() };
  array::group((got == first, "[v]"), |n, _| n == 2)
}"#, prints(r#"[[true, "error:`Aborted"], [false, "2"]]"#); FuseExpect::None);

// The calls in flight when the call site's arm sleeps are abandoned: none
// answers, not at the wake and not when the server replies. The arm wakes
// once the server has replied, with a new call, whose answer is the first.
run!(net_rpc_sleep_abandons_silently, r#"{
  let release = never<null>();
  let asleep_seen = never<null>();
  let replied = never<i64>();
  sys::net::rpc(#path: "/local/ids/asleep", #doc: "asleep", #spec: null,
    #f: |a: null| { asleep_seen <- a; let v = release ~ 1; replied <- v; v });
  sys::net::rpc(#path: "/local/ids/awake", #doc: "awake", #spec: null, #f: |a: null| a ~ 2);
  let path = "/local/ids/asleep";
  let on = true;
  on <- asleep_seen ~ false;
  release <- select on { false => on ~ null, true => never() };
  on <- replied ~ true;
  path <- replied ~ "/local/ids/awake";
  let out = select on { true => sys::net::call(path, null), false => never() };
  let (_, r) = out;
  let v: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> = r;
  once("[v]")
}"#, prints(r#""2""#); FuseExpect::None);

// Without #id each call mints a fresh id, and ids key a map.
run!(net_rpc_default_ids_are_fresh, r#"{
  sys::net::rpc(#path: "/local/ids/echo", #doc: "echo", #spec: {x: {default: 0, doc: "x"}},
    #f: |a: {x: i64}| a.x);
  let args = {x: 1};
  let (got, r) = sys::net::call("/local/ids/echo", args);
  let v: i64 = r$;
  args <- once(v) ~ {x: 2};
  let ids = array::group(got, |n, _| n == 2);
  let (a, b) = (ids[0]$, ids[1]$);
  (a != b, map::len({a => 1, b => 2}), array::group(v, |n, _| n == 2))
}"#, prints("[true, i64:2, [i64:1, i64:2]]"); FuseExpect::Jit);

// callq answers in call order: the first call is answered only once the
// server has seen the second (both are in flight at once), whose answer,
// which comes first, waits for it.
run!(net_callq_answers_in_call_order, r#"{
  let second_seen = never<null>();
  let release = never<null>();
  sys::net::rpc(#path: "/local/q/order", #doc: "order", #spec: {n: {default: 0, doc: "n"}},
    #f: |a: {n: i64}| select a.n {
      1 => release ~ 1,
      n => { second_seen <- a ~ null; n }
    });
  release <- second_seen;
  let args = {n: 1};
  args <- once(args) ~ {n: 2};
  let r: i64 = sys::net::callq("/local/q/order", args)$;
  array::group(r, |n, _| n == 2)
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

// An error keeps its call's place: a path that is no path fails at once
// and a reply that does not cast fails on arrival, and both wait for the
// slow call made before them, which answers once the last is seen.
run!(net_callq_errors_keep_their_place, r#"{
  let str_seen = never<null>();
  let release = never<null>();
  sys::net::rpc(#path: "/local/q/slow", #doc: "slow", #spec: null, #f: |a: null| release ~ 1);
  sys::net::rpc(#path: "/local/q/str", #doc: "str", #spec: null,
    #f: |a: null| { str_seen <- a; a ~ "x" });
  release <- str_seen;
  let path = array::iter(["/local/q/slow", "relative", "/local/q/str"]);
  let r: Result<i64, [`RpcError(string), `InvalidCast(string)]> = sys::net::callq(path, null);
  array::group("[r]", |n, _| n == 3)
}"#, prints(r#"["1", "error:`RpcError(\"invalid path\")", "error:`InvalidCast(\"can't cast \\\"x\\\" to i64: no primitive conversion\")"]"#); FuseExpect::None);

// The arm holding callq sleeps with two calls in flight, the second
// answered (a while before the sleep) and waiting for the first: neither
// answers, not when the server replies and not at the wake. The arm
// wakes once the server has replied, with a new call, whose answer is
// the first.
run!(net_callq_sleep_drops_its_calls, r#"{
  let release = never<null>();
  let second_seen = never<null>();
  let replied = never<i64>();
  sys::net::rpc(#path: "/local/q/asleep", #doc: "asleep", #spec: {n: {default: 0, doc: "n"}},
    #f: |a: {n: i64}| select a.n {
      1 => { let v = release ~ 1; replied <- v; v },
      n if n == 2 => { second_seen <- a ~ null; n },
      n => n
    });
  let args = {n: 1};
  args <- once(args) ~ {n: 2};
  let on = true;
  on <- sys::time::after_idle(duration:100.ms, second_seen) ~ false;
  release <- select on { false => on ~ null, true => never() };
  on <- replied ~ true;
  args <- replied ~ {n: 3};
  let r = select on { true => sys::net::callq("/local/q/asleep", args), false => never() };
  let v: Result<i64, [`RpcError(string), `InvalidCast(string)]> = r;
  once("[v]")
}"#, prints(r#""3""#); FuseExpect::Jit);

// callq shares no in-language state with another call site, so a
// `#[parallel]` fan-out over it forks (one with nothing to fork is
// refused).
run!(net_callq_fans_out_in_parallel, r#"{
  sys::net::rpc(#path: "/local/q/echo", #doc: "echo", #spec: {n: {default: 0, doc: "n"}},
    #f: |a: {n: i64}| a.n);
  let r: Array<i64> = #[parallel] array::map([1, 2, 3, 4], |n: i64| -> i64
    sys::net::callq("/local/q/echo", {n})$);
  r
}"#, prints("[i64:1, i64:2, i64:3, i64:4]"); FuseExpect::None);

// A re-woken arm's subscribe re-establishes, and its last event does not
// survive the sleep: every value it delivers, either side of a sleep
// marker (-1), is newer than the last (a resubscribe may first see the
// value published while the arm slept).
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
    let Ok(Value::Array(a)) = v else { return false };
    let mut last = -1;
    let mut slept = false;
    let mut woke = false;
    a.iter().all(|v| match v {
        Value::I64(-1) if last >= 0 => {
            slept = true;
            true
        }
        Value::I64(n) if *n > last => {
            woke |= slept;
            last = *n;
            true
        }
        _ => false,
    }) && woke
}; FuseExpect::Jit);

// The woken subscribe reads the path as it stands at the wake: one moved
// while the arm slept is the one subscribed. Each step follows the
// delivery before it.
run!(net_subscribe_arm_rewake_moved_path, r#"
{
  sys::net::publish("/local/wakepath/a", "a");
  sys::net::publish("/local/wakepath/b", "b");
  let x = 0;
  let p = "/local/wakepath/a";
  p <- select x { 1 => x ~ "/local/wakepath/b", _ => never() };
  let flip = uniq(select x { 1 => `Off, _ => `On });
  let got = select flip {
    `On => { let v: string = sys::net::subscribe(p)$; v },
    `Off => "off"
  };
  x <- count(got);
  array::group(got, |n, _| n >= 3)
}
"#, prints(r#"["a", "off", "b"]"#); FuseExpect::Jit);

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

// A write a publication's handler cannot take answers a writer that waits
// for the answer with the cast's message.
run!(net_write_refusal_answers_the_writer, r#"
{
  let x = 0;
  sys::net::publish(#on_write: |v: i64| x <- v, "/local/pin/wrefuse", x);
  let s: i64 = sys::net::subscribe("/local/pin/wrefuse")$;
  let r: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call(s ~ "/local/pin/wrefuse", null).1;
  r
}
"#, |v: Result<&Value>| {
    let want = r#"error:["RpcError", "can't cast [] to i64: no primitive conversion"]"#;
    matches!(v, Ok(v) if v.to_string() == want)
}; FuseExpect::None);

// A written error reaches a handler whose argument type holds it, and is
// refused for one whose type does not.
run!(net_write_of_an_error, r#"
{
  let refused: Array<string> = [];
  let taken: Array<string> = [];
  sys::net::publish(
    #on_write: |v: [i64, string]| refused <- v ~ array::push(refused, "[v]"),
    "/local/pin/werr/refused",
    0
  );
  sys::net::publish(
    #on_write: |v: [i64, Error<`Boom(string)>]| taken <- v ~ array::push(taken, "[v]"),
    "/local/pin/werr/taken",
    0
  );
  let s: i64 = sys::net::subscribe("/local/pin/werr/refused")$;
  let s2: i64 = sys::net::subscribe("/local/pin/werr/taken")$;
  let e = once(s ~ s2 ~ error(`Boom("remote")));
  sys::net::write("/local/pin/werr/refused", e);
  sys::net::write("/local/pin/werr/taken", e);
  sys::net::write("/local/pin/werr/refused", sys::time::after_idle(duration:50.ms, e) ~ 5);
  select (array::len(refused), array::len(taken)) {
    (1, 1) => (refused, taken),
    _ => never()
  }
}
"#, prints(r#"[["5"], ["error:`Boom(\"remote\")"]]"#); FuseExpect::Jit);

// A write issued before its target connects goes out though the arm that
// issued it sleeps the next cycle.
run!(net_write_survives_a_sleep_before_connect, r#"
{
  let got = never();
  sys::net::publish(#on_write: |x: i64| got <- x, "/local/pin/writesleep", 0);
  let ev: [`Go(i64), `Stop] = `Go(42);
  ev <- once(ev ~ `Stop);
  select ev { `Go(v) => sys::net::write("/local/pin/writesleep", v), `Stop => never() };
  got
}
"#, is(Value::I64(42)); FuseExpect::Jit);

// An unchanged path written in a form other than its canonical one
// re-fires without tearing down what it names: the subscription delivers
// once, the standing value is written once, and the publication and the
// rpc are published once.
run!(net_noncanonical_path_refires, r#"
{
  sys::net::publish("/local/pin/canon/s", 7);
  let writes = 0;
  sys::net::publish(#on_write: |v: i64| writes <- v ~ writes + 1, "/local/pin/canon/w", 0);
  let t = sys::time::timer(duration:20.ms, true);
  let v: i64 = sys::net::subscribe(t ~ "/local/pin/canon/s/")$;
  sys::net::write(t ~ "/local/pin/canon/w/", 5);
  sys::net::publish(t ~ "/local/pin/canon/p/", 7);
  let p: Result<i64, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/pin/canon/p");
  sys::net::rpc(#path: t ~ "/local/pin/canon/r/", #doc: "d", #spec: null, #f: |a: null| 1);
  let d: Result<string, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/pin/canon/r/doc");
  let counts = (count(v), writes, count(p), count(d));
  let ticks = count(t);
  let start = once(counts ~ ticks);
  select ticks { n if n == start + 10 => ticks ~ counts, _ => never() }
}
"#, |v: Result<&Value>| {
    let one = [Value::I64(1), Value::I64(1), Value::I64(1), Value::I64(1)];
    matches!(v, Ok(Value::Array(a)) if a[..] == one)
}; FuseExpect::Jit);

// A write in an arm that sleeps writes again after its wake, to the path
// as it stands. Each x follows the write before it landing, or the sleep.
run!(net_write_after_wake, r#"
{
  let x = 1;
  let log: Array<i64> = [];
  let p = "/local/pin/wakewrite";
  sys::net::publish(#on_write: |v: i64| log <- v ~ array::push(log, v), p, 0);
  let flip = select x % 3 { 0 => `Off, _ => `On };
  select flip { `On => sys::net::write(p, x * 100), `Off => null };
  x <- select array::len(log) { 0 => never(), n => n ~ x + 1 };
  x <- select flip { `Off => flip ~ x + 1, `On => never() };
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
// it while it slept: the rpc client the call keeps does, so the path is
// already streamed when the reader joins.
// The reader starts once the call answers, sleeps once it has seen the
// value, and wakes the cycle after.
run!(net_rewoken_reader_of_a_held_path, r#"
{
  sys::net::rpc(#path: "/local/pin/held", #doc: "d", #spec: {x: {default: 0, doc: "x"}}, #f: |a: {x: i64}| a.x)$;
  let c: i64 = sys::net::call("/local/pin/held", {x: 5}).1$;
  let on = true;
  let b = select on {
    true => { let x: Array<string> = sys::net::subscribe(c ~ "/local/pin/held")$; x },
    false => ["off"]
  };
  let seen = count(select b { [s] if s == "off" => never(), x => x });
  on <- select seen { 1 => seen ~ false, _ => never() };
  on <- select on { false => on ~ true, true => never() };
  select seen { 2 => (c, seen), _ => never() }
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[0] == Value::I64(5) && matches!(a[1], Value::I64(n) if n >= 2))
}; FuseExpect::Jit);

// A reader joining a busy path sees each value newer than the last: its
// first is the path's value as it joined, and no update it already
// has follows.
run!(net_late_reader_of_a_busy_path, r#"
{
  let x = 0;
  sys::net::publish("/local/pin/late", x);
  let a: i64 = sys::net::subscribe("/local/pin/late")$;
  let go = false;
  go <- once(a) ~ true;
  x <- select (go, x) { (true, n) if n < 30000 => n + 1, _ => never() };
  let late = select a { n if n >= 500 => a ~ "/local/pin/late", _ => never() };
  let b: i64 = sys::net::subscribe(late)$;
  let prev = -1;
  prev <- b;
  let rising = true;
  rising <- b ~ (rising && b > prev);
  let nb = count(b);
  select b { 30000 => (rising && b > prev, nb > 1), _ => never() }
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if &a[..] == [Value::Bool(true), Value::Bool(true)])
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
// when it comes back. The publisher goes once the value is seen and comes
// back once its loss is.
run!(net_publisher_gone_and_back, r#"
{
  let paths = ["/local/pin/gone"];
  array::map(paths, |p| sys::net::publish(p, 1));
  let v: Result<i64, [`SubscribeError(string), `InvalidCast(string)]> =
    sys::net::subscribe("/local/pin/gone");
  let n = count(v);
  paths <- select n { 1 => n ~ [], 2 => n ~ ["/local/pin/gone"], _ => never() };
  array::group(v, |n, _| n == 3)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"[i64:1, error:["SubscribeError", "unsubscribed"], i64:1]"#)
}; FuseExpect::Jit);

// A handler that raises answers its caller a generic RpcError, the error
// it raised kept on the server.
run!(net_rpc_handler_raises, r#"{
  sys::net::rpc(#path: "/local/pin/raise", #doc: "raise", #spec: null,
    #f: |a: null| { let e: Result<i64, `Boom> = error(`Boom); a ~ e? });
  let r: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call("/local/pin/raise", null).1;
  r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if matches!(&**e, Value::Array(a)
        if a[0] == Value::from("RpcError")
            && matches!(&a[1], Value::String(m) if &**m == "the rpc failed")))
}; FuseExpect::None);

// Past #max_inflight unanswered calls, a call is refused: the second is
// made once the handler holds the first.
run!(net_rpc_max_inflight, r#"{
  let holding = never<null>();
  sys::net::rpc(#path: "/local/pin/inflight", #doc: "hold", #spec: null, #max_inflight: 1,
    #f: |a: null| { holding <- a; never<i64>() });
  let held: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call("/local/pin/inflight", null).1;
  let refused: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call(holding ~ "/local/pin/inflight", null).1;
  refused
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if matches!(&**e, Value::Array(a)
        if a[0] == Value::from("RpcError")
            && matches!(&a[1], Value::String(m) if m.contains("1 calls are already in flight"))))
}; FuseExpect::None);

const NET_RPC_MISSING: &str = r#"{
  let r: Result<i64, [`RpcError(string), `InvalidCast(string), `Aborted]> =
    sys::net::call("/local/pin/missing", null).1;
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
// path's value again. Each move follows the delivery before it.
const NET_RESUBSCRIBE: &str = r#"
{
  sys::net::publish("/local/resub/a", "a");
  sys::net::publish("/local/resub/b", "b");
  let path = "/local/resub/a";
  let v: string = sys::net::subscribe(path)?;
  let n = 0;
  n <- v ~ n + 1;
  path <- select n { 1 => n ~ "/local/resub/b", 2 => n ~ "/local/resub/a", _ => never() };
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

/// `call` in an arm that sleeps and wakes, issued `issues` times: a call
/// once (a wake does not repeat it), a standing listing again at the wake.
async fn request_after_wake(call: &str, issues: i64) -> Result<()> {
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
    assert_eq!(value, Value::I64(issues), "{call}");
    assert_eq!(loads.load(Ordering::SeqCst) as i64, issues, "{call}");
    Ok(())
}

#[tokio::test]
async fn net_call_not_repeated_after_wake() -> Result<()> {
    request_after_wake("{ let answer: i64 = sys::net::call(path, args).1?; answer }", 1)
        .await
}

#[tokio::test]
async fn net_list_restarts_after_wake() -> Result<()> {
    request_after_wake("sys::net::list(#update:trigger, path)?", 2).await
}

#[tokio::test]
async fn net_list_table_restarts_after_wake() -> Result<()> {
    request_after_wake("sys::net::list_table(#update:trigger, path)?", 2).await
}

// A wake re-attaches a write to its path but writes nothing until the
// value fires: the one write is the arm's first entry's.
run!(net_write_not_repeated_after_wake, r#"{
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
}"#, is(Value::I64(1)); FuseExpect::None);

// A write handler's argument and an rpc's are read from data: the check
// refuses a function, a reference and a type it cannot settle, so every
// write reaches the handler cast (x-builtin-effects-02, gx-stdlib-01,
// sys-net-03, tests-lib-b2-01, tests-lib-b2-02).
run!(net_on_write_open_type_refused, r#"{
    let n: i64 = -1;
    sys::net::publish(#on_write: |v: Array<'b>| n <- v ~ array::len(v), "/local/ow_open", 0);
    n
}"#, testing::refused("must be fully known"); FuseExpect::None);

run!(net_on_write_inferred_part_refused, r#"{
    let n: i64 = -1;
    sys::net::publish(#on_write: |v| { let (a, b) = v; n <- a }, "/local/ow_part", 0);
    n
}"#, testing::refused("must be fully known"); FuseExpect::None);

// A write while the handler is null reaches nothing; one after it arrives
// is taken.
run!(net_on_write_null_handler, r#"{
    let got: Array<i64> = [];
    let p = "/local/ow_null";
    let armed = false;
    let h = select armed { false => null, true => |v: i64| got <- v ~ array::push(got, v) };
    sys::net::publish(#on_write: h, p, 0);
    let s: i64 = sys::net::subscribe(p)$;
    sys::net::write(p, once(s) ~ 5);
    armed <- sys::time::after_idle(duration: 100.ms, once(s)) ~ true;
    sys::net::write(p, select armed { true => 7, false => never() });
    let taken = select array::len(got) { 0 => never(), _ => got };
    sys::time::after_idle(duration: 200.ms, taken) ~ got
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(7)])
}; FuseExpect::Jit);

run!(net_on_write_function_refused, r#"{
    let got = 0;
    sys::net::publish(#on_write: |f: fn(x: i64) -> i64| got <- f(41), "/local/ow_fn", 0);
    got
}"#, testing::refused("is not data"); FuseExpect::None);

// A handler refused for its result is a type mismatch: the Data cell it
// is called with meets an i64, never the handler.
run!(net_on_write_handler_result_is_a_mismatch, r#"{
    let h: fn(v: i64) -> i64 = |v: i64| 0;
    sys::net::publish(#on_write: h, "/local/ow_ret", 1)
}"#, testing::refused("does not contain fn(v: i64) -> i64"); FuseExpect::None);

// A refusal names the part a Data cell meets, here the handler's argument.
run!(net_on_write_reference_argument_named, r#"{
    let seen = 0;
    sys::net::publish(#on_write: |v: &i64| seen <- v ~ seen + 1, "/local/ow_ref", 0);
    seen
}"#, testing::refused("&i64 is not data: data holds no function and no reference"); FuseExpect::None);

run!(net_rpc_reference_argument_refused, r#"{
    let rpc = sys::net::rpc(#path: "/local/rpc_ref", #doc: "", #spec: null, #f: |a: &i64| *a + 1);
    rpc
}"#, testing::refused("is not data"); FuseExpect::None);
