// The definition-asserting attributes `#[tail_recursive]`, `#[sync]`
// and `#[async]`: a failed assertion is a compile error (`Err(_)`).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::publisher::Value;

const TAIL_RECURSIVE_OK: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, acc + n) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_ok, TAIL_RECURSIVE_OK, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(55))
));

// fib recurses through `+` — non-tail on both self-calls.
const TAIL_RECURSIVE_NON_TAIL: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, i64:1 => i64:1, _ => f(n - i64:1) + f(n - i64:2) };
  f(i64:10)
}
"#;

run!(tail_recursive_non_tail, TAIL_RECURSIVE_NON_TAIL, refused("every recursive call must be in tail position"); FuseExpect::None);

// One tail self-call does not make a function tail-recursive when
// another self-call is non-tail.
const TAIL_RECURSIVE_MIXED: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64| -> i64 select n {
    i64:0 => i64:0,
    i64:1 => f(i64:0) + i64:1,
    _ => f(n - i64:1)
  };
  f(i64:10)
}
"#;

run!(tail_recursive_mixed, TAIL_RECURSIVE_MIXED, refused("every recursive call must be in tail position"); FuseExpect::None);

// `#[tail_recursive]` asserts a constant-space loop, which needs a
// stateless body: `count` gives every iteration its own activation.
const TAIL_RECURSIVE_STATEFUL: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, acc + count(n)) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_stateful, TAIL_RECURSIVE_STATEFUL, refused("#[tail_recursive]: this function's body is stateful or async"); FuseExpect::None);

// A vacuous assertion is an error: the function never recurses.
const TAIL_RECURSIVE_NOT_RECURSIVE: &str = r#"
{
  #[tail_recursive]
  let f = |n: i64| -> i64 n + i64:1;
  f(i64:1)
}
"#;

run!(
    tail_recursive_not_recursive,
    TAIL_RECURSIVE_NOT_RECURSIVE,
    refused("#[tail_recursive]: this function is not recursive"); FuseExpect::None);

const SYNC_OK: &str = r#"
{
  #[sync]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:21)
}
"#;

run!(sync_ok, SYNC_OK, |v: Result<&Value>| matches!(v, Ok(Value::I64(42))));

// Parentheses around the lambda do not hide it from the assertion.
const SYNC_PARENS: &str = r#"
{
  #[sync]
  let f = (|n: i64| -> i64 n * i64:2);
  f(i64:21)
}
"#;

run!(sync_parens, SYNC_PARENS, |v: Result<&Value>| matches!(v, Ok(Value::I64(42))));

// ... nor from the check: parentheses around an async body still fail.
const SYNC_PARENS_ON_ASYNC: &str = r#"
{
  #[sync]
  let f = (|n: i64| throttle(#rate: duration:0.001s, n));
  f(i64:1)
}
"#;

run!(sync_parens_on_async, SYNC_PARENS_ON_ASYNC, refused("#[sync]: this function is async"); FuseExpect::None);

// `throttle` defers deliveries across cycles: the body is async.
const SYNC_ON_ASYNC: &str = r#"
{
  #[sync]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:1)
}
"#;

run!(sync_on_async, SYNC_ON_ASYNC, refused("#[sync]: this function is async"); FuseExpect::None);

// A `&` evaluates its whole expression: an async builtin two levels
// under the reference makes the body async.
const SYNC_ON_ASYNC_UNDER_REF: &str = r#"
{
  #[sync]
  let f = |n: i64| { let r = &(throttle(#rate: duration:0.001s, n) + 1); *r };
  f(i64:1)
}
"#;

run!(sync_on_async_under_ref, SYNC_ON_ASYNC_UNDER_REF, refused("#[sync]: this function is async"); FuseExpect::None);

// A dynamic module runs code its source delivers at run time.
const SYNC_ON_DYNAMIC_MODULE: &str = r#"
{
  #[sync]
  let f = |x: i64| {
    let s = mod t dynamic {
      sandbox whitelist [core];
      sig { val foo: i64 };
      source "let foo = 42"
    };
    (s, x)
  };
  f(i64:1)
}
"#;

run!(sync_on_dynamic_module, SYNC_ON_DYNAMIC_MODULE, refused("#[sync]: this function is async"); FuseExpect::None);

// An async builtin in a dynamic module's source expression.
const ASYNC_ON_DYNAMIC_MODULE: &str = r#"
{
  #[async]
  let f = |x: i64| {
    let s = mod t dynamic {
      sandbox whitelist [core];
      sig { val foo: i64 };
      source sys::time::after_idle(duration:0.001s, "let foo = 42")
    };
    select s { error as _ => never(), null as _ => t::foo + x }
  };
  f(i64:1)
}
"#;

run!(async_on_dynamic_module, ASYNC_ON_DYNAMIC_MODULE, |v: Result<&Value>| matches!(v, Ok(Value::I64(43))); FuseExpect::Jit);

const ASYNC_OK: &str = r#"
{
  #[async]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:5)
}
"#;

run!(async_ok, ASYNC_OK, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))); FuseExpect::None);

const ASYNC_ON_SYNC: &str = r#"
{
  #[async]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:1)
}
"#;

run!(async_on_sync, ASYNC_ON_SYNC, refused("#[async]: this function is sync"); FuseExpect::None);

// The definition-asserting attributes reject non-function targets.
const SYNC_ON_VALUE: &str = r#"
{
  #[sync]
  let x = i64:5;
  x
}
"#;

run!(sync_on_value, SYNC_ON_VALUE, refused("#[sync] annotates a function definition"); FuseExpect::None);

// Effects are inferred per instance: a pure instance of `apply` does
// not stand in for the async one `delayed` reaches.
const SYNC_ON_ASYNC_INSTANCE: &str = r#"
{
  let apply = |f: fn(x: i64) -> i64, x: i64| f(x);
  let p = apply(|x| x + 1, 1);
  #[sync]
  let delayed = |x: i64| apply(|x| sys::time::after_idle(duration:0.001s, x), x);
  (p, delayed(3))
}
"#;

run!(sync_on_async_instance, SYNC_ON_ASYNC_INSTANCE, refused("#[sync]: this function is async"); FuseExpect::None);

// A definition's assertion sees every instance: the pure instance of
// `loop` does not stand in for the one whose callback counts.
const TAIL_RECURSIVE_STATEFUL_INSTANCE: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let pure = loop(|x| x, 3, 0);
  let counted = loop(|x| count(x), 3, 0);
  (pure, counted)
}
"#;

run!(
    tail_recursive_stateful_instance,
    TAIL_RECURSIVE_STATEFUL_INSTANCE,
    refused("#[tail_recursive]: this function's body is stateful or async"); FuseExpect::None);

// The same loop with pure callbacks only.
const TAIL_RECURSIVE_PURE_INSTANCES: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  (loop(|x| x, 3, 0), loop(|x| x * 2, 3, 0))
}
"#;

run!(
    tail_recursive_pure_instances,
    TAIL_RECURSIVE_PURE_INSTANCES,
    |v: Result<&Value>| format!("{}", v.unwrap()) == "[i64:6, i64:12]"; FuseExpect::Jit);

// A labeled formal keeps the self-call off the loop, so the recursion
// is not constant-space although every self-call is in tail position.
const TAIL_RECURSIVE_LABELED: &str = r#"
{
  #[tail_recursive]
  let rec f = |#acc: i64 = 0, n: i64| -> i64 select n { 0 => acc, _ => f(#acc: acc + 1, n - 1) };
  f(10)
}
"#;

run!(tail_recursive_labeled, TAIL_RECURSIVE_LABELED, refused("no loop is built"); FuseExpect::None);

// The instance `h` recurses through is mutually recursive; the tail
// instance over `|x| x` does not stand in for it.
const TAIL_RECURSIVE_MUTUAL_INSTANCE: &str = r#"
{
  #[tail_recursive]
  let rec f = |g: fn(x: i64) -> i64, n: i64| -> i64 select n { 0 => g(0), _ => f(g, n - 1) };
  let rec h = |x: i64| -> i64 select x { 0 => 0, _ => f(h, x - 1) + 1 };
  (f(h, 10), f(|x| x, 10))
}
"#;

run!(tail_recursive_mutual_instance, TAIL_RECURSIVE_MUTUAL_INSTANCE, refused("every recursive call must be in tail position"); FuseExpect::None);

// A lambda literal called in place is an instance like any other: its
// async body makes the enclosing function async.
const SYNC_ON_ASYNC_LITERAL_CALL: &str = r#"
{
  #[sync]
  let h = |n: i64| (|m: i64| throttle(#rate: duration:0.001s, m))(n);
  h(1)
}
"#;

run!(sync_on_async_literal_call, SYNC_ON_ASYNC_LITERAL_CALL, refused("#[sync]: this function is async"); FuseExpect::None);
