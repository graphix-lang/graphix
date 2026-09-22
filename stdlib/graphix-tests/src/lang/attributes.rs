// The definition-asserting attributes `#[tail_recursive]`, `#[sync]`
// and `#[async]`: a failed assertion is a compile error (`Err(_)`).

use anyhow::Result;
use graphix_package_core::run;
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

run!(tail_recursive_non_tail, TAIL_RECURSIVE_NON_TAIL, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

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

run!(tail_recursive_mixed, TAIL_RECURSIVE_MIXED, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

// `#[tail_recursive]` asserts a constant-space loop, which needs a
// stateless body: `count` gives every iteration its own activation.
const TAIL_RECURSIVE_STATEFUL: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, acc + count(n)) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_stateful, TAIL_RECURSIVE_STATEFUL, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

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
    |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

const SYNC_OK: &str = r#"
{
  #[sync]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:21)
}
"#;

run!(sync_ok, SYNC_OK, |v: Result<&Value>| matches!(v, Ok(Value::I64(42))));

// `throttle` defers deliveries across cycles: the body is async.
const SYNC_ON_ASYNC: &str = r#"
{
  #[sync]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:1)
}
"#;

run!(sync_on_async, SYNC_ON_ASYNC, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

const ASYNC_OK: &str = r#"
{
  #[async]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:5)
}
"#;

run!(async_ok, ASYNC_OK, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))); graphix_package_core::testing::FuseExpect::None);

const ASYNC_ON_SYNC: &str = r#"
{
  #[async]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:1)
}
"#;

run!(async_on_sync, ASYNC_ON_SYNC, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

// The definition-asserting attributes reject non-function targets.
const SYNC_ON_VALUE: &str = r#"
{
  #[sync]
  let x = i64:5;
  x
}
"#;

run!(sync_on_value, SYNC_ON_VALUE, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

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

run!(sync_on_async_instance, SYNC_ON_ASYNC_INSTANCE, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

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
    |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

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
    |v: Result<&Value>| format!("{}", v.unwrap()) == "[i64:6, i64:12]"; graphix_package_core::testing::FuseExpect::Jit);
