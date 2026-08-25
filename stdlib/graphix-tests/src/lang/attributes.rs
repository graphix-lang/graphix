// The definition-asserting attributes (Eric, 2026-08-14):
// `#[tail_recursive]` (every self-call in tail position — the
// cannot-trip-the-depth-limit guarantee), `#[sync]` / `#[async]`
// (the intrinsic-effect assertion, assert-only v1). All three are
// compile-time checks registered beside `#[native]` (lib.rs); a
// failed assertion is a compile error, surfaced here as `Err(_)`.

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

// One tail self-call does NOT make a function tail-recursive when
// another self-call is non-tail (the tightened RecursionKind summary):
// the non-tail site still consumes native stack.
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

// `#[tail_recursive]` asserts a constant-space loop, and a loop is
// constant-space only when its body is stateless: `count` gives every
// iteration its own activation (design/recursive_activations.md §2).
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

// `throttle` defers deliveries across cycles — the body is async, the
// `#[sync]` assertion fails.
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
