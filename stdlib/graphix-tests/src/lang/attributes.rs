// The definition-asserting attributes `#[tail_recursive]`, `#[sync]`
// and `#[async]`: a failed assertion is a compile error (`Err(_)`).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
};
use netidx::publisher::Value;

const TAIL_RECURSIVE_OK: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, acc + n) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_ok, TAIL_RECURSIVE_OK, is(Value::I64(55)));

// A `never()` arm is a pure standing bottom, so it leaves the body
// stateless and the recursion one native loop.
const TAIL_RECURSIVE_NEVER_ARM: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, x if x < 0 => never(), x => f(x - 1, acc + x) };
  f(10, 0)
}
"#;

run!(tail_recursive_never_arm, TAIL_RECURSIVE_NEVER_ARM, is(Value::I64(55)); FuseExpect::Jit);

// The loop reaching its `never()` arm returns bottom.
const TAIL_RECURSIVE_NEVER_TAKEN: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, 3 => never(), x => f(x - 1, acc + x) };
  any(f(10, 0), -1)
}
"#;

run!(tail_recursive_never_taken, TAIL_RECURSIVE_NEVER_TAKEN, is(Value::I64(-1)); FuseExpect::Jit);

// fib recurses through `+` — non-tail on both self-calls.
const TAIL_RECURSIVE_NON_TAIL: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, i64:1 => i64:1, _ => f(n - i64:1) + f(n - i64:2) };
  f(i64:10)
}
"#;

run!(tail_recursive_non_tail, TAIL_RECURSIVE_NON_TAIL, refused("every recursive call must be in tail position"); FuseExpect::None; build_only);

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

run!(tail_recursive_mixed, TAIL_RECURSIVE_MIXED, refused("every recursive call must be in tail position"); FuseExpect::None; build_only);

// `#[tail_recursive]` asserts a constant-space loop, which needs a
// stateless body: `count` gives every iteration its own activation.
const TAIL_RECURSIVE_STATEFUL: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, acc + count(n)) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_stateful, TAIL_RECURSIVE_STATEFUL, refused("#[tail_recursive]: this function's body is stateful or async"); FuseExpect::None; build_only);

// An Io call that does not fire with the formals keeps every depth its
// own activation, and the refusal names it.
const TAIL_RECURSIVE_IO: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 {
    println("step");
    select n { 0 => acc, _ => f(n - 1, acc + n) }
  };
  f(3, 0)
}
"#;

run!(tail_recursive_io, TAIL_RECURSIVE_IO, refused("no loop is built: `println(\"step\")` (line 5, column 5) performs Io that does not fire with the formals"); FuseExpect::None; build_only);

// So does a raise to a handler that does not fire with the formals.
const TAIL_RECURSIVE_RAISE: &str = r#"
{
  let e: Result<i64, `E> = error(`E);
  let caught = 0;
  catch(x) caught <- x ~ 1;
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 {
    let k = e?;
    select n { 0 => acc + k, _ => f(n - 1, acc + n) }
  };
  f(3, 0)
}
"#;

run!(tail_recursive_raise, TAIL_RECURSIVE_RAISE, refused("no loop is built: `e?` (line 8, column 13) raises to a handler"); FuseExpect::None; build_only);

// A pure body that does not fuse builds no loop: only a fused kernel runs
// a tail recursion as one, so the assertion is the JIT's.
const TAIL_RECURSIVE_UNFUSED: &str = r#"
{
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => f(n - i64:1, *(&acc) + i64:1) };
  f(i64:10, i64:0)
}
"#;

run!(tail_recursive_unfused, TAIL_RECURSIVE_UNFUSED, refused("#[tail_recursive]: no loop is built for the call `f(10, 0)` (line 5, column 3): it did not fuse"); FuseExpect::None; jit_only);

// Every instance the assertion covers builds its own loop: the one over a
// callback that reads a reference does not fuse, and the instance over
// `|x| x` beside it, written before or after, does not stand in for it.
const TAIL_RECURSIVE_UNFUSED_INSTANCE: &str = r#"
{
  let v = 1;
  let rv = &v;
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let h = |x: i64| -> i64 x + *rv;
  (loop(h, 3, 0), loop(|x| x, 3, 0))
}
"#;

run!(
    tail_recursive_unfused_instance,
    TAIL_RECURSIVE_UNFUSED_INSTANCE,
    refused("#[tail_recursive]: no loop is built for the call `loop(h, 3, 0)` (line 9, column 4)"); FuseExpect::None; jit_only);

const TAIL_RECURSIVE_UNFUSED_INSTANCE_LAST: &str = r#"
{
  let v = 1;
  let rv = &v;
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let h = |x: i64| -> i64 x + *rv;
  (loop(|x| x, 3, 0), loop(h, 3, 0))
}
"#;

run!(
    tail_recursive_unfused_instance_last,
    TAIL_RECURSIVE_UNFUSED_INSTANCE_LAST,
    refused("#[tail_recursive]: no loop is built for the call `loop(h, 3, 0)` (line 9, column 23)"); FuseExpect::None; jit_only);

// Alone, the instance over the reference is refused as well.
const TAIL_RECURSIVE_UNFUSED_INSTANCE_ALONE: &str = r#"
{
  let v = 1;
  let rv = &v;
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let h = |x: i64| -> i64 x + *rv;
  loop(h, 3, 0)
}
"#;

run!(
    tail_recursive_unfused_instance_alone,
    TAIL_RECURSIVE_UNFUSED_INSTANCE_ALONE,
    refused("#[tail_recursive]: no loop is built for the call `loop(h, 3, 0)` (line 9, column 3)"); FuseExpect::None; jit_only);

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
    refused("#[tail_recursive]: this function is not recursive"); FuseExpect::None; build_only);

const SYNC_OK: &str = r#"
{
  #[sync]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:21)
}
"#;

run!(sync_ok, SYNC_OK, is(Value::I64(42)));

// Parentheses around the lambda do not hide it from the assertion.
const SYNC_PARENS: &str = r#"
{
  #[sync]
  let f = (|n: i64| -> i64 n * i64:2);
  f(i64:21)
}
"#;

run!(sync_parens, SYNC_PARENS, is(Value::I64(42)));

// ... nor from the check: parentheses around an async body still fail.
const SYNC_PARENS_ON_ASYNC: &str = r#"
{
  #[sync]
  let f = (|n: i64| throttle(#rate: duration:0.001s, n));
  f(i64:1)
}
"#;

run!(sync_parens_on_async, SYNC_PARENS_ON_ASYNC, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// `throttle` defers deliveries across cycles: the body is async.
const SYNC_ON_ASYNC: &str = r#"
{
  #[sync]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:1)
}
"#;

run!(sync_on_async, SYNC_ON_ASYNC, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// A `&` evaluates its whole expression: an async builtin two levels
// under the reference makes the body async.
const SYNC_ON_ASYNC_UNDER_REF: &str = r#"
{
  #[sync]
  let f = |n: i64| { let r = &(throttle(#rate: duration:0.001s, n) + 1); *r };
  f(i64:1)
}
"#;

run!(sync_on_async_under_ref, SYNC_ON_ASYNC_UNDER_REF, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// A builtin's callbacks join its call's facts: an async callback makes
// the caller async, a sync one leaves it sync, and a function handing a
// builtin its own parameter leaves the effect to its caller's argument.
run!(sync_on_async_builtin_callback, r#"{
    #[sync]
    let g = |v: [i64, null]| opt::map(v, |x| sys::time::after_idle(duration:10.ms, x));
    g(1)
}"#, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// A builtin's callback literal that calls a function nothing resolves at
// its definition (a local bound to a call's result, a parameter the
// builtin supplies from data, one captured from the enclosing function
// and handed an async argument) may be async.
run!(sync_on_an_unresolved_callback_call, r#"{
    let pick = |b: bool| select b {
        true => |x: i64| sys::time::after_idle(duration:10.ms, x > 0),
        false => |x: i64| x > 0
    };
    #[sync]
    let g = |v: i64| filter(v, |x| { let k = pick(x > 0); k(x) });
    g(3)
}"#, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

run!(sync_on_a_data_supplied_callback, r#"{
    #[sync]
    let h = |m: [fn(x: i64) -> i64, null]| opt::map(m, |g| g(3));
    h(|v| v)
}"#, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

run!(sync_on_a_captured_async_callback, r#"{
    #[sync]
    let f = |y: [i64, null], k: fn(v: i64) -> i64| opt::map(y, |v| k(v));
    f(3, |v| sys::time::after_idle(duration:10.ms, v))
}"#, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

run!(sync_on_async_filter_callback, r#"{
    #[sync]
    let g = |v: i64| filter(v, |x| sys::time::after_idle(duration:10.ms, x > 0));
    g(1)
}"#, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

run!(sync_builtin_callbacks_stay_sync, r#"{
    #[sync]
    let g = |v: [i64, null]| opt::map(v, |x| x + 1);
    #[sync]
    let ap = |v: [i64, null], f: fn(x: i64) -> i64| opt::map(v, f);
    (g(1), ap(2, |x| x * 10))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(20)])
}; FuseExpect::Jit);

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

run!(sync_on_dynamic_module, SYNC_ON_DYNAMIC_MODULE, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

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

run!(async_on_dynamic_module, ASYNC_ON_DYNAMIC_MODULE, is(Value::I64(43)); FuseExpect::Jit);

const ASYNC_OK: &str = r#"
{
  #[async]
  let f = |n: i64| throttle(#rate: duration:0.001s, n);
  f(i64:5)
}
"#;

run!(async_ok, ASYNC_OK, is(Value::I64(5)); FuseExpect::None);

const ASYNC_ON_SYNC: &str = r#"
{
  #[async]
  let f = |n: i64| -> i64 n * i64:2;
  f(i64:1)
}
"#;

run!(async_on_sync, ASYNC_ON_SYNC, refused("#[async]: this function is sync"); FuseExpect::None; build_only);

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

run!(sync_on_async_instance, SYNC_ON_ASYNC_INSTANCE, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// A let-bound callback handed to a builtin carries the facts of the
// parameters it calls, as a literal callback does.
const SYNC_ON_LET_BOUND_CALLBACK: &str = r#"
{
  #[sync]
  let d = |f: fn(v: i64) -> bool, v: i64| { let g = |x| f(x); filter(v, g) };
  d(|x| sys::time::after_idle(duration:0.001s, x) > 0, 3)
}
"#;

run!(sync_on_let_bound_callback, SYNC_ON_LET_BOUND_CALLBACK, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

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
    refused("#[tail_recursive]: this function's body is stateful or async"); FuseExpect::None; build_only);

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
    prints("[i64:6, i64:12]"); FuseExpect::Jit);

// A collection's slots, each an instance bound as the program runs, reach
// the loop: fused, each slot's instance runs it as a loop; under the
// node-walk nothing fuses, a slot's instance included.
const TAIL_RECURSIVE_SLOT_INSTANCES: &str = r#"
{
  let v = 1;
  let rv = &v;
  #[tail_recursive]
  let rec loop = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => loop(n - 1, acc + n) };
  array::map([1, 2, 3], |x| loop(x, 0) + *rv)
}
"#;

run!(
    tail_recursive_slot_instances,
    TAIL_RECURSIVE_SLOT_INSTANCES,
    prints("[i64:2, i64:4, i64:7]"); FuseExpect::Jit);

// A collection handed the loop itself calls it through a function value,
// in the node-walk: the refusal names the collection's call.
const TAIL_RECURSIVE_COLLECTION_CALLBACK: &str = r#"
{
  #[tail_recursive]
  let rec tri = |n: i64| -> i64 select n { 0 => 0, _ => tri(n - 1) };
  array::map([1, 2, 3], tri)
}
"#;

run!(
    tail_recursive_collection_callback,
    TAIL_RECURSIVE_COLLECTION_CALLBACK,
    refused("#[tail_recursive]: no loop is built for the call `array::map([1, 2, 3], tri)` (line 5, column 3)"); FuseExpect::None; jit_only);

// Labeled formals loop as positional ones do: a self-call passes each by
// name, in any order, and one it omits takes its default.
const TAIL_RECURSIVE_LABELED: &str = r#"
{
  #[tail_recursive]
  let rec f = |#acc: i64 = 0, n: i64| -> i64 select n { 0 => acc, _ => f(#acc: acc + 1, n - 1) };
  #[tail_recursive]
  let rec g = |#s: string = "", #u: i64 = 5, n: i64| -> string select n {
    0 => "[s]/[u]",
    1 => g(#s: "[s]1", n - 1),
    _ => g(#u: u + n, #s: "[s][n]", n - 1)
  };
  (f(10), g(4), g(#u: 0, #s: "x", 3))
}
"#;

run!(tail_recursive_labeled, TAIL_RECURSIVE_LABELED, prints(r#"[i64:10, "4321/5", "x321/5"]"#); FuseExpect::Jit);

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

run!(tail_recursive_mutual_instance, TAIL_RECURSIVE_MUTUAL_INSTANCE, refused("every recursive call must be in tail position"); FuseExpect::None; build_only);

// A callee holding a non-tail self-call is per-call memory.
const TAIL_RECURSIVE_NON_TAIL_CALLEE: &str = r#"
{
  let rec sum = |x: i64| -> i64 select x { 0 => 0, _ => x + sum(x - 1) };
  #[tail_recursive]
  let rec loop = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => loop(n - 1, acc + sum(n)) };
  loop(3, 0)
}
"#;

run!(tail_recursive_non_tail_callee, TAIL_RECURSIVE_NON_TAIL_CALLEE, refused("no loop is built: the body holds per-call memory"); FuseExpect::None; build_only);

// So does a recursive closure the body defines, in each instance.
const TAIL_RECURSIVE_INNER_REC: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64 select n {
    0 => acc,
    _ => loop(f, { let rec g = |x: i64| { let t = select x { 0 => n, _ => g(x - 1) }; t }; g(3) } - 1, acc + f(n))
  };
  (loop(|x| x, 3, 0), loop(|x| x * 2, 3, 0))
}
"#;

run!(tail_recursive_inner_rec, TAIL_RECURSIVE_INNER_REC, refused("no loop is built: the body holds per-call memory"); FuseExpect::None; build_only);

// Without the assertion the same loop runs in every mode.
const INNER_REC_UNASSERTED: &str = r#"
{
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64 select n {
    0 => acc,
    _ => loop(f, { let rec g = |x: i64| { let t = select x { 0 => n, _ => g(x - 1) }; t }; g(3) } - 1, acc + f(n))
  };
  (loop(|x| x, 3, 0), loop(|x| x * 2, 3, 0))
}
"#;

run!(inner_rec_unasserted, INNER_REC_UNASSERTED, prints("[i64:6, i64:12]"));

// Instances blocked for different reasons: the refusal names a site
// before per-call memory, and of two sites the one written first.
const TAIL_RECURSIVE_TWO_REASONS: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let rec sum = |x: i64| -> i64 select x { 0 => 0, _ => x + sum(x - 1) };
  (loop(|x| { println("p"); x }, 3, 0), loop(sum, 3, 0))
}
"#;

run!(tail_recursive_two_reasons, TAIL_RECURSIVE_TWO_REASONS, refused("no loop is built: `f(n)` (line 5, column 52) performs Io"); FuseExpect::None; build_only);

const TAIL_RECURSIVE_TWO_SITES: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, g: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, g, n - 1, acc + f(n) + g(n)) };
  let p = |x: i64| -> i64 { println("p"); x };
  (loop(p, |x| x, 3, 0), loop(|x| x, p, 3, 0))
}
"#;

run!(tail_recursive_two_sites, TAIL_RECURSIVE_TWO_SITES, refused("no loop is built: `f(n)` (line 5, column 55) performs Io"); FuseExpect::None; build_only);

// A callee whose recursion runs through another function holds per-call
// memory too.
const TAIL_RECURSIVE_CYCLE_CALLEE: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let rec h = |x: i64| -> i64 select x { 0 => 0, _ => { let k = |y: i64| -> i64 h(y - 1) + 1; k(x) } };
  (loop(h, 3, 0), loop(|x| x, 3, 0))
}
"#;

run!(tail_recursive_cycle_callee, TAIL_RECURSIVE_CYCLE_CALLEE, refused("no loop is built: the body holds per-call memory"); FuseExpect::None; build_only);

const CYCLE_CALLEE_UNASSERTED: &str = r#"
{
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let rec h = |x: i64| -> i64 select x { 0 => 0, _ => { let k = |y: i64| -> i64 h(y - 1) + 1; k(x) } };
  (loop(h, 3, 0), loop(|x| x, 3, 0))
}
"#;

run!(cycle_callee_unasserted, CYCLE_CALLEE_UNASSERTED, prints("[i64:6, i64:6]"));

// So does one whose cycle through another function is all tail calls.
const TAIL_RECURSIVE_TAIL_CYCLE_CALLEE: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let rec h = |x: i64| -> i64 select x { 0 => 0, _ => { let k = |y: i64| -> i64 h(y - 1); k(x) } };
  (loop(h, 3, 0), loop(|x| x, 3, 0))
}
"#;

run!(tail_recursive_tail_cycle_callee, TAIL_RECURSIVE_TAIL_CYCLE_CALLEE, refused("no loop is built: the body holds per-call memory"); FuseExpect::None; build_only);

// A callee that defines a closure and recurses by tail calls to itself
// holds none, and the loop over it is native.
const TAIL_RECURSIVE_TAIL_CALLEE_CLOSURE: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let rec h = |x: i64, a: i64| -> i64 select x { 0 => a, _ => { let k = |y: i64| -> i64 y + 2; h(x - 1, k(a)) } };
  #[native] loop(|x| h(x, 0), 3, 0)
}
"#;

run!(tail_recursive_tail_callee_closure, TAIL_RECURSIVE_TAIL_CALLEE_CLOSURE, is(Value::I64(12)); FuseExpect::Jit);

// Nor does a callee that calls another instance of the loop: two
// instances of one definition on a call path are not a cycle.
const TAIL_RECURSIVE_OTHER_INSTANCE: &str = r#"
{
  #[tail_recursive]
  let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
  let h = |x: i64| -> i64 loop(|y| y * 2, x, 0);
  #[native] loop(h, 3, 0)
}
"#;

run!(tail_recursive_other_instance, TAIL_RECURSIVE_OTHER_INSTANCE, is(Value::I64(20)); FuseExpect::Jit);

// A lambda literal called in place is an instance like any other: its
// async body makes the enclosing function async.
const SYNC_ON_ASYNC_LITERAL_CALL: &str = r#"
{
  #[sync]
  let h = |n: i64| (|m: i64| throttle(#rate: duration:0.001s, m))(n);
  h(1)
}
"#;

run!(sync_on_async_literal_call, SYNC_ON_ASYNC_LITERAL_CALL, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

// A literal an instance defines keeps the enclosing parameters it calls:
// a builtin handed `|x| f(x)` is judged with the instance's `f`.
const CAPTURED_PARAM_CALL: &str = r#"
{
  let g = |v: i64, f: fn(x: i64) -> bool| filter(v, |x| f(x));
  #[ATTR]
  let h = |v: i64| g(v, |x| sys::time::after_idle(duration:1.ms, x) > 0);
  h(1)
}
"#;

run!(captured_param_call_sync_refused, &CAPTURED_PARAM_CALL.replace("ATTR", "sync"), refused("#[sync]: this function is async"); FuseExpect::None; build_only);
run!(captured_param_call_async_accepted, &CAPTURED_PARAM_CALL.replace("ATTR", "async"), |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::None);

// A collection slot's instance names no formal for the parameter its
// literal calls, so the call stays unresolved: async.
const SLOT_CAPTURED_PARAM_CALL: &str = r#"
{
  let d = |fs: Array<fn(v: i64) -> bool>, v: i64| array::map(fs, |f| filter(v, |x| f(x)));
  #[sync]
  let h = |v: i64| d([|x| sys::time::after_idle(duration:1.ms, x) > 0], v);
  h(1)
}
"#;

run!(slot_captured_param_call_sync_refused, SLOT_CAPTURED_PARAM_CALL, refused("#[sync]: this function is async"); FuseExpect::None; build_only);

run!(native_body_attribute_waits_for_instance, r#"{
    let f = |x: i64| { let y = #[native] (x * 2); y + 1 };
    0
}"#, is(Value::I64(0)); FuseExpect::Jit);

async fn check_dynamic_native(case: bool) -> Result<()> {
    use graphix_package_core::testing::{
        Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    use std::{process::Command, time::Duration};
    use tokio::time::Instant;

    const CHILD: &str = "GRAPHIX_TEST_NATIVE_REFUSAL_CHILD";
    if !case && std::env::var_os(CHILD).is_none() {
        let output = Command::new(std::env::current_exe()?)
            .args([
                "--exact",
                "lang::attributes::native_body_attribute_dynamic_rejects_stateful",
                "--nocapture",
                "--include-ignored",
            ])
            .env(CHILD, "1")
            .env("RUST_LOG", "graphix::failure=error")
            .output()?;
        let stderr = std::str::from_utf8(&output.stderr)?;
        assert!(output.status.success(), "native refusal child failed: {stderr}");
        assert_eq!(
            stderr
                .matches("#[native] expression did not fully fuse to native code")
                .count(),
            2,
            "missing native refusal in Jit or JitPar: {stderr}"
        );
        assert_eq!(
            stderr.matches("builtin `count` has no fast-call entry").count(),
            2,
            "{stderr}"
        );
        return Ok(());
    }

    for mode in [Mode::Jit, Mode::JitPar] {
        for (code, valid) in [
            (
                r#"{
                let f = |x: i64| { let y = #[native] (x * 2); y + 1 };
                let h = never(); h <- f; h(5)
            }"#,
                true,
            ),
            (
                r#"{
                let f = |x: i64| { let y = #[native] count(x); y + 1 };
                let h = never(); h <- f; h(5)
            }"#,
                false,
            ),
        ] {
            if valid != case {
                continue;
            }
            let (ctx, mut rx) = fixture_runtime(
                [("/test.gx", result_source(code))],
                crate::TEST_REGISTER,
                mode,
                |_| {},
            )
            .await?;
            ctx.rt.control().reset_invocations();
            let result = compile_result(&ctx).await?;
            let values = updates_until_quiet(
                &ctx.rt,
                &mut rx,
                result.exprs[0].id,
                Duration::from_millis(100),
                Instant::now() + Duration::from_secs(20),
            )
            .await?;
            if valid {
                assert_eq!(values, [Value::I64(11)]);
                assert!(
                    ctx.rt.control().invocations().1 > 0,
                    "native body never entered the JIT"
                );
            } else {
                assert!(values.is_empty(), "invalid native body ran: {values:?}");
            }
            ctx.shutdown().await;
        }
    }
    Ok(())
}

#[tokio::test]
async fn native_body_attribute_survives_repl_inputs() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::testing::{Mode, fixture_runtime};
    for mode in [Mode::Jit, Mode::JitPar] {
        let (ctx, _rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
        let _f = ctx
            .rt
            .compile(literal!("let f = |x: i64| { let y = #[native] (x * 2); y + 1 }"))
            .await?;
        let _call = ctx.rt.compile(literal!("f(5)")).await?;
        let _g = ctx
            .rt
            .compile(literal!("let g = |x: i64| { let y = #[native] count(x); y + 1 }"))
            .await?;
        let err = ctx.rt.compile(literal!("g(5)")).await.unwrap_err();
        assert!(err.chain().any(|e| e.to_string().contains("#[native]")), "{err:#}");
        ctx.shutdown().await;
    }
    Ok(())
}

/// A `#[tail_recursive]` covers the instances later inputs build: one
/// that does not fuse is refused after one that did, and after the loop
/// called through a function value was reported as the program ran; so is
/// one whose callback keeps state, and one after them that fuses is
/// accepted.
#[tokio::test]
async fn tail_recursive_covers_later_inputs() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::testing::{Mode, fixture_runtime, updates_until_quiet};
    use std::time::Duration;
    use tokio::time::Instant;
    for mode in [Mode::Jit, Mode::JitPar] {
        let (ctx, mut rx) =
            fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
        let _loop = ctx
            .rt
            .compile(literal!(
                "#[tail_recursive] let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| \
                 -> i64 select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) }"
            ))
            .await?;
        let _v = ctx.rt.compile(literal!("let v = 1")).await?;
        let _fused = ctx.rt.compile(literal!("loop(|x| x, 3, 0)")).await?;
        let _b = ctx.rt.compile(literal!("let b = true")).await?;
        let _g = ctx
            .rt
            .compile(literal!(
                "let g = select b { true => loop, false => |f: fn(x: i64) -> i64, n: i64, \
                 acc: i64| -> i64 n }"
            ))
            .await?;
        let bound = ctx.rt.compile(literal!("g(|x| x, 3, 0)")).await?;
        let values = updates_until_quiet(
            &ctx.rt,
            &mut rx,
            bound.exprs[0].id,
            Duration::from_millis(100),
            Instant::now() + Duration::from_secs(20),
        )
        .await?;
        assert_eq!(values, [Value::I64(6)], "{mode:?}");
        let err = ctx
            .rt
            .compile(literal!("loop(|x: i64| -> i64 x + *(&v), 3, 0)"))
            .await
            .unwrap_err();
        let msg = "#[tail_recursive]: no loop is built for the call \
                   `loop(|x: i64| -> i64 x + *(&v), 3, 0)`";
        assert!(err.chain().any(|e| e.to_string().contains(msg)), "{mode:?}: {err:#}");
        let err = ctx.rt.compile(literal!("loop(|x| count(x), 3, 0)")).await.unwrap_err();
        let msg = "#[tail_recursive]: no loop is built for the call \
                   `loop(|x| count(x), 3, 0)` (line 1, column 1): the recursion of its \
                   instance is not a stateless tail recursion";
        assert!(err.chain().any(|e| e.to_string().contains(msg)), "{mode:?}: {err:#}");
        let _again = ctx.rt.compile(literal!("loop(|x| x * 2, 3, 0)")).await?;
        ctx.shutdown().await;
    }
    Ok(())
}

/// The loop called through a function value runs in the node-walk, each
/// level a bind of its own: the first bind tries to fuse its instance and
/// is reported, and the levels after it do not try again, so a deeper
/// recursion fails no more fusions.
#[tokio::test]
async fn tail_recursive_reported_bind_fuses_once() -> Result<()> {
    use graphix_package_core::testing::{
        Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    use std::time::Duration;
    use tokio::time::Instant;
    const CODE: &str = "{
  #[tail_recursive]
  let rec loop = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => loop(n - 1, acc + n) };
  let b = true;
  let g = select b { true => loop, false => |n: i64, acc: i64| -> i64 n + acc };
  g(DEPTH, 0)
}";
    for mode in [Mode::Jit, Mode::JitPar] {
        let mut failed = vec![];
        for (depth, sum) in [(10, 55), (100, 5050)] {
            let code = CODE.replace("DEPTH", &depth.to_string());
            let (ctx, mut rx) = fixture_runtime(
                [("/test.gx", result_source(&code))],
                crate::TEST_REGISTER,
                mode,
                |_| {},
            )
            .await?;
            let result = compile_result(&ctx).await?;
            let values = updates_until_quiet(
                &ctx.rt,
                &mut rx,
                result.exprs[0].id,
                Duration::from_millis(100),
                Instant::now() + Duration::from_secs(20),
            )
            .await?;
            assert_eq!(values, [Value::I64(sum)], "{mode:?}");
            failed.push(ctx.fusion_stats().await?.failed.len());
            ctx.shutdown().await;
        }
        assert_eq!(failed[0], failed[1], "{mode:?}: fusions failed at depth 10, 100");
    }
    Ok(())
}

#[tokio::test]
async fn native_body_attribute_dynamic_instance() -> Result<()> {
    check_dynamic_native(true).await
}

#[tokio::test]
async fn native_body_attribute_dynamic_rejects_stateful() -> Result<()> {
    check_dynamic_native(false).await
}

run!(
    native_body_attribute_in_collection_slots,
    r#"{
    let f = |x: i64| { let y = #[native] (x * 2); y + 1 };
    array::map([1, 2, 3], |x| { count(x); f(x) })
}"#,
    prints("[i64:3, i64:5, i64:7]")
);

#[tokio::test]
async fn dynamic_native_kernels_are_shared() -> Result<()> {
    use graphix_package_core::testing::{
        Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    use std::time::Duration;
    use tokio::time::Instant;

    for mode in [Mode::Jit, Mode::JitPar] {
        let (ctx, mut rx) = fixture_runtime(
            [(
                "/test.gx",
                result_source(
                    r#"{
                let g = |x: i64| { let y = #[native] (x * 2 + 1); y + 1 };
                let h = never(); h <- g;
                array::map(array::init(3000, |i| i), |x| h(x))
            }"#,
                ),
            )],
            crate::TEST_REGISTER,
            mode,
            |_| {},
        )
        .await?;
        let before = ctx.rt.fusion_stats().await?.fused;
        let start = Instant::now();
        let result = compile_result(&ctx).await?;
        let values = updates_until_quiet(
            &ctx.rt,
            &mut rx,
            result.exprs[0].id,
            Duration::from_millis(100),
            Instant::now() + Duration::from_secs(30),
        )
        .await?;
        let emitted = ctx.rt.fusion_stats().await?.fused - before;
        eprintln!("{mode:?}: elapsed {:?}, emitted {emitted}", start.elapsed());
        assert_eq!(
            values,
            [Value::Array((0..3000).map(|i| Value::I64(i * 2 + 2)).collect())]
        );
        assert!(emitted < 20, "emitted {emitted} regions for one definition");
        assert!(ctx.rt.control().invocations().1 >= 3000);
        ctx.shutdown().await;
    }
    Ok(())
}
