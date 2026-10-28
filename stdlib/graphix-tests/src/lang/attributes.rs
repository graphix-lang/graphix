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

run!(tail_recursive_unfused, TAIL_RECURSIVE_UNFUSED, refused("#[tail_recursive]: no loop is built — the body did not fuse"); FuseExpect::None; jit_only);

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
    use std::time::Duration;
    use tokio::time::Instant;

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
