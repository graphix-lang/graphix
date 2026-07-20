use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const IS_ERR: &str = r#"
{
  let errors: Error<Any> = never();
  try
    let a = [42, 43, 44];
    let y = a[0]? + a[3]?
  catch(e) => errors <- e;
  is_err(errors)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(is_err, IS_ERR, |v: Result<&Value>| match v {
    Ok(Value::Bool(b)) => *b,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const FILTER_ERR: &str = r#"
{
  let a = [42, 43, 44, error("foo")];
  filter_err(array::iter(a))
}
"#;

// ASPIRE: Jit (currently None) — blocked on: filter_err builtin not yet fused
run!(filter_err, FILTER_ERR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ERROR: &str = r#"
  error("foo")
"#;

// `error(v)` is a Sync builtin (`fn(e: 'a) -> Error<'a>`) — now fuses as a
// value-shape `DynCall` (an error value, runtime `Value::Error(Arc)`,
// reusing the bytes/map two-register `Value` machinery).
run!(error, ERROR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const ONCE: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  once(array::iter(x))
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(once, ONCE, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SKIP: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(skip(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(skip, SKIP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(4), Value::I64(5), Value::I64(6)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const SKIP_ZERO: &str = r#"
{
  let x = [1, 2, 3];
  array::group(skip(#n: 0, array::iter(x)), |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(skip_zero, SKIP_ZERO, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const SKIP_ALL: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(skip(#n: 5, array::iter([1, 2, 3])), timeout)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(skip_all, SKIP_ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const TAKE: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(take(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(take, TAKE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const TAKE_ZERO: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(take(#n: 0, array::iter([1, 2, 3])), timeout)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(take_zero, TAKE_ZERO, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const TAKE_MORE: &str = r#"
{
  let x = [1, 2, 3];
  array::group(take(#n: 10, array::iter(x)), |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(take_more, TAKE_MORE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ALL: &str = r#"
{
  let x = 1;
  let y = x;
  let z = y;
  all(x, y, z)
}
"#;

// Not fused, by design: `all: fn(@args: Any) -> Any` is fully dynamic
// (`Any` args + return). The dynamism is explicit in the signature, so
// the user gets the compatible-but-slow path predictably.
run!(all, ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SUM: &str = r#"
{
  let tweeeeenywon = [1, 2, 3, 4, 5, 6];
  sum(tweeeeenywon)
}
"#;

// Not fused, by design: `sum: fn(@args: Array<Number>) -> Number`. A
// `Number` array is heterogeneous — each element may be a different
// number type — so the result type is genuinely dynamic and can't be a
// concrete kernel return. The dynamism is visible in the type, so the
// user gets the compatible-but-slow path predictably. (A monomorphic
// `'a: Number` signature would mean a *homogeneous* array — a different,
// narrower promise than `sum` makes.)
run!(sum, SUM, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const PRODUCT: &str = r#"
{
  let tweeeeenywon = [5, 2, 2, 1.05];
  product(tweeeeenywon)
}
"#;

// Not fused, by design: same as `sum` — `product: fn(@args:
// Array<Number>) -> Number` is dynamic over a heterogeneous numeric
// array. This fixture's `[5, 2, 2, 1.05]` is literally mixed (i64 +
// f64), so the runtime element type isn't statically known.
run!(product, PRODUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(21.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const DIVIDE: &str = r#"
{
  let tweeeeenywon = [84, 2, 2];
  divide(tweeeeenywon)
}
"#;

// Not fused, by design: same as `sum` — `divide` over a heterogeneous
// `Array<Number>` is dynamic.
run!(divide, DIVIDE, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// min/max are VALUE-LEVEL (Eric's ruling 2026-07-08): each argument
// compares as a whole value under graphix's total order — the old
// recursive flatten was a bscript holdover that contradicted the
// declared type (`min([1,2],[3])` promised an Array and returned a
// scalar, breaking the JIT return ABI — soak jul07b).
const MIN_VALUE_LEVEL: &str = r#"
   min([1, 9], [3, 4])
"#;

run!(min_value_level, MIN_VALUE_LEVEL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(1), Value::I64(9)]),
        _ => false,
    }
});

const MAX_VALUE_LEVEL: &str = r#"
   max([1, 9], [3, 4])
"#;

run!(max_value_level, MAX_VALUE_LEVEL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(3), Value::I64(4)]),
        _ => false,
    }
});

const MIN: &str = r#"
   min(1, 2, 3, 4, 5, 6, 0)
"#;

run!(min, MIN, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const MAX: &str = r#"
   max(1, 2, 3, 4, 5, 6, 0)
"#;

run!(max, MAX, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const AND: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  and(x < y, y < z, x > 0, z < 10)
}
"#;

run!(and, AND, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const OR: &str = r#"
  or(false, false, true)
"#;

run!(or, OR, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const INDEX: &str = r#"
{
  let a = ["foo", "bar", 1, 2, 3];
  cast<i64>(a[2]?)? + cast<i64>(a[3]?)?
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(index, INDEX, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const SLICE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  [sum(a[2..4]?), sum(a[6..]?), sum(a[..2]?)]
}
"#;

// Not fused, by design: uses `sum` over `Array<Number>` slices —
// dynamic by design (see the `sum` fixture above).
run!(slice, SLICE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(15), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const FILTER0: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| x > 7)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(filter0, FILTER0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(8)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const FILTER1: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| str::len(x) > 7)
}
"#;

run!(filter1, FILTER1, |v: Result<&Value>| {
    match v {
        Ok(_) => false,
        Err(_) => true,
    }
}; graphix_package_core::testing::FuseExpect::None);

const QUEUE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::map(a, |v| sys::net::publish("/local/[v]", v));
  let v = array::iter(a);
  let clock: Any = once(v);
  let q = queue(#clock, v);
  let out: Primitive = sys::net::subscribe("/local/[q]")?;
  clock <- out;
  array::group(out, |n, _| n == 8)
}
"#;

run!(queue, QUEUE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6), Value::I64(7), Value::I64(8)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const QUEUEFN_IMMEDIATE: &str = r#"
{
  let qf = queuefn(#trigger: never(), |x: i64| -> i64 x * 10);
  qf(7)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_immediate, QUEUEFN_IMMEDIATE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(70)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const QUEUEFN_QUEUE_POP: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64| -> i64 x * 10);
  let out = qf(array::iter([1, 2, 3, 4]));
  feedback <- out;
  array::group(out, |n, _| n == 4)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_queue_pop, QUEUEFN_QUEUE_POP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(10), Value::I64(20), Value::I64(30), Value::I64(40)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const QUEUEFN_MULTI_ARG: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64, y: i64| -> i64 x + y * 100);
  let xs = array::iter([1, 3, 5]);
  let ys = array::iter([2, 4, 6]);
  let out = qf(xs, ys);
  feedback <- out;
  array::group(out, |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_multi_arg, QUEUEFN_MULTI_ARG, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(201), Value::I64(403), Value::I64(605)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const QUEUEFN_CLOSURE_CAPTURE: &str = r#"
{
  let multiplier = 100;
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64| -> i64 x * multiplier);
  let out = qf(array::iter([1, 2, 3]));
  feedback <- out;
  array::group(out, |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_closure_capture, QUEUEFN_CLOSURE_CAPTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(100), Value::I64(200), Value::I64(300)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// Verify #count writes when the queue grows. The wrapper writes count
// every time it pushes (pops happen via the queuefn node when triggered).
// With #trigger=never(), nothing pops, so depth ramps up.
const QUEUEFN_COUNT_REF: &str = r#"
{
  let depth = 0;
  let qf = queuefn(#count: &depth, #trigger: never(), |x: i64| -> i64 x * 10);
  qf(1);  // immediate (pop_count=1), no push
  qf(2);  // push, depth -> 1
  qf(3);  // push, depth -> 2
  // depth observer sees [0 (let init), 1, 2]
  array::group(depth, |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_count_ref, QUEUEFN_COUNT_REF, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(0), Value::I64(1), Value::I64(2)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// SOUNDNESS BOUNDARY (folding `static_resolve_fn_args` into `typecheck1`):
// a queuefn (async wrapper) passed as a HOF callback must NOT be statically
// resolved/fused — that would inline the inner lambda and BYPASS the queue.
// `qf`'s bind value is a CallSite (`queuefn(...)`), not a `Lambda`, so the
// value-based discovery in `try_static_resolve` skips it, so the collection
// callback remains dynamic (`FuseExpect::None`). A regression that fused it
// would flip this to `Jit`. Single-element map so the first (immediate)
// call produces a value: `qf(7) -> 70`.
const QUEUEFN_HOF_CALLBACK: &str = r#"
{
  let qf = queuefn(#trigger: never(), |x: i64| -> i64 x * 10);
  array::map([i64:7], qf)
}
"#;

run!(queuefn_hof_callback, QUEUEFN_HOF_CALLBACK, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(70)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

// Verify the wrapped fn is called when the wrapper output is fed back to
// the trigger. Each pop allows the next to fire, so all queued
// invocations eventually drain.
const QUEUEFN_FEEDBACK_DRAIN: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64| -> i64 x + 1);
  let out = qf(array::iter([10, 20, 30, 40, 50]));
  feedback <- out;
  array::group(out, |n, _| n == 5)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(queuefn_feedback_drain, QUEUEFN_FEEDBACK_DRAIN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(11), Value::I64(21), Value::I64(31), Value::I64(41), Value::I64(51)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

// Trigger arriving before any wrapper invocations should bank pop_count, so
// later calls dispatch immediately rather than queueing.
const QUEUEFN_TRIGGER_BEFORE_FN: &str = r#"
{
  // Three triggers arrive on init via array::iter — they bank pop_count
  // (queue is empty at each tick) so subsequent calls dispatch immediately.
  let trigs: Any = array::iter([null, null, null]);
  let qf = queuefn(#trigger: trigs, |x: i64| -> i64 x * 10);
  let xs = array::iter([1, 2, 3]);
  array::group(qf(xs), |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(
    queuefn_trigger_before_fn,
    QUEUEFN_TRIGGER_BEFORE_FN,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => match &a[..] {
                [Value::I64(10), Value::I64(20), Value::I64(30)] => true,
                _ => false,
            },
            _ => false,
        }
    }; graphix_package_core::testing::FuseExpect::None);

// Verify a wrapped fn with a trigger-style arg (`tick ~ x + 1000`) is not
// fooled by queueing: each tick/x pair emits exactly once, no spurious
// emissions when one of the two fires alone.
const QUEUEFN_TRIGGER_ARG: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(
    #trigger: feedback,
    |#tick: Any, x: i64| -> i64 tick ~ x + 1000
  );
  let ticks: Any = array::iter([null, null, null]);
  let xs = array::iter([10, 20, 30]);
  let out = qf(#tick: ticks, xs);
  feedback <- out;
  array::group(out, |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(
    queuefn_trigger_arg,
    QUEUEFN_TRIGGER_ARG,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => match &a[..] {
                [Value::I64(1010), Value::I64(1020), Value::I64(1030)] => true,
                _ => false,
            },
            _ => false,
        }
    }; graphix_package_core::testing::FuseExpect::None);

// The point of queuefn is to protect an async-side-effecting fn from being
// re-entered before its current invocation has produced a result. This test
// puts an actual netidx subscription inside the wrapped lambda: each call
// subscribes to a different path. Without queuefn, all three iter values
// would fire same-cycle and three subscribes would race; with queuefn they
// serialize via feedback (each subscription's value triggers the next pop).
const QUEUEFN_NET_SUBSCRIBE: &str = r#"
{
  use sys;
  sys::net::publish("/local/q_async/a", 100);
  sys::net::publish("/local/q_async/b", 200);
  sys::net::publish("/local/q_async/c", 300);
  let feedback: Any = never();
  let qf = queuefn(
    #trigger: feedback,
    |path: string| -> i64 sys::net::subscribe(path)?
  );
  let p: string = array::iter([
    "/local/q_async/a",
    "/local/q_async/b",
    "/local/q_async/c"
  ]);
  let out = qf(p);
  feedback <- out;
  array::group(out, |n, _| n == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(
    queuefn_net_subscribe,
    QUEUEFN_NET_SUBSCRIBE,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => match &a[..] {
                [Value::I64(100), Value::I64(200), Value::I64(300)] => true,
                _ => false,
            },
            _ => false,
        }
    }; graphix_package_core::testing::FuseExpect::None);

// Verify the per-cycle delta semantics. Wrapped fn only emits when its tick
// arg fires (`tick ~ x + 1000`). Two ticks pair with two of three x values;
// the third x fires alone and gets queued without a tick. On pop, NEW impl
// sets only bid_x; pred sees x without a fresh tick, so no spurious emit.
// Total emits should be 2. (Old impl re-fires a cached tick on every pop and
// would emit a third spurious 1030.) After a delay, sample the count.
const QUEUEFN_DELTA_PER_CYCLE: &str = r#"
{
  use sys;
  let feedback: Any = never();
  let qf = queuefn(
    #trigger: feedback,
    |#tick: Any, x: i64| -> i64 tick ~ x + 1000
  );
  let ticks: Any = array::iter([null, null]);
  let xs = array::iter([10, 20, 30]);
  let out = qf(#tick: ticks, xs);
  feedback <- out;
  let count = 0;
  count <- out ~ count + 1;
  let done: Any = sys::time::timer(duration:200.ms, false);
  done ~ count
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(
    queuefn_delta_per_cycle,
    QUEUEFN_DELTA_PER_CYCLE,
    |v: Result<&Value>| {
        match v {
            Ok(Value::I64(2)) => true,
            _ => false,
        }
    }; graphix_package_core::testing::FuseExpect::Jit);

const COUNT: &str = r#"
{
  let a = [0, 1, 2, 3];
  array::group(count(array::iter(a)), |n, _| n == 4)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(count, COUNT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const SAMPLE: &str = r#"
{
  let a = [0, 1, 2, 3];
  let x = "tweeeenywon!";
  array::group(array::iter(a) ~ x, |n, _| n == 4)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(sample, SAMPLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2), Value::String(s3)] => {
                s0 == s1 && s1 == s2 && s2 == s3 && &**s3 == "tweeeenywon!"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const UNIQ: &str = r#"
{
  let a = [1, 1, 1, 1, 1, 1, 1];
  uniq(array::iter(a))
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(uniq, UNIQ, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(1)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ: &str = r#"
  array::group(seq(0, 4), |n, _| n == 4)
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(seq, SEQ, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(0), Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const THROTTLE: &str = r#"
{
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let data = throttle(array::iter(data));
    array::group(data, |n, _| n == 2)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(throttle, THROTTLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(10)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const NEVER: &str = r#"
{
   let x = never(100);
   any(x, 0)
}
"#;

run!(never, NEVER, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(0)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const MEAN: &str = r#"
{
  let a = [0, 1, 2, 3];
  mean(a)
}
"#;

run!(mean, MEAN, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(1.5)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RAND: &str = r#"
  rand::rand(#clock:null)
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(rand, RAND, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(v)) if *v >= 0. && *v < 1.0 => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const RAND_PICK: &str = r#"
  rand::pick(["Chicken is coming", "Grape", "Pilot!"])
"#;

run!(rand_pick, RAND_PICK, |v: Result<&Value>| {
    match v {
        Ok(Value::String(v)) => v == "Chicken is coming" || v == "Grape" || v == "Pilot!",
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RAND_SHUFFLE: &str = r#"
  rand::shuffle(["Chicken is coming", "Grape", "Pilot!"])
"#;

run!(rand_shuffle, RAND_SHUFFLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) if a.len() == 3 => {
            a.contains(&Value::from("Chicken is coming"))
                && a.contains(&Value::from("Grape"))
                && a.contains(&Value::from("Pilot!"))
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const HOLD_BASIC: &str = r#"
{
  let clock = 1;
  let value = 42;
  hold(#clock, value)
}
"#;

run!(hold_basic, HOLD_BASIC, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const HOLD_MULTIPLE: &str = r#"
{
  let values = [10, 20, 30];
  let triggers = [1, 1, 1];
  let v = hold(#clock:array::iter(triggers), array::iter(values));
  let held_values = array::group(v, |n, _| n == 3);
  array::len(held_values)
}
"#;

// The hold call itself node-walks (hold is deliberately Async: it is
// update-history-sensitive and the DynCall protocol re-delivers every
// arg per dispatch — findings/hold-relatch-jul2026); Jit here means
// the scalar sub-regions around it fuse.
run!(hold_multiple, HOLD_MULTIPLE, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const HOLD_NO_TRIGGER: &str = r#"
{
  let clock = never();
  let value = 42;
  any(count(hold(#clock, value)), 0)
}
"#;

// The hold call itself node-walks (hold is deliberately Async: it is
// update-history-sensitive and the DynCall protocol re-delivers every
// arg per dispatch — findings/hold-relatch-jul2026); Jit here means
// the scalar sub-regions around it fuse.
run!(hold_no_trigger, HOLD_NO_TRIGGER, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const HOLD_MULTIPLE_VALUES: &str = r#"
{
  let clock = sys::time::timer(0.5, false) ~ 1;
  let values = [100, 200, 300];
  // Only the last value should be held when clock fires
  hold(#clock, array::iter(values))
}
"#;

// The hold call itself node-walks (hold is deliberately Async: it is
// update-history-sensitive and the DynCall protocol re-delivers every
// arg per dispatch — findings/hold-relatch-jul2026); Jit here means
// the scalar sub-regions around it fuse.
run!(hold_multiple_values, HOLD_MULTIPLE_VALUES, |v: Result<&Value>| match v {
    Ok(Value::I64(300)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const NOW: &str = r#"sys::time::now(null)"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(now, NOW, |v: Result<&Value>| match v {
    Ok(Value::DateTime(_)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ONCE_TAINTED_NOT_COUNTED: &str = r#"
{
  let v = i64:0;
  let x = once({
    let rec f = |n: i64| -> i64 select n {
      m if m <= i64:0 => (m / m),
      m => f(m - i64:1)
    };
    let v = f(i64:1);
    let r = &v;
    *r <- i64:1;
    select v {
      x if true => i64:200,
      x => x
    }
  } * i64:2);
  [v, x]
}
"#;

run!(
    once_tainted_not_counted,
    ONCE_TAINTED_NOT_COUNTED,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                a.iter().map(|v| v.clone().cast_to::<i64>().unwrap()).collect::<Vec<_>>()
                    == vec![0, 400]
            }
            _ => false,
        }
    };
    graphix_package_core::testing::FuseExpect::Jit
);
