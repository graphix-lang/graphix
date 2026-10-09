use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::subscriber::Value;

const IS_ERR: &str = r#"
{
  let errors: Error<Any> = never();
  catch(e) errors <- e;
  let a = [42, 43, 44];
  let y = a[0]? + a[3]?;
  is_err(errors)
}
"#;

run!(is_err, IS_ERR, |v: Result<&Value>| match v {
    Ok(Value::Bool(b)) => *b,
    _ => false,
}; FuseExpect::Jit);

const FILTER_ERR: &str = r#"
{
  let a = [42, 43, 44, error("foo")];
  filter_err(array::iter(a))
}
"#;

// `filter_err` node-walks by rule; the kernel is the array literal's
// `error("foo")`.
run!(filter_err, FILTER_ERR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const ERROR: &str = r#"
  error("foo")
"#;

// `error(v)` is a fast fn; the error value is a value-shape return.
run!(error, ERROR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const ONCE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5, 6]);
  filter(x, |v| v == 6) ~ count(once(x))
}
"#;

run!(once, ONCE, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::None);

const SKIP: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(skip(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip, SKIP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(4), Value::I64(5), Value::I64(6)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const SKIP_ZERO: &str = r#"
{
  let x = [1, 2, 3];
  array::group(skip(#n: 0, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip_zero, SKIP_ZERO, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const SKIP_ALL: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(skip(#n: 5, array::iter([1, 2, 3])), timeout)
}
"#;

run!(skip_all, SKIP_ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; FuseExpect::None);

const TAKE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5, 6]);
  filter(x, |v| v == 6) ~ count(take(#n: 3, x))
}
"#;

run!(take, TAKE, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::None);

const TAKE_ZERO: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(take(#n: 0, array::iter([1, 2, 3])), timeout)
}
"#;

run!(take_zero, TAKE_ZERO, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; FuseExpect::None);

const TAKE_MORE: &str = r#"
{
  let x = [1, 2, 3];
  array::group(take(#n: 10, array::iter(x)), |n, _| n == 3)
}
"#;

run!(take_more, TAKE_MORE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ALL: &str = r#"
{
  let x = 1;
  let y = x;
  let z = y;
  all(x, y, z)
}
"#;

run!(all, ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; FuseExpect::Jit);

const SUM: &str = r#"
{
  let tweeeeenywon = [1, 2, 3, 4, 5, 6];
  sum(tweeeeenywon)
}
"#;

run!(sum, SUM, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; FuseExpect::Jit);

const PRODUCT: &str = r#"
{
  let tweeeeenywon = [5, 2, 2, 1.05];
  product(tweeeeenywon)
}
"#;

// `product` over a heterogeneous `Array<Number>`: the literal fuses (a
// primitive union is a Value), `product` itself node-walks.
run!(product, PRODUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(21.0)) => true,
    _ => false,
}; FuseExpect::Jit);

const DIVIDE: &str = r#"
{
  let tweeeeenywon = [84, 2, 2];
  divide(tweeeeenywon)
}
"#;

run!(divide, DIVIDE, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; FuseExpect::Jit);

// min/max compare each argument as a whole value under the total order.
const MIN_VALUE_LEVEL: &str = r#"
   min([1, 9], [3, 4])
"#;

run!(min_value_level, MIN_VALUE_LEVEL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(1), Value::I64(9)]),
        _ => false,
    }
}; FuseExpect::Jit);

const MAX_VALUE_LEVEL: &str = r#"
   max([1, 9], [3, 4])
"#;

run!(max_value_level, MAX_VALUE_LEVEL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(3), Value::I64(4)]),
        _ => false,
    }
}; FuseExpect::Jit);

const MIN: &str = r#"
   min(1, 2, 3, 4, 5, 6, 0)
"#;

run!(min, MIN, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; FuseExpect::Jit);

const MAX: &str = r#"
   max(1, 2, 3, 4, 5, 6, 0)
"#;

run!(max, MAX, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

const OR: &str = r#"
  or(false, false, true)
"#;

run!(or, OR, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
}; FuseExpect::Jit);

const INDEX: &str = r#"
{
  let a = ["foo", "bar", 1, 2, 3];
  cast<i64>(a[2]?)? + cast<i64>(a[3]?)?
}
"#;

run!(index, INDEX, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; FuseExpect::Jit);

const SLICE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  [sum(a[2..4]?), sum(a[6..]?), sum(a[..2]?)]
}
"#;

run!(slice, SLICE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(15), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const FILTER0: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| x > 7)
}
"#;

run!(filter0, FILTER0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(8)) => true,
        _ => false,
    }
}; FuseExpect::Jit);

const FILTER1: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| str::len(x) > 7)
}
"#;

run!(filter1, FILTER1, refused("string does not contain"); FuseExpect::None);

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
}; FuseExpect::Jit);

const QUEUEFN_IMMEDIATE: &str = r#"
{
  let qf = queuefn(#trigger: never(), |x: i64| -> i64 x * 10);
  qf(7)
}
"#;

run!(queuefn_immediate, QUEUEFN_IMMEDIATE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(70)) => true,
        _ => false,
    }
}; FuseExpect::None);

// The wrapper's call passes every formal and no more: a function with a
// defaulted label or a variadic argument is refused by the check.
run!(
    queuefn_refuses_defaulted_label,
    "queuefn(#trigger: never(), |#scale: i64 = 10, x: i64| -> i64 x * scale)(5)",
    refused("within Function does not contain");
    FuseExpect::None
);

run!(
    queuefn_refuses_variadic,
    "queuefn(#trigger: never(), max)(1, 5)",
    refused("within Function does not contain");
    FuseExpect::None
);

const QUEUEFN_QUEUE_POP: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64| -> i64 x * 10);
  let out = qf(array::iter([1, 2, 3, 4]));
  feedback <- out;
  array::group(out, |n, _| n == 4)
}
"#;

run!(queuefn_queue_pop, QUEUEFN_QUEUE_POP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(10), Value::I64(20), Value::I64(30), Value::I64(40)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::None);

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

run!(queuefn_multi_arg, QUEUEFN_MULTI_ARG, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(201), Value::I64(403), Value::I64(605)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::None);

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

run!(queuefn_closure_capture, QUEUEFN_CLOSURE_CAPTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(100), Value::I64(200), Value::I64(300)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

// `#count` is written each time the queue grows; with `#trigger=never()`
// nothing pops, so depth ramps up.
const QUEUEFN_COUNT_REF: &str = r#"
{
  let depth = -1;
  let qf = queuefn(#count: &mut depth, #trigger: never(), |x: i64| -> i64 x * 10);
  // immediate (pop_count=1), no push
  qf(1);
  // push, depth -> 1
  qf(2);
  // push, depth -> 2
  qf(3);
  // the depth is a level: the cycle's changes land as one write, so the
  // observer sees the let's init, then 2
  array::group(depth, |n, _| n == 2)
}
"#;

run!(queuefn_count_ref, QUEUEFN_COUNT_REF, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(-1), Value::I64(2)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

// A queuefn passed as a HOF callback must not be statically resolved
// (that would bypass the queue): the callback stays dynamic, `qf(7) -> 70`.
const QUEUEFN_HOF_CALLBACK: &str = r#"
{
  let depth = 0;
  let qf = queuefn(#count: &mut depth, #trigger: never(), |x: i64| -> i64 x * 10);
  let r = array::map([i64:7, i64:8], qf);
  filter(depth, |d| d == 1)
}
"#;

run!(queuefn_hof_callback, QUEUEFN_HOF_CALLBACK, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// Feeding the wrapper output back to the trigger drains every queued
// invocation.
const QUEUEFN_FEEDBACK_DRAIN: &str = r#"
{
  let feedback: Any = never();
  let qf = queuefn(#trigger: feedback, |x: i64| -> i64 x + 1);
  let out = qf(array::iter([10, 20, 30, 40, 50]));
  feedback <- out;
  array::group(out, |n, _| n == 5)
}
"#;

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
}; FuseExpect::None);

// A trigger arriving before any invocation banks pop_count, so later
// calls dispatch immediately.
const QUEUEFN_TRIGGER_BEFORE_FN: &str = r#"
{
  // array::iter delivers the three triggers one a cycle beside xs's
  // elements; each finds the queue empty and banks pop_count, so the calls
  // dispatch at once.
  let trigs: Any = array::iter([null, null, null]);
  let qf = queuefn(#trigger: trigs, |x: i64| -> i64 x * 10);
  let xs = array::iter([1, 2, 3]);
  array::group(qf(xs), |n, _| n == 3)
}
"#;

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
    }; FuseExpect::None);

// A wrapped fn with a trigger-style arg (`tick ~ x + 1000`) emits once
// per tick/x pair and never when one fires alone.
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
    }; FuseExpect::None);

// A netidx subscription inside the wrapped lambda: queuefn serializes
// the three subscribes via feedback.
const QUEUEFN_NET_SUBSCRIBE: &str = r#"
{
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
    }; FuseExpect::None);

// Per-cycle delta semantics: a pop sets only the queued arg, so an x
// queued without a tick emits nothing. Total emits: 2.
const QUEUEFN_DELTA_PER_CYCLE: &str = r#"
{
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

run!(
    queuefn_delta_per_cycle,
    QUEUEFN_DELTA_PER_CYCLE,
    |v: Result<&Value>| {
        match v {
            Ok(Value::I64(2)) => true,
            _ => false,
        }
    }; FuseExpect::Jit);

const COUNT: &str = r#"
{
  let a = [0, 1, 2, 3];
  array::group(count(array::iter(a)), |n, _| n == 4)
}
"#;

run!(count, COUNT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const SAMPLE: &str = r#"
{
  let a = [0, 1, 2, 3];
  let x = "tweeeenywon!";
  array::group(array::iter(a) ~ x, |n, _| n == 4)
}
"#;

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
}; FuseExpect::Jit);

const UNIQ: &str = r#"
{
  let x = array::iter([1, 1, 1, 2, 2, 2, 3]);
  filter(count(x), |n| n == 7) ~ count(uniq(x))
}
"#;

run!(uniq, UNIQ, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::None);

const RANGE: &str = r#"
  array::group(range(0, 4), |n, _| n == 4)
"#;

run!(range, RANGE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(0), Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::None);

const THROTTLE: &str = r#"
{
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let data = throttle(array::iter(data));
    array::group(data, |n, _| n == 2)
}
"#;

run!(throttle, THROTTLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(10)] => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

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
}; FuseExpect::None);

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
}; FuseExpect::Jit);

const RAND: &str = r#"
  rand::rand(#clock:null)
"#;

run!(rand, RAND, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(v)) if *v >= 0. && *v < 1.0 => true,
        _ => false,
    }
}; FuseExpect::None);

const RAND_PICK: &str = r#"
  rand::pick(["Chicken is coming", "Grape", "Pilot!"])
"#;

run!(rand_pick, RAND_PICK, |v: Result<&Value>| {
    match v {
        Ok(Value::String(v)) => v == "Chicken is coming" || v == "Grape" || v == "Pilot!",
        _ => false,
    }
}; FuseExpect::None);

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
}; FuseExpect::None);

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
}; FuseExpect::Jit);

const HOLD_MULTIPLE: &str = r#"
{
  let v = array::iter([10, 20, 30]);
  let c = filter(v, |x| x == 30);
  hold(#clock: c, v)
}
"#;

// hold releases only the value standing when its clock fires.
run!(hold_multiple, HOLD_MULTIPLE, |v: Result<&Value>| matches!(v, Ok(Value::I64(30))); FuseExpect::None);

const HOLD_NO_TRIGGER: &str = r#"
{
  let clock = never();
  let value = 42;
  any(count(hold(#clock, value)), 0)
}
"#;

// The hold call node-walks; the scalar sub-regions around it fuse.
run!(hold_no_trigger, HOLD_NO_TRIGGER, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; FuseExpect::Jit);

const HOLD_MULTIPLE_VALUES: &str = r#"
{
  let clock = sys::time::timer(0.5, false) ~ 1;
  let values = [100, 200, 300];
  // Only the last value should be held when clock fires
  hold(#clock, array::iter(values))
}
"#;

// The hold call node-walks; the scalar sub-regions around it fuse.
run!(hold_multiple_values, HOLD_MULTIPLE_VALUES, |v: Result<&Value>| match v {
    Ok(Value::I64(300)) => true,
    _ => false,
}; FuseExpect::Jit);

const NOW: &str = r#"sys::time::now(null)"#;

run!(now, NOW, |v: Result<&Value>| match v {
    Ok(Value::DateTime(_)) => true,
    _ => false,
}; FuseExpect::None);

const ONCE_TAINTED_NOT_COUNTED: &str = r#"
{
  let v = i64:0;
  let x = once({
    let rec f = |n: i64| -> i64 select n {
      m if m <= i64:0 => (m / m),
      m => f(m - i64:1)
    };
    let v = f(i64:1);
    let r = &mut v;
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
    FuseExpect::Jit
);

const TVAL_UNION_BLIND_PRINT: &str = r#"
{
  let a = [i64:0, ("foo", f64:42.)];
  let out = select a {
    [x, tl..] => tl,
    _ => never()
  };
  "[array::group(out, |i, x| true)]"
}
"#;

// The typed printer prefers informative union members: a never() arm's
// ⊥ member must not print the tuple element type-blind.
run!(
    tval_union_blind_print,
    TVAL_UNION_BLIND_PRINT,
    |v: Result<&Value>| {
        match v {
            Ok(Value::String(s)) => &**s == r#"[[("foo", 42)]]"#,
            _ => false,
        }
    };
    FuseExpect::None
);
