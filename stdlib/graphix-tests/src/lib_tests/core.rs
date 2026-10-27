use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
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

// `filter_err` answers the error among the elements and nothing for the
// rest (x-builtin-effects-06).
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

run!(once, ONCE, is(Value::I64(1)); FuseExpect::None);

const SKIP: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(skip(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip, SKIP, prints("[i64:4, i64:5, i64:6]"); FuseExpect::Jit);

const SKIP_ZERO: &str = r#"
{
  let x = [1, 2, 3];
  array::group(skip(#n: 0, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip_zero, SKIP_ZERO, prints("[i64:1, i64:2, i64:3]"); FuseExpect::Jit);

const SKIP_ALL: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(skip(#n: 5, array::iter([1, 2, 3])), timeout)
}
"#;

run!(skip_all, SKIP_ALL, is(Value::I64(0)); FuseExpect::None);

const TAKE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5, 6]);
  filter(x, |v| v == 6) ~ count(take(#n: 3, x))
}
"#;

run!(take, TAKE, is(Value::I64(3)); FuseExpect::None);

const TAKE_ZERO: &str = r#"
{
  let timeout = sys::time::timer(1, false) ~ 0;
  any(take(#n: 0, array::iter([1, 2, 3])), timeout)
}
"#;

run!(take_zero, TAKE_ZERO, is(Value::I64(0)); FuseExpect::None);

const TAKE_MORE: &str = r#"
{
  let x = [1, 2, 3];
  array::group(take(#n: 10, array::iter(x)), |n, _| n == 3)
}
"#;

run!(take_more, TAKE_MORE, prints("[i64:1, i64:2, i64:3]"); FuseExpect::Jit);

const ALL: &str = r#"
{
  let x = 1;
  let y = x;
  let z = y;
  all(x, y, z)
}
"#;

run!(all, ALL, is(Value::I64(1)); FuseExpect::Jit);

// all stays silent over arguments that differ: plain values, references
// to two variables of one value, and abstracts their Eq tells apart, with
// a reference beside them and without.
const ALL_DIFFERENT: &str = r#"
{
  type Key = Abstract<string>;
  impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
  let a = 1;
  let b = 1;
  let later = sys::time::after_idle(duration:50.ms, "none");
  any(
    all(1, 2) ~ "values",
    all(&a, &b) ~ "references",
    all(Key("Foo"), Key("bar")) ~ "keys",
    all((&a, Key("Foo")), (&a, Key("bar"))) ~ "keys beside a reference",
    later
  )
}
"#;

run!(all_different, ALL_DIFFERENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "none")
}; FuseExpect::None);

const SUM: &str = r#"
{
  let tweeeeenywon = [1, 2, 3, 4, 5, 6];
  #[native] sum(tweeeeenywon)
}
"#;

run!(sum, SUM, is(Value::I64(21)); FuseExpect::Jit);

const PRODUCT: &str = r#"
{
  let tweeeeenywon = [5, 2, 2, 1.05];
  #[native] product(tweeeeenywon)
}
"#;

// `product` over a heterogeneous `Array<Number>` fuses: a union of the
// number typedefs is the primitive set it names.
run!(product, PRODUCT, is(Value::F64(21.0)); FuseExpect::Jit);

const DIVIDE: &str = r#"
{
  let tweeeeenywon = [84, 2, 2];
  #[native] divide(tweeeeenywon)
}
"#;

run!(divide, DIVIDE, is(Value::I64(21)); FuseExpect::Jit);

// min/max compare each argument as a whole value under the total order.
const MIN_VALUE_LEVEL: &str = r#"
   min([1, 9], [3, 4])
"#;

run!(min_value_level, MIN_VALUE_LEVEL, prints("[i64:1, i64:9]"); FuseExpect::Jit);

const MAX_VALUE_LEVEL: &str = r#"
   max([1, 9], [3, 4])
"#;

run!(max_value_level, MAX_VALUE_LEVEL, prints("[i64:3, i64:4]"); FuseExpect::Jit);

const MIN: &str = r#"
   min(1, 2, 3, 4, 5, 6, 0)
"#;

run!(min, MIN, is(Value::I64(0)); FuseExpect::Jit);

const MAX: &str = r#"
   max(1, 2, 3, 4, 5, 6, 0)
"#;

run!(max, MAX, is(Value::I64(6)); FuseExpect::Jit);

const AND: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  and(x < y, y < z, x > 0, z < 10)
}
"#;

run!(and, AND, is(Value::Bool(true)); FuseExpect::Jit);

const OR: &str = r#"
  or(false, false, true)
"#;

run!(or, OR, is(Value::Bool(true)); FuseExpect::Jit);

const INDEX: &str = r#"
{
  let a = ["foo", "bar", 1, 2, 3];
  cast<i64>(a[2]?)? + cast<i64>(a[3]?)?
}
"#;

run!(index, INDEX, is(Value::I64(3)); FuseExpect::Jit);

const SLICE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  [sum(a[2..4]?), sum(a[6..]?), sum(a[..2]?)]
}
"#;

run!(slice, SLICE, prints("[i64:7, i64:15, i64:3]"); FuseExpect::Jit);

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

run!(queuefn_queue_pop, QUEUEFN_QUEUE_POP, prints("[i64:10, i64:20, i64:30, i64:40]"); FuseExpect::None);

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

run!(queuefn_multi_arg, QUEUEFN_MULTI_ARG, prints("[i64:201, i64:403, i64:605]"); FuseExpect::None);

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

run!(queuefn_closure_capture, QUEUEFN_CLOSURE_CAPTURE, prints("[i64:100, i64:200, i64:300]"); FuseExpect::None);

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

run!(queuefn_count_ref, QUEUEFN_COUNT_REF, prints("[i64:-1, i64:2]"); FuseExpect::None);

// A place target for #count is written as a level too: the cycle's
// changes land as one write, as they do for a binding.
run!(queuefn_count_place, r#"
{
  let s = { depth: -1, other: 0 };
  let qf = queuefn(#count: &mut s.depth, #trigger: never(), |x: i64| -> i64 x * 10);
  qf(1);
  qf(2);
  qf(3);
  array::group(s.depth, |n, _| n == 2)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(-1), Value::I64(2)])
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

run!(queuefn_hof_callback, QUEUEFN_HOF_CALLBACK, is(Value::I64(1)); FuseExpect::None);

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
    prints("[i64:10, i64:20, i64:30]"); FuseExpect::None);

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
    prints("[i64:1010, i64:1020, i64:1030]"); FuseExpect::None);

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
    prints("[i64:100, i64:200, i64:300]"); FuseExpect::None);

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
    }; FuseExpect::None);

const COUNT: &str = r#"
{
  let a = [0, 1, 2, 3];
  array::group(count(array::iter(a)), |n, _| n == 4)
}
"#;

run!(count, COUNT, prints("[i64:1, i64:2, i64:3, i64:4]"); FuseExpect::Jit);

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

run!(uniq, UNIQ, is(Value::I64(3)); FuseExpect::None);

const RANGE: &str = r#"
  array::group(range(0, 4), |n, _| n == 4)
"#;

run!(range, RANGE, prints("[i64:0, i64:1, i64:2, i64:3]"); FuseExpect::None);

const THROTTLE: &str = r#"
{
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let data = throttle(array::iter(data));
    array::group(data, |n, _| n == 2)
}
"#;

run!(throttle, THROTTLE, prints("[i64:1, i64:10]"); FuseExpect::Jit);

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

// A float range of finite bounds wider than f64 holds is sampled
// (x-panics-02).
run!(rand_wide_float_range, r#"
  rand::rand(#start: f64:-1e308, #end: f64:1e308, #clock: null)
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(v)) if *v >= -1e308 && *v < 1e308)
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

run!(hold_basic, HOLD_BASIC, is(Value::I64(42)); FuseExpect::None);

const HOLD_MULTIPLE: &str = r#"
{
  let v = array::iter([10, 20, 30]);
  let c = filter(v, |x| x == 30);
  hold(#clock: c, v)
}
"#;

// hold releases only the value standing when its clock fires.
run!(hold_multiple, HOLD_MULTIPLE, is(Value::I64(30)); FuseExpect::None);

const HOLD_NO_TRIGGER: &str = r#"
{
  let clock = never();
  let value = 42;
  any(count(hold(#clock, value)), 0)
}
"#;

run!(hold_no_trigger, HOLD_NO_TRIGGER, is(Value::I64(0)); FuseExpect::None);

const HOLD_MULTIPLE_VALUES: &str = r#"
{
  let clock = sys::time::timer(0.5, false) ~ 1;
  let values = [100, 200, 300];
  // Only the last value should be held when clock fires
  hold(#clock, array::iter(values))
}
"#;

run!(hold_multiple_values, HOLD_MULTIPLE_VALUES, is(Value::I64(300)); FuseExpect::Jit);

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

// Two references to one variable, taken at two sites, are equal to all
// as they are to `==`.
run!(all_compares_references_by_what_they_name, r#"{
    let a = 1;
    let r1 = &a;
    let r2 = &a;
    *all(r1, r2)
}"#, is(Value::I64(1)); FuseExpect::None);

// opt::contains compares as `==` does: a reference by what it names, and
// a type `==` refuses is refused.
run!(opt_contains_compares_references_by_what_they_name, r#"{
    let a = 1;
    let r1: [&i64, null] = &a;
    (opt::contains(r1, &a), opt::contains(null, &a))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[true, false]")
}; FuseExpect::None);

run!(opt_contains_tells_a_tuple_from_an_array, r#"{
    let x: [(i64, i64), Array<i64>, null] = (1, 2);
    opt::contains(x, [1, 2])
}"#, is(Value::Bool(false)); FuseExpect::Jit);

run!(opt_contains_refuses_the_indiscernible, r#"{
    let a = |x: i64| x;
    let x: [fn(x: i64) -> i64, fn(s: string) -> string, null] = a;
    opt::contains(x, a)
}"#, refused("'a must be Discernible"); FuseExpect::None);

// A release delivering one of a site's args and a new call of the other
// in the same cycle are two invocations, as the unqueued call makes.
run!(queuefn_release_beside_a_new_call, r#"{
    let c = 0;
    c <- select c { n if n < 8 => n + 1, _ => never() };
    let trig: Any = select c { 3 => null, 4 => null, 5 => null, _ => never() };
    let a = 0;
    a <- select c { 0 => 1, 1 => 2, _ => never() };
    let b = 0;
    b <- select c { 0 => 1, 4 => 4, _ => never() };
    let q = queuefn(#trigger: trig, |x: i64, y: i64| -> (i64, i64) (x, y));
    let out = q(a, b);
    let log: Array<(i64, i64)> = [];
    log <- out ~ array::push(log, out);
    select c { 8 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[[i64:0, i64:0], [i64:1, i64:1], [i64:2, i64:1], [i64:2, i64:4]]")
}; FuseExpect::Jit);

// A bottom argument bottoms the queued call as it does the direct one:
// nothing runs over the other argument and a stale one.
run!(queuefn_bottom_argument, r#"{
    let c = 0;
    c <- select c { n if n < 3 => n + 1, _ => never() };
    let s: [i64, null] = select c { 2 => null, n => n };
    let q = queuefn(#trigger: c, |x: i64, y: i64| -> i64 x * 100 + y);
    let out = q(c, s$);
    let log: Array<i64> = [];
    log <- out ~ array::push(log, out);
    select c { 3 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:101]")
}; FuseExpect::Jit);

// A sleeping site's queued calls keep their place: a release skips them
// while the site sleeps, and the credits banked meanwhile release them at
// its wake (core-aux-02).
run!(queuefn_sleeping_site_keeps_its_calls, r#"{
    let c = 0;
    c <- select c { n if n < 12 => n + 1, _ => never() };
    let trig: Any = select c { 4 => null, 5 => null, 6 => null, _ => never() };
    let q = queuefn(#trigger: trig, |x: i64| -> i64 x);
    let mode = uniq(select c { n if n < 3 => `A, n if n < 8 => `B, _ => `A });
    let xa = select c { 0 => 1, 1 => 2, 2 => 3, _ => never() };
    let xb = select c { 3 => 10, _ => never() };
    let r = select mode { `A => q(xa), `B => q(xb) };
    let log: Array<i64> = [];
    log <- r ~ array::push(log, r);
    select c { 12 => log, _ => never() }
}"#, prints("[i64:1, i64:10, i64:2, i64:3]"); FuseExpect::Jit);

// A trigger's fire born with the queuefn is no credit: one call is
// released per later tick, each seeing the write the one before made
// (queuefn-stale-read-01).
run!(queuefn_birth_fire_is_no_credit, r#"{
    let clk = 0;
    clk <- select clk { k if k < 6 => (k ~ clk) + 1, _ => never() };
    let got: Array<i64> = [];
    let q = queuefn(#trigger: clk, |x: i64| -> i64 { got <- x ~ array::push(got, x); x });
    let a = q(1);
    let b = q(2);
    let c = q(3);
    select clk { 6 => got, _ => never() }
}"#, prints("[i64:1, i64:2, i64:3]"); FuseExpect::Jit);

// A site's wake releases the calls its banked credit pays one a cycle, so
// each call's fired arguments reach f alone, and a call it makes in the
// wake cycle lands after them (core-aux-01, x-async-io-03).
run!(queuefn_wake_releases_one_call_a_cycle, r#"{
    let c = 0;
    c <- select c { n if n < 12 => n + 1, _ => never() };
    let trig: Any = select c { 4 => null, 5 => null, 6 => null, _ => never() };
    let a = 0;
    a <- select c { 0 => 1, 1 => 2, _ => never() };
    let b = 100;
    b <- select c { 1 => 200, 6 => 300, _ => never() };
    let mode = uniq(select c { n if n < 3 => `On, n if n < 7 => `Off, _ => `On });
    let calls: Array<(i64, i64)> = [];
    let q = queuefn(#trigger: trig, |x: i64, y: i64| -> (i64, i64) {
        calls <- (x, y) ~ array::push(calls, (x, y));
        (x, y)
    });
    let r = select mode { `On => q(a, b), `Off => never() };
    select c { 12 => calls, _ => never() }
}"#, prints("[[i64:0, i64:100], [i64:1, i64:100], [i64:2, i64:200], [i64:2, i64:300]]"); FuseExpect::Jit);

// A #count place whose key moves is told the depth at the move.
run!(queuefn_count_follows_a_moving_place, r#"{
    let c = 0;
    c <- select c { n if n < 6 => n + 1, _ => never() };
    let arr = [-1, -1];
    let i = 0;
    i <- select c { 3 => 1, _ => never() };
    let q = queuefn(#count: &mut arr[i], #trigger: never(), |x: i64| -> i64 x * 10);
    let r = q(select c { 0 => 1, 1 => 2, 2 => 3, _ => never() });
    select c { 6 => arr, _ => never() }
}"#, prints("[i64:2, i64:2]"); FuseExpect::Jit);

// The option HOFs answer every delivery of their input, as their select
// spellings do, even when the callback ignores its argument: after a
// null they take the new value.
run!(opt_hofs_answer_every_input, r#"{
    let c = 0;
    c <- select c { n if n < 4 => n + 1, _ => never() };
    let v: [i64, null] = select c { 2 => null, n => n };
    let k = 100;
    let enabled = true;
    let m = opt::map(v, |_| k);
    let sa = opt::is_some_and(v, |_| k > 0);
    let no = opt::is_none_or(v, |_| k < 0);
    let fl = opt::filter(v, |_| enabled);
    let fm = opt::flat_map(v, |_| k);
    select c { 4 => (m, sa, no, fl, fm), _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:100, true, false, i64:4, i64:100]")
}; FuseExpect::Jit);

// A queuefn whose arm sleeps keeps its queue and its credit: the calls
// made after the first wait for a trigger that never fires, before the
// sleep and after the wake (core-aux-01, x-builtin-effects-03).
run!(queuefn_sleep_is_a_pause, r#"{
    let c = 0;
    c <- select c { n if n < 8 => n + 1, _ => never() };
    let sel = select c { 3 => 1, _ => 0 };
    let qf = never();
    let r = select sel {
        0 => qf <- queuefn(#trigger: never(), |x: i64| -> i64 x * 10),
        _ => null
    };
    let x = select c { 1 => 10, 2 => 20, 5 => 50, _ => never() };
    let out = qf(x);
    let log: Array<i64> = [];
    log <- out ~ array::push(log, out);
    select c { 8 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:100]")
}; FuseExpect::Jit);

// A throttle whose arm sleeps keeps the window its last emission opened:
// the first fire after the wake waits out the rest of it (core-lib-02).
run!(throttle_sleep_keeps_the_window, r#"{
    let c = 0;
    c <- select c { n if n < 6 => n + 1, _ => never() };
    let log: Array<i64> = [];
    let on = select c { 2 => false, 3 => false, _ => true };
    select uniq(on) {
        true => {
            let t = throttle(#rate: duration:10.s, c);
            log <- t ~ array::push(log, t);
            t
        },
        false => never()
    };
    select c { 6 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:0]")
}; FuseExpect::Jit);

// The elements a range owes deliver though its arguments go bottom
// (core-lib-01).
run!(range_delivers_past_a_bottom_argument, r#"{
    let go = 0;
    let j = select go { 0 => 5, _ => never() };
    let v = range(0, j)$;
    go <- v ~ 1;
    let log: Array<i64> = [];
    log <- v ~ array::push(log, v);
    select array::len(log) { 5 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:1, i64:2, i64:3, i64:4]")
}; FuseExpect::Jit);

// all over a type holding a reference still compares an abstract part by
// its core Eq (core-lib-03).
run!(all_with_a_reference_uses_eq, r#"{
    type Key = Abstract<string>;
    impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
    let x = 0;
    let with_ref = all((&x, Key("Foo")), (&x, Key("FOO")));
    with_ref ~ "fired"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "fired")
}; FuseExpect::None);
