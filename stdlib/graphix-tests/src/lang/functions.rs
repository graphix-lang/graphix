// Tests for lambdas, first-class functions, labeled arguments, recursive functions

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
use netidx::publisher::Value;

const LAMBDA: &str = r#"
{
  let y = 10;
  let f = |x| x + y;
  f(10)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(lambda, LAMBDA, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const FIRST_CLASS_LAMBDAS: &str = r#"
{
  let doit = |x: i64| x + 1;
  let g = |f: fn(x: i64) -> i64, y| f(y) + 1;
  g(doit, 1)
}
"#;

// A monomorphic fn-typed param: `f(y) + 1` with `f: fn<'a: Number>` is
// ill-typed (concrete arithmetic on an arbitrary rigid 'a).
run!(first_class_lambdas, FIRST_CLASS_LAMBDAS, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const TWO_RIGID_VARS_INDEPENDENT: &str = r#"
{
  let pair = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ('a, 'b) (x, y);
  let (a, _) = pair(1, 1.5);
  let (_, b) = pair(2, 2);
  a + b
}
"#;

// A body that keeps its two declared variables apart accepts equal and
// unequal instantiations alike.
run!(two_rigid_vars_independent, TWO_RIGID_VARS_INDEPENDENT, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const TWO_RIGID_VARS_UNIFIED: &str = r#"
{
  let add = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y;
  add(1, 2)
}
"#;

// `+` is ('a, 'a) -> 'a, so this body is well typed only where 'a = 'b:
// a promise the signature does not make. Refused at the definition.
run!(two_rigid_vars_unified, TWO_RIGID_VARS_UNIFIED, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const DEFAULT_ILL_TYPED_AT_DEFINITION: &str = r#"
{
  let h = |#bar: i64 = |i| i, baz| bar + baz;
  1
}
"#;

// A default is checked at the definition, called or not: a lambda is
// not an i64.
run!(default_ill_typed_at_definition, DEFAULT_ILL_TYPED_AT_DEFINITION, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const DEFAULT_IN_CONSTRAINT_SET: &str = r#"
{
  let f = 'a: [Int, Float] |#start: 'a = 0.0, x: 'a| -> 'a start + x;
  let g = 'a: [Int, Float] |#start: 'a = 0.0, x: 'a| -> 'a start + x;
  select (f(1.5), g(#start: 1, 1)) {
    (f64:1.5, i64:2) => 1,
    _ => 0
  }
}
"#;

// Against a declared variable a default must fit the variable's
// constraints, not the variable: the site that omits the argument takes
// the default's type, a site that passes it takes its own.
run!(default_in_constraint_set, DEFAULT_IN_CONSTRAINT_SET, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const DEFAULT_OUTSIDE_CONSTRAINT_SET: &str = r#"
{
  let f = 'a: Int |#start: 'a = 0.0, x: 'a| -> 'a start + x;
  f(1)
}
"#;

run!(default_outside_constraint_set, DEFAULT_OUTSIDE_CONSTRAINT_SET, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LABELED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#foo: 0)
}
"#;

// None: `foo + bar` over `Number` params returns the loose `Number` set.
run!(labeled_args, LABELED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const REQUIRED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#bar: 0)
}
"#;

run!(required_args, REQUIRED_ARGS, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const MIXED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42, baz| foo + bar + baz;
  f(#foo: 0, 0)
}
"#;

// None: loose `Number` return.
run!(mixed_args, MIXED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ARG_SUBTYPING: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let g = |f: fn(#foo: Number) -> Number| f(#foo: 3);
  g(f)
}
"#;

// ASPIRE: Jit — fn-typed lambda arg passed as a value (the HOF gap).
run!(arg_subtyping, ARG_SUBTYPING, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ARG_NAME_SHORT: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let foo = 3;
  f(#foo)
}
"#;

// None: loose `Number` return.
run!(arg_name_short, ARG_NAME_SHORT, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LATE_BINDING0: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| x - y + c };
  let f = t.f;
  f(#y: 3, #x: 4)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(late_binding0, LATE_BINDING0, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LATE_BINDING1: &str = r#"
{
  type F = fn(#x: i64, #y: i64) -> i64;
  type T = { foo: string, bar: i64, f: F };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| (x - y) + c };
  let f: F = select array::iter([0, 1]) {
    0 => t.f,
    1 => u.f,
    _ => never()
  };
  array::group(f(#y: 3, #x: 4), |n, _| n == 2)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(late_binding1, LATE_BINDING1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2)] => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LATE_BINDING2: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  (t.f)(#y: 3, #x: 4)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(late_binding2, LATE_BINDING2, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LATE_BINDING3: &str = r#"
{
    let f: fn(x: i64) -> i64 = never();
    let res = f(1);
    f <- |i: i64| i + 1;
    res
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(late_binding3, LATE_BINDING3, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LATE_BINDING4: &str = r#"
{
    let f = |#foo: i64 = 0, #bar: i64 = 1, baz| (foo - bar) + baz;
    let g = |#bar: i64 = 1, #foo: i64 = 0, baz| (foo - bar) + baz;
    let h = |#bar: i64 = 1, #zam: i64 = 55, #foo: i64 = 0, baz| (foo - bar) + baz + zam;
    let fs = [f, g, h];
    let f: fn(x: i64) -> i64 = never();
    f <- array::iter(fs);
    array::group(f(1), |n, _| n == 3)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(late_binding4, LATE_BINDING4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 3]>() {
        Ok([0, 0, 55]) => true,
        Ok(_) | Err(_) => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// A tail loop collapses to one activation only when its body is
// stateless; `count` is stateful, so each iteration owns its own.
const TAIL_STATEFUL_PER_ITERATION: &str = r#"
{
  let rec go = |a: Array<i64>, acc: i64| -> i64 select a {
    [] => acc,
    [x, rest..] => go(rest, acc + count(x))
  };
  go([i64:10, i64:20, i64:30], i64:0)
}
"#;

run!(tail_stateful_per_iteration, TAIL_STATEFUL_PER_ITERATION, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3))
); graphix_package_core::testing::FuseExpect::None);

// A stateful builtin in a value-position select arm inside native
// recursion node-walks; each activation owns its `max`.
const TAIL_STATEFUL_SCALAR: &str = r#"
{
  let rec f = |n: i64, acc: i64| -> i64 select n {
    i64:0 => acc,
    _ => f(n - i64:1, acc + max(n))
  };
  f(i64:10, i64:0)
}
"#;

run!(tail_stateful_scalar, TAIL_STATEFUL_SCALAR, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(55))
); graphix_package_core::testing::FuseExpect::None);

const FOLD_STATEFUL_PER_SLOT: &str = r#"
array::fold([i64:10, i64:20, i64:30], i64:0, |acc, x| acc + count(x))
"#;

run!(fold_stateful_per_slot, FOLD_STATEFUL_PER_SLOT, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3))
); graphix_package_core::testing::FuseExpect::None);

// A stateless body still collapses: the same loop over `+` alone.
const TAIL_STATELESS_COLLAPSES: &str = r#"
{
  let rec go = |a: Array<i64>, acc: i64| -> i64 select a {
    [] => acc,
    [x, rest..] => go(rest, acc + x)
  };
  go([i64:10, i64:20, i64:30], i64:0)
}
"#;

run!(tail_stateless_collapses, TAIL_STATELESS_COLLAPSES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(60))
); graphix_package_core::testing::FuseExpect::None);

const RECURSIVE_LAMBDA0: &str = r#"
{
    let rec f = |x: i64| select x { x if x < 10 => f(x + 1), x => x };
    f(0)
}
"#;

// A `let rec` self-call knots to the def's own cells, so the loop fuses.
run!(recursive_lambda0, RECURSIVE_LAMBDA0, |v: Result<&Value>| match v {
    Ok(Value::I64(10)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A fully annotated arithmetic lambda.
const KIR_FUSED_ARITH: &str = r#"
{
    let f = |a: i64, b: i64| -> i64 a * a + b * b;
    f(3, 4)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(fused_arith, KIR_FUSED_ARITH, |v: Result<&Value>| match v {
    Ok(Value::I64(25)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A tail-recursive countdown: self-call in tail position loops.
const KIR_FUSED_TAIL_LOOP: &str = r#"
{
    let rec countdown = |n: i64, acc: i64| -> i64
        select n {
            0 => acc,
            _ => countdown(n - 1, acc + n)
        };
    countdown(100, 0)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(fused_tail_loop, KIR_FUSED_TAIL_LOOP, |v: Result<&Value>| match v {
    Ok(Value::I64(5050)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Deep sync tail recursion runs in constant stack on both engines.
const TAIL_LOOP_DEEP: &str = r#"
{
    let rec count = |n: i64, acc: i64| -> i64
        select n {
            0 => acc,
            _ => count(n - 1, acc + 1)
        };
    count(500000, 0)
}
"#;

run!(tail_loop_deep, TAIL_LOOP_DEEP, |v: Result<&Value>| match v {
    Ok(Value::I64(500000)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A self-call in operand position (`n * fact(n - 1)`) is not a tail
// call and must not be looped.
const FACT_VALUE_POSITION: &str = r#"
{
    let rec fact = |n: i64| -> i64 select n {
        0 => 1,
        _ => n * fact(n - 1)
    };
    fact(5)
}
"#;

run!(fact_value_position, FACT_VALUE_POSITION, |v: Result<&Value>| match v {
    Ok(Value::I64(120)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A mandelbrot-shape kernel.
const KIR_FUSED_MANDELBROT: &str = r#"
{
    let rec iterate = |zr: f64, zi: f64, cr: f64, ci: f64, i: i64| -> i64
        select i {
            0 => 0,
            _ if zr * zr + zi * zi > 4.0 => i,
            _ => iterate(zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci, i - 1)
        };
    iterate(0.0, 0.0, 1.0, 0.0, 10)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(fused_mandelbrot, KIR_FUSED_MANDELBROT, |v: Result<&Value>| match v {
    Ok(Value::I64(7)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// An unannotated callback passed to a HOF.
const KIR_FUSED_DEFERRED_MAP: &str = r#"
{
    use array::*;
    let xs = array::init(100, |idx: i64| idx);
    array::fold(xs, 0, |acc, x| acc + x * 2)
}
"#;

run!(fused_deferred_map, KIR_FUSED_DEFERRED_MAP, |v: Result<&Value>| match v {
    Ok(Value::I64(9900)) => true,
    _ => false,
});

// A recursive lambda with no annotations still computes correctly.
const KIR_LAZY_NO_ANNOTATIONS: &str = r#"
{
    let rec sum_to = |n, acc|
        select n {
            0 => acc,
            _ => sum_to(n - 1, acc + n)
        };
    sum_to(100, 0)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(lazy_no_annotations, KIR_LAZY_NO_ANNOTATIONS, |v: Result<&Value>| match v {
    Ok(Value::I64(5050)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A three-level call chain with no annotations. outer(5) = 20.
const KIR_LAZY_THREE_LEVEL: &str = r#"
{
    let inner = |x| x * x + 1;
    let middle = |x| inner(x) + inner(x + 1);
    let outer = |x| middle(x) - middle(x - 1);
    outer(5)
}
"#;

// The bare lambdas' operand cells settle to i64, so the chain fuses.
run!(lazy_three_level, KIR_LAZY_THREE_LEVEL, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A higher-order function with a function-typed argument: 5*5 + 1 = 26.
const KIR_DYNCALL_HOF: &str = r#"
{
    let square = |x: i64| -> i64 x * x;
    let combine = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x) + 1;
    combine(square, 5)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(dyncall_hof, KIR_DYNCALL_HOF, |v: Result<&Value>| match v {
    Ok(Value::I64(26)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A let-bound helper whose body is `array::fold` over a literal, called
// transitively: helper(5) + 1 = 51.
const KIR_DYNCALL_STATIC_NONFUSABLE: &str = r#"
{
    use array::*;
    let helper = |x: i64| -> i64 array::fold([x, x], 0, |a, b| a + b * b);
    let outer = |x: i64| -> i64 helper(x) + 1;
    outer(5)
}
"#;

run!(
    dyncall_static_nonfusable,
    KIR_DYNCALL_STATIC_NONFUSABLE,
    |v: Result<&Value>| match v {
        Ok(Value::I64(51)) => true,
        _ => false,
    }; graphix_package_core::testing::FuseExpect::Jit);

// A transitive chain g1 -> g2 -> g3 fuses whole: g1(10) = 21.
const TRANSITIVE_CHAIN: &str = r#"
{
    let g3 = |n: i64| -> i64 n + 1;
    let g2 = |n: i64| -> i64 g3(n) * 2;
    let g1 = |n: i64| -> i64 g2(n) - 1;
    g1(10)
}
"#;

run!(transitive_chain, TRANSITIVE_CHAIN, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A transitively-called callee whose body contains a cast: g(true) = 1.
const TRANSITIVE_CALLEE_DYNCALL: &str = r#"
{
    let g = |b: bool| cast<i64>(b)$;
    g(true) + g(false)
}
"#;

run!(transitive_callee_dyncall, TRANSITIVE_CALLEE_DYNCALL, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// The cast two callee levels deep: g(b) = h(b) + 10 = 21.
const TRANSITIVE_DYNCALL_CHAIN: &str = r#"
{
    let h = |b: bool| cast<i64>(b)$;
    let g = |b: bool| h(b) + 10;
    g(true) + g(false)
}
"#;

run!(transitive_dyncall_chain, TRANSITIVE_DYNCALL_CHAIN, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A callee shared by two separate regions must compile per region.
// a = g(true) + cast(false) = 1; bb = g(false) = 0; a + bb = 1.
const CROSS_REGION_CALLEE_BASE: &str = r#"
{
    let g = |b: bool| cast<i64>(b)$;
    let a = g(true) + cast<i64>(false)$;
    let bb = g(false);
    a + bb
}
"#;

run!(cross_region_callee_base, CROSS_REGION_CALLEE_BASE, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A recursive callee whose base case is a cast: g(3) = 1.
const RECURSIVE_CALLEE_DYNCALL: &str = r#"
{
    let rec g = |n: i64| -> i64 select n { 0 => cast<i64>(true)$, _ => g(n - 1) };
    g(3)
}
"#;

run!(recursive_callee_dyncall, RECURSIVE_CALLEE_DYNCALL, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LAMBDAMATCH0: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}: T| bar + bar;
  f(x)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(lambdamatch0, LAMBDAMATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LAMBDAMATCH1: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}| bar + bar;
  f(x)
}
"#;

run!(lambdamatch1, LAMBDAMATCH1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LAMBDAMATCH2: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f(x)
}
"#;

// ASPIRE: Jit — composite/value cross-kernel call args.
run!(lambdamatch2, LAMBDAMATCH2, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LAMBDAMATCH3: &str = r#"
{
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f({bar: 42, baz: 1})
}
"#;

run!(lambdamatch3, LAMBDAMATCH3, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const LAMBDAMATCH4: &str = r#"
{
  let f = |(i, _)| i * 2;
  f((42, "foo"))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(lambdamatch4, LAMBDAMATCH4, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LAMBDAMATCH5: &str = r#"
{
  let f = |(i, _)| i * 2;
  f("foo")
}
"#;

run!(lambdamatch5, LAMBDAMATCH5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const NESTED_OPTIONAL0: &str = r#"
{
    type T = { foo: i64, bar: i64 };
    let f = |#foo: i64 = 42, #bar: i64 = 42| -> T { foo, bar };
    type U = { f: T, baz: i64 };
    let g = |#f: T = f(), baz: i64| -> U { f, baz };

    let r = g(42);
    r.baz
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(nested_optional0, NESTED_OPTIONAL0, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Callsite args are updated every cycle, not only when the function
// binds: the function sees the last arg value delivered before binding.
const ARG_UPDATE_BEFORE_BIND: &str = r#"
{
    let vals = array::iter([10, 20, 30]);
    let step = 0;
    step <- select step {
        n if n < 5 => step + 1,
        _ => never()
    };
    let f: fn(x: i64) -> i64 = never();
    f <- select step {
        5 => |i: i64| -> i64 i + 1,
        _ => never()
    };
    f(vals)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(arg_update_before_bind, ARG_UPDATE_BEFORE_BIND, |v: Result<&Value>| match v {
    Ok(Value::I64(31)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Arg changes propagate after the function is already bound.
const ARG_UPDATE_AFTER_BIND: &str = r#"
{
    let x = 0;
    x <- select x {
        n if n < 3 => x + 1,
        _ => never()
    };
    let f = |i: i64| i * 10;
    array::group(f(x), |n, _| n == 4)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(arg_update_after_bind, ARG_UPDATE_AFTER_BIND, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 4]>() {
        Ok([0, 10, 20, 30]) => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Variadic args: extra positional args beyond the fixed signature.
const VARGS0: &str = r#"
array::push([1, 2], 3, 4, 5)
"#;

run!(vargs0, VARGS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5)] =>
            true,
        _ => false,
    },
    _ => false,
});

// Cross-kernel callee resolution keys on kernel identity, not source
// name: g's call to the outer f survives a later `let f` shadow.
const SHADOWED_NAME_CROSS_KERNEL: &str = r#"
{
  let f = |x: i64| -> i64 x + 1;
  let g = |y: i64| -> i64 f(y) * 2;
  let f = |x: i64| -> i64 x - 1;
  let q = 0;
  g(1) + f(2) + q
}
"#;

run!(shadowed_name_cross_kernel, SHADOWED_NAME_CROSS_KERNEL, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::I64(5))
); graphix_package_core::testing::FuseExpect::Jit);

// One polymorphic lambda called at two monomorphizations in one region
// keys two kernels.
const TWO_MONOMORPHIZATIONS_ONE_REGION: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + x;
  {
    let a = f(3);
    let b = f(2.5);
    cast<f64>(a)$ + b
  }
}
"#;

run!(two_monomorphizations_one_region, TWO_MONOMORPHIZATIONS_ONE_REGION, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::F64(11.0))
); graphix_package_core::testing::FuseExpect::Jit);

// A fold-callback local sharing a name with a nested callee's parameter
// resolves by BindId; the collection still fuses.
const FOLD_CALLBACK_NAME_COLLISION: &str = r#"
{
  let rec pair = |e: i64| -> i64 select e { 0 => 0, _ => e * 10 };
  let run_one = |s: i64| -> i64 {
    let e = s + 1;
    pair(e + 1) + e
  };
  array::fold(array::init(1, |i| i), 0, |acc, i| acc + run_one(i))
}
"#;

run!(fold_callback_name_collision, FOLD_CALLBACK_NAME_COLLISION, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::I64(21))
); graphix_package_core::testing::FuseExpect::Jit);

// An abandoned kernel-closure build (the rec lambda de-fuses on the
// error-arm base case) must not break a later region's compile.
const ABANDONED_KERNEL_CLOSURE: &str = r#"
{
  let rec f = |n: i64| -> i64 select n {
    m if m <= i64:0 => select (i64:7 +? i64:-100) { error as _ => i64:1, i64 as x => x },
    m => (m + f(m - i64:1))
  };
  let v = f(i64:8);
  false
}
"#;

run!(abandoned_kernel_closure, ABANDONED_KERNEL_CLOSURE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Bool(false))
); graphix_package_core::testing::FuseExpect::Jit);

// Taint escalation: a locally-unconsumed bottom bottoms only the
// consuming path, never the whole kernel.

// A fold whose init bottoms never dispatches; the independent tail fires.
const FOLD_BOTTOM_INIT_UNREAD_ACC: &str = r#"
{
  let b = i64:1 / i64:0;
  array::fold([i64:5, i64:7], b, |acc, x| x);
  array::fold([i64:5, i64:7], i64:0, |acc, x| x)
}
"#;

run!(fold_bottom_init_unread_acc, FOLD_BOTTOM_INIT_UNREAD_ACC, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; graphix_package_core::testing::FuseExpect::Jit);

// The dual: the callback consumes the bottom acc; the tail still fires.
const UNUSED_BOTTOM_COMPOSITE_WITH_HOF: &str = r#"
{
  let v = (array::map([i64:1], |i| i), (i64:1 / i64:0));
  false
}
"#;

run!(
    unused_bottom_composite_with_hof,
    UNUSED_BOTTOM_COMPOSITE_WITH_HOF,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(false)));
    graphix_package_core::testing::FuseExpect::Jit
);

// A bottom map slot taints the map's result, not the kernel.
const UNUSED_BOTTOM_MAP_SLOT: &str = r#"
{
  let m = array::map([i64:1, i64:0], |x| i64:5 / x);
  false
}
"#;

run!(unused_bottom_map_slot, UNUSED_BOTTOM_MAP_SLOT, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Bool(false))
); graphix_package_core::testing::FuseExpect::Jit);

// find scans all slots: a bottom predicate after the matching element
// bottoms the find; the independent tail fires.
const FIND_BOTTOM_AFTER_MATCH: &str = r#"
{
  let r = array::find([i64:1, i64:0], |x| (i64:5 / x) > i64:0);
  false
}
"#;

run!(find_bottom_after_match, FIND_BOTTOM_AFTER_MATCH, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Bool(false))
); graphix_package_core::testing::FuseExpect::Jit);

// A tainted fold init poisons the acc delivery only: a callback that
// never consumes the acc recovers.
const FOLD_TAINTED_INIT_RECOVERS: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { m if m <= i64:0 => i64:1 % m, m => f(m - i64:1) };
  let v = f(i64:1);
  array::fold([i64:5, i64:7], v, |a, x| x)
}
"#;

run!(fold_tainted_init_recovers, FOLD_TAINTED_INIT_RECOVERS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

// The consuming twin: a callback that reads the acc bottoms the fold.
const FOLD_TAINTED_INIT_CONSUMED_BOTTOMS: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { m if m <= i64:0 => i64:1 % m, m => f(m - i64:1) };
  let v = f(i64:1);
  let r = array::fold([i64:5, i64:7], v, |a, x| a + x);
  false
}
"#;

run!(
    fold_tainted_init_consumed_bottoms,
    FOLD_TAINTED_INIT_CONSUMED_BOTTOMS,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(false)))
);

// A capture read only by a sleeping select arm does not re-fire the
// retained collection slot.
const HOF_SLEEPING_ARM_CAPTURE_QUIET: &str = r#"
{
  let y = array::iter([1, 2, 3, 4]);
  let m = array::map([1], |x| select 1 { 1 => x, _ => y });
  let c = count(m);
  select count(y) { 4 => c, _ => never() }
}
"#;

run!(
    hof_sleeping_arm_capture_quiet,
    HOF_SLEEPING_ARM_CAPTURE_QUIET,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1)));
    graphix_package_core::testing::FuseExpect::Jit
);

// The dual: the body reads y in the taken path, so the map re-fires per
// y event.
const HOF_CONSUMED_CAPTURE_FIRES: &str = r#"
{
  let y = array::iter([1, 2, 3, 4]);
  let m = array::map([1], |x| x + y);
  let c = count(m);
  select count(y) { 4 => c, _ => never() }
}
"#;

run!(hof_consumed_capture_fires, HOF_CONSUMED_CAPTURE_FIRES, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(4)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A same-length source update with a constant callback body emits only
// initially.
const HOF_CONST_BODY_PREV_LEN: &str = r#"
{
  let y = array::iter([1, 2, 3, 4]);
  let src = [y];
  let m = array::map(src, |x| 7);
  let c = count(m);
  select count(y) { 4 => c, _ => never() }
}
"#;

run!(hof_const_body_prev_len, HOF_CONST_BODY_PREV_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Non-tail recursion depth is bounded by memory, not a counter: depth
// 1000 completes on both engines.
const DEEP_NONTAIL_RECURSION_COMPLETES: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  f(i64:1000)
}
"#;

run!(
    deep_nontail_recursion_completes,
    DEEP_NONTAIL_RECURSION_COMPLETES,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(500500))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A fold at the bottom of a non-tail recursion fires once.
const NONTAIL_RECURSION_WITH_FOLD_AT_BASE: &str = r#"
{
  let rec f = |n: i64| -> i64 select n {
    i64:0 => {
      let xs = array::init(i64:100, |idx: i64| idx);
      array::fold(xs, i64:0, |acc, x| acc + x * i64:2)
    },
    _ => n + f(n - i64:1)
  };
  f(i64:254)
}
"#;

run!(
    nontail_recursion_with_fold_at_base,
    NONTAIL_RECURSION_WITH_FOLD_AT_BASE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42285))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A recursion's result as a fold's init beside an independent fold:
// both fire.
const NONTAIL_RESULT_AS_FOLD_INIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  array::fold([i64:41], f(i64:256), |acc, x| x + i64:1);
  array::fold([i64:41], i64:0, |acc, x| x + i64:1)
}
"#;

run!(
    nontail_result_as_fold_init,
    NONTAIL_RESULT_AS_FOLD_INIT,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A rec lambda nested in another lambda's body tail-loops on both
// engines.
const NESTED_TAIL_LOOP: &str = r#"
{
  let f = |x: i64| -> i64 {
    let rec lp = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) };
    lp(i64:500, i64:0) + x
  };
  f(i64:1)
}
"#;

// Pins the tail loop's mode parity at depth 500.
run!(nested_tail_loop, NESTED_TAIL_LOOP, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(125251)))
}; graphix_package_core::testing::FuseExpect::Jit);

// An `-> i64` rtype annotation rejects an error-producing arm.
const RTYPE_REJECTS_ERROR_ARM: &str = r#"
{
  let countdown = |n: i64, acc| -> i64 select n {
    i64:0 => acc,
    _ => error(i64:0)
  };
  countdown(i64:100, i64:0)
}
"#;

run!(rtype_rejects_error_arm, RTYPE_REJECTS_ERROR_ARM, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A fn-valued element does not slip through a recursive-type HOF chain:
// `acc + <fn>` is rejected.
run!(
    list_map_fn_element_fold_rejected,
    |v: Result<&Value>| matches!(v, Err(_)),
    "/test.gx" => r#"
        list::fold(
            list::map(list::from_array([true]), |x| hold),
            i64:0,
            |acc, x| acc + x
        )
    "#;
    graphix_package_core::testing::FuseExpect::None
);

// Parens are transparent: `let rec f = (|n| …)` is the bare spelling.
run!(
    rec_through_parens,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0))),
    "/test.gx" => r#"
        let rec f = (|n: i64| -> i64 select n { i64:0 => i64:0, _ => f(n - i64:1) });
        let result = f(i64:3)
    "#;
    // ASPIRE: the parens spelling node-walks.
    graphix_package_core::testing::FuseExpect::None
);

// A generalized fn-valued argument's cells bind at callback
// finalization; the call site settles after, so the extracted callback
// typechecks like the inline one.
run!(
    extracted_callback_settles_after_finalize,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(3))),
    "/test.gx" => r#"
        let a = [(i64:0, i64:1), (i64:2, i64:3)];
        let g = |(k, v)| select k == i64:2 { true => v, false => null };
        let result = array::find_map(a, g)
    "#
);

const CALLSITE_REJECTS_NULLABLE_RETURN: &str = r#"
{
  let a = array::init(i64:3, |i| {
    let l = list::from_array([i64:0, i64:2, i64:3]);
    list::find(l, |x| x > i64:10)
  });
  array::fold(a, i64:0, |acc, x| acc + x)
}
"#;

run!(
    callsite_rejects_nullable_return,
    CALLSITE_REJECTS_NULLABLE_RETURN,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

const CALLSITE_REJECTS_HETEROGENEOUS_RETURN: &str = r#"
sync {
  let mut res = i64:0;
  for v in array::map([f64:23.5, i64:2, i64:3], |x| x * i64:2) {
    res = res / v
  };
  res
}
"#;

run!(
    callsite_rejects_heterogeneous_return,
    CALLSITE_REJECTS_HETEROGENEOUS_RETURN,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The `let rec` twin: recursion typing admits nothing the non-recursive
// form rejects.
const REC_RTYPE_REJECTS_ERROR_ARM: &str = r#"
{
  let rec countdown = |n: i64, acc| -> i64 select n {
    i64:0 => acc,
    _ => error(i64:0)
  };
  countdown(i64:100, i64:0)
}
"#;

run!(rec_rtype_rejects_error_arm, REC_RTYPE_REJECTS_ERROR_ARM, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// Monomorphic recursion: a self-call arg disagreeing with the entry
// call's narrowing is a def-time error.
const REC_SELFCALL_ARG_MISMATCH: &str = r#"
{
  let rec sum_to = |n, acc| select n {
    i64:0 => acc,
    _ => sum_to("hello", acc + n)
  };
  sum_to(i64:100, i64:0)
}
"#;

run!(rec_selfcall_arg_mismatch, REC_SELFCALL_ARG_MISMATCH, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// Two distinct unbound tvars in an arm union do not collapse; both
// bindings survive into the select's type (direct-return shape only).
const ARM_UNION_KEEPS_BOTH_TVARS: &str = r#"
{
  let pick = |which: bool, a, b| select which {
    true => a,
    false => b
  };
  pick(true, i64:1, "x")
}
"#;

run!(arm_union_keeps_both_tvars, ARM_UNION_KEEPS_BOTH_TVARS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; graphix_package_core::testing::FuseExpect::None);

// An unannotated variant-returning non-tail rec lambda infers its union.
const REC_VARIANT_UNION_INFERS: &str = r#"
{
  let rec f = |n: i64| select n {
    i64:0 => `A,
    _ => select f(n - i64:1) { `A => `B, `B => `A }
  };
  select f(i64:5) { `A => i64:1, `B => i64:2 }
}
"#;

run!(rec_variant_union_infers, REC_VARIANT_UNION_INFERS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A select over [`A, `B] missing the `B arm is a compile error.
const SELECT_VARIANT_NONEXHAUSTIVE: &str = r#"
{
  let x: [`A, `B] = `A;
  select x { `A => i64:1 }
}
"#;

run!(select_variant_nonexhaustive, SELECT_VARIANT_NONEXHAUSTIVE, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A tail-recursive `let rec` inside a HOF callback tail-loops on the
// node-walk (depth 500).
const REC_IN_HOF_CALLBACK: &str = r#"
{
  let a = array::init(i64:1, |x: i64| -> i64 {
    let rec lp = |n: i64, acc: i64| -> i64 select n {i64:0 => acc, _ => lp(n - i64:1, acc + n)};
    lp(i64:500, i64:0) + x
  });
  array::fold(a, i64:0, |acc, x| acc + x)
}
"#;

run!(rec_in_hof_callback, REC_IN_HOF_CALLBACK, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(125250))
); graphix_package_core::testing::FuseExpect::Jit);

// The split-callback twin: a catch in the same callback splits it and
// the rec runs in the node-walk residue; it still tail-loops.
const REC_IN_SPLIT_CALLBACK: &str = r#"
{
  let v0 = array::fold([i64:-1], i64:255, |acc, x| {
    let rec lp = |n: i64, a: i64| -> i64 select n {i64:0 => a, _ => lp(n - i64:1, a + n)};
    (lp(i64:500, i64:0) * i64:0) + { catch(e) i64:42; ((x /? i64:-1))? }
  });
  [i64:7 + v0]
}
"#;

run!(rec_in_split_callback, REC_IN_SPLIT_CALLBACK, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(8)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A recursion stays live to its captures: each cap fire recomputes.
// Collects [100, 101, 102].
const REC_TRANSIENT_CAPTURE_WAKE: &str = r#"
{
  let cap = 100;
  cap <- select cap { n if n < 102 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => cap, _ => f(n - i64:1)};
  array::group(f(i64:5), |n, _| n == 3)
}
"#;

run!(rec_transient_capture_wake, REC_TRANSIENT_CAPTURE_WAKE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(100), Value::I64(101), Value::I64(102)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// State inside a recursive function persists across fires: three
// levels of count step across three fires, sums [3, 6, 9].
const REC_TRANSIENT_STATEFUL_RETAINED: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, _ => count(n) + f(n - i64:1)};
  array::group(f(go ~ i64:3), |n, _| n == 3)
}
"#;

run!(rec_transient_stateful_retained, REC_TRANSIENT_STATEFUL_RETAINED, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(3), Value::I64(6), Value::I64(9)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Pure non-tail recursion re-fired with changing args recomputes:
// [fib(8), fib(9), fib(10)] = [21, 34, 55].
const REC_TRANSIENT_PURE_REFIRE: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, i64:1 => i64:1, _ => f(n - i64:1) + f(n - i64:2)};
  array::group(f(go + i64:8), |n, _| n == 3)
}
"#;

run!(rec_transient_pure_refire, REC_TRANSIENT_PURE_REFIRE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(21), Value::I64(34), Value::I64(55)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A recursion re-fired with the same argument value fires per delivery;
// `uniq` is the damp. count(f(10)) reaches 3.
const REC_SAME_ARG_REFIRE_FIRES: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, i64:1 => i64:1, _ => f(n - i64:1) + f(n - i64:2)};
  let c = count(f(go ~ i64:10));
  select count(go) { 3 => c, _ => never() }
}
"#;

run!(rec_same_arg_refire_fires, REC_SAME_ARG_REFIRE_FIRES, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Stateless builtins in a recursive body: f(4) = 7 per fire.
const REC_TRANSIENT_STATELESS_BUILTIN: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => str::len("abc"), _ => f(n - i64:1) + array::len([n])};
  array::group(f(go ~ i64:4), |n, _| n == 3)
}
"#;

run!(rec_transient_stateless_builtin, REC_TRANSIENT_STATELESS_BUILTIN, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(7), Value::I64(7), Value::I64(7)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Generic Graphix wrappers over compiler-owned collection nodes; a
// lambda param called inside the body resolves statically per callsite.

const INLANG_MAP: &str = r#"
{
  let m = |a: Array<'a>, f: fn(x: 'a) -> 'b| -> Array<'b>
    array::fold(a, [], |acc, v| array::push(acc, f(v)));
  (m([1, 2, 3], |x| x * 2), m(["a", "b"], |s| "[s]!"))
}
"#;

// The generic wrapper instantiates independently per call site; the
// fold callback captures the wrapper's fn formal.
run!(inlang_map, INLANG_MAP, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::Array(a), Value::Array(b)] => {
            matches!(&a[..], [Value::I64(2), Value::I64(4), Value::I64(6)])
                && matches!(&b[..], [Value::String(x), Value::String(y)]
                    if &**x == "a!" && &**y == "b!")
        }
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// `x + i64:1` under `-> 'a: Number` is ill-typed for an arbitrary 'a.
const PARAM_KNOT_NO_LEAK: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + i64:1;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(param_knot_no_leak, PARAM_KNOT_NO_LEAK, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

// A recursive callee whose self-call passes a fn-typed arg compiles
// (no infinite pre-materialization).
const REC_FN_ARG_COMPILES: &str = r#"
{
  let rec sum_to = |n, acc| select n {
    i64:0 => acc,
    _ => sum_to(n - i64:1, acc)
  };
  sum_to(i64:3, buffer::to_string)
}
"#;

run!(rec_fn_arg_compiles, REC_FN_ARG_COMPILES, |v: Result<&Value>| matches!(v, Ok(_));
     graphix_package_core::testing::FuseExpect::None);

const MUTUAL_RECURSIVE_STATIC_CALLS: &str = r#"
{
  let rec even = |n: i64| -> bool {
    let odd = |m: i64| -> bool select m {
      i64:0 => false,
      _ => even(m - i64:1)
    };
    select n {
      i64:0 => true,
      _ => odd(n - i64:1)
    }
  };
  even(i64:10)
}
"#;

run!(
    mutual_recursive_static_calls,
    MUTUAL_RECURSIVE_STATIC_CALLS,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)));
    graphix_package_core::testing::FuseExpect::None
);

// A tail-jump arg that bottoms every pass rides its previous value: the
// loop keeps acc=0 and reaches the base.
const TAIL_ARG_BOTTOM_RIDES_CACHE: &str = r#"
{
    let rec f = |n: i64, acc: i64| -> f64
        select n {
            0 => 0.0,
            _ => f(n - 1, str::parse("nan")? + n)
        };
    f(3, 0)
}
"#;

run!(
    tail_arg_bottom_rides_cache,
    TAIL_ARG_BOTTOM_RIDES_CACHE,
    |v: Result<&Value>| { matches!(v, Ok(Value::F64(x)) if *x == 0.0) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A bare-Array arg under a `[Array<i64>, null]` signature slot marshals
// as a Value.
const CALL_ARG_VALUE_SLOT_NARROW: &str = r#"
{
    let f = |v: [Array<i64>, null]| v;
    f([1, 2])
}
"#;

run!(call_arg_value_slot_narrow, CALL_ARG_VALUE_SLOT_NARROW, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(1), Value::I64(2)]),
        _ => false,
    }
});

// The result twin: a call result whose node type promises a Value is
// widened from the callee's raw array box.
const CALL_RESULT_VALUE_WIDEN_XKERNEL: &str = r#"
{
    let g = || [1];
    let f = |v: [null, Array<i64>]| v;
    f(g())
}
"#;

run!(call_result_value_widen_xkernel, CALL_RESULT_VALUE_WIDEN_XKERNEL, |v: Result<
    &Value,
>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(1)]),
        _ => false,
    }
});

const CALL_RESULT_VALUE_WIDEN_HOF: &str = r#"
{
    let f = |v: [null, Array<i64>]| v;
    f(array::map([1, 2], |x| x + 1))
}
"#;

run!(call_result_value_widen_hof, CALL_RESULT_VALUE_WIDEN_HOF, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(2), Value::I64(3)]),
        _ => false,
    }
});

// String args and returns marshal cross-kernel.
const XKERNEL_STRING_ARG_RET: &str = r#"
{
    let f = |s: string| "[s]!";
    f("hello")
}
"#;

run!(xkernel_string_arg_ret, XKERNEL_STRING_ARG_RET, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s.as_str() == "hello!",
        _ => false,
    }
});

// A string-returning callee feeding a union-typed param.
const XKERNEL_STRING_WIDEN: &str = r#"
{
    let g = || "abc";
    let f = |v: [null, string]| v;
    f(g())
}
"#;

run!(xkernel_string_widen, XKERNEL_STRING_WIDEN, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s.as_str() == "abc",
        _ => false,
    }
});

const EXCESS_POSITIONAL_REJECTED: &str = r#"
{
  type T = {bar: i64, foo: i64};
  let f = |#foo: i64 = i64:42, #bar: i64 = i64:42| -> T { bar: i64:0, foo: i64:0 };
  f(i64:5)
}
"#;

run!(
    excess_positional_rejected,
    EXCESS_POSITIONAL_REJECTED,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

const DYNCALL_SITE_IDENTITY_STATE: &str = r#"
{
  let f0 = |v: f64| -> f64 mean(v)$;
  f0(f0(10.0) + 10.0)
}
"#;

// Two call sites of one callee own separate builtin instances: `mean`
// at the outer site sees 20, not mean(10, 20).
run!(dyncall_site_identity_state, DYNCALL_SITE_IDENTITY_STATE, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(x)) => *x == 20.0,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

const DYNCALL_SEED_BACKEDGE: &str = r#"
{
  let g = |s: string| -> i64 str::len(s);
  let rec f = |n: i64| -> i64 select n {
    x if x <= i64:0 => g("a"),
    x => x + f(x - i64:1)
  };
  g("bb") + f(i64:2)
}
"#;

// A pre-bound builtin slot dispatched both from the root and from a
// recursive activation.
run!(dyncall_seed_backedge, DYNCALL_SEED_BACKEDGE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// A List reaching arithmetic is a compile error, even when the call
// elaborates per call site with the List still abstract.
const ARITH_REJECTS_ABSTRACT_OPERAND: &str = r#"
{
  let f = |x| {
    let s = |n, acc| select n {
      i64:0 => acc,
      _ => list::from_array([x])
    };
    s(i64:100, i64:20)
  };
  let v = f("foo");
  v + i64:1
}
"#;

run!(
    arith_rejects_abstract_operand,
    ARITH_REJECTS_ABSTRACT_OPERAND,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The counterpart: the same union without arithmetic compiles.
const ABSTRACT_UNION_RETURN_IS_FINE: &str = r#"
{
  let f = |x| {
    let s = |n, acc| select n {
      i64:0 => acc,
      _ => list::from_array([x])
    };
    s(i64:100, i64:20)
  };
  f("foo")
}
"#;

run!(abstract_union_return_is_fine, ABSTRACT_UNION_RETURN_IS_FINE, |v: Result<
    &Value,
>| matches!(v, Ok(_)));

// A declared return type must be proven when the body reaches it
// through a call whose callee return is still open: `-> i64` over an
// `Array<'n>` is rejected.
const DECLARED_RTYPE_PROVEN_THROUGH_OPEN_CALLEE: &str = r#"
{
  let g = |#x: i64| -> i64 {
    let s = |n| [n];
    s(x)
  };
  g(#x: i64:4)
}
"#;

run!(
    declared_rtype_proven_through_open_callee,
    DECLARED_RTYPE_PROVEN_THROUGH_OPEN_CALLEE,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The counterpart: an honest declared type propagates inward to select
// the generic callee's instance.
const DECLARED_RTYPE_DRIVES_OPEN_CALLEE: &str = r#"
{
  let g = |#x: i64| -> Array<i64> {
    let s = |n| [n];
    s(x)
  };
  g(#x: i64:4)
}
"#;

run!(
    declared_rtype_drives_open_callee,
    DECLARED_RTYPE_DRIVES_OPEN_CALLEE,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if &**a == [Value::I64(4)])
);

// The obligation is per instance: two sites of one open callee at
// different element types both compile.
const OPEN_CALLEE_OBLIGATION_IS_PER_INSTANCE: &str = r#"
{
  let s = |n| [n];
  let a: Array<i64> = s(i64:1);
  let b: Array<string> = s("two");
  (a, b)
}
"#;

run!(
    open_callee_obligation_is_per_instance,
    OPEN_CALLEE_OBLIGATION_IS_PER_INSTANCE,
    |v: Result<&Value>| matches!(v, Ok(_))
);

// A collection callback's element goes to its positional parameter;
// labeled parameters take their defaults (the callback interprets).
const LABELED_CALLBACK_DEFAULT: &str =
    r#"array::map([i64:7], |#foo: i64 = i64:42, x| foo + x)"#;
run!(labeled_callback_default, LABELED_CALLBACK_DEFAULT, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(49)] => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// A callback with only labeled parameters has no slot for the element:
// a type error.
#[tokio::test]
async fn labeled_only_callback_is_compile_error() {
    let r =
        eval("array::map([i64:7], |#foo: i64 = i64:42| foo)", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "a labeled-only callback must not satisfy fn(x: 'a) -> 'b, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A HOF callback calling a lambda whose return cell is still open (a
// trailing-`;` block) typechecks.
const OPEN_RETURN_CALLEE_IN_CALLBACK: &str = r#"
{
  let f = |a: string| { print(a); };
  let r = array::init(4, |i| f("x-[i]"));
  i64:42
}
"#;

run!(open_return_callee_in_callback, OPEN_RETURN_CALLEE_IN_CALLBACK, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::I64(42))
));

// A formal every self-call passes through unchanged is never rebound,
// so an fn-typed invariant formal does not gate the tail loop.

const FN_INVARIANT_TAIL_LOOP: &str = r#"
{
  let rec fold_go = |f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64
    select i {
      0 => acc,
      _ => fold_go(f, i - 1, f(acc, i))
    };
  fold_go(|a, x| a + x, 10, 0)
}
"#;

run!(fn_invariant_tail_loop, FN_INVARIANT_TAIL_LOOP, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(55))
); graphix_package_core::testing::FuseExpect::Jit);

// An invariant String formal is a kernel slot that is never rebound.
const STRING_INVARIANT_TAIL_LOOP: &str = r#"
{
  let rec label = |tag: string, n: i64, acc: i64| -> string
    select n {
      0 => "[tag]:[acc]",
      _ => label(tag, n - 1, acc + n)
    };
  label("sum", 10, 0)
}
"#;

run!(string_invariant_tail_loop, STRING_INVARIANT_TAIL_LOOP, |v: Result<&Value>| match v
{
    Ok(Value::String(s)) => &**s == "sum:55",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Two call sites, one recursive helper, two different callbacks of
// identical type key two kernels (3008, not 3003 or 8008).
const FN_FORMAL_TWO_CALLBACKS: &str = r#"
{
  let rec ap = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n {
      0 => acc,
      _ => ap(f, n - 1, f(acc))
    };
  ap(|x| x + 1, 3, 0) * 1000 + ap(|x| x * 2, 3, 1)
}
"#;

run!(fn_formal_two_callbacks, FN_FORMAL_TWO_CALLBACKS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3008))
); graphix_package_core::testing::FuseExpect::Jit);

// A non-invariant fn formal (rebound by a self-call) does not fuse;
// the engines agree.
const FN_FORMAL_REBOUND: &str = r#"
{
  let rec g = |f: fn(x: i64) -> i64, n: i64| -> i64
    select n {
      0 => f(0),
      _ => g(|x| x + 100, n - 1)
    };
  g(|x| x + 1, 0) * 1000 + g(|x| x + 1, 2)
}
"#;

run!(fn_formal_rebound, FN_FORMAL_REBOUND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1100))
); graphix_package_core::testing::FuseExpect::None);

// A helper forwarding its fn formal to another helper: the two
// forwarding instances key two kernels (110 vs 102 in the low part).
const FN_FORMAL_FORWARDED: &str = r#"
{
  let call1 = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x);
  let call2 = |f: fn(x: i64) -> i64, x: i64| -> i64 call1(f, x) + 100;
  call2(|x| x + 1, 1) * 1000 + call2(|x| x * 10, 1)
}
"#;

run!(fn_formal_forwarded, FN_FORMAL_FORWARDED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(102110))
); graphix_package_core::testing::FuseExpect::Jit);

// A forwarded callback capturing an outer binding spelled like the
// callee's formal `n`: the tail rebind targets the formal by BindId.
const FN_FORMAL_CAPTURE_COLLIDES_BOUND: &str = r#"
{
  let n = 10;
  let rec ap = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => ap(f, n - 1, f(acc)) };
  ap(|x| n + 1, 3, 0)
}
"#;

run!(fn_formal_capture_collides_bound, FN_FORMAL_CAPTURE_COLLIDES_BOUND,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(11)));
    graphix_package_core::testing::FuseExpect::Jit);

// The same collision on the accumulator formal `acc`.
const FN_FORMAL_CAPTURE_COLLIDES_ACC: &str = r#"
{
  let acc = 5;
  let rec fold_go = |f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64
    select i { 0 => acc, _ => fold_go(f, i - 1, f(acc, i)) };
  fold_go(|a, x| a + acc + 1, 3, 0)
}
"#;

run!(fn_formal_capture_collides_acc, FN_FORMAL_CAPTURE_COLLIDES_ACC,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(18)));
    graphix_package_core::testing::FuseExpect::Jit);

// Instantiation identity keys on the callback's source lambda, so a CPS
// wrapper recursion knots at level two instead of instantiating forever.
const CPS_WRAPPER_RECURSION: &str = r#"
{
  let rec f = |n: i64, g: fn(y: i64) -> i64| -> i64 select n {
    i64:0 => g(i64:0),
    _ => f(n - i64:1, |y| g(y + i64:1))
  };
  f(i64:3, |x| x)
}
"#;

run!(cps_wrapper_recursion, CPS_WRAPPER_RECURSION, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3))
); graphix_package_core::testing::FuseExpect::None);

// An instantiation snapshots its def's LambdaIds: calls to a returned
// lambda resolve statically (the fn-valued `let` node-walks).
const RETURNED_LAMBDA_RESOLVES: &str = r#"
{
  let mk = |x: i64| |y: i64| x + y;
  let add1 = mk(i64:1);
  add1(i64:2) + mk(i64:10)(i64:20)
}
"#;

run!(returned_lambda_resolves, RETURNED_LAMBDA_RESOLVES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(33))
); graphix_package_core::testing::FuseExpect::None);

// Loop-carried non-register formals: the tail rebind carries every
// kernel param kind.

const STRING_CARRIED_TAIL_LOOP: &str = r#"
{
  let rec go = |n: i64, acc: string| -> string
    select n { 0 => acc, _ => go(n - 1, "[acc].") };
  go(5, "x")
}
"#;

run!(string_carried_tail_loop, STRING_CARRIED_TAIL_LOOP, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "x.....",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const VALUE_CARRIED_TAIL_LOOP: &str = r#"
{
  let rec go = |n: i64, m: Map<string, i64>| -> Map<string, i64>
    select n { 0 => m, _ => go(n - 1, map::insert(m, "k[n]", n)) };
  map::len(go(3, {}))
}
"#;

run!(value_carried_tail_loop, VALUE_CARRIED_TAIL_LOOP, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3))
); graphix_package_core::testing::FuseExpect::Jit);

// A List tail bound by a non-scalar payload pattern is carried through
// the tail loop.
const LIST_CARRIED_FOLD: &str = r#"
{
  type L<'a> = [`C('a, L<'a>), `N];
  let rec fold_l = |l: L<i64>, acc: i64| -> i64
    select l { `N => acc, `C(x, rest) => fold_l(rest, acc + x) };
  fold_l(`C(1, `C(2, `C(3, `N))), 0)
}
"#;

run!(list_carried_fold, LIST_CARRIED_FOLD, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); graphix_package_core::testing::FuseExpect::Jit);
