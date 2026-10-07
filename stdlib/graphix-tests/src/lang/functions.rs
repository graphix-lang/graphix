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

run!(first_class_lambdas, FIRST_CLASS_LAMBDAS, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A formal with its own quantifier is rank-2: each call of it copies 'a,
// so the body may apply it at i64.
const RANK2_FORMAL_APPLIED_AT_A_NUMBER: &str = r#"
{
  let g = |f: fn<'a: Number>(x: 'a) -> 'a, y| f(y) + 1;
  g(|x| x, 1)
}
"#;

run!(rank2_formal_applied_at_a_number, RANK2_FORMAL_APPLIED_AT_A_NUMBER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
});

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
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  f(#foo: 0)
}
"#;

run!(labeled_args, LABELED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const REQUIRED_ARGS: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  f(#bar: 0)
}
"#;

run!(required_args, REQUIRED_ARGS, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const MIXED_ARGS: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42, baz| foo + bar + baz;
  f(#foo: 0, 0)
}
"#;

run!(mixed_args, MIXED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ARG_SUBTYPING: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  let g = |f: fn(#foo: i64) -> i64| f(#foo: 3);
  g(f)
}
"#;

run!(arg_subtyping, ARG_SUBTYPING, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ARG_NAME_SHORT: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  let foo = 3;
  f(#foo)
}
"#;

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

// Every depth of a recursion is its own activation, so each iteration
// owns its `count`.
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

// The same loop over `+` alone: a stateless body.
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
); graphix_package_core::testing::FuseExpect::Jit);

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
    #[native] f(3, 4)
}
"#;

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
    #[native] countdown(100, 0)
}
"#;

run!(fused_tail_loop, KIR_FUSED_TAIL_LOOP, |v: Result<&Value>| match v {
    Ok(Value::I64(5050)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Deep tail recursion runs in constant stack as a native loop. The
// node-walk recurses, an activation per level.
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
}; graphix_package_core::testing::FuseExpect::Jit; jit_only);

// A depth kept from an earlier cycle catches up what it missed: `y`
// fired while depth 1 took arm `0`, so its move to arm `1` writes `y`.
const TAIL_DEPTH_CATCHES_UP_AN_OUTER_WRITE: &str = r#"
{
    let t = array::iter([1, 2]);
    let y = uniq(select t { _ => 10 });
    let n = select t { 1 => 2, _ => 3 };
    let x = 0;
    let rec f = |n| select n { 0 => 0, 1 => { x <- y; 1 }, n => f(n - 2) };
    select (f(n), x) { (_, 10) => true, _ => never() }
}
"#;

run!(tail_depth_catches_up_an_outer_write, TAIL_DEPTH_CATCHES_UP_AN_OUTER_WRITE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Bool(true))
), timeout: 5; graphix_package_core::testing::FuseExpect::Jit);

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
    #[native] iterate(0.0, 0.0, 1.0, 0.0, 10)
}
"#;

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

// A fold whose init is bottom still dispatches a callback that never
// reads acc: the fold yields 7.
const FOLD_BOTTOM_INIT_UNREAD_ACC: &str = r#"
{
  let b = i64:1 / i64:0;
  any(array::fold([i64:5, i64:7], b, |acc, x| x), i64:-1)
}
"#;

run!(fold_bottom_init_unread_acc, FOLD_BOTTOM_INIT_UNREAD_ACC, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A bottom beside a collection operation in a composite bottoms the
// composite, not the block: the tail still fires.
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
  any(r, i64:-1)
}
"#;

run!(find_bottom_after_match, FIND_BOTTOM_AFTER_MATCH, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(-1))
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
  any(r, i64:-1)
}
"#;

run!(
    fold_tainted_init_consumed_bottoms,
    FOLD_TAINTED_INIT_CONSUMED_BOTTOMS,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(-1)))
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

// Each slot of a growing map calls `g` as an instance of its own, whose
// first call is an init view: its constant error raises once per new
// slot, three by the cycle the fourth slot arrives.
const HOF_SLOT_CALLEE_FIRST_CALL: &str = r#"
{
  let n = array::iter([1, 2, 3, 4]);
  let raised = 0;
  let out = {
    catch(e) raised <- e ~ raised + 1;
    let g = |x: i64| -> i64 {
      let e: [i64, Error<`E>] = error(`E);
      e? + x
    };
    array::map(array::init(n, |i| i), |x| g(x))
  };
  select count(n) { 4 => raised, _ => never() }
}
"#;

run!(hof_slot_callee_first_call, HOF_SLOT_CALLEE_FIRST_CALL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A nested map over a source outside the loop is an instance per outer
// slot: a new slot's inner map fires on its first observation of `ys`,
// and the error derived from it raises once per new slot.
const HOF_SLOT_NESTED_PREV_LEN: &str = r#"
{
  let n = array::iter([1, 2, 3, 4]);
  let raised = 0;
  let out = {
    catch(e) raised <- e ~ raised + 1;
    let ys = [1, 2, 3];
    array::map(array::init(n, |i| i), |x| {
      let k = array::len(array::map(ys, |y| y * 2));
      let e: [i64, Error<`E>] = select k { 0 => 0, _ => error(`E) };
      e? + x
    })
  };
  select count(n) { 4 => raised, _ => never() }
}
"#;

run!(hof_slot_nested_prev_len, HOF_SLOT_NESTED_PREV_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A new slot is a new instance, whose first update is an init view: a
// constant in the callback body fires in each new slot, and its error
// raises once per slot.
const HOF_SLOT_CONSTANT_FIRES: &str = r#"
{
  let n = array::iter([1, 2, 3, 4]);
  let raised = 0;
  let out = {
    catch(e) raised <- e ~ raised + 1;
    array::map(array::init(n, |i| i), |x| {
      let e: [i64, Error<`E>] = error(`E);
      e? + x
    })
  };
  select count(n) { 4 => raised, _ => never() }
}
"#;

run!(hof_slot_constant_fires, HOF_SLOT_CONSTANT_FIRES, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
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

// A non-tail recursion's result as a fold's init: the fold fires.
const NONTAIL_RESULT_AS_FOLD_INIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  any(array::fold([i64:41], f(i64:256), |acc, x| x + i64:1), i64:-1)
}
"#;

run!(
    nontail_result_as_fold_init,
    NONTAIL_RESULT_AS_FOLD_INIT,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A tail-recursive `let rec` nested in another lambda's body.
const NESTED_TAIL_LOOP: &str = r#"
{
  let f = |x: i64| -> i64 {
    let rec lp = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) };
    lp(i64:500, i64:0) + x
  };
  f(i64:1)
}
"#;

// Mode parity at depth 500.
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
    graphix_package_core::testing::FuseExpect::Jit
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

// Arithmetic takes one numeric type: a nullable element, or a union of
// two numeric types, is refused at the operator.
const OPERAND_REFUSES_NULLABLE_ELEMENT: &str = r#"
{
  let a = array::init(i64:3, |i| {
    let l = list::from_array([i64:0, i64:2, i64:3]);
    list::find(l, |x| x > i64:10)
  });
  array::fold(a, i64:0, |acc, x| acc + x)
}
"#;

run!(
    operand_refuses_nullable_element,
    OPERAND_REFUSES_NULLABLE_ELEMENT,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("both operands must be one numeric type"));
    graphix_package_core::testing::FuseExpect::None
);

const OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT: &str = r#"
array::fold(array::map([f64:23.5, i64:2, i64:3], |x| x * i64:2), i64:0, |res, v| res / v)
"#;

run!(
    operand_refuses_mixed_numeric_element,
    OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("both operands must be one numeric type"));
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
}; graphix_package_core::testing::FuseExpect::Jit);

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

// A tail-recursive `let rec` inside a HOF callback (depth 500).
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
// the rec runs in the node-walk residue.
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

// A tail-call argument that is bottom on every pass does not bottom a
// base arm that never reads it: the result is the base's 0.0.
const TAIL_ARG_BOTTOM_UNREAD_BY_BASE: &str = r#"
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
    tail_arg_bottom_unread_by_base,
    TAIL_ARG_BOTTOM_UNREAD_BY_BASE,
    |v: Result<&Value>| { matches!(v, Ok(Value::F64(x)) if *x == 0.0) };
    graphix_package_core::testing::FuseExpect::Jit
);

// The consuming twin: a base arm that reads the bottomed argument is
// bottom, it does not ride an earlier value.
const TAIL_ARG_BOTTOM_READ_BY_BASE: &str = r#"
{
    let rec f = |n: i64, acc: i64| -> i64
        select n {
            0 => acc,
            _ => f(n - 1, (i64:1 / i64:0) + n)
        };
    any(f(3, 0), i64:-1)
}
"#;

run!(
    tail_arg_bottom_read_by_base,
    TAIL_ARG_BOTTOM_READ_BY_BASE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(-1))) };
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
); graphix_package_core::testing::FuseExpect::Jit);

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

// A dynamic callee that became null makes the call bottom; the bound
// instance is not invoked in its place.
const NULL_CALLEE_IS_BOTTOM: &str = r#"
{
  let f: [fn(x: i64) -> i64, null] = |x| x + 1;
  f <- null;
  let late = (f$)(sys::time::timer(duration:0.02s, false) ~ 10);
  any(late, sys::time::timer(duration:0.1s, false) ~ -1)
}
"#;

run!(null_callee_is_bottom, NULL_CALLEE_IS_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
}; graphix_package_core::testing::FuseExpect::None);

// A bottom argument to a tail self-call bottoms the formal, as it does
// for a non-tail call: the loop does not keep the previous value. `obs`
// is written only by a value, so it stays null while the result is
// bottom.
// CR claude for claude: [risk] This fixture orders its events with wall-clock timers and
// passes only while t1 (30 ms) lands at least three cycles before t2 (60 ms). The
// runtime puts every completed timer into one cycle (graphix-rt/src/gx.rs:1029). So if
// the runtime thread stalls across the 30 ms gap (a loaded parallel run; par and
// jit_par also wait on the shared eval pool at every fork), `t2 ~ obs` reads obs before
// it is written and the test fails with [null, null]. select_sibling_binds_spent and
// let_sibling_binds_spent (select.rs:1885, 2002) depend on a 100 ms gap the same way
// and fail with (1, 0, 1). Drive the events with a step counter, as
// arm_sampled_write_keeps_trigger does; rewritten that way, all three give the expected
// values in all four modes. probe: design/review-2026-10-05/repro/tests-lang-a-02.sh
// (freezes the runtime with SIGSTOP for 35 ms or 120 ms across the gap).
// (tests-lang-a-02)
const TAIL_REBIND_CARRIES_BOTTOM: &str = r#"
{
  let rec f = |n: i64, x: i64, k: i64| -> i64 select n {
    0 => x,
    _ => f(n - 1, select n { m if m == k => null$, _ => x }, k)
  };
  let k = 2;
  let t1 = sys::time::timer(duration:0.03s, false);
  k <- t1 ~ 9;
  let r = f(3, 7, k);
  let obs: [i64, null] = null;
  obs <- r;
  let t0 = sys::time::timer(duration:0.015s, false);
  let t2 = sys::time::timer(duration:0.06s, false);
  (t0 ~ obs, t2 ~ obs)
}
"#;

run!(tail_rebind_carries_bottom, TAIL_REBIND_CARRIES_BOTTOM, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[null, i64:7]"
}; graphix_package_core::testing::FuseExpect::Jit);

// An omitted default is checked against its own parameter, found by
// name, when the declared type lists the labels in another order.
const DEFAULT_CHECKED_BY_NAME: &str = r#"
{
  let f: fn(?#b: i64, ?#a: string, x: i64) -> string =
    |#a: string = "s", #b: i64 = 2, x: i64| "[a] [b] [x]";
  f(1)
}
"#;

run!(default_checked_by_name, DEFAULT_CHECKED_BY_NAME, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "s 2 1")
}; graphix_package_core::testing::FuseExpect::Jit);

// A fn-typed parameter after a labeled one resolves to its own
// argument, not the next positional one.
const FN_PARAM_AFTER_LABELED: &str = r#"
{
  let apply2 = |#k = 0, f: fn(x: i64) -> i64, g: fn(x: i64) -> i64| f(k) + g(k) * 100;
  apply2(|x| x + 1, |x| x + 2)
}
"#;

run!(fn_param_after_labeled, FN_PARAM_AFTER_LABELED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(201)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A call's own refusals are placed at the call.
const DUPLICATE_LABEL_PLACED: &str = r#"
{
  let f = |#a: i64, y: i64| a + y;
  f(#a: 1, #a: 2, 3)
}
"#;

run!(duplicate_label_placed, DUPLICATE_LABEL_PLACED, |v: Result<&Value>| {
    matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("duplicate argument #a") && e.contains("in: f(#a: 1, #a: 2, 3)")
    })
}; graphix_package_core::testing::FuseExpect::None);

const DEAD_VARIADIC_PLACED: &str = r#"
str::concat()
"#;

run!(dead_variadic_placed, DEAD_VARIADIC_PLACED, |v: Result<&Value>| {
    matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("never fires") && e.contains("in: str::concat()")
    })
}; graphix_package_core::testing::FuseExpect::None);

// A function that requires `#a` cannot stand where `#a` may be omitted,
// so a connect that would leave a call without it is refused.
const REQUIRED_LABEL_CONNECT: &str = r#"
{
  let f: fn(?#a: i64, x: i64) -> i64 = |#a: i64 = 1, x: i64| a + x;
  let g = |#a: i64, x: i64| a * x;
  f <- sys::time::timer(duration:0.02s, false) ~ g;
  f(10)
}
"#;

run!(required_label_connect_refused, REQUIRED_LABEL_CONNECT, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A quiet bottom argument inside a tail recursion is bottom to the
// callee: after `g(5)` ran and `x` went bottom, `f(2, x)` is bottom, as
// the inline body `x + 1` is.
const TAIL_QUIET_BOTTOM_CALL: &str = r#"{
  let t = array::iter([0, 1, 2, 3, 4]);
  let g = |y| y + 1;
  let rec f = |n, x| select n { 0 => g(x), n => f(n - 1, x) };
  let n = uniq(select t { 0 => 0, 1 => 1, 2 => 1, _ => 2 });
  let b = uniq(select t { 0 => 1, 1 => 1, _ => 0 });
  f(n, 5 / b)
}"#;

const TAIL_QUIET_BOTTOM_INLINE: &str = r#"{
  let t = array::iter([0, 1, 2, 3, 4]);
  let rec f = |n, x| select n { 0 => x + 1, n => f(n - 1, x) };
  let n = uniq(select t { 0 => 0, 1 => 1, 2 => 1, _ => 2 });
  let b = uniq(select t { 0 => 1, 1 => 1, _ => 0 });
  f(n, 5 / b)
}"#;

async fn tail_quiet_bottom(code: &str, fusion_disabled: bool) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(code, fusion_disabled).await?;
    assert_eq!(super::dense_deltas::as_i64s(&values), vec![6, 6]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn tail_quiet_bottom_call_interp() -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_CALL, true).await
}

#[tokio::test(flavor = "current_thread")]
async fn tail_quiet_bottom_call_jit() -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_CALL, false).await
}

#[tokio::test(flavor = "current_thread")]
async fn tail_quiet_bottom_inline_interp() -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_INLINE, true).await
}

#[tokio::test(flavor = "current_thread")]
async fn tail_quiet_bottom_inline_jit() -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_INLINE, false).await
}

// A `let rec` annotation reads a trait parameter as a bounded
// quantifier, as a plain `let` does.
const REC_ANNOTATION_TRAIT_PARAM: &str = r#"
{
  let rec f: fn(x: Display) -> string = |x| "a";
  f(1)
}
"#;

run!(rec_annotation_trait_param, REC_ANNOTATION_TRAIT_PARAM, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a")
}; graphix_package_core::testing::FuseExpect::Jit);

// A `let` that shadows a builtin binding is not a builtin.
const BUILTIN_BINDING_SHADOWED: &str = r#"
{
  let f = |@args: [Number, Array<[Number, Array<Number>]>]| -> Number 'core_sum;
  let f = |@args: i64| -> i64 42;
  f()
}
"#;

run!(builtin_binding_shadowed, BUILTIN_BINDING_SHADOWED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A type variable only data arguments hold settles to the widest
// argument, whatever the order.
const WIDEST_ARG_EITHER_ORDER: &str = r#"
{
  let n: [u64, null] = null;
  let f = |x: 'a, y: 'a| -> 'a y;
  (f(u64:0, n), f(n, u64:0))
}
"#;

run!(widest_arg_either_order, WIDEST_ARG_EITHER_ORDER, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[null, u64:0]"
});

const LIST_CONS_WIDER_TAIL: &str = r#"
{
  let t: List<[u64, null]> = [<u64:1, null>];
  list::cons(u64:0, t)
}
"#;

run!(list_cons_wider_tail, LIST_CONS_WIDER_TAIL, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[u64:0, [u64:1, [null, []]]]"
});

// Two arguments neither of which holds the other wait for a later
// argument that holds both.
const WIDEST_ARG_LAST: &str = r#"
{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a y;
  let ab: [`A, `B] = `A;
  f(`A, `B, ab)
}
"#;

run!(widest_arg_last, WIDEST_ARG_LAST, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "\"B\""
});

// A lambda in parentheses is a lambda: its binding generalizes.
run!(
    parenthesized_lambda_generalizes,
    r#"{ let f = (|v| v * v); (array::map([u8:3], f), array::map([4], f)) }"#,
    |v: Result<&Value>| format!("{}", v.unwrap()) == "[[u8:9], [i64:16]]"
);

// A call's refusal is placed at the call, in parentheses or not.
#[tokio::test(flavor = "current_thread")]
async fn a_call_error_is_placed_at_the_call() -> Result<()> {
    use graphix_compiler::expr::ErrorSite;
    let src = "{ let f = |x: i64| x; (f(#y: 1, 2)) }";
    let e = match eval(src, crate::TEST_REGISTER).await {
        Err(e) => e,
        Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
    };
    let at = e.downcast_ref::<ErrorSite>().map(|s| s.expr().to_string());
    assert_eq!(at.as_deref(), Some("f(#y: 1, 2)"), "{e:#}");
    Ok(())
}

// A wider argument after the one that settled the variable is checked
// as written, whatever its shape.
run!(
    widest_arg_literal,
    r#"{ let a = [1]; array::concat(a, [4, null]) }"#,
    |v: Result<&Value>| format!("{}", v.unwrap()) == "[i64:1, i64:4, null]"
);

// A generalized callback's result is at most its inferred shape: an
// annotation may widen it, never narrow it.
run!(
    generalized_callback_result_widens,
    r#"{ let f = |a| (a, u8:100); let v: Array<(i64, [u8, null])> = array::map([1], f); v }"#,
    |v: Result<&Value>| format!("{}", v.unwrap()) == "[[i64:1, u8:100]]"
);

run!(
    generalized_callback_result_does_not_narrow,
    r#"{ let f = |a| (a, null); let v: Array<(i64, u8)> = array::map([1], f); v }"#,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain"));
    graphix_package_core::testing::FuseExpect::None
);

// No argument holds the others: refused, in either order.
run!(
    no_widest_arg_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; (f(`A, `B), f(`B, `A)) }"#,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain"));
    graphix_package_core::testing::FuseExpect::None
);

// The call's type is the widest argument's.
run!(
    widest_arg_types_the_result,
    r#"{ let n: [u64, null] = null; let f = |x: 'a, y: 'a| -> 'a y; let r: u64 = f(u64:0, n); r }"#,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain"));
    graphix_package_core::testing::FuseExpect::None
);

// A variable a callback holds does not widen: the callback was checked
// at the type the variable had when it was reached.
run!(
    callback_variable_does_not_widen,
    r#"{
  let n: [u64, null] = null;
  let g = |x: 'a, f: fn(v: 'a) -> 'a, y: 'a| -> 'a f(y);
  g(u64:0, |v| v, n)
}"#,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain"));
    graphix_package_core::testing::FuseExpect::None
);

// Nor one a reference holds: the callee could write the wider type
// through it.
run!(
    reference_variable_does_not_widen,
    r#"{
  let r: &u64 = &u64:0;
  let n: &[u64, null] = &null;
  let f = |x: &'a, y: &'a| -> &'a y;
  f(r, n)
}"#,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain"));
    graphix_package_core::testing::FuseExpect::None
);

// A generalized function's reference is its own instance from compile
// on: a call's argument types reach it through parens or a block
// before it typechecks, and must not bind the definition's cells.
const WRAPPED_POLY_REF_TWICE: &str = r#"
{
  let a = filter((array::flat_map), |x| true);
  let b = filter({ let z = 1; array::flat_map }, |x| true);
  let c = filter((array::flat_map), |x| true);
  (a([1], |x| [x, x]), b(["s"], |x| [x]), c([2], |x| [x]))
}
"#;

run!(wrapped_poly_ref_twice, WRAPPED_POLY_REF_TWICE, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == r#"[[i64:1, i64:1], ["s"], [i64:2]]"#
});

const FORWARDED_POLY_REF_TWICE: &str = r#"
{
  let t = array::flat_map;
  let a = filter((t), |x| true);
  let b = filter({ let z = 1; t }, |x| true);
  (a([1], |x| [x, x]), b(["s"], |x| [x]))
}
"#;

run!(forwarded_poly_ref_twice, FORWARDED_POLY_REF_TWICE, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == r#"[[i64:1, i64:1], ["s"]]"#
});

// `flat_map`'s callback returns the collection, never a bare element:
// whether to splice is never decided by a value's shape.
const FLAT_MAP_DECLARED_UNION: &str = r#"
{
  let t: fn(a: Array<i64>, f: fn(x: i64) -> [i64, Array<i64>]) -> Array<i64> = array::flat_map;
  t([1, 2], |x| select x { 1 => x, n => [n, n] })
}
"#;

run!(flat_map_bare_element_refused, FLAT_MAP_DECLARED_UNION, |v: Result<&Value>| {
    matches!(&v, Err(e) if format!("{e:#}").contains("does not contain"))
}; graphix_package_core::testing::FuseExpect::None);

// A tuple callback result is spliced as an array would never be: it is
// refused, and the result is the arrays' concatenation.
const FLAT_MAP_TUPLE_REFUSED: &str = r#"
{
  let r = array::flat_map([1, 2], |x| (x, x * 10));
  r
}
"#;

run!(flat_map_tuple_refused, FLAT_MAP_TUPLE_REFUSED, |v: Result<&Value>| {
    matches!(&v, Err(e) if format!("{e:#}").contains("does not contain"))
}; graphix_package_core::testing::FuseExpect::None);

// A binding that is not generalized holds one instance: used twice, the
// second use meets cells the first linked.
const MONOMORPHIC_FLAT_MAP_TWICE: &str = r#"
{
  let t = { let z = 1; array::flat_map };
  let a = filter(t, |x| true);
  let b = filter(t, |x| true);
  (a([1], |x| [x, x]), b([2], |x| [x]))
}
"#;

run!(monomorphic_flat_map_twice, MONOMORPHIC_FLAT_MAP_TWICE, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:1, i64:1], [i64:2]]"
});

// A callback typed body-first returns a cell constrained to an array;
// `Option<'b>` holds it through `'b`, not as the whole `['b, null]`.
const EXTRACTED_FILTER_MAP_CALLBACK: &str = r#"
{
  let tm = |x| [x, x];
  array::filter_map([1, 2], tm)
}
"#;

run!(extracted_filter_map_callback, EXTRACTED_FILTER_MAP_CALLBACK, |v: Result<
    &Value,
>| {
    format!("{}", v.unwrap()) == "[[i64:1, i64:1], [i64:2, i64:2]]"
});

// A callback's argument type holding another function's `'b` stays apart
// from the collection's own `'b`: type variables are cells, not names.
const SAME_NAMED_TVAR_IN_CALLBACK_ARG: &str = r#"
{
  let g = |x: 'b| x;
  let a = array::map([100, g, 3], |x| [x, 1]);
  let b = array::filter_map([100, array::find_map, 3], |x| [x, 1]);
  (array::len(a), array::len(b))
}
"#;

run!(same_named_tvar_in_callback_arg, SAME_NAMED_TVAR_IN_CALLBACK_ARG, |v: Result<
    &Value,
>| {
    format!("{}", v.unwrap()) == "[i64:3, i64:3]"
}; graphix_package_core::testing::FuseExpect::None);

// A call through any expression that reaches a parameter (parens, an
// alias) types against the definition's own cells: an open gate's
// signature is not generalized yet.
const PARAM_CALLED_THROUGH_AN_EXPRESSION: &str = r#"
{
  let m = |a, f: fn(x: 'a) -> 'b| -> Array<'b> array::map(a, |v| ((f))(v));
  let n = |a, f: fn(x: 'a) -> 'b| -> Array<'b> { let g = f; array::map(a, |v| g(v)) };
  let apply = |f: fn<'b: Number>(x: 'b) -> 'b| ((f))(1);
  (m([1, 2], |x| x * 2), n(["a"], |s| "[s]!"), apply(|x| x))
}
"#;

run!(
    param_called_through_an_expression,
    PARAM_CALLED_THROUGH_AN_EXPRESSION,
    |v: Result<&Value>| {
        format!("{}", v.unwrap()) == r#"[[i64:2, i64:4], ["a!"], i64:1]"#
    };
    graphix_package_core::testing::FuseExpect::None
);

// A definition does not generalize a cell it shares with its environment:
// `x + y` gives `x` the outer `y`'s cell, so each call types `y` too, as
// the inline callback does.
const ENVIRONMENT_CELL_IS_NOT_GENERALIZED: &str = r#"
{
  let y = array::iter(never());
  let t = |x| x + y;
  let m = array::map([1], t);
  42
}
"#;

run!(environment_cell_is_not_generalized, ENVIRONMENT_CELL_IS_NOT_GENERALIZED, |v: Result<
    &Value,
>| matches!(v, Ok(Value::I64(42))); graphix_package_core::testing::FuseExpect::Jit);

// So that definition is monomorphic: a second call at another type is
// refused where it is made.
run!(
    environment_cell_makes_the_definition_monomorphic,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain f64")
        && !format!("{e:#}").contains("in the instance of")),
    "/test.gx" => r#"
        let y = array::iter(never());
        let t = |x| x + y;
        let result = (t(1), t(2.0))
    "#
; graphix_package_core::testing::FuseExpect::None);

// A cell only the definition's body reaches is generalized when its
// gate closes, nested definitions included.
const CLOSED_DEFINITIONS_ARE_POLYMORPHIC: &str = r#"
{
  let f = |a| { let g = |x| (x, a); (g(1), g("s")) };
  let rec h = |x| x;
  let k: fn(x: 'a) -> 'a = |x| x;
  (f(1.0), f(true), h(1), h("s"), k(2), k("t"))
}
"#;

run!(closed_definitions_are_polymorphic, CLOSED_DEFINITIONS_ARE_POLYMORPHIC, |v: Result<
    &Value,
>| format!("{}", v.unwrap())
        == r#"[[[i64:1, f64:1.], ["s", f64:1.]], [[i64:1, true], ["s", true]], i64:1, "s", i64:2, "t"]"#; graphix_package_core::testing::FuseExpect::Jit);

// A definition whose gate has not run yet (a submodule the interface
// declares first calls into its parent) is called at its declared
// scheme: each call copies its cells.
run!(
    call_before_the_definition_is_checked,
    |v: Result<&Value>| format!("{}", v.unwrap()) == r#"[i64:1, "s"]"#,
    "/test.gx" => r#"
mod m;
let result = m::sub::both
"#,
    "/test/m.gxi" => r#"
val first: fn(a: Array<'r>) -> 'r;
mod sub;
"#,
    "/test/m.gx" => r#"
let first = |a: Array<'r>| -> 'r a[0]$;
"#,
    "/test/m/sub.gx" => r#"
use super::first;
let both = (first([1]), first(["s"]));
"#
; graphix_package_core::testing::FuseExpect::Jit);

// A declared type variable is rigid in its definition's body: passing
// it where a concrete type is wanted is refused at the definition, for
// every 'r a caller could pick, not at the instance that picks one.
run!(
    rigid_argument_to_a_concrete_parameter,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("i64 does not contain 'r")
        && !format!("{e:#}").contains("in the instance of")),
    "/test.gx" => r#"
        let g = |a: i64| 3;
        let f = |x: 'r| g(x);
        let result = f("a")
    "#
; graphix_package_core::testing::FuseExpect::None);

// So is an annotation that would narrow it, and a concrete container.
run!(
    rigid_under_a_concrete_annotation,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("i64 does not contain 'r")),
    "/test.gx" => r#"
        let f = |x: 'r| { let y: i64 = x; y };
        let result = 0
    "#
; graphix_package_core::testing::FuseExpect::None);

run!(
    rigid_element_to_a_concrete_container,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("Array<i64> does not contain Array<'r")),
    "/test.gx" => r#"
        let g = |a: Array<i64>| 3;
        let f = |x: Array<'r>| g(x);
        let result = 0
    "#
; graphix_package_core::testing::FuseExpect::None);

// What holds 'r for every 'r still does: a union naming it, Any, or a
// constraint the parameter accepts.
run!(
    rigid_where_every_choice_fits,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(3))),
    "/test.gx" => r#"
        let anything = |a: Any| 1;
        let num = |a: Number| 1;
        let f = |x: 'r| -> ['r, null] { let o: ['r, null] = x; o };
        let g = |x: 'r| anything(x);
        let h = 'r: Number |x: 'r| num(x);
        let o = f("a");
        let result = g("a") + h(2) + h(1.5)
    "#
);

// A let annotation's same-named variables are one variable: the
// annotation claims `fn(x: 'a) -> 'a`, which `fn(string) -> bytes` is not.
run!(
    annotation_variable_is_one_variable,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain fn(s: string) -> bytes")),
    "/test.gx" => r#"
        let f: fn(x: 'a) -> 'a = buffer::from_string;
        let result = 0
    "#
; graphix_package_core::testing::FuseExpect::None);

// A function value prints as its source, the same cold and warm.
const FN_PRINTS_ITS_SOURCE: &str = r#"
{
  let f = |acc, x| str::len(x) + acc;
  "[f]"
}
"#;

run!(fn_prints_its_source, FN_PRINTS_ITS_SOURCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.as_str() == "Abstract(|acc, x| str::len(x) + acc)")
}; graphix_package_core::testing::FuseExpect::None);

// A `let` holds no value: it is a statement, never an element.
const LET_IN_VALUE_POSITION: &str = r#"
{
  let a = "s";
  let t = (let a = 3, 1);
  a
}
"#;

run!(let_in_value_position, LET_IN_VALUE_POSITION, |v: Result<&Value>| {
    matches!(v, Err(e) if format!("{e:#}").contains("a let binding is not an expression"))
}; graphix_package_core::testing::FuseExpect::None);

// An instance's node can be born knowing a type its definition's check
// widened: the callback's `d.domain` is `string` in the instance, where
// the check unified its cell with `f`'s formal. The instance takes the
// narrower type; it refuses nothing its definition's check accepted.
run!(
    instance_node_narrower_than_its_row,
    r#"{
        let f = |x: [Array<i64>, string]| "ok";
        let rows = [{domain: "a"}, {domain: "b"}];
        array::map(rows, |d| f(d.domain))
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 2)
);
