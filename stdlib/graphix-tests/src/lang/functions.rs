// Tests for lambdas, first-class functions, labeled arguments, recursive functions

use super::dense_deltas::{run_delta, run_delta_fused};
use anyhow::Result;
use compact_str::{CompactString, format_compact};
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, as_i64s, eval, is, prints, refused, refused_renumbered},
};
use netidx::publisher::Value;

const LAMBDA: &str = r#"
{
  let y = 10;
  let f = |x| x + y;
  f(10)
}
"#;

run!(lambda, LAMBDA, is(Value::I64(20)); FuseExpect::Jit);

const FIRST_CLASS_LAMBDAS: &str = r#"
{
  let doit = |x: i64| x + 1;
  let g = |f: fn(x: i64) -> i64, y| f(y) + 1;
  g(doit, 1)
}
"#;

run!(first_class_lambdas, FIRST_CLASS_LAMBDAS, is(Value::I64(3)); FuseExpect::Jit);

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
run!(two_rigid_vars_independent, TWO_RIGID_VARS_INDEPENDENT, is(Value::I64(3)); FuseExpect::Jit);

const TWO_RIGID_VARS_UNIFIED: &str = r#"
{
  let add = 'a: Number, 'b: Number |x: 'a, y: 'b| -> 'a x + y;
  add(1, 2)
}
"#;

// `+` is ('a, 'a) -> 'a, so this body is well typed only where 'a = 'b:
// a promise the signature does not make. Refused at the definition.
run!(two_rigid_vars_unified, TWO_RIGID_VARS_UNIFIED, refused("both operands must be one numeric type"); FuseExpect::None);

const DEFAULT_ILL_TYPED_AT_DEFINITION: &str = r#"
{
  let h = |#bar: i64 = |i| i, baz| bar + baz;
  1
}
"#;

// A default is checked at the definition, called or not: a lambda is
// not an i64.
run!(default_ill_typed_at_definition, DEFAULT_ILL_TYPED_AT_DEFINITION, |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("i64 does not contain fn(i:")); FuseExpect::None);

const DEFAULT_IN_CONSTRAINT_SET: &str = r#"
{
  let f = 'a: [Int, Float] |#start: 'a = 0.0, x: 'a| -> 'a start + x;
  select (f(1.5), f(#start: 1, 1)) {
    (f64:1.5, i64:2) => 1,
    _ => 0
  }
}
"#;

// Against a declared variable a default must fit the variable's
// constraints, not the variable: the site that omits the argument takes
// the default's type, a site that passes it takes its own.
run!(default_in_constraint_set, DEFAULT_IN_CONSTRAINT_SET, is(Value::I64(1)); FuseExpect::Jit);

const DEFAULT_OUTSIDE_CONSTRAINT_SET: &str = r#"
{
  let f = 'a: Int |#start: 'a = 0.0, x: 'a| -> 'a start + x;
  f(1)
}
"#;

run!(default_outside_constraint_set, DEFAULT_OUTSIDE_CONSTRAINT_SET, refused("Int does not contain f64"); FuseExpect::None);

const LABELED_ARGS: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  f(#foo: 0)
}
"#;

run!(labeled_args, LABELED_ARGS, is(Value::I64(42)); FuseExpect::Jit);

const REQUIRED_ARGS: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  f(#bar: 0)
}
"#;

run!(required_args, REQUIRED_ARGS, refused("missing required argument foo"); FuseExpect::None);

const MIXED_ARGS: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42, baz| foo + bar + baz;
  f(#foo: 0, 0)
}
"#;

run!(mixed_args, MIXED_ARGS, is(Value::I64(42)); FuseExpect::Jit);

const ARG_SUBTYPING: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  let g = |f: fn(#foo: i64) -> i64| f(#foo: 3);
  g(f)
}
"#;

run!(arg_subtyping, ARG_SUBTYPING, is(Value::I64(45)); FuseExpect::Jit);

const ARG_NAME_SHORT: &str = r#"
{
  let f = |#foo: i64, #bar: i64 = 42| foo + bar;
  let foo = 3;
  f(#foo)
}
"#;

run!(arg_name_short, ARG_NAME_SHORT, is(Value::I64(45)); FuseExpect::Jit);

const LATE_BINDING0: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| x - y + c };
  let f = t.f;
  f(#y: 3, #x: 4)
}
"#;

run!(late_binding0, LATE_BINDING0, is(Value::I64(1)); FuseExpect::None);

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

run!(late_binding1, LATE_BINDING1, prints("[i64:1, i64:2]"); FuseExpect::None);

const LATE_BINDING2: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  (t.f)(#y: 3, #x: 4)
}
"#;

run!(late_binding2, LATE_BINDING2, is(Value::I64(1)); FuseExpect::None);

const LATE_BINDING3: &str = r#"
{
    let f: fn(x: i64) -> i64 = never();
    let res = f(1);
    f <- |i: i64| i + 1;
    res
}
"#;

run!(late_binding3, LATE_BINDING3, is(Value::I64(2)); FuseExpect::None);

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

run!(late_binding4, LATE_BINDING4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 3]>() {
        Ok([0, 0, 55]) => true,
        Ok(_) | Err(_) => false,
    },
    _ => false,
}; FuseExpect::None);

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

run!(tail_stateful_per_iteration, TAIL_STATEFUL_PER_ITERATION, is(Value::I64(3)); FuseExpect::None);

// A stateful builtin in a value-position select arm inside native
// recursion node-walks; each activation owns its `once`.
const TAIL_STATEFUL_SCALAR: &str = r#"
{
  let rec f = |n: i64, acc: i64| -> i64 select n {
    i64:0 => acc,
    _ => f(n - i64:1, acc + once(n))
  };
  f(i64:10, i64:0)
}
"#;

run!(tail_stateful_scalar, TAIL_STATEFUL_SCALAR, is(Value::I64(55)); FuseExpect::None);

const FOLD_STATEFUL_PER_SLOT: &str = r#"
array::fold([i64:10, i64:20, i64:30], i64:0, |acc, x| acc + count(x))
"#;

run!(fold_stateful_per_slot, FOLD_STATEFUL_PER_SLOT, is(Value::I64(3)); FuseExpect::None);

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

run!(tail_stateless_collapses, TAIL_STATELESS_COLLAPSES, is(Value::I64(60)); FuseExpect::Jit);

const RECURSIVE_LAMBDA0: &str = r#"
{
    let rec f = |x: i64| select x { x if x < 10 => f(x + 1), x => x };
    #[native] f(0)
}
"#;

// A `let rec` self-call knots to the def's own cells, so the loop fuses.
run!(recursive_lambda0, RECURSIVE_LAMBDA0, is(Value::I64(10)); FuseExpect::Jit);

// A fully annotated arithmetic lambda.
const FUSED_ARITH: &str = r#"
{
    let f = |a: i64, b: i64| -> i64 a * a + b * b;
    #[native] f(3, 4)
}
"#;

run!(fused_arith, FUSED_ARITH, is(Value::I64(25)); FuseExpect::Jit);

// A tail-recursive countdown: self-call in tail position loops.
const FUSED_TAIL_LOOP: &str = r#"
{
    let rec countdown = |n: i64, acc: i64| -> i64
        select n {
            0 => acc,
            _ => countdown(n - 1, acc + n)
        };
    #[native] countdown(100, 0)
}
"#;

run!(fused_tail_loop, FUSED_TAIL_LOOP, is(Value::I64(5050)); FuseExpect::Jit);

// Deep tail recursion runs in constant stack as a native loop. The
// node-walk recurses, an activation per level.
const TAIL_LOOP_DEEP: &str = r#"
{
    let rec count = |n: i64, acc: i64| -> i64
        select n {
            0 => acc,
            _ => count(n - 1, acc + 1)
        };
    #[native] count(500000, 0)
}
"#;

run!(tail_loop_deep, TAIL_LOOP_DEEP, is(Value::I64(500000)); FuseExpect::Jit; jit_only);

// An arm a tail depth moves to is entered into a world that stands: `y`
// fired while depth 1 took arm `0`, so at its move to arm `1` `y` stands
// and the arm writes nothing. `x` stays 0.
async fn tail_depth_arm_entered_later_writes_nothing(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(
        r#"{
            let t = array::iter([1, 2]);
            let y = uniq(select t { _ => 10 });
            let n = select t { 1 => 2, _ => 3 };
            let x = 0;
            let rec f = |n| select n { 0 => 0, 1 => { x <- y; 1 }, n => f(n - 2) };
            (f(n), x)
        }"#,
        mode,
        FuseExpect::Jit,
    )
    .await?;
    let shown: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(shown, ["[i64:0, i64:0]", "[i64:1, i64:0]"]);
    Ok(())
}

modes!(tail_depth_arm_entered_later_writes_nothing);

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

run!(fact_value_position, FACT_VALUE_POSITION, is(Value::I64(120)); FuseExpect::Jit);

// A mandelbrot-shape kernel.
const FUSED_MANDELBROT: &str = r#"
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

run!(fused_mandelbrot, FUSED_MANDELBROT, is(Value::I64(7)); FuseExpect::Jit);

// An unannotated callback passed to a HOF.
const FOLD_DEFERRED_CALLBACK: &str = r#"
{
    let xs = array::init(100, |idx: i64| idx);
    array::fold(xs, 0, |acc, x| acc + x * 2)
}
"#;

run!(fold_deferred_callback, FOLD_DEFERRED_CALLBACK, is(Value::I64(9900)));

// A recursive lambda with no annotations still computes correctly.
const UNANNOTATED_RECURSION: &str = r#"
{
    let rec sum_to = |n, acc|
        select n {
            0 => acc,
            _ => sum_to(n - 1, acc + n)
        };
    sum_to(100, 0)
}
"#;

run!(unannotated_recursion, UNANNOTATED_RECURSION, is(Value::I64(5050)); FuseExpect::Jit);

// A three-level call chain with no annotations. outer(5) = 20.
const UNANNOTATED_THREE_LEVEL: &str = r#"
{
    let inner = |x| x * x + 1;
    let middle = |x| inner(x) + inner(x + 1);
    let outer = |x| middle(x) - middle(x - 1);
    #[native] outer(5)
}
"#;

// The bare lambdas' operand cells settle to i64, so the chain fuses.
run!(unannotated_three_level, UNANNOTATED_THREE_LEVEL, is(Value::I64(20)); FuseExpect::Jit);

// A higher-order function with a function-typed argument: 5*5 + 1 = 26.
const FN_FORMAL_HOF: &str = r#"
{
    let square = |x: i64| -> i64 x * x;
    let combine = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x) + 1;
    combine(square, 5)
}
"#;

run!(fn_formal_hof, FN_FORMAL_HOF, is(Value::I64(26)); FuseExpect::Jit);

// A let-bound helper whose body is `array::fold` over a literal, called
// transitively: helper(5) + 1 = 51.
const STATIC_FOLD_HELPER: &str = r#"
{
    let helper = |x: i64| -> i64 array::fold([x, x], 0, |a, b| a + b * b);
    let outer = |x: i64| -> i64 helper(x) + 1;
    outer(5)
}
"#;

run!(
    static_fold_helper_fuses,
    STATIC_FOLD_HELPER,
    is(Value::I64(51)); FuseExpect::Jit);

// A transitive chain g1 -> g2 -> g3 fuses whole: g1(10) = 21.
const TRANSITIVE_CHAIN: &str = r#"
{
    let g3 = |n: i64| -> i64 n + 1;
    let g2 = |n: i64| -> i64 g3(n) * 2;
    let g1 = |n: i64| -> i64 g2(n) - 1;
    #[native] g1(10)
}
"#;

run!(transitive_chain, TRANSITIVE_CHAIN, is(Value::I64(21)); FuseExpect::Jit);

// A transitively-called callee whose body contains a cast: g(true) = 1.
const CALL_THROUGH_CAST_CALLEE: &str = r#"
{
    let g = |b: bool| cast<i64>(b)$;
    g(true) + g(false)
}
"#;

run!(call_through_cast_callee, CALL_THROUGH_CAST_CALLEE, is(Value::I64(1)); FuseExpect::Jit);

// The cast two callee levels deep: g(b) = h(b) + 10 = 21.
const CAST_TWO_CALLEES_DEEP: &str = r#"
{
    let h = |b: bool| cast<i64>(b)$;
    let g = |b: bool| h(b) + 10;
    g(true) + g(false)
}
"#;

run!(cast_two_callees_deep, CAST_TWO_CALLEES_DEEP, is(Value::I64(21)); FuseExpect::Jit);

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

run!(cross_region_callee_base, CROSS_REGION_CALLEE_BASE, is(Value::I64(1)); FuseExpect::Jit);

// A recursive callee whose base case is a cast: g(3) = 1.
const RECURSIVE_CALLEE_CAST_BASE: &str = r#"
{
    let rec g = |n: i64| -> i64 select n { 0 => cast<i64>(true)$, _ => g(n - 1) };
    g(3)
}
"#;

run!(recursive_callee_cast_base, RECURSIVE_CALLEE_CAST_BASE, is(Value::I64(1)); FuseExpect::Jit);

const LAMBDAMATCH0: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}: T| bar + bar;
  f(x)
}
"#;

run!(lambdamatch0, LAMBDAMATCH0, is(Value::I64(84)); FuseExpect::Jit);

const LAMBDAMATCH1: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}| bar + bar;
  f(x)
}
"#;

run!(lambdamatch1, LAMBDAMATCH1, prints("i64:84"); FuseExpect::None);

const LAMBDAMATCH2: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f(x)
}
"#;

run!(lambdamatch2, LAMBDAMATCH2, is(Value::I64(84)); FuseExpect::Jit);

const LAMBDAMATCH3: &str = r#"
{
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f({bar: 42, baz: 1})
}
"#;

run!(lambdamatch3, LAMBDAMATCH3, refused("missing match cases: nothing matches { bar: i64, baz: i64 } (3:12)"); FuseExpect::None);

const LAMBDAMATCH4: &str = r#"
{
  let f = |(i, _)| i * 2;
  f((42, "foo"))
}
"#;

run!(lambdamatch4, LAMBDAMATCH4, is(Value::I64(84)); FuseExpect::Jit);

const LAMBDAMATCH5: &str = r#"
{
  let f = |(i, _)| i * 2;
  f("foo")
}
"#;

run!(lambdamatch5, LAMBDAMATCH5, refused("missing match cases: nothing matches string (3:12)"); FuseExpect::None);

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

run!(nested_optional0, NESTED_OPTIONAL0, is(Value::I64(42)); FuseExpect::Jit);

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

run!(arg_update_before_bind, ARG_UPDATE_BEFORE_BIND, is(Value::I64(31)); FuseExpect::Jit);

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
}; FuseExpect::Jit);

// Variadic args: extra positional args beyond the fixed signature.
const VARGS0: &str = r#"
array::push([1, 2], 3, 4, 5)
"#;

run!(vargs0, VARGS0, prints("[i64:1, i64:2, i64:3, i64:4, i64:5]"));

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

run!(shadowed_name_cross_kernel, SHADOWED_NAME_CROSS_KERNEL, is(Value::I64(5)); FuseExpect::Jit);

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

run!(two_monomorphizations_one_region, TWO_MONOMORPHIZATIONS_ONE_REGION, is(Value::F64(11.0)); FuseExpect::Jit);

// A fold-callback local sharing a name with a nested callee's parameter
// resolves by BindId; the collection still fuses.
const FOLD_CALLBACK_NAME_COLLISION: &str = r#"
{
  let rec pair = |e: i64| -> i64 select e { 0 => 0, _ => e * 10 };
  let run_one = |s: i64| -> i64 {
    let e = s + 1;
    pair(e + 1) + e
  };
  #[native] array::fold(array::init(1, |i| i), 0, |acc, i| acc + run_one(i))
}
"#;

run!(fold_callback_name_collision, FOLD_CALLBACK_NAME_COLLISION, is(Value::I64(21)); FuseExpect::Jit);

// An abandoned kernel-closure build (the rec lambda de-fuses on the
// base case's type test, which has no tag form) must not break a later
// region's compile.
const ABANDONED_KERNEL_CLOSURE: &str = r#"
{
  let rec f = |n: i64| -> i64 select n {
    m if m <= i64:0 => {
      let d: [datetime, Error<`E>] = error(`E);
      select d { datetime as _ => i64:1, error as _ => i64:2 }
    },
    m => (m + f(m - i64:1))
  };
  let v = f(i64:8);
  (v, false)
}
"#;

run!(abandoned_kernel_closure, ABANDONED_KERNEL_CLOSURE, prints("[i64:38, false]"); FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
    is(Value::Bool(false));
    FuseExpect::Jit
);

// A bottom map slot taints the map's result, not the kernel.
const UNUSED_BOTTOM_MAP_SLOT: &str = r#"
{
  let m = array::map([i64:1, i64:0], |x| i64:5 / x);
  false
}
"#;

run!(unused_bottom_map_slot, UNUSED_BOTTOM_MAP_SLOT, is(Value::Bool(false)); FuseExpect::Jit);

// find scans all slots: a bottom predicate after the matching element
// bottoms the find; the independent tail fires.
const FIND_BOTTOM_AFTER_MATCH: &str = r#"
{
  let r = array::find([i64:1, i64:0], |x| (i64:5 / x) > i64:0);
  any(r, i64:-1)
}
"#;

run!(find_bottom_after_match, FIND_BOTTOM_AFTER_MATCH, is(Value::I64(-1)); FuseExpect::Jit);

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
    is(Value::I64(-1))
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
    is(Value::I64(1));
    FuseExpect::Jit
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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

/// Every update of `code`'s `raised`, a count of the errors its slots
/// raise.
async fn raised(code: &str, mode: Mode) -> Result<Vec<i64>> {
    let (values, _) = run_delta_fused(code, mode, FuseExpect::Jit).await?;
    Ok(as_i64s(&values))
}

/// `code` with its growing source, whose slots arrive after the root's
/// first cycle, replaced by four slots born with the root.
fn born_with_the_root(code: &str) -> String {
    let grown = "array::init(n, |i| i)";
    assert!(code.contains(grown));
    code.replace(grown, "array::init(4, |i| i)")
}

// Each slot of a growing map calls `g` as an instance of its own, which
// is entered, not born: its constant error never fires, so nothing
// raises. Called from slots born with the root it raises once per slot.
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
  raised
}
"#;

async fn hof_slot_callee_is_entered_not_born(mode: Mode) -> Result<()> {
    assert_eq!(raised(HOF_SLOT_CALLEE_FIRST_CALL, mode).await?, [0]);
    let born = born_with_the_root(HOF_SLOT_CALLEE_FIRST_CALL);
    assert_eq!(raised(&born, mode).await?, [0, 1, 2, 3, 4]);
    Ok(())
}

modes!(hof_slot_callee_is_entered_not_born);

// A nested map over a source outside the loop is an instance per outer
// slot: a new slot's inner map reads `ys` standing, and its first
// observation of it is no fire, so the error derived from it never
// raises. In slots born with the root it fires and raises once per slot.
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
  raised
}
"#;

async fn hof_slot_nested_map_is_quiet_over_a_standing_source(mode: Mode) -> Result<()> {
    assert_eq!(raised(HOF_SLOT_NESTED_PREV_LEN, mode).await?, [0]);
    let born = born_with_the_root(HOF_SLOT_NESTED_PREV_LEN);
    assert_eq!(raised(&born, mode).await?, [0, 1, 2, 3, 4]);
    Ok(())
}

modes!(hof_slot_nested_map_is_quiet_over_a_standing_source);

// A slot that arrives after the root's first cycle is entered, not born:
// a constant in the callback body does not fire in it, and its error
// never raises. In slots born with the root it fires and raises once per
// slot.
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
  raised
}
"#;

async fn hof_slot_constant_fires_only_in_a_birth(mode: Mode) -> Result<()> {
    assert_eq!(raised(HOF_SLOT_CONSTANT_FIRES, mode).await?, [0]);
    let born = born_with_the_root(HOF_SLOT_CONSTANT_FIRES);
    assert_eq!(raised(&born, mode).await?, [0, 1, 2, 3, 4]);
    Ok(())
}

modes!(hof_slot_constant_fires_only_in_a_birth);

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
    FuseExpect::Jit
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
    FuseExpect::Jit
);

// A non-tail recursion's result as a fold's init: the fold reads it.
const NONTAIL_RESULT_AS_FOLD_INIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  any(array::fold([i64:41], f(i64:256), |acc, x| acc + x), i64:-1)
}
"#;

run!(nontail_result_as_fold_init, NONTAIL_RESULT_AS_FOLD_INIT, is(Value::I64(32937)); FuseExpect::Jit);

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
}; FuseExpect::Jit);

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

run!(rtype_rejects_error_arm, RTYPE_REJECTS_ERROR_ARM, refused("type mismatch i64 does not contain Error<'a: unbound above i64>"); FuseExpect::None);

// A fn-valued element does not slip through a recursive-type HOF chain:
// `acc + <fn>` is rejected.
run!(
    list_map_fn_element_fold_rejected,
    refused("type mismatch Number does not contain fn(#clock: Any, v: 'a: unbound) -> 'a: unbound"),
    "/test.gx" => r#"
        let result = list::fold(
            list::map(list::from_array([true]), |x| hold),
            i64:0,
            |acc, x| acc + x
        )
    "#;
    FuseExpect::None
);

// Parens are transparent: `let rec f = (|n| …)` is the bare spelling.
run!(
    rec_through_parens,
    is(Value::I64(0)),
    "/test.gx" => r#"
        let rec f = (|n: i64| -> i64 select n { i64:0 => i64:0, _ => f(n - i64:1) });
        let result = f(i64:3)
    "#;
    FuseExpect::Jit
);

// A generalized fn-valued argument's cells bind at callback
// finalization; the call site settles after, so the extracted callback
// typechecks like the inline one.
run!(
    extracted_callback_settles_after_finalize,
    is(Value::I64(3)),
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
    refused("type mismatch Number does not contain Option<'a>");
    FuseExpect::None
);

const OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT: &str = r#"
array::fold(array::map([f64:23.5, i64:2, i64:3], |x| x * i64:2), i64:0, |res, v| res / v)
"#;

run!(
    operand_refuses_mixed_numeric_element,
    OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT,
    refused("type mismatch i64 does not contain f64");
    FuseExpect::None
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

run!(rec_rtype_rejects_error_arm, REC_RTYPE_REJECTS_ERROR_ARM, refused("type mismatch i64 does not contain Error<'a: unbound above i64>"); FuseExpect::None);

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

run!(rec_selfcall_arg_mismatch, REC_SELFCALL_ARG_MISMATCH, refused("type mismatch Number does not contain string"); FuseExpect::None);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A select over [`A, `B] missing the `B arm is a compile error.
const SELECT_VARIANT_NONEXHAUSTIVE: &str = r#"
{
  let x: [`A, `B] = `A;
  select x { `A => i64:1 }
}
"#;

run!(select_variant_nonexhaustive, SELECT_VARIANT_NONEXHAUSTIVE, refused("missing match cases"); FuseExpect::None);

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

run!(rec_in_hof_callback, REC_IN_HOF_CALLBACK, is(Value::I64(125250)); FuseExpect::Jit);

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

run!(rec_in_split_callback, REC_IN_SPLIT_CALLBACK, prints("[i64:8]"); FuseExpect::Jit);

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

run!(rec_transient_capture_wake, REC_TRANSIENT_CAPTURE_WAKE, prints("[i64:100, i64:101, i64:102]"); FuseExpect::Jit);

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

run!(rec_transient_stateful_retained, REC_TRANSIENT_STATEFUL_RETAINED, prints("[i64:3, i64:6, i64:9]"); FuseExpect::Jit);

// Shrink is delete: a depth no fire reaches is deleted and starts fresh
// when reached again, while a depth still reached, its arm asleep
// included, keeps its count. Retained depths would give 3, 2, 7, 0, 10.
async fn rec_shrink_deletes_unreached_depths(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = array::iter([3, 1, 3, 0, 3]);
        let rec f = |n: i64| -> i64 select n { 0 => 0, _ => count(n) + f(n - 1) };
        f(t)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [3, 2, 6, 0, 6]);
    Ok(())
}

modes!(rec_shrink_deletes_unreached_depths);

// A pure recursion's depth swinging 10, 5, 10, .. for 250 cycles: each
// shrink deletes the depths below it, each deep cycle enters them afresh,
// and every cycle answers its own sum.
async fn rec_pure_depth_swing_answers_each_cycle(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = array::iter(array::init(250, |i| 10 - i % 2 * 5));
        let rec f = |n: i64| -> i64 select n { 0 => 0, _ => n + f(n - 1) };
        f(t)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    let expected: Vec<i64> = (0..250).map(|i| if i % 2 == 0 { 55 } else { 15 }).collect();
    assert_eq!(as_i64s(&values), expected);
    Ok(())
}

modes!(rec_pure_depth_swing_answers_each_cycle);

// A depth entered again after a shrink reads its capture as it stands:
// the capture is written once, while depths 2 and 3 do not exist, and the
// depths regrown a cycle after the write read 200 with nothing firing it.
async fn rec_regrown_depth_reads_its_capture_as_it_stands(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = array::iter([3, 1, 1, 3]);
        let base = 100;
        base <- once(select t {1 => 200, _ => never()});
        let rec f = |n: i64| -> i64 select n { 0 => base, _ => n + f(n - 1) };
        f(t)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [106, 101, 201, 206]);
    Ok(())
}

modes!(rec_regrown_depth_reads_its_capture_as_it_stands);

const LINEAR: &str = "let rec f = |n: i64| -> i64 select n { 0 => 0, _ => n + f(n - 1) }";
const FIB: &str =
    "let rec f = |n: i64| -> i64 select n { n if n < 2 => n, n => f(n - 1) + f(n - 2) }";

fn linear(n: i64) -> i64 {
    n * (n + 1) / 2
}

fn fib(n: i64) -> i64 {
    if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
}

/// The recursion called on `t` directly.
const DIRECT: &str = "f(t)";

/// What a node-walked run of the recursion `def`, called by `call` over
/// the arguments `t`, one a cycle, a 0 a cycle in which the recursion does
/// not run, left: its values and the activations live once it settled.
async fn run_rec(
    def: &str,
    call: &str,
    ts: &[i64],
    mode: Mode,
) -> Result<(Vec<i64>, u64)> {
    use graphix_package_core::testing::{
        Collect, compile_result, fixture_runtime, result_source, updates_until,
    };
    use std::fmt::Write;
    let mut code = String::from("{\n let t = filter(array::iter([");
    for (i, t) in ts.iter().enumerate() {
        write!(code, "{}{t}", if i == 0 { "" } else { ", " })?;
    }
    write!(code, "]), |n| n > 0);\n {def};\n {call}\n}}")?;
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(&code))],
        &crate::TEST_REGISTER,
        mode,
        |_| (),
    )
    .await?;
    let control = ctx.rt.control();
    let res = compile_result(&ctx).await?;
    let values =
        updates_until(&ctx.rt, &mut rx, res.exprs[0].id, Collect::Settled).await?;
    let live = control.live_activations();
    ctx.shutdown().await;
    Ok((as_i64s(&values), live))
}

/// A shrink deletes the callee: a transient deep recursion leaves nothing
/// behind it. After its first run at a shallow depth, and after many, the
/// live activations are those of a recursion that only ever ran shallow; a
/// cycle the recursion does not run changes nothing.
async fn rec_transient_depth_is_deleted(mode: Mode) -> Result<()> {
    let cases: [(&str, i64, i64, fn(i64) -> i64); 3] =
        [(LINEAR, 200, 5, linear), (LINEAR, 50, 5, linear), (FIB, 12, 4, fib)];
    for (def, deep, shallow, eval) in cases {
        for exits in [0, 1, 50] {
            let mut ts = vec![deep];
            ts.extend(std::iter::repeat_n([shallow, 0], exits + 1).flatten());
            let (values, live) = run_rec(def, DIRECT, &ts, mode).await?;
            let runs = ts.iter().filter(|n| **n > 0);
            assert_eq!(values, runs.map(|n| eval(*n)).collect::<Vec<_>>());
            let (_, base) = run_rec(def, DIRECT, &vec![shallow; exits + 2], mode).await?;
            assert_eq!(live, base, "{def}: {exits} shallow runs after the first");
        }
    }
    Ok(())
}

/// A depth a shrink deleted is reached again: its activations are entered
/// afresh and every cycle answers its own value, through a shrink, a
/// regrow to the first depth, a shrink part of the way and one further.
/// What the last, shallowest runs do not reach is deleted.
async fn rec_deleted_depth_reached_again(mode: Mode) -> Result<()> {
    let cases: [(&str, [i64; 4], fn(i64) -> i64); 2] =
        [(LINEAR, [200, 5, 150, 3], linear), (FIB, [12, 4, 11, 3], fib)];
    for (def, [deep, shallow, again, last], eval) in cases {
        let ts = [deep, shallow, shallow, deep, again, last, last];
        let (values, live) = run_rec(def, DIRECT, &ts, mode).await?;
        assert_eq!(values, ts.iter().map(|n| eval(*n)).collect::<Vec<_>>());
        let (_, base) = run_rec(def, DIRECT, &[last; 7], mode).await?;
        assert_eq!(live, base, "{def}");
    }
    Ok(())
}

/// A chain a later run reaches only part of the way leaves nothing below
/// what that run reaches, and every cycle answers its own value. Linear
/// 200 then 5, 6, 5 and 6 from then on, 200 then 5 and 6 swinging, and
/// 200 then 5 called from a seq step, each of whose runs rebirths the
/// recursion, hold what a recursion that only ever ran at the last depth
/// does; so does fib(12) then the same shapes over 4 and 5.
async fn rec_partly_reached_chain_leaves_nothing_below(mode: Mode) -> Result<()> {
    const IN_SEQ: &str = "seqq let n = t { f(n) }";
    let regrown_short = |d: i64, s: i64, exits: usize| {
        let mut ts = vec![d, s, s + 1, s];
        ts.extend(std::iter::repeat_n(s + 1, exits));
        ts
    };
    let swing = |d: i64, s: i64, exits: usize| {
        let mut ts = vec![d, s, s + 1];
        ts.extend(std::iter::repeat_n([s, s + 1], exits).flatten());
        ts
    };
    let seq_runs = |d: i64, s: i64, exits: usize| {
        let mut ts = vec![d];
        ts.extend(std::iter::repeat_n(s, exits + 1));
        ts
    };
    let shapes: [(&str, &dyn Fn(i64, i64, usize) -> Vec<i64>); 3] =
        [(DIRECT, &regrown_short), (DIRECT, &swing), (IN_SEQ, &seq_runs)];
    for (call, shape) in shapes {
        for (def, eval, deep, shallow, all_exits) in [
            (LINEAR, linear as fn(i64) -> i64, 200, 5, &[1, 50, 150][..]),
            (FIB, fib, 12, 4, &[150]),
        ] {
            let last = *shape(deep, shallow, 1).last().unwrap();
            let (_, base) = run_rec(def, call, &[last; 4], mode).await?;
            for exits in all_exits {
                let ts = shape(deep, shallow, *exits);
                let (values, live) = run_rec(def, call, &ts, mode).await?;
                assert_eq!(values, ts.iter().map(|n| eval(*n)).collect::<Vec<_>>());
                assert_eq!(live, base, "{def} by {call}: {ts:?}");
            }
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_deleted_interp() -> Result<()> {
    rec_transient_depth_is_deleted(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_deleted_par() -> Result<()> {
    rec_transient_depth_is_deleted(Mode::Par).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_deleted_unpruned() -> Result<()> {
    rec_transient_depth_is_deleted(Mode::Unpruned).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_deleted_depth_reached_again_interp() -> Result<()> {
    rec_deleted_depth_reached_again(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_deleted_depth_reached_again_par() -> Result<()> {
    rec_deleted_depth_reached_again(Mode::Par).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_reached_chain_leaves_nothing_below_interp() -> Result<()> {
    rec_partly_reached_chain_leaves_nothing_below(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_reached_chain_leaves_nothing_below_par() -> Result<()> {
    rec_partly_reached_chain_leaves_nothing_below(Mode::Par).await
}

/// A recursion a seq step calls is born again at each run, fused or not:
/// the activations its last run left are let go, so after many runs at one
/// depth the live activations are those one run holds in the node-walk.
async fn rec_seq_runs_hold_one_run(mode: Mode) -> Result<()> {
    for call in ["seqq let n = t { f(n) }", "seq let n = t { f(n) }"] {
        let (_, one) = run_rec(LINEAR, call, &[5], Mode::Interp).await?;
        let mut live = vec![];
        for runs in [1, 5, 30] {
            let (values, n) = run_rec(LINEAR, call, &vec![5; runs], mode).await?;
            assert_eq!(values.last(), Some(&15), "{call}: {runs} runs");
            live.push(n);
        }
        assert_eq!(live, [one; 3], "{call}: live after 1, 5 and 30 runs");
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rec_seq_runs_hold_one_run_interp() -> Result<()> {
    rec_seq_runs_hold_one_run(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_seq_runs_hold_one_run_jit() -> Result<()> {
    rec_seq_runs_hold_one_run(Mode::Jit).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_seq_runs_hold_one_run_jit_par() -> Result<()> {
    rec_seq_runs_hold_one_run(Mode::JitPar).await
}

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

run!(rec_transient_pure_refire, REC_TRANSIENT_PURE_REFIRE, prints("[i64:21, i64:34, i64:55]"); FuseExpect::Jit);

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

run!(rec_same_arg_refire_fires, REC_SAME_ARG_REFIRE_FIRES, is(Value::I64(3)); FuseExpect::Jit);

// Stateless builtins in a recursive body: f(4) = 7 per fire.
const REC_TRANSIENT_STATELESS_BUILTIN: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => str::len("abc"), _ => f(n - i64:1) + array::len([n])};
  array::group(f(go ~ i64:4), |n, _| n == 3)
}
"#;

run!(rec_transient_stateless_builtin, REC_TRANSIENT_STATELESS_BUILTIN, prints("[i64:7, i64:7, i64:7]"); FuseExpect::Jit);

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
}; FuseExpect::Jit);

// `x + i64:1` under `-> 'a: Number` is ill-typed for an arbitrary 'a.
const PARAM_KNOT_NO_LEAK: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + i64:1;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(param_knot_no_leak, PARAM_KNOT_NO_LEAK, refused("both operands must be one numeric type");
     FuseExpect::None);

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
     FuseExpect::None);

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
    is(Value::Bool(true));
    FuseExpect::None
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
    FuseExpect::None
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
    FuseExpect::Jit
);

// A bare-Array arg under a `[Array<i64>, null]` signature slot marshals
// as a Value.
const CALL_ARG_VALUE_SLOT_NARROW: &str = r#"
{
    let f = |v: [Array<i64>, null]| v;
    f([1, 2])
}
"#;

run!(call_arg_value_slot_narrow, CALL_ARG_VALUE_SLOT_NARROW, prints("[i64:1, i64:2]"));

// The result twin: a call result whose node type promises a Value is
// widened from the callee's raw array box.
const CALL_RESULT_VALUE_WIDEN_XKERNEL: &str = r#"
{
    let g = || [1];
    let f = |v: [null, Array<i64>]| v;
    f(g())
}
"#;

run!(call_result_value_widen_xkernel, CALL_RESULT_VALUE_WIDEN_XKERNEL, prints("[i64:1]"));

const CALL_RESULT_VALUE_WIDEN_HOF: &str = r#"
{
    let f = |v: [null, Array<i64>]| v;
    f(array::map([1, 2], |x| x + 1))
}
"#;

run!(call_result_value_widen_hof, CALL_RESULT_VALUE_WIDEN_HOF, prints("[i64:2, i64:3]"));

// String args and returns marshal cross-kernel.
const XKERNEL_STRING_ARG_RET: &str = r#"
{
    let f = |s: string| "[s]!";
    f("hello")
}
"#;

run!(xkernel_string_arg_ret, XKERNEL_STRING_ARG_RET, prints(r#""hello!""#));

// A string-returning callee feeding a union-typed param.
const XKERNEL_STRING_WIDEN: &str = r#"
{
    let g = || "abc";
    let f = |v: [null, string]| v;
    f(g())
}
"#;

run!(xkernel_string_widen, XKERNEL_STRING_WIDEN, prints(r#""abc""#));

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
    refused("too many positional arguments");
    FuseExpect::None
);

const SITE_IDENTITY_STATE: &str = r#"
{
  let f0 = |v: f64| -> f64 once(v);
  f0(f0(10.0) + 10.0)
}
"#;

// Two call sites of one callee own separate builtin instances: `once`
// at the outer site passes 20, its own first value.
run!(site_identity_state, SITE_IDENTITY_STATE, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(x)) => *x == 20.0,
        _ => false,
    }
}; FuseExpect::None);

const BUILTIN_SLOT_BACKEDGE: &str = r#"
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
run!(builtin_slot_backedge, BUILTIN_SLOT_BACKEDGE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
}; FuseExpect::Jit);

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
    refused("both operands must be one numeric type");
    FuseExpect::None
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
  "[f("foo")]"
}
"#;

run!(abstract_union_return_is_fine, ABSTRACT_UNION_RETURN_IS_FINE, |v: Result<
    &Value,
>| matches!(v, Ok(Value::String(s)) if s.as_str() == r#"[<"foo">]"#));

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
    refused("i64 does not contain");
    FuseExpect::None
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
    prints(r#"[[i64:1], ["two"]]"#)
);

// A collection callback's element goes to its positional parameter;
// labeled parameters take their defaults (the callback interprets).
const LABELED_CALLBACK_DEFAULT: &str =
    r#"array::map([i64:7], |#foo: i64 = i64:42, x| foo + x)"#;
run!(labeled_callback_default, LABELED_CALLBACK_DEFAULT, prints("[i64:49]"); FuseExpect::Jit);

// A callback with only labeled parameters has no slot for the element:
// a type error.
#[tokio::test]
async fn labeled_only_callback_is_compile_error() {
    let r =
        eval("array::map([i64:7], |#foo: i64 = i64:42| foo)", crate::TEST_REGISTER).await;
    let msg = match r {
        Err(e) => format!("{e:#}"),
        Ok((v, _)) => {
            panic!("a labeled-only callback must not satisfy fn(x: 'a) -> 'b, got {v:?}")
        }
    };
    assert!(msg.contains("does not contain fn(?#foo: i64)"), "{msg}");
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

run!(open_return_callee_in_callback, OPEN_RETURN_CALLEE_IN_CALLBACK, is(Value::I64(42)));

// A formal every self-call passes through unchanged is never rebound,
// so an fn-typed invariant formal does not gate the tail loop.

const FN_INVARIANT_TAIL_LOOP: &str = r#"
{
  let rec fold_go = |f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64
    select i {
      0 => acc,
      _ => fold_go(f, i - 1, f(acc, i))
    };
  #[native] fold_go(|a, x| a + x, 10, 0)
}
"#;

run!(fn_invariant_tail_loop, FN_INVARIANT_TAIL_LOOP, is(Value::I64(55)); FuseExpect::Jit);

// An invariant String formal is a kernel slot that is never rebound.
const STRING_INVARIANT_TAIL_LOOP: &str = r#"
{
  let rec label = |tag: string, n: i64, acc: i64| -> string
    select n {
      0 => "[tag]:[acc]",
      _ => label(tag, n - 1, acc + n)
    };
  #[native] label("sum", 10, 0)
}
"#;

run!(string_invariant_tail_loop, STRING_INVARIANT_TAIL_LOOP, prints(r#""sum:55""#); FuseExpect::Jit);

// Two call sites, one recursive helper, two different callbacks of
// identical type key two kernels (3008, not 3003 or 8008).
const FN_FORMAL_TWO_CALLBACKS: &str = r#"
{
  let rec ap = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n {
      0 => acc,
      _ => ap(f, n - 1, f(acc))
    };
  #[native] ap(|x| x + 1, 3, 0) * 1000 + ap(|x| x * 2, 3, 1)
}
"#;

run!(fn_formal_two_callbacks, FN_FORMAL_TWO_CALLBACKS, is(Value::I64(3008)); FuseExpect::Jit);

// A non-invariant fn formal (rebound by a self-call): the callbacks
// fuse, `g` does not.
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

run!(fn_formal_rebound, FN_FORMAL_REBOUND, is(Value::I64(1100)); FuseExpect::Jit);

// A helper forwarding its fn formal to another helper: the two
// forwarding instances key two kernels (110 vs 102 in the low part).
const FN_FORMAL_FORWARDED: &str = r#"
{
  let call1 = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x);
  let call2 = |f: fn(x: i64) -> i64, x: i64| -> i64 call1(f, x) + 100;
  #[native] call2(|x| x + 1, 1) * 1000 + call2(|x| x * 10, 1)
}
"#;

run!(fn_formal_forwarded, FN_FORMAL_FORWARDED, is(Value::I64(102110)); FuseExpect::Jit);

// A forwarded callback capturing an outer binding spelled like the
// callee's formal `n`: the tail rebind targets the formal by BindId.
const FN_FORMAL_CAPTURE_COLLIDES_BOUND: &str = r#"
{
  let n = 10;
  let rec ap = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
    select n { 0 => acc, _ => ap(f, n - 1, f(acc)) };
  #[native] ap(|x| n + 1, 3, 0)
}
"#;

run!(fn_formal_capture_collides_bound, FN_FORMAL_CAPTURE_COLLIDES_BOUND,
    is(Value::I64(11));
    FuseExpect::Jit);

// The same collision on the accumulator formal `acc`.
const FN_FORMAL_CAPTURE_COLLIDES_ACC: &str = r#"
{
  let acc = 5;
  let rec fold_go = |f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64
    select i { 0 => acc, _ => fold_go(f, i - 1, f(acc, i)) };
  #[native] fold_go(|a, x| a + acc + 1, 3, 0)
}
"#;

run!(fn_formal_capture_collides_acc, FN_FORMAL_CAPTURE_COLLIDES_ACC,
    is(Value::I64(18));
    FuseExpect::Jit);

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

run!(cps_wrapper_recursion, CPS_WRAPPER_RECURSION, is(Value::I64(3)); FuseExpect::Jit);

// A returned lambda called through an alias and directly: both calls
// dispatch at run time (the fn-valued `let` node-walks).
const RETURNED_LAMBDA_RESOLVES: &str = r#"
{
  let mk = |x: i64| |y: i64| x + y;
  let add1 = mk(i64:1);
  add1(i64:2) + mk(i64:10)(i64:20)
}
"#;

run!(returned_lambda_resolves, RETURNED_LAMBDA_RESOLVES, is(Value::I64(33)); FuseExpect::None);

// Loop-carried non-register formals: the tail rebind carries every
// kernel param kind.

const STRING_CARRIED_TAIL_LOOP: &str = r#"
{
  let rec go = |n: i64, acc: string| -> string
    select n { 0 => acc, _ => go(n - 1, "[acc].") };
  #[native] go(5, "x")
}
"#;

run!(string_carried_tail_loop, STRING_CARRIED_TAIL_LOOP, prints(r#""x.....""#); FuseExpect::Jit);

const VALUE_CARRIED_TAIL_LOOP: &str = r#"
{
  let rec go = |n: i64, m: Map<string, i64>| -> Map<string, i64>
    select n { 0 => m, _ => go(n - 1, map::insert(m, "k[n]", n)) };
  #[native] map::len(go(3, {}))
}
"#;

run!(value_carried_tail_loop, VALUE_CARRIED_TAIL_LOOP, is(Value::I64(3)); FuseExpect::Jit);

// A List tail bound by a non-scalar payload pattern is carried through
// the tail loop.
const LIST_CARRIED_FOLD: &str = r#"
{
  type L<'a> = [`C('a, L<'a>), `N];
  let rec fold_l = |l: L<i64>, acc: i64| -> i64
    select l { `N => acc, `C(x, rest) => fold_l(rest, acc + x) };
  #[native] fold_l(`C(1, `C(2, `C(3, `N))), 0)
}
"#;

run!(list_carried_fold, LIST_CARRIED_FOLD, is(Value::I64(6)); FuseExpect::Jit);

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
}; FuseExpect::None);

// A bottom argument to a tail self-call bottoms the formal, as it does
// for a non-tail call: the loop does not keep the previous value. `obs`
// is written only by a value, so it stays null while the result is
// bottom.
const TAIL_REBIND_CARRIES_BOTTOM: &str = r#"
{
  let rec f = |n: i64, x: i64, k: i64| -> i64 select n {
    0 => x,
    _ => f(n - 1, select n { m if m == k => null$, _ => x }, k)
  };
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let k = 2;
  k <- select step { 3 => 9, _ => never() };
  let r = #[native] f(3, 7, k);
  let obs: [i64, null] = null;
  obs <- r;
  let first: [i64, null] = never();
  first <- select step { 1 => obs, _ => never() };
  select step { 6 => (first, obs), _ => never() }
}
"#;

run!(tail_rebind_carries_bottom, TAIL_REBIND_CARRIES_BOTTOM, prints("[null, i64:7]"); FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::None);

const DEAD_VARIADIC_PLACED: &str = r#"
str::concat()
"#;

run!(dead_variadic_placed, DEAD_VARIADIC_PLACED, |v: Result<&Value>| {
    matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("never fires") && e.contains("in: str::concat()")
    })
}; FuseExpect::None);

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

run!(required_label_connect_refused, REQUIRED_LABEL_CONNECT, refused("and cannot hold fn(#a: i64, x: i64)"); FuseExpect::None);

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

async fn tail_quiet_bottom(code: &str, mode: Mode) -> Result<()> {
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), vec![6, 6]);
    Ok(())
}

async fn tail_quiet_bottom_call(mode: Mode) -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_CALL, mode).await
}

async fn tail_quiet_bottom_inline(mode: Mode) -> Result<()> {
    tail_quiet_bottom(TAIL_QUIET_BOTTOM_INLINE, mode).await
}

modes!(tail_quiet_bottom_call, tail_quiet_bottom_inline);

// A `let rec` annotation holds no function type with a variable of its
// own, as a plain `let` does: the lambda declares it instead.
const REC_ANNOTATION_TRAIT_PARAM: &str = r#"
{
  let rec f: fn(x: Display) -> string = |x| "a";
  f(1)
}
"#;

run!(
    rec_annotation_trait_param,
    REC_ANNOTATION_TRAIT_PARAM,
    refused("stands only as a definition's signature");
    FuseExpect::None
);

run!(rec_trait_param_on_the_lambda, r#"{
  let rec f = |x: Display| -> string "a";
  (f(1), f("s"))
}"#, prints(r#"["a", "a"]"#); FuseExpect::Jit);

// A `let` that shadows a builtin binding is not a builtin.
const BUILTIN_BINDING_SHADOWED: &str = r#"
{
  let f = |@args: [Number, Array<[Number, Array<Number>]>]| -> Number 'core_sum;
  let f = || -> i64 42;
  f()
}
"#;

run!(builtin_binding_shadowed, BUILTIN_BINDING_SHADOWED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::Jit);

// Only a builtin takes a variadic argument.
const USER_VARIADIC: &str = r#"
{
  let rec f = |@args: i64| -> i64 0;
  f(0, 0)
}
"#;

run!(user_variadic_refused, USER_VARIADIC, refused("is for builtins only"); FuseExpect::None);

// A type variable only data arguments hold widens to an argument wider
// than the ones before it, so two decided arguments settle it alike in
// either order.
const WIDEST_ARG_EITHER_ORDER: &str = r#"
{
  let n: [u64, null] = null;
  let f = |x: 'a, y: 'a| -> 'a y;
  (f(u64:0, n), f(n, u64:0))
}
"#;

run!(widest_arg_either_order, WIDEST_ARG_EITHER_ORDER, prints("[null, u64:0]"));

const LIST_CONS_WIDER_TAIL: &str = r#"
{
  let t: List<[u64, null]> = [<u64:1, null>];
  list::cons(u64:0, t)
}
"#;

run!(list_cons_wider_tail, LIST_CONS_WIDER_TAIL, prints("[u64:0, u64:1, null]"));

// Arguments are met in order and nothing waits: an argument that holds
// the ones after it settles the variable, and two neither of which holds
// the other are refused where the second stands, whatever follows.
const WIDEST_ARG_FIRST: &str = r#"
{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a y;
  let ab: [`A, `B] = `A;
  f(ab, `B, `A)
}
"#;

run!(widest_arg_first, WIDEST_ARG_FIRST, prints("\"B\""));

const WIDEST_ARG_LAST: &str = r#"
{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a y;
  let ab: [`A, `B] = `A;
  f(`A, `B, ab)
}
"#;

run!(widest_arg_last, WIDEST_ARG_LAST, prints("\"B\""); FuseExpect::Jit);

run!(
    widest_arg_last_types_the_result_refused,
    r#"{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a y;
  let ab: [`A, `B] = `A;
  let r: `B = f(`A, `B, ab);
  r
}"#,
    refused("`B does not contain `A");
    FuseExpect::None
);

// A lambda in parentheses is a lambda: its binding generalizes.
run!(
    parenthesized_lambda_generalizes,
    r#"{ let f = (|v| v * v); (array::map([u8:3], f), array::map([4], f)) }"#,
    prints("[[u8:9], [i64:16]]")
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
    prints("[i64:1, i64:4, null]")
);

// A generalized callback's result is at most its inferred shape: an
// annotation may widen it, never narrow it.
run!(
    generalized_callback_result_widens,
    r#"{ let f = |a| (a, u8:100); let v: Array<(i64, [u8, null])> = array::map([1], f); v }"#,
    prints("[[i64:1, u8:100]]")
);

run!(
    generalized_callback_result_does_not_narrow,
    r#"{ let f = |a| (a, null); let v: Array<(i64, u8)> = array::map([1], f); v }"#,
    refused("type mismatch u8 does not contain null");
    FuseExpect::None
);

// Neither argument holds the other: refused, in either order.
// CR claude for claude: [test-gap/low] "Refused, in either order" is pinned by one
// program, and the check reports only its first refusal. The message here is f(`A,
// `B)'s, and the same program with the second call replaced by an accepted f(`B, `B)
// gives the same message. A change that accepted f(`B, `A), an order-dependent verdict,
// would still pass. Split this into two fixtures, each pinning its own message: "'a: `A
// does not contain `B" and "'a: `B does not contain `A". probe:
// design/review-2026-11-16/repro/tests-functions-a-either-order-half-pinned.sh
// (tests-functions-a-03)
run!(
    no_widest_arg_widens_to_both,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; (f(`A, `B), f(`B, `A)) }"#,
    prints("[\"B\", \"A\"]");
    FuseExpect::Jit
);

// The call's type is the widest argument's.
run!(
    widest_arg_types_the_result,
    r#"{ let n: [u64, null] = null; let f = |x: 'a, y: 'a| -> 'a y; let r: u64 = f(u64:0, n); r }"#,
    refused(r#"type mismatch u64 does not contain [u64, null]"#);
    FuseExpect::None
);

// An argument whose type is undecided takes its formal as the arguments
// before it left it. First, it decides the variable with what follows;
// after a decided one it must fit that, and the refusal asks for an
// annotation.
run!(
    undecided_argument_first_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; f(str::parse("5"), 1) }"#,
    refused("must be known here, an operation reads it to know what to make")
);

run!(
    undecided_argument_last_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; f(1, str::parse("5")) }"#,
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

run!(
    undecided_binding_first_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; let p = str::parse("5"); f(p, 1) }"#,
    refused("must be known here, an operation reads it to know what to make")
);

run!(
    undecided_binding_last_named,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; let p = str::parse("5"); f(1, (p, 0).0) }"#,
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

run!(
    annotated_argument_joins_either_order,
    r#"{
  let f = |x: 'a, y: 'a| -> 'a y;
  let p: [i64, Error<`ParseError(string)>] = str::parse("5");
  (f(p, 1), f(1, p))
}"#,
    prints("[i64:1, i64:5]")
);

// An instance holds a variable another's bound names at that variable's own
// place, wider than the bound's holder alone would decide it.
run!(
    bound_naming_a_wider_variable_builds,
    r#"{
  let f = 'b: Array<'a> |x: 'b, y: 'a| y;
  let g = 'a: Any, 'b: Array<'a> |x: 'b, y: 'a| x;
  let e: [i64, string] = 1;
  (f([2], e), g([2], e))
}"#,
    prints("[i64:1, [i64:2]]")
);

// The variable the bound names stands among a union's members, where it has
// no place: its formal decides it before the bound's holder is placed.
run!(
    bound_naming_a_wider_variable_in_a_union_builds,
    r#"{
  let f = 'b: Array<'a> |x: 'b, y: ['a, null]| y;
  let g = 'b: Array<'a> |x: ['b, null], y: Array<['a, Error<`E>]>| (x, y);
  let e: [i64, string] = 1;
  (f([2], e), g([2], [e]), f([e], 2))
}"#,
    prints("[i64:1, [[i64:2], [i64:1]], i64:2]")
);

// A waiting argument whose cell stands above lower bounds takes what the
// others settle only where that holds them.
// CR claude for claude: [test-gap/low] The note this pins, "annotate `v`", cannot make
// this program pass: with `let v: [i64, string] = x.0` the program is refused at `let
// r: i64` instead. undecided_binding_above_a_lower_bound_builtin_refused has the same
// problem with `Array<i64>`. Nothing pins the passing version (no `let r`, `v: [i64,
// string]`, accepted), here or in the generics.md cases, so a check that kept refusing
// the annotated read would pass every test. Drop the `let r` annotations from both
// fixtures (they get the same refusal without them) and add the annotated program as an
// accepting fixture. probe:
// design/review-2026-11-16/repro/tests-functions-a-lower-bound-twin-unpinned.sh
// (tests-functions-a-04)
run!(
    undecided_binding_above_a_lower_bound_refused,
    r#"{
  type B<'p> = Abstract<'p>;
  let x = B(1);
  x <- B("a");
  let v = x.0;
  let f = |a: Array<'a>, b: 'a| b;
  let r: i64 = f([10], v);
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    undecided_binding_above_a_lower_bound_builtin_refused,
    r#"{
  type B<'p> = Abstract<'p>;
  let x = B(1);
  x <- B("a");
  let v = x.0;
  let r: Array<i64> = array::push([10], v);
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    undecided_binding_above_a_lower_bound_it_holds_joins,
    r#"{
  type B<'p> = Abstract<'p>;
  let x = B(1);
  let v = x.0;
  let f = |a: Array<'a>, b: 'a| b;
  let n: [i64, null] = null;
  let r: i64 = f([10], v);
  (r, f([n], v))
}"#,
    prints("[i64:1, i64:1]")
);

// So does one whose cell an operator left a conjunct on: what the others
// settle must fit it, whichever the call, and whatever stands between.
// CR claude for claude: [bug/medium] This fixture,
// undecided_parameter_within_a_conjunct_builtin_refused and
// undecided_binding_within_a_conjunct_refused pin the note "`p` is undecided at this
// call; annotate `p`", and undecided_index_within_a_conjunct_refused prints it too. No
// annotation can satisfy it: `p: string`, `p: i64`, `p: [i64, string]` and `p: Any` are
// each refused, because the operator's conjunct (Number & Singleton) holds no type that
// holds, or is held by, the string the call gave 'a. That breaks the rule
// undecided_mismatch_names_no_annotation pins (a refusal that no annotation of the
// undecided argument can pass names none). Fix callsite.rs::annotation_may_fit, which
// Widening::take consults, so the note needs a type the argument's cell admits, its
// conjunct included, that relates to what the call gave the variable. Then pin these
// with refused_naming_nothing and update the matching generics.md case. probe:
// design/review-2026-11-16/repro/tests-functions-a-conjunct-note-unfollowable.sh
// (tests-functions-a-01)
run!(
    undecided_parameter_within_a_conjunct_refused,
    r#"{
  let g = |p, q| {
    let s = p + q;
    let f = |a: Array<'a>, b: 'a| b;
    (s, f(["s"], p))
  };
  g("a", "b")
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);

run!(
    undecided_parameter_within_a_conjunct_builtin_refused,
    r#"{
  let g = |p, q| {
    let s = -p;
    (s, array::push(["s"], p))
  };
  g("a", "b")
}"#,
    refused("type mismatch [Real, Sint] does not contain string");
    FuseExpect::None
);

run!(
    undecided_index_within_a_conjunct_refused,
    r#"{
  let g = |p| {
    let a = [1, 2, 3];
    (a[p], array::push([1.5], p))
  };
  g(1.5)
}"#,
    refused("type mismatch Int does not contain f64");
    FuseExpect::None
);

run!(
    undecided_binding_within_a_conjunct_refused,
    r#"{
  let v = never();
  let s = v + v;
  let r = array::push(["s"], v);
  v <- "x";
  (s, r)
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);

run!(
    undecided_parameter_within_a_conjunct_widens_to_a_wider_union,
    r#"{
  let g = |p, q| {
    let s = p + q;
    let n: [i64, null] = null;
    let f = |a: Array<'a>, b: 'a| b;
    (s, f([n], p))
  };
  g(1, 2)
}"#,
    prints("[i64:3, i64:1]");
    FuseExpect::Jit
);

// undecided_parameter_within_a_conjunct_widens_to_a_wider_union (was
// numeric type.
run!(
    undecided_parameter_within_a_conjunct_wider_union_under_arithmetic_refused,
    r#"{
  let g = |p, q| {
    let s = p + q;
    let n: [i64, null] = null;
    let f = |a: Array<'a>, b: 'a| b;
    (s, f([n], p) + q)
  };
  g(1, 2)
}"#,
    refused("Number does not contain [i64, null]");
    FuseExpect::None
);

run!(
    undecided_parameter_within_a_conjunct_annotated_joins,
    r#"{
  let g = |p: i64, q| {
    let s = p + q;
    let n: [i64, null] = null;
    let f = |a: Array<'a>, b: 'a| b;
    (s, f([n], p))
  };
  g(1, 2)
}"#,
    prints("[i64:3, i64:1]")
);

run!(
    undecided_parameter_within_a_conjunct_it_fits_joins,
    r#"{
  let h = 'a: Number |a: Array<'a>, b: 'a| b;
  let g = |p, q| {
    let a = [1, 2, 3];
    let s = p + q;
    let f = |a: Array<'a>, b: 'a| b;
    (s, a[q]$, f([1], p), h([1], q), array::push([1], q))
  };
  g(1, 2)
}"#,
    prints("[i64:3, i64:3, i64:1, i64:2, [i64:1, i64:2]]")
);

// A copy is held to the bounds its argument brought where an undecided
// argument before it gave the formal's cell a conjunct of its own and
// another's parts then decided that cell.
run!(
    undecided_parameter_within_a_conjunct_after_two_waiters_refused,
    r#"{
  let g = |p, q, y| {
    let a = [1, 2, 3];
    let s = p + p;
    let f = |u: 'a, v: ('a, 'b), w: 'a| w;
    (s, a[q], f(p, (1.5, y), q))
  };
  g(2.5, 1.5, 0)
}"#,
    refused("type mismatch Int does not contain f64");
    FuseExpect::None
);

run!(
    undecided_parameter_within_a_conjunct_after_two_waiters_it_fits_joins,
    r#"{
  let g = |p, q, y| {
    let a = [1, 2, 3];
    let s = p + p;
    let f = |u: 'a, v: ('a, 'b), w: 'a| w;
    (s, a[q]$, f(p, (1, y), q))
  };
  g(2, 1, 0)
}"#,
    prints("[i64:4, i64:2, i64:1]")
);

// A variable a callback also holds is decided by the first argument: an
// undecided argument after it is refused naming the argument that decided
// it, one before it runs, and so does the deciding argument annotated wide.
run!(
    callback_variable_undecided_first_refused,
    r#"{ let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(str::parse("5"), 1, |v| 0) }"#,
    refused("must be known here, an operation reads it to know what to make")
);

run!(
    callback_variable_undecided_last_refused,
    r#"{ let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, str::parse("5"), |v| 0) }"#,
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

run!(
    callback_variable_annotated_argument_decides_no_parse,
    r#"{
  let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y;
  let one: [i64, Error<`ParseError(string)>] = 1;
  f(one, str::parse("5"), |v| 0)
}"#,
    refused("must be known here, an operation reads it to know what to make")
);

// The note is only where a wider decision of the variable could hold the
// argument: a formal with no variable, or one no decision makes hold it,
// names nothing.
run!(
    undecided_at_closed_formal_names_nothing,
    r#"{ let f = |x: i64| x; f(str::parse("5")) }"#,
    refused_naming_nothing;
    FuseExpect::None
);

run!(
    callback_variable_unrelated_undecided_widens,
    r#"{ let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, [], |v| 0) }"#,
    prints("[]");
    FuseExpect::None
);

run!(
    seq_step_tuple_read_of_a_lambda_is_generic,
    r#"{ let t = 1; seq t { let v = (|x| x, 1).0; (v(1), v("s")) } }"#,
    prints("[i64:1, \"s\"]");
    FuseExpect::Jit
);

run!(
    seqq_step_select_of_lambdas_widens,
    r#"{ let t = 1; seqq t { let v = select true { true => |x| x, false => |y| y }; (v(1), v("s")) } }"#,
    prints("[i64:1, \"s\"]");
    FuseExpect::Jit
);

run!(
    let_rec_self_call_mismatch_names_nothing,
    r#"{
  let rec f = |n, acc| select n { 0 => acc, _ => f(buffer::to_string(buffer::from_string("hello")), acc + n) };
  f(i64:100, i64:0)
}"#,
    refused_naming_nothing;
    FuseExpect::None
);

// An undecided argument at a callback formal is refused for its own
// bounds: no decision of the callback's variable fits it, so the refusal
// names nothing, however the formal wraps the function type.
run!(
    undecided_at_callback_formal_names_nothing,
    r#"{ let f = |x: 'a, k: fn(v: 'a) -> i64| -> 'a x; f(1, { let v = str::parse("42")?; v }) }"#,
    refused("fn(v: i64) -> i64 is not data");
    FuseExpect::None
);

run!(
    undecided_at_aliased_callback_formal_names_nothing,
    r#"{
  type K<'a> = fn(v: 'a) -> i64;
  let f = |x: 'a, k: K<'a>| -> 'a x;
  f(1, { let v = str::parse("42")?; v })
}"#,
    refused("K<i64> is not data");
    FuseExpect::None
);

run!(
    undecided_at_callback_field_formal_names_nothing,
    r#"{ let f = |x: 'a, k: {g: fn(v: 'a) -> i64}| -> 'a x; f(1, { let v = str::parse("42")?; v }) }"#,
    refused("{ g: fn(v: i64) -> i64 } is not data");
    FuseExpect::None
);

run!(
    undecided_argument_takes_the_closed_join,
    r#"{ let s = "5"; let d = opt::or_default(str::parse(s)$, 0); d + 1 }"#,
    prints("i64:6")
);

// Two undecided arguments at one variable: the second meets what the
// first gave it.
run!(
    two_undecided_arguments_first_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; f(str::parse("5"), str::parse("6")$) }"#,
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

run!(
    two_undecided_arguments_last_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a x; f(str::parse("6")$, str::parse("5")) }"#,
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

// An omitted labeled default joins the other arguments as written.
run!(
    omitted_default_joins_as_written,
    r#"{
  let f = |#x: Array<'a> = [1, null], y: 'a| -> 'a y;
  (f(1), f(#x: [1, null], 1))
}"#,
    prints("[i64:1, i64:1]")
);

run!(
    omitted_default_widens_the_result,
    r#"{ let f = |#x: Array<'a> = [1, null], y: 'a| -> 'a y; let r: i64 = f(1); r }"#,
    refused("type mismatch i64 does not contain null");
    FuseExpect::None
);

// An omitted default is met after the written arguments, as one written
// last: neither wider nor narrower than what they gave the variable, it
// is the mismatch.
run!(
    omitted_incomparable_default_widens,
    r#"{ let f = |#a: 'a = "s", y: 'a| -> 'a y; f(1) }"#,
    prints("i64:1");
    FuseExpect::Jit
);

// An omitted default holding an undecided cell takes the variable as the
// arguments and the defaults before it left it, and the variable widens
// no more: a wider default declared after it is refused.
run!(
    omitted_undecided_default_takes_what_stands,
    r#"{
  let u = str::parse("abc")$;
  let n: [i64, string] = 2;
  let f = |#b: 'a = n, #a: 'a = u, y: 'a| -> 'a y;
  (f(1), u)
}"#,
    prints(r#"[i64:1, "abc"]"#)
);

run!(
    omitted_undecided_default_before_a_wider_one_refused,
    r#"{
  let u = str::parse("abc")$;
  let n: [i64, string] = 2;
  let f = |#a: 'a = u, #b: 'a = n, y: 'a| -> 'a y;
  (f(1), u)
}"#,
    refused("an undecided argument before this one met 'a");
    FuseExpect::None
);

run!(
    omitted_undecided_default_takes_what_stands_before_a_later_write,
    r#"{
  let u = never();
  let n: [i64, null] = null;
  let f = |#b: 'a = n, #a: 'a = u, y: 'a| -> 'a y;
  let r = f(1);
  u <- null;
  r
}"#,
    prints("i64:1")
);

// An omitted default's refusal names the undecided bindings it reads, as
// the same default written does.
run!(
    omitted_undecided_default_is_decided_by_a_later_write,
    r#"{
  let u = never();
  let f = |#x: Array<'a> = [u, null], y: 'a| -> 'a y;
  let r = f(1);
  u <- 3;
  r
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    omitted_annotated_default_joins,
    r#"{
  let u: [i64, null] = never();
  let f = |#x: Array<'a> = [u, null], y: 'a| -> 'a y;
  let r = f(1);
  u <- 3;
  r
}"#,
    prints("i64:1")
);

// A lambda's open cells are not undecided data: an argument holding one
// beside closed data meets its formal at once.
run!(
    lambda_beside_closed_data_joins,
    r#"{
  let n: [i64, null] = null;
  let f = |x: {a: 'a, k: fn(x: i64) -> i64}, y: 'a| -> 'a y;
  let g = |x: ('a, fn(x: 'b) -> 'b), y: 'a| -> 'a y;
  let h = |#p: ('a, fn(x: 'b) -> 'b) = (n, |x| x), y: 'a| -> 'a y;
  let s = {a: n, k: |x| x};
  (f({a: n, k: |x: i64| -> i64 x}, 1), f({a: n, k: |x| x}, 1), f(s, 1), g((n, |x| x), 1),
   h(#p: (n, |x| x), 1))
}"#,
    prints("[i64:1, i64:1, i64:1, i64:1, i64:1]");
    FuseExpect::None
);

// A waiting argument decides at once the variables of its formal that do
// not widen, so a callback after it reads them.
run!(
    waiting_argument_decides_its_callback_variable,
    r#"{
  let f = |x: {p: 'a, q: 'c}, y: 'a, k: fn(v: 'c) -> string| -> ('a, string) (y, k(x.q));
  let g = |x: Array<{p: 'a, q: 'c}>, y: 'a, k: fn(v: 'c) -> string| -> 'a y;
  (f({p: [], q: {name: "s"}}, [2], |v| v.name), g([{p: [], q: {name: "t"}}], [3], |v| v.name))
}"#,
    prints(r#"[[[i64:2], "s"], [i64:3]]"#)
);

/// A refusal that asks for no annotation.
fn refused_naming_nothing(v: Result<&Value>) -> bool {
    matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("does not contain") && !e.contains("annotate")
    })
}

// A refusal no annotation of the undecided argument passes names none,
// nor does one of an argument undecided only inside a function type.
run!(
    undecided_argument_of_another_shape_widens,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; f(1, []) }"#,
    prints("[]");
    FuseExpect::None
);

run!(
    closed_arguments_of_two_struct_types_widen,
    r#"{
  let f = |x: 'a, y: 'a| -> 'a y;
  let k = |v: i64| -> i64 v;
  let w: [i64, null] = null;
  f({n: 1, k}, {n: w, k}).n
}"#,
    prints("null");
    FuseExpect::None
);

// A variable a callback holds does not widen: the callback was checked
// at the type the variable had when it was reached.
// CR claude for claude: [bug/low] This refusal names no annotation, yet annotating the
// first argument as `[u64, null]` (`let z: [u64, null] = u64:0; g(z, |v| v, n)`)
// passes, and no fixture pins that. The undecided version
// (callback_variable_undecided_last_refused) gets "annotate that argument as wide as
// this one" from callsite.rs::decided_elsewhere, and a bound-held variable gets
// callsite.rs::bound_held_note. A decided argument at a variable a function type of the
// signature holds gets neither, because Widening::join adds only bound_held_note or
// pinned_note. writable_reference_variable_does_not_widen has the same shape. Add the
// note in Widening::join, pin it in both fixtures, and add the annotated programs as
// accepting twins. probe:
// design/review-2026-11-16/repro/tests-functions-a-callback-held-refusal-names-nothing.sh
// (tests-functions-a-02)
run!(
    callback_variable_widens,
    r#"{
  let n: [u64, null] = null;
  let g = |x: 'a, f: fn(v: 'a) -> 'a, y: 'a| -> 'a f(y);
  g(u64:0, |v| v, n)
}"#,
    prints("null");
    FuseExpect::Jit
);

run!(
    writable_reference_variable_does_not_widen,
    r#"{
  let r: &mut u64 = &mut u64:0;
  let n: &mut [u64, null] = &mut null;
  let f = |x: &mut 'a, y: &mut 'a| -> &mut 'a y;
  f(r, n)
}"#,
    refused("type mismatch u64 does not contain [u64, null]");
    FuseExpect::None
);

// What a formal's walk finds does not depend on the formals before it:
// `D<'b, 'a>` reaches `A<'a>` inside `D`'s own expansion, where an earlier
// `A<'a>` formal walks it outside, and either order of the two settles
// `'a` alike.
const RECURSIVE_TYPEDEFS: &str = "
  type A<'y> = [`A(D<'y, i64>), null];
  type D<'x, 'y> = [`D('x), `F(A<'y>), null];
  let a: A<i64> = null;
  let s: [i64, string] = 1;";

run!(
    widening_through_recursive_typedefs_after_their_part,
    format!(
        "{{ {RECURSIVE_TYPEDEFS} let d: D<string, i64> = null;
  let f = |y: A<'a>, x: D<'b, 'a>, z: 'a| -> 'a z;
  f(a, d, s) }}"
    ),
    prints("i64:1");
    FuseExpect::None
);

run!(
    widening_through_recursive_typedefs_before_their_part,
    format!(
        "{{ {RECURSIVE_TYPEDEFS} let d: D<string, i64> = null;
  let f = |x: D<'b, 'a>, y: A<'a>, z: 'a| -> 'a z;
  f(d, a, s) }}"
    ),
    prints("i64:1");
    FuseExpect::None
);

run!(
    argument_order_through_recursive_typedefs_ignores_formal_order,
    format!(
        "{{ {RECURSIVE_TYPEDEFS} let d: D<string, [i64, string]> = null;
  let f = |y: A<'a>, x: D<'b, 'a>, h: fn(v: 'a) -> i64| -> i64 1;
  let g = |x: D<'b, 'a>, y: A<'a>, h: fn(v: 'a) -> i64| -> i64 2;
  (f(a, d, |v| 1), g(d, a, |v| 1)) }}"
    ),
    prints("[i64:1, i64:2]")
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

run!(wrapped_poly_ref_twice, WRAPPED_POLY_REF_TWICE, prints(r#"[[i64:1, i64:1], ["s"], [i64:2]]"#); FuseExpect::None);

const FORWARDED_POLY_REF_TWICE: &str = r#"
{
  let t = array::flat_map;
  let a = filter((t), |x| true);
  let b = filter({ let z = 1; t }, |x| true);
  (a([1], |x| [x, x]), b(["s"], |x| [x]))
}
"#;

run!(forwarded_poly_ref_twice, FORWARDED_POLY_REF_TWICE, prints(r#"[[i64:1, i64:1], ["s"]]"#); FuseExpect::None);

// `flat_map`'s callback returns the collection, never a bare element:
// whether to splice is never decided by a value's shape.
const FLAT_MAP_BARE_ELEMENT: &str = r#"
{
  let t: fn(a: Array<i64>, f: fn(x: i64) -> [i64, Array<i64>]) -> Array<i64> = array::flat_map;
  t([1, 2], |x| select x { 1 => x, n => [n, n] })
}
"#;

run!(flat_map_bare_element_refused, FLAT_MAP_BARE_ELEMENT, refused("type mismatch Array<'b: unbound> does not contain i64"); FuseExpect::None);

// A tuple callback result is refused.
const FLAT_MAP_TUPLE_REFUSED: &str = r#"
{
  let r = array::flat_map([1, 2], |x| (x, x * 10));
  r
}
"#;

run!(flat_map_tuple_refused, FLAT_MAP_TUPLE_REFUSED,
    refused("type mismatch Array<'b: unbound> does not contain (i64, i64)"); FuseExpect::None);

// A binding that is not generalized holds one instance: used twice, the
// second use meets cells the first linked. Unwritten, it is generic.
const WRITTEN_CALLBACK_IS_ONE_INSTANCE: &str = r#"
{
  let t = |x| [x, x];
  t <- never();
  (array::flat_map([1], t), array::flat_map(["s"], t))
}
"#;

run!(written_callback_is_one_instance, WRITTEN_CALLBACK_IS_ONE_INSTANCE,
    prints("[[i64:1, i64:1], [\"s\", \"s\"]]"); FuseExpect::None);

run!(a_generic_value_written_to_a_generic_binding, r#"
{
    let f = |x| x;
    let a = f("s");
    let h = |y| y;
    f <- h;
    f <- |z| z;
    str::len(a)
}
"#, is(Value::I64(1)); FuseExpect::Jit);

const UNWRITTEN_CALLBACK_IS_GENERIC: &str = r#"
{
  let t = |x| [x, x];
  (array::flat_map([1], t), array::flat_map(["s"], t))
}
"#;

run!(
    unwritten_callback_is_generic,
    UNWRITTEN_CALLBACK_IS_GENERIC,
    prints(r#"[[i64:1, i64:1], ["s", "s"]]"#)
);

// A callback typed body-first keeps a cell constrained to an array;
// `` [`Keep('b), null] `` holds it through `'b`.
const EXTRACTED_FILTER_MAP_CALLBACK: &str = r#"
{
  let tm = |x| `Keep([x, x]);
  array::filter_map([1, 2], tm)
}
"#;

run!(
    extracted_filter_map_callback,
    EXTRACTED_FILTER_MAP_CALLBACK,
    prints("[[i64:1, i64:1], [i64:2, i64:2]]")
);

// A callback's argument type holding another function's `'b` stays apart
// from the collection's own `'b`: type variables are cells, not names.
const SAME_NAMED_TVAR_IN_CALLBACK_ARG: &str = r#"
{
  let g = |x: 'b| x;
  let a = array::map([100, g, 3], |x| [x, 1]);
  let b = array::filter_map([100, array::find_map, 3], |x| `Keep([x, 1]));
  (array::len(a), array::len(b))
}
"#;

run!(same_named_tvar_in_callback_arg, SAME_NAMED_TVAR_IN_CALLBACK_ARG, prints("[i64:3, i64:3]"); FuseExpect::None);

// A call through any expression that reaches a parameter (parens, an
// alias) types against the definition's own cells: an open gate's
// signature is not generalized yet.
const PARAM_CALLED_THROUGH_AN_EXPRESSION: &str = r#"
{
  let m = |a, f: fn(x: 'a) -> 'b| -> Array<'b> array::map(a, |v| ((f))(v));
  let n = |a, f: fn(x: 'a) -> 'b| -> Array<'b> { let g = f; array::map(a, |v| g(v)) };
  (m([1, 2], |x| x * 2), n(["a"], |s| "[s]!"))
}
"#;

run!(
    param_called_through_an_expression,
    PARAM_CALLED_THROUGH_AN_EXPRESSION,
    prints(r#"[[i64:2, i64:4], ["a!"]]"#);
    FuseExpect::Jit
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

// CR claude for claude: [test-gap/low] The value is the constant 42 and nothing reads
// `y`'s type, while the check accepts an undecided `y`. A definition that generalized
// `y`'s cell would leave `y` undecided and still yield 42, so this test cannot fail for
// the rule it names; only environment_cell_makes_the_definition_monomorphic catches
// that. Add `let z: string = y;` and pin refused("string does not contain i64"); an
// undecided `y` is refused with "unbound within Number & Singleton" instead. Or delete
// the fixture as a duplicate. probe:
// design/review-2026-11-16/repro/tests-functions-a-environment-cell-unobserved.sh
// (tests-functions-a-05)
run!(environment_cell_is_not_generalized, ENVIRONMENT_CELL_IS_NOT_GENERALIZED, is(Value::I64(42)); FuseExpect::Jit);

// So that definition is monomorphic: a second call at another type is
// refused where it is made.
run!(
    environment_cell_makes_the_definition_monomorphic,
    refused("cannot compute with [i64, f64]: both operands must be one numeric type"),
    "/test.gx" => r#"
        let y = array::iter(never());
        let t = |x| x + y;
        let result = (t(1), t(2.0))
    "#
; FuseExpect::None);

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

run!(closed_definitions_are_polymorphic, CLOSED_DEFINITIONS_ARE_POLYMORPHIC, prints(r#"[[[i64:1, f64:1.], ["s", f64:1.]], [[i64:1, true], ["s", true]], i64:1, "s", i64:2, "t"]"#); FuseExpect::Jit);

// A definition whose gate has not run yet (a submodule the interface
// declares first calls into its parent) is called at its declared
// scheme: each call copies its cells.
run!(
    call_before_the_definition_is_checked,
    prints(r#"[i64:1, "s"]"#),
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
; FuseExpect::Jit);

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
; FuseExpect::None);

// So is an annotation that would narrow it, and a concrete container.
run!(
    rigid_under_a_concrete_annotation,
    refused("i64 does not contain 'r"),
    "/test.gx" => r#"
        let f = |x: 'r| { let y: i64 = x; y };
        let result = 0
    "#
; FuseExpect::None);

run!(
    rigid_element_to_a_concrete_container,
    refused("type mismatch i64 does not contain 'r"),
    "/test.gx" => r#"
        let g = |a: Array<i64>| 3;
        let f = |x: Array<'r>| g(x);
        let result = 0
    "#
; FuseExpect::None);

// What holds 'r for every 'r still does: a union naming it, Any, or a
// constraint the parameter accepts.
run!(
    rigid_where_every_choice_fits,
    is(Value::I64(3)),
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
    refused("type mismatch string does not contain bytes"),
    "/test.gx" => r#"
        let f: fn(x: 'a) -> 'a = buffer::from_string;
        let result = 0
    "#
; FuseExpect::None);

// A function value prints as its source (cold and warm alike:
// lang::image::a_restored_function_prints_its_source).
const FN_PRINTS_ITS_SOURCE: &str = r#"
{
  let f = |acc, x| str::len(x) + acc;
  "[f]"
}
"#;

run!(fn_prints_its_source, FN_PRINTS_ITS_SOURCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.as_str() == "Abstract(|acc, x| str::len(x) + acc)")
}; FuseExpect::None);

// A `let` holds no value: it is a statement, never an element.
const LET_IN_VALUE_POSITION: &str = r#"
{
  let a = "s";
  let t = (let a = 3, 1);
  a
}
"#;

run!(let_in_value_position, LET_IN_VALUE_POSITION, refused("a let binding is not an expression"); FuseExpect::None);

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
    prints(r#"["ok", "ok"]"#)
);

// A binding that aliases a lambda resolves statically, annotated or not.
const LAMBDA_ALIAS_NATIVE: &str = r#"
{
  let f = |i: i64| i + 1;
  let h = f;
  let g: fn(i: i64) -> i64 = f;
  #[native]
  (h(1) + g(2))
}
"#;

run!(lambda_alias_native, LAMBDA_ALIAS_NATIVE, is(Value::I64(5)));

// An alias of a generalized function stays generic, parenthesized or not.
run!(
    parenthesized_alias_stays_generic,
    r#"{ let g = |x| x; let f = (g); (array::map([1], f), array::map(["s"], f)) }"#,
    prints(r#"[[i64:1], ["s"]]"#)
);

// A default that calls its own function and omits its own label is
// checked and built once: its inner call binds at run time, if it is
// ever taken.
run!(
    self_calling_default_checks_once,
    r#"{
    let n = 0;
    let rec f = |#a = select n > 0 { true => f(1), false => 0 }, b: i64| -> i64 a + b;
    f(1)
}"#,
    is(Value::I64(1))
);

// Functions gathered in an array, a map or an `any` meet, as a select's
// alternatives do: a call through one accepts each alike.
run!(
    gathered_functions_meet,
    r#"{
    let fs = [|x| x, |x| x + 1];
    let m = {"a" => |x| x, "b" => |x| x + 1};
    let g = m{"a"}$;
    let h = any(|x| x, |x| x + 1);
    let ks = [|x: i64| x * 2, |x| x];
    (array::map(fs, |f| f(1)), g(1), h(1), array::map(ks, |k| k(5)))
}"#,
    prints("[[i64:1, i64:2], i64:1, i64:1, [i64:10, i64:5]]");
    FuseExpect::None
);

// An alias a `<-` writes stays a dynamic call and follows the write.
const LAMBDA_ALIAS_WRITTEN: &str = r#"
{
  let f = |i: i64| i + 1;
  let h = f;
  h <- |i: i64| i * 10;
  filter(h(2), |v| v == 20)
}
"#;

run!(lambda_alias_written, LAMBDA_ALIAS_WRITTEN, is(Value::I64(20)); FuseExpect::None);

// A labeled default is its expression, read in the callee: at a callee's
// first dispatch after its root's first cycle it stands, so a sample on
// it has nothing to take, at an arm's first selection as at a rebind. The
// callee dispatched in the root's first cycle samples it.
async fn default_stands_at_a_later_first_dispatch(mode: Mode) -> Result<()> {
    let code = r#"{
        let d = 5;
        let f = |#k = d, x: i64| k ~ x;
        select array::iter([0, 1]) { 0 => 0, _ => f(10) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [0]);
    let code = r#"{
        let t = array::iter([0, 1]);
        let d = 5;
        let g = |#k = d, x: i64| k ~ x;
        g <- select t { 1 => |#k = d, x: i64| k ~ (x + 1), _ => never() };
        g(10)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [10]);
    Ok(())
}

modes!(default_stands_at_a_later_first_dispatch);

// A fresh bind runs each omitted default once: a counting default in a
// collection callback reads 1 in every slot, in every mode.
const DEFAULT_RUNS_ONCE_PER_BIND: &str = r#"
{
    let f = |#n = count(1), x: i64| (x, n);
    array::map([1, 2, 3], f)
}
"#;

run!(default_runs_once_per_bind, DEFAULT_RUNS_ONCE_PER_BIND, |v: Result<&Value>| {
    let pair = |x: i64| Value::Array([Value::I64(x), Value::I64(1)].into_iter().collect());
    matches!(v, Ok(Value::Array(a)) if a[..] == [pair(1), pair(2), pair(3)])
}; FuseExpect::Jit);

// A default that calls a trait method resolves in its definition's
// environment and is elaborated at every bind: a slot's and a recursive
// activation's.
const TRAIT_CALL_IN_A_DEFAULT: &str = r#"
{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "int [x]" };
    let f = |#s = Show::show(5), x: i64| "[s]:[x]";
    let rec g = |#s = Show::show(5), x: i64| -> string select x { 0 => s, x => g(x - 1) };
    (array::map([1, 2], f), g(#s: "top", 1))
}
"#;

run!(trait_call_in_a_default, TRAIT_CALL_IN_A_DEFAULT, |v: Result<&Value>| {
    let s = |s: &str| Value::String(s.into());
    matches!(v, Ok(Value::Array(a))
        if a[0] == Value::Array([s("int 5:1"), s("int 5:2")].into_iter().collect())
            && a[1] == s("int 5"))
}; FuseExpect::Jit);

// A variable widens to each wider argument in turn, a formal holding two
// of them included.
const WIDEST_ARGUMENT_ANY_ORDER: &str = r#"
{
    let g = |a: ('x, 'y), b: ('x, 'y), c: ('x, 'y)| (a, b, c);
    let i: [i64, f64] = 1;
    g((1, 1), (i, 2), (3, i))
}
"#;

run!(widest_argument_any_order, WIDEST_ARGUMENT_ANY_ORDER, prints("[[i64:1, i64:1], [i64:1, i64:2], [i64:3, i64:1]]"); FuseExpect::Jit);

// A default omitted through a fn type that names the label is judged by
// that type: the call runs the instance with the fn type's parameter, so
// a callee's wider parameter does not hold it there.
const DEFAULT_THROUGH_A_NARROWER_FN_TYPE: &str = r#"
{
    let g = |#n: [i64, string] = "hello", x: i64| -> i64 x + 1;
    let h = |f: fn(?#n: i64, x: i64) -> i64| f(1);
    h(g)
}
"#;

run!(default_through_a_narrower_fn_type, DEFAULT_THROUGH_A_NARROWER_FN_TYPE,
    refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

// The same with the callee's parameter open: its default does not decide
// it past a fn type that names the label.
run!(open_default_through_a_fn_type, r#"{
    let g = |#n = "hello", x: i64| -> i64 x + 1;
    let h = |f: fn(?#n: i64, x: i64) -> i64| f(1);
    h(g)
}"#, refused("does not contain string"); FuseExpect::None);

// Two lambdas in one array are one function type, and a call through an
// element may reach either: it judges both defaults against the label.
run!(array_defaults_of_two_types, r#"{
    let fs = [
        |#d = 1, x: i64| cast<string>(d)$,
        |#d = "q", x: i64| cast<string>(d)$
    ];
    fs[0]$(7)
}"#, refused("i64 does not contain string"); FuseExpect::None);

// The array's one member is a type of its own: a direct call of a named
// member still judges only that member's default.
run!(array_member_called_directly_keeps_its_default, r#"{
    let f1 = |#d = 1, x: i64| cast<string>(d)$;
    f1 <- never();
    let f2 = |#d = "q", x: i64| cast<string>(d)$;
    f2 <- never();
    let fs = [f1, f2];
    f1(7)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(array_member_beside_a_literal_called_directly, r#"{
    let f2 = |#d = "q", x: i64| cast<string>(d)$;
    f2 <- never();
    let fs = [|#d = 1, x: i64| cast<string>(d)$, f2];
    f2(7)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

// Every lambda of a longer array is one member: the odd default is judged
// wherever it stands.
run!(array_defaults_of_three_odd_last, r#"{
    let fs = [
        |#d = 1, x: i64| cast<string>(d)$,
        |#d = 2, x: i64| cast<string>(d)$,
        |#d = "q", x: i64| cast<string>(d)$
    ];
    fs[0]$(7)
}"#, refused("the functions met here share one #d"); FuseExpect::None);

run!(array_defaults_of_three_named, r#"{
    let f1 = |#d = 1, x: i64| cast<string>(d)$;
    f1 <- never();
    let f2 = |#d = "q", x: i64| cast<string>(d)$;
    f2 <- never();
    let f3 = |#d = 2, x: i64| cast<string>(d)$;
    f3 <- never();
    let fs = [f1, f3, f2];
    fs[0]$(7)
}"#, prints("\"1\""); FuseExpect::None);

// Each function keeps its own default, so the call's result holds both
// defaults' types.
run!(array_defaults_of_three_named_reach_the_result_refused, r#"{
    let f1 = |#d = 1, x: i64| d;
    f1 <- never();
    let f2 = |#d = "q", x: i64| d;
    f2 <- never();
    let f3 = |#d = 2, x: i64| d;
    f3 <- never();
    let fs = [f1, f3, f2];
    let r: i64 = fs[0]$(7);
    r
}"#, refused("i64 does not contain string"); FuseExpect::None);

// Met defaults are judged in the order the program writes them, though a
// struct literal compiles its fields sorted by name: `1` first, so `"q"` is
// refused, at a formal that hides the label and at a call that omits it.
run!(met_defaults_of_two_types_pass_a_formal, r#"{
    let s = {
        f: |#d = 1, x: i64| cast<string>(d)$,
        e: |#d = "q", x: i64| cast<string>(d)$,
        d: |#d = "q", x: i64| cast<string>(d)$,
        c: |#d = "q", x: i64| cast<string>(d)$,
        b: |#d = "q", x: i64| cast<string>(d)$,
        a: |#d = "q", x: i64| cast<string>(d)$
    };
    array::len([s.a, s.b, s.c, s.d, s.e, s.f])
}"#, prints("i64:6"); FuseExpect::None);

// formal that hides the label still takes each default's type in what it
// returns.
run!(met_defaults_of_two_types_reach_a_formals_result_refused, r#"{
    let s = {
        f: |#d = 1, x: i64| d,
        e: |#d = "q", x: i64| d,
        d: |#d = "q", x: i64| d,
        c: |#d = "q", x: i64| d,
        b: |#d = "q", x: i64| d,
        a: |#d = "q", x: i64| d
    };
    let h: Array<fn(x: i64) -> string> = [s.a, s.b, s.c, s.d, s.e, s.f];
    array::len(h)
}"#, refused("string does not contain i64"); FuseExpect::None);

run!(met_defaults_of_two_types_each_taken_at_a_call, r#"{
    let s = {
        b: |#d = 1, x: i64| cast<string>(d)$,
        a: |#d = "q", x: i64| cast<string>(d)$
    };
    let f = select true { true => s.a, false => s.b };
    f(7)
}"#, prints("\"q\""); FuseExpect::None);

run!(met_defaults_of_two_types_reach_the_result_at_a_call_refused, r#"{
    let s = {
        b: |#d = 1, x: i64| d,
        a: |#d = "q", x: i64| d
    };
    let f = select true { true => s.a, false => s.b };
    let r: string = f(7);
    r
}"#, refused("string does not contain i64"); FuseExpect::None);

// A function the array is passed to may call through it: the refusal names
// the annotation, whose twin passes.
run!(array_defaults_passed_on_name_the_annotation, r#"{
    let fs = [
        |#d = 1, x: i64| cast<string>(d)$,
        |#d = "q", x: i64| cast<string>(d)$
    ];
    array::len(fs)
}"#, refused("annotate the binding that holds them with a function type whose #d holds every default"); FuseExpect::None);

run!(array_defaults_passed_on_annotated, r#"{
    let fs: Array<fn(?#d: [i64, string], x: i64) -> string> = [
        |#d = 1, x: i64| cast<string>(d)$,
        |#d = "q", x: i64| cast<string>(d)$
    ];
    "[array::len(fs)] [fs[1]$(7)] [fs[0]$(7)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "2 q 1"); FuseExpect::None);

// Lambdas in the parts of a literal's elements meet as the elements' own
// lambdas do: a call through the part judges every default.
run!(nested_array_defaults_of_two_types, r#"{
    let fs = [
        [|#d = 1, x: i64| cast<string>(d)$],
        [|#d = "q", x: i64| cast<string>(d)$]
    ];
    fs[0]$[0]$(7)
}"#, refused("the functions met here share one #d"); FuseExpect::None);

run!(tuple_part_defaults_of_two_types, r#"{
    let fs = [
        ([(0, |#d = 1, x: i64| cast<string>(d)$)], 0),
        ([(1, |#d = "q", x: i64| cast<string>(d)$)], 1)
    ];
    fs[0]$.0[0]$.1(7)
}"#, prints("\"1\""); FuseExpect::None);

run!(select_of_structs_defaults_of_two_types, r#"{
    let i = 0;
    let s = select i {
        0 => {f: |#d = 1, x: i64| cast<string>(d)$},
        _ => {f: |#d = "q", x: i64| cast<string>(d)$}
    };
    s.f(7)
}"#, prints("\"1\""); FuseExpect::None);

// A read-only and a writable reference may be one value: a call through
// either reaches the functions behind both.
run!(select_of_mixed_references_defaults_of_two_types, r#"{
    let i = 0;
    let f1 = |#d = 1, x: i64| cast<string>(d)$;
    let f2 = |#d = "q", x: i64| cast<string>(d)$;
    let r = select i { 0 => &f1, _ => &mut f2 };
    (*r)(7)
}"#, prints("\"1\""); FuseExpect::None);

run!(select_of_mixed_references_annotated, r#"{
    let i = 0;
    let f1: fn(?#d: [i64, string], x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$;
    let f2: fn(?#d: [i64, string], x: i64) -> string = |#d = "q", x: i64| cast<string>(d)$;
    let r = select i { 0 => &f1, _ => &mut f2 };
    (*r)(7)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(nested_array_defaults_passed_on_name_the_annotation, r#"{
    let fs = [
        [|#d = 1, x: i64| cast<string>(d)$],
        [|#d = "q", x: i64| cast<string>(d)$]
    ];
    array::len(fs)
}"#, refused("annotate the binding that holds them with a function type whose #d holds every default"); FuseExpect::None);

run!(nested_array_defaults_annotated, r#"{
    let fs: Array<Array<fn(?#d: [i64, string], x: i64) -> string>> = [
        [|#d = 1, x: i64| cast<string>(d)$],
        [|#d = "q", x: i64| cast<string>(d)$]
    ];
    "[array::len(fs)] [fs[1]$[0]$(7)] [fs[0]$[0]$(7)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "2 q 1"); FuseExpect::None);

run!(nested_array_defaults_of_one_type, r#"{
    let fs = [[|#d = 1, x: i64| d + x], [|#d = 2, x: i64| d * x]];
    "[fs[0]$[0]$(7)] [fs[1]$[0]$(7)] [fs[1]$[0]$(#d: 3, 7)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "8 14 21"); FuseExpect::None);

run!(nested_array_of_generic_lambdas, r#"{
    let fs = [[|x| x], [|y| y]];
    "[fs[0]$[0]$(1)] [fs[1]$[0]$(2)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 2"); FuseExpect::None);

// A select of structs of lambdas in a definition is one struct: each call
// of the definition instantiates the lambda it picks.
run!(select_of_structs_of_lambdas_in_a_definition, r#"{
    let mk = |b| select b { true => {f: |x| x}, false => {f: |y| y} };
    "[mk(true).f(1)] [mk(false).f("s")]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s"); FuseExpect::None);

// An alternative that is a union meets as its members do: the lambdas of
// optional alternatives are one function, whose call judges every default.
run!(select_of_optional_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let o = select i { 0 => o1, _ => o2 };
    select o { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(array_of_optional_defaults_of_two_types, r#"{
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let a = [o1, o2];
    select a[1]$ { null as _ => "n", f => f(7) }
}"#, prints("\"q\""); FuseExpect::None);

run!(select_of_optional_tuples_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => (|#d = 1, x: i64| cast<string>(d)$, 0), _ => null };
    let o2 = select j { 0 => (|#d = "q", x: i64| cast<string>(d)$, 0), _ => null };
    let o = select i { 0 => o1, _ => o2 };
    select o { null as _ => "n", (f, _) => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(named_optional_defaults_of_two_types, r#"{
    type Opt<'a> = ['a, null];
    let i = 0;
    let j = 0;
    let o1: Opt<fn(?#d: 'z, x: i64) -> string> =
        select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2: Opt<fn(?#d: 'w, x: i64) -> string> =
        select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let o = select i { 0 => o1, _ => o2 };
    select o { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(optional_part_defaults_of_two_types, r#"{
    let i = 0;
    let a = select i {
        0 => [|#d = 1, x: i64| cast<string>(d)$, null],
        _ => [|#d = "q", x: i64| cast<string>(d)$, null]
    };
    select a[0]$ { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(optional_field_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let s = select i { 0 => {f: o1, a: 1}, _ => {f: o2, a: 2} };
    select s.f { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

// A field read over members of two shapes meets the fields it joins.
run!(optional_field_of_two_shapes_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let s = select i { 0 => {f: o1, a: 1}, _ => {f: o2, b: 2} };
    select s.f { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(optional_position_of_two_arities_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let t = select i { 0 => (o1, 1), _ => (o2, "s", 3) };
    select t.0 { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

run!(any_of_optional_defaults_of_two_types, r#"{
    let i = 0;
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let o = any(o1, i ~ o2);
    select o { null as _ => "n", f => f(7) }
}"#, prints("\"1\""); FuseExpect::None);

// A lambda beside a declared variable's optional callback takes the
// variable; one of a concrete type beside it is no one function.
run!(lambda_beside_a_generic_optional_callback, r#"{
    let g = |cb: [fn(x: 'a) -> 'a, null], v: 'a, b: bool| {
        let h = select b { true => cb, false => |x| x };
        select h { null as _ => v, f => f(v) }
    };
    "[g(|s: string| "[s]!", "k", true)] [g(null, 3, false)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "k! 3"); FuseExpect::None);

run!(concrete_lambda_beside_a_generic_optional_callback, r#"{
    let g = |cb: [fn(x: 'a) -> 'a, null], v: 'a, b: bool| {
        let h = select b { true => cb, false => |x: i64| x + 1 };
        select h { null as _ => 0, f => f(2) }
    };
    g(|s: string| "[s]!", "k", true)
}"#, refused("type mismatch 'a: unbound does not contain i64"); FuseExpect::None);

run!(optional_defaults_annotated, r#"{
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let a: Array<[fn(?#d: [i64, string], x: i64) -> string, null]> = [o1, o2];
    let call = |o: [fn(?#d: [i64, string], x: i64) -> string, null]|
        select o { null as _ => "n", f => f(7) };
    "[call(a[0]$)] [call(a[1]$)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 q"); FuseExpect::None);

run!(optional_defaults_of_one_type, r#"{
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| d + x, _ => null };
    let o2 = select j { 0 => |#d = 2, x: i64| d * x, _ => null };
    let a = [o1, o2];
    let r0 = select a[0]$ { null as _ => 0, f => f(7) };
    let r1 = select a[1]$ { null as _ => 0, f => f(7) };
    "[r0] [r1]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "8 14"); FuseExpect::Jit);

// A lambda beside an optional callback is the callback's instance.
run!(lambda_beside_an_optional_callback, r#"{
    let g = |cb: [fn(x: i64) -> i64, null], b: bool| {
        let h = select b { true => cb, false => |x| x + 1 };
        select h { null as _ => 0, f => f(2) }
    };
    "[g(null, false)] [g(|x| x * 10, true)] [g(null, true)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "3 20 0"); FuseExpect::None);

// Only one of the functions met has the default: no annotation of the
// label holds it, so the refusal names none.
run!(one_met_default_names_no_annotation, r#"{
    let f = |#x = 1, y| x == y;
    let g = |y: string| y == "s";
    let t = sys::time::timer(duration:10.ms, false);
    let b = select t { _ => true };
    let pick: fn(y: string) -> bool = g;
    pick <- select b { true => f, false => g };
    pick(t ~ "s")
}"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(array_defaults_with_declared_types, r#"{
    let fs = [
        |#d: [i64, string] = 1, x: i64| cast<string>(d)$,
        |#d: [i64, string] = "q", x: i64| cast<string>(d)$
    ];
    fs[1]$(7)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(array_defaults_of_one_type, r#"{
    let fs = [
        |#d = 1, x: i64| cast<string>(d)$,
        |#d = 2, x: i64| cast<string>(d)$
    ];
    fs[1]$(7)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "2"); FuseExpect::None);

// A function value's default is judged where the value meets a fn type
// that hides the label, at the parameter the fn type fixes.
const DEFAULT_HIDDEN_BY_A_FN_TYPE: &str = r#"
{
    let g = |#x = 4096, y| x == y;
    let apply = |h: fn(y: string) -> bool| h("seven");
    apply(g)
}
"#;

run!(default_hidden_by_a_fn_type, DEFAULT_HIDDEN_BY_A_FN_TYPE, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

// The same judgment wherever a function value meets a fn type that hides
// its label: a let's annotation, a struct field under one, a <- into a
// function-typed binding, an optional callback, a declared return type, a
// rank-2 formal.
run!(default_hidden_by_an_annotation, r#"
{
    let g = |#x = 4096, y| x == y;
    let h: fn(y: string) -> bool = g;
    h("seven")
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_hidden_by_a_field, r#"
{
    type T = {f: fn(y: string) -> bool};
    let t: T = {f: |#x = 4096, y| x == y};
    (t.f)("seven")
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_hidden_by_a_connect, r#"
{
    let f = |#x = 1, y| x == y;
    let g = |y: string| y == "s";
    let pick: fn(y: string) -> bool = g;
    pick <- f;
    pick("s")
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

// CR claude for claude: [test-gap/low] `{s with h: f}` gives `h` f's own generic type,
// so the update hides no label and judges nothing. The update with no call checks, and
// so does `(s2.h)(#d: 2.5, 1.5)`. This fixture's refusal is the omitted default at
// `(s2.h)(1.5)`, which `f(1.5)` alone gets, so it pins nothing about an update. Rewrite
// it as an annotated update (`let s2: {h: fn(x: f64) -> f64} = {s with h: f}`, refused
// at the update) beside the bare update passing the label, or delete it. The family
// comment above still lists 'a rank-2 formal', which no longer exists. probe:
// design/review-2026-11-16/repro/tests-functions-b-with-update-hides-nothing.sh
// (tests-functions-b-05)
run!(default_hidden_by_a_with_update, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let s = {h: |x: f64| x};
    let s2 = {s with h: f};
    (s2.h)(1.5)
}
"#, refused("cannot compute with [i64, f64]: both operands must be one numeric type"); FuseExpect::None);

run!(default_hidden_by_a_reference_write, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let g = |x: f64| x;
    let r = &mut g;
    *r <- f;
    let h = *r;
    h(1.5)
}
"#, refused("cannot compute with [i64, f64]: both operands must be one numeric type"); FuseExpect::None);

run!(default_hidden_by_an_optional_callback, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let g = |h: [fn(x: f64) -> f64, null]| h$(1.5);
    g(f)
}
"#, refused("does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_return_type, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let mk = |u: i64| -> fn(x: f64) -> f64 f;
    let h = mk(0);
    h(1.5)
}
"#, refused("does not contain i64"); FuseExpect::None);

// A function type a typedef's body writes names no function to its
// readers, so no call through it judges a default: a declaration that
// holds a function through one judges the default as it stands. A
// definition's declared variable refuses it, and the written type passes.
run!(default_behind_a_typedef_return_type, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |j: i64| -> W<'z> {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    let w = mk(0);
    w.cb(7)
}
"#, refused("write the default's type in the declared type"); FuseExpect::None);

run!(default_behind_a_typedef_return_type_written, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |j: i64| -> W<i64> {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    let w = mk(0);
    w.cb(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_a_typedef_fits_its_factorys_variable, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |z: 'z| -> W<'z> {cb: |#d = z, x: i64| cast<string>(d)$, n: 1};
    let a = mk(1);
    let b = mk("s");
    "[a.cb(7)] [b.cb(7)] [b.cb(#d: "t", 7)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s t"); FuseExpect::None);

run!(default_behind_a_typedef_variant_return_type, r#"
{
    type V<'z> = [`Cb(fn(?#d: 'z, x: i64) -> string), `No];
    let mk = |j: i64| -> V<'z> select j { 0 => `Cb(|#d = 1, x: i64| cast<string>(d)$), _ => `No };
    select mk(0) { `No => "n", `Cb(f) => f(7) }
}
"#, prints("\"1\""); FuseExpect::None);

run!(default_behind_a_recursive_typedef_return_type, r#"
{
    type L<'a> = [`Cons(fn(?#d: 'a, x: i64) -> string, L<'a>), `Nil];
    let mk = |j: i64| -> L<'z> `Cons(|#d = 1, x: i64| cast<string>(d)$, `Nil);
    select mk(0) { `Cons(f, _) => f(7), `Nil => "n" }
}
"#, refused("write the default's type in the declared type"); FuseExpect::None);

run!(default_behind_an_optional_typedef_field_written, r#"
{
    type W<'z> = {cb: [fn(?#d: 'z, x: i64) -> string, null], n: i64};
    let mk1 = |j: i64| -> W<[i64, string]> {cb: select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null }, n: 1};
    let mk2 = |j: i64| -> W<[i64, string]> {cb: select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null }, n: 2};
    let i = 1;
    let w = select i { 0 => mk1(0), _ => mk2(0) };
    select w.cb { null as _ => "n", f => f(7) }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(default_behind_a_typedef_body_annotation, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |z: 'z| -> string {
        let w: W<'z> = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
        w.cb(7)
    };
    mk("s")
}
"#, refused("type mismatch 'z: unbound does not contain i64"); FuseExpect::None);

// An annotation's own variable is decided by the default it holds so.
run!(default_behind_a_typedef_annotation_decides_it, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let w: W<'p> = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    "[w.cb(7)] [w.cb(#d: 2, 7)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 2"); FuseExpect::None);

run!(default_behind_a_typedef_annotation_takes_a_passed_value_of_another_type, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let w: W<'p> = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    w.cb(#d: "s", 7)
}
"#, prints("\"s\""); FuseExpect::None);

run!(defaults_behind_two_typedef_annotations_met, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let w1: W<'p> = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    let w2: W<'q> = {cb: |#d = "q", x: i64| cast<string>(d)$, n: 2};
    let i = 1;
    let w = select i { 0 => w1, _ => w2 };
    w.cb(7)
}
"#, prints("\"q\""); FuseExpect::None);

run!(default_behind_a_typedef_connect, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let v: W<'q> = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    v <- once(v) ~ {cb: |#d = 1, x: i64| cast<string>(d)$, n: 2};
    v.cb(7)
}
"#, prints("\"a\""); FuseExpect::None);

run!(default_behind_a_typedef_connect_fits, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let v: W<'q> = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    v <- once(v) ~ {cb: |#d = "b", x: i64| cast<string>(d)$, n: 2};
    select v.n { 2 => v.cb(7), _ => never() }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "b"); FuseExpect::None);

run!(default_behind_a_typedef_reference_write, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let g: W<'q> = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    let r = &mut g;
    *r <- once(g) ~ {cb: |#d = 1, x: i64| cast<string>(d)$, n: 2};
    g.cb(7)
}
"#, prints(r#""a""#); FuseExpect::None);

// A binding something writes is one function type for every reader, so a
// default its label makes optional decides the label's variable where it
// is written: at the annotation, the `<-` and the write through a
// reference.
run!(default_decides_a_written_bindings_label, r#"
{
    let v: fn(?#d: 'q, x: i64) -> string = never();
    v <- |#d = "q", x: i64| cast<string>(d)$;
    v(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(defaults_of_one_type_written_to_a_binding, r#"
{
    let v: {cb: fn(?#d: 'q, x: i64) -> string, n: i64} = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    v <- once(v) ~ {cb: |#d = "b", x: i64| cast<string>(d)$, n: 2};
    select v.n { 2 => v.cb(7), _ => never() }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "b"); FuseExpect::None);

run!(defaults_of_one_type_written_through_a_reference, r#"
{
    let g: {cb: fn(?#d: 'q, x: i64) -> string, n: i64} = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    let r = &mut g;
    *r <- once(g) ~ {cb: |#d = "b", x: i64| cast<string>(d)$, n: 2};
    select g.n { 2 => g.cb(7), _ => never() }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "b"); FuseExpect::None);

run!(default_of_another_type_written_to_a_binding, r#"
{
    let v: fn(?#d: 'q, x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$;
    let t = 1;
    v <- t ~ |#d = 1, x: i64| cast<string>(d)$;
    v(7)
}
"#, prints("\"a\""); FuseExpect::None);

run!(written_bindings_label_takes_a_passed_value_of_another_type, r#"
{
    let v: fn(?#d: 'q, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$;
    let t = 1;
    v <- t ~ |#d = 2, x: i64| cast<string>(d)$;
    v(#d: 1.5, 7)
}
"#, prints("\"1.5\""); FuseExpect::None);

run!(default_written_through_a_parameters_reference, r#"
{
    let set = |r: &mut fn(?#d: 'a, x: i64) -> string, t: Any| {
        *r <- t ~ |#d = 1, x: i64| cast<string>(d)$;
        null
    };
    let g: fn(?#d: string, x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$;
    set(&mut g, 1);
    g(7)
}
"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(default_written_through_a_parameters_reference_written, r#"
{
    let set = |r: &mut fn(?#d: i64, x: i64) -> string, t: Any| {
        *r <- t ~ |#d = 1, x: i64| cast<string>(d)$;
        null
    };
    let g: fn(?#d: i64, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$;
    set(&mut g, 1);
    g(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_written_behind_a_typedef_decides_its_label, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let v: W<'q> = never();
    v <- {cb: |#d = "q", x: i64| cast<string>(d)$, n: 1};
    v.cb(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(default_written_by_a_function_to_a_binding_it_captures, r#"
{
    let v: fn(?#d: 'q, x: i64) -> string = never();
    let w = |t: Any| { v <- t ~ |#d = "q", x: i64| cast<string>(d)$; null };
    w(1);
    v(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(default_of_another_type_written_to_a_field, r#"
{
    let s: {cb: fn(?#d: 'q, x: i64) -> string, n: i64} = {cb: |#d = "a", x: i64| cast<string>(d)$, n: 1};
    let r = &mut s.cb;
    let t = 1;
    *r <- t ~ |#d = 1, x: i64| cast<string>(d)$;
    s.cb(7)
}
"#, prints(r#""a""#); FuseExpect::None);

// A union's member that is the written function's type but for its cells
// is one function with it, so the write judges the defaults of every
// function the binding holds, as a write to a function alone does.
run!(default_of_another_type_written_beside_a_null, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    o <- once(j) ~ |#d = "q", x: i64| cast<string>(d)$;
    select o { null as _ => "n", f => f(7) }
}
"#, prints("\"1\""); FuseExpect::None);

run!(default_of_another_type_written_beside_a_null_and_unwrapped, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    o <- once(j) ~ |#d = "q", x: i64| cast<string>(d)$;
    (o$)(7)
}
"#, prints("\"1\""); FuseExpect::None);

// tw10.gx). The written function's own default reaches the result.
run!(default_of_another_type_written_beside_a_null_and_unwrapped_reaches_the_result_refused, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| d, _ => null };
    o <- once(j) ~ |#d = "q", x: i64| d;
    let r: i64 = (o$)(7);
    r
}
"#, refused("i64 does not contain string"); FuseExpect::None);

run!(default_of_another_type_written_from_an_optional_binding, r#"
{
    let j = 0;
    let o1 = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let o2 = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let o = o1;
    o <- once(j) ~ o2;
    select o { null as _ => "n", f => f(7) }
}
"#, prints("\"1\""); FuseExpect::None);

run!(default_of_another_type_written_beside_a_null_no_call_omits, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    o <- once(j) ~ |#d = "q", x: i64| cast<string>(d)$;
    select o { null as _ => "n", f => f(#d: "z", 7) }
}
"#, prints("\"z\""); FuseExpect::None);

run!(default_of_another_type_written_to_a_select_of_one, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
    o <- once(j) ~ |#d = "q", x: i64| cast<string>(d)$;
    o(7)
}
"#, prints("\"1\""); FuseExpect::None);

run!(defaults_of_two_types_written_to_a_declared_optional, r#"
{
    let j = 0;
    let o: [fn(?#d: [i64, string], x: i64) -> string, null] =
        select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    o <- once(j) ~ |#d = "q", x: i64| cast<string>(d)$;
    let r = select o { null as _ => never(), f => f(7) };
    let seen = "";
    seen <- r ~ "[seen][r]";
    filter(seen, |s| s == "1q")
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1q"); FuseExpect::None);

run!(defaults_of_one_type_written_beside_a_null, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    o <- once(j) ~ |#d = 2, x: i64| cast<string>(d)$;
    let r = select o { null as _ => never(), f => f(7) };
    let seen = "";
    seen <- r ~ "[seen][r]";
    filter(seen, |s| s == "12")
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "12"); FuseExpect::None);

// Two functions an array or a select met share their cells already: the
// member is the written function's type as it stands, and still one
// function with it.
run!(default_of_another_type_written_after_an_array_met_them, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let p = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let both = [o, p];
    o <- once(j) ~ p;
    select o { null as _ => "n", f => f(7) }
}
"#, prints("\"1\""); FuseExpect::None);

run!(default_of_another_type_written_after_a_select_met_them, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let p = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let either = select j { 0 => o, _ => p };
    o <- once(j) ~ p;
    select o { null as _ => "n", f => f(#d: "z", 7) }
}
"#, prints("\"z\""); FuseExpect::None);

run!(defaults_of_two_types_written_to_a_declared_optional_after_an_array_met_them, r#"
{
    let j = 0;
    let o: [fn(?#d: [i64, string], x: i64) -> string, null] =
        select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let p = select j { 0 => |#d = "q", x: i64| cast<string>(d)$, _ => null };
    let both = [o, p];
    o <- once(j) ~ p;
    let r = select o { null as _ => never(), f => f(7) };
    let seen = "";
    seen <- r ~ "[seen][r]";
    filter(seen, |s| s == "1q")
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1q"); FuseExpect::None);

run!(defaults_of_one_type_written_after_an_array_met_them, r#"
{
    let j = 0;
    let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
    let p = select j { 0 => |#d = 2, x: i64| cast<string>(d)$, _ => null };
    let both = [o, p];
    o <- once(j) ~ p;
    let r = select o { null as _ => never(), f => f(7) };
    let seen = "";
    seen <- r ~ "[seen][r]";
    filter(seen, |s| s == "12")
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "12"); FuseExpect::None);

// What a function returns is a part of it as a field is of a struct: a
// default of the function it returns is judged where the outer function
// meets a fn type whose return hides the label or makes it optional.
run!(default_behind_a_callbacks_return, r#"
{
    let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string| f(0)(7);
    app(|j: i64| |#d = 1, x: i64| cast<string>(d)$)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_a_callbacks_return_takes_a_passed_value_of_another_type, r#"
{
    let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string, v: 'z| f(0)(#d: v, 7);
    app(|j: i64| |#d = 1, x: i64| cast<string>(d)$, "s")
}
"#, prints("\"s\""); FuseExpect::None);

run!(default_behind_a_callbacks_return_fits_the_sites_variable, r#"
{
    let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string, v: 'z| f(0)(#d: v, 7);
    app(|j: i64| |#d = 1, x: i64| cast<string>(d)$, 2)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "2"); FuseExpect::None);

run!(default_behind_a_callbacks_typedef_return, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let app = |f: fn(j: i64) -> W<'z>| f(0);
    let w = app(|j: i64| {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1});
    w.cb(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_a_struct_fields_return, r#"
{
    let app = |s: {f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string}| s.f(0)(7);
    app({f: |j: i64| |#d = 1, x: i64| cast<string>(d)$})
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_a_returned_functions_typedef_return, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |j: i64| -> fn(k: i64) -> W<'z> |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    mk(0)(0).cb(7)
}
"#, prints("\"1\""); FuseExpect::None);

run!(default_behind_a_returned_functions_typedef_return_written, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let mk = |j: i64| -> fn(k: i64) -> W<i64> |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    mk(0)(0).cb(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_an_annotated_functions_typedef_return, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let g: fn(k: i64) -> W<'p> = |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    "[g(0).cb(7)] [g(0).cb(#d: 2, 7)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 2"); FuseExpect::None);

run!(default_behind_an_annotated_functions_typedef_return_takes_a_passed_value_of_another_type, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let g: fn(k: i64) -> W<'p> = |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1};
    g(0).cb(#d: "s", 7)
}
"#, prints("\"s\""); FuseExpect::None);

run!(default_of_another_type_written_behind_a_functions_return, r#"
{
    let v: fn(k: i64) -> fn(?#d: 'q, x: i64) -> string = |k: i64| |#d = "q", x: i64| cast<string>(d)$;
    let t = 1;
    v <- t ~ |k: i64| |#d = 1, x: i64| cast<string>(d)$;
    v(0)(7)
}
"#, prints(r#""q""#); FuseExpect::None);

// A declaration nothing writes leaves its label's variable to the calls
// through it, in a function's body as at the top level: the instance takes
// its definition's verdict on the default, whatever the calls decided.
run!(default_under_a_bodys_annotation_no_call, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        "none"
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "none"); FuseExpect::Jit);

run!(default_under_a_bodys_annotation_a_call_passes_the_label, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        v(#d: "s", 7)
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_under_a_bodys_annotation_a_call_omits_the_label, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        "[v(7)] [v(#d: 2, 8)]"
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 2"); FuseExpect::None);

// A default that calls a function returning a union of members holding
// open cells builds as the call does in a body.
run!(a_default_reading_a_union_result_of_open_members, r#"
{
    let view = |done: bool| select done {
        true => {a: 1, notes: []},
        false => {a: 2, notes: []}
    };
    let w = |#d = view(false).a, x: i64| x + d;
    w(1)
}
"#, prints("i64:3"); FuseExpect::Jit);

run!(a_default_calling_one_of_two_functions, r#"
{
    let w = |#d = { let pick = |c, a, b| select c { true => a, false => b };
                    let f = |x: i64| x; pick(false, f, f)(1) }, x: i64| x + d;
    w(1)
}
"#, prints("i64:2"); FuseExpect::Jit);

run!(default_under_a_bodys_optional_annotation, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: [fn(?#d: 'q, x: i64) -> string, null] =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };
        (v$)(#d: "s", 7)
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_under_a_bodys_struct_annotation, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: {cb: fn(?#d: 'q, x: i64) -> string} =
            select j { 0 => {cb: |#d = 1, x: i64| cast<string>(d)$}, _ => never() };
        v.cb(#d: "s", 7)
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_under_a_bodys_array_annotation, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: Array<fn(?#d: 'q, x: i64) -> string> =
            [select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() }];
        (v[0]$)(#d: "s", 7)
    };
    mk(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_under_a_bodys_annotation_returned, r#"
{
    let j = 0;
    let mk = |t: Any| {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        v
    };
    mk(1)(#d: "s", 7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_of_a_factorys_variable_under_a_bodys_annotation, r#"
{
    let j = 0;
    let mk = |t: 'a| -> string {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = t, x: i64| cast<string>(d)$, _ => never() };
        v(7)
    };
    "[mk(1)] [mk("z")]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 z"); FuseExpect::None);

// The same wherever an instance is built: at a run-time bind, in a seq's
// steps, per element of a collection, in a factory inside a factory, and
// twice with the label decided two ways.
run!(default_under_a_bodys_annotation_at_a_run_time_bind, r#"
{
    let j = 0;
    let mk = |t: Any| -> string {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        v(#d: "s", 7)
    };
    let f: fn(t: Any) -> string = never();
    f <- mk;
    f(1)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::None);

run!(default_under_a_bodys_annotation_in_a_seqs_steps, r#"
{
    let mk = |t: 'a| {
        let v: fn(?#d: 'q, x: i64) -> string = |#d = t, x: i64| cast<string>(d)$;
        v
    };
    seq { let a = mk(1)(7); let b = mk("z")(8); "[a] [b]" }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 z"); FuseExpect::Jit);

run!(default_under_a_bodys_annotation_per_element, r#"
{
    let mk = |t: 'a| {
        let v: fn(?#d: 'q, x: i64) -> string = |#d = t, x: i64| cast<string>(d)$;
        v
    };
    "[array::map([1, 2], |x| mk(x)(7))] [array::map(["a", "b"], |x| mk(x)(8))]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["1", "2"] ["a", "b"]"#); FuseExpect::None);

run!(default_under_a_nested_bodys_annotation, r#"
{
    let j = 0;
    let mk = |t: 'a| -> string {
        let inner = |u: 'b| -> string {
            let v: fn(?#d: 'q, x: i64) -> string =
                select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
            v(#d: u, 7)
        };
        "[inner(1)] [inner("k")] [inner(t)]"
    };
    "[mk(1)] [mk("z")]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 k 1 1 k z"); FuseExpect::None);

run!(default_under_a_bodys_annotation_returned_twice, r#"
{
    let j = 0;
    let mk = |t: Any| {
        let v: fn(?#d: 'q, x: i64) -> string =
            select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => never() };
        v
    };
    "[mk(1)(#d: "s", 7)] [mk(2)(8)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s 1"); FuseExpect::None);

// A default that holds a function is called through its label, as a
// written argument is: the defaults of that function a call through the
// label may omit are judged there, at the call that omits the outer one,
// and narrow the label's variable as the written argument's would.
run!(default_holding_a_defaulted_fn, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_beside_a_written_one, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    "[f(7)] [f(#w: |#d = "s", x: i64| cast<string>(d)$, 8)]"
}
"#, is(Value::from("1 s")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_in_a_field, r#"
{
    let f = |#w: {cb: fn(?#d: 'z, x: i64) -> string, n: i64} = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1}, x: i64| w.cb(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_defaulted_fn_behind_a_typedef, r#"
{
    type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string, n: i64};
    let f = |#w: W<'z> = {cb: |#d = 1, x: i64| cast<string>(d)$, n: 1}, x: i64| w.cb(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_defaulted_fn_in_an_element, r#"
{
    let f = |#w: Array<fn(?#d: 'z, x: i64) -> string> = [|#d = 1, x: i64| cast<string>(d)$], x: i64| (w[0]$)(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_defaulted_fn_under_an_option, r#"
{
    let f = |#w: [fn(?#d: 'z, x: i64) -> string, null] = |#d = 1, x: i64| cast<string>(d)$, x: i64| select w { null as _ => "n", g => g(x) };
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_fn_that_returns_a_defaulted_fn, r#"
{
    let f = |#w: fn(k: i64) -> fn(?#d: 'z, x: i64) -> string = |k: i64| |#d = 1, x: i64| cast<string>(d)$, x: i64| w(0)(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_named_defaulted_fn, r#"
{
    let inner = |#d = 1, x: i64| cast<string>(d)$;
    let f = |#w: fn(?#d: 'z, x: i64) -> string = inner, x: i64| w(x);
    "[f(7)] [inner(#d: "k", 8)]"
}
"#, is(Value::from("1 k")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_three_deep, r#"
{
    let f = |
        #w: fn(?#d: fn(?#e: 'z, y: i64) -> string, x: i64) -> string =
            |#d: fn(?#e: 'y, y: i64) -> string = |#e = 1, y: i64| cast<string>(e)$, x: i64| d(x),
        x: i64
    | w(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_reads_a_capture, r#"
{
    let mk = |t: i64| {
        let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = t, x: i64| cast<string>(d)$, x: i64| w(x);
        f(7)
    };
    mk(4)
}
"#, is(Value::from("4")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_in_a_generic_body, r#"
{
    let o = |k: 'a| {
        let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
        "[k] [f(7)] [f(#w: |#d = "s", x: i64| cast<string>(d)$, 8)]"
    };
    "[o(0)] [o("u")]"
}
"#, is(Value::from("0 1 s u 1 s")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_per_element, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    str::join(#sep: " ", array::map([1, 2], |k| f(k)))
}
"#, is(Value::from("1 1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_in_a_seqs_step, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    seq { let a = f(7); a }
}
"#, is(Value::from("1")); FuseExpect::Jit);

// The label's variable is one type for the call: an argument that decided
// it first, a second default that shares it, or the function type that
// takes the function refuses an inner default of another type, at the
// check, as the same function written at the call is refused.
run!(default_holding_a_defaulted_fn_of_an_arguments_type, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, y: 'z, x: i64| w(x);
    f(3, 7)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_of_another_type_than_an_argument, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, y: 'z, x: i64| w(x);
    f("q", 7)
}
"#, prints(r#""1""#); FuseExpect::Jit);

run!(written_defaulted_fn_of_another_type_than_an_argument, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string, y: 'z, x: i64| w(x);
    f(#w: |#d = 1, x: i64| cast<string>(d)$, "q", 7)
}
"#, prints(r#""1""#); FuseExpect::Jit);

run!(written_defaulted_fn_of_an_arguments_type, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, y: 'z, x: i64| w(x);
    f(#w: |#d = "s", x: i64| cast<string>(d)$, "q", 7)
}
"#, is(Value::from("s")); FuseExpect::Jit);

run!(two_defaults_holding_defaulted_fns_of_one_type, r#"
{
    let f = |#v: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, #w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| "[v(x)] [w(x)]";
    f(7)
}
"#, is(Value::from("1 1")); FuseExpect::Jit);

run!(two_defaults_holding_defaulted_fns_of_two_types, r#"
{
    let f = |#v: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, #w: fn(?#d: 'z, x: i64) -> string = |#d = "s", x: i64| cast<string>(d)$, x: i64| "[v(x)] [w(x)]";
    f(7)
}
"#, prints(r#""1 s""#); FuseExpect::Jit);

run!(default_holding_one_of_two_defaulted_fns, r#"
{
    let c = true;
    let f = |
        #w: fn(?#d: 'z, x: i64) -> string = select c { true => |#d = 1, x: i64| cast<string>(d)$, false => |#d = 2, x: i64| "two" },
        x: i64
    | w(x);
    f(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_one_of_two_defaulted_fns_of_two_types, r#"
{
    let c = true;
    let f = |
        #w: fn(?#d: 'z, x: i64) -> string = select c { true => |#d = 1, x: i64| cast<string>(d)$, false => |#d = "z", x: i64| "two" },
        x: i64
    | w(x);
    f(7)
}
"#, prints(r#""1""#); FuseExpect::None);

// The same where the outer label is one a function type hides or makes
// optional: a formal, a `let`'s annotation, a typedef, a function a call
// returns (one instance, whose label the first call decides).
run!(default_holding_a_defaulted_fn_behind_an_optional_formal, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let g = |h: fn(?#w: fn(?#d: 'q, x: i64) -> string, x: i64) -> string| h(7);
    g(f)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_behind_a_closed_optional_formal, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let g = |h: fn(?#w: fn(?#d: string, x: i64) -> string, x: i64) -> string| h(7);
    g(f)
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_holding_a_defaulted_fn_behind_a_formal_that_hides_it, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, y: 'z, x: i64| w(x);
    let g = |h: fn(y: i64, x: i64) -> string| h(3, 7);
    g(f)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_behind_a_formal_that_fixes_it, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, y: 'z, x: i64| w(x);
    let g = |h: fn(y: string, x: i64) -> string| h("q", 7);
    g(f)
}
"#, prints(r#""1""#); FuseExpect::Jit);

run!(default_holding_a_fn_that_returns_a_defaulted_fn_behind_a_formal_that_hides_it, r#"
{
    let f = |#w: fn(k: i64) -> fn(?#d: 'z, x: i64) -> string = |k: i64| |#d = 1, x: i64| cast<string>(d)$, x: i64| w(0)(x);
    let g = |h: fn(x: i64) -> string| h(7);
    g(f)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_fn_that_returns_a_defaulted_fn_behind_an_annotation_that_hides_it, r#"
{
    let f = |#w: fn(k: i64) -> fn(?#d: 'z, x: i64) -> string = |k: i64| |#d = 1, x: i64| cast<string>(d)$, x: i64| w(0)(x);
    let v: fn(x: i64) -> string = f;
    v(7)
}
"#, is(Value::from("1")); FuseExpect::None);

run!(default_holding_a_defaulted_fn_behind_an_annotation, r#"
{
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let v: fn(?#w: fn(?#d: 'q, x: i64) -> string, x: i64) -> string = f;
    v(7)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_behind_a_typedefs_fn, r#"
{
    type H<'q> = fn(?#w: fn(?#d: 'q, x: i64) -> string, x: i64) -> string;
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let v: H<'q> = f;
    v(7)
}
"#, is(Value::from("1")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_behind_a_closed_typedefs_fn, r#"
{
    type H = fn(?#w: fn(?#d: string, x: i64) -> string, x: i64) -> string;
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let v: H = f;
    v(7)
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_holding_a_defaulted_fn_of_a_returned_fn, r#"
{
    let mk = |t: i64| |#w: fn(?#d: 'z, x: i64) -> string = |#d = t, x: i64| cast<string>(d)$, x: i64| w(x);
    let a = mk(4);
    let b = mk(5);
    "[a(7)] [b(#w: |#d = "s", x: i64| cast<string>(d)$, 8)]"
}
"#, is(Value::from("4 s")); FuseExpect::None);

run!(default_holding_a_defaulted_fn_of_a_returned_fn_called_two_ways, r#"
{
    let mk = |t: i64| |#w: fn(?#d: 'z, x: i64) -> string = |#d = t, x: i64| cast<string>(d)$, x: i64| w(x);
    let a = mk(4);
    "[a(7)] [a(#w: |#d = "s", x: i64| cast<string>(d)$, 8)]"
}
"#, refused("i64 does not contain string"); FuseExpect::None);

run!(default_holding_a_defaulted_fn_in_an_abstracts_field, r#"
{
    type A<'z> = Abstract<{cb: fn(?#w: fn(?#d: 'z, x: i64) -> string, x: i64) -> string}>;
    let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let a = A({cb: f});
    (a.0).cb(7)
}
"#, is(Value::from("1")); FuseExpect::None);

// A label whose type has no variable is one type for the function's own
// parameter and for the function as a site holds it: the defaults under
// it are judged against it where a function type hides the label.
run!(default_holding_a_defaulted_fn_of_a_closed_label_behind_a_formal_that_hides_it, r#"
{
    let f = |#w: fn(?#d: string, x: i64) -> string = |#d = "s", x: i64| cast<string>(d)$, x: i64| w(x);
    let g = |h: fn(x: i64) -> string| h(7);
    g(f)
}
"#, is(Value::from("s")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_of_another_type_than_a_closed_label_behind_a_formal_that_hides_it, r#"
{
    let f = |#w: fn(?#d: string, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x);
    let g = |h: fn(x: i64) -> string| h(7);
    g(f)
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_holding_a_defaulted_fn_three_deep_behind_labels_that_hide_it, r#"
{
    let f = |
        #w: fn(x: i64) -> string =
            |#d: fn(?#e: string, x: i64) -> string = |#e = "s", x: i64| cast<string>(e)$, x: i64| d(x),
        x: i64
    | w(x);
    f(7)
}
"#, is(Value::from("s")); FuseExpect::Jit);

run!(default_holding_a_defaulted_fn_of_another_type_three_deep_behind_labels_that_hide_it, r#"
{
    let f = |
        #w: fn(x: i64) -> string =
            |#d: fn(?#e: string, x: i64) -> string = |#e = 1, x: i64| cast<string>(e)$, x: i64| d(x),
        x: i64
    | w(x);
    f(7)
}
"#, refused("string does not contain i64"); FuseExpect::None);

/// `depth` functions, each the default of the next one's label, whose
/// type hides the label of the function it holds.
fn hidden_default_chain(depth: usize) -> CompactString {
    let mut f = CompactString::from("|#d = 1, x: i64| cast<string>(d)$");
    for _ in 0..depth {
        f = format_compact!("|#d: fn(x: i64) -> string = {f}, x: i64| d(x)");
    }
    format_compact!("{{ let f = {f}; f(7) }}")
}

// Every judgment of a default compiles it again, and each compile's
// lambda links onto the label that holds it: the compiles of one lambda
// are judged as one, so the work grows with the depth, not with the
// compiles made on the way down.
run!(
    defaults_holding_defaulted_fns_twenty_deep_behind_labels_that_hide_them,
    hidden_default_chain(20),
    is(Value::from("1"));
    FuseExpect::Jit
);

/// [`hidden_default_chain`] whose labels keep an open variable for what
/// the function they hide returns.
fn hidden_default_open_chain(depth: usize) -> CompactString {
    let mut f = CompactString::from("|#d = 1, x: i64| cast<string>(d)$");
    for i in 1..=depth {
        f = format_compact!("|#d: fn(x: i64) -> [string, 'r{i}] = {f}, x: i64| \"h\"");
    }
    format_compact!("{{ let f = {f}; f(7) }}")
}

// A label that is one constraint in the callee's own signature and in the
// function as the site holds it is judged once: the work grows with the
// depth, not with its power of two.
run!(
    defaults_holding_defaulted_fns_twenty_deep_behind_labels_that_hide_them_and_keep_a_variable,
    hidden_default_open_chain(20),
    is(Value::from("h"));
    FuseExpect::Jit
);

// A default that holds its own function reaches its own judgment, which
// is the one it gets.
run!(default_holding_its_own_fn, r#"
{
    let rec f = |#a: fn(b: i64) -> i64 = f, b: i64| -> i64 b;
    f(1)
}
"#, is(Value::I64(1)); FuseExpect::Jit);

run!(default_passing_its_own_fn, r#"
{
    let g = |h: fn(b: i64) -> i64| 0;
    let rec f = |#a: i64 = g(f), b: i64| -> i64 b;
    f(1)
}
"#, is(Value::I64(1)); FuseExpect::Jit);

// An abstract type's constructor holds its argument through the type's
// representation, which no reader of the payload names a function by: a
// default bounds the construction's parameter from below, so the
// declaration that takes the value may widen it and one that cannot hold
// the default refuses.
run!(default_behind_an_abstract_constructor, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let c = C(|#d = 1, x: i64| cast<string>(d)$);
    c.0(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_an_abstract_constructor_takes_a_passed_value_of_another_type, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let c = C(|#d = 1, x: i64| cast<string>(d)$);
    c.0(#d: "s", 7)
}
"#, prints("\"s\""); FuseExpect::None);

run!(default_behind_an_abstract_wider_annotation, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let c: C<[i64, string]> = C(|#d = 1, x: i64| cast<string>(d)$);
    "[c.0(7)] [c.0(#d: "s", 7)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s"); FuseExpect::None);

run!(default_of_another_type_behind_an_abstract_annotation, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let c: C<string> = C(|#d = 1, x: i64| cast<string>(d)$);
    c.0(7)
}
"#, refused_renumbered("type mismatch C<string> does not contain C<'_0>"); FuseExpect::None);

run!(default_of_another_type_behind_an_abstract_representation, r#"
{
    type C = Abstract<fn(?#d: string, x: i64) -> string>;
    let c = C(|#d = 1, x: i64| cast<string>(d)$);
    c.0(7)
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_behind_an_abstract_return_type, r#"
{
    type C<'z> = Abstract<{cb: fn(?#d: 'z, x: i64) -> string, n: i64}>;
    let mk = |j: i64| -> C<'z> C({cb: |#d = 1, x: i64| cast<string>(d)$, n: 1});
    mk(0).0.cb(7)
}
"#, refused_renumbered("type mismatch C<'z> does not contain C<'_0>"); FuseExpect::None);

run!(default_behind_an_abstract_return_type_written, r#"
{
    type C<'z> = Abstract<{cb: fn(?#d: 'z, x: i64) -> string, n: i64}>;
    let mk = |j: i64| -> C<i64> C({cb: |#d = 1, x: i64| cast<string>(d)$, n: 1});
    mk(0).0.cb(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

run!(default_behind_an_abstract_fits_its_factorys_variable, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let mk = |z: 'a| -> C<'a> C(|#d = z, x: i64| cast<string>(d)$);
    let a = mk(1);
    let b = mk("s");
    "[a.0(7)] [b.0(7)] [b.0(#d: "t", 7)]"
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s t"); FuseExpect::None);

run!(defaults_of_two_types_written_behind_an_abstract, r#"
{
    type C<'z> = Abstract<{cb: fn(?#d: 'z, x: i64) -> string, n: i64}>;
    let c = C({cb: |#d = 1, x: i64| cast<string>(d)$, n: 1});
    c <- once(c) ~ C({cb: |#d = "q", x: i64| cast<string>(d)$, n: 2});
    select c.0.n { 2 => c.0.cb(7), _ => never() }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "q"); FuseExpect::None);

run!(default_behind_a_callbacks_abstract_return, r#"
{
    type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>;
    let app = |f: fn(j: i64) -> C<'z>| f(0);
    let c = app(|j: i64| C(|#d = 1, x: i64| cast<string>(d)$));
    c.0(7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1"); FuseExpect::None);

// A union member built by the value's constructor holds the value, so a
// function under it meets the fn type there: a field of an optional
// struct, a tuple member, an array element of an option, a variant's
// payload.
run!(default_hidden_under_an_optional_struct, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [{f: fn(y: string) -> bool}, null] = {f: g};
    select t { null => false, s => (s.f)("seven") }
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_hidden_under_a_tuple_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [(fn(y: string) -> bool, i64), null] = (g, 1);
    select t { null => false, s => (s.0)("seven") }
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_hidden_under_an_element_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: Array<[{f: fn(y: string) -> bool}, null]> = [{f: g}];
    select t[0]$ { null => false, s => (s.f)("seven") }
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_hidden_under_a_variant_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [`A(fn(y: string) -> bool), `B(i64)] = `A(g);
    select t { `A(f) => f("seven"), `B(_) => false }
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

// Only the member with the value's constructor judges it.
run!(default_fits_its_variant_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [`A(fn(y: i64) -> bool), `B(fn(y: string) -> bool)] = `A(g);
    select t { `A(f) => f(4096), `B(f) => f("s") }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))); FuseExpect::None);

// A read-only reference is a value of no writable member: the read-only
// member alone holds it, and judges its function's default.
run!(default_hidden_under_a_read_only_reference_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [&fn(y: string) -> bool, &mut fn(y: i64) -> bool] = &g;
    g(4096)
}
"#, refused("cannot compare i64 with string: neither type holds the other, so the two are never equal"); FuseExpect::None);

run!(default_fits_its_read_only_reference_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [&fn(y: i64) -> bool, &mut fn(y: string) -> bool] = &g;
    g(4096)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))); FuseExpect::Jit);

// A default a fn type hides decides the type its function's instance runs
// with: here the target `str::parse` reads.
run!(default_hidden_by_a_fn_type_decides_no_compared_parse, r#"
{
    let g = |#x = 4096, y: string| x == str::parse(y)$;
    let apply = |h: fn(y: string) -> bool| h("4096");
    apply(g)
}
"#, refused("must be known here, an operation reads it to know what to make"); FuseExpect::None);

// A hidden label's type that only the result or the throws hold is fixed
// by the fn type's result or throws.
run!(default_hidden_by_a_result_type, r#"
{
    let f = |#n = 1, x: i64| (x, n);
    let h: fn(x: i64) -> (i64, string) = f;
    let s = h(1).1;
    (str::len(s), "[s]!")
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_result_element, r#"
{
    let f = |#d = 1, x| [d, x];
    let g: fn(x: string) -> Array<string> = f;
    g("s")
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_generic_formal_result, r#"
{
    let f = |#d = 1, x| [d, x];
    let h = |g: fn(x: 'q) -> Array<'q>, v: 'q| g(v);
    h(f, "s")
}
"#, prints("[i64:1, \"s\"]"); FuseExpect::None);

// holds i64 beside the string.
run!(default_hidden_by_a_generic_formal_result_reaches_the_result_refused, r#"
{
    let f = |#d = 1, x| [d, x];
    let h = |g: fn(x: 'q) -> Array<'q>, v: 'q| g(v);
    let r: Array<string> = h(f, "s");
    r
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_throws, r#"
{
    let f = |#e = `Small, x: i64| select x { 0 => error(e)?, x => x };
    let g: fn(x: i64) -> i64 throws Error<ErrChain<`Big>> = f;
    let r = 0;
    catch(e) r <- e ~ 1;
    g(0) ~ r
}
"#, refused("type mismatch `Big does not contain `Small"); FuseExpect::None);

// A default a fn type hides narrows the label of the function it is
// passed as, so what decides the result later meets the default's type.
run!(default_hidden_by_a_callback_result, r#"
{
    let r: Array<(i64, string)> = array::map([1, 2, 3], |#n = count(1), x: i64| (x, n));
    r
}
"#, refused("does not contain"); FuseExpect::None);

run!(default_hidden_by_a_generic_result, r#"
{
    let m = |f: fn(x: i64) -> 'b, x: i64| -> 'b f(x);
    let r: (i64, string) = m(|#n = 1, x: i64| (x, n), 3);
    r
}
"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(default_fits_a_result_type, r#"
{
    let f = |#d = 1, x| [d, x];
    let g: fn(x: i64) -> Array<i64> = f;
    g(5)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1), Value::I64(5)]); FuseExpect::Jit);

const DEFAULT_HIDDEN_BY_A_CALLBACK: &str = r#"
{
    let scale = |#by = 2, x| x * by;
    array::map([1.5, 2.5], scale)
}
"#;

run!(default_hidden_by_a_callback, DEFAULT_HIDDEN_BY_A_CALLBACK, refused("cannot compute with [i64, f64]: both operands must be one numeric type"); FuseExpect::None);

// Siblings that reach one ordered queuefn wrapper keep their serial
// order when the fields, arguments or operands would fork.
const ORDERED_SIBLINGS_STAY_SERIAL: &str = r#"
{
    let clk = sys::time::timer(duration:1.ms, true);
    let n = 0;
    n <- clk ~ n + 1;
    let fields: Array<i64> = [];
    let args: Array<i64> = [];
    let operands: Array<i64> = [];
    let qf = queuefn(#trigger: clk, |x: i64| -> i64 { fields <- x ~ array::push(fields, x); x });
    let qa = queuefn(#trigger: clk, |x: i64| -> i64 { args <- x ~ array::push(args, x); x });
    let qo = queuefn(#trigger: clk, |x: i64| -> i64 { operands <- x ~ array::push(operands, x); x });
    let f = |a: i64, b: i64, c: i64, d: i64, e: i64, g: i64| -> i64 a + b + c + d + e + g;
    let t = (qf(1), qf(2), qf(3), qf(4), qf(5), qf(6));
    let s = f(qa(1), qa(2), qa(3), qa(4), qa(5), qa(6));
    let o = ((qo(1) + qo(2)) + (qo(3) + qo(4))) + (qo(5) + qo(6));
    select n { x if x > 9 => (fields, args, operands), _ => never() }
}
"#;

run!(ordered_siblings_stay_serial, ORDERED_SIBLINGS_STAY_SERIAL, |v: Result<&Value>| {
    let seq = || Value::Array((1..=6).map(Value::I64).collect());
    matches!(v, Ok(Value::Array(a)) if a[..] == [seq(), seq(), seq()])
}; FuseExpect::Jit);

// Slots that reach one ordered queuefn wrapper keep their serial order.
const ORDERED_SLOTS_STAY_SERIAL: &str = r#"
{
    let i = 0;
    let clock = sys::time::timer(duration:1.ms, true);
    i <- clock ~ i + 1;
    let q = queuefn(#trigger: clock, |x: i64| -> i64 x);
    let r = array::map(array::init(16, |j| j), |x| (q(x) ~ i, x));
    select i {
        n if n > 20 => array::find(array::init(15, |j| j), |j| r[j + 1]$.0 < r[j]$.0),
        _ => never()
    }
}
"#;

run!(ordered_slots_stay_serial, ORDERED_SLOTS_STAY_SERIAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; FuseExpect::Jit);

// Slots whose callback changes to one that calls an ordered builtin run
// in order from the cycle it changes: the queue sees its calls in slot
// order.
const ORDERED_SLOTS_AFTER_A_CALLBACK_CHANGE: &str = r#"
{
    let clk = sys::time::timer(duration:1.ms, true);
    let n = 0;
    n <- clk ~ n + 1;
    let got: Array<i64> = [];
    let pop = clk ~! select n { k if k >= 3 => true, _ => never() };
    let q = queuefn(#trigger: pop, |x: i64| -> i64 { got <- x ~ array::push(got, x); x });
    let g1 = |x: i64| -> i64 x;
    let g2 = |x: i64| -> i64 q(x);
    let f = select n { k if k < 3 => g1, _ => g2 };
    let r = array::map(array::init(8, |j| j + 100 * n), f);
    select n {
        k if k > 20 => (
            got[0]$,
            array::find(array::init(9, |j| j), |j| got[j + 1]$ < got[j]$)
        ),
        _ => never()
    }
}
"#;

run!(
    ordered_slots_after_a_callback_change,
    ORDERED_SLOTS_AFTER_A_CALLBACK_CHANGE,
    prints("[i64:300, null]");
    FuseExpect::Jit
);

// A lambda two definitions deep reads the outermost one's cells renamed
// through every enclosing instance.
const NESTED_TWO_DEEP: &str = r#"
{
    let outer = |d, s: string| {
        let middle = |y: string| {
            let inner = |z: string| str::parse(z)$ == d;
            inner(y)
        };
        middle(s)
    };
    (outer(1.0, "1"), outer(7, "7"))
}
"#;

run!(nested_two_deep, NESTED_TWO_DEEP, refused("must be known here, an operation reads it to know what to make"); FuseExpect::None);

// A builtin bound where the site's view omits a defaulted label is
// typed at the instance's signature, which lists it.
const BUILTIN_BOUND_WITHOUT_ITS_DEFAULT: &str = r#"
{
    let apply = |f: fn(a: Array<i64>) -> Array<i64>, a: Array<i64>| f(a);
    apply(array::rotate, [1, 2, 3])
}
"#;

run!(builtin_bound_without_its_default, BUILTIN_BOUND_WITHOUT_ITS_DEFAULT, prints("[i64:3, i64:1, i64:2]"); FuseExpect::None);

// A builtin's variadic type is written like its other types: a trait
// parameter in a function type there is refused as it is elsewhere.
const BUILTIN_VARIADIC_TRAIT: &str = r#"
{
    let f = |@args: fn(x: Eq) -> i64| -> Any 'core_all;
    let r = f(|x| 1, |x| 2);
    0
}
"#;

run!(
    builtin_variadic_trait,
    BUILTIN_VARIADIC_TRAIT,
    refused("stands only as a definition's signature");
    FuseExpect::None
);

// A builtin binding in a collection callback: forked slots each compile it,
// and the binding is entered once.
const BUILTIN_BOUND_IN_FORKED_SLOTS: &str = r#"
array::map([1, 2], |x: i64| {
    let f = |@args: fn(x: i64) -> i64| -> Any 'core_all;
    let r = f(|y| y, |y| y);
    x
})
"#;

run!(builtin_bound_in_forked_slots, BUILTIN_BOUND_IN_FORKED_SLOTS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Array(a)) if &a[..] == [Value::I64(1), Value::I64(2)]
); FuseExpect::None);

// An abstract type whose representation names itself, defined in a
// function body: each instance defines it again, the same.
const RECURSIVE_ABSTRACT_IN_A_BODY: &str = r#"
{
    let f = |x: i64| {
        type N = Abstract<[`Cons(i64, N), `Nil]>;
        let a = N(`Cons(x, N(`Nil)));
        a == N(`Cons(1, N(`Nil)))
    };
    (f(1), f(2))
}
"#;

run!(recursive_abstract_in_a_body, RECURSIVE_ABSTRACT_IN_A_BODY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Array(a)) if &a[..] == [Value::Bool(true), Value::Bool(false)]
); FuseExpect::Jit);

// A parametric abstract type in a function body: each instance's
// representation is written over the definition's own formals.
run!(
    parametric_abstract_in_a_body,
    r#"{
        let f = |b: i64| {
            type B<'p> = Abstract<'p>;
            let x = B(b);
            x.0 + 1
        };
        (f(1), f(2))
    }"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::Array(a)) if &a[..] == [Value::I64(2), Value::I64(3)]
    )
);

// An abstract type whose representation names a typedef its block
// declares after it, in a function body: each instance compares the
// representation once both are declared.
run!(
    abstract_naming_a_later_typedef_in_a_body,
    r#"{
        let f = |x: i64| {
            type N = Abstract<[`C(M), `Nil]>;
            type M = [N, i64];
            N(`C(x)) == N(`C(1))
        };
        (f(1), f(2))
    }"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::Array(a)) if &a[..] == [Value::Bool(true), Value::Bool(false)]
    )
);

// The representation's `M` is the body's own, which shadows the block's:
// a static call's instance and each slot of a run-time bind declare N
// again before that `M`, and take it.
run!(
    abstract_naming_a_shadowing_typedef_in_a_body,
    r#"{
        type M = i64;
        let f = |x: i64| {
            type N = Abstract<[`C(M), `Nil]>;
            type M = string;
            select N(`C("[x]")) { N(`C(s)) => s, N(`Nil) => "nil" }
        };
        (f(1), array::map([2, 3], f))
    }"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(t)) => match &t[..] {
            [Value::String(a), Value::Array(b)] => {
                &**a == "1"
                    && matches!(&b[..], [Value::String(x), Value::String(y)]
                        if &**x == "2" && &**y == "3")
            }
            _ => false,
        },
        _ => false,
    }
);

run!(
    abstract_naming_a_shadowing_typedef_in_a_body_refuses_the_outer,
    r#"{
        type M = i64;
        let f = |x: i64| {
            type N = Abstract<[`C(M), `Nil]>;
            type M = string;
            select N(`C(x)) { N(`C(s)) => s, N(`Nil) => "nil" }
        };
        f(1)
    }"#,
    refused("does not contain `C(i64)")
);

// A generic lambda literal a function body uses at one type: each instance
// of the body types the literal as the definition's check did, its
// quantifier decided.
run!(
    a_body_lambdas_quantifier_decided_by_its_use,
    r#"{
        let f = |x: i64| { let l: fn(s: f64) -> f64 = 'a: Number |s: 'a| -> 'a s; l(1.5) };
        f(1)
    }"#,
    is(Value::F64(1.5))
);

run!(
    a_body_lambdas_quantifier_decided_through_an_array,
    r#"{
        let g = |a: Array<'q>| array::len(a);
        let f = |x: i64| g(['a: Number |#start: 'a = 0.0| -> 'a start]);
        f(1)
    }"#,
    is(Value::I64(1));
    FuseExpect::None
);

const DEFAULT_OF_A_BOUNDED_PARAMETER: &str = r#"
{
    let scale = |k| {
        let mul = |#by = k, x| x * by;
        mul(3)
    };
    scale(2)
}
"#;

run!(default_of_a_bounded_parameter, DEFAULT_OF_A_BOUNDED_PARAMETER,
    prints("i64:6"); FuseExpect::None);

run!(default_of_a_closed_bounded_parameter, r#"{
    let scale = |k: i64| {
        let mul = |#by = k, x| x * by;
        mul(3)
    };
    scale(2)
}"#, is(Value::I64(6)); FuseExpect::Jit);

const DEFAULT_FROM_THE_ENVIRONMENT: &str = r#"
{
    let d = never();
    let f = 'a: Number |#x: 'a = d, y: 'a| [x, y];
    d <- 2;
    (f(3), d + 1)
}
"#;

run!(default_from_the_environment, DEFAULT_FROM_THE_ENVIRONMENT, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.len() == 2
        && a[0] == Value::Array([Value::I64(2), Value::I64(3)].into())
        && a[1] == Value::I64(3),
    _ => false,
}; FuseExpect::Jit);

// A destructuring let that rebinds a builtin-bound name forgets the
// builtin: the call reaches the new function.
const DESTRUCTURING_REBINDS_A_BUILTIN: &str = r#"
{
    let slen = |s: string| -> i64 'str_len;
    let (slen, k) = (|s: string| -> i64 42, 0);
    let f = |s: string| slen(s);
    f("abc")
}
"#;

run!(destructuring_rebinds_a_builtin, DESTRUCTURING_REBINDS_A_BUILTIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

// A site whose function changes binds the new definition over its quiet
// arguments as they stand: `x` fired once, at init.
async fn rebind_reads_quiet_args(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(
        r#"{
            let t = array::iter([0, 1]);
            let x = 5;
            let f = select t { 0 => |v| v + 1, _ => |v| v * 10 };
            f(x)
        }"#,
        mode,
    )
    .await?;
    assert_eq!(as_i64s(&values), vec![6, 50]);
    Ok(())
}

modes!(rebind_reads_quiet_args);

// A default whose parameter's type nests a declared variable fits some
// instance of it; each omitting call checks it with its own.
const NESTED_TVAR_DEFAULT: &str = r#"
{
  let f = |#x: Array<'a> = [1], y: 'a| -> 'a y;
  let g = 'a: Number |#x: Array<'a> = [1], y: 'a| -> 'a array::fold(x, y, |a, b| a + b);
  "[f(2)] [f(#x: ["s"], "t")] [g(3)] [g(#x: [1.5], 2.0)]"
}
"#;

run!(nested_tvar_default, NESTED_TVAR_DEFAULT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "2 t 4 3.5")
});

const NESTED_TVAR_DEFAULT_REFUSED: &str = r#"
{
  let f = |#x: Array<'a> = [1], y: 'a| -> 'a y;
  f("s")
}
"#;

run!(
    nested_tvar_default_widens,
    NESTED_TVAR_DEFAULT_REFUSED,
    prints("\"s\"");
    FuseExpect::Jit
);

/// An embedder bounds the activations its runtime holds: a node-walked
/// recursion without end aborts the runtime at the budget, where it would
/// otherwise fill memory, and the count follows the program.
/// `code` under a 2000-activation budget, built with `flags`: the runtime
/// stops it there, holding `live` activations.
async fn budget_stops(
    flags: enumflags2::BitFlags<graphix_compiler::CFlag>,
    code: &str,
    live: std::ops::Range<u64>,
) -> anyhow::Result<()> {
    use graphix_package_core::testing::{HANG_GUARD, TestCtx, config};
    let (tx, _rx) = tokio::sync::mpsc::channel(10);
    let ctx =
        TestCtx::start(config(tx, &crate::TEST_REGISTER, vec![], |_| {})?.flags(flags))
            .await?;
    ctx.rt.control().set_activation_budget(2000);
    ctx.rt.compile(arcstr::ArcStr::from(code)).await?;
    let deadline = tokio::time::Instant::now() + HANG_GUARD;
    while !ctx.rt.budget_aborted() && tokio::time::Instant::now() < deadline {
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
    }
    assert!(ctx.rt.budget_aborted(), "the runaway recursion was not stopped");
    let held = ctx.rt.control().live_activations();
    assert!(live.contains(&held), "{held} live activations");
    ctx.shutdown().await;
    Ok(())
}

/// The node-walk counts a recursion's calls.
#[tokio::test(flavor = "multi_thread")]
async fn activation_budget_stops_a_runaway_recursion() -> anyhow::Result<()> {
    budget_stops(
        graphix_compiler::CFlag::FusionDisabled.into(),
        r#"{
            let rec go = |n: i64, acc: string| -> string select n { 0 => acc, _ => go(n - 1, "x") };
            go(-1, "x")
        }"#,
        2000..10_000,
    )
    .await
}

/// A fused non-tail recursion counts its kernel blocks.
#[tokio::test(flavor = "multi_thread")]
async fn activation_budget_stops_a_fused_runaway_recursion() -> anyhow::Result<()> {
    budget_stops(
        Default::default(),
        r#"{
            let rec f = |n: i64| -> i64 select n { 0 => 0, _ => n + f(n - 1) };
            #[native] f(-1)
        }"#,
        2000..10_000,
    )
    .await
}

/// A native tail loop holds one activation and queues what it raises: the
/// queue counts.
#[tokio::test(flavor = "multi_thread")]
async fn activation_budget_stops_a_fused_runaway_loop_that_raises() -> anyhow::Result<()>
{
    budget_stops(
        Default::default(),
        r#"{
            let e: Result<i64, `E> = error(`E);
            let caught = 0;
            catch(x) caught <- x ~ 1;
            #[tail_recursive]
            let rec f = |n: i64, acc: i64| -> i64 {
                let k = e?;
                select n { 0 => acc + k, _ => f(n * 1, acc + n) }
            };
            f(3, 0)
        }"#,
        0..100,
    )
    .await
}

// A sampled function value is written without parens: a lambda is the last
// operand of its chain.
run!(lambda_sampled_on_a_trigger, r#"{
  let t = 1;
  let f = |x: i64| x * 10;
  f <- t ~ |x: i64| x + 1;
  let g = t ~ 'a: Number |x: 'a| -> 'a x + x;
  select f(5) { 6 => g(4), _ => never() }
}"#, is(Value::I64(8)); FuseExpect::None);

// A declared variable beside a concrete member cannot be made one type:
// the body is refused, not the variable narrowed to the member.
run!(
    rigid_member_beside_a_known_one_is_judged_at_the_call,
    prints("i64:1"),
    "/test.gx" => r#"
        let g = 'a: Singleton |x: 'a| -> 'a x;
        let f = 'r: Number |x: 'r, b: bool| g(select b { true => x, false => 1 });
        let result = f(3, false)
    "#
; FuseExpect::Jit);

// another numeric type the member beside the declared variable makes two.
run!(
    rigid_member_beside_a_known_one_at_another_number_refused,
    refused("cannot compute with [i64, f64]"),
    "/test.gx" => r#"
        let g = 'a: Singleton |x: 'a| -> 'a x;
        let f = 'r: Number |x: 'r, b: bool| g(select b { true => x, false => 1 });
        let result = f(3.5, false)
    "#
; FuseExpect::None);

// A settled cell keeps its relation to the live cells its bound names:
// 'b settles to Array<'a> with 'a the caller's i64, so h takes no array
// of strings, at the check as at the build.
run!(
    a_settled_bound_keeps_its_live_cells,
    refused("a test cannot name a declared type variable ('a): write `_` there, and the name takes the value's own type"),
    "/test.gx" => r#"
        let f = 'b: Array<'a> |x: 'a| -> fn(v: 'b) -> 'a |v: 'b| select v {
          Array<'a> as [h, ..] => h,
          _ => x
        };
        let g = |y| {
          let k = f(y);
          let z: i64 = y;
          k
        };
        let h = g(1);
        let result = h(["a", "b"])
    "#
; FuseExpect::None);

// A function value no binding names is one instance: an element of a
// collection a call returns is, and a callback calling it at two types is
// refused.
// CR claude for claude: [test-gap/medium] This fixture is named for elements staying
// generic but pins the opposite. Its refusal does not come from the rule its comment
// states: the callback's parameter `f` is one instance on its own. Over a generic
// holder (`let fs = [h]` with a let-bound generic h, whose direct reads check at two
// types), the same map is refused with the same message. A change that made an element
// of a call's result generic would pass this fixture. It would also pass
// collection_returned_function_is_one_instance and the generics.md case, which read
// through `let g = fs[0]$`, an index read that is one instance anyway. Pin the rule
// with direct reads: `(fs[0]$(1, 2), fs[0]$(f32:1.5, f32:2.0))` over `array::map(..)`
// refused, beside the same reads over `let fs = [h]` checking. Then rename or fold this
// fixture. probe:
// design/review-2026-11-16/repro/tests-functions-b-call-result-element-rule-unpinned.sh
// (tests-functions-b-04)
run!(
    generic_function_elements_stay_generic,
    r#"{
        let fs = array::map([1, 2], |x| 'a: Number |x: 'a, y: 'a| -> 'a x + y);
        array::map(fs, |f| (f(1, 2), f(f32:1.5, f32:2.0)))
    }"#,
    refused("cannot compute with [i64, f32]: both operands must be one numeric type");
    FuseExpect::None
);

// A parameter the body unifies with a declared quantifier is that
// quantifier in every copy of the signature: a call site's settle leaves
// it to the function value's callers.
run!(
    generic_function_element_with_an_unannotated_parameter,
    r#"{
        let tm = |x| 0;
        array::map(['a: Number |x, y: 'a| -> 'a x + y, 2], tm)
    }"#,
    prints("[i64:0, i64:0]");
    FuseExpect::None
);

run!(
    generic_function_value_with_an_unannotated_parameter,
    r#"{
        let f = 'a: Number |x, y: 'a| -> 'a x + y;
        let tm = |x| 0;
        tm(f)
    }"#,
    refused("cannot compute with Number: both operands must be one numeric type");
    FuseExpect::Jit
);

/// The statements of a block elaborate in compile tasks of their own, in
/// parallel. A quantifier the check left open in a type both read (`m`'s
/// element) is compared as a fresh copy and written by neither, so the
/// build accepts the program whatever order the tasks run in.
#[tokio::test(flavor = "multi_thread")]
async fn generic_function_elements_elaborate_in_any_task_order() -> Result<()> {
    use graphix_package_core::testing::init;
    const RUNS: usize = 60;
    let code = arcstr::literal!(
        "{ let m = array::map([1, 2], |x| 'a: Number |x: 'a, y: 'a| -> 'a x + y); \
         array::len(m) }"
    );
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut refused: Vec<String> = Vec::new();
    for _ in 0..RUNS {
        if let Err(e) = ctx.rt.compile(code.clone()).await {
            refused.push(format!("{e:#}"));
        }
    }
    ctx.shutdown().await;
    assert!(
        refused.is_empty(),
        "{} of {RUNS} builds refused: {}",
        refused.len(),
        refused[0]
    );
    Ok(())
}

// Two copies of a generic function value meet as one generic member
// wherever they stand together (an array, a select's arms), so each
// call through either picks the quantifier anew: an unannotated
// parameter the body unifies with the declared quantifier names its
// cell under a second name, and nothing binds the quantifier.
// CR claude for eric: [bug/medium] The comment above says copies of g in an array 'meet
// as one generic member ... each call through either picks the quantifier anew'.
// design/tvar_constraints.md and generics.md say a let of an array literal holding
// functions is generalized, with an annotation deciding only its own read's copy. As
// built, that holds for `[g]` and `[|x| x]` but not for `[g, g]` or `[|x| x, |y| y]`.
// The meet leaves the element an instance's cells, which a read does not copy, so `[g,
// g]` called at u8 then at i64 is refused, and two annotations of `[|x| x, |y| y]` at
// i64 and string are refused; the one-element versions check. The repl_inputs.rs pins
// encode the monomorphic verdict, and no fixture here calls two elements at two types
// (nested_array_of_generic_lambdas calls both at i64). Please rule whether the number
// of functions in a literal may decide whether its binding is generic. I think it
// should not, since ML generalizes `[id; id]`. Then fix the docs or the build, rewrite
// this comment, and pin calls at two types. probe:
// design/review-2026-11-16/repro/tests-functions-b-array-of-two-fns-not-generic.sh
// (tests-functions-b-02)
const GENERIC_ADD: &str = "let g = 'q: Number |x, y: 'q| -> 'q x + y;";

run!(
    generic_function_copies_in_an_array,
    is(Value::I64(3)),
    "/test.gx" => format!("{GENERIC_ADD} let fs = [g, g]; let result = fs[0]$(1, 2)")
; FuseExpect::None);

run!(
    generic_function_copies_mapped,
    prints("[i64:3, i64:3]"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let fs = [g, g]; let result = array::map(fs, |f| f(1, 2))"
    )
; FuseExpect::None);

run!(
    generic_function_copies_mapped_at_another_type,
    prints("[u8:3, u8:3]"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let result = array::map([g, g], |f| f(u8:1, u8:2))"
    )
; FuseExpect::None);

run!(
    generic_function_copies_through_a_select,
    prints("u8:3"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let s = select 1 {{ 0 => g, _ => g }}; let result = s(u8:1, u8:2)"
    )
; FuseExpect::None);

run!(
    generic_function_literals_in_an_array,
    refused("cannot compute with [u8, f64]: both operands must be one numeric type"),
    "/test.gx" => r#"
        let fs = ['q: Number |x, y: 'q| -> 'q x + y, 'q: Number |x, y: 'q| -> 'q x * y];
        let result = array::map(fs, |f| (f(u8:1, u8:2), f(1.5, 2.5)))
    "#
; FuseExpect::None);

run!(
    generic_function_beside_a_function_literal_in_an_array,
    refused("cannot compute with [u8, f64]: both operands must be one numeric type"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let fs = [|x, y| x * y, g]; \
         let result = array::map(fs, |f| (f(u8:2, u8:3), f(1.5, 2.0)))"
    )
; FuseExpect::None);

run!(
    generic_function_returned_through_a_select,
    refused("cannot compute with [u8, f64]: both operands must be one numeric type"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let pick = |b: bool| select b {{ true => g, false => g }}; \
         let h = pick(true); let result = (h(u8:2, u8:3), h(1.5, 2.0))"
    )
; FuseExpect::None);

// The copies' quantifier is held rigid when the array's type is checked
// against what is written to it: a monomorphic function is no generic one,
// and the earlier call at u8 relies on the quantifier staying open.
// CR claude for claude: [test-gap/medium] The comment above says the copies' quantifier
// stays open after `fs[0]$(u8:1, u8:2)` and is held rigid against the write. It does
// not stay open. `fs` is written, so it is one instance and the u8 call decides it: the
// refusal reads 'fs is Array<fn(x: u8, y: u8) -> u8> and cannot hold ...'. The same
// write of a monomorphic u8 function checks. This fixture therefore passes on a plain
// u8/i64 mismatch, and would still pass if the rigid hold of a generic holder against a
// write were lost. Rewrite the comment to what it pins and add the u8 write as the
// accepted twin. Pin the rigid hold where it still applies: a write a later input or a
// module body makes to a generic holder. probe:
// design/review-2026-11-16/repro/tests-functions-b-copies-monomorphic-write.sh
// (tests-functions-b-03)
run!(
    generic_function_copies_refuse_a_monomorphic_write,
    refused("type mismatch i64 does not contain u8"),
    "/test.gx" => format!(
        "{GENERIC_ADD} let fs = [g, g]; let a = fs[0]$(u8:1, u8:2); \
         fs <- [|x: i64, y: i64| x * y]; let result = a"
    )
; FuseExpect::None);

/// A later input copies an earlier input's generic function value twice,
/// and the copies stay generic, as in one program.
#[tokio::test(flavor = "current_thread")]
async fn generic_function_copies_in_a_later_input() -> Result<()> {
    use graphix_package_core::testing::{init, next_update};
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _g = ctx.rt.compile(arcstr::ArcStr::from(GENERIC_ADD)).await?;
    let mut values = Vec::new();
    for input in ["array::map([g, g], |f| f(u8:1, u8:2))", "g(1.5, 2.5)"] {
        let res = ctx.rt.compile(arcstr::ArcStr::from(input)).await?;
        values.push(next_update(&ctx.rt, &mut rx, res.exprs[0].id).await?);
        drop(res);
    }
    ctx.shutdown().await;
    assert!(
        matches!(&values[..], [Value::Array(a), Value::F64(f)]
            if a[..] == [Value::U8(3), Value::U8(3)] && *f == 4.0),
        "{values:?}"
    );
    Ok(())
}

// A callee's own declared quantifier that only its return holds is the
// caller's to narrow: its check held it rigid, so the body produced
// nothing into it.
run!(declared_quantifier_in_the_return_narrows, r#"
{
    let f = 'b: Any |s: string| -> Array<'b> [];
    let r: Array<i64> = f("1");
    array::len(r)
}
"#, is(Value::I64(0)); FuseExpect::Jit);

run!(declared_bound_quantifier_in_the_return_narrows, r#"
{
    type E = `ParseError(string);
    let f = 'b: Number + Concrete |s: string| -> Result<'b, E> str::parse(s);
    let r: Result<i64, E> = f("12");
    r$ + 1
}
"#, is(Value::I64(13)); FuseExpect::Jit);

// A builtin or collection intrinsic bound with a callback type of
// another arity than it calls is refused, never built short.
run!(builtin_callback_arity_refused, r#"{
    let g = |a: i64, f: fn() -> bool| -> Array<i64> 'array_group;
    g(1, || true)
}"#, refused("this function is called with 2 arguments, but its type fn() -> bool takes 0"); FuseExpect::None);

run!(intrinsic_callback_arity_refused, r#"{
    let g = |a: Array<i64>, f: fn() -> bool| -> Array<i64> 'array_filter;
    g([1], || true)
}"#, refused("this collection's callback takes 1 argument, but its type fn() -> bool takes 0"); FuseExpect::None);

// A quantified lambda handed to a builtin that calls it back: the
// formal's quantifier is the argument's own, generic in it, not a
// variable of its environment.
run!(quantified_lambda_through_a_builtin_callback, r#"{
    let gs = array::group('a: Number |x: 'a| -> 'a x, |n, _| n == 1);
    array::map(gs, |f| f(2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2)]); FuseExpect::None);

// A compile job stolen while a statement waits in a join runs under its
// own context: the defaults in check are the compile's, not the
// thread's, so every `f(i)` omitting `#k` resolves statically and fuses.
const STOLEN_JOB_CALLS: i64 = 60;

run!(omitted_defaults_resolve_under_stolen_jobs, {
    use std::fmt::Write;
    let mut src = poolshark::local::LPooled::<String>::take();
    src.push_str("{\n  let f = |#k = 1, x: i64| -> i64 { let a = x + k; let c = a * 3; let b = c * 2; b };\n");
    for i in 0..STOLEN_JOB_CALLS {
        writeln!(src, "  let r{i} = #[native] f({i});").unwrap();
    }
    src.push_str("  [");
    for i in 0..STOLEN_JOB_CALLS {
        write!(src, "r{i}, ").unwrap();
    }
    src.push_str("]\n}");
    arcstr::ArcStr::from(src.as_str())
}, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if as_i64s(a) == (1..=STOLEN_JOB_CALLS).map(|i| i * 6).collect::<Vec<i64>>())
}; FuseExpect::Jit);

// A recursive call omitting a labeled default is a self-call, not a site
// its own default's check keeps dynamic: it fuses under #[native] (the
// recursion runs at k = 1: 5 * 2 + 4 + 3 + 2 + 1).
run!(
    omitted_default_self_call_fuses,
    r#"{
  let rec f = |#k: i64 = i64:1, n: i64| -> i64 select n { m if m <= i64:0 => i64:0, m => m * k + f(m - i64:1) };
  let g = |x: i64| -> i64 #[native] f(#k: i64:2, x);
  g(i64:5)
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(20)))
);

// An argument whose type is an open cell takes the variable as it
// stands, and the variable widens no more: a wider argument before it
// runs, one after it is refused.
run!(
    open_argument_takes_the_variable_as_it_stands,
    r#"{
  let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick(v, z, i64:1);
  h(null)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[null, null, i64:1]")
);

// CR claude for claude: [test-gap/low] This refusal names two fixes: 'pass the widest
// argument first, or annotate the undecided one'. Only the reordered twin is pinned.
// The annotated twin, `let h = |z: [i64, null]| pick(i64:1, z, v)`, checks, but no
// fixture holds it, and the nested form below is the same. Add both annotated twins
// (they print `[i64:1, null, null]` and `[[i64:1], [null], [null]]`) so a change that
// breaks the told fix fails. probe:
// design/review-2026-11-16/repro/tests-functions-b-undecided-arg-annotation-twin.sh
// (tests-functions-b-06)
run!(
    open_argument_before_a_wider_one_widens,
    r#"{
  let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick(i64:1, z, v);
  h(null)
}"#,
    prints("[i64:1, null, null]");
    FuseExpect::Jit
);

// The same with the open cell nested inside the argument.
run!(
    nested_open_argument_takes_the_variable_as_it_stands,
    r#"{
  let pick = |x: Array<'a>, y: Array<'a>, z: Array<'a>| -> Array<Array<'a>> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick([v], [z], [i64:1]);
  h(null)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[[null], [null], [i64:1]]")
);

run!(
    nested_open_argument_before_a_wider_one_widens,
    r#"{
  let pick = |x: Array<'a>, y: Array<'a>, z: Array<'a>| -> Array<Array<'a>> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick([i64:1], [z], [v]);
  h(null)
}"#,
    prints("[[i64:1], [null], [null]]");
    FuseExpect::Jit
);

// A generic function held at a written function type meets the type's
// positions in the order written, and nothing waits: a variable takes the
// first position's type and the later ones must fit it, two parts of one
// position being two positions. The written type's earlier position is as
// wide as its later ones need.
run!(
    written_fn_type_first_position_decides,
    r#"{
  let f = |b: 'y, k: 'y, j: 'y| -> 'y k;
  let p = |t: ('y, 'y)| -> 'y t.0;
  let g: fn(b: [i64, null], k: null, j: i64) -> [i64, null] = f;
  let q: fn(t: ([i64, null], i64)) -> [i64, null] = p;
  (g(1, null, 2), q((3, 4)))
}"#,
    prints("[null, i64:3]")
);

run!(
    written_fn_type_wider_later_position_widens,
    r#"{ let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: [i64, null]) -> [i64, null] = f; g(1, null) }"#,
    prints("null");
    FuseExpect::Jit
);

// The same wherever the function meets the written type: a callback
// argument, a struct's field, a write.
run!(
    written_fn_type_first_position_everywhere,
    r#"{
  let f = |b: 'y, k: 'y| -> 'y k;
  let app = |h: fn(b: [i64, null], k: i64) -> [i64, null]| h(null, 1);
  let s: {h: fn(b: [i64, null], k: i64) -> [i64, null]} = {h: f};
  let w: fn(b: [i64, null], k: i64) -> [i64, null] = never();
  w <- f;
  (app(f), (s.h)(1, 2), w(1, 3))
}"#,
    prints("[i64:1, i64:2, i64:3]")
);

// A declared variable of the enclosing definition is a closed type there.
run!(
    written_fn_type_first_position_declared,
    r#"{
  let outer = |a: 'a, n: ['a, null]| {
    let f = |b: 'y, k: 'y| -> 'y k;
    let g: fn(b: ['a, null], k: 'a) -> ['a, null] = f;
    g(n, a)
  };
  outer(1, null)
}"#,
    prints("i64:1")
);

// The first position types the result too: a narrower written result is
// refused.
run!(
    written_fn_type_first_position_types_the_result,
    r#"{ let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: [i64, null], k: i64) -> i64 = f; g(1, 2) }"#,
    refused("type mismatch i64 does not contain [i64, null]");
    FuseExpect::None
);

run!(
    written_fn_type_widest_position_types_the_result_reversed,
    r#"{ let f = |k: 'y, b: 'y| -> 'y k; let g: fn(k: [i64, null], b: i64) -> i64 = f; g(null, 1) }"#,
    refused("type mismatch i64 does not contain [i64, null]");
    FuseExpect::None
);

// No position holds the others: refused, in either order, as a call is.
run!(
    written_fn_type_no_widest_position_widens,
    r#"{ let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: string) -> i64 = f; g(1, "s") }"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    written_fn_type_no_widest_position_widens_reversed,
    r#"{ let f = |k: 'y, b: 'y| 0; let g: fn(k: string, b: i64) -> i64 = f; g("s", 1) }"#,
    prints("i64:0");
    FuseExpect::Jit
);

// A union's member beside closed members holds the variable as data, and
// decides it where it is the first position.
run!(
    written_fn_type_position_in_a_union,
    r#"{
  let f = |b: ['y, null], k: 'y| -> 'y k;
  let f2 = |k: 'y, b: ['y, null]| -> 'y k;
  let g: fn(b: [i64, null], k: i64) -> i64 = f;
  let h: fn(k: [i64, string], b: [i64, null, string]) -> [i64, string] = f2;
  (g(1, 5), h(2, "t"))
}"#,
    prints("[i64:5, i64:2]")
);

// A position that holds the variable exactly (a writable reference, a
// nominal type) decides it as any other does, where it is written: first,
// the others fit it; after a narrower one it is refused.
run!(
    written_fn_type_exact_position_first,
    r#"{
  type B<'a> = Abstract<'a>;
  let f = |p: &mut 'y, k: 'y| -> 'y k;
  let f2 = |b: B<'y>, k: 'y| -> 'y k;
  let x: [i64, null] = null;
  let b: B<[i64, null]> = B(null);
  let g: fn(p: &mut [i64, null], k: i64) -> [i64, null] = f;
  let h: fn(b: B<[i64, null]>, k: i64) -> [i64, null] = f2;
  (g(&mut x, 1), h(b, 2))
}"#,
    prints("[i64:1, i64:2]")
);

run!(
    written_fn_type_exact_position_after_a_narrower_one_takes_its_type,
    r#"{
  let f = |k: 'y, p: &mut 'y| -> 'y k;
  let x: [i64, null] = null;
  let g: fn(k: i64, p: &mut [i64, null]) -> [i64, null] = f;
  g(1, &mut x)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    written_fn_type_exact_position_narrower_refused,
    r#"{
  let f = |k: 'y, p: &mut 'y| -> 'y k;
  let x = 1;
  let g: fn(k: [i64, null], p: &mut i64) -> [i64, null] = f;
  g(null, &mut x)
}"#,
    refused("type mismatch i64 does not contain [i64, null]");
    FuseExpect::None
);

run!(
    written_fn_type_exact_position_narrower_refused_reversed,
    r#"{
  let f = |p: &mut 'y, k: 'y| -> 'y k;
  let x = 1;
  let g: fn(p: &mut i64, k: [i64, null]) -> [i64, null] = f;
  g(&mut x, null)
}"#,
    refused("type mismatch i64 does not contain [i64, null]");
    FuseExpect::None
);

// A variable a function type also holds takes the first position's type,
// as at a call: the written type's first position is written as wide as
// the later ones need.
run!(
    written_fn_type_callback_variable_first_position,
    r#"{
  let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k);
  let g: fn(b: Any, k: i64, c: fn(v: Any) -> i64) -> i64 = f;
  g(1, 2, |v| 7)
}"#,
    prints("i64:7");
    FuseExpect::None
);

run!(
    written_fn_type_callback_variable_first_position_widens,
    r#"{
  let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k);
  let g: fn(b: i64, k: Any, c: fn(v: Any) -> i64) -> i64 = f;
  g(1, 2, |v| 7)
}"#,
    prints("i64:7");
    FuseExpect::None
);

// So does one a recursive typedef holds under a function type where it
// applies itself to other parameters.
run!(
    written_fn_type_typedef_callback_variable_first_position_widens,
    r#"{
  type S<'a, 'b> = [`N, `C('a, fn(x: 'b) -> i64, S<'b, 'a>)];
  let f = |a: 'x, b: 'y, k: S<'x, 'y>| a;
  let g: fn(a: i64, b: i64, k: S<i64, Any>) -> i64 = f;
  g(1, 2, `N)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A variable an application of a typedef holds as data is decided in
// order as any other, the typedef reaching itself or not: two applications
// over a variable are related through their expansions.
run!(
    written_fn_type_typedef_variable_after_its_position,
    r#"{
  type T<'a> = [`Leaf('a), `Node(T<i64>)];
  type W<'a> = {inner: T<'a>};
  let f = |k: 'y, t: T<'y>| -> 'y k;
  let fw = |k: 'y, w: W<'y>| -> 'y k;
  let g: fn(k: [i64, null], t: T<i64>) -> [i64, null] = f;
  let gw: fn(k: [i64, null], w: W<i64>) -> [i64, null] = fw;
  let app = |h: fn(k: [i64, null], t: T<i64>) -> [i64, null]| h(4, `Leaf(1));
  (g(null, `Leaf(1)), gw(2, {inner: `Leaf(1)}), app(f))
}"#,
    prints("[null, i64:2, i64:4]")
);

run!(
    written_fn_type_typedef_variable_before_a_wider_position_widens,
    r#"{
  type T<'a> = [`Leaf('a), `Node(T<i64>)];
  let fr = |t: T<'y>, k: 'y| -> 'y k;
  let gr: fn(t: T<i64>, k: [i64, null]) -> [i64, null] = fr;
  gr(`Leaf(1), 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A variable the typedef passes on to its own application keeps the
// first position: the walk expands no definition inside its own
// expansion, where a function type may hold the variable.
run!(
    written_fn_type_recursive_typedef_variable_first_position,
    r#"{
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  let f = |k: 'y, l: L<'y>| -> 'y k;
  let g: fn(k: [i64, null], l: L<i64>) -> [i64, null] = f;
  g(null, `Cons(1, `Nil))
}"#,
    prints("null")
);

run!(
    written_fn_type_recursive_typedef_variable_first_position_widens,
    r#"{
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  let f = |l: L<'y>, k: 'y| -> 'y k;
  let g: fn(l: L<i64>, k: [i64, null]) -> [i64, null] = f;
  g(`Cons(1, `Nil), null)
}"#,
    prints("null");
    FuseExpect::Jit
);

// A rigid variable going into a union whose member is a variable is held
// by the member, or by its bound's parts, as the positions before it left
// the member: the position that decides the member is written first.
run!(
    written_fn_type_union_beside_a_declared_variable,
    r#"{
  let g = 'b: [i64, null] |x: 'b| {
    let h: fn(s: i64, v: 'b) -> i64 = |s: 'a, v: ['a, null]| 1;
    let hw: fn(v: 'b, w: 'b) -> i64 = |v: ['d, null], w: 'd| 3;
    h(1, x) + hw(x, x)
  };
  [g(2), g(null)]
}"#,
    prints("[i64:4, i64:4]")
);

run!(
    written_fn_type_union_before_its_members_position_fits,
    r#"{
  let g = 'b: [i64, null] |x: 'b| {
    let hr: fn(v: 'b, s: i64) -> i64 = |v: ['c, null], s: 'c| 2;
    hr(x, 1)
  };
  [g(2), g(null)]
}"#,
    prints("[i64:2, i64:2]");
    FuseExpect::Jit
);

run!(
    written_fn_type_union_beside_a_declared_variable_fits,
    r#"{
  let g = 'b: [i64, null] |x: 'b| {
    let h: fn(s: string, v: 'b) -> i64 = |s: 'a, v: ['a, null]| 0;
    h("s", x)
  };
  g(2)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// A lambda an instance builds takes its parameters from the row in order:
// a variable under a typedef's union is decided by the positions before
// it.
run!(
    instance_lambda_typedef_union_parameters,
    r#"{
  type O<'x> = ['x, null];
  let g = 'b: [i64, null] |x: 'b| {
    let h: fn(v: 'b, w: 'b) -> i64 = |v: O<'a>, w: 'a| 1;
    let k: fn(s: i64, v: 'b) -> i64 = |s: 'c, v: Option<'c>| 2;
    h(x, x) + k(1, x)
  };
  [g(2), g(null)]
}"#,
    prints("[i64:3, i64:3]")
);

// A builtin binding declares every argument's type, its variadic's too.
run!(builtin_binding_needs_a_typed_variadic, r#"{
  let s = |@args| -> i64 'core_sum;
  s(1, 2)
}"#, refused("builtin function core_sum requires all arguments to have type annotations"); FuseExpect::None);

// A returned function's type is parenthesized where its function's
// throws follows, an inferred one (a variable bound to it) included.
run!(returned_function_type_parenthesized_before_the_throws, r#"{
  let f = |x: i64| { let g = |y: i64| y + 1; let e = select x { 0 => error(`E)?, n => n }; g };
  let h: i64 = f;
  h
}"#, refused("i64 does not contain fn(x: i64) -> fn(y: i64) -> i64"); FuseExpect::None);

// A labeled default's block names the types it declares, at the
// definition's check and at each call that omits it.
run!(default_names_a_type_its_block_declares, r#"{
  let f = |#d = { type M = [i64, null]; let q: M = 3; q }, x: i64| (d, x);
  (f(1), f(#d: null, 2))
}"#, prints("[[i64:3, i64:1], [null, i64:2]]"); FuseExpect::Jit);

run!(default_compares_a_type_its_block_declares, r#"{
  let f = |#d = {
    type M = [i64, null];
    type N = Abstract<[`C(M), `Nil]>;
    N(`C(3)) == N(`C(3))
  }, x: i64| (d, x);
  let fs = [f, f];
  let g = fs[1]$;
  (array::map([1, 2], |x| f(x)), g(5), f(#d: false, 2))
}"#, prints("[[[true, i64:1], [true, i64:2]], [true, i64:5], [false, i64:2]]"); FuseExpect::Jit);

run!(same_function_write_fires_without_reset, r#"{
    let step = 0;
    step <- select step { s if s < 7 => s + 1, _ => never() };
    let f = |x: i64| count(x);
    let function = f;
    function <- select step { 3 => f, _ => never() };
    let argument = 5;
    argument <- select step { 1 => 6, _ => never() };
    let result = function(argument);
    let seen = [];
    seen <- result ~ array::push(seen, result);
    select step { 7 => seen, _ => never() }
}"#, prints("[i64:1, i64:2, i64:2]"); FuseExpect::Jit);

run!(readonly_reference_variable_widens, r#"{
    let r: &u64 = &u64:0;
    let n: &[u64, null] = &null;
    let f = |x: &'a, y: &'a| -> &'a y;
    *f(r, n)
}"#, is(Value::Null); FuseExpect::None);

run!(readonly_reference_variable_widens_reversed, r#"{
    let r: &u64 = &u64:0;
    let n: &[u64, null] = &null;
    let f = |x: &'a, y: &'a| -> &'a y;
    *f(n, r)
}"#, is(Value::U64(0)); FuseExpect::None);

run!(writable_reference_variable_does_not_widen_reversed, r#"{
    let r: &mut u64 = &mut u64:0;
    let n: &mut [u64, null] = &mut null;
    let f = |x: &mut 'a, y: &mut 'a| -> &mut 'a y;
    f(n, r)
}"#, refused("does not contain"); FuseExpect::None);

pub(super) const CLOSURE_OUTLIVES_SLOT: &str = r#"{
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let gs = array::map(src, |k| |x: i64| -> i64 x * k);
        let saved: fn(x: i64) -> i64 = never();
        saved <- gs[0]$;
        select n { k if k >= 4 => saved(k), _ => never() }
    }"#;

run!(closure_outlives_collection_slot, CLOSURE_OUTLIVES_SLOT, is(Value::I64(40)));

run!(
    closure_outlives_collection_slot_transitively,
    r#"{
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let gs = array::map(src, |k| |x: i64| |y: i64| -> i64 k * x + y);
        let saved: fn(x: i64) -> fn(y: i64) -> i64 = never();
        saved <- gs[0]$;
        select n { k if k >= 4 => saved(k)(1), _ => never() }
    }"#,
    is(Value::I64(41))
);

run!(
    closure_outlives_collection_slot_default,
    r#"{
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let gs = array::map(src, |k: i64| |#factor: i64 = k, x: i64| -> i64 x * factor);
        let saved: fn(x: i64) -> i64 = never();
        saved <- gs[0]$;
        select n { k if k >= 4 => saved(k), _ => never() }
    }"#,
    is(Value::I64(40))
);

// A lambda literal's own quantifier is its callers' to pick: the check of
// the definition around it settles neither it nor its constructor element.
run!(
    an_argument_lambdas_quantifier_stays_open,
    r#"{ let f = |x: 'z| 0; let g = || f('c: Collection |c: 'c| 0); g() }"#,
    is(Value::I64(0))
);

// A `let rec` self-call that omits a default checks it against the shared
// signature, which a monomorphic recursion narrows.
run!(
    a_self_calls_omitted_default_is_checked,
    r#"{ let rec g = |#s = 0, x: i64| select x { 0 => 0, _ => g(x - 1) }; g(#s: "top", 1) }"#,
    prints("i64:0")
);

run!(
    a_self_calls_omitted_default_takes_the_shared_type,
    r#"{
        let rec g = |#s = 0, x: i64| select x { 0 => s, x => g(x - 1) };
        (g(#s: 5, 1), g(1))
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(0), Value::I64(0)])
);

// A select may return two function types as one value: an instance's
// signature holds `fn(i: 'c) -> 'c` by its own member, never by the
// `fn(path: string) -> ..` written before it.
run!(
    a_returned_union_of_two_function_types_instantiates,
    r#"{
        let f = |x| {
            let s = |n, acc| select n { 0 => str::row_col, _ => |i| i };
            s(-1, 20)
        };
        let v = f("foo");
        v ~ 3
    }"#,
    is(Value::I64(3));
    FuseExpect::None
);

run!(
    a_returned_union_of_two_function_types_typed_member_first,
    r#"{
        let f = |x| select x { 0 => |p: string| -> i64 0, _ => |i| i };
        f(1) ~ 3
    }"#,
    is(Value::I64(3));
    FuseExpect::None
);

// Two function types that do not meet stay two members of the select's
// type, and the attempt decides nothing of either: `h` (monomorphic, it is
// written) still takes an i64.
run!(
    a_failed_meet_of_function_arms_binds_nothing,
    r#"{
        let h = |i| i;
        h <- |i| i;
        let u = select 0 { 0 => |p: string| -> i64 0, _ => h };
        h(1)
    }"#,
    is(Value::I64(1));
    FuseExpect::None
);

// A use decides the cell beside the tuple, so the element is one type at the
// instance, and the annotated pattern covers it.
const A_PATTERN_OVER_A_UNION_OF_ONE_TYPE: &str = r#"{
    let e = never();
    let z = [e, ("a", 1)];
    let m = array::map(z, |(k, v): (string, i64)| v);
    e <- ("b", 2);
    m
}"#;

// `s + a` makes `a` and `b` one cell, whose instance the array's end settles to
// ⊥, and the definition's return is still the union of its two names.
const A_RETURN_OVER_A_UNION_OF_ONE_TYPE: &str = r#"{
    let k = |y| |a, b| { let s = select true { true => a, false => b }; s + a };
    let z = [1, k(2)];
    array::len(z)
}"#;

run!(
    a_return_over_a_union_of_one_type,
    A_RETURN_OVER_A_UNION_OF_ONE_TYPE,
    prints("i64:2");
    FuseExpect::None
);

run!(
    a_pattern_over_a_union_of_one_type,
    A_PATTERN_OVER_A_UNION_OF_ONE_TYPE,
    prints("[i64:2, i64:1]");
    FuseExpect::Jit
);

// A default omitted at a call is judged against the site's view of its label,
// each cell of the default's own copy at its place in that view, so a union of
// two of the function's variables holds the default's argument as it does the
// site's.
const A_DEFAULT_OVER_A_TYPEDEF_UNION: &str = r#"{
    type P<'p, 'q> = [Array<'p>, Array<'q>];
    let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a| k;
    let r = f(1.5);
    r([1.5])
}"#;

const A_DEFAULT_OVER_A_TYPEDEF_UNION_OF_TWO_DECIDED: &str = r#"{
    type P<'p, 'q> = [Array<'p>, Array<'q>];
    let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k;
    let r = f(1.5, "s");
    r([1.5])
}"#;

const A_DEFAULT_OVER_A_TYPEDEF_UNION_THROUGH_A_WRAPPER: &str = r#"{
    type P<'p, 'q> = [Array<'p>, Array<'q>];
    let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k;
    let g = |t| f(t, "s");
    let r = g(1.5);
    r([1.5])
}"#;

const A_DEFAULT_OVER_A_TYPEDEF_UNION_WRAPPED_UNDECIDED: &str = r#"{
    type P<'p, 'q> = [Array<'p>, Array<'q>];
    let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a| k;
    let g = |t| f(t);
    let r = g(1.5);
    r([1.5])
}"#;

run!(
    a_default_over_a_typedef_union,
    A_DEFAULT_OVER_A_TYPEDEF_UNION,
    is(Value::I64(7));
    FuseExpect::None
);

run!(
    a_default_over_a_typedef_union_of_two_decided,
    A_DEFAULT_OVER_A_TYPEDEF_UNION_OF_TWO_DECIDED,
    is(Value::I64(7));
    FuseExpect::None
);

run!(
    a_default_over_a_typedef_union_through_a_wrapper,
    A_DEFAULT_OVER_A_TYPEDEF_UNION_THROUGH_A_WRAPPER,
    is(Value::I64(7));
    FuseExpect::None
);

run!(
    a_default_over_a_typedef_union_wrapped_undecided,
    A_DEFAULT_OVER_A_TYPEDEF_UNION_WRAPPED_UNDECIDED,
    is(Value::I64(7));
    FuseExpect::None
);

// A default that is a call's result holding a function with an open cell
// leaves the cell to the omitting call's bind, which places it at the site's
// view of the label.
const A_DEFAULT_CALL_RESULT_WITH_AN_OPEN_CELL: &str = r#"{
    let mk = |k: i64| |d, x: i64| cast<string>(d)$;
    let f = |#w: fn(d: 'z, x: i64) -> string = mk(0), y: 'z, x: i64| w(y, x);
    f(1, 7)
}"#;

const A_DEFAULT_CALL_RESULT_WITH_AN_OPEN_CELL_AT_TWO_TYPES: &str = r#"{
    let mk = |k: i64| |d, x: i64| cast<string>(d)$;
    let f = |#w: fn(d: 'z, x: i64) -> string = mk(0), y: 'z, x: i64| w(y, x);
    "[f(1, 7)][f("a", 7)]"
}"#;

const A_DEFAULT_CALL_RESULT_WITH_A_DEFAULTED_FUNCTION: &str = r#"{
    let mk = |k: i64| |#d = 1, x: i64| cast<string>(d)$;
    let f = |#w: fn(?#d: 'z, x: i64) -> string = mk(0), x: i64| w(x);
    f(7)
}"#;

const A_DEFAULT_CALL_RESULT_THAT_FIXES_THE_VARIABLE: &str = r#"{
    let mk = |k: i64| |d: i64, x: i64| cast<string>(d)$;
    let f = |#w: fn(d: 'z, x: i64) -> string = mk(0), y: 'z, x: i64| w(y, x);
    f("a", 7)
}"#;

run!(
    a_default_call_result_with_an_open_cell,
    A_DEFAULT_CALL_RESULT_WITH_AN_OPEN_CELL,
    is(Value::from("1"));
    FuseExpect::None
);

run!(
    a_default_call_result_with_an_open_cell_at_two_types,
    A_DEFAULT_CALL_RESULT_WITH_AN_OPEN_CELL_AT_TWO_TYPES,
    is(Value::from("1a"));
    FuseExpect::None
);

run!(
    a_default_call_result_with_a_defaulted_function,
    A_DEFAULT_CALL_RESULT_WITH_A_DEFAULTED_FUNCTION,
    is(Value::from("1"));
    FuseExpect::None
);

run!(
    a_default_call_result_that_fixes_the_variable,
    A_DEFAULT_CALL_RESULT_THAT_FIXES_THE_VARIABLE,
    refused("does not contain");
    FuseExpect::None
);

// A default that raises is called through the function type its definition
// is held to, so that type declares what it raises.
const A_RAISING_DEFAULT_BEHIND_A_TYPEDEF: &str = r#"{
    type W = fn(?#d: i64, x: i64) -> i64;
    let mk = |k: i64| -> W |#d = error(`E)?, x: i64| d + x;
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        mk(0)(7)
    };
    r$
}"#;

const A_RAISING_DEFAULT_BEHIND_A_TYPEDEF_THAT_THROWS: &str = r#"{
    type W = fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<`E>>;
    let mk = |k: i64| -> W |#d = error(`E)?, x: i64| d + x;
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        mk(0)(7)
    };
    r$
}"#;

const A_RAISING_DEFAULT_BEHIND_A_HIDING_RETURN_TYPE: &str = r#"{
    let mk = |k: i64| -> fn(x: i64) -> i64 |#d = error(`E)?, x: i64| d + x;
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        mk(0)(7)
    };
    r$
}"#;

const A_RAISING_DEFAULT_BEHIND_A_HIDING_RETURN_TYPE_THAT_THROWS: &str = r#"{
    let mk = |k: i64| -> fn(x: i64) -> i64 throws Error<ErrChain<`E>> |#d = error(`E)?, x: i64| d + x;
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        mk(0)(7)
    };
    r$
}"#;

const A_RAISING_DEFAULT_NAMED_BY_ITS_CALL: &str = r#"{
    let mk = |k: i64| |#d = error(`E)?, x: i64| d + x;
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        mk(0)(7)
    };
    r$
}"#;

const A_RAISING_DEFAULT_PASSED_TO_AN_OPEN_THROWS: &str = r#"{
    let f = |#d = error(`E)?, x: i64| d + x;
    let app = |g: fn(x: i64) -> i64 throws 'e, y: i64| g(y);
    let r: [i64, null] = null;
    {
        catch(e) r <- e ~ 42;
        app(f, 3)
    };
    r$
}"#;

run!(
    a_raising_default_behind_a_typedef,
    A_RAISING_DEFAULT_BEHIND_A_TYPEDEF,
    refused_renumbered("type mismatch W does not contain fn(?#d: '_0, x: i64) -> '_1");
    FuseExpect::None
);

run!(
    a_raising_default_behind_a_typedef_that_throws,
    A_RAISING_DEFAULT_BEHIND_A_TYPEDEF_THAT_THROWS,
    is(Value::I64(42));
    FuseExpect::Jit
);

run!(
    a_raising_default_behind_a_hiding_return_type,
    A_RAISING_DEFAULT_BEHIND_A_HIDING_RETURN_TYPE,
    refused("type mismatch _ does not contain Error<ErrChain<`E>>");
    FuseExpect::None
);

run!(
    a_raising_default_behind_a_hiding_return_type_that_throws,
    A_RAISING_DEFAULT_BEHIND_A_HIDING_RETURN_TYPE_THAT_THROWS,
    is(Value::I64(42));
    FuseExpect::Jit
);

run!(
    a_raising_default_named_by_its_call,
    A_RAISING_DEFAULT_NAMED_BY_ITS_CALL,
    is(Value::I64(42));
    FuseExpect::Jit
);

run!(
    a_raising_default_passed_to_an_open_throws,
    A_RAISING_DEFAULT_PASSED_TO_AN_OPEN_THROWS,
    is(Value::I64(42));
    FuseExpect::Jit
);

// A default that raises through a variable its factory declares: the
// returned function's `throws` is what names the instance.
const A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE: &str = r#"{
    let mk = |k: Result<i64, 'e>| |#d = k?, x: i64| d + x;
    let r: [string, null] = null;
    {
        catch(e) r <- e ~ "[e.0.error]";
        mk(error(`E))(7)
    };
    r$
}"#;

const A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_THAT_THROWS: &str = r#"{
    let mk = |k: Result<i64, 'e>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<'e>>
        |#d = k?, x: i64| d + x;
    let r: [string, null] = null;
    {
        catch(e) r <- e ~ "[e.0.error]";
        mk(error(`E))(7)
    };
    r$
}"#;

const A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_CLOSED: &str = r#"{
    let mk = |k: Result<i64, `E>| |#d = k?, x: i64| d + x;
    let r: [string, null] = null;
    {
        catch(e) r <- e ~ "[e.0.error]";
        mk(error(`E))(7)
    };
    r$
}"#;

const A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_USED_INSIDE: &str = r#"{
    let mk = |k: Result<i64, 'e>| { let g = |#d = k?, x: i64| d + x; g(1) };
    let r: [string, null] = null;
    {
        catch(e) r <- e ~ "[e.0.error]";
        mk(error(`E))
    };
    r$
}"#;

const A_DEFAULT_RAISING_THROUGH_TWO_OF_THE_FACTORYS_VARIABLES: &str = r#"{
    let mk = |k: Result<i64, 'e>, j: Result<i64, 'f>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<'e>>
        |#d = k? + j?, x: i64| d + x;
    { catch(e) 0; mk(error(`E), 1)(7) }
}"#;

const A_DEFAULT_RAISING_THROUGH_ONE_OF_THE_FACTORYS_VARIABLES_AND_A_CLOSED_ONE: &str = r#"{
    let mk = |k: Result<i64, 'e>, j: Result<i64, `F>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<['e, `F]>>
        |#d = k? + j?, x: i64| d + x;
    let r: [string, null] = null;
    {
        catch(e) r <- e ~ "[e.0.error]";
        mk(error(`E), 1)(7)
    };
    r$
}"#;

const A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_THAT_THROWS_ANOTHER_TYPE: &str = r#"{
    let mk = |k: Result<i64, 'e>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<`Z>>
        |#d = k?, x: i64| d + x;
    mk
}"#;

run!(
    a_default_raising_through_the_factorys_variable_infers_the_throws,
    A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE,
    prints("\"`E\"");
    FuseExpect::Jit
);

// a_default_raising_through_the_factorys_variable_infers_the_throws (was
// declared for another error.
run!(
    a_default_raising_through_the_factorys_variable_to_a_catch_of_another_type_refused,
    r#"{
    let mk = |k: Result<i64, 'e>| |#d = k?, x: i64| d + x;
    let r: [string, null] = null;
    {
        catch(e: Error<ErrChain<`Z>>) r <- e ~ "[e.0.error]";
        mk(error(`E))(7)
    };
    r$
}"#,
    refused("type mismatch `Z does not contain `E");
    FuseExpect::None
);

run!(
    a_default_raising_through_the_factorys_variable_that_throws,
    A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_THAT_THROWS,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "`E");
    FuseExpect::Jit
);

run!(
    a_default_raising_through_the_factorys_variable_closed,
    A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_CLOSED,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "`E");
    FuseExpect::Jit
);

run!(
    a_default_raising_through_the_factorys_variable_used_inside,
    A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_USED_INSIDE,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "`E");
    FuseExpect::Jit
);

run!(
    a_default_raising_through_two_of_the_factorys_variables,
    A_DEFAULT_RAISING_THROUGH_TWO_OF_THE_FACTORYS_VARIABLES,
    refused("type mismatch 'e: unbound does not contain 'f: unbound");
    FuseExpect::None
);

run!(
    a_default_raising_through_one_of_the_factorys_variables_and_a_closed_one,
    A_DEFAULT_RAISING_THROUGH_ONE_OF_THE_FACTORYS_VARIABLES_AND_A_CLOSED_ONE,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "`E");
    FuseExpect::Jit
);

run!(
    a_default_raising_through_the_factorys_variable_declared_another_type,
    A_DEFAULT_RAISING_THROUGH_THE_FACTORYS_VARIABLE_THAT_THROWS_ANOTHER_TYPE,
    refused("type mismatch `Z does not contain 'e");
    FuseExpect::None
);

// A comparison is judged where the check decides its operands' types: a
// binding something writes is one function, here `y: [i64, string]`, and
// a value held in data is its copy's type. A call's instance, narrower,
// is not judged again.
run!(written_binding_with_a_declared_variable_is_one_function, r#"
{
    let t = 'a: Number |x: 'a, y| -> bool x == y;
    let r = &mut t;
    [t(1, 2), t(1, "s"), t(1, 1)]
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
    if a[..] == [Value::Bool(false), Value::Bool(false), Value::Bool(true)]);
    FuseExpect::None);

run!(written_binding_called_through_data_is_one_function, r#"
{
    let t = 'a: Number |x: 'a, y| -> bool x == y;
    let r = &mut t;
    let fs = [t];
    let g = fs[0]$;
    [g(1, 2), g(1, "s"), g(1, 1)]
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
    if a[..] == [Value::Bool(false), Value::Bool(false), Value::Bool(true)]);
    FuseExpect::None);

run!(written_binding_called_at_one_type_that_cannot_compare, r#"
{
    let t = 'a: Number |x: 'a, y| -> bool x == y;
    let r = &mut t;
    t(1, "s")
}
"#, refused("cannot compare i64 with string"); FuseExpect::None);

run!(generic_function_called_through_data_at_two_types, r#"
{
    let t = |x, y| x == y;
    let fs = [t];
    let g = fs[0]$;
    [g(1, 2), g(1, "s"), g(1, 1)]
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
    if a[..] == [Value::Bool(false), Value::Bool(false), Value::Bool(true)]);
    FuseExpect::None);

run!(generic_function_with_a_declared_variable_called_through_data_at_two_types, r#"
{
    let t = 'a: Number |x: 'a, y| -> bool x == y;
    let fs = [t];
    let g = fs[0]$;
    [g(1, 2), g(1, "s"), g(1, 1)]
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
    if a[..] == [Value::Bool(false), Value::Bool(false), Value::Bool(true)]);
    FuseExpect::None);

run!(generic_function_called_through_data_at_one_type_that_cannot_compare, r#"
{
    let t = |x, y| x == y;
    let fs = [t];
    let g = fs[0]$;
    g(1, "s")
}
"#, refused("cannot compare i64 with string"); FuseExpect::None);

// refuses ("a run-time bind at fs[0]$(7) did not type: type mismatch
// string does not contain i64"). Add it with the hole's fix.
run!(array_defaults_of_two_types_reach_the_result_refused, r#"{
    let fs = [
        |#d = 1, x: i64| d,
        |#d = "q", x: i64| d
    ];
    let r: string = fs[0]$(7);
    r
}"#, refused("string does not contain i64"); FuseExpect::None);
