// Tests for lambdas, first-class functions, labeled arguments, recursive functions

use super::dense_deltas::{run_delta, run_delta_fused};
use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, as_i64s, eval, is, prints, refused},
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

// A formal with its own quantifier is rank-2: each call of it copies 'a,
// so the body may apply it at i64.
const RANK2_FORMAL_APPLIED_AT_A_NUMBER: &str = r#"
{
  let g = |f: fn<'a: Number>(x: 'a) -> 'a, y| f(y) + 1;
  g(|x| x, 1)
}
"#;

run!(rank2_formal_applied_at_a_number, RANK2_FORMAL_APPLIED_AT_A_NUMBER, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(2)))
});

// A rank-2 formal's quantifier is held in the body, whatever the order
// of the parameters or a declared variable of the same name: a use at
// i64 takes an instance of the formal, so a callback must still be
// polymorphic.
run!(
    rank2_formal_used_at_a_type_stays_polymorphic,
    r#"{
  let k = |y: 'b, g: fn<'b: Number>(x: 'b) -> 'b| { let h: fn(x: i64) -> i64 = g; h(1) + g(2) };
  let j = |g: fn<'b: Number>(x: 'b) -> 'b, y: 'b| { let h: fn(x: i64) -> i64 = g; (h(1), g(f64:2.5), y) };
  "[k("s", |x| x)] [j(|x| x, "s")]"
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "3 (1, 2.5, \"s\")")
);

run!(rank2_formal_used_at_a_type_refuses_a_monomorphic_callback, r#"{
  let k = |y: 'b, g: fn<'b: Number>(x: 'b) -> 'b| { let h: fn(x: i64) -> i64 = g; h(1) };
  k("s", |x: i64| x + 1)
}"#, refused("does not contain fn(x: i64) -> i64"); FuseExpect::None);

run!(rank2_formal_listed_first_refuses_a_monomorphic_callback, r#"{
  let k = |g: fn<'b: Number>(x: 'b) -> 'b, y: 'b| { let h: fn(x: i64) -> i64 = g; h(1) };
  k(|x: i64| x + 1, "s")
}"#, refused("does not contain fn(x: i64) -> i64"); FuseExpect::None);

// A lambda returned through a rank-2 formal stays generic at run time:
// its first bind, at i64, decides nothing for a later one at string.
run!(
    rank2_returned_lambda_binds_at_each_type,
    r#"{
  let app = |f: fn<'b: Any>(x: 'b) -> 'b| f;
  let g = app(|x| x);
  let a = g(1);
  let b = g("s");
  "[a] [b]"
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s")
);

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
  let add = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y;
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

run!(tail_depth_catches_up_an_outer_write, TAIL_DEPTH_CATCHES_UP_AN_OUTER_WRITE, is(Value::Bool(true)), timeout: 5; FuseExpect::Jit);

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

run!(lambdamatch1, LAMBDAMATCH1, refused("non exhaustive struct matches require type annotations"); FuseExpect::None);

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

run!(lambdamatch3, LAMBDAMATCH3, refused("does not contain { bar: i64, baz: i64 }"); FuseExpect::None);

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

run!(lambdamatch5, LAMBDAMATCH5, refused("does not contain string"); FuseExpect::None);

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

/// `code`'s `raised` over a source of four slots: one raise per new
/// slot, none from a slot already there.
async fn raises_once_per_slot(code: &str, mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(code, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), [0, 1, 2, 3, 4]);
    Ok(())
}

// Each slot of a growing map calls `g` as an instance of its own, whose
// first call is an init view: its constant error raises once per new
// slot, four in all.
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

async fn hof_slot_callee_first_call(mode: Mode) -> Result<()> {
    raises_once_per_slot(HOF_SLOT_CALLEE_FIRST_CALL, mode).await
}

modes!(hof_slot_callee_first_call);

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
  raised
}
"#;

async fn hof_slot_nested_prev_len(mode: Mode) -> Result<()> {
    raises_once_per_slot(HOF_SLOT_NESTED_PREV_LEN, mode).await
}

modes!(hof_slot_nested_prev_len);

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
  raised
}
"#;

async fn hof_slot_constant_fires(mode: Mode) -> Result<()> {
    raises_once_per_slot(HOF_SLOT_CONSTANT_FIRES, mode).await
}

modes!(hof_slot_constant_fires);

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

run!(rtype_rejects_error_arm, RTYPE_REJECTS_ERROR_ARM, |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("i64 does not contain [")); FuseExpect::None);

// A fn-valued element does not slip through a recursive-type HOF chain:
// `acc + <fn>` is rejected.
run!(
    list_map_fn_element_fold_rejected,
    refused("both operands must be one numeric type"),
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
    refused("both operands must be one numeric type");
    FuseExpect::None
);

const OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT: &str = r#"
array::fold(array::map([f64:23.5, i64:2, i64:3], |x| x * i64:2), i64:0, |res, v| res / v)
"#;

run!(
    operand_refuses_mixed_numeric_element,
    OPERAND_REFUSES_MIXED_NUMERIC_ELEMENT,
    refused("both operands must be one numeric type");
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

run!(rec_rtype_rejects_error_arm, REC_RTYPE_REJECTS_ERROR_ARM, |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("i64 does not contain [")); FuseExpect::None);

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

run!(rec_selfcall_arg_mismatch, REC_SELFCALL_ARG_MISMATCH, refused("i64 does not contain string"); FuseExpect::None);

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

// A pure recursion's depth swinging 10, 5, 10, .. for 250 cycles: the
// depths left asleep are reborn when reached again, and every cycle
// answers its own sum.
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

// A depth reborn after a shrink reads its capture as it stands: the
// capture moves while depths 2 and 3 sleep, and their rebirth sees it.
async fn rec_reborn_depth_reads_its_capture(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = array::iter([3, 1, 1, 3]);
        let base = 100;
        base <- select t {1 => 200, _ => never()};
        let rec f = |n: i64| -> i64 select n { 0 => base, _ => n + f(n - 1) };
        f(t)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [106, 101, 201, 206]);
    Ok(())
}

modes!(rec_reborn_depth_reads_its_capture);

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
/// not run, left: its values, the activations live once it settled, and
/// the run-time binds and rebuild batches its cycles made.
async fn run_rec(
    def: &str,
    call: &str,
    ts: &[i64],
    mode: Mode,
) -> Result<(Vec<i64>, u64, u64)> {
    use graphix_package_core::testing::{
        compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    use std::{fmt::Write, time::Duration};
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
    let rebuilds = control.rebuilds();
    let res = compile_result(&ctx).await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(20);
    let quiet = Duration::from_millis(700);
    let values =
        updates_until_quiet(&ctx.rt, &mut rx, res.exprs[0].id, quiet, deadline).await?;
    let counts = (control.live_activations(), control.rebuilds() - rebuilds);
    ctx.shutdown().await;
    Ok((as_i64s(&values), counts.0, counts.1))
}

/// A pure recursion swinging between two depths every cycle binds about
/// what one held at the deeper depth does: the depths a shrink leaves are
/// kept asleep and reborn when reached again, never bound afresh. Deleting
/// them at each shrink binds the linear swing's five depths and fib's 72
/// activations every other cycle.
async fn rec_pure_depth_swing_binds_once(mode: Mode) -> Result<()> {
    for (def, eval, deep, shallow) in
        [(LINEAR, linear as fn(i64) -> i64, 10, 5), (FIB, fib, 10, 9)]
    {
        let swing: Vec<i64> = (0..250).map(|i| [deep, shallow][i % 2]).collect();
        let (values, _, swing_binds) = run_rec(def, DIRECT, &swing, mode).await?;
        assert_eq!(values, swing.iter().map(|n| eval(*n)).collect::<Vec<_>>());
        let (_, _, fixed_binds) = run_rec(def, DIRECT, &[deep; 250], mode).await?;
        assert!(
            swing_binds <= fixed_binds + 5,
            "{def}: {swing_binds} binds swinging against {fixed_binds} held deep"
        );
    }
    Ok(())
}

/// The activations kept after `exits` exits under a site that kept a fib(m)
/// tree, `levels(m)` deep: `levels * exits / 100` of its deepest levels are
/// shed, all of them at the hundredth.
fn fib_kept(m: i64, exits: u64) -> u64 {
    fn levels(m: i64) -> u64 {
        if m < 2 { 1 } else { 1 + levels(m - 1) }
    }
    fn cells(m: i64, depth: u64) -> u64 {
        match depth {
            0 => 0,
            _ if m < 2 => 1,
            _ => 1 + cells(m - 1, depth - 1) + cells(m - 2, depth - 1),
        }
    }
    let levels = levels(m);
    cells(m, levels - levels * exits.min(100) / 100)
}

/// A transient deep recursion is shed after it: each exit through a
/// shallow depth deletes the deepest 1% of the levels a site kept, all of
/// them after 100 exits; a cycle the recursion does not run is no exit. Linear 200 then 5 keeps 195 levels at one site;
/// fib(12) then fib(4) keeps two subtrees at each of fib(4)'s leaves,
/// fib(8) and fib(7) under a leaf of 1, fib(7) and fib(6) under one of 0.
async fn rec_transient_depth_is_shed(mode: Mode) -> Result<()> {
    let linear_kept = |exits: u64| 195 - 195 * exits.min(100) / 100;
    let fib_leaves = |exits: u64| {
        3 * (fib_kept(8, exits) + fib_kept(7, exits))
            + 2 * (fib_kept(7, exits) + fib_kept(6, exits))
    };
    let cases: [(&str, i64, i64, &dyn Fn(u64) -> u64, fn(i64) -> i64); 2] =
        [(LINEAR, 200, 5, &linear_kept, linear), (FIB, 12, 4, &fib_leaves, fib)];
    for (def, deep, shallow, kept, eval) in cases {
        for exits in [1, 50, 99, 100, 150] {
            let mut ts = vec![deep];
            ts.extend(std::iter::repeat_n([shallow, 0], exits as usize + 1).flatten());
            let (values, live, _) = run_rec(def, DIRECT, &ts, mode).await?;
            let runs = ts.iter().filter(|n| **n > 0);
            assert_eq!(values, runs.map(|n| eval(*n)).collect::<Vec<_>>());
            let (_, base, _) =
                run_rec(def, DIRECT, &vec![shallow; exits as usize + 2], mode).await?;
            assert_eq!(live, base + kept(exits), "{def}: {exits} exits");
        }
    }
    Ok(())
}

/// A depth partly shed is reached again: what is left is reborn, what was
/// shed is bound afresh, and every cycle answers its own value. A deeper
/// edge's own kept levels are shed with its caller's, once its caller
/// keeps them.
async fn rec_partly_shed_depth_reached_again(mode: Mode) -> Result<()> {
    let cases: [(&str, [i64; 4], fn(i64) -> i64); 2] =
        [(LINEAR, [200, 5, 150, 3], linear), (FIB, [12, 4, 11, 3], fib)];
    for (def, [deep, shallow, again, last], eval) in cases {
        let mut ts = vec![deep];
        ts.extend(std::iter::repeat_n(shallow, 51));
        ts.extend([deep, again]);
        ts.extend(std::iter::repeat_n(last, 101));
        let (values, live, _) = run_rec(def, DIRECT, &ts, mode).await?;
        assert_eq!(values, ts.iter().map(|n| eval(*n)).collect::<Vec<_>>());
        let (_, base, _) = run_rec(def, DIRECT, &vec![last; ts.len()], mode).await?;
        assert_eq!(live, base, "{def}");
    }
    Ok(())
}

/// A kept chain a later run reaches only part of the way is shed below
/// what it reaches: the activation reborn short of the chain's end leaves
/// the arm that holds the rest as a shrink does, and each exit after sheds
/// it. Linear 200 then 5, 6, 5 and 6 from then on, 200 then 5 and 6
/// swinging, and 200 then 5 called from a seq step, each of whose runs
/// rebirths the recursion, keep 194, 194 and 195 levels at one site, shed
/// 1% an exit; fib(12) then the same shapes over 4 and 5 is shed whole
/// after 150 exits.
async fn rec_partly_reached_chain_is_shed(mode: Mode) -> Result<()> {
    const IN_SEQ: &str = "seqq let n = t { f(n) }";
    let reborn_short = |d: i64, s: i64, exits: usize| {
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
        [(DIRECT, &reborn_short), (DIRECT, &swing), (IN_SEQ, &seq_runs)];
    for (call, shape) in shapes {
        for (def, eval, deep, shallow, all_exits) in [
            (LINEAR, linear as fn(i64) -> i64, 200, 5, &[1, 50, 150][..]),
            (FIB, fib, 12, 4, &[150]),
        ] {
            let last = *shape(deep, shallow, 1).last().unwrap();
            let (_, base, _) = run_rec(def, call, &[last; 4], mode).await?;
            let levels = (deep - last) as u64;
            for exits in all_exits {
                let ts = shape(deep, shallow, *exits);
                let (values, live, _) = run_rec(def, call, &ts, mode).await?;
                assert_eq!(values, ts.iter().map(|n| eval(*n)).collect::<Vec<_>>());
                let kept = levels - levels * (*exits as u64).min(100) / 100;
                assert_eq!(live, base + kept, "{def} by {call}: {ts:?}");
            }
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rec_pure_depth_swing_binds_once_interp() -> Result<()> {
    rec_pure_depth_swing_binds_once(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_pure_depth_swing_binds_once_par() -> Result<()> {
    rec_pure_depth_swing_binds_once(Mode::Par).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_shed_interp() -> Result<()> {
    rec_transient_depth_is_shed(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_shed_par() -> Result<()> {
    rec_transient_depth_is_shed(Mode::Par).await
}

/// An exit is the chain running, not a walk of its select.
#[tokio::test(flavor = "current_thread")]
async fn rec_transient_depth_is_shed_unpruned() -> Result<()> {
    rec_transient_depth_is_shed(Mode::Unpruned).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_shed_depth_reached_again_interp() -> Result<()> {
    rec_partly_shed_depth_reached_again(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_shed_depth_reached_again_par() -> Result<()> {
    rec_partly_shed_depth_reached_again(Mode::Par).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_reached_chain_is_shed_interp() -> Result<()> {
    rec_partly_reached_chain_is_shed(Mode::Interp).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_partly_reached_chain_is_shed_par() -> Result<()> {
    rec_partly_reached_chain_is_shed(Mode::Par).await
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
}; FuseExpect::Jit);

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

// A type variable only data arguments hold settles to the widest
// argument, whatever the order.
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

// Two arguments neither of which holds the other wait for a later
// argument that holds both.
const WIDEST_ARG_LAST: &str = r#"
{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a y;
  let ab: [`A, `B] = `A;
  f(`A, `B, ab)
}
"#;

run!(widest_arg_last, WIDEST_ARG_LAST, prints("\"B\""));

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
    refused(r#"Array<(i64, u8)> does not contain Array<"#);
    FuseExpect::None
);

// No argument holds the others: refused, in either order.
run!(
    no_widest_arg_refused,
    r#"{ let f = |x: 'a, y: 'a| -> 'a y; (f(`A, `B), f(`B, `A)) }"#,
    refused(r#"'a: `A does not contain `B"#);
    FuseExpect::None
);

// The call's type is the widest argument's.
run!(
    widest_arg_types_the_result,
    r#"{ let n: [u64, null] = null; let f = |x: 'a, y: 'a| -> 'a y; let r: u64 = f(u64:0, n); r }"#,
    refused(r#"type mismatch u64 does not contain [u64, null]"#);
    FuseExpect::None
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
    refused(r#"'a: u64 does not contain [u64, null]"#);
    FuseExpect::None
);

run!(
    writable_reference_variable_does_not_widen,
    r#"{
  let r: &mut u64 = &mut u64:0;
  let n: &mut [u64, null] = &mut null;
  let f = |x: &mut 'a, y: &mut 'a| -> &mut 'a y;
  f(r, n)
}"#,
    refused(r#"&mut 'a: u64 does not contain &mut [u64, null]"#);
    FuseExpect::None
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

run!(flat_map_bare_element_refused, FLAT_MAP_BARE_ELEMENT, refused(
    "fn(a: Array<i64>, f: fn(x: i64) -> [i64, Array<i64>]) -> Array<i64> does not contain"
); FuseExpect::None);

// A tuple callback result is refused.
const FLAT_MAP_TUPLE_REFUSED: &str = r#"
{
  let r = array::flat_map([1, 2], |x| (x, x * 10));
  r
}
"#;

run!(flat_map_tuple_refused, FLAT_MAP_TUPLE_REFUSED,
    refused("does not contain fn(x: i64) -> (i64, i64)"); FuseExpect::None);

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
    refused("does not contain fn(x: i64) -> Array<i64>"); FuseExpect::None);

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
  let apply = |f: fn<'b: Number>(x: 'b) -> 'b| ((f))(1);
  (m([1, 2], |x| x * 2), n(["a"], |s| "[s]!"), apply(|x| x))
}
"#;

run!(
    param_called_through_an_expression,
    PARAM_CALLED_THROUGH_AN_EXPRESSION,
    prints(r#"[[i64:2, i64:4], ["a!"], i64:1]"#);
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

run!(environment_cell_is_not_generalized, ENVIRONMENT_CELL_IS_NOT_GENERALIZED, is(Value::I64(42)); FuseExpect::Jit);

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
    refused("Array<i64> does not contain Array<'r"),
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
    refused("does not contain fn(s: string) -> bytes"),
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

// A labeled default is born with the binding and delivers FIRED at a
// fresh callee's first dispatch: an arm's first selection and a rebind
// each sample it. A stale default would give [0] and [10] on both
// engines alike.
async fn default_fires_at_first_dispatch(mode: Mode) -> Result<()> {
    let code = r#"{
        let d = 5;
        let f = |#k = d, x: i64| k ~ x;
        select array::iter([0, 1]) { 0 => 0, _ => f(10) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [0, 10]);
    let code = r#"{
        let t = array::iter([0, 1]);
        let d = 5;
        let g = |#k = d, x: i64| k ~ x;
        g <- select t { 1 => |#k = d, x: i64| k ~ (x + 1), _ => never() };
        g(10)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [10, 11]);
    Ok(())
}

modes!(default_fires_at_first_dispatch);

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

// A variable settles to the widest argument whatever the order, a
// formal holding two of them included.
const WIDEST_ARGUMENT_ANY_ORDER: &str = r#"
{
    let g = |a: ('x, 'y), b: ('x, 'y), c: ('x, 'y)| (a, b, c);
    let i: [i64, f64] = 1;
    g((1, 1), (i, 2.0), (3, i))
}
"#;

run!(widest_argument_any_order, WIDEST_ARGUMENT_ANY_ORDER, prints("[[i64:1, i64:1], [i64:1, f64:2.], [i64:3, i64:1]]"); FuseExpect::Jit);

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
    refused("does not contain string"); FuseExpect::None);

// The same with the callee's parameter open: its default does not decide
// it past a fn type that names the label.
run!(open_default_through_a_fn_type, r#"{
    let g = |#n = "hello", x: i64| -> i64 x + 1;
    let h = |f: fn(?#n: i64, x: i64) -> i64| f(1);
    h(g)
}"#, refused("does not contain string"); FuseExpect::None);

// A function value's default is judged where the value meets a fn type
// that hides the label, at the parameter the fn type fixes.
const DEFAULT_HIDDEN_BY_A_FN_TYPE: &str = r#"
{
    let g = |#x = 4096, y| x == y;
    let apply = |h: fn(y: string) -> bool| h("seven");
    apply(g)
}
"#;

run!(default_hidden_by_a_fn_type, DEFAULT_HIDDEN_BY_A_FN_TYPE, |v: Result<&Value>| {
    refused("string does not contain i64")(v)
}; FuseExpect::None);

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
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_field, r#"
{
    type T = {f: fn(y: string) -> bool};
    let t: T = {f: |#x = 4096, y| x == y};
    (t.f)("seven")
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_connect, r#"
{
    let f = |#x = 1, y| x == y;
    let g = |y: string| y == "s";
    let pick: fn(y: string) -> bool = g;
    pick <- f;
    pick("s")
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_with_update, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let s = {h: |x: f64| x};
    let s2 = {s with h: f};
    (s2.h)(1.5)
}
"#, refused("does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_reference_write, r#"
{
    let f = 'a: Number |#d: 'a = 5, x: 'a| x + d;
    let g = |x: f64| x;
    let r = &mut g;
    *r <- f;
    let h = *r;
    h(1.5)
}
"#, refused("does not contain i64"); FuseExpect::None);

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

run!(default_hidden_by_a_rank2_formal, r#"
{
    let f = 'a: Any |#x: 'a = 1, y: 'a| -> 'a select y { _ => x };
    let app = |g: fn<'b: Any>(y: 'b) -> 'b| (g(1), g("s"));
    app(f)
}
"#, refused("does not contain i64"); FuseExpect::None);

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
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_under_a_tuple_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [(fn(y: string) -> bool, i64), null] = (g, 1);
    select t { null => false, s => (s.0)("seven") }
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_under_an_element_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: Array<[{f: fn(y: string) -> bool}, null]> = [{f: g}];
    select t[0]$ { null => false, s => (s.f)("seven") }
}
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_under_a_variant_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [`A(fn(y: string) -> bool), `B(i64)] = `A(g);
    select t { `A(f) => f("seven"), `B(_) => false }
}
"#, refused("string does not contain i64"); FuseExpect::None);

// Only the member with the value's constructor judges it.
run!(default_fits_its_variant_member, r#"
{
    let g = |#x = 4096, y| x == y;
    let t: [`A(fn(y: i64) -> bool), `B(fn(y: string) -> bool)] = `A(g);
    select t { `A(f) => f(4096), `B(f) => f("s") }
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))); FuseExpect::None);

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
"#, refused("string does not contain i64"); FuseExpect::None);

run!(default_hidden_by_a_throws, r#"
{
    let f = |#e = `Small, x: i64| select x { 0 => error(e)?, x => x };
    let g: fn(x: i64) -> i64 throws Error<ErrChain<`Big>> = f;
    let r = 0;
    catch(e) r <- e ~ 1;
    g(0) ~ r
}
"#, refused("`Big does not contain `Small"); FuseExpect::None);

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
"#, refused("(i64, string) does not contain (i64, i64)"); FuseExpect::None);

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

run!(default_hidden_by_a_callback, DEFAULT_HIDDEN_BY_A_CALLBACK, |v: Result<&Value>| {
    refused("f64 does not contain i64")(v)
}; FuseExpect::None);

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

run!(nested_two_deep, NESTED_TWO_DEEP, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a[..] == [Value::Bool(true), Value::Bool(true)],
    _ => false,
}; FuseExpect::None);

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
// in it is rewritten.
const BUILTIN_VARIADIC_TRAIT: &str = r#"
{
    let f = |@args: fn(x: Eq) -> i64| -> Any 'core_all;
    let r = f(|x| 1, |x| 2);
    0
}
"#;

run!(builtin_variadic_trait, BUILTIN_VARIADIC_TRAIT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(0)))
}; FuseExpect::None);

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
    refused("default reads an enclosing open type"); FuseExpect::None);

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
    nested_tvar_default_refused,
    NESTED_TVAR_DEFAULT_REFUSED,
    refused("Array<'a: string> does not contain Array<i64>");
    FuseExpect::None
);

/// An embedder bounds the activations its runtime holds: a node-walked
/// recursion without end aborts the runtime at the budget, where it would
/// otherwise fill memory, and the count follows the program.
/// `code` under a 2000-activation budget, built with `flags`: the runtime
/// stops it there.
async fn budget_stops(
    flags: enumflags2::BitFlags<graphix_compiler::CFlag>,
    code: &str,
) -> anyhow::Result<()> {
    use graphix_package_core::testing::{TestCtx, config};
    let (tx, _rx) = tokio::sync::mpsc::channel(10);
    let ctx =
        TestCtx::start(config(tx, &crate::TEST_REGISTER, vec![], |_| {})?.flags(flags))
            .await?;
    ctx.rt.control().set_activation_budget(2000);
    ctx.rt.compile(arcstr::ArcStr::from(code)).await?;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(30);
    while !ctx.rt.budget_aborted() && tokio::time::Instant::now() < deadline {
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
    }
    assert!(ctx.rt.budget_aborted(), "the runaway recursion was not stopped");
    let live = ctx.rt.control().live_activations();
    assert!((2000..10_000).contains(&live), "{live} live activations");
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

// A nested fn type's quantifier named like the signature's is its own:
// the outer 'b stays rigid whichever parameter comes first.
run!(
    shadowing_quantifier_keeps_outer_rigid,
    refused("cannot compute 'b: unbound within Number + i64"),
    "/test.gx" => "let f = 'b: Number |g: fn<'b: Number>(x: 'b) -> 'b, y: 'b| -> 'b y + 1; let result = 0"
; FuseExpect::None);

run!(
    shadowing_quantifier_keeps_outer_rigid_swapped,
    refused("cannot compute 'b: unbound within Number + i64"),
    "/test.gx" => "let f = 'b: Number |y: 'b, g: fn<'b: Number>(x: 'b) -> 'b| -> 'b y + 1; let result = 0"
; FuseExpect::None);

// A declared variable beside a concrete member cannot be made one type:
// the body is refused, not the variable narrowed to the member.
run!(
    rigid_member_beside_a_known_one_is_refused,
    refused("does not contain [i64, 'r: unbound within Number]"),
    "/test.gx" => r#"
        let g = 'a: Singleton |x: 'a| -> 'a x;
        let f = 'r: Number |x: 'r, b: bool| g(select b { true => x, false => 1 });
        let result = f(3, false)
    "#
; FuseExpect::None);

// A settled cell keeps its relation to the live cells its bound names:
// 'b settles to Array<'a> with 'a the caller's i64, so h takes no array
// of strings, at the check as at the build.
run!(
    a_settled_bound_keeps_its_live_cells,
    refused("Array<i64> does not contain Array<string>"),
    "/test.gx" => r#"
        let f = 'b: Array<'a> |x: 'a| -> fn(v: 'b) -> 'a |v: 'b| select v { [h, ..] => h, _ => x };
        let g = |y| {
          let k = f(y);
          let z: i64 = y;
          k
        };
        let h = g(1);
        let result = h(["a", "b"])
    "#
; FuseExpect::None);

// A rank-2 formal's quantifier named like the definition's own is its
// own, whichever parameter comes first: check and build agree.
run!(
    shadowing_rank2_formal_after,
    is(Value::F64(2.5)),
    "/test.gx" => "let f = 'a: Number |y: 'a, g: fn<'a: Number>(x: 'a) -> 'a| -> 'a g(y); let result = f(2.5, |x| x)"
);

run!(
    shadowing_rank2_formal_before,
    is(Value::F64(2.5)),
    "/test.gx" => "let f = 'a: Number |g: fn<'a: Number>(x: 'a) -> 'a, y: 'a| -> 'a g(y); let result = f(|x| x, 2.5)"
);

// A rank-2 formal passes on to another rank-2 formal: its value is any
// instance of itself. One bounded tighter than the target is refused.
run!(
    rank2_formal_forwards_to_rank2,
    is(Value::I64(1)),
    "/test.gx" => r#"
        let app = |f: fn<'b: Any>(x: 'b) -> 'b| f(1);
        let app2 = |g: fn<'c: Any>(x: 'c) -> 'c| app(g);
        let result = app2(|x| x)
    "#
);

run!(
    rank2_formal_bounded_tighter_is_refused,
    refused("does not contain fn<'c: Number>"),
    "/test.gx" => r#"
        let app = |f: fn<'b: Any>(x: 'b) -> 'b| f("s");
        let app2 = |g: fn<'c: Number>(x: 'c) -> 'c| app(g);
        let result = app2(|x| x)
    "#
; FuseExpect::None);

// A generic function value is generic wherever it stands: an element of
// a collection keeps its quantifier, and each call picks it anew.
run!(
    generic_function_elements_stay_generic,
    r#"{
        let fs = array::map([1, 2], |x| 'a: Number |x: 'a, y: 'a| -> 'a x + y);
        array::map(fs, |f| (f(1, 2), f(f32:1.5, f32:2.0)))
    }"#,
    prints("[[i64:3, f32:3.5], [i64:3, f32:3.5]]");
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
    is(Value::I64(0));
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

// A data-only variable settles to the widest argument whatever the order,
// an argument whose type is an open cell included.
run!(
    widening_waits_for_an_open_argument,
    r#"{
  let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick(i64:1, z, v);
  h(null)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, null, null]")
);

// The same with the open cell nested inside the argument.
run!(
    widening_waits_for_a_nested_open_argument,
    r#"{
  let pick = |x: Array<'a>, y: Array<'a>, z: Array<'a>| -> Array<Array<'a>> [x, y, z];
  let v: [i64, null] = null;
  let h = |z| pick([i64:1], [z], [v]);
  h(null)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[[i64:1], [null], [null]]")
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
}"#, refused("does not contain fn(x: i64) -> (fn(y: i64) -> i64) throws Error<ErrChain<`E>>"); FuseExpect::None);

// A parameter whose own parameter is rank-2 takes a polymorphic
// argument at each call: the call copies the inner quantifier, in the
// body and in a lambda nested in it alike.
run!(rank2_quantifier_of_a_parameter_copied_at_each_call, r#"{
  let use_it = |k: fn(f: fn<'c: Any>(x: 'c) -> 'c) -> (i64, string)| k(|z| z);
  let nested = |k: fn(f: fn<'c: Any>(x: 'c) -> 'c) -> (i64, string)| { let g = |u: i64| k(|z| z); g(1) };
  let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
  (use_it(apply), nested(apply))
}"#, prints(r#"[[i64:1, "s"], [i64:1, "s"]]"#); FuseExpect::Jit);

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
    r#"{ let rec g = |#s = 0, x: i64| g(x - -1); g(#s: "top", 1) }"#,
    refused("type mismatch i64 does not contain string")
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
