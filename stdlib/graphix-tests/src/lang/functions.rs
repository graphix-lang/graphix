// Tests for lambdas, first-class functions, labeled arguments, recursive functions

use anyhow::Result;
use graphix_package_core::run_no_jit;
use netidx::publisher::Value;

const LAMBDA: &str = r#"
{
  let y = 10;
  let f = |x| x + y;
  f(10)
}
"#;

run_no_jit!(lambda, LAMBDA, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
});

const FIRST_CLASS_LAMBDAS: &str = r#"
{
  let doit = |x: Number| x + 1;
  let g = |f: fn<'a: Number>(x: 'a) -> 'a, y| f(y) + 1;
  g(doit, 1)
}
"#;

run_no_jit!(first_class_lambdas, FIRST_CLASS_LAMBDAS, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const LABELED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#foo: 0)
}
"#;

run_no_jit!(labeled_args, LABELED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const REQUIRED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#bar: 0)
}
"#;

run_no_jit!(required_args, REQUIRED_ARGS, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const MIXED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42, baz| foo + bar + baz;
  f(#foo: 0, 0)
}
"#;

run_no_jit!(mixed_args, MIXED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const ARG_SUBTYPING: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let g = |f: fn(#foo: Number) -> Number| f(#foo: 3);
  g(f)
}
"#;

run_no_jit!(arg_subtyping, ARG_SUBTYPING, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
});

const ARG_NAME_SHORT: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let foo = 3;
  f(#foo)
}
"#;

run_no_jit!(arg_name_short, ARG_NAME_SHORT, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
});

const LATE_BINDING0: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| x - y + c };
  let f = t.f;
  f(#y: 3, #x: 4)
}
"#;

run_no_jit!(late_binding0, LATE_BINDING0, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

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

run_no_jit!(late_binding1, LATE_BINDING1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2)] => true,
        _ => false,
    },
    _ => false,
});

const LATE_BINDING2: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  (t.f)(#y: 3, #x: 4)
}
"#;

run_no_jit!(late_binding2, LATE_BINDING2, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

const LATE_BINDING3: &str = r#"
{
    let f: fn(x: i64) -> i64 = never();
    let res = f(1);
    f <- |i: i64| i + 1;
    res
}
"#;

run_no_jit!(late_binding3, LATE_BINDING3, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

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

run_no_jit!(late_binding4, LATE_BINDING4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 3]>() {
        Ok([0, 0, 55]) => true,
        Ok(_) | Err(_) => false,
    },
    _ => false,
});

const RECURSIVE_LAMBDA0: &str = r#"
{
    let rec f = |x: i64| select x { x if x < 10 => f(x + 1), x => x };
    f(0)
}
"#;

run_no_jit!(recursive_lambda0, RECURSIVE_LAMBDA0, |v: Result<&Value>| match v {
    Ok(Value::I64(10)) => true,
    _ => false,
});

// Fusion smoke test: a fully-annotated arithmetic lambda. With the
// KIR path wired through Lambda::compile, this should run via
// KirNode (tree-walking interpreter over typed primitives) instead
// of GXLambda (node-graph walker). Output equality is the
// regression check; the speed benefit is exercised in M5.
const KIR_FUSED_ARITH: &str = r#"
{
    let f = |a: i64, b: i64| -> i64 a * a + b * b;
    f(3, 4)
}
"#;

run_no_jit!(kir_fused_arith, KIR_FUSED_ARITH, |v: Result<&Value>| match v {
    Ok(Value::I64(25)) => true,
    _ => false,
});

// Fusion smoke test: tail-recursive countdown with full annotations
// and the binding-name hint. Self-call in tail position lowers to
// `KirStmt::TailCall` + `loop` back-edge, runs through KirNode.
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

run_no_jit!(kir_fused_tail_loop, KIR_FUSED_TAIL_LOOP, |v: Result<&Value>| match v {
    // 1 + 2 + ... + 100 = 5050
    Ok(Value::I64(5050)) => true,
    _ => false,
});

// Fusion smoke test: a mandelbrot-shape kernel. Same iterate as the
// unit tests, exercised through the runtime's Apply path.
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

run_no_jit!(kir_fused_mandelbrot, KIR_FUSED_MANDELBROT, |v: Result<&Value>| match v {
    // c=1+0i: trace 0 → 1 → 2 → 5 → escape; |5|² = 25 > 4 at i=7.
    Ok(Value::I64(7)) => true,
    _ => false,
});

// Deferred fusion: an unannotated callback `|x| x * 2` passed to a
// HOF. Eager fusion fails (no type on `x`); the deferred path
// re-attempts at first call using the typechecker-resolved FnType
// from the call site, fills in `x: i64`, and fuses. Output equality
// is the regression check; `array::fold` exercises a per-element
// invocation pattern that's the realistic deferred-fusion target.
const KIR_FUSED_DEFERRED_MAP: &str = r#"
{
    use array;
    let xs = array::init(100, |idx: i64| idx);
    array::fold(xs, 0, |acc, x| acc + x * 2)
}
"#;

run_no_jit!(kir_fused_deferred_map, KIR_FUSED_DEFERRED_MAP, |v: Result<&Value>| match v {
    // sum_{i=0}^{99} 2i = 2 * 99*100/2 = 9900
    Ok(Value::I64(9900)) => true,
    _ => false,
});

// Lazy fusion correctness: a recursive lambda with NO annotations
// should still produce correct output. The typechecker infers types
// from the call site; lazy fusion uses fn_types via spec_id when
// building the kernel. If lazy resolution falls back to the user's
// argspec annotations alone, this test would still produce the
// right value (just unfused), so it's a regression test for
// correctness only — not for "fuses".
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

run_no_jit!(kir_lazy_no_annotations, KIR_LAZY_NO_ANNOTATIONS, |v: Result<&Value>| match v {
    // 1 + 2 + ... + 100 = 5050
    Ok(Value::I64(5050)) => true,
    _ => false,
});

// Three-level recursive fusion with NO annotations. Tests that
// lazy fusion threads through arbitrarily nested call chains using
// typechecker-inferred types alone.
//
//   inner(x)  = x * x + 1
//   middle(x) = inner(x) + inner(x + 1)
//   outer(x)  = middle(x) - middle(x - 1)
//
// outer(5):
//   middle(5)  = inner(5) + inner(6) = 26 + 37 = 63
//   middle(4)  = inner(4) + inner(5) = 17 + 26 = 43
//   outer(5)   = 63 - 43 = 20
const KIR_LAZY_THREE_LEVEL: &str = r#"
{
    let inner = |x| x * x + 1;
    let middle = |x| inner(x) + inner(x + 1);
    let outer = |x| middle(x) - middle(x - 1);
    outer(5)
}
"#;

run_no_jit!(kir_lazy_three_level, KIR_LAZY_THREE_LEVEL, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
});

// Higher-order function with a function-typed argument. Pre-DynCall
// the kernel build for `combine` would fail because `f: fn(i64) ->
// i64` isn't a primitive — so combine's body ran through GXLambda.
// With DynCall, fusion registers `f` as a fn-typed param, the body
// `f(x) + 1` lowers to KirOp::DynCall, and the interpreter
// dispatches to the LambdaDef passed at the call site. Result is
// 5*5 + 1 = 26.
const KIR_DYNCALL_HOF: &str = r#"
{
    let square = |x: i64| -> i64 x * x;
    let combine = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x) + 1;
    combine(square, 5)
}
"#;

run_no_jit!(kir_dyncall_hof, KIR_DYNCALL_HOF, |v: Result<&Value>| match v {
    Ok(Value::I64(26)) => true,
    _ => false,
});

// Static-but-non-fusable callee. `helper` is a let-bound stable
// lambda whose body uses a non-primitive intermediate (Array<i64>),
// so its kernel build returns None and the lazy-fusion path
// registers it as a `FnSource::Binding` DynCall slot. `outer`
// fuses, lowering `helper(x)` to `KirOp::DynCall` against the
// binding's BindId; at runtime KirNode reads ctx.cached[bind_id]
// for the LambdaDef and dispatches via `Apply::update`. Result is
// helper(5) + 1 = (5*5+5*5) + 1 = 51.
const KIR_DYNCALL_STATIC_NONFUSABLE: &str = r#"
{
    use array;
    let helper = |x: i64| -> i64 array::fold([x, x], 0, |a, b| a + b * b);
    let outer = |x: i64| -> i64 helper(x) + 1;
    outer(5)
}
"#;

run_no_jit!(
    kir_dyncall_static_nonfusable,
    KIR_DYNCALL_STATIC_NONFUSABLE,
    |v: Result<&Value>| match v {
        Ok(Value::I64(51)) => true,
        _ => false,
    }
);

const LAMBDAMATCH0: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}: T| bar + bar;
  f(x)
}
"#;

run_no_jit!(lambdamatch0, LAMBDAMATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH1: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}| bar + bar;
  f(x)
}
"#;

run_no_jit!(lambdamatch1, LAMBDAMATCH1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const LAMBDAMATCH2: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f(x)
}
"#;

run_no_jit!(lambdamatch2, LAMBDAMATCH2, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH3: &str = r#"
{
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f({bar: 42, baz: 1})
}
"#;

run_no_jit!(lambdamatch3, LAMBDAMATCH3, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const LAMBDAMATCH4: &str = r#"
{
  let f = |(i, _)| i * 2;
  f((42, "foo"))
}
"#;

run_no_jit!(lambdamatch4, LAMBDAMATCH4, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH5: &str = r#"
{
  let f = |(i, _)| i * 2;
  f("foo")
}
"#;

run_no_jit!(lambdamatch5, LAMBDAMATCH5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

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

run_no_jit!(nested_optional0, NESTED_OPTIONAL0, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

// Regression test: callsite args must be updated every cycle, not just
// when the function is bound. array::iter produces 10, 20, 30 across
// the first 3 cycles then exhausts. The function binds on cycle 6
// (when step reaches 5). By then the arg value (30) is only in cached,
// not in event.variables. The fix ensures bind() populates cached values
// so the function sees the last arg value.
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

run_no_jit!(arg_update_before_bind, ARG_UPDATE_BEFORE_BIND, |v: Result<&Value>| match v {
    Ok(Value::I64(31)) => true,
    _ => false,
});

// Verify that arg changes propagate through the ArgRef proxy after the
// function is already bound (steady-state !bound path).
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

run_no_jit!(arg_update_after_bind, ARG_UPDATE_AFTER_BIND, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 4]>() {
        Ok([0, 10, 20, 30]) => true,
        _ => false,
    },
    _ => false,
});

// Variadic args: extra positional args beyond the fixed signature
const VARGS0: &str = r#"
array::push([1, 2], 3, 4, 5)
"#;

run_no_jit!(vargs0, VARGS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5)] => true,
        _ => false,
    },
    _ => false,
});
