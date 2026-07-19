// Tests for lambdas, first-class functions, labeled arguments, recursive functions

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

const LAMBDA: &str = r#"
{
  let y = 10;
  let f = |x| x + y;
  f(10)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// Homogeneous arithmetic note: the previous fixture's
// `|f: fn<'a: Number>(x: 'a) -> 'a, y| f(y) + 1` is now correctly a
// compile error — `f(y)` is the param's ARBITRARY rigid 'a, and
// adding a concrete i64 to arbitrary 'a is exactly the param_knot
// shape the ruling rejects. First-class fn-typed params are the
// purpose here, so the fn type is monomorphic — which also lets the
// site statically resolve and the whole program FUSE (the old
// generic-param version was pinned None).
run!(first_class_lambdas, FIRST_CLASS_LAMBDAS, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const LABELED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#foo: 0)
}
"#;

// Not fused, by design: `#foo: Number, #bar: Number` → `foo + bar`
// returns the loose `Number` set (foo/bar may be different number
// types — genuinely dynamic), same root cause as `sum`. Labeled-arg
// lambda fusion itself WORKS — verified: the identical lambda with
// concrete `i64` params fuses + JITs.
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

// Not fused, by design: loose `Number` return (same as labeled_args /
// sum). Labeled+positional mixed-arg lambda fusion works with concrete
// types; the blocker here is the dynamic `Number` result.
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

// ASPIRE: Jit (currently None) — blocked on: fn-typed lambda arg (HOF /
// dynamic dispatch — `g` takes `f: fn(...)` as a value). NOT labeled
// args (those fuse) and not loose Number — this is the HOF gap.
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

// Not fused, by design: loose `Number` return (same as labeled_args /
// sum). The `#foo` arg-name shorthand and labeled-arg machinery fuse
// with concrete types; the blocker is the dynamic `Number` result.
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(late_binding4, LATE_BINDING4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 3]>() {
        Ok([0, 0, 55]) => true,
        Ok(_) | Err(_) => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const RECURSIVE_LAMBDA0: &str = r#"
{
    let rec f = |x: i64| select x { x if x < 10 => f(x + 1), x => x };
    f(0)
}
"#;

// Was ASPIRE (blocked on recursive lambda lazy fusion) until the
// monomorphic-recursion tc0 knot: the self-call's orphaned rtype cell
// used to leave the signature unresolvable; knotted to the def's own
// cells it μ-collapses to i64 and the tail loop fuses.
run!(recursive_lambda0, RECURSIVE_LAMBDA0, |v: Result<&Value>| match v {
    Ok(Value::I64(10)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Fusion smoke test: a fully-annotated arithmetic lambda. With
// fusion wired through Lambda::compile, this should run via
// Kernel (tree-walking interpreter over typed primitives) instead
// of GXLambda (node-graph walker). Output equality is the
// regression check; the speed benefit is exercised in M5.
const KIR_FUSED_ARITH: &str = r#"
{
    let f = |a: i64, b: i64| -> i64 a * a + b * b;
    f(3, 4)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(fused_arith, KIR_FUSED_ARITH, |v: Result<&Value>| match v {
    Ok(Value::I64(25)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Fusion smoke test: tail-recursive countdown with full annotations
// and the binding-name hint. Self-call in tail position lowers to
// a tail-call rebind-and-jump loop, runs through Kernel.
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(fused_tail_loop, KIR_FUSED_TAIL_LOOP, |v: Result<&Value>| match v {
    // 1 + 2 + ... + 100 = 5050
    Ok(Value::I64(5050)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Deep sync tail recursion. 500k levels overflows the OLD node-walk's
// native stack (~50k frames) — the bug this whole change fixes. With the
// shared tail-loop facts the interpreter loops in place (constant stack),
// matching the JIT's native loop; the differential `run!` asserts both
// modes reach the same value without overflowing.
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

// A self-call in OPERAND (not tail) position — `n * fact(n - 1)` — must
// NOT be looped: the tail position is the `*`, not the call. Both backends
// recurse normally (shallow here, so no overflow); guards against the
// tail-loop firing for non-tail recursion.
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(fused_mandelbrot, KIR_FUSED_MANDELBROT, |v: Result<&Value>| match v {
    // c=1+0i: trace 0 → 1 → 2 → 5 → escape; |5|² = 25 > 4 at i=7.
    Ok(Value::I64(7)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

run!(fused_deferred_map, KIR_FUSED_DEFERRED_MAP, |v: Result<&Value>| match v {
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(lazy_no_annotations, KIR_LAZY_NO_ANNOTATIONS, |v: Result<&Value>| match v {
    // 1 + 2 + ... + 100 = 5050
    Ok(Value::I64(5050)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

// Fuses since tvar cell constraints (#20): the bare lambdas' operand
// cells settle to their conjunction witness (i64 from the literals)
// instead of binding wide, so the whole three-level chain JITs.
run!(lazy_three_level, KIR_LAZY_THREE_LEVEL, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Higher-order function with a function-typed argument. Pre-DynCall
// the kernel build for `combine` would fail because `f: fn(i64) ->
// i64` isn't a primitive — so combine's body ran through GXLambda.
// With DynCall, fusion registers `f` as a fn-typed param, the body
// `f(x) + 1` lowers to a DynCall, and the fused kernel
// dispatches to the LambdaDef passed at the call site. Result is
// 5*5 + 1 = 26.
const KIR_DYNCALL_HOF: &str = r#"
{
    let square = |x: i64| -> i64 x * x;
    let combine = |f: fn(x: i64) -> i64, x: i64| -> i64 f(x) + 1;
    combine(square, 5)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(dyncall_hof, KIR_DYNCALL_HOF, |v: Result<&Value>| match v {
    Ok(Value::I64(26)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A let-bound `helper` whose body is `array::fold` over a literal
// (scalar i64 -> i64), called by `outer`. #203 Phase C discovers the
// `outer` -> `helper` call transitively and builds `helper`'s kernel
// (its `array::fold`-over-literal body fuses), so the region JITs
// instead of falling back to the `FnSource::Binding` DynCall slot the
// older non-transitive path used. Result is helper(5) + 1 =
// (5*5 + 5*5) + 1 = 51.
const KIR_DYNCALL_STATIC_NONFUSABLE: &str = r#"
{
    use array;
    let helper = |x: i64| -> i64 array::fold([x, x], 0, |a, b| a + b * b);
    let outer = |x: i64| -> i64 helper(x) + 1;
    outer(5)
}
"#;

// The transitive call chain includes a compiler-owned array fold in
// `helper`; direct collection lowering keeps the whole chain native.
run!(
    dyncall_static_nonfusable,
    KIR_DYNCALL_STATIC_NONFUSABLE,
    |v: Result<&Value>| match v {
        Ok(Value::I64(51)) => true,
        _ => false,
    }; graphix_package_core::testing::FuseExpect::Jit);

// #203 Phase C — a deep TRANSITIVE chain g1 -> g2 -> g3. Discovery walks
// each callee's body in turn, builds every kernel in the closure, and the
// define loop declares them all so each can CLIF-call the next. The whole
// chain JITs (interp == jit). g1(10) = g2(10)-1 = g3(10)*2-1 = 11*2-1 = 21.
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

// Stage 2 — a transitively-called callee whose BODY contains a sync DynCall
// (`cast<i64>` is non-numeric, so it lowers to the cast machinery rather than
// inline arithmetic). The callee `g` is a bare cross-kernel FuncId with no
// `Kernel` of its own; its cast dispatches through the REGION-WIDE combined
// `dyn_slots` table (parent slots first, then `g`'s), with `g`'s body baking
// `fn_index = base + local`. g(true)=1, g(false)=0 → 1. Before Stage 2 the
// callee emitter used an empty apply-site map and this de-fused.
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

// Stage 2 — the DynCall sits two callee levels deep (g -> h -> cast). The
// combined table carries both callees' slots; each body bakes its own base.
// h(true)=1,h(false)=0; g(b)=h(b)+10 → 11 + 10 = 21.
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

// Stage 2 SOUNDNESS — the cross-region cache-key witness. The function-valued
// `let g` node-walks, splitting the block into SEPARATE regions that each call
// `g`. Region `a` also has a root-level cast, so `g`'s slots land at base 1
// there; region `bb` has no root cast, so `g` is at base 0. `g`'s body bakes
// `base + local`, so the SAME `g` `KernelSig` needs two distinct compiled
// bodies — keyed `(ptr, base)`. A ptr-only cache would hand region `bb` region
// `a`'s body (baked at base 1) and dispatch off the end of `bb`'s 1-slot table.
// a = g(true) + cast(false) = 1 + 0 = 1; bb = g(false) = 0; a + bb = 1.
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

// Stage 2 — a RECURSIVE callee whose base case has a DynCall. The cast in the
// `0 =>` arm dispatches through the combined table; the `_ =>` arm self-calls
// via the kernel's own FuncRef (a CLIF call, not a DynCall, so unaffected by
// the offset). g(3)→g(2)→g(1)→g(0)→cast<i64>(true) = 1.
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — blocked on: composite/value cross-kernel call args
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(nested_optional0, NESTED_OPTIONAL0, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(arg_update_before_bind, ARG_UPDATE_BEFORE_BIND, |v: Result<&Value>| match v {
    Ok(Value::I64(31)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(arg_update_after_bind, ARG_UPDATE_AFTER_BIND, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 4]>() {
        Ok([0, 10, 20, 30]) => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Variadic args: extra positional args beyond the fixed signature
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

// Cross-kernel callee resolution is keyed by kernel IDENTITY, not
// source name (audit-jul2026/01): g's body call to the OUTER f must
// bind to the outer f's kernel even though a later `let f` shadows the
// name. Name-keyed resolution silently answered 1 here.
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
// (audit-jul2026/02): the kernel cache must key on the CALL SITE's
// resolved FnType, and a site whose lambda instance reports a different
// monomorphization (the def's TVar cells were won by the first site)
// must refuse to fuse rather than emit against the wrong body. The
// name-keyed version panicked cranelift's FunctionBuilder here and
// killed the runtime worker.
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

// A fold-callback body whose local (`e`) shares a name with a nested
// callee's parameter. It pins BindId-based resolution even though the
// nested call currently keeps this collection on the node-walk.
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
); graphix_package_core::testing::FuseExpect::None);

// An ABANDONED kernel-closure build (the base arm's select-with-error-
// arm de-fuses the rec lambda) used to leave declared-but-undefined
// callee symbols in the shared per-context module; the next successful
// compile's finalize_definitions then PANICKED cranelift ("can't
// resolve symbol") and killed the runtime. All three ingredients are
// load-bearing: the rec (a cross-kernel callee gets declared), the
// select-with-error-arm base (the abandon), and the UNUSED call
// binding (a separate region compiles after the abandon and triggers
// finalize). Abandoned declarations now get trap stubs
// (emit.rs define_stub_body). Found by fuzzer-v2's generated sweep.
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

// #219 taint-escalation regressions (fuzz/triage-fuzzer-v2, found by
// the fuzzer-v2 generated campaigns): a locally-unconsumed bottom used
// to abort the WHOLE kernel at HOF/composite boundaries where the
// node-walk bottoms only the consuming path.

// A compiler-owned fold whose init BOTTOMS never dispatches: the
// collection call waits for every argument, even one the callback
// never reads. The tail is independent so the fixture
// observes agreement: the tripped fold is silent, the sibling fires.
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

// The dual: the callback CONSUMES the bottom acc — stays bottom in
// both modes (the run! harness sees the interp/jit agreement; the
// observable is the gate never firing, so pin a program whose tail is
// independent).
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

// A bottom map SLOT taints the map's result, not the kernel — the
// unrelated const tail still fires.
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

// find scans ALL slots: a bottom predicate AFTER the matching element
// still bottoms the find (the node-walk's aggregator requires every
// slot complete). The early-exiting loop returned the match — the JIT
// produced a value where the node-walk produced nothing (found while
// FIXING the escalation class; the opposite failure direction). The
// observable: the independent tail fires, the find result does not.
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

// A tainted fold INIT is a poisoned acc delivery, not a whole-fold
// abort: a callback that never consumes the acc recovers (the
// kernel's FoldAcc carries init taint on the acc alone). The interp's
// force-taint silenced the fold where the kernel emitted — jul19b
// generate class: the tail-looped callee's base-case bottom escapes
// as a tainted production at init and poisons the init bind. The
// depth-0 call (no tail jump → bottom is None, init absent) agreed
// all along; the jump is what mints the tainted production.
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

// The consuming twin: a callback that READS the acc must not recover —
// the poisoned delivery taints slot 0 and the fold bottoms in both
// modes (the kernel's sticky flags fold). The independent const tail
// is the observable.
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

// A capture read only by a sleeping select arm must not re-fire the
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

// The consumed-capture dual: the body READS y in the taken path, so
// the map re-fires per y event.
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

// A same-length source update with a constant callback body leaves
// every retained slot quiet, so the collection emits only initially.
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

// The call-depth guard (DEFAULT_MAX_CALL_DEPTH = 256), limit±1 in both
// modes: each nested non-tail lambda dispatch counts one against the
// shared Control counter (node-walk GXLambda::update; JIT lambda-call
// sites), so the outer f(n) call is depth 1 and n = 255 is the deepest
// argument that completes. At the limit the dispatch produces BOTTOM
// (logged), like unchecked-arith failures — before the guard this was
// a runtime-killing stack overflow at ~1,600 frames in the node-walk.
// Tail self-calls are exempt on both sides (the 5M-deep
// jit_deep_tail_probe pins that).
const DEPTH_GUARD_UNDER_LIMIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  f(i64:255)
}
"#;

run!(depth_guard_under_limit, DEPTH_GUARD_UNDER_LIMIT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(32640)))
}; graphix_package_core::testing::FuseExpect::Jit);

// The at-limit sibling (f(256) → bottom in both modes) lives in the
// fuzz findings corpus (findings/depth-guard-jul2026/) — empty-trace
// agreement is a first-class assertion there, while the run! harness
// has no runtime-bottom expectation (it would wait out its timeout).

// Depth accounting with compiler-owned collection nodes: f(254) is 255
// dispatch units and the collection callback is one more, exactly at
// the 256 limit. The reserved collection marker is compiler plumbing,
// not another user-lambda dispatch.
const DEPTH_GUARD_HOF_UNIT_UNDER_LIMIT: &str = r#"
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
    depth_guard_hof_unit_under_limit,
    DEPTH_GUARD_HOF_UNIT_UNDER_LIMIT,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42285))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A depth trip bottoms the CALL, not the kernel: the tripping f(256)
// sits in fold's init argument, so the fold never dispatches (a call
// waits for every arg), and
// the trip stays LOCAL: the independent sibling fold still fires in
// both modes (the former whole-kernel abort swallowed it — soak
// jul07h, findings/depth-guard-jul2026/06).
const DEPTH_TRIP_LOCAL_RECOVERY: &str = r#"
{
  let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) };
  array::fold([i64:41], f(i64:256), |acc, x| x + i64:1);
  array::fold([i64:41], i64:0, |acc, x| x + i64:1)
}
"#;

run!(
    depth_trip_local_recovery,
    DEPTH_TRIP_LOCAL_RECOVERY,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A rec lambda NESTED in another lambda's body tail-loops in BOTH
// modes: the #203 resolution cascade (drive a resolved callee's body
// through typecheck1 so nested sites resolve) used to be fusion-gated,
// so under FusionDisabled `analysis::analyze` never saw lp's callsite
// as resolved, never tail-marked it, and the interp stack-recursed
// what the fused path looped — at depth 500 (past the 256 call-depth
// guard) the interp bottomed while the JIT returned 125251
// (fuzz/triage-fuzzer-v2/divergence_000008). The cascade now runs in
// every mode.
const NESTED_TAIL_LOOP: &str = r#"
{
  let f = |x: i64| -> i64 {
    let rec lp = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) };
    lp(i64:500, i64:0) + x
  };
  f(i64:1)
}
"#;

// ASPIRE(FuseExpect::Jit): the shape doesn't fuse (a rec lambda bound
// inside another lambda's body is the local-lambda-in-body missed-
// fusion class); what this fixture pins is the MODE PARITY of the
// tail loop — the node-walk completes depth 500 with the same value.
run!(nested_tail_loop, NESTED_TAIL_LOOP, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(125251)))
}; graphix_package_core::testing::FuseExpect::Jit);

// An `-> i64` rtype annotation must reject an error-producing arm.
// This compiled for a while: `Type::union` collapsed two DISTINCT
// unbound tvars into one (`TVar::eq` calls None == None equal), so
// the select's arm union dropped the error arm's not-yet-bound
// rtype cell — by the time `error(i64:0)` resolved to `Error<i64>`
// the body type no longer referenced its cell, and the def-time
// rtype check bound the lone survivor to i64 vacuously. The JIT then
// froze a scalar i64 return slot and leaked the Error payload as a
// pointer (fuzz soak jul05 item 11, divergence_000010).
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

// The `let rec` twin of the above — recursion typing must not admit
// what the non-recursive form rejects.
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

// Monomorphic recursion: a self-call unifies against the def's OWN
// ftype cells (the tc0 knot in `CallSite::typecheck0` /
// `ExecCtx::rec_defs`), so a self-call arg that disagrees with the
// entry call's narrowing is a DEF-TIME error — exactly as it is for a
// non-recursive twin. Before the knot the self-call site freshened its
// cells, the arm union carried an orphan tvar, `constrain_known`
// widened it to Any, and the JIT's marshal tried to PARSE the string
// into the i64 slot — SIGABRT (fuzz soak jul05 item 17,
// crash_000016). The freshening also silently degraded every rec
// lambda's checked signature to Any (lazy_no_annotations de-fused).
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

// The positive control for the union fix: two distinct unbound tvars
// in an arm union must NOT collapse — each arm's cell binds later and
// both bindings must survive into the select's type. `pick`'s body
// union is ['a-from-a, 'b-from-b]; both resolve at the call site and
// the mixed-type call stays typeable. (Wrapping the call in a
// type-dispatch select is a separate, PRE-EXISTING limitation — the
// scrutinee's rtype isn't resolved at select-tc0 time — so this pins
// the direct-return shape only.)
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

// An UNANNOTATED variant-returning non-tail rec lambda infers its
// honest union. Broken two ways before: (a) pre-knot, the orphaned
// self-call rtype widened the signature to Any (item 12's crasher
// fused a laundered type and stack-overflowed); (b) post-knot, bare
// variant arms classified as select WILDCARDS (`is_refutable` is
// payload-only — the tag test lives in the TYPE predicate), so
// coverage never ran and the first arm's narrowing walk greedily
// bound the knotted open scrutinee cell to `A alone — a spurious
// dead-arm reject. `matches_anything` now classifies wildcards;
// variant arms join the coverage unions.
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
}; graphix_package_core::testing::FuseExpect::None);

// The exhaustiveness soundness hole the wildcard misclassification
// hid: a select over [`A, `B] missing the `B arm COMPILED (all-bare-
// variant arm sets bypassed the coverage checks entirely). Now a
// compile error.
const SELECT_VARIANT_NONEXHAUSTIVE: &str = r#"
{
  let x: [`A, `B] = `A;
  select x { `A => i64:1 }
}
"#;

run!(select_variant_nonexhaustive, SELECT_VARIANT_NONEXHAUSTIVE, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A tail-recursive `let rec` INSIDE an HOF callback, deep enough
// (500 > the 256 call-depth guard) that the node-walk must tail-loop
// it. The per-slot pred lazy-binds at runtime; before the bind()-time
// typecheck1 cascade + analyze_bound_callee (soak-jul06c B8,
// findings/depth-guard-jul2026/02) the fresh body's tail sites were
// never marked, each tail call was a nested dispatch, and the guard
// bottomed the whole program where the JIT looped to the value.
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

// The split-callback twin of REC_IN_HOF_CALLBACK: a try/catch in the
// same fold callback splits it (TryCatch is async), so the rec runs in
// the split's node-walk residue — cloned per slot from MapQ's
// analysis_pred, which the analysis pass must descend (via
// for_each_hof_callback_body) or the residue's unmarked tail sites
// stack-dispatch into the 256 call-depth guard under fusion only
// (soak-jul06d, findings/depth-guard-jul2026/03).
const REC_IN_SPLIT_CALLBACK: &str = r#"
{
  let v0 = array::fold([i64:-1], i64:255, |acc, x| {
    let rec lp = |n: i64, a: i64| -> i64 select n {i64:0 => a, _ => lp(n - i64:1, a + n)};
    (lp(i64:500, i64:0) * i64:0) + (try ((x /? i64:-1))? catch(e) => i64:42)
  });
  [i64:7 + v0]
}
"#;

run!(rec_in_split_callback, REC_IN_SPLIT_CALLBACK, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(8)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Transient recursion (the O(depth) memory fix): a recursively-bound
// Sync callee instance whose body is a pure activation record is
// DELETED when its call returns and re-bound from the parked def on
// the next genuine call. These three pin the observable edges of that
// mechanism against the retained-instance behavior it replaced.

// A parked site must stay reactively LIVE to the deleted instance's
// captures: cap fires while the recursion's args are quiet, and each
// fire must recompute the result (the parked wake-set,
// `Callee::TransientParked`). Collects [100, 101, 102].
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

// RULED SEMANTICS (Eric, 2026-07-16): state inside a recursive
// function PERSISTS across fires, like reactive state anywhere else —
// recursion doesn't reset it. A stateful builtin in the body keeps the
// retained-unfold instances (`count` is EFFECT=Async so its body is
// excluded from parking structurally; the sync accumulators
// sum/min/max/mean/product refuse `transient_body_ok`'s STATELESS
// check — both roads lead to retention, one semantics): three levels
// of count step 1,2,3 across the three fires, so the sums are
// [3, 6, 9]. (Fresh state per call — [3, 3, 3] — was considered and
// rejected: it either splits the accumulator family across the
// fusion-safety classification or requires resetting async builtins
// too. Node state persisting is also the class-D-consistent answer:
// evaluation-FRAME state dies across cycles, semantic NODE state
// lives.)
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

// Pure non-tail recursion re-fired repeatedly: each fire re-binds the
// parked instances and recomputes the same value — the park/re-bind
// cycle must be idempotent for a pure body. Collects [55, 55, 55].
const REC_TRANSIENT_PURE_REFIRE: &str = r#"
{
  let go = 0;
  go <- select go { n if n < 2 => n + 1, _ => never() };
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, i64:1 => i64:1, _ => f(n - i64:1) + f(n - i64:2)};
  array::group(f(go ~ i64:10), |n, _| n == 3)
}
"#;

run!(rec_transient_pure_refire, REC_TRANSIENT_PURE_REFIRE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(55), Value::I64(55), Value::I64(55)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// The widened transient gate: STATELESS builtins (`str::len`,
// `array::len`) in a recursive body no longer refuse — each re-fire
// re-binds parked instances and re-inits the builtins fresh, which
// must be unobservable for a declared-stateless builtin. f(4) =
// len("abc") + 4*len([n]) = 7 per fire.
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

// Generic Graphix wrappers over compiler-owned collection nodes.
// A lambda param called inside another lambda's body now statically
// resolves per callsite (the def-gate param KNOT types the body against
// the param's declared cells; `try_static_resolve` registers
// statically-known fn args under the instance's param BindIds). The
// same generic def instantiates at unrelated types.

const INLANG_MAP: &str = r#"
{
  let m = |a: Array<'a>, f: fn(x: 'a) -> 'b| -> Array<'b>
    array::fold(a, [], |acc, v| array::push(acc, f(v)));
  (m([1, 2, 3], |x| x * 2), m(["a", "b"], |s| "[s]!"))
}
"#;

// The generic wrapper instantiates independently at each call site.
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
}; graphix_package_core::testing::FuseExpect::None);

// The param knot must not leak the callsite's narrowing into the def:
// `x + i64:1` under `-> 'a` stays polymorphic (the flagless operand
// pre-bind is SUPPRESSED on rigid cells rather than fact-ified into a
// poisoned conjunct — this exact program used to reject the f64 call).
const PARAM_KNOT_NO_LEAK: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + i64:1;
  (f(i64:3), f(f64:2.5))
}
"#;

// Homogeneous arithmetic (Eric's ruling, 2026-07-12): the DEF is
// ill-typed — `x + i64:1` cannot be well-typed for ARBITRARY rigid
// 'a: Number, so the promotion polymorphism this test used to pin
// (`f(i64:3)` AND `f(f64:2.5)` both legal, per-site runtime
// promotion) is deliberately GONE. Generic numeric bodies must not
// mix concrete literals into 'a-typed arithmetic; monomorphize the
// formal or convert explicitly.
run!(param_knot_no_leak, PARAM_KNOT_NO_LEAK, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

// COMPILE-recursion guard pin (soak-jul12l crash_000000): a recursive
// callee whose self-call passes a fn-typed arg used to re-drive its
// own body's fn-arg pre-materialization forever (fresh per-instance
// param BindIds each level) until the compiler stack overflowed. The
// back-edge guard keys on the call-SITE ExprId — specs are shared
// across instances, so self-call re-entry is recursion — while
// map-in-map (same callee, different sites) still re-drives.
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

// A tail-jump arg that BOTTOMS every pass rides its previous value
// instead of killing the call (Eric's ruling 2026-07-15: bottom is
// "no event this cycle", never a NaN-like poison — the node-walk's
// dispatch backfills a quiet-or-failed arg from its cache, and the
// kernel's rebind now keeps the loop-carried previous value on a
// tainted new formal where it used to whole-kernel abort,
// soak-jul14b 000004). str::parse("nan") errors, `?` propagates,
// the acc arg bottoms — the loop keeps acc=0 and reaches the base.
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

// A bare-Array arg node under a VALUE-shaped signature slot
// (`[Array<i64>, null]`): the marshal raw-emitted the composite box
// pointer as the in-band Value payload — the callee then decoded
// garbage (jul17a, findings/value-shape-seam-jul2026/03; the call-arg
// twin of the FoldAcc::Value seam). Narrower-than-slot args now
// normalize through `emit_owned_value_operand_node`.
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

// The RESULT twin of `call_arg_value_slot_narrow` (jul18d fuzz
// crash_000000, findings/value-shape-seam-jul2026/04+05): inference
// widens a CALLSITE arg node's type to the param union, so the arg
// normalization sees "already value-shaped" — but the callee ABI
// (cross-kernel return / inline collection loop) delivers a raw
// array box pointer. `widen_result_to_value` now wraps call results
// whose node type promises a Value.
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

// Unified Value ABI coverage: String args and returns marshal
// cross-kernel (the old asymmetry gate is gone — a string's ArcStr
// bits ARE `Value::String`'s payload word, and every return is the
// genuine two-word pair).
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

// A string-returning callee feeding a union-typed param: the callee's
// (STRING|flags, bits) pair IS the Value the union slot needs — no
// reconciliation layer involved.
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
