// End-to-end fusion tests over `rt.load()`.

use crate::init;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BitFlags, CFlag,
    expr::{ExprId, Source},
};
use graphix_package_core::{
    run,
    testing::{Events, FuseExpect, init_with_flags_and_setup, next_update},
};
use netidx::publisher::Value;
use std::time::Duration;
use tokio::{sync::mpsc, time::Instant};

/// Run `code` in a fresh runtime compiled with `flags`, after compiling
/// each of `pre` (kept alive): loaded as a file when `as_file`, else
/// compiled. Its first value and the (fused kernel runs, JIT wrapper
/// entries) the runtime made from just before it.
async fn run_code(
    flags: BitFlags<CFlag>,
    pre: &[&str],
    code: &str,
    as_file: bool,
) -> Result<(Value, (u64, u64))> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init_with_flags_and_setup(tx, crate::TEST_REGISTER, vec![], flags, |_| {})
        .await?;
    let mut held = Vec::new();
    for p in pre {
        held.push(ctx.rt.compile(ArcStr::from(*p)).await?);
    }
    ctx.rt.control().reset_invocations();
    let res = match as_file {
        true => ctx.rt.load(Source::Internal(ArcStr::from(code))).await?,
        false => ctx.rt.compile(ArcStr::from(code)).await?,
    };
    let eid = res.exprs.first().context("no top-level expr")?.id;
    let v = next_update(&mut rx, eid, Instant::now() + Duration::from_secs(5)).await?;
    let n = ctx.rt.control().invocations();
    ctx.shutdown().await;
    Ok((v, n))
}

async fn load_and_await(code: &str) -> Result<Value> {
    Ok(run_code(BitFlags::empty(), &[], code, true).await?.0)
}

/// [`load_and_await`] with the runtime's (fused kernel runs, JIT wrapper
/// entries).
async fn load_and_count(code: &str) -> Result<(Value, (u64, u64))> {
    run_code(BitFlags::empty(), &[], code, true).await
}

/// [`load_and_await`] under the node-walk.
async fn load_node_walked(code: &str) -> Result<Value> {
    Ok(run_code(CFlag::FusionDisabled.into(), &[], code, true).await?.0)
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_qop_unwraps_result() -> Result<()> {
    // A `?` over checked arith: the unwrap emits in-kernel and the JIT
    // fires.
    let (v, (_, inv)) = load_and_count("(i64:1 +? i64:1)? == i64:2\n").await?;
    assert_eq!(v, Value::Bool(true));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — Qop kernel didn't run via JIT");
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_variadic_fuses() -> Result<()> {
    // A variadic builtin with a fast call fuses like any other.
    let (v, (_, inv)) = load_and_count("and(true, true, false)").await?;
    assert_eq!(v, Value::Bool(false));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — the variadic fast call didn't run via JIT");
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_array_literal_jits() -> Result<()> {
    // `[1, 2, 3]` as a program body; the counter proves the kernel ran.
    let (v, (_, inv)) = load_and_count("[1, 2, 3]").await?;
    assert_i64s(&v, &[1, 2, 3])?;
    assert!(inv > 0, "JIT_INVOCATIONS=0 — array literal kernel didn't run via JIT");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_single_arith() -> Result<()> {
    // The smallest fusable program: one expression, no free variables.
    let v = load_and_await("1 + 2").await?;
    assert_eq!(v, Value::I64(3));
    Ok(())
}

/// A sync builtin call (`core::bit_and`) in a fused region.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_calls_builtin_bit_and() -> Result<()> {
    let (v, (_, inv)) = load_and_count("bit_and(i64:0xFF, i64:0x0F)").await?;
    assert_eq!(v, Value::I64(0x0F));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — bit_and call didn't run via JIT");
    Ok(())
}

/// The JIT-invocation counter itself: `1 + 2` through `rt.load()`
/// counts.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn jit_counter_bumps_on_load() -> Result<()> {
    let (v, (_, inv)) = load_and_count("3 * 4 + 5").await?;
    assert_eq!(v, Value::I64(17));
    assert!(
        inv > 0,
        "JIT_INVOCATIONS=0 after a kernel-spliced load — the JIT \
         wrapper should have run at least once",
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_arith_chain() -> Result<()> {
    // Nested arithmetic, zero free variables.
    let v = load_and_await("(2 * 3) + (4 * 5)").await?;
    assert_eq!(v, Value::I64(26));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_bind_then_expr() -> Result<()> {
    // A Bind followed by an output expression referencing it.
    let v = load_and_await("let x = 5; x + 1").await?;
    assert_eq!(v, Value::I64(6));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn compile_then_compile_external_scalar() -> Result<()> {
    // A compile registers a binding a subsequent compile sees.
    let (v, _) = run_code(BitFlags::empty(), &["let foo = 7;"], "foo * 6", false).await?;
    assert_eq!(v, Value::I64(42));
    Ok(())
}

/// An external string binding flows into a fused kernel as a string
/// param consumed by `str::len`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn external_string_region_param() -> Result<()> {
    let (v, (_, inv)) =
        run_code(BitFlags::empty(), &["let s = \"hello\";"], "str::len(s)", false)
            .await?;
    assert_eq!(v, Value::I64(5));
    assert!(inv > 0, "string region-param kernel should JIT-dispatch");
    Ok(())
}

/// An external `datetime` binding flows into a fused kernel as a value
/// param consumed by `d + duration:1.s`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn external_datetime_region_param() -> Result<()> {
    let (v, (_, inv)) = run_code(
        BitFlags::empty(),
        &["let d = datetime:\"2024-01-01T00:00:00Z\";"],
        "sys::time::add(d, duration:1.s)",
        false,
    )
    .await?;
    let expected: chrono::DateTime<chrono::Utc> = "2024-01-01T00:00:01Z".parse().unwrap();
    assert!(
        matches!(&v, Value::DateTime(dt) if **dt == expected),
        "expected 2024-01-01T00:00:01Z, got {v:?}"
    );
    assert!(inv > 0, "a datetime fastcall site fuses across a region param");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_uses_external_scalar() -> Result<()> {
    // `foo` is bound at root scope by an earlier compile, so it is a
    // free-var Ref inside the loaded file: a scalar kernel input.
    let (v, _) = run_code(BitFlags::empty(), &["let foo = 7;"], "foo * 6", true).await?;
    assert_eq!(v, Value::I64(42));
    Ok(())
}

// Closure conversion: a capturing lambda's captures become extra kernel
// args the caller forwards.

/// Load `code`, returning the produced Value and the JIT-invocation
/// delta across the load.
async fn load_value_and_jit(code: &str) -> Result<(Value, u64)> {
    let (v, (_, inv)) = load_and_count(code).await?;
    Ok((v, inv))
}

/// A single primitive capture: `let y = 7; let f = |x| x + y; f(3)` is 10.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn closure_primitive_capture() -> Result<()> {
    let (v, inv) = load_value_and_jit("let y = 7; let f = |x| x + y; f(3)").await?;
    assert_eq!(v, Value::I64(10));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — capturing closure didn't fuse");
    Ok(())
}

/// A composite (tuple) capture passed across the kernel boundary:
/// `g(10)` is 13.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn closure_tuple_capture() -> Result<()> {
    let (v, inv) =
        load_value_and_jit("let t = (1, 2); let g = |x| t.0 + t.1 + x; g(10)").await?;
    assert_eq!(v, Value::I64(13));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — tuple-capturing closure didn't fuse");
    Ok(())
}

/// Nested closures both capturing `z`: `outer(5)` is 105.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn closure_nested_capture() -> Result<()> {
    let (v, inv) = load_value_and_jit(
        "let z = 100; let outer = |x| { let inner = |y| y + z; inner(x) }; outer(5)",
    )
    .await?;
    assert_eq!(v, Value::I64(105));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — nested capturing closures didn't fuse");
    Ok(())
}

/// A capture resolves by BindId, not name: `f` captures the outer y=7
/// past an inner `let y = 100`, so `f(5)` is 12.
#[tokio::test(flavor = "current_thread")]
async fn closure_capture_respects_shadow() -> Result<()> {
    let v = load_and_await("let y = 7; let f = |x| x + y; { let y = 100; f(5) }").await?;
    assert_eq!(v, Value::I64(12));
    Ok(())
}

/// A fn-typed external resolves statically to a cross-kernel call:
/// `g(5)` is 12.
#[tokio::test(flavor = "current_thread")]
async fn closure_fn_external_static() -> Result<()> {
    let v = load_and_await("let f = |x| x + 1; let g = |y| f(y) * 2; g(5)").await?;
    assert_eq!(v, Value::I64(12));
    Ok(())
}

/// Cross-kernel call args are bucketed by ABI kind: `g((10,20), 5)` is
/// 35 with `(composite, scalar)` formals.
#[tokio::test(flavor = "current_thread")]
async fn call_arg_order_composite_then_scalar() -> Result<()> {
    let v = load_and_await(
        "let g = |p: (i64, i64), n: i64| p.0 + p.1 + n; let pair = (10, 20); g(pair, 5)",
    )
    .await?;
    assert_eq!(v, Value::I64(35));
    Ok(())
}

/// An impure HOF callback (`counter <- v`): the sync sub-region fuses
/// while the connect node-walks.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn impure_hof_callback_splits() -> Result<()> {
    let (v, inv) = load_value_and_jit(
        "let counter = 0; \
         array::map([1, 2, 3], |x| { let v = x * 2 + 1; counter <- v; v })",
    )
    .await?;
    match v {
        Value::Array(a) if &a[..] == [Value::I64(3), Value::I64(5), Value::I64(7)] => {}
        other => bail!("unexpected value: {other:?}"),
    }
    assert!(
        inv > 0,
        "JIT_INVOCATIONS=0 — impure HOF callback's sync sub-region \
         didn't fuse"
    );
    Ok(())
}

/// The impure callback's sync sub-region captures an outer binding `k`:
/// `[10,20,30]`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn impure_hof_callback_split_captures() -> Result<()> {
    let (v, inv) = load_value_and_jit(
        "let k = 10; let counter = 0; \
         array::map([1, 2, 3], |x| { let v = x * k; counter <- v; v })",
    )
    .await?;
    match v {
        Value::Array(a) if &a[..] == [Value::I64(10), Value::I64(20), Value::I64(30)] => {
        }
        other => bail!("unexpected value: {other:?}"),
    }
    assert!(inv > 0, "JIT_INVOCATIONS=0 — captured sync sub-region didn't fuse");
    Ok(())
}

/// An async builtin (`sys::net::publish`) inside an impure callback
/// leaves the map value `[2,4,6]`.
#[tokio::test(flavor = "current_thread")]
async fn impure_hof_builtin_in_residue() -> Result<()> {
    let v = load_and_await(
        "array::map([1, 2, 3], |x| { \
           let v = x * 2; \
           sys::net::publish(\"/local/clone_residue_test_[x]\", v); \
           v \
         })",
    )
    .await?;
    match v {
        Value::Array(a) if &a[..] == [Value::I64(2), Value::I64(4), Value::I64(6)] => {}
        other => bail!("unexpected value: {other:?}"),
    }
    Ok(())
}

// Impure callbacks: `counter <- x` gives each slot an instance of its
// own (sharing the prototype's kernels); each fixture captures an outer
// `k`.

/// Map `body` (an expr over element `x: i64` and captured `k: i64 = 3`)
/// over `[1,2,3,4]` as an impure callback.
async fn impure_map(body: &str) -> Result<Value> {
    // `counter <- x` makes the callback async; `body` may be `let …; expr`.
    let prog = format!(
        "let counter = 0; let k = 3; \
         array::map([1, 2, 3, 4], |x: i64| {{ counter <- x; {body} }})"
    );
    load_and_await(&prog).await
}

/// The same `body` over the same inputs as a pure callback, the native
/// loop. `body` must be a single expression.
async fn pure_map(body: &str) -> Result<Value> {
    load_and_await(&pure_map_program(body)).await
}

fn pure_map_program(body: &str) -> String {
    format!("let k = 3; array::map([1, 2, 3, 4], |x: i64| {body})")
}

fn assert_i64s(v: &Value, expected: &[i64]) -> Result<()> {
    let Value::Array(a) = v else { bail!("not an array: {v:?}") };
    let got: Vec<Option<i64>> = a
        .iter()
        .map(|x| match x {
            Value::I64(n) => Some(*n),
            _ => None,
        })
        .collect();
    let ok = got.len() == expected.len()
        && got.iter().zip(expected).all(|(g, e)| *g == Some(*e));
    if ok { Ok(()) } else { bail!("expected {expected:?}, got {v:?}") }
}

fn assert_strs(v: &Value, expected: &[&str]) -> Result<()> {
    let Value::Array(a) = v else { bail!("not an array: {v:?}") };
    let ok = a.len() == expected.len()
        && a.iter()
            .zip(expected)
            .all(|(x, e)| matches!(x, Value::String(s) if s.as_str() == *e));
    if ok { Ok(()) } else { bail!("expected {expected:?}, got {v:?}") }
}

fn assert_nested_i64s(v: &Value, expected: &[&[i64]]) -> Result<()> {
    let Value::Array(outer) = v else { bail!("not an array: {v:?}") };
    if outer.len() != expected.len() {
        bail!("outer len {} != {}, got {v:?}", outer.len(), expected.len());
    }
    for (inner, exp) in outer.iter().zip(expected) {
        assert_i64s(inner, exp)?;
    }
    Ok(())
}

// Nested HOFs: an inner callback referencing a grandparent capture (a
// binding outside both HOFs) resolves.

#[tokio::test(flavor = "current_thread")]
async fn nested_hof_grandparent_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| x + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[101], &[101]])
}

/// The inner callback captures both the outer element `y` and a
/// grandparent `n`.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_capture_element_and_grandparent() -> Result<()> {
    let v = load_and_await(
        "let n = 5; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| x + y + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[7], &[8]])
}

/// A nested `fold` referencing a grandparent capture.
#[tokio::test(flavor = "current_thread")]
async fn nested_fold_grandparent_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; \
         array::map([1, 2], |y: i64| \
           array::fold([1], 0, |acc: i64, x: i64| acc + x + n))",
    )
    .await?;
    assert_i64s(&v, &[101, 101])
}

/// The grandparent-capture nest under the pure node-walk.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_grandparent_capture_node_walk() -> Result<()> {
    let v = load_node_walked(
        "let n = 100; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| x + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[101], &[101]])
}

// A function-typed grandparent capture called from a nested HOF's
// inner callback resolves.

#[tokio::test(flavor = "current_thread")]
async fn nested_hof_function_capture() -> Result<()> {
    let v = load_and_await(
        "let f = |z: i64| z * 2; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[2], &[2]])
}

/// A function capture mixed with a value capture.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_function_and_value_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; let f = |z: i64| z * 2; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x) + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[102], &[102]])
}

/// A nested anonymous lambda call capturing a grandparent.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_anon_lambda_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 5; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| (|z: i64| z + n)(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[6], &[6]])
}

/// The function-typed grandparent capture under the pure node-walk.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_function_capture_node_walk() -> Result<()> {
    let v = load_node_walked(
        "let f = |z: i64| z * 2; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[2], &[2]])
}

// Captures in `StructWith.source` and labeled-arg default positions
// inside a nested HOF resolve.

/// `{base with a: x}` whose `base` is a grandparent capture.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_structwith_source_capture() -> Result<()> {
    let v = load_and_await(
        "let base = {a: 1, b: 9}; \
         array::map([1, 2], |y: i64| \
           array::map([10], |x: i64| { let r = {base with a: x}; r.b }))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[9], &[9]])
}

/// A labeled-arg default that captures a grandparent (`#off = n`).
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_labeled_default_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; let g = |#off: i64 = n, x: i64| x + off; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| g(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[101], &[101]])
}

/// `Add`/`Mul` carrying a capture (`x*k + k`).
#[tokio::test(flavor = "current_thread")]
async fn impure_arith_capture() -> Result<()> {
    assert_i64s(&impure_map("x * k + k").await?, &[6, 9, 12, 15])
}

/// `Select` (single arm + wildcard) on the element, returning a capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_select_capture() -> Result<()> {
    assert_i64s(&impure_map("select x { 1 => k, n => n * k }").await?, &[3, 6, 9, 12])
}

/// `Select` with TWO arms binding the SAME name (`a`) — the transient
/// name-map case; each arm re-mints `a` and its body must resolve to the
/// fresh id.
#[tokio::test(flavor = "current_thread")]
async fn impure_select_same_name() -> Result<()> {
    assert_i64s(
        &impure_map("select x { i64 as a if a > k => a + k, i64 as a => a * k }").await?,
        &[3, 6, 9, 7],
    )
}

/// Comparison (`Gt`) + capture in the scrutinee.
#[tokio::test(flavor = "current_thread")]
async fn impure_compare_capture() -> Result<()> {
    assert_i64s(
        &impure_map("select x > k { true => k * 10, false => x }").await?,
        &[1, 2, 3, 30],
    )
}

/// `Or` + `Eq` + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_boolops_capture() -> Result<()> {
    assert_i64s(
        &impure_map("select (x > k) || (x == 1) { true => 1, false => 0 }").await?,
        &[1, 0, 0, 1],
    )
}

/// `Not` + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_not_capture() -> Result<()> {
    assert_i64s(
        &impure_map("select !(x > k) { true => x, false => k }").await?,
        &[1, 2, 3, 3],
    )
}

/// `Tuple` producer + `TupleRef` accessor, both touching a capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_tuple_accessor_capture() -> Result<()> {
    assert_i64s(&impure_map("let t = (x, k); t.0 + t.1").await?, &[4, 5, 6, 7])
}

/// `Struct` producer + `StructRef` accessor + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_struct_accessor_capture() -> Result<()> {
    assert_i64s(&impure_map("let s = {a: x, b: k}; s.a * s.b").await?, &[3, 6, 9, 12])
}

/// `Array` producer + `ArrayRef` accessor + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_array_accessor_capture() -> Result<()> {
    assert_i64s(&impure_map("let a2 = [x, k, x + k]; a2[2]").await?, &[4, 5, 6, 7])
}

/// `Variant` producer + `Select` destructure of it + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_variant_capture() -> Result<()> {
    assert_i64s(
        &impure_map("let v = `Pair(x, k); select v { `Pair(a, b) => a + b }").await?,
        &[4, 5, 6, 7],
    )
}

/// `Map` producer + `MapRef` access + capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_map_accessor_capture() -> Result<()> {
    assert_i64s(
        &impure_map("let m = {\"a\" => x, \"b\" => k}; m{\"b\"}").await?,
        &[3, 3, 3, 3],
    )
}

/// `StringInterpolate` carrying both element and capture.
#[tokio::test(flavor = "current_thread")]
async fn impure_string_capture() -> Result<()> {
    assert_strs(&impure_map("\"[x]:[k]\"").await?, &["1:3", "2:3", "3:3", "4:3"])
}

/// Nested: `Tuple` + `Add`/`Mul` + `TupleRef` + `StringInterpolate`.
#[tokio::test(flavor = "current_thread")]
async fn impure_nested_capture() -> Result<()> {
    assert_strs(
        &impure_map("let p = (x * k, x + k); \"[p.0]/[p.1]\"").await?,
        &["3/4", "6/5", "9/6", "12/7"],
    )
}

/// A `select` with an arm binding (`n =>` catch-all) as a `let` value
/// inside an impure callback.
#[tokio::test(flavor = "current_thread")]
async fn impure_select_let_bound() -> Result<()> {
    assert_i64s(
        &impure_map("let r = select x { 1 => k, n => n * k }; r").await?,
        &[3, 6, 9, 12],
    )
}

/// The same body as a pure block-bodied callback.
async fn region_map(body: &str) -> Result<Value> {
    let prog = format!("let k = 3; array::map([1, 2, 3, 4], |x: i64| {{ {body} }})");
    load_and_await(&prog).await
}

#[tokio::test(flavor = "current_thread")]
async fn region_select_let_bound() -> Result<()> {
    assert_i64s(
        &region_map("let r = select x { 1 => k, n => n * k }; r").await?,
        &[3, 6, 9, 12],
    )
}

// Fused select-with-binding through the pure `pure_map` harness, which
// fuses the select into the kernel; asserts the value and that a kernel
// ran.

#[cfg(debug_assertions)]
async fn pure_select_value_and_fusion(body: &str) -> Result<(Value, u64)> {
    let (v, (f, _)) = load_and_count(&pure_map_program(body)).await?;
    Ok((v, f))
}

/// Catch-all binding `n =>` binds the scrutinee. `[3,6,9,12]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_catch_all() -> Result<()> {
    let (v, f) = pure_select_value_and_fusion("select x { 1 => k, n => n * k }").await?;
    assert!(f > 0, "select-with-binding should fuse, FUSION=0");
    assert_i64s(&v, &[3, 6, 9, 12])
}

/// Typed capture `i64 as n =>`: `[3,6,9,12]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_typed_capture() -> Result<()> {
    let (v, f) =
        pure_select_value_and_fusion("select x { 1 => k, i64 as n => n * k }").await?;
    assert!(f > 0, "typed-capture select should fuse, FUSION=0");
    assert_i64s(&v, &[3, 6, 9, 12])
}

/// Typed capture used in a guard and the body: `[3,6,9,7]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_guard_capture() -> Result<()> {
    let (v, f) = pure_select_value_and_fusion(
        "select x { i64 as a if a > k => a + k, i64 as a => a * k }",
    )
    .await?;
    assert!(f > 0, "guarded-capture select should fuse, FUSION=0");
    assert_i64s(&v, &[3, 6, 9, 7])
}

/// A binding select in arithmetic position: `[4,7,10,13]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_arith_wrapped() -> Result<()> {
    let (v, f) =
        pure_select_value_and_fusion("1 + (select x { 1 => k, n => n * k })").await?;
    assert!(f > 0, "arith-wrapped binding select should fuse, FUSION=0");
    assert_i64s(&v, &[4, 7, 10, 13])
}

/// A non-idempotent scrutinee (`rand()`) is evaluated exactly once:
/// `n == n` is true on the fused path.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_scrutinee_evaluated_once() -> Result<()> {
    let (v, (fused, _)) = load_and_count(
        "select rand::rand(#start: 0, #end: 1000000, #clock: 1) \
         { n => n == n }",
    )
    .await?;
    assert_eq!(v, Value::Bool(true));
    assert!(fused > 0, "rand-scrutinee select should fuse (the dup bug was fusion-only)");
    Ok(())
}

/// A non-Local scrutinee `x + 1` bound to a temp and referenced three
/// times: `[6,9,12,15]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_stabilize_multiref() -> Result<()> {
    let (v, f) =
        pure_select_value_and_fusion("select (x + 1) { m => m + m + m }").await?;
    assert!(f > 0, "stabilized-scrutinee select should fuse, FUSION=0");
    assert_i64s(&v, &[6, 9, 12, 15])
}

/// Same with subtraction: `n - n` is 0.
#[tokio::test(flavor = "current_thread")]
async fn fused_select_scrutinee_once_subtract() -> Result<()> {
    let v = load_and_await(
        "select rand::rand(#start: 1, #end: 1000000, #clock: 1) \
         { n => n - n }",
    )
    .await?;
    assert_eq!(v, Value::I64(0));
    Ok(())
}

/// A variant payload bind whose name shadows a kernel input reads the
/// payload, not the input: `[14,15,16,17]`.
#[tokio::test(flavor = "current_thread")]
async fn fused_variant_payload_shadow() -> Result<()> {
    let v = load_and_await(
        "let k = 3; \
         array::map([1, 2, 3, 4], |x: i64| { \
           let v = `Pair(x + 10, k); \
           select v { `Pair(x, b) => x + b } })",
    )
    .await?;
    assert_i64s(&v, &[14, 15, 16, 17])
}

#[tokio::test(flavor = "current_thread")]
async fn shadow_arm_binding_outer_ref() -> Result<()> {
    let v = load_and_await(
        "let n = 100; \
         array::map([1, 2, 3, 4], |x: i64| select x { 1 => n, n => n * 2 })",
    )
    .await?;
    assert_i64s(&v, &[100, 4, 6, 8])
}

/// The same under the pure node-walk: `[100,4,6,8]`.
#[tokio::test(flavor = "current_thread")]
async fn shadow_arm_binding_node_walk() -> Result<()> {
    let v = load_node_walked(
        "let n = 100; \
         array::map([1, 2, 3, 4], |x: i64| select x { 1 => n, n => n * 2 })",
    )
    .await?;
    assert_i64s(&v, &[100, 4, 6, 8])
}

// Env accounting: every per-slot grow mints bindings and every shrink
// must reverse them. Drives an impure HOF array up and down and asserts
// the registries return to the same size at the bottom of every cycle.

/// Drain `rx` until the map at `eid` emits an array of `target` length;
/// times out after 5s.
async fn await_map_len(rx: &mut Events, eid: ExprId, target: usize) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        if let Value::Array(a) = next_update(rx, eid, deadline).await?
            && a.len() == target
        {
            return Ok(());
        }
    }
}

/// `Value::Array([1, 2, …, n])`.
fn iota(n: i64) -> Value {
    let v: Vec<Value> = (1..=n).map(Value::I64).collect();
    Value::Array(netidx_value::ValArray::from(v))
}

#[tokio::test(flavor = "current_thread")]
async fn env_accounting_grow_shrink() -> Result<()> {
    use graphix_compiler::{Scope, expr::ModPath};

    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;

    // Keep `_first` alive so its Delete does not race the second compile.
    // `throttle(x)` is the async fusion boundary that forces the per-slot
    // node-walk residue this test measures.
    let _first = ctx.rt.compile(ArcStr::from("let arr: Array<i64> = [];")).await?;
    let res = ctx
        .rt
        .compile(ArcStr::from("array::map(arr, |x| { let v = throttle(x) * 2 + 1; v })"))
        .await?;
    let eid = res.exprs[0].id;

    // Resolve `arr`'s BindId from the env; a compiled Ref would itself
    // mint a binding and skew the baseline.
    let env = ctx.rt.get_env().await?;
    let scope = Scope::root();
    let arr_id = env
        .lookup_bind(&scope.lexical, &ModPath::from_iter(["arr"]))?
        .ok_or_else(|| anyhow::anyhow!("arr not in scope"))?
        .1
        .id;

    const N: i64 = 4;
    const CYCLES: usize = 4;

    let mut bottoms = Vec::new();
    let mut peak = None;
    for _ in 0..CYCLES {
        ctx.rt.set(arr_id, iota(N))?;
        await_map_len(&mut rx, eid, N as usize).await?;
        peak = Some(ctx.rt.env_stats().await?);
        ctx.rt.set(
            arr_id,
            Value::Array(netidx_value::ValArray::from_iter_exact(std::iter::empty())),
        )?;
        await_map_len(&mut rx, eid, 0).await?;
        bottoms.push(ctx.rt.env_stats().await?);
    }

    ctx.shutdown().await;

    // Every bottom-of-cycle snapshot (0 slots) must be identical.
    let base = bottoms[0];
    for (i, b) in bottoms.iter().enumerate() {
        if *b != base {
            bail!(
                "env-accounting leak: cycle {i} bottom {b:?} != baseline \
                 {base:?} — a slot's instance minted bindings/refs that \
                 Slot::delete did not reverse. Full series: {bottoms:?}"
            );
        }
    }

    // The peak (N slots) must exceed the bottom, or the invariant holds
    // trivially.
    let peak = peak.unwrap();
    if !(peak.by_id_len > base.by_id_len) {
        bail!(
            "env-accounting test is vacuous: peak {peak:?} did not exceed \
             bottom {base:?} — per-slot bindings aren't being minted, so \
             this wouldn't catch a leak"
        );
    }
    Ok(())
}

// A statement that builds and then fails its typecheck releases what it
// registered: the REPL keeps running after it.
#[tokio::test(flavor = "current_thread")]
async fn failed_statement_releases_refs() -> Result<()> {
    let (tx, _rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let _x = ctx.rt.compile(ArcStr::from("let x = 1;")).await?;
    let base = ctx.rt.env_stats().await?;
    for _ in 0..4 {
        let r = ctx.rt.compile(ArcStr::from(r#"{ let y = x + 1; y + "a" }"#)).await;
        if r.is_ok() {
            bail!("the ill-typed statement compiled")
        }
    }
    let after = ctx.rt.env_stats().await?;
    ctx.shutdown().await;
    if after != base {
        bail!("failed statements leaked: {base:?} -> {after:?}")
    }
    Ok(())
}

// A `~` that paid banked debt through its private variable takes that
// variable's store entry with it when its slot is deleted: two triggers
// bank before `v` arrives, one is answered, the other is paid.
#[tokio::test(flavor = "current_thread")]
async fn deleted_sample_releases_store() -> Result<()> {
    use graphix_compiler::{Scope, expr::ModPath};

    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let _first = ctx.rt.compile(ArcStr::from("let arr: Array<i64> = [];")).await?;
    let res = ctx
        .rt
        .compile(ArcStr::from(
            "array::map(arr, |x| { let v = never(); v <- x; let t = never(); \
             t <- x; any(x, t) ~ v })",
        ))
        .await?;
    let eid = res.exprs[0].id;
    let env = ctx.rt.get_env().await?;
    let arr_id = env
        .lookup_bind(&Scope::root().lexical, &ModPath::from_iter(["arr"]))?
        .ok_or_else(|| anyhow::anyhow!("arr not in scope"))?
        .1
        .id;
    let mut bottoms = Vec::new();
    for _ in 0..3 {
        ctx.rt.set(arr_id, iota(4))?;
        await_map_len(&mut rx, eid, 4).await?;
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        ctx.rt.set(
            arr_id,
            Value::Array(netidx_value::ValArray::from_iter_exact(std::iter::empty())),
        )?;
        await_map_len(&mut rx, eid, 0).await?;
        bottoms.push(ctx.rt.env_stats().await?.store_len);
    }
    ctx.shutdown().await;
    if bottoms.iter().any(|b| *b != bottoms[0]) {
        bail!("deleted slots leaked store entries: {bottoms:?}")
    }
    Ok(())
}

// Env/reference nodes (TryCatch, Sample, ByRef/Deref, ...) inside an
// impure callback that captures the outer `k`: each slot must resolve
// the capture without contaminating a sibling slot.

/// A catch handler that fires and captures both the element `x` and
/// the outer `k`, driving `res <- x + k`: `[4,5,6,7]`.
#[tokio::test(flavor = "current_thread")]
async fn impure_trycatch_catch_capture() -> Result<()> {
    assert_i64s(
        &impure_map(
            "let res = never(); \
             catch(e) res <- (x + k); \
             (0 /? 0)?; \
             res",
        )
        .await?,
        &[4, 5, 6, 7],
    )
}

/// A covered block that captures `k` and does not error: `x / k`.
#[tokio::test(flavor = "current_thread")]
async fn impure_trycatch_try_capture() -> Result<()> {
    assert_i64s(&impure_map("{ catch(e) -1; x / k }").await?, &[0, 0, 1, 1])
}

/// `x ~ k` emits `k` when the element fires: `[3,3,3,3]`.
#[tokio::test(flavor = "current_thread")]
async fn impure_sample_capture() -> Result<()> {
    assert_i64s(&impure_map("x ~ k").await?, &[3, 3, 3, 3])
}

/// ByRef + Deref capturing `k`: `*r + x` is `[4,5,6,7]`.
#[tokio::test(flavor = "current_thread")]
async fn impure_byref_deref_capture() -> Result<()> {
    assert_i64s(&impure_map("let r = &k; *r + x").await?, &[4, 5, 6, 7])
}

// Proptest swarm: random total i64 callback bodies over `x` and `k`;
// the impure callback must agree with the pure one.

/// A random single-expression i64 body over `x`, `k`, and small literals.
fn body_strategy() -> impl proptest::strategy::Strategy<Value = String> {
    use proptest::prelude::*;
    let leaf = prop_oneof![
        Just("x".to_string()),
        Just("k".to_string()),
        (0i64..5).prop_map(|n| n.to_string()),
    ];
    leaf.prop_recursive(4, 48, 8, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({a} + {b})")),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({a} - {b})")),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({a} * {b})")),
            // literal-pattern select
            (inner.clone(), inner.clone(), inner.clone())
                .prop_map(|(a, b, c)| format!("(select {a} {{ 0 => {b}, _ => {c} }})")),
            // arm-binding select; `q` is fresh so no sibling arm reads it
            (inner.clone(), inner.clone(), inner.clone()).prop_map(|(a, b, c)| format!(
                "(select {a} {{ 0 => {b}, q => (q + {c}) }})"
            )),
            // tuple + accessor needs a binding (`(a,b).0` is a parse error)
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let p = ({a}, {b}); p.0 }})")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let p = ({a}, {b}); p.1 }})")),
            // struct + accessor
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let s = {{f: {a}, g: {b}}}; s.f }})")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let s = {{f: {a}, g: {b}}}; s.g }})")),
        ]
    })
}

proptest::proptest! {
    #![proptest_config(proptest::prelude::ProptestConfig {
        cases: 128,
        max_shrink_iters: 256,
        ..proptest::prelude::ProptestConfig::default()
    })]
    #[test]
    fn impure_matches_pure(body in body_strategy()) {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let (reference, cloned) = rt
            .block_on(async {
                let r = pure_map(&body).await?;
                let c = impure_map(&body).await?;
                anyhow::Ok((r, c))
            })
            .map_err(|e| {
                proptest::test_runner::TestCaseError::fail(format!(
                    "runtime error for body `{body}`: {e}"
                ))
            })?;
        proptest::prop_assert_eq!(
            &reference,
            &cloned,
            "body `{}`: reference {:?} != clone {:?}",
            body,
            reference,
            cloned
        );
    }
}

// `NodeShape` asserts on the compiled artifact: `foo * 6` with `foo`
// external fuses into one kernel with a scalar input.
#[tokio::test(flavor = "current_thread")]
async fn node_shape_external_scalar() -> Result<()> {
    use graphix_compiler::{
        fusion::kernel_abi::{PrimType, prim_type},
        node_shape::{KernelMatcher, NodeShape},
    };

    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let _first = ctx.rt.compile(ArcStr::from("let foo = 7;")).await?;
    let res = ctx.rt.compile(ArcStr::from("foo * 6")).await?;
    let eid = res.exprs[0].id;

    let spec = NodeShape::fused(
        KernelMatcher::new().returns(prim_type(PrimType::I64)).params(&["foo"]),
    );
    ctx.rt.match_shape(eid, spec).await?;

    // A wrong criterion must produce a mismatch.
    let bad = NodeShape::fused(KernelMatcher::new().params(&["nope"]));
    let err = ctx.rt.match_shape(eid, bad).await;
    assert!(err.is_err(), "matcher should reject a wrong spec, but passed");

    // Asserting a plain node on a fused root must fail too.
    let bad2 = NodeShape::node("Block");
    assert!(
        ctx.rt.match_shape(eid, bad2).await.is_err(),
        "root is Fused, a Block spec should not match"
    );

    ctx.shutdown().await;
    Ok(())
}

// A node is matched by its own kind's name and refused by another's
// (fusion off, so the root is the node itself, not a kernel).
#[tokio::test(flavor = "current_thread")]
async fn node_shape_matches_by_kind_name() -> Result<()> {
    use graphix_compiler::{CFlag, node_shape::NodeShape};
    use graphix_package_core::testing::init_with_flags_and_setup;

    let (tx, _rx) = mpsc::channel(10);
    let ctx = init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        CFlag::FusionDisabled.into(),
        |_| {},
    )
    .await?;
    let res = ctx.rt.compile(ArcStr::from(r#""a""#)).await?;
    let eid = res.exprs[0].id;
    ctx.rt.match_shape(eid, NodeShape::node("Constant")).await?;
    assert!(
        ctx.rt.match_shape(eid, NodeShape::node("Other")).await.is_err(),
        "a Constant matched the kind Other"
    );
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_just_bind_no_output() -> Result<()> {
    // A file whose last statement is a Bind emits nothing; `load()`
    // reports `output: false`.
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let res = ctx.rt.load(Source::Internal(ArcStr::from("let x = 5"))).await?;
    let comp = res.exprs.first().ok_or_else(|| anyhow::anyhow!("no comp expr"))?;
    assert!(!comp.output, "let-only file should have output=false");
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn effect_rejection_preserves_native_children() -> Result<()> {
    for code in [
        "{ let x = 1; let c = count(x); #[native] c + 1 }",
        "{ let f = |x: i64| { let c = count(x); #[native] c + 1 }; f(1) }",
        "{ let x = 1; let r = &x; let v = *r; #[native] v + 1 }",
    ] {
        let (tx, _rx) = mpsc::channel(10);
        let ctx = init(tx).await?;
        let before = ctx.rt.fusion_stats().await?;
        ctx.rt.compile(ArcStr::from(code)).await?;
        let stats = ctx.rt.fusion_stats().await?;
        assert!(
            stats.rejected_before_emit > before.rejected_before_emit,
            "{code}: {stats:?}"
        );
        assert!(stats.fused > before.fused, "{code}: {stats:?}");
        ctx.shutdown().await;
    }
    Ok(())
}

#[tokio::test]
async fn effect_rejection_reports_native_blocker() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let error = ctx
        .rt
        .compile(arcstr::literal!("#[native] count(1)"))
        .await
        .expect_err("stateful builtin must reject #[native]");
    let message = format!("{error:#}");
    assert!(message.contains("count(1)"), "{message}");
    assert!(message.contains("fast-call entry"), "{message}");
    ctx.shutdown().await;
    Ok(())
}

// A `#[native]` call names why its callee has no kernel, the callee one
// call deeper included.
#[tokio::test]
async fn native_reports_callee_cause() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    for prog in [
        "{ let a = 5; let g = |x: i64| { print(x); x + 1 }; #[native] g(a) }",
        "{ let a = 5; let g = |x: i64| { print(x); x + 1 }; \
         let h = |x: i64| g(x) * 3; #[native] h(a) }",
    ] {
        let error = ctx
            .rt
            .compile(ArcStr::from(prog))
            .await
            .expect_err("print must reject #[native]");
        let message = format!("{error:#}");
        assert!(message.contains("`print` has no fast-call entry"), "{prog}: {message}");
    }
    ctx.shutdown().await;
    Ok(())
}

// Of two refusals, the one reported is the first in source order, as the
// serial walk's is.
#[tokio::test]
async fn native_reports_first_refusal() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let error = ctx
        .rt
        .compile(arcstr::literal!(
            "{ let a = 5; let g = |x: i64| { print(x); x + 1 }; \
             let r = { #[native] g(a); #[native] once(a); 0 }; r }"
        ))
        .await
        .expect_err("both parts must reject #[native]");
    let message = format!("{error:#}");
    assert!(message.contains("g(a)"), "{message}");
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn native_block_discards_unused_reference() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    ctx.rt
        .compile(arcstr::literal!("#[native] { let x = 1; let unused = &x; x + 1 }"))
        .await?;
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn root_rejection_preserves_binding_values_and_dead_statements() -> Result<()> {
    assert_eq!(
        load_and_await(
            "let x = #[native] 1 + 2; \
             #[native] { let unused = never<i64>(x); x * 2 }"
        )
        .await?,
        Value::I64(6)
    );
    Ok(())
}

// A `never()` arm is a bottom production of the merge shape: the select
// fuses, fires only with the scrutinee, and never writes a connect.
const NEVER_ARM_FUSES: &str = r#"
{
  let x = 0;
  x <- select x { n if n < 4 => n + 1, _ => never() };
  let s = #[native] select x { 1 | 3 => x * 10, _ => never() };
  let n = 0;
  n <- s ~ (n + 1);
  let last = 0;
  last <- s;
  let t = #[native] select x { 2 => "two", _ => never<string>(x) };
  let k = 0;
  k <- t ~ (k + 1);
  select x { 4 => n * 1000 + k * 100 + last, _ => never() }
}
"#;

run!(never_arm_fuses, NEVER_ARM_FUSES, |v: Result<&Value>| match v {
    Ok(Value::I64(2130)) => true,
    _ => false,
}; FuseExpect::Jit);

// An arm whose body is bottom-TYPED but not `never()` still runs: `g`
// returns the connect's bottom, so the arm's value is a bottom of the
// merge shape, but the call must reach `g` and land the write. The
// select node-walks (the callee has no kernel); the guard below fuses.
const BOTTOM_TYPED_ARM_RUNS: &str = r#"
{
  let z = 0;
  let g = |y| z <- y;
  let w = 5;
  select w { i64:0 => 1, _ => g(w) };
  select z { 0 => never(), n => n }
}
"#;

run!(bottom_typed_arm_runs, BOTTOM_TYPED_ARM_RUNS, |v: Result<&Value>| match v {
    Ok(Value::I64(5)) => true,
    _ => false,
}, timeout: 5; FuseExpect::Jit);

// A `never(args..)` arm is a standing bottom whatever its args do, but
// the args are consumed: the handler-ful `?` inside one delivers its
// raise (its select node-walks: the raise is an edge the arm may owe
// to the select's fire tracker).
const NEVER_ARM_ARGS_RAISE: &str = r#"
{
  let x = array::iter([1, 2, 3]);
  let n = 0;
  let v = { catch(e) n <- e ~ n + 1; select x { 2 => never(error(`E(x))?), _ => x } };
  select count(x) { 3 => n, _ => never() }
}
"#;

run!(never_arm_args_raise, NEVER_ARM_ARGS_RAISE, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}, timeout: 5; FuseExpect::Jit);

// The error derives from `n`, a let whose init fire no selected arm
// read: the tracker re-delivers it when the arm is entered and the
// raise lands. A kernel reads `n` standing and would lose the raise.
const ARM_RAISE_STANDING_INPUT: &str = r#"
{
  let x = array::iter([1, 2, 3]);
  let n = 0;
  { catch(e) n <- e ~ n + 1; select x { 2 => never(error(`E(n))?), _ => x } };
  select count(x) { 3 => n, _ => never() }
}
"#;

run!(arm_raise_standing_input, ARM_RAISE_STANDING_INPUT, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}, timeout: 5; FuseExpect::Jit);

// The soak's shape: a call into a connecting body inside a never arm of
// a collection callback. The call must reach `g` and land the write;
// the map region node-walks (the callee has no kernel), the guard fuses.
const NEVER_ARM_ARGS_EFFECT: &str = r#"
{
  let z = 0;
  let g = |y| z <- y;
  array::map([1, null], |v| select v { i64 as n => never(g(n)), null as _ => true });
  select z { 0 => never(), n => n }
}
"#;

run!(never_arm_args_effect, NEVER_ARM_ARGS_EFFECT, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}, timeout: 5; FuseExpect::Jit);

// A tail call whose new value for one composite formal is another
// formal's old value: the rebind owns every new value before an old one
// drops.
const TAIL_REBIND_SWAPPED_COMPOSITES: &str = r#"
{
  let rec f = |n: i64, a: Array<i64>, b: Array<i64>| -> Array<i64> select n {
    0 => b,
    n => f(n - 1, [n, n, n, n], a)
  };
  f(3, [0], [1])
}
"#;

run!(tail_rebind_swapped_composites, TAIL_REBIND_SWAPPED_COMPOSITES, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.iter().all(|v| *v == Value::I64(2)) && a.len() == 4,
        _ => false,
    }
}; FuseExpect::Jit);

// A let shadowing a formal is a body local like any other: the rebind
// drops it by position, and the formal's new value is its clone.
const TAIL_REBIND_SHADOWED_FORMAL: &str = r#"
{
  let rec f = |n: i64, a: Array<i64>| -> Array<i64> {
    let a = [n, n, n];
    select n { 0 => a, n => f(n - 1, a) }
  };
  f(3, [7])
}
"#;

run!(tail_rebind_shadowed_formal, TAIL_REBIND_SHADOWED_FORMAL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.iter().all(|v| *v == Value::I64(0)) && a.len() == 3,
        _ => false,
    }
}; FuseExpect::Jit);

// A string read into a value-shaped formal is owned (a string read
// clones), in a lambda call and in a tail rebind.
const STRING_READ_INTO_VALUE_FORMAL: &str = r#"
{
  let g = |v: [string, null]| -> i64 select v { null as _ => 0, _ => 1 };
  let rec f = |n: i64, acc: i64, last: [string, null]| -> [string, null] {
    let s = "s[n]";
    select n { 0 => select acc { 5 => last, _ => null }, n => f(n - 1, acc + g(s), s) }
  };
  f(5, 0, null)
}
"#;

run!(string_read_into_value_formal, STRING_READ_INTO_VALUE_FORMAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "s1")
}; FuseExpect::Jit);

// Two constants equal under `==` but not identical keep their own
// symbols: -0.0 is not 0.0.
const DISTINCT_ZERO_CONSTANTS: &str = r#"
{
  let k = 6;
  select k { 5 => {"a" => 0.0}, _ => {"a" => -0.0} }
}
"#;

run!(distinct_zero_constants, DISTINCT_ZERO_CONSTANTS, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) => matches!(
        m.get(&Value::from("a")),
        Some(Value::F64(x)) if *x == 0.0 && x.is_sign_negative()
    ),
    _ => false,
}; FuseExpect::Jit);

// A guarded nested tuple pattern: its leaf binds read through an
// interior pointer the guard prologue computes in straight-line code.
const NESTED_TUPLE_GUARD: &str = r#"
{
  let a = ((7, 1), 2);
  let limit = 5;
  #[native] select a { ((x, y), z) if x > limit => x + y + z, _ => 0 }
}
"#;

run!(nested_tuple_guard, NESTED_TUPLE_GUARD, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
}; FuseExpect::Jit);

// An owned composite scrutinee in tail position: every terminator drops
// it with the rest of the env.
const TAIL_SELECT_OWNED_SCRUTINEE: &str = r#"
{
  let rec f = |n: i64, acc: i64| -> i64 select (n, acc) {
    (0, a) => a,
    (m, a) => f(m - 1, a + m)
  };
  #[native] f(10, 0)
}
"#;

run!(tail_select_owned_scrutinee, TAIL_SELECT_OWNED_SCRUTINEE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(55)))
}; FuseExpect::Jit);

// A string arm widens to a value-shaped merge.
const STRING_ARM_VALUE_MERGE: &str = r#"
{
  let x = 0;
  #[native] select x { 0 => "a", _ => null }
}
"#;

run!(string_arm_value_merge, STRING_ARM_VALUE_MERGE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a")
}; FuseExpect::Jit);

// A recursive body whose activations each own a nested loop's slot
// chain, at alternating depth: shed activations free their chains.
const SHED_ACTIVATIONS_FREE_CHAINS: &str = r#"
{
  let rec f = |k: i64, a: Array<Array<i64>>| -> i64 select k {
    0 => 0,
    _ => array::len(array::map(a, |r| array::map(r, |x| x + k))) + f(k - 1, a)
  };
  let d = array::iter([20, 1, 20, 1, 3]);
  let r = f(d, [[1, 2], [3, 4]]);
  select count(r) { 5 => r, _ => never() }
}
"#;

run!(shed_activations_free_chains, SHED_ACTIVATIONS_FREE_CHAINS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(6)))
}; FuseExpect::Jit);

// A loop over calls to a recursive callee, at alternating length: a
// dropped slot's block frees the callee's activation tree.
const DROPPED_SLOTS_FREE_TREES: &str = r#"
{
  let rec f = |k: i64| -> i64 select k { 0 => 0, _ => k + f(k - 1) };
  let src = array::iter([[1, 2, 3, 4, 5, 6], [1], [4, 5, 6], [2]]);
  let r = array::map(src, |x| f(x));
  select count(r) { 4 => r, _ => never() }
}
"#;

run!(dropped_slots_free_trees, DROPPED_SLOTS_FREE_TREES, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 1 && a[0] == Value::I64(3))
}; FuseExpect::Jit);
// A float `%` is Rust's (`fmod`: the sign of the dividend) in both
// engines and fuses.
const FLOAT_REM_FUSES: &str = r#"
{
  let x = -7.5;
  let y = 2.;
  let z = f32:5.5;
  let w = f32:2.;
  let a = #[native] (x % y);
  let b = #[native] (z % w);
  (a, b)
}
"#;

run!(float_rem_fuses, FLOAT_REM_FUSES, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a[0] == Value::F64(-1.5) && a[1] == Value::F32(1.5),
    _ => false,
}; FuseExpect::Jit);

// Region inputs resolve by BindId, so a capture and an argument of one
// basename are two inputs.
const SHADOWED_INPUTS_FUSE: &str = r#"
{
  let x = [1, 2, 3];
  let f = |y: i64| y + array::len(x);
  let x = [4, 5];
  let r = #[native] f(array::len(x));
  r
}
"#;

run!(shadowed_inputs_fuse, SHADOWED_INPUTS_FUSE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(5))
); FuseExpect::Jit);

// A statically resolved call of a lambda literal has a kernel.
const LAMBDA_LITERAL_CALL_FUSES: &str = r#"
{
  let n = 3;
  let r = #[native] ((|x: i64| x * 2 + 1)(n));
  r
}
"#;

run!(lambda_literal_call_fuses, LAMBDA_LITERAL_CALL_FUSES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(7))
); FuseExpect::Jit);

// A call that is the argument of a node fusion does not descend into
// as a whole (a builtin without a fast call) is its own region: the
// kernel whose only input is `m` is the loop's call.
const LOOP_UNDER_A_NODE_WALKED_BUILTIN: &str = r#"
{
  let m = 10;
  let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) };
  count(f(m, 0))
}
"#;

run!(loop_under_a_node_walked_builtin, LOOP_UNDER_A_NODE_WALKED_BUILTIN, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); FuseExpect::Jit; shape: graphix_compiler::node_shape::NodeShape::contains_fused(
    graphix_compiler::node_shape::KernelMatcher::new().params(&["m"])
));

// The same in every position a failed region descends: an array element
// beside a node-walked sibling, an operand, a sample, a connect.
const LOOP_BESIDE_NODE_WALKED_SIBLINGS: &str = r#"
{
  let m = 10;
  let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) };
  let y = never();
  y <- #[native] f(m, 0);
  let a = [#[native] f(m, 0), count(m)];
  let b = (#[native] f(m, 0)) + count(m);
  let c = m ~ (#[native] f(m, 0));
  select y { y => (a, b, c, y) }
}
"#;

run!(loop_beside_node_walked_siblings, LOOP_BESIDE_NODE_WALKED_SIBLINGS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(
        &t[..],
        [Value::Array(a), Value::I64(56), Value::I64(55), Value::I64(55)]
            if matches!(&a[..], [Value::I64(55), Value::I64(1)])
    ),
    _ => false,
}; FuseExpect::Jit);

// A local `let rec` fuses wherever its block sits: an operand, an array
// element, a collection callback. A kernel calls a local lambda
// statically, so the binding emits nothing.
const LOCAL_LET_REC_IN_NESTED_BLOCKS: &str = r#"
{
  let a = { let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) }; #[native] f(10, 0) } + 1;
  let b = [1, { let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) }; #[native] f(10, 0) }];
  let c = array::init(2, |x| { let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) }; #[native] f(10, x) });
  (a, b, c)
}
"#;

run!(local_let_rec_in_nested_blocks, LOCAL_LET_REC_IN_NESTED_BLOCKS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::I64(56), Value::Array(b), Value::Array(c)] => {
            matches!(&b[..], [Value::I64(1), Value::I64(55)])
                && matches!(&c[..], [Value::I64(55), Value::I64(56)])
        }
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// A lambda bound by a `let` inside a loop body does not keep the loop
// out of native code.
const LOCAL_LAMBDA_IN_A_LOOP_BODY: &str = r#"
{
  let rec g = |n: i64| -> i64 select n { 0 => { let h = |k: i64| k + 1; h(n) }, _ => g(n - 1) };
  #[native] g(3)
}
"#;

run!(local_lambda_in_a_loop_body, LOCAL_LAMBDA_IN_A_LOOP_BODY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); FuseExpect::Jit);

// A lambda call whose arguments do not all fuse (an effect, a stateful
// builtin) fuses with each such argument as a feeder: the node-walk runs
// it and the kernel reads its production as an input.
const CALL_FED_BY_NODE_WALKED_ARGS: &str = r#"
{
  let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => f(n - 1, acc + n) };
  let a = #[native] f(10, { let s = 0; s <- 1; s });
  let b = #[native] f(10, count(a));
  select count(a) { 2 => (a, b), _ => never() }
}
"#;

run!(call_fed_by_node_walked_args, CALL_FED_BY_NODE_WALKED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(56), Value::I64(57)]),
    _ => false,
}; FuseExpect::Jit);

// A select nested in tail position reads its outer arm's binds on every
// path: a tail jump in one inner arm drops them at run time only, and
// the sibling arm still reads them (the `filter_map` shape).
const NESTED_TAIL_SELECT_KEEPS_OUTER_BINDS: &str = r#"
{
  type L = [`C(i64, L), `N];
  let rec fm = |l: L| -> L select l {
    `N => `N,
    `C(x, rest) => select x > 1 { false => fm(rest), true => `C(x, fm(rest)) }
  };
  let r = #[native] fm(`C(1, `C(2, `C(3, `N))));
  r
}
"#;

run!(nested_tail_select_keeps_outer_binds, NESTED_TAIL_SELECT_KEEPS_OUTER_BINDS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(c), Value::I64(2), Value::Array(t)] if &**c == "C" => matches!(
            &t[..],
            [Value::String(c), Value::I64(3), Value::String(n)] if &**c == "C" && &**n == "N"
        ),
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// A nullable's non-scalar payload binds natively: a string, an array, a
// struct; the guard's masked bind of a null is a drop-safe default.
const NULLABLE_VALUE_BINDS: &str = r#"
{
  let s: [null, string] = "abc";
  let z: [null, string] = null;
  let a: [null, Array<i64>] = [1, 2, 3];
  let t: [null, {x: i64, y: string}] = {x: 3, y: "ab"};
  let b = #[native] select s { null as _ => 0, s => str::len(s) };
  let c = #[native] select z { null as _ => 10, s if str::len(s) > 1 => 1, _ => 2 };
  let d = #[native] select a { null as _ => 0, a => array::len(a) };
  let e = #[native] select t { null as _ => 0, t => t.x + str::len(t.y) };
  (b, c, d, e)
}
"#;

run!(nullable_value_binds, NULLABLE_VALUE_BINDS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => {
        matches!(&t[..], [Value::I64(3), Value::I64(10), Value::I64(3), Value::I64(5)])
    }
    _ => false,
}; FuseExpect::Jit);

// Slice patterns bind their rest, head and whole slice natively, as
// owned subslices; a guard's masked bind of a short array reads empty.
const SLICE_REST_BINDS: &str = r#"
{
  let rec g = |a: Array<i64>, acc: i64| -> i64 select a { [] => acc, [x, tail..] => g(tail, acc + x) };
  let a = [1, 2, 3];
  let p = #[native] g([1, 2, 3, 4], 0);
  let q = #[native] select a { [init.., l] => l * 10 + array::len(init), [] => 0 };
  let r = #[native] select a { all@ [x, ..] => x + array::len(all), [] => 0 };
  let s = #[native] select [5] { [x, rest..] if array::len(rest) > 0 => x, [x, ..] => x + 100, [] => 0 };
  (p, q, r, s)
}
"#;

run!(slice_rest_binds, SLICE_REST_BINDS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => {
        matches!(&t[..], [Value::I64(10), Value::I64(32), Value::I64(4), Value::I64(105)])
    }
    _ => false,
}; FuseExpect::Jit);

// A composite pattern binds non-scalar elements natively: strings and
// arrays out of arrays, tuples and structs.
const NON_SCALAR_ELEMENT_BINDS: &str = r#"
{
  let rec g = |a: Array<string>, acc: i64| -> i64 select a { [] => acc, [s, tail..] => g(tail, acc + str::len(s)) };
  let p = #[native] g(["ab", "c"], 0);
  let q = #[native] select (3, "abcd") { (n, s) => n + str::len(s) };
  let r = #[native] select {x: 3, y: "ab"} { {x, y} => x + str::len(y) };
  let s = #[native] select [[1, 2], [3]] { [a, b] => array::len(a) * 10 + array::len(b), _ => 0 };
  (p, q, r, s)
}
"#;

run!(non_scalar_element_binds, NON_SCALAR_ELEMENT_BINDS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => {
        matches!(&t[..], [Value::I64(3), Value::I64(7), Value::I64(5), Value::I64(21)])
    }
    _ => false,
}; FuseExpect::Jit);

// A primitive union is one two-word Value in a kernel: a loop may return
// one, and a select over one tests and binds its members natively.
const PRIMITIVE_UNION_VALUES: &str = r#"
{
  let rec g = |n: i64| select n { 0 => 1.5, 1 => 2, _ => g(n - 1) };
  let n = 0;
  let u = select n { 0 => null, 1 => 7, _ => 2.5 };
  let a = #[native] g(5);
  let b = #[native] g(0);
  let c = #[native] select g(0) { i64 as i => i, f => cast<i64>(f)$ + 10 };
  let d = #[native] select u { null as _ => 99, i64 as i => i, f64 as _ => 3 };
  (a, b, c, d)
}
"#;

run!(primitive_union_values, PRIMITIVE_UNION_VALUES, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => {
        matches!(&t[..], [Value::I64(2), Value::F64(b), Value::I64(11), Value::I64(99)] if *b == 1.5)
    }
    _ => false,
}; FuseExpect::Jit);

// A varint is a Value in a kernel: it passes through and casts natively;
// its arithmetic stays in the node-walk.
const VARINT_VALUES: &str = r#"
{
  let z = z64:-5;
  let n = 300;
  let a = #[native] cast<i64>(z)$ + 1;
  let b = #[native] cast<v64>(n)$;
  let c = #[native] select n { 300 => v32:7, _ => v32:0 };
  (a, b, c)
}
"#;

run!(varint_values, VARINT_VALUES, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(-4), Value::V64(300), Value::V32(7)]),
    _ => false,
}; FuseExpect::Jit);

// Over a result union, `error as _` tests the error tag and a later
// untested bind reads the success payload.
const RESULT_UNION_BINDS: &str = r#"
{
  let o: [Error<`E>, string] = "abc";
  let e: [Error<`E>, string] = error(`E);
  let a = #[native] select o { error as _ => 0, s => str::len(s) };
  let b = #[native] select e { error as _ => 10, s => str::len(s) };
  (a, b)
}
"#;

run!(result_union_binds, RESULT_UNION_BINDS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(3), Value::I64(10)]),
    _ => false,
}; FuseExpect::Jit);

// A lambda that captures a recursive lambda calls it statically: the
// capture takes no slot, so the caller fuses.
const CAPTURED_RECURSION_CALLS_STATICALLY: &str = r#"
{
  let rec build = |n: i64| -> i64 select n { 0 => 1, _ => 2 * build(n - 1) };
  let run = |i: i64| -> i64 build(i) + 1;
  let r = #[native] run(3);
  r
}
"#;

run!(captured_recursion_calls_statically, CAPTURED_RECURSION_CALLS_STATICALLY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
}; FuseExpect::Jit);

// A scalar literal matches over an option or a result by tag, then by
// payload.
const LITERAL_OVER_A_VALUE_SCRUTINEE: &str = r#"
{
  let f = |n: i64| select n { 0 => 100, _ => error(`E) };
  let o: [null, i64] = 5;
  let a = #[native] select f(0) { 100 => 1, _ => 2 };
  let b = #[native] select f(1) { 100 => 1, _ => 2 };
  let c = #[native] select o { 5 => 1, _ => 2 };
  (a, b, c)
}
"#;

run!(literal_over_a_value_scrutinee, LITERAL_OVER_A_VALUE_SCRUTINEE, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(1), Value::I64(2), Value::I64(1)]),
    _ => false,
}; FuseExpect::Jit);

// A varint is a two-word Value: its literal matches by its own tag, not
// its fixed-width prim's.
const LITERAL_OVER_A_VARINT: &str = r#"
{
  let z: z32 = z32:0;
  let v: v64 = v64:7;
  let a = #[native] select z { z32:0 => 1, _ => 2 };
  let b = #[native] select v { v64:7 => 1, _ => 2 };
  let c = #[native] select v { v64:0 => 1, _ => 2 };
  (a, b, c)
}
"#;

run!(literal_over_a_varint, LITERAL_OVER_A_VARINT, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(1), Value::I64(1), Value::I64(2)]),
    _ => false,
}; FuseExpect::Jit);

// Variant payload patterns nest natively: a variant in a payload, a
// literal in a payload, binds under both, a guard over a nested bind.
const NESTED_VARIANT_PATTERNS: &str = r#"
{
  type S = [`Connect, `Panel([`D, `Q])];
  type T = [`A(`B(i64, string)), `C];
  type L = [`K(i64, L), `N];
  let rec g = |l: L, acc: i64| -> i64 select l {
    `N => acc,
    `K(0, rest) => g(rest, acc + 100),
    `K(x, rest) => g(rest, acc + x)
  };
  let s: S = `Panel(`Q);
  let t: T = `A(`B(7, "xy"));
  let a = #[native] select s { `Connect => 0, `Panel(`Q) => 1, `Panel(`D) => 2 };
  let b = #[native] select t { `A(`B(n, y)) if n > 3 => n + str::len(y), `A(_) => 1, `C => 0 };
  let c = #[native] g(`K(1, `K(0, `K(3, `N))), 0);
  (a, b, c)
}
"#;

run!(nested_variant_patterns, NESTED_VARIANT_PATTERNS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(1), Value::I64(9), Value::I64(104)]),
    _ => false,
}; FuseExpect::Jit);

// A collection callback that does not fuse whole (a stateful builtin
// beside the loop) runs its fusing regions natively in every slot: the
// slots' instances take the kernels the prototype's fusion built. The
// slots are the only place a kernel can run.
const CALLBACK_REGIONS_FUSE_IN_SLOTS: &str = r#"
array::map([1, 2, 3], |x| {
  let rec lp = |n: i64, a: i64| -> i64 select n { 0 => a, _ => lp(n - 1, a + n) };
  let l = #[native] lp(100, x);
  l + count(x)
})
"#;

run!(callback_regions_fuse_in_slots, CALLBACK_REGIONS_FUSE_IN_SLOTS, |v: Result<&Value>| match v {
    Ok(Value::Array(m)) => {
        matches!(&m[..], [Value::I64(5052), Value::I64(5053), Value::I64(5054)])
    }
    _ => false,
}; FuseExpect::Jit);

const FOLD_CALLBACK_REGIONS_FUSE_IN_SLOTS: &str = r#"
array::fold([1, 2], 0, |acc, x| {
  let rec lp = |n: i64, a: i64| -> i64 select n { 0 => a, _ => lp(n - 1, a + n) };
  let l = #[native] lp(10, x);
  acc + l + count(x)
})
"#;

run!(fold_callback_regions_fuse_in_slots, FOLD_CALLBACK_REGIONS_FUSE_IN_SLOTS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(115))
); FuseExpect::Jit);

// A slot's call fed by a node-walked argument takes the prototype's
// kernel with the slot's own feeder.
const FED_CALLBACK_REGIONS_FUSE_IN_SLOTS: &str = r#"
array::map([1, 2, 3], |x| {
  let rec lp = |n: i64, a: i64| -> i64 select n { 0 => a, _ => lp(n - 1, a + n) };
  #[native] lp(10, count(x) + x)
})
"#;

run!(fed_callback_regions_fuse_in_slots, FED_CALLBACK_REGIONS_FUSE_IN_SLOTS, |v: Result<&Value>| match v {
    Ok(Value::Array(m)) => matches!(&m[..], [Value::I64(57), Value::I64(58), Value::I64(59)]),
    _ => false,
}; FuseExpect::Jit);

// A collection nested in a slot's callback shares its own prototype's
// kernels with its slots.
const NESTED_CALLBACK_REGIONS_FUSE_IN_SLOTS: &str = r#"
{
  let rec lp = |n: i64, a: i64| -> i64 select n { 0 => a, _ => lp(n - 1, a + n) };
  array::map([1, 2], |x| array::map([x, 10], |y| { let l = #[native] lp(10, y); l + count(y) }))
}
"#;

run!(nested_callback_regions_fuse_in_slots, NESTED_CALLBACK_REGIONS_FUSE_IN_SLOTS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::Array(a), Value::Array(b)] => {
            matches!(&a[..], [Value::I64(57), Value::I64(66)])
                && matches!(&b[..], [Value::I64(58), Value::I64(66)])
        }
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// A shared kernel's `?` raises to the handler of the slot that runs it:
// only the third element overflows, and only its slot's `r` moves.
const SHARED_REGION_RAISES_TO_ITS_SLOT: &str = r#"
skip(#n: 1, array::map([1, 2, 3], |x| {
  let r = 0;
  catch(e) r <- e ~ 1;
  let rec lp = |n: i64, a: i64| -> i64 select n { 0 => a, _ => lp(n - 1, a + n) };
  let v = #[native] (lp(200, x) +? (9223372036854775807 - 20100 - 2))?;
  r
}))
"#;

run!(shared_region_raises_to_its_slot, SHARED_REGION_RAISES_TO_ITS_SLOT, |v: Result<&Value>| match v {
    Ok(Value::Array(m)) => matches!(&m[..], [Value::I64(0), Value::I64(0), Value::I64(1)]),
    _ => false,
}; FuseExpect::Jit);

// A scalar binding read where its declared type is wider emits that
// type's two-word value: the float's bits, not the float register.
const SCALAR_READ_AT_A_NULLABLE: &str = r#"
{
  let a: [f64, null] = 1.5;
  let b: [i64, null] = 2;
  let c: [bool, null] = true;
  (a, b, c)
}
"#;

run!(scalar_read_at_a_nullable, SCALAR_READ_AT_A_NULLABLE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(
        &a[..],
        [Value::F64(x), Value::I64(2), Value::Bool(true)] if *x == 1.5
    ),
    _ => false,
}; FuseExpect::Jit);

// A capture of that binding passes its declared type's value word to the
// callee kernel, not the scalar register.
const SCALAR_LET_CAPTURED_AT_A_NULLABLE: &str = r#"
{
  let v0: [bool, null] = !false;
  let f = |x: i64| -> bool select v0 { null as _ => false, bool as n => n && (x > 0) };
  f(3)
}
"#;

run!(scalar_let_captured_at_a_nullable, SCALAR_LET_CAPTURED_AT_A_NULLABLE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Bool(true))
); FuseExpect::Jit);

// A tail call passing a scalar to a value-shaped formal widens it to the
// formal's Value encoding.
const TAIL_SCALAR_TO_NULLABLE: &str = r#"
{
  let rec last = |best: [f64, null], xs: Array<f64>| -> [f64, null] select xs {
    [] => best,
    [h, t..] => last(h, t)
  };
  last(null, [1.0, 2.0, 3.0])
}
"#;

run!(tail_scalar_to_nullable, TAIL_SCALAR_TO_NULLABLE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::F64(3.0))
); FuseExpect::Jit);

// A `?` that always raises stands in its consumer's shape: here an f64
// operand, which the fused add reads as one.
async fn always_raising_operand(mode: graphix_package_core::testing::Mode) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(
        r#"{
            let got = 0;
            let f = |v: f64| v * 2.0 + error(`Boom)?;
            let r = { catch(e) got <- e ~ 1; f(1.5) };
            got
        }"#,
        mode,
    )
    .await?;
    assert_eq!(super::dense_deltas::as_i64s(&values), vec![0, 1]);
    Ok(())
}

modes!(always_raising_operand);

// A callee body cached for one instance and emitted from another binds
// its own formals: both arms' `#[native]` fuse, `g -> h` reached directly
// and through `k`.
const CALLEE_BODY_OTHER_INSTANCE: &str = r#"
{
  let h = |a: i64| a * 3 + 1;
  let g = |a: i64| h(a) + 1;
  let k = |c: i64| g(c) * 3;
  let x = sys::time::after_idle(duration:1.ms, 3);
  select count(x) { 1 => #[native] (g(x) + 1), _ => #[native] (k(x) + 2) }
}
"#;

run!(callee_body_other_instance, CALLEE_BODY_OTHER_INSTANCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(12)))
}; FuseExpect::Jit);

// A tail-recursive body holding a collection loop recurses natively:
// each depth's loop keeps its own length, as each activation's does.
async fn tail_body_with_collection(
    mode: graphix_package_core::testing::Mode,
) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(
        r#"{
            let z = 0;
            z <- 1;
            let rec f = |a: Array<i64>, acc: i64, z: i64| -> i64 select a {
                [] => acc,
                [x, rest..] => f(rest, acc + array::len(array::map(a, |v| v)), z)
            };
            f([1, 2, 3], 0, z)
        }"#,
        mode,
    )
    .await?;
    assert_eq!(super::dense_deltas::as_i64s(&values), vec![6]);
    Ok(())
}

modes!(tail_body_with_collection);

// A recursion depth first reached after init is a fresh activation in
// the JIT as in the node-walk: its constants fire and its constant-derived
// error raises once per new depth.
async fn fresh_depth_is_born(mode: graphix_package_core::testing::Mode) -> Result<()> {
    let (values, _) = super::dense_deltas::run_delta(
        r#"{
            let errors = 0;
            let depth = 2;
            depth <- 4;
            let rec f = |n: i64| -> i64 {
                cast<i64>("x")?;
                select n { 0 => 0, n => f(n - 1) + 1 }
            };
            let r = { catch(e) errors <- e ~ errors + 1; f(depth) };
            errors
        }"#,
        mode,
    )
    .await?;
    assert_eq!(super::dense_deltas::as_i64s(&values).last(), Some(&5));
    Ok(())
}

modes!(fresh_depth_is_born);

// A kernel-built abstract value carries its type's params resolved, as a
// node-walked one does: one instantiation, so the user's Eq applies.
const KERNEL_BUILT_ABSTRACT_EQ: &str = r#"
{
  type Box<'a> = Abstract<('a, i64)>;
  impl<'a> Eq for Box<'a> { let eq = |a, b| a.0.1 == b.0.1 };
  array::map([1, 2, 3], |i| Box(("x", i)) == Box(("y", 2)))
}
"#;

run!(kernel_built_abstract_eq, KERNEL_BUILT_ABSTRACT_EQ, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[false, true, false]")
});

// A type test over an option tests the value's own type: "is a string"
// over `[Array<i64>, null]` is never "is not null".
const OPTION_TYPE_TEST_TESTS_THE_TYPE: &str = r#"
{
  let f = |x: ['a, null]| -> i64 select x {
    string as s => str::len(s) + 100,
    null as _ => 0,
    _ => 7
  };
  let a: [Array<i64>, null] = [1];
  let d: [datetime, null] = datetime:"2020-01-01T00:00:00Z";
  let c: [string, null] = "ab";
  (f(a), f(d), f(c))
}
"#;

run!(option_type_test_tests_the_type, OPTION_TYPE_TEST_TESTS_THE_TYPE, |v: Result<
    &Value,
>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:7, i64:7, i64:102]")
});

// Two instantiations of one typedef with bodies of one size expand each
// to its own type: `p` is `[i64, f64]`, never `[i64, i64]`.
const TYPEDEF_INSTANCES_EXPAND_APART: &str = r#"
{
  type Inner = i64;
  type W<'a> = ['a, Inner];
  let p: [W<i64>, W<f64>] = once(f64:1.5);
  let g = |q: [W<i64>, W<f64>], k: i64| -> i64 k + 1;
  g(p, 1)
}
"#;

run!(typedef_instances_expand_apart, TYPEDEF_INSTANCES_EXPAND_APART, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(2)))
});

// A labeled formal is passed by its name: a self-recursive lambda with
// one and a lambda with a labeled callback both fuse.
const LABELED_FORMALS_FUSE: &str = r#"
{
  let rec f = |#k: i64, n: i64| -> i64 select n { 0 => k, m => f(#k: k + 1, m - 1) + 1 };
  let g = |#cb: fn(v: i64) -> i64, x: i64| -> i64 cb(x) + 1;
  #[native] (f(#k: 0, 5), g(#cb: |v| v * 2, 3))
}
"#;

run!(labeled_formals_fuse, LABELED_FORMALS_FUSE, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:10, i64:7]")
});

// A result union's error test is the value's own tag, whatever the
// success type: the split fuses over `[i64, Error<E>]`.
const RESULT_SPLIT_OVER_A_SCALAR: &str = r#"
{
  let o: [Error<`E>, i64] = 4;
  let e: [Error<`E>, i64] = error(`E);
  let a = #[native] select o { error as _ => 0, x => x + 1 };
  let b = #[native] select e { error as _ => 10, x => x + 1 };
  (a, b)
}
"#;

run!(result_split_over_a_scalar, RESULT_SPLIT_OVER_A_SCALAR, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(5), Value::I64(10)]),
    _ => false,
}; FuseExpect::Jit);

// A map literal whose keys or values are variants or structs is a
// constant: it fuses.
const CONSTANT_MAP_OF_VARIANTS_FUSES: &str = r#"
{
  let f = |k: i64| #[native] (k, {`A => 1, `B => 2}, {"a" => {x: 1}}, {"t" => `C(3)});
  let (k, m, n, o) = f(4);
  (k, m{`B}$, (n{"a"}$).x, o{"t"}$)
}
"#;

run!(constant_map_of_variants_fuses, CONSTANT_MAP_OF_VARIANTS_FUSES, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"[i64:4, i64:2, i64:1, ["C", i64:3]]"#)
}; FuseExpect::Jit);

// A `T | null` union whose `T` has no register form is a plain Value:
// a record carrying one fuses, and a select over one tests it.
const NULLABLE_DURATION_IS_A_VALUE: &str = r#"
{
  let rows: Array<{n: i64, t: [duration, null]}> = [{n: 1, t: duration:1.s}, {n: 2, t: null}];
  let a = #[native] array::map(rows, |r| r.n * 2);
  let b = array::map(rows, |r| select r.t { null as _ => 0, _ => r.n });
  (a, b)
}
"#;

run!(nullable_duration_is_a_value, NULLABLE_DURATION_IS_A_VALUE, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[[i64:2, i64:4], [i64:1, i64:0]]")
}; FuseExpect::Jit);

// A union-typed call result is one kernel shape for every consumer: a
// struct field, an equality and an index all fuse over it.
const UNION_RESULT_CONSUMERS_AGREE: &str = r#"
{
  let b = true;
  let pick = |b, x, y| select b { true => x, false => y };
  #[native] ({a: pick(b, 1, 2) + 1, c: 0}, pick(b, "a", "b") == "a", pick(b, [1, 2], [3])[0])
}
"#;

run!(union_result_consumers_agree, UNION_RESULT_CONSUMERS_AGREE, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"[[["a", i64:2], ["c", i64:0]], true, i64:1]"#)
}; FuseExpect::Jit);

// Ordering over operands with no register form, and arithmetic on a
// decimal or a varint, fuse through the Value helpers.
const VALUE_ORDERING_AND_ARITH_FUSE: &str = r#"
{
  let names = ["apple", "melon", "kiwi", "zebra", "m"];
  let small = #[native] array::filter(names, |n| n < "m");
  let o: [i64, null] = null;
  let p: [i64, null] = 3;
  let ords = #[native] (o < p, p <= p, "b" >= "a", "a" > "b");
  let d = decimal:1.5;
  let ar = #[native] (d * decimal:2.0, z64:7 - z64:10, v32:3 + v32:4, d / decimal:0.5);
  (small, ords, ar)
}
"#;

run!(value_ordering_and_arith_fuse, VALUE_ORDERING_AND_ARITH_FUSE, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"[["apple", "kiwi"], [false, true, true, false], [decimal:3.00, z64:-3, v32:7, decimal:3.]]"#)
}; FuseExpect::Jit);

// Whole-value binds, `@` captures of every shape and string literal
// patterns lower: these selects fuse.
const WHOLE_BINDS_AND_STRING_LITERALS: &str = r#"
{
  let g = |v: [`A, `B(i64)]| select v { `A => 0, other => select other { `B(n) => n } };
  let h = |a: Array<i64>| select a { [] => 0, all => array::len(all) };
  let t = |p: (i64, i64)| select p { all@ (x, y) => x + y + all.0 };
  let s = |p: {x: i64, y: i64}| select p { all@ {x, y} => x + y + all.x };
  let w = |v: [`Some(i64), `None]| select v { all@ `Some(n) => select all { `Some(m) => m + n }, `None => 0 };
  let l = |xs: List<i64>| select xs { all@ [<h, ..>] => h + list::len(all), [<>] => 0 };
  let k = |s: string| select s { "a" => 1, "bc" => 2, other => str::len(other) };
  let c = |e: [`Char(string), `Key(i64)]| select e { `Char("q") => 1, `Char(_) => 2, `Key(n) => n };
  #[native] (g(`B(3)), h([1, 2]), t((1, 2)), s({x: 1, y: 2}), w(`Some(2)), l([<4, 5>]),
    k("a"), k("bc"), k("xyz"), c(`Char("q")), c(`Char("r")), c(`Key(7)))
}
"#;

run!(whole_binds_and_string_literals, WHOLE_BINDS_AND_STRING_LITERALS, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == "[i64:3, i64:2, i64:4, i64:4, i64:4, i64:6, i64:1, i64:2, i64:3, i64:1, i64:2, i64:7]")
}; FuseExpect::Jit);
