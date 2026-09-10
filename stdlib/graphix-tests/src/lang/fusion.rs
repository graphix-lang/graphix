// End-to-end fusion tests over `rt.load()`.

use crate::init;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use graphix_compiler::expr::Source;
use graphix_package_core::{run, testing::FuseExpect};
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

async fn load_and_await(code: &str) -> Result<Value> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let res = ctx.rt.load(Source::Internal(ArcStr::from(code))).await?;
    let eid = res
        .exprs
        .first()
        .ok_or_else(|| anyhow::anyhow!("no top-level expr in load result"))?
        .id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting for load result"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                ctx.shutdown().await;
                                return Ok(v);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_qop_unwraps_result() -> Result<()> {
    // A `?` over checked arith: the unwrap emits in-kernel and the JIT
    // fires.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from("(i64:1 +? i64:1)? == i64:2\n")))
        .await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    let value = loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                found = Some(v);
                            }
                        }
                    }
                    if let Some(v) = found { break v; }
                }
            }
        }
    };
    assert_eq!(value, Value::Bool(true));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    assert!(inv > 0, "JIT_INVOCATIONS=0 — Qop kernel didn't run via JIT");
    ctx.shutdown().await;
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_variadic_and_jits() -> Result<()> {
    // A variadic builtin call node-walks; the value is unchanged.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res =
        ctx.rt.load(Source::Internal(ArcStr::from("and(true, true, false)"))).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    let value = loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                found = Some(v);
                            }
                        }
                    }
                    if let Some(v) = found { break v; }
                }
            }
        }
    };
    assert_eq!(value, Value::Bool(false));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    assert!(inv == 0, "strict fusion: the variadic DynCall path must node-walk");
    ctx.shutdown().await;
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_array_literal_jits() -> Result<()> {
    // `[1, 2, 3]` as a program body; the counter proves the kernel ran.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx.rt.load(Source::Internal(ArcStr::from("[1, 2, 3]"))).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    let value = loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                found = Some(v);
                            }
                        }
                    }
                    if let Some(v) = found { break v; }
                }
            }
        }
    };
    let arr = match value {
        Value::Array(a) => a,
        other => bail!("expected array, got {other:?}"),
    };
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], Value::I64(1));
    assert_eq!(arr[1], Value::I64(2));
    assert_eq!(arr[2], Value::I64(3));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    assert!(inv > 0, "JIT_INVOCATIONS=0 — array literal kernel didn't run via JIT");
    ctx.shutdown().await;
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
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // Reset after init so only fixture invocations are counted.
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from("bit_and(i64:0xFF, i64:0x0F)")))
        .await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    let value = loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                found = Some(v);
                            }
                        }
                    }
                    if let Some(v) = found { break v; }
                }
            }
        }
    };
    assert_eq!(value, Value::I64(0x0F));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    assert!(inv > 0, "JIT_INVOCATIONS=0 — bit_and call didn't run via JIT");
    ctx.shutdown().await;
    Ok(())
}

/// The JIT-invocation counter itself: `1 + 2` through `rt.load()`
/// counts.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn jit_counter_bumps_on_load() -> Result<()> {
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    assert_eq!(graphix_compiler::fusion::emit_helpers::jit_invocations(), 0);
    let v = load_and_await("3 * 4 + 5").await?;
    assert_eq!(v, Value::I64(17));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
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
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // `CompExp::drop` unbinds `foo`; keep the first result alive through
    // the second compile.
    let _first = ctx.rt.compile(ArcStr::from("let foo = 7;")).await?;
    let res = ctx.rt.compile(ArcStr::from("foo * 6")).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e {
                        if id == eid {
                            assert_eq!(v, Value::I64(42));
                            ctx.shutdown().await;
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
}

/// An external string binding flows into a fused kernel as a string
/// param consumed by `str::len`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn external_string_region_param() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let _first = ctx.rt.compile(ArcStr::from("let s = \"hello\";")).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx.rt.compile(ArcStr::from("str::len(s)")).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e {
                        if id == eid {
                            assert_eq!(v, Value::I64(5));
                            assert!(
                                graphix_compiler::fusion::emit_helpers::jit_invocations() > 0,
                                "string region-param kernel should JIT-dispatch"
                            );
                            ctx.shutdown().await;
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
}

/// An external `datetime` binding flows into a fused kernel as a value
/// param consumed by `d + duration:1.s`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn external_datetime_region_param() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let _first = ctx
        .rt
        .compile(ArcStr::from("let d = datetime:\"2024-01-01T00:00:00Z\";"))
        .await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx.rt.compile(ArcStr::from("sys::time::add(d, duration:1.s)")).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e {
                        if id == eid {
                            let expected: chrono::DateTime<chrono::Utc> =
                                "2024-01-01T00:00:01Z".parse().unwrap();
                            assert!(
                                matches!(&v, Value::DateTime(dt) if **dt == expected),
                                "expected 2024-01-01T00:00:01Z, got {v:?}"
                            );
                            assert!(
                                graphix_compiler::fusion::emit_helpers::jit_invocations() > 0,
                                "a datetime fastcall site fuses across a region param"
                            );
                            ctx.shutdown().await;
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
}

#[tokio::test(flavor = "current_thread")]
async fn load_uses_external_scalar() -> Result<()> {
    // `foo` is bound at root scope by an earlier compile, so it is a
    // free-var Ref inside the loaded file: a scalar kernel input.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // Keep the first compile's result alive (see above).
    let _first = ctx.rt.compile(ArcStr::from("let foo = 7;")).await?;
    let res = ctx.rt.load(Source::Internal(ArcStr::from("foo * 6"))).await?;
    let eid = res.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting for foo * 6 result"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                assert_eq!(v, Value::I64(42));
                                ctx.shutdown().await;
                                return Ok(());
                            }
                        }
                    }
                }
            }
        }
    }
}

// Closure conversion: a capturing lambda's captures become extra kernel
// args the caller forwards.

/// Load `code`, returning the produced Value and the JIT-invocation
/// delta across the load.
#[cfg(debug_assertions)]
async fn load_value_and_jit(code: &str) -> Result<(Value, u64)> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx.rt.load(Source::Internal(ArcStr::from(code))).await?;
    let eid = res.exprs.first().ok_or_else(|| anyhow::anyhow!("no top-level expr"))?.id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    let value = loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting for result"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid { found = Some(v); }
                        }
                    }
                    if let Some(v) = found { break v; }
                }
            }
        }
    };
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    ctx.shutdown().await;
    Ok((value, inv))
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
#[tokio::test(flavor = "current_thread")]
async fn closure_tuple_capture_falls_back() -> Result<()> {
    let v = load_and_await("let t = (1, 2); let g = |x| t.0 + t.1 + x; g(10)").await?;
    assert_eq!(v, Value::I64(13));
    Ok(())
}

/// Nested closures both capturing `z`: `outer(5)` is 105.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn closure_nested_capture() -> Result<()> {
    let (v, _inv) = load_value_and_jit(
        "let z = 100; let outer = |x| { let inner = |y| y + z; inner(x) }; outer(5)",
    )
    .await?;
    assert_eq!(v, Value::I64(105));
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

/// An async builtin (`sys::net::publish`) inside an impure callback:
/// each slot gets its own publication; the map value is `[2,4,6]`.
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

// clone_rebind equivalence: each fixture forces a callback body through
// MapQ's per-slot clone path and captures an outer `k`.

/// Map `body` (an expr over element `x: i64` and captured `k: i64 = 3`)
/// over `[1,2,3,4]` through the clone path.
async fn clone_map(body: &str) -> Result<Value> {
    // `counter <- x` makes the callback async; `body` may be `let …; expr`.
    let prog = format!(
        "let counter = 0; let k = 3; \
         array::map([1, 2, 3, 4], |x: i64| {{ counter <- x; {body} }})"
    );
    load_and_await(&prog).await
}

/// The reference path: the same `body` over the same inputs as a pure
/// callback (no clone). `body` must be a single expression.
async fn pure_map(body: &str) -> Result<Value> {
    let prog = format!("let k = 3; array::map([1, 2, 3, 4], |x: i64| {body})");
    load_and_await(&prog).await
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
    let (tx, mut rx) = mpsc::channel(16);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        graphix_compiler::CFlag::FusionDisabled.into(),
        |_| {},
    )
    .await?;
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from(
            "let n = 100; \
             array::map([1, 2], |y: i64| array::map([1], |x: i64| x + n))",
        )))
        .await?;
    let eid = res.exprs.first().unwrap().id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => {
                ctx.shutdown().await;
                bail!("#168 node-walk regressed — hang under FusionDisabled");
            }
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut b) => for e in b.drain(..) {
                    if let GXEvent::Updated(id, v) = &e {
                        if *id == eid {
                            let r = assert_nested_i64s(v, &[&[101], &[101]]);
                            ctx.shutdown().await;
                            return r;
                        }
                    }
                }
            }
        }
    }
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
    let (tx, mut rx) = mpsc::channel(16);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        graphix_compiler::CFlag::FusionDisabled.into(),
        |_| {},
    )
    .await?;
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from(
            "let f = |z: i64| z * 2; \
             array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x)))",
        )))
        .await?;
    let eid = res.exprs.first().unwrap().id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => {
                ctx.shutdown().await;
                bail!("#169 node-walk regressed — hang under FusionDisabled");
            }
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut b) => for e in b.drain(..) {
                    if let GXEvent::Updated(id, v) = &e {
                        if *id == eid {
                            let r = assert_nested_i64s(v, &[&[2], &[2]]);
                            ctx.shutdown().await;
                            return r;
                        }
                    }
                }
            }
        }
    }
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
async fn clone_arith_capture() -> Result<()> {
    assert_i64s(&clone_map("x * k + k").await?, &[6, 9, 12, 15])
}

/// `Select` (single arm + wildcard) on the element, returning a capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_select_capture() -> Result<()> {
    assert_i64s(&clone_map("select x { 1 => k, n => n * k }").await?, &[3, 6, 9, 12])
}

/// `Select` with TWO arms binding the SAME name (`a`) — the transient
/// name-map case; each arm re-mints `a` and its body must resolve to the
/// fresh id.
#[tokio::test(flavor = "current_thread")]
async fn clone_select_same_name() -> Result<()> {
    assert_i64s(
        &clone_map("select x { i64 as a if a > k => a + k, i64 as a => a * k }").await?,
        &[3, 6, 9, 7],
    )
}

/// Comparison (`Gt`) + capture in the scrutinee.
#[tokio::test(flavor = "current_thread")]
async fn clone_compare_capture() -> Result<()> {
    assert_i64s(
        &clone_map("select x > k { true => k * 10, false => x }").await?,
        &[1, 2, 3, 30],
    )
}

/// `Or` + `Eq` + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_boolops_capture() -> Result<()> {
    assert_i64s(
        &clone_map("select (x > k) || (x == 1) { true => 1, false => 0 }").await?,
        &[1, 0, 0, 1],
    )
}

/// `Not` + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_not_capture() -> Result<()> {
    assert_i64s(
        &clone_map("select !(x > k) { true => x, false => k }").await?,
        &[1, 2, 3, 3],
    )
}

/// `Tuple` producer + `TupleRef` accessor, both touching a capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_tuple_accessor_capture() -> Result<()> {
    assert_i64s(&clone_map("let t = (x, k); t.0 + t.1").await?, &[4, 5, 6, 7])
}

/// `Struct` producer + `StructRef` accessor + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_struct_accessor_capture() -> Result<()> {
    assert_i64s(&clone_map("let s = {a: x, b: k}; s.a * s.b").await?, &[3, 6, 9, 12])
}

/// `Array` producer + `ArrayRef` accessor + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_array_accessor_capture() -> Result<()> {
    assert_i64s(&clone_map("let a2 = [x, k, x + k]; a2[2]").await?, &[4, 5, 6, 7])
}

/// `Variant` producer + `Select` destructure of it + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_variant_capture() -> Result<()> {
    assert_i64s(
        &clone_map("let v = `Pair(x, k); select v { `Pair(a, b) => a + b }").await?,
        &[4, 5, 6, 7],
    )
}

/// `Map` producer + `MapRef` access + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_map_accessor_capture() -> Result<()> {
    assert_i64s(
        &clone_map("let m = {\"a\" => x, \"b\" => k}; m{\"b\"}").await?,
        &[3, 3, 3, 3],
    )
}

/// `StringInterpolate` carrying both element and capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_string_capture() -> Result<()> {
    assert_strs(&clone_map("\"[x]:[k]\"").await?, &["1:3", "2:3", "3:3", "4:3"])
}

/// Nested: `Tuple` + `Add`/`Mul` + `TupleRef` + `StringInterpolate`.
#[tokio::test(flavor = "current_thread")]
async fn clone_nested_capture() -> Result<()> {
    assert_strs(
        &clone_map("let p = (x * k, x + k); \"[p.0]/[p.1]\"").await?,
        &["3/4", "6/5", "9/6", "12/7"],
    )
}

/// A `select` with an arm binding (`n =>` catch-all) as a `let` value
/// inside the per-slot clone path.
#[tokio::test(flavor = "current_thread")]
async fn clone_select_let_bound() -> Result<()> {
    assert_i64s(
        &clone_map("let r = select x { 1 => k, n => n * k }; r").await?,
        &[3, 6, 9, 12],
    )
}

/// The same body region-fused through `pure_map`.
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
    graphix_compiler::fusion::emit_helpers::reset_fusion_invocations();
    let v = pure_map(body).await?;
    let f = graphix_compiler::fusion::emit_helpers::fusion_invocations();
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
    graphix_compiler::fusion::emit_helpers::reset_fusion_invocations();
    let v = load_and_await(
        "select rand::rand(#start: 0, #end: 1000000, #clock: 1) \
         { n => n == n }",
    )
    .await?;
    assert_eq!(v, Value::Bool(true));
    assert!(
        graphix_compiler::fusion::emit_helpers::fusion_invocations() > 0,
        "rand-scrutinee select should fuse (the dup bug was fusion-only)"
    );
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

/// A `select` arm binding that shadows an outer `n` another arm reads,
/// inside a per-slot callback: `[100,4,6,8]`. The timeout keeps a
/// regression from hanging the suite.
#[tokio::test(flavor = "current_thread")]
async fn shadow_arm_binding_outer_ref() -> Result<()> {
    let fut = load_and_await(
        "let n = 100; \
         array::map([1, 2, 3, 4], |x: i64| select x { 1 => n, n => n * 2 })",
    );
    match tokio::time::timeout(std::time::Duration::from_secs(5), fut).await {
        Ok(r) => assert_i64s(&r?, &[100, 4, 6, 8]),
        Err(_) => bail!("#167 regressed — select arm-binding shadow hangs"),
    }
}

/// The same under the pure node-walk: `[100,4,6,8]`.
#[tokio::test(flavor = "current_thread")]
async fn shadow_arm_binding_node_walk() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(16);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        graphix_compiler::CFlag::FusionDisabled.into(),
        |_| {},
    )
    .await?;
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from(
            "let n = 100; \
             array::map([1, 2, 3, 4], |x: i64| select x { 1 => n, n => n * 2 })",
        )))
        .await?;
    let eid = res.exprs.first().unwrap().id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => {
                ctx.shutdown().await;
                bail!("#167 node-walk regressed — hang under FusionDisabled");
            }
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut b) => for e in b.drain(..) {
                    if let GXEvent::Updated(id, v) = &e {
                        if *id == eid {
                            let r = assert_i64s(v, &[100, 4, 6, 8]);
                            ctx.shutdown().await;
                            return r;
                        }
                    }
                }
            }
        }
    }
}

// Env accounting: every per-slot grow mints bindings and every shrink
// must reverse them. Drives an impure HOF array up and down and asserts
// the registries return to the same size at the bottom of every cycle.

/// Drain `rx` until the map at `eid` emits an array of `target` length;
/// times out after 5s.
async fn await_map_len(
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    eid: graphix_compiler::expr::ExprId,
    target: usize,
) -> Result<()> {
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting for map len {target}"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, Value::Array(a)) = &e {
                        if *id == eid && a.len() == target {
                            return Ok(());
                        }
                    }
                }
            }
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
                 {base:?} — clone_rebind minted bindings/refs that \
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

// Env/reference nodes (TryCatch, Sample, ByRef/Deref, ...) inside an
// impure callback that captures the outer `k`: each slot must resolve
// the capture without contaminating a sibling slot.

/// A catch handler that fires and captures both the element `x` and
/// the outer `k`, driving `res <- x + k`: `[4,5,6,7]`.
#[tokio::test(flavor = "current_thread")]
async fn clone_trycatch_catch_capture() -> Result<()> {
    assert_i64s(
        &clone_map(
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
async fn clone_trycatch_try_capture() -> Result<()> {
    assert_i64s(&clone_map("{ catch(e) -1; x / k }").await?, &[0, 0, 1, 1])
}

/// `x ~ k` emits `k` when the element fires: `[3,3,3,3]`.
#[tokio::test(flavor = "current_thread")]
async fn clone_sample_capture() -> Result<()> {
    assert_i64s(&clone_map("x ~ k").await?, &[3, 3, 3, 3])
}

/// ByRef + Deref capturing `k`: `*r + x` is `[4,5,6,7]`.
#[tokio::test(flavor = "current_thread")]
async fn clone_byref_deref_capture() -> Result<()> {
    assert_i64s(&clone_map("let r = &k; *r + x").await?, &[4, 5, 6, 7])
}

// Proptest swarm: random total i64 callback bodies over `x` and `k`;
// the clone path must agree with the reference path.

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
    fn clone_matches_reference(body in body_strategy()) {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let (reference, cloned) = rt
            .block_on(async {
                let r = pure_map(&body).await?;
                let c = clone_map(&body).await?;
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
