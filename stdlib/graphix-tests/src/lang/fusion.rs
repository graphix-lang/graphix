// End-to-end fusion tests: drive `rt.load()` with small graphix
// programs and verify the resulting Value. Exercises the synthetic
// Do-wrap path in `gx.load()` and (where applicable) the splice
// of zero-input Region kernels in `fusion::fuse`.

use crate::init;
use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::Source;
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
    // `str::parse("42")?` — Qop on a `Result<i64, ParseError>` return.
    // The Result lowers to `Nullable<i64>` and Qop is lowered to
    // the qop-unwrap CLIF emission which extracts the success i64. JIT must
    // fire because there's no Block-with-let wrapper here.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from(
            "re::is_match(#pat:r'a', \"abc\")?\n",
        )))
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
    // `and(true, true, false)` — variadic builtin (`@args: bool`)
    // with uniform bool arg type. Exercises the
    // `BuiltinSlot::Variadic` slot path through DynCall.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from("and(true, true, false)")))
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
    assert_eq!(value, Value::Bool(false));
    let inv = graphix_compiler::fusion::emit_helpers::jit_invocations();
    assert!(inv > 0, "JIT_INVOCATIONS=0 — variadic and didn't run via JIT");
    ctx.shutdown().await;
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_array_literal_jits() -> Result<()> {
    // `[1, 2, 3]` as a program body — exercises `ExprKind::Array`
    // lowering via the new `emit_array_new` path, which reuses
    // the tuple-new CLIF emission and tags the outer type as an i64 array.
    // Counter > 0 proves the JIT'd kernel ran.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from("[1, 2, 3]")))
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
    // Smallest possible fusable program: one top-level expression
    // with no free variables. After the load-wrap this becomes a
    // Do block containing one Region candidate (the `1 + 2`); the
    // fusion phase replaces it with a kernel-backed FusedKernel.
    let v = load_and_await("1 + 2").await?;
    assert_eq!(v, Value::I64(3));
    Ok(())
}

/// End-to-end test of the builtin-call DynCall path. Loads a
/// program that calls `core::bit_and` — a sync builtin with two
/// `i64` args and an `i64` return. Discovery should register the
/// call site as a `FnSource::Builtin` slot; the JIT-compiled
/// kernel issues a DynCall that dispatches into the
/// builtin's `Apply::update` through `dispatch_typed`. The
/// counter assertion proves the JIT'd wrapper ran (so the
/// builtin's DynCall actually executed natively).
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_calls_builtin_bit_and() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // Reset AFTER init so we count only fixture JIT, not stdlib
    // root-module compilation.
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from(
            "bit_and(i64:0xFF, i64:0x0F)",
        )))
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

/// Positive verification of the test-harness's JIT-invocation
/// counter. Reset the counter, drive a fixture that should JIT
/// end-to-end (`1 + 2` through `rt.load()`), then read the
/// counter — must be `> 0` after the cycle. Proves the counter
/// itself works, independently of the `run!` macro's three-mode
/// expansion which doesn't reach the wrap-Do path today.
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
    // A slightly more interesting Region: nested arithmetic, still
    // zero free variables.
    let v = load_and_await("(2 * 3) + (4 * 5)").await?;
    assert_eq!(v, Value::I64(26));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn load_bind_then_expr() -> Result<()> {
    // A file with a Bind followed by an output expression. The
    // walker registers the Bind as a ValueBind (currently skipped
    // — no kernel, just stays as the normal Bind Node) and the
    // trailing `x + 1` as a Region. The Region has a free-var
    // Ref to `x`, so today's zero-input-only build path will
    // refuse to fuse it (build_region returns Err); the
    // expression still evaluates correctly via the regular
    // interpreter.
    let v = load_and_await("let x = 5; x + 1").await?;
    assert_eq!(v, Value::I64(6));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn compile_then_compile_external_scalar() -> Result<()> {
    // Sanity check the underlying "compile registers a binding,
    // subsequent compile sees it" property without invoking the
    // load-wrap path. If this fails the wrap-based test would too
    // for unrelated reasons.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // Keep the first compile's result alive — `CompExp::drop` sends
    // ToGX::Delete which calls Bind::delete and unbinds `foo` from
    // env. `let _ = ...` would drop the result immediately, racing
    // the Delete against the second compile. Binding to `_first`
    // (or any non-`_` name) keeps it alive through the second
    // compile.
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

/// End-to-end string region-input param: an external (cross-compile)
/// string binding `s` flows into a fused kernel as a real
/// `string_params` slot, consumed by `str::len`. Proves the discovery
/// → `populate_kernel_inputs` → runtime arg-packing chain for a String
/// kernel parameter (not const-inlined, since the binding lives in a
/// separate compilation unit). The direct ABI round-trip lives in
/// `fusion::{kernel,emit}::tests::*string_and_value_kernel_params`; this
/// closes the discovery + native-dispatch half.
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

/// End-to-end value-shape region-input param: an external `datetime`
/// binding flows into a fused kernel as a `value_params` slot and is
/// consumed by the value-arith CLIF emission (`d + duration:1.s`). The result is
/// a `datetime`, decoded back through the two-register value boundary.
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
    let res = ctx.rt.compile(ArcStr::from("d + duration:1.s")).await?;
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
                                "datetime region-param kernel should JIT-dispatch"
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
    // First inline-compile a binding so it lives at root scope:
    // `let foo = 7`. Then load a file `foo * 6`. The load wraps the
    // file in a synthetic Do block, but `foo` is bound OUTSIDE the
    // wrap (at root scope from the earlier compile). That makes
    // `foo` a free-var Ref inside the wrap-Do's body — exactly the
    // case `fusion::fuse` is supposed to handle: discover the
    // external ref, build a scalar kernel input slot for it, splice
    // a `FusedKernel` whose `Ref` feeder subscribes to `foo`'s
    // BindId, pack the latest value into the wrapper's u64 args,
    // and invoke native code.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    // Stage 1: register `foo = 7` at root scope.
    // Keep first compile result alive — see note in
    // `compile_then_compile_external_scalar`.
    let _first = ctx.rt.compile(ArcStr::from("let foo = 7;")).await?;
    // Stage 2: load a file referring to it.
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

// ─── Closure conversion (Phase C) ────────────────────────────────
//
// A capturing lambda's body references an outer binding. Closure
// conversion lifts each capture to an extra positional kernel arg;
// the caller forwards the capture's current value. These tests drive
// the full load() pipeline and assert the produced Value. Where the
// closure should fuse, a JIT_INVOCATIONS assertion proves the
// kernel actually ran natively (not an interpreter fall-back).

/// Load `code`, return both the produced Value and the
/// JIT-invocation delta across the load (reset before, read after).
#[cfg(debug_assertions)]
async fn load_value_and_jit(code: &str) -> Result<(Value, u64)> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
    let res = ctx.rt.load(Source::Internal(ArcStr::from(code))).await?;
    let eid = res
        .exprs
        .first()
        .ok_or_else(|| anyhow::anyhow!("no top-level expr"))?
        .id;
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

/// C1 — single primitive capture. `let y = 7; let f = |x| x + y;
/// f(3)` → 10. The lambda `f` captures `y`; closure conversion
/// lifts `y` to `f`'s kernel as a second positional arg, and the
/// parent forwards its current value (7) at the call site.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn closure_primitive_capture() -> Result<()> {
    let (v, inv) = load_value_and_jit("let y = 7; let f = |x| x + y; f(3)").await?;
    assert_eq!(v, Value::I64(10));
    assert!(inv > 0, "JIT_INVOCATIONS=0 — capturing closure didn't fuse");
    Ok(())
}

/// C2 — composite capture (tuple). `let t = (1, 2); let g = |x|
/// t.0 + t.1 + x; g(10)` → 13. The capture `t` is a tuple, so the
/// call passes a composite arg across the kernel boundary. The
/// interpreter routes it into the callee's tuple slot
/// (`eval_kernel_full`); the JIT's cross-kernel-call emission doesn't lower
/// composite args yet, so in jit mode `fuse()` falls back to the
/// interpreter. Either way the value is correct. When #131-JIT lands,
/// upgrade this to `load_value_and_jit` + assert `JIT_INVOCATIONS > 0`.
#[tokio::test(flavor = "current_thread")]
async fn closure_tuple_capture_falls_back() -> Result<()> {
    let v =
        load_and_await("let t = (1, 2); let g = |x| t.0 + t.1 + x; g(10)")
            .await?;
    assert_eq!(v, Value::I64(13));
    Ok(())
}

/// C3 — nested closures. `let z = 100; let outer = |x| { let inner =
/// |y| y + z; inner(x) }; outer(5)` → 105. Both `outer` and `inner`
/// capture `z`; the cascade is automatic via the recursive `Refs`
/// walk.
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

/// C7 — capture vs same-named parent shadow. `let y = 7; let f = |x|
/// x + y; { let y = 100; f(5) }` → 12 (f captures the OUTER y=7, not
/// the inner y=100). This is the BindId-keyed-lookup correctness
/// test: a name-keyed capture lookup would forward 100 and yield 105.
#[tokio::test(flavor = "current_thread")]
async fn closure_capture_respects_shadow() -> Result<()> {
    let v = load_and_await("let y = 7; let f = |x| x + y; { let y = 100; f(5) }").await?;
    assert_eq!(v, Value::I64(12));
    Ok(())
}

/// C8 — fn-typed external (statically resolved). `let f = |x| x + 1;
/// let g = |y| f(y) * 2; g(5)` → 12. `g` references `f` (a function),
/// which is NOT a value capture — the body's CallSite resolves to
/// `f`'s kernel and emits a cross-kernel call. Verifies fn externals
/// don't break closure conversion.
#[tokio::test(flavor = "current_thread")]
async fn closure_fn_external_static() -> Result<()> {
    let v = load_and_await("let f = |x| x + 1; let g = |y| f(y) * 2; g(5)").await?;
    assert_eq!(v, Value::I64(12));
    Ok(())
}

/// Cross-kernel-call arg ordering regression guard: a lambda whose
/// formal args are `(composite, scalar)`. The callee's ABI groups
/// params by kind (scalars first, then composite pointers — see
/// `KernelSig::abi_params`), so a naive positional arg pass would
/// route the scalar `5` into the tuple-pointer slot and dereference
/// it (a misaligned-pointer crash). The interpreter buckets each arg
/// by `AbiKind` into the right slot, so `g((10,20), 5)` = 35 with no
/// crash. (The JIT cross-kernel-call emission returns Err for the composite
/// arg and falls back to interp until #131-JIT; that's where this
/// guard matters most — the JIT must assemble args in kind-grouped
/// order.)
#[tokio::test(flavor = "current_thread")]
async fn call_arg_order_composite_then_scalar() -> Result<()> {
    let v = load_and_await(
        "let g = |p: (i64, i64), n: i64| p.0 + p.1 + n; let pair = (10, 20); g(pair, 5)",
    )
    .await?;
    assert_eq!(v, Value::I64(35));
    Ok(())
}

/// #153 — impure HOF callback, maximal sync-subgraph split. The
/// callback `|x| { let v = x*2+1; counter <- v; v }` is impure (the
/// `counter <- v` Connect is an async op), so the WHOLE body can't
/// fuse. The split engine carves the sync sub-region `x*2+1` out of
/// the body, builds a shared kernel for it, and splices that kernel
/// into each per-slot callback body at slot construction — fusing the
/// calc while the Connect stays interpreted. Asserts both the value
/// AND that the sync kernel JIT-ran (`inv > 0`). See
/// design/impure_hof_fusion.md.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn impure_hof_callback_splits() -> Result<()> {
    let (v, inv) = load_value_and_jit(
        "let counter = 0; \
         array::map([1, 2, 3], |x| { let v = x * 2 + 1; counter <- v; v })",
    )
    .await?;
    match v {
        Value::Array(a)
            if &a[..] == [Value::I64(3), Value::I64(5), Value::I64(7)] => {}
        other => bail!("unexpected value: {other:?}"),
    }
    assert!(
        inv > 0,
        "JIT_INVOCATIONS=0 — impure HOF callback's sync sub-region \
         didn't fuse"
    );
    Ok(())
}

/// #153 — impure HOF callback whose sync sub-region CAPTURES an outer
/// binding. `let k=10; … |x| { let v = x*k; counter <- v; v }` →
/// [10,20,30]. The in-place split fuses `x*k` with inputs `{x (element),
/// k (capture)}`; `try_fuse`'s `collect_region_inputs` builds a feeder
/// for each, and `clone_rebind` re-resolves them per slot by name — `k`'s
/// feeder reads the live outer binding. Locks in the capture-feeder path
/// the element-only fixture above doesn't exercise.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn impure_hof_callback_split_captures() -> Result<()> {
    let (v, inv) = load_value_and_jit(
        "let k = 10; let counter = 0; \
         array::map([1, 2, 3], |x| { let v = x * k; counter <- v; v })",
    )
    .await?;
    match v {
        Value::Array(a)
            if &a[..]
                == [Value::I64(10), Value::I64(20), Value::I64(30)] => {}
        other => bail!("unexpected value: {other:?}"),
    }
    assert!(
        inv > 0,
        "JIT_INVOCATIONS=0 — captured sync sub-region didn't fuse"
    );
    Ok(())
}

/// #157 — a BUILTIN call (`sys::net::publish`, async) inside an impure
/// callback. The callback goes through MapQ's per-slot clone path; the
/// inner `publish` CallSite is unbound in the analysis-only template, so
/// each slot's clone re-binds it on first update → a fresh independent
/// publication per slot (no per-builtin clone code). End-to-end guard
/// that a builtin in a callback residue fuses + runs through clone_rebind.
/// Map value is the tail `v` = `[2,4,6]`.
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
        Value::Array(a)
            if &a[..]
                == [Value::I64(2), Value::I64(4), Value::I64(6)] => {}
        other => bail!("unexpected value: {other:?}"),
    }
    Ok(())
}

// ─── clone_rebind equivalence matrix (design/clone_rebind_testing.md #1) ──
//
// Each fixture forces a callback body through MapQ's per-slot CLONE path —
// the `counter <- x` makes the callback async, so MapQ runs and the
// separate-clone clones the *pristine* body through the structural
// `clone_rebind` impls — AND captures an outer binding `k`. If any
// structural clone has a wrong field, a dropped child, or a lost capture,
// the produced value goes wrong and the test fails. The job is to find a
// bug, not to pass.

/// Map `body` (an expr over element `x: i64` and captured `k: i64 = 3`)
/// over `[1,2,3,4]` through the clone path; return the result array.
async fn clone_map(body: &str) -> Result<Value> {
    // `counter <- x` first (makes the callback async → MapQ + clone path);
    // `body` is the block's value, so it may itself be `let …; expr`.
    let prog = format!(
        "let counter = 0; let k = 3; \
         array::map([1, 2, 3, 4], |x: i64| {{ counter <- x; {body} }})"
    );
    load_and_await(&prog).await
}

/// Reference path: the SAME `body` over the SAME inputs WITHOUT the
/// clone — a PURE callback (no `counter <-`), so MapQ either region-
/// fuses it into one kernel or falls back to a fresh per-slot
/// `genn::apply` interpreted CallSite. Either way it never touches the
/// `clone_rebind` template path. `body` must be a single expression
/// (no leading `let`), so the bare-expression lambda body is valid.
async fn pure_map(body: &str) -> Result<Value> {
    let prog =
        format!("let k = 3; array::map([1, 2, 3, 4], |x: i64| {body})");
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
    if ok {
        Ok(())
    } else {
        bail!("expected {expected:?}, got {v:?}")
    }
}

fn assert_strs(v: &Value, expected: &[&str]) -> Result<()> {
    let Value::Array(a) = v else { bail!("not an array: {v:?}") };
    let ok = a.len() == expected.len()
        && a.iter().zip(expected).all(|(x, e)| {
            matches!(x, Value::String(s) if s.as_str() == *e)
        });
    if ok {
        Ok(())
    } else {
        bail!("expected {expected:?}, got {v:?}")
    }
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

// ─── #168 nested-HOF grandparent-capture regression ───────────────────
//
// A HOF whose INNER callback references a GRANDPARENT capture (a binding
// outside BOTH HOFs) hung: the inner bare Lambda node fell to the
// recompile-default `clone_rebind`, which — because `Lambda::refs` is
// empty — never aliased the capture into the per-slot clone scope (rooted
// in the array package, not the program), so the capture couldn't resolve
// and the inner map produced nothing. Fixed by aliasing the lambda body's
// def-resolvable free vars in `Lambda::clone_rebind`. The single-level and
// outer-element-capture cases always worked (the latter is bound into the
// per-slot scope by MapQ); these assert the grandparent case + that the
// working cases didn't regress.

#[tokio::test(flavor = "current_thread")]
async fn nested_hof_grandparent_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| x + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[101], &[101]])
}

/// Inner callback captures BOTH the outer element `y` (slot-local) and a
/// grandparent `n` — the fix must alias `n` while leaving `y` (resolvable
/// in the per-slot scope) alone.
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_capture_element_and_grandparent() -> Result<()> {
    let v = load_and_await(
        "let n = 5; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| x + y + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[7], &[8]])
}

/// nested `fold` (not map-specific) referencing a grandparent capture.
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

/// Same grandparent-capture nest under the PURE NODE WALK
/// (`CFlag::FusionDisabled`) — the canonical model must produce the value
/// (MapQ uses clone_rebind for the per-slot graph even with fusion off).
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

// ─── #169: function-typed grandparent capture in a nested HOF ──────────
//
// `let f = |z| z*2; array::map([1,2], |y| array::map([1], |x| f(x)))` —
// the inner callback CALLS `f`, a function-typed grandparent capture.
// Was a one-line root cause: `Expr::fold` skipped the Apply's `function`
// position, so the #168 `Lambda::clone_rebind` alias pass never saw the
// callee `f` and the inner callback's recompile couldn't resolve it.
// Fixed by making `Expr::fold` a full-tree walk. Value-position captures
// (#168) always went through fold's already-covered arms; only call-
// position refs were dropped.

#[tokio::test(flavor = "current_thread")]
async fn nested_hof_function_capture() -> Result<()> {
    let v = load_and_await(
        "let f = |z: i64| z * 2; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[2], &[2]])
}

/// Function capture mixed with a value capture — exercises both fold
/// arms (call position `f` + value position `n`).
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_function_and_value_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 100; let f = |z: i64| z * 2; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| f(x) + n))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[102], &[102]])
}

/// A nested ANONYMOUS lambda call capturing a grandparent — the callee is
/// a Lambda expr in function position, which the fold fix also now
/// descends into (collecting the inner lambda's own capture `n`).
#[tokio::test(flavor = "current_thread")]
async fn nested_hof_anon_lambda_capture() -> Result<()> {
    let v = load_and_await(
        "let n = 5; \
         array::map([1, 2], |y: i64| array::map([1], |x: i64| (|z: i64| z + n)(x)))",
    )
    .await?;
    assert_nested_i64s(&v, &[&[6], &[6]])
}

/// #169 under the PURE NODE WALK (`CFlag::FusionDisabled`) — the
/// canonical model must produce the value.
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

// ─── #170: Expr::fold completeness (StructWith.source + arg defaults) ──
//
// The #168/#169 capture-alias pass `fold`s the callback body to collect
// free-var names. `Expr::fold` was silently INCOMPLETE — beyond the
// #169 `Apply.function` gap it also skipped `StructWith.source` (via a
// `..` pattern) and lambda labeled-arg DEFAULT expressions. So a capture
// in those positions inside a nested HOF wasn't aliased → hang. Fixed by
// making `Expr::fold` a true full-tree walk.

/// A struct functional-update `{base with a: x}` whose `base` is a
/// grandparent capture (StructWith.source position).
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

/// A labeled-arg DEFAULT that captures a grandparent (`#off = n`) — fold
/// now visits `Arg.labeled` defaults.
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
    assert_i64s(
        &clone_map("select x { 1 => k, n => n * k }").await?,
        &[3, 6, 9, 12],
    )
}

/// `Select` with TWO arms binding the SAME name (`a`) — the transient
/// name-map case; each arm re-mints `a` and its body must resolve to the
/// fresh id.
#[tokio::test(flavor = "current_thread")]
async fn clone_select_same_name() -> Result<()> {
    assert_i64s(
        &clone_map(
            "select x { i64 as a if a > k => a + k, i64 as a => a * k }",
        )
        .await?,
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
        &clone_map("select (x > k) || (x == 1) { true => 1, false => 0 }")
            .await?,
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
    assert_i64s(
        &clone_map("let s = {a: x, b: k}; s.a * s.b").await?,
        &[3, 6, 9, 12],
    )
}

/// `Array` producer + `ArrayRef` accessor + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_array_accessor_capture() -> Result<()> {
    assert_i64s(
        &clone_map("let a2 = [x, k, x + k]; a2[2]").await?,
        &[4, 5, 6, 7],
    )
}

/// `Variant` producer + `Select` destructure of it + capture.
#[tokio::test(flavor = "current_thread")]
async fn clone_variant_capture() -> Result<()> {
    assert_i64s(
        &clone_map("let v = `Pair(x, k); select v { `Pair(a, b) => a + b }")
            .await?,
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
    assert_strs(
        &clone_map("\"[x]:[k]\"").await?,
        &["1:3", "2:3", "3:3", "4:3"],
    )
}

/// Nested: `Tuple` + `Add`/`Mul` + `TupleRef` + `StringInterpolate`.
#[tokio::test(flavor = "current_thread")]
async fn clone_nested_capture() -> Result<()> {
    assert_strs(
        &clone_map("let p = (x * k, x + k); \"[p.0]/[p.1]\"").await?,
        &["3/4", "6/5", "9/6", "12/7"],
    )
}

/// #162 — a `select` with an arm BINDING (`n =>` catch-all) as a `let`
/// value, inside the per-slot clone path. The arm body's `Ref(n)` now
/// resolves to the scrutinee via `known_consts` (the fix in
/// `emit_arm_condition`'s `Bind` arm); previously it lowered to a
/// dangling local read of `n` that panicked at runtime.
#[tokio::test(flavor = "current_thread")]
async fn clone_select_let_bound() -> Result<()> {
    assert_i64s(
        &clone_map("let r = select x { 1 => k, n => n * k }; r").await?,
        &[3, 6, 9, 12],
    )
}

/// Same body region-fused (no `counter <-`, so it never touches MapQ's
/// clone path) — the region-fuse path also panicked before the #162 fix,
/// confirming the defect was in `emit_select_as_expr`/`emit_arm_condition`,
/// not the per-slot clone.
async fn region_map(body: &str) -> Result<Value> {
    let prog = format!(
        "let k = 3; array::map([1, 2, 3, 4], |x: i64| {{ {body} }})"
    );
    load_and_await(&prog).await
}

#[tokio::test(flavor = "current_thread")]
async fn region_select_let_bound() -> Result<()> {
    assert_i64s(
        &region_map("let r = select x { 1 => k, n => n * k }; r").await?,
        &[3, 6, 9, 12],
    )
}

// ─── #162 fused-select-with-binding regression suite ──────────────────
//
// The matrix's `clone_select_*` fixtures use the IMPURE `clone_map`
// harness, whose split path does NOT fuse the callback's tail expr — so
// the select ran INTERPRETED there, silently dodging the bug. These use
// the PURE `pure_map` harness, which actually fuses the select into the
// kernel, and assert BOTH the value AND that a fused kernel ran
// (`fusion_invocations > 0`) — so a regression that makes the
// select-with-binding stop fusing (or fuse wrong) fails loudly.

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
    let (v, f) =
        pure_select_value_and_fusion("select x { 1 => k, n => n * k }")
            .await?;
    assert!(f > 0, "select-with-binding should fuse, FUSION=0");
    assert_i64s(&v, &[3, 6, 9, 12])
}

/// Typed capture `i64 as n =>` — structurally identical to catch-all
/// (the `i64` is only the type predicate). `[3,6,9,12]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_typed_capture() -> Result<()> {
    let (v, f) = pure_select_value_and_fusion(
        "select x { 1 => k, i64 as n => n * k }",
    )
    .await?;
    assert!(f > 0, "typed-capture select should fuse, FUSION=0");
    assert_i64s(&v, &[3, 6, 9, 12])
}

/// Typed capture used in a GUARD and the body (`a > k`, `a + k`). The
/// binding must be visible to the guard, which is emitted after the
/// structure predicate. `[3,6,9,7]`, fused.
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

/// Binding select wrapped in arithmetic (non-tail, non-let position).
/// `1 + (select …)` → `[4,7,10,13]`, fused.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_arith_wrapped() -> Result<()> {
    let (v, f) = pure_select_value_and_fusion(
        "1 + (select x { 1 => k, n => n * k })",
    )
    .await?;
    assert!(f > 0, "arith-wrapped binding select should fuse, FUSION=0");
    assert_i64s(&v, &[4, 7, 10, 13])
}

// ─── adversarial-review findings (scrutinee eval-once + variant shadow) ─

/// Review Finding 1: a NON-IDEMPOTENT scrutinee (a `rand()` Sync DynCall)
/// must be evaluated EXACTLY ONCE — bound to a temp — not re-evaluated at
/// each arm-condition / pattern-binding reference. `n == n` is `true` iff
/// `n` denotes a single draw; before the stabilize-scrutinee fix it
/// returned `false` (the two `n` references each re-dispatched `rand`,
/// drawing divergent values). Deterministic-true despite the random
/// scrutinee — that's the whole point. `fusion > 0` proves it's the fused
/// path (the bug was fusion-only; interp always evaluated once).
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

/// Stabilization machinery in a JIT-able kernel: a NON-Local (arith)
/// scrutinee `x + 1` is bound to a temp, captured by `m`, referenced 3×
/// — `m + m + m` = `3*(x+1)` → `[6,9,12,15]`. The Local-scrutinee
/// fixtures above skip the temp path (a raw Local is left as-is); this
/// exercises the `Block`/`Let` wrapper end-to-end with no DynCall, so it
/// JITs. `fusion > 0`.
#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn fused_select_stabilize_multiref() -> Result<()> {
    let (v, f) =
        pure_select_value_and_fusion("select (x + 1) { m => m + m + m }")
            .await?;
    assert!(f > 0, "stabilized-scrutinee select should fuse, FUSION=0");
    assert_i64s(&v, &[6, 9, 12, 15])
}

/// Same, with subtraction: `n - n` is `0` iff `n` is one draw.
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

/// Review Finding 2: a variant payload bind whose name shadows a kernel
/// input (here the element `x`) must NOT read the input. The shadow guard
/// bails to the interpreter, producing the correct payload value
/// `[14,15,16,17]` (= `(x+10) + k`); before the guard the fused kernel
/// read the element `x` + `k` = `[4,5,6,7]`.
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

/// #167 (FIXED): a `select` arm BINDING (`n =>`) that shadows an outer
/// `n` which ANOTHER arm references (`1 => n`), inside a per-slot HOF
/// callback. Was a node-graph correctness bug: `Select::clone_rebind`
/// (which builds the per-slot node graph) re-minted every arm's pattern
/// in ONE shared scope instead of per-arm `sel<id>` sub-scopes (as
/// `Select::compile` does), so a later clone resolved arm 1's `Ref(n)` to
/// arm 2's stale sibling binding — never written when arm 1 fires — and
/// the slot produced nothing → the map hung. Fixed by appending a fresh
/// per-arm scope in `Select::clone_rebind`. x=1 → arm 1 → outer `n`=100;
/// x=2..4 → arm 2 binds the scrutinee → 4,6,8. The timeout keeps a
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

/// #167 under the PURE NODE WALK (`CFlag::FusionDisabled`). The node-walk
/// interpreter is graphix's canonical execution model and must be correct
/// independently of fusion — MapQ builds the per-slot graph via
/// `clone_rebind` even with fusion off, so this exercises the same
/// `Select::clone_rebind` scope fix. Asserts `[100,4,6,8]` with a timeout
/// so a regression fails rather than hangs the suite.
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

// ─── env-accounting invariant (design/clone_rebind_testing.md #2) ─────
//
// The clone↔delete symmetry nag: every per-slot grow mints bindings
// (MapQ's `bind_variable("x")` + the cloned template's internal
// bindings + their runtime ref-var edges); every shrink runs
// `Slot::delete` (`pred.delete` + `unbind_variable`). If delete fails
// to fully reverse clone_rebind, `env.by_id` and/or the runtime
// `by_ref` registry grow without bound in a long-lived reactive
// program — a leak nothing turns red for. This drives an impure HOF
// array up and down repeatedly and asserts the registries return to
// the SAME size at the bottom of every cycle. A per-cycle leak shows
// as growth; the peak-vs-bottom assert proves the slots actually
// allocate (so a trivially-passing "nothing leaks because nothing
// binds" can't hide). Uses `GXHandle::env_stats` (the introspection
// hook this effort added) and drives `arr` by BindId via `set`, so
// the measurement apparatus mints no bindings of its own.

/// Drain `rx` until the map at `eid` emits an array of `target` length
/// (its length is fixed by the input array length the instant the set
/// lands — the per-element values settle later, but length is
/// immediate — so this is a clean "the grow/shrink cycle completed"
/// signal). Times out after 5s.
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

/// Build `Value::Array([1, 2, …, n])`.
fn iota(n: i64) -> Value {
    let v: Vec<Value> = (1..=n).map(Value::I64).collect();
    Value::Array(netidx_value::ValArray::from(v))
}

#[tokio::test(flavor = "current_thread")]
async fn env_accounting_grow_shrink() -> Result<()> {
    use graphix_compiler::expr::ModPath;
    use graphix_compiler::Scope;

    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;

    // Bind `counter` (the `<-` target that makes the callback impure →
    // clone path) and `arr` (the reactive input we drive by id) at root
    // scope. Keep `_first` alive so its `CompExp::drop`-triggered Delete
    // doesn't race the second compile (see compile_then_compile_*).
    let _first = ctx
        .rt
        .compile(ArcStr::from("let counter = 0; let arr: Array<i64> = [];"))
        .await?;
    let res = ctx
        .rt
        .compile(ArcStr::from(
            "array::map(arr, |x| { let v = x * 2 + 1; counter <- v; v })",
        ))
        .await?;
    let eid = res.exprs[0].id;

    // Resolve `arr`'s BindId from the env (no Ref node compiled — that
    // would itself mint a binding + ref and skew the baseline).
    let env = ctx.rt.get_env().await?;
    let scope = Scope::root();
    let arr_id = env
        .lookup_bind(&scope.lexical, &ModPath::from_iter(["arr"]))
        .ok_or_else(|| anyhow::anyhow!("arr not in scope"))?
        .1
        .id;

    const N: i64 = 4;
    const CYCLES: usize = 4;

    let mut bottoms = Vec::new();
    let mut peak = None;
    for _ in 0..CYCLES {
        // grow → N slots
        ctx.rt.set(arr_id, iota(N))?;
        await_map_len(&mut rx, eid, N as usize).await?;
        peak = Some(ctx.rt.env_stats().await?);
        // shrink → 0 slots
        ctx.rt.set(
            arr_id,
            Value::Array(netidx_value::ValArray::from_iter_exact(
                std::iter::empty(),
            )),
        )?;
        await_map_len(&mut rx, eid, 0).await?;
        bottoms.push(ctx.rt.env_stats().await?);
    }

    ctx.shutdown().await;

    // Every bottom-of-cycle snapshot (arr=[], 0 slots) must be
    // identical: a per-cycle clone↔delete asymmetry would show as
    // growth across cycles. The one-time template build persists across
    // all cycles, so it's present in every snapshot and cancels.
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

    // The test is only meaningful if the slots actually allocate env
    // bindings — otherwise the invariant holds trivially. Peak (N slots)
    // must exceed the bottom.
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

// ─── env-node-in-callback fixtures (design/clone_rebind_testing.md #3) ─
//
// The recompile-default `clone_rebind` (lib.rs) is the ONLY path that
// `alias_variable`-pollutes the clone scope's name map (nag #1). It
// fires for the env/reference nodes with no structural override —
// TryCatch, Sample (~), ByRef/Deref (&/*), ConnectDeref, Module,
// Use/TypeDef. Each fixture below plants one of those inside an impure
// callback (the `counter <- x` in `clone_map` forces the per-slot clone
// path) AND captures the outer binding `k`, so the recompile-default
// must (a) re-resolve the capture correctly per slot and (b) not let
// the alias persist into a sibling slot's resolution. A wrong value —
// or cross-slot contamination — means a bug. The job is to break it.

/// TryCatch whose CATCH handler FIRES and captures both the element `x`
/// and the outer `k`. The catch is side-effect-only (a direct-value
/// catch surfaces nothing — try-catch evaluates to the try body's value,
/// which is `never` when all-error), so the firing path is observed the
/// way every real test does it (cf. CHECKED_DIV0 in lang/errors.rs): the
/// catch drives `res <- x + k`. `res = never()` gates the slot's output
/// until the catch settles, so the map emits `[4,5,6,7]` in one shot
/// (no racy intermediate). Exercises the cloned catch handler RUNNING +
/// a Connect inside it + capture of `k` AND `x`.
#[tokio::test(flavor = "current_thread")]
async fn clone_trycatch_catch_capture() -> Result<()> {
    assert_i64s(
        &clone_map(
            "let res = never(); \
             try (0 /? 0)? catch(e) => res <- (x + k); \
             res",
        )
        .await?,
        &[4, 5, 6, 7],
    )
}

/// TryCatch whose TRY body captures `k` and does NOT error (no `?`
/// propagates), so the try value `x / k` is returned every slot.
#[tokio::test(flavor = "current_thread")]
async fn clone_trycatch_try_capture() -> Result<()> {
    assert_i64s(
        &clone_map("try (x / k) catch(e) => -1").await?,
        &[0, 0, 1, 1],
    )
}

/// Sample `x ~ k`: emit `k`'s current value when the element `x` fires.
/// NOT soundly fusable (k's external refs ⊄ x's), so it stays a
/// structural `Sample` node → recompile-default. Captures `k` on the
/// RHS → `[3,3,3,3]`.
#[tokio::test(flavor = "current_thread")]
async fn clone_sample_capture() -> Result<()> {
    assert_i64s(&clone_map("x ~ k").await?, &[3, 3, 3, 3])
}

/// ByRef + Deref capturing `k`: `&k` builds a reference to the captured
/// binding, `*r` reads it. Neither has a structural `clone_rebind`, so
/// both ride the recompile-default. `*r + x` → `[4,5,6,7]`.
#[tokio::test(flavor = "current_thread")]
async fn clone_byref_deref_capture() -> Result<()> {
    assert_i64s(&clone_map("let r = &k; *r + x").await?, &[4, 5, 6, 7])
}

// ─── proptest swarm (design/clone_rebind_testing.md #4) ───────────────
//
// The ceiling test: generate random valid i64-valued callback bodies
// over the element `x` and the capture `k`, and assert the CLONE path
// (`clone_map`, impure → per-slot clone_rebind of a fused template)
// produces the SAME array as the non-clone REFERENCE path (`pure_map`,
// region-fuse or fresh per-slot interpreted CallSite). Any structural
// clone that drops a child, swaps a field, or loses a capture under
// some composition makes the two disagree; proptest shrinks to a
// minimal offending body. The grammar is closed over total i64
// operations (+, -, *, literal-pattern select, tuple/struct accessors)
// so every generated body typechecks and evaluates without error — no
// division (zero), no array index (out-of-bounds), no string.

/// Random single-expression i64 body over `x`, `k`, and small literals.
fn body_strategy() -> impl proptest::strategy::Strategy<Value = String> {
    use proptest::prelude::*;
    let leaf = prop_oneof![
        Just("x".to_string()),
        Just("k".to_string()),
        (0i64..5).prop_map(|n| n.to_string()),
    ];
    leaf.prop_recursive(4, 48, 8, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({a} + {b})")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({a} - {b})")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({a} * {b})")),
            // literal-pattern select (`_` makes it exhaustive)
            (inner.clone(), inner.clone(), inner.clone()).prop_map(
                |(a, b, c)| format!("(select {a} {{ 0 => {b}, _ => {c} }})")
            ),
            // arm-BINDING select — exercises the #162 fix: the catch-all
            // `q =>` binds the scrutinee, and the body reads it (`q +
            // c`). `q` is a fresh name (never x/k and never referenced by
            // a sibling arm), so it can't hit the #167 shadow-hang. Both
            // the clone and reference paths must resolve `q` to the
            // scrutinee value identically.
            (inner.clone(), inner.clone(), inner.clone()).prop_map(
                |(a, b, c)| format!("(select {a} {{ 0 => {b}, q => (q + {c}) }})")
            ),
            // tuple + accessor — field access needs a binding first
            // (`(a,b).0` is a parse error), so emit a block-expr; nesting
            // shadows `p`, which is valid and extra coverage.
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let p = ({a}, {b}); p.0 }})")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("({{ let p = ({a}, {b}); p.1 }})")),
            // struct + accessor — same block-expr shape via `s`.
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!(
                "({{ let s = {{f: {a}, g: {b}}}; s.f }})"
            )),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!(
                "({{ let s = {{f: {a}, g: {b}}}; s.g }})"
            )),
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

// #124: the third test axis — assert on the *compiled artifact*, not
// just the value (run!) or whether fusion fired (FuseExpect). A
// `NodeShape` declares what the graph should look like; the runtime
// checks it against the live post-fusion graph and returns a verdict.
// `foo * 6` (foo external) is the canonical region fusion case — `foo`
// lifts to a kernel input and the Mul splices a FusedKernel (see
// `load_uses_external_scalar`).
#[tokio::test(flavor = "current_thread")]
async fn node_shape_external_scalar() -> Result<()> {
    use graphix_compiler::fusion::kernel_abi::{prim_type, PrimType};
    use graphix_compiler::node_shape::{KernelMatcher, NodeShape};

    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let _first = ctx.rt.compile(ArcStr::from("let foo = 7;")).await?;
    let res = ctx.rt.compile(ArcStr::from("foo * 6")).await?;
    let eid = res.exprs[0].id;

    // The whole expression fuses into one kernel: scalar input `foo`,
    // returns i64, body multiplies (a `Bin` op).
    let spec = NodeShape::fused(
        KernelMatcher::new()
            .returns(prim_type(PrimType::I64))
            .params(&["foo"]),
        // F4 (#213): the binary-op tag pin removed at the F2
        // flip — direct kernels carry no op-tag metadata to tag-match;
        // restore as an EmitTag assertion.
    );
    ctx.rt.match_shape(eid, spec).await?;

    // The matcher must have teeth: a wrong criterion (the kernel's
    // one param is `foo`, not `nope`) must produce a mismatch, not
    // silently pass.
    let bad = NodeShape::fused(KernelMatcher::new().params(&["nope"]));
    let err = ctx.rt.match_shape(eid, bad).await;
    assert!(err.is_err(), "matcher should reject a wrong spec, but passed");

    // And asserting it's a plain (non-fused) node must also fail —
    // the root really is a Fused kernel.
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
    // A file whose last statement is a Bind. The synth-Do wraps
    // around the file, the last child (a Bind Node) returns None
    // from its update, so the wrap-Do's update also returns None
    // and the runtime never emits an Updated event. `load()`
    // reports this via `output: false` on the returned CompExp.
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let res = ctx
        .rt
        .load(Source::Internal(ArcStr::from("let x = 5")))
        .await?;
    let comp =
        res.exprs.first().ok_or_else(|| anyhow::anyhow!("no comp expr"))?;
    assert!(!comp.output, "let-only file should have output=false");
    ctx.shutdown().await;
    Ok(())
}
