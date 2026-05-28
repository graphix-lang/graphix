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
    // `GirOp::QopUnwrap` which extracts the success i64. JIT must
    // fire because there's no Block-with-let wrapper here.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::gir_jit_helpers::reset_jit_invocations();
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
    let inv = graphix_compiler::gir_jit_helpers::jit_invocations();
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
    graphix_compiler::gir_jit_helpers::reset_jit_invocations();
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
    let inv = graphix_compiler::gir_jit_helpers::jit_invocations();
    assert!(inv > 0, "JIT_INVOCATIONS=0 — variadic and didn't run via JIT");
    ctx.shutdown().await;
    Ok(())
}

#[cfg(debug_assertions)]
#[tokio::test(flavor = "current_thread")]
async fn load_array_literal_jits() -> Result<()> {
    // `[1, 2, 3]` as a program body — exercises `ExprKind::Array`
    // lowering via the new `emit_array_new` path, which reuses
    // `GirOp::TupleNew` and tags the outer GirType as `Array(I64)`.
    // Counter > 0 proves the JIT'd kernel ran.
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    graphix_compiler::gir_jit_helpers::reset_jit_invocations();
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
    let inv = graphix_compiler::gir_jit_helpers::jit_invocations();
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
/// kernel issues a `GirOp::DynCall` that dispatches into the
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
    graphix_compiler::gir_jit_helpers::reset_jit_invocations();
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
    let inv = graphix_compiler::gir_jit_helpers::jit_invocations();
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
    graphix_compiler::gir_jit_helpers::reset_jit_invocations();
    assert_eq!(graphix_compiler::gir_jit_helpers::jit_invocations(), 0);
    let v = load_and_await("3 * 4 + 5").await?;
    assert_eq!(v, Value::I64(17));
    let inv = graphix_compiler::gir_jit_helpers::jit_invocations();
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
