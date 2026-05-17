// Cross-mode differential tests for kernel fusion.
//
// For each fixture snippet we compile + evaluate the program three
// times in series, with the process-global fusion/JIT knobs flipped:
//
//   Mode A: fusion disabled. Every lambda runs as GXLambda (the
//           regular node-graph interpreter). Ground truth.
//   Mode B: fusion enabled, JIT off. Fused kernels dispatch through
//           kir_interp.
//   Mode C: fusion enabled, JIT sync. Fused kernels dispatch through
//           cranelift-emitted native code where possible; the
//           runtime silently falls back to kir_interp for kernels
//           the JIT doesn't support yet (composite producers,
//           composite-arg kernels, etc.).
//
// All three must produce the same Value. Any disagreement is a real
// bug — either in the fusion lowering, in kir_interp's op
// implementations, or in the JIT codegen.
//
// Tests are intentionally sequential (one big #[tokio::test])
// because the knobs live in process-global atomics. Parallel tests
// would race.
//
// Add fixtures here as kir_interp learns more ops. Today's coverage
// is restricted to consumer-only composite kernels (tuple/struct/
// variant inputs reading scalars out). Producer ops (TupleNew,
// StructNew, VariantNew, ArrayInit, ArrayMap, ArrayFilter) still
// panic in kir_interp — fixtures that exercise them belong with
// step 2 of the M9 plan.

use anyhow::{anyhow, Result};
use graphix_compiler::{FusionConfig, JitMode};
use netidx::publisher::Value;

/// Evaluate `code` under the given fusion/JIT mode and return the
/// produced Value. The config is applied to this evaluation's own
/// ExecCtx via `eval_with_setup` — no global state, so concurrent
/// runtimes in the same process don't race.
async fn eval_under(
    code: &str,
    fusion_disabled: bool,
    jit: JitMode,
    whole_graph: bool,
) -> Result<Value> {
    let cfg = FusionConfig { fusion_disabled, jit_mode: jit, whole_graph };
    let (v, ctx) = graphix_package_core::testing::eval_with_setup(
        code,
        &crate::TEST_REGISTER,
        move |ctx| {
            ctx.fusion_config = cfg;
        },
    )
    .await?;
    ctx.shutdown().await;
    Ok(v)
}

/// Run `code` under Mode A / B / C / D and assert all four produce
/// the same Value. Returns Mode A's value (ground truth). `label`
/// shows up in failure messages.
///
/// Modes:
/// - **A** (`fusion_disabled: true`, no JIT, no whole-graph): every
///   lambda runs as `GXLambda`. Ground truth.
/// - **B** (lazy fusion, no JIT, no whole-graph): fused kernels
///   dispatch through `kir_interp`.
/// - **C** (lazy fusion, sync JIT, no whole-graph): fused kernels
///   dispatch through cranelift-emitted native code.
/// - **D** (lazy fusion, sync JIT, whole-graph on): M8.4
///   `analyze_program` runs and any region with a buildable kernel
///   gets a `FusedRegion` node spliced in. Mode D should be a
///   strict superset of Mode C's fusion — anything that fused in C
///   should produce the same result in D, plus regions that the
///   per-lambda lazy path couldn't see (cross-lambda chains).
async fn assert_modes_agree(label: &str, code: &str) -> Result<Value> {
    let a = eval_under(code, true, JitMode::Off, false).await
        .map_err(|e| anyhow!("[{label}] mode A (no fusion) failed: {e}"))?;
    let b = eval_under(code, false, JitMode::Off, false).await
        .map_err(|e| anyhow!("[{label}] mode B (fusion, no JIT) failed: {e}"))?;
    let c = eval_under(code, false, JitMode::Sync, false).await
        .map_err(|e| anyhow!("[{label}] mode C (fusion + JIT) failed: {e}"))?;
    let d = eval_under(code, false, JitMode::Sync, true).await
        .map_err(|e| anyhow!("[{label}] mode D (whole-graph) failed: {e}"))?;
    if a != b {
        return Err(anyhow!(
            "[{label}] mode A != mode B:\n  A: {a:?}\n  B: {b:?}"
        ));
    }
    if b != c {
        return Err(anyhow!(
            "[{label}] mode B != mode C:\n  B: {b:?}\n  C: {c:?}"
        ));
    }
    if c != d {
        return Err(anyhow!(
            "[{label}] mode C != mode D:\n  C: {c:?}\n  D: {d:?}"
        ));
    }
    Ok(a)
}

#[tokio::test(flavor = "current_thread")]
async fn cross_mode_consumer_only() -> Result<()> {
    // dist2: two tuple inputs, scalar arithmetic body.
    let v = assert_modes_agree(
        "tuple_dist2",
        "{
            let dist2 = |a: (f64, f64), b: (f64, f64)| -> f64
                (a.0 - b.0) * (a.0 - b.0) + (a.1 - b.1) * (a.1 - b.1);
            dist2((1.0, 2.0), (4.0, 6.0))
        }",
    ).await?;
    assert!(matches!(v, Value::F64(x) if (x - 25.0).abs() < 1e-9), "got {v:?}");

    // manhattan: struct input with two f64 fields. Exercises the
    // kv-pair struct layout that earlier broke fusion (we read flat
    // indices instead of the [name, value] subarrays).
    let v = assert_modes_agree(
        "struct_manhattan",
        "{
            let manhattan = |d: {dx: f64, dy: f64}| -> f64 {
                let ax = select d.dx > 0.0 { true => d.dx, false => 0.0 - d.dx };
                let ay = select d.dy > 0.0 { true => d.dy, false => 0.0 - d.dy };
                ax + ay
            };
            manhattan({dx: -3.0, dy: 4.0})
        }",
    ).await?;
    assert!(matches!(v, Value::F64(x) if (x - 7.0).abs() < 1e-9), "got {v:?}");

    // Variant with payload: each arm extracts payload slots.
    let v = assert_modes_agree(
        "variant_classify_add",
        "{
            let classify = |v: [`Add(i64, i64), `Neg(i64)]| -> i64
                select v {
                    `Add(a, b) => a + b,
                    `Neg(n) => 0 - n
                };
            classify(`Add(3, 4))
        }",
    ).await?;
    assert_eq!(v, Value::I64(7));

    let v = assert_modes_agree(
        "variant_classify_neg",
        "{
            let classify = |v: [`Add(i64, i64), `Neg(i64)]| -> i64
                select v {
                    `Add(a, b) => a + b,
                    `Neg(n) => 0 - n
                };
            classify(`Neg(7))
        }",
    ).await?;
    assert_eq!(v, Value::I64(-7));

    // Nullary variant: runtime stores as Value::String(tag), not
    // Value::Array. Kernel must dispatch on Value shape, not assume
    // ValArray.
    let v = assert_modes_agree(
        "variant_nullary_red",
        "{
            let intensity = |c: [`Red, `Green, `Blue]| -> i64
                select c {
                    `Red => 255,
                    `Green => 128,
                    `Blue => 64
                };
            intensity(`Red)
        }",
    ).await?;
    assert_eq!(v, Value::I64(255));

    let v = assert_modes_agree(
        "variant_nullary_blue",
        "{
            let intensity = |c: [`Red, `Green, `Blue]| -> i64
                select c {
                    `Red => 255,
                    `Green => 128,
                    `Blue => 64
                };
            intensity(`Blue)
        }",
    ).await?;
    assert_eq!(v, Value::I64(64));

    // Mixed variant: some cases have payloads, some are nullary.
    let v = assert_modes_agree(
        "variant_mixed_unit",
        "{
            let area = |s: [`Unit, `Square(i64), `Rect(i64, i64)]| -> i64
                select s {
                    `Unit => 0,
                    `Square(side) => side * side,
                    `Rect(w, h) => w * h
                };
            area(`Unit)
        }",
    ).await?;
    assert_eq!(v, Value::I64(0));

    // Direct ArrayGet + ArrayLen — the kernel takes an Array<i64>
    // input and reads/indexes via the JIT runtime helpers. Doesn't
    // use any producer ops, so this exercises the JIT composite-arg
    // path (Mode C) end-to-end. The outer builder runs at the
    // graphix runtime layer, so the array is a regular ValArray
    // when it reaches the kernel.
    let v = assert_modes_agree(
        "array_get_len_via_jit",
        "{
            use array;
            let head_plus_len: fn(xs: Array<i64>) -> i64 =
                |xs: Array<i64>| -> i64
                    xs[0]$ + cast<i64>(array::len(xs))$;
            head_plus_len(array::init(5, |i: i64| i))
        }",
    ).await?;
    // [0,1,2,3,4], head=0, len=5 → 5
    assert_eq!(v, Value::I64(5));

    let v = assert_modes_agree(
        "variant_mixed_rect",
        "{
            let area = |s: [`Unit, `Square(i64), `Rect(i64, i64)]| -> i64
                select s {
                    `Unit => 0,
                    `Square(side) => side * side,
                    `Rect(w, h) => w * h
                };
            area(`Rect(3, 7))
        }",
    ).await?;
    assert_eq!(v, Value::I64(21));

    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn cross_mode_producer_ops() -> Result<()> {
    // ArrayInit: build a flat f64 array of computed values, fold to
    // a scalar. Tests the ArrayInit producer + ArrayFold consumer in
    // one kernel chain.
    let v = assert_modes_agree(
        "array_init_fold",
        "{
            let dot = |n: i64| -> f64 {
                let xs = array::init(n, |i: i64| cast<f64>(i)$);
                array::fold(xs, 0.0, |acc: f64, x: f64| acc + x)
            };
            dot(10)
        }",
    ).await?;
    // 0+1+2+...+9 = 45
    assert!(matches!(v, Value::F64(x) if (x - 45.0).abs() < 1e-9), "got {v:?}");

    // ArrayMap: map over an input array, fold the result.
    let v = assert_modes_agree(
        "array_map_then_fold",
        "{
            let times2_then_sum: fn(xs: Array<i64>) -> i64 =
                |xs: Array<i64>| -> i64 {
                    let doubled = array::map(xs, |x: i64| -> i64 x * 2);
                    array::fold(doubled, 0, |acc: i64, x: i64| acc + x)
                };
            times2_then_sum(array::init(5, |i: i64| i + 1))
        }",
    ).await?;
    // [1,2,3,4,5] *2 → [2,4,6,8,10] sum → 30
    assert_eq!(v, Value::I64(30));

    // ArrayFilter: keep even elements, count them.
    let v = assert_modes_agree(
        "array_filter_count",
        "{
            let evens_count: fn(xs: Array<i64>) -> i64 =
                |xs: Array<i64>| -> i64 {
                    let kept = array::filter(xs, |x: i64| -> bool x % 2 == 0);
                    cast<i64>(array::len(kept))$
                };
            evens_count(array::init(10, |i: i64| i))
        }",
    ).await?;
    // [0..10] → keep evens [0,2,4,6,8] → count 5
    assert_eq!(v, Value::I64(5));

    // HOF param dispatch through JIT (scalar return).
    // `compose(f, x)` takes a fn-typed param `f` and calls it; this
    // produces a `KirOp::DynCall { fn_index, ret=Prim(I64) }` site
    // in the JIT'd body. `square` is passed as the HOF arg, so
    // FnSource::Param.
    let v = assert_modes_agree(
        "dyncall_scalar_hof_param",
        "{
            let compose: fn(f: fn(x: i64) -> i64, x: i64) -> i64 =
                |f: fn(x: i64) -> i64, x: i64| -> i64 f(x) + 1;
            let square: fn(x: i64) -> i64 = |x: i64| -> i64 x * x;
            compose(square, 5)
        }",
    ).await?;
    // square(5) + 1 = 26
    assert_eq!(v, Value::I64(26));

    // HOF as Binding source: `helper` is non-fusable (uses an
    // array literal that fusion can't lower), so the outer kernel
    // resolves it via FnSource::Binding and dispatches through
    // graphix_dyncall.
    let v = assert_modes_agree(
        "dyncall_scalar_hof_binding",
        "{
            use array;
            let helper: fn(x: i64) -> i64 =
                |x: i64| -> i64
                    array::fold([x, x], 0, |a: i64, b: i64| a + b * b);
            let outer: fn(x: i64) -> i64 =
                |x: i64| -> i64 helper(x) + 1;
            outer(5)
        }",
    ).await?;
    // helper(5) = 5*5 + 5*5 = 50; outer = 50 + 1 = 51
    assert_eq!(v, Value::I64(51));

    // TupleNew + TupleGet round-trip via a tail-recursive kernel
    // that builds a fresh tuple each iteration.
    let v = assert_modes_agree(
        "tuple_new_tail_call",
        "{
            let rec iter = |t: (i64, i64), n: i64| -> i64
                select n {
                    0 => t.0 + t.1,
                    _ => iter((t.0 + 1, t.1 * 2), n - 1)
                };
            iter((0, 1), 5)
        }",
    ).await?;
    // t evolves: (0,1) → (1,2) → (2,4) → (3,8) → (4,16) → (5,32), sum=37
    assert_eq!(v, Value::I64(37));

    // StructNew + StructGet
    let v = assert_modes_agree(
        "struct_new_roundtrip",
        "{
            let make = |a: i64, b: i64| -> {x: i64, y: i64} {x: a, y: b};
            let read = |p: {x: i64, y: i64}| -> i64 p.x * 10 + p.y;
            read(make(3, 7))
        }",
    ).await?;
    assert_eq!(v, Value::I64(37));

    // VariantNew (with-payload) + pattern match
    let v = assert_modes_agree(
        "variant_new_with_payload",
        "{
            let wrap = |n: i64| -> [`Pos(i64), `Neg(i64)]
                select n > 0 {
                    true => `Pos(n),
                    false => `Neg(0 - n)
                };
            let unwrap = |v: [`Pos(i64), `Neg(i64)]| -> i64
                select v {
                    `Pos(x) => x,
                    `Neg(x) => 0 - x
                };
            unwrap(wrap(-5))
        }",
    ).await?;
    assert_eq!(v, Value::I64(-5));

    // DynCall to a non-fused helper that itself contains a multi-
    // cycle fold. Triggers the eager-init path on the binding-source
    // fn_param so the inner Apply's `ref_var` subscriptions get
    // registered before the parent kernel's first update — without
    // this the parent kernel hangs (we caught it via the harness).
    let v = assert_modes_agree(
        "dyncall_to_non_fused_fold",
        "{
            use array;
            let helper: fn(x: i64) -> i64 =
                |x: i64| -> i64
                    array::fold([x, x], 0, |a: i64, b: i64| a + b * b);
            let outer: fn(x: i64) -> i64 =
                |x: i64| -> i64 helper(x) + 1;
            outer(5)
        }",
    ).await?;
    assert_eq!(v, Value::I64(51));

    // VariantNew (nullary) — picked through a non-nullary kernel
    // so the construction is the producer op, not a literal.
    let v = assert_modes_agree(
        "variant_new_nullary",
        "{
            let pick = |n: i64| -> [`Red, `Green, `Blue]
                select n {
                    0 => `Red,
                    1 => `Green,
                    _ => `Blue
                };
            let intensity = |c: [`Red, `Green, `Blue]| -> i64
                select c { `Red => 255, `Green => 128, `Blue => 64 };
            intensity(pick(0))
        }",
    ).await?;
    assert_eq!(v, Value::I64(255));

    // Composite-return DynCall: HOF param `g` returns a tuple. The
    // `g(x)` site is a `KirOp::DynCall { ret=Tuple(..) }` — the JIT
    // marshals the result as an owned `*mut ValArray`, binds it as a
    // composite local, then reads it via TupleGet.
    let v = assert_modes_agree(
        "dyncall_tuple_return_hof",
        "{
            let mk: fn(n: i64) -> (i64, i64) =
                |n: i64| -> (i64, i64) (n, n * 2);
            let sp: fn(g: fn(n: i64) -> (i64, i64), x: i64) -> i64 =
                |g: fn(n: i64) -> (i64, i64), x: i64| -> i64 {
                    let p = g(x);
                    p.0 + p.1
                };
            sp(mk, 10)
        }",
    ).await?;
    // mk(10) = (10, 20); p.0 + p.1 = 30
    assert_eq!(v, Value::I64(30));

    // Composite-return DynCall: HOF param returns a struct, consumed
    // via StructGet (the kv-pair layout).
    let v = assert_modes_agree(
        "dyncall_struct_return_hof",
        "{
            let mk: fn(a: i64) -> {x: i64, y: i64} =
                |a: i64| -> {x: i64, y: i64} {x: a, y: a + 1};
            let rd: fn(g: fn(a: i64) -> {x: i64, y: i64}, n: i64) -> i64 =
                |g: fn(a: i64) -> {x: i64, y: i64}, n: i64| -> i64 {
                    let s = g(n);
                    s.x * 100 + s.y
                };
            rd(mk, 7)
        }",
    ).await?;
    // mk(7) = {x:7, y:8}; 7*100 + 8 = 708
    assert_eq!(v, Value::I64(708));

    // Composite-return DynCall: HOF param returns a variant. The
    // `f(x)` site is `KirOp::DynCall { ret=Variant(..) }` — the JIT
    // marshals the result as an owned `*mut Value`, binds it as a
    // variant local, then matches it.
    let v = assert_modes_agree(
        "dyncall_variant_return_hof",
        "{
            let wrap: fn(n: i64) -> [`Pos(i64), `Neg(i64)] =
                |n: i64| -> [`Pos(i64), `Neg(i64)]
                    select n > 0 {
                        true => `Pos(n),
                        false => `Neg(0 - n)
                    };
            let ah: fn(
                f: fn(n: i64) -> [`Pos(i64), `Neg(i64)],
                x: i64
            ) -> i64 =
                |f: fn(n: i64) -> [`Pos(i64), `Neg(i64)], x: i64| -> i64 {
                    let v = f(x);
                    select v {
                        `Pos(a) => a,
                        `Neg(a) => 0 - a
                    }
                };
            ah(wrap, -5)
        }",
    ).await?;
    // wrap(-5) = `Neg(5); select → 0 - 5 = -5
    assert_eq!(v, Value::I64(-5));

    // Composite-ARG DynCall: HOF param `f` takes a tuple. The tuple
    // is bound to a local first (`t`), so the DynCall arg push is the
    // borrow-mode path — `t` stays owned and is dropped at function
    // exit.
    let v = assert_modes_agree(
        "dyncall_tuple_arg_hof",
        "{
            let sum: fn(p: (i64, i64)) -> i64 =
                |p: (i64, i64)| -> i64 p.0 + p.1;
            let app: fn(
                f: fn(p: (i64, i64)) -> i64,
                a: i64,
                b: i64
            ) -> i64 =
                |f: fn(p: (i64, i64)) -> i64, a: i64, b: i64| -> i64 {
                    let t = (a, b);
                    f(t) * 2
                };
            app(sum, 3, 4)
        }",
    ).await?;
    // sum((3, 4)) = 7; 7 * 2 = 14
    assert_eq!(v, Value::I64(14));

    // Composite-ARG DynCall, owned source: the tuple is constructed
    // inline at the call site (`f((a, b))`), so the DynCall arg is a
    // fresh owned `TupleNew` result — the JIT must move it into the
    // args buf rather than refcount-bump (which would leak the
    // original).
    let v = assert_modes_agree(
        "dyncall_inline_tuple_arg_hof",
        "{
            let sum: fn(p: (i64, i64)) -> i64 =
                |p: (i64, i64)| -> i64 p.0 + p.1;
            let app: fn(
                f: fn(p: (i64, i64)) -> i64,
                a: i64,
                b: i64
            ) -> i64 =
                |f: fn(p: (i64, i64)) -> i64, a: i64, b: i64| -> i64
                    f((a, b)) * 2;
            app(sum, 5, 6)
        }",
    ).await?;
    // sum((5, 6)) = 11; 11 * 2 = 22
    assert_eq!(v, Value::I64(22));

    // Composite let inside a nested block EXPRESSION (`KirOp::Block`,
    // not statement-level lets). The JIT must route the tuple let
    // through `bind_composite`, scope it to the block, and drop it on
    // block exit — the old code `prim_of`-panicked on it.
    let v = assert_modes_agree(
        "block_expr_composite_let",
        "{
            let f: fn(a: i64, b: i64) -> i64 =
                |a: i64, b: i64| -> i64 {
                    let r = { let t = (a, b); t.0 + t.1 };
                    r * 2
                };
            f(3, 4)
        }",
    ).await?;
    // { let t = (3,4); t.0 + t.1 } = 7; 7 * 2 = 14
    assert_eq!(v, Value::I64(14));

    // Nested block EXPRESSION whose tail is itself a composite — the
    // block must `ensure_owned_composite` the tail so the value
    // outlives the block-scoped local it aliases.
    let v = assert_modes_agree(
        "block_expr_composite_tail",
        "{
            let g: fn(a: i64) -> i64 = |a: i64| -> i64 {
                let p = { let inner = (a, a + 1); inner };
                p.0 * 10 + p.1
            };
            g(5)
        }",
    ).await?;
    // inner = (5, 6); p.0*10 + p.1 = 56
    assert_eq!(v, Value::I64(56));

    // `select` at expression position with tuple arms lowers to a
    // composite-typed `KirOp::IfChain`. The JIT used to `prim_of`-
    // panic on the merge-block param type; it now widens to a pointer
    // and runs each arm through `ensure_owned_composite`.
    let v = assert_modes_agree(
        "ifchain_expr_composite_tuple",
        "{
            let h: fn(n: i64) -> i64 = |n: i64| -> i64 {
                let pick: (i64, i64) =
                    select n > 0 {
                        true => (n, n * 2),
                        false => (n - n, n)
                    };
                pick.0 + pick.1
            };
            h(7) + h(0 - 3)
        }",
    ).await?;
    // h(7): (7,14) → 21; h(-3): (0,-3) → -3; total 18
    assert_eq!(v, Value::I64(18));

    // Composite `IfChain` whose arms disagree on ownership: one arm is
    // a `Local` (Borrowed — aliases an outer composite param), the
    // other an inline `TupleNew` (Owned). Each arm must be made owned
    // before the merge so the merged pointer has a single, consistent
    // owner.
    let v = assert_modes_agree(
        "ifchain_expr_composite_mixed_ownership",
        "{
            let j: fn(base: (i64, i64), n: i64) -> i64 =
                |base: (i64, i64), n: i64| -> i64 {
                    let chosen: (i64, i64) =
                        select n > 0 {
                            true => base,
                            false => (n, n)
                        };
                    chosen.0 + chosen.1
                };
            j((10, 20), 1) + j((10, 20), 0 - 1)
        }",
    ).await?;
    // j((10,20),1): base → 30; j((10,20),-1): (-1,-1) → -2; total 28
    assert_eq!(v, Value::I64(28));

    // Variant `select` at expression position → variant-typed
    // `KirOp::IfChain`.
    let v = assert_modes_agree(
        "ifchain_expr_variant",
        "{
            let k: fn(n: i64) -> i64 = |n: i64| -> i64 {
                let v: [`Pos(i64), `Neg(i64)] =
                    select n > 0 {
                        true => `Pos(n),
                        false => `Neg(0 - n)
                    };
                select v { `Pos(a) => a, `Neg(a) => 0 - a }
            };
            k(8) + k(0 - 8)
        }",
    ).await?;
    // k(8) = 8; k(-8) = -8; total 0
    assert_eq!(v, Value::I64(0));

    Ok(())
}
