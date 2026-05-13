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
) -> Result<Value> {
    let cfg = FusionConfig { fusion_disabled, jit_mode: jit };
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

/// Run `code` under Mode A / B / C and assert all three produce the
/// same Value. Returns the Value (Mode A's, which is ground truth)
/// on success. `label` shows up in failure messages.
async fn assert_modes_agree(label: &str, code: &str) -> Result<Value> {
    let a = eval_under(code, true, JitMode::Off).await
        .map_err(|e| anyhow!("[{label}] mode A (no fusion) failed: {e}"))?;
    let b = eval_under(code, false, JitMode::Off).await
        .map_err(|e| anyhow!("[{label}] mode B (fusion, no JIT) failed: {e}"))?;
    let c = eval_under(code, false, JitMode::Sync).await
        .map_err(|e| anyhow!("[{label}] mode C (fusion + JIT) failed: {e}"))?;
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

    Ok(())
}
