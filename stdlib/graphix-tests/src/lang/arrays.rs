// Tests for arrays: indexing, matching, operations

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::node_shape::{KernelMatcher, NodeShape};
use graphix_package_core::run;
use netidx::publisher::Value;

const ARRAY_INDEXING0: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[0]
}
"#;

run!(array_indexing0, ARRAY_INDEXING0, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; shape: NodeShape::contains_fused(KernelMatcher::new()));

// ── array[i] bounds-check seam (node-walk / JIT) ──
// `array[i]` is `[elem, Error<…>]`: out-of-bounds (or negative
// underflow) produces an `ArrayIndexError` rather than the element,
// via the shared `array_index`. These exercise every branch across all
// three backends (the `run!` modes) — the coverage whose absence let
// the JIT model `a[i]` as a bare scalar with no bounds check. The
// in-bounds cases pin the exact element value (catching an off-by-one
// in the negative-from-end math); the error cases pin `is_err` (before
// the fix, the JIT read garbage on these out-of-bounds reads
// without the check).

// positive index past the end → error
run!(
    array_index_oob_pos,
    r#"{ let a = [10, 20, 30]; a[10] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)))
);

// negative index: -1 is the last element
run!(
    array_index_neg_last,
    r#"{ let a = [10, 20, 30]; a[-1] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(30)))
);

// negative index: -2 is the second-to-last
run!(
    array_index_neg_mid,
    r#"{ let a = [10, 20, 30]; a[-2] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(20)))
);

// a[-len] reaches the first element (offset 0). Pinned so all three
// backends agree on the boundary between the last reachable negative
// index and underflow.
run!(
    array_index_neg_first,
    r#"{ let a = [10, 20, 30]; a[-3] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(10)))
);

// negative underflow past the start → error
run!(array_index_neg_underflow, r#"{ let a = [10, 20, 30]; a[-10] }"#, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::Error(_))
));

// the error can be recovered with `$` (drop-on-error → never) or `?`;
// here `is_err` over the option observes the error directly.
run!(array_index_is_err, r#"{ let a = [10, 20, 30]; is_err(a[10]) }"#, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::Bool(true))
));

const ARRAY_INDEXING1: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[0..3]
}
"#;

// `a[i..j]` (ArraySlice) lowers to the array-slice op (Nullable<Array>).
run!(array_indexing1, ARRAY_INDEXING1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(0), Value::I64(1), Value::I64(2)] =>
        true,
    _ => false,
});

const ARRAY_INDEXING2: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[..2]
}
"#;

// end-only slice `a[..j]`.
run!(array_indexing2, ARRAY_INDEXING2, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(0), Value::I64(1)] => true,
    _ => false,
});

const ARRAY_INDEXING3: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[5..]
}
"#;

// start-only slice `a[i..]`.
run!(array_indexing3, ARRAY_INDEXING3, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(5), Value::I64(6)] => true,
    _ => false,
});

const ARRAY_INDEXING4: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[..]
}
"#;

// unbounded slice `a[..]` (both bounds absent → full copy).
run!(array_indexing4, ARRAY_INDEXING4, |v: Result<&Value>| match v {
    Ok(Value::Array(a))
        if &a[..]
            == [
                Value::I64(0),
                Value::I64(1),
                Value::I64(2),
                Value::I64(3),
                Value::I64(4),
                Value::I64(5),
                Value::I64(6)
            ] =>
        true,
    _ => false,
});

// out-of-bounds slice → error (the `Nullable<Array>` error arm). All
// three backends route through the shared `array_slice`, so they must
// agree on the error.
run!(
    array_slice_oob,
    r#"{ let a = [0, 1, 2]; a[1..10] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)))
);

// the error flows into `is_err`, which also fuses+JITs.
run!(
    array_slice_oob_is_err,
    r#"{ let a = [0, 1, 2]; is_err(a[1..10]) }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

// Negative slice bound → error. The node-walk's `cast_to::<usize>()` wraps
// a negative i64 to usize::MAX (`i as usize`); `array_slice_i64` now does
// the same wrap, so all three backends route through the SAME `array_slice`
// with the SAME usize::MAX bound and produce the identical out-of-bounds
// error (rather than the fused path's old "expected a non negative number").
run!(
    array_slice_negative,
    r#"{ let a = [0, 1, 2]; let s = -1; a[s..] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)))
);

const ARRAY_INDEXING5: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  let out = select array::iter(a) {
    i64 as i => a[i] + 1
  };
  array::group(out, |i, x| i == 7)
}
"#;

run!(array_indexing5, ARRAY_INDEXING5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_INDEXING6: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  let out = select array::iter(a) {
    i64 as i => a[i]? + 1
  };
  array::group(out, |i, x| i == 7)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_indexing6, ARRAY_INDEXING6, |v: Result<&Value>| match v {
    Ok(Value::Array(a))
        if &a[..]
            == [
                Value::I64(1),
                Value::I64(2),
                Value::I64(3),
                Value::I64(4),
                Value::I64(5),
                Value::I64(6),
                Value::I64(7)
            ] =>
        true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_SLICE_NON_ARRAY: &str = r#"
  ("foo")[..]
"#;

run!(array_slice_non_array, ARRAY_SLICE_NON_ARRAY, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_INDEX_NON_ARRAY: &str = r#"
  ("foo")[0]
"#;

run!(array_index_non_array, ARRAY_INDEX_NON_ARRAY, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_MATCH0: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  select a {
    [a, b, c, d, ..] => a + b + c + d,
    _ => never()
  }
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_match0, ARRAY_MATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_MATCH1: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6, 7];
  let out = select a {
    [x, y, tl..] => {
      a <- tl;
      [x, y]
    },
    _ => never()
  };
  array::group(out, |i, x| i == 4)
}
"#;

run!(array_match1, ARRAY_MATCH1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        a.len() == 4 && {
            a.iter().enumerate().all(|(i, a)| match a {
                Value::Array(a) => {
                    a.len() == 2
                        && match &a[0] {
                            Value::I64(x) => *x as usize == i * 2,
                            _ => false,
                        }
                        && match &a[1] {
                            Value::I64(x) => *x as usize == i * 2 + 1,
                            _ => false,
                        }
                }
                _ => false,
            })
        }
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_MATCH2: &str = r#"
{
    let a = [];
    let b = [0, 1, 2, 3, 4, 5, 6];
    let r = select uniq(array::iter([a, a, a, b])) {
        [] => `Empty,
        _ => `Nonempty
    };
    array::group(r, |n, _| n == 2)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_match2, ARRAY_MATCH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[ArcStr; 2]>() {
        Ok([s0, s1]) if &*s0 == "Empty" && &*s1 == "Nonempty" => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// fold over an EMPTY but present array is the init (foldl identity), NOT
// bottom — a latent node-walk bug (only reachable via a reactively-empty
// array; `init(0)` / a fully-removing `filter`). Both modes must agree.
const FOLD_EMPTY: &str = r#"
{
    let xs = array::init(i64:0, |idx: i64| idx);
    array::fold(xs, i64:42, |acc, x| acc + x)
}
"#;

run!(fold_empty, FOLD_EMPTY, |v: Result<&Value>| matches!(v, Ok(Value::I64(42))));

// fold that REMOVES every element via filter then reduces — also empty,
// also the init.
const FOLD_FILTERED_EMPTY: &str = r#"
{
    let xs = array::filter(array::init(i64:5, |idx: i64| idx), |x| x > i64:99);
    array::fold(xs, i64:7, |acc, x| acc + x)
}
"#;

run!(fold_filtered_empty, FOLD_FILTERED_EMPTY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(7))
));

// find over an EMPTY array must yield Null in both evaluators. The
// node-walk's MapQ empty-input shortcut returned the projected empty
// collection (the array itself) without consulting FindImpl::finish —
// soak-jul06c B7, findings/hof-empty-input-jul2026.
const FIND_EMPTY: &str = r#"
array::find({let a: Array<i64> = []; a}, |x| true)
"#;

run!(find_empty, FIND_EMPTY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Null)
); graphix_package_core::testing::FuseExpect::Jit);

// An OVERSIZE array::init (> MAX_ARRAY_INIT_LEN) bottoms LOCALLY: the
// node-walk logs and emits nothing for the init while unrelated
// outputs in the same region still fire, and the JIT's runaway guard
// must match by riding the #219 taint (a tainted empty placeholder)
// instead of whole-kernel aborting — item 28's last residual, fixed
// after soak jul06h re-found it (findings/foldq-empty-overfire-
// jul2026/02).
const INIT_RUNAWAY_LOCAL_BOTTOM: &str = r#"
{
  array::init(i64:9223372036854775807, |idx: i64| f64:0.);
  array::fold([i64:2, i64:1, i64:10], i64:42, |acc, x| acc + x)
}
"#;

run!(init_runaway_local_bottom, INIT_RUNAWAY_LOCAL_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(55)))
}; graphix_package_core::testing::FuseExpect::Jit);

// ── composite / string fold ACCUMULATORS as native loops ─────────────
// The fold scaffold's acc was register-scalar-only; tuple/struct/array/
// string accs rode the per-slot FoldQ path. Now the loop OWNS a
// pointer-shaped acc (clone a borrowed init/body result, drop the
// replaced acc per iteration); strings are owned by read-clone.
// P4: `array::fold` is in-language — the call site DISPATCHES by
// node-walk (fn-typed args have no ABI) while the For loop fuses
// inside the per-site instance, so these are `fuse: Jit` (the
// instance kernel runs) without a `#[native]` pin. ASPIRE: inline the
// site-monomorphic instance body into the enclosing region (the fn
// arg is statically known per site) and restore the pins.

const FOLD_TUPLE_ACC: &str = r#"
array::fold([i64:1, i64:2, i64:3], (i64:0, i64:1), |(s, p), v| (s + v, p * v))
"#;

// ASPIRE: Jit — the `|(s, p), v|` destructured acc formal has no
// single BindId, so the callee kernel can't bind its leaves and the
// For's cross-kernel call de-fuses (see hof_leaf_string).
run!(fold_tuple_acc, FOLD_TUPLE_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(6), Value::I64(6)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const FOLD_STRUCT_ACC: &str = r#"
{
    let st = array::fold([i64:1, i64:2, i64:3], {n: i64:0, sum: i64:0}, |acc, v| {
        n: acc.n + i64:1,
        sum: acc.sum + v
    });
    st.n * i64:100 + st.sum
}
"#;

run!(fold_struct_acc, FOLD_STRUCT_ACC, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(306))
); graphix_package_core::testing::FuseExpect::Jit);

const FOLD_ARRAY_ACC: &str = r#"
{
    let evens: Array<i64> = array::fold(
        [i64:1, i64:2, i64:3, i64:4],
        [],
        |acc, v| select v % i64:2 { i64:0 => array::push(acc, v), _ => acc }
    );
    evens
}
"#;

run!(fold_array_acc, FOLD_ARRAY_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(2), Value::I64(4)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// ASPIRE: Jit — the callback takes the STRING acc as a formal, and
// the For loop's cross-kernel call can't marshal string args yet
// (see hof_str_fold).
const FOLD_STRING_ACC: &str = r#"
array::fold([i64:1, i64:2, i64:3], "", |acc, v| "[acc][v]")
"#;

run!(fold_string_acc, FOLD_STRING_ACC, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "123",
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

// ownership edges of the owned-acc carry: a body that RETURNS the acc
// unchanged (borrowed → cloned before the old acc drops), and a body
// that returns the ELEMENT (borrowed from the elem local).
const FOLD_ACC_IDENTITY: &str = r#"
array::fold([[i64:1], [i64:2]], [i64:9], |acc, v| acc)
"#;

run!(fold_acc_identity, FOLD_ACC_IDENTITY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(9)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const FOLD_ACC_ELEM_BODY: &str = r#"
array::fold([[i64:1], [i64:2]], [i64:9], |acc, v| v)
"#;

run!(fold_acc_elem_body, FOLD_ACC_ELEM_BODY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(2)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);
