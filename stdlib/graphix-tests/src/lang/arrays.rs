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

// `array[i]` is `[elem, Error<…>]`: out-of-bounds and negative underflow
// produce an error on every backend.

// A positive index past the end is an error.
run!(
    array_index_oob_pos,
    r#"{ let a = [10, 20, 30]; a[10] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)))
);

// -1 is the last element.
run!(
    array_index_neg_last,
    r#"{ let a = [10, 20, 30]; a[-1] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(30)))
);

// -2 is the second-to-last.
run!(
    array_index_neg_mid,
    r#"{ let a = [10, 20, 30]; a[-2] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(20)))
);

// a[-len] reaches the first element.
run!(
    array_index_neg_first,
    r#"{ let a = [10, 20, 30]; a[-3] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(10)))
);

// Negative underflow past the start is an error.
run!(array_index_neg_underflow, r#"{ let a = [10, 20, 30]; a[-10] }"#, |v: Result<
    &Value,
>| matches!(
    v,
    Ok(Value::Error(_))
));

// `is_err` over the index observes the error directly.
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

// `a[i..j]`.
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

// End-only slice `a[..j]`.
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

// Start-only slice `a[i..]`.
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

// Unbounded slice `a[..]`.
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

// An out-of-bounds slice is an error on every backend.
run!(
    array_slice_oob,
    r#"{ let a = [0, 1, 2]; a[1..10] }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)))
);

// The slice error flows into `is_err`.
run!(
    array_slice_oob_is_err,
    r#"{ let a = [0, 1, 2]; is_err(a[1..10]) }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

// A negative slice bound is the same out-of-bounds error on every
// backend.
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

// ASPIRE: Jit — the body does not fuse into a kernel yet.
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

// ASPIRE: Jit — the body does not fuse into a kernel yet.
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

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(array_match2, ARRAY_MATCH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[ArcStr; 2]>() {
        Ok([s0, s1]) if &*s0 == "Empty" && &*s1 == "Nonempty" => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A fold over an empty but present array is the init, not bottom.
const FOLD_EMPTY: &str = r#"
{
    let xs = array::init(i64:0, |idx: i64| idx);
    array::fold(xs, i64:42, |acc, x| acc + x)
}
"#;

run!(fold_empty, FOLD_EMPTY, |v: Result<&Value>| matches!(v, Ok(Value::I64(42))));

// A fold whose filter removes every element is also the init.
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

// find over an empty array yields null.
const FIND_EMPTY: &str = r#"
array::find({let a: Array<i64> = []; a}, |x| true)
"#;

run!(find_empty, FIND_EMPTY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Null)
); graphix_package_core::testing::FuseExpect::Jit);

// An oversize array::init (> MAX_ARRAY_INIT_LEN) bottoms locally:
// unrelated outputs in the same region still fire.
const INIT_RUNAWAY_LOCAL_BOTTOM: &str = r#"
{
  array::init(i64:9223372036854775807, |idx: i64| f64:0.);
  array::fold([i64:2, i64:1, i64:10], i64:42, |acc, x| acc + x)
}
"#;

run!(init_runaway_local_bottom, INIT_RUNAWAY_LOCAL_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(55)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A fold over the oversize init whose init argument fires on the
// over-limit cycle is bottom; the previous value stands.
const FOLD_OVER_OVERSIZE_INIT_BOTTOMS: &str = r#"
{
  let n = array::iter([i64:0, i64:9223372036854775807]);
  array::fold(array::init(n, |i| i), count(array::iter([i64:1, i64:2])), |acc, x| x + i64:2)
}
"#;

run!(fold_over_oversize_init_bottoms, FOLD_OVER_OVERSIZE_INIT_BOTTOMS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Composite / string fold accumulators as native loops. `array::fold`
// is in-language, so these are `Jit` without a `#[native]` pin.

const FOLD_TUPLE_ACC: &str = r#"
array::fold([i64:1, i64:2, i64:3], (i64:0, i64:1), |(s, p), v| (s + v, p * v))
"#;

// ASPIRE: Jit — a destructured acc formal has no single BindId.
run!(fold_tuple_acc, FOLD_TUPLE_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::I64(6), Value::I64(6)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

// ASPIRE: Jit — a non-tail Value producer in a callee body.
run!(fold_array_acc, FOLD_ARRAY_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(2), Value::I64(4)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// ASPIRE: Jit — a string acc formal across the cross-kernel call.
const FOLD_STRING_ACC: &str = r#"
array::fold([i64:1, i64:2, i64:3], "", |acc, v| "[acc][v]")
"#;

run!(fold_string_acc, FOLD_STRING_ACC, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "123",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Ownership edges of the owned-acc carry: a body that returns the acc
// unchanged, and a body that returns the element.
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

// A bottoming predicate poisons the whole collection for that cycle:
// three honest deliveries, so the group closes at n == 3.
const FILTER_DIV0_SLOT_CACHE: &str = r#"
{
  let a = [1, 2, array::iter([1, 2, 0, 4]), 4, 5, 6, 7, 8];
  let out = array::filter(a, |x| 10 / x > 2);
  array::group(out, |n, _| n == 3)
}
"#;

run!(filter_div0_slot_cache, FILTER_DIV0_SLOT_CACHE, |v: Result<&Value>| {
    let expect: Vec<Vec<i64>> = vec![vec![1, 2, 1], vec![1, 2, 2], vec![1, 2]];
    match v {
        Ok(Value::Array(gs)) => {
            gs.len() == expect.len()
                && gs.iter().zip(expect.iter()).all(|(g, e)| match g {
                    Value::Array(a) => {
                        a.iter()
                            .map(|v| v.clone().cast_to::<i64>().unwrap())
                            .collect::<Vec<_>>()
                            == *e
                    }
                    _ => false,
                })
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);
