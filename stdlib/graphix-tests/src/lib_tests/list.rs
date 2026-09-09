use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::run;
use netidx::subscriber::Value;

const LIST_NIL: &str = r#"
  list::is_empty(list::nil(null))
"#;

run!(list_nil, LIST_NIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_CONS: &str = r#"
  list::to_array(list::cons(1, list::cons(2, list::cons(3, list::nil(null)))))
"#;

run!(list_cons, LIST_CONS, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_SINGLETON: &str = r#"
  list::to_array(list::singleton(42))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_singleton, LIST_SINGLETON, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_HEAD_NONEMPTY: &str = r#"
  list::head(list::from_array([10, 20, 30]))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_head_nonempty, LIST_HEAD_NONEMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_HEAD_EMPTY: &str = r#"
  list::head(list::nil(null))
"#;

run!(list_head_empty, LIST_HEAD_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_TAIL_NONEMPTY: &str = r#"
  list::tail(list::from_array([1, 2, 3]))
"#;

// ASPIRE: Jit — composite/value cross-kernel call args.
run!(list_tail_nonempty, LIST_TAIL_NONEMPTY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(2), Value::Array(rest)] => match &rest[..] {
                [Value::I64(3), Value::Array(nil)] => nil.is_empty(),
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_TAIL_EMPTY: &str = r#"
  list::tail(list::nil(null))
"#;

run!(list_tail_empty, LIST_TAIL_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_UNCONS_NONEMPTY: &str = r#"
  list::uncons(list::from_array([10, 20, 30]))
"#;

// ASPIRE: Jit — composite/value cross-kernel call args.
run!(list_uncons_nonempty, LIST_UNCONS_NONEMPTY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(t)) => match &t[..] {
            [Value::I64(10), Value::Array(tail)] => match &tail[..] {
                [Value::I64(20), Value::Array(rest)] => match &rest[..] {
                    [Value::I64(30), Value::Array(nil)] => nil.is_empty(),
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_UNCONS_EMPTY: &str = r#"
  list::uncons(list::nil(null))
"#;

run!(list_uncons_empty, LIST_UNCONS_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_IS_EMPTY_TRUE: &str = r#"
  list::is_empty(list::nil(null))
"#;

run!(list_is_empty_true, LIST_IS_EMPTY_TRUE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_IS_EMPTY_FALSE: &str = r#"
  list::is_empty(list::singleton(1))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_is_empty_false, LIST_IS_EMPTY_FALSE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(false)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_NTH: &str = r#"
{
  let l = list::from_array([10, 20, 30, 40, 50]);
  (list::nth(l, 0), list::nth(l, 2), list::nth(l, 4))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_nth, LIST_NTH, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(t)) => match &t[..] {
            [Value::I64(10), Value::I64(30), Value::I64(50)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_NTH_OOB: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  (list::nth(l, 5), list::nth(l, -1))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_nth_oob, LIST_NTH_OOB, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(t)) => match &t[..] {
            [Value::Null, Value::Null] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_LEN: &str = r#"
  list::len(list::from_array([1, 2, 3, 4, 5]))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_len, LIST_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_LEN_EMPTY: &str = r#"
  list::len(list::nil(null))
"#;

run!(list_len_empty, LIST_LEN_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(0)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_REVERSE: &str = r#"
  list::to_array(list::reverse(list::from_array([1, 2, 3])))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_reverse, LIST_REVERSE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(3), Value::I64(2), Value::I64(1)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_TAKE: &str = r#"
  list::to_array(list::take(2, list::from_array([1, 2, 3, 4, 5])))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_take, LIST_TAKE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_TAKE_MORE: &str = r#"
  list::to_array(list::take(10, list::from_array([1, 2, 3])))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_take_more, LIST_TAKE_MORE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_DROP: &str = r#"
  list::to_array(list::drop(2, list::from_array([1, 2, 3, 4, 5])))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_drop, LIST_DROP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(3), Value::I64(4), Value::I64(5)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_DROP_MORE: &str = r#"
  list::to_array(list::drop(10, list::from_array([1, 2, 3])))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_drop_more, LIST_DROP_MORE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_ROUNDTRIP: &str = r#"
  list::to_array(list::from_array([10, 20, 30]))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_roundtrip, LIST_ROUNDTRIP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(10), Value::I64(20), Value::I64(30)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FROM_ARRAY_LEN: &str = r#"
  list::len(list::from_array([1, 2, 3]))
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_from_array_len, LIST_FROM_ARRAY_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_CONCAT: &str = r#"
{
  let a = list::from_array([1, 2, 3]);
  let b = list::from_array([4, 5]);
  let c = list::from_array([6]);
  list::to_array(list::concat(a, b, c))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_concat, LIST_CONCAT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FLATTEN: &str = r#"
{
  let a = list::from_array([1, 2]);
  let b = list::from_array([3, 4]);
  let c = list::from_array([5]);
  let outer = list::cons(a, list::cons(b, list::singleton(c)));
  list::to_array(list::flatten(outer))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_flatten, LIST_FLATTEN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_MAP: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  list::to_array(list::map(l, |x| x * 2))
}
"#;

run!(list_map, LIST_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(2), Value::I64(4), Value::I64(6)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_MAP_TYPE_ERR: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  list::map(l, |x| str::len(x))
}
"#;

run!(list_map_type_err, LIST_MAP_TYPE_ERR, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

const LIST_FILTER: &str = r#"
{
  let l = list::from_array([1, 2, 3, 4, 5, 6, 7, 8]);
  list::to_array(list::filter(l, |x| x > 3))
}
"#;

run!(list_filter, LIST_FILTER, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [
                Value::I64(4),
                Value::I64(5),
                Value::I64(6),
                Value::I64(7),
                Value::I64(8),
            ] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FILTER_MAP: &str = r#"
{
  let l = list::from_array([1, 2, 3, 4, 5, 6, 7, 8]);
  list::to_array(list::filter_map(l, |x: i64| -> [i64, null] select x > 5 {
    true => x + 1,
    false => x ~ null
  }))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_filter_map, LIST_FILTER_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(8), Value::I64(9)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FLAT_MAP: &str = r#"
{
  let l = list::from_array([1, 2]);
  list::to_array(list::flat_map(l, |x| list::from_array([x, x + 1])))
}
"#;

run!(list_flat_map, LIST_FLAT_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FOLD: &str = r#"
{
  let l = list::from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  list::fold(l, 0, |acc, x| x + acc)
}
"#;

run!(list_fold, LIST_FOLD, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(55)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Dynamic re-firing is a convergence test: `list_fold_dynamic_init_converges`.

const LIST_FOLD_TYPE_ERR: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  list::fold(l, 0, |acc, x| str::len(x) + acc)
}
"#;

run!(list_fold_type_err, LIST_FOLD_TYPE_ERR, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

const LIST_FIND: &str = r#"
{
  type T = (string, i64);
  let l: List<T> = list::from_array([("foo", 1), ("bar", 2), ("baz", 3)]);
  list::find(l, |(k, _): T| k == "bar")
}
"#;

run!(list_find, LIST_FIND, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s), Value::I64(2)] => &**s == "bar",
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_FIND_MISS: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  list::find(l, |x| x > 10)
}
"#;

run!(list_find_miss, LIST_FIND_MISS, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::Jit);

// A heterogeneous list with a Fn member flowing through find into
// Number-constrained arith is rejected.
const LIST_FIND_HET_FN_FOLD_TYPE_ERR: &str = r#"
{
  let a = array::init(3, |i| {
    let l = list::from_array([null, list::cons, 3]);
    list::find(l, |x| x > 10)
  });
  array::fold(a, 0, |acc, x| acc + x)
}
"#;

run!(
    list_find_het_fn_fold_type_err,
    LIST_FIND_HET_FN_FOLD_TYPE_ERR,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

const LIST_FIND_MAP: &str = r#"
{
  type T = (string, i64);
  let l: List<T> = list::from_array([("foo", 1), ("bar", 2), ("baz", 3)]);
  list::find_map(l, |(k, v): T| select k == "bar" {
    true => v,
    false => v ~ null
  })
}
"#;

run!(list_find_map, LIST_FIND_MAP, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_SORT_ASC: &str = r#"
  list::to_array(list::sort(list::from_array([5, 3, 1, 4, 2])))
"#;

run!(list_sort_asc, LIST_SORT_ASC, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([1, 2, 3, 4, 5]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_SORT_DESC: &str = r#"
  list::to_array(list::sort(#dir:`Descending, list::from_array([5, 3, 1, 4, 2])))
"#;

run!(list_sort_desc, LIST_SORT_DESC, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([5, 4, 3, 2, 1]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_SORT_NUMERIC: &str = r#"
  list::to_array(list::sort(#numeric:true, list::from_array(["5", "50", "6", "40", "1"])))
"#;

run!(list_sort_numeric, LIST_SORT_NUMERIC, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 5]>() {
            Ok([a0, a1, a2, a3, a4]) => {
                &*a0 == "1" && &*a1 == "5" && &*a2 == "6" && &*a3 == "40" && &*a4 == "50"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_SORT_NUMERIC_DESC: &str = r#"
  list::to_array(list::sort(#dir:`Descending, #numeric:true, list::from_array(["5", "50", "6", "40", "1"])))
"#;

run!(list_sort_numeric_desc, LIST_SORT_NUMERIC_DESC, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 5]>() {
            Ok([a0, a1, a2, a3, a4]) => {
                &*a0 == "50" && &*a1 == "40" && &*a2 == "6" && &*a3 == "5" && &*a4 == "1"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_ENUMERATE: &str = r#"
{
  let l = list::from_array([10, 20, 30]);
  list::to_array(list::enumerate(l))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_enumerate, LIST_ENUMERATE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(0, 10), (1, 20), (2, 30)]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_ZIP: &str = r#"
{
  let a = list::from_array([1, 2, 3]);
  let b = list::from_array([10, 20, 30]);
  list::to_array(list::zip(a, b))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_zip, LIST_ZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(1, 10), (2, 20), (3, 30)]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_ZIP_UNEQUAL: &str = r#"
{
  let a = list::from_array([1, 2, 3, 4, 5]);
  let b = list::from_array([10, 20]);
  list::to_array(list::zip(a, b))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_zip_unequal, LIST_ZIP_UNEQUAL, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 2]>() {
            Ok([(1, 10), (2, 20)]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_UNZIP: &str = r#"
{
  let l: List<(i64, i64)> = list::from_array([(1, 10), (2, 20), (3, 30)]);
  let p = list::unzip(l);
  (list::to_array(p.0), list::to_array(p.1))
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_unzip, LIST_UNZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<([i64; 3], [i64; 3])>() {
            Ok(([1, 2, 3], [10, 20, 30])) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_INIT: &str = r#"
  list::to_array(list::init(5, |i| i * 2))
"#;

run!(list_init, LIST_INIT, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([0, 2, 4, 6, 8]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_INIT_ZERO: &str = r#"
  list::to_array(list::init(0, |i| i))
"#;

run!(list_init_zero, LIST_INIT_ZERO, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_INIT_TYPE_ERR: &str = r#"
  list::init(3, |i| str::len(i))
"#;

run!(list_init_type_err, LIST_INIT_TYPE_ERR, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

const LIST_ITER: &str = r#"
  filter(list::iter(list::from_array([1, 2, 3, 4])), |x| x == 4)
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_iter, LIST_ITER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(4)))
}; graphix_package_core::testing::FuseExpect::None);

const LIST_ITERQ: &str = r#"
{
   let l = list::from_array([1, 2, 3, 4]);
   l <- list::from_array([5, 6, 7, 8]);
   let clock: Any = once(null);
   let v = list::iterq(#clock, l);
   clock <- v;
   filter(v, |x| x == 8)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(list_iterq, LIST_ITERQ, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
}; graphix_package_core::testing::FuseExpect::Jit);

// `list::fold`'s init changes 0 -> 100 on the second cycle and the fold
// recomputes (106); `eval_converged` reads the settled value.
#[tokio::test]
async fn list_fold_dynamic_init_converges() {
    let prog = "{ \
        let init = 0; \
        init <- select init { 0 => init ~ 100, _ => never() }; \
        list::fold(list::from_array([1, 2, 3]), init, |acc, x| x + acc) \
    }";
    let (v, ctx) =
        graphix_package_core::testing::eval_converged(prog, crate::TEST_REGISTER)
            .await
            .expect("eval_converged");
    assert_eq!(
        v,
        Value::I64(106),
        "fold must re-fire the accumulator chain when init changes"
    );
    ctx.shutdown().await;
}

// A composite callback return shape.
const LIST_FIND_MAP_TUPLE: &str = r#"
{type T = (string, i64); let l: List<T> = list::from_array([("foo", i64:1), ("bar", i64:2)]); list::find_map(l, |(k, v): T| (i64:1, i64:2))}
"#;
run!(list_find_map_tuple, LIST_FIND_MAP_TUPLE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<(i64, i64)>()), Ok(Ok((1, 2))))
}; graphix_package_core::testing::FuseExpect::Jit);

// Direct List-HOF lowering: the loops compile to native kernels.

// A whole map+fold pipeline as one kernel; `#[native]` proves zero
// node-walk residue.
const LIST_NATIVE_PIPELINE: &str = r#"
#[native]
list::fold(list::map(list::from_array([1, 2, 3]), |x| x * 2), 0, |acc, x| acc + x)
"#;
run!(list_native_pipeline, LIST_NATIVE_PIPELINE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(12)))
});

// The fold of an empty list fires with the initializer.
const LIST_FOLD_EMPTY: &str = r#"
list::fold(list::from_array([]), 42, |acc, x| acc + x)
"#;
run!(list_fold_empty, LIST_FOLD_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// `|(k, v)|` leaves bind from the flattened elements as for arrays.
const LIST_MAP_TUPLE_DESTRUCTURE: &str = r#"
{
  let l = list::from_array([("a", 1), ("b", 2)]);
  list::to_array(list::map(l, |(k, v)| v * 2))
}
"#;
run!(list_map_tuple_destructure, LIST_MAP_TUPLE_DESTRUCTURE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<Vec<i64>>()), Ok(Ok(v)) if v == vec![2, 4])
});

// flat_map splicing an empty list for one element.
const LIST_FLAT_MAP_EMPTY_SPLICE: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  list::to_array(list::flat_map(l, |x| select x {
    2 => list::from_array([]),
    _ => list::from_array([x, x * 10])
  }))
}
"#;
run!(list_flat_map_empty_splice, LIST_FLAT_MAP_EMPTY_SPLICE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<Vec<i64>>()), Ok(Ok(v)) if v == vec![1, 10, 3, 30])
});

// A List-valued fold accumulator (the idiomatic reverse).
const LIST_FOLD_LIST_ACC: &str = r#"
{
  let l = list::from_array([1, 2, 3]);
  let seed: List<i64> = list::from_array([]);
  let rev = {
    let r = #[native] list::fold(l, seed, |acc, x| list::cons(x, acc));
    r
  };
  list::to_array(rev)
}
"#;
run!(list_fold_list_acc, LIST_FOLD_LIST_ACC, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<Vec<i64>>()), Ok(Ok(ref a)) if a == &[3, 2, 1])
});

// The max-by idiom: a nullable accumulator seeded null.
const LIST_FOLD_NULLABLE_ACC: &str = r#"
{
  let l = list::from_array([3, 9, 4]);
  let seed: [i64, null] = null;
  list::fold(l, seed, |best, x| select best {
    null as _ => x,
    i64 as b => select x > b { true => x, false => b }
  })
}
"#;
run!(list_fold_nullable_acc, LIST_FOLD_NULLABLE_ACC, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
});

// The group-by idiom: a Map accumulator built by `map::insert`.
const LIST_FOLD_MAP_ACC: &str = r#"
{
  let l = list::from_array([("a", 1), ("b", 2), ("a", 10)]);
  let m = list::fold(l, {"" => 0}, |acc, (k, v)| map::insert(acc, k, v));
  (map::len(m), map::get(m, "a"), map::get(m, "b"))
}
"#;
run!(list_fold_map_acc, LIST_FOLD_MAP_ACC, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<(i64, i64, i64)>()), Ok(Ok((3, 10, 2))))
});

// The list grows a cycle in; the fold re-fires with the new length.
#[tokio::test]
async fn list_fold_resize_converges() {
    let prog = "{ \
        let l = list::from_array([1, 2, 3]); \
        l <- select list::len(l) { 3 => l ~ list::from_array([1, 2, 3, 4]), _ => never() }; \
        list::fold(l, 0, |acc, x| acc + x) \
    }";
    let (v, ctx) =
        graphix_package_core::testing::eval_converged(prog, crate::TEST_REGISTER)
            .await
            .expect("eval_converged");
    assert_eq!(v, Value::I64(10), "fold must re-fire when the list resizes");
    ctx.shutdown().await;
}
