use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::run;
use netidx::subscriber::Value;

const ARRAY_MAP0: &str = r#"
{
  let a = [1, 2, 3, 4];
  array::map(a, |x| x > 3)
}
"#;

run!(array_map0, ARRAY_MAP0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::Bool(false), Value::Bool(false), Value::Bool(false), Value::Bool(true)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_MAP1: &str = r#"
{
  let a = [1, 2];
  let b = [1, 2];
  array::map(a, |x| array::map(b, |y| x + y))
}
"#;

run!(array_map1, ARRAY_MAP1, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 2]; 2]>() {
            Ok([[2, 3], [3, 4]]) => true,
            _ => false,
        },
        Err(_) => false,
    }
});

const ARRAY_MAP2: &str = r#"
  array::map([1, 2], |x| str::len(x))
"#;

run!(array_map2, ARRAY_MAP2, |v: Result<&Value>| {
    match v {
        Err(_) => true,
        Ok(_) => false,
    }
});

const ARRAY_FILTER: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter(a, |x| x > 3)
}
"#;

run!(array_filter, ARRAY_FILTER, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(4), Value::I64(5), Value::I64(6), Value::I64(7), Value::I64(8)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_FLAT_MAP: &str = r#"
{
  let a = [1, 2];
  array::flat_map(a, |x| [x, x + 1])
}
"#;

run!(array_flat_map, ARRAY_FLAT_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_FILTER_MAP: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter_map(a, |x: i64| -> [i64, null] select x > 5 {
    true => x + 1,
    false => x ~ null
  })
}
"#;

run!(array_filter_map, ARRAY_FILTER_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(8), Value::I64(9)] => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_FIND: &str = r#"
{
  type T = (string, i64);
  let a: Array<T> = [("foo", 1), ("bar", 2), ("baz", 3)];
  array::find(a, |(k, _): T| k == "bar")
}
"#;

run!(array_find, ARRAY_FIND, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s), Value::I64(2)] => &**s == "bar",
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_FIND_MAP: &str = r#"
{
  type T = (string, i64);
  let a: Array<T> = [("foo", 1), ("bar", 2), ("baz", 3)];
  array::find_map(a, |(k, v): T| select k == "bar" {
    true => v,
    false => v ~ null
  })
}
"#;

run!(array_find_map, ARRAY_FIND_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(2)) => true,
        _ => false,
    }
});

const ARRAY_ITER: &str = r#"
   filter(array::iter([1, 2, 3, 4]), |x| x == 4)
"#;

run!(array_iter, ARRAY_ITER, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(4)) => true,
        _ => false,
    }
});

const ARRAY_ITERQ: &str = r#"
{
   let a = [1, 2, 3, 4];
   a <- [5, 6, 7, 8];
   let clock: Any = once(null);
   let v = array::iterq(#clock, a);
   clock <- v;
   filter(v, |x| x == 8)
}
"#;

run!(array_iterq, ARRAY_ITERQ, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(8)) => true,
        _ => false,
    }
});

const ARRAY_FOLD0: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  array::fold(a, 0, |acc, x| x + acc)
}
"#;

run!(array_fold0, ARRAY_FOLD0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(55)) => true,
        _ => false,
    }
});

const ARRAY_FOLD1: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  array::fold(a, 0, |acc, x| str::len(x) + acc)
}
"#;

run!(array_fold1, ARRAY_FOLD1, |v: Result<&Value>| {
    match v {
        Err(_) => true,
        Ok(_) => false,
    }
});

const ARRAY_CONCAT: &str = r#"
  array::concat([1, 2, 3], [4, 5], [6])
"#;

run!(array_concat, ARRAY_CONCAT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_PUSH: &str = r#"
  array::push([(1, 2), (3, 4)], (5, 6))
"#;

run!(array_push, ARRAY_PUSH, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(1, 2), (3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
});

const ARRAY_PUSH_FRONT: &str = r#"
  array::push_front([(1, 2), (3, 4)], (5, 6))
"#;

run!(array_push_front, ARRAY_PUSH_FRONT, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(5, 6), (1, 2), (3, 4)]) => true,
        Ok(_) | Err(_) => false,
    }
});

const ARRAY_WINDOW0: &str = r#"
  array::window(#n:1, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window0, ARRAY_WINDOW0, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 1]>()) {
        Ok([(5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
});

const ARRAY_WINDOW1: &str = r#"
  array::window(#n:2, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window1, ARRAY_WINDOW1, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 2]>()) {
        Ok([(3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
});

const ARRAY_WINDOW2: &str = r#"
  array::window(#n:3, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window2, ARRAY_WINDOW2, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(1, 2), (3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
});

const ARRAY_LEN: &str = r#"
{
  use array;
  len(concat([1, 2, 3], [4, 5], [6]))
}
"#;

run!(array_len, ARRAY_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
});

const ARRAY_FLATTEN: &str = r#"
  array::flatten([[1, 2, 3], [4, 5], [6]])
"#;

run!(array_flatten, ARRAY_FLATTEN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_GROUP0: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |_, v| v == 3)
}
"#;

run!(array_group0, ARRAY_GROUP0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_GROUP1: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |x, v| (str::len(x) == 2) || (v == 3))
}
"#;

run!(array_group1, ARRAY_GROUP1, |v: Result<&Value>| {
    match v {
        Ok(_) => false,
        Err(_) => true,
    }
});

const ARRAY_GROUP2: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |v| v == 3)
}
"#;

run!(array_group2, ARRAY_GROUP2, |v: Result<&Value>| {
    match v {
        Ok(_) => false,
        Err(_) => true,
    }
});

const ARRAY_SORT0: &str = r#"
{
   let a = [5, 4, 3, 2, 1];
   array::sort(a)
}
"#;

run!(array_sort0, ARRAY_SORT0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([1, 2, 3, 4, 5]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_SORT1: &str = r#"
{
   let a = [5, 4, 3, 2, 1];
   array::sort(#dir:`Descending, a)
}
"#;

run!(array_sort1, ARRAY_SORT1, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([5, 4, 3, 2, 1]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_SORT2: &str = r#"
{
   let a = ["5", "6", "50", "60", "40", "4", "3", "2", "1"];
   array::sort(#numeric:true, a)
}
"#;

run!(array_sort2, ARRAY_SORT2, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 9]>() {
            Ok([a0, a1, a2, a3, a4, a5, a6, a7, a8]) => {
                &*a0 == "1"
                    && &*a1 == "2"
                    && &*a2 == "3"
                    && &*a3 == "4"
                    && &*a4 == "5"
                    && &*a5 == "6"
                    && &*a6 == "40"
                    && &*a7 == "50"
                    && &*a8 == "60"
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_SORT3: &str = r#"
{
   let a = ["5", "6", "50", "60", "40", "4", "3", "2", "1"];
   array::sort(#dir:`Descending, #numeric:true, a)
}
"#;

run!(array_sort3, ARRAY_SORT3, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 9]>() {
            Ok([a0, a1, a2, a3, a4, a5, a6, a7, a8]) => {
                &*a0 == "60"
                    && &*a1 == "50"
                    && &*a2 == "40"
                    && &*a3 == "6"
                    && &*a4 == "5"
                    && &*a5 == "4"
                    && &*a6 == "3"
                    && &*a7 == "2"
                    && &*a8 == "1"
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_ENUMERATE: &str = r#"
{
   let a = [1, 2, 3];
   array::enumerate(a)
}
"#;

run!(array_enumerate, ARRAY_ENUMERATE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(0, 1), (1, 2), (2, 3)]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_ZIP: &str = r#"
{
   let a0 = [1, 2, 5];
   let a1 = [1, 2, 3];
   array::zip(a0, a1)
}
"#;

run!(array_zip, ARRAY_ZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(1, 1), (2, 2), (5, 3)]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_UNZIP: &str = r#"
{
   let a = [(1, 1), (2, 2), (5, 3)];
   array::unzip(a)
}
"#;

run!(array_unzip, ARRAY_UNZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<([i64; 3], [i64; 3])>() {
            Ok(([1, 2, 5], [1, 2, 3])) => true,
            _ => false,
        },
        _ => false,
    }
});
