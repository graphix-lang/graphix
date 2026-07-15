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
            [
                Value::Bool(false),
                Value::Bool(false),
                Value::Bool(false),
                Value::Bool(true),
            ] => true,
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

// ASPIRE: Jit (currently None) — the body is a nested `array::map(b,
// |y| x + y)` that captures the outer element `x`; that nested HOF
// doesn't lower yet (its captured `x` + inner array input `b` aren't
// threaded into the inner kernel), so the outer map's body fails to
// emit. `array_map_tuple` below exercises composite-output map without
// the nesting.
run!(array_map1, ARRAY_MAP1, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 2]; 2]>() {
            Ok([[2, 3], [3, 4]]) => true,
            _ => false,
        },
        Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// Nested map with a CONST callback body over a loop-invariant captured
// source: the inner loop claims a state word despite being nested
// (`SlotFlags::src_invariant` — `b` is identical on every outer
// iteration), giving it MapQ's exact firing rule instead of the
// stateless approximation that over-fired when `b` was stream-fed
// (findings/firing-jul2026/03). This fixture asserts the shape still
// FUSES with the claim in place; the multi-cycle firing parity is the
// findings pin's job.
const ARRAY_MAP_NESTED_CONST: &str = r#"
{
  let a = [1, 2];
  let b = [3, 4];
  array::map(a, |x| array::map(b, |y| 7))
}
"#;

run!(array_map_nested_const, ARRAY_MAP_NESTED_CONST, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 2]; 2]>() {
            Ok([[7, 7], [7, 7]]) => true,
            _ => false,
        },
        Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// Composite-output `array::map`: the body produces a tuple per element,
// so the output is `Array<(i64, i64)>`. Exercises the map loop's
// composite-output push (`compile_and_push_field`) without nesting.
const ARRAY_MAP_TUPLE: &str = r#"
{
  let a = [1, 2, 3];
  array::map(a, |x| (x, x * 2))
}
"#;

run!(array_map_tuple, ARRAY_MAP_TUPLE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(1, 2), (2, 4), (3, 6)]) => true,
            _ => false,
        },
        Err(_) => false,
    }
});

// Composite-element *input* with a `|(k, v)|` destructure callback.
// The element `(i64, i64)` binds into the loop's composite slot (both
// backends); the callback's tuple pattern lowers to per-leaf `TupleGet`
// lets. The JIT loop gets each element via `graphix_valarray_get_array`,
// binds it, runs the body, then drops the owned per-iter element.
const ARRAY_MAP_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4)];
  array::map(a, |(k, v)| k + v)
}
"#;

run!(array_map_destructure, ARRAY_MAP_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => &a[..] == [Value::I64(3), Value::I64(7)],
        _ => false,
    }
});

// `array::fold` with a `|acc, (k, v)|` destructure callback over a
// composite element — the accumulator stays scalar, the element binds
// into the composite slot (interp + JIT, same split as map).
const ARRAY_FOLD_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4)];
  array::fold(a, 0, |acc, (k, v)| acc + k + v)
}
"#;

run!(array_fold_destructure, ARRAY_FOLD_DESTRUCTURE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// `array::filter` with a `|(k, v)|` destructure over a composite
// element. Keeps the *original* composite elements where the predicate
// holds. The JIT loop gets each element (owned `*ValArray`), binds it,
// evals the predicate, and on keep moves it into the output / on
// not-keep drops it (the conditional-drop split).
const ARRAY_FILTER_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4), (5, 6)];
  array::filter(a, |(k, v)| v > 3)
}
"#;

run!(array_filter_destructure, ARRAY_FILTER_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::Array(p0), Value::Array(p1)] => {
                &p0[..] == [Value::I64(3), Value::I64(4)]
                    && &p1[..] == [Value::I64(5), Value::I64(6)]
            }
            _ => false,
        },
        _ => false,
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
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_FILTER: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter(a, |x| x > 3)
}
"#;

run!(array_filter, ARRAY_FILTER, |v: Result<&Value>| {
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

const ARRAY_FLAT_MAP_DESTRUCTURE: &str = r#"
{
  let a = [(1, 10), (2, 20)];
  array::flat_map(a, |(k, v)| [k, v])
}
"#;

run!(array_flat_map_destructure, ARRAY_FLAT_MAP_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(
            &a[..],
            [Value::I64(1), Value::I64(10), Value::I64(2), Value::I64(20)]
        ),
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

// The `false => x ~ null` arm uses the sample operator `~`; in a fully-
// sync fused kernel `a ~ b` lowers to `b` (the trigger always fires), so
// the body fuses+JITs.
run!(array_filter_map, ARRAY_FILTER_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(8), Value::I64(9)] => true,
            _ => false,
        },
        _ => false,
    }
});

// Scalar `array::filter_map` whose body is an option-typed `select`
// with no sample operator — lowers to the filter-map loop and JITs
// (the Nullable-collecting loop checks each body result's discriminant
// against `null` and pushes the non-null payload).
const ARRAY_FILTER_MAP_SCALAR: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter_map(a, |x: i64| -> [i64, null] select x > 5 {
    true => x + 1,
    false => null
  })
}
"#;

run!(array_filter_map_scalar, ARRAY_FILTER_MAP_SCALAR, |v: Result<&Value>| {
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

// Composite element `(string, i64)` + `|(k, _)|` destructure + composite
// *output*: `array::find` returns the matched element, so the result is
// `Nullable<(string, i64)>`. The array-find loop's found edge wraps the
// owned `*ValArray` element into a value-shape Value (consumes it); the
// advance edge drops it (conditional consume, like `array::filter`).
run!(array_find, ARRAY_FIND, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s), Value::I64(2)] => &**s == "bar",
            _ => false,
        },
        _ => false,
    }
});

// Scalar-element `array::find` lowers to the array-find loop (result
// `Nullable<i64>`) and JITs via an early-exit loop whose found / not-
// found edges feed a two-word `(disc, payload)` merge.
const ARRAY_FIND_SCALAR: &str = r#"
{
  let a = [1, 2, 3, 4, 5];
  array::find(a, |x| x > 3)
}
"#;

run!(array_find_scalar, ARRAY_FIND_SCALAR, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(4)) => true,
        _ => false,
    }
});

// No element matches — `array::find` returns `null`.
const ARRAY_FIND_SCALAR_NONE: &str = r#"
{
  let a = [1, 2, 3];
  array::find(a, |x| x > 10)
}
"#;

run!(array_find_scalar_none, ARRAY_FIND_SCALAR_NONE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

// Composite element + composite output, all-prim — exercises the
// array-find found-edge `graphix_value_new_from_array` wrap +
// advance-edge `graphix_valarray_drop` without a string leaf. Result is
// the matched `(i64, i64)` element as a `Nullable<(i64, i64)>`.
const ARRAY_FIND_COMPOSITE: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find(a, |(k, _)| k == 2)
}
"#;

run!(array_find_composite, ARRAY_FIND_COMPOSITE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(2), Value::I64(20)]),
        _ => false,
    }
});

// No element matches — every composite element is fetched + dropped on the
// advance edge, then `not_found` returns `null`. Exercises the
// array-find conditional-drop path with zero wraps (the most likely
// place a leak or double-free in the owned-element drop would surface).
const ARRAY_FIND_COMPOSITE_NONE: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find(a, |(k, _)| k == 99)
}
"#;

run!(array_find_composite_none, ARRAY_FIND_COMPOSITE_NONE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
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

// Composite element `(string, i64)` + `|(k, v)|` destructure + the
// `false => v ~ null` Sample arm — all fuse + JIT, via
// the array-find-map loop (early-exit, first-non-null `Nullable<i64>`).
// The string leaf (`k`) binds as a string local in the destructure
// Block — which JITs since the Block-let-String codegen gap was fixed.
run!(array_find_map, ARRAY_FIND_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(2)) => true,
        _ => false,
    }
});

// All-prim composite element — exercises the array-find-map JIT
// path (composite element + destructure + early-exit Nullable merge)
// without the string field that keeps `array_find_map` on the interp.
const ARRAY_FIND_MAP_PRIM: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find_map(a, |(k, v)| select k == 2 {
    true => v,
    false => null
  })
}
"#;

run!(array_find_map_prim, ARRAY_FIND_MAP_PRIM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(20)))
});

const ARRAY_ITER: &str = r#"
   filter(array::iter([1, 2, 3, 4]), |x| x == 4)
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_iter, ARRAY_ITER, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(4)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

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

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_iterq, ARRAY_ITERQ, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(8)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

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

// A may-bottom predicate (`10 / x` can div0) over a runtime-clean array
// still fuses: the scaffold routes the predicate through `emit_forced`,
// which runtime-aborts to bottom only if the predicate actually taints
// (here it never does → real filtered array, both modes agree).
const ARRAY_FILTER_MAY_BOTTOM: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter(a, |x| 10 / x > 2)
}
"#;

run!(array_filter_may_bottom, ARRAY_FILTER_MAY_BOTTOM, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            matches!(&a[..], [Value::I64(1), Value::I64(2), Value::I64(3)])
        }
        _ => false,
    }
});

// A may-bottom fold body (`acc / x`) over a runtime-clean array fuses the
// same way (the scaffold `emit_forced`s the body).
const ARRAY_FOLD_MAY_BOTTOM: &str = r#"
{
  let a = [2, 5, 10];
  array::fold(a, 1000, |acc, x| acc / x)
}
"#;

run!(array_fold_may_bottom, ARRAY_FOLD_MAY_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// A may-bottom find predicate fuses (runtime-aborts via `emit_forced`);
// `10 / x > 4` matches the first x with 10/x > 4, i.e. x = 2.
const ARRAY_FIND_MAY_BOTTOM: &str = r#"
{
  let a = [4, 2, 1];
  array::find(a, |x| 10 / x > 4)
}
"#;

run!(array_find_may_bottom, ARRAY_FIND_MAY_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
});

// A may-bottom flat_map body (`[10 / x]` — the element div can bottom)
// fuses; the array-literal body's internal bottom-abort + `emit_forced`
// runtime-abort the kernel only if it actually taints.
const ARRAY_FLAT_MAP_MAY_BOTTOM: &str = r#"
{
  let a = [1, 2, 5];
  array::flat_map(a, |x| [10 / x])
}
"#;

run!(array_flat_map_may_bottom, ARRAY_FLAT_MAP_MAY_BOTTOM, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            matches!(&a[..], [Value::I64(10), Value::I64(5), Value::I64(2)])
        }
        _ => false,
    }
});

// A scalar `array::fold` result flowing directly into a `connect` once
// SIGSEGV'd (fold wrapped its scalar result in an ARRAY disc → the connect's
// set_var deref'd the scalar as a *ValArray) and over-fired (no source STALE
// → the self-connect busy-spun while the node-walk quiesced). Now it fuses,
// sets `s` once, and quiesces. See findings/hof-connect-jun2026 and the
// `fold_into_connect_quiesces` stream test.
const FOLD_INTO_CONNECT: &str = r#"
{ let a = [1, 2, 3]; let s = 0; s <- array::fold(a, 0, |acc, e| acc + e); s }
"#;

run!(fold_into_connect, FOLD_INTO_CONNECT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(0)))
});

// A fold whose input array GROWS each cycle (reactive-size init) must
// re-emit per resize. FoldQ conflated the per-cycle emit gate with the
// held chain state in `inits`: a resize left the old slots quiet
// (closed callback — `x` unused), the reset wiped the chain, and the
// fold went permanently silent after its first emission while the JIT
// re-ran the loop per resize (soak jul07e, pinned
// findings/foldq-reactive-size-jul2026/01). The separate `held` vec
// primes new slots from their predecessor's last produced acc.
const FOLD_REACTIVE_SIZE: &str = r#"
{
  let a = array::init(array::iter([1, 2, 3, 4]), |i| i + 1);
  let f = array::fold(a, 0, |acc, x| acc + 2);
  array::group(f, |n, _| n == 4)
}
"#;

run!(fold_reactive_size, FOLD_REACTIVE_SIZE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(2), Value::I64(4), Value::I64(6), Value::I64(8)] => true,
            _ => false,
        },
        _ => false,
    }
});

// The SHRINK sibling: sizes [1, 2, 1, 4] shrink mid-stream. After a
// shrink every remaining slot is quiet (closed callback), so the chain
// gate is empty — FoldQ must emit the HELD chain tail (a resize is a
// firing event; the fused loop re-runs and fires). Pinned at
// findings/foldq-reactive-size-jul2026/02.
const FOLD_REACTIVE_SHRINK: &str = r#"
{
  let a = array::init(array::iter([1, 2, 1, 4]), |i| i + 1);
  let f = array::fold(a, 0, |acc, x| acc + 2);
  array::group(f, |n, _| n == 4)
}
"#;

run!(fold_reactive_shrink, FOLD_REACTIVE_SHRINK, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(2), Value::I64(4), Value::I64(2), Value::I64(8)] => true,
            _ => false,
        },
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
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_CONCAT: &str = r#"
  array::concat([1, 2, 3], [4, 5], [6])
"#;

run!(array_concat, ARRAY_CONCAT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [
                Value::I64(1),
                Value::I64(2),
                Value::I64(3),
                Value::I64(4),
                Value::I64(5),
                Value::I64(6),
            ] => true,
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
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_PUSH_FRONT: &str = r#"
  array::push_front([(1, 2), (3, 4)], (5, 6))
"#;

run!(array_push_front, ARRAY_PUSH_FRONT, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(5, 6), (1, 2), (3, 4)]) => true,
        Ok(_) | Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// A select-union callback ([string, Spec]) whose result array feeds
// push_front with a bare Spec. The callback's CLOSED inferred rtype
// must survive its def gate BOUND — re-opened to an upper-bound
// constraint, whichever consumer unified first claimed the cell
// (push_front narrowed it to bare Spec before the map instance's
// recheck re-derived the union), so compilability depended on
// typecheck order and therefore on env contents: the shell rejected
// what the fuzz driver accepted, same build (2026-07-15, the
// data_table_virtual break).
const ARRAY_MAP_UNION_CALLBACK_PUSH_FRONT: &str = r#"
{
  type Spec = { name: string };
  let mk = |n: string| -> Spec { name: n };
  let a = array::map(["a", "b"], |n| select n { "a" => mk(n), n => n });
  array::push_front(a, mk("z"))
}
"#;

run!(
    array_map_union_callback_push_front,
    ARRAY_MAP_UNION_CALLBACK_PUSH_FRONT,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => matches!(
                &a[..],
                [Value::Array(_), Value::Array(_), Value::String(s)] if &**s == "b"
            ),
            _ => false,
        }
    };
    graphix_package_core::testing::FuseExpect::Jit
);

const ARRAY_WINDOW0: &str = r#"
  array::window(#n:1, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window0, ARRAY_WINDOW0, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 1]>()) {
        Ok([(5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_WINDOW1: &str = r#"
  array::window(#n:2, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window1, ARRAY_WINDOW1, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 2]>()) {
        Ok([(3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_WINDOW2: &str = r#"
  array::window(#n:3, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window2, ARRAY_WINDOW2, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(1, 2), (3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_LEN: &str = r#"
{
  use array;
  len(concat([1, 2, 3], [4, 5], [6]))
}
"#;

// Builtins called by their UNQUALIFIED imported names (`use array; len(…)`)
// now fuse: builtin-call discovery resolves the name in the CALL SITE's own
// lexical scope (which carries the `use array`), not the region root's — so
// `len`/`concat` register as DynCall sites and the whole body fuses. Before
// the scope fix, root-scope lookup couldn't see the unqualified name and the
// region de-fused.
run!(array_len, ARRAY_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// A per-slot HOF callback whose body is a non-numeric `cast` (bool → i64,
// excluded from the inline scalar fast path, so it lowers to a cast-machinery
// DynCall). `build_lambda_kernel` discovers the cast in the callback body and
// installs its slot, so the WHOLE callback fuses to one per-slot kernel that
// dispatches the cast in-kernel — instead of splitting around it. Values agree
// across modes.
const CAST_CALLBACK_PER_SLOT: &str = r#"
  array::map([true, false, true], |b| cast<i64>(b))
"#;

run!(cast_callback_per_slot, CAST_CALLBACK_PER_SLOT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            matches!(&a[..], [Value::I64(1), Value::I64(0), Value::I64(1)])
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_GROUP0: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |_, v| v == 3)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(array_group0, ARRAY_GROUP0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

const ARRAY_INIT0: &str = r#"
  array::init(5, |i| i * 2)
"#;

run!(array_init0, ARRAY_INIT0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([0, 2, 4, 6, 8]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_INIT1: &str = r#"
  array::init(0, |i| i)
"#;

run!(array_init1, ARRAY_INIT1, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
});

// Negative count → empty array (clamped to 0). Both fused backends used
// to abort: interp `as_usize()` and the JIT `buf_new(neg)` reserve
// usize::MAX and panic. Both now clamp `n.max(0)` like the node-walk.
const ARRAY_INIT_NEGATIVE: &str = r#"
{
  let k = -1;
  array::init(k, |i| i)
}
"#;

run!(array_init_negative, ARRAY_INIT_NEGATIVE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
});

const ARRAY_INIT2: &str = r#"
{
  let a = array::init(3, |i| i + 1);
  array::fold(a, 0, |acc, x| acc + x)
}
"#;

run!(array_init2, ARRAY_INIT2, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
});

const ARRAY_INIT3: &str = r#"
  array::init(4, |i| (i, i * i))
"#;

run!(array_init3, ARRAY_INIT3, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 4]>() {
            Ok([(0, 0), (1, 1), (2, 4), (3, 9)]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_INIT4: &str = r#"
  array::init(3, |i| str::len(i))
"#;

run!(array_init4, ARRAY_INIT4, |v: Result<&Value>| {
    match v {
        Err(_) => true,
        Ok(_) => false,
    }
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_DEDUP0: &str = r#"
{
   let a = [1, 2, 2, 3, 1, 4, 3, 5];
   array::dedup(a)
}
"#;

run!(array_dedup0, ARRAY_DEDUP0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([1, 2, 3, 4, 5]) => true,
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_DEDUP1: &str = r#"
{
   let a: Array<i64> = [];
   array::dedup(a)
}
"#;

run!(array_dedup1, ARRAY_DEDUP1, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const ARRAY_DEDUP2: &str = r#"
{
   let a = ["a", "b", "a", "c", "b", "d"];
   array::dedup(a)
}
"#;

run!(array_dedup2, ARRAY_DEDUP2, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 4]>() {
            Ok([s0, s1, s2, s3]) => {
                &*s0 == "a" && &*s1 == "b" && &*s2 == "c" && &*s3 == "d"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// ─── Phase 3: HOF over String / Value-shape ELEMENTS (#150) ──────────
// String arrays are ubiquitous; these de-fused before the bind_elem
// String/Value arms + drop_owned_elem landed. interp==jit agreement is
// the drop-exactly-once proof (the value harness can't see a leak, so
// the adversarial no-match / found-at-last / used-twice paths matter).

const HOF_STR_MAP_LEN: &str = r#"array::map(["a", "bb", "ccc"], |s| str::len(s))"#;
run!(hof_str_map_len, HOF_STR_MAP_LEN, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 3]>()),
    Ok(Ok([1, 2, 3]))
));

// String element AND String output — the element is dropped, the upper
// String is pushed.
const HOF_STR_MAP_UPPER: &str = r#"array::map(["hi", "yo"], |s| str::to_upper(s))"#;
run!(hof_str_map_upper, HOF_STR_MAP_UPPER, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "HI" && &*b == "YO")
});

// filter MOVES the kept string element into the output; drops the rest.
const HOF_STR_FILTER: &str = r#"array::filter(["a", "bb", "ccc"], |s| str::len(s) > 1)"#;
run!(hof_str_filter, HOF_STR_FILTER, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "bb" && &*b == "ccc")
});

// No-match filter: EVERY string element hits the drop edge (the most
// likely place a leak/double-free in the owned-element drop would show).
const HOF_STR_FILTER_NONE: &str = r#"array::filter(["a", "b"], |s| str::len(s) > 5)"#;
run!(hof_str_filter_none, HOF_STR_FILTER_NONE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Array(a)) if a.is_empty()
));

// P4 firing rework (2026-07-10 late): kernels now run for this shape
// (the fired-element delivery + first-call priming unblocked it).
const HOF_STR_FOLD: &str =
    r#"array::fold(["a", "bb", "ccc"], 0, |acc, s| acc + str::len(s))"#;
run!(hof_str_fold, HOF_STR_FOLD, |v: Result<&Value>| matches!(v, Ok(Value::I64(6)));
    graphix_package_core::testing::FuseExpect::Jit);

// find RETURNS the matched string element (moved into the Nullable result);
// non-matches drop every iteration.
const HOF_STR_FIND: &str = r#"array::find(["a", "bb", "ccc"], |s| str::len(s) == 2)"#;
run!(hof_str_find, HOF_STR_FIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "bb"
));

const HOF_STR_FIND_NONE: &str = r#"array::find(["a", "b"], |s| str::len(s) == 9)"#;
run!(hof_str_find_none, HOF_STR_FIND_NONE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Null)
));

const HOF_STR_FLATMAP: &str = r#"array::flat_map(["a", "b"], |s| [s, s])"#;
run!(hof_str_flatmap, HOF_STR_FLATMAP, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 4]>()),
        Ok(Ok([a, b, c, d])) if &*a == "a" && &*b == "a" && &*c == "b" && &*d == "b")
});

// Element used TWICE in the body (interpolation reads `s` twice → two
// refcount clones vs the single element drop).
const HOF_STR_USED_TWICE: &str = r#"array::map(["a", "b"], |s| "[s][s]")"#;
run!(hof_str_used_twice, HOF_STR_USED_TWICE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "aa" && &*b == "bb")
});

// Value-shape (Nullable) ELEMENT read path: bind_elem's Value arm +
// drop_owned_elem exercised on the node-walk. The BODY here `select`s over
// the owned value element, which hits the owned-value-scrutinee de-fuse
// P4: the callback's per-site instance body (the select over the
// nullable element) fuses and runs as its own kernel — upgraded from
// None (the old owned-scrutinee gate applied to the deleted map
// scaffold, not to the instance region).
const HOF_NULLABLE_MAP: &str = r#"
array::map([1, null], |v| select v { i64 as n => n, null as _ => i64:0 })
"#;
run!(hof_nullable_map, HOF_NULLABLE_MAP, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 2]>()),
    Ok(Ok([1, 0]))
); graphix_package_core::testing::FuseExpect::Jit);

// Value-shape (variant) ELEMENT in a filter whose predicate is a `==`
// (ValueEq, fuses) rather than a select — so the value-element read + the
// keep-push Value arm fuse now (no owned-scrutinee dependency).
const HOF_VARIANT_FILTER: &str = r#"
array::filter([`Red, `Green, `Red], |v| v == `Red)
"#;
run!(hof_variant_filter, HOF_VARIANT_FILTER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 2)
});

// Value-shape variant element RETURNED by find (the find Value pack arm,
// pass-through) with the preceding non-match dropped (drop_owned_elem Value).
const HOF_VARIANT_FIND: &str = r#"
array::find([`Red, `Green, `Blue], |v| v == `Green)
"#;
run!(hof_variant_find, HOF_VARIANT_FIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "Green"
));

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// ─── Phase 5: composite / string / value destructure LEAVES ──────────
// `|(k, v)|` callbacks whose leaf is itself composite/string/value now
// fuse: the leaf is an OWNED clone bound as an env local (pending-exit
// drop for free) and dropped at body end on every edge.

const HOF_LEAF_COMPOSITE: &str = r#"
array::map([((1, 2), 10), ((3, 4), 20)], |(pt, n)| pt.0 + pt.1 + n)
"#;
run!(hof_leaf_composite, HOF_LEAF_COMPOSITE, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 2]>()),
    Ok(Ok([13, 27]))
));

// P4 firing rework (2026-07-10 late): kernels now run for this shape.
const HOF_LEAF_STRING: &str = r#"
array::fold([("a", 1), ("bb", 2)], 0, |acc, (s, n)| acc + str::len(s) + n)
"#;
run!(hof_leaf_string, HOF_LEAF_STRING, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); graphix_package_core::testing::FuseExpect::Jit);

// filter: the leaf drops pre-branch on BOTH edges (kept elements move,
// leaves never do) — no-match + all-match covered by the two predicates.
const HOF_LEAF_FILTER: &str = r#"
array::filter([((1, 2), 0), ((5, 6), 1)], |(pt, n)| pt.1 > 3)
"#;
run!(hof_leaf_filter, HOF_LEAF_FILTER, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Array(a)) if a.len() == 1
));

// A string leaf read TWICE in the body (two refcount clones vs one leaf
// drop), through interpolation.
const HOF_LEAF_STRING_TWICE: &str = r#"
array::map([("x", 1), ("y", 2)], |(s, n)| "[s][s][n]")
"#;
run!(hof_leaf_string_twice, HOF_LEAF_STRING_TWICE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "xx1" && &*b == "yy2")
});

// A nullable (value-shape) leaf: `==` over the two-word leaf fuses
// (ValueEq); the leaf's owned clone drops at body end.
const HOF_LEAF_NULLABLE: &str = r#"
array::filter_map([(1, 10), (2, 20)], |(k, v)| select k == 2 { true => v, false => null })
"#;
run!(hof_leaf_nullable, HOF_LEAF_NULLABLE, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 1]>()),
    Ok(Ok([20]))
));

// COMPOSITE-returning callbacks through the per-slot template kernel
// (soak jul05 items 5/10/13). The callback's declared rtype
// `['b, null]` freezes to the value-shape (in-band 2-word) return
// convention, but a composite body emits the composite convention
// (payload = *mut ValArray box pointer) — returning the raw pair
// handed the runtime decode a box pointer as the in-band ValArray
// word: SIGSEGV/SIGABRT. `emit_return_from_node` now widens the body
// to a genuine owned Value pair (via graphix_value_new_from_array),
// so these fuse AND return correctly.
const FIND_MAP_CAPTURED_ARRAY: &str = r#"
{let a = [i64:1, i64:2]; array::find_map(a, |x: i64| a)}
"#;
run!(find_map_captured_array, FIND_MAP_CAPTURED_ARRAY, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[i64; 2]>()), Ok(Ok([1, 2])))
}; graphix_package_core::testing::FuseExpect::Jit);

const FILTER_MAP_FRESH_ARRAY: &str = r#"
{
  let a = [i64:1, i64:2, i64:3];
  array::filter_map(a, |x| [x, x + i64:1])
}
"#;
run!(filter_map_fresh_array, FILTER_MAP_FRESH_ARRAY, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[[i64; 2]; 3]>()),
        Ok(Ok([[1, 2], [2, 3], [3, 4]])))
}; graphix_package_core::testing::FuseExpect::Jit);

// The select-arm variant: null in one arm, a tuple in the other — the
// widening runs per tail-select arm (emit_body_tail → the same
// emit_return_from_node seam).
const FIND_MAP_TUPLE_ARM: &str = r#"
{let a = [i64:1, i64:2]; array::find_map(a, |x: i64| select x { i64:1 => null, _ => (x, "s") })}
"#;
run!(find_map_tuple_arm, FIND_MAP_TUPLE_ARM, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<(i64, ArcStr)>()),
        Ok(Ok((2, s))) if &*s == "s")
}; graphix_package_core::testing::FuseExpect::Jit);
