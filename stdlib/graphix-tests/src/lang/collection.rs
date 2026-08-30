// The `Collection` trait (design/recursive_activations.md §6–§7): a
// constructor trait — `self<'a>` is the receiver's type constructor
// applied to the element type — implemented by core for `Array` and
// `Map`, by the list package for `List`, and by programs for their own
// types; `use Collection::*` makes `map(c, f)` mean the same thing
// whatever `c` is.

use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};

use netidx::publisher::Value;

// The built-in Array implementation: the intrinsics, reached through
// the trait's dispatcher.
run!(
    collection_array_map,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(12))),
    "/test.gx" => r#"
        use Collection::*;
        let r = map([1, 2, 3], |x| x * 2);
        let result = fold(r, 0, |a, x| a + x)
    "#
);

// The list implementation: the builtin `List` constructor head
// (compiler-known since `design/list_native.md`; the impl lives in
// core beside Array's).
run!(
    collection_list_fold_map_len,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(63))),
    "/test.gx" => r#"
        use Collection::*;
        let l = list::from_array([1, 2, 3]);
        let s = fold(l, 0, |a, x| a + x);
        let n = len(map(l, |x| x + 1));
        let result = s * 10 + n
    "#
);

// Map is a collection of its VALUES under the last-parameter hole
// (`self<'a>` ≡ `Map<'k, 'a>`): `map` keeps the keys and maps the
// values; `map::map` remains the pair operation.
run!(
    collection_map_values,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(301))),
    "/test.gx" => r#"
        use Collection::*;
        let m = {"a" => 1, "b" => 2};
        let m2 = map(m, |v| v * 10);
        let s = fold(m2, 0, |a, v| a + v);
        let kept = len(filter(m2, |v| v > 15));
        let result = s * 10 + kept
    "#
);

// A parameter typed by the trait is a constructor variable applied to
// a fresh element (`|c: Collection|` ≡ `'c: Collection, c: 'c<'e>`);
// each call site resolves the implementation for its own receiver.
run!(
    collection_generic_param,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(10))),
    "/test.gx" => r#"
        use Collection::*;
        let total = |c: Collection| fold(c, 0, |a, x| a + x);
        let result = total([1, 2]) + total(list::from_array([3, 4]))
    "#
);

// Newtype delegation (pressure test 1): three required methods over
// an abstract wrapper of an array; `filter`, `map`, `find` and `len`
// are the trait's defaults, derived from them.
run!(
    collection_newtype_defaults,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(240))),
    "/test.gx" => r#"
        type Bag<'a> = Abstract<Array<'a>>;
        impl Collection for Bag<'_> {
            let fold = |b, init, f| array::fold(b.0, init, f);
            let filter_map = |b, f| Bag(array::filter_map(b.0, f));
            let flat_map = |b, f| Bag(array::flat_map(b.0, |x| f(x).0))
        };
        use Collection::*;
        let b = filter(Bag([1, 2, 3, 4]), |x| x > 2);
        let m = map(b, |x| x * 10);
        let result = select find(m, |x| x > 35) {
            null as _ => -1,
            v => len(b) * 100 + v
        }
    "#;
    // The default bodies (`filter`/`map`/`find`) fuse now that
    // `resolve_static` registers the fn-param before the body typecheck,
    // so their derived callback `|x| f(x)` resolves the captured method.
    // Still partial: `len`'s `fold(c, 0, |n,_| n+1)` and the abstract
    // `Bag` payload paths node-walk (separate blockers), so this asserts
    // only that fusion now happens.
    FuseExpect::Jit
);

// A linear structure the program defines (pressure test 2's shape): a
// cons list as a union typedef, its required methods written as
// annotated recursions at module level. The head `L<'_>` names the
// constructor; a value's annotated type `L<i64>` decomposes to it by
// name, and a value whose cell holds the union's EXPANSION recovers it
// by unifying against the registered heads.
run!(
    collection_user_cons_list,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(122))),
    "/test.gx" => r#"
        type L<'a> = [`C('a, L<'a>), `N];
        let rec fold_l = |l: L<'a>, acc: 'b, f: fn(acc: 'b, x: 'a) -> 'b| -> 'b select l {
            `N => acc,
            `C(x, rest) => fold_l(rest, f(acc, x), f)
        };
        let rec filter_map_l = |l: L<'a>, f: fn(x: 'a) -> Option<'b>| -> L<'b> select l {
            `N => `N,
            `C(x, rest) => select f(x) { null as _ => filter_map_l(rest, f), y => `C(y, filter_map_l(rest, f)) }
        };
        let rec append_l = |a: L<'a>, b: L<'a>| -> L<'a> select a { `N => b, `C(x, rest) => `C(x, append_l(rest, b)) };
        let rec flat_map_l = |l: L<'a>, f: fn(x: 'a) -> L<'b>| -> L<'b> select l {
            `N => `N,
            `C(x, rest) => append_l(f(x), flat_map_l(rest, f))
        };
        impl Collection for L<'_> {
            let fold = |l, init, f| fold_l(l, init, f);
            let filter_map = |l, f| filter_map_l(l, f);
            let flat_map = |l, f| flat_map_l(l, f)
        };
        use Collection::*;
        let l: L<i64> = `C(1, `C(2, `C(3, `N)));
        let doubled = map(l, |x| x * 2);
        let s = fold(doubled, 0, |a, x| a + x);
        let result = s * 10 + len(filter(l, |x| x > 1))
    "#
);

// `find_map` is a default over `fold` with a typed accumulator seed.
run!(
    collection_find_map_default,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(30))),
    "/test.gx" => r#"
        type Bag<'a> = Abstract<Array<'a>>;
        impl Collection for Bag<'_> {
            let fold = |b, init, f| array::fold(b.0, init, f);
            let filter_map = |b, f| Bag(array::filter_map(b.0, f));
            let flat_map = |b, f| Bag(array::flat_map(b.0, |x| f(x).0))
        };
        use Collection::*;
        let result = select find_map(Bag([1, 2, 3]), |x| select x > 2 { true => x * 10, false => null }) {
            null as _ => -1,
            v => v
        }
    "#;
    // The return-position `select` over `find_map`'s NULLABLE result fuses
    // via THE UNIFIED RIDE's index dispatch (a value-shaped scrutinee's
    // bind reads the disc/payload directly, no cache).
    FuseExpect::Jit
);

// A union receiver is not a constructor.
run!(
    collection_union_receiver_rejected,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        use Collection::*;
        let x: [Array<i64>, null] = [1, 2];
        let result = map(x, |v| v + 1)
    "#;
    FuseExpect::None
);

// An impl head of a constructor trait names a constructor with the
// hole as its last parameter — a type without parameters is refused.
run!(
    collection_impl_non_constructor_rejected,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        impl Collection for i64 { let fold = |a, i, f| i; let filter_map = |a, f| a; let flat_map = |a, f| a };
        let result = 0
    "#;
    FuseExpect::None
);

// ... and so is a head with the last parameter filled.
run!(
    collection_impl_head_without_hole_rejected,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        type Bag<'a> = Abstract<Array<'a>>;
        impl Collection for Bag<i64> {
            let fold = |b, init, f| array::fold(b.0, init, f);
            let filter_map = |b, f| Bag(array::filter_map(b.0, f));
            let flat_map = |b, f| Bag(array::flat_map(b.0, |x| f(x).0))
        };
        let result = 0
    "#;
    FuseExpect::None
);

// The hole is legal only in such a head.
run!(
    collection_hole_outside_impl_rejected,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        let x: Array<'_> = [1, 2];
        let result = x
    "#;
    FuseExpect::None
);

// A trait spells its receiver one way: applied (`self<'a>`) throughout
// or bare throughout.
run!(
    collection_trait_mixed_self_rejected,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        trait Bad { val a: fn(self<'a>) -> i64; val b: fn(self) -> i64 };
        let result = 0
    "#;
    FuseExpect::None
);

// ---- P2b cross-implementation agreement (the semantic face of
// bench/collection/): the intrinsic, the trait default's body shape,
// and a hand-written Graphix recursion must agree on VALUES. The
// harness adds the cross-engine axis; bench/collection/ holds the
// timed differential over the same shapes.

run!(
    collection_bodies_fold_array,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let src = array::init(200, |i| i);
        let rec fold_go = |a: Array<i64>, f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64
            select i < array::len(a) {
                true => fold_go(a, f, i + 1, f(acc, a[i]$)),
                false => acc
            };
        let a = array::fold(src, 7, |a, x| a + x);
        let b = Collection::fold(src, 7, |a, x| a + x);
        let c = fold_go(src, |a, x| a + x, 0, 7);
        let result = (a == b) && (b == c)
    "#
);

run!(
    collection_bodies_map_array,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let rec map_go = |a: Array<i64>, f: fn(x: i64) -> i64, i: i64, out: Array<i64>| -> Array<i64>
            select i < array::len(a) {
                true => map_go(a, f, i + 1, array::push(out, f(a[i]$))),
                false => out
            };
        let a = array::map(src, |x| x * 3 + 1);
        let b = array::filter_map(src, |x| x * 3 + 1);
        let c = array::init(array::len(src), |i| src[i]$ * 3 + 1);
        let d = map_go(src, |x| x * 3 + 1, 0, []);
        let result = (a == b) && (b == c) && (c == d)
    "#
);

run!(
    collection_bodies_filter_array,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let a = array::filter(src, |x| x % 2 == 0);
        let b = array::filter_map(src, |x| select x % 2 == 0 {
            true => x,
            false => null
        });
        let result = a == b
    "#
);

run!(
    collection_bodies_find_array,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let ffold = |a: Array<i64>, target: i64| -> [i64, null] {
            let init: [i64, null] = null;
            array::fold(a, init, |acc, x| select acc {
                null as _ => select x == target { true => x, false => null },
                found => found
            })
        };
        let a1 = array::find(src, |x| x == 40);
        let b1 = ffold(src, 40);
        let a2 = array::find(src, |x| x == 999);
        let b2 = ffold(src, 999);
        let result = (a1 == b1) && (a2 == b2)
    "#
);

run!(
    collection_bodies_flat_map_array,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let empty: Array<i64> = [];
        let a = array::flat_map(src, |x| [x, x * 2]);
        let b = array::fold(src, empty, |acc, x| array::concat(acc, [x, x * 2]));
        let result = a == b
    "#
);

run!(
    collection_bodies_fold_list,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let l = list::init(200, |i| i);
        let rec lfold_go = |l: List<i64>, f: fn(acc: i64, x: i64) -> i64, acc: i64| -> i64
            select list::uncons(l) {
                null as _ => acc,
                (x, rest) => lfold_go(rest, f, f(acc, x))
            };
        let a = list::fold(l, 7, |a, x| a + x);
        let b = lfold_go(l, |a, x| a + x, 7);
        let result = a == b
    "#
);

run!(
    collection_bodies_fold_map,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r#"
        let empty: Map<i64, i64> = {};
        let m = array::fold(array::init(200, |i| i), empty, |m, i| map::insert(m, i, i * 2));
        let a = map::fold(m, 0, |acc, (_, v)| acc + v);
        let b = Collection::fold(m, 0, |acc, v| acc + v);
        let result = a == b
    "#
);

// A TOTAL filter_map callback (no null in its return type) can never
// produce the Null the intrinsic drops, so the emitter routes it to
// the map loop — the trait map DEFAULT's shape
// (`|c, f| filter_map(c, |x| f(x))`) fuses (P2b map-default widening,
// 2026-08-25).
const FILTER_MAP_TOTAL_CALLBACK: &str = r#"
{
  let a = array::init(10, |i| i);
  array::fold(array::filter_map(a, |x| x * 3 + 1), 0, |s, x| s + x)
}
"#;

run!(filter_map_total_callback, FILTER_MAP_TOTAL_CALLBACK, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(145))
); graphix_package_core::testing::FuseExpect::Jit);

// A collection callback with a labeled-DEFAULT parameter before its
// positional one, instantiated against the HOF's narrow declared type
// (`fn(x: 'a) -> 'b`), used to TRUNCATE the lambda's param patterns —
// the zip kept `foo` (bound to its default) and dropped the positional
// `x`, so the element was never delivered and the body's `x` fell
// through to an outer binding of the same name (aug27a katana: interp
// f64:0. / jit i64:0, both wrong — should be 2). The instance now
// carries both params (a narrow instance signature bails so the
// dynamic dispatch retries with the full definition signature).
const LABELED_CALLBACK_OUTER_SHADOW: &str = r#"
{
  let x = f64:0.;
  {
    let a = array::init(i64:3, |#foo: i64 = i64:42, x| x);
    array::fold(a, i64:0, |acc, x| x)
  }
}
"#;

run!(labeled_callback_outer_shadow, LABELED_CALLBACK_OUTER_SHADOW,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2)));
    graphix_package_core::testing::FuseExpect::Jit);

// The labeled default is actually READ in the body (foo + x): foo uses
// its default 42, x is the element, and the outer `x` must NOT leak.
// (42+10)+(42+20)+(42+30) = 186; the truncation bug gave 42+0. each.
const LABELED_CALLBACK_DEFAULT_USED: &str = r#"
{
  let x = f64:0.;
  array::fold(
    array::map([i64:10, i64:20, i64:30], |#foo: i64 = i64:42, x| foo + x),
    i64:0,
    |a, e| a + e
  )
}
"#;

run!(labeled_callback_default_used, LABELED_CALLBACK_DEFAULT_USED,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(186)));
    graphix_package_core::testing::FuseExpect::Jit);

// A HOF nested under ITS OWN callback is a nested loop, not recursion:
// the callback premats while the outer site resolves, so the inner
// site arrives with the outer def still active, and the recursion knot
// (keyed on def alone) stamped it with the outer INSTANCE — the
// analysis graph read `fold -> callback -> fold` as a cycle and the
// emitter refused the region as mutual recursion, so the shape
// node-walked, where every outer slot lazily instantiates the inner
// loop (bench/collection/flatmap_list, quadratic). The knot keys on
// instantiation identity now (`FnArgIdentity`: def + the source lambda
// each fn arg resolves to). The harness's demand for `Jit` is the pin.
const NESTED_SAME_INTRINSIC: &str = r#"
{
  let src = array::init(i64:200, |i| i);
  array::fold(src, i64:0, |acc, x| array::fold([x + i64:1, x * i64:2], acc, |a, y| a + y))
}
"#;

run!(nested_same_intrinsic, NESTED_SAME_INTRINSIC, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(59900))
); graphix_package_core::testing::FuseExpect::Jit);

// The same through `map`, with the inner loop reached via a named lambda.
const NESTED_MAP_IN_MAP: &str = r#"
{
  let src = array::init(i64:100, |i| i);
  let inner = |x| array::map([x, x + i64:1], |y| y * i64:2);
  array::fold(array::map(src, |x| inner(x)), i64:0, |a, ys| a + array::len(ys))
}
"#;

run!(nested_map_in_map, NESTED_MAP_IN_MAP, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(200))
); graphix_package_core::testing::FuseExpect::Jit);

// The same knot for a USER-written HOF: `apply` nested under its own
// callback. Not a collection intrinsic — the fix is the identity, not a
// special case.
const USER_HOF_NESTED: &str = r#"
{
  let apply = |f: fn(x: i64) -> i64, x: i64| f(x);
  let g = |y| y + i64:1;
  let src = array::init(i64:200, |i| i);
  array::fold(src, i64:0, |a, x| a + apply(|y| apply(g, y), x))
}
"#;

run!(user_hof_nested, USER_HOF_NESTED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(20100))
); graphix_package_core::testing::FuseExpect::Jit);

// Nested same-def use with DIFFERENT element types: each instantiation
// gets its own cells (a shared knot would unify the inner fold's
// strings against the outer's i64s).
const NESTED_MIXED_TYPES: &str = r#"
{
  let z = i64:0;
  z + array::fold([i64:1, i64:2], i64:0, |acc, x|
    acc + x + array::fold(["a", "bb"], i64:0, |a, s| a + str::len(s)))
}
"#;

run!(nested_mixed_types, NESTED_MIXED_TYPES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(9))
); graphix_package_core::testing::FuseExpect::Jit);
