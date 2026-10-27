// The `Collection` constructor trait: `self<'a>` is the receiver's type
// constructor applied to the element type; `use Collection::*` makes
// `map(c, f)` mean the same thing whatever `c` is.

use anyhow::Result;

use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, is, prints, refused},
};
use netidx::publisher::Value;

// The built-in Array implementation: the intrinsics, reached through
// the trait's dispatcher.
run!(
    collection_array_map,
    is(Value::I64(12)),
    "/test.gx" => r#"
        use Collection::*;
        let r = map([1, 2, 3], |x| x * 2);
        let result = fold(r, 0, |a, x| a + x)
    "#
);

// The list implementation.
run!(
    collection_list_fold_map_len,
    is(Value::I64(63)),
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
    is(Value::I64(301)),
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
    is(Value::I64(10)),
    "/test.gx" => r#"
        use Collection::*;
        let total = |c: Collection| fold(c, 0, |a, x| a + x);
        let result = total([1, 2]) + total(list::from_array([3, 4]))
    "#
);

// Newtype delegation: three required methods over an abstract wrapper
// of an array; `filter`, `map`, `find` and `len` are the defaults.
run!(
    collection_newtype_defaults,
    is(Value::I64(240)),
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
        let result = select #[native] find(m, |x| x > 35) {
            null as _ => -1,
            v => len(b) * 100 + v
        }
    "#;
    // The default bodies fuse, `find` whole; `len` and the abstract
    // payload paths node-walk.
    FuseExpect::Jit
);

// `find`'s default over a map: the first value `f` accepts, a null one
// included, which ends the search (`5` after it is not the answer); the
// fold fuses.
run!(
    collection_map_find_stops_at_a_null,
    prints("[null, i64:5, null]"),
    "/test.gx" => r#"
        use Collection::*;
        let m: Map<i64, [i64, null]> = {1 => 4, 2 => null, 3 => 5};
        let result = (
            #[native] find(m, |x: [i64, null]| select x { null as _ => true, 5 => true, _ => false }),
            #[native] find(m, |x: [i64, null]| select x { 5 => true, _ => false }),
            #[native] find(m, |x: [i64, null]| select x { 7 => true, _ => false })
        )
    "#;
    FuseExpect::Jit
);

// A cons list the program defines as a union typedef; `L<'_>` names the
// constructor and `L<i64>` decomposes to it by name.
run!(
    collection_user_cons_list,
    is(Value::I64(122)),
    "/test.gx" => r#"
        type L<'a> = [`C('a, L<'a>), `N];
        let rec fold_l = |l: L<'a>, acc: 'b, f: fn(acc: 'b, x: 'a) -> 'b throws 'e| -> 'b throws 'e select l {
            `N => acc,
            `C(x, rest) => fold_l(rest, f(acc, x), f)
        };
        let rec filter_map_l = |l: L<'a>, f: fn(x: 'a) -> ['b, null, `Keep('b)] throws 'e| -> L<'b> throws 'e select l {
            `N => `N,
            `C(x, rest) => select f(x) {
                null as _ => filter_map_l(rest, f),
                `Keep(y) => `C(y, filter_map_l(rest, f)),
                y => `C(y, filter_map_l(rest, f))
            }
        };
        let rec append_l = |a: L<'a>, b: L<'a>| -> L<'a> select a { `N => b, `C(x, rest) => `C(x, append_l(rest, b)) };
        let rec flat_map_l = |l: L<'a>, f: fn(x: 'a) -> L<'b> throws 'e| -> L<'b> throws 'e select l {
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
    is(Value::I64(30)),
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
    // The return-position select over `find_map`'s nullable result fuses.
    FuseExpect::Jit
);

// A union receiver is not a constructor.
run!(
    collection_union_receiver_rejected,
    refused("does not contain [null, Array<i64>]"),
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
    refused("impl Collection for i64: Collection is a constructor trait"),
    "/test.gx" => r#"
        impl Collection for i64 { let fold = |a, i, f| i; let filter_map = |a, f| a; let flat_map = |a, f| a };
        let result = 0
    "#;
    FuseExpect::None
);

// ... and so is a head with the last parameter filled.
run!(
    collection_impl_head_without_hole_rejected,
    refused("impl Collection for Bag<i64>: Collection is a constructor trait"),
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
    refused("'_ is the hole of a constructor trait's implementation target"),
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
    refused("a trait spells its receiver one way throughout"),
    "/test.gx" => r#"
        trait Bad { val a: fn(self<'a>) -> i64; val b: fn(self) -> i64 };
        let result = 0
    "#;
    FuseExpect::None
);

// Cross-implementation agreement: the intrinsic, the trait default's
// body shape, and a hand-written recursion agree on values.

// CR claude for claude: [test-gap/low] `b = Collection::fold(src, ..)` dispatches to
// the Array impl, which is the 'array_fold intrinsic, so `a == b` compares array::fold
// with itself and cannot fail. The map and find siblings use shapes that are not the
// trait defaults' either (no `Keep, no `Found). Use the defaults' real shapes over the
// array, e.g. `array::filter_map(src, |x| `Keep(x * 3 + 1))`, and drop the
// self-comparison. Found by reading; not run. (tests-lang-rest-b-07)
run!(
    collection_bodies_fold_array,
    is(Value::Bool(true)),
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
    is(Value::Bool(true)),
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
    is(Value::Bool(true)),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let a = array::filter(src, |x| x % 2 == 0);
        let b = array::filter_map(src, |x| select x % 2 == 0 {
            true => x,
            false => null
        });
        let result = a == b && array::len(a) == 32
    "#
);

run!(
    collection_bodies_find_array,
    is(Value::Bool(true)),
    "/test.gx" => r#"
        let src = array::init(64, |i| i);
        let ffold = |a: Array<i64>, target: i64| -> [i64, null] {
            let init: [i64, null] = null;
            let (_, found) = array::fold(a, (false, init), |(done, acc), x| select done {
                true => (done, acc),
                false => select x == target { true => (true, x), false => (false, acc) }
            });
            found
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
    is(Value::Bool(true)),
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
    is(Value::Bool(true)),
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
    is(Value::Bool(true)),
    "/test.gx" => r#"
        let empty: Map<i64, i64> = {};
        let m = array::fold(array::init(200, |i| i), empty, |m, i| map::insert(m, i, i * 2));
        let a = map::fold(m, 0, |acc, (_, v)| acc + v);
        let b = Collection::fold(m, 0, |acc, v| acc + v);
        let result = a == b && a == 39800
    "#
);

// A total filter_map callback (no null in its return type) routes to
// the map loop, so the trait map default's shape fuses.
// CR claude for claude: [readability/low] The comment above says the trait map
// default's shape fuses. The default is `filter_map(c, |x| `Keep(f(x)))`, and a
// `Keep-returning callback node-walks: `#[native] map(Bag(..), f)` is refused at
// `array::filter_map(b.0, f)`. This fixture shows a total callback fusing, a different
// shape. The same claim ('The default bodies fuse') sits on
// collection_newtype_defaults, where only find's default fuses. Reword both. probe:
// design/review-2026-10-26/repro/tests-lang-rest-b-keep-default-nodewalks.gx
// (tests-lang-rest-b-05)
// 2026-10-26 claude: f-call-flow-01 made a `Keep callback fuse, so the trait map
// default's shape (`|x| `Keep(f(x))`) now fuses; the comment's claim holds for it, but
// this fixture still tests a total callback, not that shape.
// lang::collection::filter_map_keep_fuses pins the default's shape under #[native].
const FILTER_MAP_TOTAL_CALLBACK: &str = r#"
{
  let a = array::init(10, |i| i);
  #[native] array::fold(array::filter_map(a, |x| x * 3 + 1), 0, |s, x| s + x)
}
"#;

run!(filter_map_total_callback, FILTER_MAP_TOTAL_CALLBACK, is(Value::I64(145)); FuseExpect::Jit);

// A collection callback with a labeled-default parameter before its
// positional one receives the element in the positional slot; an outer
// binding of the same name does not leak in. Result 2.
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
    is(Value::I64(2));
    FuseExpect::Jit);

// The labeled default is read in the body: (42+10)+(42+20)+(42+30) = 186.
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
    is(Value::I64(186));
    FuseExpect::Jit);

// A callback with a labeled parameter has no fused loop: the collection
// interprets it, and #[native] names the parameter.
run!(labeled_callback_map_node_walks,
    "#[native] array::map([10, 20, 30], |#foo: i64 = 42, x| foo + x)",
    refused("a collection callback with a labeled parameter (`#foo`) has no fused loop");
    FuseExpect::None; jit_only);

run!(labeled_callback_fold_node_walks,
    "#[native] array::fold([1, 2], 0, |#foo: i64 = 42, a, x| a + x + foo)",
    refused("a collection callback with a labeled parameter (`#foo`) has no fused loop");
    FuseExpect::None; jit_only);

// A HOF nested under its own callback is a nested loop, not recursion:
// instantiation keys on identity, so the shape fuses.
const NESTED_SAME_INTRINSIC: &str = r#"
{
  let src = array::init(i64:200, |i| i);
  #[native] array::fold(src, i64:0, |acc, x| array::fold([x + i64:1, x * i64:2], acc, |a, y| a + y))
}
"#;

run!(nested_same_intrinsic, NESTED_SAME_INTRINSIC, is(Value::I64(59900)); FuseExpect::Jit);

// The same through `map`, with the inner loop reached via a named lambda.
const NESTED_MAP_IN_MAP: &str = r#"
{
  let src = array::init(i64:100, |i| i);
  let inner = |x| array::map([x, x + i64:1], |y| y * i64:2);
  #[native] array::fold(array::map(src, |x| inner(x)), i64:0, |a, ys| a + array::len(ys))
}
"#;

run!(nested_map_in_map, NESTED_MAP_IN_MAP, is(Value::I64(200)); FuseExpect::Jit);

// The same for a user-written HOF: `apply` nested under its own callback.
const USER_HOF_NESTED: &str = r#"
{
  let apply = |f: fn(x: i64) -> i64, x: i64| f(x);
  let g = |y| y + i64:1;
  let src = array::init(i64:200, |i| i);
  #[native] array::fold(src, i64:0, |a, x| a + apply(|y| apply(g, y), x))
}
"#;

run!(user_hof_nested, USER_HOF_NESTED, is(Value::I64(20100)); FuseExpect::Jit);

// Nested same-def use with different element types: each instantiation
// gets its own cells.
const NESTED_MIXED_TYPES: &str = r#"
{
  let z = i64:0;
  #[native] (z + array::fold([i64:1, i64:2], i64:0, |acc, x|
    acc + x + array::fold(["a", "bb"], i64:0, |a, s| a + str::len(s))))
}
"#;

run!(nested_mixed_types, NESTED_MIXED_TYPES, is(Value::I64(9)); FuseExpect::Jit);

// A trait-typed parameter of a fn type takes a fresh element at each
// call, so a rank-2 formal applies to collections of two element types.
run!(
    collection_rank2_elements_per_call,
    is(Value::I64(21)),
    "/test.gx" => r#"
        use Collection::*;
        let n = |c: Collection| fold(c, 0, |acc, x| acc + 1);
        let apply = |f: fn(c: Collection) -> i64| (f([1, 2]), f(["x"]));
        let (a, b) = apply(n);
        let result = a * 10 + b
    "#
);

// A callback that returns a function with its own quantifier leaves the
// quantifier open: each call of an element picks it.
run!(collection_returns_a_generic_function, r#"{
    let fs = array::map([1, 2], |r| 'a: Number |#start: 'a = 0, x: 'a| -> 'a start + x);
    let g = fs[0]$;
    g(#start: 3, 4)
}"#, is(Value::I64(7)); FuseExpect::None);

// A collection fires when it resizes or its source comes back from bottom,
// whatever tag the source arrives with (an arm waking after another arm
// consumed the source's fire), as the fused loop does.
async fn collection_fires_on_a_stale_resize(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    use graphix_package_core::testing::as_i64s;
    let (values, _) = run_delta(
        r#"{
            let n = 0;
            n <- select n { 8 => never(), v => v + 1 };
            let k = uniq(select n { 0 => 0, 1 => 0, 2 => 0, 3 => 2, _ => 2 });
            let s = uniq(select n { 0 => 1, 1 => 1, 2 => 0, 3 => 0, _ => 1 });
            let xs = select k { 0 => [1, 2, 3], _ => [1] };
            select s {
                0 => array::len(xs) - 100,
                _ => array::map(xs, |x| x) ~ n
            }
        }"#,
        mode,
    )
    .await?;
    assert_eq!(as_i64s(&values), vec![0, -97, -99, 4]);
    Ok(())
}

// What a slot's run-time callback reads is the collection's: a fire it
// missed while its arm slept is raised at the wake.
async fn collection_refs_reach_its_slots(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
            let a = 0;
            let mode = 0;
            let g = select mode { 0 => |x| a ~ x, _ => |x| x };
            let n = 0;
            let dummy = 0;
            n <- select n { k if k < 12 => k + 1, _ => never() };
            a <- select n { 6 => 7, _ => never() };
            select n { 2 | 3 | 9 => { dummy <- n; array::map([n], g) }, _ => never() }
        }"#,
        mode,
    )
    .await?;
    assert_eq!(values, vec![Value::Array([Value::I64(9)].into())]);
    Ok(())
}

// A bottom source runs no slot: its slots pause until it returns, so a
// raise only a slot under the bottom source would make never happens, and
// a kernel runs no phantom slot over the source's placeholder.
async fn bottom_source_runs_no_slot(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
            let t = array::iter([0, 1, 2]);
            let phantom = 0;
            let retained = 0;
            let a = {
                catch(e) phantom <- e ~ phantom + 1;
                array::map(array::map([1, 2], |z| z + t), |x| {
                    let e: [i64, Error<`E>] = error(`E);
                    e? + x
                })
            };
            let b = {
                catch(e) retained <- e ~ retained + 1;
                let src = select t { 1 => never<Array<i64>>(), _ => [1, 2] };
                array::map(src, |x| {
                    let e: [i64, Error<`E>] = select t { 1 => error(`E), n => n };
                    e? + x
                })
            };
            (phantom, retained)
        }"#,
        mode,
    )
    .await?;
    let pair = |a, b| Value::Array([Value::I64(a), Value::I64(b)].into());
    assert_eq!(values.last(), Some(&pair(2, 0)), "{values:?}");
    Ok(())
}

// The slots a bottom source paused wake caught up when it returns: an
// event a slot reads that fired meanwhile is raised again, so a standing
// slot answers as a fresh one does.
async fn bottom_source_slots_wake_caught_up(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
            let a = 0;
            let n = 0;
            n <- select n { k if k < 12 => k + 1, _ => never() };
            a <- select n { 6 => 7, _ => never() };
            let xs = select n { 2 => [n], 9 => [n, n], _ => never() };
            (array::map(xs, |x| a ~ x), array::fold(xs, 0, |acc, x| acc + (a ~ x)))
        }"#,
        mode,
    )
    .await?;
    let pair = |m: &[i64], f| {
        let m = Value::Array(m.iter().map(|i| Value::I64(*i)).collect());
        Value::Array([m, Value::I64(f)].into())
    };
    assert_eq!(values, vec![pair(&[2], 2), pair(&[9, 9], 18)]);
    Ok(())
}

modes!(
    collection_fires_on_a_stale_resize,
    collection_refs_reach_its_slots,
    bottom_source_runs_no_slot,
    bottom_source_slots_wake_caught_up
);

// filter_map drops an element on null and keeps `Keep(v), null included,
// so the derived map and filter of a program-defined collection keep its
// null elements.
run!(
    filter_map_keeps_with_keep,
    prints("[[i64:1, null, i64:3], [i64:1, i64:3], i64:3, i64:3, i64:2]"),
    "/test.gx" => r#"
        type L<'a> = [`C('a, L<'a>), `N];
        let rec fold_l = |l: L<'a>, acc: 'b, f: fn(acc: 'b, x: 'a) -> 'b throws 'e| -> 'b throws 'e select l {
            `N => acc,
            `C(x, rest) => fold_l(rest, f(acc, x), f)
        };
        let rec filter_map_l = |l: L<'a>, f: fn(x: 'a) -> ['b, null, `Keep('b)] throws 'e| -> L<'b> throws 'e select l {
            `N => `N,
            `C(x, rest) => select f(x) {
                null as _ => filter_map_l(rest, f),
                `Keep(y) => `C(y, filter_map_l(rest, f)),
                y => `C(y, filter_map_l(rest, f))
            }
        };
        let rec append_l = |a: L<'a>, b: L<'a>| -> L<'a> select a { `N => b, `C(x, rest) => `C(x, append_l(rest, b)) };
        let rec flat_map_l = |l: L<'a>, f: fn(x: 'a) -> L<'b> throws 'e| -> L<'b> throws 'e select l {
            `N => `N,
            `C(x, rest) => append_l(f(x), flat_map_l(rest, f))
        };
        impl Collection for L<'_> {
            let fold = |l, init, f| fold_l(l, init, f);
            let filter_map = |l, f| filter_map_l(l, f);
            let flat_map = |l, f| flat_map_l(l, f)
        };
        let a: Array<[i64, null]> = [1, null, 3];
        let ln: L<[i64, null]> = `C(1, `C(null, `C(3, `N)));
        let m: Map<string, [i64, null]> = {"a" => 1, "b" => null};
        let result = [
            cast<Any>(array::filter_map(a, |x| `Keep(x)))$,
            cast<Any>(array::filter_map(a, |x| x))$,
            cast<Any>(Collection::len(Collection::map(ln, |x| x)))$,
            cast<Any>(Collection::len(Collection::filter(ln, |x| true)))$,
            cast<Any>(map::len(map::filter_map(m, |(k, v)| `Keep((k, v)))))$
        ]
    "#;
    FuseExpect::Jit
);

// A filter_map callback that keeps with `Keep fuses: the derived map and
// filter of a program-defined collection, and `Keep over each flavor.
run!(
    filter_map_keep_fuses,
    prints(r#"[[i64:1, i64:2, i64:4], [i64:10, i64:20, i64:30], [i64:3, i64:4], {"a" => i64:1, "b" => null}, [i64:1, null]]"#),
    "/test.gx" => r#"
        type Bag<'a> = Abstract<Array<'a>>;
        impl Collection for Bag<'_> {
            let fold = |b, init, f| array::fold(b.0, init, f);
            let filter_map = |b, f| Bag(array::filter_map(b.0, f));
            let flat_map = |b, f| Bag(array::flat_map(b.0, |x| f(x).0))
        };
        let m: Map<string, [i64, null]> = {"a" => 1, "b" => null, "c" => 3};
        let l: List<[i64, null]> = [<1, null, 3>];
        let result = (
            #[native] array::filter_map([1, 2, 3, 4], |x| select x { 3 => null, v => `Keep(v) }),
            #[native] Collection::map(Bag([1, 2, 3]), |x| x * 10).0,
            #[native] Collection::filter(Bag([1, 2, 3, 4]), |x| x > 2).0,
            #[native] map::filter_map(m, |(k, v)| select v { 3 => null, v => `Keep((k, v)) }),
            #[native] list::filter_map(l, |x| select x { 3 => null, v => `Keep(v) })
        )
    "#;
    FuseExpect::Jit
);
