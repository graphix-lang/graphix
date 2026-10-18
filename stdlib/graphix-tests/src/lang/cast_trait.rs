// An abstract type crosses data only through its `Cast` implementation:
// casts and typed reads make a value with `from_value`, writers write
// what `to_value` returns, and a method the implementation leaves out is
// the representation (design/abstract_cast.md).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::publisher::Value;

const SORTED: &str = r#"
type Sorted = Abstract<Array<i64>>;
impl Cast for Sorted {
    let to_value = |s: Sorted| s.0;
    let from_value = |v| select cast<Array<i64>>(v) {
        error as e => e,
        a => select array::sort(a) == a {
            true => Sorted(a),
            false => error(`InvalidCast("not sorted"))
        }
    }
};
type Pt = Abstract<{x: i64, y: [i64, null]}>;
impl Cast for Pt {};
type Doc = {name: string, nums: Sorted, at: Pt};
"#;

fn with_sorted(body: &str) -> String {
    format!("{{{SORTED}{body}}}")
}

// A cast makes a value by `from_value`, which may refuse it; a method
// left out reads the representation; a value already of the type is held.
run!(cast_reads_through_from_value, &with_sorted(r#"
let good: Any = [1, 2, 3];
let bad: Any = [3, 1];
let p: Any = {x: 5};
"[cast<Sorted>(good)] [cast<Sorted>(bad)] [cast<Pt>(p)] [cast<Pt>(Pt({x: 3, y: 4}))]"
"#), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"Sorted([1, 2, 3]) error:`InvalidCast("can't cast [3, 1] to Sorted: not sorted") Pt({x: 5, y: null}) Pt({x: 3, y: 4})"#)
}; FuseExpect::Jit);

// json writes `to_value`'s data, and the representation at its own type
// (a struct is an object); a read makes the values back, and refuses
// what `from_value` refuses.
run!(json_round_trip_through_cast, &with_sorted(r##"
let d: Doc = {name: "a", nums: Sorted([1, 2]), at: Pt({x: 1, y: null})};
let j = json::write_str(d)$;
let back: Result<Doc, [`JsonErr(string), `InvalidCast(string)]> = json::read(j);
let bad: Result<Doc, [`JsonErr(string), `InvalidCast(string)]> =
    json::read(r#"{"name": "b", "nums": [2, 1], "at": {"x": 3}}"#);
"[j] [back] [bad]"
"##), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"{"at":{"x":1,"y":null},"name":"a","nums":[1,2]} {at: Pt({x: 1, y: null}), name: "a", nums: Sorted([1, 2])} error:`InvalidCast("can't cast [2, 1] to Sorted: not sorted")"#)
}; FuseExpect::Jit);

// pack and toml carry the data too, never the abstract value.
run!(pack_and_toml_round_trip_through_cast, &with_sorted(r#"
let d: Doc = {name: "a", nums: Sorted([1, 2]), at: Pt({x: 1, y: null})};
let pb: Result<Doc, [`PackErr(string), `InvalidCast(string)]> =
    pack::read(pack::write_bytes(d)$);
let tb: Result<Doc, [`TomlErr(string), `InvalidCast(string)]> =
    toml::read(toml::write_str(d)$);
"[pb] [tb]"
"#), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"{at: Pt({x: 1, y: null}), name: "a", nums: Sorted([1, 2])} {at: Pt({x: 1, y: null}), name: "a", nums: Sorted([1, 2])}"#)
}; FuseExpect::Jit);

// A type with no implementation crosses no data, refused by the check: a
// cast, a read and a write.
run!(cast_without_cast_refused, r#"{
    type Box = Abstract<i64>;
    let v: Any = 3;
    cast<Box>(v)
}"#, refused("can't cast to Box: it holds an abstract type with no Cast implementation"); FuseExpect::None);

run!(read_without_cast_refused, r#"{
    type Box = Abstract<i64>;
    let r: Result<Box, [`JsonErr(string), `InvalidCast(string)]> = json::read("3");
    r
}"#, refused("Box has no Cast implementation, so no read, cast or writer reaches it"); FuseExpect::None);

run!(write_without_cast_refused, r#"{
    type Box = Abstract<i64>;
    json::write_str(Box(3))
}"#, refused("Box has no Cast implementation, so no read, cast or writer reaches it"); FuseExpect::None);

// A tree's value type is data: a handle backed by Rust crosses none.
run!(db_tree_of_a_handle_refused, r#"{
    let t: Result<db::Tree<string, db::Db>, `DbErr(string)> = db::tree(db::open("/nonexistent")$, null);
    t
}"#, refused("Db has no Cast implementation"); FuseExpect::None);

// A writer writes data: a function or a reference is refused.
run!(write_of_a_function_refused, r#"json::write_str(|x: i64| x)"#,
    refused("fn(x: i64) -> i64 is not data: a writer writes no function and no reference"); FuseExpect::None);

// Casts and writers call the methods, a program never does.
run!(cast_method_not_called_directly, r#"{
    type Box = Abstract<i64>;
    impl Cast for Box {};
    Cast::to_value(Box(3))
}"#, refused("Cast::to_value is called by casts and writers, never by a program"); FuseExpect::None);

run!(cast_impl_needs_an_abstract_type, r#"{
    type P = {x: i64};
    impl Cast for P {};
    0
}"#, refused("only an abstract type implements Cast, where its representation is visible"); FuseExpect::None);

// A representation holding a reference is no data to read or write.
run!(cast_impl_over_a_reference_refused, r#"{
    type R = Abstract<&mut i64>;
    impl Cast for R {};
    0
}"#, refused("its representation holds a function or a reference, which no data carries"); FuseExpect::None);

// A `to_value` that writes its own type inside its result is refused,
// not followed forever.
run!(cast_write_nesting_without_end_refused, r#"{
    type Loop = Abstract<i64>;
    impl Cast for Loop { let to_value = |s: Loop| [s] };
    select json::write_str(Loop(1)) { error as e => "[e]", s => s }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("Loop is written as data more than 1024 abstract values deep"))
}; FuseExpect::Jit);

// Only the module that defines the type implements Cast for it: a
// sibling sees no representation.
run!(
    cast_impl_outside_its_module_refused,
    refused("impl Cast for super::a::Box: only an abstract type implements Cast"),
    "/test.gx" => r#"
        mod a;
        mod b;
        0
    "#,
    "/test/a.gxi" => r#"
        type Box;
        val mk: fn(x: i64) -> Box
    "#,
    "/test/a.gx" => r#"
        type Box = Abstract<i64>;
        let mk = |x: i64| -> Box Box(x)
    "#,
    "/test/b.gx" => r#"
        impl Cast for super::a::Box {}
    "#; FuseExpect::None);

// An impl an interface declares may leave a method to the representation:
// the hidden type writes as its representation and reads through the
// module's check.
run!(
    cast_declared_in_an_interface,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if &**s == r#"[3] error:`InvalidCast("can't cast [-1] to Pos: negative") Pos([5])"#)
    },
    "/test.gx" => r#"
        mod inner;
        let w = json::write_str(inner::mk(3))$;
        let bad: Result<inner::Pos, [`JsonErr(string), `InvalidCast(string)]> = json::read("\[-1\]");
        let good: Result<inner::Pos, [`JsonErr(string), `InvalidCast(string)]> = json::read("\[5\]");
        let result = "[w] [bad] [good]"
    "#,
    "/test/inner.gxi" => r#"
        type Pos;
        impl Cast for Pos;
        val mk: fn(x: i64) -> Pos
    "#,
    "/test/inner.gx" => r#"
        type Pos = Abstract<Array<i64>>;
        impl Cast for Pos {
            let from_value = |v| select cast<Array<i64>>(v) {
                error as e => e,
                a => select array::find(a, |x| x < 0) {
                    null as _ => Pos(a),
                    _ => error(`InvalidCast("negative"))
                }
            }
        };
        let mk = |x: i64| -> Pos Pos([x])
    "#; FuseExpect::Jit);
