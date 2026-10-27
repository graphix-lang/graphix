// Tests for tuples and structs

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::node_shape::{KernelMatcher, NodeShape};
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
};
use netidx::publisher::Value;

const TUPLES0: &str = r#"
{
  let t: (string, Number, Number) = ("foo", 42, 23.5);
  t
}
"#;

run!(tuples0, TUPLES0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s), Value::I64(42), Value::F64(23.5)] => &*s == "foo",
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

// A composite literal with a value-shape (Duration) field fuses.
const TUPLE_DURATION_FIELD: &str = r#"
{
  let t = (duration:1.s, 2);
  t
}
"#;

run!(tuple_duration_field, TUPLE_DURATION_FIELD, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::Duration(_), Value::I64(2)])
    }
    _ => false,
});

// A computed bool as a composite field: a total-order float comparison
// pushed into a struct reads false, not garbage.
const STRUCT_COMPUTED_BOOL_FIELD: &str = r#"
{ x: f64:0.1 < f64:0.1 }
"#;

run!(struct_computed_bool_field, STRUCT_COMPUTED_BOOL_FIELD, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(
        &a[..],
        [Value::Array(f)]
            if matches!(&f[..], [Value::String(_), Value::Bool(false)])
    ),
    _ => false,
}; FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const TUPLES1: &str = r#"
{
  let t: (string, i64, f64) = ("foo", 42, 23.5);
  let (_, y, z) = t;
  cast<f64>(y)$ + z
}
"#;

run!(tuples1, TUPLES1, is(Value::F64(65.5)); FuseExpect::Jit);

const TUPLES2: &str = r#"
{
  let t = ("foo", 42.0, 23.5);
  select t {
    ("foo", x, y) => x + y,
    _ => never()
  }
}
"#;

run!(tuples2, TUPLES2, is(Value::F64(65.5)); FuseExpect::Jit);

const TUPLEACCESSOR: &str = r#"
{
  let x = ( "bar", 42, 84.0 );
  x.1
}
"#;

run!(tupleaccessor, TUPLEACCESSOR, is(Value::I64(42)); FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const STRUCTS0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x
}
"#;

run!(structs0, STRUCTS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if a.len() == 3 => match &a[..] {
        [Value::Array(f0), Value::Array(f1), Value::Array(f2)]
            if f0.len() == 2 && f1.len() == 2 && f2.len() == 2 =>
        {
            let f0 = match &f0[..] {
                [Value::String(n), Value::I64(42)] if n == "bar" => true,
                _ => false,
            };
            let f1 = match &f1[..] {
                [Value::String(n), Value::F64(84.0)] if n == "baz" => true,
                _ => false,
            };
            let f2 = match &f2[..] {
                [Value::String(n), Value::String(s)] if n == "foo" && s == "bar" => true,
                _ => false,
            };
            f0 && f1 && f2
        }
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const BINDSTRUCT: &str = r#"
{
  let x = { foo: "bar", bar: 42.0, baz: 84.0 };
  let { foo: _, bar, baz } = x;
  bar + baz
}
"#;

run!(bindstruct, BINDSTRUCT, is(Value::F64(126.0)); FuseExpect::Jit);

const STRUCTACCESSOR: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x.foo
}
"#;

run!(structaccessor, STRUCTACCESSOR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => s == "bar",
    _ => false,
});

const STRUCTWITH0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let x = { x with foo: 1 };
  x.foo
}
"#;

run!(structwith0, STRUCTWITH0, refused("string does not contain i64"); FuseExpect::None);

const STRUCTWITH1: &str = r#"
{
  let x = { foo: "bar", bar: 42.0, baz: 84.0 };
  let x = { x with bar: 1.0 };
  x.bar + x.baz
}
"#;

// A struct-with copying an unchanged `string` field fuses.
run!(structwith1, STRUCTWITH1, is(Value::F64(85.0)); FuseExpect::Jit);

const STRUCTWITH2: &str = r#"
{
  let selected = { x: 0, y: 0 };
  let y = 1;
  { selected with y }
}
"#;

// `{ selected with y }` (field shorthand).
run!(structwith2, STRUCTWITH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH3: &str = r#"
{
  let selected = { x: 0, y: 0 };
  { selected with y: selected.y + 1 }
}
"#;

// `{ selected with y: selected.y + 1 }`: the replacement reads the
// source struct.
run!(structwith3, STRUCTWITH3, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH4: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up, `Down, `Left, `Right]| -> `Stop select e {
        e@ `Left => {
            selected <- e ~ { selected with x: selected.x - 1 };
            `Stop
        },
        e@ `Right => {
            selected <- e ~ { selected with x: selected.x + 1 };
            `Stop
        },
        e@ `Down => {
            selected <- e ~ { selected with y: selected.y + 1 };
            `Stop
        },
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up, `Down, `Left, `Right]));
    (array::group(selected, |n, _| n == 5))[1..]
}
"#;

run!(structwith4, STRUCTWITH4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 4]>() {
        Ok(
            [[(f00, 0), (f01, -1)], [(f10, 0), (f11, 0)], [(f20, -1), (f21, 0)], [(f30, 0), (f31, 0)]],
        ) if f00 == "x"
            && f01 == "y"
            && f10 == f00
            && f20 == f00
            && f30 == f00
            && f11 == f01
            && f21 == f01
            && f31 == f01 =>
            true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

const STRUCTWITH5: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up]| -> `Stop select e {
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up]));
    (array::group(selected, |n, _| n == 2))[1..]
}
"#;

run!(structwith5, STRUCTWITH5, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 1]>() {
        Ok([[(f00, 0), (f01, -1)]]) if f00 == "x" && f01 == "y" => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// A struct-with copying an unchanged composite field while replacing a
// scalar; reading a field back proves the copy.
const STRUCTWITH_COMPOSITE: &str = r#"
{
  let s = { pt: (i64:1, i64:2), n: i64:0 };
  let s2 = { s with n: i64:5 };
  s2.pt.1 + s2.n
}
"#;

run!(structwith_composite, STRUCTWITH_COMPOSITE, is(Value::I64(7)));

// A `#[native]` struct-with in the differential harness: a no-op with
// fusion off, native with it on.
const STRUCTWITH_NATIVE: &str = r#"
#[native]
{
  let s = { x: i64:1, y: i64:2 };
  ({ s with y: i64:9 }).y
}
"#;

run!(structwith_native, STRUCTWITH_NATIVE, is(Value::I64(9)));

// A may-bottom replacement field (`i64:10 / d`) that is runtime-clean.
const STRUCTWITH_MAYBOTTOM: &str = r#"
{
  let s = { x: i64:0, y: i64:0 };
  let d = i64:2;
  ({ s with x: i64:10 / d }).x
}
"#;

run!(structwith_maybottom, STRUCTWITH_MAYBOTTOM, is(Value::I64(5)));

// Composite / value-shape cross-kernel calls: the call must sit inside
// another lambda's body, so `g`'s kernel contains the call to `h`.

const CALL_TUPLE_ARG: &str = r#"
{
  let h = |p: (i64, i64), n: i64| p.0 + p.1 + n;
  let g = |a: i64, b: i64, c: i64| h((a, b), c);
  g(10, 20, 5)
}
"#;

run!(call_tuple_arg, CALL_TUPLE_ARG, is(Value::I64(35)); FuseExpect::Jit);

const CALL_STRUCT_ARG: &str = r#"
{
  let h = |s: {a: i64, b: i64}| s.a + s.b;
  let g = |x: i64, y: i64| h({a: x, b: y});
  g(3, 4)
}
"#;

run!(call_struct_arg, CALL_STRUCT_ARG, is(Value::I64(7)); FuseExpect::Jit);

// A value-shape (nullable) return from a lambda called inside the
// result block.
const CALL_NULLABLE_RETURN: &str = r#"
{
  let f = |x: i64| select x {
    0 => null,
    n => n
  };
  f(5)
}
"#;

run!(call_nullable_return, CALL_NULLABLE_RETURN, is(Value::I64(5)); FuseExpect::Jit);

// String equality.
const VALUE_EQ_STRING: &str = r#"
{
  let s = "hello";
  s == "hello"
}
"#;

run!(value_eq_string, VALUE_EQ_STRING, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const VALUE_EQ_STRING_NE: &str = r#"
{
  let s = "hello";
  s != "world"
}
"#;

run!(value_eq_string_ne, VALUE_EQ_STRING_NE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Tuple equality: a borrowed local against an owned literal.
const VALUE_EQ_TUPLE: &str = r#"
{
  let t = (1, 2);
  t == (1, 2)
}
"#;

run!(value_eq_tuple, VALUE_EQ_TUPLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// An out-of-range tuple index is a type error, not a compiler panic.
const TUPLE_INDEX_OOB: &str = r#"
{
  let t = (1, 2);
  t.5
}
"#;

run!(tuple_index_oob, TUPLE_INDEX_OOB, refused("no such field 5");
    FuseExpect::None);

// `{src with f}` where the source type sits behind TVars and the
// replacement typechecks a select over the same struct.
const STRUCT_WITH_SELECT_OVER_SOURCE: &str = r#"
{
  let g = |v: {x: i64, y: i64}| v;
  let t = g({x: 1, y: 2});
  {t with x: select t { {x, y} => x + y }}
}
"#;

run!(struct_with_select_over_source, STRUCT_WITH_SELECT_OVER_SOURCE, |v: Result<
    &Value,
>| match v {
    Ok(Value::Array(flds)) => match &flds[..] {
        [Value::Array(x), Value::Array(y)] => {
            matches!(&x[..], [Value::String(n), Value::I64(3)] if &**n == "x")
                && matches!(&y[..], [Value::String(n), Value::I64(2)] if &**n == "y")
        }
        _ => false,
    },
    _ => false,
});

// An unannotated partial struct pattern in a let completes against the
// value, as a select arm's does against its scrutinee.
run!(
    let_partial_struct_pattern,
    r#"{
    let p = {x: 1, y: 2};
    let {x, ..} = p;
    x
}"#,
    is(Value::I64(1))
);

// The value's type need not be known when the let compiles: a call's
// result completes at the check, nested too.
run!(
    let_partial_struct_over_a_call,
    r#"{
    let g = |a: i64| {x: a, y: a + 1};
    let v = g(1);
    let {y, ..} = v;
    let (k, {y: z, ..}) = (1, g(7));
    (y, k, z)
}"#,
    prints("[i64:2, i64:1, i64:8]")
);

run!(
    let_partial_struct_missing_field,
    r#"{
    let g = |a: i64| {x: a, y: a + 1};
    let {w, ..} = g(1);
    w
}"#,
    refused("can't be matched by");
    FuseExpect::None
);

// A field read over a union of structs of one shape, or of tuples of one
// arity, reads the field every member holds at one index: a typedef and
// its expansion, records differing in a variant field.
run!(
    field_read_over_a_union_of_one_shape,
    r#"{
    type Dom = {name: string};
    let o: [i64, null] = null;
    let a: {d: Dom} = {d: {name: "x"}};
    let b = {d: {name: "y"}};
    let r = select o { null as _ => b, _ => a };
    let xs = [{act: `A, n: 1}, {act: `B, n: 2}];
    let ts = [(1, "a"), (2, `B)];
    (r.d.name, array::map(xs, |x| x.n), xs[0]$.act, ts[1]$.0)
}"#,
    prints(r#"["y", [i64:1, i64:2], "A", i64:2]"#)
);

// A source typed by a typedef name is a struct or an abstract like any
// other: a functional update and a payload read go through the name.
run!(
    struct_with_through_a_typedef_name,
    r#"{
    type Point = {x: f64, y: f64};
    let w: {p: Point, n: i64} = {p: {x: 1.0, y: 2.0}, n: 0};
    let q = {(w.p) with x: 3.0};
    q.x + q.y
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == 5.0)
);

run!(
    abstract_payload_through_a_typedef_name,
    r#"{
    type Counter = Abstract<i64>;
    type W = {c: Counter, n: i64};
    let f = |w: W| -> i64 #[native] (w.c.0 + w.n);
    f({c: Counter(1), n: 2})
}"#,
    is(Value::I64(3))
);

// A let's pattern over a union is irrefutable when it fits every member:
// its binds narrow per member, as a select arm's do.
run!(
    let_destructures_a_union_of_one_shape,
    r#"{
  let f = |v: [(i64, string), (string, i64)]| { let (x, y) = v; "[x] [y]" };
  let g = |v: [{x: i64, y: string}, {x: string, y: i64}]| { let {x, y} = v; "[x] [y]" };
  (f((1, "s")), f(("s", 1)), g({x: 1, y: "s"}))
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["1 s", "s 1", "1 s"]"#)
);

run!(let_over_a_union_refuses_a_member_it_misses, r#"{
  let f = |v: [(i64, string), (string, i64, bool)]| { let (x, y) = v; x };
  f((1, "s"))
}"#, refused("refutable patterns are not allowed in let: (i64, string) matches only part of"); FuseExpect::None);

// A let's pattern over a value the compile has no type for yet (a
// select's, a call's) fits it at the check: each member of the union it
// turns out to be, and a whole capture is the union of the members.
run!(
    let_destructures_a_union_typed_by_the_check,
    r#"{
  let c = 1;
  let t = select c { 0 => (1, "x"), _ => ("y", 2) };
  let (a, b) = t;
  let f = |x: i64| select x { 0 => {p: 1, q: "x"}, _ => {p: "y", q: 2} };
  let {p, q} = f(0);
  let w@ (m, n) = select c { 1 => (1, "x"), _ => ("y", 2) };
  let u: [(i64, string), (string, i64)] = w;
  (b, a, p, q, u, m)
}"#,
    prints(r#"[i64:2, "y", i64:1, "x", [i64:1, "x"], i64:1]"#)
);

run!(let_over_a_later_typed_union_refuses_a_member_it_misses, r#"{
  let f = |x: i64| select x { 0 => (1, "x"), _ => ("y", 2) };
  let (1, y) = f(0);
  y
}"#, refused("refutable patterns are not allowed in let: (i64, string) matches only part of"); FuseExpect::None);

// A let of one name over a union keeps the value's type, so a writable
// reference to it is no reference to a generalized binding; a
// destructured union's binds are the same.
run!(let_over_a_union_takes_a_writable_reference, r#"{
  type Q = [`A(i64), `B(string)];
  let q = never<Q>();
  let set = |r: &mut Q| *r <- `A(1);
  let v: [(i64, string), (string, i64)] = (1, "s");
  let (x, y) = v;
  let rx = &mut x;
  *rx <- 2;
  set(&mut q);
  (q, x)
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"[["A", i64:1], i64:2]"#); FuseExpect::Jit);

run!(
    union_field_common_index,
    r#"
{
let f = |s: [{a: i64, b: i64}, {a: i64, c: i64}]| s.a;
[f({a: 1, b: 2}), f({a: 3, c: 4})]
}
"#,
    prints("[i64:1, i64:3]"); FuseExpect::None
);

run!(
    union_field_different_indices,
    r#"
{
let f = |s: [{a: i64, z: i64}, {z: i64}]| s.z;
[f({a: 1, z: 2}), f({z: 3})]
}
"#,
    prints("[i64:2, i64:3]"); FuseExpect::None
);

run!(
    union_field_different_types,
    r#"
{
let f = |s: [{a: i64, z: i64}, {z: string}]| s.z;
[f({a: 1, z: 2}), f({z: "three"})]
}
"#,
    prints("[i64:2, \"three\"]"); FuseExpect::None
);

run!(
    union_field_tuple_arities,
    r#"
{
let f = |t: [(i64, i64), (i64, i64, i64)]| t.1;
[f((1, 2)), f((3, 4, 5))]
}
"#,
    prints("[i64:2, i64:4]"); FuseExpect::None
);

run!(
    union_field_missing_struct,
    r#"
{
let f = |s: [{a: i64}, {b: i64}]| s.a;
f({a: 1})
}
"#,
    refused("unknown field a")
);

run!(
    union_field_missing_tuple,
    r#"
{
let f = |t: [(i64, i64), (i64, i64, i64)]| t.2;
f((1, 2))
}
"#,
    refused("no such field 2")
);

run!(
    union_field_replace_indices,
    r#"
{
let f = |s: [{a: i64, z: i64}, {z: i64}]| {s with z: 10};
[f({a: 1, z: 2}).z, f({z: 3}).z]
}
"#,
    prints("[i64:10, i64:10]"); FuseExpect::None
);
