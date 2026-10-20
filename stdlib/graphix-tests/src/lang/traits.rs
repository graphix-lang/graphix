// Traits: declarations, implementations, bounds, static dispatch, and
// dispatch over a union self type.

use anyhow::Result;

use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, is, prints, refused},
};
use netidx::publisher::Value;

// A trait with one required method, implemented for an abstract type;
// called through its trait path.
run!(
    trait_basic_abstract,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(42)"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let result = Show::show(Counter(42))
    "#
);

// The trait's methods are module-like items: `use Trait::*` makes them
// callable bare. An implementation over a primitive is legal in the
// trait's own package.
run!(
    trait_use_bare_and_primitive,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 7"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        use Show::*;
        let result = show(7)
    "#
);

// A default method calls a sibling method bare; the implementor
// inherits it.
run!(
    trait_default_method,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 1 int 1"),
    "/test.gx" => r#"
        trait Show {
            val show: fn(self) -> string;
            val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
        };
        impl Show for i64 { let show = |x| "int [x]" };
        let result = Show::twice(1)
    "#
);

// An implementation may override a default.
run!(
    trait_default_overridden,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "own twice"),
    "/test.gx" => r#"
        trait Show {
            val show: fn(self) -> string;
            val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
        };
        impl Show for i64 {
            let show = |x| "int [x]";
            let twice = |x| "own twice"
        };
        let result = Show::twice(1)
    "#
);

// A trait as a bound: the generic body resolves per instance.
run!(
    trait_bound_generic,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "<int 3> <Counter(4)>"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        impl Show for i64 { let show = |x| "int [x]" };
        use Show::show;
        let describe = 'a: Show |x: 'a| "<[show(x)]>";
        let result = "[describe(3)] [describe(Counter(4))]"
    "#
);

// A trait written as a parameter's type is a fresh bound per parameter
// (`fn(a: Show, b: Show)` is two variables).
run!(
    trait_arg_position,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 3 Counter(4)"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        impl Show for i64 { let show = |x| "int [x]" };
        use Show::show;
        let both = |a: Show, b: Show| "[show(a)] [show(b)]";
        let result = both(3, Counter(4))
    "#
);

// Calling with a type that has no implementation is a compile error.
run!(
    trait_no_impl_refused,
    refused("string does not implement Show"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        let result = Show::show("not an int")
    "#
    ; FuseExpect::None
);

// A required method missing from an implementation is a compile error.
run!(
    trait_missing_method_refused,
    refused("is missing the required method other"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string; val other: fn(self) -> i64 };
        impl Show for i64 { let show = |x| "int [x]" };
        let result = Show::show(1)
    "#
    ; FuseExpect::None
);

// Two implementations for one type conflict.
run!(
    trait_duplicate_impl_refused,
    refused("conflicting implementation: Show is already implemented for i64"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        impl Show for i64 { let show = |x| "again [x]" };
        let result = Show::show(1)
    "#
    ; FuseExpect::None
);

// Dispatch over a union self type picks the member's implementation at
// runtime.
run!(
    trait_union_dispatch,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 5 Counter(6)"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        impl Show for i64 { let show = |x| "int [x]" };
        let pick = |b: bool, i: i64, c: Counter| -> [i64, Counter] select b { true => i, false => c };
        let result = "[Show::show(pick(true, 5, Counter(6)))] [Show::show(pick(false, 5, Counter(6)))]"
    "#
    ; FuseExpect::Jit
);

// A union member without an implementation is refused at the call.
run!(
    trait_union_member_missing_refused,
    refused("string does not implement Show"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        let pick = |b: bool| -> [i64, string] select b { true => 1, false => "s" };
        let result = Show::show(pick(true))
    "#
    ; FuseExpect::None
);

// An interface-declared `fn(c: Collection)` exports and dispatches on
// both an Array and a Map.
run!(
    collection_generic_interface_declared,
    is(Value::I64(5)),
    "/test.gx" => r#"
mod m;
let result = m::csize([i64:1, i64:2, i64:3]) + m::csize({"a" => i64:1, "b" => i64:2})
"#,
    "/test/m.gxi" => r#"
val csize: fn(c: Collection) -> i64;
"#,
    "/test/m.gx" => r#"
let csize = |c: Collection| Collection::fold(c, i64:0, |acc, x| acc + i64:1)
"#
    ; FuseExpect::Jit);

// A `never()` arm is the identity of the dispatch union: `[⊥, Counter]`
// dispatches on Counter.
run!(
    trait_dispatch_never_arm_union,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(3)"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let opt: [Counter, null] = Counter(3);
        let c = select opt { null as _ => never(), s => s };
        let result = Show::show(c)
    "#
);

// A trait method as a higher-order argument: the callback's call sites
// resolve by the element type.
run!(
    trait_method_as_callback,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::Array(a)) if &a[..] == [Value::from("int 1"), Value::from("int 2")])
    },
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        let result = array::map([1, 2], Show::show)
    "#
);

// A parameterized head: the element bound discharges through the
// implementation table.
run!(
    trait_parameterized_head,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "[int 1, int 2]"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        impl<'a: Show> Show for Array<'a> {
            let show = |xs| "\[[str::join(#sep: ", ", array::map(xs, Show::show))]\]"
        };
        let result = Show::show([1, 2])
    "#
    ; FuseExpect::Jit
);

// The interface declares the trait and the implementation; a consumer
// module dispatches through the interface's declarations.
run!(
    trait_interface_declared,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(9)"),
    "/test.gx" => r#"
        mod m;
        let result = m::Show::show(m::make(9))
    "#,
    "/test/m.gxi" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter;
        impl Show for Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/m.gx" => r#"
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    "#
);

// A method named as a value outside the trait's module: its signature's
// names resolve where the trait wrote them, not where it is named.
run!(
    trait_method_value_from_another_module,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(9)"),
    "/test.gx" => r#"
        mod m;
        let show = m::Show::show;
        let result = show(m::make(9))
    "#,
    "/test/m.gxi" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter;
        impl Show for Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/m.gx" => r#"
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    "#
);

// A program is one package: a sibling module may implement the trait
// for a primitive.
run!(
    trait_impl_in_sibling_module,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 1"),
    "/test.gx" => r#"
        mod t;
        mod u;
        let result = t::Show::show(1)
    "#,
    "/test/t.gxi" => r#"
        trait Show { val show: fn(self) -> string }
    "#,
    "/test/t.gx" => r#"
        let unused = 0
    "#,
    "/test/u.gx" => r#"
        use super::t::Show;
        impl Show for i64 { let show = |x| "int [x]" }
    "#
);

// Modules with interfaces reach each other only through them: an impl
// a body adds without declaring it is unseen by its siblings' checks,
// and seen by the statements after them.
run!(
    trait_undeclared_impl_hidden_from_siblings,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("compiling module")
        && format!("{e:#}").contains("::test::b")
        && format!("{e:#}").contains("i64 does not implement Show here")
        && format!("{e:#}").contains("is not in its interface")),
    "/test.gx" => r#"
        mod t;
        mod a;
        mod b;
        let result = b::shown
    "#,
    "/test/t.gxi" => r#"
        trait Show { val show: fn(self) -> string }
    "#,
    "/test/t.gx" => r#"
        let unused = 0
    "#,
    "/test/a.gxi" => r#"
        val unused: i64
    "#,
    "/test/a.gx" => r#"
        use super::t::Show;
        impl Show for i64 { let show = |x| "int [x]" };
        let unused = 0
    "#,
    "/test/b.gxi" => r#"
        val shown: string
    "#,
    "/test/b.gx" => r#"
        let shown = super::t::Show::show(1)
    "#
    ; FuseExpect::None);

run!(
    trait_undeclared_impl_seen_after_its_siblings,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 1"),
    "/test.gx" => r#"
        mod t;
        mod a;
        let result = t::Show::show(1)
    "#,
    "/test/t.gxi" => r#"
        trait Show { val show: fn(self) -> string }
    "#,
    "/test/t.gx" => r#"
        let unused = 0
    "#,
    "/test/a.gxi" => r#"
        val unused: i64
    "#,
    "/test/a.gx" => r#"
        use super::t::Show;
        impl Show for i64 { let show = |x| "int [x]" };
        let unused = 0
    "#
);

// A constructor-trait parameter in an interface is a quantifier, its
// element included: each call has its own.
run!(
    interface_ctor_trait_param_per_call,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(3)]),
    "/test.gx" => r#"
        mod m;
        let result = (m::csize(["a", "b"]), m::csize([<1, 2, 3>]))
    "#,
    "/test/m.gxi" => r#"
        val csize: fn(c: Collection) -> i64
    "#,
    "/test/m.gx" => r#"
        let csize = |c: Collection| Collection::fold(c, 0, |acc, x| acc + 1)
    "#
);

// A quantifier bounded by a constructor trait is a constructor applied,
// as a trait-typed parameter is, in either form and on either side of
// an interface.
const CTOR_QUANTIFIER: &str = r#"
{
  let csize = 'c: Collection |c: 'c| Collection::fold(c, 0, |acc, x| acc + 1);
  let same = 'c: Collection |c: 'c| -> 'c c;
  (csize(["a", "b"]), csize(same([<1, 2, 3>])))
}
"#;
run!(ctor_quantifier, CTOR_QUANTIFIER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(3)])
});

run!(
    ctor_quantifier_in_an_interface,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(3)]),
    "/test.gx" => r#"
        mod m;
        let result = (m::csize(["a", "b"]), m::csize([<1, 2, 3>]))
    "#,
    "/test/m.gxi" => r#"
        val csize: fn<'c: Collection>(c: 'c) -> i64
    "#,
    "/test/m.gx" => r#"
        let csize = 'c: Collection |c: 'c| Collection::fold(c, 0, |acc, x| acc + 1)
    "#
);

run!(
    ctor_quantifier_sig_over_a_param,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(3)]),
    "/test.gx" => r#"
        mod m;
        let result = (m::csize(["a", "b"]), m::csize([<1, 2, 3>]))
    "#,
    "/test/m.gxi" => r#"
        val csize: fn<'c: Collection>(c: 'c) -> i64
    "#,
    "/test/m.gx" => r#"
        let csize = |c: Collection| Collection::fold(c, 0, |acc, x| acc + 1)
    "#
);

run!(
    ctor_param_sig_over_a_quantifier,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(3)]),
    "/test.gx" => r#"
        mod m;
        let result = (m::csize(["a", "b"]), m::csize([<1, 2, 3>]))
    "#,
    "/test/m.gxi" => r#"
        val csize: fn(c: Collection) -> i64
    "#,
    "/test/m.gx" => r#"
        let csize = 'c: Collection |c: 'c| Collection::fold(c, 0, |acc, x| acc + 1)
    "#
);

run!(
    ctor_quantifier_wrong_bound_refused,
    refused("missing constraint"),
    "/test.gx" => r#"
        mod m;
        let result = (m::csize(["a", "b"]), m::csize([<1, 2, 3>]))
    "#,
    "/test/m.gxi" => r#"
        val csize: fn<'c: Collection>(c: 'c) -> i64
    "#,
    "/test/m.gx" => r#"
        let csize = 'c: Number |c: 'c| 0
    "#
    ; FuseExpect::None);

// An abstract type's implementation may live in the type's package.
run!(
    trait_abstract_impl_in_type_package,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(3)"),
    "/test.gx" => r#"
        mod t;
        mod u;
        let result = t::Show::show(u::make(3))
    "#,
    "/test/t.gxi" => r#"
        trait Show { val show: fn(self) -> string }
    "#,
    "/test/t.gx" => r#"
        let unused = 0
    "#,
    "/test/u.gxi" => r#"
        type Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/u.gx" => r#"
        use super::t::Show;
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    "#
);

// A quantifier bound written in a `let` annotation is enforced.
run!(
    annotation_bound_enforced,
    refused("within Number does not contain string"),
    "/test.gx" => r#"
        let f: fn<'a: Number>(x: 'a) -> 'a = |x| x;
        let result = f("hi")
    "#
    ; FuseExpect::None
);

// A polymorphic binding used as a value is instantiated per occurrence.
run!(
    poly_value_two_types,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "[1] [1.5]"),
    "/test.gx" => r#"
        let f = 'a: Number |x: 'a| x;
        let result = "[array::map([1], f)] [array::map([1.5], f)]"
    "#
    ; FuseExpect::Jit
);

// The same for a trait method passed as a value, then called on
// another type inside a bounded generic.
run!(
    trait_method_value_then_generic,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "int 3 Counter(4) [\"int 1\"]"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        impl Show for i64 { let show = |x| "int [x]" };
        use Show::show;
        let both = |a: Show, b: Show| "[show(a)] [show(b)]";
        let mapped = array::map([1], Show::show);
        let result = "[both(3, Counter(4))] [mapped]"
    "#
);

// The core traits Eq, Ord, Display: operators and printing consult an
// implementation wherever one sits in the static type.

// A case-insensitive key: `==` calls the implementation.
run!(
    core_eq_abstract,
    is(Value::Bool(true)),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        let result = Key("Foo") == Key("FOO") && Key("a") != Key("b")
    "##
);

// Values an implementation calls equal hash equal: a dedup keeps one.
run!(
    core_eq_hash_agrees,
    is(Value::I64(1)),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        let result = array::len(array::dedup([Key("Foo"), Key("FOO"), Key("foo")]))
    "##
);

// A type with no Eq implementation dedups by its payload, beside a type
// that has one and with none anywhere.
run!(
    core_eq_absent_dedups_by_payload,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(2)]),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        type T = Abstract<i64>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        let ts = array::len(array::dedup([T(1), T(2), T(1)]));
        let ks = array::len(array::dedup([Key("a"), Key("A"), Key("b")]));
        let result = (ts, ks)
    "##
);

run!(
    abstract_dedups_by_payload,
    is(Value::I64(2)),
    "/test.gx" => r##"
        type T = Abstract<i64>;
        let result = array::len(array::dedup([T(1), T(2), T(1)]))
    "##
);

// Inside a composite the walk calls it per element; the structural
// parts compare as values.
run!(
    core_eq_nested,
    is(Value::Bool(true)),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        let arrays = [Key("a"), Key("B")] == [Key("A"), Key("b")];
        let tuples = (Key("x"), 1) == (Key("X"), 1) && (Key("x"), 1) != (Key("X"), 2);
        let structs = {k: Key("q"), n: "n"} == {k: Key("Q"), n: "n"};
        let maps = {"a" => Key("v")} == {"a" => Key("V")};
        let variants = `Some(Key("z")) == `Some(Key("Z"));
        let result = arrays && tuples && structs && maps && variants
    "##
);

// A reversed order: `<` consults `Ord::cmp`, and so does the walk for
// a tuple holding the type.
run!(
    core_ord_abstract,
    is(Value::Bool(true)),
    "/test.gx" => r##"
        type Rev = Abstract<i64>;
        impl Ord for Rev {
            let cmp = |a, b| select (a.0, b.0) {
                (x, y) if x > y => `Less,
                (x, y) if x < y => `Greater,
                _ => `Equal
            }
        };
        let root = Rev(1) > Rev(2) && Rev(2) <= Rev(1) && Rev(3) >= Rev(3) && !(Rev(1) < Rev(2));
        let nested = (Rev(1), 5) > (Rev(2), 9) && (Rev(1), 5) < (Rev(1), 6);
        let result = root && nested
    "##
);

// Printing: interpolation, nested anywhere in the printed type —
// arrays, structs, tuples, variants, a union member, a recursive type.
run!(
    core_display_interp,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::String(s))
            if s == "#123|[#123, #456]|{c: #123, n: 5}|(#456, \"s\")|`Some(#123)|#123|7|`Cons(#123, `Cons(#456, `Nil))"
    ),
    "/test.gx" => r##"
        type Color = Abstract<{r: i64, g: i64, b: i64}>;
        impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };
        type L = [`Cons(Color, L), `Nil];
        let c1 = Color({r: 1, g: 2, b: 3});
        let c2 = Color({r: 4, g: 5, b: 6});
        let u1: [Color, i64] = c1;
        let u2: [Color, i64] = 7;
        let l: L = `Cons(c1, `Cons(c2, `Nil));
        let result = "[c1]|[[c1, c2]]|[{c: c1, n: 5}]|[(c2, "s")]|[`Some(c1)]|[u1]|[u2]|[l]"
    "##
);

// The core dispatchers are the operators: they work on every type,
// implementation or not, and the core traits hold as bounds for every
// type.
run!(
    core_dispatchers_structural,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "true|false|`Less|`Equal|`Greater|(1, 2)|#123"),
    "/test.gx" => r##"
        type Color = Abstract<{r: i64, g: i64, b: i64}>;
        impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };
        let same = 'a: Eq |x: 'a, y: 'a| Eq::eq(x, y);
        let show = 'a: Display |x: 'a| Display::fmt(x);
        let result = "[same(1, 1)]|[same("a", "b")]|[Ord::cmp(1, 2)]|[Ord::cmp((1, 2), (1, 2))]|[Ord::cmp("b", "a")]|[show((1, 2))]|[show(Color({r: 1, g: 2, b: 3}))]"
    "##
    ; FuseExpect::Jit
);

// A core trait method runs inside the comparison, so it is implicitly
// `#[sync]`: an async body is a compile error.
run!(
    core_method_async_refused,
    refused("this function is async"),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Display for Key { let fmt = |k| sys::time::timer(duration:0.01s, false) ~ k.0 };
        let result = "[Key("a")]"
    "##
    ; FuseExpect::None; build_only
);

// `println`/`dbg`/`log` print through the implementation too, on
// both engines (the builtin formats through the hook from inside a
// fused kernel's DynCall as well). A block's statements are unordered,
// so are its lines.
async fn core_display_println(mode: Mode) -> Result<()> {
    let code = r##"{
        type Color = Abstract<{r: i64, g: i64, b: i64}>;
        impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };
        let c = Color({r: 1, g: 2, b: 3});
        println(c);
        println([c, c]);
        println("[c]");
        dbg(#dest: `Stdout, {c, n: 1});
        42
    }"##;
    let (values, out) =
        super::dense_deltas::run_delta_fused(code, mode, FuseExpect::Jit).await?;
    assert_eq!(graphix_package_core::testing::as_i64s(&values), vec![42]);
    let (dbg, mut lines): (Vec<&str>, Vec<&str>) =
        out.lines().partition(|l| l.ends_with("): {c: #123, n: 1}"));
    lines.sort();
    assert_eq!(dbg.len(), 1, "{out}");
    assert_eq!(lines, ["#123", "#123", "[#123, #123]"], "{out}");
    Ok(())
}

modes!(core_display_println);

// A trait call on a union self type inside a lambda: the lowered select
// binds the call's argument nodes rather than recompiling their source.
run!(
    trait_union_dispatch_in_lambda,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "A1 Bx"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        type A = Abstract<i64>;
        type B = Abstract<string>;
        impl Show for A { let show = |a| "A[a.0]" };
        impl Show for B { let show = |b| "B[b.0]" };
        let f = |x: [A, B]| Show::show(x);
        let result = "[f(A(1))] [f(B("x"))]"
    "#
    ; FuseExpect::Jit
);

// Under `Any` (or an open cell) the runtime tag is the type id: an
// abstract value finds its type's implementation by id, and a site is
// built for each tag on first use; anything else is structural.
run!(
    core_dynamic_under_any,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "true|false|false|[key:Foo, 5]|key:Foo|true"),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        impl Display for Key { let fmt = |k| "key:[k.0]" };
        let a: Any = cast<Any>(Key("Foo"));
        let b: Any = cast<Any>(Key("FOO"));
        let five: Any = cast<Any>(5);
        let xs: Array<Any> = [a, five];
        let result = "[a == b]|[a != b]|[a == five]|[xs]|[a]|[xs == [b, five]]"
    "##
    ; FuseExpect::None
);

// `Value`'s own eq/cmp reach a core implementation, so a map is keyed
// by the user's Ord: a reversed order reverses the key order.
run!(
    core_map_keyed_by_ord,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "{T(2) => 2, T(1) => 1, T(0) => 0}|0|2"),
    "/test.gx" => r##"
        type T = Abstract<i64>;
        impl Ord for T {
            let cmp = |x, y| select (x.0, y.0) {
                (a, b) if a < b => `Greater,
                (a, b) if a > b => `Less,
                _ => `Equal
            }
        };
        let m = {T(0) => 0, T(1) => 1, T(2) => 2};
        let result = "[m]|[m{T(0)}$]|[m{T(2)}$]"
    "##
    ; FuseExpect::Jit
);

// A map HOF builds its result under the user's Ord too.
run!(
    core_map_hof_keyed_by_ord,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "{T(2) => 2, T(1) => 1, T(0) => 0}|0|{T(2) => 3, T(1) => 2, T(0) => 1}|{T(2) => 2}"),
    "/test.gx" => r##"
        type T = Abstract<i64>;
        impl Ord for T {
            let cmp = |x, y| select (x.0, y.0) {
                (a, b) if a < b => `Greater,
                (a, b) if a > b => `Less,
                _ => `Equal
            }
        };
        let m = {T(0) => 0, T(1) => 1, T(2) => 2};
        let filtered = map::filter(m, |_| true);
        let mapped = map::map(m, |(k, v)| (k, v + 1));
        let fm = map::filter_map(m, |(k, v)| select v { 2 => (k, v), _ => null });
        let result = "[filtered]|[filtered{T(0)}$]|[mapped]|[fm]"
    "##
);

// An Ord that calls distinct payloads equal UNIFIES map keys: the
// second insert replaces the first, and either spelling looks it up.
run!(
    core_map_key_unified,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "1|2|2"),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Ord for Key {
            let cmp = |a, b| select (str::to_lower(a.0), str::to_lower(b.0)) {
                (x, y) if x < y => `Less,
                (x, y) if x > y => `Greater,
                _ => `Equal
            }
        };
        let m = {Key("a") => 1, Key("b") => 9};
        let m2 = map::insert(m, Key("A"), 2);
        let result = "[map::len(m2) - 1]|[m2{Key("a")}$]|[m2{Key("A")}$]"
    "##
);

// The comparing builtins go through the same seam: sort, min, max.
run!(
    core_sort_min_max_by_ord,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "[T(3), T(2), T(1)]|T(3)|T(1)"),
    "/test.gx" => r##"
        type T = Abstract<i64>;
        impl Ord for T {
            let cmp = |x, y| select (x.0, y.0) {
                (a, b) if a < b => `Greater,
                (a, b) if a > b => `Less,
                _ => `Equal
            }
        };
        let xs = array::sort(#dir: `Ascending, [T(2), T(3), T(1)]);
        let result = "[xs]|[min(T(2), T(3), T(1))]|[max(T(2), T(3), T(1))]"
    "##
    ; FuseExpect::Jit
);

// A bottoming Ord resolves per key like NaN: a bottom key sorts below
// every real key and equal to its fellow bottom keys.
run!(
    core_bottom_key_rule,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "[T(0), T(1), T(2)]|false|true|true"),
    "/test.gx" => r##"
        type T = Abstract<i64>;
        impl Ord for T {
            let cmp = |x, y| select ((i64:1 /? x.0)$, (i64:1 /? y.0)$) {
                (a, b) if a < b => `Greater,
                (a, b) if a > b => `Less,
                _ => `Equal
            }
        };
        impl Eq for T { let eq = |x, y| (i64:1 /? x.0)$ == (i64:1 /? y.0)$ };
        let xs = array::sort(#dir: `Ascending, [T(2), T(0), T(1)]);
        let result = "[xs]|[T(0) == T(1)]|[T(0) == T(0)]|[T(0) < T(1)]"
    "##
    ; FuseExpect::Jit
);

// An implementation method may be a builtin reference; the trait's
// signature, instantiated at the target, annotates the lambda.
run!(
    trait_builtin_bodied_impl,
    is(Value::I64(2)),
    "/test.gx" => r#"
        trait Masked { val masked: fn(self, m: i64) -> i64 };
        impl Masked for i64 { let masked = |x, m| 'core_bit_and };
        let result = Masked::masked(6, 3)
    "#
    ; FuseExpect::Jit
);

// The same, through a bound rather than the dispatcher — the builtin
// body is instantiated per resolved implementation.
run!(
    trait_builtin_bodied_bound,
    is(Value::I64(4)),
    "/test.gx" => r#"
        trait Masked { val masked: fn(self, m: i64) -> i64 };
        impl Masked for i64 { let masked = |x, m| 'core_bit_and };
        let f = 'a: Masked |v: 'a| Masked::masked(v, 12);
        let result = f(6) + f(1)
    "#
    ; FuseExpect::Jit
);

// An interface's `impl` declaration does not displace the type
// declarations that follow it.
run!(
    interface_type_after_impl,
    is(Value::I64(1)),
    "/test.gx" => r#"
        mod m;
        let result = m::f(`A)
    "#,
    "/test/m.gxi" => r#"
        trait T { val g: fn(self) -> i64 };
        type X;
        impl T for X;
        type Y = [`A, `B];
        val f: fn(y: Y) -> i64
    "#,
    "/test/m.gx" => r#"
        type X = Abstract<i64>;
        impl T for X { let g = |x| x.0 };
        let f = |y: Y| select y { `A => 1, `B => 2 }
    "#
);

// A core-trait implementation for a Rust-backed abstract type is
// refused (no payload to consult).
run!(
    core_impl_rust_backed_refused,
    |v: Result<&Value>| {
        matches!(&v, Err(e) if format!("{e:#}").contains("backed by Rust"))
    },
    "/test.gx" => r#"
        impl Display for sys::fs::tempdir::T { let fmt = |_| "a tempdir" };
        let result = 1
    "#
    ; FuseExpect::None
);

// A stream written in Graphix: `Mem` supplies `read` and the trait's
// default `read_all` works over it.
run!(
    graphix_defined_read,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello world"),
    "/test.gx" => r#"
        use sys::io::Read;
        type Mem = Abstract<&mut bytes>;
        impl Read for Mem {
            let read = |s, n| {
                let cell = s.0;
                let b = *cell;
                let take = select buffer::len(b) < n {
                    true => buffer::len(b),
                    false => n
                };
                *cell <- take ~ b[take..]$;
                b[..take]$
            }
        };
        let src = buffer::from_string("hello world");
        let m = Mem(&mut src);
        let result = buffer::to_string(Read::read_all(m)?)?
    "#
);

// The other derived method, on the same Graphix-defined stream:
// `read_exact` loops over `read` until it has n bytes.
// CR claude for claude: [test-gap/low] This pin and graphix_defined_read above it use a
// `Mem` reader that is a self-loop. `*cell <- take ~ b[take..]$` fires its own reader,
// so the stream gives out a byte every cycle whether or not `read` was called, then
// spins on the empty buffer. A read_exact that never re-issues `read` (its `want <-`
// deleted) still yields "hello" first here, so the pin cannot catch a broken re-arm.
// Sample the request instead: `let take = (s, n) ~ select ..; *cell <- take ~ ..; take
// ~ b[..take]$`. Then add a pin that calls read_exact several times on one stream.
// probe: design/review-2026-10-19/repro/gx-stdlib-read-fixture-selfloop.gx
// (gx-stdlib-06)
run!(
    graphix_defined_read_exact,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello"),
    "/test.gx" => r#"
        use sys::io::Read;
        type Mem = Abstract<&mut bytes>;
        // reads one byte at a time, so read_exact has to loop
        impl Read for Mem {
            let read = |s, n| {
                let cell = s.0;
                let b = *cell;
                let take = select buffer::len(b) == u64:0 {
                    true => u64:0,
                    false => u64:1
                };
                *cell <- take ~ b[take..]$;
                b[..take]$
            }
        };
        let src = buffer::from_string("hello world");
        let m = Mem(&mut src);
        let result = buffer::to_string(Read::read_exact(m, u64:5)?)?
    "#
);

// Dynamic modules: a signature declares an impl as a gxi does; the
// consumer's calls reach the loaded implementation, also after reload.

const DYNAMIC_IMPL_DISPATCH: &str = r#"
{
    let source = """
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    """;
    sys::net::publish("/local/dimpl0", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            trait Show { val show: fn(self) -> string };
            type Counter;
            impl Show for Counter;
            val make: fn(x: i64) -> Counter
        };
        source sys::net::subscribe("/local/dimpl0")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::Show::show(foo::make(9))
    }
}
"#;

run!(
    trait_dynamic_impl_dispatch,
    DYNAMIC_IMPL_DISPATCH,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(9)"); FuseExpect::None
);

// The trait's default method, reached through the dynamic module.
const DYNAMIC_IMPL_DEFAULT: &str = r#"
{
    let source = """
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    """;
    sys::net::publish("/local/dimpl1", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            trait Show {
                val show: fn(self) -> string;
                val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
            };
            type Counter;
            impl Show for Counter;
            val make: fn(x: i64) -> Counter
        };
        source sys::net::subscribe("/local/dimpl1")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::Show::twice(foo::make(2))
    }
}
"#;

run!(
    trait_dynamic_impl_default,
    DYNAMIC_IMPL_DEFAULT,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(2) Counter(2)"); FuseExpect::None
);

// The loaded source dispatches on its own implementation internally.
const DYNAMIC_IMPL_INTERNAL: &str = r#"
{
    let source = """
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let describe = |x| Show::show(Counter(x))
    """;
    sys::net::publish("/local/dimpl2", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            trait Show { val show: fn(self) -> string };
            type Counter;
            impl Show for Counter;
            val describe: fn(x: i64) -> string
        };
        source sys::net::subscribe("/local/dimpl2")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::describe(4)
    }
}
"#;

run!(
    trait_dynamic_impl_internal,
    DYNAMIC_IMPL_INTERNAL,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(4)"); FuseExpect::None
);

// A reload replaces the implementation; the consumer's call follows.
const DYNAMIC_IMPL_RELOAD: &str = r#"
{
    let one = """
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "one" };
        let make = |x| Counter(x)
    """;
    let two = """
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "two" };
        let make = |x| Counter(x)
    """;
    let source = one;
    sys::net::publish("/local/dimpl3", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            trait Show { val show: fn(self) -> string };
            type Counter;
            impl Show for Counter;
            val make: fn(x: i64) -> Counter
        };
        source sys::net::subscribe("/local/dimpl3")?
    };
    let shown = select status {
        error as e => never(dbg(e)),
        null as _ => foo::Show::show(foo::make(1))
    };
    source <- select shown { "one" => two, _ => never() };
    select shown { "two" => shown, _ => never() }
}
"#;

run!(
    trait_dynamic_impl_reload,
    DYNAMIC_IMPL_RELOAD,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "two")
);

// The trait lives OUTSIDE the dynamic module; its signature imports
// it, and the consumer dispatches through the outer trait's own name.
run!(
    trait_dynamic_impl_outer_trait,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(7)"),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        let source = """
            type Counter = Abstract<i64>;
            impl Show for Counter { let show = |c| "Counter([c.0])" };
            let make = |x| Counter(x)
        """;
        sys::net::publish("/local/dimpl4", source)?;
        let status = mod foo dynamic {
            sandbox whitelist [core];
            sig {
                use super::Show;
                type Counter;
                impl Show for Counter;
                val make: fn(x: i64) -> Counter
            };
            source sys::net::subscribe("/local/dimpl4")?
        };
        let result = select status {
            error as e => never(dbg(e)),
            null as _ => Show::show(foo::make(7))
        }
    "#; FuseExpect::None
);

// An outer trait's DEFAULT method through a dynamic module: the
// default's binding produced at program start, long before the load,
// so the declared method binding is fed from its standing value.
run!(
    trait_dynamic_impl_outer_default,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(2) Counter(2)"),
    "/test.gx" => r#"
        trait Show {
            val show: fn(self) -> string;
            val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
        };
        let source = """
            type Counter = Abstract<i64>;
            impl Show for Counter { let show = |c| "Counter([c.0])" };
            let make = |x| Counter(x)
        """;
        sys::net::publish("/local/dimpl5", source)?;
        let status = mod foo dynamic {
            sandbox whitelist [core];
            sig {
                use super::Show;
                type Counter;
                impl Show for Counter;
                val make: fn(x: i64) -> Counter
            };
            source sys::net::subscribe("/local/dimpl5")?
        };
        let result = select status {
            error as e => never(dbg(e)),
            null as _ => Show::twice(foo::make(2))
        }
    "#; FuseExpect::None
);

// The static twin: the declared impl's `twice` is the outer trait's
// default, shared with the root's own i64 impl in the same cycle.
run!(
    trait_interface_outer_default,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if s == "Counter(2) Counter(2) / int 3 int 3")
    },
    "/test.gx" => r#"
        trait Show {
            val show: fn(self) -> string;
            val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
        };
        impl Show for i64 { let show = |x| "int [x]" };
        mod m;
        let result = "[Show::twice(m::make(2))] / [Show::twice(3)]"
    "#,
    "/test/m.gxi" => r#"
        use super::Show;
        type Counter;
        impl Show for Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/m.gx" => r#"
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "Counter([c.0])" };
        let make = |x| Counter(x)
    "#
);

// A loaded source is `typecheck1`'d like a file: a module-level call
// with a labeled default gets its default materialized.
const DYNAMIC_MODULE_TC1: &str = r#"
{
    let source = """
        let f = |#k = 10, x| x + k;
        let g = f(2)
    """;
    sys::net::publish("/local/dtc1", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val g: i64 };
        source sys::net::subscribe("/local/dtc1")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::g
    }
}
"#;

run!(dynamic_module_typecheck1, DYNAMIC_MODULE_TC1, is(Value::I64(12)));

// A CORE trait declared by a dynamic module's signature: the value
// seam's hook site resolves to the declared method binding, which the
// loaded implementation feeds.
const DYNAMIC_CORE_IMPL: &str = r#"
{
    let source = """
        type Counter = Abstract<i64>;
        impl Display for Counter { let fmt = |c| "C<[c.0]>" };
        let make = |x| Counter(x)
    """;
    sys::net::publish("/local/dcore", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            type Counter;
            impl Display for Counter;
            val make: fn(x: i64) -> Counter
        };
        source sys::net::subscribe("/local/dcore")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => "[foo::make(3)]"
    }
}
"#;

run!(
    trait_dynamic_core_impl,
    DYNAMIC_CORE_IMPL,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "C<3>"); FuseExpect::None
);

// A parameterized DECLARED impl: consumers resolve to the declared
// method bindings, which must instantiate afresh per call like the
// implementation's own.
run!(
    trait_interface_declared_parameterized,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "i1,i2 btrue"),
    "/test.gx" => r#"
        mod m;
        let a = m::Show::show([1, 2]);
        let b = m::Show::show([true]);
        let result = "[a] [b]"
    "#,
    "/test/m.gxi" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64;
        impl Show for bool;
        impl<'a: Show> Show for Array<'a>;
    "#,
    "/test/m.gx" => r#"
        impl Show for i64 { let show = |x| "i[x]" };
        impl Show for bool { let show = |b| "b[b]" };
        impl<'a: Show> Show for Array<'a> {
            let show = |xs| str::join(#sep: ",", array::map(xs, Show::show))
        }
    "#
);

// A CORE trait declared by an interface for its own hidden abstract
// type: whether the type is Graphix-minted or Rust-backed is the
// implementation's to say, so the declaration is not refused.
run!(
    core_impl_interface_declared,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "C<3>"),
    "/test.gx" => r#"
        mod m;
        let result = "[m::make(3)]"
    "#,
    "/test/m.gxi" => r#"
        type Counter;
        impl Display for Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/m.gx" => r#"
        type Counter = Abstract<i64>;
        impl Display for Counter { let fmt = |c| "C<[c.0]>" };
        let make = |x| Counter(x)
    "#
    ; FuseExpect::Jit
);

// ...and the implementation IS refused when the type turns out to be
// Rust-backed.
run!(
    core_impl_interface_rust_backed_refused,
    |v: Result<&Value>| {
        matches!(&v, Err(e) if format!("{e:#}").contains("backed by Rust"))
    },
    "/test.gx" => r#"
        mod m;
        let result = 1
    "#,
    "/test/m.gxi" => r#"
        type Counter;
        impl Display for Counter;
    "#,
    "/test/m.gx" => r#"
        type Counter;
        impl Display for Counter { let fmt = |c| "never" }
    "#
    ; FuseExpect::None
);

// Two implementations of one declared impl in the same module.
run!(
    trait_interface_impl_twice,
    |v: Result<&Value>| matches!(&v, Err(e) if format!("{e:#}").contains("implemented twice")),
    "/test.gx" => r#"
        mod m;
        let result = m::Show::show(m::make(1))
    "#,
    "/test/m.gxi" => r#"
        trait Show { val show: fn(self) -> string };
        type Counter;
        impl Show for Counter;
        val make: fn(x: i64) -> Counter
    "#,
    "/test/m.gx" => r#"
        type Counter = Abstract<i64>;
        impl Show for Counter { let show = |c| "one" };
        impl Show for Counter { let show = |c| "two" };
        let make = |x| Counter(x)
    "#
    ; FuseExpect::None
);

// A constructor-trait call's filled result type is what select
// coverage, `cast` and the typed printer see.
const TRAIT_RESULT_IS_FILLED: &str = r#"
{
  use core::Collection::{self, *};
  let c = map([i64:1, i64:2, i64:5], |x| x * x);
  let n = select c { [] => i64:0, [h, rest..] => h + array::len(rest) };
  let t = cast<Array<i64>>(c)$;
  n + t[1]$
}
"#;

run!(trait_result_is_filled, TRAIT_RESULT_IS_FILLED, is(Value::I64(7)); FuseExpect::Jit);

// A generic core implementation (`impl<'a> Eq for Box<'a>`) is the one
// the value's operator consults.
run!(
    core_eq_generic_impl,
    r#"
{
  type Box<'a> = Abstract<'a>;
  impl<'a> Eq for Box<'a> { let eq = |a, b| true };
  Box(1) == Box(2)
}
"#,
    is(Value::Bool(true));
    FuseExpect::Jit
);

// A map ordered by a core `Ord` impl on its key type is searched by
// that order through a place reference too.
run!(
    core_ord_map_place,
    r#"
{
  type Rev = Abstract<i64>;
  impl Ord for Rev {
    let cmp = |a, b| select (a.0, b.0) {
      (x, y) if x > y => `Less,
      (x, y) if x < y => `Greater,
      _ => `Equal
    }
  };
  let m = {Rev(1) => 10, Rev(2) => 20, Rev(3) => 30};
  *(&m{Rev(1)})
}
"#,
    is(Value::I64(10));
    FuseExpect::Jit
);

// A type variable in an impl head must be declared on the impl.
run!(
    impl_undeclared_tvar,
    r#"
{
  trait Mark { val mark: fn(self) -> i64 };
  impl Mark for Array<'a> { let mark = |x| 1 };
  Mark::mark([1])
}
"#,
    |v: Result<&Value>| matches!(&v, Err(e) if format!("{e:#}").contains("undeclared type variable"));
    FuseExpect::None
);

// A core implementation for one instantiation is not consulted for
// another: `Box<string>` falls back to structural equality.
run!(
    core_eq_other_instantiation_is_structural,
    r#"
{
  type Box<'a> = Abstract<'a>;
  impl Eq for Box<i64> { let eq = |a, b| true };
  (Box(1) == Box(2), Box("a") == Box("b"))
}
"#,
    prints("[true, false]");
    FuseExpect::Jit
);

// A phantom parameter still selects the implementation: the value
// carries the type it was constructed at, not just its payload's.
run!(
    core_eq_phantom_parameter,
    r#"
{
  type Marker<'a> = Abstract<i64>;
  impl Eq for Marker<i64> { let eq = |a, b| true };
  impl Eq for Marker<string> { let eq = |a, b| false };
  let a: Marker<string> = Marker(1);
  let b: Marker<string> = Marker(1);
  let c: Marker<i64> = Marker(1);
  let d: Marker<i64> = Marker(2);
  (a == b, c == d)
}
"#,
    prints("[false, true]");
    FuseExpect::Jit
);

// An implementation applies to the whole instantiation, not to one
// that overlaps it; a repeated variable binds once; a bound is checked.
run!(
    core_eq_impl_must_apply,
    r#"
{
  type Marker<'a> = Abstract<i64>;
  type Pair<'a, 'b> = Abstract<('a, 'b)>;
  type Bounded<'a> = Abstract<i64>;
  trait Mark { val mark: fn(self) -> bool };
  impl Mark for i64 { let mark = |x| true };
  impl Eq for Marker<i64> { let eq = |a, b| true };
  impl<'a> Eq for Pair<'a, 'a> { let eq = |a, b| true };
  impl<'a: Mark> Eq for Bounded<'a> { let eq = |a, b| true };
  let u: Marker<[i64, string]> = Marker(1);
  let v: Marker<[i64, string]> = Marker(2);
  let s: Bounded<string> = Bounded(1);
  let t: Bounded<string> = Bounded(2);
  let n: Bounded<i64> = Bounded(1);
  let m: Bounded<i64> = Bounded(2);
  (u == v, Pair((1, "a")) == Pair((2, "b")), Pair((1, 2)) == Pair((3, 4)), s == t, n == m)
}
"#,
    prints("[false, false, true, false, true]");
    FuseExpect::Jit
);

// A nominal type test is on the instantiation: `Box<i64> as b` does
// not take a `Box<string>`.
run!(
    abstract_test_matches_parameters,
    r#"
{
  type Box<'a> = Abstract<'a>;
  let x: [Box<i64>, Box<string>] = Box("hi");
  select x {
    Box<i64> as b => `Number(b.0),
    Box<string> as b => `Text(b.0)
  }
}
"#,
    prints(r#"["Text", "hi"]"#);
    FuseExpect::Jit
);

// A binary implementation is for one instantiation: a pair constructed
// at two compares structurally, in both directions.
run!(
    core_eq_mixed_instantiations_are_structural,
    r#"
{
  type Box<'a> = Abstract<'a>;
  impl Eq for Box<i64> { let eq = |a, b| true };
  let a: [Box<i64>, Box<string>] = Box(1);
  let b: [Box<i64>, Box<string>] = Box("hi");
  let c: [Box<i64>, Box<string>] = Box(2);
  (a == b, b == a, a == c)
}
"#,
    prints("[false, false, true]");
    FuseExpect::Jit
);

// A nested dispatch of the same trait (a Display impl printing its
// payload, itself of the type) finds the registry and its own site.
run!(
    core_display_nested_same_tag,
    r#"
{
  type Box<'a> = Abstract<'a>;
  impl<'a> Display for Box<'a> { let fmt = |a| "<[a.0]>" };
  let x = Box(Box(Box(1)));
  let n = count(sys::time::timer(duration:0.001s, true));
  select n {
    3 => "[x]",
    _ => never()
  }
}
"#,
    prints(r#""<<<1>>>""#);
    FuseExpect::Jit
);

// A labeled parameter before `self` does not shift which argument a
// union dispatch reads as `self`.
run!(
    trait_union_labeled_before_self,
    r#"
{
  type A = Abstract<i64>;
  type B = Abstract<string>;
  trait Show { val show: fn(#pre: string, self, n: i64) -> string };
  impl Show for A { let show = |#pre: string, s: A, n: i64| "[pre] A [n]" };
  impl Show for B { let show = |#pre: string, s: B, n: i64| "[pre] B [n]" };
  let x: [A, B] = A(1);
  Show::show(#pre: "p", x, 3)
}
"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "p A 3");
    FuseExpect::Jit
);

// A bound naming another declared variable reads the fresh one: the
// implementation applies to `Pair<string, [string, null]>`.
run!(
    core_eq_bound_names_other_var,
    r#"
{
  type Pair<'a, 'b> = Abstract<('a, 'b)>;
  impl<'a, 'b: ['a, null]> Eq for Pair<'a, 'b> { let eq = |x, y| true };
  let q1: Pair<string, [string, null]> = Pair(("a", "x"));
  let q2: Pair<string, [string, null]> = Pair(("b", "y"));
  q1 == q2
}
"#,
    is(Value::Bool(true));
    FuseExpect::Jit
);

/// A missing impl is named as one.
const TRAIT_MISSING_IMPL_NAMED: &str = r#"
{
    trait Show { val show: fn(self) -> string };
    Show::show(2)
}
"#;

run!(trait_missing_impl_named, TRAIT_MISSING_IMPL_NAMED, refused("i64 does not implement Show"); FuseExpect::None);

// A trait method named as a value is a function: an alias, a struct
// field, a callback and an impl bound to it all dispatch.
run!(
    trait_method_as_a_value,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "desc2 desc3 [\"desc4\"] desc5"),
    "/test.gx" => r##"
        trait Desc { val desc: fn(self) -> string };
        trait Show { val show: fn(self) -> string };
        type A = Abstract<i64>;
        impl Desc for A { let desc = |a| "desc[a.0]" };
        impl Show for A { let show = Desc::desc };
        let d = Desc::desc;
        let ops = {f: Desc::desc};
        let result = "[d(A(2))] [(ops.f)(A(3))] [array::map([A(4)], d)] [Show::show(A(5))]"
    "##
);

// A comparison runs its core-trait impl, which reads what the summary
// of the comparison cannot see: forked and serial evaluation agree.
run!(
    core_impl_reads_follow_the_plan,
    r#"{
    type Key = Abstract<i64>;
    let n = array::iter([0, 1, 2, 3]);
    let k = n * 10;
    let same = Key(n) == Key(n + n * 10);
    impl Eq for Key { let eq = |a, b| (a.0 + k) == b.0 };
    array::group(same, |i, _| i == 4)
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => a.len() == 4 && a.iter().all(|v| *v == Value::Bool(true)),
        _ => false,
    }
);

// A trait as a method parameter's type is written as in any signature.
run!(method_parameter_of_trait_type, r#"{
    type K = Abstract<i64>;
    type J = Abstract<string>;
    impl Display for J { let fmt = |j| "J([j.0])" };
    trait Sh { val sh: fn(self, c: Display) -> string };
    impl Sh for K { let sh = |k, c| "[k.0]:[c]" };
    Sh::sh(K(1), J("a"))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1:J(a)"); FuseExpect::Jit);

// A union of primitives as a self dispatches per primitive.
run!(primitive_union_self, r#"{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |a| "i[a]" };
    impl Show for string { let show = |b| "s[b]" };
    let one: [i64, string] = "q";
    Show::show(one)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "sq"); FuseExpect::Jit);

// A bare sibling method in a default body dispatches, whatever the order.
run!(default_calls_its_sibling_through_dispatch, r#"{
    trait Show {
        val show: fn(self) -> string;
        val before: fn(self) -> string = |s| "[twice(s)]+[show(s)]";
        val twice: fn(self) -> string = |s| "[show(s)]+[show(s)]";
        val after: fn(self) -> string = |s| "[twice(s)]+[show(s)]"
    };
    type B = Abstract<string>;
    impl Show for B {
        let show = |b| "B[b.0]";
        let twice = |b| "BB[b.0]"
    };
    "[Show::before(B("y"))] [Show::after(B("y"))]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "BBy+By BBy+By"); FuseExpect::Jit);

// An implementation is the declared signature: a union of primitives is no
// target, an impl in a function body would register per call, and a method
// may not need a bound the trait does not declare.
#[tokio::test(flavor = "current_thread")]
async fn implementations_are_declared_ones() {
    use graphix_package_core::testing::refusal;
    for (src, why) in [
        (
            "{ trait Show { val show: fn(self) -> string }; impl Show for [i64, string] { let show = |p| \"u\" }; 0 }",
            "a union is never an implementation target",
        ),
        (
            "{ trait Show { val show: fn(self) -> string }; type T = Abstract<i64>; let f = |x: i64| -> string { impl Show for T { let show = |t| \"t\" }; \"[x]\" }; f(1) }",
            "is in a function body",
        ),
        // an interface's impl too: the body binds the signature per compile
        (
            "{ let f = |x: i64| mod foo dynamic { sandbox whitelist [core]; sig { trait Show { val show: fn(self) -> string }; type Counter; impl Show for Counter }; source \"\" }; f(0) }",
            "is in a function body",
        ),
        (
            "{ type Counter = Abstract<i64>; trait Twice { val twice: fn(self, x: 'a) -> 'a }; impl Twice for Counter { let twice = |c, x| x + x }; Twice::twice(Counter(1), 2) }",
            "which the signature does not declare",
        ),
        // the orphan rule: a non-abstract type is the trait's package's
        (
            "{ impl Display for i64 { let fmt = |x| \"boo\" }; \"[1]\" }",
            "only the trait's own package (/core) may implement it",
        ),
        (
            "{ type P = {x: i64}; impl Eq for P { let eq = |a, b| true }; 0 }",
            "only the trait's own package (/core) may implement it",
        ),
        // an abstract type's, the trait's or the type's package's
        (
            "{ impl sys::io::Close for sys::process::Proc { let close = |p| Ok(null) }; 0 }",
            "an abstract type's implementation must live in the type's package",
        ),
        (
            "{ trait T { val t: fn(self) -> i64 }; impl<'a> T for 'a { let t = |x| 1 }; 0 }",
            "a bare type variable is not an implementation target",
        ),
        (
            "{ trait T { val t: fn(self) -> i64 }; impl T for Any { let t = |x| 1 }; 0 }",
            "impl T for Any: not an implementation target",
        ),
    ] {
        let e = refusal(src, crate::TEST_REGISTER).await.unwrap();
        assert!(e.contains(why), "{src}: {e}");
    }
}

// A trait call whose self argument is never produced dispatches nothing:
// it is bottom, its other arguments still live, and the program builds.
// CR claude for claude: [test-gap/medium] trait_call_over_bottom_self cannot fail. run!
// asserts the first update, and `seen <- never_sized ~ true` lands a cycle later, so
// the first value is (2, false) even when never_sized produces: with `csize(["x"])` in
// place of `csize(never())` the program prints (2, false) then (2, true), and this
// predicate still passes. Read the result after the write could land (gate it on
// `count(sys::time::timer(..)) == 3`, as core_display_nested_same_tag does) or collect
// every update with run_delta and assert [(2, false)]. probe:
// design/review-2026-10-19/repro/tests-fusion-traits-bottom-self-pin-vacuous.gx
// (tests-fusion-traits-06)
run!(
    trait_call_over_bottom_self,
    r#"{
    let csize = 'c: Collection |c: 'c| Collection::fold(c, 0, |acc, x| acc + 1);
    let never_sized = csize(never());
    let sized = csize(["a", "b"]);
    let seen = false;
    seen <- never_sized ~ true;
    (sized, seen)
}"#,
    prints("[i64:2, false]")
);

// The self argument written later dispatches when it arrives.
run!(
    trait_call_over_late_self,
    r#"{
    let csize = 'c: Collection |c: 'c| Collection::fold(c, 0, |acc, x| acc + 1);
    let x = never();
    let s = csize(x);
    x <- ["a", "b"];
    s
}"#,
    is(Value::I64(2))
);

// A trait method with an optional label is called, never used as a value:
// its default is each impl's, which a value of the method cannot give.
run!(
    optional_labeled_method_value_refused,
    r#"{
    trait Show { val show: fn(?#n: i64, self) -> string };
    impl Show for i64 { let show = |#n = 1, i| "i[i * n]" };
    array::map([1, 2], Show::show)
}"#,
    refused("Show::show takes the optional label #n");
    FuseExpect::None
);

// A trait method named as a value is its eta-expansion: an ordinary
// function, passed to a collection, forwarded through a parameter,
// stored, and polymorphic.
run!(
    trait_method_value_flows,
    r#"{
    trait Show { val show: fn(self) -> string };
    type A = Abstract<i64>;
    type B = Abstract<string>;
    impl Show for A { let show = |a| "A[a.0]" };
    impl Show for B { let show = |b| "B[b.0]" };
    impl Show for i64 { let show = |i| "i[i]" };
    let ys: Array<[A, B]> = [A(1), B("x")];
    let app = |f: fn(x: i64) -> string, xs: Array<i64>| array::map(xs, |x| f(x));
    let held = { f: Show::show };
    let g = Show::show;
    [
        array::map([1, 2], Display::fmt),
        array::map(ys, Show::show),
        app(Display::fmt, [1, 2]),
        [(held.f)(A(3)), g(4), g(B("y"))]
    ]
}"#,
    prints(r#"[["1", "2"], ["A1", "Bx"], ["1", "2"], ["A3", "i4", "By"]]"#)
);

#[tokio::test(flavor = "current_thread")]
async fn trait_method_variadic_value_refused() {
    use graphix_package_core::testing::refusal;
    let src = "{ trait V { val v: fn(self, @args: i64) -> i64 }; impl V for i64 { let v = |a: i64, @args: i64| -> i64 'core_max }; let f = V::v; f(1) }";
    let e = refusal(src, crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("can be called, not used as a value"), "{e}");
}

// A bound a definition's body gives its variable resolves once, at the
// check: an instance built at run time, outside the block that declared
// the trait, checks it there.
run!(
    trait_bound_resolved_at_the_check,
    r#"{
    trait Desc { val desc: fn(self) -> string };
    type A = Abstract<i64>;
    impl Desc for A { let desc = |a| "desc[a.0]" };
    let g = |x| Desc::desc(x);
    array::map([A(4), A(5)], g)
}"#,
    prints(r#"["desc4", "desc5"]"#)
);

// A generic impl whose bound leads back to a recursive typedef: the trait
// question met again inside the bound is the one in progress.
run!(
    trait_over_recursive_typedef,
    r#"{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "i" };
    impl Show for string { let show = |x| x };
    impl<'a: Show> Show for Array<'a> { let show = |a| "arr" };
    type Json = [i64, string, Array<Json>];
    let j: Json = [1, "a"];
    Show::show(j)
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "arr")
);

run!(
    trait_over_self_array,
    r#"{
    trait Show { val show: fn(self) -> string };
    impl<'a: Show> Show for Array<'a> { let show = |a| "arr" };
    type L = Array<L>;
    let l: L = [];
    Show::show(l)
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "arr")
);

run!(trait_over_recursive_typedef_missing_impl, r#"{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "i" };
    impl<'a: Show> Show for Array<'a> { let show = |a| "arr" };
    type Json = [i64, bool, Array<Json>];
    let j: Json = 1;
    Show::show(j)
}"#, refused("Show"); FuseExpect::None);

// A trait method bounded by Any implies no Ordered for its implementation.
run!(trait_any_bound_does_not_imply_ordered, r#"{
    trait Cmp { val lt: fn<'a: Any>(self, x: 'a, y: 'a) -> bool };
    type W = Abstract<i64>;
    impl Cmp for W { let lt = 'a: Any |s: W, x: 'a, y: 'a| -> bool x < y };
    let a = 1;
    Cmp::lt(W(1), &a, &a)
}"#, refused("the implementation requires 'a: Ordered"); FuseExpect::None);

// A select over an open constructor application: a wildcard, a capture,
// a null test beside it and a tuple pattern around it each match what
// the constructor's bound admits.
run!(
    select_over_ctor_application,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
        if a[..] == [Value::I64(1), Value::I64(3), Value::I64(0), Value::I64(2)]),
    "/test.gx" => r#"
        let anyarm = 'c: Collection |xs: 'c<i64>| select xs { _ => 1 };
        let len = |xs: Collection| select xs { v => Collection::len(v) };
        let opt = 'c: Collection |xs: ['c<i64>, null]| select xs { null as _ => 0, _ => 2 };
        let tup = 'c: Collection |p: ('c<i64>, bool)| select p {
            (x, true) => Collection::len(x),
            (_, false) => 0
        };
        let result = (anyarm([1]), len([1, 2, 3]), opt(null), tup(([1, 2], true)))
    "#
);

// A second wildcard over an application is dead.
run!(
    select_over_ctor_application_dead_wildcard,
    refused("unreachable arm: the earlier arms already cover the whole scrutinee"),
    "/test.gx" => r#"
        let f = 'c: Collection |xs: 'c<i64>| select xs { _ => 1, _ => 2 };
        let result = f([1])
    "#
    ; FuseExpect::None
);

// No head of Collection is null.
run!(
    select_over_ctor_application_null_arm_dead,
    refused("pattern null will never match"),
    "/test.gx" => r#"
        let f = 'c: Collection |xs: 'c<i64>| select xs { null as _ => 0, _ => 2 };
        let result = f([1])
    "#
    ; FuseExpect::None
);

// An open constructor applied to a union holds a filled type only as the
// constructor's variance allows: a Cell<i64> is no 'c<[i64, string]>,
// whose writer could put a string in it.
run!(
    an_open_constructor_respects_the_filled_variance,
    refused("Cell<[i64, string]> does not contain Cell<i64>"),
    "/test.gx" => r#"
        type Cell<'a> = Abstract<&mut 'a>;
        type Other<'a> = Abstract<Array<'a>>;
        trait Store { val put: fn(self<'a>, v: 'a) -> i64 };
        impl Store for Cell<'_> { let put = |c, v| { let r = c.0; *r <- v; 0 } };
        impl Store for Other<'_> { let put = |c, v| 1 };
        let widen = 'c: Store |c: 'c<[i64, string]>| -> i64 Store::put(c, "oops");
        let x: i64 = 1;
        let c: Cell<i64> = Cell(&mut x);
        let result = widen(c)
    "#
; FuseExpect::None);

// A trait as a variadic's type is a bounded quantifier, as it is for a
// positional parameter: one type across the arguments.
run!(variadic_trait_param, r#"{
    let f = |@args: Eq| -> Any 'core_all;
    f(1, 1)
}"#, is(Value::I64(1)); FuseExpect::None);

run!(variadic_trait_param_is_one_type, r#"{
    let f = |@args: Eq| -> Any 'core_all;
    f(1, "s")
}"#, refused("does not contain string"); FuseExpect::None);

// A method's variable named like the impl target's is a variable of its
// own: the two `'a`s are unrelated, in a file and across an interface.
run!(
    method_tvar_named_like_target_tvar,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s"),
    "/test.gx" => r#"
        trait Conv { val conv: fn(self, x: 'a) -> 'a };
        type Box<'a> = Abstract<'a>;
        impl<'a> Conv for Box<'a> { let conv = |b, x| x };
        let result = Conv::conv(Box(1), "s")
    "#
    ; FuseExpect::Jit
);

run!(
    method_tvar_named_like_target_tvar_interface,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s"),
    "/test.gx" => r#"
mod m;
let result = m::Conv::conv(m::Box(1), "s")
"#,
    "/test/m.gxi" => r#"
trait Conv { val conv: fn(self, x: 'a) -> 'a };
type Box<'a> = Abstract<'a>;
impl<'a> Conv for Box<'a>;
"#,
    "/test/m.gx" => r#"
trait Conv { val conv: fn(self, x: 'a) -> 'a };
type Box<'a> = Abstract<'a>;
impl<'a> Conv for Box<'a> { let conv = |b, x| x };
"#
    ; FuseExpect::Jit
);

// The renamed method variable keeps its bound.
run!(
    method_tvar_renamed_keeps_its_bound,
    refused("does not contain string"),
    "/test.gx" => r#"
        trait Conv { val conv: fn<'a: Number>(self, x: 'a) -> 'a };
        type Box<'a> = Abstract<'a>;
        impl<'a> Conv for Box<'a> { let conv = |b, x| x };
        let result = Conv::conv(Box(1), "s")
    "#
    ; FuseExpect::None
);

// Parentheses around a method's lambda do not hide it from the
// signature's annotations.
run!(impl_method_in_parens_is_annotated, r#"{
    type Box = Abstract<(i64, i64)>;
    impl Eq for Box { let eq = (|a, b| a.0.1 == b.0.1) };
    (Box((1, 2)) == Box((3, 2)), Box((1, 2)) == Box((1, 3)))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::Bool(true), Value::Bool(false)]); FuseExpect::Jit);

// A dispatch whose self nothing produced into (an empty literal's
// element) settles that self to ⊥: the call is a standing bottom, not a
// refusal at the build.
run!(dispatch_on_a_never_produced_self_is_bottom, r#"{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "int [x]" };
    let a = [];
    any(Show::show(a[0]$), "none")
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "none"); FuseExpect::Jit);

// A core-trait impl runs only on values that exist: a kernel compares,
// orders, renders and keys an abstract value built over a late input
// only once the input has produced.
const HOOKS_ON_LATE_INPUT: &str = r#"{
    type K = Abstract<i64>;
    impl Eq for K { let eq = |a, b| { println("eq [a.0] [b.0]"); a.0 == b.0 } };
    impl Ord for K {
        let cmp = |a, b| {
            println("cmp [a.0] [b.0]");
            select (a.0, b.0) {
                (x, y) if x < y => `Less,
                (x, y) if x > y => `Greater,
                _ => `Equal
            }
        }
    };
    impl Display for K { let fmt = |k| { println("fmt [k.0]"); "K[k.0]" } };
    let clock = array::iter([1, 2]);
    (
        #[native] (K(clock) == K(1)),
        #[native] (K(clock) < K(2)),
        #[native] "[K(clock)]",
        #[native] map::len({K(clock) => 1, K(5) => 2})
    )
}"#;

async fn hooks_on_late_input(mode: Mode) -> Result<()> {
    let (values, out) =
        super::dense_deltas::run_delta_fused(HOOKS_ON_LATE_INPUT, mode, FuseExpect::Jit)
            .await?;
    let values: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(
        values,
        [r#"[true, true, "K1", i64:2]"#, r#"[false, false, "K2", i64:2]"#],
        "{mode:?}"
    );
    let mut lines: Vec<&str> = out.lines().collect();
    lines.sort_unstable();
    assert_eq!(
        lines,
        [
            "cmp 1 2", "cmp 2 2", "cmp 5 1", "cmp 5 1", "cmp 5 2", "cmp 5 2", "eq 1 1",
            "eq 2 1", "fmt 1", "fmt 2"
        ],
        "{mode:?}: {out}"
    );
    Ok(())
}

modes!(hooks_on_late_input);

// A core trait a dynamic module implements follows its reloads: a
// hook site built before a reload is rebuilt after it.
run!(
    core_trait_dynamic_impl_reload,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "two1"),
    "/test.gx" => r#"
        let one = """
            type Counter = Abstract<i64>;
            impl Display for Counter { let fmt = |c| "one[c.0]" };
            let make = |x| Counter(x)
        """;
        let two = """
            type Counter = Abstract<i64>;
            impl Display for Counter { let fmt = |c| "two[c.0]" };
            let make = |x| Counter(x)
        """;
        let source = one;
        sys::net::publish("/local/dimpl_core_reload", source)?;
        let status = mod foo dynamic {
            sandbox whitelist [core];
            sig {
                type Counter;
                impl Display for Counter;
                val make: fn(x: i64) -> Counter
            };
            source sys::net::subscribe("/local/dimpl_core_reload")?
        };
        let shown = select status {
            error as e => never(dbg(e)),
            null as _ => "[foo::make(1)]"
        };
        source <- select shown { "one1" => two, _ => never() };
        let result = select shown { "two1" => shown, _ => never() }
    "#; FuseExpect::Jit
);

// A call into a dynamic module compiled at run time (a collection slot)
// dispatches dynamically, so it follows a reload as a top-level call does.
run!(
    dynamic_module_slot_calls_follow_reloads,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:200, i64:201, i64:202]"),
    "/test.gx" => r#"
        let one = "let f = |x: i64| x + 100";
        let two = "let f = |x: i64| x + 200";
        let source = one;
        let status = mod foo dynamic {
            sandbox whitelist [core];
            sig { val f: fn(x: i64) -> i64 };
            source source
        };
        let n = 1;
        let xs = select status {
            error as e => never(dbg(e)),
            null as _ => array::map(array::init(n, |i| i), |i| foo::f(i))
        };
        n <- select xs { [100] => 3, _ => never() };
        source <- select xs { [100, 101, 102] => two, _ => never() };
        let result = select xs { [200, ..] => xs, _ => never() }
    "#; FuseExpect::Jit
);

// Every name a trait method's signature writes must name a type.
run!(trait_signature_undefined_type_refused, r#"{
  trait Q { val q: fn(self) -> Nope };
  0
}"#, refused("undefined type Nope"); FuseExpect::None);

// An impl head's variables are its declared ones: a method may not
// narrow them.
run!(impl_method_does_not_narrow_the_head, r#"{
  type Box<'a> = Abstract<'a>;
  impl<'a> Display for Box<'a> { let fmt = |b: Box<i64>| "n=[b.0 + 1]" };
  "[Box(41)]"
}"#, refused("method fmt of impl Display for Box<'a> has type"); FuseExpect::None);

// A core-trait hook site a dynamic load retires is deleted by whichever
// dispatch finds it stale: its references are registered, and released,
// under no walk's boundary.
run!(
    retired_hook_site_deleted_from_another_statement,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "T3 T0"),
    "/test.gx" => r#"
        type T = Abstract<i64>;
        let un = |t: T| -> i64 t.0;
        impl Display for T { let fmt = |t| "T[un(t)]" };
        let step = 0;
        step <- select step { k if k < 3 => (k ~ step) + 1, _ => never() };
        let a = "[T(0)]";
        let src = select step { 1 => "let x = 1", _ => never() };
        let st = mod foo dynamic { sandbox whitelist [core]; sig { val x: i64 }; source src };
        let result = select step { 3 => "[T(step)] [a]", _ => never() }
    "#; FuseExpect::Jit);
