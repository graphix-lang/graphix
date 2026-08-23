// Traits (design/traits.md): declarations with required and default
// methods, implementations over abstract types and (in the trait's own
// package) structural types, traits as bounds, static dispatch through
// per-callsite elaboration, and dispatch over a union self type as a
// generated select.

use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};

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
    |v: Result<&Value>| v.is_err(),
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
    |v: Result<&Value>| v.is_err(),
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
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        impl Show for i64 { let show = |x| "again [x]" };
        let result = Show::show(1)
    "#
    ; FuseExpect::None
);

// Dispatch over a union self type: the generated select picks the
// member's implementation at runtime.
// ASPIRE: Jit (currently None) — the generated select's arms test an
// abstract type, and abstract patterns de-fuse a select for now.
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
    ; FuseExpect::None
);

// A union member without an implementation is refused at the call.
run!(
    trait_union_member_missing_refused,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        let pick = |b: bool| -> [i64, string] select b { true => 1, false => "s" };
        let result = Show::show(pick(true))
    "#
    ; FuseExpect::None
);

// A trait method as a higher-order argument: the callback's call sites
// resolve by the element type.
run!(
    trait_method_as_callback,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 2),
    "/test.gx" => r#"
        trait Show { val show: fn(self) -> string };
        impl Show for i64 { let show = |x| "int [x]" };
        let result = array::map([1, 2], Show::show)
    "#
);

// A parameterized head: the bound on the element discharges through
// the implementation table.
// ASPIRE: Jit (currently None) — the impl body's `array::map(xs, Show::show)`
// callback prototype resolves statically but the map does not lower yet.
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
    ; FuseExpect::None
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

// A program is one package: a sibling module may implement the trait
// for a primitive (the orphan rule bites only ACROSS packages — a
// stranger package's structural impl is what it forbids).
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

// A quantifier bound written in a `let` annotation is enforced
// (`scope_refs` used to drop the cell conjunct, 2026-08-22).
run!(
    annotation_bound_enforced,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        let f: fn<'a: Number>(x: 'a) -> 'a = |x| x;
        let result = f("hi")
    "#
    ; FuseExpect::None
);

// A polymorphic binding used as a VALUE is instantiated per
// occurrence, like a call: two uses at different types do not pin
// each other through the definition's cells.
// ASPIRE: Jit (currently None) — one lambda instantiated at two
// element types in one region does not lower yet.
run!(
    poly_value_two_types,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "[1] [1.5]"),
    "/test.gx" => r#"
        let f = 'a: Number |x: 'a| x;
        let result = "[array::map([1], f)] [array::map([1.5], f)]"
    "#
    ; FuseExpect::None
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

// ── The core traits: Eq, Ord, Display (design/traits.md §8) ─────────
//
// `==`/`!=`, `<`/`>`/`<=`/`>=` and printing consult an implementation
// of the core trait wherever one sits in the STATIC type, and take the
// structural case everywhere else. A whole-type implementation lowers
// to a static call (fuses); one nested inside a composite runs the
// hooked walk (interprets).

// A case-insensitive key: `==` calls the implementation.
run!(
    core_eq_abstract,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };
        let result = Key("Foo") == Key("FOO") && Key("a") != Key("b")
    "##
);

// Inside a composite the walk calls it per element; the structural
// parts compare as values.
run!(
    core_eq_nested,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "true|false|Less|Equal|Greater|(1, 2)|#123"),
    "/test.gx" => r##"
        type Color = Abstract<{r: i64, g: i64, b: i64}>;
        impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };
        let same = 'a: Eq |x: 'a, y: 'a| Eq::eq(x, y);
        let show = 'a: Display |x: 'a| Display::fmt(x);
        let result = "[same(1, 1)]|[same("a", "b")]|[Ord::cmp(1, 2)]|[Ord::cmp((1, 2), (1, 2))]|[Ord::cmp("b", "a")]|[show((1, 2))]|[show(Color({r: 1, g: 2, b: 3}))]"
    "##
    ; FuseExpect::None
);

// A core trait method runs inside the comparison, so it is implicitly
// `#[sync]`: an async body is a compile error.
run!(
    core_method_async_refused,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r##"
        type Key = Abstract<string>;
        impl Display for Key { let fmt = |k| sys::time::timer(duration:0.01s, false) ~ k.0 };
        let result = "[Key("a")]"
    "##
    ; FuseExpect::None
);

// `println`/`dbg`/`log` print through the implementation too, on
// both engines (the builtin formats through the hook from inside a
// fused kernel's DynCall as well).
async fn core_display_println(fusion_disabled: bool) -> Result<()> {
    let code = r##"{
        type Color = Abstract<{r: i64, g: i64, b: i64}>;
        impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };
        let c = Color({r: 1, g: 2, b: 3});
        println(c);
        println([c, c]);
        print("[c]");
        println("");
        dbg(#dest: `Stdout, {c, n: 1});
        42
    }"##;
    let (values, out) = super::dense_deltas::run_delta(code, fusion_disabled).await?;
    assert_eq!(super::dense_deltas::as_i64s(&values), vec![42]);
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "#123");
    assert_eq!(lines[1], "[#123, #123]");
    assert_eq!(lines[2], "#123");
    assert!(lines[3].ends_with("): {c: #123, n: 1}"), "{}", lines[3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn core_display_println_interp() -> Result<()> {
    core_display_println(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn core_display_println_jit() -> Result<()> {
    core_display_println(false).await
}

// A trait call on a union self type INSIDE a lambda: the lowered
// select binds the call's argument nodes, it does not recompile their
// source (the lambda's parameters are out of lexical scope by the
// time the call is lowered at typecheck1).
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
    ; FuseExpect::None
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
