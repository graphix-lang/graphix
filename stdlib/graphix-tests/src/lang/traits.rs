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

// A `never()` arm types as a cell that resolves to bottom, and the
// select's type is the union of its arms' CELLS — so the narrowing
// idiom `select opt { null as _ => never(), s => s }` hands dispatch a
// self type of `[⊥, Counter]`. Bottom is the identity of the union, so
// there is one member to dispatch on; resolving the cells without
// normalizing left the bottom standing and demanded an impl for it
// (`sys::process`'s `[Pipe, null]` stdin, through Write).
// An INTERFACE-DECLARED Collection-generic fn: `fn(c: Collection)`
// elaborates to `App('#c, '_elem)` on both the gxi and the impl side,
// and `sig_matches_int` had no App arm — the pair fell to the
// catch-all and no module could export such a fn (found by gen-check
// the day the generator learned the constructor-trait vocabulary:
// collection-generic-call 0/8 DEAD ARM). Called at BOTH an Array and
// a Map so the dispatch decomposes two constructors through one
// export.
run!(
    collection_generic_interface_declared,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(5))),
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
    // The `c: Collection` param is `App(self, 'e)`; `abi_kind`/
    // `freeze_for_abi` now reduce that constructor application, so the
    // fold kernel builds and the generic function fuses over both the
    // Array and Map call.
    ; graphix_package_core::testing::FuseExpect::Jit);

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

// THE VALUE SEAM (design/traits.md §12): `Value`'s own eq/cmp reach a
// core implementation through the abstract vtable, so a MAP is keyed
// by the user's Ord — Eric's motivating example: a reversed order
// reverses the key order, and lookups agree.
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
    ; FuseExpect::None
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
    ; graphix_package_core::testing::FuseExpect::None
);

// THE BOTTOM-KEY RULE (Eric's ruling 2026-08-23): a bottoming
// implementation resolves per KEY, like NaN — a key the impl bottoms
// on (here payload 0: 1 /? 0 errors, `$` drops it) sorts below every
// real key and equal to its fellow bottom keys; pairs of real keys
// answer by the impl. A structural fallback per pair would break the
// total order.
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
    ; graphix_package_core::testing::FuseExpect::None
);

// An implementation method may be a BUILTIN reference. This is how a
// package gives a Rust-backed abstract type its io methods
// (`impl Read for File { let read = |s, n| 'sys_io_read }`), and it
// works because the trait's signature, instantiated at the target, is
// pushed into the lambda — a builtin body needs every argument and
// the return annotated, and the trait declaration is where they come
// from.
run!(
    trait_builtin_bodied_impl,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
        trait Masked { val masked: fn(self, m: i64) -> i64 };
        impl Masked for i64 { let masked = |x, m| 'core_bit_and };
        let result = Masked::masked(6, 3)
    "#
    ; FuseExpect::None
);

// The same, through a bound rather than the dispatcher — the builtin
// body is instantiated per resolved implementation.
run!(
    trait_builtin_bodied_bound,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(4))),
    "/test.gx" => r#"
        trait Masked { val masked: fn(self, m: i64) -> i64 };
        impl Masked for i64 { let masked = |x, m| 'core_bit_and };
        let f = 'a: Masked |v: 'a| Masked::masked(v, 12);
        let result = f(6) + f(1)
    "#
    ; FuseExpect::None
);

// An interface's `impl` declaration does not displace the type
// declarations that follow it. The interface's types/mods/uses are
// spliced into the implementation anchored on the item BEFORE them,
// and an `impl` is never spliced (the implementation writes its own),
// so it must anchor nothing — otherwise everything after it landed at
// the end of the module body, invisible to the code above (found
// migrating `sys::process` to the io traits, 2026-08-23).
run!(
    interface_type_after_impl,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1))),
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

// A core trait rides the value, and a Rust-backed abstract type has no
// payload for the implementation to read — so an implementation for
// one would compile and never be consulted. Refused (2026-08-23).
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

// THE POINT OF THE FEATURE (`design/traits.md` §0): a stream written
// in Graphix. `Mem` supplies `read` and nothing else — `read_all` is
// the trait's own default, written over `read`, so it works on a
// stream the io package has never heard of.
run!(
    graphix_defined_read,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello world"),
    "/test.gx" => r#"
        use sys::io::Read;
        type Mem = Abstract<&bytes>;
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
        let m = Mem(&src);
        let result = buffer::to_string(Read::read_all(m)?)?
    "#
);

// The other derived method, on the same Graphix-defined stream:
// `read_exact` loops over `read` until it has n bytes.
run!(
    graphix_defined_read_exact,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello"),
    "/test.gx" => r#"
        use sys::io::Read;
        type Mem = Abstract<&bytes>;
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
        let m = Mem(&src);
        let result = buffer::to_string(Read::read_exact(m, u64:5)?)?
    "#
);

// ── Dynamic modules ─────────────────────────────────────────────────
//
// A dynamic module's signature declares an impl exactly as a gxi does;
// the consumer compiles against the declaration before any source has
// loaded, so its calls must reach whatever implementation the loaded
// source registers — and re-reach it after a reload.

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
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(9)")
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
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(2) Counter(2)")
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
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "Counter(4)")
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
    "#
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
    "#
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

run!(dynamic_module_typecheck1, DYNAMIC_MODULE_TC1, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(12))
));

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
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "C<3>")
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
    ; FuseExpect::None
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

// A constructor-trait call's result type is `App(self, 'b)` with `self`
// bound to the receiver's constructor. Every consumer that derefs a
// type must see the FILLED type (`Type::app_filled`, through
// `with_deref`): the select's coverage check refused this program
// ("no unguarded arm irrefutably covers '_: Array<'b: i64>"), `cast`
// refused the value, and the typed printer logged a mismatch and fell
// back to naked printing. Fusion already filled it (kernel_abi).
const TRAIT_RESULT_IS_FILLED: &str = r#"
{
  use core::Collection::{self, *};
  let c = map([i64:1, i64:2, i64:5], |x| x * x);
  let n = select c { [] => i64:0, [h, rest..] => h + array::len(rest) };
  let t = cast<Array<i64>>(c)$;
  n + t[1]$
}
"#;

run!(trait_result_is_filled, TRAIT_RESULT_IS_FILLED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(7))
); graphix_package_core::testing::FuseExpect::Jit);
