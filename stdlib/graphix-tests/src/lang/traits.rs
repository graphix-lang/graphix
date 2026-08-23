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
