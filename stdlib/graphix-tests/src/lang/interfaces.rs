// Abstract types and interface files (.gxi). An abstract type is nominal:
// values are minted only by `T(..)` and read through `.0` or `T(x)`
// where the definition is visible (design/nominal_abstract_types.md).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused, refused_renumbered, settles},
};
use netidx::publisher::Value;

// The interface declares an abstract type; the implementation defines it.
run!(
    abstract_type_basic,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get(inner::make(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let make = |x: i64| -> T T(x);
        let get = |t: T| -> i64 t.0
    "#);

// An abstract type implemented as a struct.
run!(
    abstract_type_struct_impl,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get_name(inner::make("hello"))
    "#,
    "/test/inner.gxi" => r#"
        type Handle;
        val make: fn(x: string) -> Handle;
        val get_name: fn(h: Handle) -> string
    "#,
    "/test/inner.gx" => r#"
        type Handle = Abstract<{ value: string }>;
        let make = |x: string| -> Handle Handle({ value: x });
        let get_name = |h: Handle| h.0.value
    "#; FuseExpect::Jit);

// An interface without abstract types.
run!(
    interface_no_abstract_types,
    is(Value::I64(30)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::add(10, 20)
    "#,
    "/test/inner.gxi" => r#"
        type Point = { x: i64, y: i64 };
        val add: fn(a: i64, b: i64) -> i64     "#,
    "/test/inner.gx" => r#"
        let add = |a: i64, b: i64| -> i64 a + b
    "#);

// Multiple abstract types in one interface.
run!(
    abstract_type_multiple,
    is(Value::I64(15)),
    "/test.gx" => r#"
        mod inner;
        let a = inner::make_a(10);
        let b = inner::make_b(5);
        let result = inner::combine(a, b)
    "#,
    "/test/inner.gxi" => r#"
        type A;
        type B;
        val make_a: fn(x: i64) -> A;
        val make_b: fn(y: i64) -> B;
        val combine: fn(a: A, b: B) -> i64     "#,
    "/test/inner.gx" => r#"
        type A = Abstract<{ x: i64 }>;
        type B = Abstract<{ y: i64 }>;
        let make_a = |x: i64| -> A A({ x });
        let make_b = |y: i64| -> B B({ y });
        let combine = |a: A, b: B| -> i64 a.0.x + b.0.y
    "#);

// Two modules using the same abstract type name with different definitions.
run!(
    abstract_type_different_modules,
    is(Value::I64(142)),
    "/test.gx" => r#"
        mod mod_a;
        mod mod_b;
        let a = mod_a::make(42);
        let b = mod_b::make(100);
        let result = mod_a::get(a) + mod_b::get(b)
    "#,
    "/test/mod_a.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64     "#,
    "/test/mod_a.gx" => r#"
        type T = Abstract<{ value: i64 }>;
        let make = |x: i64| -> T T({ value: x });
        let get = |t: T| -> i64 t.0.value
    "#,
    "/test/mod_b.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64     "#,
    "/test/mod_b.gx" => r#"
        type T = Abstract<i64>;
        let make = |x: i64| -> T T(x);
        let get = |t: T| -> i64 t.0
    "#);

// An abstract type used in an exported type definition.
run!(
    abstract_type_in_typedef,
    is(Value::I64(77)),
    "/test.gx" => r#"
        mod inner;
        let p = inner::make_pair(77, "test");
        let result = inner::get_first(p)
    "#,
    "/test/inner.gxi" => r#"
        type First;
        type Pair = { first: First, second: string };
        val make_pair: fn(a: i64, b: string) -> Pair;
        val get_first: fn(p: Pair) -> i64
    "#,
    "/test/inner.gx" => r#"
        type First = Abstract<i64>;
        let make_pair = |a: i64, b: string| -> Pair { first: First(a), second: b };
        let get_first = |p: Pair| -> i64 p.first.0
    "#; FuseExpect::Jit);

// An abstract type in a variant.
run!(
    abstract_type_in_variant,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let opt = inner::some(42);
        let result = inner::get_or_default(opt, 0)
    "#,
    "/test/inner.gxi" => r#"
        type T;
        type Option = [`Some(T), `None];
        val some: fn(x: i64) -> Option;
        val get_or_default: fn(opt: Option, default: i64) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<{ value: i64 }>;
        let some = |x: i64| -> Option `Some(T({ value: x }));
        let get_or_default = |opt: Option, default: i64| -> i64 select opt {
            `Some(t) => t.0.value,
            `None => default
        }
    "#; FuseExpect::Jit);

// An abstract type in a tuple.
run!(
    abstract_type_in_tuple,
    is(Value::I64(15)),
    "/test.gx" => r#"
        mod inner;
        let pair = inner::make_pair(5, 10);
        let result = inner::sum_pair(pair)
    "#,
    "/test/inner.gxi" => r#"
        type Elem;
        type Pair = (Elem, Elem);
        val make_pair: fn(a: i64, b: i64) -> Pair;
        val sum_pair: fn(p: Pair) -> i64     "#,
    "/test/inner.gx" => r#"
        type Elem = Abstract<i64>;
        let make_pair = |a: i64, b: i64| -> Pair (Elem(a), Elem(b));
        let sum_pair = |p: Pair| -> i64 p.0.0 + p.1.0
    "#);

// An abstract type in an array.
run!(
    abstract_type_in_array,
    is(Value::I64(6)),
    "/test.gx" => r#"
        mod inner;
        let arr = inner::make_array([1, 2, 3]);
        let result = inner::sum_array(arr)
    "#,
    "/test/inner.gxi" => r#"
        type Elem;
        val make_array: fn(arr: Array<i64>) -> Array<Elem>;
        val sum_array: fn(arr: Array<Elem>) -> i64     "#,
    "/test/inner.gx" => r#"
        type Elem = Abstract<i64>;
        let make_array = |arr: Array<i64>| -> Array<Elem> array::map(arr, |x| Elem(x));
        let sum_array = |arr: Array<Elem>| -> i64 array::fold(arr, 0, |acc, x| acc + x.0)
    "#);

// An abstract type in a recursive type.
run!(
    abstract_type_recursive,
    is(Value::I64(6)),
    "/test.gx" => r#"
        mod inner;
        let list = inner::cons(1, inner::cons(2, inner::cons(3, inner::nil())));
        let result = inner::sum(list)
    "#,
    "/test/inner.gxi" => r#"
        type Elem;
        type L = [`Cons(Elem, L), `Nil];
        val cons: fn(x: i64, rest: L) -> L;
        val nil: fn() -> L;
        val sum: fn(list: L) -> i64     "#,
    "/test/inner.gx" => r#"
        type Elem = Abstract<i64>;
        let cons = |x: i64, rest: L| -> L `Cons(Elem(x), rest);
        let nil = || -> L `Nil;
        let rec sum = |list: L| -> i64 select list {
            `Cons(x, rest) => x.0 + sum(rest),
            `Nil => 0
        }
    "#; FuseExpect::Jit);

// An abstract type behind a byref parameter.
run!(
    abstract_type_byref,
    prints("[i64:42, i64:43]"),
    "/test.gx" => r#"
        mod inner;
        let counter = inner::make(42);
        inner::increment(&mut counter);
        let result = array::group(inner::get(counter), |n, _| n == 2)
    "#,
    "/test/inner.gxi" => r#"
        type Counter;
        val make: fn(x: i64) -> Counter;
        val get: fn(c: Counter) -> i64;
        val increment: fn(c: &mut Counter) -> null     "#,
    "/test/inner.gx" => r#"
        type Counter = Abstract<i64>;
        let make = |x: i64| -> Counter Counter(x);
        let get = |c: Counter| -> i64 c.0;
        let increment = |c: &mut Counter| -> null { *c <- Counter(once(*c).0 + 1); null }
    "#);

// A nested module with an abstract type.
run!(
    abstract_type_nested_module,
    is(Value::I64(99)),
    "/test.gx" => r#"
        mod outer;
        let result = outer::inner::get(outer::inner::make(99))
    "#,
    "/test/outer.gxi" => r#"
        mod inner
    "#,
    "/test/outer.gx" => r#"
        mod inner
    "#,
    "/test/outer/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64
    "#,
    "/test/outer/inner.gx" => r#"
        type T = Abstract<{ v: i64 }>;
        let make = |x: i64| -> T T({ v: x });
        let get = |t: T| -> i64 t.0.v
    "#);

// A dynamic module with an abstract type in its signature.
run!(
    abstract_type_dynamic_module,
    is(Value::I64(84)),
    "/test.gx" => r#"
        let source = "
            type T = Abstract<i64>;
            let make = |x: i64| -> T T(x);
            let double = |t: T| -> i64 t.0 + t.0
        ";
        sys::net::publish("/local/dyn_test", source)?;
        let status = mod dyn dynamic {
            sig { use core;
                type T;
                val make: fn(x: i64) -> T;
                val double: fn(t: T) -> i64             };
            source sys::net::subscribe("/local/dyn_test")?
        };
        let result = select status {
            error as e => never(dbg(e)),
            null as _ => dyn::double(dyn::make(42))
        }
    "#
; FuseExpect::None);

// A gxi `type T;` the gx does not define is a Rust-backed type: it
// compiles.
run!(
    abstract_type_rust_backed,
    is(Value::I64(0)),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        let x = 42
    "#
; FuseExpect::None);

// An abstract type in the implementation is allowed (stays opaque).
run!(
    abstract_type_still_abstract,
    |v: Result<&Value>| v.map(|v| v == &Value::I64(0)).unwrap_or(false),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        type T;
        let x = 42
    "#
; FuseExpect::None);

// A signature's union beside a function-typed parameter pairs with its
// implementation's. Whether a crossed pairing waits for the parameters after
// it, which only a union of two variables of the signature asks, is unpinned:
// a union of two bare variables is refused, and one of members holding them
// is formal-union-nested-01.
// CR claude for claude: [readability/low] The name says the pairing waits for later
// parameters, while the comment above says that question is unpinned and CLAUDE.md says
// nothing waits for a later argument. What the fixture pins is a union with one
// variable member (`['a, string]` against `['d, string]`) pairing with its
// implementation's, which waits for nothing. Rename it for that
// (`union_with_one_variable_member_pairs_with_its_implementations`) and drop the
// finding id `formal-union-nested-01` from the comment. Found by reading; not run.
// (tests-interfaces-image-08)
run!(
    union_pairing_waits_for_later_parameters,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0))),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f(1, |n: i64| n, 1, "s")
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: ['a, string], g: fn(n: i64) -> i64, y: 'a, z: 'b) -> i64
    "#,
    "/test/inner.gx" => r#"
        let f = |x: ['d, string], g: fn(n: i64) -> i64, y: 'd, z: 'c| -> i64 0
    "#
; FuseExpect::Jit);

// A throws mismatch names the types as written, and a bottom throws as
// throwing nothing.
run!(
    throws_mismatch_names_nothing,
    refused("val f is declared fn(x: i64) -> i64 but implemented fn(x: i64) -> i64: type mismatch _ does not contain Error<ErrChain<`Zero>>"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f(1)
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: i64) -> i64
    "#,
    "/test/inner.gx" => r#"
        let f = |x: i64| -> i64 select x { 0 => error(`Zero)?, n => n }
    "#
; FuseExpect::None);

run!(
    throws_mismatch_names_the_written_types,
    refused("val f is declared fn(x: i64) -> i64 throws Error<ErrChain<`A>> but implemented fn(x: i64) -> i64: type mismatch `A does not contain `Zero"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f(1)
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: i64) -> i64 throws Error<ErrChain<`A>>
    "#,
    "/test/inner.gx" => r#"
        let f = |x: i64| -> i64 select x { 0 => error(`Zero)?, n => n }
    "#
; FuseExpect::None);

// Error: the function returns the wrong type.
run!(
    abstract_type_sig_mismatch,
    refused("val make is declared fn(x: i64) -> T but implemented fn(x: i64) -> i64"),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<string>;
        let make = |x: i64| -> i64 x
    "#
; FuseExpect::None);

// Error: abstract type parameter constraint mismatch.
run!(
    abstract_type_constraint_mismatch,
    refused("constraint mismatch on a, signature constraint Number vs implementation constraint missing"),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T<'a: Number>;
        val make: fn(x: 'a) -> T<'a>
    "#,
    "/test/inner.gx" => r#"
        type T<'a> = Abstract<{ val: 'a }>;
        let make = |x: 'a| -> T<'a> T({ val: x })
    "#
; FuseExpect::None);

// The constraint on `Box<'a: Number>` propagates to wrap/unwrap without
// repeating it in the val declarations.
run!(
    abstract_type_constraint_auto_enforced,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let box = inner::wrap(42);
        let result = inner::unwrap(box)
    "#,
    "/test/inner.gxi" => r#"
        type Box<'a: Number>;
        val wrap: fn(x: 'a) -> Box<'a>;
        val unwrap: fn(b: Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a: Number> = Abstract<{ value: 'a }>;
        let wrap = |x: 'a| -> Box<'a> Box({ value: x });
        let unwrap = |b: Box<'a>| -> 'a b.0.value
    "#
; FuseExpect::Jit);

// Error: string does not satisfy `Box<'a: Number>`.
run!(
    abstract_type_constraint_auto_enforced_error,
    refused("type mismatch Number does not contain string"),
    "/test.gx" => r#"
        mod inner;
        let box = inner::wrap("hello");
        let result = inner::unwrap(box)
    "#,
    "/test/inner.gxi" => r#"
        type Box<'a: Number>;
        val wrap: fn(x: 'a) -> Box<'a>;
        val unwrap: fn(b: Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a: Number> = Abstract<{ value: 'a }>;
        let wrap = |x: 'a| -> Box<'a> Box({ value: x });
        let unwrap = |b: Box<'a>| -> 'a b.0.value
    "#
; FuseExpect::None);

// An interface declares every bound its implementation's variables
// carry: a call through it is checked by it alone.
run!(
    interface_declares_the_arithmetic_bound,
    is(Value::I64(4)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::add(2, 2)
    "#,
    "/test/inner.gxi" => r#"
        val add: fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a
    "#,
    "/test/inner.gx" => r#"
        let add = |x, y| x + y
    "#
);

run!(
    interface_omits_singleton,
    |v: Result<&Value>| v.is_err_and(|e| format!("{e:#}").contains("'a: Singleton")),
    "/test.gx" => r#"
        mod inner;
        let result = inner::add(2, 2)
    "#,
    "/test/inner.gxi" => r#"
        val add: fn<'a: Number>(x: 'a, y: 'a) -> 'a
    "#,
    "/test/inner.gx" => r#"
        let add = |x, y| x + y
    "#
; FuseExpect::None);

run!(
    interface_omits_number,
    refused_renumbered("val double is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within Singleton & '_0: … & [u8, i8, u16, i16, u32, v32, i32, z32, u64, v64, i64, z64, f32, f64, decimal] & '_2: unbound within '_3: unbound) -> '_3: unbound: the implementation requires 'a: Singleton, which the signature does not declare"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::double(2)
    "#,
    "/test/inner.gxi" => r#"
        val double: fn(x: 'a) -> 'a
    "#,
    "/test/inner.gx" => r#"
        let double = |x| x + x
    "#
; FuseExpect::None);

// Error: extra type parameter in the implementation.
run!(
    abstract_type_extra_param,
    refused("signature mismatch in T, the interface declares 1 parameters, the implementation 2"),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T<'a>;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        type T<'a, 'b> = Abstract<('a, 'b)>;
        let x = 42
    "#
; FuseExpect::None);

run!(
    abstract_type_missing_param,
    refused("signature mismatch in T, the interface declares 2 parameters, the implementation 1"),
    "/test.gx" => r#"
        mod inner;
        let b: inner::T<i64, string> = never();
        let result = inner::x
    "#,
    "/test/inner.gxi" => r#"
        type T<'a, 'b>;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        type T<'a> = Abstract<'a>;
        let x = 42
    "#
; FuseExpect::None);

run!(
    abstract_type_reordered_params,
    refused("signature mismatch in T, parameter b where the interface declares a"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::x
    "#,
    "/test/inner.gxi" => r#"
        type T<'a, 'b: Number>;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        type T<'b: Number, 'a> = Abstract<('a, 'b)>;
        let x = 42
    "#
; FuseExpect::None);

// Error: the implementation's argument type does not match the
// signature's abstract type.
run!(
    abstract_type_wrong_arg,
    refused("val get is declared fn(t: T) -> i64 but implemented fn(t: i64) -> i64"),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<string>;
        let get = |t: i64| -> i64 t
    "#
; FuseExpect::None);

// A parameterized abstract type.
run!(
    abstract_type_parameterized_basic,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let box = inner::wrap(42);
        let result = inner::unwrap(box)
    "#,
    "/test/inner.gxi" => r#"
        type Box<'a>;
        val wrap: fn(x: 'a) -> Box<'a>;
        val unwrap: fn(b: Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a> = Abstract<{ value: 'a }>;
        let wrap = |x: 'a| -> Box<'a> Box({ value: x });
        let unwrap = |b: Box<'a>| -> 'a b.0.value
    "#; FuseExpect::Jit);

// A parameterized abstract type at different concrete types.
run!(
    abstract_type_parameterized_multi_instantiation,
    is(Value::I64(47)),
    "/test.gx" => r#"
        mod inner;
        let int_box = inner::wrap(42);
        let str_box = inner::wrap("hello");
        let result = inner::unwrap(int_box) + str::len(inner::unwrap(str_box))
    "#,
    "/test/inner.gxi" => r#"
        type Box<'a>;
        val wrap: fn(x: 'a) -> Box<'a>;
        val unwrap: fn(b: Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a> = Abstract<{ value: 'a }>;
        let wrap = |x: 'a| -> Box<'a> Box({ value: x });
        let unwrap = |b: Box<'a>| -> 'a b.0.value
    "#; FuseExpect::Jit);

// A parameterized abstract type with a constraint, instantiated
// concretely in the interface.
run!(
    abstract_type_parameterized_constrained,
    is(Value::I64(84)),
    "/test.gx" => r#"
        mod inner;
        let wrapper = inner::wrap(42);
        let result = inner::double(wrapper)
    "#,
    "/test/inner.gxi" => r#"
        type NumWrapper<'a: Number>;
        type IntWrapper = NumWrapper<i64>;
        val wrap: fn(x: i64) -> IntWrapper;
        val double: fn(w: IntWrapper) -> i64     "#,
    "/test/inner.gx" => r#"
        type NumWrapper<'a: Number> = Abstract<'a>;
        let wrap = |x: i64| -> IntWrapper NumWrapper(x);
        let double = |w: IntWrapper| -> i64 w.0 + w.0
    "#; FuseExpect::Jit);

// A parameterized abstract type in nested position (Array of Box).
run!(
    abstract_type_parameterized_nested,
    is(Value::I64(6)),
    "/test.gx" => r#"
        mod inner;
        let boxes = [inner::wrap(1), inner::wrap(2), inner::wrap(3)];
        let result = inner::sum_boxes(boxes)
    "#,
    "/test/inner.gxi" => r#"
        type Box<'a>;
        type IntBoxArray = Array<Box<i64>>;
        val wrap: fn(x: 'a) -> Box<'a>;
        val sum_boxes: fn(boxes: IntBoxArray) -> i64     "#,
    "/test/inner.gx" => r#"
        type Box<'a> = Abstract<{ value: 'a }>;
        let wrap = |x: 'a| -> Box<'a> Box({ value: x });
        let sum_boxes = |boxes: IntBoxArray| -> i64
            array::fold(boxes, 0, |acc, b| acc + b.0.value)
    "#; FuseExpect::Jit);

// Two type parameters.
run!(
    abstract_type_parameterized_two_params,
    is(Value::I64(47)),
    "/test.gx" => r#"
        mod inner;
        let pair = inner::make(42, "hello");
        let result = inner::get_first(pair) + str::len(inner::get_second(pair))
    "#,
    "/test/inner.gxi" => r#"
        type Pair<'a, 'b>;
        val make: fn(a: 'a, b: 'b) -> Pair<'a, 'b>;
        val get_first: fn(p: Pair<'a, 'b>) -> 'a;
        val get_second: fn(p: Pair<'a, 'b>) -> 'b
    "#,
    "/test/inner.gx" => r#"
        type Pair<'a, 'b> = Abstract<{ first: 'a, second: 'b }>;
        let make = |a: 'a, b: 'b| -> Pair<'a, 'b> Pair({ first: a, second: b });
        let get_first = |p: Pair<'a, 'b>| -> 'a p.0.first;
        let get_second = |p: Pair<'a, 'b>| -> 'b p.0.second
    "#; FuseExpect::Jit);

// An abstract type as a Map key.
run!(
    abstract_type_map_key,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "found"),
    "/test.gx" => r#"
        mod inner;
        let key = inner::make_key(42);
        let m = inner::make_map();
        let result = inner::lookup(m, key)
    "#,
    "/test/inner.gxi" => r#"
        type Key;
        type KeyMap = Map<Key, string>;
        val make_key: fn(x: i64) -> Key;
        val make_map: fn() -> KeyMap;
        val lookup: fn(m: KeyMap, k: Key) -> string throws Error<ErrChain<`MapKeyError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Key = Abstract<i64>;
        let make_key = |x: i64| -> Key Key(x);
        let make_map = || -> KeyMap {Key(42) => "found", Key(99) => "other"};
        let lookup = |m: KeyMap, k: Key| m{k}?
    "#);

// An abstract type as a Map value.
run!(
    abstract_type_map_value,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let m = inner::make_map();
        let v = inner::get(m, "key");
        let result = inner::unwrap(v)
    "#,
    "/test/inner.gxi" => r#"
        type Val;
        type ValMap = Map<string, Val>;
        val make_map: fn() -> ValMap;
        val get: fn(m: ValMap, k: string) -> Val throws Error<ErrChain<`MapKeyError(string)>>;
        val unwrap: fn(v: Val) -> i64
    "#,
    "/test/inner.gx" => r#"
        type Val = Abstract<{ inner: i64 }>;
        let make_map = || -> ValMap {"key" => Val({ inner: 42 })};
        let get = |m: ValMap, k: string| -> Val m{k}?;
        let unwrap = |v: Val| -> i64 v.0.inner
    "#);

// Abstract types as both Map key and value.
run!(
    abstract_type_map_key_and_value,
    is(Value::I64(100)),
    "/test.gx" => r#"
        mod inner;
        let k = inner::make_key("test");
        let m = inner::make_map(k, 100);
        let v = inner::lookup(m, k);
        let result = inner::get_val(v)
    "#,
    "/test/inner.gxi" => r#"
        type K;
        type V;
        type KVMap = Map<K, V>;
        val make_key: fn(s: string) -> K;
        val make_map: fn(k: K, n: i64) -> KVMap;
        val lookup: fn(m: KVMap, k: K) -> V throws Error<ErrChain<`MapKeyError(string)>>;
        val get_val: fn(v: V) -> i64
    "#,
    "/test/inner.gx" => r#"
        type K = Abstract<{ name: string }>;
        type V = Abstract<i64>;
        let make_key = |s: string| -> K K({ name: s });
        let make_map = |k: K, n: i64| -> KVMap {k => V(n)};
        let lookup = |m: KVMap, k: K| -> V m{k}?;
        let get_val = |v: V| -> i64 v.0
    "#);

// An abstract type as an error payload in a throws clause.
run!(
    abstract_type_in_throws,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let result = {
            catch(e) {
                let chain = e.0;
                select chain.error {
                    `CustomError(_) => 0
                }
            };
            inner::risky(42)
        }
    "#,
    "/test/inner.gxi" => r#"
        type ErrPayload;
        val risky: fn(x: i64) -> i64 throws Error<ErrChain<`CustomError(ErrPayload)>>
    "#,
    "/test/inner.gx" => r#"
        type ErrPayload = Abstract<{ code: i64, msg: string }>;
        let risky = |x: i64| -> i64 x
    "#; FuseExpect::Jit);

// An abstract type with a throwing function whose `?` raises through the
// interface's `throws` to the caller's catch.
run!(
    abstract_type_with_throws_clause,
    is(Value::I64(-1)),
    "/test.gx" => r#"
        mod inner;
        let caught = 0;
        let r = {
            catch(e) caught <- select e.0.error { `ArrayIndexError(_) => -1 };
            inner::get_value(inner::make(1))
        };
        let result = filter(caught, |c| c != 0)
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get_value: fn(t: T) -> i64 throws Error<ErrChain<`ArrayIndexError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<{ value: i64 }>;
        let make = |x: i64| -> T T({ value: x });
        let get_value = |t: T| -> i64 {
            let a = [t.0.value + 41];
            a[t.0.value]?
        }
    "#; FuseExpect::Jit);

// Nominal abstract types: the tag at runtime.

// A hidden abstract's constructor, payload and pattern are usable
// only where the definition is visible: the caller gets a compile
// error for all three.
run!(
    abstract_construct_outside_refused,
    refused("the definition of inner::T is not visible here, so it cannot be constructed"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get(inner::T(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let get = |t: T| -> i64 t.0
    "#
; FuseExpect::None);

run!(
    abstract_payload_outside_refused,
    refused("so its payload cannot be read"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::make(42).0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let make = |x: i64| -> T T(x)
    "#
; FuseExpect::None);

run!(
    abstract_pattern_outside_refused,
    refused("its values cannot be destructured"),
    "/test.gx" => r#"
        mod inner;
        let result = select inner::make(42) { inner::T(x) => x }
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let make = |x: i64| -> T T(x)
    "#
; FuseExpect::None);

// A type hidden by the interface must be Abstract<..> (or Rust-backed):
// hiding a transparent alias is the two-view case itself.
run!(
    abstract_hidden_alias_refused,
    refused("T is hidden by the interface, so its definition must be"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get(inner::make(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = i64;
        let make = |x: i64| -> T x;
        let get = |t: T| -> i64 t
    "#
; FuseExpect::None);

// A PUBLIC newtype: the interface exports the body, so anyone can
// construct and read it — nominal without being hidden.
run!(
    abstract_public_newtype,
    is(Value::I64(43)),
    "/test.gx" => r#"
        mod inner;
        let t = inner::T(42);
        let result = inner::get(t) + t.0 - 41
    "#,
    "/test/inner.gxi" => r#"
        type T = Abstract<i64>;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        let get = |t: T| -> i64 t.0
    "#);

// `let T(x) = v` destructures irrefutably; `select` can nest the
// payload pattern.
run!(
    abstract_pattern_let,
    is(Value::I64(42)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get(inner::make(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val get: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<{ value: i64 }>;
        let make = |x: i64| -> T T({ value: x });
        let get = |t: T| -> i64 { let T({ value }) = t; value }
    "#);

run!(
    abstract_pattern_select,
    is(Value::I64(1)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::sign(inner::make(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(x: i64) -> T;
        val sign: fn(t: T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let make = |x: i64| -> T T(x);
        let sign = |t: T| -> i64 select t {
            T(x) if x > 0 => 1,
            T(0) => 0,
            T(_) => -1
        }
    "#);

// Two boxed types with the SAME representation are told apart by
// their tags: `T as _` is a runtime type test, also on a union.
run!(
    abstract_type_test_union,
    is(Value::I64(2)),
    "/test.gx" => r#"
        mod a;
        mod b;
        let pick = true;
        let v: [a::A, b::B] = select pick {
            true => b::make(1),
            false => a::make(1)
        };
        let result = select v {
            a::A as _ => 1,
            b::B as _ => 2
        }
    "#,
    "/test/a.gxi" => r#"
        type A;
        val make: fn(x: i64) -> A
    "#,
    "/test/a.gx" => r#"
        type A = Abstract<i64>;
        let make = |x: i64| -> A A(x)
    "#,
    "/test/b.gxi" => r#"
        type B;
        val make: fn(x: i64) -> B
    "#,
    "/test/b.gx" => r#"
        type B = Abstract<i64>;
        let make = |x: i64| -> B B(x)
    "#
);

// Equality and printing go through the box: same tag + same payload.
run!(
    abstract_equality_and_print,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "true false T(5)"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::show()
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val show: fn() -> string
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>;
        let show = || -> string "[T(5) == T(5)] [T(5) == T(6)] [T(5)]"
    "#
; FuseExpect::Jit);

// A destructuring `let` implements every `val` it binds.
run!(
    interface_val_by_destructuring,
    is(Value::I64(3)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::a + inner::b
    "#,
    "/test/inner.gxi" => r#"
        val a: i64;
        val b: i64
    "#,
    "/test/inner.gx" => r#"
        let (a, b) = (1, 2)
    "#
; FuseExpect::Jit);

// An interface-only type declared after a `val` the implementation binds
// by destructuring lands after that `let`, where the body can name it.
// CR claude for claude: [test-gap/medium] This fixture cannot fail for the anchoring
// its comment names. A gxi-only `type` needs no value and goes ahead of the whole body
// (`resolver::needs_no_value`): T is in force above `let (a, c)` (a cast to T there
// checks), and `let b: T` reads it wherever it lands. What places a gxi-only `mod` or
// `use` after the val it follows is the destructuring anchor (`Anchor::of_expr`
// collecting a pattern's names), and nothing pins it. Give the fixture gxi `val a: i64;
// mod l; val w: i64; val c: i64` over `let (a, c) = (1, 1); let w = l::z + 1`; it is
// refused "l::z not defined" when the anchor is missed. Fix the comment to match.
// probe: design/review-2026-11-16/repro/tests-interfaces-image-type-not-anchored.sh
// (tests-interfaces-image-03)
run!(
    interface_type_anchored_after_destructuring,
    is(Value::I64(3)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::a + inner::c + inner::b
    "#,
    "/test/inner.gxi" => r#"
        val a: i64;
        type T = i64;
        val b: T;
        val c: i64
    "#,
    "/test/inner.gx" => r#"
        let (a, c) = (1, 1);
        let b: T = 1
    "#
; FuseExpect::Jit);

// A binding behind an interface goes bottom through it: a reader that
// arrives after it went bottom sees bottom, not the value before.
run!(
    interface_val_carries_bottom,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "bottom"),
    "/test.gx" => r#"
        mod inner;
        let step = 0;
        select step { n if n < 5 => step <- n + 1, _ => never() };
        inner::k <- step ~ step;
        let seen = select step { 3 => inner::x, _ => never() };
        let result = any(seen ~ "stale", select step { 4 => "bottom", _ => never() })
    "#,
    "/test/inner.gxi" => r#"
        val k: i64;
        val x: i64
    "#,
    "/test/inner.gx" => r#"
        let k = 0;
        let x = select k { 0 => 1, _ => never() }
    "#
; FuseExpect::Jit);

// An error in an interface item carries the item's position and file.
#[tokio::test(flavor = "current_thread")]
async fn interface_error_is_placed() -> Result<()> {
    use graphix_compiler::expr::{ParserContext, Source};
    let e = graphix_package_core::testing::compile_error(
        &[
            ("/test.gx", "mod inner;\nlet result = 0"),
            ("/test/inner.gxi", "val x: i64;\nimpl Nope for i64"),
            ("/test/inner.gx", "let x = 1"),
        ],
        "{ mod test; test::result }",
        crate::TEST_REGISTER,
    )
    .await?;
    let pc = e.downcast_ref::<ParserContext>().expect("a placed error");
    assert_eq!(pc.pos.line, 2, "{e:#}");
    assert!(matches!(&pc.ori.source, Source::Vfs(n) if n == "/test/inner"), "{e:#}");
    Ok(())
}

// A trait the interface declares is the implementation's already: a
// definition of it there is refused, identical or not.
run!(
    interface_trait_redefined_refused,
    refused("trait Show is declared by its interface; delete this definition"),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        trait Show { val show: fn(self) -> string }
    "#,
    "/test/inner.gx" => r#"
        trait Show { val show: fn(self) -> string }
    "#
; FuseExpect::None);

// An implementation's type may be a constructor application the
// signature spells out: `Collection::map` returns `self<'b>`, an array.
run!(
    signature_holds_a_filled_application,
    is(Value::I64(2)),
    "/test.gx" => r#"
        mod inner;
        let result = array::len(inner::f(1))
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: i64) -> Array<i64>;
    "#,
    "/test/inner.gx" => r#"
        let f = |x: i64| Collection::map(["a", "b"], |s| x)
    "#
; FuseExpect::Jit);

// Two signature variables inside a type are two variables: an
// implementation that unifies them is less general than its interface.
run!(
    nested_signature_variables_stay_distinct,
    refused("val f is declared fn(x: Array<'a>, y: Array<'b>) -> Array<'a> but implemented fn(x: 'c: unbound, y: 'c: unbound) -> 'c: unbound: type mismatch 'a: unbound does not contain 'b: unbound"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f([1, 2], ["a", "b"])
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: Array<'a>, y: Array<'b>) -> Array<'a>
    "#,
    "/test/inner.gx" => r#"
        let f = |x: 'c, y: 'c| -> 'c y
    "#; FuseExpect::None);

// So is a type the interface defines.
run!(
    interface_typedef_redefined_refused,
    refused("T is defined by its interface; delete this definition"),
    "/test.gx" => r#"
        mod m;
        let result = m::x
    "#,
    "/test/m.gxi" => r#"
        type T = [i64, string];
        val x: T
    "#,
    "/test/m.gx" => r#"
        type T = [i64, string];
        let x: T = 1
    "#; FuseExpect::None);

// A union has several normal forms: an implementation that infers the
// same members in another order is the same type.
run!(
    val_union_inferred_in_another_order,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1), Value::Bool(true)]),
    "/test.gx" => r#"
        mod m;
        let result = m::x
    "#,
    "/test/m.gxi" => r#"
        val x: [(i64, bool), (string, bool), (i64, f64)]
    "#,
    "/test/m.gx" => r#"
        let c = 1;
        let x = select c { 0 => (1, 2.0), 1 => (1, true), _ => ("a", true) }
    "#; FuseExpect::Jit);

// A module never sees a static sibling's undeclared impl, whatever
// stands between them.
run!(
    undeclared_impl_hidden_apart,
    refused("type mismatch Show does not contain i64"),
    "/test.gx" => r#"
        mod t;
        mod a;
        let z = 0;
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
    "#; FuseExpect::None);

// A module with no interface hides nothing: its abstract type is a
// public newtype, constructed, read and matched from outside.
run!(
    abstract_type_public_without_interface,
    is(Value::I64(10)),
    "/test.gx" => r#"
        mod inner;
        let u = inner::T(5);
        let result = select u { inner::T(p) => p + u.0 }
    "#,
    "/test/inner.gx" => r#"
        type T = Abstract<i64>
    "#);

// The same under a parent whose interface re-exports the module.
run!(
    abstract_type_public_under_an_interface,
    is(Value::I64(5)),
    "/test.gx" => r#"
        mod outer;
        let result = outer::sub::T(5).0
    "#,
    "/test/outer.gxi" => r#"
        mod sub
    "#,
    "/test/outer.gx" => r#"
        mod sub
    "#,
    "/test/outer/sub.gx" => r#"
        type T = Abstract<i64>
    "#);

// A module whose interface writes `type T;` hides it, at any depth.
run!(
    abstract_type_hidden_by_its_interface,
    refused("the definition of outer::sub::T is not visible here"),
    "/test.gx" => r#"
        mod outer;
        let result = outer::sub::T(5).0
    "#,
    "/test/outer.gxi" => r#"
        mod sub
    "#,
    "/test/outer.gx" => r#"
        mod sub
    "#,
    "/test/outer/sub.gxi" => r#"
        type T
    "#,
    "/test/outer/sub.gx" => r#"
        type T = Abstract<i64>
    "#
; FuseExpect::None);

// A bound holding `Any` implies no predicate: a quantifier bounded by Any
// ranges over references, functions and indiscernible unions, so the
// implementation's Ordered, Discernible or Concrete must be declared.
run!(
    any_bound_does_not_imply_ordered,
    refused("the implementation requires 'a: Ordered"),
    "/test.gx" => "mod m; let result = m::lt(1, 2)",
    "/test/m.gxi" => "val lt: fn<'a: Any>(x: 'a, y: 'a) -> bool;",
    "/test/m.gx" => "let lt = 'a: Ordered |x: 'a, y: 'a| -> bool x < y"
; FuseExpect::None);

run!(
    array_any_bound_does_not_imply_discernible,
    refused("the implementation requires 'a: Discernible"),
    "/test.gx" => "mod m; let result = m::eq([1], [2])",
    "/test/m.gxi" => "val eq: fn<'a: Array<Any>>(x: 'a, y: 'a) -> bool;",
    "/test/m.gx" => "let eq = 'a: Array<Any> + Discernible |x: 'a, y: 'a| -> bool x == y"
; FuseExpect::None);

run!(
    any_bound_does_not_imply_concrete,
    refused("the implementation requires 'b: Concrete"),
    "/test.gx" => "mod m; let result = m::parse(\"1\", 0)",
    "/test/m.gxi" => "val parse: fn<'b: Any>(s: string, d: 'b) -> Result<'b, `ParseError(string)>;",
    "/test/m.gx" => "let parse = 'b: Concrete |s: string, d: 'b| -> Result<'b, `ParseError(string)> str::parse(s)"
; FuseExpect::None);

// A union of members holding variables behind two typedefs pairs with the
// implementation's however either side orders it, as the rest of the
// signature decides.
run!(
    open_union_pairs_by_the_rest_of_the_signature,
    is(Value::I64(2)),
    "/test.gx" => "mod m; let result = { let g = m::f; 2 }",
    "/test/m.gxi" => "type A<'x> = Array<'x>;\ntype B<'x> = Array<'x>;\nval f: fn(x: [A<'a>, B<'b>], y: 'a) -> 'a;",
    "/test/m.gx" => "let f = |x: [A<'b>, B<'a>], y: 'a| -> 'a y"
);

// A value that either member could take is refused at the call where no
// argument before it decides the members, and goes to the member one
// decides.
run!(
    open_union_call_refused,
    settles(),
    "/test.gx" => "mod m; let result = m::f([1], never())",
    "/test/m.gxi" => "type A<'x> = Array<'x>;\ntype B<'x> = Array<'x>;\nval f: fn(x: [A<'a>, B<'b>], y: 'a) -> 'a;",
    "/test/m.gx" => "let f = |x: [A<'b>, B<'a>], y: 'a| -> 'a y"
; FuseExpect::None);

run!(
    open_union_call_decided_by_the_argument_before,
    is(Value::I64(2)),
    "/test.gx" => "mod m; let result = m::f(2, [1])",
    "/test/m.gxi" => "type A<'x> = Array<'x>;\ntype B<'x> = Array<'x>;\nval f: fn(y: 'a, x: [A<'a>, B<'b>]) -> 'a;",
    "/test/m.gx" => "let f = |y: 'a, x: [A<'b>, B<'a>]| -> 'a y"
);

// A val's use is checked against its declaration alone, and builds an
// instance of the implementation: one whose own signature would have that
// instance choose between two undecided members is refused as the val is
// checked; one whose variables stand at the declaration's places is not.
run!(
    val_implementation_at_a_union_of_two_variables_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "type E<'p, 'q> = ['p, 'q];\nval f: fn(x: 'a) -> i64;",
    "/test/m.gx" => "let f = |x: E<'a, 'b>| -> i64 0"
; FuseExpect::Jit);

run!(
    val_in_data_implementation_at_a_union_of_two_variables_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::s.g([1])",
    "/test/m.gxi" => "type E<'p, 'q> = ['p, 'q];\nval s: {g: fn(x: Array<'a>) -> i64};",
    "/test/m.gx" => "let s = {g: |x: Array<E<'a, 'b>>| -> i64 0}"
; FuseExpect::None);

run!(
    val_at_one_variable_implemented_at_two,
    prints("[[], []]"),
    "/test.gx" => "mod m; let result = m::f(1, 2)",
    "/test/m.gxi" => "type E<'p, 'q> = ['p, 'q];\nval f: fn(x: E<'a, 'a>, y: 'a) -> (Array<'a>, Array<'a>);",
    "/test/m.gx" => "let f = |x: E<'a, 'b>, y: 'b| -> (Array<'a>, Array<'b>) ([], [])"
);

// No pairing of the union fits the rest of the signature: refused.
run!(
    open_union_without_a_pairing_is_refused,
    refused("val f is declared fn(x: [A<'a>, B<'b>], y: 'a, z: 'a) -> 'a but implemented fn(x: [A<'b: unbound>, B<'a: unbound>], y: 'a: unbound, z: 'b: unbound) -> 'a: unbound: type mismatch [A<'a: unbound>, B<'a: unbound>] does not contain Array<'b: unbound>"),
    "/test.gx" => "mod m; let result = m::f([1], 2, 3)",
    "/test/m.gxi" => "type A<'x> = Array<'x>;\ntype B<'x> = Array<'x>;\nval f: fn(x: [A<'a>, B<'b>], y: 'a, z: 'a) -> 'a;",
    "/test/m.gx" => "let f = |x: [A<'b>, B<'a>], y: 'a, z: 'b| -> 'a y"
; FuseExpect::None);

run!(
    val_union_of_written_types,
    is(Value::I64(2)),
    "/test.gx" => "mod m; let result = m::f(1, 2)",
    "/test/m.gxi" => "val f: fn(x: [i64, string], y: i64) -> i64;",
    "/test/m.gx" => "let f = |x: [i64, string], y: i64| -> i64 y"
);

// A ground union matches its signature as a set, whatever normal form
// either side holds: primitives folded into one, or a binding never
// normalized ([i64, i64]).
run!(
    ground_union_matches_a_folded_primitive_union,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s"),
    "/test.gx" => "mod m; let result = m::g(\"s\")",
    "/test/m.gxi" => "type T = [i64, string];\nval g: fn(x: T) -> [T, null]",
    "/test/m.gx" => "let g = |x: [string, i64]| -> [null, i64, string] x"
);

run!(
    ground_union_matches_an_unnormalized_binding,
    is(Value::I64(1)),
    "/test.gx" => r#"
        let b = false;
        let y = never();
        let x = select b { true => y, false => 1 };
        y <- 2;
        mod m;
        let result = m::z
    "#,
    "/test/m.gxi" => "val z: i64;",
    "/test/m.gx" => "let z = super::x"
);

// An implementation more general than its signature: each parameter's
// cell maps to the signature's 'a, so their union is 'a.
run!(
    union_of_open_cells_implements_a_variable,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::pick(true, 1, 2)",
    "/test/m.gxi" => "val pick: fn<'a: Any>(b: bool, x: 'a, y: 'a) -> 'a;",
    "/test/m.gx" => "let pick = |b, x, y| select b { true => x, false => y }"
);

// ... but not when the cells map to two variables.
run!(
    union_of_open_cells_does_not_implement_one_of_two,
    refused_renumbered("val pick is declared fn(b: bool, x: 'a, y: 'b) -> 'a but implemented fn(b: '_0: unbound within bool, x: '_1: unbound within '_2: unbound within '_3: unbound, y: '_4: unbound within '_2: unbound within '_3: unbound) -> '_3: unbound: type mismatch 'a: unbound does not contain 'b: unbound"),
    "/test.gx" => "mod m; let result = m::pick(true, 1, 2)",
    "/test/m.gxi" => "val pick: fn(b: bool, x: 'a, y: 'b) -> 'a;",
    "/test/m.gx" => "let pick = |b, x, y| select b { true => x, false => y }"
; FuseExpect::None);

// ... and a concrete type when each cell maps to it.
run!(
    union_of_open_cells_implements_a_concrete_type,
    is(Value::I64(2)),
    "/test.gx" => "mod m; let result = m::pick(false, 1, 2)",
    "/test/m.gxi" => "val pick: fn(b: bool, x: i64, y: i64) -> i64;",
    "/test/m.gx" => "let pick = |b: bool, x, y| select b { true => x, false => y }"
);

run!(
    union_of_open_cells_does_not_implement_one_of_two_types,
    refused_renumbered("val pick is declared fn(b: bool, x: i64, y: string) -> i64 but implemented fn(b: bool, x: '_0: unbound within '_1: unbound within '_2: unbound, y: '_3: unbound within '_1: unbound within '_2: unbound) -> '_2: unbound: type mismatch i64 does not contain string"),
    "/test.gx" => "mod m; let result = m::pick(false, 1, \"s\")",
    "/test/m.gxi" => "val pick: fn(b: bool, x: i64, y: string) -> i64;",
    "/test/m.gx" => "let pick = |b: bool, x, y| select b { true => x, false => y }"
; FuseExpect::None);

// A union whose open cells sit inside its members pairs them in whatever
// order the rest of the signature accepts: the implementation's union was
// ordered while its cells were open.
run!(
    union_with_nested_cells_pairs_in_any_order,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(5)]),
    "/test.gx" => "mod m; let result = m::f(true, 5)",
    "/test/m.gxi" => "val f: fn(b: bool, x: 'a) -> [Array<i64>, Array<'a>];",
    "/test/m.gx" => "let f = |b: bool, x| select b { true => [x], false => [3] }"
);

// Unions that share no cell pair apart: one that cannot pair is refused
// without trying every pairing of the others (typedefs hide the members'
// one constructor from the written-form rule).
run!(
    unpairable_union_refused_without_retrying_the_others,
    refused("val f is declared fn(x0: [string, A<'a0>, B<'a1>, C<'a2>, D<'a3>, E<'a4>], x1: [i64, A<'b0>, B<'b1>, C<'b2>, D<'b3>, E<'b4>], x2: [i64, A<'c0>, B<'c1>, C<'c2>, D<'c3>, E<'c4>], x3: [i64, A<'d0>, B<'d1>, C<'d2>, D<'d3>, E<'d4>]) -> i64 but implemented fn(x0: [f64, A<'a0: unbound>, B<'a1: unbound>, C<'a2: unbound>, D<'a3: unbound>, E<'a4: unbound>], x1: [i64, A<'b0: unbound>, B<'b1: unbound>, C<'b2: unbound>, D<'b3: unbound>, E<'b4: unbound>], x2: [i64, A<'c0: unbound>, B<'c1: unbound>, C<'c2: unbound>, D<'c3: unbound>, E<'c4: unbound>], x3: [i64, A<'d0: unbound>, B<'d1: unbound>, C<'d2: unbound>, D<'d3: unbound>, E<'d4: unbound>]) -> i64: type mismatch [f64, A<'a0: unbound>, B<'a1: unbound>, C<'a2: unbound>, D<'a3: unbound>, E<'a4: unbound>] does not contain string"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "type A<'x> = Array<'x>;\ntype B<'x> = Array<'x>;\ntype C<'x> = Array<'x>;\ntype D<'x> = Array<'x>;\ntype E<'x> = Array<'x>;\nval f: fn(x0: [A<'a0>, B<'a1>, C<'a2>, D<'a3>, E<'a4>, string], x1: [A<'b0>, B<'b1>, C<'b2>, D<'b3>, E<'b4>, i64], x2: [A<'c0>, B<'c1>, C<'c2>, D<'c3>, E<'c4>, i64], x3: [A<'d0>, B<'d1>, C<'d2>, D<'d3>, E<'d4>, i64]) -> i64;",
    "/test/m.gx" => "let f = |x0: [A<'a0>, B<'a1>, C<'a2>, D<'a3>, E<'a4>, f64], x1: [A<'b0>, B<'b1>, C<'b2>, D<'b3>, E<'b4>, i64], x2: [A<'c0>, B<'c1>, C<'c2>, D<'c3>, E<'c4>, i64], x3: [A<'d0>, B<'d1>, C<'d2>, D<'d3>, E<'d4>, i64]| -> i64 0"
; FuseExpect::None);

// A val holds its implementation where the implementation fits it: a value
// or result narrower than declared, a parameter wider, fewer throws. Every
// module sees the declared type.
run!(
    a_val_wider_than_its_value_holds,
    prints("i64:4"),
    "/test.gx" => "mod m; let result = m::x$ + 1",
    "/test/m.gxi" => "val x: [i64, null];",
    "/test/m.gx" => "let x = 3"
; FuseExpect::Jit);

run!(
    a_val_returning_wider_than_its_implementation_holds,
    prints("i64:42"),
    "/test.gx" => "mod m; let result = select m::f(41) { null => 0, i64 as n => n }",
    "/test/m.gxi" => "val f: fn(x: i64) -> [i64, null];",
    "/test/m.gx" => "let f = |x: i64| x + 1"
; FuseExpect::Jit);

run!(
    a_val_taking_narrower_than_its_implementation_holds,
    prints("i64:42"),
    "/test.gx" => "mod m; let result = m::g(21)",
    "/test/m.gxi" => "val g: fn(x: i64) -> i64;",
    "/test/m.gx" => "let g = |x: [i64, null]| select x { null => 0, i64 as n => n * 2 }"
; FuseExpect::Jit);

run!(
    a_native_call_through_vals_wider_than_their_implementations,
    prints("i64:4"),
    "/test.gx" => "mod m; let result = #[native] (m::g(m::f(1)$) + 0)",
    "/test/m.gxi" => "val f: fn(x: i64) -> [i64, null];\nval g: fn(x: i64) -> i64;",
    "/test/m.gx" => "let f = |x: i64| x + 1;\nlet g = |x: [i64, null]| select x { null => 0, i64 as n => n * 2 }"
; FuseExpect::Jit);

run!(
    a_val_throwing_more_than_its_lambda_holds,
    prints("i64:1"),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn(x: i64) -> i64 throws `E;",
    "/test/m.gx" => "let f = |x: i64| x"
; FuseExpect::Jit);

run!(
    a_val_throwing_more_than_its_annotated_binding_holds,
    prints("i64:1"),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn(x: i64) -> i64 throws `E;",
    "/test/m.gx" => "let f: fn(x: i64) -> i64 = |x| x"
; FuseExpect::Jit);

// A mismatch names the implemented type, not its cells.
run!(
    implemented_type_prints_resolved,
    refused("but implemented fn(x: f64) -> f64"),
    "/test.gx" => "mod m; let result = m::add(1)",
    "/test/m.gxi" => "val add: fn(x: i64) -> i64;",
    "/test/m.gx" => "let add = |x| x + 1."
; FuseExpect::None);

// A signature and its implementation each name their own variables: a
// name both write prints alike on both sides, the implementation's cell
// under the name the refusal's `implemented` gives it.
run!(
    a_signature_and_its_implementation_print_a_name_both_write_alike,
    refused("val f is declared fn(x: Array<'a>) -> 'a but implemented fn(x: Map<'a: unbound, i64>) -> 'a: unbound: type mismatch Map<'a: unbound, i64> does not contain Array<'a1: unbound>"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "val f: fn(x: Array<'a>) -> 'a;",
    "/test/m.gx" => "let f = |x: Map<'a, i64>| -> 'a never()"
; FuseExpect::None);

run!(
    a_signature_through_a_type_reference_prints_a_name_both_write_alike,
    refused("val f is declared fn(x: T<'a>) -> 'a but implemented fn(x: { w: 'a: unbound }) -> 'a: unbound: type mismatch { w: 'a } does not contain T<'a>"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "type T<'a> = { v: 'a };\nval f: fn(x: T<'a>) -> 'a;",
    "/test/m.gx" => "let f = |x: { w: 'a }| -> 'a never()"
; FuseExpect::None);

// An argument whose kinds differ is named in Graphix syntax.
run!(
    an_implementation_default_for_a_required_label_holds,
    prints("i64:1"),
    "/test.gx" => "mod m; let result = m::f(#a: 1)",
    "/test/m.gxi" => "val f: fn(#a: i64) -> i64;",
    "/test/m.gx" => "let f = |#a = 0| -> i64 a"
; FuseExpect::Jit);

// nothing to pass.
run!(
    arg_default_the_signature_gives_the_implementation_requires_refused,
    refused("val f is declared fn(?#a: i64) -> i64 but implemented fn(#a: i64) -> i64"),
    "/test.gx" => "mod m; let result = m::f()",
    "/test/m.gxi" => "val f: fn(?#a: i64) -> i64;",
    "/test/m.gx" => "let f = |#a: i64| -> i64 a"
; FuseExpect::None);

// A variable named only inside another's bound is matched with the
// signature's: its bound must be declared, and when it is, it holds at
// the call.
run!(
    a_bound_inside_a_bound_must_be_declared,
    refused("val f is declared fn<'a: Array<'b>>(x: 'a) -> 'a but implemented fn<'a: Array<'b: unbound within Int>, 'b: Int>(x: 'a: unbound within Array<'b: unbound within Int>) -> 'a: unbound within Array<'b: unbound within Int>: type mismatch Array<'b: unbound within Int> does not contain 'a: unbound within Array<'b1: unbound>"),
    "/test.gx" => "mod m; let result = m::f([1])",
    "/test/m.gxi" => "val f: fn<'a: Array<'b>>(x: 'a) -> 'a;",
    "/test/m.gx" => "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x"
; FuseExpect::None);

run!(
    a_declared_bound_inside_a_bound_holds,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1)]),
    "/test.gx" => "mod m; let result = m::f([1])",
    "/test/m.gxi" => "val f: fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;",
    "/test/m.gx" => "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x"
);

run!(
    a_declared_bound_inside_a_bound_refuses_at_the_call,
    refused("type mismatch Int does not contain f64"),
    "/test.gx" => "mod m; let result = m::f([1.5])",
    "/test/m.gxi" => "val f: fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;",
    "/test/m.gx" => "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x"
; FuseExpect::None);

// A quantifier named only inside the bound of a bound is in the declared
// list a mismatch prints.
run!(
    a_chain_of_bounds_prints_every_quantifier,
    refused("val f is declared fn<'a: [i64, string], 'b: ['c, `X], 'c: [null, 'a]>"),
    "/test.gx" => "mod m; let result = m::f(`X)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: ['a, null], 'b: ['c, `X]>(x: 'b) -> [`X, null, i64];",
    "/test/m.gx" => "let f = |x| x"
; FuseExpect::None);

// A variable named only inside a union bound, at the end of a chain of
// bounds, or met through a bound later in the signature is judged as one
// at a position is.
run!(
    a_bound_inside_a_union_bound_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'a: [i64, f64], 'b: ['a: unbound within [i64, f64], `X]>(x: 'b: unbound within ['a: unbound within [i64, f64], `X]) -> 'b: unbound within ['a: unbound within [i64, f64], `X]: type mismatch [i64, f64] does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'a: [i64, f64], 'b: ['a, `X] |x: 'b| -> 'b x"
; FuseExpect::None);

run!(
    a_chain_of_union_bounds_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['c, `X], 'c: [null, 'a]>(x: 'b) -> 'b but implemented fn<'p: [i64, f64], 'q: ['r: unbound within [null, 'p: unbound within [i64, f64]], `X], 'r: [null, 'p: unbound within [i64, f64]]>(x: 'q: unbound within ['r: unbound within [null, 'p: unbound within [i64, f64]], `X]) -> 'q: unbound within ['r: unbound within [null, 'p: unbound within [i64, f64]], `X]: type mismatch [i64, f64] does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: ['a, null], 'b: ['c, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'r: ['p, null], 'q: ['r, `X] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    a_bound_met_through_a_later_bound_must_be_declared,
    refused("val f is declared fn<'a: Array<'z>, 'b: Array<'a>, 'z: [i64, string]>(x: 'b) -> 'b but implemented fn<'p: Array<'r: unbound within [i64, f64]>, 'q: Array<'p: unbound within Array<'r: unbound within [i64, f64]>>, 'r: [i64, f64]>(x: 'q: unbound within Array<'p: unbound within Array<'r: unbound within [i64, f64]>>) -> 'q: unbound within Array<'p: unbound within Array<'r: unbound within [i64, f64]>>: type mismatch Array<'p: unbound within Array<'r: unbound within [i64, f64]>> does not contain 'b: unbound within Array<'a: unbound within Array<'z: unbound within [i64, string]>>"),
    "/test.gx" => "mod m; let result = m::f([[\"s\"]])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'b: Array<'a>, 'z: [i64, string]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Array<'r>, 'q: Array<'p>, 'r: [i64, f64] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    a_declared_chain_of_union_bounds_holds,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: ['a, null], 'b: ['c, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'r: ['p, null], 'q: ['r, `X] |x: 'q| -> 'q x"
);

run!(
    a_wider_bound_inside_a_union_bound_holds,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string, f64], 'q: ['p, `X] |x: 'q| -> 'q x"
);

run!(
    a_wider_bound_met_through_a_later_bound_holds,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 1),
    "/test.gx" => "mod m; let result = m::f([[\"s\"]])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'b: Array<'a>, 'z: [i64, string]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Array<'r>, 'q: Array<'p>, 'r: [i64, string, f64] |x: 'q| -> 'q x"
);

// A bound pairs the implementation's variables with the signature's
// through the pairing alone: the names each side gives them say nothing.
run!(
    a_chain_of_bounds_with_the_names_moved_round_holds,
    prints("[[i64:1]]"),
    "/test.gx" => "mod m; let result = m::f([[1]])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'b: Array<'a>, 'z: i64>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'a: Array<'q>, 'q: Array<'r>, 'r: i64 |x: 'a| -> 'a x"
);

run!(
    a_union_bound_with_the_names_moved_round_holds,
    is(Value::String("P".into())),
    "/test.gx" => "mod m; let result = m::f(`P)",
    "/test/m.gxi" => "val f: fn<'a: [`P, `Q], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'b: [`P, `Q], 'a: ['b, `X] |x: 'a| -> 'a x"
);

run!(
    a_chain_of_bounds_with_the_names_moved_round_must_be_declared,
    refused("val f is declared fn<'a: Array<'z>, 'b: Array<'a>, 'z: [i64, string]>(x: 'b) -> 'b but implemented fn<'a: Array<'q: unbound within Array<'r: unbound within [i64, f64]>>, 'q: Array<'r: unbound within [i64, f64]>, 'r: [i64, f64]>(x: 'a: unbound within Array<'q: unbound within Array<'r: unbound within [i64, f64]>>) -> 'a: unbound within Array<'q: unbound within Array<'r: unbound within [i64, f64]>>: type mismatch Array<'q: unbound within Array<'r: unbound within [i64, f64]>> does not contain 'b: unbound within Array<'a: unbound within Array<'z: unbound within [i64, string]>>"),
    "/test.gx" => "mod m; let result = m::f([[\"s\"]])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'b: Array<'a>, 'z: [i64, string]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'a: Array<'q>, 'q: Array<'r>, 'r: [i64, f64] |x: 'a| -> 'a x"
; FuseExpect::None);

run!(
    a_union_bound_with_the_names_moved_round_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'a: ['b: unbound within [i64, f64], `X], 'b: [i64, f64]>(x: 'a: unbound within ['b: unbound within [i64, f64], `X]) -> 'a: unbound within ['b: unbound within [i64, f64], `X]: type mismatch [i64, f64] does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'b: [i64, f64], 'a: ['b, `X] |x: 'a| -> 'a x"
; FuseExpect::None);

// An implementation variable a bound leaves unpaired stands for a
// signature variable only where its own bound admits every type the
// signature's admits: the signature's bounds are fixed, its callers read
// them.
run!(
    an_unpaired_variable_in_a_wider_union_bound_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'p: [i64, f64], 'q: ['p: unbound within [i64, f64], `X, `Y]>(x: 'q: unbound within ['p: unbound within [i64, f64], `X, `Y]) -> 'q: unbound within ['p: unbound within [i64, f64], `X, `Y]: type mismatch [i64, f64] does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_in_a_wider_union_bound_holds,
    is(Value::String("s".into())),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_variable_under_a_constructor_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b but implemented fn<'p: [i64, f64], 'q: [null, Array<'p: unbound within [i64, f64]>]>(x: 'q: unbound within [null, Array<'p: unbound within [i64, f64]>]) -> 'q: unbound within [null, Array<'p: unbound within [i64, f64]>]: type mismatch [null, Array<'p: unbound within [i64, f64]>] does not contain 'b: unbound within Array<'a: unbound within [i64, string]>"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'q: [Array<'p>, null] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_under_a_constructor_holds,
    prints("[\"s\"]"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [Array<'p>, null] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_variable_in_a_tuple_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: (i64, 'a)>(x: 'b) -> 'b but implemented fn<'p: [i64, f64], 'q: [null, (i64, 'p: unbound within [i64, f64])]>(x: 'q: unbound within [null, (i64, 'p: unbound within [i64, f64])]) -> 'q: unbound within [null, (i64, 'p: unbound within [i64, f64])]: type mismatch [null, (i64, 'p: unbound within [i64, f64])] does not contain 'b: unbound within (i64, 'a: unbound within [i64, string])"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: (i64, 'a)>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'q: [(i64, 'p), null] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_against_a_position_must_be_admitted,
    refused("val f is declared fn<'a: [i64, string]>(x: Array<'a>) -> Array<'a> but implemented fn<'p: [i64, f64], 'q: [null, Array<'p: unbound within [i64, f64]>]>(x: 'q: unbound within [null, Array<'p: unbound within [i64, f64]>]) -> 'q: unbound within [null, Array<'p: unbound within [i64, f64]>]: type mismatch [i64, f64] does not contain 'a"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: Array<'a>) -> Array<'a>;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'q: [Array<'p>, null] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_against_a_position_holds,
    prints("[\"s\"]"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: Array<'a>) -> Array<'a>;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [Array<'p>, null] |x: 'q| -> 'q x"
);

// A union bound of the signature's length whose members cannot pair one
// to one pairs nothing, as a bound of another shape: the implementation's
// may be the wider member by member, and the bounds' judgment holds it.
run!(
    a_union_bound_wider_in_a_member_holds,
    is(Value::String("s".into())),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: [`A(i64), 'a]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [`A(Number), 'p] |x: 'q| -> 'q x"
);

run!(
    a_union_bound_wider_inside_a_members_payload_holds,
    prints("[\"A\", i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = m::f(`A(1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: [`A(i64, 'a), null]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [`A(Number, 'p), null] |x: 'q| -> 'q x"
);

run!(
    a_variable_in_two_union_bounds_that_cannot_pair_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f(\"s\", 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: [`A(i64), 'a], 'd: [`A(i64), 'c]>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [`A(Number), 'p], 'r: [`A(Number), 'p] |x: 'q, y: 'r| -> i64 0"
);

run!(
    a_union_bound_wider_in_a_member_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `A(i64)]>(x: 'b) -> 'b but implemented fn<'p: [i64, f64], 'q: ['p: unbound within [i64, f64], `A(Number)]>(x: 'q: unbound within ['p: unbound within [i64, f64], `A(Number)]) -> 'q: unbound within ['p: unbound within [i64, f64], `A(Number)]: type mismatch [i64, f64] does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: [`A(i64), 'a]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, f64], 'q: [`A(Number), 'p] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    a_union_bound_narrower_in_a_member_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `A(i64)]>(x: 'b) -> 'b but implemented fn<'p: [i64, string], 'q: ['p: unbound within [i64, string], `A(string)]>(x: 'q: unbound within ['p: unbound within [i64, string], `A(string)]) -> 'q: unbound within ['p: unbound within [i64, string], `A(string)]: type mismatch `A(string) does not contain `A(i64)"),
    "/test.gx" => "mod m; let result = m::f(`A(1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: [`A(i64), 'a]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [`A(string), 'p] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    a_variable_in_two_union_bounds_that_cannot_pair_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: ['a, `A(i64)], 'c: [i64, string] + Singleton, 'd: ['c, `A(i64)]>(x: 'b, y: 'd) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: ['p: unbound within [i64, string] & Singleton, `A(Number)], 'r: ['p: unbound within [i64, string] & Singleton, `A(Number)]>(x: 'q: unbound within ['p: unbound within [i64, string] & Singleton, `A(Number)], y: 'r: unbound within ['p: unbound within [i64, string] & Singleton, `A(Number)]) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f(\"s\", 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: [`A(i64), 'a], 'd: [`A(i64), 'c]>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: [`A(Number), 'p], 'r: [`A(Number), 'p] |x: 'q, y: 'r| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_twice_in_a_union_bound_that_cannot_pair_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: [('a, 'c), `A(i64)], 'c: [i64, string] + Singleton>(x: 'b) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: [('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton), `A(Number)]>(x: 'q: unbound within [('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton), `A(Number)]) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((\"s\", 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: [`A(i64), ('a, 'c)]>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: [`A(Number), ('p, 'p)] |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_predicate_variable_in_a_union_bound_that_cannot_pair_must_be_declared,
    refused("val f is declared fn<'a: Number, 'b: ['a, `A(i64)]>(x: 'b) -> i64 but implemented fn<'p: OneNumber, 'q: ['p: unbound within OneNumber, `A(string)]>(x: 'q: unbound within ['p: unbound within OneNumber, `A(string)]) -> i64: the implementation requires 'b: OneNumber, which the signature does not declare"),
    "/test.gx" => "mod m; let result = { let v: [i64, f64] = 1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number, 'b: [`A(i64), 'a]>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: [`A(string), 'p] |x: 'q| -> i64 0"
; FuseExpect::None);

// The variable inside a signature's bound is as much the caller's as one
// at a position, whatever the implementation names its own.
run!(
    a_variable_inside_a_positions_bound_must_be_admitted,
    refused("val f is declared fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64 but implemented fn<'c: Array<'q: unbound within Number>, 'q: Number>(x: 'c: unbound within Array<'q: unbound within Number>) -> i64: type mismatch Number does not contain 'b"),
    "/test.gx" => "mod m; let result = m::f([[1]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Number |x: 'c| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_inside_a_positions_bound_of_a_bound_must_be_admitted,
    refused("val f is declared fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64 but implemented fn<'c: Array<'q: unbound within Array<'r: unbound within i64>>, 'q: Array<'r: unbound within i64>, 'r: i64>(x: 'c: unbound within Array<'q: unbound within Array<'r: unbound within i64>>) -> i64: type mismatch Array<'r: unbound within i64> does not contain 'b: unbound within Array<'z: unbound within Number>"),
    "/test.gx" => "mod m; let result = m::f([[1.0]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: i64 |x: 'c| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_inside_a_positions_bound_of_a_bound_named_as_the_signature_must_be_admitted,
    refused("val f is declared fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64 but implemented fn<'b: Array<'q: unbound within Array<'r: unbound within i64>>, 'q: Array<'r: unbound within i64>, 'r: i64>(x: 'b: unbound within Array<'q: unbound within Array<'r: unbound within i64>>) -> i64: type mismatch Array<'r: unbound within i64> does not contain 'b: unbound within Array<'z: unbound within Number>"),
    "/test.gx" => "mod m; let result = m::f([[1.0]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'b: Array<'q>, 'q: Array<'r>, 'r: i64 |x: 'b| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_inside_a_positions_bound_of_a_bound_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f([[1]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: Number |x: 'c| -> i64 0"
);

run!(
    a_variable_three_bounds_inside_a_position_must_be_admitted,
    refused("val f is declared fn<'b: Array<'z>, 'y: Number, 'z: Array<'y>>(x: Array<'b>) -> i64 but implemented fn<'c: Array<'q: unbound within Array<'r: unbound within Array<'s: unbound within i64>>>, 'q: Array<'r: unbound within Array<'s: unbound within i64>>, 'r: Array<'s: unbound within i64>, 's: i64>(x: 'c: unbound within Array<'q: unbound within Array<'r: unbound within Array<'s: unbound within i64>>>) -> i64: type mismatch Array<'r: unbound within Array<'s: unbound within i64>> does not contain 'b: unbound within Array<'z: unbound within Array<'y: unbound within Number>>"),
    "/test.gx" => "mod m; let result = m::f([[[1.0]]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Array<'y>, 'y: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: Array<'s>, 's: i64 |x: 'c| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_three_bounds_inside_a_position_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f([[[1.0]]])",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Array<'y>, 'y: Number>(x: Array<'b>) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: Array<'s>, 's: Number |x: 'c| -> i64 0"
);

run!(
    a_variable_inside_a_bounds_variables_bound_must_be_declared,
    refused("val f is declared fn<'a: Array<'z>, 'b: ['a, `X], 'z: [i64, string]>(x: 'b) -> 'b but implemented fn<'p: Array<'r: unbound within [i64, f64]>, 'q: ['p: unbound within Array<'r: unbound within [i64, f64]>, `X, `Y], 'r: [i64, f64]>(x: 'q: unbound within ['p: unbound within Array<'r: unbound within [i64, f64]>, `X, `Y]) -> 'q: unbound within ['p: unbound within Array<'r: unbound within [i64, f64]>, `X, `Y]: type mismatch Array<'r: unbound within [i64, f64]> does not contain 'b: unbound within ['a: unbound within Array<'z: unbound within [i64, string]>, `X]"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'z: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Array<'r>, 'r: [i64, f64], 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    a_variable_inside_a_bounds_variables_bound_holds,
    prints("[\"s\"]"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: Array<'z>, 'z: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Array<'r>, 'r: [i64, string], 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
);

// An implementation variable paired at one position is that signature
// type wherever the implementation's bounds name it.
run!(
    a_paired_variable_in_a_bound_holds_both_signature_variables,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f([1], 1.5)",
    "/test/m.gxi" => "val f: fn<'z: Number, 'w: Number>(x: Array<'z>, y: 'w) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'r>, 'r: Number |x: 'c, y: 'r| -> i64 0"
; FuseExpect::Jit);

// the implementation's one variable that pairs them would hold both.
run!(
    a_paired_variable_in_a_bound_under_singleton_refused,
    refused("cannot compute with ['z, 'w]: both operands must be one numeric type"),
    "/test.gx" => "mod m; let result = m::f([1], 1.5)",
    "/test/m.gxi" => "val f: fn<'z: Number + Singleton, 'w: Number + Singleton>(x: Array<'z>, y: 'w) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'r>, 'r: Number + Singleton |x: 'c, y: 'r| -> i64 0"
; FuseExpect::None);

run!(
    a_paired_variable_in_an_unpaired_variables_bound_holds_both_signature_variables,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f([[1]], 1.5)",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number, 'w: Number>(x: Array<'b>, y: 'w) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: Number |x: 'c, y: 'r| -> i64 0"
; FuseExpect::Jit);

// a_paired_variable_in_an_unpaired_variables_bound_holds_both_signature_variables
run!(
    a_paired_variable_in_an_unpaired_variables_bound_under_singleton_refused,
    refused("cannot compute with ['z, 'w]: both operands must be one numeric type"),
    "/test.gx" => "mod m; let result = m::f([[1]], 1.5)",
    "/test/m.gxi" => "val f: fn<'b: Array<'z>, 'z: Number + Singleton, 'w: Number + Singleton>(x: Array<'b>, y: 'w) -> i64;",
    "/test/m.gx" => "let f = 'c: Array<'q>, 'q: Array<'r>, 'r: Number + Singleton |x: 'c, y: 'r| -> i64 0"
; FuseExpect::None);

// A predicate on an unpaired implementation variable holds of the
// signature variable it stands for only where one of that variable's
// bounds implies it.
run!(
    an_unpaired_variable_with_a_singleton_bound_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'p: Singleton, 'q: ['p: unbound within Singleton, `X, `Y]>(x: 'q: unbound within ['p: unbound within Singleton, `X, `Y]) -> 'q: unbound within ['p: unbound within Singleton, `X, `Y]: the implementation requires 'b: Singleton, which the signature does not declare"),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Singleton, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_with_a_singleton_bound_holds,
    is(Value::String("s".into())),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Singleton, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_variable_with_a_no_error_bound_must_be_declared,
    refused("val f is declared fn<'a: [i64, Error<`E>], 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'p: NoError, 'q: ['p: unbound within NoError, `X, `Y]>(x: 'q: unbound within ['p: unbound within NoError, `X, `Y]) -> 'q: unbound within ['p: unbound within NoError, `X, `Y]: the implementation requires 'b: NoError, which the signature does not declare"),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, Error<`E>], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_with_a_no_error_bound_holds,
    is(Value::String("s".into())),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_variable_with_a_one_number_bound_must_be_declared,
    refused("val f is declared fn<'a: Number, 'b: ['a, `X]>(x: 'b) -> 'b but implemented fn<'p: OneNumber, 'q: ['p: unbound within OneNumber, `X, `Y]>(x: 'q: unbound within ['p: unbound within OneNumber, `X, `Y]) -> 'q: unbound within ['p: unbound within OneNumber, `X, `Y]: the implementation requires 'b: OneNumber, which the signature does not declare"),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn<'a: Number, 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    an_unpaired_variable_with_a_one_number_bound_holds,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber, 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ['p, `X, `Y] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_variable_with_a_singleton_bound_against_a_position_must_be_admitted,
    refused("val f is declared fn<'a: [i64, string]>(x: Array<'a>) -> i64 but implemented fn<'p: Singleton, 'q: [null, Array<'p: unbound within Singleton>]>(x: 'q: unbound within [null, Array<'p: unbound within Singleton>]) -> i64: the implementation requires 'a: Singleton, which the signature does not declare"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: Array<'a>) -> i64;",
    "/test/m.gx" => "let f = 'p: Singleton, 'q: [Array<'p>, null] |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_with_a_singleton_bound_against_a_position_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton>(x: Array<'a>) -> i64;",
    "/test/m.gx" => "let f = 'p: Singleton, 'q: [Array<'p>, null] |x: 'q| -> i64 0"
);

// A member predicate (`OneNumber`, `NoError`) on an implementation
// variable that stands for a union of the signature's holds of every
// type a caller may pick: a signature variable that is a member of the
// union is whatever its bounds admit.
run!(
    a_one_number_bound_over_a_union_holding_a_variable_must_be_declared,
    refused("val f is declared fn<'a: Number>(x: ['a, `X]) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires ['a, `X] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [i64, f64, `X] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number>(x: ['a, `X]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_one_number_bound_over_a_union_holding_a_variable_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, `X] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber>(x: ['a, `X]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

run!(
    a_no_error_bound_over_a_union_holding_a_variable_must_be_declared,
    refused("val f is declared fn<'a: [i64, Error<`E>]>(x: ['a, `X]) -> i64 but implemented fn<'q: NoError>(x: 'q: unbound within NoError) -> i64: the implementation requires ['a, `X] to be NoError, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [i64, Error<`E>, `X] = error(`E); m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: [i64, Error<`E>]>(x: ['a, `X]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_no_error_bound_over_a_union_holding_a_variable_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, `X] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: [i64, Error<`E>] + NoError>(x: ['a, `X]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
);

run!(
    a_no_error_bound_over_a_union_holding_an_unbounded_variable_must_be_declared,
    refused("val f is declared fn(x: [null, 'a]) -> i64 but implemented fn<'q: NoError>(x: 'q: unbound within NoError) -> i64: the implementation requires [null, 'a] to be NoError, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [Error<`E>, null] = error(`E); m::f(v) }",
    "/test/m.gxi" => "val f: fn(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_no_error_bound_over_a_union_holding_a_variable_no_error_can_be_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number>(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
);

// The variable's one numeric type is the caller's pick too: another
// beside it makes two, unless its bounds leave it no other.
run!(
    a_one_number_variable_beside_a_number_may_be_another,
    refused("val f is declared fn<'a: Number + OneNumber>(x: [i64, 'a]) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires [i64, 'a] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [f64, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_bounded_by_the_number_beside_it_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: i64>(x: ['a, i64, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

run!(
    a_variable_bounded_by_a_type_of_one_number_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, string, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

// The union may sit inside the signature's type, behind a typedef or
// under a bound of the implementation's.
run!(
    a_one_number_bound_over_a_union_in_a_field_must_be_declared,
    refused("val f is declared fn<'a: Number>(x: { a: [null, 'a] }) -> i64 but implemented fn<'p: OneNumber>(x: { a: 'p: unbound within OneNumber }) -> i64: the implementation requires [null, 'a] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: {a: [i64, f64, null]} = {a: i64:1}; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number>(x: {a: ['a, null]}) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber |x: {a: 'p}| -> i64 0"
; FuseExpect::None);

run!(
    a_one_number_bound_over_a_union_in_a_field_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: {a: [i64, null]} = {a: i64:1}; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber>(x: {a: ['a, null]}) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber |x: {a: 'p}| -> i64 0"
);

run!(
    a_one_number_bound_over_a_typedefs_union_must_be_declared,
    refused("val f is declared fn<'a: Number>(x: Opt<'a>) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires Opt<'a> to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [i64, f64, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "type Opt<'x> = ['x, null]; val f: fn<'a: Number>(x: Opt<'a>) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_one_number_bound_over_a_typedefs_union_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "type Opt<'x> = ['x, null]; val f: fn<'a: Number + OneNumber>(x: Opt<'a>) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

run!(
    a_one_number_bound_under_a_bound_over_a_union_must_be_declared,
    refused("val f is declared fn<'a: Number>(x: Array<['a, `X]>) -> i64 but implemented fn<'p: OneNumber, 'q: Array<'p: unbound within OneNumber>>(x: 'q: unbound within Array<'p: unbound within OneNumber>) -> i64: the implementation requires ['a, `X] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: Array<[i64, f64, `X]> = [i64:1]; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number>(x: Array<['a, `X]>) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: Array<'p> |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_one_number_bound_under_a_bound_over_a_union_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: Array<[i64, `X]> = [i64:1]; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber>(x: Array<['a, `X]>) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: Array<'p> |x: 'q| -> i64 0"
);

// A variable only a bound names that takes two members of the
// signature's bound is judged over that union the same way.
run!(
    an_unpaired_one_number_variable_over_a_variable_and_a_number_may_be_two,
    refused("val f is declared fn<'a: Number + OneNumber, 'b: [i64, 'a, `Y]>(x: 'b) -> i64 but implemented fn<'p: OneNumber, 'q: ['p: unbound within OneNumber, `Y]>(x: 'q: unbound within ['p: unbound within OneNumber, `Y]) -> i64: the implementation requires 'b: OneNumber, which the signature does not declare"),
    "/test.gx" => "mod m; let result = { let v: [f64, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber, 'b: ['a, i64, `Y]>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ['p, `Y] |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_one_number_variable_over_a_variable_and_its_number_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, `Y] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: i64, 'b: ['a, i64, `Y]>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ['p, `Y] |x: 'q| -> i64 0"
);

run!(
    an_unpaired_one_number_variable_over_a_variable_and_a_tag_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, `X] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + OneNumber, 'b: ['a, `X, `Y]>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ['p, `Y] |x: 'q| -> i64 0"
);

// A bound with a variable of its own inside implies no predicate, in a
// union as alone: the signature declares it.
run!(
    a_no_error_bound_over_a_union_holding_a_variable_bounded_by_an_open_type_must_be_declared,
    refused("val f is declared fn<'a: Array<'c>>(x: [null, 'a]) -> i64 but implemented fn<'q: NoError>(x: 'q: unbound within NoError) -> i64: the implementation requires [null, 'a] to be NoError, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [Array<i64>, null] = [1]; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Array<'c>>(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_no_error_bound_over_a_union_holding_a_variable_bounded_by_an_open_type_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [Array<i64>, null] = [1]; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Array<'c> + NoError>(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
);

// One type holds at most one numeric type: `Singleton` implies
// `OneNumber`.
run!(
    a_singleton_variable_holds_a_one_number_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f(i64:1)",
    "/test/m.gxi" => "val f: fn<'a: Number + Singleton>(x: 'a) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

run!(
    a_singleton_variable_in_a_union_holds_a_one_number_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [i64, null] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number + Singleton>(x: ['a, null]) -> i64;",
    "/test/m.gx" => "let f = 'q: [Number, null] + OneNumber |x: 'q| -> i64 0"
);

// A bound counts by its members: an unknown below a constructor is no
// number, a member that is a variable or `Any` may be any.
run!(
    a_variable_bounded_by_a_type_with_a_variable_below_is_no_number,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [Array<string>, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Array<'c>>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
);

run!(
    a_variable_bounded_by_a_union_with_any_below_counts_its_numbers,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = { let v: [Array<Any>, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: [Array<Any>, i64]>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_bounded_by_a_union_holding_a_variable_may_be_any_number,
    refused("val f is declared fn<'a: [i64, 'c]>(x: [i64, 'a]) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires [i64, 'a] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [f64, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: [i64, 'c]>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_variable_bounded_by_a_union_holding_any_may_be_any_number,
    refused("val f is declared fn<'a: [string, A]>(x: [i64, 'a]) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires [i64, 'a] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [f64, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "type A = Any; val f: fn<'a: [A, string]>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_one_number_bound_over_a_variable_beside_a_number_names_no_bound,
    refused("val f is declared fn<'a: Number>(x: [i64, 'a]) -> i64 but implemented fn<'q: OneNumber>(x: 'q: unbound within OneNumber) -> i64: the implementation requires [i64, 'a] to be OneNumber, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [f64, i64] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn<'a: Number>(x: ['a, i64]) -> i64;",
    "/test/m.gx" => "let f = 'q: OneNumber |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    a_no_error_bound_over_a_variable_beside_an_error_names_no_bound,
    refused("val f is declared fn(x: ['a, Error<`E>]) -> i64 but implemented fn<'q: NoError>(x: 'q: unbound within NoError) -> i64: the implementation requires ['a, Error<`E>] to be NoError, which the signature's bounds do not say"),
    "/test.gx" => "mod m; let result = { let v: [i64, Error<`E>] = i64:1; m::f(v) }",
    "/test/m.gxi" => "val f: fn(x: ['a, Error<`E>]) -> i64;",
    "/test/m.gx" => "let f = 'q: NoError |x: 'q| -> i64 0"
; FuseExpect::None);

// An implementation variable only bounds name is one type at an instance:
// what stands where the bound naming it is held, or, named at two places,
// what its own bounds settle it to, which must then hold both. One whose
// bounds settle nothing (none, a predicate alone, a union under
// `Singleton`) is refused at two places, unless the signature holds the
// same type exactly at both.
run!(
    an_unpaired_variable_in_two_bounds_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: (i64, 'a), 'c: [i64, string] + Singleton, 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: (Number, 'p: unbound within [i64, string] & Singleton), 'r: (Number, 'p: unbound within [i64, string] & Singleton)>(x: 'q: unbound within (Number, 'p: unbound within [i64, string] & Singleton), y: 'r: unbound within (Number, 'p: unbound within [i64, string] & Singleton)) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_in_two_bounds_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
);

run!(
    an_unpaired_variable_in_two_union_bounds_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: ['a, `X], 'c: [i64, string] + Singleton, 'd: ['c, `X]>(x: 'b, y: 'd) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: ['p: unbound within [i64, string] & Singleton, `X, `Y], 'r: ['p: unbound within [i64, string] & Singleton, `X, `Y]>(x: 'q: unbound within ['p: unbound within [i64, string] & Singleton, `X, `Y], y: 'r: unbound within ['p: unbound within [i64, string] & Singleton, `X, `Y]) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f(\"s\", 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: ['a, `X], 'd: ['c, `X]>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: ['p, `X, `Y], 'r: ['p, `X, `Y] |x: 'q, y: 'r| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_in_two_union_bounds_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f(\"s\", 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: ['a, `X], 'd: ['c, `X]>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: ['p, `X, `Y], 'r: ['p, `X, `Y] |x: 'q, y: 'r| -> i64 0"
);

run!(
    an_unpaired_variable_twice_in_one_bound_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: ('a, 'c), 'c: [i64, string] + Singleton>(x: 'b) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: [null, ('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton)]>(x: 'q: unbound within [null, ('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton)]) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((\"s\", 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: ('a, 'c)>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: [('p, 'p), null] |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_twice_in_one_bound_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((\"s\", 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: ('a, 'c)>(x: 'b) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [('p, 'p), null] |x: 'q| -> i64 0"
);

run!(
    an_unpaired_variable_twice_against_a_position_is_one_type,
    refused("val f is declared fn(x: (i64, string)) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: [null, ('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton)]>(x: 'q: unbound within [null, ('p: unbound within [i64, string] & Singleton, 'p: unbound within [i64, string] & Singleton)]) -> i64: [i64, string] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"))",
    "/test/m.gxi" => "val f: fn(x: (i64, string)) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: [('p, 'p), null] |x: 'q| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_twice_against_a_position_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"))",
    "/test/m.gxi" => "val f: fn(x: (i64, string)) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: [('p, 'p), null] |x: 'q| -> i64 0"
);

run!(
    an_unpaired_variable_with_a_predicate_alone_in_two_bounds_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_with_a_predicate_and_a_type_in_two_bounds_is_the_type,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + NoError, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
);

run!(
    an_unpaired_variable_with_no_bound_in_two_bounds_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_bounded_any_in_two_bounds_is_any,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: Any, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
);

// Two callers' picks of one signature variable's holders may differ: only a
// position holds the variable's type exactly.
run!(
    an_unpaired_variable_at_one_variable_in_two_bounds_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (1, 1), (1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: (i64, 'a), 'd: (i64, 'a)>(z: 'a, x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: (Number, 'p), 'r: (Number, 'p) |z, x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_at_one_variable_at_two_positions_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (1, 1), (1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(z: 'a, x: (i64, 'a), y: (i64, 'a)) -> i64;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: (Number, 'p), 'r: (Number, 'p) |z, x: 'q, y: 'r| -> i64 0"
);

// A variable named at two places of ONE signature variable is refused too
// when no bound of its own settles it, though a signature whose first place
// is a parameter makes the refusal necessary and one without it does not
// need it: the check does not model which place a call decides first.
run!(
    an_unpaired_variable_at_one_variable_in_two_bounds_without_a_parameter_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: (i64, 'a), 'd: (i64, 'a)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_at_one_variable_in_two_bounds_without_a_parameter_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, \"s\"))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: (i64, 'a), 'd: (i64, 'a)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + NoError, 'q: (Number, 'p), 'r: (Number, 'p) |x: 'q, y: 'r| -> i64 0"
);

run!(
    an_unpaired_variable_at_one_variable_in_two_arrays_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f([1], [1])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'd: Array<'a>>(x: Array<'a>, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: Array<'p>, 'r: Array<'p> |x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_at_one_variable_in_a_bound_and_a_callback_holds,
    prints("i64:3"),
    "/test.gx" => "mod m; let result = m::f((1, 2), |x: i64, y: Number| x + 1)",
    "/test/m.gxi" => "val f: fn<'a: Any, 'b: (i64, 'a), 'd: fn(x: 'a, y: Number) -> i64>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'q: (Number, 'p), 'r: fn(x: 'p, y: i64) -> i64 |x: 'q, y: 'r| -> i64 y(x.1, 0)"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_in_a_bound_and_an_argument_is_one_type,
    refused("val f is declared fn<'a: Any, 'b: (i64, 'a), 'c: Any, 'd: fn(x: 'c, y: Number) -> i64>(x: 'b, y: 'd) -> i64 but implemented fn<'q: (Number, 'p: unbound), 'r: fn(x: 'p: unbound, y: i64) -> i64>(x: 'q: unbound within (Number, 'p: unbound), y: 'r: unbound within fn(x: 'p: unbound, y: i64) -> i64) -> i64: type mismatch fn(x: 'p: unbound above 'a: unbound within Any, y: i64) -> i64 does not contain 'd: unbound within fn(x: 'c: unbound within Any, y: Number) -> i64"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), |x: i64, y: Number| x + 1)",
    "/test/m.gxi" => "val f: fn<'a: Any, 'c: Any, 'b: (i64, 'a), 'd: fn(x: 'c, y: Number) -> i64>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'q: (Number, 'p), 'r: fn(x: 'p, y: i64) -> i64 |x: 'q, y: 'r| -> i64 y(x.1, 0)"
; FuseExpect::None);

run!(
    an_unpaired_variable_at_a_position_and_an_argument_holds,
    is(Value::I64(3)),
    "/test.gx" => "mod m; let result = m::f((1, 2), |x: i64, y: Number| x + 1)",
    "/test/m.gxi" => "val f: fn<'a: Any>(x: (i64, 'a), y: fn(x: 'a, y: Number) -> i64) -> i64;",
    "/test/m.gx" => "let f = 'q: (Number, 'p), 'r: fn(x: 'p, y: i64) -> i64 |x: 'q, y: 'r| -> i64 y(x.1, 0)"
);

run!(
    an_unpaired_variable_named_once_in_an_argument_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f(|x: i64, y: Number| x)",
    "/test/m.gxi" => "val f: fn<'c: [i64, string], 'd: fn(x: 'c, y: Number) -> i64>(y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'r: fn(x: 'p, y: i64) -> i64 |y: 'r| -> i64 0"
);

run!(
    an_unpaired_variable_two_bounds_in_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: (i64, Array<'a>), 'c: [i64, string] + Singleton, 'd: (i64, Array<'c>)>(x: 'b, y: 'd) -> i64 but implemented fn<'q: (Number, 's: unbound within Array<'r: unbound within [i64, string] & Singleton>), 'r: [i64, string] + Singleton, 's: Array<'r: unbound within [i64, string] & Singleton>, 't: Array<'r: unbound within [i64, string] & Singleton>, 'u: (Number, 't: unbound within Array<'r: unbound within [i64, string] & Singleton>)>(x: 'q: unbound within (Number, 's: unbound within Array<'r: unbound within [i64, string] & Singleton>), y: 'u: unbound within (Number, 't: unbound within Array<'r: unbound within [i64, string]…: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((1, [\"s\"]), (1, [1]))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: (i64, Array<'a>), 'd: (i64, Array<'c>)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'r: [i64, string] + Singleton, 's: Array<'r>, 't: Array<'r>, 'q: (Number, 's), 'u: (Number, 't) |x: 'q, y: 'u| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_two_bounds_in_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, [\"s\"]), (1, [1]))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, Array<'a>), 'd: (i64, Array<'c>)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'r: [i64, string], 's: Array<'r>, 't: Array<'r>, 'q: (Number, 's), 'u: (Number, 't) |x: 'q, y: 'u| -> i64 0"
);

run!(
    an_unpaired_variable_inside_a_named_bound_is_one_type,
    refused("val f is declared fn<'a: [i64, string] + Singleton, 'b: (i64, 'a), 'c: [i64, string] + Singleton, 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64 but implemented fn<'p: [i64, string] + Singleton, 'q: T<'p: unbound within [i64, string] & Singleton>, 'r: T<'p: unbound within [i64, string] & Singleton>>(x: 'q: unbound within T<'p: unbound within [i64, string] & Singleton>, y: 'r: unbound within T<'p: unbound within [i64, string] & Singleton>) -> i64: ['a, 'c] is more than one type, where `Singleton` asks for one"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "type T<'x> = (Number, 'x); val f: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: T<'p>, 'r: T<'p> |x: 'q, y: 'r| -> i64 0"
; FuseExpect::None);

run!(
    an_unpaired_variable_inside_a_named_bound_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1))",
    "/test/m.gxi" => "type T<'x> = (Number, 'x); val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: T<'p>, 'r: T<'p> |x: 'q, y: 'r| -> i64 0"
);

// The bound an unpaired variable settles to holds every place it is named
// at, or the implementation is refused.
run!(
    an_unpaired_variables_own_bound_must_hold_each_place,
    refused("val f is declared fn<'a: [i64, string], 'b: (i64, 'a), 'c: [i64, string], 'd: fn(x: 'c, y: Number) -> i64>(x: 'b, y: 'd) -> i64 but implemented fn<'p: [i64, string], 'q: (Number, 'p: unbound within [i64, string]), 'r: fn(x: 'p: unbound within [i64, string], y: i64) -> i64>(x: 'q: unbound within (Number, 'p: unbound within [i64, string]), y: 'r: unbound within fn(x: 'p: unbound within [i64, string], y: i64) -> i64) -> i64: type mismatch fn(x: 'p: unbound within [i64, string] above 'a: unbound within [i64, string], y: i64) -> i64 does not contain 'd: unbound within fn(x: 'c: unbound within [i64, string], y: Number) -> i64"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), |x: i64, y: Number| x + 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: fn(x: 'c, y: Number) -> i64>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: fn(x: 'p, y: i64) -> i64 |x: 'q, y: 'r| -> i64 0"
; FuseExpect::None);

// A bound of the signature's own shape pairs its variable, and is no
// second place: an implementation that restates the signature holds. The
// pairing is one place beside a bound of another shape that names the
// variable, since a caller's pick of the bound's holder may be narrower.
run!(
    a_variable_paired_by_two_bounds_of_the_signatures_shape_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, 1), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string] + Singleton, 'b: (i64, 'a), 'd: (i64, 'a)>(x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: (i64, 'p), 'r: (i64, 'p) |x: 'q, y: 'r| -> i64 0"
);

run!(
    a_variable_paired_by_a_bound_and_named_in_another_with_a_predicate_alone_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (\"s\", 1), [[1]])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ('a, i64)>(z: 'a, x: 'b, y: Array<Array<'a>>) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ('p, i64), 'r: [Array<[Array<'p>, null]>, null] |z, x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    a_variable_paired_by_a_bound_and_named_in_another_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (\"s\", 1), [[1]])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ('a, i64)>(z: 'a, x: 'b, y: Array<Array<'a>>) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: ('p, i64), 'r: [Array<[Array<'p>, null]>, null] |z, x: 'q, y: 'r| -> i64 0"
);

run!(
    a_variable_paired_by_a_bound_and_named_in_a_functions_result_with_a_predicate_alone_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (\"s\", 1), |x: i64| 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ('a, i64), 'd: fn(x: i64) -> 'a>(z: 'a, x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: OneNumber, 'q: ('p, i64), 'r: fn(x: i64) -> ['p, null] |z, x: 'q, y: 'r| -> i64 0"
; FuseExpect::Jit);

run!(
    a_variable_paired_by_a_bound_and_named_in_a_functions_result_is_its_own_bound,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let u: [i64, string] = \"s\"; let result = m::f(u, (\"s\", 1), |x: i64| 1)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ('a, i64), 'd: fn(x: i64) -> 'a>(z: 'a, x: 'b, y: 'd) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: ('p, i64), 'r: fn(x: i64) -> ['p, null] |z, x: 'q, y: 'r| -> i64 0"
);

// A variable its bound does not admit where an argument names it stands
// nowhere, and the argument's bound is refused as it is of any other.
run!(
    an_unpaired_variable_in_an_argument_may_be_narrower,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f(|x: i64, y: Number| 0)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: fn(x: 'a, y: Number) -> i64) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: fn(x: 'p, y: i64) -> i64 |x: 'q| -> i64 0"
; FuseExpect::Jit);

// which Singleton does not.
run!(
    an_unpaired_variable_in_an_arguments_result_must_be_admitted,
    refused("the implementation requires 'a: Singleton, which the signature does not declare"),
    "/test.gx" => "mod m; let result = m::f(|x: i64, y: Number| 0)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string]>(x: fn(x: i64, y: Number) -> 'a) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string] + Singleton, 'q: fn(x: i64, y: i64) -> 'p |x: 'q| -> i64 0"
; FuseExpect::None);

// What a bare variable takes of a union's members is one type.
run!(
    an_unpaired_variable_taking_two_members_of_a_union_bound_holds,
    is(Value::String("s".into())),
    "/test.gx" => "mod m; let result = m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: NoError, 'q: ['p, `Y, `Z] |x: 'q| -> 'q x"
);

run!(
    an_unpaired_number_variable_in_a_nullable_bound_holds,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn<'b: [i64, null]>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: Number, 'q: ['p, null] |x: 'q| -> 'q x"
);

// An instance that takes a variable named at two places as its own bound
// takes every variable only bounds name that has such a bound as its own
// too, so beside one named at two places each is judged as its bound as
// well as at its place: a function's argument does not hold the wider type.
run!(
    an_unpaired_variable_in_an_argument_beside_one_at_two_places_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: Number| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e, y: Number) -> i64>(x: 'b, y: 'd, z: 'g) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: [i64, string], 's: fn(x: 'k, y: i64) -> i64 |x: 'q, y: 'r, z: 's| -> i64 0"
; FuseExpect::Jit);

run!(
    a_variable_a_bound_paired_in_an_argument_beside_one_at_two_places_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: i64| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e, y: i64) -> i64>(x: 'b, y: 'd, z: 'g) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: [i64, string], 's: fn(x: 'k, y: i64) -> i64 |x: 'q, y: 'r, z: 's| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_in_a_positions_argument_beside_one_at_two_places_holds,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: Number| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string]>(x: 'b, y: 'd, z: fn(x: 'e, y: Number) -> i64) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: [i64, string], 's: fn(x: 'k, y: i64) -> i64 |x: 'q, y: 'r, z: 's| -> i64 0"
; FuseExpect::Jit);

run!(
    an_unpaired_variable_with_a_predicate_alone_in_an_argument_beside_one_at_two_places_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: Number| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e, y: Number) -> i64>(x: 'b, y: 'd, z: 'g) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: NoError, 's: fn(x: 'k, y: i64) -> i64 |x: 'q, y: 'r, z: 's| -> i64 0"
);

run!(
    an_unpaired_variable_with_no_bound_in_an_argument_beside_one_at_two_places_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: i64| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e, y: i64) -> i64>(x: 'b, y: 'd, z: 'g) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 's: fn(x: 'k, y: i64) -> i64 |x: 'q, y: 'r, z: 's| -> i64 0"
);

run!(
    a_variable_in_a_parameters_argument_beside_one_at_two_places_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), |x: i64, y: i64| x)",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string]>(x: 'b, y: 'd, z: fn(x: 'e, y: i64) -> i64) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: [i64, string] |x: 'q, y: 'r, z: fn(x: 'k, y: i64) -> i64| -> i64 0"
);

run!(
    an_unpaired_variable_in_a_tuple_beside_one_at_two_places_holds,
    is(Value::I64(0)),
    "/test.gx" => "mod m; let result = m::f((1, \"s\"), (1, 1), (1, 1))",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: (i64, 'e)>(x: 'b, y: 'd, z: 'g) -> i64;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p), 'k: [i64, string], 's: (Number, 'k) |x: 'q, y: 'r, z: 's| -> i64 0"
);

// A cell carrying several conjuncts of one shape has each paired with
// the signature's bound, so every variable inside them is judged.
run!(
    each_conjunct_of_a_bound_must_be_declared,
    refused("val f is declared fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b but implemented fn<'p: [i64, string], 'q: Array<'p: unbound within [i64, string]> + Array<'r: unbound within [i64, f64]>, 'r: [i64, f64]>(x: 'q: unbound within Array<'p: unbound within [i64, string]> & Array<'r: unbound within [i64, f64]>) -> 'q: unbound within Array<'p: unbound within [i64, string]> & Array<'r: unbound within [i64, f64]>: type mismatch Array<'r: unbound within [i64, f64]> does not contain 'b: unbound within Array<'a: unbound within [i64, string]>"),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'r: [i64, f64], 'q: Array<'p> + Array<'r> |x: 'q| -> 'q x"
; FuseExpect::None);

run!(
    each_declared_conjunct_of_a_bound_holds,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 1),
    "/test.gx" => "mod m; let result = m::f([\"s\"])",
    "/test/m.gxi" => "val f: fn<'a: [i64, string], 'b: Array<'a>>(x: 'b) -> 'b;",
    "/test/m.gx" => "let f = 'p: [i64, string], 'r: [i64, string, f64], 'q: Array<'p> + Array<'r> |x: 'q| -> 'q x"
);

// A predicate the implementation needs of a compound signature type asks
// it of the variables inside: Array<'a> is discernible only if 'a is.
run!(
    a_compound_signature_type_needs_its_variables_bounds,
    refused("the implementation requires 'a: Discernible"),
    "/test.gx" => "mod m; let result = m::eq([1], [1])",
    "/test/m.gxi" => "val eq: fn(x: Array<'a>, y: Array<'a>) -> bool;",
    "/test/m.gx" => "let eq = |x, y| x == y"
; FuseExpect::None);

run!(
    a_compound_signature_type_with_its_bounds_checks,
    is(Value::Bool(true)),
    "/test.gx" => "mod m; let result = m::eq([1], [1])",
    "/test/m.gxi" => "val eq: fn<'a: Discernible>(x: Array<'a>, y: Array<'a>) -> bool;",
    "/test/m.gx" => "let eq = 'a: Discernible |x: Array<'a>, y: Array<'a>| -> bool x == y"
);

// An interface's names resolve through what it imports or declares
// itself, never through the implementation's imports or types.
run!(
    interface_name_through_implementation_use_is_refused,
    refused("undefined type T"),
    "/test.gx" => "mod other; mod inner; let result = inner::v",
    "/test/other.gx" => "type T = i64",
    "/test/inner.gxi" => "val v: T",
    "/test/inner.gx" => "use super::other::T; let v: T = 1"
);

run!(
    interface_typedef_body_through_implementation_use_is_refused,
    refused("undefined type T"),
    "/test.gx" => "mod other; mod inner; let result = inner::v",
    "/test/other.gx" => "type T = i64",
    "/test/inner.gxi" => "type U = T; val v: U",
    "/test/inner.gx" => "use super::other::T; let v: U = 1"
);

// The implementation reads what its interface imports, and may not import
// it again: a name is imported once.
run!(
    implementation_reads_its_interfaces_import,
    is(Value::I64(1)),
    "/test.gx" => "mod other; mod inner; let result = inner::v",
    "/test/other.gx" => "type T = i64",
    "/test/inner.gxi" => "use super::other::T; val v: T",
    "/test/inner.gx" => "let v: T = 1"
; FuseExpect::None);

run!(
    implementation_repeating_its_interfaces_import_refused,
    refused("`T` is already imported here: remove the repeated import"),
    "/test.gx" => "mod other; mod inner; let result = inner::v",
    "/test/other.gx" => "type T = i64",
    "/test/inner.gxi" => "use super::other::T; val v: T",
    "/test/inner.gx" => "use super::other::T; let v: T = 1"
);

run!(
    interface_name_of_an_implementation_type_is_refused,
    refused("undefined type T"),
    "/test.gx" => "mod inner; let result = inner::v",
    "/test/inner.gxi" => "val v: T",
    "/test/inner.gx" => "type T = i64; let v: T = 1"
);

run!(
    interface_use_names_its_types,
    is(Value::I64(1)),
    "/test.gx" => "mod other; mod inner; let result = inner::v",
    "/test/other.gx" => "type T = i64",
    "/test/inner.gxi" => "use super::other::T; type U = T; val v: U",
    "/test/inner.gx" => "let v: U = 1"
; FuseExpect::None);

// A mismatch in a labeled argument names the argument by its label.
run!(
    interface_labeled_argument_mismatch_names_the_label,
    refused("val f is declared fn(#b: i64, x: i64) -> i64 but implemented fn(#b: string, x: i64) -> i64"),
    "/test.gx" => "mod inner; let result = inner::f(#b: 1, 2)",
    "/test/inner.gxi" => "val f: fn(#b: i64, x: i64) -> i64",
    "/test/inner.gx" => "let f = |#b: string, x: i64| -> i64 x"
);

// `'a: Any` declares the variable and bounds nothing: the plain
// variable in the interface declares it.
run!(
    interface_plain_variable_takes_an_any_bound,
    is(Value::I64(1)),
    "/test.gx" => "mod inner; let result = inner::id(1)",
    "/test/inner.gxi" => "val id: fn(x: 'a) -> 'a",
    "/test/inner.gx" => "let id = 'a: Any |x: 'a| -> 'a x"
; FuseExpect::Jit);

// A signature's variable is every caller's to pick: it meets an
// implementation bound only through its own.
run!(
    interface_unbounded_variable_under_a_bounded_implementation_refused,
    refused("val f is declared fn(x: [null, 'b]) -> string but implemented fn<'a: [null, Number]>(x: 'a: unbound within [null, Number]) -> string: type mismatch [Number, null] does not contain 'b"),
    "/test.gx" => "mod inner; let result = inner::f(\"hello\")",
    "/test/inner.gxi" => "val f: fn(x: ['b, null]) -> string",
    "/test/inner.gx" => "let f = 'a: [Number, null] |x: 'a| -> string \"[x]\""
);

run!(
    interface_bounded_variable_under_a_bounded_implementation,
    prints("\"3\""),
    "/test.gx" => "mod inner; let result = inner::f(3)",
    "/test/inner.gxi" => "val f: fn<'b: Number>(x: ['b, null]) -> string",
    "/test/inner.gx" => "let f = 'a: [Number, null] |x: 'a| -> string \"[x]\""
; FuseExpect::Jit);

// A recursive typedef's union pairs its members under the typedef pair
// it expands, so the match meets that pair again and ends.
run!(
    recursive_typedef_of_functions_val_pairs_its_union,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = select m::l { `C(f, _) => f(1), `N => 0 }",
    "/test/m.gxi" => "type L<'a> = [`N, `C(fn(x: 'a) -> 'a, L<'a>)]; val l: L<'a>",
    "/test/m.gx" => "let l: L<'q> = `C(|x| x, `N)"
; FuseExpect::None);

// A val's type variable outside every function type would be one type all
// its uses share, which a val may not leave open: declare it at one type.
run!(
    val_variable_outside_a_function_type_refused,
    refused("val v has its type variable 'a outside a function type, one type all its uses share: declare the val at one type"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { m::v <- x; x };
        let rd = |u| u ~ m::v;
        let w = wr([1]);
        let a: Array<string> = rd(null);
        let result = a
    "#,
    "/test/m.gxi" => "val v: Array<'a>",
    "/test/m.gx" => "let v = never()"
);

run!(
    val_at_one_type_written_and_read,
    prints("[i64:1]"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { m::v <- x; x };
        let rd = |u| u ~ m::v;
        let w = wr([1]);
        let result = rd(w)
    "#,
    "/test/m.gxi" => "val v: Array<i64>",
    "/test/m.gx" => "let v = never()"
; FuseExpect::None);

run!(
    val_variable_outside_a_function_type_under_a_sibling_refused,
    refused("val v has its type variable 'a outside a function type"),
    "/test.gx" => r#"
        mod a;
        mod b;
        let w = b::set([1]);
        let r: Array<string> = b::k(null);
        let result = r
    "#,
    "/test/a.gxi" => "val v: Array<'a>",
    "/test/a.gx" => "let v = never()",
    "/test/b.gxi" => "val set: fn(x: Array<'a>) -> Array<'a>; val k: fn(u: Any) -> Array<'a>",
    "/test/b.gx" => "let set = |x| { super::a::v <- x; x }; let k = |u| u ~ super::a::v"
);

run!(
    val_variable_outside_a_function_type_beside_generic_vals_refused,
    refused("val v has its type variable 'a outside a function type"),
    "/test.gx" => r#"
        mod m;
        let w = m::set([1]);
        let a: Array<string> = m::k(null);
        let result = a
    "#,
    "/test/m.gxi" => r#"
        val v: Array<'a>;
        val set: fn(x: Array<'a>) -> Array<'a>;
        val k: fn(u: Any) -> Array<'a>
    "#,
    "/test/m.gx" => "let v = never(); let set = |x| { v <- x; x }; let k = |u| u ~ v"
);

// One standing both in a function type and beside it is outside one.
run!(
    val_variable_in_a_function_type_and_in_data_refused,
    refused("val c has its type variable 'a outside a function type"),
    "/test.gx" => r#"
        mod m;
        let save = |x| { m::c <- x; x };
        let w = save({show: |x| "[x]", last: 1});
        let peek = |u| (u ~ m::c).last;
        let l: [string, null] = peek(null);
        let result = l
    "#,
    "/test/m.gxi" => r#"
        type Codec<'a> = {show: fn(x: 'a) -> string, last: ['a, null]};
        val c: Codec<'a>
    "#,
    "/test/m.gx" => "let c = {show: |x| \"[x]\", last: never<['a, null]>()}"
);

run!(
    val_variable_in_a_struct_of_function_and_data_refused,
    refused("val p has its type variable 'a outside a function type"),
    "/test.gx" => "mod m; let result = m::p.f(1)",
    "/test/m.gxi" => "val p: {f: fn(x: 'a) -> 'a, d: Array<'a>}",
    "/test/m.gx" => "let p = {f: |x| x, d: never<Array<'a>>()}"
);

// An abstract type's parameter stands outside function types.
run!(
    val_variable_as_an_abstract_parameter_refused,
    refused("val w has its type variable 'a outside a function type"),
    "/test.gx" => "mod m; let result = m::w.0.f(1)",
    "/test/m.gxi" => "type W<'a> = Abstract<{f: fn(x: 'a) -> 'a}>; val w: W<'a>",
    "/test/m.gx" => "let w = W({f: |x| x})"
);

// An interface's typedef is refused at its definition as a body's is.
run!(
    interface_growing_typedef_refused,
    refused("recursive type T grows its parameter 'a as it recurses"),
    "/test.gx" => "mod m; let x: m::T<i64> = m::t; m::t <- null; let result = x",
    "/test/m.gxi" => "type T<'a> = [null, ('a, T<Array<'a>>)]; val t: T<'a>",
    "/test/m.gx" => "let t = never()"
);

// A dynamic module's sig is held to it at the program's check.
run!(
    dynamic_sig_variable_outside_a_function_type_refused,
    refused("val v has its type variable 'a outside a function type"),
    "/test.gx" => r#"
        let st = mod d dynamic {
            sig { val v: Array<'a>; val set: fn(x: Array<string>) -> null };
            source "let v = never(); let set = |x| { v <- x; null }"
        };
        let w = d::set(["hello"]);
        let a: Array<i64> = d::v;
        let result = a
    "#
);

// And to the names it imports and declares, as each load holds it: a
// val, a typedef and a trait method alike.
run!(
    dynamic_sig_name_it_does_not_import_refused,
    refused("undefined type Foo"),
    "/test.gx" => r#"
        type Foo = {a: i64};
        let st = mod foo dynamic {
            sig { val f: fn(x: Foo) -> i64; val k: i64 };
            source "let f = |x| 1; let k = 3"
        };
        let result = select st { error as e => 0, null as _ => foo::k }
    "#
; FuseExpect::None);

run!(
    dynamic_sig_name_it_imports_checks,
    is(Value::I64(7)),
    "/test.gx" => r#"
        type Foo = {a: i64};
        let st = mod foo dynamic {
            sig { use super::Foo; val f: fn(x: Foo) -> i64; val k: i64 };
            source "let f = |x: Foo| x.a; let k = 3"
        };
        let result = select st { error as e => 0, null as _ => foo::f({a: 4}) + foo::k }
    "#
; FuseExpect::None);

run!(
    dynamic_sig_typedef_name_it_does_not_import_refused,
    refused("undefined type Foo"),
    "/test.gx" => r#"
        type Foo = {a: i64};
        let st = mod foo dynamic {
            sig { type P = {p: Foo}; val k: i64 };
            source "let k = 3"
        };
        let result = select st { error as e => 0, null as _ => foo::k }
    "#
; FuseExpect::None);

run!(
    dynamic_sig_trait_name_it_does_not_import_refused,
    refused("undefined type Foo"),
    "/test.gx" => r#"
        type Foo = {a: i64};
        let st = mod foo dynamic {
            sig { trait Q { val q: fn(self, x: Foo) -> i64 }; val k: i64 };
            source "let k = 3"
        };
        let result = select st { error as e => 0, null as _ => foo::k }
    "#
; FuseExpect::None);

// A sig's import of a type its list declares below the module names
// nothing where the module stands.
run!(
    dynamic_sig_import_of_a_type_declared_below_refused,
    refused("the sig imports `Foo`, which nothing above this module declares"),
    "/test.gx" => r#"
        let st = mod foo dynamic {
            sig { use super::Foo; val f: fn(x: Foo) -> i64; val k: i64 };
            source "let f = |x| 1; let k = 3"
        };
        type Foo = {a: i64};
        let result = select st { error as e => 0, null as _ => foo::k }
    "#
; FuseExpect::None);

// A loaded body's dynamic module is held to its names by the load that
// declares it: the outer load is refused.
run!(
    dynamic_sig_name_in_a_loaded_body_refuses_its_load,
    is(Value::Bool(true)),
    "/test.gx" => r#"
        let st = mod outer dynamic {
            sig { val k: i64 };
            source """type Bar = {b: i64}; let st = mod inner dynamic { sig { val f: fn(x: Bar) -> i64 }; source "let f = |x| 1" }; let k = select st { error as e => 1, null as _ => 2 }"""
        };
        let result = select st {
            error as e => str::contains(#part: "undefined type Bar", "[e]"),
            null as _ => false
        }
    "#
; FuseExpect::Jit);

// An instance built while the program runs checks its sig's names too:
// one built after another's load, whose body renamed the import, passes.
run!(
    dynamic_sig_names_check_at_an_instance_built_after_a_load,
    is(Value::I64(9)),
    "/test.gx" => r#"
        type Foo = {a: i64};
        let mk = |src: string| {
            let st = mod foo dynamic {
                sig { use super::Foo; val f: fn(x: Foo) -> i64 };
                source src
            };
            select st { error as e => 0, null as _ => foo::f({a: 4}) }
        };
        let t = 0;
        t <- 1;
        let g = select t { 1 => mk, _ => never() };
        let a = g(t ~ "use super::Foo as Bar; let f = |x: Bar| x.a");
        let h = select a { 4 => mk, _ => never() };
        let b = h(a ~ "let f = |x: Foo| x.a + 1");
        let result = a + b
    "#
; FuseExpect::Jit);

// Inside function types each use picks a val's variable: a struct of
// functions, an alias of a function type (the alias defined after the
// module included) and a recursive typedef of functions.
run!(
    struct_of_functions_val_is_generic_per_use,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::s.f(1), m::s.f(\"s\"))",
    "/test/m.gxi" => "val s: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let s = {f: |x| x}"
; FuseExpect::None);

run!(
    aliased_function_val_is_generic_per_use,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
);

run!(
    alias_defined_after_the_module_is_generic_per_use,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => r#"
        mod a;
        type F<'a> = fn(x: 'a) -> 'a;
        let result = (a::id(1), a::id("s"))
    "#,
    "/test/a.gxi" => "val id: super::F<'a>",
    "/test/a.gx" => "let id = |x| x"
);

// A val's variable is each use's to pick: where the implementation has one
// type all uses share (a written let's, an outer binding's), it does not
// implement the val, a rank-2 variable included. Declared at one type, it
// does.
run!(
    generic_val_over_a_written_let_refused,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented [fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound]: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let f = |x| x; f <- |x| x"
);

run!(
    val_at_one_type_over_a_written_let,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn(x: i64) -> i64",
    "/test/m.gx" => "let f = |x| x; f <- |x| x"
; FuseExpect::None);

run!(
    generic_val_whose_argument_a_written_let_holds_refused,
    refused_renumbered("val set is declared fn(g: fn(x: 'a) -> 'a) -> null but implemented fn(g: '_0: unbound within fn(x: '_1: unbound within '_2: unbound) -> '_2: unbound) -> null: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = m::set(|x: i64| -> i64 x + 100)",
    "/test/m.gxi" => "val set: fn(g: fn(x: 'a) -> 'a) -> null",
    "/test/m.gx" => "let f = |x| x; let set = |g| { f <- g; null }"
);

run!(
    rank2_val_over_an_outer_binding_refused,
    refused_renumbered("val h is declared fn<'a: Any>(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::h(1), m::h(\"s\"))",
    "/test/m.gxi" => "val h: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let h = |x| { y <- x; x }"
);

run!(
    struct_of_functions_val_over_an_outer_binding_refused,
    refused_renumbered("val s is declared { f: fn(x: 'a) -> 'a, g: fn(t: Any) -> 'a } but implemented { f: fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound, g: fn(t: '_3: unbound) -> '_1: unbound }: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => r#"
        mod m;
        let a = m::s.f(1);
        let b = m::s.f("s");
        let v: i64 = m::s.g(null);
        let result = v + 1
    "#,
    "/test/m.gxi" => "val s: {f: fn(x: 'a) -> 'a, g: fn(t: Any) -> 'a}",
    "/test/m.gx" => "let y = never(); let s = {f: |x| { y <- x; x }, g: |t| t ~ y}"
);

run!(
    aliased_function_val_over_an_outer_binding_refused,
    refused_renumbered("val f is declared F<'a> but implemented fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let y = never(); let f = |x| { y <- x; x }"
);

// The refusal names the way out: the outer binding's type where the
// implementation is generic in its own cells, the val at one type where a
// written let's lambda writes the outer binding, else a quantified
// annotation on the let.
run!(
    val_over_an_outer_binding_names_its_type,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let f = |x| { y <- x; x }"
);

// A data literal's helper that writes an outer binding is followed: the
// refusal names the binding written, not the holder or nothing.
run!(
    val_struct_holding_a_writing_helper_names_what_it_writes,
    refused_renumbered("val c is declared { f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a } but implemented { f: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, g: fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound }: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::c.g(1), m::c.g(\"s\"))",
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let y = never(); let store = |x| { y <- x; y }; let c = {f: |x| x, g: store}"
);

// Of the captures that hold the cell in data, the refusal names the binding
// the program writes first, whichever lambda captures it: one built from
// another is written after it, and the first's annotation lifts the refusal.
run!(
    val_struct_names_the_holder_written_first,
    refused_renumbered("val c is declared { a: fn(x: 'a) -> i64, b: fn(x: 'b) -> 'b } but implemented { a: fn(x: '_0: unbound) -> i64, b: fn(x: '_1: unbound within '_2: unbound within '_2_1: unbound & '_3: unbound) -> '_3: unbound }: where the signature declares 'b, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'b, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::c.b(1), m::c.b(\"s\"))",
    "/test/m.gxi" => "val c: {a: fn(x: 'a) -> i64, b: fn(x: 'b) -> 'b}",
    "/test/m.gx" => "let p = never(); let q = [p]; let r = [q]; let c = {a: |x| array::len(r), b: |x| { q <- [x]; x }}"
);

run!(
    val_struct_holder_written_first_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::c.b(1), m::c.b(\"s\"))",
    "/test/m.gxi" => "val c: {a: fn(x: 'a) -> i64, b: fn(x: 'b) -> 'b}",
    "/test/m.gx" => "let p = never(); let q: Array<Any> = [p]; let r = [q]; let c = {a: |x| array::len(r), b: |x| { q <- [x]; x }}"
    ; FuseExpect::None
);

run!(
    val_names_the_holder_written_first_of_one_lambda_s_captures,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_1_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let p = never(); let q = [p]; let r = [q]; let f = |x| { let n = array::len(r); q <- [x]; x }"
);

run!(
    val_holder_written_first_of_one_lambda_s_captures_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let p = never(); let q: Array<Any> = [p]; let r = [q]; let f = |x| { let n = array::len(r); q <- [x]; x }"
    ; FuseExpect::None
);

// Of two captures that hold the cell in lambdas they hold as data and that
// something writes, the one written first is named, then the other, and
// with both annotated it runs.
run!(
    val_names_the_lambda_holder_written_first,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_2: unbound & '_3: unbound within '_4: unbound & '_5: unbound within '_6: unbound & '_7: unbound within '_8: unbound & '_9: unbound) -> '_9: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let st = {f: |x| x}; st <- {f: |x| x}; let st2 = {f: |x| x}; st2 <- {f: |x| x}; let f = |x| { st2.f(x); st.f(x); x }"
);

run!(
    val_names_the_second_lambda_holder_once_the_first_is_annotated,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_2: unbound & '_3: unbound within '_4: unbound & '_5: unbound) -> '_5: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let st: {f: fn(x: Any) -> Any} = {f: |x| x}; st <- {f: |x| x}; let st2 = {f: |x| x}; st2 <- {f: |x| x}; let f = |x| { st2.f(x); st.f(x); x }"
);

run!(
    val_lambda_holders_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let st: {f: fn(x: Any) -> Any} = {f: |x| x}; st <- {f: |x| x}; let st2: {f: fn(x: Any) -> Any} = {f: |x| x}; st2 <- {f: |x| x}; let f = |x| { st2.f(x); st.f(x); x }"
    ; FuseExpect::None
);

run!(
    val_names_a_written_lambda_it_calls,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; k <- |x| x; let f = |x| k(x)"
);

run!(
    val_names_a_written_lambda_it_hands_its_argument,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound & '_6: unbound) -> '_6: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; k <- |x| x; let f = |x| { k(x); x }"
);

run!(
    val_written_lambda_it_hands_its_argument_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k: fn(x: Any) -> Any = |x| x; k <- |x| x; let f = |x| { k(x); x }"
    ; FuseExpect::None
);

run!(
    val_unwritten_lambda_it_calls_is_generic,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; let f = |x| k(x)"
    ; FuseExpect::Jit
);

// Past a generic lambda the implementation calls, to the written one it
// calls in turn.
run!(
    val_names_a_written_lambda_behind_a_generic_one,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_1_1: unbound within '_2: unbound within '_3: unbound within '_4: unbound & '_5: unbound within '_6: unbound within '_4: unbound) -> '_4: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; k <- |x| x; let g = |x| k(x); let f = |x| g(x)"
);

// A written binding's lambdas are not followed: what they capture is no
// annotation's to widen while every read of the binding shares its type.
run!(
    val_names_a_written_lambda_not_what_it_captures,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let d: Any = never(); let k = |x| { d <- x; x }; k <- |x| x; let f = |x| k(x)"
);

run!(
    val_names_a_written_lambda_over_an_open_capture,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound & '_3: unbound within '_4: unbound & '_5: unbound within '_6: unbound within '_4: unbound) -> '_4: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let d = never(); let k = |x| { d <- x; x }; k <- |x| x; let f = |x| k(x)"
);

run!(
    val_names_a_written_struct_of_lambdas_not_what_they_capture,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_2: unbound & '_3: unbound & '_4: unbound within '_5: unbound) -> ['_3: unbound, '_5: unbound]: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let d = never(); let st = {h: |x| { d <- x; x }}; st <- {h: |x| x}; let f = |x| st.h(x)"
);

// An implementation something writes is one type itself: the val at one
// type is the way out.
run!(
    val_written_implementation_names_the_written_lambda_it_calls,
    refused("a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; k <- |x| x; let f = |x| k(x); f <- |x| k(x)"
);

// A function binding written through a reference, a written read of a
// generic function and a written call's result are each one type for every
// reader, named as written as a written lambda is.
run!(
    val_names_a_lambda_a_reference_writes,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let k = |x| x; let r = &mut k; *r <- |x| x; let f = |x| k(x)"
);

run!(
    val_names_a_written_read_of_a_function,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let s0 = {g: |x| x}; let j = s0.g; j <- |x| x; let f = |x| j(x)"
);

run!(
    val_written_read_of_a_function_it_hands_its_argument_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let s0 = {g: |x| x}; let j: fn(x: Any) -> Any = s0.g; j <- |x| x; let f = |x| { j(x); x }"
    ; FuseExpect::None
);

run!(
    val_names_a_written_call_result,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound & '_4: unbound within '_5: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let k = mk(); k <- |x| x; let f = |x| k(x)"
);

// A capture whose own type is the cell is named before one that holds it
// in data, whichever lambda is written first: only its annotation lifts
// the refusal.
run!(
    val_struct_names_the_capture_whose_type_is_the_cell,
    refused_renumbered("val c is declared { a: fn(x: 'a) -> 'a, b: fn(x: 'b) -> i64 } but implemented { a: fn(x: '_0: unbound within '_1: unbound within '_2: unbound within 'a: unbound & '_3: unbound) -> '_3: unbound, b: fn(x: '_4: unbound) -> i64 }: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::c.a(1), m::c.a(\"s\"))",
    "/test/m.gxi" => "val c: {a: fn(x: 'a) -> 'a, b: fn(x: 'b) -> i64}",
    "/test/m.gx" => "let p = never(); let q = [p]; let c = {b: |x| array::len(q), a: |x| { p <- x; x }}"
);

run!(
    val_struct_capture_whose_type_is_the_cell_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::c.a(1), m::c.a(\"s\"))",
    "/test/m.gxi" => "val c: {a: fn(x: 'a) -> 'a, b: fn(x: 'b) -> i64}",
    "/test/m.gx" => "let p: Any = never(); let q = [p]; let c = {b: |x| array::len(q), a: |x| { p <- x; x }}"
    ; FuseExpect::None
);

// So is one a lambda further in captures, past a nearer capture that only
// holds the cell in data.
run!(
    val_names_the_capture_whose_type_is_the_cell_past_a_nearer_holder,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_2: unbound within '_3: unbound within 'a: unbound & '_4: unbound within '_5: unbound within '_6: unbound) -> '_6: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let p = never(); let q = [p]; let g = |y| { p <- y; y }; let f = |x| { let n = array::len(q); g(x) }"
);

run!(
    val_capture_whose_type_is_the_cell_past_a_nearer_holder_annotated,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let p: Any = never(); let q = [p]; let g = |y| { p <- y; y }; let f = |x| { let n = array::len(q); g(x) }"
    ; FuseExpect::None
);

run!(
    val_struct_made_by_a_call_is_told_to_be_a_literal,
    refused_renumbered("val c is declared { f: fn(x: 'a) -> 'a } but implemented { f: fn(x: '_0: unbound within '_1: unbound within '_2: unbound) -> '_2: unbound }: a declared type variable meets a value from outside the definition that declares it ({ f: fn(x: 'a) -> 'a })"),
    "/test.gx" => "mod m; let result = (m::c.f(1), m::c.f(\"s\"))",
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let mk = |z| {f: |x| select true { true => x, false => z }}; let c = mk(never())"
);

// A struct update holding a call's result is generic in the parts it takes
// from a generic binding alone: the result is one instance, which no
// binding's annotation widens, so the refusal names a lambda in its place.
run!(
    val_struct_update_holding_a_call_result_refused,
    refused_renumbered("val s is declared { g: fn(x: 'a) -> 'a, h: fn(x: 'a) -> 'a } but implemented { g: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, h: fn(x: '_2: unbound within '_3: unbound & '_4: unbound) -> '_4: unbound }: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::s.h(1), m::s.h(\"s\"))",
    "/test/m.gxi" => "val s: {g: fn(x: 'a) -> 'a, h: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let mk = || { let v = never(); |x| { v <- x; x } }; let base = {g: |x| x, h: |x| x}; let s = {base with h: mk()}"
);

run!(
    val_struct_update_holding_a_read_of_a_call_result_names_a_lambda,
    refused_renumbered("val s is declared { g: fn(x: 'a) -> 'a, h: fn(x: 'a) -> 'a } but implemented { g: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, h: fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound }: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::s.h(1), m::s.h(\"s\"))",
    "/test/m.gxi" => "val s: {g: fn(x: 'a) -> 'a, h: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let mk = || |x| x; let g0 = mk(); let base = {g: |x| x, h: |x| x}; let s = {base with h: g0}"
);

run!(
    val_struct_update_holding_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::s.h(1), m::s.h(\"s\"))",
    "/test/m.gxi" => "val s: {g: fn(x: 'a) -> 'a, h: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let base = {g: |x| x, h: |x| x}; let s = {base with h: |x| x}"
; FuseExpect::None);

// A generic binding's read of a function is followed to the lambdas behind
// it: what they capture is named, and a call's result behind it is a
// lambda to write in its place.
run!(
    val_generic_read_names_the_binding_its_lambda_writes,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let s0 = {g: |x| { y <- x; x }}; let f = s0.g"
);

run!(
    val_generic_read_over_a_binding_of_every_type,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y: Any = never(); let s0 = {g: |x| { y <- x; x }}; let f = s0.g"
; FuseExpect::None);

run!(
    val_names_what_a_captured_generic_read_writes,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_2: unbound & '_3: unbound within '_4: unbound within '_5: unbound) -> '_5: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let s0 = {g: |x| { y <- x; x }}; let j = s0.g; let f = |x| j(x)"
);

run!(
    val_captured_generic_read_over_a_binding_of_every_type,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y: Any = never(); let s0 = {g: |x| { y <- x; x }}; let j = s0.g; let f = |x| j(x)"
; FuseExpect::None);

run!(
    val_generic_read_of_a_call_result_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let s0 = {g: |x| x, h: mk()}; let f = s0.h"
);

run!(
    val_generic_read_of_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let s0 = {g: |x| x, h: |x| x}; let f = s0.h"
; FuseExpect::None);

run!(
    val_capturing_a_struct_holding_a_call_result_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let s0 = {g: |x| x, h: mk()}; let f = |x| s0.h(x)"
);

run!(
    val_capturing_a_struct_holding_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let s0 = {g: |x| x, h: |x| x}; let f = |x| s0.h(x)"
; FuseExpect::None);

run!(
    val_capturing_a_generic_read_of_a_call_result_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let s0 = {g: |x| x, h: mk()}; let j = s0.h; let f = |x| j(x)"
);

run!(
    val_capturing_a_generic_read_of_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let s0 = {g: |x| x, h: |x| x}; let j = s0.h; let f = |x| j(x)"
; FuseExpect::None);

run!(
    val_capturing_a_call_result_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_0_1: unbound within '_1: unbound within '_2: unbound within '_3: unbound) -> '_3: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let k = mk(); let f = |x| k(x)"
);

run!(
    val_capturing_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let k = |x| mk()(x); let f = |x| k(x)"
; FuseExpect::None);

// Past a struct whose lambda calls into another, to the one holding the
// call's result.
run!(
    val_names_the_struct_holding_the_call_result_past_one_calling_into_it,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound within '_1_1: unbound within '_2: unbound within '_3: unbound within '_4: unbound) -> '_4: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let b1 = {h: mk()}; let b0 = {g: |x| b1.h(x)}; let f = |x| b0.g(x)"
);

run!(
    val_struct_calling_into_one_holding_a_lambda_in_place_of_the_call,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let b1 = {h: |x| x}; let b0 = {g: |x| b1.h(x)}; let f = |x| b0.g(x)"
; FuseExpect::None);

run!(
    written_val_capturing_a_struct_holding_a_call_result_names_one_type,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented [fn(x: '_0: unbound within '_1: unbound within '_2: unbound within '_3: unbound within '_4: unbound & '_5: unbound within '_6: unbound) -> '_4: unbound, fn(x: '_7: unbound within '_1: unbound within '_2: unbound within '_3: unbound within '_4: unbound & '_5: unbound within '_6: unbound) -> '_6: unbound]: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = || |x| x; let s0 = {g: |x| x, h: mk()}; let f = |x| s0.h(x); f <- |x| s0.h(x)"
);

// A struct of lambdas that holds the cell in data beside them is named as a
// holder in data, and its annotation lifts the refusal.
run!(
    val_struct_holding_the_cell_beside_lambdas_names_its_type,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound: where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share: give that binding a type that holds every 'a, or declare the signature at one type"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let s0 = {g: |x| x, r: &mut y}; let f = |x| { let r = s0.r; *r <- x; x }"
);

run!(
    val_struct_holding_the_cell_beside_lambdas_annotated,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let s0: {g: fn(x: Any) -> Any, r: &mut Any} = {g: |x| x, r: &mut y}; let f = |x| { let r = s0.r; *r <- x; x }"
; FuseExpect::None);

run!(
    val_struct_update_holding_a_call_result_at_one_type,
    prints("[i64:1, \"s\", i64:2]"),
    "/test.gx" => "mod m; let result = (m::s.g(1), m::s.g(\"s\"), m::s.h(2))",
    "/test/m.gxi" => "val s: {g: fn(x: 'a) -> 'a, h: fn(x: i64) -> i64}",
    "/test/m.gx" => "let mk = || { let v = never(); |x| { v <- x; x } }; let base = {g: |x| x, h: |x| x}; let s = {base with h: mk()}"
; FuseExpect::None);

run!(
    val_over_an_outer_binding_of_every_type,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y: Any = never(); let f = |x| { y <- x; x }"
; FuseExpect::None);

run!(
    written_val_over_an_outer_binding_names_one_type,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented [fn(x: '_0: unbound within '_1: unbound & '_2: unbound) -> '_2: unbound, fn(x: '_3: unbound within '_1: unbound & '_4: unbound) -> '_4: unbound]: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let f = |x| { y <- x; x }; f <- |x| { y <- x; x }"
);

// An implementation that is no lambda (a call's result) has the types its
// value fixed: the way out is a lambda, not a quantified annotation, which
// the value would not fit.
run!(
    val_over_a_call_result_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = |u: i64| |x| x; let f = mk(0)"
);

// A let of a select is not generalized, so a val over it is refused as the
// same let outside a module is.
run!(
    val_over_a_select_of_lambdas_names_a_lambda,
    refused_renumbered("val f is declared fn(x: 'a) -> 'a but implemented [fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound]: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let f = select true { true => |x| x, false => |x| x }"
);

run!(
    val_over_a_lambda_of_a_select_at_two_types,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let f = |x| select true { true => x, false => x }"
);

// A val whose type is no function, implemented by no literal of lambdas,
// is told to be one.
run!(
    data_val_over_no_literal_names_a_literal_of_lambdas,
    refused_renumbered("val v is declared Array<fn(x: 'a) -> 'a> but implemented '_0: unbound: a declared type variable meets a value from outside the definition that declares it (Array<fn(x: 'a) -> 'a>)"),
    "/test.gx" => "mod m; let result = m::v",
    "/test/m.gxi" => "val v: Array<fn(x: 'a) -> 'a>",
    "/test/m.gx" => "let v = never()"
);

run!(
    rank2_val_over_a_written_let_names_the_annotation,
    refused_renumbered("val h is declared fn<'a: Any>(x: 'a) -> 'a but implemented [fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound]: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; let result = (m::h(1), m::h(\"s\"))",
    "/test/m.gxi" => "val h: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let h = |x| x; h <- |x| x"
);

// The refusal names the variable first by name, whichever the walk met.
run!(
    val_over_an_open_let_names_its_first_variable,
    refused_renumbered("val f is declared fn(x: 'a, y: 'b) -> ('a, 'b) but implemented '_0: unbound: a declared type variable meets a value from outside the definition that declares it (fn(x: 'a, y: 'b) -> ('a, 'b))"),
    "/test.gx" => "mod m; let result = m::f(1, 2)",
    "/test/m.gxi" => "val f: fn(x: 'a, y: 'b) -> ('a, 'b)",
    "/test/m.gx" => "let f = never()"
);

// A val decides what its implementation leaves open where no use copies
// it, as an annotation on the let would: the value is built at the
// declared type.
run!(
    val_decides_an_open_parameter,
    is(Value::Bool(true)),
    "/test.gx" => "mod m; let result = m::e == m::i",
    "/test/m.gxi" => "type B<'x>; val e: B<i64>; val i: B<i64>",
    "/test/m.gx" => "type B<'x> = Abstract<i64>; let e = B(1); let i: B<i64> = B(1)"
);

run!(
    val_decides_an_open_parameter_map_key,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = map::len(map::insert(map::insert({}, m::e, 1), m::i, 2))",
    "/test/m.gxi" => "type B<'x>; val e: B<i64>; val i: B<i64>",
    "/test/m.gx" => "type B<'x> = Abstract<i64>; let e = B(1); let i: B<i64> = B(1)"
);

run!(
    val_decides_an_open_parameter_dedup,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = array::len(array::dedup([m::e, m::i]))",
    "/test/m.gxi" => "type B<'x>; val e: B<i64>; val i: B<i64>",
    "/test/m.gx" => "type B<'x> = Abstract<i64>; let e = B(1); let i: B<i64> = B(1)"
);

run!(
    val_decides_an_open_parameter_through_an_alias,
    is(Value::Bool(true)),
    "/test.gx" => "mod m; let result = m::e == m::c",
    "/test/m.gxi" => r#"
        type B<'x>;
        type R<'a> = [`Nil, `Cons('a, R<'a>)];
        type E = [`Nil, `Cons(i64, R<i64>)];
        val c: B<R<i64>>;
        val e: B<E>
    "#,
    "/test/m.gx" => "type B<'x> = Abstract<i64>; let c: B<R<i64>> = B(1); let e = B(1)"
);

// Two vals over one open cell decide it once.
run!(
    two_vals_over_one_open_cell_at_two_types_refused,
    refused("val b is declared B<string> but implemented B<i64>"),
    "/test.gx" => "mod m; let result = (m::a, m::b)",
    "/test/m.gxi" => "type B<'x>; val a: B<i64>; val b: B<string>",
    "/test/m.gx" => "type B<'x> = Abstract<i64>; let a = B(1); let b = a"
);

// A parameter its payload bounds from below takes only a type holding it.
run!(
    val_below_its_open_parameter_bound_refused,
    refused("val w is declared W<string> but implemented W<i64>"),
    "/test.gx" => "mod m; let result = m::w",
    "/test/m.gxi" => "type W<'x>; val w: W<string>",
    "/test/m.gx" => "type W<'x> = Abstract<'x>; let w = W(1)"
);

// A val that would decide a cell the code around the module left open is
// refused for an annotation there: the module's check writes no cell it
// did not create.
run!(
    val_over_an_outer_open_cell_refused,
    refused("type mismatch i64 does not contain string"),
    "/test.gx" => "let z = never(); mod m; z <- \"s\"; let result = m::e",
    "/test/m.gxi" => "val e: i64",
    "/test/m.gx" => "let e = super::z"
);

run!(
    val_over_an_annotated_outer_cell,
    is(Value::I64(3)),
    "/test.gx" => "let z: i64 = never(); mod m; z <- 3; let result = m::e",
    "/test/m.gxi" => "val e: i64",
    "/test/m.gx" => "let e = super::z"
; FuseExpect::None);

/// A refusal whose message holds each of `phrases`, in order.
fn told_in_order(phrases: &'static [&'static str]) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| {
        let Err(e) = v else { return false };
        let e = format!("{e:#}");
        let mut at = 0;
        phrases.iter().all(|p| match e[at..].find(p) {
            Some(i) => {
                at += i + p.len();
                true
            }
            None => false,
        })
    }
}

// Calls at two types through a function the code around the module left
// one instance would decide its type there: the refusal names the binding
// that holds it.
run!(
    calls_at_two_types_through_an_outer_instance_refused,
    refused("the check decides a type the code around the module left open: annotate `fs`"),
    "/test.gx" => "let fs = array::map([1], |r| |x| x + x); mod m; let result = m::go",
    "/test/m.gxi" => "val go: (i64, f64)",
    "/test/m.gx" => "let g = super::fs[0]$; let go = (g(1), g(1.5))"
);

// A check that fails for its own reason after such a decision tells both.
run!(
    an_error_after_an_outer_decision_refused,
    told_in_order(&[
        "the check decides a type the code around the module left open: annotate `fs`",
        "cannot compute with +",
    ]),
    "/test.gx" => "let fs = array::map([1], |r| |x| x + x); mod m; let result = m::go",
    "/test/m.gxi" => "val go: i64",
    "/test/m.gx" => "let g = super::fs[0]$; let a = g(1); let go = \"x\" + 1"
);

run!(
    calls_at_one_type_through_an_annotated_outer_instance,
    prints("[i64:2, i64:4]"),
    "/test.gx" => "let fs: Array<fn(x: i64) -> i64> = array::map([1], |r| |x| x + x); mod m; let result = m::go",
    "/test/m.gxi" => "val go: (i64, i64)",
    "/test/m.gx" => "let g = super::fs[0]$; let go = (g(1), g(2))"
; FuseExpect::None);

// A function type in a val quantifies its own variables: one standing
// whole where the implementation has a cell no use copies is decided there.
run!(
    rank2_val_over_an_open_let_refused,
    refused("a declared type variable meets a value from outside the definition that declares it (fn(x: 'a) -> 'a)"),
    "/test.gx" => "mod m; m::e <- |x| x; let result = (m::e(1), m::e(\"s\"))",
    "/test/m.gxi" => "val e: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let e = never()"
; FuseExpect::None);

// What is written through a val lands on its implementation, which every
// use takes at its own types: the val must fit it, its bounds included.
run!(
    narrow_write_through_a_val_refused,
    refused("val f is written, and what is written lands on its implementation"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x: i64| -> i64 x + 100;
        let result = m::g(null)
    "#,
    "/test/m.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> string",
    "/test/m.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")"
);

run!(
    write_through_a_val_over_an_implementation_at_its_type,
    is(Value::I64(101)),
    "/test.gx" => r#"
        mod m;
        m::f <- |x: i64| -> i64 x + 100;
        let result = select m::g(null) { n if n > 2 => n, _ => never() }
    "#,
    "/test/m.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> i64",
    "/test/m.gx" => "let f: fn(x: i64) -> i64 = |x| x; let g = |t| f(t ~ 1)");

// The val's variable is decided by every position it meets: the argument
// puts it above the implementation's `'x`, and the result makes it the
// implementation's `'c`, which holds `'x` from below.
run!(
    write_through_a_val_whose_result_holds_the_argument_from_below,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s 7"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x| m::B(x);
        let result = "[m::f("s").0] [m::f(7).0]"
    "#,
    "/test/m.gxi" => "type B<'p> = Abstract<'p>; val f: fn(x: 'a) -> B<'a>",
    "/test/m.gx" => "let f = |x| B(x)"
; FuseExpect::None);

run!(
    write_through_a_val_whose_result_holds_the_argument_from_below_lands,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "s 7"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x| m::B((x, 1));
        let (r, q) = (m::f("s").0, m::f(7).0);
        let result = select (r.1, q.1) { (1, 1) => "[r.0] [q.0]", _ => never() }
    "#,
    "/test/m.gxi" => "type B<'p> = Abstract<('p, i64)>; val f: fn(x: 'a) -> B<'a>",
    "/test/m.gx" => "let f = |x| B((x, 0))"
; FuseExpect::Jit);

run!(
    write_through_a_val_whose_variable_joins_two_arguments,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "i1"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x, y| x;
        let result = select m::h(null) { "ss" => never(), s => s }
    "#,
    "/test/m.gxi" => "val f: fn(x: 'a, y: 'a) -> 'a; val h: fn(t: Any) -> string",
    "/test/m.gx" => r#"
        let f = |x, y| select true { true => y, false => x };
        let h = |t| select f(t ~ 1, t ~ "s") { i64 as i => "i[i]", s => "s[s]" }
    "#
);

run!(
    write_through_a_val_whose_result_is_one_argument_refused,
    refused("val f is written, and what is written lands on its implementation"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x, y| x;
        let result = m::h(null)
    "#,
    "/test/m.gxi" => "val f: fn(x: 'a, y: 'a) -> 'a; val h: fn(t: Any) -> i64",
    "/test/m.gx" => r#"
        let f = |x, y| y;
        let h = |t| { let r: string = f(t ~ 1, t ~ "s"); str::len(r) }
    "#
);

run!(
    write_through_a_val_whose_result_does_not_hold_the_argument_refused,
    refused("val f is written, and what is written lands on its implementation"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x| m::B(x);
        let result = m::g(null)
    "#,
    "/test/m.gxi" => "type B<'p> = Abstract<'p>; val f: fn(x: 'a) -> B<'a>; val g: fn(t: Any) -> string",
    "/test/m.gx" => "let f = |x| B(never()); let g = |t| { let b: B<string> = f(t ~ 1); b.0 }"
);

run!(
    bounded_write_through_a_val_refused,
    refused("val f is written, and what is written lands on its implementation"),
    "/test.gx" => r#"
        mod m;
        m::f <- |x| x * x;
        let result = m::g(null)
    "#,
    "/test/m.gxi" => "val f: fn<'a: Number + Singleton>(x: 'a) -> 'a; val g: fn(t: Any) -> string",
    "/test/m.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")"
);

// A writable reference to a val at one type is a write through it.
run!(
    mut_ref_to_a_val_at_one_type,
    is(Value::I64(101)),
    "/test.gx" => r#"
        mod a;
        let rf = &mut a::f;
        *rf <- |x: i64| -> i64 x + 100;
        let result = select a::f(1) { n if n > 2 => n, _ => never() }
    "#,
    "/test/a.gxi" => "val f: fn(x: i64) -> i64",
    "/test/a.gx" => "let f = |x: i64| -> i64 x + 1");

run!(
    mut_ref_to_a_val_at_one_type_over_a_generic_body_refused,
    refused("val f is written, and what is written lands on its implementation"),
    "/test.gx" => r#"
        mod m;
        let rf = &mut m::f;
        *rf <- |x: i64| -> i64 x + 100;
        let result = m::g(null)
    "#,
    "/test/m.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> string",
    "/test/m.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")"
);

// A write a sibling module's lambda makes through a val is judged at the
// val's implementation once the siblings are checked, whichever check ran
// first.
run!(
    narrow_write_from_a_sibling_lambda_refused_at_the_val,
    refused("in: let f = |x| x: val f is written, and what is written lands on its implementation"),
    "/test.gx" => "mod a; mod w; let result = (a::g(null), w::z)",
    "/test/a.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> string",
    "/test/a.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")",
    "/test/w.gxi" => "val z: null",
    "/test/w.gx" => "let set = |v| { super::a::f <- v; null }; let z = set(|x: i64| -> i64 x + 100)"
);

run!(
    mut_ref_from_a_sibling_lambda_refused_at_the_val,
    refused("in: let f = |x| x: val f is written, and what is written lands on its implementation"),
    "/test.gx" => "mod a; mod w; let result = (a::g(null), w::z)",
    "/test/a.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> string",
    "/test/a.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")",
    "/test/w.gxi" => "val z: null",
    "/test/w.gx" => "let set = |v| { let r = &mut super::a::f; *r <- v; null }; let z = set(|x: i64| -> i64 x + 100)"
);

run!(
    narrow_write_from_a_sibling_into_a_submodule_refused_at_the_val,
    refused("a::s: at: line: 1, column: 1 in module /test/a/s, in: let f = |x| x: val f is written"),
    "/test.gx" => "mod a; mod w; let result = (a::s::g(null), w::z)",
    "/test/a.gxi" => "mod s",
    "/test/a.gx" => "null",
    "/test/a/s.gxi" => "val f: fn(x: i64) -> i64; val g: fn(t: Any) -> string",
    "/test/a/s.gx" => "let f = |x| x; let g = |t| f(t ~ \"s\")",
    "/test/w.gxi" => "val z: null",
    "/test/w.gx" => "let set = |v| { super::a::s::f <- v; null }; let z = set(|x: i64| -> i64 x + 100)"
);

run!(
    write_from_a_sibling_lambda_over_an_implementation_at_its_type,
    is(Value::I64(101)),
    "/test.gx" => "mod a; mod w; let result = select a::f(w::z ~ 1) { n if n > 2 => n, _ => never() }",
    "/test/a.gxi" => "val f: fn(x: i64) -> i64",
    "/test/a.gx" => "let f: fn(x: i64) -> i64 = |x| x",
    "/test/w.gxi" => "val z: null",
    "/test/w.gx" => "let set = |v| { super::a::f <- v; null }; let z = set(|x: i64| -> i64 x + 100)"
; FuseExpect::Jit);

// A variable standing only in a quantifier's bound is the val's too, held
// rigid against what is written.
run!(
    write_through_a_val_deciding_a_bound_variable_refused,
    refused("a generic binding, so what is written to it must be as general, and fn(x: [i64, null]) -> i64 is not"),
    "/test.gx" => "mod m; m::f <- |x: [i64, null]| -> i64 x$ + 1; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn<'b: ['a, null]>(x: 'b) -> i64",
    "/test/m.gx" => "let f = 'b: ['a, null] |x: 'b| -> i64 0"
);

run!(
    write_through_a_val_with_a_bound_variable_as_general,
    is(Value::I64(0)),
    "/test.gx" => "mod m; m::f <- |x| 0; let result = m::f(1) + m::f(\"s\")",
    "/test/m.gxi" => "val f: fn<'b: ['a, null]>(x: 'b) -> i64",
    "/test/m.gx" => "let f = 'b: ['a, null] |x: 'b| -> i64 0"
; FuseExpect::None);

// A writable reference to a generic val, which uses have taken at their
// own types, is refused.
run!(
    mut_ref_to_a_generic_val_refused,
    refused("a::f is generic, and a writable reference to it would let a write narrow it: write it with <- a value as general; to write it at one type, write that type on its let (on its val, where an interface declares it)"),
    "/test.gx" => "mod a; let rf = &mut a::f; let result = a::f(1)",
    "/test/a.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/a.gx" => "let f = |x| x"
);

// The val is what makes the binding generic: its implementation's
// annotation alone does not let the write pass, the val's does
// (mut_ref_to_a_val_at_one_type).
run!(
    mut_ref_to_a_generic_val_over_a_typed_let_refused,
    refused("a::f is generic, and a writable reference to it would let a write narrow it: write it with <- a value as general; to write it at one type, write that type on its let (on its val, where an interface declares it)"),
    "/test.gx" => "mod a; let rf = &mut a::f; let result = a::f(1)",
    "/test/a.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/a.gx" => "let f: fn(x: i64) -> i64 = |x| x"
);

// A val's own quantifiers are its scheme's, which each use copies, as an
// unquantified val's variables are.
run!(
    mut_ref_to_a_quantified_val_refused,
    refused("a::f is generic, and a writable reference to it would let a write narrow it"),
    "/test.gx" => r#"
        mod a;
        let set = |r: &mut fn(x: i64) -> i64, v| { *r <- v; null };
        let z = set(&mut a::f, |x: i64| -> i64 x + 100);
        let result = a::f("s")
    "#,
    "/test/a.gxi" => "val f: fn<'b: Any>(x: 'b) -> 'b",
    "/test/a.gx" => "let f = |x| x"
);

// A val whose type holds a variable is generic wherever its function types
// stand (a typedef, a struct, an array): what is written through it must
// be as general, and a writable reference to it is refused.
// CR claude for claude: [test-gap/medium] This fixture,
// write_through_a_struct_of_functions_val_refused and
// write_through_an_array_of_functions_val_refused all write through `wr = |x| { m::f <-
// x; x }`. Their refusal is for x being wr's own variable ("it holds a type variable of
// the code around the write"), and it comes the same when wr is never called or is
// called with a value as general as the val. The narrow `fn(x: i64) -> i64` they pass
// is never judged, and the struct fixture pins wr's cell `'a1` as the written type. A
// check that let a narrow write into a struct or array val through would keep all three
// green, and no other fixture pins the array case. Write the narrow value directly
// (`m::v <- [|x: i64| -> i64 x + 100]`) and pin "and Array<fn(x: i64) -> i64> is not"
// (likewise the struct and the typedef). Keep one wr fixture, named for the
// outer-variable rule. probe:
// design/review-2026-11-16/repro/tests-interfaces-image-write-through-wr.sh
// (tests-interfaces-image-02)
run!(
    write_through_a_typedef_val_refused,
    refused("f is F<'a: unbound>, a generic binding, so what is written to it must be as general"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { m::f <- x; x };
        let w = wr(|x: i64| -> i64 x + 100);
        let result = m::f("s")
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
);

run!(
    write_through_a_typedef_val_as_general,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::f <- |x| x; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
; FuseExpect::None);

// The val ties both fields to one 'a, where the implementation's lambdas
// have one each; what is written through the val lands on the
// implementation, so its let is written at the val's type.
run!(
    write_through_a_typedef_of_two_functions_val_as_general,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::c <- {f: |x| x, g: |x| x}; let result = (m::c.f(1), m::c.g(\"s\"))",
    "/test/m.gxi" => "type S<'a> = {f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a}; val c: S<'a>",
    "/test/m.gx" => "let c: S<'a> = {f: |x| x, g: |x| x}"
; FuseExpect::None);

run!(
    write_through_a_typedef_of_two_typedefs_val_as_general,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::c <- {f: |x| x, g: |x| x}; let result = (m::c.f(1), m::c.g(\"s\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; type S<'a> = {f: F<'a>, g: F<'a>}; val c: S<'a>",
    "/test/m.gx" => "let c: S<'a> = {f: |x| x, g: |x| x}"
; FuseExpect::None);

run!(
    write_through_a_recursive_typedef_of_two_functions_val_over_a_bare_variant_refused,
    refused("where the signature declares 'a, a type each use picks, the implementation writes it to a binding outside its let, which has one type all uses share"),
    "/test.gx" => "mod m; m::c <- `Cons(|x| x, |x| x, `Nil); let result = (select m::c { `Cons(f, _, _) => f(1), `Nil => never() }, select m::c { `Cons(_, g, _) => g(\"s\"), `Nil => never() })",
    "/test/m.gxi" => "type L<'a> = [`Cons(fn(x: 'a) -> 'a, fn(x: 'a) -> 'a, L<'a>), `Nil]; val c: L<'a>",
    "/test/m.gx" => "let c: L<'a> = `Nil"
; FuseExpect::Jit);

run!(
    write_through_an_alias_of_a_typedef_application_val_as_general,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::c <- {f: |x| x, g: |x| x}; let result = (m::c.f(1), m::c.g(\"s\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; type P<'a, 'b> = {f: F<'a>, g: F<'b>}; type S<'a> = P<'a, 'a>; val c: S<'a>",
    "/test/m.gx" => "let c: S<'a> = {f: |x| x, g: |x| x}"
; FuseExpect::None);

// A typedef whose body names its parameter twice, applied to a typedef of
// a function type: its expansion copies each place apart.
run!(
    write_through_a_typedef_naming_a_function_typedef_twice_val_as_general,
    prints("[i64:1, \"s\", i64:2, \"t\"]"),
    "/test.gx" => "mod m; m::c <- {f: |x| x, g: |x| x}; m::d <- (|x| x, |x| x); let result = (m::c.f(1), m::c.g(\"s\"), m::d.0(2), m::d.1(\"t\"))",
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; type P<'t> = {f: 't, g: 't}; type Q<'t> = ('t, 't); val c: P<F<'a>>; val d: Q<F<'a>>",
    "/test/m.gx" => "let c: P<F<'a>> = {f: |x| x, g: |x| x}; let d: Q<F<'a>> = (|x| x, |x| x)"
; FuseExpect::None);

// Each application of a nest of typedefs doubling at every level is
// copied as its expansion only so many times; the rest as one.
run!(
    write_through_a_deep_nest_of_typedef_applications_ends,
    refused("a declared type variable meets a value from outside the definition that declares it ('a)"),
    "/test.gx" => "mod m; m::c <- m::c; let result = 1",
    "/test/m.gxi" => "type T0<'a> = fn(x: 'a) -> 'a; type T1<'a> = {f: T0<'a>, g: T0<'a>}; type T2<'a> = {f: T1<'a>, g: T1<'a>}; type T3<'a> = {f: T2<'a>, g: T2<'a>}; type T4<'a> = {f: T3<'a>, g: T3<'a>}; type T5<'a> = {f: T4<'a>, g: T4<'a>}; type T6<'a> = {f: T5<'a>, g: T5<'a>}; type T7<'a> = {f: T6<'a>, g: T6<'a>}; type T8<'a> = {f: T7<'a>, g: T7<'a>}; type T9<'a> = {f: T8<'a>, g: T8<'a>}; type T10<'a> = {f: T9<'a>, g: T9<'a>}; type T11<'a> = {f: T10<'a>, g: T10<'a>}; type T12<'a> = {f: T11<'a>, g: T11<'a>}; type T13<'a> = {f: T12<'a>, g: T12<'a>}; type T14<'a> = {f: T13<'a>, g: T13<'a>}; type T15<'a> = {f: T14<'a>, g: T14<'a>}; type T16<'a> = {f: T15<'a>, g: T15<'a>}; type T17<'a> = {f: T16<'a>, g: T16<'a>}; type T18<'a> = {f: T17<'a>, g: T17<'a>}; type T19<'a> = {f: T18<'a>, g: T18<'a>}; type T20<'a> = {f: T19<'a>, g: T19<'a>}; type T21<'a> = {f: T20<'a>, g: T20<'a>}; type T22<'a> = {f: T21<'a>, g: T21<'a>}; type T23<'a> = {f: T22<'a>, g: T22<'a>}; type T24<'a> = {f: T23<'a>, g: T23<'a>}; val c: T24<'a>",
    "/test/m.gx" => "let c: T24<'a> = never()"
; FuseExpect::None);

run!(
    write_through_a_struct_of_functions_val_refused,
    refused_renumbered("test::m::c is { f: fn(x: 'a: unbound) -> 'a: unbound }, a generic binding, so what is written to it must be as general, and '_0: unbound within { f: fn(x: 'a1: unbound) -> 'a1: unbound } is not: it holds a type variable of the code around the write; to write it at one type, write that type on its let (on its val, where an interface declares it)"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { m::c <- x; x };
        let w = wr({f: |x: i64| -> i64 x + 100});
        let result = m::c.f("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
);

run!(
    write_through_a_struct_of_functions_val_as_general,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::c <- {f: |x| x}; let result = (m::c.f(1), m::c.f(\"s\"))",
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
; FuseExpect::None);

run!(
    write_through_an_array_of_functions_val_refused,
    refused_renumbered("test::m::v is Array<fn(x: 'a: unbound) -> 'a: unbound>, a generic binding, so what is written to it must be as general, and '_0: unbound within Array<fn(x: 'a1: unbound) -> 'a1: unbound> is not: it holds a type variable of the code around the write; to write it at one type, write that type on its let (on its val, where an interface declares it)"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { m::v <- x; x };
        let w = wr([|x: i64| -> i64 x + 100]);
        let result = m::v[0]$("s")
    "#,
    "/test/m.gxi" => "val v: Array<fn(x: 'a) -> 'a>",
    "/test/m.gx" => "let v = [|x| x]"
);

run!(
    mut_ref_to_a_struct_of_functions_val_refused,
    refused("m::c is generic, and a writable reference to it would let a write narrow it"),
    "/test.gx" => r#"
        mod m;
        let wr = |x| { let r = &mut m::c; *r <- x; x };
        let w = wr({f: |x: i64| -> i64 x + 100});
        let result = m::c.f("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
);

// Two function types of one val that name their variable alike are two
// schemes: a write is held to each, whatever their names.
run!(
    write_through_a_val_of_alike_named_functions_refused,
    refused_renumbered("val c is written, and what is written lands on its implementation, of type { f: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, g: fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound }, which the val's { f: fn(x: 'a: unbound) -> 'a: unbound, g: fn(x: 'a: unbound) -> 'a: unbound } does not fit: annotate the implementation's let with the val's type"),
    "/test.gx" => r#"
        mod m;
        let y = never();
        let store = |x| { y <- x; y };
        m::c <- {f: |x| x, g: store};
        let result = (m::c.g(1), m::c.g("s"))
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x, g: |x| x}"
);

run!(
    write_through_a_val_of_alike_named_typedefs_refused,
    refused_renumbered("val c is written, and what is written lands on its implementation, of type { f: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, g: fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound }, which the val's { f: F<'a: unbound>, g: F<'a: unbound> } does not fit: annotate the implementation's let with the val's type"),
    "/test.gx" => r#"
        mod m;
        let y = never();
        let store = |x| { y <- x; y };
        m::c <- {f: |x| x, g: store};
        let result = (m::c.g(1), m::c.g("s"))
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val c: {f: F<'a>, g: F<'a>}",
    "/test/m.gx" => "let c = {f: |x| x, g: |x| x}"
);

run!(
    narrow_write_through_a_val_of_alike_named_functions_refused,
    refused_renumbered("val c is written, and what is written lands on its implementation, of type { f: fn(x: '_0: unbound within '_1: unbound) -> '_1: unbound, g: fn(x: '_2: unbound within '_3: unbound) -> '_3: unbound }, which the val's { f: fn(x: 'a: unbound) -> 'a: unbound, g: fn(x: 'a: unbound) -> 'a: unbound } does not fit: annotate the implementation's let with the val's type"),
    "/test.gx" => r#"
        mod m;
        m::c <- {f: |x| x, g: |x: i64| -> i64 x + 100};
        let result = m::c.g("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x, g: |x| x}"
);

// A name that aliases a generic val and is written holds one instance of
// the val, its own: what is written decides it, for every use.
run!(
    written_alias_of_a_struct_of_functions_val_is_one_instance,
    refused("type mismatch"),
    "/test.gx" => r#"
        mod m;
        let g = m::c;
        let h = |v| { g <- v; v };
        let w = h({f: |x: i64| -> i64 x + 100});
        let result: string = g.f("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
);

run!(
    written_alias_of_a_struct_of_functions_val_holds_its_writes,
    prints(r#"["s", i64:1]"#),
    "/test.gx" => r#"
        mod m;
        let y = never();
        let g = m::c;
        g <- {f: |x| { y <- x; x }};
        let result = (g.f("s"), g.f(1))
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
; FuseExpect::None);

run!(
    written_alias_of_a_struct_of_functions_val_at_one_type,
    is(Value::I64(1)),
    "/test.gx" => r#"
        mod m;
        let g = m::c;
        g <- {f: |x: i64| -> i64 x};
        let result = g.f(1)
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
; FuseExpect::None);

run!(
    written_alias_of_a_function_val_is_one_instance,
    refused("type mismatch"),
    "/test.gx" => r#"
        mod m;
        let g = m::f;
        let h = |v| { g <- v; v };
        let w = h(|x: i64| -> i64 x + 100);
        let result: string = g("s")
    "#,
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let f = |x| x"
);

// A val typed by a typedef of a function type is generic per use as a val
// typed by the function type is: each read copies it.
run!(
    typedef_val_annotated_read_decides_nothing,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => r#"
        mod m;
        let g: fn(x: i64) -> i64 = m::f;
        let result = (g(1), m::f("s"))
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
; FuseExpect::Jit);

run!(
    write_through_a_typedef_val_after_an_annotated_read_refused,
    refused("type mismatch F<'a> does not contain fn(x: i64) -> i64"),
    "/test.gx" => r#"
        mod m;
        let s: string = m::f("s");
        let g: fn(x: i64) -> i64 = m::f;
        m::f <- |x: i64| -> i64 x + 1;
        let result = (g(1), s)
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
);

run!(
    write_through_a_typedef_val_after_a_call_takes_it_refused,
    refused("type mismatch F<'a> does not contain fn(x: i64) -> i64"),
    "/test.gx" => r#"
        mod m;
        let s: string = m::f("s");
        let app = |h: fn(x: i64) -> i64, v| h(v);
        let k = app(m::f, 1);
        m::f <- |x: i64| -> i64 x + 1;
        let result = (k, s)
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val f: F<'a>",
    "/test/m.gx" => "let f = |x| x"
);

run!(
    struct_of_typedef_val_annotated_read_decides_nothing,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => r#"
        mod m;
        let g: {f: fn(x: i64) -> i64} = m::c;
        let result = (g.f(1), m::c.f("s"))
    "#,
    "/test/m.gxi" => "type F<'a> = fn(x: 'a) -> 'a; val c: {f: F<'a>}",
    "/test/m.gx" => "let c = {f: |x| x}"
; FuseExpect::None);

// A writable reference to a place of a generic val is refused only where
// the place's type holds one of the val's variables.
run!(
    mut_ref_to_a_plain_field_of_a_generic_val,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => r#"
        mod m;
        let r = &mut m::c.n;
        *r <- 5;
        let result = (m::c.f(1), m::c.f("s"))
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, n: i64}",
    "/test/m.gx" => "let c = {f: |x| x, n: 0}"
; FuseExpect::None);

run!(
    mut_ref_to_a_function_field_of_a_generic_val_refused,
    refused("m::c is generic, and a writable reference to it would let a write narrow it"),
    "/test.gx" => r#"
        mod m;
        let r = &mut m::c.f;
        *r <- |x: i64| -> i64 x + 100;
        let result = m::c.f("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, n: i64}",
    "/test/m.gx" => "let c = {f: |x| x, n: 0}"
);

// A part of a generic val that holds none of its variables narrows
// nothing: a writable reference to it checks.
run!(
    mut_ref_to_a_monomorphic_field_of_a_generic_val,
    is(Value::I64(5)),
    "/test.gx" => r#"
        mod m;
        let r = &mut m::c.n;
        *r <- 5;
        let result = select m::c.n { 0 => never(), n => n }
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, n: i64}",
    "/test/m.gx" => "let c = {f: |x| x, n: 0}"
; FuseExpect::None);

// A val whose signature and implementation hold unions of open variables
// over shared parameters is judged at once: each of the implementation's
// variables may stand for a union of the signature's.
run!(
    a_val_over_three_unions_of_shared_variables_checks_at_once,
    prints("i64:0"),
    "/test.gx" => "mod m; let result = m::f(1, [1], [1], [1])",
    "/test/m.gxi" => r#"type U<'a, 'b, 'c, 'd, 'e, 'f> = [Array<'a>, Array<'b>, Array<'c>, Array<'d>, Array<'e>, Array<'f>];
type V<'a, 'b, 'c, 'd, 'e, 'f> = [Array<'a>, Array<'b>, Array<'c>, Array<'d>, Array<'e>, Array<'f>];
val f: fn(w: 'g, x: U<'a, 'b, 'c, 'd, 'e, 'f>, y: U<'a, 'b, 'c, 'd, 'e, 'f>, z: U<'a, 'b, 'c, 'd, 'e, 'f>) -> i64;"#,
    "/test/m.gx" => r#"let f = |w: 'p1, x: V<'p1, 'p2, 'p3, 'p4, 'p5, 'p6>, y: V<'p2, 'q2, 'q3, 'q4, 'q5, 'q6>, z: V<'p3, 'r2, 'r3, 'r4, 'r5, 'r6>| 0"#;
    FuseExpect::Jit
);

// A val's bound that crosses nine names is the implementation's bound
// written out.
run!(
    a_val_bound_through_nine_names_holds_the_implementation,
    prints("i64:1"),
    "/test.gx" => "mod m; let result = 1",
    "/test/m.gxi" => r#"type T0<'a> = {v: 'a};
type T1<'a> = {v: T0<'a>};
type T2<'a> = {v: T1<'a>};
type T3<'a> = {v: T2<'a>};
type T4<'a> = {v: T3<'a>};
type T5<'a> = {v: T4<'a>};
type T6<'a> = {v: T5<'a>};
type T7<'a> = {v: T6<'a>};
type T8<'a> = {v: T7<'a>};
type T9<'a> = {v: T8<'a>};
val f: fn<'b: T9<'a>>(x: 'b) -> 'b;"#,
    "/test/m.gx" => r#"let f = 'q: {v: {v: {v: {v: {v: {v: {v: {v: {v: {v: ['p, null]}}}}}}}}}} |x: 'q| -> 'q x"#;
    FuseExpect::None
);

// An implementation's types are checked against the interface where the
// body's own type names resolve: a val whose implementation writes a type
// the body declares matches, in a static module and in a dynamic one.
run!(
    val_implemented_over_a_type_the_body_declares,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::f(1)",
    "/test/m.gxi" => "val f: fn(x: i64) -> i64;",
    "/test/m.gx" => "type V = i64;\nlet f = |x: V| x";
    FuseExpect::Jit
);

run!(
    dynamic_val_implemented_over_a_type_the_body_declares,
    r#"{
    let st = mod foo dynamic { sig { val f: fn(x: i64) -> i64 }; source "type V = i64; let f = |x: V| x" };
    select st { error as e => "[e]", null as _ => "ok [foo::f(1)]" }
}"#,
    prints(r#""ok 1""#);
    FuseExpect::Jit
);
