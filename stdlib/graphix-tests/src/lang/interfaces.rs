// Abstract types and interface files (.gxi). An abstract type is nominal:
// values are minted only by `T(..)` and read through `.0` or `T(x)`
// where the definition is visible (design/nominal_abstract_types.md).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
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
            sandbox whitelist [core];
            sig {
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

// A function-typed parameter does not pair the signature's unions before
// the parameters after it have paired their variables.
run!(
    union_pairing_waits_for_later_parameters,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0))),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f(1, |n: i64| n, 1, "s")
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(x: ['a, 'b], g: fn(n: i64) -> i64, y: 'a, z: 'b) -> i64
    "#,
    "/test/inner.gx" => r#"
        let f = |x: ['c, 'd], g: fn(n: i64) -> i64, y: 'd, z: 'c| -> i64 0
    "#
; FuseExpect::Jit);

// A throws mismatch names the types as written, and a bottom throws as
// throwing nothing.
run!(
    throws_mismatch_names_nothing,
    refused("in throws clause: the signature throws nothing, the implementation throws Error<ErrChain<`Zero>>"),
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
    refused("in throws clause: signature has ErrChain<`A>, implementation has ErrChain<`Zero>"),
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
    refused("signature has T, implementation has i64"),
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
    refused("'a: unbound within Number does not contain string"),
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
    |v: Result<&Value>| v.is_err_and(|e| format!("{e:#}").contains("'a: Number")),
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
    refused("signature has T, implementation has i64"),
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
    refused("type variable usage mismatch"),
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
    refused("the impl in module /test/a is not in its interface"),
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
    "/test/m.gx" => "let lt = 'a: Any |x: 'a, y: 'a| -> bool x < y"
; FuseExpect::None);

run!(
    array_any_bound_does_not_imply_discernible,
    refused("the implementation requires 'a: Discernible"),
    "/test.gx" => "mod m; let result = m::eq([1], [2])",
    "/test/m.gxi" => "val eq: fn<'a: Array<Any>>(x: 'a, y: 'a) -> bool;",
    "/test/m.gx" => "let eq = 'a: Array<Any> |x: 'a, y: 'a| -> bool x == y"
; FuseExpect::None);

run!(
    any_bound_does_not_imply_concrete,
    refused("the implementation requires 'b: Concrete"),
    "/test.gx" => "mod m; let result = m::parse(\"1\", 0)",
    "/test/m.gxi" => "val parse: fn<'b: Any>(s: string, d: 'b) -> Result<'b, `ParseError(string)>;",
    "/test/m.gx" => "let parse = 'b: Any |s: string, d: 'b| -> Result<'b, `ParseError(string)> str::parse(s)"
; FuseExpect::None);

// A union of variables pairs with the implementation's however either side
// orders it, as the rest of the signature decides.
run!(
    open_union_pairs_by_the_rest_of_the_signature,
    is(Value::I64(2)),
    "/test.gx" => "mod m; let result = m::f(1, 2)",
    "/test/m.gxi" => "val f: fn(x: ['a, 'b], y: 'a) -> 'a;",
    "/test/m.gx" => "let f = |x: ['b, 'a], y: 'a| -> 'a y"
; FuseExpect::Jit);

// No pairing of the union fits the rest of the signature: refused.
run!(
    open_union_without_a_pairing_is_refused,
    refused("type variable usage mismatch"),
    "/test.gx" => "mod m; let result = m::f(1, 2)",
    "/test/m.gxi" => "val f: fn(x: ['a, 'b], y: 'a, z: 'a) -> 'a;",
    "/test/m.gx" => "let f = |x: ['b, 'a], y: 'a, z: 'b| -> 'a y"
; FuseExpect::None);

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
    refused("type variable usage mismatch"),
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
    refused("type variable usage mismatch: expected string, got i64"),
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
// without trying every pairing of the others.
run!(
    unpairable_union_refused_without_retrying_the_others,
    refused("signature has string, implementation has f64"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "val f: fn(x0: ['a0, 'a1, 'a2, 'a3, 'a4, string], x1: ['b0, 'b1, 'b2, 'b3, 'b4, i64], x2: ['c0, 'c1, 'c2, 'c3, 'c4, i64], x3: ['d0, 'd1, 'd2, 'd3, 'd4, i64]) -> i64;",
    "/test/m.gx" => "let f = |x0: ['a0, 'a1, 'a2, 'a3, 'a4, f64], x1: ['b0, 'b1, 'b2, 'b3, 'b4, i64], x2: ['c0, 'c1, 'c2, 'c3, 'c4, i64], x3: ['d0, 'd1, 'd2, 'd3, 'd4, i64]| -> i64 0"
; FuseExpect::None);

// A mismatch names the implemented type, not its cells.
run!(
    implemented_type_prints_resolved,
    refused("but implemented fn(x: f64) -> f64"),
    "/test.gx" => "mod m; let result = m::add(1)",
    "/test/m.gxi" => "val add: fn(x: i64) -> i64;",
    "/test/m.gx" => "let add = |x| x + 1."
; FuseExpect::None);

// An argument whose kinds differ is named in Graphix syntax.
run!(
    arg_default_mismatch_is_named,
    refused("argument #a: the signature gives it no default, the implementation a default"),
    "/test.gx" => "mod m; let result = m::f(#a: 1)",
    "/test/m.gxi" => "val f: fn(#a: i64) -> i64;",
    "/test/m.gx" => "let f = |#a = 0| -> i64 a"
; FuseExpect::None);

// A variable named only inside another's bound is matched with the
// signature's: its bound must be declared, and when it is, it holds at
// the call.
run!(
    a_bound_inside_a_bound_must_be_declared,
    refused("the implementation requires 'b: Int"),
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
    refused("unbound within Int"),
    "/test.gx" => "mod m; let result = m::f([1.5])",
    "/test/m.gxi" => "val f: fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;",
    "/test/m.gx" => "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x"
; FuseExpect::None);

// A val's own quantifiers are its scheme's, so their bounds name any
// variable; a rank-2 function type inside it is held to its own.
run!(
    a_rank2_bound_in_a_val_names_only_its_own_quantifiers,
    refused("a rank-2 quantifier's bound names 'c, which this function type does not quantify"),
    "/test.gx" => "mod m; let result = m::f(|x| x, 1)",
    "/test/m.gxi" => "val f: fn(k: fn<'a: 'c>(x: 'a) -> 'a, z: 'c) -> 'c;",
    "/test/m.gx" => "let f = |k: fn<'a: i64>(x: 'a) -> 'a, z: i64| k(z)"
; FuseExpect::None);

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

// An implementation that calls a function it is handed at types of its
// own choosing (a rank-2 parameter) does not implement a signature that
// hands it a function at one type.
run!(
    rank2_parameter_does_not_implement_a_monomorphic_one,
    refused("the implementation picks 'b at each call of the function it is handed"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::f(|x: i64| x * 2)
    "#,
    "/test/inner.gxi" => r#"
        val f: fn(g: fn(x: i64) -> i64) -> f64;
    "#,
    "/test/inner.gx" => r#"
        let f = |g: fn<'b: Number>(x: 'b) -> 'b| -> f64 g(2.5)
    "#; FuseExpect::None);

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
    refused("in argument #b"),
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

// A function the implementation is handed is used at any type the
// implementation's own quantifier admits: the interface must hand it
// one at least that polymorphic.
run!(
    interface_rank2_argument_narrower_than_its_use_refused,
    refused("the implementation uses the function it is handed at any 'c, where the signature hands it one at 'b: Number only"),
    "/test.gx" => "mod inner; let result = inner::f(|x| x + x)",
    "/test/inner.gxi" => "val f: fn(g: fn<'b: Number + Singleton>(x: 'b) -> 'b) -> (i64, string)",
    "/test/inner.gx" => "let f = |g: fn<'c: Any>(x: 'c) -> 'c| (g(1), g(\"s\"))"
);

run!(
    interface_rank2_argument_wider_than_its_use,
    prints("[i64:1, f64:2.5]"),
    "/test.gx" => "mod inner; let result = inner::f(|x| x)",
    "/test/inner.gxi" => "val f: fn(g: fn<'b: Any>(x: 'b) -> 'b) -> (i64, f64)",
    "/test/inner.gx" => "let f = |g: fn<'c: Number + Singleton>(x: 'c) -> 'c| (g(1), g(2.5))"
; FuseExpect::Jit);

// A signature's variable is every caller's to pick: it meets an
// implementation bound only through its own.
run!(
    interface_unbounded_variable_under_a_bounded_implementation_refused,
    refused("signature has concrete type [null, 'b], which the implementation constraint [null, Number] does not admit"),
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

// An abstract type's parameter, and a typedef that grows its parameters
// as it recurses, stand outside function types.
run!(
    val_variable_as_an_abstract_parameter_refused,
    refused("val w has its type variable 'a outside a function type"),
    "/test.gx" => "mod m; let result = m::w.0.f(1)",
    "/test/m.gxi" => "type W<'a> = Abstract<{f: fn(x: 'a) -> 'a}>; val w: W<'a>",
    "/test/m.gx" => "let w = W({f: |x| x})"
);

run!(
    val_variable_under_a_growing_typedef_refused,
    refused("val t has its type variable 'a outside a function type"),
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
            sandbox unrestricted;
            sig { val v: Array<'a>; val set: fn(x: Array<string>) -> null };
            source "let v = never(); let set = |x| { v <- x; null }"
        };
        let w = d::set(["hello"]);
        let a: Array<i64> = d::v;
        let result = a
    "#
);

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
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
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
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
    "/test.gx" => "mod m; let result = m::set(|x: i64| -> i64 x + 100)",
    "/test/m.gxi" => "val set: fn(g: fn(x: 'a) -> 'a) -> null",
    "/test/m.gx" => "let f = |x| x; let set = |g| { f <- g; null }"
);

run!(
    rank2_val_over_an_outer_binding_refused,
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
    "/test.gx" => "mod m; let result = (m::h(1), m::h(\"s\"))",
    "/test/m.gxi" => "val h: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let h = |x| { y <- x; x }"
);

run!(
    rank2_val_result_over_an_outer_binding_refused,
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
    "/test.gx" => "mod m; let k = m::mk(0); let result = (k(1), k(\"s\"))",
    "/test/m.gxi" => "val mk: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let mk = |z| |x| { y <- x; x }"
);

run!(
    rank2_val_result_over_its_own_variable,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let k = m::mk(0); let result = (k(1), k(\"s\"))",
    "/test/m.gxi" => "val mk: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = |z| |x| x"
; FuseExpect::None);

run!(
    struct_of_functions_val_over_an_outer_binding_refused,
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
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
    refused("where the val declares 'a, a type each use picks, the implementation has one type all uses share"),
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
    refused("that of y, a binding outside its let: declare the val at one type, or give y a type that holds every 'a"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let f = |x| { y <- x; x }"
);

run!(
    val_over_an_outer_binding_of_every_type,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y: Any = never(); let f = |x| { y <- x; x }"
; FuseExpect::None);

run!(
    written_val_over_an_outer_binding_names_one_type,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").ends_with(
        "the implementation has one type all uses share, that of y, a binding \
         outside its let: declare the val at one type"
    )),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let y = never(); let f = |x| { y <- x; x }; f <- |x| { y <- x; x }"
);

// An implementation that is no lambda (a call's result) has the types its
// value fixed: the way out is a lambda, not a quantified annotation, which
// the value would not fit.
run!(
    val_over_a_call_result_names_a_lambda,
    refused("the implementation has one type all uses share: declare the val at one type, or make the implementation a lambda over variables of its own"),
    "/test.gx" => "mod m; let result = (m::f(1), m::f(\"s\"))",
    "/test/m.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/m.gx" => "let mk = |u: i64| |x| x; let f = mk(0)"
);

// A val whose type is no function, implemented by no literal of lambdas,
// is told to be one.
run!(
    data_val_over_no_literal_names_a_literal_of_lambdas,
    refused("the implementation has one type all uses share: declare the val at one type, or make the implementation a literal of lambdas over variables of their own"),
    "/test.gx" => "mod m; let result = m::v",
    "/test/m.gxi" => "val v: Array<fn(x: 'a) -> 'a>",
    "/test/m.gx" => "let v = never()"
);

run!(
    rank2_val_over_a_written_let_names_the_annotation,
    refused("annotate the implementation's let with a quantified type (fn<'a: ..>)"),
    "/test.gx" => "mod m; let result = (m::h(1), m::h(\"s\"))",
    "/test/m.gxi" => "val h: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let h = |x| x; h <- |x| x"
);

run!(
    rank2_val_over_a_written_let_at_its_quantified_type,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; let result = (m::h(1), m::h(\"s\"))",
    "/test/m.gxi" => "val h: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let h: fn<'a: Any>(x: 'a) -> 'a = |x| x; h <- |x| x"
; FuseExpect::None);

// The refusal names the variable first by name, whichever the walk met.
run!(
    val_over_an_open_let_names_its_first_variable,
    refused("whose 'a each use picks"),
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
    refused("the check decides a type the code around the module left open: annotate `z`"),
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

// A function type in a val quantifies its own variables: one standing
// whole where the implementation has a cell no use copies is decided there.
run!(
    rank2_val_over_an_open_let,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => "mod m; m::e <- |x| x; let result = (m::e(1), m::e(\"s\"))",
    "/test/m.gxi" => "val e: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let e = never()"
; FuseExpect::None);

run!(
    rank2_val_parameter_over_a_written_let,
    is(Value::I64(1)),
    "/test.gx" => "mod m; let result = m::app(|x| x)",
    "/test/m.gxi" => "val app: fn(f: fn<'a: Any>(x: 'a) -> 'a) -> i64",
    "/test/m.gx" => "let g = never(); let app = |f| { g <- f; 1 }"
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
    refused("made generic by an earlier compile, so what is written to it must be as general, and fn(x: [i64, null]) -> i64 is not"),
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
    refused("a::f was made generic by an earlier compile, and a writable reference to it would let a write narrow it"),
    "/test.gx" => "mod a; let rf = &mut a::f; let result = a::f(1)",
    "/test/a.gxi" => "val f: fn(x: 'a) -> 'a",
    "/test/a.gx" => "let f = |x| x"
);

// A val's own quantifiers are its scheme's, which each use copies, as an
// unquantified val's variables are.
run!(
    mut_ref_to_a_quantified_val_refused,
    refused("a::f was made generic by an earlier compile, and a writable reference to it would let a write narrow it"),
    "/test.gx" => r#"
        mod a;
        let set = |r: &mut fn(x: i64) -> i64, v| { *r <- v; null };
        let z = set(&mut a::f, |x: i64| -> i64 x + 100);
        let result = a::f("s")
    "#,
    "/test/a.gxi" => "val f: fn<'b: Any>(x: 'b) -> 'b",
    "/test/a.gx" => "let f = |x| x"
);

// A rank-2 function type in a val's data is one type, which a writable
// reference holds whole.
run!(
    mut_ref_to_a_val_holding_a_quantified_function,
    prints("[i64:1, \"s\"]"),
    "/test.gx" => r#"
        mod a;
        let set = |r: &mut {f: fn<'b: Any>(x: 'b) -> 'b}, v| { *r <- v; null };
        let z = set(&mut a::c, {f: |x| x});
        let result = (a::c.f(1), a::c.f("s"))
    "#,
    "/test/a.gxi" => "val c: {f: fn<'b: Any>(x: 'b) -> 'b}",
    "/test/a.gx" => "let c: {f: fn<'b: Any>(x: 'b) -> 'b} = {f: |x| x}"
; FuseExpect::None);

// A val whose type holds a variable is generic wherever its function types
// stand (a typedef, a struct, an array): what is written through it must
// be as general, and a writable reference to it is refused.
run!(
    write_through_a_typedef_val_refused,
    refused("f is F<'a: unbound>, made generic by an earlier compile, so what is written to it must be as general"),
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

run!(
    write_through_a_struct_of_functions_val_refused,
    refused("made generic by an earlier compile, so what is written to it must be as general, and { f: fn(x: 'a: unbound) -> 'a: unbound } is not"),
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
    refused("v is Array<fn(x: 'a: unbound) -> 'a: unbound>, made generic by an earlier compile, so what is written to it must be as general"),
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
    refused("m::c was made generic by an earlier compile, and a writable reference to it would let a write narrow it"),
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
    refused("it holds a type variable of the code around the write"),
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
    refused("it holds a type variable of the code around the write"),
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
    refused("g: fn(x: 'a: unbound) -> 'a: unbound }, made generic by an earlier compile, so what is written to it must be as general"),
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
    refused("type mismatch"),
    "/test.gx" => r#"
        mod m;
        let y = never();
        let g = m::c;
        g <- {f: |x| { y <- x; x }};
        let result = (g.f("s"), g.f(1))
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a}",
    "/test/m.gx" => "let c = {f: |x| x}"
);

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
    refused("f is F<'a: unbound>, made generic by an earlier compile, so what is written to it must be as general"),
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
    refused("f is F<'a: unbound>, made generic by an earlier compile, so what is written to it must be as general"),
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
    refused("m::c was made generic by an earlier compile, and a writable reference to it would let a write narrow it"),
    "/test.gx" => r#"
        mod m;
        let r = &mut m::c.f;
        *r <- |x: i64| -> i64 x + 100;
        let result = m::c.f("s")
    "#,
    "/test/m.gxi" => "val c: {f: fn(x: 'a) -> 'a, n: i64}",
    "/test/m.gx" => "let c = {f: |x| x, n: 0}"
);
