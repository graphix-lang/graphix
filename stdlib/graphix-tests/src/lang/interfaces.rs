// Abstract types and interface files (.gxi). An abstract type is nominal:
// values are minted only by `T(..)` and read through `.0` or `T(x)`
// where the definition is visible (design/nominal_abstract_types.md).

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

// The interface declares an abstract type; the implementation defines it.
run!(
    abstract_type_basic,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
// ASPIRE: Jit — cross-module struct-arg / string-return fn.
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// An interface without abstract types.
run!(
    interface_no_abstract_types,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(30))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(15))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(142))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(77))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// An abstract type in a variant.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    abstract_type_in_variant,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// An abstract type in a tuple.
run!(
    abstract_type_in_tuple,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(15))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(6))),
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
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    abstract_type_recursive,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(6))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// An abstract type behind a byref parameter.
run!(
    abstract_type_byref,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(43)] => true,
            _ => false,
        },
        _ => false,
    },
    "/test.gx" => r#"
        mod inner;
        let counter = inner::make(42);
        inner::increment(&counter);
        let result = array::group(inner::get(counter), |n, _| n == 2)
    "#,
    "/test/inner.gxi" => r#"
        type Counter;
        val make: fn(x: i64) -> Counter;
        val get: fn(c: Counter) -> i64;
        val increment: fn(c: &Counter) -> null     "#,
    "/test/inner.gx" => r#"
        type Counter = Abstract<i64>;
        let make = |x: i64| -> Counter Counter(x);
        let get = |c: Counter| -> i64 c.0;
        let increment = |c: &Counter| -> null { *c <- Counter(once(*c).0 + 1); null }
    "#);

// A nested module with an abstract type.
run!(
    abstract_type_nested_module,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(99))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(84))),
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
; graphix_package_core::testing::FuseExpect::Jit);

// Error: missing concrete definition for an abstract type.
run!(
    abstract_type_missing_definition,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val x: T
    "#,
    "/test/inner.gx" => r#"
        let x = 42
    "#
; graphix_package_core::testing::FuseExpect::None);

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
; graphix_package_core::testing::FuseExpect::Jit);

// Error: the function returns the wrong type.
run!(
    abstract_type_sig_mismatch,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// Error: abstract type parameter constraint mismatch.
run!(
    abstract_type_constraint_mismatch,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// The constraint on `Box<'a: Number>` propagates to wrap/unwrap without
// repeating it in the val declarations.
// ASPIRE: Jit — constrained abstract-type fn.
run!(
    abstract_type_constraint_auto_enforced,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
; graphix_package_core::testing::FuseExpect::Jit);

// Error: string does not satisfy `Box<'a: Number>`.
run!(
    abstract_type_constraint_auto_enforced_error,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// Error: extra type parameter in the implementation.
run!(
    abstract_type_extra_param,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// Error: the implementation's argument type does not match the
// signature's abstract type.
run!(
    abstract_type_wrong_arg,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// A parameterized abstract type.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    abstract_type_parameterized_basic,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// A parameterized abstract type at different concrete types.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    abstract_type_parameterized_multi_instantiation,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(47))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// A parameterized abstract type with a constraint, instantiated
// concretely in the interface.
run!(
    abstract_type_parameterized_constrained,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(84))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// A parameterized abstract type in nested position (Array of Box).
run!(
    abstract_type_parameterized_nested,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(6))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// Two type parameters.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    abstract_type_parameterized_two_params,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(47))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(100))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
    "#; graphix_package_core::testing::FuseExpect::Jit);

// An abstract type with a throwing function whose `?` is caught by the
// enclosing catch.
run!(
    abstract_type_with_throws_clause,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
    "/test.gx" => r#"
        mod inner;
        let result = {
            catch(e) {
                let chain = e.0;
                select chain.error {
                    `ArrayIndexError(_) => -1
                }
            };
            inner::get_value(inner::make(1))
        }
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
            a[0]?
        }
    "#; graphix_package_core::testing::FuseExpect::Jit);

// Two modules with separate abstract types, combined by the caller.
run!(
    abstract_type_two_modules_combined,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(15))),
    "/test.gx" => r#"
        mod mod_a;
        mod mod_b;
        let a = mod_a::make(10);
        let b = mod_b::make(5);
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

// Nominal abstract types: the tag at runtime.

// A hidden abstract's constructor, payload and pattern are usable
// only where the definition is visible: the caller gets a compile
// error for all three.
run!(
    abstract_construct_outside_refused,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

run!(
    abstract_payload_outside_refused,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// A type hidden by the interface must be Abstract<..> (or Rust-backed):
// hiding a transparent alias is the two-view case itself.
run!(
    abstract_hidden_alias_refused,
    |v: Result<&Value>| v.is_err(),
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
; graphix_package_core::testing::FuseExpect::None);

// A PUBLIC newtype: the interface exports the body, so anyone can
// construct and read it — nominal without being hidden.
run!(
    abstract_public_newtype,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(43))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1))),
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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
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
; graphix_package_core::testing::FuseExpect::None);
