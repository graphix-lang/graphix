// Tests for abstract types and interface files (.gxi)
//
// Abstract types are types declared in an interface without a concrete definition.
// The implementation file must provide a concrete definition for each abstract type.
// Abstract types are opaque - the caller cannot see the concrete type.

use graphix_package_core::run;
use anyhow::Result;
use netidx::publisher::Value;

// =============================================================================
// Basic Abstract Type Tests
// =============================================================================

// Basic abstract type: interface declares abstract type, implementation provides concrete
run!(
    abstract_type_basic,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get(inner::make(42))
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(i64) -> T;
        val get: fn(T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = i64;
        let make = |x: i64| -> T x;
        let get = |t: T| -> i64 t
    "#
);

// Abstract type implemented as a struct
run!(
    abstract_type_struct_impl,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "hello"),
    "/test.gx" => r#"
        mod inner;
        let result = inner::get_name(inner::make("hello"))
    "#,
    "/test/inner.gxi" => r#"
        type Handle;
        val make: fn(string) -> Handle;
        val get_name: fn(Handle) -> string
    "#,
    "/test/inner.gx" => r#"
        type Handle = { value: string };
        let make = |x: string| -> Handle { value: x };
        let get_name = |h: Handle| h.value
    "#
);

// Interface without abstract types (regression test - should still work)
run!(
    interface_no_abstract_types,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(30))),
    "/test.gx" => r#"
        mod inner;
        let result = inner::add(10, 20)
    "#,
    "/test/inner.gxi" => r#"
        type Point = { x: i64, y: i64 };
        val add: fn(i64, i64) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        let add = |a: i64, b: i64| -> i64 a + b
    "#
);

// =============================================================================
// Multiple Abstract Types
// =============================================================================

// Multiple abstract types in same interface
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
        val make_a: fn(i64) -> A;
        val make_b: fn(i64) -> B;
        val combine: fn(A, B) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type A = { x: i64 };
        type B = { y: i64 };
        let make_a = |x: i64| -> A { x };
        let make_b = |y: i64| -> B { y };
        let combine = |a: A, b: B| -> i64 a.x + b.y
    "#
);

// Two modules using same abstract type name with different definitions
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
        val make: fn(i64) -> T;
        val get: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/mod_a.gx" => r#"
        type T = { value: i64 };
        let make = |x: i64| -> T { value: x };
        let get = |t: T| -> i64 t.value
    "#,
    "/test/mod_b.gxi" => r#"
        type T;
        val make: fn(i64) -> T;
        val get: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/mod_b.gx" => r#"
        type T = i64;
        let make = |x: i64| -> T x;
        let get = |t: T| -> i64 t
    "#
);

// Abstract type used in exported type definition
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
        val make_pair: fn(i64, string) -> Pair;
        val get_first: fn(Pair) -> i64
    "#,
    "/test/inner.gx" => r#"
        type First = i64;
        let make_pair = |a: i64, b: string| -> Pair { first: a, second: b };
        let get_first = |p: Pair| -> i64 p.first
    "#
);

// =============================================================================
// Abstract Types in Compound Types
// =============================================================================

// Abstract type in variant (exported type references abstract type)
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
        val some: fn(i64) -> Option;
        val get_or_default: fn(Option, i64) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = { value: i64 };
        let some = |x: i64| -> Option `Some({ value: x });
        let get_or_default = |opt: Option, default: i64| -> i64 select opt {
            `Some(t) => t.value,
            `None => default
        }
    "#
);

// Abstract type in tuple
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
        val make_pair: fn(i64, i64) -> Pair;
        val sum_pair: fn(Pair) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Elem = i64;
        let make_pair = |a: i64, b: i64| -> Pair (a, b);
        let sum_pair = |p: Pair| -> i64 p.0 + p.1
    "#
);

// Abstract type in array
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
        val make_array: fn(Array<i64>) -> Array<Elem>;
        val sum_array: fn(Array<Elem>) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Elem = i64;
        let make_array = |arr: Array<i64>| -> Array<Elem> arr;
        let sum_array = |arr: Array<Elem>| -> i64 array::fold(arr, 0, |acc, x| acc + x)
    "#
);

// =============================================================================
// Abstract Type used in Recursive Type
// =============================================================================

// Abstract type used in recursive type
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
        type List = [`Cons(Elem, List), `Nil];
        val cons: fn(i64, List) -> List;
        val nil: fn() -> List;
        val sum: fn(List) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Elem = i64;
        let cons = |x: i64, rest: List| -> List `Cons(x, rest);
        let nil = || -> List `Nil;
        let rec sum = |list: List| -> i64 select list {
            `Cons(x, rest) => x + sum(rest),
            `Nil => 0
        }
    "#
);

// =============================================================================
// Abstract Types with ByRef
// =============================================================================

// Abstract type with byref parameter - collects values to verify update
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
        val make: fn(i64) -> Counter;
        val get: fn(Counter) -> i64;
        val increment: fn(&Counter) -> null throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Counter = i64;
        let make = |x: i64| -> Counter x;
        let get = |c: Counter| -> i64 c;
        let increment = |c: &Counter| -> null { *c <- once(*c) + 1; null }
    "#
);

// =============================================================================
// Nested Modules with Abstract Types
// =============================================================================

// Nested module with abstract type
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
        val make: fn(i64) -> T;
        val get: fn(T) -> i64
    "#,
    "/test/outer/inner.gx" => r#"
        type T = { v: i64 };
        let make = |x: i64| -> T { v: x };
        let get = |t: T| -> i64 t.v
    "#
);

// =============================================================================
// Dynamic Modules with Abstract Types
// =============================================================================

// Dynamic module with abstract type in signature
run!(
    abstract_type_dynamic_module,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(84))),
    "/test.gx" => r#"
        let source = "
            type T = i64;
            let make = |x: i64| -> T x;
            let double = |t: T| -> i64 t + t
        ";
        net::publish("/local/dyn_test", source)?;
        let status = mod dyn dynamic {
            sandbox whitelist [core];
            sig {
                type T;
                val make: fn(i64) -> T;
                val double: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
            };
            source cast<string>(net::subscribe("/local/dyn_test")$)$
        };
        let result = select status {
            error as e => never(dbg(e)),
            null as _ => dyn::double(dyn::make(42))
        }
    "#
);

// =============================================================================
// Error Cases
// =============================================================================

// Error: missing concrete definition for abstract type
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
);

// Error: implementation still declares abstract type (no concrete def)
run!(
    abstract_type_still_abstract,
    |v: Result<&Value>| v.is_err(),
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
);

// Error: signature type mismatch (function returns wrong type)
run!(
    abstract_type_sig_mismatch,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(i64) -> T
    "#,
    "/test/inner.gx" => r#"
        type T = string;
        let make = |x: i64| -> i64 x
    "#
);

// Error: abstract type parameter constraint mismatch
run!(
    abstract_type_constraint_mismatch,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T<'a: Number>;
        val make: fn('a) -> T<'a>
    "#,
    "/test/inner.gx" => r#"
        type T<'a> = { val: 'a };
        let make = |x: 'a| -> T<'a> { val: x }
    "#
);

// Abstract type constraint is automatically enforced on functions
// The constraint on type Box<'a: Number> should propagate to wrap/unwrap
// without needing to repeat the constraint in the val declarations
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
        val wrap: fn('a) -> Box<'a>;
        val unwrap: fn(Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a: Number> = { value: 'a };
        let wrap = |x: 'a| -> Box<'a> { value: x };
        let unwrap = |b: Box<'a>| -> 'a b.value
    "#
);

// Error: abstract type constraint violation - string doesn't satisfy Number
// The constraint from type Box<'a: Number> should reject non-Number types
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
        val wrap: fn('a) -> Box<'a>;
        val unwrap: fn(Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a: Number> = { value: 'a };
        let wrap = |x: 'a| -> Box<'a> { value: x };
        let unwrap = |b: Box<'a>| -> 'a b.value
    "#
);

// Error: extra type parameter in implementation
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
        type T<'a, 'b> = ('a, 'b);
        let x = 42
    "#
);

// Error: function argument type doesn't match abstract type
// Signature says get takes T, but implementation's concrete type doesn't match
run!(
    abstract_type_wrong_arg,
    |v: Result<&Value>| v.is_err(),
    "/test.gx" => r#"
        mod inner;
        let result = 0
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val get: fn(T) -> i64
    "#,
    "/test/inner.gx" => r#"
        type T = string;
        let get = |t: i64| -> i64 t
    "#
);

// =============================================================================
// Parameterized Abstract Types
// =============================================================================

// Basic parameterized abstract type
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
        val wrap: fn('a) -> Box<'a>;
        val unwrap: fn(Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a> = { value: 'a };
        let wrap = |x: 'a| -> Box<'a> { value: x };
        let unwrap = |b: Box<'a>| -> 'a b.value
    "#
);

// Parameterized abstract type instantiated with different concrete types
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
        val wrap: fn('a) -> Box<'a>;
        val unwrap: fn(Box<'a>) -> 'a
    "#,
    "/test/inner.gx" => r#"
        type Box<'a> = { value: 'a };
        let wrap = |x: 'a| -> Box<'a> { value: x };
        let unwrap = |b: Box<'a>| -> 'a b.value
    "#
);

// Parameterized abstract type with constraint - use concrete type in interface
// Note: Constrained type parameters in val declarations use a different syntax.
// This test uses a concrete instantiation to sidestep that complexity.
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
        val wrap: fn(i64) -> IntWrapper;
        val double: fn(IntWrapper) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type NumWrapper<'a: Number> = 'a;
        let wrap = |x: i64| -> IntWrapper x;
        let double = |w: IntWrapper| -> i64 w + w
    "#
);

// Parameterized abstract type in nested position (Array of Box)
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
        val wrap: fn('a) -> Box<'a>;
        val sum_boxes: fn(IntBoxArray) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Box<'a> = { value: 'a };
        let wrap = |x: 'a| -> Box<'a> { value: x };
        let sum_boxes = |boxes: IntBoxArray| -> i64
            array::fold(boxes, 0, |acc, b| acc + b.value)
    "#
);

// Parameterized abstract type with two type parameters
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
        val make: fn('a, 'b) -> Pair<'a, 'b>;
        val get_first: fn(Pair<'a, 'b>) -> 'a;
        val get_second: fn(Pair<'a, 'b>) -> 'b
    "#,
    "/test/inner.gx" => r#"
        type Pair<'a, 'b> = { first: 'a, second: 'b };
        let make = |a: 'a, b: 'b| -> Pair<'a, 'b> { first: a, second: b };
        let get_first = |p: Pair<'a, 'b>| -> 'a p.first;
        let get_second = |p: Pair<'a, 'b>| -> 'b p.second
    "#
);

// =============================================================================
// Abstract Types in Map
// =============================================================================

// Abstract type as Map key
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
        val make_key: fn(i64) -> Key;
        val make_map: fn() -> KeyMap;
        val lookup: fn(KeyMap, Key) -> string throws Error<ErrChain<`MapKeyError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type Key = i64;
        let make_key = |x: i64| -> Key x;
        let make_map = || -> KeyMap {42 => "found", 99 => "other"};
        let lookup = |m: KeyMap, k: Key| m{k}?
    "#
);

// Abstract type as Map value
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
        val get: fn(ValMap, string) -> Val throws Error<ErrChain<`MapKeyError(string)>>;
        val unwrap: fn(Val) -> i64
    "#,
    "/test/inner.gx" => r#"
        type Val = { inner: i64 };
        let make_map = || -> ValMap {"key" => { inner: 42 }};
        let get = |m: ValMap, k: string| -> Val m{k}?;
        let unwrap = |v: Val| -> i64 v.inner
    "#
);

// Abstract types as both Map key and value
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
        val make_key: fn(string) -> K;
        val make_map: fn(K, i64) -> KVMap;
        val lookup: fn(KVMap, K) -> V throws Error<ErrChain<`MapKeyError(string)>>;
        val get_val: fn(V) -> i64
    "#,
    "/test/inner.gx" => r#"
        type K = { name: string };
        type V = i64;
        let make_key = |s: string| -> K { name: s };
        let make_map = |k: K, n: i64| -> KVMap {k => n};
        let lookup = |m: KVMap, k: K| -> V m{k}?;
        let get_val = |v: V| -> i64 v
    "#
);

// =============================================================================
// Abstract Types in Throws Clause
// =============================================================================

// Abstract type as error payload in throws clause
run!(
    abstract_type_in_throws,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
    "/test.gx" => r#"
        mod inner;
        let result = try inner::risky(42)
            catch(e) => {
                let chain = e.0;
                select chain.error {
                    `CustomError(_) => 0
                }
            }
    "#,
    "/test/inner.gxi" => r#"
        type ErrPayload;
        val risky: fn(i64) -> i64 throws Error<ErrChain<`CustomError(ErrPayload)>>
    "#,
    "/test/inner.gx" => r#"
        type ErrPayload = { code: i64, msg: string };
        let risky = |x: i64| -> i64 x
    "#
);

// Abstract type used with a function that has throws clause
// This tests that functions returning abstract types can be declared with throws
run!(
    abstract_type_with_throws_clause,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42))),
    "/test.gx" => r#"
        mod inner;
        let result = try inner::get_value(inner::make(1))
            catch(e) => {
                let chain = e.0;
                select chain.error {
                    `ArithError(_) => -1
                }
            }
    "#,
    "/test/inner.gxi" => r#"
        type T;
        val make: fn(i64) -> T;
        val get_value: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/inner.gx" => r#"
        type T = { value: i64 };
        let make = |x: i64| -> T { value: x };
        let get_value = |t: T| -> i64 t.value + 41
    "#
);

// =============================================================================
// Cross-Module Abstract Type Usage
// =============================================================================

// NOTE: Cross-module abstract type references (where one module's interface
// references another module's abstract type) require careful module path
// resolution. The following tests demonstrate simpler patterns that work.

// Two modules with separate abstract types, combined at the caller level
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
        val make: fn(i64) -> T;
        val get: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/mod_a.gx" => r#"
        type T = { value: i64 };
        let make = |x: i64| -> T { value: x };
        let get = |t: T| -> i64 t.value
    "#,
    "/test/mod_b.gxi" => r#"
        type T;
        val make: fn(i64) -> T;
        val get: fn(T) -> i64 throws Error<ErrChain<`ArithError(string)>>
    "#,
    "/test/mod_b.gx" => r#"
        type T = i64;
        let make = |x: i64| -> T x;
        let get = |t: T| -> i64 t
    "#
);
