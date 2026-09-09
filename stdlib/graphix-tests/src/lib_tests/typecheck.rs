use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Deserialization builtins require a concrete result type at compile time.

// json::read without a concrete return type is a compile error.
run!(json_no_type, r#"json::read("42")"#, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

// toml::read without a concrete return type is a compile error.
run!(toml_no_type, r#"toml::read("x = 42")"#, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

// pack::read without a concrete return type is a compile error.
run!(pack_no_type, r#"pack::read(pack::write_bytes(42)$)"#, |v: Result<&Value>| v
    .is_err(); graphix_package_core::testing::FuseExpect::None);

// str::parse without a concrete return type is a compile error.
run!(str_parse_no_type, r#"str::parse("42")"#, |v: Result<&Value>| v.is_err(); graphix_package_core::testing::FuseExpect::None);

// json::read with a concrete type.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(json_typed_i64, r#"{let v: i64 = json::read("42")?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; graphix_package_core::testing::FuseExpect::None);

// json::read with a struct type.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    json_typed_struct,
    r#"{
    type P = {x: i64, y: string};
    let v: P = json::read(json::write_str({x: 1, y: "a"})$)?;
    v.x
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) }; graphix_package_core::testing::FuseExpect::Jit);

// Late binding: deserializers passed through higher-order functions.

// A deserializer stored in a variable, called with a concrete type.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    late_bind_var,
    r#"{
    let decoder = json::read;
    let v: i64 = decoder("42")?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::None);

// A function wrapping a deserializer with an explicit return type.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    late_bind_wrap,
    r#"{
    let decode = |data: [string, bytes]| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(data);
    let v: i64 = decode(json::write_str(99)$)?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(99))) }; graphix_package_core::testing::FuseExpect::None);

// Multiple calls to the same typed wrapper.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    late_bind_multi_json,
    r#"{
    let apply = |f: fn(x: [string, bytes]) -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>, data| f(data);
    let a: i64 = apply(json::read, json::write_str(42)$)?;
    let b: i64 = apply(json::read, json::write_str(42)$)?;
    a + b
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(84))) }; graphix_package_core::testing::FuseExpect::Jit);

// json + pack through one typed call site over bytes; the error types
// unify to the superset.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    late_bind_mixed_deser,
    r#"{
    let apply = |f: fn(x: bytes) -> Result<i64, [`JsonErr(string), `PackErr(string), `IOErr(string), `InvalidCast(string)]>, data| f(data);
    let a: i64 = apply(json::read, json::write_bytes(42)$)?;
    let b: i64 = apply(pack::read, pack::write_bytes(42)$)?;
    a + b
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(84))) }; graphix_package_core::testing::FuseExpect::Jit);

// Struct types through a typed wrapper.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    late_bind_struct,
    r#"{
    type Point = {x: i64, y: i64};
    let decode = |data: [string, bytes]| -> Result<Point, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(data);
    let p: Point = decode(json::write_str({x: 10, y: 20})$)?;
    p.x + p.y
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(30))) }; graphix_package_core::testing::FuseExpect::Jit);

// Type propagation through higher-order functions.

// array::map with json::read: the call site's type reaches json::read
// before the callback instance is bound.
run!(
    hof_map_json_read,
    r#"{
    let data = [json::write_str(42)$];
    let results: Array<Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>> =
        array::map(data, json::read);
    results[0]
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::I64(42)))
    }
; graphix_package_core::testing::FuseExpect::Jit);

run!(
    hof_map_json_untyped,
    r#"{
    let data = [json::write_str(42)$];
    let results = array::map(data, json::read);
    results[0]
}"#,
    |v: Result<&Value>| { matches!(v, Err(_)) }
; graphix_package_core::testing::FuseExpect::None);

// array::fold: json::read in the fold closure receives its concrete type.
run!(
    hof_fold_json_read,
    r#"{
        let data = [
            json::write_str(10)$,
            json::write_str(20)$,
            json::write_str(12)$,
        ];
        array::fold(data, 0, |acc, s| acc + json::read(s)$)
    }"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::Jit);

// array::init: json::read in an unannotated init closure.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    hof_init_json_read,
    r#"{
    let s = json::write_str(42)$;
    let results =
        array::init(1, |i| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(s));
    results[0]
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::Jit);

// list::init: json::read in an unannotated init closure.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    hof_list_init_json_read,
    r#"{
    use list::*;
    let s = json::write_str(7)$;
    let results =
        list::init(1, |i| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(s));
    list::head(results)
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(7))) }; graphix_package_core::testing::FuseExpect::Jit);

// Nested array::map with json::read passed as a bare fn value to the
// inner map: the annotated result type reaches it through the inner
// callback's own fn-typed cell.
run!(
    hof_nested_map_json_read,
    r#"{
    let data = [[json::write_str(1)$, json::write_str(2)$], [json::write_str(3)$]];
    let results: Array<Array<Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>>> =
        array::map(data, |x| array::map(x, json::read));
    let row = results[0]$;
    row[0]$
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) }
; graphix_package_core::testing::FuseExpect::Jit);

// core::filter: json::read piped through filter.
run!(
    hof_filter_json_read,
    r#"{
    let s = json::write_str(42)$;
    let v: i64 = filter(json::read(s)$, |x| x > 0);
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::Jit);

// Subscribe type-aware casting.

// subscribe with a typed result.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    subscribe_typed_i64,
    r#"{
    sys::net::publish("/local/typed_sub", 42);
    let v: i64 = sys::net::subscribe("/local/typed_sub")?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::None);

// subscribe with Primitive (no cast).
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    subscribe_primitive,
    r#"{
    sys::net::publish("/local/prim_sub", 42);
    let v: Primitive = sys::net::subscribe("/local/prim_sub")?;
    cast<i64>(v)?
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::None);

// subscribe without a type annotation is a compile error.
run!(
    subscribe_no_type,
    r#"{
    sys::net::publish("/local/untyped_sub", 42);
    sys::net::subscribe("/local/untyped_sub")
}"#,
    |v: Result<&Value>| { v.is_err() }
; graphix_package_core::testing::FuseExpect::None);

// RPC client type-aware casting.

// call with a typed result.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    call_typed,
    r#"{
    sys::net::rpc(
        #path: "/local/typed_call_rpc",
        #doc: "test",
        #spec: {x: {default: 0, doc: "input"}},
        #f: |args: {x: i64}| args.x * 2
    );
    let v: i64 = sys::net::call("/local/typed_call_rpc", {x: 21})?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }; graphix_package_core::testing::FuseExpect::None);

// Publish on_write type-aware casting.

// on_write callback with a typed arg.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    publish_typed_onwrite,
    r#"{
    let p = "/local/typed_pub";
    let x = 0;
    sys::net::publish(#on_write: |v: i64| x <- v, p, x);
    let s: i64 = sys::net::subscribe(p)?;
    sys::net::write(p, once(s + 1));
    array::group(s, |n, _| n == 2)
}"#,
    |v: Result<&Value>| {
        if let Ok(Value::Array(a)) = v {
            matches!(&a[..], [Value::I64(0), Value::I64(1)])
        } else {
            false
        }
    }; graphix_package_core::testing::FuseExpect::Jit);

// RPC with a typed spec and callback.

// rpc with a typed struct callback arg.
// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(
    rpc_typed_struct,
    r#"{
    sys::net::rpc(
        #path: "/local/typed_rpc",
        #doc: "typed rpc test",
        #spec: {name: {default: "world", doc: "who to greet"}, count: {default: 1, doc: "how many"}},
        #f: |args: {name: string, count: i64}| str::concat("hello ", args.name)
    );
    let v: string = sys::net::call("/local/typed_rpc", {name: "graphix", count: 1})?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::String(s)) if &**s == "hello graphix") }; graphix_package_core::testing::FuseExpect::None);
