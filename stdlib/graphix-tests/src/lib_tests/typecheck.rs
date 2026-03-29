use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// ============================================================================
// Compile-time type checking: deserialization builtins require concrete types
// ============================================================================

// json::read without concrete return type → compile error
run!(json_no_type, r#"json::read("42")"#, |v: Result<&Value>| v.is_err());

// toml::read without concrete return type → compile error
run!(toml_no_type, r#"toml::read("x = 42")"#, |v: Result<&Value>| v.is_err());

// pack::read without concrete return type → compile error
run!(pack_no_type, r#"pack::read(pack::write_bytes(42)$)"#, |v: Result<&Value>| v
    .is_err());

// str::parse without concrete return type → compile error
run!(str_parse_no_type, r#"str::parse("42")"#, |v: Result<&Value>| v.is_err());

// json::read with concrete type → compiles and runs
run!(json_typed_i64, r#"{let v: i64 = json::read("42")?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// json::read with struct type → compiles and casts
run!(
    json_typed_struct,
    r#"{
    type P = {x: i64, y: string};
    let v: P = json::read(json::write_str({x: 1, y: "a"})$)?;
    v.x
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) }
);

// ============================================================================
// Late binding: deserializers passed through higher-order functions
// ============================================================================

// Late binding: deserializer stored in variable, called with concrete type
run!(
    late_bind_var,
    r#"{
    let decoder = json::read;
    let v: i64 = decoder("42")?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// Late binding: function wraps a deserializer with explicit return type
run!(
    late_bind_wrap,
    r#"{
    let decode = |data: [string, bytes]| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(data);
    let v: i64 = decode(json::write_str(99)$)?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(99))) }
);

// Late binding: multiple calls to same typed wrapper with json
run!(
    late_bind_multi_json,
    r#"{
    let apply = |f: fn([string, bytes]) -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>, data| f(data);
    let a: i64 = apply(json::read, json::write_str(42)$)?;
    let b: i64 = apply(json::read, json::write_str(42)$)?;
    a + b
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(84))) }
);

// Late binding: json + pack through same typed call site using bytes input
// (both accept bytes; error types unify to the superset)
run!(
    late_bind_mixed_deser,
    r#"{
    let apply = |f: fn(bytes) -> Result<i64, [`JsonErr(string), `PackErr(string), `IOErr(string), `InvalidCast(string)]>, data| f(data);
    let a: i64 = apply(json::read, json::write_bytes(42)$)?;
    let b: i64 = apply(pack::read, pack::write_bytes(42)$)?;
    a + b
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(84))) }
);

// Late binding with struct types through typed wrapper
run!(
    late_bind_struct,
    r#"{
    type Point = {x: i64, y: i64};
    let decode = |data: [string, bytes]| -> Result<Point, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(data);
    let p: Point = decode(json::write_str({x: 10, y: 20})$)?;
    p.x + p.y
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(30))) }
);

// ============================================================================
// Higher-order function type propagation
// ============================================================================

// array::map with json::read — CallSite type must propagate through builtin HOF.
// json::read requires a concrete return type via the CallSite typecheck phase.
// When passed as a predicate to array::map, the resolved FnType from the outer
// CallSite must propagate through MapQ to json::read. Currently this fails:
// MapQ::typecheck ignores the phase and returns Done, so the deferred check
// cascade never reaches json::read, leaving its cast_typ unset.
run!(
    hof_map_json_read,
    r#"{
    let data = [json::write_str(42)$];
    let results: Array<Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>> =
        array::map(data, json::read);
    results[0]
}"#,
    |v: Result<&Value>| {
        // When fixed: json::read gets cast_typ, deserializes to Ok(42), is_err => false
        // Bug present: json::read has no cast_typ, returns Error, is_err => true
        matches!(v, Ok(Value::I64(42)))
    }
);

run!(
    hof_map_json_untyped,
    r#"{
    let data = [json::write_str(42)$];
    let results = array::map(data, json::read);
    results[0]
}"#,
    |v: Result<&Value>| { matches!(v, Err(_)) }
);

// array::fold — FoldQ: json::read in fold closure, type must propagate
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
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// array::init — Init: json::read in unannotated init closure,
// type must propagate through Init's resolved mftyp
// let results: Array<Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>> =
//        array::init(1, |i| json::read(s));
run!(
    hof_init_json_read,
    r#"{
    let s = json::write_str(42)$;
    let results =
        array::init(1, |i| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(s));
    results[0]
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// list::init — ListInit: json::read in unannotated init closure,
// type must propagate through ListInit's resolved mftyp
run!(
    hof_list_init_json_read,
    r#"{
    use list;
    let s = json::write_str(7)$;
    let results =
        list::init(1, |i| -> Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]> json::read(s));
    list::head(results)
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(7))) }
);

// nested array::map — json::read passed directly to inner map.
// The inner callsite typechecks before the outer deferred check runs,
// so the concrete return type never propagates to json::read.
// This is a known limitation of the current single-pass deferred check
// scheduling: by the time the outer CallSite phase fires, the inner
// array::map's callsite has already been checked and won't re-schedule.
run!(
    hof_nested_map_json_read,
    r#"{
    let data = [[json::write_str(1)$, json::write_str(2)$], [json::write_str(3)$]];
    let results: Array<Array<Result<i64, [`JsonErr(string), `IOErr(string), `InvalidCast(string)]>>> =
        array::map(data, |x| array::map(x, json::read));
    let row = results[0]$;
    row[0]$
}"#,
    |v: Result<&Value>| { v.is_err() }
);

// core::filter — Filter: json::read piped through filter,
// type must propagate through Filter's resolved predicate type
run!(
    hof_filter_json_read,
    r#"{
    let s = json::write_str(42)$;
    let v: i64 = filter(json::read(s)$, |x| x > 0);
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// ============================================================================
// Subscribe type-aware casting
// ============================================================================

// subscribe with typed result
run!(
    subscribe_typed_i64,
    r#"{
    sys::net::publish("/local/typed_sub", 42);
    let v: i64 = sys::net::subscribe("/local/typed_sub")?;
    v
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// subscribe with Primitive (backwards compatible, no cast)
run!(
    subscribe_primitive,
    r#"{
    sys::net::publish("/local/prim_sub", 42);
    let v: Primitive = sys::net::subscribe("/local/prim_sub")?;
    cast<i64>(v)?
}"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// subscribe without type annotation → compile error
run!(
    subscribe_no_type,
    r#"{
    sys::net::publish("/local/untyped_sub", 42);
    sys::net::subscribe("/local/untyped_sub")
}"#,
    |v: Result<&Value>| { v.is_err() }
);

// ============================================================================
// Call (RPC client) type-aware casting
// ============================================================================

// call with typed result
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
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(42))) }
);

// ============================================================================
// Publish on_write type-aware casting
// ============================================================================

// on_write callback with typed arg
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
    }
);

// ============================================================================
// RPC with typed spec and callback
// ============================================================================

// rpc with typed struct callback arg
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
    |v: Result<&Value>| { matches!(v, Ok(Value::String(s)) if &**s == "hello graphix") }
);
