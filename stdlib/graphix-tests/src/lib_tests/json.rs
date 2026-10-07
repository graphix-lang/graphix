use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::subscriber::Value;

run!(json_i64, r#"{let v: i64 = json::read(json::write_str(42)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

run!(json_f64, r#"{let v: f64 = json::read(json::write_str(3.14)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
}; FuseExpect::None);

run!(json_bool, r#"{let v: bool = json::read(json::write_str(true)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

run!(json_null, r#"{let v: null = json::read(json::write_str(null)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; FuseExpect::None);

run!(json_string, r#"{let v: string = json::read(json::write_str("hello")$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::None);

run!(json_array, r#"{
    let arr: Array<i64> = json::read(json::write_str([1, 2, 3])$)?;
    arr
}"#, |v: Result<&Value>| {
    if let Ok(Value::Array(arr)) = v {
        arr.len() == 3
            && arr[0] == Value::I64(1)
            && arr[1] == Value::I64(2)
            && arr[2] == Value::I64(3)
    } else {
        false
    }
}; FuseExpect::None);

// CR claude for claude: [test-gap] The predicate accepts any two-element array and never
// looks at x = 42 or y = "hi"; json_struct_cast's x + y would not notice swapped fields
// either. Compare the decoded value exactly: [["x", 42], ["y", "hi"]]. json_nested
// (line 80) accepts any array and json_no_concrete_type (line 146) any compile error.
// Compare the nested pairs, and match the "must be fully known" refusal as types.rs:473
// does. (tests-lib-b1-12)
run!(json_struct, r#"{
    type S = {x: i64, y: string};
    let obj: S = json::read(json::write_str({x: 42, y: "hi"})$)?;
    obj
}"#, |v: Result<&Value>| {
    // a struct comes back as a sorted array of pairs
    if let Ok(Value::Array(arr)) = v {
        arr.len() == 2
    } else {
        false
    }
}; FuseExpect::None);

run!(json_read_bytes, r#"{
    let b = json::write_bytes(42)$;
    let v: i64 = json::read(b)?;
    v
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::Jit);

run!(json_pretty, r#"{
    let compact = json::write_str({a: 1, b: 2})$;
    let pretty = json::write_str(#pretty: true, {a: 1, b: 2})$;
    str::len(pretty) > str::len(compact)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(json_invalid, r#"{
    let r: Result<i64, [`JsonErr(string), `InvalidCast(string)]> = json::read("not json{{{");
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(json_nested, r#"{
    type Nested = {items: Array<i64>, meta: {count: i64}};
    let obj: Nested = json::read(json::write_str({items: [1, 2], meta: {count: 2}})$)?;
    obj
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(_)))
}; FuseExpect::None);

// json over a tcp stream, read back from the other end.
// CR claude for claude: [risk] write_exact and shutdown both fire when `client` fires and
// run as two spawned tasks with nothing ordering them, so the shutdown can lock the
// stream first: the write fails with EPIPE and the server reads EOF. run! passes only
// because tokio's current_thread runtime polls tasks in spawn order; the same program
// on the shell's multi-thread runtime fails 5 to 12 runs in 50, probe:
// design/review-2026-10-05/repro/tests-lib-b1-03.gx. Sequence the shutdown on the
// write, `let written = Write::write_exact(client, ..)?; Socket::shutdown(written ~
// client)?` (50 of 50 pass), here, in json_stream_nested (line 101), toml_stream_tcp
// (toml.rs:68) and pack_stream_tcp (pack.rs:61). book/src/stdlib/sys/io.md:13-15 has
// the same shape (read_all and close on one fire of `f`) and fails every run with
// `stream unavailable`. (tests-lib-b1-03)
run!(json_stream_tcp, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    Write::write_exact(client, json::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown(client)?;
    let msg: Msg = json::read(Read::read_all(server)?)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
}; FuseExpect::Jit);

// json over a tcp stream, read back into a nested struct.
run!(json_stream_nested, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Inner = {label: string, value: i64};
    type Outer = {items: Array<Inner>, count: i64};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    let data: Outer = {items: [{label: "a", value: 1}, {label: "b", value: 2}], count: 2};
    Write::write_exact(client, json::write_bytes(data)?)?;
    Socket::shutdown(client)?;
    let out: Outer = json::read(Read::read_all(server)?)?;
    let items = out.items;
    out.count + (items[0]$).value + (items[1]$).value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
}; FuseExpect::Jit);

// A struct round-trip through a json string with a typed read.
run!(json_struct_cast, r#"{
    type Point = {x: i64, y: i64};
    let p: Point = {x: 10, y: 20};
    let s = json::write_str(p)$;
    let p2: Point = json::read(s)?;
    p2.x + p2.y
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
}; FuseExpect::Jit);

// A nested struct round-trip through a json string.
run!(json_nested_struct_cast, r#"{
    type Inner = {label: string, value: i64};
    type Outer = {items: Array<Inner>, count: i64};
    let data: Outer = {items: [{label: "a", value: 1}, {label: "b", value: 2}], count: 2};
    let s = json::write_str(data)$;
    let out: Outer = json::read(s)?;
    let items = out.items;
    out.count + (items[0]$).value + (items[1]$).value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
}; FuseExpect::Jit);

// json::read without a concrete return type is a compile error.
run!(json_no_concrete_type, r#"json::read("42")"#, refused("the type 'b must be fully known here"); FuseExpect::None);
