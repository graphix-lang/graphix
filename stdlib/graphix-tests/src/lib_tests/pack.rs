use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

// A `bytes` literal bound to a let-local and returned as a Value.
run!(bytes_const_local, r#"{ let x = bytes:SGVsbG8=; x }"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if b.as_ref() == b"Hello")
}; FuseExpect::Jit);

run!(pack_i64, r#"{let v: i64 = pack::read(pack::write_bytes(42)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

run!(pack_f64, r#"{let v: f64 = pack::read(pack::write_bytes(3.14)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
}; FuseExpect::None);

run!(pack_bool, r#"{let v: bool = pack::read(pack::write_bytes(true)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

run!(pack_null, r#"{let v: null = pack::read(pack::write_bytes(null)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; FuseExpect::None);

run!(pack_string, r#"{let v: string = pack::read(pack::write_bytes("hello")$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::None);

run!(pack_array, r#"{
    let arr: Array<i64> = pack::read(pack::write_bytes([1, 2, 3])$)?;
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

run!(pack_struct, r#"{
    type S = {x: i64, y: string};
    let obj: S = pack::read(pack::write_bytes({x: 42, y: "hi"})$)?;
    obj
}"#, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == r#"[["x", i64:42], ["y", "hi"]]"#
}; FuseExpect::None);

run!(pack_bytes, r#"{
    let b = buffer::from_string("abc");
    let encoded = pack::write_bytes(b)$;
    let decoded: bytes = pack::read(encoded)?;
    buffer::to_string(decoded)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "abc")
}; FuseExpect::Jit);

run!(pack_stream_tcp, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    let written = Write::write_exact(client, pack::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown(written ~ client)?;
    let msg: Msg = pack::read(Read::read_all(server)?)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
}; FuseExpect::Jit);

// CR claude for claude: [test-gap] pack_invalid, json_invalid (json.rs:68) and
// toml_invalid (toml.rs:84) annotate the whole `Result<i64, [..]>`. That is the one
// target type under which the reader's cast keeps its decode error. The usual spelling,
// `let v: i64 = pack::read(garbage)?` (or `$`), casts the PackErr/JsonErr/TomlErr
// itself to the target: it yields 0, false or null, and a struct target re-tags it
// InvalidCast (small-pkgs-01). No fixture reads invalid input that way, so that bug has
// no pin. Add fixtures that read garbage through `?` into a primitive and into a struct
// target, and assert the catch receives the reader's own error tag. Probe: `let j: i64
// = json::read("this is not json")$` is 0 in both engines. (tests-lib-b2-04)
// 2026-10-06 claude: deferred to the fix of small-pkgs-01 (json/lib.rs), which these
// fixtures pin; a catch around `json::read("this is not json")?` still receives nothing.
run!(pack_invalid, r#"{
    let r: Result<i64, [`PackErr(string), `InvalidCast(string)]> = pack::read(buffer::from_array([u8:255, u8:255, u8:255]));
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);
