use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// A `bytes` literal bound to a let-local and returned as a Value.
run!(bytes_const_local, r#"{ let x = bytes:SGVsbG8=; x }"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if b.as_ref() == b"Hello")
}; graphix_package_core::testing::FuseExpect::Jit);

run!(pack_i64, r#"{let v: i64 = pack::read(pack::write_bytes(42)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_f64, r#"{let v: f64 = pack::read(pack::write_bytes(3.14)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_bool, r#"{let v: bool = pack::read(pack::write_bytes(true)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_null, r#"{let v: null = pack::read(pack::write_bytes(null)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_string, r#"{let v: string = pack::read(pack::write_bytes("hello")$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_struct, r#"{
    type S = {x: i64, y: string};
    let obj: S = pack::read(pack::write_bytes({x: 42, y: "hi"})$)?;
    obj
}"#, |v: Result<&Value>| {
    // CR claude for eric: [test-gap] pack_struct accepts any two-element array, so a
    // decode that swaps or zeroes the fields of {x: 42, y: "hi"} still passes. Have the
    // fixture return `obj.x == 42 && obj.y == "hi"`, or match the exact Value.
    // sqlite_exec_params (sqlite.rs:48) has the same weakness: it binds [1, 3.14] and
    // checks only the id; check `(rows[0]$).val == 3.14` too. (tests-lib-b2-13)
    matches!(v, Ok(Value::Array(arr)) if arr.len() == 2)
}; graphix_package_core::testing::FuseExpect::None);

run!(pack_bytes, r#"{
    let b = buffer::from_string("abc");
    let encoded = pack::write_bytes(b)$;
    let decoded: bytes = pack::read(encoded)?;
    buffer::to_string(decoded)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "abc")
}; graphix_package_core::testing::FuseExpect::Jit);

// CR claude for eric: [risk] The write_exact and the shutdown below (lines 69-70) both
// fire when `client` fires, and nothing orders the shutdown after the write: their two
// tasks race for the stream's lock. run! passes only because its current_thread runtime
// polls tasks in spawn order; under the shell's multi-thread runtime the shutdown wins
// in 10-25% of runs, the write fails with EPIPE and `msg.name` never arrives.
// toml_stream_tcp (toml.rs:76-77) has the same race. Sequence it: `let written =
// Write::write_exact(client, ..)?; Socket::shutdown(written ~ client)?;`. probe:
// design/review-2026-10-05/repro/tests-lib-b2-02.sh (tests-lib-b2-02)
run!(pack_stream_tcp, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    Write::write_exact(client, pack::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown(client)?;
    let msg: Msg = pack::read(Read::read_all(server)?)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
}; graphix_package_core::testing::FuseExpect::Jit);

// CR claude for eric: [test-gap] pack_invalid, json_invalid (json.rs:68) and
// toml_invalid (toml.rs:84) annotate the whole `Result<i64, [..]>`. That is the one
// target type under which the reader's cast keeps its decode error. The usual spelling,
// `let v: i64 = pack::read(garbage)?` (or `$`), casts the PackErr/JsonErr/TomlErr
// itself to the target: it yields 0, false or null, and a struct target re-tags it
// InvalidCast (small-pkgs-01). No fixture reads invalid input that way, so that bug has
// no pin. Add fixtures that read garbage through `?` into a primitive and into a struct
// target, and assert the catch receives the reader's own error tag. Probe: `let j: i64
// = json::read("this is not json")$` is 0 in both engines. (tests-lib-b2-04)
run!(pack_invalid, r#"{
    let r: Result<i64, [`PackErr(string), `InvalidCast(string)]> = pack::read(buffer::from_array([u8:255, u8:255, u8:255]));
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);
