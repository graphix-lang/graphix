use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

run!(pack_i64, r#"{let v: i64 = pack::read(pack::write_bytes(42)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

run!(pack_f64, r#"{let v: f64 = pack::read(pack::write_bytes(3.14)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
});

run!(pack_bool, r#"{let v: bool = pack::read(pack::write_bytes(true)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(pack_null, r#"{let v: null = pack::read(pack::write_bytes(null)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

run!(pack_string, r#"{let v: string = pack::read(pack::write_bytes("hello")$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

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
});

run!(pack_struct, r#"{
    type S = {x: i64, y: string};
    let obj: S = pack::read(pack::write_bytes({x: 42, y: "hi"})$)?;
    obj
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(arr)) if arr.len() == 2)
});

run!(pack_bytes, r#"{
    let b = buffer::from_string("abc");
    let encoded = pack::write_bytes(b)$;
    let decoded: bytes = pack::read(encoded)?;
    buffer::to_string(decoded)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "abc")
});

run!(pack_stream_tcp, r#"{
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    pack::write_stream(client, {name: "alice", age: 30})?;
    sys::tcp::shutdown(client)?;
    let msg: Msg = pack::read(server)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
});

run!(pack_invalid, r#"{
    let r: Result<i64, [`PackErr(string), `IOErr(string), `InvalidCast(string)]> = pack::read(buffer::from_array([u8:255, u8:255, u8:255]));
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});
