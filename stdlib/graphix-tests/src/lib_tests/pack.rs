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

run!(pack_invalid, r#"{
    let r: Result<i64, [`PackErr(string), `InvalidCast(string)]> = pack::read(buffer::from_array([u8:255, u8:255, u8:255]));
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A reader's own error reaches the catch through `?`, whatever the target:
// never cast to the target (0, false, null) nor re-tagged InvalidCast.
macro_rules! reader_error {
    ($name:ident, $read:literal, $tag:literal) => {
        run!(
            $name,
            concat!(
                "{\n  let errors: Error<Any> = never();\n  catch(e) errors <- e;\n  ",
                $read,
                ";\n  \"[errors]\"\n}"
            ),
            |v: Result<&Value>| match v {
                // the tag in its error position, never inside an InvalidCast
                Ok(Value::String(s)) => {
                    s.contains(concat!("[\"error\", [\"", $tag, "\""))
                        && !s.contains("InvalidCast")
                }
                _ => false,
            };
            FuseExpect::Jit
        );
    };
}

reader_error!(
    json_garbage_as_i64,
    r#"let v: i64 = json::read("this is not json")?"#,
    "JsonErr"
);
reader_error!(json_garbage_as_struct, r#"let v: {x: i64} = json::read("{")?"#, "JsonErr");
reader_error!(toml_garbage_as_i64, r#"let v: i64 = toml::read("= =")?"#, "TomlErr");
reader_error!(
    toml_garbage_as_struct,
    r#"let v: {x: i64} = toml::read("= =")?"#,
    "TomlErr"
);
reader_error!(
    pack_garbage_as_i64,
    r#"let v: i64 = pack::read(buffer::from_array([u8:255, u8:255, u8:255]))?"#,
    "PackErr"
);
reader_error!(
    pack_garbage_as_struct,
    r#"let v: {x: i64} = pack::read(buffer::from_array([u8:255, u8:255, u8:255]))?"#,
    "PackErr"
);

// A duration whose nanoseconds carry past u64 seconds is a PackErr, the
// read's declared error, and no panic.
run!(pack_read_oversize_duration_nanos, r#"{
    let b = buffer::encode([`U8(u8:11), `U64(u64:18446744073709551615), `U32(u32:1000000000)]);
    let d: Result<duration, [`PackErr(string), `InvalidCast(string)]> = pack::read(b);
    select d { error as e => "[e]", d => "[d]" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"error:["PackErr", "InvalidFormat"]"#)
}; FuseExpect::Jit);

// Abstract values nest at most 1024 deep in bytes; the writer refuses a
// deeper one, as the reader would, instead of writing what never reads.
run!(pack_abstract_too_deep_is_refused_by_the_writer, r#"{
    type T = Abstract<[T, null]>;
    let v = array::fold(array::init(1100, |i| i), T(null), |acc, i| T(acc));
    select pack::write_bytes(v) { error as e => "[e]", _ => "written" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("TooBig"))
}; FuseExpect::Jit);

// A read of a Graphix abstract type casts the payload against the
// representation: bytes written where `Box` held a string do not read as
// a `Box` that holds an i64. `Box` is at module level in both programs,
// so it is one type to the reader's id check.
#[tokio::test(flavor = "current_thread")]
async fn pack_abstract_read_checks_the_payload() -> Result<()> {
    use graphix_package_core::testing::{
        Mode, compile_result, fixture_runtime, next_update,
    };
    use std::time::Duration;
    use tokio::time::Instant;
    async fn result(src: &str) -> Result<Value> {
        let file = [("/test.gx", arcstr::ArcStr::from(src).into())];
        let (ctx, mut rx) =
            fixture_runtime(file, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
        let res = compile_result(&ctx).await?;
        let deadline = Instant::now() + Duration::from_secs(5);
        next_update(&mut rx, res.exprs[0].id, deadline).await
    }
    let written = result(
        r#"type Box = Abstract<string>;
        let result = pack::write_bytes(Box("hello"))$"#,
    )
    .await?;
    let read = result(&format!(
        r#"type Box = Abstract<i64>;
        let r: Result<Box, [`InvalidCast(string), `PackErr(string)]> = pack::read({written});
        let result = select r {{ error as e => "[e]", b => "[b.0 + 1]" }}"#
    ))
    .await?;
    assert_eq!(
        read.to_string(),
        r#""error:[\"InvalidCast\", \"can't cast \\\"hello\\\" to i64: no primitive conversion\"]""#
    );
    Ok(())
}

// Data never carries a reference: an abstract whose representation is
// one reads as an InvalidCast, and the referent is untouched.
run!(pack_abstract_read_refuses_a_reference, r#"{
    type R = Abstract<&mut i64>;
    let secret = 5;
    let forged: Result<R, [`InvalidCast(string), `PackErr(string)]> =
        pack::read(pack::write_bytes(R(&mut secret))$);
    (select forged { error as e => "[e]", _ => "read" }, secret)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["error:[\"InvalidCast\", \"can't cast &ref to &mut i64: a reference can't be read from data\"]", i64:5]"#)
}; FuseExpect::Jit);
