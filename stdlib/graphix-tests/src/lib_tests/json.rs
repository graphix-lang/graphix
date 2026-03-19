use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

run!(json_i64, r#"json::read(json::write_str(42)$)$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

run!(json_f64, r#"json::read(json::write_str(3.14)$)$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
});

run!(json_bool, r#"json::read(json::write_str(true)$)$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(json_null, r#"json::read(json::write_str(null)$)$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

run!(json_string, r#"json::read(json::write_str("hello")$)$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

run!(json_array, r#"{
    let arr = cast<Array<i64>>(json::read(json::write_str([1, 2, 3])$)$)?;
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

run!(json_struct, r#"{
    let obj = json::read(json::write_str({x: 42, y: "hi"})$)$;
    obj
}"#, |v: Result<&Value>| {
    // struct comes back as sorted array of pairs
    if let Ok(Value::Array(arr)) = v {
        arr.len() == 2
    } else {
        false
    }
});

run!(json_read_bytes, r#"{
    let b = json::write_bytes(42)$;
    json::read(b)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

run!(json_pretty, r#"{
    let compact = json::write_str({a: 1, b: 2})$;
    let pretty = json::write_str(#pretty: true, {a: 1, b: 2})$;
    str::len(pretty) > str::len(compact)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(json_invalid, r#"{
    let r = json::read("not json{{{");
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(json_nested, r#"{
    let obj = json::read(json::write_str({items: [1, 2], meta: {count: 2}})$)$;
    obj
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(_)))
});

// write json to a tcp stream, read it back from the other end, cast to struct
run!(json_stream_tcp, r#"{
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    json::write_stream(client, {name: "alice", age: 30})?;
    sys::tcp::shutdown(client)?;
    let msg = cast<Msg>(json::read(server)?)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
});

// write json to a tcp stream, read back and cast to nested struct
run!(json_stream_nested, r#"{
    type Inner = {label: string, value: i64};
    type Outer = {items: Array<Inner>, count: i64};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    let data: Outer = {items: [{label: "a", value: 1}, {label: "b", value: 2}], count: 2};
    json::write_stream(client, data)?;
    sys::tcp::shutdown(client)?;
    let out = cast<Outer>(json::read(server)?)?;
    let items = out.items;
    out.count + (items[0]$).value + (items[1]$).value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});

// struct round-trip: write as json string, read back, cast to original type
run!(json_struct_cast, r#"{
    type Point = {x: i64, y: i64};
    let p: Point = {x: 10, y: 20};
    let s = json::write_str(p)$;
    let v = json::read(s)$;
    let p2 = cast<Point>(v)?;
    p2.x + p2.y
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
});

// nested struct round-trip through json string
run!(json_nested_struct_cast, r#"{
    type Inner = {label: string, value: i64};
    type Outer = {items: Array<Inner>, count: i64};
    let data: Outer = {items: [{label: "a", value: 1}, {label: "b", value: 2}], count: 2};
    let s = json::write_str(data)$;
    let v = json::read(s)$;
    let out = cast<Outer>(v)?;
    let items = out.items;
    out.count + (items[0]$).value + (items[1]$).value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});
