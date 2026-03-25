use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

run!(toml_i64, r#"{
    let s = toml::write_str({value: 42})$;
    let obj: {value: i64} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

run!(toml_f64, r#"{
    let s = toml::write_str({value: 3.14})$;
    let obj: {value: f64} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
});

run!(toml_bool, r#"{
    let s = toml::write_str({value: true})$;
    let obj: {value: bool} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(toml_string, r#"{
    let s = toml::write_str({value: "hello"})$;
    let obj: {value: string} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

run!(toml_struct, r#"{
    type Point = {x: i64, y: i64};
    let p: Point = {x: 10, y: 20};
    let s = toml::write_str(p)$;
    let p2: Point = toml::read(s)?;
    p2.x + p2.y
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
});

run!(toml_nested_struct, r#"{
    type Inner = {label: string, value: i64};
    type Outer = {count: i64, items: Array<Inner>};
    let data: Outer = {count: 2, items: [{label: "a", value: 1}, {label: "b", value: 2}]};
    let s = toml::write_str(data)$;
    let out: Outer = toml::read(s)?;
    let items = out.items;
    out.count + (items[0]$).value + (items[1]$).value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});

run!(toml_array, r#"{
    let s = toml::write_str({items: [1, 2, 3]})$;
    let obj: {items: Array<i64>} = toml::read(s)?;
    let arr = obj.items;
    arr[0]$ + arr[1]$ + arr[2]$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(6)))
});

run!(toml_stream_tcp, r#"{
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    toml::write_stream(client, {name: "alice", age: 30})?;
    sys::tcp::shutdown(client)?;
    let msg: Msg = toml::read(server)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
});

run!(toml_invalid, r#"{
    let r: Result<i64, [`TomlErr(string), `IOErr(string), `InvalidCast(string)]> = toml::read("not valid toml \[\[\[");
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(toml_null_err, r#"{
    let r = toml::write_str(null);
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});
