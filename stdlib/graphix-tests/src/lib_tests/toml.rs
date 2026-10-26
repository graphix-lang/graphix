use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{
    run,
    testing::{FuseExpect, prints},
};
use netidx::subscriber::Value;

run!(toml_i64, r#"{
    let s = toml::write_str({value: 42})$;
    let obj: {value: i64} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::Jit);

run!(toml_f64, r#"{
    let s = toml::write_str({value: 3.14})$;
    let obj: {value: f64} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if (*f - 3.14).abs() < 1e-10)
}; FuseExpect::Jit);

run!(toml_bool, r#"{
    let s = toml::write_str({value: true})$;
    let obj: {value: bool} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(toml_string, r#"{
    let s = toml::write_str({value: "hello"})$;
    let obj: {value: string} = toml::read(s)?;
    obj.value
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::Jit);

run!(toml_struct, r#"{
    type Point = {x: i64, y: i64};
    let p: Point = {x: 10, y: 20};
    let s = toml::write_str(p)$;
    let p2: Point = toml::read(s)?;
    p2.x + p2.y
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

run!(toml_array, r#"{
    let s = toml::write_str({items: [1, 2, 3]})$;
    let obj: {items: Array<i64>} = toml::read(s)?;
    let arr = obj.items;
    arr[0]$ + arr[1]$ + arr[2]$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(6)))
}; FuseExpect::Jit);

run!(toml_stream_tcp, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    let written = Write::write_exact(client, toml::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown(written ~ client)?;
    let msg: Msg = toml::read(Read::read_all(server)?)?;
    msg.name
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "alice")
}; FuseExpect::Jit);

run!(toml_invalid, r#"{
    let r: Result<i64, [`TomlErr(string), `InvalidCast(string)]> = toml::read("not valid toml \[\[\[");
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(toml_null_err, r#"{
    let r = toml::write_str(null);
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A map's null value is data TOML cannot hold: refused, never dropped.
run!(toml_map_null_err, r#"{
    let w: Map<string, [i64, null]> = {"k0" => null, "k1" => i64:1};
    "[toml::write_str({a: w})]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("cannot represent null in TOML"))
}; FuseExpect::Jit);

// An optional field goes through TOML both ways: a null field is a key left
// out, and a key the document leaves out reads as null.
run!(toml_optional_field_round_trips, r#"{
    type Cfg = {host: string, port: [i64, null]};
    let c: Cfg = {host: "h", port: null};
    let back: Cfg = toml::read(toml::write_str(c)$)$;
    "[back.host] [back.port]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "h null")
}; FuseExpect::Jit);

// A map writes as a table.
run!(toml_map_round_trips, r#"{
    let m: Map<string, string> = {"serde" => "1.0"};
    let back: Map<string, string> = toml::read(toml::write_str(m)$)$;
    map::get(back, "serde")$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "1.0")
}; FuseExpect::Jit);

// An array of pairs is no table, in any order: toml refuses it by its type.
run!(
    toml_refuses_pairs_by_type,
    r#"{
    let sorted: Array<(string, i64)> = [("a", 1), ("b", 2)];
    let unsorted: Array<(string, i64)> = [("b", 1), ("a", 2)];
    (is_err(toml::write_str(sorted)), is_err(toml::write_str(unsorted)), toml::write_str({a: 1})$)
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            a.len() == 3
                && a[0] == Value::Bool(true)
                && a[1] == Value::Bool(true)
                && a[2] == Value::String(literal!("a = 1\n"))
        }
        _ => false,
    }
);

// An omitted field reads as null only where its type admits null: a
// required string or array field left out is refused, never "null" or
// [null].
run!(omitted_required_field_refused, r##"{
    let refused = |r: Any| select r { error as _ => true, _ => false };
    let c: Result<{a: i64, b: string}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(r#"{"a": 1}"#);
    let n: Result<{a: i64, b: Array<[i64, null]>}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(r#"{"a": 1}"#);
    let t: Result<{a: i64, b: string}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read("a = 1");
    let o: Result<{a: i64, b: [string, null]}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(r#"{"a": 1}"#);
    (refused(c), refused(n), refused(t), o$)
}"##, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"[true, true, true, [["a", i64:1], ["b", null]]]"#)
}; FuseExpect::None);

/// What toml::write_str writes, toml::read reads back: a table holding
/// the deepest arrays the reader takes round-trips, and one array
/// deeper is refused on write (small-pkgs-06).
#[tokio::test(flavor = "multi_thread")]
async fn the_deepest_written_table_reads_back() -> Result<()> {
    use graphix_package_core::testing::eval_modes;
    let program = |n: usize| {
        let typ = format!("{{a: {}i64{}}}", "Array<".repeat(n), ">".repeat(n));
        let v = format!("{{a: {}1{}}}", "[".repeat(n), "]".repeat(n));
        format!(
            "{{ let v: {typ} = {v}; \
             select toml::write_str(v) {{ \
               error as e => \"WRITE [e]\", \
               s => {{ let r: Result<{typ}, [`TomlErr(string), `InvalidCast(string)]> = toml::read(s); \
                 select r {{ error as e => \"READ [e]\", _ => \"ok\" }} }} \
             }} }}"
        )
    };
    let deepest = eval_modes(&program(80), crate::TEST_REGISTER).await?;
    assert_eq!(deepest, Ok(Value::String(literal!("ok"))));
    match eval_modes(&program(81), crate::TEST_REGISTER).await? {
        Ok(Value::String(s)) if s.starts_with("WRITE") => (),
        r => panic!("{r:?}"),
    }
    Ok(())
}

// A bare variant key is written as its tag, which a read casts back.
run!(toml_variant_map_key_reads_back, r#"{
    let m: Map<[`A, `B], i64> = {`A => 1, `B => 2};
    let s = toml::write_str({m})$;
    let r: {m: Map<[`A, `B], i64>} = toml::read(s)?;
    (s, r.m == m)
}"#, prints(r#"["[m]\nA = 1\nB = 2\n", true]"#); FuseExpect::Jit);
