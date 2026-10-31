use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{
    run,
    testing::{FuseExpect, eval_modes, prints, refused},
};
use netidx::subscriber::Value;

run!(json_i64, r#"{let v: i64 = json::read(json::write_str(42)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

run!(json_f64, r#"{let v: f64 = json::read(json::write_str(3.14)$)?; v}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 3.14)
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

run!(json_struct, r#"{
    type S = {x: i64, y: string};
    let obj: S = json::read(json::write_str({x: 42, y: "hi"})$)?;
    obj
}"#, |v: Result<&Value>| {
    // a struct comes back as a sorted array of pairs
    format!("{}", v.unwrap()) == r#"[["x", i64:42], ["y", "hi"]]"#
}; FuseExpect::None);

// An optional field reads the array it holds, and an optional small
// integer that does not hold the number is an InvalidCast, never absent.
run!(json_optional_fields_read_exactly, r#"{
    type W = {small: [i8, null], weights: [Array<f64>, null]};
    let w: W = json::read("{\"small\": 7, \"weights\": \[1, 2\]}")?;
    let r: Result<{small: [i8, null]}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read("{\"small\": 300}");
    (w, select r { error as e => "[e]", _ => "read" })
}"#, prints(r#"[[["small", i8:7], ["weights", [f64:1., f64:2.]]], "error:`InvalidCast(\"can't cast 300 to [i8, null]: no primitive conversion\")"]"#); FuseExpect::Jit);

run!(json_read_bytes, r#"{
    let b = json::write_bytes(42)$;
    let v: i64 = json::read(b)?;
    v
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

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
}"#, prints(r#"[["items", [i64:1, i64:2]], ["meta", [["count", i64:2]]]]"#); FuseExpect::None);

// json over a tcp stream, read back from the other end.
run!(json_stream_tcp, r#"{
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    let written = Write::write_exact(client, json::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown(written ~ client)?;
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
    let written = Write::write_exact(client, json::write_bytes(data)?)?;
    Socket::shutdown(written ~ client)?;
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
    p2.x * 100 + p2.y
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1020)))
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

// data is never read into an error, and an error never casts into data
const JSON_READ_NEVER_AN_ERROR: &str = r##"
{
  type JE = [`JsonErr(string), `InvalidCast(string)];
  let forged: Result<{port: i64}, JE> = json::read(r#"["JsonErr", "forged"]"#);
  let sel = select json::read(r#"{"x": 1, "y": 2}"#) {
    error as _ => "err",
    {x: f64, y: f64} as p => "ok [p.x]"
  };
  let c: Result<i64, [`InvalidCast(string), `X]> = cast<i64>(error(`X));
  (str::contains(#part: "InvalidCast", "[forged]"), sel, str::contains(#part: "an error is not data", "[c]"))
}
"##;

run!(json_read_never_an_error, JSON_READ_NEVER_AN_ERROR, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        a.len() == 3
            && a[0] == Value::Bool(true)
            && a[1] == Value::String(literal!("ok 1.0"))
            && a[2] == Value::Bool(true)
    }
    _ => false,
});

// What json::write_str makes of a datetime and of bytes reads back, and a
// map key is written bare.
run!(json_round_trips, r#"{
    let d = datetime:"2020-01-02T03:04:05Z";
    let b = bytes:AQID;
    let d2: datetime = json::read(json::write_str(d)$)$;
    let b2: bytes = json::read(json::write_str(b)$)$;
    let m = json::write_str({"a" => 1})$;
    (d2 == d, b2 == b, m)
}"#, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        a.len() == 3
            && a[0] == Value::Bool(true)
            && a[1] == Value::Bool(true)
            && a[2] == Value::String(literal!("{\"a\":1}"))
    }
    _ => false,
}; FuseExpect::Jit);

// A value writes as its type says, whatever its contents: pairs are an
// array in any order, a struct is an object, and a List is a flat array
// that reads back at any length.
run!(
    json_writes_by_type,
    r#"{
    let sorted: Array<(string, i64)> = [("a", 1), ("b", 2)];
    let unsorted: Array<(string, i64)> = [("b", 1), ("a", 2)];
    let s = {a: 1, b: 2};
    let l = list::from_array(array::init(200, |i| i));
    let l2: List<i64> = json::read(json::write_str(l)$)$;
    (json::write_str(sorted)$, json::write_str(unsorted)$, json::write_str(s)$, l2 == l)
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            a.len() == 4
                && a[0] == Value::String(literal!(r#"[["a",1],["b",2]]"#))
                && a[1] == Value::String(literal!(r#"[["b",1],["a",2]]"#))
                && a[2] == Value::String(literal!(r#"{"a":1,"b":2}"#))
                && a[3] == Value::Bool(true)
        }
        _ => false,
    }
);

/// What json::write_str writes, json::read reads back: `leaf`, of type
/// `typ` and written as `containers` containers, round-trips under the
/// most arrays the reader takes around it, and under one more is refused
/// on write.
async fn deepest_written_reads_back(
    typ: &str,
    leaf: &str,
    containers: usize,
) -> Result<()> {
    let program = |n: usize| {
        let typ = format!("{}{typ}{}", "Array<".repeat(n), ">".repeat(n));
        let v = format!("{}{leaf}{}", "[".repeat(n), "]".repeat(n));
        format!(
            "{{ let v: {typ} = {v}; \
             select json::write_str(v) {{ \
               error as e => \"WRITE [e]\", \
               s => {{ let r: Result<{typ}, [`JsonErr(string), `InvalidCast(string)]> = json::read(s); \
                 select r {{ error as e => \"READ [e]\", _ => \"ok\" }} }} \
             }} }}"
        )
    };
    let n = 127 - containers;
    let deepest = eval_modes(&program(n), crate::TEST_REGISTER).await?;
    assert_eq!(deepest, Ok(Value::String(literal!("ok"))), "{typ} under {n}");
    match eval_modes(&program(n + 1), crate::TEST_REGISTER).await? {
        Ok(Value::String(s)) if s.starts_with("WRITE") => (),
        r => panic!("{typ} under {}: {r:?}", n + 1),
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn the_deepest_written_value_reads_back() -> Result<()> {
    deepest_written_reads_back("i64", "1", 0).await
}

/// An empty container is a container though it holds no leaf
/// (small-pkgs-09).
#[tokio::test(flavor = "multi_thread")]
async fn an_empty_innermost_container_counts() -> Result<()> {
    deepest_written_reads_back("Array<i64>", "[]", 1).await?;
    deepest_written_reads_back("Map<string, i64>", "{}", 1).await
}

// Two map keys that print alike are never merged into one: a key type
// holding strings and another class writes pairs, which a TOML document
// cannot hold at its top.
run!(json_keys_writing_alike_are_not_merged, r#"{
    let m: Map<[i64, string], i64> = {1 => 10, "1" => 20};
    "[json::write_str(m)] [toml::write_str(m)]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s))
        if s.contains(r#"[[1,10],["1",20]]"#) && s.contains("TomlErr"))
}; FuseExpect::Jit);

// An f32 is written as the number the program holds, not its exact f64
// widening, by json and toml alike, and reads back to itself.
run!(f32_written_as_held, r#"{
    let v = {c: f32:0.1};
    let s = json::write_str(v)$;
    let t = toml::write_str(v)$;
    let back: {c: f32} = json::read(s)$;
    "[s] [str::trim(t)] [back.c == f32:0.1]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"{"c":0.1} c = 0.1 true"#)
}; FuseExpect::Jit);

// A reference is not data: a writer's `Data` bound refuses one the check
// sees, and the writers refuse one only the value holds rather than write
// its cell.
run!(writers_refuse_a_typed_reference, r#"{
    let x = 5;
    json::write_str({a: &x, b: 1})
}"#, refused("{ a: &i64, b: i64 } is not data: data holds no function and no reference"); FuseExpect::None);

run!(writers_refuse_references, r#"{
    let x = 5;
    let v: Any = {a: &x, b: 1};
    let j = select json::write_str(v) { error as e => "[e]", s => s };
    let t = select toml::write_str(v) { error as e => "[e]", s => s };
    let h = select hbs::render("{{a}}", v) { error as e => "[e]", s => s };
    "[j]|[t]|[h]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"error:`JsonErr("a reference is not data")|error:`TomlErr("a reference is not data")|error:`HbsErr("a reference is not data")"#)
}; FuseExpect::Jit);

run!(writers_refuse_functions, r#"{
    let v: Any = {a: |x: i64| x + 1, b: 1};
    let j = select json::write_str(v) { error as e => "[e]", s => s };
    let t = select toml::write_str(v) { error as e => "[e]", s => s };
    let h = select hbs::render("{{a}}", v) { error as e => "[e]", s => s };
    "[j]|[t]|[h]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"error:`JsonErr("a function or a value of a type backed by Rust is not data")|error:`TomlErr("a function or a value of a type backed by Rust is not data")|error:`HbsErr("a function or a value of a type backed by Rust is not data")"#)
}; FuseExpect::Jit);

run!(json_composite_key_uses_pairs, r#"json::write_str({[1, 2] => 3})$"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"[[[1,2],3]]"#)
}; FuseExpect::None);

// json and toml write a list as a flat array and read it back so: a list
// whose last element is an empty array keeps it.
run!(flat_list_round_trips, r#"{
    type T = {xs: List<Array<i64>>};
    let a: T = {xs: [<[1, 2], []>]};
    let b: T = {xs: [<[], []>]};
    let ja: T = json::read(json::write_str(a)$)$;
    let jb: T = json::read(json::write_str(b)$)$;
    let ta: T = toml::read(toml::write_str(a)$)$;
    let tb: T = toml::read(toml::write_str(b)$)$;
    (list::len(ja.xs), list::len(jb.xs), list::len(ta.xs), list::len(tb.xs))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:2, i64:2, i64:2, i64:2]")
}; FuseExpect::Jit);

// An untyped array is never a list's cells: cast to a list it converts
// element by element, whatever its last element is, and a list written
// by pack, json or toml reads back whole (t-expr-core-01).
run!(untyped_arrays_cast_to_lists_whole, r#"{
    let a: Any = [[1], []];
    let b: Any = [[1], [2]];
    let c: Any = [[1], [2], []];
    let la: List<Array<i64>> = cast<List<Array<i64>>>(a)$;
    let lb: List<Array<i64>> = cast<List<Array<i64>>>(b)$;
    let lc: List<Array<i64>> = cast<List<Array<i64>>>(c)$;
    let l: List<Array<i64>> = [<[1, 2], []>];
    let j: List<Array<i64>> = json::read(json::write_str(l)$)$;
    let p: List<Array<i64>> = pack::read(pack::write_bytes(l)$)$;
    (list::len(la), list::len(lb), list::len(lc), json::write_str(l)$, j, p)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"[i64:2, i64:2, i64:3, "[[1,2],[]]", [[i64:1, i64:2], []], [[i64:1, i64:2], []]]"#)
}; FuseExpect::Jit);

// A bare variant casts to its tag; data reads into each form a union
// holds by its own class first.
run!(composite_casts_by_form, r#"{
    let tag = cast<string>(`Ready)$;
    let s: [string, `A] = cast<[string, `A]>("A")$;
    let t: [`A, `B] = cast<[`A, `B]>("A")$;
    let pair: (i64, string) = pack::read(pack::write_bytes((1, "x"))$)$;
    (tag, select s { string as _ => "string", `A => "tag" }, t, pair)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["Ready", "string", "A", [i64:1, "x"]]"#)
}; FuseExpect::Jit);

// A writer reads a value's kind markers, never its data form: a union
// member is the one the value is (a struct beside an array of pairs, a
// tuple of pairs beside null, an inline struct under a nullable), and a
// list's elements keep their type (x-kind-markers-01, t-rep-03).
run!(writers_choose_members_by_kind, r#"{
    let f: [{a: i64}, Array<(string, i64)>] = {a: 1};
    let c: [((string, i64), (string, i64)), null] = (("a", 1), ("b", 2));
    let l: List<Array<(string, i64)>> = [<[("a", 1)]>];
    let o: [{a: Array<(string, i64)>, b: i64}, null] = {a: [("k", 1)], b: 7};
    let t = toml::write_str({v: f})$;
    "[json::write_str(f)$] [json::write_str(c)$] [json::write_str(l)$] [json::write_str(o)$] [str::trim(t)]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s
        == "{\"a\":1} [[\"a\",1],[\"b\",2]] [[[\"a\",1]]] {\"a\":[[\"k\",1]],\"b\":7} [v]\na = 1")
}; FuseExpect::Jit);

// A datetime or duration map key is written as such a value is, so the
// map reads back into its type (x-rerun-3-01).
run!(time_map_keys_read_back, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let m: Map<datetime, i64> = {datetime:"2020-01-01T00:00:00Z" => 1};
    let d: Map<duration, i64> = {duration:1.5s => 2};
    let m2: Result<Map<datetime, i64>, E> = json::read(json::write_str(m)$);
    let d2: Result<Map<duration, i64>, E> = json::read(json::write_str(d)$);
    (json::write_str(m)$, m2$ == m, json::write_str(d)$, d2$ == d)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"["{\"2020-01-01T00:00:00+00:00\":1}", true, "{\"1.5\":2}", true]"#)
}; FuseExpect::Jit);

// A key whose data is a string is written bare: a map keyed by a bare
// variant or by an abstract string type reads back into its type, and a
// bare variant beside the string it writes as is refused.
run!(string_data_map_keys_read_back, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type Id = Abstract<string>;
    impl Cast for Id {};
    let m: Map<[`A, `B], i64> = {`A => 1, `B => 2};
    let i: Map<Id, i64> = {Id("abc") => 3};
    let both: Map<[`A, string], i64> = {`A => 1, "A" => 2};
    let m2: Result<Map<[`A, `B], i64>, E> = json::read(json::write_str(m)$);
    let i2: Result<Map<Id, i64>, E> = json::read(json::write_str(i)$);
    (json::write_str(m)$, m2$ == m, json::write_str(i)$, i2$ == i, json::write_str(both))
}"#, prints(r#"["{\"A\":1,\"B\":2}", true, "{\"abc\":3}", true, error:["JsonErr", "two keys write as the same JSON data"]]"#); FuseExpect::Jit);

// An integer or decimal target reads a number's text exactly, refusing
// what does not fit, a float target the float nearest it, and a read with
// neither an f64 (small-pkgs-02).
run!(json_number_read_exactly, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let show = |r: Result<[i64, decimal, f64], E>| select r { error as _ => "refused", v => "[v]" };
    let a: Result<i64, E> = json::read("-9223372036854775809");
    let b: Result<i64, E> = json::read("-9223372036854776000");
    let c: Result<i64, E> = json::read("9007199254740993.0");
    let d: Result<decimal, E> = json::read("12345678901234567890.123456789");
    let e: Result<{n: i64, x: f64}, E> = json::read("{\"n\": 1, \"x\": 0.30000000000000000001}");
    let f: Result<Array<Any>, E> = json::read("\[9007199254740993.5, 0.1\]");
    (show(a), show(b), show(c), show(d), e$.x, f$)
}"#, prints(r#"["refused", "refused", "9007199254740993", "12345678901234567890.123456789", f64:0.3, [f64:9007199254740994., f64:0.1]]"#); FuseExpect::Jit);

// Exactness is the position's: beside an integer field, a number read
// into Any or a string is the f64 a read with no integer gives, and one
// read into a decimal stays exact (x-kind-markers-06).
run!(json_exact_number_is_per_position, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let a: Result<{n: i64, s: string, xs: Array<Any>}, E> = json::read(
        "{\"n\": 1, \"s\": 0.1000000000000000000001, \"xs\": \[0.1000000000000000000001, 9007199254740993.5\]}"
    );
    let b: Result<{d: decimal, x: Any}, E> = json::read(
        "{\"d\": 0.1000000000000000000001, \"x\": 0.1000000000000000000001}"
    );
    (a$.s, a$.xs, b$.d, b$.x)
}"#, prints(r#"["0.1", [f64:0.1, f64:9007199254740994.], decimal:0.1000000000000000000001, f64:0.1]"#); FuseExpect::None);

// A number past f64's range is an error, never a NaN the text never held
// (x-engine-collections-05).
run!(json_number_out_of_range, r#"{
    let r: Result<f64, [`JsonErr(string), `InvalidCast(string)]> = json::read("1e400");
    select r { error as e => "[e]", v => "[v]" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.starts_with("error:`JsonErr(\"number out of range"))
}; FuseExpect::Jit);

/// Bytes are written as an array: one container more (x-rerun-3-03).
#[tokio::test(flavor = "multi_thread")]
async fn written_bytes_count_as_a_container() -> Result<()> {
    deepest_written_reads_back("bytes", "bytes:AQID", 1).await
}

run!(
    composite_map_keys_use_pairs,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let round_trip = 'k: Ordered + Concrete |m: Map<'k, i64>| {
        let r: Result<Map<'k, i64>, E> = json::read(json::write_str(m)$);
        select r { error as _ => false, v => v == m }
    };
    [round_trip({(1, 2) => 3}), round_trip({{a: 1} => 2}),
     round_trip({[1, 2] => 3}), round_trip({list::from_array([1, 2]) => 3}),
     round_trip({`A(1) => 2}), round_trip({null => 1}),
     round_trip({buffer::from_string("hi") => 1})]
}
"#,
    prints("[true, true, true, true, true, true, true]")
);

// an object's keys read back as strings: string keys beside keys of
// another class, or under a key type that may hold both, round-trip as pairs
run!(
    mixed_class_map_keys_use_pairs,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let round_trip = 'k: Ordered + Concrete |m: Map<'k, i64>| {
        let r: Result<Map<'k, i64>, E> = json::read(json::write_str(m)$);
        select r { error as _ => false, v => v == m }
    };
    let a: Map<[string, i64], i64> = {1 => 1, "a" => 2};
    let b: Map<[string, i64], i64> = {1 => 1};
    let c: Map<[bool, string], i64> = {true => 1, "true" => 2};
    [round_trip(a), round_trip(b), round_trip(c), round_trip({1 => 2}), round_trip({"x" => 2})]
}
"#,
    prints("[true, true, true, true, true]")
);

// A whole f32 or duration is written as a float, which a union reads
// into an integer member as its own class (x-rerun-5-04): a target, or a
// map key, holding either beside an integer is refused.
run!(
    json_target_float_beside_an_integer_refused,
    r#"{
    let d: [duration, i64] = duration:2.s;
    let r: Result<[duration, i64], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(d)$);
    r
}"#,
    refused(
        "'b: JsonData refuses [i64, duration]: what its member i64 or duration writes \
         as JSON data may read back as the other"
    )
);

run!(
    json_map_key_float_beside_an_integer_refused,
    r#"{
    let m: Map<[duration, i64], i64> = {duration:2.s => 1, 3 => 2};
    let r: Result<Map<[duration, i64], i64>, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(m)$);
    r
}"#,
    refused("'b: JsonData refuses Map<[i64, duration], i64>")
);

// A float key's text keeps its point, so a whole one reads back as the
// float beside a datetime, which reads the same digits as seconds.
run!(
    json_float_key_beside_a_datetime_reads_back,
    r#"{
    type T = Map<[f64, datetime], i64>;
    type E = [`JsonErr(string), `InvalidCast(string)];
    let rt = |v: T| -> bool {
        let r: Result<T, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    [
        rt({3.0 => 1}),
        rt({1e20 => 1}),
        rt({0.5 => 1}),
        rt({18014398509481984.0 => 1, datetime:"1970-01-01T00:00:03Z" => 2})
    ]
}"#,
    prints("[true, true, true, true]")
);

run!(
    json_target_f32_beside_an_integer_refused,
    r#"{
    let v: [f32, i64] = f32:2.0;
    let r: Result<[f32, i64], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [i64, f32]")
);

// An f32 beside an integer no wider than 16 bits reads back as written: a
// whole f32 that integer could hold is written exactly and stays real, a
// fraction rounds back to the f32, and the integer's text stays an
// integer; so too in a tuple, and an integer beside an f64 under a name.
run!(
    json_f32_beside_a_narrow_integer_reads_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type U = [f32, u8];
    type S = [f32, i16];
    type B = u8;
    type N = [f64, B];
    type T = [(f32, i64), (u8, i64)];
    let u = |v: U| -> bool {
        let r: Result<U, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let s = |v: S| -> bool {
        let r: Result<S, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let n = |v: N| -> bool {
        let r: Result<N, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let t = |v: T| -> bool {
        let r: Result<T, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [u(f32:0.1), u(f32:2.0), u(f32:1e10), u(u8:2), u(u8:255)],
        [s(f32:-3.0), s(i16:-3), s(f32:300.5), s(i16:32767)],
        [n(u8:2), n(f64:2.0), n(f64:0.1)],
        [t((u8:2, 5)), t((f32:2.0, 5)), t((f32:0.1, 5))]
    )
}"#,
    prints(
        "[[true, true, true, true, true], [true, true, true, true], [true, true, true], [true, true, true]]"
    )
);

// A tuple whose f32s a narrow integer could hold reads back beside a
// tuple of such integers: an f32 the integer holds is written exactly.
run!(
    json_f32_tuple_beside_a_narrow_integer_tuple_reads_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type T = [(f32, f32), (u8, u8)];
    type A = [Array<f32>, (u8, u8)];
    let t = |v: T| -> bool {
        let r: Result<T, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let a = |v: A| -> bool {
        let r: Result<A, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [t((f32:2.0, f32:3.0)), t((f32:0.1, f32:3.0)), t((u8:2, u8:3)), t((f32:1e10, f32:0.5))],
        [a([f32:2.0, f32:3.0]), a([f32:0.1, f32:3.0]), a((u8:2, u8:3))]
    )
}"#,
    prints("[[true, true, true, true], [true, true, true]]")
);

// A member's reading is judged whole: an f32 its own member rounds loses
// to a member reading every part exactly, numbers crossed or not.
run!(
    json_target_f32_tuple_beside_an_exact_reader_refused,
    r#"{
    type T = [(f32, u8), (f64, f64)];
    let v: T = (f32:0.1, u8:1);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(f32, u8), (f64, f64)]")
);

run!(
    json_target_f32_tuple_beside_a_wide_integer_refused,
    r#"{
    type T = [(f32, u16), (i64, f32)];
    let v: T = (f32:1234567936.0, u16:1);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(i64, f32), (f32, u16)]")
);

run!(
    json_target_f32_struct_beside_an_exact_reader_refused,
    r#"{
    type T = [{a: f32, b: u8}, {a: f64, b: f32}];
    let v: T = {a: f32:0.1, b: u8:1};
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [{ a: f32, b: u8 }, { a: f64, b: f32 }]")
);

// A duration's member reads its seconds as no number, so a member reading
// every part exactly takes the tuple whatever it crosses.
run!(
    json_target_duration_tuple_beside_an_exact_reader_refused,
    r#"{
    type T = [(duration, u8), (f64, f64)];
    let v: T = (duration:3.s, u8:1);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(f64, f64), (duration, u8)]")
);

// A member reading a value outside its class takes it where the writer's
// own member only rounds it: an f32's 0.1 reads back as the string "0.1",
// a string's "1" beside an f32's 0.1 as the integer 1.
run!(
    json_target_f32_beside_a_string_refused,
    r#"{
    let v: [f32, string] = f32:0.1;
    let r: Result<[f32, string], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [f32, string]")
);

run!(
    json_target_string_tuple_beside_a_number_tuple_refused,
    r#"{
    type T = [(string, f32), (i64, f64)];
    let v: T = ("1", f32:0.1);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(i64, f64), (string, f32)]")
);

// Two readings outside the data's class go by the union's order: (1, 3.5)
// written by (i64, duration) reads back as (u8:1, "3.5"), which comes
// first.
run!(
    json_target_readings_outside_the_class_refused,
    r#"{
    type T = [(i64, duration), (u8, string)];
    let v: T = (1, duration:3.5s);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(u8, string), (i64, duration)]")
);

// A union is in the data's class where any member is, whichever member
// reads it: (u8, [i64, string]) keeps the class of [3, 3.5] (i64 is a
// number) and comes first, so (3, 3.5) reads back as (u8:3, "3.5").
run!(
    json_target_union_part_in_the_class_refused,
    r#"{
    type T = [(i64, f64), (u8, [i64, string])];
    let v: T = (3, 3.5);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(u8, [i64, string]), (i64, f64)]")
);

// Members that recurse into each other judge every level of a written
// value: `Cons(f32:0.1, 3) writes ["Cons",0.1,3], which TB reads exactly
// as `Cons(0.1, 3.0) where TA only rounds the f32.
run!(
    json_target_recursive_members_refused,
    r#"{
    type TA = [i64, `Cons(f32, TA)];
    type TB = [f64, `Cons(f64, TB)];
    let v: [TA, TB] = `Cons(f32:0.1, 3);
    let r: Result<[TA, TB], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [TA, TB]")
);

// A bool rounds into a real member: P's (f32:0.1, true) writes
// [0.1,true], which A reads as [0.1, 1.0] in the round where P's f32
// rounds too, and A comes first. A tuple read as a part is never in the
// data's class, so ((1, 2), 3) is tried after the first member too, which
// reads it back as (["1", "2"], 3).
run!(
    json_target_bool_read_as_a_real_refused,
    r#"{
    type A = Array<f64>;
    type P = [bool, (f32, bool)];
    let p: P = (f32:0.1, true);
    let v: [A, P] = p;
    let r: Result<[A, P], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [A, P]")
);

run!(
    json_target_nested_tuple_outside_the_class_refused,
    r#"{
    type T = [(Array<string>, i64), ((u8, u8), u8)];
    let v: T = ((u8:1, u8:2), u8:3);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [(Array<string>, i64), ((u8, u8), u8)]")
);

// Recursive members whose tags tell their values apart read back.
run!(
    json_recursive_members_read_back,
    r#"{
    type TA = [`NA, (f32, TA)];
    type TB = [`NB, (f64, TB)];
    type E = [`JsonErr(string), `InvalidCast(string)];
    let rt = |v: [TA, TB]| -> bool {
        let r: Result<[TA, TB], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let a: TA = (f32:0.1, (f32:2.5, `NA));
    let b: TB = (0.1, (3.0, `NB));
    [rt(a), rt(b), rt(`NA), rt(`NB)]
}"#,
    prints("[true, true, true, true]")
);

// A typedef applied to arguments expands once for each list of them, so
// the check of a recursion that passes its parameter on ends.
run!(
    json_applied_recursive_typedef_reads_back,
    r#"{
    type R<'a> = [`Nil, `Cons('a, R<'a>)];
    type E = [`JsonErr(string), `InvalidCast(string)];
    let v: R<u64> = `Cons(u64:1, `Cons(u64:2, `Nil));
    let r: Result<R<u64>, E> = json::read(json::write_str(v)$);
    (r$ == v, r$)
}"#,
    prints("[true, [\"Cons\", u64:1, [\"Cons\", u64:2, \"Nil\"]]]")
);

// One that grows its parameters at every level never ends.
run!(
    json_data_refuses_a_typedef_growing_its_parameters,
    r#"{
    type N<'a> = [`Nil, `Cons('a, N<Array<'a>>)];
    let f = 'a: JsonData |x: 'a| -> 'a x;
    let v: N<i64> = `Nil;
    f(v)
}"#,
    refused(
        "'a: JsonData refuses [`Cons(i64, N<Array<i64>>), `Nil]: N grows its \
         parameters as it recurses"
    )
);

// A map's keys are written as their text, which reads back by the key
// union's order: a decimal's "3" as the datetime 3 seconds past the epoch.
run!(
    json_target_key_text_read_as_another_member_refused,
    r#"{
    type M = Map<[decimal, datetime], i64>;
    let v: M = {cast<decimal>(3)$ => 1};
    let r: Result<M, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses Map<[datetime, decimal], i64>")
);

// What the union's pick reads back as written: a struct that takes no
// field the data lacks comes first, the reader refuses a field it has
// not; a datetime's text names no variant; a reading outside the class
// that comes first in the union keeps a tuple.
run!(
    json_unions_read_back_by_their_pick,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type S = [{a: i64}, {a: i64, b: [i64, null]}];
    type V = [`B(u8), (datetime, f32)];
    type D = [(u8, duration), (i64, string)];
    type O = [(duration, u8), (string, u8)];
    let s = |v: S| -> bool {
        let r: Result<S, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let d = |v: D| -> bool {
        let r: Result<D, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let vt = |v: V| -> bool {
        let r: Result<V, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let o = |v: O| -> bool {
        let r: Result<O, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [s({a: 1}), s({a: 1, b: null}), s({a: 1, b: 2})],
        [vt(`B(u8:2)), vt((datetime:"2020-01-01T00:00:00Z", f32:0.1))],
        [d((u8:1, duration:3.5s)), d((1, "3.5")), d((300, "a"))],
        [o((duration:3.5s, u8:1)), o(("3.5", u8:1))]
    )
}"#,
    prints("[[true, true, true], [true, true], [true, true, true], [true, true]]")
);

// An abstract type written as its representation is built from the data,
// never read in its class: a bool member that comes first takes S's
// "true", an integer member Id's "1"; the abstract still takes a number
// a float member would read across into a real.
run!(
    json_target_abstract_string_beside_a_bool_refused,
    r#"{
    type S = Abstract<string>;
    impl Cast for S {};
    let v: [S, bool] = S("true");
    let r: Result<[S, bool], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [bool, S]")
);

run!(
    json_target_abstract_key_beside_an_integer_refused,
    r#"{
    type Id = Abstract<string>;
    impl Cast for Id {};
    let m: Map<[Id, i64], i64> = {Id("1") => 1};
    let r: Result<Map<[Id, i64], i64>, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(m)$);
    r
}"#,
    refused("'b: JsonData refuses Map<[i64, Id], i64>")
);

// A map keyed by a type that may read a key's text as a string writes its
// keys as pairs where any is not a string: as an object's key Id(3) would
// read back as Id("3").
run!(
    json_abstract_key_that_may_write_a_string_reads_back,
    r#"{
    type Id = Abstract<[i64, string]>;
    impl Cast for Id {};
    type E = [`JsonErr(string), `InvalidCast(string)];
    let m = |v: Map<Id, string>| -> bool {
        let r: Result<Map<Id, string>, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let a = |v: Array<{users: Map<Id, string>}>| -> bool {
        let r: Result<Array<{users: Map<Id, string>}>, E> =
            json::read(json::write_str(v)$);
        r$ == v
    };
    [
        m({Id(3) => "a"}),
        m({Id("x") => "b"}),
        m({Id(3) => "a", Id("x") => "b"}),
        a([{users: {Id(3) => "a"}}])
    ]
}"#,
    prints("[true, true, true, true]")
);

// An abstract key type read as its representation reads a key's text as
// its representation does, whatever its `to_value` writes: Id's keys make
// an object. One whose `from_value` is written may read the text as a
// string, so its keys make pairs.
run!(
    json_abstract_key_read_as_its_representation_writes_an_object,
    r##"{
    type Id = Abstract<i64>;
    impl Cast for Id { let to_value = |x| x.0 };
    type Fv = Abstract<i64>;
    impl Cast for Fv {
        let to_value = |x| x.0;
        let from_value = |v: Any| -> Result<Fv, `InvalidCast(string)> select cast<i64>(v) {
            error as _ => error(`InvalidCast("fv")),
            n => Fv(n)
        }
    };
    type E = [`JsonErr(string), `InvalidCast(string)];
    let id: Map<Id, string> = {Id(3) => "a", Id(4) => "b"};
    let fv: Map<Fv, string> = {Fv(3) => "a"};
    let (wi, wf) = (json::write_str(id)$, json::write_str(fv)$);
    let ri: Result<Map<Id, string>, E> = json::read(wi);
    let rf: Result<Map<Fv, string>, E> = json::read(wf);
    [wi == r#"{"3":"a","4":"b"}"#, ri$ == id, wf == r#"[[3,"a"]]"#, rf$ == fv]
}"##,
    prints("[true, true, true, true]")
);

// The primitive `array` writes an array of any data, which a tuple, a
// payload variant and bytes read: beside any of them it is refused.
run!(
    json_target_primitive_array_beside_a_tuple_refused,
    r#"{
    type T = [array, (i64, i64)];
    let v: T = (1, 2);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [array, (i64, i64)]")
);

run!(
    json_target_primitive_array_beside_a_payload_variant_refused,
    r#"{
    type T = [array, `A(i64)];
    let v: T = `A(2);
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [array, `A(i64)]")
);

run!(
    json_target_primitive_array_beside_bytes_refused,
    r#"{
    type T = [array, bytes];
    let v: T = bytes:AQI=;
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [array, bytes]")
);

// A key's text is never read as an array, so the primitive `array` in a key
// type leaves its other members' texts judged.
run!(
    json_target_primitive_array_key_beside_a_datetime_refused,
    r#"{
    type Aa = datetime;
    type Zz = f64;
    type T = Map<[array, Aa, Zz], i64>;
    let v: T = {3.0 => 1};
    let r: Result<T, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses Map<[array, Aa, Zz], i64>")
);

run!(
    json_primitive_array_beside_scalars_reads_back,
    r#"{
    type T = [array, i64, string, null];
    type E = [`JsonErr(string), `InvalidCast(string)];
    let t = |v: T| -> bool {
        let r: Result<T, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    [t([1, "a"]), t([3.0, null, [2]]), t(3), t("a"), t(null)]
}"#,
    prints("[true, true, true, true, true]")
);

// The writer follows an abstract key type's representation through
// recursion: N's never holds a string, B's does, and G's arguments grow as
// it recurses, so it may.
run!(
    json_abstract_key_writer_follows_recursive_representations,
    r#"{
    type N = Abstract<[N, i64]>;
    impl Cast for N {};
    type A = Abstract<[B, i64]>;
    type B = Abstract<[A, string]>;
    impl Cast for A {};
    impl Cast for B {};
    type G<'a> = Abstract<['a, G<Array<'a>>]>;
    impl<'a> Cast for G<'a> {};
    let n: Map<N, i64> = {N(N(5)) => 1};
    let a: Map<A, i64> = {A(5) => 1};
    let g: Map<G<i64>, i64> = {G(5) => 1};
    [
        json::write_str(n)$ == "{\"5\":1}",
        json::write_str(a)$ == r"[[5,1]]",
        json::write_str(g)$ == r"[[5,1]]"
    ]
}"#,
    prints("[true, true, true]")
);

// An abstract type whose `Cast` implementation writes `from_value` may read
// any value it is tried on: tried before a bare variant, it reads the
// variant's tag. A payload variant's tag is tried first, so one beside it
// reads back.
run!(
    json_target_abstract_reading_by_from_value_beside_a_tag_refused,
    r#"{
    type Id = Abstract<i64>;
    impl Cast for Id {
        let from_value = |v| select cast<i64>(v) {
            error as _ => select cast<string>(v) { error as e => e, s => Id(str::len(s)) },
            i => Id(i)
        }
    };
    let v: [Id, `A] = `A;
    let r: Result<[Id, `A], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [Id, `A]")
);

run!(
    json_abstract_reading_by_from_value_beside_a_payload_variant_reads_back,
    r#"{
    type Id = Abstract<i64>;
    impl Cast for Id {
        let from_value = |v| select cast<i64>(v) {
            error as _ => select cast<string>(v) { error as e => e, s => Id(str::len(s)) },
            i => Id(i)
        }
    };
    type E = [`JsonErr(string), `InvalidCast(string)];
    let rt = |v: [Id, `B(string)]| -> bool {
        let r: Result<[Id, `B(string)], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    [rt(Id(3)), rt(`B("x")), rt(`B("12"))]
}"#,
    prints("[true, true, true]");
    FuseExpect::None
);

// An abstract whose Cast implementation writes to_value and leaves
// from_value out writes what to_value makes and reads as its
// representation, after every member keeping the data's class.
run!(
    json_target_abstract_reading_as_its_representation_beside_a_tag_refused,
    r#"{
    type S = Abstract<string>;
    impl Cast for S { let to_value = |x| x.0 };
    let v: [S, `A] = `A;
    let r: Result<[S, `A], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [S, `A]")
);

run!(
    json_target_abstract_reading_as_its_representation_beside_a_decimal_refused,
    r#"{
    type N = Abstract<i64>;
    impl Cast for N { let to_value = |x| x.0 };
    let v: [N, decimal] = cast<decimal>(3)$;
    let r: Result<[N, decimal], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [decimal, N]")
);

run!(
    json_target_abstract_beside_a_decimal_refused,
    r#"{
    type N = Abstract<i64>;
    impl Cast for N {};
    let v: [N, decimal] = cast<decimal>(3)$;
    let r: Result<[N, decimal], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [decimal, N]")
);

// An abstract number beside a set of a real and a string: the set reads
// an integer past an f64's precision as its text before the abstract is
// tried.
run!(
    json_target_abstract_beside_a_primitive_set_refused,
    r#"{
    type Id = Abstract<i64>;
    impl Cast for Id {};
    type Val = [f64, string];
    let v: [Id, Val] = Id(9007199254740993);
    let r: Result<[Id, Val], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [Id, Val]")
);

run!(
    json_abstract_number_beside_a_float_reads_back,
    r#"{
    type N = Abstract<i64>;
    impl Cast for N {};
    type E = [`JsonErr(string), `InvalidCast(string)];
    let rt = |v: [N, f64]| -> bool {
        let r: Result<[N, f64], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    [rt(N(2)), rt(f64:2.5), rt(f64:2.0)]
}"#,
    prints("[true, true, true]");
    FuseExpect::None
);

// An abstract type whose representation holds itself writes N(N(x)) as
// N(x) does, so a target holding it is refused, as a map keyed by one is,
// and two abstracts holding each other.
run!(
    json_target_abstract_holding_itself_refused,
    r#"{
    type N = Abstract<[N, `L(i64)]>;
    impl Cast for N {};
    let v: N = N(N(`L(3)));
    let r: Result<N, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses N: what its member N or `L(i64)")
);

run!(
    json_target_key_abstract_holding_itself_refused,
    r#"{
    type N = Abstract<[N, i64]>;
    impl Cast for N {};
    let m: Map<N, i64> = {N(N(5)) => 1};
    let r: Result<Map<N, i64>, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(m)$);
    r
}"#,
    refused("'b: JsonData refuses Map<N, i64>: what its member i64 or N")
);

run!(
    json_target_abstracts_holding_each_other_refused,
    r#"{
    type N = Abstract<[M, `L(i64)]>;
    type M = Abstract<[N, `K(string)]>;
    impl Cast for N {};
    impl Cast for M {};
    let v: [N, `B(string)] = N(M(N(`L(1))));
    let r: Result<[N, `B(string)], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [N, `B(string)]: what its member N or `K(string)")
);

// A union member of a union is tried where any of its members keeps the
// data's class, as a scalar is: numbers read back beside a string, a
// nullable beside a string or a bool. A text union beside it still holds
// a tag the string member reads.
run!(
    json_union_typedef_members_read_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type Num = [i64, f64];
    type Opt = [i64, null];
    type T = [Num, string];
    type S = {a: [Opt, string], b: Array<[Opt, bool]>};
    let t = |v: T| -> bool {
        let r: Result<T, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let s = |v: S| -> bool {
        let r: Result<S, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [t(3), t(3.0), t(0.5), t("3"), t("x")],
        [s({a: 3, b: [null, true]}), s({a: "3", b: [4, false]}), s({a: null, b: []})]
    )
}"#,
    prints("[[true, true, true, true, true], [true, true, true]]")
);

// One primitive reads one value one way: two members holding an f32 at
// one position read a large whole f32 alike, so neither takes the other's.
run!(
    json_typedef_tuples_sharing_an_f32_read_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type A = (i32, f32);
    type B = (datetime, f32);
    let rt = |v: [A, B]| -> bool {
        let r: Result<[A, B], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let d = datetime:"1970-01-01T00:00:03Z";
    [rt((i32:5, f32:1234567936.0)), rt((i32:5, f32:0.1)), rt((d, f32:1234567936.0)), rt((d, f32:0.1))]
}"#,
    prints("[true, true, true, true]");
    FuseExpect::None
);

run!(
    json_target_union_typedef_beside_a_tag_refused,
    r#"{
    type Num = [i64, f64];
    type Txt = [string, `None];
    let v: [Num, Txt] = `None;
    let r: Result<[Num, Txt], [`JsonErr(string), `InvalidCast(string)]> =
        json::read(json::write_str(v)$);
    r
}"#,
    refused("'b: JsonData refuses [Num, Txt]: what its member string or `None")
);

// A primitive set reads a value as one member, the first holding it
// exactly by class and then bit, and a member the value read back already
// is takes it whatever the union's order: an integer beside a set of a
// real and a string, a real beside a set of a string and an integer.
run!(
    json_primitive_set_beside_a_member_reads_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type Amount = [f64, string];
    type Qty = i64;
    type Count = u32;
    type Label = [string, i64];
    type Score = f64;
    type A = Array<{amount: [Amount, Qty]}>;
    type C = {total: [Amount, Count]};
    type L = [Label, Score];
    let a = |v: A| -> bool {
        let r: Result<A, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let c = |v: C| -> bool {
        let r: Result<C, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let l = |v: L| -> bool {
        let r: Result<L, E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [
            a([{amount: 3}]),
            a([{amount: 9007199254740993}]),
            a([{amount: 2.5}]),
            a([{amount: "TBD"}]),
            a([{amount: "3"}])
        ],
        [
            c({total: u32:0}),
            c({total: u32:4294967295}),
            c({total: 4294967296.0}),
            c({total: "3"}),
            c({total: ""})
        ],
        [l(3.0), l(0.5), l(1e300), l("x"), l("3.0"), l(3), l(9007199254740993)]
    )
}"#,
    prints(
        "[[true, true, true, true, true], [true, true, true, true, true], \
         [true, true, true, true, true, true, true]]"
    )
);

// A union member whose own members write values of two classes is tried
// by the class of each value it writes: a struct or a variant it holds is
// outside the class, the f64 or the string beside it inside.
run!(
    json_union_typedef_members_of_two_classes_read_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type Shape = [{w: f64, h: f64}, f64];
    type Ev = [`Click(i64), string];
    type D = [duration, string];
    let a = |v: [Shape, string]| -> bool {
        let r: Result<[Shape, string], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let b = |v: [Ev, i64]| -> bool {
        let r: Result<[Ev, i64], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    let c = |v: [D, bool]| -> bool {
        let r: Result<[D, bool], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    (
        [a({w: 1.0, h: 2.0}), a(1.5), a(3.0), a("1.5"), a("x")],
        [b(`Click(1)), b("1"), b("x"), b(1)],
        [c(duration:3.s), c("true"), c("x"), c(true)]
    )
}"#,
    prints(
        "[[true, true, true, true, true], [true, true, true, true], [true, true, true, true]]"
    )
);

// A recursive member's values are all in the class of its own reader, so
// a tuple beside it keeps to its own values.
run!(
    json_recursive_member_beside_a_tuple_reads_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type B = [(string, string), null];
    type R = [Array<R>, i64];
    let rt = |v: [B, R]| -> bool {
        let r: Result<[B, R], E> = json::read(json::write_str(v)$);
        r$ == v
    };
    [rt(("a", "b")), rt(("1", "2")), rt(null), rt([1, 2]), rt([[1], 2]), rt(5), rt([])]
}"#,
    prints("[true, true, true, true, true, true, true]");
    FuseExpect::None
);

// An f64 member keeps a whole float beside an integer, and a duration
// key beside a bool reads back.
run!(
    json_float_members_written_apart_read_back,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let a: [f64, u8] = f64:2.0;
    let b: [f64, u8] = u8:2;
    let c: Map<[duration, bool], i64> = {duration:2.s => 1, true => 2};
    let ra: Result<[f64, u8], E> = json::read(json::write_str(a)$);
    let rb: Result<[f64, u8], E> = json::read(json::write_str(b)$);
    let rc: Result<Map<[duration, bool], i64>, E> = json::read(json::write_str(c)$);
    (ra$ == a, rb$ == b, rc$ == c)
}"#,
    prints("[true, true, true]")
);

// An array of whole floats reads back as itself beside a tuple of
// integers its length: the tuple would read each float as an integer.
run!(
    json_whole_floats_read_back_beside_an_integer_tuple,
    r#"{
    type U = [(i64, i64), Array<f64>];
    type E = [`JsonErr(string), `InvalidCast(string)];
    let a: U = [1.0, 2.0];
    let t: U = (1, 2);
    let ra: Result<U, E> = json::read(json::write_str(a)$);
    let rt: Result<U, E> = json::read(json::write_str(t)$);
    (ra$ == a, rt$ == t)
}"#,
    prints("[true, true]")
);

run!(colliding_pair_keys_are_refused, r#"{
    let m: Map<[(i64, i64), List<i64>], i64> = {(1, 2) => 1, [<1, 2>] => 2};
    select json::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

run!(colliding_bytes_and_array_keys_are_refused, r#"{
    let m: Map<[bytes, Array<i64>], i64> = {bytes:AQI= => 1, [1, 2] => 2};
    select json::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

run!(decimal_excess_precision_rounds, r#"{
    let text = "12345678901234567890.123456789123";
    let expected = decimal:12345678901234567890.123456789;
    let a: Result<decimal, [`JsonErr(string), `InvalidCast(string)]> = json::read(text);
    let b: Result<decimal, `ParseError(string)> = str::parse(text);
    let c = cast<decimal>(text);
    [a$ == expected, b$ == expected, c$ == expected]
}"#, prints("[true, true, true]"); FuseExpect::Jit);

// A decimal is written as an exact JSON number, so beside a string it
// reads back as itself (small-pkgs-08).
run!(json_decimal_written_as_an_exact_number, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let d = decimal:12345678901234567890.123456789;
    let u: [decimal, string] = decimal:1.10;
    let back: Result<[decimal, string], E> = json::read(json::write_str(u)$);
    let exact: Result<decimal, E> = json::read(json::write_str(d)$);
    (json::write_str(d)$, json::write_str(u)$, back$ == u, exact$ == d)
}"#, prints(r#"["12345678901234567890.123456789", "1.10", true, true]"#); FuseExpect::None);
