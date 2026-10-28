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
            && a[1] == Value::String(literal!("ok 1"))
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

// Two map keys that print alike are refused, never merged into one.
run!(json_keys_writing_alike_refused, r#"{
    let m: Map<[i64, string], i64> = {1 => 10, "1" => 20};
    "[json::write_str(m)] [toml::write_str(m)]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s))
        if s.contains("same JSON key 1") && s.contains("same TOML key 1"))
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
}; FuseExpect::Jit);

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
}"#, prints(r#"["{\"A\":1,\"B\":2}", true, "{\"abc\":3}", true, error:["JsonErr", "two keys write as the same JSON key A"]]"#); FuseExpect::Jit);

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
