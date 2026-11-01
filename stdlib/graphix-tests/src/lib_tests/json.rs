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

// Two map keys are never merged into one: a key type other than string
// writes pairs (in TOML inside a table), and two keys that write as one
// value (under `Any`) are refused.
run!(json_keys_writing_alike_are_not_merged, r#"{
    let m: Map<Any, i64> = {1 => 10, "1" => 20};
    let n: Map<Any, i64> = {`A => 1, "A" => 2};
    "[json::write_str(m)] [toml::write_str({m})] [json::write_str(n)]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s))
        if s.contains(r#"[[1,10],["1",20]]"#) && s.contains(r#"m = [[1, 10], ["1", 20]]"#)
            && s.contains("two keys write as the same JSON data"))
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
// member is the one the value is (a struct beside null, a tuple of pairs
// beside null, an inline struct under a nullable), and a list's elements
// keep their type (x-kind-markers-01, t-rep-03).
run!(writers_choose_members_by_kind, r#"{
    let f: [{a: i64}, null] = {a: 1};
    let c: [((string, i64), (string, i64)), null] = (("a", 1), ("b", 2));
    let l: List<Array<(string, i64)>> = [<[("a", 1)]>];
    let o: [{a: Array<(string, i64)>, b: i64}, null] = {a: [("k", 1)], b: 7};
    let t = toml::write_str({v: f})$;
    "[json::write_str(f)$] [json::write_str(c)$] [json::write_str(l)$] [json::write_str(o)$] [str::trim(t)]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s
        == "{\"a\":1} [[\"a\",1],[\"b\",2]] [[[\"a\",1]]] {\"a\":[[\"k\",1]],\"b\":7} [v]\na = 1")
}; FuseExpect::Jit);

// A map keyed by datetimes or durations is written as its pairs, each key
// as such a value is, so it reads back into its type (x-rerun-3-01).
run!(time_map_keys_read_back, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let m: Map<datetime, i64> = {datetime:"2020-01-01T00:00:00Z" => 1};
    let d: Map<duration, i64> = {duration:1.5s => 2};
    let m2: Result<Map<datetime, i64>, E> = json::read(json::write_str(m)$);
    let d2: Result<Map<duration, i64>, E> = json::read(json::write_str(d)$);
    (json::write_str(m)$, m2$ == m, json::write_str(d)$, d2$ == d)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == r#"["[[\"2020-01-01T00:00:00+00:00\",1]]", true, "[[1.5,2]]", true]"#)
}; FuseExpect::Jit);

// A map is an object only under the key type `string`, whatever its keys
// write as: one keyed by bare variants or by an abstract string type is
// written as its pairs, and reads back into its type.
run!(string_data_map_keys_read_back, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    type Id = Abstract<string>;
    impl Cast for Id {};
    let m: Map<[`A, `B], i64> = {`A => 1, `B => 2};
    let i: Map<Id, i64> = {Id("abc") => 3};
    let s: Map<string, i64> = {"abc" => 3};
    let m2: Result<Map<[`A, `B], i64>, E> = json::read(json::write_str(m)$);
    let i2: Result<Map<Id, i64>, E> = json::read(json::write_str(i)$);
    let s2: Result<Map<string, i64>, E> = json::read(json::write_str(s)$);
    (json::write_str(m)$, m2$ == m, json::write_str(i)$, i2$ == i, json::write_str(s)$, s2$ == s)
}"#, prints(r#"["[[\"A\",1],[\"B\",2]]", true, "[[{\"Id\":\"abc\"},3]]", true, "{\"abc\":3}", true]"#); FuseExpect::Jit);

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

// A map's form follows its key type alone: an object under `string`, its
// pairs under any other, whatever its keys are, and either reads back.
run!(
    mixed_class_map_keys_use_pairs,
    r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let round_trip = 'k: Ordered + Concrete |m: Map<'k, i64>| {
        let r: Result<Map<'k, i64>, E> = json::read(json::write_str(m)$);
        select r { error as _ => false, v => v == m }
    };
    let a: Map<[`S(string), `I(i64)], i64> = {`I(1) => 1, `S("a") => 2};
    let b: Map<bool, i64> = {true => 1};
    (
        [round_trip(a), round_trip(b), round_trip({1 => 2}), round_trip({"x" => 2})],
        json::write_str(b)$,
        json::write_str({1 => 2})$,
        json::write_str({"x" => 2})$
    )
}
"#,
    prints(r#"[[true, true, true, true], "[[true,1]]", "[[1,2]]", "{\"x\":2}"]"#)
);

// Every union in a type JSON reads or writes tells its members apart by a
// tag (x-rerun-5-04): an integer beside a whole f32 or duration, which
// JSON writes as a float, is refused as a target, a written value and a
// map key.
run!(
    json_target_float_beside_an_integer_refused,
    r#"{
    let r: Result<[duration, i64], [`JsonErr(string), `InvalidCast(string)]> =
        json::read("2.0");
    r
}"#,
    refused(
        "'b: JsonData refuses [i64, duration]: i64 in [i64, duration] has no tag (a \
         variant's, or an abstract type's name), so what another member writes as \
         JSON data may read back as it"
    )
);

run!(
    json_map_key_float_beside_an_integer_refused,
    r#"{
    let m: Map<[duration, i64], i64> = {duration:2.s => 1, 3 => 2};
    json::write_str(m)
}"#,
    refused(
        "'a: JsonData refuses Map<[i64, duration], i64>: i64 in [i64, duration] has no tag"
    )
);

run!(
    json_target_f32_beside_an_integer_refused,
    r#"{
    let v: [f32, i64] = f32:2.0;
    json::write_str(v)
}"#,
    refused("'a: JsonData refuses [i64, f32]: i64 in [i64, f32] has no tag")
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

// An abstract value is written inside an object its type's name keys, so
// it reads back through its from_value beside a variant, whatever that
// from_value reads.
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

// Two keys of a map under `Any` that write as one value are refused when
// written: the reader would merge them.
run!(colliding_pair_keys_are_refused, r#"{
    let m: Map<Any, i64> = {(1, 2) => 1, [<1, 2>] => 2};
    select json::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

run!(colliding_bytes_and_array_keys_are_refused, r#"{
    let m: Map<Any, i64> = {bytes:AQI= => 1, [1, 2] => 2};
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

// A decimal is written as an exact JSON number, so it reads back as
// itself (small-pkgs-08).
run!(json_decimal_written_as_an_exact_number, r#"{
    type E = [`JsonErr(string), `InvalidCast(string)];
    let d = decimal:12345678901234567890.123456789;
    let u: [decimal, null] = decimal:1.10;
    let back: Result<[decimal, null], E> = json::read(json::write_str(u)$);
    let exact: Result<decimal, E> = json::read(json::write_str(d)$);
    (json::write_str(d)$, json::write_str(u)$, back$ == u, exact$ == d)
}"#, prints(r#"["12345678901234567890.123456789", "1.10", true, true]"#); FuseExpect::None);

/// The forms JSON and TOML write: abstract values, `Transparent`, a
/// union of tags, an option, a map keyed by strings and one keyed by an
/// abstract type.
const FORMS: &str = r#"
type Ito = Abstract<i64>;
impl Cast for Ito {
    let to_value = |x: Ito| x.0;
    let from_value = |v| select cast<i64>(v) { error as e => e, n => Ito(n) }
};
type Port = Abstract<u16>;
impl Cast for Port {};
type Doc = {
    i: Ito,
    p: Port,
    t: Transparent<Port>,
    o: [Transparent<Port>, null],
    u: Array<[Ito, Port, `A, `B(i64), null]>,
    m: Map<string, Ito>,
    k: Map<Ito, string>
};
let d: Doc = {
    i: Ito(3),
    p: Port(u16:80),
    t: Transparent(Port(u16:81)),
    o: null,
    u: [Ito(4), Port(u16:82), `A, `B(5), null],
    m: {"a" => Ito(6)},
    k: {Ito(7) => "seven"}
};
"#;

fn with_forms(body: &str) -> String {
    format!("{{{FORMS}{body}}}")
}

// An abstract value is written inside an object its type's name keys, its
// data inside (its to_value, else its representation at its own type),
// and `Transparent<T>` writes T's data bare; a union of tags beside null
// and a map keyed by an abstract type, written as its pairs, read back.
run!(json_forms_read_back, &with_forms(r#"
type E = [`JsonErr(string), `InvalidCast(string)];
let e = {d with o: Transparent(Port(u16:83))};
let j = json::write_str(d)$;
let back: Result<Doc, E> = json::read(j);
let eb: Result<Doc, E> = json::read(json::write_str(e)$);
(j, back$ == d, eb$ == e)
"#), prints(r#"["{\"i\":{\"Ito\":3},\"k\":[[{\"Ito\":7},\"seven\"]],\"m\":{\"a\":{\"Ito\":6}},\"o\":null,\"p\":{\"Port\":80},\"t\":81,\"u\":[{\"Ito\":4},{\"Port\":82},\"A\",[\"B\",5],null]}", true, true]"#); FuseExpect::Jit);

// A read takes an abstract value only from the object its type's name
// keys, and under `Transparent` only its data bare.
run!(json_abstract_reads_only_its_object, &with_forms(r#"
type E = [`JsonErr(string), `InvalidCast(string)];
let bare: Result<Ito, E> = json::read("3");
let other: Result<Ito, E> = json::read("{\"Port\": 3}");
let t: Result<Transparent<Ito>, E> = json::read("3");
let tw: Result<Transparent<Ito>, E> = json::read("{\"Ito\": 3}");
(bare, other, t$ == Transparent(Ito(3)), tw)
"#), prints(r#"[error:["InvalidCast", "can't cast 3 to Ito: expected an object whose one key is Ito"], error:["InvalidCast", "can't cast [[\"Port\", 3]] to Ito: expected an object whose one key is Ito"], true, error:["InvalidCast", "can't cast [[\"Ito\", 3]] to Ito: can't cast [[\"Ito\", 3]] to i64: no primitive conversion"]]"#); FuseExpect::Jit);

// A `T` written bare that writes null reads back from `[Transparent<T>,
// null]` as the option's null.
run!(json_transparent_null_reads_back_as_the_option, r#"{
    type Z = Abstract<i64>;
    impl Cast for Z {
        let to_value = |z: Z| -> [i64, null] select z.0 { 0 => null, n => n };
        let from_value = |v| select cast<[i64, null]>(v) {
            error as e => e,
            null as _ => Z(0),
            n => Z(n)
        }
    };
    type E = [`JsonErr(string), `InvalidCast(string)];
    let a: [Transparent<Z>, null] = Transparent(Z(0));
    let b: [Transparent<Z>, null] = Transparent(Z(5));
    let ra: Result<[Transparent<Z>, null], E> = json::read(json::write_str(a)$);
    let rb: Result<[Transparent<Z>, null], E> = json::read(json::write_str(b)$);
    (json::write_str(a)$, ra$, rb$ == b)
}"#, prints(r#"["null", null, true]"#); FuseExpect::Jit);

// An abstract type whose representation holds itself is a union of tags,
// which the check walks once.
run!(json_abstract_holding_itself_reads_back, r#"{
    type N = Abstract<[N, `L(i64)]>;
    impl Cast for N {};
    type E = [`JsonErr(string), `InvalidCast(string)];
    let v = N(N(`L(1)));
    let r: Result<N, E> = json::read(json::write_str(v)$);
    (json::write_str(v)$, r$ == v)
}"#, prints(r#"["{\"N\":{\"N\":[\"L\",1]}}", true]"#); FuseExpect::Jit);

// A union whose members a tag does not tell apart is refused as a written
// value too: an abstract type beside a number, `Transparent` beside a tag,
// two members under one tag, an open member beside another, and
// `Transparent` over a type that is not abstract.
run!(
    json_written_untagged_union_refused,
    r#"{
    type Ito = Abstract<i64>;
    impl Cast for Ito {};
    let v: [Ito, u8] = u8:3;
    json::write_str(v)
}"#,
    refused("'a: JsonData refuses [u8, Ito]: u8 in [u8, Ito] has no tag")
);

run!(
    json_written_transparent_beside_a_tag_refused,
    r#"{
    type P = Abstract<i64>;
    impl Cast for P {};
    let v: [Transparent<P>, `A] = `A;
    json::write_str(v)
}"#,
    refused(
        "'a: JsonData refuses [Transparent<P>, `A]: Transparent<P> in [Transparent<P>, `A] has no tag"
    )
);

run!(
    json_members_sharing_a_tag_refused,
    r#"{
    type A = Abstract<i64>;
    impl Cast for A {};
    let v: [A, `A] = `A;
    json::write_str(v)
}"#,
    refused("'a: JsonData refuses [A, `A]: A and `A in [A, `A] share the tag A")
);

// A variant written through a name is the variant: two spellings of one
// type under one tag are one member, and it reads back.
run!(
    json_one_variant_through_a_name_reads_back,
    r#"{
    type K = string;
    type AK = [`A(K), `B];
    type E = [`JsonErr(string), `InvalidCast(string)];
    let x: Array<[`A(K), `A(string), `B]> = [`A("x"), `B];
    let y: Array<[AK, `A(string)]> = [`A("y"), `B];
    let (jx, jy) = (json::write_str(x)$, json::write_str(y)$);
    let rx: Result<Array<[`A(K), `A(string), `B]>, E> = json::read(jx);
    let ry: Result<Array<[AK, `A(string)]>, E> = json::read(jy);
    (jx, jy, rx$ == x, ry$ == y)
}"#,
    prints(r#"["[[\"A\",\"x\"],\"B\"]", "[[\"A\",\"y\"],\"B\"]", true, true]"#)
);

run!(
    json_one_tag_over_two_types_through_a_name_refused,
    r#"{
    type K = i64;
    let x: Array<[`A(K), `A(string)]> = [`A(1)];
    json::write_str(x)
}"#,
    refused("share the tag A")
);

run!(
    json_open_member_beside_a_tag_refused,
    r#"{
    let f = |b: bool, x| json::write_str(select b { true => `A, false => x });
    f(true, `B)
}"#,
    refused("JsonData refuses")
);

run!(
    json_transparent_over_a_type_not_abstract_refused,
    r#"{
    let v: Transparent<i64> = Transparent(3);
    json::write_str(v)
}"#,
    refused(
        "'a: JsonData refuses Transparent<i64>: Transparent<i64> holds a type that is not abstract, which JSON writes bare already"
    )
);

/// Abstract values whose data holds abstract values: through `from_value`
/// (the identity impl, and one leaving `to_value` out), through the
/// representation, and `Transparent` around them.
const NESTED_ABSTRACTS: &str = r#"
type Tag = Abstract<string>;
impl Cast for Tag {};
type Tags = Abstract<Array<Tag>>;
impl Cast for Tags {
    let to_value = |t: Tags| t.0;
    let from_value = |v| select cast<Array<Tag>>(v) { error as e => e, a => Tags(a) }
};
type Port = Abstract<u16>;
impl Cast for Port {};
type Svc = Abstract<{port: Port}>;
impl Cast for Svc {
    let from_value = |v| select cast<{port: Port}>(v) { error as e => e, s => Svc(s) }
};
type Pt = Abstract<{x: Port}>;
impl Cast for Pt {
    let to_value = |p: Pt| {x: p.0.x}
};
type Doc = {
    tags: Tags,
    svc: Svc,
    tsvc: Transparent<Svc>,
    pt: Pt,
    tt: Transparent<Transparent<Port>>
};
let d: Doc = {
    tags: Tags([Tag("a"), Tag("b")]),
    svc: Svc({port: Port(u16:80)}),
    tsvc: Transparent(Svc({port: Port(u16:81)})),
    pt: Pt({x: Port(u16:82)}),
    tt: Transparent(Transparent(Port(u16:83)))
};
"#;

pub(crate) fn with_nested_abstracts(body: &str) -> String {
    format!("{{{NESTED_ABSTRACTS}{body}}}")
}

// The data inside an abstract value is written as its reader reads it: a
// written from_value casts it as data, so the abstract values in it are
// bare, as a cast or pack writes them; read as the representation, they
// are wrapped. `Transparent` around a `Transparent` writes what the inner
// one does.
run!(json_abstract_data_holding_abstracts_reads_back, &with_nested_abstracts(r#"
type E = [`JsonErr(string), `InvalidCast(string)];
let j = json::write_str(d)$;
let back: Result<Doc, E> = json::read(j);
(j, back$ == d)
"#), prints(r#"["{\"pt\":{\"Pt\":{\"x\":{\"Port\":82}}},\"svc\":{\"Svc\":{\"port\":80}},\"tags\":{\"Tags\":[\"a\",\"b\"]},\"tsvc\":{\"port\":81},\"tt\":83}", true]"#); FuseExpect::Jit);

/// Abstract types a `from_value` reads inside its data, where they are
/// written bare and carry no tag.
const FROM_VALUE_DATA: &str = r#"
type Port = Abstract<i64>;
impl Cast for Port {};
type Ito = Abstract<i64>;
impl Cast for Ito {};
type S = Abstract<string>;
impl Cast for S {};
type N = Abstract<[i64, null]>;
impl Cast for N {};
"#;

pub(crate) fn with_from_value_data(body: &str) -> String {
    format!("{{{FROM_VALUE_DATA}{body}}}")
}

// Inside the data a from_value reads, abstract values are written bare, so
// two abstract types, or one beside a variant, are told apart by nothing;
// the check refuses them there, directly, through Transparent and through
// an abstract read as its representation.
run!(
    json_from_value_data_two_abstracts_refused,
    &with_from_value_data(
        r#"
type Bx = Abstract<Array<[Port, Ito]>>;
impl Cast for Bx {
    let from_value = |v| select cast<Array<[Port, Ito]>>(v) { error as e => e, r => Bx(r) }
};
json::write_str(Bx([Port(1), Ito(2)]))
"#
    ),
    refused(
        "'a: JsonData refuses Bx: Ito in [Ito, Port] has no tag: in the data Bx's from_value reads, abstract values are written bare"
    )
);

run!(
    json_from_value_data_abstract_beside_a_tag_refused,
    &with_from_value_data(
        r#"
type E = [`JsonErr(string), `InvalidCast(string)];
type Bx = Abstract<{s: Array<[S, `A]>}>;
impl Cast for Bx {
    let from_value = |v| select cast<{s: Array<[S, `A]>}>(v) { error as e => e, r => Bx(r) }
};
let r: Result<Bx, E> = json::read("""{"Bx":{"s":["A"]}}""");
r
"#
    ),
    refused(
        "'b: JsonData refuses Bx: S in [S, `A] has no tag: in the data Bx's from_value reads"
    )
);

run!(
    json_from_value_data_through_transparent_refused,
    &with_from_value_data(
        r#"
type Bx = Abstract<Array<[Port, Ito]>>;
impl Cast for Bx {
    let from_value = |v| select cast<Array<[Port, Ito]>>(v) { error as e => e, r => Bx(r) }
};
let v: Transparent<Bx> = Transparent(Bx([Port(1)]));
json::write_str(v)
"#
    ),
    refused("in the data Bx's from_value reads")
);

run!(
    json_from_value_data_inside_a_representation_refused,
    &with_from_value_data(
        r#"
type Bx = Abstract<Array<[Port, Ito]>>;
impl Cast for Bx {
    let from_value = |v| select cast<Array<[Port, Ito]>>(v) { error as e => e, r => Bx(r) }
};
type Out = Abstract<{b: Bx}>;
impl Cast for Out {};
json::write_str(Out({b: Bx([Port(1)])}))
"#
    ),
    refused("in the data Bx's from_value reads")
);

// A named type the text admits is judged again inside a from_value's data.
run!(
    json_from_value_data_judges_a_named_type_again,
    &with_from_value_data(
        r#"
type R = Array<[Port, Ito]>;
type Bx = Abstract<R>;
impl Cast for Bx {
    let from_value = |v| select cast<R>(v) { error as e => e, r => Bx(r) }
};
let d: {a: R, b: Bx} = {a: [Port(1)], b: Bx([Ito(2)])};
json::write_str(d)
"#
    ),
    refused("in the data Bx's from_value reads")
);

// The same types read back in the text, where each abstract value is
// wrapped, and in a from_value's data tags and one member beside null
// read back; there a member that writes null reads back as the outer
// null, as under Transparent.
run!(json_from_value_data_tags_read_back, &with_from_value_data(r#"
type E = [`JsonErr(string), `InvalidCast(string)];
type Rep = {t: Array<[`A, `B(i64), null]>, p: [Port, null], n: [N, null]};
type Bx = Abstract<Rep>;
impl Cast for Bx {
    let from_value = |v| select cast<Rep>(v) { error as e => e, r => Bx(r) }
};
let a: Array<[Port, Ito, S, `A]> = [Port(1), Ito(2), S("A"), `A];
let ra: Result<Array<[Port, Ito, S, `A]>, E> = json::read(json::write_str(a)$);
let b = Bx({t: [`A, `B(1), null], p: Port(3), n: N(null)});
let rb: Result<Bx, E> = json::read(json::write_str(b)$);
(ra$ == a, json::write_str(b)$, rb$ == Bx({t: [`A, `B(1), null], p: Port(3), n: null}))
"#), prints(r#"[true, "{\"Bx\":{\"n\":null,\"p\":3,\"t\":[\"A\",[\"B\",1],null]}}", true]"#); FuseExpect::Jit);

// A member not known yet beside a tag is refused without the cell's
// generated name; a definition's declared variable is named.
run!(
    json_open_member_refusal_names_no_cell,
    r#"{
    let w = |x| json::write_str([x, `A]);
    w(`B)
}"#,
    |v: Result<&Value>| matches!(v, Err(e) if {
        let m = format!("{e:#}");
        m.contains("JsonData refuses a type not known yet beside `A: beside another member a member needs a tag")
            && !m.contains("'_")
    })
);

run!(
    json_declared_variable_beside_a_tag_refused,
    r#"{
    let w = 'a: JsonData |x: 'a| json::write_str([x, `A]);
    w(`B)
}"#,
    refused(
        "'a may be any type, and beside another member in ['a, `A] it would need a tag"
    )
);

// A refusal at a generic definition's call names the type it refuses, not
// the definition's variable.
run!(
    json_generic_writer_refusal_names_the_type,
    r#"{
    let w = |x| json::write_str(x);
    let v: [i64, string] = 3;
    w(v)
}"#,
    |v: Result<&Value>| matches!(v, Err(e) if {
        let m = format!("{e:#}");
        m.contains("JsonData refuses [i64, string]: i64 in [i64, string] has no tag")
            && !m.contains("'_")
    })
);
