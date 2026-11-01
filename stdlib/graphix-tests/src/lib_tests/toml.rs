use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{
    run,
    testing::{FuseExpect, prints, refused},
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

// A map keyed by bare variants is written as its pairs, each key its tag,
// which a read casts back; a map keyed by strings is a table.
run!(toml_variant_map_key_reads_back, r#"{
    let m: Map<[`A, `B], i64> = {`A => 1, `B => 2};
    let s = toml::write_str({m})$;
    let r: {m: Map<[`A, `B], i64>} = toml::read(s)?;
    let n: Map<string, i64> = {"A" => 1};
    (s, r.m == m, toml::write_str({n})$)
}"#, prints(r#"["m = [[\"A\", 1], [\"B\", 2]]\n", true, "[n]\nA = 1\n"]"#); FuseExpect::Jit);

// An offset date-time reads as its instant, the TOML 1.1 form without
// seconds and a numeric offset included; a local date-time names no
// instant and reads as its text.
run!(
    toml_offset_datetimes_read_as_instants,
    r#"{
    let r = |s: string| -> Any {
        let d: {t: Any} = toml::read(s)$;
        d.t
    };
    [
        r("t = 1979-05-27T07:32Z"),
        r("t = 1979-05-27T00:32-07:00"),
        r("t = 1979-05-27 07:32:00.5Z"),
        r("t = 1979-05-27T07:32:00")
    ]
}"#,
    |v: Result<&Value>| {
        use chrono::{TimeZone, Utc};
        let at = Utc.with_ymd_and_hms(1979, 5, 27, 7, 32, 0).unwrap();
        let half = at + chrono::Duration::milliseconds(500);
        match v {
            Ok(Value::Array(a)) => match &a[..] {
                [
                    Value::DateTime(a),
                    Value::DateTime(b),
                    Value::DateTime(c),
                    Value::String(d),
                ] => {
                    **a == at && **b == at && **c == half && &**d == "1979-05-27T07:32:00"
                }
                _ => false,
            },
            _ => false,
        }
    };
    FuseExpect::None
);

run!(
    composite_map_keys_use_pairs,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    let round_trip = 'k: Ordered + Concrete |m: Map<'k, i64>| {
        let r: Result<{m: Map<'k, i64>}, E> = toml::read(toml::write_str({m})$);
        select r { error as _ => false, v => v.m == m }
    };
    [round_trip({(1, 2) => 3}), round_trip({{a: 1} => 2}),
     round_trip({[1, 2] => 3}), round_trip({list::from_array([1, 2]) => 3}),
     round_trip({`A(1) => 2}), round_trip({buffer::from_string("hi") => 1})]
}
"#,
    prints("[true, true, true, true, true, true]")
);

// An f32 beside an integer is a union with no tags, refused as a target.
run!(
    toml_target_f32_beside_an_integer_refused,
    r#"{
    let r: Result<{k: [f32, i64]}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(toml::write_str({k: f32:2.0})$);
    r
}"#,
    refused("'b: TomlData refuses { k: [i64, f32] }: i64 in [i64, f32] has no tag")
);

run!(
    toml_applied_recursive_typedef_reads_back,
    r#"{
    type R<'a> = [`Nil, `Cons('a, R<'a>)];
    type E = [`TomlErr(string), `InvalidCast(string)];
    let v: R<i64> = `Cons(1, `Cons(2, `Nil));
    let r: Result<{k: R<i64>}, E> = toml::read(toml::write_str({k: v})$);
    (r$.k == v, r$.k)
}"#,
    prints("[true, [\"Cons\", i64:1, [\"Cons\", i64:2, \"Nil\"]]]")
);

// Two keys of a map under `Any` that write as one value are refused when
// written: the reader would merge them.
run!(colliding_pair_keys_are_refused, r#"{
    let m: Map<Any, i64> = {(1, 2) => 1, [<1, 2>] => 2};
    select toml::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

run!(colliding_bytes_and_array_keys_are_refused, r#"{
    let m: Map<Any, i64> = {bytes:AQI= => 1, [1, 2] => 2};
    select toml::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

// TOML writes the forms JSON does: an abstract value as a table its
// type's name keys, `Transparent<T>` as T's data bare, a map keyed by an
// abstract type as its pairs, and they read back.
run!(toml_forms_read_back, r#"{
    type Ito = Abstract<i64>;
    impl Cast for Ito {
        let to_value = |x: Ito| x.0;
        let from_value = |v| select cast<i64>(v) { error as e => e, n => Ito(n) }
    };
    type Port = Abstract<u16>;
    impl Cast for Port {};
    type Doc = {
        i: Ito,
        t: Transparent<Port>,
        o: [Transparent<Port>, null],
        u: Array<[Ito, Port, `A, `B(i64)]>,
        k: Map<Ito, string>
    };
    type E = [`TomlErr(string), `InvalidCast(string)];
    let d: Doc = {
        i: Ito(3),
        t: Transparent(Port(u16:81)),
        o: null,
        u: [Ito(4), Port(u16:82), `A, `B(5)],
        k: {Ito(7) => "seven"}
    };
    let s = toml::write_str(d)$;
    let back: Result<Doc, E> = toml::read(s);
    (s, back$ == d)
}"#, prints(r#"["k = [[{ Ito = 7 }, \"seven\"]]\nt = 81\nu = [{ Ito = 4 }, { Port = 82 }, \"A\", [\"B\", 5]]\n\n[i]\nIto = 3\n", true]"#); FuseExpect::Jit);

// TOML has no null: an option that is null is written only as a struct's
// field, which it leaves out; anywhere else, an array's element or an
// abstract value's data, it is refused when written.
run!(toml_null_outside_a_field_refused_when_written, r#"{
    type Z = Abstract<i64>;
    impl Cast for Z { let to_value = |z: Z| -> [i64, null] null };
    let a: Array<[i64, null]> = [1, null];
    let z = Z(0);
    (select toml::write_str({a}) { error as e => "[e]", s => s },
     select toml::write_str({z}) { error as e => "[e]", s => s })
}"#, prints(r#"["error:`TomlErr(\"cannot represent null in TOML\")", "error:`TomlErr(\"cannot represent null in TOML\")"]"#); FuseExpect::Jit);

// Inside the data a from_value reads abstract values are bare in TOML as
// in JSON: two abstract types or one beside a variant are refused there,
// and tags read back.
run!(
    toml_from_value_data_untagged_abstracts_refused,
    &crate::lib_tests::json::with_from_value_data(
        r#"
type E = [`TomlErr(string), `InvalidCast(string)];
type Rep = {p: Array<[Port, Ito]>, s: Array<[S, `A]>};
type Bx = Abstract<Rep>;
impl Cast for Bx {
    let from_value = |v| select cast<Rep>(v) { error as e => e, r => Bx(r) }
};
let r: Result<{f: Bx}, E> = toml::read("""[f.Bx]
p = [1, 2]
s = ["A"]
""");
r
"#
    ),
    refused(
        "'b: TomlData refuses { f: Bx }: Ito in [Ito, Port] has no tag: in the data Bx's from_value reads"
    )
);

run!(toml_from_value_data_tags_read_back, &crate::lib_tests::json::with_from_value_data(r#"
type E = [`TomlErr(string), `InvalidCast(string)];
type Rep = {t: Array<[`A, `B(i64)]>, p: [Port, null]};
type Bx = Abstract<Rep>;
impl Cast for Bx {
    let from_value = |v| select cast<Rep>(v) { error as e => e, r => Bx(r) }
};
let b = {f: Bx({t: [`A, `B(1)], p: Port(3)})};
let rb: Result<{f: Bx}, E> = toml::read(toml::write_str(b)$);
(toml::write_str(b)$, rb$ == b)
"#), prints(r#"["[f.Bx]\np = 3\nt = [\"A\", [\"B\", 1]]\n", true]"#); FuseExpect::Jit);

// TOML writes the data inside an abstract value as JSON does, for its
// reader: bare abstract values where from_value reads it, wrapped ones
// where the representation is read.
run!(toml_abstract_data_holding_abstracts_reads_back, &crate::lib_tests::json::with_nested_abstracts(r#"
type E = [`TomlErr(string), `InvalidCast(string)];
let s = toml::write_str(d)$;
let back: Result<Doc, E> = toml::read(s);
(s, back$ == d)
"#), prints(r#"["tt = 83\n\n[pt.Pt.x]\nPort = 82\n\n[svc.Svc]\nport = 80\n\n[tags]\nTags = [\"a\", \"b\"]\n\n[tsvc]\nport = 81\n", true]"#); FuseExpect::Jit);
