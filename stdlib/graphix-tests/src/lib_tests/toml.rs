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

// A bare variant key is written as its tag, which a read casts back.
run!(toml_variant_map_key_reads_back, r#"{
    let m: Map<[`A, `B], i64> = {`A => 1, `B => 2};
    let s = toml::write_str({m})$;
    let r: {m: Map<[`A, `B], i64>} = toml::read(s)?;
    (s, r.m == m)
}"#, prints(r#"["[m]\nA = 1\nB = 2\n", true]"#); FuseExpect::Jit);

// An offset date-time reads as its instant, the TOML 1.1 form without
// seconds and a numeric offset included; a local date-time names no
// instant and reads as its text.
run!(
    toml_offset_datetimes_read_as_instants,
    r#"{
    let r = |s: string| -> [datetime, string] {
        let d: {t: [datetime, string]} = toml::read(s)$;
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
    }
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

// A map keyed by an abstract type that may write a string writes its keys
// as pairs where none is a string: as a table's key Id(3) would read back
// as Id("3").
run!(
    toml_abstract_key_that_may_write_a_string_reads_back,
    r#"{
    type Id = Abstract<[i64, string]>;
    impl Cast for Id {};
    type E = [`TomlErr(string), `InvalidCast(string)];
    type D = {k: Map<Id, string>};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [t({k: {Id(3) => "a"}}), t({k: {Id("x") => "b"}}), t({k: {Id(3) => "a", Id("x") => "b"}})]
}"#,
    prints("[true, true, true]")
);

// A float key's text keeps its point, so a whole one reads back as the
// float beside a datetime, which reads the same digits as seconds.
run!(
    toml_float_key_beside_a_datetime_reads_back,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    type D = {k: Map<[f64, datetime], i64>};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [
        t({k: {3.0 => 1}}),
        t({k: {1e20 => 1}}),
        t({k: {0.5 => 1}}),
        t({k: {18014398509481984.0 => 1, datetime:"1970-01-01T00:00:03Z" => 2}})
    ]
}"#,
    prints("[true, true, true, true]")
);

// A large whole f32 is written as its shortest text (1234567936 as
// 1234568000.0), which an integer member holds exactly and the f32 only
// rounded: a target holding one beside such an integer is refused.
run!(
    toml_target_f32_beside_an_integer_refused,
    r#"{
    let r: Result<{k: [f32, i64]}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(toml::write_str({k: f32:2.0})$);
    r
}"#,
    refused("'b: TomlData refuses { k: [i64, f32] }")
);

// An f32 beside an integer no wider than 16 bits reads back as written.
run!(
    toml_f32_beside_a_narrow_integer_reads_back,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    type D = {k: [f32, u16]};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [t({k: f32:0.1}), t({k: f32:2.0}), t({k: u16:3}), t({k: u16:65535}), t({k: f32:1e10})]
}"#,
    prints("[true, true, true, true, true]")
);

// A tuple's parts are judged together: the f32 tuple's own reading of
// 0.1 rounds, the other reads both parts exactly.
run!(
    toml_target_f32_tuple_beside_an_exact_reader_refused,
    r#"{
    let r: Result<{k: [(f32, u8), (f64, f32)]}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(toml::write_str({k: (f32:0.1, u8:1)})$);
    r
}"#,
    refused("'b: TomlData refuses { k: [(f32, u8), (f64, f32)] }")
);

run!(
    toml_target_string_tuple_beside_a_number_tuple_refused,
    r#"{
    let r: Result<{k: [(string, f32), (i64, f64)]}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(toml::write_str({k: ("1", f32:0.1)})$);
    r
}"#,
    refused("'b: TomlData refuses { k: [(i64, f64), (string, f32)] }")
);

run!(
    toml_target_recursive_members_refused,
    r#"{
    type TA = [i64, `Cons(f32, TA)];
    type TB = [f64, `Cons(f64, TB)];
    let v: [TA, TB] = `Cons(f32:0.1, 3);
    let r: Result<{k: [TA, TB]}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(toml::write_str({k: v})$);
    r
}"#,
    refused("'b: TomlData refuses { k: [TA, TB] }")
);

// A union member of a union is tried where any of its members keeps the
// data's class, as a scalar is.
run!(
    toml_union_typedef_members_read_back,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    type Num = [i64, f64];
    type Txt = [string, bool];
    type D = {k: [Num, string], t: [Txt, i64]};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [t({k: 3, t: 4}), t({k: 3.0, t: "4"}), t({k: "3", t: true}), t({k: "x", t: "true"})]
}"#,
    prints("[true, true, true, true]")
);

// A primitive set reads a value as one member, and a member the value
// read back already is takes it whatever the union's order.
run!(
    toml_primitive_set_beside_a_member_reads_back,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    type Amount = [f64, string];
    type Count = u32;
    type Label = [string, i64];
    type Score = f64;
    type D = {c: [Amount, Count], l: [Label, Score]};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [
        t({c: u32:0, l: 3.0}),
        t({c: u32:4294967295, l: 0.5}),
        t({c: 4294967296.0, l: "x"}),
        t({c: "3", l: "3.0"}),
        t({c: "", l: 3})
    ]
}"#,
    prints("[true, true, true, true, true]")
);

// A union member whose own members write values of two classes is tried
// by the class of each value it writes.
run!(
    toml_union_typedef_members_of_two_classes_read_back,
    r#"{
    type E = [`TomlErr(string), `InvalidCast(string)];
    type Shape = [{w: f64, h: f64}, f64];
    type Ev = [`Click(i64), string];
    type D = {s: [Shape, string], e: [Ev, i64]};
    let t = |v: D| -> bool {
        let r: Result<D, E> = toml::read(toml::write_str(v)$);
        r$ == v
    };
    [
        t({s: {w: 1.0, h: 2.0}, e: `Click(1)}),
        t({s: 1.5, e: "1"}),
        t({s: "1.5", e: 1}),
        t({s: "x", e: "x"})
    ]
}"#,
    prints("[true, true, true, true]");
    FuseExpect::None
);

run!(
    toml_recursive_members_read_back,
    r#"{
    type TA = [`NA, (f32, TA)];
    type TB = [`NB, (f64, TB)];
    type E = [`TomlErr(string), `InvalidCast(string)];
    let rt = |v: [TA, TB]| -> bool {
        let r: Result<{k: [TA, TB]}, E> = toml::read(toml::write_str({k: v})$);
        r$.k == v
    };
    let a: TA = (f32:0.1, (f32:2.5, `NA));
    let b: TB = (0.1, (3.0, `NB));
    [rt(a), rt(b), rt(`NA), rt(`NB)]
}"#,
    prints("[true, true, true, true]")
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

run!(colliding_pair_keys_are_refused, r#"{
    let m: Map<[(i64, i64), List<i64>], i64> = {(1, 2) => 1, [<1, 2>] => 2};
    select toml::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);

run!(colliding_bytes_and_array_keys_are_refused, r#"{
    let m: Map<[bytes, Array<i64>], i64> = {bytes:AQI= => 1, [1, 2] => 2};
    select toml::write_str({keys: m}) {
        error as e => "[e]",
        _ => "written"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.contains("two keys"))
}; FuseExpect::Jit);
