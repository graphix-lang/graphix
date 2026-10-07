#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{
    CachedArgsAsync, CachedVals, ReadFormat, ReadInput, TypedRead, is_struct,
};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::local::LPooled;

fn json_to_value(json: serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::I64(i)
            } else if let Some(u) = n.as_u64() {
                Value::U64(u)
            } else {
                Value::F64(n.as_f64().unwrap_or(f64::NAN))
            }
        }
        serde_json::Value::String(s) => Value::String(ArcStr::from(s.as_str())),
        serde_json::Value::Array(arr) => {
            let mut vals: LPooled<Vec<Value>> =
                arr.into_iter().map(json_to_value).collect();
            Value::Array(ValArray::from_iter_exact(vals.drain(..)))
        }
        serde_json::Value::Object(obj) => {
            let mut pairs: LPooled<Vec<(String, Value)>> =
                obj.into_iter().map(|(k, v)| (k, json_to_value(v))).collect();
            pairs.sort_by(|a, b| a.0.cmp(&b.0));
            let mut vals: LPooled<Vec<Value>> = pairs
                .drain(..)
                .map(|(k, v)| {
                    Value::Array(ValArray::from([
                        Value::String(ArcStr::from(k.as_str())),
                        v,
                    ]))
                })
                .collect();
            Value::Array(ValArray::from_iter_exact(vals.drain(..)))
        }
    }
}

// CR claude for claude: [bug] value_to_json recurses once per nesting level with no depth
// bound and no ensure_sufficient. A native List is one nesting level per element
// (design/list_native.md keeps that shape on the wire), so
// `json::write_str(list::init(1000, |i| i))` aborts the whole process with a stack
// overflow (exit 134) in both engines: from 400 elements in the dev build, from 1000 in
// a quick build. value_to_toml (graphix-package-toml/src/lib.rs:59) aborts the same way
// from 3000 elements, and hbs::render (graphix-package-hbs/src/lib.rs:88) aborts
// through this function from 1000. ensure_sufficient alone does not fix it, because
// serde_json's serializer and the drop of the built serde_json::Value recurse too.
// Bound the depth and return JsonErr/TomlErr/HbsErr past it; json::read stops at 128
// levels and toml::read at 80, so nothing deeper reads back anyway. probe:
// design/review-2026-10-05/repro/x-panics-07.gx (x-panics-07)
pub fn value_to_json(value: &Value) -> Result<serde_json::Value, String> {
    match value {
        Value::Null => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::I8(n) => Ok(serde_json::Value::from(*n)),
        Value::I16(n) => Ok(serde_json::Value::from(*n)),
        Value::I32(n) => Ok(serde_json::Value::from(*n)),
        Value::I64(n) => Ok(serde_json::Value::from(*n)),
        Value::U8(n) => Ok(serde_json::Value::from(*n)),
        Value::U16(n) => Ok(serde_json::Value::from(*n)),
        Value::U32(n) => Ok(serde_json::Value::from(*n)),
        Value::U64(n) => Ok(serde_json::Value::from(*n)),
        Value::V32(n) => Ok(serde_json::Value::from(*n)),
        Value::V64(n) => Ok(serde_json::Value::from(*n)),
        Value::Z32(n) => Ok(serde_json::Value::from(*n)),
        Value::Z64(n) => Ok(serde_json::Value::from(*n)),
        Value::F32(n) => {
            let f = *n as f64;
            if f.is_finite() {
                Ok(serde_json::Value::from(f))
            } else {
                Err(format!("cannot represent {n} as JSON"))
            }
        }
        Value::F64(n) => {
            if n.is_finite() {
                Ok(serde_json::Value::from(*n))
            } else {
                Err(format!("cannot represent {n} as JSON"))
            }
        }
        Value::Decimal(d) => Ok(serde_json::Value::String(d.to_string())),
        Value::String(s) => Ok(serde_json::Value::String(s.to_string())),
        // CR claude for claude: [bug] json::write_str writes bytes as an array of numbers
        // (here) and a datetime as an RFC3339 string (line 96), but json::read's cast
        // (line 180) takes neither back: reading the output into the type it was
        // written from raises InvalidCast (the reader takes a datetime only as epoch
        // seconds). The toml package has the reverse gap: toml::read reads a table into
        // a Map<string, T>, but toml::write_str refuses a Map
        // (graphix-package-toml/src/lib.rs:108) and any null field (lib.rs:61), and a
        // key the document omits does not read into a [T, null] field (struct size
        // mismatch), so a struct with an optional field goes through TOML in neither
        // direction. probe: design/review-2026-10-05/repro/small-pkgs-13.gx
        // (small-pkgs-13)
        Value::Bytes(b) => {
            let mut arr: LPooled<Vec<serde_json::Value>> =
                b.iter().map(|byte| serde_json::Value::from(*byte)).collect();
            Ok(serde_json::Value::Array(arr.drain(..).collect()))
        }
        Value::DateTime(dt) => Ok(serde_json::Value::String(dt.to_rfc3339())),
        Value::Duration(d) => Ok(serde_json::Value::from(d.as_secs_f64())),
        // CR claude for claude: [bug] A List reaches this arm as its private
        // representation (cons cells of two-slot arrays). It is written as nested
        // pairs, `[0,[1,[2,[]]]]`, one nesting level per element; design/list_native.md
        // records this shape. Because of that, json::read refuses this function's own
        // output for any List of 127 or more elements (serde_json stops at 128 levels).
        // value_to_toml (graphix-package-toml/src/lib.rs:88) does the same, and
        // toml::read refuses 80 or more. hbs::render reuses this function, so
        // `{{#each}}` over `[<"a", "b", "c">]` renders `<a><[b, [c, []]]>`. The writers
        // take `value: Any`, so at run time they cannot tell a List from a pair. With
        // the argument's static type they could write a List as a flat array, which the
        // readers' cast already turns back into a List once its shape guess
        // (graphix-types/src/typ/cast.rs:373) stops reading `[x, []]` as a cons cell.
        // probe: design/review-2026-10-05/repro/x-stack-06.gx (x-stack-06)
        Value::Array(arr) => {
            if is_struct(arr) {
                let mut map = serde_json::Map::with_capacity(arr.len());
                for v in arr.iter() {
                    if let Value::Array(pair) = v {
                        if let Value::String(k) = &pair[0] {
                            map.insert(k.to_string(), value_to_json(&pair[1])?);
                        }
                    }
                }
                Ok(serde_json::Value::Object(map))
            } else {
                let mut vals: LPooled<Vec<serde_json::Value>> =
                    arr.iter().map(value_to_json).collect::<Result<_, _>>()?;
                Ok(serde_json::Value::Array(vals.drain(..).collect()))
            }
        }
        Value::Map(m) => {
            let mut map = serde_json::Map::with_capacity(m.len());
            for (k, v) in m.into_iter() {
                // CR claude for claude: [bug] A Map key is written through Value's
                // Display, which is netidx's typed-literal syntax. A string key keeps
                // its quotes and escapes: `{"a" => 1}` writes `{"\"a\"":1}`, and
                // json::read turns that back into a different map. Any other key gets a
                // type prefix: `{1 => "x"}` writes `{"i64:1":"x"}`. hbs::render
                // converts its data through this function, so Map data renders empty
                // (`hello {{name}}` over `{"name" => "Eric"}` gives `hello `), and
                // register_partials (graphix-package-hbs/src/lib.rs:56) registers Map
                // partials under the quoted name, so `{{> hdr}}` fails with `Partial
                // not found hdr`. A string key should be written as its bare text and
                // any other key in its naked form (`to_string_naked`), in both places.
                // probe: design/review-2026-10-05/repro/small-pkgs-03.gx
                // (small-pkgs-03)
                map.insert(format!("{k}"), value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        Value::Error(_) => Err("cannot serialize Error to JSON".into()),
        Value::Abstract(_) => Err("cannot serialize abstract type to JSON".into()),
    }
}

#[derive(Debug)]
struct Json;

impl ReadFormat for Json {
    const NAME: &str = "json_read";
    const TAG: ArcStr = literal!("JsonErr");
    type Args = ReadInput;

    fn prepare_args(cached: &CachedVals) -> Option<ReadInput> {
        ReadInput::of(cached)
    }

    fn parse(input: ReadInput) -> impl Future<Output = Value> + Send {
        async move {
            let json = match &input {
                ReadInput::Str(s) => serde_json::from_str::<serde_json::Value>(s),
                ReadInput::Bytes(b) => serde_json::from_slice::<serde_json::Value>(b),
            };
            match json {
                Ok(json) => json_to_value(json),
                Err(e) => errf!(Self::TAG, "{e}"),
            }
        }
    }
}

type JsonRead = CachedArgsAsync<TypedRead<Json>>;

/// `[pretty, value]` as JSON, or the error to answer with.
fn encode(args: &[Value]) -> Option<Result<LPooled<Vec<u8>>, Value>> {
    let pretty = graphix_package_core::fast_get::<bool>(args, 0)?;
    let json = match value_to_json(args.get(1)?) {
        Ok(j) => j,
        Err(e) => return Some(Err(errf!("JsonErr", "{e}"))),
    };
    let mut buf: LPooled<Vec<u8>> = LPooled::take();
    let res = if pretty {
        serde_json::to_writer_pretty(&mut *buf, &json)
    } else {
        serde_json::to_writer(&mut *buf, &json)
    };
    Some(res.map(|()| buf).map_err(|e| errf!("JsonErr", "{e}")))
}

fn fc_write_str(args: &[Value]) -> Option<Value> {
    // serde_json always produces valid UTF-8
    Some(encode(args)?.map_or_else(
        |e| e,
        |buf| Value::String(ArcStr::from(unsafe { std::str::from_utf8_unchecked(&buf) })),
    ))
}

graphix_package_core::fast_builtin!(
    JsonWriteStr,
    JsonWriteStrEv,
    "json_write_str",
    fc_write_str
);

fn fc_write_bytes(args: &[Value]) -> Option<Value> {
    Some(encode(args)?.map_or_else(
        |e| e,
        |buf| Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf))),
    ))
}

graphix_package_core::fast_builtin!(
    JsonWriteBytes,
    JsonWriteBytesEv,
    "json_write_bytes",
    fc_write_bytes
);

graphix_derive::defpackage! {
    builtins => [
        JsonRead,
        JsonWriteStr,
        JsonWriteBytes,
    ],
}
