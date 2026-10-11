#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use graphix_compiler::{
    env::Env,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, ReadFormat, ReadInput, Shape, TypedRead, shape,
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

/// The deepest nesting a value writes as JSON: as deep as json::read reads,
/// and short of the serializer's recursion running out of stack.
const MAX_DEPTH: usize = 128;

/// `value` as JSON, written as its type `typ` says (`None` reads the
/// value alone).
pub fn value_to_json(
    env: &Env,
    typ: Option<&Type>,
    value: &Value,
) -> Result<serde_json::Value, String> {
    to_json(env, typ, value, 0)
}

fn to_json(
    env: &Env,
    typ: Option<&Type>,
    value: &Value,
    depth: usize,
) -> Result<serde_json::Value, String> {
    if depth > MAX_DEPTH {
        return Err(format!("the value nests deeper than {MAX_DEPTH} levels"));
    }
    match shape(env, typ, value) {
        Shape::Object(mut entries) => {
            let mut map = serde_json::Map::with_capacity(entries.len());
            for (k, v, t) in entries.drain(..) {
                map.insert(k.to_string(), to_json(env, t.as_ref(), &v, depth + 1)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        Shape::Seq(mut elems) => {
            let mut vals: LPooled<Vec<serde_json::Value>> = elems
                .drain(..)
                .map(|(v, t)| to_json(env, t.as_ref(), &v, depth + 1))
                .collect::<Result<_, _>>()?;
            Ok(serde_json::Value::Array(vals.drain(..).collect()))
        }
        Shape::Leaf => leaf_to_json(value),
    }
}

fn leaf_to_json(value: &Value) -> Result<serde_json::Value, String> {
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
        Value::Bytes(b) => {
            let mut arr: LPooled<Vec<serde_json::Value>> =
                b.iter().map(|byte| serde_json::Value::from(*byte)).collect();
            Ok(serde_json::Value::Array(arr.drain(..).collect()))
        }
        Value::DateTime(dt) => Ok(serde_json::Value::String(dt.to_rfc3339())),
        Value::Duration(d) => Ok(serde_json::Value::from(d.as_secs_f64())),
        Value::Array(_) | Value::Map(_) => {
            unreachable!("shape reads every array and map")
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

/// `[pretty, value]` as JSON, `value` written as its type at the call
/// site says, or the error to answer with.
fn encode(
    env: &Env,
    ft: &FnType,
    args: &[Value],
) -> Option<Result<LPooled<Vec<u8>>, Value>> {
    let pretty = graphix_package_core::fast_get::<bool>(args, 0)?;
    let json = match value_to_json(env, ft.args.get(1).map(|a| &a.typ), args.get(1)?) {
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

fn fc_write_str(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    // serde_json always produces valid UTF-8
    Some(encode(env, ft, args)?.map_or_else(
        |e| e,
        |buf| Value::String(ArcStr::from(unsafe { std::str::from_utf8_unchecked(&buf) })),
    ))
}

graphix_package_core::typed_fast_builtin!(
    JsonWriteStr,
    JsonWriteStrEv,
    "json_write_str",
    fc_write_str
);

fn fc_write_bytes(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    Some(encode(env, ft, args)?.map_or_else(
        |e| e,
        |buf| Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf))),
    ))
}

graphix_package_core::typed_fast_builtin!(
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
