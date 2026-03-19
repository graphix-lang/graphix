#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{errf, ExecCtx, Rt, UserEvent};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use graphix_package_sys::{StreamKind, get_stream};
use netidx_value::{PBytes, ValArray, Value};
use std::sync::Arc;
use tokio::{io::AsyncReadExt, io::AsyncWriteExt, sync::Mutex};

// ── JSON ↔ Value conversion ──────────────────────────────────────

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
            let vals: Vec<Value> = arr.into_iter().map(json_to_value).collect();
            Value::Array(ValArray::from(vals))
        }
        serde_json::Value::Object(obj) => {
            let mut pairs: Vec<(String, Value)> = obj
                .into_iter()
                .map(|(k, v)| (k, json_to_value(v)))
                .collect();
            pairs.sort_by(|a, b| a.0.cmp(&b.0));
            let struct_vals: Vec<Value> = pairs
                .into_iter()
                .map(|(k, v)| {
                    Value::Array(ValArray::from([Value::String(ArcStr::from(k.as_str())), v]))
                })
                .collect();
            Value::Array(ValArray::from(struct_vals))
        }
    }
}

/// Check if a Value is a struct-shaped array: non-empty, every element is
/// a 2-element array with a string first element, keys sorted ascending.
fn is_struct(arr: &ValArray) -> bool {
    if arr.is_empty() {
        return false;
    }
    let mut prev: Option<&ArcStr> = None;
    for v in arr.iter() {
        match v {
            Value::Array(pair) if pair.len() == 2 => match &pair[0] {
                Value::String(k) => {
                    if let Some(p) = prev {
                        if k <= p {
                            return false;
                        }
                    }
                    prev = Some(k);
                }
                _ => return false,
            },
            _ => return false,
        }
    }
    true
}

fn value_to_json(value: &Value) -> Result<serde_json::Value, String> {
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
            let arr: Vec<serde_json::Value> =
                b.iter().map(|byte| serde_json::Value::from(*byte)).collect();
            Ok(serde_json::Value::Array(arr))
        }
        Value::DateTime(dt) => Ok(serde_json::Value::String(dt.to_rfc3339())),
        Value::Duration(d) => Ok(serde_json::Value::from(d.as_secs_f64())),
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
                let vals: Result<Vec<serde_json::Value>, String> =
                    arr.iter().map(value_to_json).collect();
                Ok(serde_json::Value::Array(vals?))
            }
        }
        Value::Map(m) => {
            let mut map = serde_json::Map::with_capacity(m.len());
            for (k, v) in m.into_iter() {
                map.insert(format!("{k}"), value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        Value::Error(_) => Err("cannot serialize Error to JSON".into()),
        Value::Abstract(_) => Err("cannot serialize abstract type to JSON".into()),
    }
}

// ── JsonRead (async — handles string, bytes, and stream) ────────

#[derive(Debug)]
enum ReadInput {
    Data(Bytes),
    Stream(Arc<Mutex<Option<StreamKind>>>),
}

#[derive(Debug, Default)]
struct JsonReadEv;

impl EvalCachedAsync for JsonReadEv {
    const NAME: &str = "json_read";
    type Args = ReadInput;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        match v {
            Value::String(s) => Some(ReadInput::Data(Bytes::from(s.as_bytes().to_vec()))),
            Value::Bytes(b) => Some(ReadInput::Data(Bytes::copy_from_slice(b))),
            Value::Abstract(_) => Some(ReadInput::Stream(get_stream(cached, 0)?)),
            _ => None,
        }
    }

    fn eval(input: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let data = match input {
                ReadInput::Data(b) => b,
                ReadInput::Stream(stream) => {
                    let mut guard = stream.lock().await;
                    let s = match guard.as_mut() {
                        Some(s) => s,
                        None => return errf!("IOErr", "stream unavailable"),
                    };
                    let mut buf = Vec::new();
                    if let Err(e) = s.read_to_end(&mut buf).await {
                        return errf!("IOErr", "read failed: {e}");
                    }
                    Bytes::from(buf)
                }
            };
            match serde_json::from_slice::<serde_json::Value>(&data) {
                Ok(json) => json_to_value(json),
                Err(e) => errf!("JsonErr", "{e}"),
            }
        }
    }
}

type JsonRead = CachedArgsAsync<JsonReadEv>;

// ── JsonWriteStr (sync) ──────────────────────────────────────────

#[derive(Debug, Default)]
struct JsonWriteStrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for JsonWriteStrEv {
    const NAME: &str = "json_write_str";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let json = match value_to_json(v) {
            Ok(j) => j,
            Err(e) => return Some(errf!("JsonErr", "{e}")),
        };
        let s = if pretty {
            serde_json::to_string_pretty(&json)
        } else {
            serde_json::to_string(&json)
        };
        Some(match s {
            Ok(s) => Value::String(ArcStr::from(s.as_str())),
            Err(e) => errf!("JsonErr", "{e}"),
        })
    }
}

type JsonWriteStr = CachedArgs<JsonWriteStrEv>;

// ── JsonWriteBytes (sync) ────────────────────────────────────────

#[derive(Debug, Default)]
struct JsonWriteBytesEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for JsonWriteBytesEv {
    const NAME: &str = "json_write_bytes";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let json = match value_to_json(v) {
            Ok(j) => j,
            Err(e) => return Some(errf!("JsonErr", "{e}")),
        };
        let b = if pretty {
            serde_json::to_vec_pretty(&json)
        } else {
            serde_json::to_vec(&json)
        };
        Some(match b {
            Ok(b) => Value::Bytes(PBytes::new(Bytes::from(b))),
            Err(e) => errf!("JsonErr", "{e}"),
        })
    }
}

type JsonWriteBytes = CachedArgs<JsonWriteBytesEv>;

// ── JsonWriteStream (async) ──────────────────────────────────────

#[derive(Debug, Default)]
struct JsonWriteStreamEv;

impl EvalCachedAsync for JsonWriteStreamEv {
    const NAME: &str = "json_write_stream";
    type Args = (bool, Arc<Mutex<Option<StreamKind>>>, serde_json::Value);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let pretty = cached.get::<bool>(0)?;
        let stream = get_stream(cached, 1)?;
        let v = cached.0.get(2)?.as_ref()?;
        let json = value_to_json(v).ok()?;
        Some((pretty, stream, json))
    }

    fn eval((pretty, stream, json): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let buf = if pretty {
                serde_json::to_vec_pretty(&json)
            } else {
                serde_json::to_vec(&json)
            };
            let buf = match buf {
                Ok(b) => b,
                Err(e) => return errf!("JsonErr", "{e}"),
            };
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOErr", "stream unavailable"),
            };
            match s.write_all(&buf).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOErr", "write failed: {e}"),
            }
        }
    }
}

type JsonWriteStream = CachedArgsAsync<JsonWriteStreamEv>;

// ── Package registration ─────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        JsonRead,
        JsonWriteStr,
        JsonWriteBytes,
        JsonWriteStream,
    ],
}
