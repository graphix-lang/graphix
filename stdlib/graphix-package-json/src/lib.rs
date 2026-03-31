#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    errf, typ::FnType, typ::Type, ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent,
};
use graphix_package_core::{
    extract_cast_type, is_struct, CachedArgs, CachedArgsAsync, CachedVals, EvalCached,
    EvalCachedAsync,
};
use graphix_package_sys::{get_stream, StreamKind};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::local::LPooled;
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
        Value::Bytes(b) => {
            let mut arr: LPooled<Vec<serde_json::Value>> =
                b.iter().map(|byte| serde_json::Value::from(*byte)).collect();
            Ok(serde_json::Value::Array(arr.drain(..).collect()))
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
                let mut vals: LPooled<Vec<serde_json::Value>> =
                    arr.iter().map(value_to_json).collect::<Result<_, _>>()?;
                Ok(serde_json::Value::Array(vals.drain(..).collect()))
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
    Str(ArcStr),
    Bytes(Bytes),
    Stream(Arc<Mutex<Option<StreamKind>>>),
}

#[derive(Debug, Default)]
struct JsonReadEv {
    cast_typ: Option<Type>,
}

impl EvalCachedAsync for JsonReadEv {
    const NAME: &str = "json_read";
    const NEEDS_CALLSITE: bool = true;
    type Args = ReadInput;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: graphix_compiler::expr::ExprId,
    ) -> Self {
        Self { cast_typ: extract_cast_type(resolved) }
    }

    fn typecheck<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("json read requires a concrete return type")
                }
                Ok(())
            }
        }
    }

    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        v: Value,
    ) -> Option<Value> {
        match self.cast_typ.as_ref() {
            Some(typ) => Some(typ.cast_value(&ctx.env, v)),
            None => Some(errf!("JsonErr", "no concrete return type found")),
        }
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        match v {
            Value::String(s) => Some(ReadInput::Str(s.clone())),
            Value::Bytes(b) => Some(ReadInput::Bytes((**b).clone())),
            Value::Abstract(_) => Some(ReadInput::Stream(get_stream(cached, 0)?)),
            _ => None,
        }
    }

    fn eval(input: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match input {
                ReadInput::Str(s) => {
                    match serde_json::from_str::<serde_json::Value>(&s) {
                        Ok(json) => json_to_value(json),
                        Err(e) => errf!("JsonErr", "{e}"),
                    }
                }
                ReadInput::Bytes(b) => {
                    match serde_json::from_slice::<serde_json::Value>(&b) {
                        Ok(json) => json_to_value(json),
                        Err(e) => errf!("JsonErr", "{e}"),
                    }
                }
                ReadInput::Stream(stream) => {
                    let mut guard = stream.lock().await;
                    let s = match guard.as_mut() {
                        Some(s) => s,
                        None => return errf!("IOErr", "stream unavailable"),
                    };
                    let mut buf: LPooled<Vec<u8>> = LPooled::take();
                    if let Err(e) = s.read_to_end(&mut buf).await {
                        return errf!("IOErr", "read failed: {e}");
                    }
                    match serde_json::from_slice::<serde_json::Value>(&buf) {
                        Ok(json) => json_to_value(json),
                        Err(e) => errf!("JsonErr", "{e}"),
                    }
                }
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
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let json = match value_to_json(v) {
            Ok(j) => j,
            Err(e) => return Some(errf!("JsonErr", "{e}")),
        };
        let mut buf: LPooled<Vec<u8>> = LPooled::take();
        let res = if pretty {
            serde_json::to_writer_pretty(&mut *buf, &json)
        } else {
            serde_json::to_writer(&mut *buf, &json)
        };
        Some(match res {
            Ok(()) => {
                // serde_json always produces valid UTF-8
                let s = unsafe { std::str::from_utf8_unchecked(&buf) };
                Value::String(ArcStr::from(s))
            }
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
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let json = match value_to_json(v) {
            Ok(j) => j,
            Err(e) => return Some(errf!("JsonErr", "{e}")),
        };
        let mut buf: LPooled<Vec<u8>> = LPooled::take();
        let res = if pretty {
            serde_json::to_writer_pretty(&mut *buf, &json)
        } else {
            serde_json::to_writer(&mut *buf, &json)
        };
        Some(match res {
            Ok(()) => Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf))),
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
    const NEEDS_CALLSITE: bool = false;
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
