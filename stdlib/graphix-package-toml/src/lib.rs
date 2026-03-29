#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::ArcStr;
use bytes::Bytes;
use chrono::Utc;
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
use triomphe::Arc as TArc;

// ── TOML ↔ Value conversion ──────────────────────────────────────

fn toml_to_value(v: toml::Value) -> Value {
    match v {
        toml::Value::String(s) => Value::String(ArcStr::from(s.as_str())),
        toml::Value::Integer(i) => Value::I64(i),
        toml::Value::Float(f) => Value::F64(f),
        toml::Value::Boolean(b) => Value::Bool(b),
        toml::Value::Datetime(dt) => {
            let s = dt.to_string();
            match chrono::DateTime::parse_from_rfc3339(&s) {
                Ok(parsed) => Value::DateTime(TArc::new(parsed.with_timezone(&Utc))),
                Err(_) => Value::String(ArcStr::from(s.as_str())),
            }
        }
        toml::Value::Array(arr) => {
            let mut vals: LPooled<Vec<Value>> =
                arr.into_iter().map(toml_to_value).collect();
            Value::Array(ValArray::from_iter_exact(vals.drain(..)))
        }
        toml::Value::Table(table) => {
            let mut pairs: LPooled<Vec<(String, Value)>> =
                table.into_iter().map(|(k, v)| (k, toml_to_value(v))).collect();
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

fn value_to_toml(value: &Value) -> Result<toml::Value, String> {
    match value {
        Value::Null => Err("cannot represent null in TOML".into()),
        Value::Bool(b) => Ok(toml::Value::Boolean(*b)),
        Value::I8(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::I16(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::I32(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::I64(n) => Ok(toml::Value::Integer(*n)),
        Value::U8(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::U16(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::U32(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::U64(n) => i64::try_from(*n)
            .map(toml::Value::Integer)
            .map_err(|_| format!("u64 value {n} exceeds TOML i64 range")),
        Value::V32(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::V64(n) => i64::try_from(*n)
            .map(toml::Value::Integer)
            .map_err(|_| format!("v64 value {n} exceeds TOML i64 range")),
        Value::Z32(n) => Ok(toml::Value::Integer(i64::from(*n))),
        Value::Z64(n) => Ok(toml::Value::Integer(*n)),
        Value::F32(n) => Ok(toml::Value::Float(*n as f64)),
        Value::F64(n) => Ok(toml::Value::Float(*n)),
        Value::String(s) => Ok(toml::Value::String(s.to_string())),
        Value::DateTime(dt) => {
            let s = dt.to_rfc3339();
            s.parse()
                .map(toml::Value::Datetime)
                .map_err(|e| format!("cannot convert datetime to TOML: {e}"))
        }
        Value::Array(arr) => {
            if is_struct(arr) {
                let mut table = toml::map::Map::new();
                for v in arr.iter() {
                    if let Value::Array(pair) = v {
                        if let Value::String(k) = &pair[0] {
                            table.insert(k.to_string(), value_to_toml(&pair[1])?);
                        }
                    }
                }
                Ok(toml::Value::Table(table))
            } else {
                let mut vals: LPooled<Vec<toml::Value>> =
                    arr.iter().map(value_to_toml).collect::<Result<_, _>>()?;
                Ok(toml::Value::Array(vals.drain(..).collect()))
            }
        }
        Value::Bytes(_) => Err("cannot represent bytes in TOML".into()),
        Value::Duration(_) => Err("cannot represent duration in TOML".into()),
        Value::Decimal(_) => Err("cannot represent decimal in TOML".into()),
        Value::Map(_) => Err("cannot represent map in TOML".into()),
        Value::Error(_) => Err("cannot serialize Error to TOML".into()),
        Value::Abstract(_) => Err("cannot serialize abstract type to TOML".into()),
    }
}

// ── TomlRead (async — handles string, bytes, and stream) ────────

#[derive(Debug)]
enum ReadInput {
    Str(ArcStr),
    Bytes(Bytes),
    Stream(Arc<Mutex<Option<StreamKind>>>),
}

#[derive(Debug, Default)]
struct TomlReadEv {
    cast_typ: Option<Type>,
}

impl EvalCachedAsync for TomlReadEv {
    const NAME: &str = "toml_read";
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
                    bail!("toml::read requires a concrete return type")
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
        match &self.cast_typ {
            Some(typ) => Some(typ.cast_value(&ctx.env, v)),
            None => Some(errf!("TomlErr", "no concrete return type found")),
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
                ReadInput::Str(s) => match toml::from_str::<toml::Value>(&s) {
                    Ok(t) => toml_to_value(t),
                    Err(e) => errf!("TomlErr", "{e}"),
                },
                ReadInput::Bytes(b) => {
                    let s = match std::str::from_utf8(&b) {
                        Ok(s) => s,
                        Err(e) => return errf!("TomlErr", "invalid UTF-8: {e}"),
                    };
                    match toml::from_str::<toml::Value>(s) {
                        Ok(t) => toml_to_value(t),
                        Err(e) => errf!("TomlErr", "{e}"),
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
                    let text = match std::str::from_utf8(&buf) {
                        Ok(s) => s,
                        Err(e) => return errf!("TomlErr", "invalid UTF-8: {e}"),
                    };
                    match toml::from_str::<toml::Value>(text) {
                        Ok(t) => toml_to_value(t),
                        Err(e) => errf!("TomlErr", "{e}"),
                    }
                }
            }
        }
    }
}

type TomlRead = CachedArgsAsync<TomlReadEv>;

// ── TomlWriteStr (sync) ──────────────────────────────────────────

#[derive(Debug, Default)]
struct TomlWriteStrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TomlWriteStrEv {
    const NAME: &str = "toml_write_str";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let toml_val = match value_to_toml(v) {
            Ok(t) => t,
            Err(e) => return Some(errf!("TomlErr", "{e}")),
        };
        let res = if pretty {
            toml::to_string_pretty(&toml_val)
        } else {
            toml::to_string(&toml_val)
        };
        Some(match res {
            Ok(s) => Value::String(ArcStr::from(s.as_str())),
            Err(e) => errf!("TomlErr", "{e}"),
        })
    }
}

type TomlWriteStr = CachedArgs<TomlWriteStrEv>;

// ── TomlWriteBytes (sync) ────────────────────────────────────────

#[derive(Debug, Default)]
struct TomlWriteBytesEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TomlWriteBytesEv {
    const NAME: &str = "toml_write_bytes";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let pretty = cached.get::<bool>(0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let toml_val = match value_to_toml(v) {
            Ok(t) => t,
            Err(e) => return Some(errf!("TomlErr", "{e}")),
        };
        let res = if pretty {
            toml::to_string_pretty(&toml_val)
        } else {
            toml::to_string(&toml_val)
        };
        Some(match res {
            Ok(s) => Value::Bytes(PBytes::new(Bytes::from(s.into_bytes()))),
            Err(e) => errf!("TomlErr", "{e}"),
        })
    }
}

type TomlWriteBytes = CachedArgs<TomlWriteBytesEv>;

// ── TomlWriteStream (async) ──────────────────────────────────────

#[derive(Debug, Default)]
struct TomlWriteStreamEv;

impl EvalCachedAsync for TomlWriteStreamEv {
    const NAME: &str = "toml_write_stream";
    const NEEDS_CALLSITE: bool = false;
    type Args = (bool, Arc<Mutex<Option<StreamKind>>>, toml::Value);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let pretty = cached.get::<bool>(0)?;
        let stream = get_stream(cached, 1)?;
        let v = cached.0.get(2)?.as_ref()?;
        let toml_val = value_to_toml(v).ok()?;
        Some((pretty, stream, toml_val))
    }

    fn eval(
        (pretty, stream, toml_val): Self::Args,
    ) -> impl Future<Output = Value> + Send {
        async move {
            let s = if pretty {
                toml::to_string_pretty(&toml_val)
            } else {
                toml::to_string(&toml_val)
            };
            let s = match s {
                Ok(s) => s,
                Err(e) => return errf!("TomlErr", "{e}"),
            };
            let mut guard = stream.lock().await;
            let st = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOErr", "stream unavailable"),
            };
            match st.write_all(s.as_bytes()).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOErr", "write failed: {e}"),
            }
        }
    }
}

type TomlWriteStream = CachedArgsAsync<TomlWriteStreamEv>;

// ── Package registration ─────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        TomlRead,
        TomlWriteStr,
        TomlWriteBytes,
        TomlWriteStream,
    ],
}
