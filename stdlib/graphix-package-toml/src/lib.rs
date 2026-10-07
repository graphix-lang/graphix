#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use chrono::Utc;
use graphix_compiler::errf;
use graphix_package_core::{
    CachedArgsAsync, CachedVals, ReadFormat, ReadInput, TypedRead, is_struct,
};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::local::LPooled;
use triomphe::Arc as TArc;

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

#[derive(Debug)]
struct Toml;

impl ReadFormat for Toml {
    const NAME: &str = "toml_read";
    const TAG: ArcStr = literal!("TomlErr");
    type Args = ReadInput;

    fn prepare_args(cached: &CachedVals) -> Option<ReadInput> {
        ReadInput::of(cached)
    }

    fn parse(input: ReadInput) -> impl Future<Output = Value> + Send {
        async move {
            let text = match &input {
                ReadInput::Str(s) => Ok(&**s),
                ReadInput::Bytes(b) => std::str::from_utf8(b),
            };
            match text {
                Err(e) => errf!(Self::TAG, "invalid UTF-8: {e}"),
                Ok(s) => match toml::from_str::<toml::Value>(s) {
                    Ok(t) => toml_to_value(t),
                    Err(e) => errf!(Self::TAG, "{e}"),
                },
            }
        }
    }
}

type TomlRead = CachedArgsAsync<TypedRead<Toml>>;

/// `[pretty, value]` as TOML, or the error to answer with.
fn encode(args: &[Value]) -> Option<Result<String, Value>> {
    let pretty = graphix_package_core::fast_get::<bool>(args, 0)?;
    let toml_val = match value_to_toml(args.get(1)?) {
        Ok(t) => t,
        Err(e) => return Some(Err(errf!("TomlErr", "{e}"))),
    };
    let res = if pretty {
        toml::to_string_pretty(&toml_val)
    } else {
        toml::to_string(&toml_val)
    };
    Some(res.map_err(|e| errf!("TomlErr", "{e}")))
}

fn fc_write_str(args: &[Value]) -> Option<Value> {
    Some(encode(args)?.map_or_else(|e| e, |s| Value::String(ArcStr::from(s.as_str()))))
}

graphix_package_core::fast_builtin!(
    TomlWriteStr,
    TomlWriteStrEv,
    "toml_write_str",
    fc_write_str
);

fn fc_write_bytes(args: &[Value]) -> Option<Value> {
    Some(
        encode(args)?.map_or_else(
            |e| e,
            |s| Value::Bytes(PBytes::new(Bytes::from(s.into_bytes()))),
        ),
    )
}

graphix_package_core::fast_builtin!(
    TomlWriteBytes,
    TomlWriteBytesEv,
    "toml_write_bytes",
    fc_write_bytes
);

graphix_derive::defpackage! {
    builtins => [
        TomlRead,
        TomlWriteStr,
        TomlWriteBytes,
    ],
}
