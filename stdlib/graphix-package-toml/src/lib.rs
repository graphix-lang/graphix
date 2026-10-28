#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::Result;
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveTime, TimeZone, Utc};
use graphix_compiler::{
    env::Env,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, Keys, ReadFormat, ReadInput, Shape, TypedRead,
    f32_as_written, shape,
};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::local::LPooled;
use serde::Serialize;
use triomphe::Arc as TArc;

fn toml_to_value(v: toml::Value) -> Value {
    match v {
        toml::Value::String(s) => Value::String(ArcStr::from(s.as_str())),
        toml::Value::Integer(i) => Value::I64(i),
        toml::Value::Float(f) => Value::F64(f),
        toml::Value::Boolean(b) => Value::Bool(b),
        toml::Value::Datetime(dt) => match offset_datetime(&dt) {
            Some(dt) => Value::DateTime(TArc::new(dt)),
            None => Value::String(ArcStr::from(dt.to_string().as_str())),
        },
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

/// An offset date-time as a UTC instant; `None` for a local date,
/// time or date-time, which names no instant.
fn offset_datetime(dt: &toml::value::Datetime) -> Option<DateTime<Utc>> {
    let (date, time, offset) = (dt.date?, dt.time?, dt.offset?);
    let date =
        NaiveDate::from_ymd_opt(date.year.into(), date.month.into(), date.day.into())?;
    let (second, leap) = match time.second.unwrap_or(0) {
        60 => (59, 1_000_000_000),
        s => (s, 0),
    };
    let time = NaiveTime::from_hms_nano_opt(
        time.hour.into(),
        time.minute.into(),
        second.into(),
        time.nanosecond.unwrap_or(0) + leap,
    )?;
    let minutes = match offset {
        toml::value::Offset::Z => 0,
        toml::value::Offset::Custom { minutes } => i32::from(minutes),
    };
    let local = FixedOffset::east_opt(minutes * 60)?
        .from_local_datetime(&date.and_time(time))
        .single()?;
    Some(local.with_timezone(&Utc))
}

/// The deepest nesting a value writes as TOML: as deep as toml::read
/// reads.
const MAX_DEPTH: usize = 80;

/// `value` as TOML, written as its type `typ` says (`None` reads the
/// value alone).
fn value_to_toml(
    env: &Env,
    typ: Option<&Type>,
    value: &Value,
) -> Result<toml::Value, String> {
    to_toml(env, typ, value, 0)
}

fn to_toml(
    env: &Env,
    typ: Option<&Type>,
    value: &Value,
    depth: usize,
) -> Result<toml::Value, String> {
    let deeper = |depth: usize| match depth <= MAX_DEPTH {
        true => Ok(depth + 1),
        false => Err(format!("the value nests deeper than {MAX_DEPTH} levels")),
    };
    match shape(env, typ, value).map_err(|e| format!("{e}"))? {
        Shape::Object(mut entries, keys) => {
            let depth = deeper(depth)?;
            let mut table = toml::map::Map::new();
            // TOML has no null: a null field is a key left out, which a
            // cast reads back as null; a map's null value is refused
            let field_null = |v: &Value| keys == Keys::Fields && *v == Value::Null;
            for (k, v, t) in entries.drain(..).filter(|(_, v, _)| !field_null(v)) {
                let v = to_toml(env, t.as_ref(), &v, depth)?;
                // two keys may print alike (`1` and `"1"`): never one for two
                if table.insert(k.to_string(), v).is_some() {
                    return Err(format!("two keys write as the same TOML key {k}"));
                }
            }
            Ok(toml::Value::Table(table))
        }
        Shape::Seq(mut elems) => {
            let depth = deeper(depth)?;
            let mut vals: LPooled<Vec<toml::Value>> = elems
                .drain(..)
                .map(|(v, t)| to_toml(env, t.as_ref(), &v, depth))
                .collect::<Result<_, _>>()?;
            Ok(toml::Value::Array(vals.drain(..).collect()))
        }
        Shape::Pairs(mut pairs) => {
            let depth = deeper(deeper(depth)?)?;
            let mut seen: LPooled<AHashSet<LPooled<String>>> = LPooled::take();
            let mut vals: LPooled<Vec<toml::Value>> = LPooled::take();
            for (k, kt, v, vt) in pairs.drain(..) {
                let k = to_toml(env, kt.as_ref(), &k, depth)?;
                let mut written: LPooled<String> = LPooled::take();
                k.serialize(toml::ser::ValueSerializer::new(&mut *written))
                    .map_err(|e| e.to_string())?;
                if !seen.insert(written) {
                    return Err("two keys write as the same TOML data".into());
                }
                vals.push(toml::Value::Array(vec![
                    k,
                    to_toml(env, vt.as_ref(), &v, depth)?,
                ]));
            }
            Ok(toml::Value::Array(vals.drain(..).collect()))
        }
        Shape::Leaf(v @ Value::Bytes(_)) => {
            deeper(depth)?;
            leaf_to_toml(&v)
        }
        Shape::Leaf(v) => leaf_to_toml(&v),
    }
}

fn leaf_to_toml(value: &Value) -> Result<toml::Value, String> {
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
        Value::F32(n) => Ok(toml::Value::Float(f32_as_written(*n))),
        Value::F64(n) => Ok(toml::Value::Float(*n)),
        Value::String(s) => Ok(toml::Value::String(s.to_string())),
        Value::DateTime(dt) => {
            let s = dt.to_rfc3339();
            s.parse()
                .map(toml::Value::Datetime)
                .map_err(|e| format!("cannot convert datetime to TOML: {e}"))
        }
        Value::Bytes(b) => Ok(toml::Value::Array(
            b.iter().map(|b| toml::Value::Integer(i64::from(*b))).collect(),
        )),
        Value::Duration(_) => Err("cannot represent duration in TOML".into()),
        Value::Decimal(_) => Err("cannot represent decimal in TOML".into()),
        Value::Array(_) | Value::Map(_) => {
            unreachable!("shape reads every array and map")
        }
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

    fn prepare_args(cached: &CachedVals, _exact: bool) -> Option<ReadInput> {
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

/// `[pretty, value]` as TOML, `value` written as its type at the call
/// site says, or the error to answer with.
fn encode(env: &Env, ft: &FnType, args: &[Value]) -> Option<Result<String, Value>> {
    let pretty = graphix_package_core::fast_get::<bool>(args, 0)?;
    let toml_val = match value_to_toml(env, ft.args.get(1).map(|a| &a.typ), args.get(1)?)
    {
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

fn fc_write_str(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    Some(
        encode(env, ft, args)?
            .map_or_else(|e| e, |s| Value::String(ArcStr::from(s.as_str()))),
    )
}

graphix_package_core::typed_fast_builtin!(
    TomlWriteStr,
    TomlWriteStrEv,
    "toml_write_str",
    fc_write_str; cached
);

fn fc_write_bytes(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    Some(
        encode(env, ft, args)?.map_or_else(
            |e| e,
            |s| Value::Bytes(PBytes::new(Bytes::from(s.into_bytes()))),
        ),
    )
}

graphix_package_core::typed_fast_builtin!(
    TomlWriteBytes,
    TomlWriteBytesEv,
    "toml_write_bytes",
    fc_write_bytes; cached
);

graphix_derive::defpackage! {
    builtins => [
        TomlRead,
        TomlWriteStr,
        TomlWriteBytes,
    ],
}
