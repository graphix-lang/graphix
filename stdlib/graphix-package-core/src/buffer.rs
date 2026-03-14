use ::bytes::{Bytes, BytesMut};
use arcstr::ArcStr;
use graphix_compiler::errf;
use netidx_value::{PBytes, ValArray, Value};

use crate::{deftype, CachedArgs, CachedVals, EvalCached};

#[derive(Debug, Default)]
pub(crate) struct BytesToStringEv;

impl EvalCached for BytesToStringEv {
    const NAME: &str = "core_bytes_to_string";
    deftype!("fn(bytes) -> Result<string, `EncodingError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        match String::from_utf8(b.into()) {
            Ok(s) => Some(Value::String(ArcStr::from(&s))),
            Err(e) => Some(errf!("EncodingError", "invalid UTF-8: {e}")),
        }
    }
}

pub(crate) type BytesToString = CachedArgs<BytesToStringEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesToStringLossyEv;

impl EvalCached for BytesToStringLossyEv {
    const NAME: &str = "core_bytes_to_string_lossy";
    deftype!("fn(bytes) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        let s = String::from_utf8_lossy(&b).into_owned();
        Some(Value::String(ArcStr::from(&s)))
    }
}

pub(crate) type BytesToStringLossy = CachedArgs<BytesToStringLossyEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesFromStringEv;

impl EvalCached for BytesFromStringEv {
    const NAME: &str = "core_bytes_from_string";
    deftype!("fn(string) -> bytes");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let s = from.get::<ArcStr>(0)?;
        Some(Value::Bytes(PBytes::new(Bytes::copy_from_slice(s.as_bytes()))))
    }
}

pub(crate) type BytesFromString = CachedArgs<BytesFromStringEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesConcatEv;

impl EvalCached for BytesConcatEv {
    const NAME: &str = "core_bytes_concat";
    deftype!("fn(@args: [bytes, Array<bytes>]) -> bytes");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut buf = BytesMut::new();
        for v in from.0.iter() {
            match v {
                None => return None,
                Some(Value::Bytes(b)) => buf.extend_from_slice(b),
                Some(Value::Array(a)) => {
                    for elem in a.iter() {
                        match elem {
                            Value::Bytes(b) => buf.extend_from_slice(b),
                            _ => return None,
                        }
                    }
                }
                _ => return None,
            }
        }
        Some(Value::Bytes(PBytes::new(buf.freeze())))
    }
}

pub(crate) type BytesConcat = CachedArgs<BytesConcatEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesToArrayEv;

impl EvalCached for BytesToArrayEv {
    const NAME: &str = "core_bytes_to_array";
    deftype!("fn(bytes) -> Array<u8>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        Some(Value::Array(ValArray::from_iter_exact(b.iter().map(|byte| Value::U8(*byte)))))
    }
}

pub(crate) type BytesToArray = CachedArgs<BytesToArrayEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesFromArrayEv;

impl EvalCached for BytesFromArrayEv {
    const NAME: &str = "core_bytes_from_array";
    deftype!("fn(Array<u8>) -> bytes");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let arr = match from.0.first()?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let mut buf = BytesMut::with_capacity(arr.len());
        for v in arr.iter() {
            match v {
                Value::U8(b) => buf.extend_from_slice(&[*b]),
                _ => return None,
            }
        }
        Some(Value::Bytes(PBytes::new(buf.freeze())))
    }
}

pub(crate) type BytesFromArray = CachedArgs<BytesFromArrayEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesLenEv;

impl EvalCached for BytesLenEv {
    const NAME: &str = "core_bytes_len";
    deftype!("fn(bytes) -> u64");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        Some(Value::U64(b.len() as u64))
    }
}

pub(crate) type BytesLen = CachedArgs<BytesLenEv>;
