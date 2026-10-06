use arcstr::ArcStr;
use netidx::publisher::Typ;
use netidx_core::pack::Pack;
use netidx_value::{ValArray, Value};
use poolshark::global::{GPooled, Pool};
use std::sync::LazyLock;

static ENCODE_POOL: LazyLock<Pool<Vec<u8>>> = LazyLock::new(|| Pool::new(64, 4096));
pub(crate) static ENCODE_MANY_POOL: LazyLock<Pool<Vec<GPooled<Vec<u8>>>>> =
    LazyLock::new(|| Pool::new(64, 4096));

pub(crate) fn encode_value(v: &Value) -> Option<GPooled<Vec<u8>>> {
    let len = v.encoded_len();
    let mut buf = ENCODE_POOL.take();
    buf.reserve(len);
    // CR claude for eric: [bug] The Pack error is dropped here. Every prepare_args that
    // calls encode_value or encode_key (insert, batch, compare_and_swap,
    // get/contains_key/remove on such a key, and the txn twins) reads the None as "do
    // not fire". `'v: Concrete` admits Rust-backed abstract types whose Pack is
    // impl_no_pack (db::Db, http::Client, sys::process::Proc, ...), so an op on a value
    // holding one answers nothing: no DbErr and no log line, and a seq step waiting on
    // it stalls with its try never taken. Fix: return the PackError and answer DbErr
    // from eval, or refuse such types in db::tree's typecheck1. probe:
    // design/review-2026-10-05/repro/http-sqlite-db1-10.gx (http-sqlite-db1-10)
    v.encode(&mut *buf).ok()?;
    Some(buf)
}

pub(crate) fn decode_value(data: &[u8]) -> Option<Value> {
    Value::decode(&mut &*data).ok()
}

// Order-preserving key encoding: String/Bytes raw, unsigned ints
// fixed-width big-endian, signed ints big-endian with the sign bit
// flipped; everything else Pack-encoded (usable as a key, unordered).

pub(crate) fn encode_key(key_typ: Option<Typ>, v: &Value) -> Option<GPooled<Vec<u8>>> {
    match key_typ {
        Some(Typ::String) => match v {
            Value::String(s) => {
                let mut buf = ENCODE_POOL.take();
                buf.extend_from_slice(s.as_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::Bytes) => match v {
            Value::Bytes(b) => {
                let mut buf = ENCODE_POOL.take();
                buf.extend_from_slice(&**b);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U8) => match v {
            Value::U8(n) => {
                let mut buf = ENCODE_POOL.take();
                buf.push(*n);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I8) => match v {
            Value::I8(n) => {
                let mut buf = ENCODE_POOL.take();
                buf.push((*n as u8) ^ 0x80);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U16) => match v {
            Value::U16(n) => {
                let mut buf = ENCODE_POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I16) => match v {
            Value::I16(n) => {
                let mut buf = ENCODE_POOL.take();
                let raw = (*n as u16) ^ 0x8000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U32 | Typ::V32) => match v {
            Value::U32(n) | Value::V32(n) => {
                let mut buf = ENCODE_POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I32 | Typ::Z32) => match v {
            Value::I32(n) | Value::Z32(n) => {
                let mut buf = ENCODE_POOL.take();
                let raw = (*n as u32) ^ 0x8000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U64 | Typ::V64) => match v {
            Value::U64(n) | Value::V64(n) => {
                let mut buf = ENCODE_POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I64 | Typ::Z64) => match v {
            Value::I64(n) | Value::Z64(n) => {
                let mut buf = ENCODE_POOL.take();
                let raw = (*n as u64) ^ 0x8000_0000_0000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        // CR claude for eric: [bug] Every key type other than string, bytes and the
        // integers falls through to Pack here, and Pack's byte order is not the
        // language's order: negative f64/f32 sort after the positives and in reverse,
        // pre-1970 datetimes sort last, true sorts before false, decimals sort by scale
        // and sign before value, and tuple/struct keys compare strings by length first
        // and put negative ints last. So first, last, pop_min, pop_max, get_lt, get_gt
        // and cursor::range, which mod.gxi documents as minimum, maximum and
        // strictly-less, return the wrong entries on such trees: a Tree<f64, string>
        // holding -3, -1, 0.5, 2 answers first = 0.5 and get_lt(0.0) = null. Pack bytes
        // also split keys the language treats as equal: get(t, -0.0) misses a key
        // inserted as 0.0, though 0.0 == -0.0 and a Map finds it. The fix is an
        // order-preserving encoding per key type (with -0.0 and NaN normalized),
        // versioned in the tree meta so existing trees still decode, or else the
        // interface must say which key types are ordered. probe:
        // design/review-2026-10-05/repro/http-sqlite-db1-03.gx (http-sqlite-db1-03)
        _ => encode_value(v),
    }
}

pub(crate) fn decode_key(key_typ: Option<Typ>, data: &[u8]) -> Option<Value> {
    match key_typ {
        Some(Typ::String) => {
            std::str::from_utf8(data).ok().map(|s| Value::String(ArcStr::from(s)))
        }
        Some(Typ::Bytes) => {
            Some(Value::Bytes(bytes::Bytes::copy_from_slice(data).into()))
        }
        Some(Typ::U8) if data.len() == 1 => Some(Value::U8(data[0])),
        Some(Typ::I8) if data.len() == 1 => Some(Value::I8((data[0] ^ 0x80) as i8)),
        Some(Typ::U16) if data.len() == 2 => {
            Some(Value::U16(u16::from_be_bytes([data[0], data[1]])))
        }
        Some(Typ::I16) if data.len() == 2 => {
            let raw = u16::from_be_bytes([data[0], data[1]]);
            Some(Value::I16((raw ^ 0x8000) as i16))
        }
        Some(Typ::U32 | Typ::V32) if data.len() == 4 => {
            let n = u32::from_be_bytes(data[..4].try_into().ok()?);
            Some(if key_typ == Some(Typ::V32) { Value::V32(n) } else { Value::U32(n) })
        }
        Some(Typ::I32 | Typ::Z32) if data.len() == 4 => {
            let raw = u32::from_be_bytes(data[..4].try_into().ok()?);
            let n = (raw ^ 0x8000_0000) as i32;
            Some(if key_typ == Some(Typ::Z32) { Value::Z32(n) } else { Value::I32(n) })
        }
        Some(Typ::U64 | Typ::V64) if data.len() == 8 => {
            let n = u64::from_be_bytes(data[..8].try_into().ok()?);
            Some(if key_typ == Some(Typ::V64) { Value::V64(n) } else { Value::U64(n) })
        }
        Some(Typ::I64 | Typ::Z64) if data.len() == 8 => {
            let raw = u64::from_be_bytes(data[..8].try_into().ok()?);
            let n = (raw ^ 0x8000_0000_0000_0000) as i64;
            Some(if key_typ == Some(Typ::Z64) { Value::Z64(n) } else { Value::I64(n) })
        }
        _ => decode_value(data),
    }
}

pub(crate) fn parse_batch_ops(
    key_typ: Option<Typ>,
    arr: &ValArray,
) -> Option<sled::Batch> {
    let mut batch = sled::Batch::default();
    for op in arr.iter() {
        match op {
            Value::Array(a) => match a.first() {
                Some(Value::String(tag)) if &**tag == "Insert" && a.len() == 3 => {
                    let key = encode_key(key_typ, &a[1])?;
                    let val = encode_value(&a[2])?;
                    batch.insert(key.as_slice(), val.as_slice());
                }
                Some(Value::String(tag)) if &**tag == "Remove" && a.len() == 2 => {
                    let key = encode_key(key_typ, &a[1])?;
                    batch.remove(key.as_slice());
                }
                _ => return None,
            },
            _ => return None,
        }
    }
    Some(batch)
}
