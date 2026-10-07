use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use chrono::DateTime;
use enumflags2::BitFlags;
use netidx::publisher::Typ;
use netidx_core::pack::Pack;
use netidx_value::{ValArray, ValError, Value};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use rust_decimal::Decimal;
use std::{sync::LazyLock, time::Duration};
use triomphe::Arc;

static ENCODE_POOL: LazyLock<Pool<Vec<u8>>> = LazyLock::new(|| Pool::new(64, 4096));
pub(crate) static ENCODE_MANY_POOL: LazyLock<Pool<Vec<GPooled<Vec<u8>>>>> =
    LazyLock::new(|| Pool::new(64, 4096));

/// A key nests at most this deep.
const MAX_DEPTH: usize = 64;

pub(crate) fn encode_value(v: &Value) -> Result<GPooled<Vec<u8>>> {
    let mut buf = ENCODE_POOL.take();
    buf.reserve(v.encoded_len());
    v.encode(&mut *buf).map_err(|e| anyhow!("the value cannot be stored: {e}"))?;
    Ok(buf)
}

pub(crate) fn decode_value(data: &[u8]) -> Result<Value> {
    Value::decode(&mut &*data).map_err(|e| anyhow!("undecodable value: {e}"))
}

// A key's bytes sort as the language orders its values, since sled orders
// keys by their bytes. A tree whose keys are one primitive type stores them
// without a tag (a string or bytes key raw, so a prefix is a byte prefix);
// any other key type stores each value tagged by its type and
// self-delimiting.

// XCR claude for claude: [bug] Every key type other than string, bytes and the
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
// 2026-10-07 claude: every key type now has an order-preserving encoding (above);
// -0.0 is 0.0 and NaN sorts first, as Value orders them. The tree meta carries a
// format version (tree.rs META_VERSION), so a tree written before refuses to open
// instead of misreading. Not matched: a Map key, which Value orders by pairing
// entries from the back when the lengths differ; this encodes entries front to
// back. Pin: encoding::test::key_bytes_sort_as_values, lib_tests db_float_keys_order.
pub(crate) fn encode_key(key_typ: Option<Typ>, v: &Value) -> Result<GPooled<Vec<u8>>> {
    let mut buf = ENCODE_POOL.take();
    match (key_typ, v) {
        (Some(Typ::String), Value::String(s)) => buf.extend_from_slice(s.as_bytes()),
        (Some(Typ::Bytes), Value::Bytes(b)) => buf.extend_from_slice(b),
        (Some(t), v) if Typ::get(v) == t => write_payload(v, &mut buf, 0)?,
        (Some(t), v) => bail!("a key of type {t:?} cannot be {v}"),
        (None, v) => write_tagged(v, &mut buf, 0)?,
    }
    Ok(buf)
}

pub(crate) fn decode_key(key_typ: Option<Typ>, mut data: &[u8]) -> Result<Value> {
    let v = match key_typ {
        Some(Typ::String) => {
            return Ok(Value::String(ArcStr::from(std::str::from_utf8(data)?)));
        }
        Some(Typ::Bytes) => {
            return Ok(Value::Bytes(bytes::Bytes::copy_from_slice(data).into()));
        }
        Some(t) => read_payload(t, &mut data, 0)?,
        None => read_tagged(&mut data, 0)?,
    };
    if !data.is_empty() {
        bail!("undecodable key: {} trailing bytes", data.len())
    }
    Ok(v)
}

fn tag(t: Typ) -> u8 {
    (t as u64).trailing_zeros() as u8
}

fn write_tagged(v: &Value, buf: &mut Vec<u8>, depth: usize) -> Result<()> {
    buf.push(tag(Typ::get(v)));
    write_payload(v, buf, depth)
}

fn read_tagged(data: &mut &[u8], depth: usize) -> Result<Value> {
    let t = BitFlags::<Typ>::from_bits(1u64 << (take::<1>(data)?[0] & 63))
        .ok()
        .and_then(|f| f.exactly_one())
        .ok_or_else(|| anyhow!("undecodable key: unknown type tag"))?;
    read_payload(t, data, depth)
}

fn take<const N: usize>(data: &mut &[u8]) -> Result<[u8; N]> {
    let (head, rest) =
        data.split_first_chunk::<N>().ok_or_else(|| anyhow!("undecodable key: short"))?;
    *data = rest;
    Ok(*head)
}

/// Each integer big-endian, a signed one with its sign bit flipped.
macro_rules! signed {
    ($n:expr, $u:ty) => {
        (($n as $u) ^ (1 << (<$u>::BITS - 1))).to_be_bytes()
    };
}

/// A float's bits made to sort as the language orders floats: NaN first,
/// then the numbers in order, -0.0 equal to 0.0.
macro_rules! float_key {
    ($f:expr, $u:ty) => {{
        let f = if $f == 0.0 { 0.0 } else { $f };
        let bits = f.to_bits();
        let k: $u = if f.is_nan() {
            0
        } else if bits >> (<$u>::BITS - 1) == 1 {
            !bits
        } else {
            bits ^ (1 << (<$u>::BITS - 1))
        };
        k.to_be_bytes()
    }};
}

macro_rules! float_of_key {
    ($k:expr, $u:ty, $f:ty) => {{
        let k = <$u>::from_be_bytes($k);
        if k == 0 {
            <$f>::NAN
        } else if k >> (<$u>::BITS - 1) == 1 {
            <$f>::from_bits(k ^ (1 << (<$u>::BITS - 1)))
        } else {
            <$f>::from_bits(!k)
        }
    }};
}

/// The bytes of a string or bytes inside a tagged key: each 0 escaped as
/// 0 0xff, the end marked 0 0.
fn write_delimited(b: &[u8], buf: &mut Vec<u8>) {
    for &c in b {
        buf.push(c);
        if c == 0 {
            buf.push(0xff);
        }
    }
    buf.extend_from_slice(&[0, 0]);
}

fn read_delimited(data: &mut &[u8]) -> Result<LPooled<Vec<u8>>> {
    let mut out: LPooled<Vec<u8>> = LPooled::take();
    loop {
        match take::<1>(data)?[0] {
            0 => match take::<1>(data)?[0] {
                0 => return Ok(out),
                0xff => out.push(0),
                _ => bail!("undecodable key: bad escape"),
            },
            c => out.push(c),
        }
    }
}

/// A decimal as its sign, then its decimal exponent and digits (0.d1d2..
/// x 10^e, no trailing zero), every byte complemented for a negative.
fn write_decimal(d: &Decimal, buf: &mut Vec<u8>) {
    if d.is_zero() {
        return buf.push(1);
    }
    let d = d.normalize();
    let neg = d.is_sign_negative();
    let start = buf.len() + 1;
    buf.push(if neg { 0 } else { 2 });
    let mut digits = compact_str::format_compact!("{}", d.mantissa().unsigned_abs());
    let e = digits.len() as i32 - d.scale() as i32;
    let trimmed = digits.trim_end_matches('0').len();
    digits.truncate(trimmed);
    buf.push((e + 128) as u8);
    buf.extend_from_slice(digits.as_bytes());
    buf.push(0);
    if neg {
        for b in &mut buf[start..] {
            *b = !*b;
        }
    }
}

fn read_decimal(data: &mut &[u8]) -> Result<Decimal> {
    let neg = match take::<1>(data)?[0] {
        1 => return Ok(Decimal::ZERO),
        0 => true,
        2 => false,
        _ => bail!("undecodable key: bad decimal"),
    };
    let byte = |b: u8| if neg { !b } else { b };
    let e = byte(take::<1>(data)?[0]) as i32 - 128;
    let mut mantissa: i128 = 0;
    let mut n = 0i32;
    loop {
        match byte(take::<1>(data)?[0]) {
            0 => break,
            c @ b'0'..=b'9' if n < 29 => {
                mantissa = mantissa * 10 + (c - b'0') as i128;
                n += 1;
            }
            _ => bail!("undecodable key: bad decimal"),
        }
    }
    let mut scale = n - e;
    while scale < 0 {
        mantissa *= 10;
        scale += 1;
    }
    let d = Decimal::try_from_i128_with_scale(mantissa, scale as u32)?;
    Ok(if neg { -d } else { d })
}

fn write_payload(v: &Value, buf: &mut Vec<u8>, depth: usize) -> Result<()> {
    if depth > MAX_DEPTH {
        bail!("a key nests deeper than {MAX_DEPTH} levels")
    }
    match v {
        Value::U8(n) => buf.push(*n),
        Value::I8(n) => buf.extend_from_slice(&signed!(*n, u8)),
        Value::U16(n) => buf.extend_from_slice(&n.to_be_bytes()),
        Value::I16(n) => buf.extend_from_slice(&signed!(*n, u16)),
        Value::U32(n) | Value::V32(n) => buf.extend_from_slice(&n.to_be_bytes()),
        Value::I32(n) | Value::Z32(n) => buf.extend_from_slice(&signed!(*n, u32)),
        Value::U64(n) | Value::V64(n) => buf.extend_from_slice(&n.to_be_bytes()),
        Value::I64(n) | Value::Z64(n) => buf.extend_from_slice(&signed!(*n, u64)),
        Value::F32(f) => buf.extend_from_slice(&float_key!(*f, u32)),
        Value::F64(f) => buf.extend_from_slice(&float_key!(*f, u64)),
        Value::Bool(b) => buf.push(*b as u8),
        Value::Null => (),
        Value::String(s) => write_delimited(s.as_bytes(), buf),
        Value::Bytes(b) => write_delimited(b, buf),
        Value::Decimal(d) => write_decimal(d, buf),
        Value::DateTime(dt) => {
            buf.extend_from_slice(&signed!(dt.timestamp(), u64));
            buf.extend_from_slice(&dt.timestamp_subsec_nanos().to_be_bytes());
        }
        Value::Duration(d) => {
            buf.extend_from_slice(&d.as_secs().to_be_bytes());
            buf.extend_from_slice(&d.subsec_nanos().to_be_bytes());
        }
        Value::Error(e) => write_tagged(e, buf, depth + 1)?,
        Value::Array(a) => {
            for v in a.iter() {
                buf.push(1);
                write_tagged(v, buf, depth + 1)?;
            }
            buf.push(0);
        }
        Value::Map(m) => {
            for (k, v) in m.into_iter() {
                buf.push(1);
                write_tagged(k, buf, depth + 1)?;
                write_tagged(v, buf, depth + 1)?;
            }
            buf.push(0);
        }
        Value::Abstract(_) => bail!("{v} cannot be a key"),
    }
    Ok(())
}

fn read_payload(t: Typ, data: &mut &[u8], depth: usize) -> Result<Value> {
    if depth > MAX_DEPTH {
        bail!("undecodable key: nests deeper than {MAX_DEPTH} levels")
    }
    macro_rules! unsigned {
        ($u:ty, $n:literal) => {
            <$u>::from_be_bytes(take::<$n>(data)?)
        };
    }
    macro_rules! signed_of {
        ($u:ty, $i:ty, $n:literal) => {
            (<$u>::from_be_bytes(take::<$n>(data)?) ^ (1 << (<$u>::BITS - 1))) as $i
        };
    }
    Ok(match t {
        Typ::U8 => Value::U8(take::<1>(data)?[0]),
        Typ::I8 => Value::I8(signed_of!(u8, i8, 1)),
        Typ::U16 => Value::U16(unsigned!(u16, 2)),
        Typ::I16 => Value::I16(signed_of!(u16, i16, 2)),
        Typ::U32 => Value::U32(unsigned!(u32, 4)),
        Typ::V32 => Value::V32(unsigned!(u32, 4)),
        Typ::I32 => Value::I32(signed_of!(u32, i32, 4)),
        Typ::Z32 => Value::Z32(signed_of!(u32, i32, 4)),
        Typ::U64 => Value::U64(unsigned!(u64, 8)),
        Typ::V64 => Value::V64(unsigned!(u64, 8)),
        Typ::I64 => Value::I64(signed_of!(u64, i64, 8)),
        Typ::Z64 => Value::Z64(signed_of!(u64, i64, 8)),
        Typ::F32 => Value::F32(float_of_key!(take::<4>(data)?, u32, f32)),
        Typ::F64 => Value::F64(float_of_key!(take::<8>(data)?, u64, f64)),
        Typ::Bool => Value::Bool(take::<1>(data)?[0] != 0),
        Typ::Null => Value::Null,
        Typ::String => {
            let b = read_delimited(data)?;
            Value::String(ArcStr::from(std::str::from_utf8(&b)?))
        }
        Typ::Bytes => {
            Value::Bytes(bytes::Bytes::copy_from_slice(&read_delimited(data)?).into())
        }
        Typ::Decimal => Value::Decimal(Arc::new(read_decimal(data)?)),
        Typ::DateTime => {
            let secs = signed_of!(u64, i64, 8);
            let nanos = unsigned!(u32, 4);
            let dt = DateTime::from_timestamp(secs, nanos)
                .ok_or_else(|| anyhow!("undecodable key: bad datetime"))?;
            Value::DateTime(Arc::new(dt))
        }
        Typ::Duration => {
            let secs = unsigned!(u64, 8);
            Value::Duration(Arc::new(Duration::new(secs, unsigned!(u32, 4))))
        }
        Typ::Error => Value::Error(ValError::new(read_tagged(data, depth + 1)?)),
        Typ::Array => {
            let mut elts: LPooled<Vec<Value>> = LPooled::take();
            while take::<1>(data)?[0] != 0 {
                elts.push(read_tagged(data, depth + 1)?);
            }
            Value::Array(ValArray::from_iter_exact(elts.drain(..)))
        }
        Typ::Map => {
            let mut m = netidx_value::Map::new();
            while take::<1>(data)?[0] != 0 {
                let k = read_tagged(data, depth + 1)?;
                let v = read_tagged(data, depth + 1)?;
                m.insert_cow(k, v);
            }
            Value::Map(m)
        }
        Typ::Abstract => bail!("undecodable key: an abstract value"),
    })
}

pub(crate) fn parse_batch_ops(
    key_typ: Option<Typ>,
    arr: &ValArray,
) -> Result<sled::Batch> {
    let mut batch = sled::Batch::default();
    for op in arr.iter() {
        match op {
            Value::Array(a) => match &a[..] {
                [Value::String(tag), k, v] if &**tag == "Insert" => batch.insert(
                    encode_key(key_typ, k)?.as_slice(),
                    encode_value(v)?.as_slice(),
                ),
                [Value::String(tag), k] if &**tag == "Remove" => {
                    batch.remove(encode_key(key_typ, k)?.as_slice())
                }
                _ => bail!("not a batch op: {op}"),
            },
            _ => bail!("not a batch op: {op}"),
        }
    }
    Ok(batch)
}

#[cfg(test)]
mod test {
    use super::*;

    fn key(v: &Value) -> Vec<u8> {
        encode_key(None, v).unwrap().to_vec()
    }

    fn typed(v: &Value) -> Vec<u8> {
        encode_key(Some(Typ::get(v)), v).unwrap().to_vec()
    }

    fn values() -> Vec<Value> {
        let dec = |s: &str| Value::Decimal(Arc::new(s.parse().unwrap()));
        let dt = |s: i64, n: u32| {
            Value::DateTime(Arc::new(DateTime::from_timestamp(s, n).unwrap()))
        };
        vec![
            Value::I64(-5),
            Value::I64(0),
            Value::I64(7),
            Value::F64(f64::NAN),
            Value::F64(f64::NEG_INFINITY),
            Value::F64(-3.0),
            Value::F64(-1.0),
            Value::F64(0.0),
            Value::F64(0.5),
            Value::F64(2.0),
            Value::F64(f64::INFINITY),
            Value::F32(-2.5),
            Value::F32(1.5),
            Value::Bool(false),
            Value::Bool(true),
            Value::Null,
            dec("-12.5"),
            dec("-1.25"),
            dec("-0.1"),
            dec("0"),
            dec("0.001"),
            dec("0.1"),
            dec("0.12"),
            dec("1"),
            dec("10"),
            dec("12.5"),
            dt(-100, 5),
            dt(-100, 7),
            dt(0, 0),
            dt(1_000_000, 0),
            Value::String(ArcStr::from("")),
            Value::String(ArcStr::from("a")),
            Value::String(ArcStr::from("a\0")),
            Value::String(ArcStr::from("ab")),
            Value::String(ArcStr::from("b")),
            Value::Array(ValArray::from([])),
            Value::Array(ValArray::from([Value::I64(-1)])),
            Value::Array(ValArray::from([Value::I64(-1), Value::String("x".into())])),
            Value::Array(ValArray::from([Value::I64(2)])),
            Value::Array(ValArray::from([Value::String("ab".into())])),
            Value::Array(ValArray::from([Value::String("b".into())])),
        ]
    }

    #[test]
    fn key_bytes_sort_as_values() {
        let vs = values();
        for a in &vs {
            for b in &vs {
                assert_eq!(a.cmp(b), key(a).cmp(&key(b)), "{a} vs {b}");
                if Typ::get(a) == Typ::get(b) {
                    assert_eq!(a.cmp(b), typed(a).cmp(&typed(b)), "typed {a} vs {b}");
                }
            }
        }
    }

    #[test]
    fn keys_round_trip() {
        for v in values() {
            let back = decode_key(None, &key(&v)).unwrap();
            assert_eq!(back, v);
            let back = decode_key(Some(Typ::get(&v)), &typed(&v)).unwrap();
            assert_eq!(back, v);
        }
        assert_eq!(decode_key(None, &key(&Value::F64(-0.0))).unwrap(), Value::F64(0.0));
    }
}
