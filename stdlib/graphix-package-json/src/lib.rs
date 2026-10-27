#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::Result;
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    env::Env,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, CastTarget, ReadFormat, ReadInput, Shape, TypedRead,
    f32_as_written, shape,
};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::local::LPooled;
use rust_decimal::{Decimal, prelude::ToPrimitive};
use serde::{
    Deserialize, Deserializer, Serialize, Serializer,
    de::{self, DeserializeSeed, MapAccess, SeqAccess, Visitor},
    ser::{Error as _, SerializeMap, SerializeSeq},
};
use std::fmt;

/// A JSON document read straight into a graphix value: a number is the
/// first of i64 and u64 that holds it, else as [`number`] says, and an
/// object an array of `[key, value]` pairs sorted by key, a repeated key's
/// last value kept.
#[derive(Clone, Copy)]
struct JsonValue {
    exact: bool,
}

impl JsonValue {
    /// The one document `de` holds, nothing after it.
    fn read<'de, R: serde_json::de::Read<'de>>(
        self,
        mut de: serde_json::Deserializer<R>,
    ) -> serde_json::Result<Value> {
        let v = self.deserialize(&mut de)?;
        de.end()?;
        Ok(v)
    }
}

impl<'de> DeserializeSeed<'de> for JsonValue {
    type Value = Value;

    fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<Value, D::Error> {
        d.deserialize_any(self)
    }
}

/// An object's key, read into an `ArcStr` once.
struct JsonKey(ArcStr);

impl<'de> Deserialize<'de> for JsonKey {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct KeyVisitor;
        impl<'de> Visitor<'de> for KeyVisitor {
            type Value = ArcStr;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("an object key")
            }

            fn visit_str<E: de::Error>(self, s: &str) -> Result<ArcStr, E> {
                Ok(ArcStr::from(s))
            }
        }
        d.deserialize_str(KeyVisitor).map(JsonKey)
    }
}

/// The key serde_json's arbitrary_precision passes a number under.
const NUMBER_TOKEN: &str = "$serde_json::private::Number";

/// A number past i64 and u64: an f64, or, when `exact`, a decimal where
/// the f64 does not read back as the text and a decimal holds it, so an
/// integer target refuses what does not fit and a decimal target reads it
/// whole. `None` past f64's range.
// CR claude for eric: [bug/medium] Text with more digits than a decimal holds fails
// from_str_exact and from_scientific and goes through the f64, so a decimal target
// keeps about 16 digits where 28 fit: "12345678901234567890.123456789123" reads as
// 12345678901234567168. cast.rs `parse` (Typ::Decimal) does the same for cast<decimal>
// and str::parse. Decide between rounding to the nearest decimal (fall back to
// Decimal::from_str, which rounds) and refusing with InvalidCast, and apply the rule in
// both places. probe:
// design/review-2026-10-26/repro/small-pkgs-json-decimal-past-scale.gx (small-pkgs-06)
fn number(n: &serde_json::Number, exact: bool) -> Option<Value> {
    let f = n.as_f64()?;
    let text = n.as_str();
    let d = match exact {
        false => None,
        true => {
            Decimal::from_str_exact(text).or_else(|_| Decimal::from_scientific(text)).ok()
        }
    };
    Some(match d {
        Some(d) if !reads_back(f, &d) => Value::Decimal(d.into()),
        _ => Value::F64(f),
    })
}

/// Whether `f` reads back as `d`: `d` itself when it is a whole number,
/// else in its shortest form.
fn reads_back(f: f64, d: &Decimal) -> bool {
    match d.fract().is_zero() {
        true => d.to_i128() == Some(f as i128),
        false => Decimal::from_str_exact(&format_compact!("{f}")).is_ok_and(|s| s == *d),
    }
}

impl<'de> Visitor<'de> for JsonValue {
    type Value = Value;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a JSON value")
    }

    fn visit_unit<E: de::Error>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_bool<E: de::Error>(self, b: bool) -> Result<Value, E> {
        Ok(Value::Bool(b))
    }

    fn visit_i64<E: de::Error>(self, i: i64) -> Result<Value, E> {
        Ok(Value::I64(i))
    }

    fn visit_u64<E: de::Error>(self, u: u64) -> Result<Value, E> {
        Ok(i64::try_from(u).map_or(Value::U64(u), Value::I64))
    }

    fn visit_f64<E: de::Error>(self, f: f64) -> Result<Value, E> {
        Ok(Value::F64(f))
    }

    fn visit_str<E: de::Error>(self, s: &str) -> Result<Value, E> {
        Ok(Value::String(ArcStr::from(s)))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut a: A) -> Result<Value, A::Error> {
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        while let Some(v) = a.next_element_seed(self)? {
            vals.push(v)
        }
        Ok(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut a: A) -> Result<Value, A::Error> {
        let mut pairs: LPooled<Vec<(ArcStr, Value)>> = LPooled::take();
        while let Some(JsonKey(k)) = a.next_key()? {
            // arbitrary_precision hands a number over as a one-entry map
            // under NUMBER_TOKEN, so an object whose first key is it reads as
            // a number
            // CR claude for eric: [bug/low] The visitor cannot tell serde_json's number
            // map from a real object whose first key is the token, whether written
            // plainly or as $. So json::read of {"$serde_json::private::Number": "42"}
            // into i64 gives 42, and a Map<string, i64> holding that key fails to
            // parse. With user-controlled keys this lets input change a document's
            // shape or make it fail. Decide whether the key is reserved (refuse and
            // document it), or read exact numbers without arbitrary_precision so no
            // object can read as a number. probe:
            // design/review-2026-10-26/repro/x-security-json-number-token.gx
            // (x-security-06)
            if pairs.is_empty() && &*k == NUMBER_TOKEN {
                let n: CompactString = a.next_value()?;
                let n = n.parse::<serde_json::Number>().map_err(de::Error::custom)?;
                return match (n.as_i64(), n.as_u64()) {
                    (Some(i), _) => Ok(Value::I64(i)),
                    (None, Some(u)) => Ok(Value::U64(u)),
                    (None, None) => number(&n, self.exact)
                        .ok_or_else(|| de::Error::custom("number out of range")),
                };
            }
            let v = a.next_value_seed(self)?;
            pairs.push((k, v))
        }
        // stable, so of a repeated key's pairs the last written is last
        pairs.sort_by(|a, b| a.0.cmp(&b.0));
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        let mut pairs = pairs.drain(..).peekable();
        while let Some((k, v)) = pairs.next() {
            if pairs.peek().is_some_and(|(next, _)| *next == k) {
                continue;
            }
            vals.push(Value::Array(ValArray::from([Value::String(k), v])));
        }
        Ok(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
    }
}

/// How many containers deep a value may not nest to be written as JSON:
/// where json::read's recursion limit refuses, short of the serializer's
/// recursion running out of stack.
const MAX_DEPTH: usize = 128;

/// `value` as JSON, written as its type `typ` says (`None` reads the value
/// alone): a serde view a writer serializes as it walks the value.
pub struct JsonView<'a> {
    env: &'a Env,
    typ: Option<&'a Type>,
    value: &'a Value,
    depth: usize,
}

impl<'a> JsonView<'a> {
    pub fn new(env: &'a Env, typ: Option<&'a Type>, value: &'a Value) -> Self {
        Self { env, typ, value, depth: 0 }
    }

    fn under(&self, typ: Option<&'a Type>, value: &'a Value) -> Self {
        Self { env: self.env, typ, value, depth: self.depth + 1 }
    }
}

impl Serialize for JsonView<'_> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        // XCR claude for claude: [bug/low] This bounds the depth of leaves, but
        // json::read bounds containers. 127 arrays around an empty array (128
        // containers, no leaf) are written and then refused on read with "recursion
        // limit exceeded". The bytes arm patched one instance of this. Check
        // `self.depth + 1 >= MAX_DEPTH` for every container (Object, Seq, Bytes)
        // instead of this entry check, and pin the empty-innermost case. probe:
        // design/review-2026-10-26/repro/small-pkgs-json-empty-container-depth.gx
        // (small-pkgs-09)
        // 2026-10-27 claude: JsonView::serialize now refuses every container
        // (object, seq, bytes) at depth 127 or more, after shape, in place of the
        // entry check on leaves and the bytes arm; the repro's w(127) is refused on
        // write. Pin: graphix-tests
        // lib_tests::json::an_empty_innermost_container_counts (an empty array and
        // an empty map innermost; the round-trip tests now share
        // deepest_written_reads_back).
        let shape = shape(self.env, self.typ, self.value).map_err(S::Error::custom)?;
        // a container at depth n is the n + 1th: MAX_DEPTH of them is where
        // json::read stops
        if self.depth + 1 >= MAX_DEPTH
            && matches!(
                shape,
                Shape::Object(..) | Shape::Seq(_) | Shape::Leaf(Value::Bytes(_))
            )
        {
            return Err(S::Error::custom(format_args!(
                "the value nests {MAX_DEPTH} containers deep or more"
            )));
        }
        match shape {
            Shape::Object(entries, _) => {
                let mut map = s.serialize_map(Some(entries.len()))?;
                let mut keys: LPooled<AHashSet<ArcStr>> = LPooled::take();
                for (k, v, t) in entries.iter() {
                    // two keys may print alike (`1` and `"1"`): never one for two
                    if !keys.insert(k.clone()) {
                        return Err(S::Error::custom(format_args!(
                            "two keys write as the same JSON key {k}"
                        )));
                    }
                    map.serialize_entry(k.as_str(), &self.under(t.as_ref(), v))?;
                }
                map.end()
            }
            Shape::Seq(elems) => {
                let mut seq = s.serialize_seq(Some(elems.len()))?;
                for (v, t) in elems.iter() {
                    seq.serialize_element(&self.under(t.as_ref(), v))?;
                }
                seq.end()
            }
            Shape::Leaf(v) => serialize_leaf(&v, s),
        }
    }
}

fn serialize_leaf<S: Serializer>(value: &Value, s: S) -> Result<S::Ok, S::Error> {
    match value {
        Value::Null => s.serialize_unit(),
        Value::Bool(b) => s.serialize_bool(*b),
        Value::I8(n) => s.serialize_i8(*n),
        Value::I16(n) => s.serialize_i16(*n),
        Value::I32(n) | Value::Z32(n) => s.serialize_i32(*n),
        Value::I64(n) | Value::Z64(n) => s.serialize_i64(*n),
        Value::U8(n) => s.serialize_u8(*n),
        Value::U16(n) => s.serialize_u16(*n),
        Value::U32(n) | Value::V32(n) => s.serialize_u32(*n),
        Value::U64(n) | Value::V64(n) => s.serialize_u64(*n),
        Value::F32(n) => match f32_as_written(*n) {
            f if f.is_finite() => s.serialize_f64(f),
            _ => Err(S::Error::custom(format_args!("cannot represent {n} as JSON"))),
        },
        Value::F64(n) if n.is_finite() => s.serialize_f64(*n),
        Value::F64(n) => {
            Err(S::Error::custom(format_args!("cannot represent {n} as JSON")))
        }
        // CR claude for eric: [bug/low] A decimal is written as a JSON string. Other
        // consumers get "1.5" where a number belongs, and a [decimal, string] value
        // reads back as the string member, so the round trip changes the value.
        // arbitrary_precision is already on, so a decimal can be written as an exact
        // number through serde_json::Number. Decide whether that is the rule; a
        // [decimal, f64] target would then read a short decimal as the f64 member.
        // probe:
        // design/review-2026-10-26/repro/small-pkgs-json-decimal-writes-string.gx
        // (small-pkgs-08)
        Value::Decimal(d) => s.collect_str(d),
        Value::String(st) => s.serialize_str(st),
        Value::Bytes(b) => s.collect_seq(b.iter()),
        Value::DateTime(dt) => s.serialize_str(&dt.to_rfc3339()),
        Value::Duration(d) => s.serialize_f64(d.as_secs_f64()),
        Value::Array(_) | Value::Map(_) => {
            unreachable!("shape reads every array and map")
        }
        Value::Error(_) => Err(S::Error::custom("cannot serialize Error to JSON")),
        Value::Abstract(_) => {
            Err(S::Error::custom("cannot serialize abstract type to JSON"))
        }
    }
}

#[derive(Debug)]
struct Json;

impl ReadFormat for Json {
    const NAME: &str = "json_read";
    const TAG: ArcStr = literal!("JsonErr");
    type Args = (ReadInput, bool);

    fn prepare_args(cached: &CachedVals, exact: bool) -> Option<(ReadInput, bool)> {
        Some((ReadInput::of(cached)?, exact))
    }

    fn parse((input, exact): (ReadInput, bool)) -> impl Future<Output = Value> + Send {
        async move {
            let seed = JsonValue { exact };
            let json = match &input {
                ReadInput::Str(s) => seed.read(serde_json::Deserializer::from_str(s)),
                ReadInput::Bytes(b) => seed.read(serde_json::Deserializer::from_slice(b)),
            };
            match json {
                Ok(v) => v,
                Err(e) => errf!(Self::TAG, "{e}"),
            }
        }
    }

    /// Every decimal `parse` makes is a float it read exactly.
    fn read(target: &CastTarget, env: &Env, v: Value) -> Value {
        target.read_exact_floats(env, &Self::TAG, v)
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
    let json = JsonView::new(env, ft.args.get(1).map(|a| &a.typ), args.get(1)?);
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
