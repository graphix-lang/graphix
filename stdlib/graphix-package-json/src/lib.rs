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
fn number(n: &serde_json::Number, exact: bool) -> Option<Value> {
    let f = n.as_f64()?;
    let text = n.as_str();
    let d = match exact {
        false => None,
        true => text.parse::<Decimal>().ok(),
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

    fn under(&self, typ: Option<&'a Type>, value: &'a Value, levels: usize) -> Self {
        Self { env: self.env, typ, value, depth: self.depth + levels }
    }
}

impl Serialize for JsonView<'_> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let shape = shape(self.env, self.typ, self.value).map_err(S::Error::custom)?;
        let containers = match &shape {
            Shape::Pairs(_) => 2,
            Shape::Object(..) | Shape::Seq(_) | Shape::Leaf(Value::Bytes(_)) => 1,
            Shape::Leaf(_) => 0,
        };
        if containers > 0 && self.depth + containers >= MAX_DEPTH {
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
                    map.serialize_entry(k.as_str(), &self.under(t.as_ref(), v, 1))?;
                }
                map.end()
            }
            Shape::Seq(elems) => {
                let mut seq = s.serialize_seq(Some(elems.len()))?;
                for (v, t) in elems.iter() {
                    seq.serialize_element(&self.under(t.as_ref(), v, 1))?;
                }
                seq.end()
            }
            Shape::Pairs(pairs) => {
                let mut seq = s.serialize_seq(Some(pairs.len()))?;
                let mut seen: LPooled<AHashSet<serde_json::Value>> = LPooled::take();
                for (k, kt, v, vt) in pairs.iter() {
                    let k = serde_json::to_value(self.under(kt.as_ref(), k, 2))
                        .map_err(S::Error::custom)?;
                    if seen.contains(&k) {
                        return Err(S::Error::custom(
                            "two keys write as the same JSON data",
                        ));
                    }
                    seq.serialize_element(&(&k, self.under(vt.as_ref(), v, 2)))?;
                    seen.insert(k);
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
        Value::Decimal(d) => match d.to_string().parse::<serde_json::Number>() {
            Ok(n) => n.serialize(s),
            Err(e) => {
                Err(S::Error::custom(format_args!("cannot write {d} as JSON: {e}")))
            }
        },
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
    fc_write_str; cached
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
    fc_write_bytes; cached
);

graphix_derive::defpackage! {
    builtins => [
        JsonRead,
        JsonWriteStr,
        JsonWriteBytes,
    ],
}

#[cfg(test)]
mod test {
    use super::*;
    use graphix_compiler::{expr::parser::parse_type, list, rep, typ::Format};
    use netidx_value::Typ;
    use std::time::Duration;
    use triomphe::Arc;

    /// The source of `typ::textdata`, whose unit tests name the unions
    /// this test reads back.
    const TEXTDATA: &str = include_str!("../../../graphix-types/src/typ/textdata.rs");

    /// The most values a composite's witnesses take from the product of
    /// its parts', evenly spaced through it.
    const CAP: usize = 2000;

    /// A type and the typedefs it names, each `(name, body)`.
    type Case = (Vec<(&'static str, &'static str)>, &'static str);

    /// Every type textdata's unit tests judge: `json("..")`, `toml("..")`
    /// and `named(&[("Name", "body"), ..], "..", ..)`, its typedefs
    /// written in the call.
    fn cases() -> Vec<Case> {
        let mut found = vec![];
        for call in ["json(\"", "toml(\""] {
            let mut rest = TEXTDATA;
            while let Some(i) = rest.find(call) {
                rest = &rest[i + call.len()..];
                let end = rest.find("\")").expect("a closed call");
                found.push((vec![], &rest[..end]));
                rest = &rest[end..];
            }
        }
        // the literal `rest` starts in, past its opening quote
        let literal = |rest: &mut &'static str| {
            let end = rest.find('"').expect("a closed string");
            let s = &rest[..end];
            *rest = &rest[end + 1..];
            s
        };
        let mut rest = TEXTDATA;
        while let Some(i) = rest.find("named(") {
            rest = rest[i + "named(".len()..].trim_start();
            let Some(r) = rest.strip_prefix("&[") else { continue };
            rest = r;
            let mut defs = vec![];
            loop {
                let at = rest.find(['"', ']']).expect("a closed list");
                let closed = rest.as_bytes()[at] == b']';
                rest = &rest[at + 1..];
                if closed {
                    break;
                }
                defs.push(literal(&mut rest));
            }
            assert!(
                !defs.is_empty() && defs.len() % 2 == 0,
                "textdata.rs: write a named call's typedefs in the call"
            );
            rest = &rest[rest.find('"').expect("a type") + 1..];
            let t = literal(&mut rest);
            found.push((defs.chunks(2).map(|d| (d[0], d[1])).collect(), t));
        }
        found.sort();
        found.dedup();
        found
    }

    /// The values of a type a JSON reader may confuse, by the classes
    /// textdata judges: each integer type's values among the edges
    /// (every integer type holds it, an f32 holds it, an f32 or an f64
    /// only rounds it, its type's extremes); floats whole and fractional
    /// at the same edges; decimals with and without a scale; durations
    /// whole and fractional; datetimes; strings that read as each class,
    /// as every tag and field name in the type, or as nothing. A
    /// composite takes the product of its parts' (at most [`CAP`]); an
    /// array and a map are empty, of one element or entry, and of two.
    struct Witnesses {
        env: Env,
        texts: Vec<ArcStr>,
    }

    impl Witnesses {
        fn new(env: Env, t: &Type) -> Self {
            let mut texts: Vec<ArcStr> = [
                "",
                "x",
                "1",
                "-5",
                "0.1",
                "3.0",
                "3.5",
                "1e20",
                "16777217",
                "true",
                "null",
                "2020-01-01T00:00:00+00:00",
                "1970-01-01T00:00:03Z",
            ]
            .into_iter()
            .map(ArcStr::from)
            .collect();
            names(&env, t, &mut vec![], &mut texts);
            Self { env, texts }
        }

        fn of(&self, t: &Type, depth: usize) -> Vec<Value> {
            match t {
                Type::Primitive(p) => p.iter().flat_map(|p| self.prim(p)).collect(),
                Type::Set(ms) => ms.iter().flat_map(|m| self.of(m, depth)).collect(),
                Type::TVar(tv) => tv.binding().map_or(vec![], |b| self.of(&b, depth)),
                Type::Ref(_) if depth == 0 => vec![],
                Type::Ref(_) => {
                    t.lookup_ref(&self.env).map_or(vec![], |b| self.of(&b, depth - 1))
                }
                Type::Array(e) => {
                    seqs(self.of(e, depth)).map(|vs| Value::Array(vs.into())).collect()
                }
                Type::List(e) => seqs(self.of(e, depth)).map(list::from_iter).collect(),
                Type::Tuple(ts) => {
                    product(ts.iter().map(|t| self.of(t, depth)).collect(), rep::tuple)
                }
                Type::Struct(fs) => {
                    let parts = fs.iter().map(|(_, t, _)| self.of(t, depth)).collect();
                    product(parts, |vs| {
                        rep::structure(fs.iter().map(|(n, _, _)| n.clone()).zip(vs))
                    })
                }
                Type::Variant(tag, ps, _) => {
                    let parts = ps.iter().map(|t| self.of(t, depth)).collect();
                    product(parts, |vs| rep::make_variant(tag.clone(), vs))
                }
                Type::Map { key, value } => {
                    let (ks, vs) = (self.of(key, depth), self.of(value, depth));
                    let entry = |vs: Vec<Value>| match &vs[..] {
                        [k, v] => (k.clone(), v.clone()),
                        _ => unreachable!("a key and a value"),
                    };
                    let mut maps = vec![Value::Map(netidx_value::Map::new())];
                    maps.extend(product(vec![ks.clone(), vs.clone()], |e| {
                        Value::Map(netidx_value::Map::from_iter([entry(e)]))
                    }));
                    if let Some(v) = vs.first() {
                        maps.extend(ks.windows(2).map(|k| {
                            let es = k.iter().map(|k| (k.clone(), v.clone()));
                            Value::Map(netidx_value::Map::from_iter(es))
                        }))
                    }
                    maps
                }
                _ => vec![],
            }
        }

        fn prim(&self, p: Typ) -> Vec<Value> {
            const INTS: [i128; 19] = [
                0,
                1,
                100,
                -100,
                200,
                1000,
                -1000,
                60000,
                70000,
                -70000,
                16777217,
                -16777217,
                i32::MAX as i128,
                i32::MIN as i128,
                u32::MAX as i128,
                9007199254740993,
                i64::MAX as i128,
                i64::MIN as i128,
                u64::MAX as i128,
            ];
            const F64S: [f64; 14] = [
                0.0,
                0.5,
                -2.5,
                0.1,
                3.0,
                255.0,
                16777216.0,
                16777217.0,
                1234567936.0,
                9007199254740992.0,
                1e20,
                0.30000000000000004,
                1.5e300,
                3.4028234663852886e38,
            ];
            const F32S: [f32; 11] = [
                0.0,
                0.5,
                -2.5,
                0.1,
                3.0,
                255.0,
                16777216.0,
                1234567936.0,
                1e20,
                3.4028235e38,
                1e-7,
            ];
            let int =
                |f: fn(i128) -> Option<Value>| INTS.iter().filter_map(move |i| f(*i));
            let datetime = |s: &'static str| {
                Type::Primitive(Typ::DateTime.into())
                    .cast_value(&self.env, Value::from(s))
            };
            match p {
                Typ::Null => vec![Value::Null],
                Typ::Bool => vec![Value::Bool(true), Value::Bool(false)],
                Typ::I8 => int(|i| Some(Value::I8(i.try_into().ok()?))).collect(),
                Typ::U8 => int(|i| Some(Value::U8(i.try_into().ok()?))).collect(),
                Typ::I16 => int(|i| Some(Value::I16(i.try_into().ok()?))).collect(),
                Typ::U16 => int(|i| Some(Value::U16(i.try_into().ok()?))).collect(),
                Typ::I32 => int(|i| Some(Value::I32(i.try_into().ok()?))).collect(),
                Typ::Z32 => int(|i| Some(Value::Z32(i.try_into().ok()?))).collect(),
                Typ::U32 => int(|i| Some(Value::U32(i.try_into().ok()?))).collect(),
                Typ::V32 => int(|i| Some(Value::V32(i.try_into().ok()?))).collect(),
                Typ::I64 => int(|i| Some(Value::I64(i.try_into().ok()?))).collect(),
                Typ::Z64 => int(|i| Some(Value::Z64(i.try_into().ok()?))).collect(),
                Typ::U64 => int(|i| Some(Value::U64(i.try_into().ok()?))).collect(),
                Typ::V64 => int(|i| Some(Value::V64(i.try_into().ok()?))).collect(),
                Typ::F32 => F32S.iter().map(|f| Value::F32(*f)).collect(),
                Typ::F64 => F64S.iter().map(|f| Value::F64(*f)).collect(),
                Typ::Decimal => {
                    ["3", "1.5", "1.10", "0.1", "12345678901234567890.123456789"]
                        .iter()
                        .map(|d| Value::Decimal(Arc::new(d.parse().expect("a decimal"))))
                        .collect()
                }
                Typ::Duration => [3.0, 3.5, 0.1, 0.0]
                    .iter()
                    .map(|s| Value::Duration(Arc::new(Duration::from_secs_f64(*s))))
                    .collect(),
                Typ::DateTime => {
                    vec![
                        datetime("2020-01-01T00:00:00Z"),
                        datetime("1970-01-01T00:00:03Z"),
                    ]
                }
                Typ::String => self.texts.iter().cloned().map(Value::String).collect(),
                Typ::Bytes => {
                    vec![Value::Bytes(PBytes::new(Bytes::from_static(&[1, 2])))]
                }
                _ => vec![],
            }
        }
    }

    /// The tags and field names in `t`, through each reference not in
    /// `seen`.
    fn names(env: &Env, t: &Type, seen: &mut Vec<Type>, out: &mut Vec<ArcStr>) {
        let parts: Vec<&Type> = match t {
            Type::Variant(tag, ps, _) => {
                out.push(tag.clone());
                ps.iter().collect()
            }
            Type::Struct(fs) => fs
                .iter()
                .map(|(n, t, _)| {
                    out.push(n.clone());
                    t
                })
                .collect(),
            Type::Set(ts) | Type::Tuple(ts) => ts.iter().collect(),
            Type::Array(e) | Type::List(e) => vec![&**e],
            Type::Map { key, value } => vec![&**key, &**value],
            Type::Ref(_) if !seen.contains(t) => {
                seen.push(t.clone());
                if let Ok(body) = t.lookup_ref(env) {
                    names(env, &body, seen, out)
                }
                vec![]
            }
            _ => vec![],
        };
        parts.into_iter().for_each(|p| names(env, p, seen, out))
    }

    /// The elements of a sequence's witnesses: none, each alone, and each
    /// beside the next.
    fn seqs(es: Vec<Value>) -> impl Iterator<Item = Vec<Value>> {
        let pairs: Vec<Vec<Value>> = es.windows(2).map(|w| w.to_vec()).collect();
        std::iter::once(vec![]).chain(es.into_iter().map(|e| vec![e])).chain(pairs)
    }

    /// `build` of at most [`CAP`] combinations of one value of each part,
    /// spaced through the product by a stride coprime to every part's
    /// length, so no part's values stand still.
    fn product(
        parts: Vec<Vec<Value>>,
        build: impl Fn(Vec<Value>) -> Value,
    ) -> Vec<Value> {
        fn gcd(a: usize, b: usize) -> usize {
            if b == 0 { a } else { gcd(b, a % b) }
        }
        if parts.iter().any(|p| p.is_empty()) {
            return vec![];
        }
        let total = parts.iter().fold(1usize, |n, p| n.saturating_mul(p.len()));
        let mut step = total.div_ceil(CAP);
        while parts.iter().any(|p| gcd(step, p.len()) != 1) {
            step += 1
        }
        (0..total)
            .step_by(step)
            .map(|mut i| {
                build(
                    parts
                        .iter()
                        .map(|p| {
                            let v = p[i % p.len()].clone();
                            i /= p.len();
                            v
                        })
                        .collect(),
                )
            })
            .collect()
    }

    // textdata mirrors the union reader's pick (`cast_member`); this
    // checks the two agree with the real writer and the real reader: of
    // every type textdata's unit tests name that JsonData admits, every
    // witness written as JSON reads back as itself.
    #[test]
    fn admitted_unions_read_back() {
        let (mut admitted, mut named, mut checked) = (0, 0, 0);
        let mut failed = vec![];
        let tested = cases();
        assert!(tested.len() > 80, "found {} types in textdata.rs", tested.len());
        for (defs, src) in tested {
            let (env, t) = Env::typed_at_root(&defs, src).expect("a type");
            let w = Witnesses::new(env, &t);
            if t.text_ambiguity(&w.env, Format::Json).is_some() {
                continue;
            }
            admitted += 1;
            named += !defs.is_empty() as usize;
            let err = Type::Error(Arc::new(Type::Primitive(Typ::String.into())));
            let result = Type::Set(Arc::from_iter([t.clone(), err]));
            let target = CastTarget::of_result(&w.env, Some(&result));
            let exact = target.holds_exact_number(&w.env);
            for v in w.of(&t, 3) {
                let Ok(text) =
                    serde_json::to_string(&JsonView::new(&w.env, Some(&t), &v))
                else {
                    continue;
                };
                checked += 1;
                let read = JsonValue { exact }
                    .read(serde_json::Deserializer::from_str(&text))
                    .expect("the writer's JSON");
                let back = Json::read(&target, &w.env, read);
                if back != v {
                    failed.push(format!("{src}: {v} wrote {text}, read back {back}"));
                }
            }
        }
        assert!(
            admitted > 30 && named > 5 && checked > 5000,
            "{admitted} types ({named} over typedefs), {checked} values"
        );
        assert!(failed.is_empty(), "{}", failed.join("\n"));
    }
}
