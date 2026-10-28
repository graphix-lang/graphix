//! The runtime forms of a tuple, a struct, a variant and a list cell;
//! only this module and `list.rs` know them. Each is an array whose
//! element 0 is its kind's marker, a Rust-registered abstract no Graphix
//! type names, so no array, string or other kind can
//! pass for one. A tuple holds its elements after the marker, a struct
//! its `[name, value]` fields sorted by name, a variant its tag (a
//! string) and then its payload, a list cell its head and tail (`list.rs`).
//!
//! Text data carries no markers; pack and db preserve them:
//! a tuple is an array, a struct an array of pairs, a variant its tag or
//! an array of its tag and payload, a list a flat array. The `read_*`
//! readers take data as well as values, telling a cast which one it got
//! ([`Parts`]); [`strip`] writes a value as data.
use anyhow::bail;
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use immutable_chunkmap::map::Map;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, ValArray, Value, abstract_type::AbstractWrapper};
use poolshark::local::LPooled;
use std::{cell::Cell, convert::Infallible, fmt, sync::LazyLock};

/// What a marker says a composite is.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Kind {
    Tuple,
    Struct,
    Variant,
    List,
}

impl Kind {
    pub fn from_u8(b: u8) -> Option<Self> {
        Some(match b {
            0 => Kind::Tuple,
            1 => Kind::Struct,
            2 => Kind::Variant,
            3 => Kind::List,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Marker(Kind);

impl fmt::Debug for Marker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Kind::Tuple => write!(f, "#tuple"),
            Kind::Struct => write!(f, "#struct"),
            Kind::Variant => write!(f, "#variant"),
            Kind::List => write!(f, "#list"),
        }
    }
}

/// Markers decode only inside an image or a validated binary data read.
impl Pack for Marker {
    fn encoded_len(&self) -> usize {
        1
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        Ok(buf.put_u8(self.0 as u8))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if (!crate::ids::in_session() && !READING_DATA.get()) || !buf.has_remaining() {
            return Err(PackError::InvalidFormat);
        }
        Kind::from_u8(buf.get_u8()).map(Marker).ok_or(PackError::InvalidFormat)
    }
}

thread_local! {
    static READING_DATA: Cell<bool> = const { Cell::new(false) };
}

/// Decode data whose composite kinds are preserved, rejecting markers outside
/// a well-formed composite and abstract values that data cannot carry.
pub fn decode_marked_data(buf: &mut impl Buf) -> Result<Value, PackError> {
    register();
    let _reading = crate::Restore::replace(&READING_DATA, true);
    let value = Value::decode(buf)?;
    let mut pending: LPooled<Vec<&Value>> = LPooled::from_iter([&value]);
    while let Some(v) = pending.pop() {
        match v {
            Value::Array(a) => match kind(v) {
                None => pending.extend(a.iter()),
                Some(Kind::Tuple) => pending.extend(a[1..].iter()),
                Some(Kind::Struct) => {
                    let fields = &a[1..];
                    if !fields.is_empty() && !is_struct(fields) {
                        return Err(PackError::InvalidFormat);
                    }
                    for f in fields {
                        pending.push(field(f).ok_or(PackError::InvalidFormat)?.1);
                    }
                }
                Some(Kind::Variant) => {
                    let (_, payload) = variant(v).ok_or(PackError::InvalidFormat)?;
                    pending.extend(payload.iter());
                }
                Some(Kind::List) => match &a[1..] {
                    [] => (),
                    [head, tail] if crate::list::is_list(tail) => {
                        pending.extend([head, tail]);
                    }
                    _ => return Err(PackError::InvalidFormat),
                },
            },
            Value::Map(m) => {
                for (k, v) in m {
                    pending.extend([k, v]);
                }
            }
            Value::Error(e) => pending.push(e),
            Value::Abstract(_) => return Err(PackError::InvalidFormat),
            _ => (),
        }
    }
    drop(pending);
    Ok(value)
}

static WRAPPER: LazyLock<AbstractWrapper<Marker>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0x7c, 0x2e, 0x91, 0x0b, 0x44, 0xd8, 0x4a, 0x13, 0x9f, 0x65, 0x28, 0xe0, 0x5a,
        0x17, 0xc3, 0x8d,
    ]);
    Abstract::register::<Marker>(id).expect("failed to register Marker")
});

thread_local! {
    static MARKS: [Value; 4] = [Kind::Tuple, Kind::Struct, Kind::Variant, Kind::List]
        .map(|k| WRAPPER.wrap(Marker(k)));
}

/// Register the marker with netidx before any value decodes.
#[doc(hidden)]
pub fn register() {
    LazyLock::force(&WRAPPER);
}

/// The marker of the kind `k`, which opens every value of it.
pub fn marker(k: Kind) -> Value {
    MARKS.with(|m| m[k as usize].clone())
}

/// The kind `v` marks, if it is a kind marker.
pub fn marker_kind(v: &Value) -> Option<Kind> {
    match v {
        Value::Abstract(m) => m.downcast_ref::<Marker>().map(|m| m.0),
        _ => None,
    }
}

/// The kind `v` is marked as, if it is a tuple, struct, variant or list
/// cell.
pub fn kind(v: &Value) -> Option<Kind> {
    match v {
        Value::Array(a) => marker_kind(a.first()?),
        _ => None,
    }
}

/// The parts of `v` after its marker, if it is marked `k`.
pub(crate) fn marked(v: &Value, k: Kind) -> Option<&[Value]> {
    match v {
        Value::Array(a) if kind(v) == Some(k) => Some(&a[1..]),
        _ => None,
    }
}

/// The value of kind `k` holding `parts`.
pub(crate) fn build(k: Kind, parts: impl IntoIterator<Item = Value>) -> Value {
    let mut a: LPooled<Vec<Value>> = LPooled::take();
    a.push(marker(k));
    a.extend(parts);
    Value::Array(ValArray::from_iter_exact(a.drain(..)))
}

/// Where a tuple's first element sits in its array.
pub const TUPLE_BASE: usize = 1;

/// Where a struct's first field sits in its array.
pub const STRUCT_BASE: usize = 1;

/// The tuple of `elems`.
pub fn tuple(
    elems: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    build(Kind::Tuple, elems)
}

/// A tuple's elements.
pub fn tuple_elems(v: &Value) -> Option<&[Value]> {
    marked(v, Kind::Tuple)
}

/// A composite's parts as a cast reads them: from a value already in its
/// form, or from data in its written form, which the cast rebuilds.
#[derive(Clone, Copy)]
pub enum Parts<'a> {
    Held(&'a [Value]),
    Data(&'a [Value]),
}

impl<'a> Parts<'a> {
    pub fn elems(self) -> &'a [Value] {
        match self {
            Self::Held(p) | Self::Data(p) => p,
        }
    }

    /// Whether the value is in its form already.
    pub fn held(self) -> bool {
        matches!(self, Self::Held(_))
    }
}

/// `v` read as a tuple: a tuple's elements, or an array's.
pub fn read_tuple(v: &Value) -> Option<Parts<'_>> {
    match tuple_elems(v) {
        Some(e) => Some(Parts::Held(e)),
        None => array_elems(v).map(|a| Parts::Data(&a[..])),
    }
}

/// `v` read as a struct: a struct's fields, or an array of pairs.
pub fn read_struct(v: &Value) -> Option<Parts<'_>> {
    match struct_fields(v) {
        Some(f) => Some(Parts::Held(f)),
        None => array_elems(v).map(|a| Parts::Data(&a[..])),
    }
}

/// `v` read as a variant: its tag and payload, or a string's tag, or an
/// array of a tag and its payload.
pub fn read_variant(v: &Value) -> Option<(&ArcStr, Parts<'_>)> {
    if let Some((tag, p)) = variant(v) {
        return Some((tag, Parts::Held(p)));
    }
    match v {
        Value::String(tag) => Some((tag, Parts::Data(&[]))),
        v => match &array_elems(v)?[..] {
            [Value::String(tag), payload @ ..] if !payload.is_empty() => {
                Some((tag, Parts::Data(payload)))
            }
            _ => None,
        },
    }
}

/// An array's elements: `None` for any other value, a tuple, struct,
/// variant or list included.
pub fn array_elems(v: &Value) -> Option<&ValArray> {
    match v {
        Value::Array(a) if kind(v).is_none() => Some(a),
        _ => None,
    }
}

/// The elements an index reads: a tuple's or an array's.
pub fn indexed(v: &Value) -> Option<&[Value]> {
    tuple_elems(v).or_else(|| array_elems(v).map(|a| &a[..]))
}

/// `v`, a tuple or an array, with the element at `i` replaced by `e`.
pub fn replace_indexed(v: &Value, i: usize, e: Value) -> Option<Value> {
    let elems = indexed(v)?;
    let mut e = Some(e);
    let elems = elems.iter().enumerate().map(|(k, x)| match k == i {
        true => e.take().expect("one element replaced"),
        false => x.clone(),
    });
    Some(match tuple_elems(v) {
        Some(_) => tuple(elems),
        None => Value::Array(ValArray::from_iter_exact(elems)),
    })
}

/// The struct `v` with its field at sorted position `i` holding `value`.
pub fn replace_field(v: &Value, i: usize, value: Value) -> Option<Value> {
    let fields = struct_fields(v)?;
    let (name, _) = field_at(fields, i)?;
    let mut value = Some(make_field(name.clone(), value));
    Some(structure_of_fields(fields.iter().enumerate().map(|(k, f)| match k == i {
        true => value.take().expect("one field replaced"),
        false => f.clone(),
    })))
}

/// The struct of `fields`, which must be sorted by name.
pub fn structure(
    fields: impl IntoIterator<Item = (ArcStr, Value), IntoIter: ExactSizeIterator>,
) -> Value {
    build(Kind::Struct, fields.into_iter().map(|(name, v)| make_field(name, v)))
}

/// The struct of already-made fields ([`make_field`]), sorted by name.
pub fn structure_of_fields(
    fields: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    build(Kind::Struct, fields)
}

/// A struct's fields, in sorted order; each is read with [`field`].
pub fn struct_fields(v: &Value) -> Option<&[Value]> {
    marked(v, Kind::Struct)
}

/// A struct field's name and value.
pub fn field(v: &Value) -> Option<(&ArcStr, &Value)> {
    match v {
        Value::Array(kv) => match &kv[..] {
            [Value::String(name), v] => Some((name, v)),
            _ => None,
        },
        _ => None,
    }
}

/// The field at sorted position `i` of the struct `fields`.
pub fn field_at(fields: &[Value], i: usize) -> Option<(&ArcStr, &Value)> {
    fields.get(i).and_then(field)
}

/// The position and value of the field `name` of the struct `fields`.
pub fn field_named<'a>(fields: &'a [Value], name: &str) -> Option<(usize, &'a Value)> {
    fields.iter().enumerate().find_map(|(i, f)| match field(f) {
        Some((n, v)) if n == name => Some((i, v)),
        _ => None,
    })
}

/// The field `name` holding `value`.
pub fn make_field(name: ArcStr, value: Value) -> Value {
    Value::Array(ValArray::from_iter_exact([Value::String(name), value].into_iter()))
}

/// Whether `fields` is a struct: one field or more, names ascending.
pub fn is_struct(fields: &[Value]) -> bool {
    let mut prev: Option<&ArcStr> = None;
    !fields.is_empty()
        && fields.iter().all(|f| match field(f) {
            Some((name, _)) if prev.is_none_or(|p| p < name) => {
                prev = Some(name);
                true
            }
            _ => false,
        })
}

/// The variant `tag` with `payload`, empty for a bare variant.
pub fn make_variant(
    tag: ArcStr,
    payload: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    build(Kind::Variant, std::iter::once(Value::String(tag)).chain(payload))
}

/// A variant's tag and payload, empty for a bare variant.
pub fn variant(v: &Value) -> Option<(&ArcStr, &[Value])> {
    match marked(v, Kind::Variant)? {
        [Value::String(tag), payload @ ..] => Some((tag, payload)),
        _ => None,
    }
}

/// The tag of `v`, a variant without a payload.
pub fn bare_tag(v: &Value) -> anyhow::Result<&str> {
    match variant(v) {
        Some((tag, [])) => Ok(tag.as_str()),
        _ => bail!("expected a variant without a payload, got {}", crate::typ::Naked(v)),
    }
}

/// Whether `v` is the variant `tag` with `arity` payload values.
pub fn is_variant(v: &Value, tag: &str, arity: usize) -> bool {
    variant(v).is_some_and(|(t, p)| t == tag && p.len() == arity)
}

/// The payload value at `i` of the variant `v`.
pub fn payload(v: &Value, i: usize) -> Option<&Value> {
    variant(v).and_then(|(_, p)| p.get(i))
}

/// `v` written as data: every tuple, struct, variant and list in it in
/// its data form, markers gone; `None` when nothing in it is marked.
pub fn strip(v: &Value) -> Option<Value> {
    match strip_with(v, &mut |c| Ok::<_, Infallible>(strip(c))) {
        Ok(v) => v,
    }
}

/// `v`'s marked form written as data, one level: each part in it is `f`'s
/// rewrite of that part, or the part itself when `f` changed nothing;
/// `None` when neither the level nor any part changed. Unchanged parts
/// are copied only once one has changed.
pub fn strip_with<E>(
    v: &Value,
    f: &mut impl FnMut(&Value) -> Result<Option<Value>, E>,
) -> Result<Option<Value>, E> {
    crate::stack::ensure_sufficient(|| {
        let mut each =
            |vs: &mut dyn Iterator<Item = &Value>| -> Result<LPooled<Vec<Value>>, E> {
                vs.map(|v| Ok(f(v)?.unwrap_or_else(|| v.clone()))).collect()
            };
        let array = |mut vs: LPooled<Vec<Value>>| {
            Value::Array(ValArray::from_iter_exact(vs.drain(..)))
        };
        Ok(match kind(v) {
            Some(Kind::Tuple) => match tuple_elems(v) {
                None => None,
                Some(es) => Some(array(each(&mut es.iter())?)),
            },
            Some(Kind::Struct) => match struct_fields(v) {
                None => None,
                Some(fs) => {
                    let mut fields: LPooled<Vec<Value>> = LPooled::take();
                    for fl in fs.iter() {
                        fields.push(match field(fl) {
                            Some((n, fv)) => make_field(
                                n.clone(),
                                f(fv)?.unwrap_or_else(|| fv.clone()),
                            ),
                            None => fl.clone(),
                        })
                    }
                    Some(array(fields))
                }
            },
            Some(Kind::Variant) => match variant(v) {
                None => None,
                Some((tag, payload)) if payload.is_empty() => {
                    Some(Value::String(tag.clone()))
                }
                Some((tag, payload)) => {
                    let mut vs = each(&mut payload.iter())?;
                    vs.insert(0, Value::String(tag.clone()));
                    Some(array(vs))
                }
            },
            Some(Kind::List) => {
                let mut vs: LPooled<Vec<Value>> = LPooled::take();
                for e in crate::list::Iter::new(v.clone()) {
                    vs.push(f(&e)?.unwrap_or(e))
                }
                Some(array(vs))
            }
            None => map_parts(v, f)?,
        })
    })
}

/// `v`'s parts, each `f`'s rewrite of it, as `strip_with` rewrites an
/// unmarked level: an array's elements (a marker included), a list's
/// heads and the end of its spine, a map's keys and values, an error's
/// payload; `None` when no part changed.
pub fn map_parts<E>(
    v: &Value,
    f: &mut impl FnMut(&Value) -> Result<Option<Value>, E>,
) -> Result<Option<Value>, E> {
    crate::stack::ensure_sufficient(|| {
        if crate::list::split(v).is_some() {
            return map_spine(v, f);
        }
        let array = |mut vs: LPooled<Vec<Value>>| {
            Value::Array(ValArray::from_iter_exact(vs.drain(..)))
        };
        Ok(match v {
            Value::Array(a) => {
                let mut out: LPooled<Vec<Value>> = LPooled::take();
                for (i, e) in a.iter().enumerate() {
                    match f(e)? {
                        Some(e) => {
                            if out.is_empty() {
                                out.extend(a[..i].iter().cloned());
                            }
                            out.push(e)
                        }
                        None if !out.is_empty() => out.push(e.clone()),
                        None => (),
                    }
                }
                (!out.is_empty()).then(|| array(out))
            }
            Value::Map(m) => {
                let mut kvs: Option<LPooled<Vec<(Value, Value)>>> = None;
                for (i, (k, e)) in m.into_iter().enumerate() {
                    let (k1, e1) = (f(k)?, f(e)?);
                    if kvs.is_none() && (k1.is_some() || e1.is_some()) {
                        kvs = Some(
                            m.into_iter()
                                .take(i)
                                .map(|(k, e)| (k.clone(), e.clone()))
                                .collect(),
                        );
                    }
                    if let Some(kvs) = &mut kvs {
                        kvs.push((
                            k1.unwrap_or_else(|| k.clone()),
                            e1.unwrap_or_else(|| e.clone()),
                        ));
                    }
                }
                kvs.map(|mut kvs| Value::Map(Map::from_iter(kvs.drain(..))))
            }
            Value::Error(e) => f(e)?.map(|e| Value::Error(e.into())),
            _ => None,
        })
    })
}

/// [`map_parts`] of the list cell `v`, walking its spine in a loop (a
/// list is a value as deep as it is long); the cells after the last part
/// `f` changed are shared.
fn map_spine<E>(
    v: &Value,
    f: &mut impl FnMut(&Value) -> Result<Option<Value>, E>,
) -> Result<Option<Value>, E> {
    let mut heads: LPooled<Vec<Value>> = LPooled::take();
    let (mut shared, mut unchanged) = (v, 0);
    let mut cur = v;
    while let Some((head, tail)) = crate::list::split(cur) {
        match f(head)? {
            None => unchanged += 1,
            Some(h) => {
                heads.extend(crate::list::heads(shared).take(unchanged).cloned());
                heads.push(h);
                (shared, unchanged) = (tail, 0);
            }
        }
        cur = tail;
    }
    let rest = match f(cur)? {
        Some(end) => {
            heads.extend(crate::list::heads(shared).take(unchanged).cloned());
            end
        }
        None if heads.is_empty() => return Ok(None),
        None => shared.clone(),
    };
    Ok(Some(crate::list::cons_all(&mut heads, rest)))
}

/// The derives' [`netidx_value::Shape`] for a type Graphix code sees:
/// `#[value(shape = "graphix_compiler::rep::GraphixShape")]`. Its reads
/// take data in its written form as well as values.
pub struct GraphixShape;

impl netidx_value::Shape for GraphixShape {
    fn make_struct(
        fields: impl IntoIterator<Item = (ArcStr, Value), IntoIter: ExactSizeIterator>,
    ) -> Value {
        structure(fields)
    }

    fn struct_fields(v: &Value) -> Option<&[Value]> {
        read_struct(v).map(Parts::elems)
    }

    fn make_tuple(
        elems: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
    ) -> Value {
        tuple(elems)
    }

    fn tuple_elems(v: &Value) -> Option<&[Value]> {
        read_tuple(v).map(Parts::elems)
    }

    fn make_variant(
        tag: ArcStr,
        payload: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
    ) -> Value {
        make_variant(tag, payload)
    }

    fn variant(v: &Value) -> Option<(&ArcStr, &[Value])> {
        read_variant(v).map(|(tag, p)| (tag, p.elems()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::list;
    use netidx_value::Value;
    use std::convert::Infallible;

    fn ints(v: &[i64]) -> Value {
        list::from_iter(v.iter().map(|i| Value::I64(*i)))
    }

    fn times10(
        at: &'static [i64],
    ) -> impl FnMut(&Value) -> Result<Option<Value>, Infallible> {
        |c| {
            Ok(match c {
                Value::I64(i) if at.contains(i) => Some(Value::I64(i * 10)),
                _ => None,
            })
        }
    }

    #[test]
    fn a_list_maps_its_heads_and_its_end() {
        let l = ints(&[1, 2, 3, 4, 5]);
        assert_eq!(map_parts(&l, &mut times10(&[])), Ok(None));
        assert_eq!(map_parts(&l, &mut times10(&[2])), Ok(Some(ints(&[1, 20, 3, 4, 5]))));
        assert_eq!(
            map_parts(&l, &mut times10(&[1, 5])),
            Ok(Some(ints(&[10, 2, 3, 4, 50])))
        );
        let mut end =
            |c: &Value| Ok::<_, Infallible>(list::is_nil(c).then(|| ints(&[6])));
        assert_eq!(map_parts(&l, &mut end), Ok(Some(ints(&[1, 2, 3, 4, 5, 6]))));
    }

    #[test]
    fn a_long_list_maps_in_a_loop() {
        let n = 1_000_000;
        let l = list::from_iter((0..n).map(Value::I64));
        assert_eq!(map_parts(&l, &mut times10(&[])), Ok(None));
        let mapped = map_parts(&l, &mut times10(&[999_999])).unwrap().unwrap();
        assert_eq!(list::heads(&mapped).last(), Some(&Value::I64(9_999_990)));
        assert_eq!(list::len(&mapped), Some(n as usize));
    }
    #[test]
    fn binary_data_preserves_composite_kinds() {
        let value = tuple([
            make_variant(arcstr::literal!("A"), []),
            crate::list::from_iter((0..10_000).map(Value::I64)),
            structure_of_fields([make_field(arcstr::literal!("x"), Value::I64(1))]),
        ]);
        let mut bytes: LPooled<Vec<u8>> = LPooled::take();
        value.encode(&mut *bytes).unwrap();
        let read = decode_marked_data(&mut bytes.as_slice()).unwrap();
        let parts = tuple_elems(&read).unwrap();
        assert!(is_variant(&parts[0], "A", 0));
        assert_eq!(crate::list::len(&parts[1]), Some(10_000));
        assert_eq!(
            field_at(struct_fields(&parts[2]).unwrap(), 0).unwrap().1,
            &Value::I64(1)
        );
    }

    #[test]
    fn binary_data_rejects_malformed_markers() {
        let malformed = [
            marker(Kind::Tuple),
            Value::Array([Value::Null, marker(Kind::Tuple)].into()),
            build(Kind::Variant, [Value::I64(1)]),
            build(Kind::List, [Value::I64(1), Value::Null]),
            build(
                Kind::Struct,
                [
                    make_field(arcstr::literal!("z"), Value::Null),
                    make_field(arcstr::literal!("a"), Value::Null),
                ],
            ),
        ];
        for value in malformed {
            let mut bytes: LPooled<Vec<u8>> = LPooled::take();
            value.encode(&mut *bytes).unwrap();
            assert!(decode_marked_data(&mut bytes.as_slice()).is_err());
        }
    }

    #[test]
    fn binary_data_decode_does_not_enable_other_readers() {
        let value = tuple([Value::I64(1), Value::I64(2)]);
        let mut bytes: LPooled<Vec<u8>> = LPooled::take();
        value.encode(&mut *bytes).unwrap();
        assert_eq!(decode_marked_data(&mut bytes.as_slice()).unwrap(), value);
        assert!(Value::decode(&mut bytes.as_slice()).is_err());
    }
}
