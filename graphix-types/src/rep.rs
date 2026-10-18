//! The runtime forms of a tuple, a struct and a variant; only this module
//! knows them (a list's are `list.rs`'s). A tuple is an array of its
//! elements. A struct is an array of `[name, value]` fields sorted by
//! name. A variant is its tag, a string, when it has no payload, else an
//! array of its tag and then its payload.
use arcstr::ArcStr;
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;

/// Where a tuple's first element sits in its array.
pub const TUPLE_BASE: usize = 0;

/// Where a struct's first field sits in its array.
pub const STRUCT_BASE: usize = 0;

/// Where a payload variant's tag sits in its array; its payload follows.
pub const VARIANT_TAG: usize = 0;

/// The tuple of `elems`.
pub fn tuple(
    elems: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    Value::Array(ValArray::from_iter_exact(elems.into_iter()))
}

/// A tuple's elements.
pub fn tuple_elems(v: &Value) -> Option<&[Value]> {
    match v {
        Value::Array(a) => Some(&a[TUPLE_BASE..]),
        _ => None,
    }
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
    tuple_elems(v).map(Parts::Held)
}

/// `v` read as a struct: a struct's fields, or an array of pairs.
pub fn read_struct(v: &Value) -> Option<Parts<'_>> {
    struct_fields(v).map(Parts::Held)
}

/// `v` read as a variant: its tag and payload, or a string's tag.
pub fn read_variant(v: &Value) -> Option<(&ArcStr, Parts<'_>)> {
    variant(v).map(|(tag, p)| (tag, Parts::Held(p)))
}

/// An array's elements: `None` for any other value, a tuple, struct or
/// variant included.
pub fn array_elems(v: &Value) -> Option<&ValArray> {
    match v {
        Value::Array(a) => Some(a),
        _ => None,
    }
}

/// The elements an index reads: a tuple's or an array's.
pub fn indexed(v: &Value) -> Option<&[Value]> {
    match v {
        Value::Array(a) => Some(&a[..]),
        _ => None,
    }
}

/// `v`, a tuple or an array, with the element at `i` replaced by `e`.
pub fn replace_indexed(v: &Value, i: usize, e: Value) -> Option<Value> {
    let Value::Array(a) = v else { return None };
    let mut e = Some(e);
    Some(Value::Array(ValArray::from_iter_exact(a.iter().enumerate().map(
        |(k, x)| match k == i {
            true => e.take().expect("one element replaced"),
            false => x.clone(),
        },
    ))))
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
    Value::Array(ValArray::from_iter_exact(
        fields.into_iter().map(|(name, v)| make_field(name, v)),
    ))
}

/// The struct of already-made fields ([`make_field`]), sorted by name.
pub fn structure_of_fields(
    fields: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    Value::Array(ValArray::from_iter_exact(fields.into_iter()))
}

/// A struct's fields, in sorted order; each is read with [`field`].
pub fn struct_fields(v: &Value) -> Option<&[Value]> {
    match v {
        Value::Array(a) => Some(&a[STRUCT_BASE..]),
        _ => None,
    }
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

/// The variant `tag` with `payload`: its bare tag when the payload is
/// empty.
pub fn make_variant(
    tag: ArcStr,
    payload: impl IntoIterator<Item = Value, IntoIter: ExactSizeIterator>,
) -> Value {
    let payload = payload.into_iter();
    if payload.len() == 0 {
        return Value::String(tag);
    }
    let mut a: LPooled<Vec<Value>> = LPooled::take();
    a.push(Value::String(tag));
    a.extend(payload);
    Value::Array(ValArray::from_iter_exact(a.drain(..)))
}

/// A variant's tag and payload, empty for a bare tag.
pub fn variant(v: &Value) -> Option<(&ArcStr, &[Value])> {
    match v {
        Value::String(tag) => Some((tag, &[])),
        Value::Array(a) => match &a[VARIANT_TAG..] {
            [Value::String(tag), payload @ ..] if !payload.is_empty() => {
                Some((tag, payload))
            }
            _ => None,
        },
        _ => None,
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
