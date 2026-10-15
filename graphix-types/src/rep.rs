//! The runtime forms of a struct and a variant; only this module knows
//! them. A struct is an array of `[name, value]` fields sorted by name. A
//! variant is its tag, a string, when it has no payload, else an array of
//! its tag and then its payload.
use arcstr::ArcStr;
use netidx_value::{ValArray, Value};

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

/// A variant's tag and payload, empty for a bare tag.
pub fn variant(v: &Value) -> Option<(&ArcStr, &[Value])> {
    match v {
        Value::String(tag) => Some((tag, &[])),
        Value::Array(a) => match &a[..] {
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
