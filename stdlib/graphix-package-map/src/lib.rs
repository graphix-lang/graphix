#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use netidx::subscriber::Value;
use netidx_value::ValArray;
use std::fmt::Debug;

fn fc_get(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) => Some(m.get(key).cloned().unwrap_or(Value::Null)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Get, GetEv, "map_get", fc_get);

fn fc_get_or(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, default) => {
            Some(m.get(key).cloned().unwrap_or_else(|| default.clone()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(GetOr, GetOrEv, "map_get_or", fc_get_or);

fn fc_insert(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, value) => {
            Some(Value::Map(m.insert(key.clone(), value.clone()).0))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Insert, InsertEv, "map_insert", fc_insert);

fn fc_remove(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) => Some(Value::Map(m.remove(key).0)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Remove, RemoveEv, "map_remove", fc_remove);

/// A map's `(key, value)` pairs in key order; the cursor holds the map and
/// the last key it gave, so a queued map is never copied.
#[derive(Debug)]
struct MapElems;

impl graphix_package_core::Elements for MapElems {
    const ITER: &str = "map_iter";
    const ITERQ: &str = "map_iterq";
    type Cursor = (immutable_chunkmap::map::Map<Value, Value, 32>, Option<Value>);

    fn cursor(v: Value) -> Option<Self::Cursor> {
        match v {
            Value::Map(m) if m.len() > 0 => Some((m, None)),
            _ => None,
        }
    }

    fn next((m, last): &mut Self::Cursor) -> Option<Value> {
        use std::ops::Bound::{Excluded, Unbounded};
        let (k, v) = match last {
            None => m.into_iter().next()?,
            Some(l) => m.range::<Value, _>((Excluded(&*l), Unbounded)).next()?,
        };
        let pair =
            Value::Array(ValArray::from_iter_exact([k.clone(), v.clone()].into_iter()));
        *last = Some(k.clone());
        Some(pair)
    }
}

type Iter = graphix_package_core::Iter<MapElems>;
type IterQ = graphix_package_core::IterQ<MapElems>;

graphix_derive::defpackage! {
    builtins => [
        Get,
        GetOr,
        Insert,
        Remove,
        Iter,
        IterQ,
    ],
}
