#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use graphix_compiler::{node::coretraits::ordered, rep};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use std::fmt::Debug;

fn fc_get(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) if ordered(key) => {
            Some(m.get(key).cloned().unwrap_or(Value::Null))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Get, GetEv, "map_get", fc_get);

fn fc_get_or(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, default) if ordered(key) => {
            Some(m.get(key).cloned().unwrap_or_else(|| default.clone()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(GetOr, GetOrEv, "map_get_or", fc_get_or);

fn fc_insert(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, value) if ordered(key) => {
            Some(Value::Map(m.insert(key.clone(), value.clone()).0))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Insert, InsertEv, "map_insert", fc_insert);

fn fc_remove(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) if ordered(key) => Some(Value::Map(m.remove(key).0)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Remove, RemoveEv, "map_remove", fc_remove);

/// A map's `(key, value)` pairs in key order. The cursor copies the pairs
/// when the map arrives and walks them by position: finding the next key
/// by a search would compare keys outside the core-trait hooks, in an
/// order other than the one a user `Ord` built the map in.
#[derive(Debug)]
struct MapElems;

impl graphix_package_core::Elements for MapElems {
    const ITER: &str = "map_iter";
    const ITERQ: &str = "map_iterq";
    type Cursor = (usize, ValArray);

    fn cursor(v: Value) -> Option<Self::Cursor> {
        match v {
            Value::Map(m) if m.len() > 0 => {
                let pair = |(k, v): (&Value, &Value)| rep::tuple([k.clone(), v.clone()]);
                Some((0, graphix_compiler::valarray_of(m.len(), m.into_iter().map(pair))))
            }
            _ => None,
        }
    }

    fn next((i, pairs): &mut Self::Cursor) -> Option<Value> {
        let pair = pairs.get(*i)?.clone();
        *i += 1;
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
