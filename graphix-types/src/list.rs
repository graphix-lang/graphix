//! The native `List` rep: a cell is an array of the list marker, its
//! head and its tail; nil is the marker alone, one value per thread (not
//! `Null`: `[List, null]` must not collapse, and not the empty array: an
//! empty list is no array). Only this module and `rep.rs` know the
//! layout; a list's data form is a flat array.
use crate::rep::{self, Kind};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;

thread_local! {
    static NIL: Value = rep::build(Kind::List, []);
}

pub fn nil() -> Value {
    NIL.with(Value::clone)
}

pub fn cons(head: Value, tail: Value) -> Value {
    rep::build(Kind::List, [head, tail])
}

pub fn split(v: &Value) -> Option<(&Value, &Value)> {
    match rep::marked(v, Kind::List)? {
        [head, tail] => Some((head, tail)),
        _ => None,
    }
}

/// The heads of the list `v`, borrowed from it.
pub fn heads(v: &Value) -> impl Iterator<Item = &Value> {
    let mut cur = v;
    std::iter::from_fn(move || {
        let (head, tail) = split(cur)?;
        cur = tail;
        Some(head)
    })
}

pub fn is_nil(v: &Value) -> bool {
    rep::marked(v, Kind::List).is_some_and(|p| p.is_empty())
}

/// Walk one cell per item of `items`, handing each item its head:
/// the rest of the list after them, or `None` when the list is
/// shorter or `f` refuses a head.
pub fn zip_prefix<'a, T>(
    v: &'a Value,
    items: &[T],
    mut f: impl FnMut(&T, &'a Value) -> bool,
) -> Option<&'a Value> {
    let mut cur = v;
    for item in items {
        let (h, t) = split(cur)?;
        if !f(item, h) {
            return None;
        }
        cur = t;
    }
    Some(cur)
}

pub fn is_list(v: &Value) -> bool {
    is_nil(v) || split(v).is_some()
}

/// The length of a well-formed list; `None` when the spine is not
/// one (a shape the outer pair alone cannot tell).
pub fn len(v: &Value) -> Option<usize> {
    let mut n = 0;
    let mut cur = v;
    loop {
        if is_nil(cur) {
            return Some(n);
        }
        let (_, tail) = split(cur)?;
        n += 1;
        cur = tail;
    }
}

pub fn from_iter(iter: impl IntoIterator<Item = Value>) -> Value {
    let mut values: LPooled<Vec<Value>> = iter.into_iter().collect();
    cons_all(&mut values, nil())
}

/// `values`, front to back, consed onto `rest`; `values` is left empty.
pub fn cons_all(values: &mut Vec<Value>, rest: Value) -> Value {
    let mut result = rest;
    while let Some(value) = values.pop() {
        result = cons(value, result);
    }
    result
}

#[derive(Debug, Clone)]
pub struct Iter {
    cur: Value,
}

impl Iter {
    pub fn new(value: Value) -> Self {
        Self { cur: value }
    }
}

impl Iterator for Iter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        // a non-list leaves Null behind, which ends the walk the same way
        let cur = std::mem::replace(&mut self.cur, Value::Null);
        let (head, tail) = split(&cur)?;
        self.cur = tail.clone();
        Some(head.clone())
    }
}

pub fn to_array(value: &Value) -> Option<ValArray> {
    Some(crate::valarray_of(len(value)?, Iter::new(value.clone())))
}
