//! The native `List` rep: cons = a 2-slot array `[head, tail]`, nil = a
//! clone of one static empty array (not `Null`: `[List, null]` must not
//! collapse). The discriminant is the length; only this module knows
//! the layout.
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::sync::LazyLock;

static EMPTY: LazyLock<ValArray> =
    LazyLock::new(|| ValArray::from_iter_exact(std::iter::empty()));

pub fn nil() -> Value {
    Value::Array(EMPTY.clone())
}

pub fn cons(head: Value, tail: Value) -> Value {
    Value::Array(ValArray::from_iter_exact([head, tail].into_iter()))
}

pub fn split(v: &Value) -> Option<(&Value, &Value)> {
    match v {
        Value::Array(a) if a.len() == 2 => Some((&a[0], &a[1])),
        _ => None,
    }
}

pub fn is_nil(v: &Value) -> bool {
    matches!(v, Value::Array(a) if a.is_empty())
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

// CR claude for claude: [perf/low] from_iter always collects into a pooled Vec before
// consing from the back. The list package's fc_flatten, fc_sort and fc_unzip already
// hold such a buffer and pass drain(..), so every element is copied through two
// vectors. fc_concat avoids that by hand-rolling the same cons-from-back fold, a copy
// of this function. Add a variant that conses from the back of a caller's
// LPooled<Vec<Value>> and use it in all four, and in from_iter itself. Iter::next
// clones the whole cursor and then the tail: take the cursor with mem::replace instead.
// to_array computes len and then discards it: build with ValArray::from_iter_exact over
// that length. (collections-str-07)
pub fn from_iter(iter: impl IntoIterator<Item = Value>) -> Value {
    let mut values: LPooled<Vec<Value>> = iter.into_iter().collect();
    let mut result = nil();
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
        // CR claude for claude: [perf/low] Cloning the cursor here costs an atomic
        // increment and decrement on every list cell walked. It exists only to satisfy
        // the borrow checker, and this is the per-element path for list map/fold slots,
        // list casts and `to_array`. Take the cursor instead: `let cur =
        // std::mem::replace(&mut self.cur, Value::Null);`, then split it, clone the
        // head and the tail, and store the tail. A non-list leaves `Null`, which ends
        // the iteration the same way. (t-expr-core-11)
        let cur = self.cur.clone();
        let (head, tail) = split(&cur)?;
        let head = head.clone();
        self.cur = tail.clone();
        Some(head)
    }
}

pub fn to_array(value: &Value) -> Option<ValArray> {
    len(value).map(|_| ValArray::from_iter(Iter::new(value.clone())))
}
