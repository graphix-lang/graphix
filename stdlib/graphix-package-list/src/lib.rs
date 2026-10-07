#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use graphix_compiler::node::list::{
    Iter as ListIter, cons as make_cons, from_iter as from_iter_back, is_list, is_nil,
    len as count_list, nil as make_nil, split as get_cons, to_array,
};
use graphix_package_core::sort_values;
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::fmt::Debug;

fn list_to_array(list: &Value) -> Option<Value> {
    to_array(list).map(Value::Array)
}

fn fc_nil(_args: &[Value]) -> Option<Value> {
    Some(make_nil())
}

graphix_package_core::fast_builtin!(Nil, NilEv, "list_nil", fc_nil);

fn fc_cons(args: &[Value]) -> Option<Value> {
    Some(make_cons(args[0].clone(), args[1].clone()))
}

graphix_package_core::fast_builtin!(Cons, ConsEv, "list_cons", fc_cons);

fn fc_singleton(args: &[Value]) -> Option<Value> {
    Some(make_cons(args[0].clone(), make_nil()))
}

graphix_package_core::fast_builtin!(
    Singleton,
    SingletonEv,
    "list_singleton",
    fc_singleton
);

fn fc_head(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((head, _)) => Some(head.clone()),
        None => Some(Value::Null),
    }
}

graphix_package_core::fast_builtin!(Head, HeadEv, "list_head", fc_head);

fn fc_tail(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((_, tail)) => Some(tail.clone()),
        None => Some(Value::Null),
    }
}

graphix_package_core::fast_builtin!(Tail, TailEv, "list_tail", fc_tail);

fn fc_uncons(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((head, tail)) => Some(Value::Array(ValArray::from_iter_exact(
            [head.clone(), tail.clone()].into_iter(),
        ))),
        None => Some(Value::Null),
    }
}

graphix_package_core::fast_builtin!(Uncons, UnconsEv, "list_uncons", fc_uncons);

fn fc_is_empty(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(is_nil(&args[0])))
}

graphix_package_core::fast_builtin!(IsEmpty, IsEmptyEv, "list_is_empty", fc_is_empty);

fn fc_nth(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    let n = match &args[1] {
        Value::I64(n) => *n,
        _ => return None,
    };
    if n < 0 {
        return Some(Value::Null);
    }
    let mut cur = list.clone();
    for _ in 0..n {
        match get_cons(&cur) {
            Some((_, tail)) => cur = tail.clone(),
            None => return Some(Value::Null),
        }
    }
    match get_cons(&cur) {
        Some((head, _)) => Some(head.clone()),
        None => Some(Value::Null),
    }
}

graphix_package_core::fast_builtin!(Nth, NthEv, "list_nth", fc_nth);

fn fc_len(args: &[Value]) -> Option<Value> {
    Some(Value::I64(count_list(&args[0])? as i64))
}

graphix_package_core::fast_builtin!(Len, LenEv, "list_len", fc_len);

fn fc_reverse(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    let mut result = make_nil();
    for v in ListIter::new(list.clone()) {
        result = make_cons(v, result);
    }
    Some(result)
}

graphix_package_core::fast_builtin!(Reverse, ReverseEv, "list_reverse", fc_reverse);

fn fc_take(args: &[Value]) -> Option<Value> {
    let n = match &args[0] {
        Value::I64(n) => (*n).max(0) as usize,
        _ => return None,
    };
    let list = &args[1];
    if !is_list(list) {
        return None;
    }
    Some(from_iter_back(ListIter::new(list.clone()).take(n)))
}

graphix_package_core::fast_builtin!(Take, TakeEv, "list_take", fc_take);

fn fc_drop(args: &[Value]) -> Option<Value> {
    let n = match &args[0] {
        Value::I64(n) => (*n).max(0) as usize,
        _ => return None,
    };
    let list = &args[1];
    if !is_list(list) {
        return None;
    }
    let mut cur = list.clone();
    for _ in 0..n {
        match get_cons(&cur) {
            Some((_, tail)) => cur = tail.clone(),
            None => return Some(make_nil()),
        }
    }
    Some(cur)
}

graphix_package_core::fast_builtin!(Drop_, DropEv, "list_drop", fc_drop);

fn fc_to_array(args: &[Value]) -> Option<Value> {
    list_to_array(&args[0])
}

graphix_package_core::fast_builtin!(ToArray, ToArrayEv, "list_to_array", fc_to_array);

/// The list's elements as an array in REVERSE order, in one walk: the
/// finish for a front-to-back accumulator that consed as it went.
fn fc_to_array_rev(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    let mut buf: LPooled<Vec<Value>> = ListIter::new(list.clone()).collect();
    Some(Value::Array(ValArray::from_iter_exact(buf.drain(..).rev())))
}

graphix_package_core::fast_builtin!(
    ToArrayRev,
    ToArrayRevEv,
    "list_to_array_rev",
    fc_to_array_rev
);

fn fc_from_array(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => Some(from_iter_back(a.iter().cloned())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    FromArray,
    FromArrayEv,
    "list_from_array",
    fc_from_array
);

/// The lists in order; the last is shared, the others' elements consed
/// onto it from the back, so the cost is the size of all but the last.
fn fc_concat(args: &[Value]) -> Option<Value> {
    let (last, front) = args.split_last()?;
    if !args.iter().all(is_list) {
        return None;
    }
    let mut buf: LPooled<Vec<Value>> = LPooled::take();
    for l in front {
        buf.extend(ListIter::new(l.clone()));
    }
    Some(buf.drain(..).rev().fold(last.clone(), |rest, v| make_cons(v, rest)))
}

graphix_package_core::fast_builtin!(Concat, ConcatEv, "list_concat", fc_concat);

fn fc_flatten(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    let mut buf: LPooled<Vec<Value>> =
        ListIter::new(list.clone()).flat_map(ListIter::new).collect();
    Some(from_iter_back(buf.drain(..)))
}

graphix_package_core::fast_builtin!(Flatten, FlattenEv, "list_flatten", fc_flatten);

fn fc_sort(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(dir), Value::Bool(numeric), list] if is_list(list) => {
            let mut sorted = sort_values(dir, *numeric, ListIter::new(list.clone()))?;
            Some(from_iter_back(sorted.drain(..)))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Sort, SortEv, "list_sort", fc_sort);

fn fc_enumerate(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    Some(from_iter_back(
        ListIter::new(list.clone()).enumerate().map(|(i, v)| (i as i64, v).into()),
    ))
}

graphix_package_core::fast_builtin!(
    Enumerate_,
    EnumerateEv,
    "list_enumerate",
    fc_enumerate
);

fn fc_zip(args: &[Value]) -> Option<Value> {
    let (l0, l1) = (&args[0], &args[1]);
    if !is_list(l0) || !is_list(l1) {
        return None;
    }
    Some(from_iter_back(
        ListIter::new(l0.clone()).zip(ListIter::new(l1.clone())).map(|p| p.into()),
    ))
}

graphix_package_core::fast_builtin!(Zip, ZipEv, "list_zip", fc_zip);

fn fc_unzip(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    let mut t0: LPooled<Vec<Value>> = LPooled::take();
    let mut t1: LPooled<Vec<Value>> = LPooled::take();
    for v in ListIter::new(list.clone()) {
        if let Value::Array(a) = v
            && a.len() == 2
        {
            t0.push(a[0].clone());
            t1.push(a[1].clone());
        }
    }
    let v0 = from_iter_back(t0.drain(..));
    let v1 = from_iter_back(t1.drain(..));
    Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
}

graphix_package_core::fast_builtin!(Unzip, UnzipEv, "list_unzip", fc_unzip);

/// A list's elements, front to back; the cursor is the rest of the
/// list, so a queued list is never copied.
#[derive(Debug)]
struct ListElems;

impl graphix_package_core::Elements for ListElems {
    const ITER: &str = "list_iter";
    const ITERQ: &str = "list_iterq";
    type Cursor = Value;

    fn cursor(v: Value) -> Option<Value> {
        (is_list(&v) && !is_nil(&v)).then_some(v)
    }

    fn next(rest: &mut Value) -> Option<Value> {
        let (head, tail) = get_cons(rest).map(|(h, t)| (h.clone(), t.clone()))?;
        *rest = tail;
        Some(head)
    }
}

type ListIterBI = graphix_package_core::Iter<ListElems>;
type ListIterQ = graphix_package_core::IterQ<ListElems>;

graphix_derive::defpackage! {
    builtins => [
        Concat,
        Cons,
        Drop_ as Drop_,
        Enumerate_ as Enumerate_,
        Flatten,
        FromArray,
        Head,
        IsEmpty,
        Len,
        ListIterBI,
        ListIterQ,
        Nil,
        Nth,
        Reverse,
        Singleton,
        Sort,
        Tail,
        Take,
        ToArray,
        ToArrayRev,
        Uncons,
        Unzip,
        Zip,
    ],
}
