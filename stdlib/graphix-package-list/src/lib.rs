#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::literal;
use compact_str::format_compact;
use graphix_compiler::{
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope,
    TypecheckPhase, UserEvent,
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapCollection, MapFn, MapQ, Slot,
};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value};
use netidx_value::ValArray;
use smallvec::SmallVec;
use std::{collections::hash_map::Entry, collections::VecDeque, fmt::Debug};
use triomphe::Arc as TArc;

// ── Value-level list helpers ─────────────────────────────────────

fn make_nil() -> Value {
    Value::String(literal!("Nil"))
}

fn make_cons(head: Value, tail: Value) -> Value {
    Value::Array(ValArray::from_iter_exact(
        [Value::String(literal!("Cons")), head, tail].into_iter(),
    ))
}

/// Extract head and tail from a Cons cell, or None if Nil/invalid.
fn get_cons(v: &Value) -> Option<(&Value, &Value)> {
    match v {
        Value::Array(a) if a.len() == 3 => match &a[0] {
            Value::String(s) if &**s == "Cons" => Some((&a[1], &a[2])),
            _ => None,
        },
        _ => None,
    }
}

fn is_nil(v: &Value) -> bool {
    matches!(v, Value::String(s) if &**s == "Nil")
}

fn is_list(v: &Value) -> bool {
    is_nil(v) || get_cons(v).is_some()
}

/// Count the number of elements in a list value.
fn count_list(v: &Value) -> Option<usize> {
    let mut len = 0;
    let mut cur = v.clone();
    loop {
        if is_nil(&cur) {
            return Some(len);
        }
        match get_cons(&cur) {
            Some((_, tail)) => {
                len += 1;
                cur = tail.clone();
            }
            None => return None,
        }
    }
}

/// Build a list from an iterator by collecting to a buffer and folding
/// right with cons. O(n) time, O(n) temporary space via SmallVec.
fn from_iter_back(iter: impl Iterator<Item = Value>) -> Value {
    let mut buf: SmallVec<[Value; 32]> = iter.collect();
    let mut result = make_nil();
    while let Some(v) = buf.pop() {
        result = make_cons(v, result);
    }
    result
}

/// Iterator over list elements. Clones the Arc inside each Value::Array
/// on each step (O(1) per step).
struct ListIter {
    cur: Value,
}

impl Iterator for ListIter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let cur = self.cur.clone();
        match get_cons(&cur) {
            Some((head, tail)) => {
                let head = head.clone();
                self.cur = tail.clone();
                Some(head)
            }
            None => None,
        }
    }
}

// ── MapCollection for lists ──────────────────────────────────────

/// Thin wrapper used by MapQ/FoldQ. Not stored in Values — only lives
/// inside the reactive node machinery.
#[derive(Debug, Clone)]
struct ListColl {
    value: Value,
    len: usize,
}

impl Default for ListColl {
    fn default() -> Self {
        Self { value: make_nil(), len: 0 }
    }
}

impl MapCollection for ListColl {
    fn len(&self) -> usize {
        self.len
    }

    fn iter_values(&self) -> impl Iterator<Item = Value> {
        ListIter { cur: self.value.clone() }
    }

    fn select(v: Value) -> Option<Self> {
        let len = count_list(&v)?;
        Some(ListColl { value: v, len })
    }

    fn project(self) -> Value {
        self.value
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        // When called from outside the module the first arg is
        // Type::Abstract { params: [elem], .. }
        if let Type::Abstract { params, .. } = &ft.args[0].typ {
            if !params.is_empty() {
                return Ok(params[0].clone());
            }
        }
        // Inside the module the abstract type is resolved, so fall
        // back to extracting the element type from the last argument
        // of whichever function argument we can find (map: fn('a)->...,
        // fold: fn('b,'a)->..., filter: fn('a)->bool, etc.). The list
        // element type is always the last parameter of that function.
        for arg in ft.args.iter() {
            if let Type::Fn(inner) = &arg.typ {
                if let Some(last) = inner.args.last() {
                    return Ok(last.typ.clone());
                }
            }
        }
        bail!("cannot extract list element type from {:?}", ft.args[0].typ)
    }
}

// ── MapFn implementations ────────────────────────────────────────

#[derive(Debug, Default)]
struct ListMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListMapImpl {
    type Collection = ListColl;
    const NAME: &str = "list_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListColl) -> Option<Value> {
        Some(from_iter_back(slots.iter().map(|s| s.cur.clone().unwrap())))
    }
}

type ListMap<R, E> = MapQ<R, E, ListMapImpl>;

#[derive(Debug, Default)]
struct ListFilterImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFilterImpl {
    type Collection = ListColl;
    const NAME: &str = "list_filter";

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ListColl) -> Option<Value> {
        Some(from_iter_back(
            slots.iter().zip(ListIter { cur: a.value.clone() }).filter_map(|(p, v)| {
                match p.cur {
                    Some(Value::Bool(true)) => Some(v),
                    _ => None,
                }
            }),
        ))
    }
}

type ListFilter<R, E> = MapQ<R, E, ListFilterImpl>;

#[derive(Debug, Default)]
struct ListFilterMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFilterMapImpl {
    type Collection = ListColl;
    const NAME: &str = "list_filter_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListColl) -> Option<Value> {
        Some(from_iter_back(slots.iter().filter_map(|s| match s.cur.as_ref().unwrap() {
            Value::Null => None,
            v => Some(v.clone()),
        })))
    }
}

type ListFilterMap<R, E> = MapQ<R, E, ListFilterMapImpl>;

#[derive(Debug, Default)]
struct ListFlatMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFlatMapImpl {
    type Collection = ListColl;
    const NAME: &str = "list_flat_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListColl) -> Option<Value> {
        Some(from_iter_back(slots.iter().flat_map(|s| {
            let v = s.cur.as_ref().unwrap();
            if is_list(v) {
                let items: SmallVec<[Value; 32]> = ListIter { cur: v.clone() }.collect();
                items.into_iter()
            } else {
                let mut one: SmallVec<[Value; 32]> = SmallVec::new();
                one.push(v.clone());
                one.into_iter()
            }
        })))
    }
}

type ListFlatMap<R, E> = MapQ<R, E, ListFlatMapImpl>;

#[derive(Debug, Default)]
struct ListFindImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFindImpl {
    type Collection = ListColl;
    const NAME: &str = "list_find";

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ListColl) -> Option<Value> {
        let r = slots
            .iter()
            .zip(ListIter { cur: a.value.clone() })
            .find(|(s, _)| matches!(s.cur.as_ref(), Some(Value::Bool(true))))
            .map(|(_, v)| v)
            .unwrap_or(Value::Null);
        Some(r)
    }
}

type ListFind<R, E> = MapQ<R, E, ListFindImpl>;

#[derive(Debug, Default)]
struct ListFindMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFindMapImpl {
    type Collection = ListColl;
    const NAME: &str = "list_find_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListColl) -> Option<Value> {
        let r = slots
            .iter()
            .find_map(|s| match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone()),
            })
            .unwrap_or(Value::Null);
        Some(r)
    }
}

type ListFindMap<R, E> = MapQ<R, E, ListFindMapImpl>;

// ── FoldFn implementation ────────────────────────────────────────

#[derive(Debug)]
struct ListFoldImpl;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for ListFoldImpl {
    type Collection = ListColl;
    const NAME: &str = "list_fold";
}

type ListFold<R, E> = FoldQ<R, E, ListFoldImpl>;

// ── EvalCached implementations ───────────────────────────────────

#[derive(Debug, Default)]
struct NilEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NilEv {
    const NAME: &str = "list_nil";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        from.0[0].as_ref()?;
        Some(make_nil())
    }
}

type Nil = CachedArgs<NilEv>;

#[derive(Debug, Default)]
struct ConsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConsEv {
    const NAME: &str = "list_cons";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let head = from.0[0].as_ref()?;
        let tail = from.0[1].as_ref()?;
        Some(make_cons(head.clone(), tail.clone()))
    }
}

type Cons = CachedArgs<ConsEv>;

#[derive(Debug, Default)]
struct SingletonEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SingletonEv {
    const NAME: &str = "list_singleton";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let v = from.0[0].as_ref()?;
        Some(make_cons(v.clone(), make_nil()))
    }
}

type Singleton = CachedArgs<SingletonEv>;

#[derive(Debug, Default)]
struct HeadEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HeadEv {
    const NAME: &str = "list_head";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        match get_cons(list) {
            Some((head, _)) => Some(head.clone()),
            None => Some(Value::Null),
        }
    }
}

type Head = CachedArgs<HeadEv>;

#[derive(Debug, Default)]
struct TailEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TailEv {
    const NAME: &str = "list_tail";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        match get_cons(list) {
            Some((_, tail)) => Some(tail.clone()),
            None => Some(Value::Null),
        }
    }
}

type Tail = CachedArgs<TailEv>;

#[derive(Debug, Default)]
struct UnconsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnconsEv {
    const NAME: &str = "list_uncons";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        match get_cons(list) {
            Some((head, tail)) => Some(Value::Array(ValArray::from_iter_exact(
                [head.clone(), tail.clone()].into_iter(),
            ))),
            None => Some(Value::Null),
        }
    }
}

type Uncons = CachedArgs<UnconsEv>;

#[derive(Debug, Default)]
struct IsEmptyEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsEmptyEv {
    const NAME: &str = "list_is_empty";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        Some(Value::Bool(is_nil(list)))
    }
}

type IsEmpty = CachedArgs<IsEmptyEv>;

#[derive(Debug, Default)]
struct NthEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NthEv {
    const NAME: &str = "list_nth";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        let n = match from.0[1].as_ref()? {
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
}

type Nth = CachedArgs<NthEv>;

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const NAME: &str = "list_len";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        Some(Value::I64(count_list(list)? as i64))
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct ReverseEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ReverseEv {
    const NAME: &str = "list_reverse";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        let mut result = make_nil();
        for v in (ListIter { cur: list.clone() }) {
            result = make_cons(v, result);
        }
        Some(result)
    }
}

type Reverse = CachedArgs<ReverseEv>;

#[derive(Debug, Default)]
struct TakeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TakeEv {
    const NAME: &str = "list_take";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let n = match from.0[0].as_ref()? {
            Value::I64(n) => (*n).max(0) as usize,
            _ => return None,
        };
        let list = from.0[1].as_ref()?;
        if !is_list(list) {
            return None;
        }
        Some(from_iter_back(ListIter { cur: list.clone() }.take(n)))
    }
}

type Take = CachedArgs<TakeEv>;

#[derive(Debug, Default)]
struct DropEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DropEv {
    const NAME: &str = "list_drop";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let n = match from.0[0].as_ref()? {
            Value::I64(n) => (*n).max(0) as usize,
            _ => return None,
        };
        let list = from.0[1].as_ref()?;
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
}

type Drop_ = CachedArgs<DropEv>;

#[derive(Debug, Default)]
struct ToArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ToArrayEv {
    const NAME: &str = "list_to_array";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        Some(Value::Array(ValArray::from_iter(ListIter { cur: list.clone() })))
    }
}

type ToArray = CachedArgs<ToArrayEv>;

#[derive(Debug, Default)]
struct FromArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FromArrayEv {
    const NAME: &str = "list_from_array";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match from.0[0].as_ref()? {
            Value::Array(a) => Some(from_iter_back(a.iter().cloned())),
            _ => None,
        }
    }
}

type FromArray = CachedArgs<FromArrayEv>;

#[derive(Debug, Default)]
struct ConcatEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConcatEv {
    const NAME: &str = "list_concat";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        // Collect all lists into a flat buffer, then build from back.
        // This handles variadic concat: concat(l1, l2, l3, ...) = l1 ++ l2 ++ l3 ++ ...
        let mut present = true;
        for v in from.0.iter() {
            match v {
                Some(v) if is_list(v) => {
                    self.0.extend(ListIter { cur: v.clone() });
                }
                _ => present = false,
            }
        }
        if present {
            let result = from_iter_back(self.0.drain(..));
            Some(result)
        } else {
            self.0.clear();
            None
        }
    }
}

type Concat = CachedArgs<ConcatEv>;

#[derive(Debug, Default)]
struct FlattenEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FlattenEv {
    const NAME: &str = "list_flatten";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        for inner in (ListIter { cur: list.clone() }) {
            self.0.extend(ListIter { cur: inner });
        }
        let result = from_iter_back(self.0.drain(..));
        Some(result)
    }
}

type Flatten = CachedArgs<FlattenEv>;

#[derive(Debug, Default)]
struct SortEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SortEv {
    const NAME: &str = "list_sort";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fn cn(v: &Value) -> Value {
            v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
        }
        match &from.0[..] {
            [Some(Value::String(dir)), Some(Value::Bool(numeric)), Some(list)]
                if is_list(list) =>
            {
                match &**dir {
                    "Ascending" => {
                        self.0.extend(ListIter { cur: list.clone() });
                        if *numeric {
                            self.0.sort_by(|v0, v1| cn(v0).cmp(&cn(v1)))
                        } else {
                            self.0.sort();
                        }
                        Some(from_iter_back(self.0.drain(..)))
                    }
                    "Descending" => {
                        self.0.extend(ListIter { cur: list.clone() });
                        if *numeric {
                            self.0.sort_by(|a0, a1| cn(a1).cmp(&cn(a0)))
                        } else {
                            self.0.sort_by(|a0, a1| a1.cmp(a0));
                        }
                        Some(from_iter_back(self.0.drain(..)))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

type Sort = CachedArgs<SortEv>;

#[derive(Debug, Default)]
struct EnumerateEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EnumerateEv {
    const NAME: &str = "list_enumerate";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        Some(from_iter_back(
            ListIter { cur: list.clone() }.enumerate().map(|(i, v)| (i, v).into()),
        ))
    }
}

type Enumerate_ = CachedArgs<EnumerateEv>;

#[derive(Debug, Default)]
struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const NAME: &str = "list_zip";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let l0 = from.0[0].as_ref()?;
        let l1 = from.0[1].as_ref()?;
        if !is_list(l0) || !is_list(l1) {
            return None;
        }
        Some(from_iter_back(
            ListIter { cur: l0.clone() }
                .zip(ListIter { cur: l1.clone() })
                .map(|p| p.into()),
        ))
    }
}

type Zip = CachedArgs<ZipEv>;

#[derive(Debug, Default)]
struct UnzipEv {
    t0: SmallVec<[Value; 32]>,
    t1: SmallVec<[Value; 32]>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const NAME: &str = "list_unzip";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        for v in (ListIter { cur: list.clone() }) {
            if let Value::Array(a) = v {
                if a.len() == 2 {
                    self.t0.push(a[0].clone());
                    self.t1.push(a[1].clone());
                }
            }
        }
        let v0 = from_iter_back(self.t0.drain(..));
        let v1 = from_iter_back(self.t1.drain(..));
        Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
    }
}

type Unzip = CachedArgs<UnzipEv>;

// ── Custom BuiltIn/Apply implementations ─────────────────────────

#[derive(Debug)]
struct ListIterBI(BindId, ExprId);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ListIterBI {
    const NAME: &str = "list_iter";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(ListIterBI(id, top_id)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ListIterBI {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(list) = from[0].update(ctx, event) {
            for v in (ListIter { cur: list }) {
                ctx.rt.set_var(self.0, v);
            }
        }
        event.variables.get(&self.0).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1);
        self.0 = BindId::new();
        ctx.rt.ref_var(self.0, self.1);
    }
}

#[derive(Debug)]
struct ListIterQ {
    triggered: usize,
    queue: VecDeque<(usize, Vec<Value>)>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ListIterQ {
    const NAME: &str = "list_iterq";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(ListIterQ { triggered: 0, queue: VecDeque::new(), id, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ListIterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(list) = from[1].update(ctx, event) {
            if is_list(&list) {
                let elems: Vec<Value> = ListIter { cur: list }.collect();
                if !elems.is_empty() {
                    self.queue.push_back((0, elems));
                }
            }
        }
        while self.triggered > 0 && !self.queue.is_empty() {
            let (i, elems) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < elems.len() {
                ctx.rt.set_var(self.id, elems[*i].clone());
                *i += 1;
                self.triggered -= 1;
            }
            if *i == elems.len() {
                self.queue.pop_front();
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        self.queue.clear();
        self.triggered = 0;
    }
}

#[derive(Debug)]
struct ListInit<R: Rt, E: UserEvent> {
    scope: Scope,
    fid: BindId,
    top_id: ExprId,
    mftyp: TArc<FnType>,
    slots: Vec<Slot<R, E>>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ListInit<R, E> {
    const NAME: &str = "list_init";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                Ok(Box::new(Self {
                    scope: scope
                        .append(&format_compact!("fn{}", LambdaId::new().inner())),
                    fid: BindId::new(),
                    top_id,
                    mftyp: match &typ.args[1].typ {
                        Type::Fn(ft) => ft.clone(),
                        t => bail!("expected a function not {t}"),
                    },
                    slots: vec![],
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ListInit<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let slen = self.slots.len();
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let (size_fired, resized) = match from[0].update(ctx, event) {
            Some(Value::I64(n)) => {
                let n = n.max(0) as usize;
                if n == slen {
                    (true, false)
                } else if n < slen {
                    while self.slots.len() > n {
                        if let Some(mut s) = self.slots.pop() {
                            s.delete(ctx)
                        }
                    }
                    (true, true)
                } else {
                    let i_typ = Type::Primitive(Typ::I64.into());
                    while self.slots.len() < n {
                        let i = self.slots.len();
                        let (id, node) = genn::bind(
                            ctx,
                            &self.scope.lexical,
                            "i",
                            i_typ.clone(),
                            self.top_id,
                        );
                        ctx.cached.insert(id, Value::I64(i as i64));
                        let fnode = genn::reference(
                            ctx,
                            self.fid,
                            Type::Fn(self.mftyp.clone()),
                            self.top_id,
                        );
                        let pred = genn::apply(
                            fnode,
                            self.scope.clone(),
                            vec![node],
                            &self.mftyp,
                            self.top_id,
                        );
                        self.slots.push(Slot { id, pred, cur: None });
                    }
                    (true, true)
                }
            }
            _ => (false, false),
        };
        if resized && self.slots.len() > slen {
            for i in slen..self.slots.len() {
                let id = self.slots[i].id;
                event.variables.insert(id, Value::I64(i as i64));
            }
        }
        if size_fired && self.slots.is_empty() {
            return Some(make_nil());
        }
        let init = event.init;
        let mut up = resized;
        for (i, s) in self.slots.iter_mut().enumerate() {
            if i == slen {
                event.init = true;
                if let Entry::Vacant(e) = event.variables.entry(self.fid)
                    && let Some(v) = ctx.cached.get(&self.fid)
                {
                    e.insert(v.clone());
                }
            }
            if let Some(v) = s.pred.update(ctx, event) {
                s.cur = Some(v);
                up = true;
            }
        }
        event.init = init;
        if up && self.slots.iter().all(|s| s.cur.is_some()) {
            Some(from_iter_back(self.slots.iter().map(|s| s.cur.clone().unwrap())))
        } else {
            None
        }
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: TypecheckPhase,
    ) -> anyhow::Result<()> {
        let i_typ = Type::Primitive(Typ::I64.into());
        let (_, node) = genn::bind(ctx, &self.scope.lexical, "i", i_typ, self.top_id);
        let ft = self.mftyp.clone();
        let fnode = genn::reference(ctx, self.fid, Type::Fn(ft.clone()), self.top_id);
        let mut node =
            genn::apply(fnode, self.scope.clone(), vec![node], &ft, self.top_id);
        let r = node.typecheck(ctx);
        node.delete(ctx);
        r?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        for s in &self.slots {
            s.pred.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
        for sl in &mut self.slots {
            sl.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for sl in &mut self.slots {
            sl.cur = None;
            sl.pred.sleep(ctx);
        }
    }
}

// ── Package registration ─────────────────────────────────────────

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
        ListFilter as ListFilter<GXRt<X>, X::UserEvent>,
        ListFilterMap as ListFilterMap<GXRt<X>, X::UserEvent>,
        ListFind as ListFind<GXRt<X>, X::UserEvent>,
        ListFindMap as ListFindMap<GXRt<X>, X::UserEvent>,
        ListFlatMap as ListFlatMap<GXRt<X>, X::UserEvent>,
        ListFold as ListFold<GXRt<X>, X::UserEvent>,
        ListInit as ListInit<GXRt<X>, X::UserEvent>,
        ListIterBI,
        ListIterQ,
        ListMap as ListMap<GXRt<X>, X::UserEvent>,
        Nil,
        Nth,
        Reverse,
        Singleton,
        Sort,
        Tail,
        Take,
        ToArray,
        Uncons,
        Unzip,
        Zip,
    ],
}
