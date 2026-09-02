#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, FastFn, Node, Rt, Scope, TagValue, UserEvent,
    effects::EffectKind,
    expr::ExprId,
    node::collection::list::{
        Iter as ListIter, cons as make_cons, from_iter as from_iter_back, is_list,
        is_nil, len as count_list, nil as make_nil, split as get_cons, to_array,
    },
    typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, fast_eval, seam_tick, sort_values,
};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug};

fn list_to_array(list: &Value) -> Option<Value> {
    to_array(list).map(Value::Array)
}

// ── EvalCached implementations ───────────────────────────────────

fn fc_nil(_args: &[Value]) -> Option<Value> {
    Some(make_nil())
}

#[derive(Debug, Default)]
struct NilEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NilEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_nil";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_nil);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_nil, from)
    }
}

type Nil = CachedArgs<NilEv>;

fn fc_cons(args: &[Value]) -> Option<Value> {
    Some(make_cons(args[0].clone(), args[1].clone()))
}

#[derive(Debug, Default)]
struct ConsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConsEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_cons";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_cons);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_cons, from)
    }
}

type Cons = CachedArgs<ConsEv>;

fn fc_singleton(args: &[Value]) -> Option<Value> {
    Some(make_cons(args[0].clone(), make_nil()))
}

#[derive(Debug, Default)]
struct SingletonEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SingletonEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_singleton";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_singleton);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_singleton, from)
    }
}

type Singleton = CachedArgs<SingletonEv>;

fn fc_head(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((head, _)) => Some(head.clone()),
        None => Some(Value::Null),
    }
}

#[derive(Debug, Default)]
struct HeadEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HeadEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_head";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_head);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_head, from)
    }
}

type Head = CachedArgs<HeadEv>;

fn fc_tail(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((_, tail)) => Some(tail.clone()),
        None => Some(Value::Null),
    }
}

#[derive(Debug, Default)]
struct TailEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TailEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_tail";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_tail);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_tail, from)
    }
}

type Tail = CachedArgs<TailEv>;

fn fc_uncons(args: &[Value]) -> Option<Value> {
    match get_cons(&args[0]) {
        Some((head, tail)) => Some(Value::Array(ValArray::from_iter_exact(
            [head.clone(), tail.clone()].into_iter(),
        ))),
        None => Some(Value::Null),
    }
}

#[derive(Debug, Default)]
struct UnconsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnconsEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_uncons";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_uncons);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_uncons, from)
    }
}

type Uncons = CachedArgs<UnconsEv>;

fn fc_is_empty(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(is_nil(&args[0])))
}

#[derive(Debug, Default)]
struct IsEmptyEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsEmptyEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_is_empty";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_is_empty);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_is_empty, from)
    }
}

type IsEmpty = CachedArgs<IsEmptyEv>;

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

#[derive(Debug, Default)]
struct NthEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NthEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_nth";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_nth);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_nth, from)
    }
}

type Nth = CachedArgs<NthEv>;

fn fc_len(args: &[Value]) -> Option<Value> {
    Some(Value::I64(count_list(&args[0])? as i64))
}

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_len";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_len);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_len, from)
    }
}

type Len = CachedArgs<LenEv>;

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

#[derive(Debug, Default)]
struct ReverseEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ReverseEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_reverse";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_reverse);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_reverse, from)
    }
}

type Reverse = CachedArgs<ReverseEv>;

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

#[derive(Debug, Default)]
struct TakeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TakeEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_take";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_take);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_take, from)
    }
}

type Take = CachedArgs<TakeEv>;

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

#[derive(Debug, Default)]
struct DropEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DropEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_drop";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_drop);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_drop, from)
    }
}

type Drop_ = CachedArgs<DropEv>;

fn fc_to_array(args: &[Value]) -> Option<Value> {
    list_to_array(&args[0])
}

#[derive(Debug, Default)]
struct ToArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ToArrayEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_to_array";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_to_array);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_to_array, from)
    }
}

type ToArray = CachedArgs<ToArrayEv>;

/// The list's elements as an array in REVERSE order, in one walk: the
/// finish for a front-to-back accumulator that consed as it went.
fn fc_to_array_rev(args: &[Value]) -> Option<Value> {
    let a = to_array(&args[0])?;
    Some(Value::Array(ValArray::from_iter_exact(a.iter().rev().cloned())))
}

#[derive(Debug, Default)]
struct ToArrayRevEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ToArrayRevEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_to_array_rev";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_to_array_rev);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_to_array_rev, from)
    }
}

type ToArrayRev = CachedArgs<ToArrayRevEv>;

fn fc_from_array(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => Some(from_iter_back(a.iter().cloned())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct FromArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FromArrayEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_from_array";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_from_array);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_from_array, from)
    }
}

type FromArray = CachedArgs<FromArrayEv>;

fn fc_concat(args: &[Value]) -> Option<Value> {
    let mut buf: LPooled<Vec<Value>> = LPooled::take();
    for l in args {
        if !is_list(l) {
            return None;
        }
        buf.extend(ListIter::new(l.clone()));
    }
    Some(from_iter_back(buf.drain(..)))
}

#[derive(Debug, Default)]
struct ConcatEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConcatEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_concat";
    const FASTCALL: Option<FastFn> = Some(fc_concat);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_concat, from)
    }
}

type Concat = CachedArgs<ConcatEv>;

fn fc_flatten(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    let mut buf: LPooled<Vec<Value>> =
        ListIter::new(list.clone()).flat_map(ListIter::new).collect();
    Some(from_iter_back(buf.drain(..)))
}

#[derive(Debug, Default)]
struct FlattenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FlattenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_flatten";
    const FASTCALL: Option<FastFn> = Some(fc_flatten);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_flatten, from)
    }
}

type Flatten = CachedArgs<FlattenEv>;

fn fc_sort(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(dir), Value::Bool(numeric), list] if is_list(list) => {
            let mut sorted = sort_values(dir, *numeric, ListIter::new(list.clone()))?;
            Some(from_iter_back(sorted.drain(..)))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct SortEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SortEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_sort";
    const FASTCALL: Option<FastFn> = Some(fc_sort);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_sort, from)
    }
}

type Sort = CachedArgs<SortEv>;

fn fc_enumerate(args: &[Value]) -> Option<Value> {
    let list = &args[0];
    if !is_list(list) {
        return None;
    }
    Some(from_iter_back(
        ListIter::new(list.clone()).enumerate().map(|(i, v)| (i as i64, v).into()),
    ))
}

#[derive(Debug, Default)]
struct EnumerateEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EnumerateEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_enumerate";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_enumerate);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_enumerate, from)
    }
}

type Enumerate_ = CachedArgs<EnumerateEv>;

fn fc_zip(args: &[Value]) -> Option<Value> {
    let (l0, l1) = (&args[0], &args[1]);
    if !is_list(l0) || !is_list(l1) {
        return None;
    }
    Some(from_iter_back(
        ListIter::new(l0.clone()).zip(ListIter::new(l1.clone())).map(|p| p.into()),
    ))
}

#[derive(Debug, Default)]
struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_zip";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_zip);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_zip, from)
    }
}

type Zip = CachedArgs<ZipEv>;

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

#[derive(Debug, Default)]
struct UnzipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_unzip";
    const FASTCALL: Option<FastFn> = Some(fc_unzip);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_unzip, from)
    }
}

type Unzip = CachedArgs<UnzipEv>;

// ── Custom BuiltIn/Apply implementations ─────────────────────────

#[derive(Debug)]
struct ListIterBI(BindId, ExprId, TagValue);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ListIterBI {
    const NAME: &str = "list_iter";

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
        Ok(Box::new(ListIterBI(id, top_id, TagValue::phantom())))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ListIterBI {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(list) =
            seam_tick(from[0].update(ctx, event)).map(|tv| tv.value_cloned())
        {
            for v in ListIter::new(list) {
                ctx.rt.set_var(self.0, v);
            }
        }
        let res = event.variables.get(&self.0).map(|tv| tv.value_cloned());
        match res {
            Some(v) => self.2.set(TagValue::fired(v)),
            None => self.2.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1);
        self.0 = BindId::new();
        ctx.rt.ref_var(self.0, self.1);
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Delivery rides set_var (async); the wake registration is
        // sleep's business, never reset_replay's.
    }
}

#[derive(Debug)]
struct ListIterQ {
    triggered: usize,
    queue: VecDeque<(usize, Vec<Value>)>,
    id: BindId,
    top_id: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ListIterQ {
    const NAME: &str = "list_iterq";

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
        Ok(Box::new(ListIterQ {
            triggered: 0,
            queue: VecDeque::new(),
            id,
            top_id,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ListIterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx, event)).is_some() {
            self.triggered += 1;
        }
        if let Some(list) =
            seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
        {
            if is_list(&list) {
                let elems: Vec<Value> = ListIter::new(list).collect();
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
        let res = event.variables.get(&self.id).map(|tv| tv.value_cloned());
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The queue and trigger debt are semantic buffering (async
        // delivery) — sleep's clearing is the arm-rewake restart.
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
