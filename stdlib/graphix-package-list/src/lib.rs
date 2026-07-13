#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
    effects::EffectKind,
    expr::ExprId,
    node::collection::list::{
        Iter as ListIter, cons as make_cons, from_iter as from_iter_back, is_list,
        is_nil, len as count_list, nil as make_nil, split as get_cons, to_array,
    },
    typ::FnType,
};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use netidx::{publisher::Typ, subscriber::Value};
use netidx_value::ValArray;
use smallvec::SmallVec;
use std::{collections::VecDeque, fmt::Debug};

fn list_to_array(list: &Value) -> Option<Value> {
    to_array(list).map(Value::Array)
}

// ── EvalCached implementations ───────────────────────────────────

#[derive(Debug, Default)]
struct NilEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NilEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_nil";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        from.0[0].as_ref()?;
        Some(make_nil())
    }
}

type Nil = CachedArgs<NilEv>;

#[derive(Debug, Default)]
struct ConsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConsEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_cons";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_singleton";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let v = from.0[0].as_ref()?;
        Some(make_cons(v.clone(), make_nil()))
    }
}

type Singleton = CachedArgs<SingletonEv>;

#[derive(Debug, Default)]
struct HeadEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HeadEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_head";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_tail";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_uncons";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_is_empty";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        Some(Value::Bool(is_nil(list)))
    }
}

type IsEmpty = CachedArgs<IsEmptyEv>;

#[derive(Debug, Default)]
struct NthEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for NthEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_nth";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        Some(Value::I64(count_list(list)? as i64))
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct ReverseEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ReverseEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_reverse";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        let mut result = make_nil();
        for v in ListIter::new(list.clone()) {
            result = make_cons(v, result);
        }
        Some(result)
    }
}

type Reverse = CachedArgs<ReverseEv>;

#[derive(Debug, Default)]
struct TakeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TakeEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_take";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let n = match from.0[0].as_ref()? {
            Value::I64(n) => (*n).max(0) as usize,
            _ => return None,
        };
        let list = from.0[1].as_ref()?;
        if !is_list(list) {
            return None;
        }
        Some(from_iter_back(ListIter::new(list.clone()).take(n)))
    }
}

type Take = CachedArgs<TakeEv>;

#[derive(Debug, Default)]
struct DropEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DropEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_drop";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_to_array";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        list_to_array(list)
    }
}

type ToArray = CachedArgs<ToArrayEv>;

#[derive(Debug, Default)]
struct FromArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FromArrayEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_from_array";

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_concat";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        // Collect all lists into a flat buffer, then build from back.
        // This handles variadic concat: concat(l1, l2, l3, ...) = l1 ++ l2 ++ l3 ++ ...
        let mut present = true;
        for v in from.0.iter() {
            match v {
                Some(v) if is_list(v) => {
                    self.0.extend(ListIter::new(v.clone()));
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_flatten";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        for inner in ListIter::new(list.clone()) {
            self.0.extend(ListIter::new(inner));
        }
        let result = from_iter_back(self.0.drain(..));
        Some(result)
    }
}

type Flatten = CachedArgs<FlattenEv>;

#[derive(Debug, Default)]
struct SortEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SortEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_sort";

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
                        self.0.extend(ListIter::new(list.clone()));
                        if *numeric {
                            self.0.sort_by(|v0, v1| cn(v0).cmp(&cn(v1)))
                        } else {
                            self.0.sort();
                        }
                        Some(from_iter_back(self.0.drain(..)))
                    }
                    "Descending" => {
                        self.0.extend(ListIter::new(list.clone()));
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_enumerate";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        Some(from_iter_back(
            ListIter::new(list.clone()).enumerate().map(|(i, v)| (i as i64, v).into()),
        ))
    }
}

type Enumerate_ = CachedArgs<EnumerateEv>;

#[derive(Debug, Default)]
struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "list_zip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let l0 = from.0[0].as_ref()?;
        let l1 = from.0[1].as_ref()?;
        if !is_list(l0) || !is_list(l1) {
            return None;
        }
        Some(from_iter_back(
            ListIter::new(l0.clone()).zip(ListIter::new(l1.clone())).map(|p| p.into()),
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "list_unzip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let list = from.0[0].as_ref()?;
        if !is_list(list) {
            return None;
        }
        for v in ListIter::new(list.clone()) {
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
            for v in ListIter::new(list.value()) {
                ctx.rt.set_var(self.0, v);
            }
        }
        event.variables.get(&self.0).map(|tv| tv.value_cloned())
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
        if let Some(list) = from[1].update(ctx, event).map(|tv| tv.value()) {
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
        event.variables.get(&self.id).map(|tv| tv.value_cloned())
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
        Uncons,
        Unzip,
        Zip,
    ],
}
