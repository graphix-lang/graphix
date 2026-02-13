use anyhow::{bail, Result};
use arcstr::ArcStr;
use fxhash::FxHashMap;
use graphix_compiler::{
    expr::ExprId,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use graphix_package::{CustomDisplay, Package};
use graphix_package_core::{
    deftype, CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapCollection, MapFn,
    MapQ, Slot,
};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::collections::VecDeque;
use std::fmt::Debug;
use triomphe::Arc as TArc;

#[derive(Debug, Default)]
struct MapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapImpl {
    type Collection = CMap<Value, Value, 32>;

    const NAME: &str = "map_map";
    deftype!(
        "fn(Map<'a, 'b>, fn(('a, 'b)) -> ('c, 'd) throws 'e) -> Map<'c, 'd> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], _: &Self::Collection) -> Option<Value> {
        Some(Value::Map(CMap::from_iter(
            slots
                .iter()
                .map(|s| s.cur.clone().unwrap().cast_to::<(Value, Value)>().unwrap()),
        )))
    }
}

type Map<R, E> = MapQ<R, E, MapImpl>;

#[derive(Debug, Default)]
struct FilterImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterImpl {
    type Collection = CMap<Value, Value, 32>;

    const NAME: &str = "map_filter";
    deftype!("fn(Map<'a, 'b>, fn(('a, 'b)) -> bool throws 'e) -> Map<'a, 'b> throws 'e");

    fn finish(
        &mut self,
        slots: &[Slot<R, E>],
        m: &CMap<Value, Value, 32>,
    ) -> Option<Value> {
        Some(Value::Map(CMap::from_iter(slots.iter().zip(m.into_iter()).filter_map(
            |(p, (k, v))| match p.cur {
                Some(Value::Bool(true)) => Some((k.clone(), v.clone())),
                _ => None,
            },
        ))))
    }
}

type Filter<R, E> = MapQ<R, E, FilterImpl>;

#[derive(Debug, Default)]
struct FilterMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterMapImpl {
    type Collection = CMap<Value, Value, 32>;

    const NAME: &str = "map_filter_map";
    deftype!(
        "fn(Map<'a, 'b>, fn(('a, 'b)) -> Option<('c, 'd)> throws 'e) -> Map<'c, 'd> throws 'e"
    );

    fn finish(
        &mut self,
        slots: &[Slot<R, E>],
        _: &CMap<Value, Value, 32>,
    ) -> Option<Value> {
        Some(Value::Map(CMap::from_iter(slots.iter().filter_map(|s| {
            match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone().cast_to::<(Value, Value)>().unwrap()),
            }
        }))))
    }
}

type FilterMap<R, E> = MapQ<R, E, FilterMapImpl>;

#[derive(Debug)]
struct FoldImpl;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for FoldImpl {
    type Collection = CMap<Value, Value, 32>;

    const NAME: &str = "map_fold";
    deftype!("fn(Map<'a, 'b>, 'c, fn('c, ('a, 'b)) -> 'c throws 'e) -> 'c throws 'e");
}

type Fold<R, E> = FoldQ<R, E, FoldImpl>;

#[derive(Debug, Default)]
struct LenEv;

impl EvalCached for LenEv {
    const NAME: &str = "map_len";
    deftype!("fn(Map<'a, 'b>) -> i64");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Map(m)) => Some(Value::I64(m.len() as i64)),
            Some(_) | None => None,
        }
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct GetEv;

impl EvalCached for GetEv {
    const NAME: &str = "map_get";
    deftype!("fn(Map<'a, 'b>, 'a) -> Option<'b>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Map(m)), Some(key)) => {
                Some(m.get(key).cloned().unwrap_or(Value::Null))
            }
            _ => None,
        }
    }
}

type Get = CachedArgs<GetEv>;

#[derive(Debug, Default)]
struct InsertEv;

impl EvalCached for InsertEv {
    const NAME: &str = "map_insert";
    deftype!("fn(Map<'a, 'b>, 'a, 'b) -> Map<'a, 'b>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1], &from.0[2]) {
            (Some(Value::Map(m)), Some(key), Some(value)) => {
                Some(Value::Map(m.insert(key.clone(), value.clone()).0))
            }
            _ => None,
        }
    }
}

type Insert = CachedArgs<InsertEv>;

#[derive(Debug, Default)]
struct RemoveEv;

impl EvalCached for RemoveEv {
    const NAME: &str = "map_remove";
    deftype!("fn(Map<'a, 'b>, 'a) -> Map<'a, 'b>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Map(m)), Some(key)) => Some(Value::Map(m.remove(key).0)),
            _ => None,
        }
    }
}

type Remove = CachedArgs<RemoveEv>;

#[derive(Debug)]
struct Iter {
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Iter {
    const NAME: &str = "map_iter";
    deftype!("fn(Map<'a, 'b>) -> ('a, 'b)");

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(Self { id, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(Value::Map(m)) = from[0].update(ctx, event) {
            for (k, v) in m.into_iter() {
                let pair = Value::Array(ValArray::from_iter_exact(
                    [k.clone(), v.clone()].into_iter(),
                ));
                ctx.rt.set_var(self.id, pair);
            }
        }
        event.variables.get(&self.id).map(|v| v.clone())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
struct IterQ {
    triggered: usize,
    queue: VecDeque<(usize, LPooled<Vec<(Value, Value)>>)>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IterQ {
    const NAME: &str = "map_iterq";
    deftype!("fn(#clock:Any, Map<'a, 'b>) -> ('a, 'b)");

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(IterQ { triggered: 0, queue: VecDeque::new(), id, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(Value::Map(m)) = from[1].update(ctx, event) {
            let pairs: LPooled<Vec<(Value, Value)>> =
                m.into_iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            if !pairs.is_empty() {
                self.queue.push_back((0, pairs));
            }
        }
        while self.triggered > 0 && !self.queue.is_empty() {
            let (i, pairs) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < pairs.len() {
                let (k, v) = pairs[*i].clone();
                let pair = Value::Array(ValArray::from_iter_exact([k, v].into_iter()));
                ctx.rt.set_var(self.id, pair);
                *i += 1;
                self.triggered -= 1;
            }
            if *i == pairs.len() {
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
        ctx.rt.ref_var(self.id, self.top_id);
        self.queue.clear();
        self.triggered = 0;
    }
}

graphix_derive::defpackage! {
    builtins => [
        Map as Map<GXRt<X>, X::UserEvent>,
        Filter as Filter<GXRt<X>, X::UserEvent>,
        FilterMap as FilterMap<GXRt<X>, X::UserEvent>,
        Fold as Fold<GXRt<X>, X::UserEvent>,
        Len,
        Get,
        Insert,
        Remove,
        Iter,
        IterQ,
    ],
}
