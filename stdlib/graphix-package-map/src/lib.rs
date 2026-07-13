#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
    effects::EffectKind, expr::ExprId, typ::FnType,
};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug};

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "map_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Map(m)) => Some(Value::I64(m.len() as i64)),
            Some(_) | None => None,
        }
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct GetEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for GetEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "map_get";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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
struct GetOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for GetOrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "map_get_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1], &from.0[2]) {
            (Some(Value::Map(m)), Some(key), Some(default)) => {
                Some(m.get(key).cloned().unwrap_or_else(|| default.clone()))
            }
            _ => None,
        }
    }
}

type GetOr = CachedArgs<GetOrEv>;

#[derive(Debug, Default)]
struct InsertEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for InsertEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "map_insert";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for RemoveEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "map_remove";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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
        if let Some(Value::Map(m)) = from[0].update(ctx, event).map(|tv| tv.value()) {
            for (k, v) in m.into_iter() {
                let pair = Value::Array(ValArray::from_iter_exact(
                    [k.clone(), v.clone()].into_iter(),
                ));
                ctx.rt.set_var(self.id, pair);
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
        ctx.rt.ref_var(self.id, self.top_id);
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Delivery rides set_var (async); the only state is the wake
        // registration, which reset_replay never touches.
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
        if let Some(Value::Map(m)) = from[1].update(ctx, event).map(|tv| tv.value()) {
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
        event.variables.get(&self.id).map(|tv| tv.value_cloned())
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The queue and trigger debt are semantic buffering; delivery
        // rides set_var (async, so never inside a sync frame anyway).
    }
}

graphix_derive::defpackage! {
    builtins => [
        Len,
        Get,
        GetOr,
        Insert,
        Remove,
        Iter,
        IterQ,
    ],
}
