#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, FastCall, Node, Rt, Scope, TagValue,
    UserEvent, effects::Effect, expr::ExprId, typ::FnType,
};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, seam_tick};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug};

fn fc_get(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) => Some(m.get(key).cloned().unwrap_or(Value::Null)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct GetEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for GetEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_get)));
    const NAME: &str = "map_get";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_get, from)
    }
}

type Get = CachedArgs<GetEv>;

fn fc_get_or(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, default) => {
            Some(m.get(key).cloned().unwrap_or_else(|| default.clone()))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct GetOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for GetOrEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_get_or)));
    const NAME: &str = "map_get_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_get_or, from)
    }
}

type GetOr = CachedArgs<GetOrEv>;

fn fc_insert(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Map(m), key, value) => {
            Some(Value::Map(m.insert(key.clone(), value.clone()).0))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct InsertEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for InsertEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_insert)));
    const NAME: &str = "map_insert";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_insert, from)
    }
}

type Insert = CachedArgs<InsertEv>;

fn fc_remove(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(m), key) => Some(Value::Map(m.remove(key).0)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct RemoveEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for RemoveEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_remove)));
    const NAME: &str = "map_remove";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_remove, from)
    }
}

type Remove = CachedArgs<RemoveEv>;

#[derive(Debug)]
struct Iter {
    id: BindId,
    top_id: ExprId,
    out: TagValue,
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
        Ok(Box::new(Self { id, top_id, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(Value::Map(m)) =
            seam_tick(from[0].update(ctx, event)).map(|tv| tv.value_cloned())
        {
            for (k, v) in m.into_iter() {
                let pair = Value::Array(ValArray::from_iter_exact(
                    [k.clone(), v.clone()].into_iter(),
                ));
                ctx.rt.set_var(self.id, pair);
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
    out: TagValue,
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
        Ok(Box::new(IterQ {
            triggered: 0,
            queue: VecDeque::new(),
            id,
            top_id,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx, event)).is_some() {
            self.triggered += 1;
        }
        if let Some(Value::Map(m)) =
            seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
        {
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
        Get,
        GetOr,
        Insert,
        Remove,
        Iter,
        IterQ,
    ],
}
