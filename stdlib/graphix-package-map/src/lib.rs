#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Refs, Rt, Scope, UserEvent,
    effects::EffectKind,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapFn, MapQ, Slot,
};
use graphix_rt::GXRt;
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug};

#[derive(Debug, Default)]
struct MapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapImpl {
    type Collection = CMap<Value, Value, 32>;

    const NAME: &str = "map_map";

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
}

type Fold<R, E> = FoldQ<R, E, FoldImpl>;

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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The queue and trigger debt are semantic buffering; delivery
        // rides set_var (async, so never inside a sync frame anyway).
    }
}

#[derive(Debug)]
struct Change<R: Rt, E: UserEvent> {
    inner: Node<R, E>,
    fid: BindId,
    x: BindId,
    last_m: Option<CMap<Value, Value, 32>>,
    last_k: Option<Value>,
    last_d: Option<Value>,
    last_f: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Change<R, E> {
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "map_change";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 4 {
            bail!("expected four arguments");
        }
        let typ = resolved.unwrap_or(typ);
        let ptyp = match &typ.args[3].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("expected a function not {t}"),
        };
        if ptyp.args.is_empty() {
            bail!("expected unary callback");
        }
        let x_typ = ptyp.args[0].typ.clone();
        let (x, xn) = genn::bind(ctx, &scope.lexical, "x", x_typ, top_id);
        let fid = BindId::new();
        let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
        let inner = genn::apply(fnode, scope.clone(), vec![xn], &ptyp, top_id);
        Ok(Box::new(Self {
            inner,
            fid,
            x,
            last_m: None,
            last_k: None,
            last_d: None,
            last_f: None,
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Change<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[3].update(ctx, event) {
            ctx.rt.cached_mut().insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let mut changed = false;
        if let Some(Value::Map(m)) = from[0].update(ctx, event) {
            self.last_m = Some(m);
            changed = true;
        }
        if let Some(k) = from[1].update(ctx, event) {
            self.last_k = Some(k);
            changed = true;
        }
        if let Some(d) = from[2].update(ctx, event) {
            self.last_d = Some(d);
            changed = true;
        }
        if changed {
            if let (Some(m), Some(k), Some(d)) =
                (&self.last_m, &self.last_k, &self.last_d)
            {
                let current = m.get(k).cloned().unwrap_or_else(|| d.clone());
                ctx.rt.cached_mut().insert(self.x, current.clone());
                event.variables.insert(self.x, current);
            }
        }
        let f_fired = if let Some(v) = self.inner.update(ctx, event) {
            self.last_f = Some(v);
            true
        } else {
            false
        };
        if changed || f_fired {
            if let (Some(m), Some(k), Some(fv)) =
                (&self.last_m, &self.last_k, &self.last_f)
            {
                return Some(Value::Map(m.insert(k.clone(), fv.clone()).0));
            }
        }
        None
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.inner.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.inner.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.cached_mut().remove(&self.fid);
        ctx.rt.cached_mut().remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.inner.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.last_m = None;
        self.last_k = None;
        self.last_d = None;
        self.last_f = None;
        self.inner.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The held last-m/k/d/f values and the published
        // callback-fn/element values are all per-invocation replay
        // memory (the same ids `delete` removes).
        self.last_m = None;
        self.last_k = None;
        self.last_d = None;
        self.last_f = None;
        ctx.rt.cached_mut().remove(&self.fid);
        ctx.rt.cached_mut().remove(&self.x);
        self.inner.reset_replay(ctx);
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
        GetOr,
        Insert,
        Remove,
        Iter,
        IterQ,
        Change as Change<GXRt<X>, X::UserEvent>,
    ],
}
