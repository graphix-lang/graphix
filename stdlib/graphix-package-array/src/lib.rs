#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::{Result, bail};
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue,
    UserEvent,
    effects::EffectKind,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{collections::VecDeque, fmt::Debug};

#[derive(Debug, Default)]
struct ConcatEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConcatEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_concat";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        for v in from.0.iter() {
            match v {
                Some(Value::Array(a)) => {
                    for v in a.iter() {
                        self.0.push(v.clone())
                    }
                }
                Some(v) => self.0.push(v.clone()),
                None => present = false,
            }
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type Concat = CachedArgs<ConcatEv>;

#[derive(Debug, Default)]
struct PushBackEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PushBackEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_push_back";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                self.0.extend(a.iter().map(|v| v.clone()));
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type PushBack = CachedArgs<PushBackEv>;

#[derive(Debug, Default)]
struct PushFrontEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PushFrontEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_push_front";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
                self.0.extend(a.iter().map(|v| v.clone()));
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type PushFront = CachedArgs<PushFrontEv>;

#[derive(Debug, Default)]
struct WindowEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for WindowEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_window";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        // window requires ALL its args before producing anything
        // (Eric's ruling 2026-07-20, dyncall-partial-args-jul2026):
        // the old per-branch presence tracking let the degenerate
        // `#n: 0` case emit `[]` with the val slot still absent (the
        // window needed zero elements from it).
        match &from.0[..] {
            [Some(Value::I64(window)), Some(Value::Array(a)), tl @ ..]
                if tl.iter().all(|v| v.is_some()) =>
            {
                let window = *window as usize;
                let total = a.len() + tl.len();
                let tl_vals = tl.iter().map(|v| v.clone().unwrap());
                if total <= window {
                    self.0.extend(a.iter().cloned());
                    self.0.extend(tl_vals);
                } else if a.len() >= (total - window) {
                    self.0.extend(a[(total - window)..].iter().cloned());
                    self.0.extend(tl_vals);
                } else {
                    self.0.extend(tl_vals.skip(tl.len() - window));
                }
                let a = ValArray::from_iter_exact(self.0.drain(..));
                Some(Value::Array(a))
            }
            _ => {
                self.0.clear();
                None
            }
        }
    }
}

type Window = CachedArgs<WindowEv>;

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => Some(Value::I64(a.len() as i64)),
            Some(_) | None => None,
        }
    }

    // `array::len(arr)` lowers to a pure array-length read (a
    // length read off the array kernel input) — no DynCall needed. Only
    // fuses when the array arg resolves to a kernel input; otherwise
    // `None` falls back to DynCall.
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct FlattenEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FlattenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_flatten";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => {
                for v in a.iter() {
                    match v {
                        Value::Array(a) => self.0.extend(a.iter().map(|v| v.clone())),
                        v => self.0.push(v.clone()),
                    }
                }
                let a = ValArray::from_iter_exact(self.0.drain(..));
                Some(Value::Array(a))
            }
            Some(_) | None => None,
        }
    }
}

type Flatten = CachedArgs<FlattenEv>;

#[derive(Debug, Default)]
struct SortEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SortEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_sort";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fn cn(v: &Value) -> Value {
            v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
        }
        match &from.0[..] {
            [
                Some(Value::String(dir)),
                Some(Value::Bool(numeric)),
                Some(Value::Array(a)),
            ] => match &**dir {
                "Ascending" => {
                    self.0.extend(a.iter().cloned());
                    if *numeric {
                        self.0.sort_by(|v0, v1| cn(v0).cmp(&cn(v1)))
                    } else {
                        self.0.sort();
                    }
                    Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                }
                "Descending" => {
                    self.0.extend(a.iter().cloned());
                    if *numeric {
                        self.0.sort_by(|a0, a1| cn(a1).cmp(&cn(a0)))
                    } else {
                        self.0.sort_by(|a0, a1| a1.cmp(a0));
                    }
                    Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

type Sort = CachedArgs<SortEv>;

#[derive(Debug, Default)]
struct DedupEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DedupEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_dedup";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => {
                let mut seen: LPooled<AHashSet<Value>> = LPooled::take();
                for v in a.iter() {
                    if !seen.contains(v) {
                        seen.insert(v.clone());
                        self.0.push(v.clone());
                    }
                }
                Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
            }
            Some(_) | None => None,
        }
    }
}

type Dedup = CachedArgs<DedupEv>;

#[derive(Debug, Default)]
struct EnumerateEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EnumerateEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_enumerate";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::Array(a)) = &from.0[0] {
            let a = ValArray::from_iter_exact(
                a.iter().enumerate().map(|(i, v)| (i as i64, v.clone()).into()),
            );
            return Some(Value::Array(a));
        }
        None
    }
}

type Enumerate = CachedArgs<EnumerateEv>;

#[derive(Debug, Default)]
struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_zip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a0)), Some(Value::Array(a1))] => {
                Some(Value::Array(ValArray::from_iter_exact(
                    a0.iter().cloned().zip(a1.iter().cloned()).map(|p| p.into()),
                )))
            }
            _ => None,
        }
    }
}

type Zip = CachedArgs<ZipEv>;

#[derive(Debug, Default)]
struct UnzipEv {
    t0: Vec<Value>,
    t1: Vec<Value>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_unzip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a))] => {
                for v in a {
                    if let Value::Array(a) = v {
                        match &a[..] {
                            [v0, v1] => {
                                self.t0.push(v0.clone());
                                self.t1.push(v1.clone());
                            }
                            _ => (),
                        }
                    }
                }
                let v0 = Value::Array(ValArray::from_iter_exact(self.t0.drain(..)));
                let v1 = Value::Array(ValArray::from_iter_exact(self.t1.drain(..)));
                Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
            }
            _ => None,
        }
    }
}

type Unzip = CachedArgs<UnzipEv>;

#[derive(Debug)]
struct Group<R: Rt, E: UserEvent> {
    queue: VecDeque<Value>,
    buf: SmallVec<[Value; 16]>,
    pred: Node<R, E>,
    ready: bool,
    pid: BindId,
    nid: BindId,
    xid: BindId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Group<R, E> {
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_group";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
                let n_typ = Type::Primitive(Typ::I64.into());
                let etyp = typ.args[0].typ.clone();
                let mftyp = match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let (nid, n) =
                    genn::bind(ctx, &scope.lexical, "n", n_typ.clone(), top_id);
                let (xid, x) = genn::bind(ctx, &scope.lexical, "x", etyp.clone(), top_id);
                let pid = BindId::new();
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let pred = genn::apply(fnode, scope, vec![n, x], &mftyp, top_id);
                Ok(Box::new(Self {
                    queue: VecDeque::new(),
                    buf: smallvec![],
                    pred,
                    ready: true,
                    pid,
                    nid,
                    xid,
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Group<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                self.buf.push($v.clone());
                let len = Value::I64(self.buf.len() as i64);
                ctx.rt.cached_mut().insert(self.nid, len.clone());
                event.variables.insert(self.nid, TagValue::fired(len));
                ctx.rt.cached_mut().insert(self.xid, $v.clone());
                event.variables.insert(self.xid, TagValue::fired($v));
            }};
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.queue.push_back(v.value());
        }
        if let Some(v) = from[1].update(ctx, event) {
            let v = v.value();
            ctx.rt.cached_mut().insert(self.pid, v.clone());
            event.variables.insert(self.pid, TagValue::fired(v));
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.pop_front().unwrap();
            set!(v);
        }
        loop {
            // Cooperative interrupt: abort a wedged grouping loop.
            if ctx.interrupted() {
                break None;
            }
            match self.pred.update(ctx, event) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    match v.value() {
                        Value::Bool(true) => {
                            break Some(Value::Array(ValArray::from_iter_exact(
                                self.buf.drain(..),
                            )));
                        }
                        _ => match self.queue.pop_front() {
                            None => break None,
                            Some(v) => set!(v),
                        },
                    }
                }
            }
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.cached_mut().remove(&self.nid);
        ctx.rt.cached_mut().remove(&self.pid);
        ctx.rt.cached_mut().remove(&self.xid);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The published pred-fn/length/element values are
        // per-invocation replay memory (the same ids `delete`
        // removes); the queue, the group buffer, and the ready flag
        // are the grouping contract — they aggregate across events
        // and survive.
        ctx.rt.cached_mut().remove(&self.nid);
        ctx.rt.cached_mut().remove(&self.pid);
        ctx.rt.cached_mut().remove(&self.xid);
        self.pred.reset_replay(ctx);
    }
}

#[derive(Debug)]
struct Iter(BindId, ExprId);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Iter {
    const NAME: &str = "array_iter";

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
        Ok(Box::new(Iter(id, top_id)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(Value::Array(a)) = from[0].update(ctx, event).map(|tv| tv.value()) {
            for v in a.iter() {
                // Cooperative interrupt: abort a wedged iter over a huge
                // array (partial emit is accepted for a deliberate kill).
                if ctx.interrupted() {
                    return None;
                }
                ctx.rt.set_var(self.0, v.clone());
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
        // Delivery rides set_var (async); the only state is the wake
        // registration, which reset_replay never touches.
    }
}

#[derive(Debug)]
struct IterQ {
    triggered: usize,
    queue: VecDeque<(usize, ValArray)>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IterQ {
    const NAME: &str = "array_iterq";

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
        if let Some(Value::Array(a)) = from[1].update(ctx, event).map(|tv| tv.value()) {
            if a.len() > 0 {
                self.queue.push_back((0, a));
            }
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            let (i, a) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < a.len() {
                if ctx.interrupted() {
                    return None;
                }
                ctx.rt.set_var(self.id, a[*i].clone());
                *i += 1;
                self.triggered -= 1;
            }
            if *i == a.len() {
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
        // The queue and trigger debt are semantic buffering; delivery
        // rides set_var (async, so never inside a sync frame anyway).
    }
}

#[derive(Debug, Default)]
struct IotaEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IotaEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "array_iota";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::I64(n))] => {
                if *n > graphix_compiler::node::MAX_ARRAY_INIT_LEN {
                    log::error!(
                        "array::init: size {n} exceeds the {} element \
                         limit — producing no value",
                        graphix_compiler::node::MAX_ARRAY_INIT_LEN
                    );
                    return None;
                }
                let n = (*n).max(0) as usize;
                Some(Value::Array(ValArray::from_iter_exact(
                    (0..n).map(|i| Value::I64(i as i64)),
                )))
            }
            _ => None,
        }
    }
}

type Iota = CachedArgs<IotaEv>;

graphix_derive::defpackage! {
    builtins => [
        Concat,
        Dedup,
        Enumerate,
        Zip,
        Unzip,
        Flatten,
        Group as Group<GXRt<X>, X::UserEvent>,
        Iota,
        Iter,
        IterQ,
        Len,
        PushBack,
        PushFront,
        Sort,
        Window,
    ],
}
