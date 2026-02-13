use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use fxhash::FxHashMap;
use graphix_compiler::{
    env::Env,
    err, errf,
    expr::{Expr, ExprId},
    node::genn,
    typ::{FnType, TVal, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, UserEvent,
};
use graphix_package::{CustomDisplay, Package};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_core::utils::Either;
use netidx_value::{FromValue, ValArray};
use poolshark::local::LPooled;
use std::{
    any::Any,
    collections::{hash_map::Entry, VecDeque},
    fmt::Debug,
    iter,
    marker::PhantomData,
    sync::LazyLock,
    time::Duration,
};
use tokio::{sync::oneshot, time::Instant};
use triomphe::Arc as TArc;

// ── Shared macros ──────────────────────────────────────────────────

#[macro_export]
macro_rules! deftype {
    ($s:literal) => {
        const TYP: ::std::sync::LazyLock<graphix_compiler::typ::FnType> =
            ::std::sync::LazyLock::new(|| {
                graphix_compiler::expr::parser::parse_fn_type($s)
                    .expect("failed to parse fn type {s}")
            });
    };
}

#[macro_export]
macro_rules! arity1 {
    ($from:expr, $updates:expr) => {
        match (&*$from, &*$updates) {
            ([arg], [arg_up]) => (arg, arg_up),
            (_, _) => unreachable!(),
        }
    };
}

#[macro_export]
macro_rules! arity2 {
    ($from:expr, $updates:expr) => {
        match (&*$from, &*$updates) {
            ([arg0, arg1], [arg0_up, arg1_up]) => ((arg0, arg1), (arg0_up, arg1_up)),
            (_, _) => unreachable!(),
        }
    };
}

// ── Shared traits and structs ──────────────────────────────────────

#[derive(Debug)]
pub struct CachedVals(pub Box<[Option<Value>]>);

impl CachedVals {
    pub fn new<R: Rt, E: UserEvent>(from: &[Node<R, E>]) -> CachedVals {
        CachedVals(from.into_iter().map(|_| None).collect())
    }

    pub fn clear(&mut self) {
        for v in &mut self.0 {
            *v = None
        }
    }

    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> bool {
        from.into_iter().enumerate().fold(false, |res, (i, src)| {
            match src.update(ctx, event) {
                None => res,
                v @ Some(_) => {
                    self.0[i] = v;
                    true
                }
            }
        })
    }

    /// Like update, but return the indexes of the nodes that updated
    /// instead of a consolidated bool
    pub fn update_diff<R: Rt, E: UserEvent>(
        &mut self,
        up: &mut [bool],
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) {
        for (i, n) in from.iter_mut().enumerate() {
            match n.update(ctx, event) {
                None => (),
                v => {
                    self.0[i] = v;
                    up[i] = true
                }
            }
        }
    }

    pub fn flat_iter<'a>(&'a self) -> impl Iterator<Item = Option<Value>> + 'a {
        self.0.iter().flat_map(|v| match v {
            None => Either::Left(iter::once(None)),
            Some(v) => Either::Right(v.clone().flatten().map(Some)),
        })
    }

    pub fn get<T: FromValue>(&self, i: usize) -> Option<T> {
        self.0.get(i).and_then(|v| v.as_ref()).and_then(|v| v.clone().cast_to::<T>().ok())
    }
}

pub trait EvalCached: Debug + Default + Send + Sync + 'static {
    const NAME: &str;
    const TYP: LazyLock<FnType>;

    fn eval(&mut self, from: &CachedVals) -> Option<Value>;
}

#[derive(Debug)]
pub struct CachedArgs<T: EvalCached> {
    cached: CachedVals,
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCached> BuiltIn<R, E> for CachedArgs<T> {
    const NAME: &str = T::NAME;
    const TYP: LazyLock<FnType> = T::TYP;

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let t = CachedArgs::<T> { cached: CachedVals::new(from), t: T::default() };
        Ok(Box::new(t))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCached> Apply<R, E> for CachedArgs<T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            self.t.eval(&self.cached)
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.cached.clear()
    }
}

pub trait EvalCachedAsync: Debug + Default + Send + Sync + 'static {
    const NAME: &str;
    const TYP: LazyLock<FnType>;
    type Args: Debug + Any + Send + Sync;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args>;
    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send;
}

#[derive(Debug)]
pub struct CachedArgsAsync<T: EvalCachedAsync> {
    cached: CachedVals,
    id: BindId,
    top_id: ExprId,
    queued: VecDeque<T::Args>,
    running: bool,
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> BuiltIn<R, E> for CachedArgsAsync<T> {
    const NAME: &str = T::NAME;
    const TYP: LazyLock<FnType> = T::TYP;

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let t = CachedArgsAsync::<T> {
            id,
            top_id,
            cached: CachedVals::new(from),
            queued: VecDeque::new(),
            running: false,
            t: T::default(),
        };
        Ok(Box::new(t))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> Apply<R, E> for CachedArgsAsync<T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event)
            && let Some(args) = self.t.prepare_args(&self.cached)
        {
            self.queued.push_back(args);
        }
        let res = event.variables.remove(&self.id).map(|v| {
            self.running = false;
            v
        });
        if !self.running
            && let Some(args) = self.queued.pop_front()
        {
            self.running = true;
            let id = self.id;
            ctx.rt.spawn_var(async move { (id, T::eval(args).await) });
        }
        res
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.queued.clear();
        self.cached.clear();
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.delete(ctx);
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
    }
}

pub trait MapCollection: Debug + Clone + Default + Send + Sync + 'static {
    /// return the length of the collection
    fn len(&self) -> usize;

    /// iterate the collection elements as values
    fn iter_values(&self) -> impl Iterator<Item = Value>;

    /// given a value, return Some if the value is the collection type
    /// we are mapping.
    fn select(v: Value) -> Option<Self>;

    /// given a collection wrap it in a value
    fn project(self) -> Value;

    /// return the element type given the function type
    fn etyp(ft: &FnType) -> Result<Type>;
}

impl MapCollection for ValArray {
    fn iter_values(&self) -> impl Iterator<Item = Value> {
        (**self).iter().cloned()
    }

    fn len(&self) -> usize {
        (**self).len()
    }

    fn select(v: Value) -> Option<Self> {
        match v {
            Value::Array(a) => Some(a.clone()),
            _ => None,
        }
    }

    fn project(self) -> Value {
        Value::Array(self)
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Array(et) => Ok((**et).clone()),
            _ => bail!("expected array"),
        }
    }
}

impl MapCollection for CMap<Value, Value, 32> {
    fn iter_values(&self) -> impl Iterator<Item = Value> {
        self.into_iter().map(|(k, v)| {
            Value::Array(ValArray::from_iter_exact([k.clone(), v.clone()].into_iter()))
        })
    }

    fn len(&self) -> usize {
        CMap::len(self)
    }

    fn select(v: Value) -> Option<Self> {
        match v {
            Value::Map(m) => Some(m.clone()),
            _ => None,
        }
    }

    fn project(self) -> Value {
        Value::Map(self)
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Map { key, value } => {
                Ok(Type::Tuple(TArc::from_iter([(**key).clone(), (**value).clone()])))
            }
            _ => bail!("expected Map, got {:?}", ft.args[0].typ),
        }
    }
}

pub trait MapFn<R: Rt, E: UserEvent>: Debug + Default + Send + Sync + 'static {
    type Collection: MapCollection;

    const NAME: &str;
    const TYP: LazyLock<FnType>;

    /// finish will be called when every lambda instance has produced
    /// a value for the updated array. Out contains the output of the
    /// predicate lambda for each index i, and a is the array. out and
    /// a are guaranteed to have the same length. out\[i\].cur is
    /// guaranteed to be Some.
    fn finish(&mut self, slots: &[Slot<R, E>], a: &Self::Collection) -> Option<Value>;
}

#[derive(Debug)]
pub struct Slot<R: Rt, E: UserEvent> {
    id: BindId,
    pred: Node<R, E>,
    pub cur: Option<Value>,
}

impl<R: Rt, E: UserEvent> Slot<R, E> {
    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.delete(ctx);
        ctx.cached.remove(&self.id);
        ctx.env.unbind_variable(self.id);
    }
}

#[derive(Debug)]
pub struct MapQ<R: Rt, E: UserEvent, T: MapFn<R, E>> {
    scope: Scope,
    predid: BindId,
    top_id: ExprId,
    mftyp: TArc<FnType>,
    etyp: Type,
    slots: Vec<Slot<R, E>>,
    cur: T::Collection,
    t: T,
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> BuiltIn<R, E> for MapQ<R, E, T> {
    const NAME: &str = T::NAME;
    const TYP: LazyLock<FnType> = T::TYP;

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => Ok(Box::new(Self {
                scope: scope.append(&format_compact!("fn{}", LambdaId::new().inner())),
                predid: BindId::new(),
                top_id,
                etyp: T::Collection::etyp(typ)?,
                mftyp: match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                },
                slots: vec![],
                cur: Default::default(),
                t: T::default(),
            })),
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> Apply<R, E> for MapQ<R, E, T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let slen = self.slots.len();
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.predid, v.clone());
            event.variables.insert(self.predid, v);
        }
        let (up, resized) =
            match from[0].update(ctx, event).and_then(|v| T::Collection::select(v)) {
                Some(a) if a.len() == slen => (Some(a), false),
                Some(a) if a.len() < slen => {
                    while self.slots.len() > a.len() {
                        if let Some(mut s) = self.slots.pop() {
                            s.delete(ctx)
                        }
                    }
                    (Some(a), true)
                }
                Some(a) => {
                    while self.slots.len() < a.len() {
                        let (id, node) = genn::bind(
                            ctx,
                            &self.scope.lexical,
                            "x",
                            self.etyp.clone(),
                            self.top_id,
                        );
                        let fargs = vec![node];
                        let fnode = genn::reference(
                            ctx,
                            self.predid,
                            Type::Fn(self.mftyp.clone()),
                            self.top_id,
                        );
                        let pred = genn::apply(
                            fnode,
                            self.scope.clone(),
                            fargs,
                            &self.mftyp,
                            self.top_id,
                        );
                        self.slots.push(Slot { id, pred, cur: None });
                    }
                    (Some(a), true)
                }
                None => (None, false),
            };
        if let Some(a) = up {
            for (s, v) in self.slots.iter().zip(a.iter_values()) {
                ctx.cached.insert(s.id, v.clone());
                event.variables.insert(s.id, v);
            }
            self.cur = a.clone();
            if a.len() == 0 {
                return Some(T::Collection::project(a));
            }
        }
        let init = event.init;
        let mut up = resized;
        for (i, s) in self.slots.iter_mut().enumerate() {
            if i == slen {
                // new nodes were added starting here
                event.init = true;
                if let Entry::Vacant(e) = event.variables.entry(self.predid)
                    && let Some(v) = ctx.cached.get(&self.predid)
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
            self.t.finish(&mut &self.slots, &self.cur)
        } else {
            None
        }
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        let (_, node) =
            genn::bind(ctx, &self.scope.lexical, "x", self.etyp.clone(), self.top_id);
        let fargs = vec![node];
        let ft = self.mftyp.clone();
        let fnode = genn::reference(ctx, self.predid, Type::Fn(ft.clone()), self.top_id);
        let mut node = genn::apply(fnode, self.scope.clone(), fargs, &ft, self.top_id);
        let r = node.typecheck(ctx);
        node.delete(ctx);
        r
    }

    fn refs(&self, refs: &mut Refs) {
        for s in &self.slots {
            s.pred.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.predid);
        for sl in &mut self.slots {
            sl.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.cur = Default::default();
        for sl in &mut self.slots {
            sl.cur = None;
            sl.pred.sleep(ctx);
        }
    }
}

pub trait FoldFn<R: Rt, E: UserEvent>: Debug + Send + Sync + 'static {
    type Collection: MapCollection;

    const NAME: &str;
    const TYP: LazyLock<FnType>;
}

#[derive(Debug)]
pub struct FoldQ<R: Rt, E: UserEvent, T: FoldFn<R, E>> {
    top_id: ExprId,
    fid: BindId,
    scope: Scope,
    binds: Vec<BindId>,
    nodes: Vec<Node<R, E>>,
    inits: Vec<Option<Value>>,
    initids: Vec<BindId>,
    initid: BindId,
    mftype: TArc<FnType>,
    etyp: Type,
    ityp: Type,
    init: Option<Value>,
    t: PhantomData<T>,
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> BuiltIn<R, E> for FoldQ<R, E, T> {
    const NAME: &str = T::NAME;
    const TYP: LazyLock<FnType> = T::TYP;

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _] => Ok(Box::new(Self {
                top_id,
                scope: scope.clone(),
                binds: vec![],
                nodes: vec![],
                inits: vec![],
                initids: vec![],
                initid: BindId::new(),
                fid: BindId::new(),
                etyp: T::Collection::etyp(typ)?,
                ityp: typ.args[1].typ.clone(),
                mftype: match &typ.args[2].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                },
                init: None,
                t: PhantomData,
            })),
            _ => bail!("expected three arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> Apply<R, E> for FoldQ<R, E, T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let init = match from[0].update(ctx, event).and_then(|v| T::Collection::select(v))
        {
            None => self.nodes.len(),
            Some(a) if a.len() == self.binds.len() => {
                for (id, v) in self.binds.iter().zip(a.iter_values()) {
                    ctx.cached.insert(*id, v.clone());
                    event.variables.insert(*id, v.clone());
                }
                self.nodes.len()
            }
            Some(a) => {
                let vals = a.iter_values().collect::<LPooled<Vec<Value>>>();
                while self.binds.len() < a.len() {
                    self.binds.push(BindId::new());
                    self.inits.push(None);
                    self.initids.push(BindId::new());
                }
                while a.len() < self.binds.len() {
                    if let Some(id) = self.binds.pop() {
                        ctx.cached.remove(&id);
                    }
                    if let Some(id) = self.initids.pop() {
                        ctx.cached.remove(&id);
                    }
                    self.inits.pop();
                    if let Some(mut n) = self.nodes.pop() {
                        n.delete(ctx);
                    }
                }
                let init = self.nodes.len();
                for i in 0..self.binds.len() {
                    ctx.cached.insert(self.binds[i], vals[i].clone());
                    event.variables.insert(self.binds[i], vals[i].clone());
                    if i >= self.nodes.len() {
                        let n = genn::reference(
                            ctx,
                            if i == 0 { self.initid } else { self.initids[i - 1] },
                            self.ityp.clone(),
                            self.top_id,
                        );
                        let x = genn::reference(
                            ctx,
                            self.binds[i],
                            self.etyp.clone(),
                            self.top_id,
                        );
                        let fnode = genn::reference(
                            ctx,
                            self.fid,
                            Type::Fn(self.mftype.clone()),
                            self.top_id,
                        );
                        let node = genn::apply(
                            fnode,
                            self.scope.clone(),
                            vec![n, x],
                            &self.mftype,
                            self.top_id,
                        );
                        self.nodes.push(node);
                    }
                }
                init
            }
        };
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.initid, v.clone());
            event.variables.insert(self.initid, v.clone());
            self.init = Some(v);
        }
        if let Some(v) = from[2].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let old_init = event.init;
        for i in 0..self.nodes.len() {
            if i == init {
                event.init = true;
                if let Some(v) = ctx.cached.get(&self.fid)
                    && let Entry::Vacant(e) = event.variables.entry(self.fid)
                {
                    e.insert(v.clone());
                }
                if i == 0 {
                    if let Some(v) = self.init.as_ref()
                        && let Entry::Vacant(e) = event.variables.entry(self.initid)
                    {
                        e.insert(v.clone());
                    }
                } else {
                    if let Some(v) = self.inits[i - 1].clone() {
                        event.variables.insert(self.initids[i - 1], v);
                    }
                }
            }
            match self.nodes[i].update(ctx, event) {
                Some(v) => {
                    ctx.cached.insert(self.initids[i], v.clone());
                    event.variables.insert(self.initids[i], v.clone());
                    self.inits[i] = Some(v);
                }
                None => {
                    ctx.cached.remove(&self.initids[i]);
                    event.variables.remove(&self.initids[i]);
                    self.inits[i] = None;
                }
            }
        }
        event.init = old_init;
        self.inits.last().and_then(|v| v.clone())
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        let mut n = genn::reference(ctx, self.initid, self.ityp.clone(), self.top_id);
        let x = genn::reference(ctx, BindId::new(), self.etyp.clone(), self.top_id);
        let fnode =
            genn::reference(ctx, self.fid, Type::Fn(self.mftype.clone()), self.top_id);
        n = genn::apply(fnode, self.scope.clone(), vec![n, x], &self.mftype, self.top_id);
        let r = n.typecheck(ctx);
        n.delete(ctx);
        r
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.nodes {
            n.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let i =
            iter::once(&self.initid).chain(self.binds.iter()).chain(self.initids.iter());
        for id in i {
            ctx.cached.remove(id);
        }
        for n in &mut self.nodes {
            n.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.init = None;
        for v in &mut self.inits {
            *v = None
        }
        for n in &mut self.nodes {
            n.sleep(ctx)
        }
    }
}

// ── Core builtins ──────────────────────────────────────────────────

#[derive(Debug)]
struct IsErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IsErr {
    const NAME: &str = "core_is_err";
    deftype!("fn(Any) -> bool");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(IsErr))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IsErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|v| match v {
            Value::Error(_) => Value::Bool(true),
            _ => Value::Bool(false),
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct FilterErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    const NAME: &str = "core_filter_err";
    deftype!("fn(Result<'a, 'b>) -> Error<'b>");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(FilterErr))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for FilterErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| match v {
            v @ Value::Error(_) => Some(v),
            _ => None,
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct ToError;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ToError {
    const NAME: &str = "core_error";
    deftype!("fn('a) -> Error<'a>");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(ToError))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ToError {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|e| Value::Error(triomphe::Arc::new(e)))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Once {
    val: bool,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    const NAME: &str = "core_once";
    deftype!("fn('a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Once { val: false }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Once {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        match from {
            [s] => s.update(ctx, event).and_then(|v| {
                if self.val {
                    None
                } else {
                    self.val = true;
                    Some(v)
                }
            }),
            _ => None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.val = false
    }
}

#[derive(Debug)]
struct Take {
    n: Option<usize>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Take {
    const NAME: &str = "core_take";
    deftype!("fn(#n:Any, 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Take { n: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Take {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(n) =
            from[0].update(ctx, event).and_then(|v| v.cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        match from[1].update(ctx, event) {
            None => None,
            Some(v) => match &mut self.n {
                None => None,
                Some(n) if *n > 0 => {
                    *n -= 1;
                    return Some(v);
                }
                Some(_) => None,
            },
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }
}

#[derive(Debug)]
struct Skip {
    n: Option<usize>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Skip {
    const NAME: &str = "core_skip";
    deftype!("fn(#n:Any, 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Skip { n: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Skip {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(n) =
            from[0].update(ctx, event).and_then(|v| v.cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        match from[1].update(ctx, event) {
            None => None,
            Some(v) => match &mut self.n {
                None => Some(v),
                Some(n) if *n > 0 => {
                    *n -= 1;
                    None
                }
                Some(_) => Some(v),
            },
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }
}

#[derive(Debug, Default)]
struct AllEv;

impl EvalCached for AllEv {
    const NAME: &str = "core_all";
    deftype!("fn(@args: Any) -> Any");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &*from.0 {
            [] => None,
            [hd, tl @ ..] => match hd {
                None => None,
                v @ Some(_) => {
                    if tl.into_iter().all(|v1| v1 == v) {
                        v.clone()
                    } else {
                        None
                    }
                }
            },
        }
    }
}

type All = CachedArgs<AllEv>;

fn add_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l + r),
    }
}

#[derive(Debug, Default)]
struct SumEv;

impl EvalCached for SumEv {
    const NAME: &str = "core_sum";
    deftype!("fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => add_vals(res, v.clone()),
        })
    }
}

type Sum = CachedArgs<SumEv>;

#[derive(Debug, Default)]
struct ProductEv;

fn prod_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l * r),
    }
}

impl EvalCached for ProductEv {
    const NAME: &str = "core_product";
    deftype!("fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => prod_vals(res, v.clone()),
        })
    }
}

type Product = CachedArgs<ProductEv>;

#[derive(Debug, Default)]
struct DivideEv;

fn div_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l / r),
    }
}

impl EvalCached for DivideEv {
    const NAME: &str = "core_divide";
    deftype!("fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => div_vals(res, v.clone()),
        })
    }
}

type Divide = CachedArgs<DivideEv>;

#[derive(Debug, Default)]
struct MinEv;

impl EvalCached for MinEv {
    const NAME: &str = "core_min";
    deftype!("fn('a, @args:'a) -> 'a");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v < v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Min = CachedArgs<MinEv>;

#[derive(Debug, Default)]
struct MaxEv;

impl EvalCached for MaxEv {
    const NAME: &str = "core_max";
    deftype!("fn('a, @args: 'a) -> 'a");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v > v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Max = CachedArgs<MaxEv>;

#[derive(Debug, Default)]
struct AndEv;

impl EvalCached for AndEv {
    const NAME: &str = "core_and";
    deftype!("fn(@args: bool) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(true));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => (),
                Some(_) => {
                    res = Some(Value::Bool(false));
                }
            }
        }
        res
    }
}

type And = CachedArgs<AndEv>;

#[derive(Debug, Default)]
struct OrEv;

impl EvalCached for OrEv {
    const NAME: &str = "core_or";
    deftype!("fn(@args: bool) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(false));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => {
                    res = Some(Value::Bool(true));
                }
                Some(_) => (),
            }
        }
        res
    }
}

type Or = CachedArgs<OrEv>;

#[derive(Debug)]
struct Filter<R: Rt, E: UserEvent> {
    ready: bool,
    queue: VecDeque<Value>,
    pred: Node<R, E>,
    top_id: ExprId,
    fid: BindId,
    x: BindId,
    out: BindId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Filter<R, E> {
    const NAME: &str = "core_filter";
    deftype!("fn('a, fn('a) -> bool throws 'e) -> 'a throws 'e");

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let (x, xn) =
                    genn::bind(ctx, &scope.lexical, "x", typ.args[0].typ.clone(), top_id);
                let fid = BindId::new();
                let ptyp = match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                };
                let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
                let pred = genn::apply(fnode, scope.clone(), vec![xn], &ptyp, top_id);
                let queue = VecDeque::new();
                let out = BindId::new();
                ctx.rt.ref_var(out, top_id);
                Ok(Box::new(Self { ready: true, queue, pred, fid, x, out, top_id }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Filter<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                ctx.cached.insert(self.x, $v.clone());
                event.variables.insert(self.x, $v);
            }};
        }
        macro_rules! maybe_cont {
            () => {{
                if let Some(v) = self.queue.front().cloned() {
                    set!(v);
                    continue;
                }
                break;
            }};
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.queue.push_back(v);
        }
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.front().unwrap().clone();
            set!(v);
        }
        loop {
            match self.pred.update(ctx, event) {
                None => break,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            ctx.rt.set_var(self.out, self.queue.pop_front().unwrap());
                            maybe_cont!();
                        }
                        _ => {
                            let _ = self.queue.pop_front();
                            maybe_cont!();
                        }
                    }
                }
            }
        }
        event.variables.get(&self.out).map(|v| v.clone())
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
        ctx.cached.remove(&self.out);
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.pred.delete(ctx);
        ctx.rt.unref_var(self.out, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.out, self.top_id);
        self.out = BindId::new();
        ctx.rt.ref_var(self.out, self.top_id);
        self.queue.clear();
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
struct Queue {
    triggered: usize,
    queue: VecDeque<Value>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Queue {
    const NAME: &str = "core_queue";
    deftype!("fn(#clock:Any, 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                Ok(Box::new(Self { triggered: 0, queue: VecDeque::new(), id, top_id }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Queue {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = from[1].update(ctx, event) {
            self.queue.push_back(v);
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.triggered = 0;
        self.queue.clear();
    }
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Hold {
    const NAME: &str = "core_hold";
    deftype!("fn(#clock:Any, 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => Ok(Box::new(Self { triggered: 0, current: None })),
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Hold {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = from[1].update(ctx, event) {
            self.current = Some(v);
        }
        if self.triggered > 0
            && let Some(v) = self.current.take()
        {
            self.triggered -= 1;
            Some(v)
        } else {
            None
        }
    }

    fn delete(&mut self, _: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _: &mut ExecCtx<R, E>) {
        self.triggered = 0;
        self.current = None;
    }
}

#[derive(Debug)]
struct Seq {
    id: BindId,
    top_id: ExprId,
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Seq {
    const NAME: &str = "core_seq";
    deftype!("fn(i64, i64) -> Result<i64, `SeqError(string)>");

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let args = CachedVals::new(from);
        Ok(Box::new(Self { id, top_id, args }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Seq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.args.update(ctx, from, event) {
            match &self.args.0[..] {
                [Some(Value::I64(i)), Some(Value::I64(j))] if i <= j => {
                    for v in *i..*j {
                        ctx.rt.set_var(self.id, Value::I64(v));
                    }
                }
                _ => {
                    let e = literal!("SeqError");
                    return Some(err!(e, "invalid args i must be <= j"));
                }
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    last: Option<Instant>,
    tid: Option<BindId>,
    top_id: ExprId,
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Throttle {
    const NAME: &str = "core_throttle";
    deftype!("fn(?#rate:duration, 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let args = CachedVals::new(from);
        Ok(Box::new(Self { wait: Duration::ZERO, last: None, tid: None, top_id, args }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Throttle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! maybe_schedule {
            ($last:expr) => {{
                let now = Instant::now();
                if now - *$last >= self.wait {
                    *$last = now;
                    return self.args.0[1].clone();
                } else {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, self.top_id);
                    ctx.rt.set_timer(id, self.wait - (now - *$last));
                    self.tid = Some(id);
                    return None;
                }
            }};
        }
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        if up[0]
            && let Some(Value::Duration(d)) = &self.args.0[0]
        {
            self.wait = **d;
            if let Some(id) = self.tid.take()
                && let Some(last) = &mut self.last
            {
                ctx.rt.unref_var(id, self.top_id);
                maybe_schedule!(last)
            }
        }
        if up[1] && self.tid.is_none() {
            match &mut self.last {
                Some(last) => maybe_schedule!(last),
                None => {
                    self.last = Some(Instant::now());
                    return self.args.0[1].clone();
                }
            }
        }
        if let Some(id) = self.tid
            && let Some(_) = event.variables.get(&id)
        {
            ctx.rt.unref_var(id, self.top_id);
            self.tid = None;
            self.last = Some(Instant::now());
            return self.args.0[1].clone();
        }
        None
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.tid.take() {
            ctx.rt.unref_var(id, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.delete(ctx);
        self.last = None;
        self.wait = Duration::ZERO;
        self.args.clear();
    }
}

#[derive(Debug)]
struct Count {
    count: i64,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    const NAME: &str = "core_count";
    deftype!("fn(Any) -> i64");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Count { count: 0 }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Count {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from.into_iter().fold(false, |u, n| u || n.update(ctx, event).is_some()) {
            self.count += 1;
            Some(Value::I64(self.count))
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.count = 0
    }
}

#[derive(Debug, Default)]
struct MeanEv;

impl EvalCached for MeanEv {
    const NAME: &str = "core_mean";
    deftype!(
        "fn([Number, Array<Number>], @args: [Number, Array<Number>]) -> Result<f64, `MeanError(string)>"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("MeanError");
        let mut total = 0.;
        let mut samples = 0;
        let mut error = None;
        for v in from.flat_iter() {
            if let Some(v) = v {
                match v.cast_to::<f64>() {
                    Err(e) => error = Some(errf!(TAG, "{e:?}")),
                    Ok(v) => {
                        total += v;
                        samples += 1;
                    }
                }
            }
        }
        if let Some(e) = error {
            Some(e)
        } else if samples == 0 {
            Some(err!(TAG, "mean requires at least one argument"))
        } else {
            Some(Value::F64(total / samples as f64))
        }
    }
}

type Mean = CachedArgs<MeanEv>;

#[derive(Debug)]
struct Uniq(Option<Value>);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Uniq {
    const NAME: &str = "core_uniq";
    deftype!("fn('a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Uniq(None)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Uniq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| {
            if Some(&v) != self.0.as_ref() {
                self.0 = Some(v.clone());
                Some(v)
            } else {
                None
            }
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.0 = None
    }
}

#[derive(Debug)]
struct Never;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Never {
    const NAME: &str = "core_never";
    deftype!("fn(@args: Any) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Never))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Never {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        for n in from {
            n.update(ctx, event);
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Clone, Copy)]
enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl FromValue for Level {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Trace" => Ok(Self::Trace),
            "Debug" => Ok(Self::Debug),
            "Info" => Ok(Self::Info),
            "Warn" => Ok(Self::Warn),
            "Error" => Ok(Self::Error),
            v => bail!("invalid log level {v}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LogDest {
    Stdout,
    Stderr,
    Log(Level),
}

impl FromValue for LogDest {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.clone().cast_to::<ArcStr>()? {
            "Stdout" => Ok(Self::Stdout),
            "Stderr" => Ok(Self::Stderr),
            _ => Ok(Self::Log(v.cast_to()?)),
        }
    }
}

#[derive(Debug)]
struct Dbg {
    spec: Expr,
    dest: LogDest,
    typ: Type,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Dbg {
    const NAME: &str = "core_dbg";
    deftype!("fn(?#dest:[`Stdout, `Stderr, Log], 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Dbg {
            spec: from[1].spec().clone(),
            dest: LogDest::Stderr,
            typ: Type::Bottom,
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Dbg {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        from[1].update(ctx, event).map(|v| {
            let tv = TVal { env: &ctx.env, typ: &self.typ, v: &v };
            match self.dest {
                LogDest::Stderr => {
                    eprintln!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                }
                LogDest::Stdout => {
                    println!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                }
                LogDest::Log(level) => match level {
                    Level::Trace => {
                        log::trace!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Debug => {
                        log::debug!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Info => {
                        log::info!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Warn => {
                        log::warn!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Error => {
                        log::error!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                },
            };
            v
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.typ = from[1].typ().clone();
        Ok(())
    }
}

#[derive(Debug)]
struct Log {
    scope: Scope,
    dest: LogDest,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    const NAME: &str = "core_log";
    deftype!("fn(?#dest:Log, 'a) -> _");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { scope: scope.clone(), dest: LogDest::Stdout }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Log {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        if let Some(v) = from[1].update(ctx, event) {
            let tv = TVal { env: &ctx.env, typ: from[1].typ(), v: &v };
            match self.dest {
                LogDest::Stdout => println!("{}: {}", self.scope.lexical, tv),
                LogDest::Stderr => eprintln!("{}: {}", self.scope.lexical, tv),
                LogDest::Log(lvl) => match lvl {
                    Level::Trace => log::trace!("{}: {}", self.scope.lexical, tv),
                    Level::Debug => log::debug!("{}: {}", self.scope.lexical, tv),
                    Level::Info => log::info!("{}: {}", self.scope.lexical, tv),
                    Level::Warn => log::warn!("{}: {}", self.scope.lexical, tv),
                    Level::Error => log::error!("{}: {}", self.scope.lexical, tv),
                },
            }
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! printfn {
    ($type:ident, $name:literal, $print:ident, $eprint:ident) => {
        #[derive(Debug)]
        struct $type {
            dest: LogDest,
            buf: String,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type {
            const NAME: &str = $name;
            deftype!("fn(?#dest:Log, 'a) -> _");

            fn init<'a, 'b, 'c>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a graphix_compiler::typ::FnType,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self { dest: LogDest::Stdout, buf: String::new() }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $type {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                use std::fmt::Write;
                if let Some(v) = from[0].update(ctx, event)
                    && let Ok(d) = v.cast_to::<LogDest>()
                {
                    self.dest = d;
                }
                if let Some(v) = from[1].update(ctx, event) {
                    self.buf.clear();
                    match v {
                        Value::String(s) => write!(self.buf, "{s}"),
                        v => write!(
                            self.buf,
                            "{}",
                            TVal { env: &ctx.env, typ: &from[1].typ(), v: &v }
                        ),
                    }
                    .unwrap();
                    match self.dest {
                        LogDest::Stdout => $print!("{}", self.buf),
                        LogDest::Stderr => $eprint!("{}", self.buf),
                        LogDest::Log(lvl) => match lvl {
                            Level::Trace => log::trace!("{}", self.buf),
                            Level::Debug => log::debug!("{}", self.buf),
                            Level::Info => log::info!("{}", self.buf),
                            Level::Warn => log::warn!("{}", self.buf),
                            Level::Error => log::error!("{}", self.buf),
                        },
                    }
                }
                None
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
        }
    };
}

printfn!(Print, "core_print", print, eprint);
printfn!(Println, "core_println", println, eprintln);

// ── Package registration ───────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        IsErr,
        FilterErr,
        ToError,
        Once,
        Take,
        Skip,
        All,
        Sum,
        Product,
        Divide,
        Min,
        Max,
        And,
        Or,
        Filter as Filter<GXRt<X>, X::UserEvent>,
        Queue,
        Hold,
        Seq,
        Throttle,
        Count,
        Mean,
        Uniq,
        Never,
        Dbg,
        Log,
        Print,
        Println,
    ],
}
