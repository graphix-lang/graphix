use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::{bitflags, BitFlags};
use fxhash::FxHashMap;
use graphix_compiler::{
    expr::{ExprId, ModuleResolver},
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, BuiltInInitFn, Event, ExecCtx, LambdaId, Node, Refs, Rt,
    Scope, UserEvent,
};
use netidx::{path::Path, subscriber::Value};
use netidx_core::utils::Either;
use netidx_value::FromValue;
use poolshark::local::LPooled;
use std::{
    any::Any,
    collections::{hash_map::Entry, VecDeque},
    fmt::Debug,
    iter,
    marker::PhantomData,
    sync::{Arc, LazyLock},
};
use triomphe::Arc as TArc;

mod array;
mod core;
mod fs;
mod map;
mod net;
mod rand;
mod re;
mod str;
#[cfg(test)]
mod test;
mod time;

#[macro_export]
macro_rules! deftype {
    ($scope:literal, $s:literal) => {
        const TYP: ::std::sync::LazyLock<graphix_compiler::typ::FnType> =
            ::std::sync::LazyLock::new(|| {
                let scope =
                    graphix_compiler::expr::ModPath(::netidx::path::Path::from($scope));
                graphix_compiler::expr::parser::parse_fn_type($s)
                    .expect("failed to parse fn type {s}")
                    .scope_refs(&scope)
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

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, _| {
            let t = CachedArgs::<T> { cached: CachedVals::new(from), t: T::default() };
            Ok(Box::new(t))
        })
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

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, _, _, from, top_id| {
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
        })
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

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_ctx, typ, scope, from, top_id| match from {
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
        })
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

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_ctx, typ, scope, from, top_id| match from {
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
        })
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

#[bitflags]
#[derive(Clone, Copy)]
#[repr(u64)]
pub enum Module {
    Array,
    Map,
    NetAndTime,
    Rand,
    Re,
    Str,
    Fs,
}

/// Register selected modules of the standard graphix library
///
/// and return a root module that will load them along with a module resolver
/// that contains the necessary code. You need both of these for the `rt`
/// module.
///
/// Note, core is always included and registered, all the other
/// modules are optional
///
/// # Example
///
/// ```no_run
/// use netidx::{publisher::Publisher, subscriber::Subscriber};
/// use anyhow::Result;
/// use poolshark::global::GPooled;
/// use graphix_compiler::ExecCtx;
/// use graphix_rt::{GXRt, GXConfigBuilder, GXHandle, GXEvent, NoExt};
/// use tokio::sync::mpsc;
/// use enumflags2::BitFlags;
///
/// async fn start_runtime(
///     publisher: Publisher,
///     subscriber: Subscriber,
///     sub: mpsc::Sender<GPooled<Vec<GXEvent<NoExt>>>>
/// ) -> Result<GXHandle<NoExt>> {
///     let mut ctx = ExecCtx::new(GXRt::<NoExt>::new(publisher, subscriber));
///     let (root, mods) = graphix_stdlib::register(&mut ctx, BitFlags::all())?;
///     GXConfigBuilder::default()
///        .ctx(ctx)
///        .root(root)
///        .resolvers(vec![mods])
///        .sub(sub)
///        .build()?
///        .start()
///        .await
/// }
/// ```
pub fn register<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    modules: BitFlags<Module>,
) -> Result<(ArcStr, ModuleResolver)> {
    let mut tbl = FxHashMap::default();
    let (core_impl, core_intf) = core::register(ctx)?;
    tbl.insert(Path::from("/core.gx"), core_impl);
    tbl.insert(Path::from("/core.gxi"), core_intf);
    let mut root = String::from("pub mod core;\nuse core;\n");
    for module in modules {
        match module {
            Module::Array => {
                root.push_str("pub mod array;\n");
                let (impl_code, intf_code) = array::register(ctx)?;
                tbl.insert(Path::from("/array.gx"), impl_code);
                tbl.insert(Path::from("/array.gxi"), intf_code);
            }
            Module::Map => {
                root.push_str("pub mod map;\n");
                let (impl_code, intf_code) = map::register(ctx)?;
                tbl.insert(Path::from("/map.gx"), impl_code);
                tbl.insert(Path::from("/map.gxi"), intf_code);
            }
            Module::NetAndTime => {
                root.push_str("pub mod time;\n");
                let (impl_code, intf_code) = time::register(ctx)?;
                tbl.insert(Path::from("/time.gx"), impl_code);
                tbl.insert(Path::from("/time.gxi"), intf_code);
                root.push_str("pub mod net;\n");
                let (impl_code, intf_code) = net::register(ctx)?;
                tbl.insert(Path::from("/net.gx"), impl_code);
                tbl.insert(Path::from("/net.gxi"), intf_code);
            }
            Module::Rand => {
                root.push_str("pub mod rand;\n");
                let (impl_code, intf_code) = rand::register(ctx)?;
                tbl.insert(Path::from("/rand.gx"), impl_code);
                tbl.insert(Path::from("/rand.gxi"), intf_code);
            }
            Module::Re => {
                root.push_str("pub mod re;\n");
                let (impl_code, intf_code) = re::register(ctx)?;
                tbl.insert(Path::from("/re.gx"), impl_code);
                tbl.insert(Path::from("/re.gxi"), intf_code);
            }
            Module::Str => {
                root.push_str("pub mod str;\n");
                let (impl_code, intf_code) = str::register(ctx)?;
                tbl.insert(Path::from("/str.gx"), impl_code);
                tbl.insert(Path::from("/str.gxi"), intf_code);
            }
            Module::Fs => {
                root.push_str("pub mod fs;\n");
                let (impl_code, intf_code) = fs::register(ctx)?;
                tbl.insert(Path::from("/fs.gx"), impl_code);
                tbl.insert(Path::from("/fs.gxi"), intf_code);
            }
        }
    }
    root.pop();
    root.pop();
    Ok((ArcStr::from(root), ModuleResolver::VFS(tbl)))
}
