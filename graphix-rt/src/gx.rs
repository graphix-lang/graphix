use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use futures::{channel::mpsc, future::try_join_all, StreamExt};
use fxhash::{FxBuildHasher, FxHashMap};
use graphix_compiler::{
    compile,
    expr::{self, Expr, ExprId, ExprKind, ModuleResolver, Origin, Source},
    node::genn,
    typ::Type,
    BindId, CFlag, CustomBuiltinType, Event, ExecCtx, LambdaId, Node, Refs, Scope,
};
use indexmap::IndexMap;
use log::{debug, error, info};
use netidx::{
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{self, Dval},
};
use netidx_protocols::rpc::server::RpcCall;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::{smallvec, SmallVec};
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    future, mem, result,
    sync::Weak,
    time::Duration,
};
use tokio::{
    fs, select,
    sync::mpsc::{self as tmpsc, error::SendTimeoutError, UnboundedReceiver},
    task::{JoinError, JoinSet},
    time::{self, Instant},
};
use triomphe::Arc;

use crate::{
    Callable, CallableId, CompExp, CompRes, GXConfig, GXEvent, GXExt, GXHandle, GXRt,
    Ref, ToGX, UpdateBatch, WriteBatch,
};

fn is_output<X: GXExt>(n: &Node<GXRt<X>, X::UserEvent>) -> bool {
    match &n.spec().kind {
        ExprKind::Bind { .. }
        | ExprKind::Lambda { .. }
        | ExprKind::Use { .. }
        | ExprKind::Connect { .. }
        | ExprKind::Module { .. }
        | ExprKind::TypeDef { .. } => false,
        _ => true,
    }
}

async fn or_never(b: bool) {
    if !b {
        future::pending().await
    }
}

async fn join_or_wait<T: 'static>(
    js: &mut JoinSet<(BindId, T)>,
) -> result::Result<(BindId, T), JoinError> {
    match js.join_next().await {
        None => future::pending().await,
        Some(r) => r,
    }
}

async fn maybe_next<T>(go: bool, ch: &mut mpsc::Receiver<T>) -> T {
    if go {
        match ch.next().await {
            None => future::pending().await,
            Some(v) => v,
        }
    } else {
        future::pending().await
    }
}

async fn unsubscribe_ready(pending: &VecDeque<(Instant, Dval)>, now: Instant) {
    if pending.len() == 0 {
        future::pending().await
    } else {
        let (ts, _) = pending.front().unwrap();
        let one = Duration::from_secs(1);
        let elapsed = now - *ts;
        if elapsed < one {
            time::sleep(one - elapsed).await
        }
    }
}

struct CallableInt {
    expr: ExprId,
    args: Box<[BindId]>,
}

pub(super) struct GX<X: GXExt> {
    ctx: ExecCtx<GXRt<X>, X::UserEvent>,
    event: Event<X::UserEvent>,
    nodes: IndexMap<ExprId, Node<GXRt<X>, X::UserEvent>, FxBuildHasher>,
    callables: FxHashMap<CallableId, CallableInt>,
    sub: tmpsc::Sender<GPooled<Vec<GXEvent<X>>>>,
    resolvers: Arc<[ModuleResolver]>,
    publish_timeout: Option<Duration>,
    last_rpc_gc: Instant,
    batch_pool: Pool<Vec<GXEvent<X>>>,
    flags: BitFlags<CFlag>,
    commit_tasks: JoinSet<()>,
}

impl<X: GXExt> GX<X> {
    pub(super) async fn new(mut cfg: GXConfig<X>) -> Result<Self> {
        let resolvers_default = |r: &mut Vec<ModuleResolver>| match dirs::data_dir() {
            None => (),
            Some(dd) => r.push(ModuleResolver::Files(dd.join("graphix"))),
        };
        match std::env::var("GRAPHIX_MODPATH") {
            Err(_) => resolvers_default(&mut cfg.resolvers),
            Ok(mp) => match ModuleResolver::parse_env(
                cfg.ctx.rt.subscriber.clone(),
                cfg.resolve_timeout,
                &mp,
            ) {
                Ok(r) => cfg.resolvers.extend(r),
                Err(e) => {
                    error!("failed to parse GRAPHIX_MODPATH, using default {e:?}");
                    resolvers_default(&mut cfg.resolvers)
                }
            },
        };
        let event = Event::new(cfg.ctx.rt.ext.empty_event());
        let mut t = Self {
            ctx: cfg.ctx,
            event,
            nodes: IndexMap::default(),
            callables: HashMap::default(),
            sub: cfg.sub,
            resolvers: Arc::from(cfg.resolvers),
            publish_timeout: cfg.publish_timeout,
            last_rpc_gc: Instant::now(),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            commit_tasks: JoinSet::new(),
        };
        let st = Instant::now();
        if let Some(root) = cfg.root {
            t.compile_root(cfg.flags, root).await?;
        }
        info!("root init time: {:?}", st.elapsed());
        Ok(t)
    }

    async fn do_cycle(
        &mut self,
        updates: Option<UpdateBatch>,
        writes: Option<WriteBatch>,
        tasks: &mut Vec<(BindId, Value)>,
        custom_tasks: &mut Vec<(BindId, Box<dyn CustomBuiltinType>)>,
        rpcs: &mut Vec<(BindId, RpcCall)>,
        to_rt: &mut UnboundedReceiver<ToGX<X>>,
        input: &mut Vec<ToGX<X>>,
        mut batch: GPooled<Vec<GXEvent<X>>>,
    ) {
        macro_rules! push_event {
            ($id:expr, $v:expr, $event:ident, $refed:ident, $overflow:ident) => {
                match self.event.$event.entry($id) {
                    Entry::Vacant(e) => {
                        e.insert($v);
                        if let Some(exps) = self.ctx.rt.$refed.get(&$id) {
                            for id in exps.keys() {
                                self.ctx.rt.updated.entry(*id).or_insert(false);
                            }
                        }
                    }
                    Entry::Occupied(_) => {
                        self.ctx.rt.$overflow.push_back(($id, $v));
                    }
                }
            };
        }
        for _ in 0..self.ctx.rt.var_updates.len() {
            let (id, v) = self.ctx.rt.var_updates.pop_front().unwrap();
            push_event!(id, v, variables, by_ref, var_updates)
        }
        for (id, v) in tasks.drain(..) {
            push_event!(id, v, variables, by_ref, var_updates)
        }
        for _ in 0..self.ctx.rt.custom_updates.len() {
            let (id, u) = self.ctx.rt.custom_updates.pop_front().unwrap();
            push_event!(id, u, custom, by_ref, custom_updates)
        }
        for (id, u) in custom_tasks.drain(..) {
            push_event!(id, u, custom, by_ref, custom_updates)
        }
        for _ in 0..self.ctx.rt.rpc_overflow.len() {
            let (id, v) = self.ctx.rt.rpc_overflow.pop_front().unwrap();
            push_event!(id, v, rpc_calls, by_ref, rpc_overflow)
        }
        for (id, v) in rpcs.drain(..) {
            push_event!(id, v, rpc_calls, by_ref, rpc_overflow)
        }
        for _ in 0..self.ctx.rt.net_updates.len() {
            let (id, v) = self.ctx.rt.net_updates.pop_front().unwrap();
            push_event!(id, v, netidx, subscribed, net_updates)
        }
        if let Some(mut updates) = updates {
            for (id, v) in updates.drain(..) {
                push_event!(id, v, netidx, subscribed, net_updates)
            }
        }
        for _ in 0..self.ctx.rt.net_writes.len() {
            let (id, v) = self.ctx.rt.net_writes.pop_front().unwrap();
            push_event!(id, v, writes, published, net_writes)
        }
        if let Some(mut writes) = writes {
            for wr in writes.drain(..) {
                let id = wr.id;
                push_event!(id, wr, writes, published, net_writes)
            }
        }
        if let Err(e) = self.ctx.rt.ext.do_cycle(&mut self.event) {
            error!("could not marshall user events {e:?}")
        }
        for (id, n) in self.nodes.iter_mut() {
            if let Some(init) = self.ctx.rt.updated.get(id) {
                let mut clear: LPooled<Vec<BindId>> = LPooled::take();
                self.event.init = *init;
                if self.event.init {
                    let mut refs = Refs::default();
                    n.refs(&mut refs);
                    refs.with_external_refs(|id| {
                        if let Some(v) = self.ctx.cached.get(&id) {
                            if let Entry::Vacant(e) = self.event.variables.entry(id) {
                                e.insert(v.clone());
                                clear.push(id);
                            }
                        }
                    });
                }
                if let Some(v) = n.update(&mut self.ctx, &mut self.event) {
                    batch.push(GXEvent::Updated(*id, v))
                }
                for id in clear.drain(..) {
                    self.event.variables.remove(&id);
                }
            }
        }
        loop {
            match self.sub.send_timeout(batch, Duration::from_millis(100)).await {
                Ok(()) => break,
                Err(SendTimeoutError::Closed(_)) => {
                    error!("could not send batch");
                    break;
                }
                Err(SendTimeoutError::Timeout(b)) => {
                    batch = b;
                    // prevent deadlock on input
                    while let Ok(m) = to_rt.try_recv() {
                        input.push(m);
                    }
                    self.process_input_batch(tasks, input, &mut batch).await;
                }
            }
        }
        self.event.clear();
        self.ctx.rt.updated.clear();
        if self.ctx.rt.batch.len() > 0 {
            let batch =
                mem::replace(&mut self.ctx.rt.batch, self.ctx.rt.publisher.start_batch());
            let timeout = self.publish_timeout;
            while let Some(_) = self.commit_tasks.try_join_next() {}
            self.commit_tasks.spawn(async move { batch.commit(timeout).await });
        }
    }

    async fn process_input_batch(
        &mut self,
        tasks: &mut Vec<(BindId, Value)>,
        input: &mut Vec<ToGX<X>>,
        batch: &mut GPooled<Vec<GXEvent<X>>>,
    ) {
        for m in input.drain(..) {
            match m {
                ToGX::GetEnv { res } => {
                    let _ = res.send(self.ctx.env.clone());
                }
                ToGX::Check { path, res } => {
                    let _ = res.send(self.check(&path).await);
                }
                ToGX::Compile { text, rt, res } => {
                    let _ = res.send(self.compile(rt, text).await);
                }
                ToGX::Load { path, rt, res } => {
                    let _ = res.send(self.load(rt, &path).await);
                }
                ToGX::Delete { id } => {
                    if let Some(mut n) = self.nodes.shift_remove(&id) {
                        n.delete(&mut self.ctx);
                    }
                    debug!("delete {id:?}");
                    batch.push(GXEvent::Env(self.ctx.env.clone()));
                }
                ToGX::CompileCallable { id, rt, res } => {
                    let _ = res.send(self.compile_callable(id, rt));
                }
                ToGX::CompileRef { id, rt, res } => {
                    let _ = res.send(self.compile_ref(rt, id));
                }
                ToGX::Set { id, v } => {
                    self.ctx.cached.insert(id, v.clone());
                    tasks.push((id, v))
                }
                ToGX::DeleteCallable { id } => self.delete_callable(id),
                ToGX::Call { id, args } => {
                    if let Err(e) = self.call_callable(id, args, tasks) {
                        error!("calling callable {id:?} failed with {e:?}")
                    }
                }
            }
        }
    }

    fn cycle_ready(&self) -> bool {
        self.ctx.rt.var_updates.len() > 0
            || self.ctx.rt.custom_updates.len() > 0
            || self.ctx.rt.net_updates.len() > 0
            || self.ctx.rt.net_writes.len() > 0
            || self.ctx.rt.rpc_overflow.len() > 0
            || self.ctx.rt.ext.is_ready()
    }

    async fn compile_root(&mut self, flags: BitFlags<CFlag>, text: ArcStr) -> Result<()> {
        let scope = Scope::root();
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())
            .with_context(|| format!("parsing the root module {ori}"))?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        let mut nodes = exprs
            .iter()
            .map(|e| {
                compile(&mut self.ctx, flags, &scope, e.clone())
                    .with_context(|| format!("compiling root expression {e}"))
            })
            .collect::<Result<LPooled<Vec<_>>>>()
            .with_context(|| ori.clone())?;
        for (e, n) in exprs.iter().zip(nodes.drain(..)) {
            self.ctx.rt.updated.insert(e.id, true);
            self.nodes.insert(e.id, n);
        }
        Ok(())
    }

    async fn compile(&mut self, rt: GXHandle<X>, text: ArcStr) -> Result<CompRes<X>> {
        let scope = Scope::root();
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        let mut nodes = exprs
            .iter()
            .map(|e| compile(&mut self.ctx, self.flags, &scope, e.clone()))
            .collect::<Result<LPooled<Vec<_>>>>()
            .with_context(|| ori.clone())?;
        let exprs = exprs
            .iter()
            .zip(nodes.drain(..))
            .map(|(e, n)| {
                let output = is_output(&n);
                let typ = n.typ().clone();
                self.ctx.rt.updated.insert(e.id, true);
                self.nodes.insert(e.id, n);
                CompExp { id: e.id, output, typ, rt: rt.clone() }
            })
            .collect::<SmallVec<[_; 1]>>();
        Ok(CompRes { exprs, env: self.ctx.env.clone() })
    }

    async fn load_exprs(&self, source: &Source) -> Result<(Origin, Arc<[Expr]>)> {
        let (ori, exprs) = match source {
            Source::File(file) => {
                let file = fs::canonicalize(file).await?;
                let s = fs::read_to_string(&file).await?;
                let s = if s.starts_with("#!") {
                    if let Some(i) = s.find('\n') {
                        &s[i..]
                    } else {
                        s.as_str()
                    }
                } else {
                    s.as_str()
                };
                let ori = Origin {
                    parent: None,
                    source: Source::File(file),
                    text: ArcStr::from(s),
                };
                (ori.clone(), expr::parser::parse(ori)?)
            }
            Source::Netidx(path) => {
                let val = self
                    .ctx
                    .rt
                    .subscriber
                    .subscribe_nondurable_one(path.clone(), None)
                    .await?;
                let src = match val.last() {
                    subscriber::Event::Unsubscribed => {
                        bail!("could not subscribe to {path}")
                    }
                    subscriber::Event::Update(Value::String(s)) => s,
                    subscriber::Event::Update(v) => {
                        bail!("can't load {v} expected a string")
                    }
                };
                let ori =
                    Origin { parent: None, source: source.clone(), text: src.clone() };
                (ori.clone(), expr::parser::parse(ori)?)
            }
            Source::Internal(src) => {
                let ori =
                    Origin { parent: None, source: source.clone(), text: src.clone() };
                (ori.clone(), expr::parser::parse(ori)?)
            }
            Source::Unspecified => bail!("can't load from an unspecified source"),
        };
        Ok((ori, exprs))
    }

    async fn check(&mut self, source: &Source) -> Result<()> {
        let env = self.ctx.env.clone();
        let go = async {
            let st = Instant::now();
            info!("parse time: {:?}", st.elapsed());
            let scope = Scope::root();
            let (ori, exprs) = self.load_exprs(source).await?;
            let exprs =
                try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                    .await?;
            info!("resolve time: {:?}", st.elapsed());
            let mut nodes: LPooled<Vec<_>> = LPooled::take();
            for e in exprs.iter() {
                let res = compile(&mut self.ctx, self.flags, &scope, e.clone())
                    .with_context(|| ori.clone());
                match res {
                    Ok(n) => nodes.push(n),
                    Err(e) => {
                        for mut n in nodes.drain(..) {
                            n.delete(&mut self.ctx);
                        }
                        return Err(e);
                    }
                }
            }
            for mut n in nodes.drain(..) {
                n.delete(&mut self.ctx);
            }
            Ok(())
        };
        let res = go.await;
        self.ctx.env = env;
        res
    }

    async fn load(&mut self, rt: GXHandle<X>, source: &Source) -> Result<CompRes<X>> {
        let scope = Scope::root();
        let st = Instant::now();
        let (ori, exprs) = self.load_exprs(source).await?;
        info!("parse time: {:?}", st.elapsed());
        let st = Instant::now();
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        info!("resolve time: {:?}", st.elapsed());
        let mut res = smallvec![];
        for e in exprs.iter() {
            let top_id = e.id;
            let n = compile(&mut self.ctx, self.flags, &scope, e.clone())
                .with_context(|| ori.clone())?;
            let has_out = is_output(&n);
            let typ = n.typ().clone();
            self.nodes.insert(top_id, n);
            self.ctx.rt.updated.insert(top_id, true);
            res.push(CompExp { id: top_id, output: has_out, typ, rt: rt.clone() })
        }
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    fn compile_callable(&mut self, id: Value, rt: GXHandle<X>) -> Result<Callable<X>> {
        let id = match id {
            Value::U64(id) => LambdaId::from(id),
            v => bail!("invalid lambda id {v}"),
        };
        let lb = self.ctx.env.lambdas.get(&id).and_then(Weak::upgrade);
        let lb = lb.ok_or_else(|| anyhow!("unknown lambda {id:?}"))?;
        let args = lb.typ.args.iter();
        let args = args
            .map(|a| {
                if a.label.as_ref().map(|(_, opt)| *opt).unwrap_or(false) {
                    bail!("can't call lambda with an optional argument from rust")
                } else {
                    Ok(BindId::new())
                }
            })
            .collect::<Result<Box<[_]>>>()?;
        let eid = ExprId::new();
        let argn = lb.typ.args.iter().zip(args.iter());
        let argn = argn
            .map(|(arg, id)| genn::reference(&mut self.ctx, *id, arg.typ.clone(), eid))
            .collect::<Vec<_>>();
        let fnode = genn::constant(Value::U64(id.inner()));
        let mut n = genn::apply(fnode, Scope::root(), argn, &lb.typ, eid);
        self.event.init = true;
        n.update(&mut self.ctx, &mut self.event);
        self.event.clear();
        let cid = CallableId::new();
        self.callables.insert(cid, CallableInt { expr: eid, args });
        self.nodes.insert(eid, n);
        let env = self.ctx.env.clone();
        Ok(Callable { expr: eid, rt, env, id: cid, typ: (*lb.typ).clone() })
    }

    fn compile_ref(&mut self, rt: GXHandle<X>, id: BindId) -> Result<Ref<X>> {
        let eid = ExprId::new();
        let typ = self
            .ctx
            .env
            .by_id
            .get(&id)
            .map(|b| b.typ.clone())
            .unwrap_or_else(|| Type::Any);
        let n = genn::reference(&mut self.ctx, id, typ.clone(), eid);
        self.nodes.insert(eid, n);
        let target_bid = self.ctx.env.byref_chain.get(&id).copied();
        Ok(Ref {
            id: eid,
            bid: id,
            typ,
            target_bid,
            last: self.ctx.cached.get(&id).cloned(),
            rt,
        })
    }

    fn call_callable(
        &mut self,
        id: CallableId,
        args: ValArray,
        tasks: &mut Vec<(BindId, Value)>,
    ) -> Result<()> {
        let c =
            self.callables.get(&id).ok_or_else(|| anyhow!("unknown callable {id:?}"))?;
        if args.len() != c.args.len() {
            bail!("expected {} arguments", c.args.len());
        }
        let a = c.args.iter().zip(args.iter()).map(|(id, v)| (*id, v.clone()));
        tasks.extend(a);
        Ok(())
    }

    fn delete_callable(&mut self, id: CallableId) {
        if let Some(c) = self.callables.remove(&id) {
            if let Some(mut n) = self.nodes.shift_remove(&c.expr) {
                n.delete(&mut self.ctx)
            }
        }
    }

    pub(super) async fn run(
        mut self,
        mut to_rt: tmpsc::UnboundedReceiver<ToGX<X>>,
    ) -> Result<()> {
        let mut tasks = vec![];
        let mut custom_tasks = vec![];
        let mut input = vec![];
        let mut rpcs = vec![];
        let onemin = Duration::from_secs(60);
        'main: loop {
            let now = Instant::now();
            let ready = self.cycle_ready();
            let mut updates = None;
            let mut writes = None;
            macro_rules! peek {
                (updates) => {
                    if self.ctx.rt.net_updates.is_empty() {
                        while let Ok(Some(mut up)) = self.ctx.rt.updates.try_next() {
                            match &mut updates {
                                None => updates = Some(up),
                                Some(prev) => prev.extend(up.drain(..)),
                            }
                        }
                    }
                };
                (writes) => {
                    if self.ctx.rt.net_writes.is_empty() {
                        if let Ok(Some(wr)) = self.ctx.rt.writes.try_next() {
                            writes = Some(wr);
                        }
                    }
                };
                (tasks) => {
                    while let Some(Ok(up)) = self.ctx.rt.tasks.try_join_next() {
                        tasks.push(up);
                    }
                };
                (custom_tasks) => {
                    while let Some(Ok(up)) = self.ctx.rt.custom_tasks.try_join_next() {
                        custom_tasks.push(up);
                    }
                };
                (watches) => {
                    for rx in self.ctx.rt.watches.iter_mut() {
                        while let Ok(Some(mut up)) = rx.try_next() {
                            custom_tasks.extend(up.drain(..))
                        }
                    }
                };
                (var_watches) => {
                    for rx in self.ctx.rt.var_watches.iter_mut() {
                        while let Ok(Some(mut up)) = rx.try_next() {
                            tasks.extend(up.drain(..))
                        }
                    }
                };
                (rpcs) => {
                    if self.ctx.rt.rpc_overflow.is_empty() {
                        while let Ok(Some(up)) = self.ctx.rt.rpcs.try_next() {
                            rpcs.push(up);
                        }
                    }
                };
                (input) => {
                    while let Ok(m) = to_rt.try_recv() {
                        input.push(m);
                    }
                };
                ($($item:tt),+) => {{
                    $(peek!($item));+
                }};
            }
            select! {
                rp = maybe_next(
                    self.ctx.rt.rpc_overflow.is_empty(),
                    &mut self.ctx.rt.rpcs
                ) => {
                    rpcs.push(rp);
                    peek!(updates, tasks, custom_tasks, watches, var_watches, writes, rpcs, input)
                }
                wr = maybe_next(
                    self.ctx.rt.net_writes.is_empty(),
                    &mut self.ctx.rt.writes
                ) => {
                    writes = Some(wr);
                    peek!(updates, tasks, custom_tasks, watches, var_watches, rpcs, input);
                },
                up = maybe_next(
                    self.ctx.rt.net_updates.is_empty(),
                    &mut self.ctx.rt.updates
                ) => {
                    updates = Some(up);
                    peek!(updates, writes, custom_tasks, watches, var_watches, tasks, rpcs, input);
                },
                up = join_or_wait(&mut self.ctx.rt.tasks) => {
                    if let Ok(up) = up {
                        tasks.push(up);
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input)
                },
                up = join_or_wait(&mut self.ctx.rt.custom_tasks) => {
                    if let Ok(up) = up {
                        custom_tasks.push(up);
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input)
                },
                up = self.ctx.rt.watches.next() => {
                    if let Some(mut up) = up {
                        for v in up.drain(..) {
                            custom_tasks.push(v);
                        }
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input)
                },
                up = self.ctx.rt.var_watches.next() => {
                    if let Some(mut up) = up {
                        for v in up.drain(..) {
                            tasks.push(v);
                        }
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input)
                },
                _ = or_never(ready) => {
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input)
                },
                n = to_rt.recv_many(&mut input, 100000) => {
                    if n == 0 {
                        break 'main Ok(())
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs);
                },
                r = self.ctx.rt.ext.update_sources() => {
                    if let Err(e) = r {
                        error!("failed to update custom event sources {e:?}")
                    }
                    peek!(updates, writes, watches, tasks, var_watches, custom_tasks, rpcs, input);
                },
                () = unsubscribe_ready(&self.ctx.rt.pending_unsubscribe, now) => {
                    while let Some((ts, _)) = self.ctx.rt.pending_unsubscribe.front() {
                        if ts.elapsed() >= Duration::from_secs(1) {
                            self.ctx.rt.pending_unsubscribe.pop_front();
                        } else {
                            break
                        }
                    }
                    continue 'main
                },
            }
            let mut batch = self.batch_pool.take();
            self.process_input_batch(&mut tasks, &mut input, &mut batch).await;
            self.do_cycle(
                updates,
                writes,
                &mut tasks,
                &mut custom_tasks,
                &mut rpcs,
                &mut to_rt,
                &mut input,
                batch,
            )
            .await;
            if !self.ctx.rt.rpc_clients.is_empty() {
                if now - self.last_rpc_gc >= onemin {
                    self.last_rpc_gc = now;
                    self.ctx.rt.rpc_clients.retain(|_, c| now - c.last_used <= onemin);
                }
            }
        }
    }
}
