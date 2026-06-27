use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use futures::{StreamExt, channel::mpsc, future::try_join_all};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, Event, ExecCtx, Node, Refs, Scope, compile,
    expr::{
        self, Expr, ExprId, ExprKind, ModPath, ModuleResolver, Origin, Source,
        read_to_arcstr,
    },
    node::{genn, lambda::LambdaDef},
    typ::Type,
};
use indexmap::IndexMap;
use log::{debug, error, info};
use netidx::{
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{self, Dval},
};
use netidx_protocols::rpc::server::RpcCall;
use nohash::{BuildNoHashHasher, IntMap};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::{SmallVec, smallvec};
use std::{
    collections::{VecDeque, hash_map::Entry},
    future, mem, result,
    time::Duration,
};
use tokio::{
    fs, select,
    sync::{
        mpsc::{self as tmpsc, UnboundedReceiver, error::SendTimeoutError},
        oneshot,
    },
    task::{JoinError, JoinSet},
    time::{self, Instant},
};
use triomphe::Arc;

use crate::{
    Callable, CallableId, CompExp, CompRes, GXConfig, GXEvent, GXExt, GXHandle, GXRt,
    Ref, ToGX, UpdateBatch, WriteBatch,
};

fn is_output<X: GXExt>(n: &Node<GXRt<X>, X::UserEvent>) -> bool {
    is_output_kind(&n.spec().kind)
}

fn is_output_kind(kind: &ExprKind) -> bool {
    match kind {
        ExprKind::Bind { .. }
        | ExprKind::Lambda { .. }
        | ExprKind::Use { .. }
        | ExprKind::Connect { .. }
        | ExprKind::Module { .. }
        | ExprKind::TypeDef { .. } => false,
        _ => true,
    }
}

/// Wrap a sequence of top-level Exprs from a file into a synthetic
/// `ExprKind::Do`. Per the unified-fusion design (§ "Files-as-
/// modules"), this gives the compiler ONE Expr to compile and ONE
/// Node back — fusion runs over the whole file's graph in one
/// pass, with cross-Bind dependencies visible because they're all
/// inside the one Do block.
///
/// `Do` was chosen over `Module` for the wrap because the file's
/// last expression value needs to propagate out as the runtime
/// output; `Module` blocks discard their last value while `Do`
/// blocks return it. The "file IS a module" framing in the design
/// applies to module-resolver imports — when another file does
/// `mod foo;` against this file, the resolver inserts a real
/// `ExprKind::Module` in the importing file's AST. The
/// load-execution wrap is purely an execution-time scope to make
/// fusion see the whole file at once.
fn wrap_file_in_do(exprs: Arc<[Expr]>, ori: Arc<Origin>) -> Expr {
    Expr {
        id: ExprId::new(),
        ori,
        pos: Default::default(),
        kind: ExprKind::Do { exprs },
        dec: None,
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
    nodes: IndexMap<ExprId, Node<GXRt<X>, X::UserEvent>, BuildNoHashHasher<ExprId>>,
    callables: IntMap<CallableId, CallableInt>,
    sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    resolvers: Arc<[ModuleResolver]>,
    publish_timeout: Option<Duration>,
    last_rpc_gc: Instant,
    batch_pool: Pool<Vec<GXEvent>>,
    flags: BitFlags<CFlag>,
    commit_tasks: JoinSet<()>,
    /// A pending `WaitResultOrIdle` request: deliver the first value the
    /// watched expr emits, or `None` when the runtime next goes idle. See
    /// `GXHandle::wait_result_or_idle`.
    result_watch: Option<(ExprId, oneshot::Sender<Option<Value>>)>,
}

impl<X: GXExt> GX<X> {
    pub(super) async fn new(mut cfg: GXConfig<X>) -> Result<Self> {
        let resolvers_default = |r: &mut Vec<ModuleResolver>| match dirs::data_dir() {
            None => (),
            Some(dd) => r.push(ModuleResolver::Files {
                base: dd.join("graphix"),
                overrides: None,
            }),
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
        let mut ctx = cfg.ctx;
        ctx.env.lsp_mode = cfg.lsp_mode;
        let mut t = Self {
            ctx,
            event,
            nodes: IndexMap::default(),
            callables: IntMap::default(),
            sub: cfg.sub,
            resolvers: Arc::from(cfg.resolvers),
            publish_timeout: cfg.publish_timeout,
            last_rpc_gc: Instant::now(),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            commit_tasks: JoinSet::new(),
            result_watch: None,
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
        mut batch: GPooled<Vec<GXEvent>>,
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
        // Point the JIT interrupt helper at this runtime's control on the
        // current worker (the cycle may run on a migrated thread).
        graphix_compiler::fusion::emit_helpers::set_interrupt_ptr(&self.ctx.control);
        // Run the synchronous node updates inside `block_in_place` so a
        // wedged node (a runaway loop) doesn't starve the rest of the
        // tokio runtime — the IO tasks and the caller that would
        // `interrupt()`/`abort()` it. `block_in_place` only isolates on a
        // multi-threaded runtime; on `current_thread` there's nowhere to
        // migrate, so run inline (the abort flag still breaks the loop;
        // IO just stalls until it does).
        let mut run_nodes = || {
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
                        let watched = matches!(
                            self.result_watch.as_ref(),
                            Some((wid, _)) if wid == id
                        );
                        if watched {
                            if let Some((_, tx)) = self.result_watch.take() {
                                let _ = tx.send(Some(v.clone()));
                            }
                        }
                        batch.push(GXEvent::Updated(*id, v))
                    }
                    for id in clear.drain(..) {
                        self.event.variables.remove(&id);
                    }
                }
            }
        };
        if matches!(
            tokio::runtime::Handle::current().runtime_flavor(),
            tokio::runtime::RuntimeFlavor::CurrentThread
        ) {
            run_nodes();
        } else {
            tokio::task::block_in_place(run_nodes);
        }
        // The one-shot interrupt is consumed; abort stays sticky for the
        // run loop's pre-cycle shutdown check.
        self.ctx.control.clear_interrupt();
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
        batch: &mut GPooled<Vec<GXEvent>>,
    ) {
        for m in input.drain(..) {
            match m {
                ToGX::GetEnv { res } => {
                    let _ = res.send(self.ctx.env.clone());
                }
                ToGX::Check { path, resolvers, initial_scope, res } => {
                    let _ = res.send(self.check(&path, resolvers, initial_scope).await);
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
                ToGX::MatchShape { id, spec, res } => {
                    let outcome = self
                        .nodes
                        .get(&id)
                        .map(|n| graphix_compiler::node_shape::match_node(n, &spec));
                    let _ = res.send(outcome);
                }
                ToGX::DescribeShape { id, res } => {
                    let desc = self
                        .nodes
                        .get(&id)
                        .map(graphix_compiler::node_shape::describe_node);
                    let _ = res.send(desc);
                }
                ToGX::EnvStats { res } => {
                    let by_id_len = self.ctx.env.by_id.len();
                    let ref_var_keys = self.ctx.rt.by_ref.len();
                    let ref_var_total = self
                        .ctx
                        .rt
                        .by_ref
                        .values()
                        .map(|m| m.values().copied().sum::<usize>())
                        .sum();
                    let _ = res.send(crate::EnvStats {
                        by_id_len,
                        ref_var_keys,
                        ref_var_total,
                    });
                }
                ToGX::FusionStats { res } => {
                    let _ = res.send(self.ctx.fusion.stats.clone());
                }
                ToGX::CycleReady { res } => {
                    let _ = res.send(self.cycle_ready());
                }
                ToGX::WaitResultOrIdle { id, res } => {
                    // Register the watch. `do_cycle` runs unconditionally
                    // right after this batch, so the watch gets at least one
                    // shot at being fulfilled (Some) before the top-of-loop
                    // idle check fulfills it None on quiescence. A second
                    // request supersedes any pending watch (its sender drops,
                    // surfacing as a cancellation to that caller).
                    self.result_watch = Some((id, res));
                }
            }
        }
    }

    fn cycle_ready(&self) -> bool {
        !self.ctx.rt.updated.is_empty()
            || self.ctx.rt.var_updates.len() > 0
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
        // Reset the unstable-bindings set for this batch. `Connect::compile`
        // re-populates it lazily during the `compile` pass below — every
        // `<-` site inserts the resolved BindId, so by the time the
        // fusion passes run, the set reflects all Connect targets.
        self.ctx.unstable_bindings.clear();
        self.ctx.bind_to_lambda.clear(); // batch-scoped sibling; see field doc (#203)
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
        let _ = &scope;
        Ok(())
    }

    async fn compile(&mut self, rt: GXHandle<X>, text: ArcStr) -> Result<CompRes<X>> {
        let scope = Scope::root();
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        // Pre-scan for `<-` (Connect) targets across the whole
        // program so the fusion call-resolution path can refuse to
        // register those bindings. `Connect::compile` populates
        // `ctx.unstable_bindings` with the resolved BindId as each
        // `<-` site is compiled — clear the carry-over from a
        // previous batch, then let compile populate.
        self.ctx.unstable_bindings.clear();
        self.ctx.bind_to_lambda.clear(); // batch-scoped sibling; see field doc (#203)
        let mut nodes = exprs
            .iter()
            .map(|e| compile(&mut self.ctx, self.flags, &scope, e.clone()))
            .collect::<Result<LPooled<Vec<_>>>>()
            .with_context(|| ori.clone())?;
        let comp_exprs = exprs
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
        let _ = &scope;
        Ok(CompRes { exprs: comp_exprs, env: self.ctx.env.clone() })
    }

    async fn load_exprs(&self, source: &Source) -> Result<(Origin, Arc<[Expr]>)> {
        let (ori, exprs) = match source {
            Source::File(file) => {
                let file = fs::canonicalize(file).await?;
                let s = fs::read_to_string(&file).await?;
                let s = if s.starts_with("#!") {
                    if let Some(i) = s.find('\n') { &s[i..] } else { s.as_str() }
                } else {
                    s.as_str()
                };
                let ori = Origin {
                    parent: None,
                    source: Source::File(file.clone()),
                    text: ArcStr::from(s),
                };
                let exprs = expr::parser::parse(ori.clone())?;
                let exprs = if file.extension().and_then(|s| s.to_str()) == Some("gx") {
                    let intf = file.with_extension("gxi");
                    match read_to_arcstr(&intf).await {
                        Ok(intf_text) => {
                            let intf_ori = Origin {
                                parent: None,
                                source: Source::File(intf),
                                text: ArcStr::from(intf_text),
                            };
                            let sig = expr::parser::parse_sig(intf_ori)?;
                            expr::add_interface_modules(exprs, &sig)
                        }
                        Err(_) => exprs,
                    }
                } else {
                    exprs
                };
                (ori, exprs)
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

    async fn check(
        &mut self,
        source: &Source,
        resolver_override: Option<Vec<ModuleResolver>>,
        initial_scope: Option<ArcStr>,
    ) -> Result<crate::CheckResult> {
        self.check_inner(source, resolver_override, initial_scope).await.map(|(_, r)| r)
    }

    /// Like `check`, but also returns the (post-resolve) Expr tree.
    async fn check_inner(
        &mut self,
        source: &Source,
        resolver_override: Option<Vec<ModuleResolver>>,
        initial_scope: Option<ArcStr>,
    ) -> Result<(Arc<[Expr]>, crate::CheckResult)> {
        let env = self.ctx.env.clone();
        let prev_ide = if self.ctx.env.lsp_mode {
            self.ctx.env.ide.replace(Arc::new(parking_lot::Mutex::new(
                graphix_compiler::ide::Ide::new(),
            )))
        } else {
            None
        };
        let resolvers_for_call: Arc<[ModuleResolver]> = match resolver_override {
            Some(v) => Arc::from(v),
            None => self.resolvers.clone(),
        };
        let go = async {
            let st = Instant::now();
            info!("parse time: {:?}", st.elapsed());
            let scope = match &initial_scope {
                None => Scope::root(),
                Some(s) => {
                    let path = ModPath(netidx::path::Path::root().append(s.as_str()));
                    self.ctx.env.unbind_scope_subtree(&path);
                    Scope { lexical: path.clone(), dynamic: path }
                }
            };
            let (ori, exprs) = self.load_exprs(source).await?;
            let exprs = try_join_all(exprs.iter().map(|e| {
                e.resolve_modules_in_scope(&scope.lexical, &resolvers_for_call)
            }))
            .await?;
            info!("resolve time: {:?}", st.elapsed());
            // Reset the batch-scoped static-resolution index (see its field
            // doc, #203). Bounds growth on the shared check/lsp runtime and
            // keeps a `<-`-reassigned lambda from one check from resolving
            // stalely in the next. (`unstable_bindings` is not reset on this
            // path — pre-existing; fusion is off under lsp so it's inert.)
            self.ctx.bind_to_lambda.clear();
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
            let env = self.ctx.env.clone();
            let ide = match self.ctx.env.ide.as_ref() {
                None => graphix_compiler::ide::Ide::new(),
                Some(ide) => {
                    std::mem::replace(&mut *ide.lock(), graphix_compiler::ide::Ide::new())
                }
            };
            for mut n in nodes.drain(..) {
                n.delete(&mut self.ctx);
            }
            Ok((Arc::from_iter(exprs), crate::CheckResult { env, ide }))
        };
        let res = go.await;
        self.ctx.env = env;
        self.ctx.env.ide = prev_ide;
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
        // Wrap the file's top-level Exprs in a synthetic Do block
        // so the compiler sees ONE Expr and produces ONE Node. The
        // fusion pass then walks the file's entire graph in a
        // single pass with cross-Bind dependencies visible inside
        // the Do's Block.
        let output = exprs.last().map(|e| is_output_kind(&e.kind)).unwrap_or(false);
        let wrapped =
            wrap_file_in_do(Arc::from_iter(exprs.into_iter()), Arc::new(ori.clone()));
        let top_id = wrapped.id;
        // Clear the carry-over `unstable_bindings`; `Connect::compile`
        // re-populates with resolved BindIds as each `<-` site is
        // compiled below.
        self.ctx.unstable_bindings.clear();
        self.ctx.bind_to_lambda.clear(); // batch-scoped sibling; see field doc (#203)
        let n = compile(&mut self.ctx, self.flags, &scope, wrapped)
            .with_context(|| ori.clone())?;
        let typ = n.typ().clone();
        self.nodes.insert(top_id, n);
        self.ctx.rt.updated.insert(top_id, true);
        let res = smallvec![CompExp { id: top_id, output, typ, rt: rt.clone() }];
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    fn compile_callable(&mut self, v: Value, rt: GXHandle<X>) -> Result<Callable<X>> {
        let lb = v
            .downcast_ref::<LambdaDef<GXRt<X>, X::UserEvent>>()
            .ok_or_else(|| anyhow!("invalid lambda {v}"))?;
        let args = lb.typ.args.iter();
        let args = args
            .map(|a| {
                if a.has_default() {
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
        let fnode = genn::constant(v.clone());
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
            // Abort/shutdown: if the handle was dropped or `abort()` was
            // called, exit before doing any more work. A wedged
            // `do_cycle` loop also breaks on this (the in-loop poll), then
            // returns here. Pending commands' response channels drop on
            // return, so blocked callers get an error instead of hanging.
            if self.ctx.control.aborted() {
                return Ok(());
            }
            let now = Instant::now();
            let ready = self.cycle_ready();
            // A `WaitResultOrIdle` watcher whose expr didn't emit during the
            // previous cycle resolves to `None` the moment the runtime goes
            // idle — no future cycle can produce its result.
            if !ready {
                if let Some((_, tx)) = self.result_watch.take() {
                    let _ = tx.send(None);
                }
            }
            let mut updates = None;
            let mut writes = None;
            macro_rules! peek {
                (updates) => {
                    if self.ctx.rt.net_updates.is_empty() {
                        while let Ok(mut up) = self.ctx.rt.updates.try_recv() {
                            match &mut updates {
                                None => updates = Some(up),
                                Some(prev) => prev.extend(up.drain(..)),
                            }
                        }
                    }
                };
                (writes) => {
                    if self.ctx.rt.net_writes.is_empty() {
                        if let Ok(wr) = self.ctx.rt.writes.try_recv() {
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
                        while let Ok(mut up) = rx.try_recv() {
                            custom_tasks.extend(up.drain(..))
                        }
                    }
                };
                (var_watches) => {
                    for rx in self.ctx.rt.var_watches.iter_mut() {
                        while let Ok(mut up) = rx.try_recv() {
                            tasks.extend(up.drain(..))
                        }
                    }
                };
                (rpcs) => {
                    if self.ctx.rt.rpc_overflow.is_empty() {
                        while let Ok(up) = self.ctx.rt.rpcs.try_recv() {
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
