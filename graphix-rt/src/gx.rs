use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use futures::{StreamExt, future::try_join_all};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, Event, ExecCtx, Node, Rt, Scope, compile,
    expr::{
        self, Expr, ExprId, ExprKind, FilesResolver, ModPath, Origin, ResolverRef,
        Resolvers, Source, parse_modpath, read_to_arcstr,
    },
    node::place::{self, VarUpdate},
    node::{genn, lambda::LambdaDef},
    typ::Type,
};
use indexmap::IndexMap;
use log::{debug, error, info};
use netidx_value::{ValArray, Value};
use nohash::{BuildNoHashHasher, IntMap};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::{SmallVec, smallvec};
use std::{collections::hash_map::Entry, future, mem, result, time::Duration};
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
    Ref, ToGX, TraceEvent, TraceSegment,
};

static TRACE_EVENTS: std::sync::LazyLock<Pool<Vec<TraceEvent>>> =
    std::sync::LazyLock::new(|| Pool::new(4, 8192));

/// Runtime-side trace recording (see [`GXHandle::trace_start`]).
/// What gets recorded is a function of the traced program's own event
/// stream, never of when control messages arrived: both budgets are
/// fixed at `trace_start`, only WORKED cycles (cycles that handed the
/// graph program events) count against `max_cycles`, and tripping
/// either cap silences the trace permanently.
struct TraceState {
    events: GPooled<Vec<TraceEvent>>,
    max_events: usize,
    max_cycles: u64,
    /// worked cycles since the last segment boundary
    worked_cycles: u64,
    capped_cycles: bool,
    capped_events: bool,
    waiter: Option<oneshot::Sender<Option<TraceSegment>>>,
}

impl TraceState {
    fn new(max_events: usize, max_cycles: u64) -> Self {
        Self {
            events: TRACE_EVENTS.take(),
            max_events,
            max_cycles,
            worked_cycles: 0,
            capped_cycles: false,
            capped_events: false,
            waiter: None,
        }
    }

    fn capped(&self) -> bool {
        self.capped_cycles || self.capped_events
    }

    fn record(&mut self, cycle: u64, id: ExprId, v: &Value) {
        if self.capped() {
            return;
        }
        if self.events.len() >= self.max_events {
            self.capped_events = true;
        } else {
            self.events.push(TraceEvent::Updated { cycle, id, value: v.clone() });
        }
    }

    /// Compile anchors count against neither budget.
    fn record_compiled(&mut self, cycle: u64, id: ExprId) {
        if !self.capped() {
            self.events.push(TraceEvent::Compiled { cycle, id });
        }
    }

    /// Bookkeeping at the end of `do_cycle` for the cycle that just
    /// ran; `worked` = the cycle delivered program events to the graph.
    fn cycle_end(&mut self, cycle: u64, worked: bool) {
        if worked {
            self.worked_cycles += 1;
            if self.worked_cycles >= self.max_cycles {
                self.capped_cycles = true;
            }
        }
        if self.capped() {
            self.resolve(cycle)
        }
    }

    fn wait(&mut self, res: oneshot::Sender<Option<TraceSegment>>, cycle: u64) {
        self.waiter = Some(res);
        if self.capped() {
            self.resolve(cycle)
        }
    }

    fn resolve(&mut self, end_cycle: u64) {
        if let Some(tx) = self.waiter.take() {
            let seg = TraceSegment {
                events: mem::replace(&mut self.events, TRACE_EVENTS.take()),
                end_cycle,
                capped_cycles: self.capped_cycles,
                capped_events: self.capped_events,
            };
            self.worked_cycles = 0;
            let _ = tx.send(Some(seg));
        }
    }
}

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
        | ExprKind::Catch { .. }
        | ExprKind::TypeDef { .. } => false,
        _ => true,
    }
}

/// Wrap a file's top-level Exprs in one synthetic `ExprKind::Do` so the
/// compiler produces one Node and fusion sees the whole file at once.
/// `Do` rather than `Module` because the last expression's value must
/// propagate out as the runtime output.
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

/// The idle-grace re-poll: when armed, wake the main loop after a
/// short delay so an idle verdict is confirmed on a second pass. A
/// spawned task mid-flight is invisible to every drainable queue, so
/// a single-pass idle test races it. Long-running tasks (timers, IO)
/// still read as idle.
async fn idle_grace(armed: bool) {
    if armed {
        time::sleep(Duration::from_millis(2)).await
    } else {
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
    resolvers: Resolvers,
    batch_pool: Pool<Vec<GXEvent>>,
    flags: BitFlags<CFlag>,
    /// A pending `WaitResultOrIdle` request: deliver the first value the
    /// watched expr emits, or `None` when the runtime next goes idle. See
    /// `GXHandle::wait_result_or_idle`.
    result_watch: Option<(ExprId, oneshot::Sender<Option<Value>>)>,
    /// Active trace recording, if any. See [`GXHandle::trace_start`].
    trace: Option<TraceState>,
    /// The session scope for statement-at-a-time compiles: a top-level
    /// `catch(e) expr` advances it so later inputs compile under its
    /// coverage. File loads do not touch it.
    scope: Scope,
}

impl<X: GXExt> GX<X> {
    pub(super) async fn new(mut cfg: GXConfig<X>) -> Result<Self> {
        let resolvers_default = |r: &mut Vec<ResolverRef>| match dirs::data_dir() {
            None => (),
            Some(dd) => r.push(FilesResolver::new(dd.join("graphix"), None)),
        };
        match std::env::var("GRAPHIX_MODPATH") {
            Err(_) => resolvers_default(&mut cfg.resolvers),
            Ok(mp) => {
                match parse_modpath(&cfg.resolver_factories, &mut cfg.ctx.libstate, &mp) {
                    Ok(r) => cfg.resolvers.extend(r),
                    Err(e) => {
                        error!("failed to parse GRAPHIX_MODPATH, using default {e:?}");
                        resolvers_default(&mut cfg.resolvers)
                    }
                }
            }
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
            resolvers: std::sync::Arc::from(cfg.resolvers),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            result_watch: None,
            trace: None,
            scope: Scope::root(),
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
        tasks: &mut Vec<(BindId, Value)>,
        custom_tasks: &mut Vec<(BindId, Box<dyn CustomBuiltinType>)>,
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
        // The store advances only at delivery (the Vacant arm); a repeat
        // set in one cycle is re-queued, not delivered. A patch resolves
        // against the store at delivery, so patches to one root land in
        // order on each other's result.
        macro_rules! push_var_event {
            ($id:expr, $u:expr) => {
                match self.event.variables.entry($id) {
                    Entry::Vacant(e) => {
                        let v = match $u {
                            VarUpdate::Set(v) => Some(v),
                            VarUpdate::Patch(path, v) => {
                                match self.ctx.rt.store_value(&$id) {
                                    Some(cur) => match place::write_path(&cur, &path, v) {
                                        Ok(nv) => Some(nv),
                                        Err(err) => {
                                            error!("write through a reference into {:?}: {err}", $id);
                                            None
                                        }
                                    },
                                    None => {
                                        error!("write through a reference into {:?}: no value to update", $id);
                                        None
                                    }
                                }
                            }
                        };
                        if let Some(v) = v {
                            self.ctx.rt.store_insert(
                                $id,
                                graphix_compiler::TagValue::fired(v.clone()),
                            );
                            // an ordinary runtime delivery is a FIRED event
                            e.insert(graphix_compiler::TagValue::fired(v));
                            if let Some(exps) = self.ctx.rt.by_ref.get(&$id) {
                                for id in exps.keys() {
                                    self.ctx.rt.updated.entry(*id).or_insert(false);
                                }
                            }
                        }
                    }
                    Entry::Occupied(_) => {
                        self.ctx.rt.var_updates.push_back(($id, $u));
                    }
                }
            };
        }
        for _ in 0..self.ctx.rt.var_updates.len() {
            let (id, v) = self.ctx.rt.var_updates.pop_front().unwrap();
            push_var_event!(id, v)
        }
        for (id, v) in tasks.drain(..) {
            push_var_event!(id, VarUpdate::Set(v))
        }
        for _ in 0..self.ctx.rt.custom_updates.len() {
            let (id, u) = self.ctx.rt.custom_updates.pop_front().unwrap();
            push_event!(id, u, custom, by_ref, custom_updates)
        }
        for (id, u) in custom_tasks.drain(..) {
            push_event!(id, u, custom, by_ref, custom_updates)
        }
        if let Err(e) = self.ctx.rt.ext.do_cycle(&mut self.event) {
            error!("could not marshall user events {e:?}")
        }
        // The cycle may run on a migrated worker thread.
        graphix_compiler::fusion::emit_helpers::set_interrupt_ptr(&self.ctx.control);
        let worked = !self.ctx.rt.updated.is_empty()
            || !self.event.variables.is_empty()
            || !self.event.custom.is_empty();
        // `block_in_place` keeps a wedged node from starving the IO tasks
        // and the caller that would `interrupt()`/`abort()` it; on
        // `current_thread` there is nowhere to migrate, so run inline.
        // The interrupt bit is meaningful only to the cycle in flight when
        // it is set: one that arrived while idle must not poison this cycle.
        self.ctx.control.clear_interrupt();
        let mut run_nodes = || {
            for (id, n) in self.nodes.iter_mut() {
                if let Some(init) = self.ctx.rt.updated.get(id) {
                    self.event.init = *init;
                    // Only a FIRED production becomes an event.
                    let tv = n.update(&mut self.ctx, &mut self.event);
                    if tv.is_fired() {
                        let v = tv.value_cloned();
                        let watched = matches!(
                            self.result_watch.as_ref(),
                            Some((wid, _)) if wid == id
                        );
                        if watched {
                            if let Some((_, tx)) = self.result_watch.take() {
                                let _ = tx.send(Some(v.clone()));
                            }
                        }
                        if let Some(tr) = self.trace.as_mut() {
                            tr.record(self.ctx.rt.cycle, *id, &v);
                        }
                        batch.push(GXEvent::Updated(*id, v))
                    }
                    // Diagnostics the update produced, attributed to this expression.
                    for d in self.ctx.diagnostics.drain(..) {
                        batch.push(GXEvent::Diagnostic(Some(*id), d));
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
        // Diagnostics produced outside a node update have no expression to
        // attribute to.
        for d in self.ctx.diagnostics.drain(..) {
            batch.push(GXEvent::Diagnostic(None, d));
        }
        if let Some(tr) = self.trace.as_mut() {
            tr.cycle_end(self.ctx.rt.cycle, worked);
        }
        self.ctx.rt.cycle += 1;
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
    }

    async fn process_input_batch(
        &mut self,
        tasks: &mut Vec<(BindId, Value)>,
        input: &mut Vec<ToGX<X>>,
        batch: &mut GPooled<Vec<GXEvent>>,
    ) {
        for m in input.drain(..) {
            match m {
                ToGX::WithCtx { f } => f(&mut self.ctx),
                ToGX::GetEnv { res } => {
                    let _ = res.send(self.ctx.env.clone());
                }
                ToGX::Check { path, resolvers, initial_scope, res } => {
                    let _ = res.send(self.check(&path, resolvers, initial_scope).await);
                }
                ToGX::Compile { text, rt, res } => {
                    let r = self.compile(rt, text).await;
                    self.record_compiled(&r);
                    let _ = res.send(r);
                }
                ToGX::Load { path, rt, res } => {
                    let r = self.load(rt, &path).await;
                    self.record_compiled(&r);
                    let _ = res.send(r);
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
                ToGX::Set { id, v } => tasks.push((id, v)),
                ToGX::SetMany { mut sets } => {
                    // One message ⇒ one input batch ⇒ one cycle (GXHandle::set_many).
                    for (id, v) in sets.drain(..) {
                        tasks.push((id, v))
                    }
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
                    // `do_cycle` runs right after this batch, so the watch gets one
                    // chance to fulfil Some before the idle check fulfils None. A
                    // second request supersedes a pending one (its sender drops).
                    self.result_watch = Some((id, res));
                }
                ToGX::TraceStart { max_events, max_cycles } => {
                    // Replacing an active trace drops its pending waiter and events.
                    self.trace = Some(TraceState::new(max_events, max_cycles));
                }
                ToGX::TraceWaitIdle { res } => match self.trace.as_mut() {
                    None => {
                        let _ = res.send(None);
                    }
                    // A capped trace resolves here; otherwise at the idle check or
                    // when a cap trips.
                    Some(tr) => tr.wait(res, self.ctx.rt.cycle),
                },
            }
        }
    }

    /// Record a [`TraceEvent::Compiled`] anchor for each expression of a
    /// successful `compile`/`load`; their init cycle is the next to run.
    fn record_compiled(&mut self, r: &Result<CompRes<X>>) {
        if let (Ok(cr), Some(tr)) = (r, self.trace.as_mut()) {
            for e in cr.exprs.iter() {
                tr.record_compiled(self.ctx.rt.cycle, e.id);
            }
        }
    }

    fn cycle_ready(&self) -> bool {
        !self.ctx.rt.updated.is_empty()
            || self.ctx.rt.var_updates.len() > 0
            || self.ctx.rt.custom_updates.len() > 0
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
        // Prune the static-resolution index by the outgoing batch's `<-`
        // targets rather than clearing it: stable cross-batch entries must
        // survive or resolution falls to the `store_value` fallback, whose
        // contents depend on whether the previous init cycle has run.
        for id in self.ctx.unstable_bindings.iter() {
            self.ctx.bind_to_lambda.remove(id);
        }
        self.ctx.unstable_bindings.clear();
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        for e in exprs.iter() {
            let (n, advanced) = graphix_compiler::compile_stmt(
                &mut self.ctx,
                flags,
                &self.scope,
                e.clone(),
            )
            .with_context(|| format!("compiling root expression {e}"))
            .with_context(|| ori.clone())?;
            self.scope = advanced;
            nodes.push(n);
        }
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
        // Prune the static-resolution index by the outgoing batch's `<-`
        // targets rather than clearing it: stable cross-batch entries must
        // survive or resolution falls to the `store_value` fallback, whose
        // contents depend on whether the previous init cycle has run.
        for id in self.ctx.unstable_bindings.iter() {
            self.ctx.bind_to_lambda.remove(id);
        }
        self.ctx.unstable_bindings.clear();
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        for e in exprs.iter() {
            let (n, advanced) = graphix_compiler::compile_stmt(
                &mut self.ctx,
                self.flags,
                &self.scope,
                e.clone(),
            )
            .with_context(|| ori.clone())?;
            self.scope = advanced;
            nodes.push(n);
        }
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
            source @ Source::Netidx(_) => {
                // Non-file transports are fetched by whichever resolver claims
                // the source.
                let fetch = self
                    .resolvers
                    .iter()
                    .find_map(|r| r.fetch_source(source))
                    .ok_or_else(|| anyhow!("no module resolver can load {source:?}"))?;
                let src = fetch.await?;
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
        resolver_override: Option<Vec<ResolverRef>>,
        initial_scope: Option<ArcStr>,
    ) -> Result<crate::CheckResult> {
        self.check_inner(source, resolver_override, initial_scope).await.map(|(_, r)| r)
    }

    /// Like `check`, but also returns the (post-resolve) Expr tree.
    async fn check_inner(
        &mut self,
        source: &Source,
        resolver_override: Option<Vec<ResolverRef>>,
        initial_scope: Option<ArcStr>,
    ) -> Result<(Arc<[Expr]>, crate::CheckResult)> {
        // The LSP shares one runtime across every checked file and never
        // executes a kernel; without a reset each file's kernels accumulate
        // in the persistent JIT module until finalize fails.
        if self.ctx.env.lsp_mode {
            self.ctx.fusion.reset_jit_for_check()?;
        }
        let env = self.ctx.env.clone();
        let prev_ide = if self.ctx.env.lsp_mode {
            self.ctx.env.ide.replace(Arc::new(parking_lot::Mutex::new(
                graphix_compiler::ide::Ide::new(),
            )))
        } else {
            None
        };
        let resolvers_for_call: Resolvers = match resolver_override {
            Some(v) => std::sync::Arc::from(v),
            None => self.resolvers.clone(),
        };
        let go = async {
            let st = Instant::now();
            info!("parse time: {:?}", st.elapsed());
            let scope = match &initial_scope {
                None => Scope::root(),
                Some(s) => {
                    let path =
                        ModPath(netidx_core::path::Path::root().append(s.as_str()));
                    self.ctx.env.unbind_scope_subtree(&path);
                    Scope { lexical: path, ..Scope::root() }
                }
            };
            let (ori, exprs) = self.load_exprs(source).await?;
            let exprs = try_join_all(exprs.iter().map(|e| {
                e.resolve_modules_in_scope(&scope.lexical, &resolvers_for_call)
            }))
            .await?;
            info!("resolve time: {:?}", st.elapsed());
            // Prune by the outgoing `<-` targets so check diagnostics resolve
            // the same way run-to-run; `Bind::delete` bounds the growth.
            for id in self.ctx.unstable_bindings.iter() {
                self.ctx.bind_to_lambda.remove(id);
            }
            let mut nodes: LPooled<Vec<_>> = LPooled::take();
            let mut scope = scope;
            for e in exprs.iter() {
                let res = graphix_compiler::compile_stmt(
                    &mut self.ctx,
                    self.flags,
                    &scope,
                    e.clone(),
                )
                .with_context(|| ori.clone());
                match res {
                    Ok((n, advanced)) => {
                        scope = advanced;
                        nodes.push(n);
                    }
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
        let output = exprs.last().map(|e| is_output_kind(&e.kind)).unwrap_or(false);
        let wrapped =
            wrap_file_in_do(Arc::from_iter(exprs.into_iter()), Arc::new(ori.clone()));
        let top_id = wrapped.id;
        // Prune the static-resolution index by the outgoing batch's `<-`
        // targets rather than clearing it: stable cross-batch entries must
        // survive or resolution falls to the `store_value` fallback, whose
        // contents depend on whether the previous init cycle has run.
        for id in self.ctx.unstable_bindings.iter() {
            self.ctx.bind_to_lambda.remove(id);
        }
        self.ctx.unstable_bindings.clear();
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
            .collect::<smallvec::SmallVec<[_; 2]>>();
        let fnode = genn::constant(v.clone());
        let mut n = genn::apply(fnode, Scope::root(), argn, &lb.typ, eid);
        self.ctx.begin_runtime_node(eid);
        graphix_compiler::check_and_fuse(&mut self.ctx, &mut n)?;
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
            last: self.ctx.rt.store_value(&id),
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
        let mut tasks: Vec<(BindId, Value)> = vec![];
        let mut custom_tasks: Vec<(BindId, Box<dyn CustomBuiltinType>)> = vec![];
        let mut input = vec![];
        // Consecutive apparently-idle passes; reset by any ready work.
        let mut idle_passes: u32 = 0;
        'main: loop {
            // Pending commands' response channels drop on return, so blocked
            // callers get an error instead of hanging.
            if self.ctx.control.aborted() {
                return Ok(());
            }
            macro_rules! peek {
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
                (input) => {
                    while let Ok(m) = to_rt.try_recv() {
                        input.push(m);
                    }
                };
                ($($item:tt),+) => {{
                    $(peek!($item));+
                }};
            }
            // Drain every non-blocking source before the idle test: an
            // undelivered update in a watch channel is pending work that
            // `cycle_ready` cannot see until it is drained.
            peek!(watches, tasks, var_watches, custom_tasks, input);
            let ready = self.cycle_ready()
                || !tasks.is_empty()
                || !custom_tasks.is_empty()
                || !input.is_empty();
            // A `WaitResultOrIdle` watcher resolves `None` once the runtime is
            // idle, confirmed on a second consecutive pass: the first may race
            // an in-flight spawned task.
            if !ready {
                let waiter = self.result_watch.is_some() || self.trace.is_some();
                if waiter && idle_passes == 0 {
                    idle_passes = 1;
                } else {
                    if let Some((_, tx)) = self.result_watch.take() {
                        let _ = tx.send(None);
                    }
                    if let Some(tr) = self.trace.as_mut() {
                        tr.resolve(self.ctx.rt.cycle);
                    }
                    idle_passes = 0;
                }
            } else {
                idle_passes = 0;
            }
            select! {
                _ = idle_grace(idle_passes > 0 && !ready) => {
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = join_or_wait(&mut self.ctx.rt.tasks) => {
                    if let Ok(up) = up {
                        tasks.push(up);
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = join_or_wait(&mut self.ctx.rt.custom_tasks) => {
                    if let Ok(up) = up {
                        custom_tasks.push(up);
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = self.ctx.rt.watches.next() => {
                    if let Some(mut up) = up {
                        for v in up.drain(..) {
                            custom_tasks.push(v);
                        }
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = self.ctx.rt.var_watches.next() => {
                    if let Some(mut up) = up {
                        for v in up.drain(..) {
                            tasks.push(v);
                        }
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                _ = or_never(ready) => {
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                n = to_rt.recv_many(&mut input, 100000) => {
                    if n == 0 {
                        break 'main Ok(())
                    }
                    peek!(watches, tasks, var_watches, custom_tasks);
                },
                r = self.ctx.rt.ext.update_sources() => {
                    if let Err(e) = r {
                        error!("failed to update custom event sources {e:?}")
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input);
                },
            }
            let mut batch = self.batch_pool.take();
            self.process_input_batch(&mut tasks, &mut input, &mut batch).await;
            self.do_cycle(&mut tasks, &mut custom_tasks, &mut to_rt, &mut input, batch)
                .await;
        }
    }
}
