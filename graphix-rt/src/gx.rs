use crate::{ProgramImage, RegistrationImage};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use enumflags2::BitFlags;
use futures::{StreamExt, future::try_join_all};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, ExecState, Node, Rt, Scope, compile,
    expr::{
        self, Expr, ExprId, ExprKind, FilesResolver, ModPath, ModuleKind, Origin,
        ResolverRef, Resolvers, RootFile, Source, parse_modpath,
    },
    ide::{Ide, IdeMode},
    image::ProgramRoot,
    node::{
        coretraits, genn,
        lambda::LambdaDef,
        place::{self, VarUpdate},
    },
    typ::{FnType, Type},
};
use indexmap::IndexMap;
use log::{debug, error, info, warn};
use netidx_value::{ValArray, Value};
use nohash::{BuildNoHashHasher, IntMap};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::{SmallVec, smallvec};
use std::{future, mem, result, time::Duration};
use tokio::{
    select,
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
    Ref, ToGX, TraceEvent, TraceSegment, rt::Via,
};

static TRACE_EVENTS: std::sync::LazyLock<Pool<Vec<TraceEvent>>> =
    std::sync::LazyLock::new(|| Pool::new(4, 8192));

static SETS: std::sync::LazyLock<Pool<Vec<(BindId, Value)>>> =
    std::sync::LazyLock::new(|| Pool::new(64, 64));

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
    /// The cycle a cap tripped in: a segment resolved later still ends
    /// there, so what the trace saw does not depend on when the waiter
    /// arrived.
    capped_at: Option<u64>,
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
            capped_at: None,
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
            self.capped_at = Some(cycle);
        } else {
            self.events.push(TraceEvent::Updated { cycle, id, value: v.clone() });
        }
    }

    /// A compile anchor trips neither cap, though it takes a place among
    /// the segment's events.
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
                self.capped_at.get_or_insert(cycle);
            }
        }
        if let Some(at) = self.capped_at {
            self.resolve(at)
        }
    }

    fn wait(&mut self, res: oneshot::Sender<Option<TraceSegment>>, cycle: u64) {
        self.waiter = Some(res);
        match self.capped_at {
            Some(at) => self.resolve(at),
            None if self.capped() => self.resolve(cycle.saturating_sub(1)),
            None => (),
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
        | ExprKind::TypeDef { .. }
        | ExprKind::Trait(_)
        | ExprKind::Impl(_) => false,
        _ => true,
    }
}

/// Add each origin under `root` to `sources` once: a file's, each
/// module's and each interface's.
fn program_sources(root: &Expr, sources: &mut Vec<Arc<Origin>>) {
    let mut add = |o: &Arc<Origin>| {
        if !sources.iter().rev().any(|s| Arc::ptr_eq(s, o)) {
            sources.push(o.clone())
        }
    };
    let mut todo = vec![root];
    while let Some(e) = todo.pop() {
        add(&e.ori);
        if let ExprKind::Module {
            value: ModuleKind::Resolved { sig: Some(sig), .. },
            ..
        } = &e.kind
        {
            for item in sig.iter() {
                if let Some(o) = &item.ori {
                    add(o)
                }
            }
        }
        e.for_each_child(&mut |c| todo.push(c));
    }
}

/// Wrap a file's top-level Exprs in one synthetic `ExprKind::Block` so the
/// compiler produces one Node and fusion sees the whole file at once. A
/// block rather than a module, because the last expression's value must
/// propagate out as the runtime output.
fn wrap_file_in_block(exprs: Arc<[Expr]>, ori: Arc<Origin>) -> Expr {
    Expr {
        id: ExprId::new(),
        ori,
        pos: Default::default(),
        kind: ExprKind::Block { exprs },
        dec: None,
        str_form: Default::default(),
        end: Default::default(),
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
    ctx: ExecState<GXRt<X>, X::UserEvent>,
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
    /// Pending `WaitIdle` requests, answered at the next idle verdict.
    idle_waiters: Vec<oneshot::Sender<()>>,
    /// The program compiled or restored at construction; a compile
    /// failure is kept for the embedder to report, the runtime starts.
    program: Option<Result<ProgramRoot, String>>,
    /// The registration was restored from an image, not compiled.
    restored: bool,
    /// Active trace recording, if any. See [`GXHandle::trace_start`].
    trace: Option<TraceState>,
    /// The session scope for statement-at-a-time compiles: a top-level
    /// `catch(e) expr` advances it so later inputs compile under its
    /// coverage. File loads do not touch it.
    scope: Scope,
}

impl<X: GXExt> GX<X> {
    /// Drop the outgoing batch's `<-` targets from the static-resolution
    /// index: a later batch's call through one dispatches dynamically.
    fn prune_static_resolution(&mut self) {
        for id in self.ctx.cx.batch_connect_targets.iter() {
            self.ctx.cx.bind_to_lambda.remove(id);
        }
    }

    pub(super) async fn new(mut cfg: GXConfig<X>) -> Result<Self> {
        let st_new = Instant::now();
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
                        // CR claude for claude: [bug] When parse_modpath refuses one
                        // entry (graphix-types/src/expr/resolver.rs:170: no scheme, an
                        // unknown scheme, or the empty entry a trailing comma leaves),
                        // this replaces the whole list with the data dir and logs where
                        // the shell shows nothing, so every valid entry is lost and
                        // `mod x;` says only 'could not be found'. The book documents
                        // such entries: book/src/shell.md:383
                        // `GRAPHIX_MODPATH=/opt/graphix-libs`, and
                        // book/src/modules/implementation.md says an entry without
                        // `netidx:` is a file path. Skip empty entries and report a
                        // refused one to the user, or accept a bare path as a file as
                        // the book says. Separately, a list that parses drops the data
                        // dir, while both book pages keep it in the search path. probe:
                        // design/review-2026-10-05/repro/t-format-resolver-12.sh
                        // (t-format-resolver-12)
                        error!("failed to parse GRAPHIX_MODPATH, using default {e:?}");
                        resolvers_default(&mut cfg.resolvers)
                    }
                }
            }
        };
        let mut ctx = cfg.ctx;
        if cfg.lsp_mode {
            ctx.env.ide = IdeMode::Lsp(None);
        }
        let mut t = Self {
            ctx,
            nodes: IndexMap::default(),
            callables: IntMap::default(),
            sub: cfg.sub,
            resolvers: std::sync::Arc::from(cfg.resolvers),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            result_watch: None,
            idle_waiters: Vec::new(),
            trace: None,
            scope: Scope::root(),
            program: None,
            restored: false,
        };
        info!("runtime construction before the root: {:?}", st_new.elapsed());
        t.trace = cfg
            .trace
            .map(|(max_events, max_cycles)| TraceState::new(max_events, max_cycles));
        let st = Instant::now();
        let RegistrationImage { restore, save } = cfg.registration.unwrap_or_default();
        for (what, bytes) in restore {
            match t.restore_registration(bytes) {
                Ok(()) => {
                    t.restored = true;
                    break;
                }
                Err(e) => warn!("{what}: {e}"),
            }
        }
        if !t.restored {
            if let Some(root) = cfg.root {
                // The root declares packages; fusing their constants
                // buys nothing and would put kernels in the image. Its
                // flags are fixed, so an image is a function of its root.
                t.compile_root(CFlag::FusionDisabled.into(), root).await?;
            }
            if let Some(tx) = save {
                let _ = tx.send(t.registration_image());
            }
        }
        info!("root init time: {:?}", st.elapsed());
        let st_after = Instant::now();
        if t.program.is_none()
            && let Some(source) = cfg.program
        {
            let st = Instant::now();
            let mut sources = cfg.program_image.as_ref().map(|_| vec![]);
            match t.load_program(&source, sources.as_mut()).await {
                Ok(root) => {
                    t.program = Some(Ok(root));
                    info!("program init time: {:?}", st.elapsed());
                    if let (Some(tx), Some(sources)) = (cfg.program_image, sources) {
                        let image = t.registration_image();
                        let _ =
                            tx.send(image.map(|image| ProgramImage { image, sources }));
                    }
                }
                Err(e) => t.program = Some(Err(format!("{e:?}"))),
            }
        }
        if let (Some(Ok(root)), Some(tr)) = (&t.program, t.trace.as_mut()) {
            tr.record_compiled(t.ctx.rt.cycle, root.id);
        }
        info!("runtime construction after the root: {:?}", st_after.elapsed());
        Ok(t)
    }

    fn registration_image(&self) -> Result<Bytes> {
        let nodes: Vec<(ExprId, &Node<GXRt<X>, X::UserEvent>)> =
            self.nodes.iter().map(|(id, n)| (*id, n)).collect();
        self.ctx
            .write_registration(
                &nodes,
                &self.scope,
                self.program.as_ref().and_then(|p| p.as_ref().ok()),
            )
            .map_err(|e| anyhow!("writing the registration image: {e:?}"))
    }

    fn restore_registration(&mut self, bytes: Bytes) -> Result<()> {
        let reg = self
            .ctx
            .view()
            .read_registration(bytes)
            .map_err(|e| anyhow!("reading the registration image: {e:?}"))?;
        for (id, n) in reg.nodes {
            self.ctx.rt.updated.insert(id, true);
            self.nodes.insert(id, n);
        }
        self.scope = reg.scope;
        self.program = reg.program.map(Ok);
        Ok(())
    }

    /// One cycle's updates of the scheduled roots; what fired goes into
    /// `batch`.
    fn update_nodes(&mut self, batch: &mut GPooled<Vec<GXEvent>>) {
        for (id, n) in self.nodes.iter_mut() {
            if let Some(init) = self.ctx.rt.updated.get(id) {
                self.ctx.event.init = *init;
                // Only a FIRED production becomes an event.
                let tv = n.update(&mut self.ctx.view());
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
            }
        }
    }

    async fn do_cycle(
        &mut self,
        tasks: &mut Vec<(BindId, Value)>,
        sets: &mut Vec<GPooled<Vec<(BindId, Value)>>>,
        custom_tasks: &mut Vec<(BindId, Box<dyn CustomBuiltinType>)>,
        to_rt: &mut UnboundedReceiver<ToGX<X>>,
        input: &mut Vec<ToGX<X>>,
        mut batch: GPooled<Vec<GXEvent>>,
    ) {
        debug_assert!(
            !self.ctx.view().deferred_pending(),
            "compiled references left unreplayed"
        );
        self.ctx.view().apply_deferred();
        // A variable takes one delivery a cycle: what waited lands first,
        // each variable's oldest write, then this cycle's input, which waits
        // behind anything its variable has waiting or was given.
        let mut due: LPooled<Vec<(BindId, VarUpdate, Via)>> = LPooled::take();
        self.ctx.rt.var_updates.take(&mut due);
        for (id, u, via) in due.drain(..) {
            self.deliver(id, u, via);
        }
        for (id, v) in tasks.drain(..) {
            self.ctx.rt.timers.remove(&id);
            self.write(id, VarUpdate::Set(v), Via::Reply);
        }
        for set in sets.drain(..) {
            self.write_set(set);
        }
        let mut due: Vec<(BindId, Box<dyn CustomBuiltinType>, Via)> = Vec::new();
        self.ctx.rt.custom_updates.take(&mut due);
        for (id, u, via) in due {
            self.deliver_custom(id, u, via);
        }
        for (id, u) in custom_tasks.drain(..) {
            if self.ctx.event.custom.lock().contains_key(&id)
                || self.ctx.rt.custom_updates.has(&id)
            {
                self.ctx.rt.custom_updates.push(id, u, Via::Reply);
            } else {
                self.deliver_custom(id, u, Via::Reply);
            }
        }
        if let Err(e) = self.ctx.rt.ext.do_cycle(&mut self.ctx.event) {
            error!("could not marshall user events {e:?}")
        }
        let worked = !self.ctx.rt.updated.is_empty()
            || !self.ctx.event.variables.is_empty()
            || !self.ctx.event.custom.lock().is_empty();
        // `block_in_place` keeps a wedged node from starving the IO tasks
        // and the caller that would `interrupt()`/`abort()` it; on
        // `current_thread` there is nowhere to migrate, so run inline.
        // The interrupt bit is meaningful only to the cycle in flight when
        // it is set: one that arrived while idle must not poison this cycle.
        self.ctx.control.clear_interrupt();
        let control = self.ctx.control.clone();
        let mut run_nodes = || {
            // On the thread that runs the nodes: the task may migrate
            // between cycles.
            let _interrupt = graphix_compiler::InterruptScope::new(&control);
            self.update_nodes(&mut batch)
        };
        if matches!(
            tokio::runtime::Handle::current().runtime_flavor(),
            tokio::runtime::RuntimeFlavor::CurrentThread
        ) {
            run_nodes();
        } else {
            tokio::task::block_in_place(run_nodes);
        }
        // before the send loop, which may compile roots for the next cycle
        self.ctx.event.clear();
        self.ctx.rt.updated.clear();
        if let Some(tr) = self.trace.as_mut() {
            tr.cycle_end(self.ctx.rt.cycle, worked);
        }
        self.ctx.rt.cycle += 1;
        loop {
            match self.sub.send_timeout(batch, Duration::from_millis(100)).await {
                Ok(()) => break,
                // a subscriber gone after an abort is the abort
                Err(SendTimeoutError::Closed(_)) if self.ctx.control.aborted() => break,
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
                    self.process_input_batch(sets, input, &mut batch).await;
                }
            }
        }
    }

    /// A write through the reference `cell`: a place patches its root, a
    /// chained reference sets its binding, a chainless one sets the cell.
    fn set_deref(
        &mut self,
        cell: BindId,
        v: Value,
        sets: &mut Vec<GPooled<Vec<(BindId, Value)>>>,
    ) {
        match self.ctx.rt.ref_path(&cell).cloned() {
            Some((root, path)) => self.ctx.rt.patch_var(root, path, v),
            None => {
                let id = self.ctx.env.byref_chain.get(&cell).copied().unwrap_or(cell);
                let mut one = SETS.take();
                one.push((id, v));
                sets.push(one)
            }
        }
    }

    /// Readers of `id` update this cycle.
    fn wake_readers(&mut self, id: &BindId) {
        if let Some(exps) = self.ctx.rt.by_ref.get(id) {
            for e in exps.keys() {
                self.ctx.rt.updated.entry(*e).or_insert(false);
            }
        }
    }

    /// Deliver `u` to `id` this cycle: into the store, which advances only
    /// here (a patch resolves against it now, so patches to one root land
    /// in order on each other's result), and the event as FIRED. A reply
    /// nothing references any more goes nowhere.
    fn deliver(&mut self, id: BindId, u: VarUpdate, via: Via) {
        if matches!(via, Via::Reply) && !self.ctx.rt.by_ref.contains_key(&id) {
            return;
        }
        let v = match u {
            VarUpdate::Set(v) => v,
            VarUpdate::Patch(path, v) => {
                let Some(cur) = self.ctx.rt.store_value(&id) else {
                    error!("write through a reference into {id:?}: no value to update");
                    return;
                };
                let written = coretraits::with_hooks(&mut self.ctx.view(), || {
                    place::write_path(&cur, &path, v)
                });
                match written {
                    Ok(nv) => nv,
                    Err(err) => {
                        error!("write through a reference into {id:?}: {err}");
                        return;
                    }
                }
            }
        };
        self.ctx.rt.store_insert(id, graphix_compiler::TagValue::fired(v.clone()));
        self.ctx.event.variables.insert(id, graphix_compiler::TagValue::fired(v));
        self.wake_readers(&id);
    }

    /// `id`'s delivery this cycle, or its wait behind what `id` has waiting
    /// or was given.
    fn busy(&self, id: &BindId) -> bool {
        self.ctx.event.variables.contains_key(id) || self.ctx.rt.var_updates.has(id)
    }

    fn write(&mut self, id: BindId, u: VarUpdate, via: Via) {
        if self.busy(&id) {
            self.ctx.rt.var_updates.push(id, u, via);
        } else {
            self.deliver(id, u, via);
        }
    }

    /// Writes that land in one cycle: now, or together in the first cycle
    /// where each is first for its variable. A set naming a variable twice
    /// cannot, and lands write by write.
    fn write_set(&mut self, mut set: GPooled<Vec<(BindId, Value)>>) {
        let distinct = set
            .iter()
            .enumerate()
            .all(|(i, (a, _))| set[..i].iter().all(|(b, _)| a != b));
        if !distinct || set.len() == 1 {
            for (id, v) in set.drain(..) {
                self.write(id, VarUpdate::Set(v), Via::Write);
            }
        } else if set.iter().any(|(id, _)| self.busy(id)) {
            let members: Arc<[BindId]> = Arc::from_iter(set.iter().map(|(id, _)| *id));
            for (id, v) in set.drain(..) {
                self.ctx.rt.var_updates.push(
                    id,
                    VarUpdate::Set(v),
                    Via::Set(members.clone()),
                );
            }
        } else {
            for (id, v) in set.drain(..) {
                self.deliver(id, VarUpdate::Set(v), Via::Write);
            }
        }
    }

    fn deliver_custom(&mut self, id: BindId, u: Box<dyn CustomBuiltinType>, via: Via) {
        if matches!(via, Via::Reply) && !self.ctx.rt.by_ref.contains_key(&id) {
            return;
        }
        self.ctx.event.custom.lock().insert(id, u);
        self.wake_readers(&id);
    }

    async fn process_input_batch(
        &mut self,
        sets: &mut Vec<GPooled<Vec<(BindId, Value)>>>,
        input: &mut Vec<ToGX<X>>,
        batch: &mut GPooled<Vec<GXEvent>>,
    ) {
        for m in input.drain(..) {
            match m {
                ToGX::WithCtx { f } => f(&mut self.ctx),
                ToGX::GetEnv { res } => {
                    let _ = res.send(self.ctx.env.clone());
                }
                ToGX::Check { path, resolvers, initial_scope, expr_types, res } => {
                    let r = self.check(&path, resolvers, initial_scope, expr_types).await;
                    let _ = res.send(r);
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
                        n.delete(&mut self.ctx.view());
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
                    let mut one = SETS.take();
                    one.push((id, v));
                    sets.push(one)
                }
                // one message, one set, one cycle (GXHandle::set_many)
                ToGX::SetMany { sets: set } => sets.push(set),
                ToGX::DeleteCallable { id } => self.delete_callable(id),
                ToGX::Call { id, args } => {
                    if let Err(e) = self.call_callable(id, args, sets) {
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
                // the root's owner deletes the program: it is handed out once
                ToGX::Program { res } => {
                    let root = match self.program.take() {
                        Some(Ok(root)) => {
                            self.program =
                                Some(Err("the program was handed out already".into()));
                            Some(Ok(root))
                        }
                        other => {
                            self.program = other.clone();
                            other
                        }
                    };
                    let _ = res.send((root, self.ctx.env.clone()));
                }
                ToGX::SetDeref { cell, v } => self.set_deref(cell, v, sets),
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
                        store_len: self.ctx.rt.store.len(),
                        lambda_defs_len: self.ctx.lambda_defs.len(),
                        restored: self.restored,
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
                ToGX::WaitIdle { res } => self.idle_waiters.push(res),
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
            || !self.ctx.rt.var_updates.is_empty()
            || !self.ctx.rt.custom_updates.is_empty()
            || self.ctx.rt.ext.is_ready()
    }

    async fn compile_root(&mut self, flags: BitFlags<CFlag>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())
            .with_context(|| format!("parsing the root module {ori}"))?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        for e in exprs.iter() {
            let (n, advanced) = graphix_compiler::compile_stmt(
                &mut self.ctx.view(),
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
        Ok(())
    }

    async fn compile(&mut self, rt: GXHandle<X>, text: ArcStr) -> Result<CompRes<X>> {
        let scope = Scope::root();
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        for e in exprs.iter() {
            // CR claude for claude: [bug] When a later statement fails, this `?` returns
            // after the earlier statements of the same input passed their checks. Their
            // names stay in the env, their lambdas in lambda_defs and bind_to_lambda,
            // their refs in by_ref and a top-level catch in self.scope, while `nodes`
            // drops them without `delete`. In the REPL, after `let x = 41; let y =
            // nosuch`, `x + 1` has type i64 and never produces a value. After `let f =
            // |a| a + 1; let z = nosuch`, `f(1)` prints 2 but `f` has no value. After
            // `catch(e) println(e); let w = nosuch`, every later `error(`E)?` is lost,
            // and the unhandled warning is not printed either. On error, delete the
            // compiled nodes and restore the env, the other registries and self.scope,
            // or install the statements that succeeded; probe:
            // design/review-2026-10-05/repro/c-lib-04.py (c-lib-04)
            let (n, advanced) = graphix_compiler::compile_stmt(
                &mut self.ctx.view(),
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

    async fn load_exprs(
        &self,
        source: &Source,
        resolvers: &Resolvers,
    ) -> Result<(Origin, Arc<[Expr]>)> {
        let (ori, exprs) = match source {
            Source::File(file) => {
                let overrides = resolvers.iter().find_map(|r| r.overrides());
                let root = RootFile::load(file, overrides.as_ref()).await?;
                // CR claude for claude: [bug] A script root's .gxi is only half applied.
                // RootFile::load splices its types, uses, mods and traits into the
                // script, but this line drops root.sig, so the vals are never checked.
                // `graphix --check foo.gx` and the LSP check a lone module (one that no
                // file's `mod` reaches) this way. An implementation that does not match
                // its interface therefore passes. A valid module is refused when its
                // .gxi puts a type, use or trait after the val of its last binding,
                // because the splice puts that declaration in the file block's value
                // slot ("a type definition is not an expression", reported at the
                // .gxi); through `mod foo;` the same pair checks correctly. probe:
                // design/review-2026-10-05/repro/t-format-resolver-03.sh
                // (t-format-resolver-03)
                (root.ori, root.exprs)
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
        expr_types: bool,
    ) -> Result<crate::CheckResult> {
        self.check_inner(source, resolver_override, initial_scope, expr_types)
            .await
            .map(|(_, r)| r)
    }

    /// Like `check`, but also returns the (post-resolve) Expr tree.
    async fn check_inner(
        &mut self,
        source: &Source,
        resolver_override: Option<Vec<ResolverRef>>,
        initial_scope: Option<ArcStr>,
        expr_types: bool,
    ) -> Result<(Arc<[Expr]>, crate::CheckResult)> {
        let env = self.ctx.env.clone();
        if let IdeMode::Lsp(sink) = &mut self.ctx.cx.env.ide {
            *sink = Some(Arc::new(parking_lot::Mutex::new(Ide::new())));
        }
        let resolvers_for_call: Resolvers = match resolver_override {
            Some(v) => std::sync::Arc::from(v),
            None => self.resolvers.clone(),
        };
        let go = async {
            let st = Instant::now();
            // A package root is the body of `mod <package>`, recompiled
            // over the copy registered at startup.
            let (ori, exprs, modules_at) = match (&initial_scope, source) {
                (Some(name), Source::File(file)) => {
                    let path =
                        ModPath(netidx_core::path::Path::root().append(name.as_str()));
                    self.ctx.env.unbind_scope_subtree(&path);
                    // a package this binary was not built with is still
                    // the root `package::` names
                    self.ctx.env.package_roots.insert(name.clone());
                    let overrides = resolvers_for_call.iter().find_map(|r| r.overrides());
                    let root = RootFile::load(file, overrides.as_ref()).await?;
                    let ori = root.ori.clone();
                    (ori, Arc::from_iter([root.into_module(name.clone())]), path)
                }
                (Some(_), _) => bail!("only a file can be checked as a package root"),
                (None, _) => {
                    let (ori, exprs) =
                        self.load_exprs(source, &resolvers_for_call).await?;
                    (ori, exprs, ModPath::root())
                }
            };
            let exprs =
                try_join_all(exprs.iter().map(|e| {
                    e.resolve_modules_in_scope(&modules_at, &resolvers_for_call)
                }))
                .await?;
            info!("parse and resolve time: {:?}", st.elapsed());
            self.prune_static_resolution();
            // the check alone: definitions and call sites, no elaboration;
            // `--expand` prints each instance's step boundaries, so it builds
            let flags = if self.flags.contains(CFlag::ExpandSeq) {
                self.flags
            } else {
                self.flags | CFlag::CheckOnly
            };
            let mut nodes: LPooled<Vec<_>> = LPooled::take();
            let res = match &initial_scope {
                Some(_) => exprs.iter().try_for_each(|e| {
                    let (n, _) = graphix_compiler::compile_stmt(
                        &mut self.ctx.view(),
                        flags,
                        &Scope::root(),
                        e.clone(),
                    )?;
                    nodes.push(n);
                    Ok(())
                }),
                // A script checks as it runs: its file is one block.
                None => {
                    let stmts = Arc::from_iter(exprs.iter().cloned());
                    let spec = wrap_file_in_block(stmts.clone(), Arc::new(ori.clone()));
                    // CR claude for claude: [bug] The check compiles a script with its
                    // names at `/` (compile_script at Scope::root()). load_program, the
                    // run, compiles the same file as a Block that compile() scopes at
                    // `/#do<id>`, so any verdict that depends on the scope can differ.
                    // `self::` anchors at mod_root, which strips the `#do` level back
                    // to `/`. As a result, `self::a`, `self::T` and `use self::m::x`
                    // over a script's own top-level names pass `--check` and the LSP
                    // but the run refuses them, and the check refuses `mod str;`
                    // (duplicate module at `/str`) while the run accepts it. probe:
                    // design/review-2026-10-05/repro/t-typ-mod-04.sh. Compile both at
                    // one scope, or anchor `self::` at a script's `#do` level as
                    // Env::package_root does for `package::`, then correct CLAUDE.md's
                    // "as it runs, with its names at the root". (t-typ-mod-04)
                    graphix_compiler::compile_script(
                        &mut self.ctx.view(),
                        flags,
                        &Scope::root(),
                        spec,
                        &stmts,
                    )
                    .map(|n| nodes.push(n))
                }
            };
            if let Err(e) = res.with_context(|| ori.clone()) {
                for mut n in nodes.drain(..) {
                    n.delete(&mut self.ctx.view());
                }
                return Err(e);
            }
            let env = self.ctx.env.clone();
            let mut ide = match self.ctx.env.ide.sink() {
                None => Ide::new(),
                Some(ide) => mem::replace(&mut *ide.lock(), Ide::new()),
            };
            if expr_types {
                graphix_compiler::record_expr_types(&nodes, &mut ide.expr_types);
            }
            for mut n in nodes.drain(..) {
                n.delete(&mut self.ctx.view());
            }
            Ok((Arc::from_iter(exprs), crate::CheckResult { env, ide }))
        };
        let res = go.await;
        self.ctx.env = env;
        res
    }

    async fn load(&mut self, rt: GXHandle<X>, source: &Source) -> Result<CompRes<X>> {
        let ProgramRoot { id, output, typ } = self.load_program(source, None).await?;
        let res = smallvec![CompExp { id, output, typ, rt: rt.clone() }];
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    /// Compile the program in `source`, adding every source its compile
    /// read to `sources`.
    async fn load_program(
        &mut self,
        source: &Source,
        sources: Option<&mut Vec<Arc<Origin>>>,
    ) -> Result<ProgramRoot> {
        let scope = Scope::root();
        let st = Instant::now();
        let (ori, exprs) = self.load_exprs(source, &self.resolvers).await?;
        info!("parse time: {:?}", st.elapsed());
        let st = Instant::now();
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers)))
                .await?;
        info!("resolve time: {:?}", st.elapsed());
        let output = exprs.last().map(|e| is_output_kind(&e.kind)).unwrap_or(false);
        let wrapped =
            wrap_file_in_block(Arc::from_iter(exprs.into_iter()), Arc::new(ori.clone()));
        if let Some(sources) = sources {
            program_sources(&wrapped, sources);
        }
        let id = wrapped.id;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let n = compile(&mut self.ctx.view(), self.flags, &scope, wrapped)
            .with_context(|| ori.clone())?;
        let typ = n.typ().clone();
        self.nodes.insert(id, n);
        self.ctx.rt.updated.insert(id, true);
        Ok(ProgramRoot { id, output, typ })
    }

    fn compile_callable(&mut self, v: Value, rt: GXHandle<X>) -> Result<Callable<X>> {
        let lb = v
            .downcast_ref::<LambdaDef<GXRt<X>, X::UserEvent>>()
            .ok_or_else(|| anyhow!("invalid lambda {v}"))?;
        let eid = ExprId::new();
        // the call's own instance: typing the argument references with the
        // definition's cells would merge this site's copy into them. A
        // defaulted label is left out, and the bind fills its default.
        let ftype = lb.typ.instantiate(&nohash::IntSet::default());
        let ftype = FnType {
            args: ftype.args.iter().filter(|a| !a.has_default()).cloned().collect(),
            ..ftype
        };
        let args: Box<[BindId]> = ftype.args.iter().map(|_| BindId::new()).collect();
        let argn = ftype.args.iter().zip(args.iter());
        let argn = argn
            .map(|(arg, id)| {
                genn::reference(&mut self.ctx.view(), *id, arg.typ.clone(), eid)
            })
            .collect::<smallvec::SmallVec<[_; 2]>>();
        let fnode = genn::constant(v.clone(), Type::Fn(lb.typ.clone()));
        let mut n = genn::apply(fnode, Scope::root(), argn, &ftype, eid);
        self.ctx.view().begin_runtime_node(eid);
        graphix_compiler::check_and_fuse(&mut self.ctx.view(), self.flags, &mut n)?;
        // its init runs in the next cycle, as a compiled root's does
        self.ctx.rt.updated.insert(eid, true);
        let cid = CallableId::new();
        self.callables.insert(cid, CallableInt { expr: eid, args });
        self.nodes.insert(eid, n);
        let env = self.ctx.env.clone();
        Ok(Callable { expr: eid, rt, env, id: cid, lambda: lb.id, typ: ftype })
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
        let n = genn::reference(&mut self.ctx.view(), id, typ.clone(), eid);
        self.ctx.view().apply_deferred();
        self.nodes.insert(eid, n);
        Ok(Ref { id: eid, bid: id, typ, last: self.ctx.rt.store_value(&id), rt })
    }

    fn call_callable(
        &mut self,
        id: CallableId,
        args: ValArray,
        sets: &mut Vec<GPooled<Vec<(BindId, Value)>>>,
    ) -> Result<()> {
        let c =
            self.callables.get(&id).ok_or_else(|| anyhow!("unknown callable {id:?}"))?;
        if args.len() != c.args.len() {
            bail!("expected {} arguments", c.args.len());
        }
        let mut set = SETS.take();
        set.extend(c.args.iter().zip(args.iter()).map(|(id, v)| (*id, v.clone())));
        sets.push(set);
        Ok(())
    }

    fn delete_callable(&mut self, id: CallableId) {
        if let Some(c) = self.callables.remove(&id) {
            for a in c.args.iter() {
                self.ctx.rt.store_remove(a);
            }
            if let Some(mut n) = self.nodes.shift_remove(&c.expr) {
                n.delete(&mut self.ctx.view())
            }
        }
    }

    pub(super) async fn run(
        mut self,
        mut to_rt: tmpsc::UnboundedReceiver<ToGX<X>>,
    ) -> Result<()> {
        let mut tasks: Vec<(BindId, Value)> = vec![];
        let mut sets: Vec<GPooled<Vec<(BindId, Value)>>> = vec![];
        let mut custom_tasks: Vec<(BindId, Box<dyn CustomBuiltinType>)> = vec![];
        let mut input = vec![];
        // Consecutive apparently-idle passes; reset by any ready work.
        let mut idle_passes: u32 = 0;
        let mut first_cycle = true;
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
            // An idle waiter resolves once the runtime is idle, confirmed on
            // a second consecutive pass: the first may race an in-flight
            // spawned task.
            if !ready {
                let waiter = self.result_watch.is_some()
                    || self.trace.is_some()
                    || !self.idle_waiters.is_empty();
                if waiter && idle_passes == 0 {
                    idle_passes = 1;
                } else {
                    if let Some((_, tx)) = self.result_watch.take() {
                        let _ = tx.send(None);
                    }
                    for tx in self.idle_waiters.drain(..) {
                        let _ = tx.send(());
                    }
                    if let Some(tr) = self.trace.as_mut() {
                        tr.resolve(self.ctx.rt.cycle.saturating_sub(1));
                    }
                    idle_passes = 0;
                }
            } else {
                idle_passes = 0;
            }
            let mut woke = true;
            select! {
                _ = idle_grace(idle_passes > 0 && !ready) => {
                    woke = false;
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
            // the grace confirms idleness only across two quiet passes in a row
            if woke {
                idle_passes = 0;
            }
            // an abort that landed while waiting ends the loop before the
            // next cycle
            if self.ctx.control.aborted() {
                return Ok(());
            }
            let mut batch = self.batch_pool.take();
            self.process_input_batch(&mut sets, &mut input, &mut batch).await;
            let st = Instant::now();
            self.do_cycle(
                &mut tasks,
                &mut sets,
                &mut custom_tasks,
                &mut to_rt,
                &mut input,
                batch,
            )
            .await;
            if first_cycle {
                first_cycle = false;
                info!("first cycle time: {:?}", st.elapsed());
            }
        }
    }
}
