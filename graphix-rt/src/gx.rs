use crate::RegistrationImage;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use enumflags2::BitFlags;
use futures::{StreamExt, future::try_join_all};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, ExecState, Node, Rt, Scope, compile,
    expr::{
        self, Expr, ExprId, ExprKind, FilesResolver, ModPath, Origin, ResolverRef,
        Resolvers, RootFile, Source, parse_modpath,
    },
    ide::{Ide, IdeMode},
    image::ProgramRoot,
    node::{
        coretraits, genn,
        lambda::LambdaDef,
        place::{self, VarUpdate},
    },
    typ::Type,
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
use std::{collections::hash_map::Entry, future, mem, result, time::Duration};
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
            None if self.capped() => self.resolve(cycle),
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
        // CR claude for claude: [bug] Trait and Impl are missing from the non-output
        // kinds, so a REPL `trait`/`impl` line compiles as output. The shell prints `-:
        // _`, moves the CompExp into its output and waits for Ctrl-C, swallowing
        // whatever is typed. The Ctrl-C that ends the wait drops the CompExp, which
        // deletes the declaration, and a following `impl Shw for i64 {..}` fails with
        // 'no trait `Shw` in scope'. A trait or impl on its own line therefore cannot
        // be declared at the REPL (probe: design/review-2026-10-05/repro/rt-09.py). Add
        // ExprKind::Trait(_) and ExprKind::Impl(_) to the false arm. (rt-09)
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

/// Wrap a file's top-level Exprs in one synthetic `ExprKind::Block` so the
/// compiler produces one Node and fusion sees the whole file at once.
/// `Do` rather than `Module` because the last expression's value must
/// propagate out as the runtime output.
// CR claude for claude: [doc-drift] ExprKind::Do no longer exists: this builds an
// ExprKind::Block, but its name and the doc above (`Do` rather than `Module`) still say
// Do. Rename it wrap_file_in_block and say Block in the doc. The same dead term is in
// graphix-fuzz/src/typemorph.rs:601, graphix-fuzz/src/mutate.rs:459 and 467, and
// stdlib/graphix-tests/src/lib_tests/module_stmt.rs:12, and StepKind::Do
// (graphix-fuzz/src/generate/reactive.rs:59) generates a `{ .. }` block.
// (x-expr-walks-08)
fn wrap_file_in_do(exprs: Arc<[Expr]>, ori: Arc<Origin>) -> Expr {
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
        let (image, save) = match cfg.registration {
            Some(RegistrationImage::Load(bytes)) => (Some(bytes), None),
            Some(RegistrationImage::Save(tx)) => (None, Some(tx)),
            None => (None, None),
        };
        t.restored = match image {
            None => false,
            Some(bytes) => match t.restore_registration(bytes) {
                Ok(()) => true,
                Err(e) => {
                    warn!("{e}; compiling cold");
                    false
                }
            },
        };
        if !t.restored {
            if let Some(root) = cfg.root {
                // The root declares packages; fusing their constants
                // buys nothing and would put kernels in the image.
                // CR claude for claude: [risk] The package root compiles under the
                // session's flags, and every definition in it keeps them for its
                // instances (DefOrigin::Source { flags }, imaged). The registration
                // key, however, is format + root only (graphix-shell/src/cache.rs:135).
                // So a registration written by the REPL (ReplaceImports), by --expand
                // (ExpandSeq) or under -W error serves every later run, whatever that
                // run's flags. A package whose root warns would then fail `-W error`
                // cold and pass it warm. Nothing diverges today only because the stdlib
                // root emits no warning and has no seq block. Compile the root with one
                // fixed flag set (FusionDisabled, plus WarnUnhandled if wanted) so the
                // image is a function of its key. (shell-12)
                t.compile_root(cfg.flags | CFlag::FusionDisabled, root).await?;
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
            match t.load_program(&source).await {
                Ok(root) => {
                    t.program = Some(Ok(root));
                    info!("program init time: {:?}", st.elapsed());
                    if let Some(tx) = cfg.program_image {
                        let _ = tx.send(t.registration_image());
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
        macro_rules! push_custom {
            ($id:expr, $v:expr) => {
                match self.ctx.event.custom.lock().entry($id) {
                    Entry::Vacant(e) => {
                        e.insert($v);
                        if let Some(exps) = self.ctx.rt.by_ref.get(&$id) {
                            for id in exps.keys() {
                                self.ctx.rt.updated.entry(*id).or_insert(false);
                            }
                        }
                    }
                    Entry::Occupied(_) => {
                        self.ctx.rt.custom_updates.push_back(($id, $v));
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
                if self.ctx.event.variables.contains_key(&$id) {
                    self.ctx.rt.var_updates.push_back(($id, $u));
                } else {
                    let v = match $u {
                        VarUpdate::Set(v) => Some(v),
                        VarUpdate::Patch(path, v) => match self.ctx.rt.store_value(&$id) {
                            Some(cur) => {
                                match coretraits::with_hooks(&mut self.ctx.view(), || {
                                    place::write_path(&cur, &path, v)
                                }) {
                                    Ok(nv) => Some(nv),
                                    Err(err) => {
                                        error!("write through a reference into {:?}: {err}", $id);
                                        None
                                    }
                                }
                            }
                            None => {
                                error!("write through a reference into {:?}: no value to update", $id);
                                None
                            }
                        },
                    };
                    if let Some(v) = v {
                        // CR claude for claude: [bug] Every task and watch delivery is
                        // stored here even when no node references its id any more, and
                        // nothing removes it later. CachedArgsAsync::sleep/delete
                        // (graphix-package-core/src/lib.rs:930-943) remint or unref the
                        // reply id without store_remove, and a timer task that
                        // Timer::sleep released still completes and lands here; GXRt
                        // keeps no task handle and Rt has no cancel, so that task also
                        // runs its full duration. A passed seq step sleeps, so every
                        // seq run with an async step leaks the step's result and every
                        // wake of an arm holding an async call leaks one; after_idle
                        // and timer also re-arm without releasing the previous id
                        // (graphix-package-sys/src/time.rs:111, 306), which leaves its
                        // by_ref entry behind too. Effects still complete when their
                        // arm sleeps, so fix the store side: drop a task or watch
                        // delivery whose id has no by_ref entry, store_remove the old
                        // id on sleep/delete, and abort a released timer's task. probe:
                        // design/review-2026-10-05/repro/rt-01.sh (a seq reading a 100
                        // KB file every 10 ms grows ~100 KB per run; the same read
                        // outside a seq stays flat). (rt-01)
                        // 2026-10-06 claude: it also keeps a #[kill_on_drop] child alive after
                        // the expression holding it is deleted (the stored spawn reply owns the
                        // Proc); pin: design/review-2026-10-05/repro/tests-lib-b2-15.rs.
                        self.ctx.rt.store_insert(
                            $id,
                            graphix_compiler::TagValue::fired(v.clone()),
                        );
                        // an ordinary runtime delivery is a FIRED event
                        self.ctx.event.variables.insert($id, graphix_compiler::TagValue::fired(v));
                        if let Some(exps) = self.ctx.rt.by_ref.get(&$id) {
                            for id in exps.keys() {
                                self.ctx.rt.updated.entry(*id).or_insert(false);
                            }
                        }
                    }
                }
            };
        }
        // CR claude for claude: [perf] Each cycle this loop pops every queued write and
        // pushes back each one whose variable was already delivered this cycle, so N
        // writes queued to one variable cost O(N) per cycle for N cycles. range and
        // array::iter queue all their elements at once, so their delivery is O(N^2): in
        // a debug build range(0, 40000) takes 15.4 s, about 4x per doubling, against
        // 0.38 s for 40000 cycles of a self-loop counter, and every cycle of the
        // program pays the scan until the queue drains. A FIFO per variable (BindId to
        // VecDeque, plus the ids with pending writes) makes a cycle cost the number of
        // distinct pending variables and keeps the per-variable order patches rely on;
        // the custom_updates loop at line 477 has the same shape. probe:
        // design/review-2026-10-05/repro/core-lib-04.gx (core-lib-04)
        for _ in 0..self.ctx.rt.var_updates.len() {
            let (id, v) = self.ctx.rt.var_updates.pop_front().unwrap();
            push_var_event!(id, v)
        }
        // CR claude for claude: [bug] Task entries are delivered one at a time, and an
        // entry whose variable already has a delivery this cycle is re-queued on its
        // own. A set_many entry that meets another write to its variable (a program
        // write queued from the last cycle, or a set in the same input batch) therefore
        // lands a cycle after the rest of its set. That breaks set_many's contract
        // (lib.rs:982-984, every update in the same cycle), which the fuzzer's
        // schedules rely on (graphix-fuzz/src/lib.rs:597-599). For example, set(sx, 5)
        // then set_many([(sx, 1), (sy, 2)]) gives (sx, sy) = [5, 2] then [1, 2] (probe:
        // design/review-2026-10-05/repro/rt-15.rs). Deliver a SetMany as a unit: if any
        // entry collides, re-queue the whole set. (rt-15)
        for (id, v) in tasks.drain(..) {
            push_var_event!(id, VarUpdate::Set(v))
        }
        for _ in 0..self.ctx.rt.custom_updates.len() {
            let (id, u) = self.ctx.rt.custom_updates.pop_front().unwrap();
            push_custom!(id, u)
        }
        for (id, u) in custom_tasks.drain(..) {
            push_custom!(id, u)
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
        // CR claude for claude: [bug] While the subscriber is full, the send loop above
        // handles input every 100 ms. A Compile or Load handled there puts its roots
        // into `updated` with init = true, and these two clears then erase them. The
        // reply reports success, but the new roots never get their init update:
        // constants never fire and lets never publish. Clearing `event` and `updated`
        // right after `update_nodes`, before the loop, leaves whatever the loop
        // compiled scheduled for the next cycle. probe:
        // design/review-2026-10-05/repro/rt-05.rs (copy it to
        // graphix-rt/tests/review_rt_05.rs; with a full one-slot channel, `40 + 2` and
        // `let k = 7` never deliver). (rt-05)
        self.ctx.event.clear();
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
                ToGX::Program { res } => {
                    let _ = res.send((self.program.clone(), self.ctx.env.clone()));
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
        // CR claude for claude: [doc-drift] A check runs CheckOnly, which never fuses, so
        // the comment above is stale and reset_jit_for_check here only clears
        // fusion.stats. Line 804 logs a 'parse time' measured from an Instant created
        // on the line before. Other runtime docs are stale too: rt.rs:31 says the cycle
        // is bumped at the top of do_cycle, but gx.rs:514 bumps it after the nodes ran.
        // trace_start's doc (lib.rs:870) calls max_events a total, but record() counts
        // the current segment, which resolve() replaces, and the Compiled anchors that
        // 'count against neither budget' (gx.rs:102) fill that same vector.
        // wrap_file_in_do and its comment (gx.rs:164-168) name ExprKind::Do, which is
        // now Block, and Control::interrupt's doc (graphix-types/src/stack.rs:331) says
        // the bit is cleared at the end of the cycle, while gx.rs:495 clears it at the
        // start of the next. GXExt's doc (lib.rs:57-67) offers setting variables from
        // do_cycle, but ext.do_cycle runs after delivery (gx.rs:484), so a write to
        // event.variables reaches neither the store nor rt.updated, and no reader is
        // scheduled. (rt-18)
        // 2026-10-06 claude: the JIT reset and the comment above it are gone
        // (tests-lib-b2-16).
        if let IdeMode::Lsp(sink) = &mut self.ctx.cx.env.ide {
            *sink = Some(Arc::new(parking_lot::Mutex::new(Ide::new())));
        }
        let resolvers_for_call: Resolvers = match resolver_override {
            Some(v) => std::sync::Arc::from(v),
            None => self.resolvers.clone(),
        };
        let go = async {
            let st = Instant::now();
            info!("parse time: {:?}", st.elapsed());
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
            info!("resolve time: {:?}", st.elapsed());
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
                    let spec = wrap_file_in_do(stmts.clone(), Arc::new(ori.clone()));
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
        let ProgramRoot { id, output, typ } = self.load_program(source).await?;
        let res = smallvec![CompExp { id, output, typ, rt: rt.clone() }];
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    async fn load_program(&mut self, source: &Source) -> Result<ProgramRoot> {
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
            wrap_file_in_do(Arc::from_iter(exprs.into_iter()), Arc::new(ori.clone()));
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
        let args = lb.typ.args.iter();
        let args = args
            .map(|a| {
                // CR claude for claude: [bug] This refuses every lambda with a defaulted
                // labeled argument. The checker accepts such a lambda wherever a
                // callback type like `fn(e: null) -> Any` is expected (fntyp.rs::align
                // lets the default be omitted), and an in-language call through that
                // value fills the default. So a well-typed GUI or TUI handler such as
                // `|#x = 1, e: null| ..` fails at run time. At construction the error
                // propagates through every ancestor widget and reconcile_windows stops:
                // that window and every later one are never created, and the error only
                // reaches the log. When the handler changes later, update_callable! has
                // already stored `last`, and the old Callable stays installed. probe:
                // design/review-2026-10-05/repro/gui-widgets-a-03.gx (`graphix-fuzz
                // check` reports a route DIVERGENCE: in-language 107 then 109, dispatch
                // RuntimeErr). (gui-widgets-a-03)
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
            .map(|(arg, id)| {
                // CR claude for claude: [bug] The argument references are typed with the
                // definition's own cells (arg.typ from lb.typ), while genn::apply
                // checks the call against an instantiated copy. This site's check
                // therefore merges the copy into the definition's cells, and its settle
                // binds them. After one compile_callable, every later-compiled call of
                // the definition is refused. For `g = |x| x` (also when first passed as
                // `&fn(x: i64) -> i64`, like a widget handler), `g(2)` and `g("t")`
                // fail with '_ does not contain i64'. For `h = 'a: Number |x: 'a| -> 'a
                // x`, `h(2) + 2` fails with 'Number + i64'. All of these compiled
                // before the callable (probe: design/review-2026-10-05/repro/rt-10.rs).
                // Running instances are unaffected, but REPL lines and embedder
                // compiles after a GUI/TUI handler is built are not. Instantiate the
                // signature once and type both the argument references and the apply
                // from that instance. (rt-10)
                genn::reference(&mut self.ctx.view(), *id, arg.typ.clone(), eid)
            })
            .collect::<smallvec::SmallVec<[_; 2]>>();
        let fnode = genn::constant(v.clone(), Type::Fn(lb.typ.clone()));
        let mut n = genn::apply(fnode, Scope::root(), argn, &lb.typ, eid);
        self.ctx.view().begin_runtime_node(eid);
        graphix_compiler::check_and_fuse(&mut self.ctx.view(), self.flags, &mut n)?;
        // CR claude for claude: [bug] The callable's init runs here, between cycles. The
        // cycle counter was already advanced at the end of the last do_cycle (line
        // 514), so every let the body publishes is stamped with the next cycle, and its
        // notify_set leaves this root in rt.updated. That next cycle updates the root
        // again and read_var takes the stamps as deliveries, so init-time work driven
        // by a body let runs twice: a handler's `let k = 10; a <- k ~ a + 1` adds 2
        // where `a <- 10 ~ a + 1` adds 1, in both engines. The update also runs outside
        // InterruptScope, so a fused kernel here cannot see an interrupt and the stack
        // budget cannot abort a runaway init. Schedule the init the way compile and
        // load do (`self.ctx.rt.updated.insert(eid, true)`) instead of updating here.
        // probe: design/review-2026-10-05/repro/rt-03.gx (`graphix-fuzz run`: every
        // Dispatch line ends at [i64:3, i64:2], expected [i64:2, i64:2]). (rt-03)
        self.ctx.event.init = true;
        n.update(&mut self.ctx.view());
        self.ctx.event.clear();
        let cid = CallableId::new();
        self.callables.insert(cid, CallableInt { expr: eid, args });
        self.nodes.insert(eid, n);
        let env = self.ctx.env.clone();
        Ok(Callable {
            expr: eid,
            rt,
            env,
            id: cid,
            lambda: lb.id,
            typ: (*lb.typ).clone(),
        })
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
            // CR claude for claude: [bug] Call delivers each argument through
            // push_var_event, which stores it under the callable's argument id, and
            // nothing removes those entries when the callable goes. Twenty rounds of
            // compile_callable + call + drop raise store_len by 20; the same rounds
            // without the call raise it by 0 (probe:
            // design/review-2026-10-05/repro/rt-11.rs). Every GUI/TUI callable that was
            // called and then rebuilt or dropped keeps its last arguments for the rest
            // of the run. Call store_remove on each of c.args here, as
            // SynthCall::delete (graphix-compiler/src/node/genn.rs:164) does for its
            // argument ids. (rt-11)
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
                // CR claude for claude: [risk] idle_passes returns to 0 only when a pass
                // finds work ready at the top of the loop. Suppose a pass arms the
                // grace, but a task completion or a message wakes the select first. The
                // cycle that handles it can spawn the next task, and the very next idle
                // pass resolves every waiter with no grace at all. wait_idle,
                // wait_result_or_idle and trace_wait_idle can then resolve between the
                // links of a chain of fast async operations (a read whose completion
                // issues another read), contrary to 'confirmed on a second pass'. Reset
                // idle_passes whenever the select woke for anything but the grace
                // timer. (rt-13)
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
            // CR claude for claude: [bug] The loop checks control.aborted() only at its
            // top. An abort that lands while select! waits or a cycle runs still runs
            // process_input_batch and do_cycle, and the send to a receiver the embedder
            // already dropped logs "could not send batch" at ERROR (line 519);
            // Control::abort promises the loop returns before the next cycle.
            // graphix-fuzz shuts its registration-image runtime down right after taking
            // the image, so every `graphix-fuzz check` logs 1-5 spurious ERRORs.
            // TestCtx::shutdown only drops the handle without waiting, so the five
            // `tokio::time::timeout(.., ctx.shutdown())` calls in
            // graphix-fuzz/src/lib.rs can never time out. Re-check aborted() here and
            // before the send, and treat a closed subscriber after an abort as normal.
            // probe: design/review-2026-10-05/repro/x-errors-13.gx (x-errors-13)
            let mut batch = self.batch_pool.take();
            self.process_input_batch(&mut tasks, &mut input, &mut batch).await;
            let st = Instant::now();
            self.do_cycle(&mut tasks, &mut custom_tasks, &mut to_rt, &mut input, batch)
                .await;
            if first_cycle {
                first_cycle = false;
                info!("first cycle time: {:?}", st.elapsed());
            }
        }
    }
}
