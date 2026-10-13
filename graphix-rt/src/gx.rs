use crate::{ProgramImage, RegistrationImage};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use enumflags2::BitFlags;
use futures::{StreamExt, future::try_join_all};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, ExecState, FAILURE_TARGET, Node, Rt, Saved, Scope,
    View,
    expr::{
        self, Expr, ExprId, ExprKind, ModPath, Origin, Probes, ResolverRef, Resolvers,
        RootFile, Sig, Source,
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
use std::{collections::VecDeque, future, mem, result, time::Duration};
use tokio::{
    select,
    sync::{
        mpsc::{self as tmpsc, UnboundedReceiver},
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

/// An embedder's write, landed in arrival order with the others of its
/// cycle: a set of variables, or a patch through a place reference.
enum EmbedderWrite {
    Set(GPooled<Vec<(BindId, Value)>>),
    Patch(BindId, place::Path, Value),
}

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

/// The most input messages the runtime takes at once.
const INPUT_BATCH: usize = 100_000;

/// A finished task's reply, or nothing when the task was cancelled or
/// died; a death is logged, its reply never comes.
fn joined<T>(r: result::Result<(BindId, T), JoinError>) -> Option<(BindId, T)> {
    // the runtime cancels its own tasks (a timer's re-arm, a superseded
    // call, an abort at delete or sleep): only a panic is news
    r.map_err(|e| {
        if e.is_panic() {
            error!("a runtime task ended without replying: {e}")
        }
    })
    .ok()
}

struct CallInFlight {
    arg: Option<BindId>,
    expr: ExprId,
    answer: Option<ExprId>,
}

struct CallableInt {
    expr: ExprId,
    answer: ExprId,
    args: Box<[BindId]>,
}

pub(super) struct GX<X: GXExt> {
    ctx: ExecState<GXRt<X>, X::UserEvent>,
    nodes: IndexMap<ExprId, Node<GXRt<X>, X::UserEvent>, BuildNoHashHasher<ExprId>>,
    callables: IntMap<CallableId, CallableInt>,
    /// Calls whose arguments have not landed, oldest first: a call is
    /// made in the cycle its first argument is delivered, which a write
    /// already waiting on that variable puts off.
    calls: VecDeque<CallInFlight>,
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
        cfg.resolvers
            .extend(crate::search_path(&cfg.resolver_factories, &mut cfg.ctx.libstate)?);
        let mut ctx = cfg.ctx;
        if cfg.lsp_mode {
            ctx.env.ide = IdeMode::Lsp(None);
        }
        let mut t = Self {
            ctx,
            nodes: IndexMap::default(),
            callables: IntMap::default(),
            calls: VecDeque::new(),
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
            let probes = cfg.program_image.as_ref().map(|_| Probes::default());
            match t.load_program(&source, probes.as_ref()).await {
                Ok(root) => {
                    t.program = Some(Ok(root));
                    info!("program init time: {:?}", st.elapsed());
                    if let (Some(tx), Some(probes)) = (cfg.program_image, probes) {
                        let image = t.registration_image();
                        let sources = probes.take();
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
        // the scheduled roots past `after`, the next last; a root's update
        // may schedule a later one this cycle (`Rt::notify_set`)
        let scheduled = |gx: &Self, after: Option<usize>| {
            let mut due: LPooled<Vec<(usize, bool)>> = gx
                .ctx
                .rt
                .updated
                .iter()
                .filter_map(|(id, init)| Some((gx.nodes.get_index_of(id)?, *init)))
                .filter(|(i, _)| after.is_none_or(|a| *i > a))
                .collect();
            due.sort_unstable_by(|a, b| b.cmp(a));
            due
        };
        let mut due = scheduled(self, None);
        let mut known = self.ctx.rt.updated.len();
        while let Some((i, init)) = due.pop() {
            let (id, n) = self.nodes.get_index_mut(i).expect("scheduled root");
            self.ctx.event.view = if init { View::Birth } else { View::Cycle };
            // Only a FIRED production becomes an event.
            let tv = n.update(&mut self.ctx.view());
            if tv.is_fired() {
                let v = tv.value_cloned();
                let watched =
                    matches!(self.result_watch.as_ref(), Some((wid, _)) if wid == id);
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
            if self.ctx.rt.updated.len() != known {
                known = self.ctx.rt.updated.len();
                due = scheduled(self, Some(i));
            }
        }
    }

    async fn do_cycle(
        &mut self,
        tasks: &mut Vec<(BindId, Value)>,
        sets: &mut Vec<EmbedderWrite>,
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
        for w in sets.drain(..) {
            match w {
                EmbedderWrite::Set(set) => self.write_set(set),
                EmbedderWrite::Patch(root, path, v) => {
                    self.write(root, VarUpdate::Patch(path, v), Via::Write)
                }
            }
        }
        let mut answering: LPooled<Vec<(ExprId, ExprId)>> = LPooled::take();
        self.calls_made(&mut answering);
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
            graphix_compiler::with_control(&control, || {
                self.update_nodes(&mut batch);
                for (expr, answer) in answering.drain(..) {
                    let v = batch.iter().find_map(|e| match e {
                        GXEvent::Updated(id, v) if *id == expr => Some(v.clone()),
                        _ => None,
                    });
                    batch.push(GXEvent::Updated(answer, v.unwrap_or(Value::Null)))
                }
            })
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
        // input is served while the batch waits for room: a display's
        // rebuild compiles through it while the shell waits on the display
        let sub = self.sub.clone();
        let mut open = true;
        loop {
            select! {
                biased;
                permit = sub.reserve() => {
                    match permit {
                        Ok(permit) => permit.send(batch),
                        // a subscriber gone after an abort is the abort
                        Err(_) if self.ctx.control.aborted() => (),
                        Err(_) => error!("could not send batch"),
                    }
                    break;
                }
                n = to_rt.recv_many(input, INPUT_BATCH), if open => match n {
                    0 => open = false,
                    _ => self.process_input_batch(sets, input, &mut batch).await,
                },
                _ = time::sleep(Duration::from_millis(100)) => (),
            }
            if self.ctx.control.aborted() {
                break;
            }
        }
    }

    /// A write through the reference `cell` (`ExecCtx::ref_target`): a
    /// place patches its root, else the referent or the cell is set.
    fn set_deref(&mut self, cell: BindId, v: Value, sets: &mut Vec<EmbedderWrite>) {
        let (id, path) = {
            let ctx = self.ctx.view();
            let (id, path) = ctx.ref_target(cell);
            (id, (!path.is_empty()).then(|| place::Path::from(path)))
        };
        sets.push(match path {
            Some(path) => EmbedderWrite::Patch(id, path, v),
            None => {
                let mut one = SETS.take();
                one.push((id, v));
                EmbedderWrite::Set(one)
            }
        })
    }

    /// Readers of `id` update this cycle.
    fn wake_readers(&mut self, id: &BindId) {
        if let Some(exps) = self.ctx.rt.by_ref.get(id) {
            for (e, _) in exps {
                self.ctx.rt.updated.entry(*e).or_insert(false);
            }
        }
    }

    /// Deliver `u` to `id` this cycle: into the store, which advances here
    /// for a written variable as at a let's publication (a patch resolves
    /// against it now, so patches to one root land in order on each
    /// other's result), and the event as FIRED. A reply
    /// nothing references any more goes nowhere.
    fn deliver(&mut self, id: BindId, u: VarUpdate, via: Via) {
        // CR claude for claude: [bug/medium] Waiting writes outlive their variable and
        // leave store entries behind. deliver() drops only a Via::Reply write whose
        // variable nothing references. A Write or Set queued in var_updates or `sets`
        // lands in the store after the binding that owned the variable was deleted. Two
        // cases. delete_callable removes the argument store entries (gx.rs) but not the
        // call already in `sets`, so call() followed by drop re-creates them in the
        // next do_cycle (the rt-11 fix covers only one site). And a REPL or embedder
        // Delete of an expression whose `<-` queued a write: Bind::delete's
        // store_remove runs first, then the queued write lands. Failure: Five
        // compile_callable + call + drop iterations: store_len goes from 1 to 6. Five
        // compile/drop iterations of `{ let x = 0; x <- x + 1; x }`: store_len goes
        // from 0 to 5. The control `{ let x = 0; x + 1 }` stays at 0. Each entry holds
        // its value until the runtime ends, and env_stats-based leak checks drift. Fix:
        // When a binding is released, purge its waiting writes: a Waiting::forget(id)
        // called from the paths that store_remove a dead binding (Bind::delete,
        // delete_callable, release_var), with delete_callable also dropping its calls'
        // pending sets. Alternatively, have deliver() skip any write to an id that a
        // release has marked dead, not only replies. Unverified: the reviewer's report,
        // not checked by a verifier. (rt-05)
        // CR claude for claude: [bug/high] A `Via::Write` still waiting in
        // `var_updates` when its id is released (`ExecCtx::release_var`) or deleted
        // (the `store_remove` in pattern, slot, sample, wrapper and catch delete) lands
        // here afterwards. It puts a store entry back that nothing ever removes. Any id
        // written more than once in a cycle can leak this way: `array::iter`/`iterq`,
        // `queue`, `range`, queuefn pops, a `<-` target with several writers, and
        // sys::net var_watches. The leak is one value per release, unbounded, and the
        // runtime also runs cycles that do no work. Dropping writes to unreferenced ids
        // here would be wrong, because the store keeps writes for readers that bind
        // later. Instead, add an `Rt` method that removes the id from `var_updates`
        // (`Waiting::by_id` and `ids`) together with the `store_remove`, and call it
        // from every release and delete path. Then pin it with a test that releases an
        // id while writes to it are queued. probe:
        // design/review-2026-10-12/repro/x-node-contract-01.gx (x-node-contract-01)
        if matches!(via, Via::Reply) && !self.ctx.rt.by_ref.contains_key(&id) {
            return;
        }
        let v = match u {
            VarUpdate::Set(v) => v,
            // CR claude for claude: [bug/low] When a patch fails here (the path is out
            // of range, or the root has no value), the log names neither the file nor
            // the line of the `*r <- v` that wrote it. Every other runtime failure
            // carries `diagnostic_site(spec)` (`report_failure`, `unhandled_msg`, the
            // arith errors). In a program with many reference writes, the dropped one
            // cannot be found. The site is only known at the writer:
            // `ConnectDeref::update` holds its spec when it calls `Rt::patch_var`, but
            // `VarUpdate::Patch` carries only a path and a value. Fix: carry an
            // optional site (`ArcStr` from `diagnostic_site`) in `VarUpdate::Patch`
            // through `Rt::patch_var` and the branch log's `RtOp::PatchVar`, and prefix
            // both log lines with it. The embedder path `GX::set_deref` passes none.
            // probe: design/review-2026-10-12/repro/x-errors-03.gx (x-errors-03)
            VarUpdate::Patch(path, v) => {
                let Some(cur) = self.ctx.rt.store_value(&id) else {
                    error!(
                        target: FAILURE_TARGET,
                        "a write through a reference was dropped: its root has no value"
                    );
                    return;
                };
                let written = coretraits::with_hooks(&mut self.ctx.view(), || {
                    place::write_path(&cur, &path, v)
                });
                match written {
                    Ok(nv) => nv,
                    Err(err) => {
                        error!(
                            target: FAILURE_TARGET,
                            "a write through a reference was dropped: {err}"
                        );
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
        sets: &mut Vec<EmbedderWrite>,
        input: &mut Vec<ToGX<X>>,
        batch: &mut GPooled<Vec<GXEvent>>,
    ) {
        for m in input.drain(..) {
            match m {
                ToGX::WithCtx { f } => f(&mut self.ctx),
                ToGX::GetEnv { res } => {
                    let _ = res.send(self.ctx.env.clone());
                }
                // CR claude for claude: [risk/low] A panic anywhere in the check
                // (parse, resolve, typecheck) unwinds out of the runtime task, because
                // nothing catches it here. Every later GXHandle::exec then fails with
                // 'runtime is dead', and the language server publishes that at 0:0 of
                // every root until the editor restarts it. The server's own handlers
                // are guarded so one bad input never ends it. A buffer of half-typed
                // code is the input most likely to reach a checker bug, so the check is
                // where that guard matters most. Fix: run the check under
                // FutureExt::catch_unwind and restore its Saved on unwind, returning
                // the panic as an error. Or have ShellLspBackend rebuild its runtime
                // when exec reports it dead, which also survives a panic that leaves
                // the context inconsistent. (x-lsp-probe-08)
                // CR claude for claude: [risk/low] The check runs inside the runtime's
                // spawned task with no unwind guard. Any panic during parse, resolve or
                // typecheck therefore ends the task for good: this check answers
                // "runtime did not respond", and every later exec on the handle answers
                // "runtime is dead". ShellLspBackend keeps one GXHandle and never
                // rebuilds it. After one buffer triggers a check-time panic, every root
                // is published with that error, and env() quietly falls back to an
                // empty Env until the editor restarts the server. The server's
                // `guarded` cannot help, because it only covers the server thread. No
                // check-time panic is known at HEAD, but the fuzz corpus shows
                // compile-time panics have happened before. Fix: wrap this arm's future
                // in `AssertUnwindSafe(..).catch_unwind()`. On a panic, restore the
                // `Saved` taken in check_inner and send an Err, so the session
                // survives. (lsp-02)
                // CR claude for claude: [risk/medium] A panic anywhere in a check
                // (parser, resolution, checker, a builtin's typecheck) unwinds out of
                // this arm and ends the GX task, because GXConfig::start spawns it with
                // no guard. The server's `guarded` only covers its own thread. The
                // request then answers "runtime did not respond", and every later check
                // of any root publishes "runtime is dead" at 0:0 with hovers returning
                // nothing, until the editor restarts the server. One buffer that hits a
                // checker bug, such as lsp-02's genn assert, disables the language
                // server. The fix is to run check_inner under catch_unwind
                // (AssertUnwindSafe over the future, e.g. FutureExt::catch_unwind),
                // restore `Saved` and the env on unwind, and answer `res` with a "the
                // check panicked" error; otherwise the LSP backend must rebuild its
                // runtime when it finds it dead. probe:
                // design/review-2026-10-12/repro/x-fix-tools-06.sh (same root as
                // lsp-02) (x-fix-tools-06)
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
                    sets.push(EmbedderWrite::Set(one))
                }
                // one message, one set, one cycle (GXHandle::set_many)
                ToGX::SetMany { sets: set } => sets.push(EmbedderWrite::Set(set)),
                ToGX::DeleteCallable { id } => self.delete_callable(id),
                ToGX::Call { id, args, answered } => {
                    if let Err(e) = self.call_callable(id, args, answered, sets) {
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
                        .map(|m| m.iter().map(|(_, n)| *n as usize).sum::<usize>())
                        .sum();
                    let _ = res.send(crate::EnvStats {
                        by_id_len,
                        ref_var_keys,
                        ref_var_total,
                        store_len: self.ctx.rt.store.len(),
                        lambda_defs_len: self.ctx.lambda_defs.len(),
                        lowered_seqs_len: self.ctx.lowered_seqs_len(),
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
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers, None)))
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

    /// Compile and install each statement of `text`. The statements
    /// before one that fails are installed, as they would be entered one
    /// at a time, and the failure is returned.
    async fn compile(&mut self, rt: GXHandle<X>, text: ArcStr) -> Result<CompRes<X>> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            try_join_all(exprs.iter().map(|e| e.resolve_modules(&self.resolvers, None)))
                .await?;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        let mut failed = None;
        for e in exprs.iter() {
            let stmt = graphix_compiler::compile_stmt(
                &mut self.ctx.view(),
                self.flags,
                &self.scope,
                e.clone(),
            );
            // CR claude for claude: [bug/medium] c-lib-04 regressed: failed REPL input
            // runs earlier statements, drops their names, leaves a dead catch.
            // GX::compile's doc (gx.rs) says the statements before a failing one are
            // installed, as if entered one at a time. What actually happens differs.
            // The earlier statements' nodes are installed and run (`println("hi"); let
            // c = nosuch` prints hi), but their names are gone from the env afterwards
            // (`let b = 5; let d = nosuch` then `b` gives `b not defined`). A `catch`
            // in the failed input stays in `self.scope` as the handler for every later
            // input while its bind is gone, so every later `?` is refused with an
            // internal error: `BUG: catch BindId(..) has no inferred bind`. Failure:
            // REPL: `catch(e) println("caught [e]"); let w = nosuch` (error, as
            // expected), then `error(`Boom)?` gives `error: ... BUG: catch
            // BindId(4611686018427392006) has no inferred bind`. Every later input
            // using `?` fails the same way for the rest of the session. `let b = 5; let
            // d = nosuch` installs a node for b whose name no later input can reach.
            // Fix: Make the input atomic: on failure, delete the nodes already compiled
            // for this input and restore `self.scope` and the env to their pre-input
            // values. Or make it truly partial: keep the env and scope those statements
            // produced. The code currently does half of each. Then fix the doc comment
            // to match. Unverified: the reviewer's report, not checked by a verifier.
            // (x-rerun-1-02)
            match stmt.with_context(|| ori.clone()) {
                Ok((n, advanced)) => {
                    self.scope = advanced;
                    nodes.push(n);
                }
                Err(e) => {
                    failed = Some(e);
                    break;
                }
            }
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
        match failed {
            // CR claude for claude: [bug/medium] Returning Err here drops comp_exprs.
            // Each CompExp's Drop sends ToGX::Delete, so the statements this function
            // just installed are deleted again. self.scope still points past them,
            // which contradicts the doc comment above. As a result, after `catch(e)
            // println("caught [e]"); let w = nosuch`, every later `?` in the session is
            // refused with "BUG: catch .. has no inferred bind". After `let x = 41; let
            // y = nosuch`, `x` is not defined. Choose one behaviour: - Keep the
            // installed statements: return their CompExps together with the failure,
            // for example a CompRes that carries the error, so the shell holds them. -
            // Roll the whole input back: save self.scope (and the env) before the loop,
            // and on failure delete the nodes, restore both, and drop the deferred
            // refs. Then make the doc comment match. Probe:
            // design/review-2026-10-12/repro/rt-02.py (rt-02)
            Some(e) => Err(e),
            None => Ok(CompRes { exprs: comp_exprs, env: self.ctx.env.clone() }),
        }
    }

    async fn load_exprs(
        &self,
        source: &Source,
        resolvers: &Resolvers,
        probes: Option<&Probes>,
    ) -> Result<(Origin, Arc<[Expr]>, Option<Sig>)> {
        let (ori, exprs, sig) = match source {
            Source::File(file) => {
                let overrides = resolvers.iter().find_map(|r| r.overrides());
                let root = RootFile::load(file, overrides.as_ref(), probes).await?;
                (root.ori, root.exprs, root.sig)
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
                (ori.clone(), expr::parser::parse(ori)?, None)
            }
            Source::Program(src) => {
                let ori =
                    Origin { parent: None, source: source.clone(), text: src.clone() };
                (ori.clone(), expr::parser::parse(ori)?, None)
            }
            Source::Vfs(m) => bail!("{m} is a VFS module: it loads through a `mod`"),
            Source::Unspecified => bail!("can't load from an unspecified source"),
        };
        Ok((ori, exprs, sig))
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
        // a check runs nothing: everything it compiled is rolled back
        let saved = Saved::take(&self.ctx.cx);
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
            let overrides = resolvers_for_call.iter().find_map(|r| r.overrides());
            // a package root is the body of a module under its name; a lone
            // root is a script, its interface checked over its names
            let (ori, exprs, modules_at, as_module, sig) = match (&initial_scope, source)
            {
                (Some(name), Source::File(file)) => {
                    let path =
                        ModPath(netidx_core::path::Path::root().append(name.as_str()));
                    self.ctx.env.unbind_scope_subtree(&path);
                    // a package this binary was not built with is still
                    // the root `package::` names
                    self.ctx.env.package_roots.insert(name.clone());
                    let root = RootFile::load(file, overrides.as_ref(), None).await?;
                    let ori = root.ori.clone();
                    (
                        ori,
                        Arc::from_iter([root.into_module(name.clone())]),
                        path,
                        true,
                        None,
                    )
                }
                (Some(_), _) => bail!("only a file can be checked as a package root"),
                (None, _) => {
                    let (ori, exprs, sig) =
                        self.load_exprs(source, &resolvers_for_call, None).await?;
                    (ori, exprs, ModPath::root(), false, sig)
                }
            };
            let exprs = try_join_all(exprs.iter().map(|e| {
                e.resolve_modules_in_scope(&modules_at, &resolvers_for_call, None)
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
            let res = match as_module {
                true => exprs.iter().try_for_each(|e| {
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
                false => {
                    let stmts = Arc::from_iter(exprs.iter().cloned());
                    let spec = wrap_file_in_block(stmts.clone(), Arc::new(ori.clone()));
                    graphix_compiler::compile_script(
                        &mut self.ctx.view(),
                        flags,
                        &Scope::root(),
                        spec,
                        &stmts,
                        sig.as_ref(),
                    )
                    .map(|n| nodes.push(n))
                }
            };
            let res = res.and_then(|()| {
                self.ctx.env.unused_imports_since(&env).try_for_each(|(key, e)| {
                    let mut end = e.name_at;
                    end.column += key.chars().count() as i32;
                    self.ctx.env.warn_at(
                        flags,
                        &e.ori,
                        e.name_at,
                        end,
                        format_args!("unused import `{key}`"),
                    )
                })
            });
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
        saved.restore(&mut self.ctx.cx);
        res
    }

    async fn load(&mut self, rt: GXHandle<X>, source: &Source) -> Result<CompRes<X>> {
        let ProgramRoot { id, output, typ } = self.load_program(source, None).await?;
        let res = smallvec![CompExp { id, output, typ, rt: rt.clone() }];
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    /// Compile the program in `source`, adding every place its module
    /// resolution looked to `probes`.
    async fn load_program(
        &mut self,
        source: &Source,
        probes: Option<&Probes>,
    ) -> Result<ProgramRoot> {
        let scope = Scope::root();
        let st = Instant::now();
        let (ori, exprs, sig) = self.load_exprs(source, &self.resolvers, probes).await?;
        info!("parse time: {:?}", st.elapsed());
        let st = Instant::now();
        let exprs = try_join_all(
            exprs.iter().map(|e| e.resolve_modules(&self.resolvers, probes)),
        )
        .await?;
        info!("resolve time: {:?}", st.elapsed());
        let output = exprs.last().map(|e| is_output_kind(&e.kind)).unwrap_or(false);
        let stmts = Arc::from_iter(exprs.into_iter());
        let wrapped = wrap_file_in_block(stmts.clone(), Arc::new(ori.clone()));
        let id = wrapped.id;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let n = graphix_compiler::compile_script(
            &mut self.ctx.view(),
            self.flags,
            &scope,
            wrapped,
            &stmts,
            sig.as_ref(),
        )
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
        // CR claude for claude: [perf/low] This call site's callee is a Constant, so it
        // is unbound when check_and_fuse runs. emit_clif then falls through to the
        // builtin path and fails as 'builtin call site ("nop")("nop") not discovered',
        // and CallSite::fuse has no apply to fuse. The run-time bind at the first
        // update fuses the instance only for a shared collection slot. So the body of
        // every Callable (a GUI/TUI handler, even a pure `|x: i64| -> i64 x * 2 + 1`)
        // always runs on the node-walk, while the same call written in Graphix would be
        // offered to fusion, and FusionStats.failed shows it as an emitter gap. The
        // callee is a known LambdaDef here: bind its instance when compiling, as a
        // statically resolved call is pre-bound, so fusion sees a Lambda callee and
        // fuses its body. Add a FuseExpect pin for a pure callable. (rt-12)
        let mut n = genn::apply(fnode, Scope::root(), argn, &ftype, eid);
        self.ctx.view().open_compile_frame(eid);
        if let Err(e) =
            graphix_compiler::check_and_fuse(&mut self.ctx.view(), self.flags, &mut n)
        {
            let mut ctx = self.ctx.view();
            n.delete(&mut ctx);
            ctx.drop_deferred();
            return Err(e);
        }
        // its init runs in the next cycle, as a compiled root's does
        // CR claude for claude: [bug/medium] A callable with no argument to deliver
        // runs here and never again. This covers `|| e` and also a lambda whose every
        // label has a default, because those labels are filtered out above. Nothing
        // stays bottom, so this birth update applies the lambda once at compile time.
        // Later, call_callable writes an empty set, which schedules nothing, while
        // calls_made counts the `arg: None` call as made. So Callable::call never runs
        // it again and call_answered answers null. The fix is to give every call a
        // delivery: the call site gets a hidden trigger (a BindId argument or
        // reference) that call_callable always writes. Then a zero-argument call is a
        // write like any other, and the birth update finds the trigger absent and does
        // not apply the lambda. With that in place calls_made no longer needs its
        // `None` case for live callables. probe:
        // design/review-2026-10-12/repro/rt-03-zero-arg-callable.rs (rt-03)
        self.ctx.rt.updated.insert(eid, true);
        let cid = CallableId::new();
        let answer = ExprId::new();
        self.callables.insert(cid, CallableInt { expr: eid, answer, args });
        self.nodes.insert(eid, n);
        let env = self.ctx.env.clone();
        Ok(Callable { expr: eid, answer, rt, env, id: cid, lambda: lb.id, typ: ftype })
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
        answered: bool,
        sets: &mut Vec<EmbedderWrite>,
    ) -> Result<()> {
        let c =
            self.callables.get(&id).ok_or_else(|| anyhow!("unknown callable {id:?}"))?;
        // CR claude for claude: [risk/medium] GXHandle::call skips argument type
        // checks; the runtime checks only arity (b-16's root cause). gui-widgets-b-16
        // (a radio sent null into a callback typed from its value, and a fused reader
        // panicked) was fixed in RadioW alone. The root cause is still there.
        // Callable::call/call_answered check the argument types (graphix-rt/src/lib.rs
        // check_args), but GXHandle::call(id, args) (lib.rs), which the GUI event loop
        // uses for every Message::Call, sends unchecked. The runtime's call_callable
        // (gx.rs) checks only `args.len()`. A widget that ever sends a value outside
        // the callback's declared type stores a mistyped value in a binding the checker
        // typed, and fused code reads it through the compiled ABI. Failure: I found no
        // mistyped send in the current widgets (data_table toggle/spin/button send the
        // declared types). Any future widget, or an embedder calling GXHandle::call
        // with a wrong value, reproduces b-16: a kernel reading the parameter panics or
        // misreads, the runtime dies. Fix: Check `is_a` against the callable's argument
        // types in call_callable, on the runtime side, so every entry is checked; or
        // make GXHandle::call take a Callable and route it through check_args. Refuse a
        // mismatched call with a logged error. Unverified: the reviewer's report, not
        // checked by a verifier. (x-rerun-4-04)
        if args.len() != c.args.len() {
            bail!("expected {} arguments", c.args.len());
        }
        // CR claude for claude: [bug/low] A callable with no arguments to deliver (a
        // `||` lambda, or one whose arguments are all labeled defaults, which
        // compile_callable drops) records `arg: None` and an empty set. calls_made
        // counts the call as made at once, but nothing schedules c.expr, so its body
        // never runs again after the init cycle and call_answered answers null. Either
        // refuse such a lambda in compile_callable, or schedule the callable's
        // expression here (`updated.insert(c.expr, ..)`) so the call's cycle runs it
        // and the answer is what that update produced. (x-fix-tools-08)
        self.calls.push_back(CallInFlight {
            arg: c.args.first().copied(),
            expr: c.expr,
            answer: answered.then_some(c.answer),
        });
        let mut set = SETS.take();
        set.extend(c.args.iter().zip(args.iter()).map(|(id, v)| (*id, v.clone())));
        sets.push(EmbedderWrite::Set(set));
        Ok(())
    }

    /// The calls whose arguments this cycle delivered, leaving the rest:
    /// (call site, answer id) for each answered one. A variable takes one
    /// delivery a cycle, so its oldest call is the one made.
    fn calls_made(&mut self, answering: &mut Vec<(ExprId, ExprId)>) {
        let mut seen: LPooled<Vec<BindId>> = LPooled::take();
        let variables = &self.ctx.event.variables;
        self.calls.retain(|c| {
            let now = match c.arg {
                None => true,
                Some(arg) if seen.contains(&arg) => false,
                Some(arg) => {
                    seen.push(arg);
                    variables.contains_key(&arg)
                }
            };
            if now && let Some(answer) = c.answer {
                answering.push((c.expr, answer))
            }
            !now
        });
    }

    fn delete_callable(&mut self, id: CallableId) {
        if let Some(c) = self.callables.remove(&id) {
            // its calls are made, with nothing to answer, next cycle
            for call in self.calls.iter_mut().filter(|call| call.expr == c.expr) {
                call.arg = None
            }
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
        let mut sets: Vec<EmbedderWrite> = vec![];
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
                    while let Some(r) = self.ctx.rt.tasks.try_join_next() {
                        tasks.extend(joined(r));
                    }
                };
                (custom_tasks) => {
                    while let Some(r) = self.ctx.rt.custom_tasks.try_join_next() {
                        custom_tasks.extend(joined(r));
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
            // CR claude for claude: [bug/medium] The idle test misses work parked in
            // `sets` (and `calls`). When the subscriber is full, do_cycle's send loop
            // runs process_input_batch, which turns ToGX::Set, SetMany, Call and a
            // chainless SetDeref into entries of `sets`. That cycle has already drained
            // `sets`, and nothing here or in cycle_ready looks at it. So `ready` is
            // false, `or_never` never fires, and the write or call waits for an
            // unrelated wake: a timer, another message, a source. In a GUI, a button
            // press made while a frame stalls the channel does nothing until the mouse
            // moves. Fix: count `!sets.is_empty()` in `ready`, or better, move `sets`
            // and `calls` into GX so cycle_ready sees them. Then extend
            // tests/send_loop.rs to cover a set and a call received in the send loop.
            // Probe: design/review-2026-10-12/repro/rt-01.rs
            // (set_in_send_loop_is_delivered, call_in_send_loop_is_delivered). (rt-01)
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
                    tasks.extend(joined(up));
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = join_or_wait(&mut self.ctx.rt.custom_tasks) => {
                    custom_tasks.extend(joined(up));
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
                n = to_rt.recv_many(&mut input, INPUT_BATCH) => {
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
