use crate::{ProgramImage, RegistrationImage};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use enumflags2::BitFlags;
use futures::{FutureExt, Stream, StreamExt, stream::SelectAll};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, ExecState, FAILURE_TARGET, Node, Rt, Saved, Scope,
    View, WriteTarget, diagnostic_site,
    expr::{
        self, Expr, ExprId, ExprKind, Load, ModPath, Origin, Probes, ResolverRef,
        Resolvers, RootFile, Sig, Source, join_in_order,
    },
    fusion_on,
    ide::{Ide, IdeMode},
    image::ProgramRoot,
    node::{
        coretraits, genn,
        lambda::LambdaDef,
        place::{self, VarUpdate},
    },
    typ::{FnType, Type},
};
use log::{debug, error, info, warn};
use netidx_value::{ValArray, Value};
use nohash::IntMap;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::{SmallVec, smallvec};
use std::{
    collections::{BTreeMap, VecDeque, hash_map::Entry},
    future, mem,
    panic::AssertUnwindSafe,
    result,
    task::Poll,
    time::Duration,
};
use tokio::{
    select,
    sync::{
        Notify,
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

/// The sets an embedder's writes and a callable's arguments travel in.
pub(crate) static SETS: std::sync::LazyLock<Pool<Vec<(BindId, Value)>>> =
    std::sync::LazyLock::new(|| Pool::new(64, 1024));

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

/// Room in the subscriber's channel, when `armed`.
async fn reserve_or_never<T>(
    sub: &tmpsc::Sender<T>,
    armed: bool,
) -> result::Result<tmpsc::Permit<'_, T>, tmpsc::error::SendError<()>> {
    match armed {
        true => sub.reserve().await,
        false => future::pending().await,
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

/// The next item of `s`, or `None` when a stream of it ended: `SelectAll`
/// drops an ended stream inside its poll and stays pending, and the end
/// of a watch may be the end of what was outstanding.
fn next_or_ended<S: Stream + Unpin>(
    s: &mut SelectAll<S>,
) -> impl Future<Output = Option<S::Item>> + '_ {
    future::poll_fn(move |cx| {
        let len = s.len();
        match s.poll_next_unpin(cx) {
            Poll::Pending if s.len() < len => Poll::Ready(None),
            p => p,
        }
    })
}

/// Every update `s` has ready now, polling only the streams woken since
/// the last poll and dropping those that ended.
fn drain_ready<T, S>(s: &mut SelectAll<S>, out: &mut Vec<T>)
where
    S: Stream<Item = GPooled<Vec<T>>> + Unpin,
{
    while let Some(Some(mut up)) = s.next().now_or_never() {
        out.extend(up.drain(..))
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
    /// The signature a call's arguments are checked against.
    typ: FnType,
}

/// The roots in compile order, each under the sequence number its first
/// insert drew, so removing one moves no other.
struct Roots<N> {
    by_seq: BTreeMap<u64, (ExprId, N)>,
    seq: IntMap<ExprId, u64>,
    next: u64,
}

impl<N> Default for Roots<N> {
    fn default() -> Self {
        Self { by_seq: BTreeMap::new(), seq: IntMap::default(), next: 0 }
    }
}

impl<N> Roots<N> {
    /// Add `id` last, or replace its node where it stands.
    fn insert(&mut self, id: ExprId, n: N) {
        match self.seq.entry(id) {
            Entry::Occupied(e) => {
                self.by_seq.get_mut(e.get()).expect("a sequenced root").1 = n
            }
            Entry::Vacant(e) => {
                e.insert(self.next);
                self.by_seq.insert(self.next, (id, n));
                self.next += 1;
            }
        }
    }

    fn remove(&mut self, id: &ExprId) -> Option<N> {
        let seq = self.seq.remove(id)?;
        self.by_seq.remove(&seq).map(|(_, n)| n)
    }

    fn get(&self, id: &ExprId) -> Option<&N> {
        self.by_seq.get(self.seq.get(id)?).map(|(_, n)| n)
    }

    /// `id`'s place in compile order.
    fn seq(&self, id: &ExprId) -> Option<u64> {
        self.seq.get(id).copied()
    }

    fn at_mut(&mut self, seq: u64) -> Option<(ExprId, &mut N)> {
        self.by_seq.get_mut(&seq).map(|(id, n)| (*id, n))
    }

    fn iter(&self) -> impl Iterator<Item = (ExprId, &N)> {
        self.by_seq.values().map(|(id, n)| (*id, n))
    }
}

pub(super) struct GX<X: GXExt> {
    ctx: ExecState<GXRt<X>>,
    nodes: Roots<Node<GXRt<X>>>,
    callables: IntMap<CallableId, CallableInt>,
    /// Calls whose arguments have not landed, oldest first: a call is
    /// made in the cycle its first argument is delivered, which a write
    /// already waiting on that variable puts off.
    calls: VecDeque<CallInFlight>,
    /// Answers whose call's cycle queued writes: they go out at the end of
    /// the next cycle, once those writes have landed.
    held_answers: Vec<(ExprId, Value)>,
    /// The replies delivered this cycle, variables and custom: those an
    /// interrupt kept from every reader are delivered again
    /// ([`Self::redeliver_unseen_replies`]).
    replied: Vec<BindId>,
    sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    resolvers: Resolvers,
    batch_pool: Pool<Vec<GXEvent>>,
    flags: BitFlags<CFlag>,
    /// Pending `WaitIdle` requests, answered at the next idle verdict.
    idle_waiters: Vec<oneshot::Sender<()>>,
    /// A `NotifyIdle` is pending: the next idle verdict sends
    /// [`GXEvent::Idle`].
    notify_idle: bool,
    /// Pending `WaitSettled` requests, answered at the first idle verdict
    /// with nothing outstanding (`GXRt::outstanding`).
    settle_waiters: Vec<oneshot::Sender<()>>,
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

    /// The construction-time program's compile error, if it had one.
    pub(super) fn program_compiled(&self) -> Result<()> {
        match &self.program {
            Some(Err(e)) => Err(anyhow!("{e}")),
            Some(Ok(_)) | None => Ok(()),
        }
    }

    pub(super) async fn new(mut cfg: GXConfig<X>) -> Result<Self> {
        let st_new = Instant::now();
        cfg.resolvers.extend(crate::search_path(
            &cfg.resolver_factories,
            &mut cfg.ctx.libstate,
            None,
        )?);
        let mut ctx = cfg.ctx;
        if cfg.lsp_mode {
            ctx.env.ide = IdeMode::Lsp(None);
        }
        let mut t = Self {
            ctx,
            nodes: Roots::default(),
            callables: IntMap::default(),
            calls: VecDeque::new(),
            held_answers: Vec::new(),
            replied: Vec::new(),
            sub: cfg.sub,
            resolvers: std::sync::Arc::from(cfg.resolvers),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            idle_waiters: Vec::new(),
            notify_idle: false,
            settle_waiters: Vec::new(),
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
            if probes.is_some() {
                t.ctx.env.record_warnings();
            }
            let loaded = t.load_program(&source, probes.as_ref()).await;
            let warnings = t.ctx.env.recorded_warnings();
            match loaded {
                Ok(root) => {
                    t.program = Some(Ok(ProgramRoot { warnings, ..root }));
                    info!("program init time: {:?}", st.elapsed());
                    if let (Some(tx), Some(probes)) = (cfg.program_image, probes) {
                        let image = if fusion_on(t.flags) && !t.ctx.fusion.available() {
                            Err(anyhow!(
                                "the JIT is unavailable, so the program has no kernels"
                            ))
                        } else {
                            t.registration_image()
                        };
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
        let nodes: Vec<(ExprId, &Node<GXRt<X>>)> = self.nodes.iter().collect();
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
        if let Some(p) = &reg.program {
            self.ctx.env.replay_warnings(&p.warnings);
        }
        self.program = reg.program.map(Ok);
        Ok(())
    }

    /// One cycle's updates of the scheduled roots; what fired goes into
    /// `batch`.
    fn update_nodes(&mut self, batch: &mut GPooled<Vec<GXEvent>>) {
        // the scheduled roots past `after`, the next last; a root's update
        // may schedule a later one this cycle (`Rt::notify_set`)
        let scheduled = |gx: &Self, after: Option<u64>| {
            let mut due: LPooled<Vec<(u64, bool)>> = gx
                .ctx
                .rt
                .updated
                .iter()
                .filter_map(|(id, init)| Some((gx.nodes.seq(id)?, *init)))
                .filter(|(i, _)| after.is_none_or(|a| *i > a))
                .collect();
            due.sort_unstable_by(|a, b| b.cmp(a));
            due
        };
        let mut due = scheduled(self, None);
        let mut known = self.ctx.rt.updated.len();
        while let Some((i, init)) = due.pop() {
            let (id, n) = self.nodes.at_mut(i).expect("scheduled root");
            self.ctx.event.view = if init { View::Birth } else { View::Cycle };
            // Only a FIRED production becomes an event.
            let tv = n.update(&mut self.ctx.view());
            if tv.is_fired() {
                let v = tv.value_cloned();
                if let Some(tr) = self.trace.as_mut() {
                    tr.record(self.ctx.rt.cycle, id, &v);
                }
                batch.push(GXEvent::Updated(id, v))
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
        aborted: &Notify,
        mut batch: GPooled<Vec<GXEvent>>,
    ) {
        debug_assert!(
            !self.ctx.view().deferred_pending(),
            "compiled references left unreplayed"
        );
        self.ctx.control.cycle_started();
        // no kernel runs between cycles
        self.ctx.fusion.relocate_kernels();
        self.ctx.view().apply_deferred();
        self.ctx.view().release_references();
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
                    self.write(root, VarUpdate::Patch(path, v, None), Via::Write)
                }
            }
        }
        let mut answering: LPooled<Vec<(ExprId, ExprId)>> = LPooled::take();
        self.calls_made(&mut answering);
        let mut due: LPooled<Vec<(BindId, Box<dyn CustomBuiltinType>, Via)>> =
            LPooled::take();
        self.ctx.rt.custom_updates.take(&mut due);
        for (id, u, via) in due.drain(..) {
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
                for (answer, v) in self.held_answers.drain(..) {
                    batch.push(GXEvent::Updated(answer, v))
                }
                let writes_queued = !self.ctx.rt.var_updates.is_empty();
                for (expr, answer) in answering.drain(..) {
                    let v = batch
                        .iter()
                        .find_map(|e| match e {
                            GXEvent::Updated(id, v) if *id == expr => Some(v.clone()),
                            _ => None,
                        })
                        .unwrap_or(Value::Null);
                    match writes_queued {
                        true => self.held_answers.push((answer, v)),
                        false => batch.push(GXEvent::Updated(answer, v)),
                    }
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
        self.redeliver_unseen_replies();
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
            // an aborted runtime sends what has room and waits for nothing
            if self.ctx.control.aborted() {
                if let Ok(permit) = sub.try_reserve() {
                    permit.send(batch)
                }
                break;
            }
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
                _ = aborted.notified() => (),
            }
        }
    }

    /// Whether an embedder's write of `v` to `id` fits its binding's type,
    /// logging one that does not: the nodes and kernels that read `id` were
    /// compiled for that type.
    fn admits(&self, id: BindId, v: &Value) -> bool {
        match self.ctx.env.by_id.get(&id) {
            Some(b) if !b.typ.is_a(&self.ctx.env, v) => {
                error!("a write of {v} to {} was refused: it is not a {}", b.name, b.typ);
                false
            }
            _ => true,
        }
    }

    /// A write through the reference `cell` (`ExecCtx::ref_target`): a
    /// place patches its root, else the referent or the cell is set. A
    /// patch is checked by the root it would make now.
    fn set_deref(&mut self, cell: BindId, v: Value, sets: &mut Vec<EmbedderWrite>) {
        let (id, path) = match self.ctx.view().write_target(cell) {
            None => return,
            Some(WriteTarget::Bind(id)) => (id, None),
            Some(WriteTarget::Place(id, path)) => {
                (id, (!path.is_empty()).then_some(path))
            }
        };
        match path {
            Some(path) => {
                let patched = self.ctx.rt.store_value(&id).and_then(|cur| {
                    coretraits::with_hooks(&mut self.ctx.view(), || {
                        place::write_path(&cur, &path, v.clone())
                    })
                    .ok()
                });
                if patched.is_none_or(|root| self.admits(id, &root)) {
                    sets.push(EmbedderWrite::Patch(id, path, v))
                }
            }
            None => {
                if self.admits(id, &v) {
                    let mut one = SETS.take();
                    one.push((id, v));
                    sets.push(EmbedderWrite::Set(one))
                }
            }
        }
    }

    /// Readers of `id` update this cycle.
    // CR claude for claude: [perf/low] A delivery costs O(readers of the id), not
    // O(boundaries that run). Each Reader costs an `updated` entry and a mark, even one
    // already stamped or asleep, and notify_set and mark_readers do the same. Every seq
    // step reads the machine's pc (`seqpc ~! v` in --expand), so each step transition
    // walks all S steps' readers: a seq of S cycle-separated steps costs 18.7 us a step
    // at S=400 and 58.3 at S=3200 (O(S^2) per run). 1600 idle arms reading a ticking
    // variable add about 7 ns each to every cycle. Two decisions: group readers by root
    // so `updated` is touched once per root, and decide whether the seq lowering needs
    // every step to register a read of pc. probe:
    // design/review-2026-10-19/repro/x-perf-runtime-seq-pc-readers.sh
    // (x-perf-runtime-04)
    // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no ruling
    // needed. Group readers by root so `updated` is touched once per root; drop the
    // per-step pc read if the lowering does not need it.
    // 2026-10-26 claude: Not fixed in L4-b06. Re-measured on a loaded host:
    // 106/87/160/369 us per step at S=400/800/1600/3200 (debug), noisy. Grouping
    // by_ref's readers by root saves one updated entry per reader but leaves one
    // boundary mark per reader, so a seq delivery stays O(S); the O(S) goes only if a
    // step's sleeping arms stop reading pc (each step reads it in its snapshot and its
    // value gate, --expand) or a mark skips a boundary whose arm sleeps, both changes
    // to the seq lowering or the wake catch-up that want their own pass.
    fn wake_readers(&mut self, id: &BindId) {
        let cycle = self.ctx.rt.cycle;
        if let Some(readers) = self.ctx.rt.by_ref.get(id) {
            for (r, _) in readers.iter() {
                self.ctx.rt.updated.entry(r.root).or_insert(false);
                r.mark(cycle);
            }
        }
    }

    /// Deliver `u` to `id` this cycle: into the store, which advances here
    /// for a written variable as at a let's publication (a patch resolves
    /// against it now, so patches to one root land in order on each
    /// other's result), and the event as FIRED. A reply
    /// nothing references any more goes nowhere.
    fn deliver(&mut self, id: BindId, u: VarUpdate, via: Via) {
        if matches!(via, Via::Reply) && !self.ctx.rt.by_ref.contains_key(&id) {
            return;
        }
        let v = match u {
            VarUpdate::Set(v) => v,
            VarUpdate::Patch(path, v, writer) => {
                let dropped = |why: &dyn std::fmt::Display| match &writer {
                    Some(w) => error!(
                        target: FAILURE_TARGET,
                        "a write through a reference {} was dropped: {why}",
                        diagnostic_site(w)
                    ),
                    None => error!(
                        target: FAILURE_TARGET,
                        "a write through a reference was dropped: {why}"
                    ),
                };
                let Some(cur) = self.ctx.rt.store_value(&id) else {
                    return dropped(&"its root has no value");
                };
                let written = coretraits::with_hooks(&mut self.ctx.view(), || {
                    place::write_path(&cur, &path, v)
                });
                match written {
                    Ok(nv) => nv,
                    Err(err) => return dropped(&err),
                }
            }
        };
        self.ctx.rt.store_insert(id, graphix_compiler::TagValue::fired(v.clone()));
        self.ctx.event.variables.insert(id, graphix_compiler::TagValue::fired(v));
        if matches!(via, Via::Reply) {
            self.replied.push(id);
        }
        self.wake_readers(&id);
    }

    /// After a cycle the interrupt cut short: a reply no reader reached,
    /// its boundary unvisited (a dispatch or a loop the interrupt skipped),
    /// is delivered again next cycle, so the builtin waiting on it is not
    /// left waiting forever. A reply some reader reached was consumed.
    fn redeliver_unseen_replies(&mut self) {
        let cycle = self.ctx.rt.cycle;
        let interrupted = self.ctx.control.interrupted();
        for id in self.replied.drain(..) {
            // XCR claude for claude: [bug/medium] A visited boundary is not a reader
            // reached. A builtin passed directly as a collection callback has no
            // boundary of its own: its reply reader registers under the enclosing
            // statement, which the cycle visits before the slot loop stops on
            // ctx.interrupted(). A reply landing in the interrupted cycle is counted as
            // seen and dropped, and the slot waits forever (5 of 11 runs; the
            // lambda-wrapped callback is redelivered). Give each slot call its own
            // Prune boundary, or have readers mark their own replies consumed. probe:
            // design/review-2026-10-26/repro/x-refs-catchup-interrupt-builtin-slot-reply.sh
            // (x-refs-catchup-07)
            // 2026-10-26 claude: The reported scenario no longer loses the reply: the
            // fold's own call (a GXLambda) stops at the interrupt before its body, whose
            // boundary the reply's reader sits in, so the reply is redelivered
            // (stdlib/graphix-tests
            // lib_tests::interrupt::interrupted_slot_reply_is_delivered_again drives it
            // deterministically: two wedges, both timers landing in the second; it passes
            // with and without the change below, and the REPL repro printed the fold in 16
            // of 16 runs at HEAD and with the review binary). The gap the CR names is still
            // there for an interrupt that lands inside the slot loop: a slot that may call
            // a builtin had no boundary of its own. Callback::call now puts every such slot
            // (a builtin definition, or a dynamic callee) in a boundary no cycle skips
            // (Prune::fence; Prune's ride state is an Option, None for a fence), so every
            // interrupt cut point sits at a boundary edge. No pin fails without it: that
            // window needs an interrupt landing mid-loop, which no test drives
            // deterministically. Review whether the fence is worth its per-slot boundary.
            // 2026-10-27 claude: Checked: Callback::call puts every slot whose callee may
            // be a builtin (a builtin def or a dynamic callee) inside Prune::fence. A fence
            // is a boundary no cycle skips, so its visit means the walk reached the slot.
            // It is never imaged (image_encode refuses NOT_QUIESCENT, and slots exist only
            // after a cycle). This is the CR's own suggested remedy, and CLAUDE.md's
            // walk-pruning rule now states it. Two things are open: the fence has no
            // failing pin, and I see no deterministic way to land an interrupt between two
            // builtin-callee slots (a builtin slot cannot spin, and a lambda callee already
            // has its instance boundary); and the note asks whether one boundary per
            // builtin slot is worth its cost. That trade-off is Eric's call. If he accepts
            // an unpinned containment fence, delete this; if not, revert the fence and
            // write a CR for redelivery keyed per reader.
            let seen = self.ctx.rt.by_ref.get(&id).is_none_or(|readers| {
                readers
                    .iter()
                    .any(|(r, _)| r.boundary.as_ref().is_none_or(|b| b.visited(cycle)))
            });
            if !interrupted || seen {
                continue;
            }
            if let Some(u) = self.ctx.event.take_custom(&id) {
                self.ctx.rt.custom_updates.push(id, u, Via::Reply);
            } else if let Some((tv, _)) = self.ctx.rt.store_get(&id) {
                let u = VarUpdate::Set(tv.value_cloned());
                self.ctx.rt.var_updates.push(id, u, Via::Reply);
            }
        }
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
        if matches!(via, Via::Reply) {
            self.replied.push(id);
        }
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
                    if let Some(mut n) = self.nodes.remove(&id) {
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
                    if self.admits(id, &v) {
                        let mut one = SETS.take();
                        one.push((id, v));
                        sets.push(EmbedderWrite::Set(one))
                    }
                }
                // one message, one set, one cycle (GXHandle::set_many)
                ToGX::SetMany { sets: set } => {
                    if set.iter().all(|(id, v)| self.admits(*id, v)) {
                        sets.push(EmbedderWrite::Set(set))
                    }
                }
                ToGX::DeleteCallable { id } => self.delete_callable(id, sets),
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
                        .map(|m| m.iter().map(|(_, n)| n as usize).sum::<usize>())
                        .sum();
                    let _ = res.send(crate::EnvStats {
                        by_id_len,
                        ref_var_keys,
                        ref_var_total,
                        store_len: self.ctx.rt.store.len(),
                        lambda_defs_len: self.ctx.lambda_defs.len(),
                        lowerings_len: self.ctx.lowerings_len(),
                        restored: self.restored,
                    });
                }
                ToGX::FusionStats { res } => {
                    let _ = res.send(self.ctx.fusion.stats());
                }
                ToGX::WaitIdle { res } => self.idle_waiters.push(res),
                ToGX::NotifyIdle => self.notify_idle = true,
                ToGX::WaitSettled { res } => self.settle_waiters.push(res),
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
        !self.held_answers.is_empty()
            || !self.ctx.rt.updated.is_empty()
            || !self.ctx.rt.var_updates.is_empty()
            || !self.ctx.rt.custom_updates.is_empty()
    }

    async fn compile_root(&mut self, flags: BitFlags<CFlag>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())
            .with_context(|| format!("parsing the root module {ori}"))?;
        let exprs =
            join_in_order(exprs.iter().map(|e| e.resolve_modules(&self.resolvers, None)))
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

    /// Compile and install the statements of `text`, all or none: a
    /// failure leaves the session as it was.
    async fn compile(&mut self, rt: GXHandle<X>, text: ArcStr) -> Result<CompRes<X>> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            join_in_order(exprs.iter().map(|e| e.resolve_modules(&self.resolvers, None)))
                .await?;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let scope = self.scope.clone();
        let saved = Saved::take(&self.ctx.cx);
        let mut nodes: LPooled<Vec<_>> = LPooled::take();
        let mut failed = None;
        for e in exprs.iter() {
            let stmt = graphix_compiler::compile_stmt(
                &mut self.ctx.view(),
                self.flags,
                &self.scope,
                e.clone(),
            );
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
        if let Some(e) = failed {
            for mut n in nodes.drain(..) {
                n.delete(&mut self.ctx.view());
            }
            self.scope = scope;
            saved.restore(&mut self.ctx.cx);
            return Err(e);
        }
        let comp_exprs = exprs
            .iter()
            .zip(nodes.drain(..))
            .map(|(e, n)| {
                let output = is_output_kind(&e.kind);
                let typ = n.typ().clone();
                self.ctx.rt.updated.insert(e.id, true);
                self.nodes.insert(e.id, n);
                CompExp { id: e.id, output, typ, rt: rt.clone() }
            })
            .collect::<SmallVec<[_; 1]>>();
        Ok(CompRes { exprs: comp_exprs, env: self.ctx.env.clone() })
    }

    async fn load_exprs(
        &self,
        source: &Load,
        resolvers: &Resolvers,
        probes: Option<&Probes>,
    ) -> Result<(Origin, Arc<[Expr]>, Option<Sig>)> {
        let (ori, exprs, sig) = match source {
            Load::File(file) => {
                let overrides = resolvers.iter().find_map(|r| r.overrides());
                let root = RootFile::load(file, overrides.as_ref(), probes).await?;
                (root.ori, root.exprs, root.sig)
            }
            Load::Netidx(_) => {
                // Non-file transports are fetched by whichever resolver claims
                // the source.
                let source = source.source();
                let fetch = self
                    .resolvers
                    .iter()
                    .find_map(|r| r.fetch_source(&source))
                    .ok_or_else(|| {
                    anyhow!("no module resolver can load {source:?}")
                })?;
                let src = fetch.await?;
                let ori = Origin { parent: None, source, text: src };
                (ori.clone(), expr::parser::parse(ori)?, None)
            }
            Load::Text(src) => {
                let ori =
                    Origin { parent: None, source: Source::Program, text: src.clone() };
                (ori.clone(), expr::parser::parse(ori)?, None)
            }
        };
        Ok((ori, exprs, sig))
    }

    async fn check(
        &mut self,
        source: &Load,
        resolver_override: Option<Vec<ResolverRef>>,
        initial_scope: Option<ArcStr>,
        expr_types: bool,
    ) -> Result<crate::CheckResult> {
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
        let mut nodes: LPooled<Vec<Node<GXRt<X>>>> = LPooled::take();
        let go = async {
            let st = Instant::now();
            // A package root is the body of `mod <package>`, recompiled
            // over the copy registered at startup.
            let overrides = resolvers_for_call.iter().find_map(|r| r.overrides());
            // a package root is the body of a module under its name; a lone
            // root is a script, its interface checked over its names
            let (ori, exprs, modules_at, as_module, sig) = match (&initial_scope, source)
            {
                (Some(name), Load::File(file)) => {
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
            let exprs = join_in_order(exprs.iter().map(|e| {
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
            let res = res.and_then(|()| self.warn_unused_imports(&env, flags));
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
            Ok(crate::CheckResult { env, ide })
        };
        // a checker bug in one buffer must not end the runtime: the check
        // answers the panic and the session is put back
        let res = match AssertUnwindSafe(go).catch_unwind().await {
            Ok(res) => res,
            Err(panic) => {
                for mut n in nodes.drain(..) {
                    n.delete(&mut self.ctx.view());
                }
                self.ctx.view().drop_deferred();
                self.ctx.view().clear_resolution_scratch();
                let what = panic
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| panic.downcast_ref::<String>().map(|s| s.as_str()))
                    .unwrap_or("a panic");
                Err(anyhow!("the check panicked: {what}"))
            }
        };
        saved.restore(&mut self.ctx.cx);
        res
    }

    async fn load(&mut self, rt: GXHandle<X>, source: &Load) -> Result<CompRes<X>> {
        let ProgramRoot { id, output, typ, .. } = self.load_program(source, None).await?;
        let res = smallvec![CompExp { id, output, typ, rt: rt.clone() }];
        Ok(CompRes { exprs: res, env: self.ctx.env.clone() })
    }

    /// Warn of each import a compile since `since` added that no lookup
    /// went through, the check and the run alike.
    fn warn_unused_imports(
        &self,
        since: &graphix_compiler::env::Env,
        flags: BitFlags<CFlag>,
    ) -> Result<()> {
        self.ctx.env.unused_imports_since(since).try_for_each(|(key, e)| {
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
    }

    /// Compile the program in `source`, adding every place its module
    /// resolution looked to `probes`.
    async fn load_program(
        &mut self,
        source: &Load,
        probes: Option<&Probes>,
    ) -> Result<ProgramRoot> {
        let scope = Scope::root();
        let st = Instant::now();
        let (ori, exprs, sig) = self.load_exprs(source, &self.resolvers, probes).await?;
        info!("parse time: {:?}", st.elapsed());
        let st = Instant::now();
        let exprs = join_in_order(
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
        let before = self.ctx.env.clone();
        let mut n = graphix_compiler::compile_script(
            &mut self.ctx.view(),
            self.flags,
            &scope,
            wrapped,
            &stmts,
            sig.as_ref(),
        )
        .with_context(|| ori.clone())?;
        if let Err(e) = self.warn_unused_imports(&before, self.flags) {
            n.delete(&mut self.ctx.view());
            return Err(e.context(ori.clone()));
        }
        let typ = n.typ().clone();
        self.nodes.insert(id, n);
        self.ctx.rt.updated.insert(id, true);
        Ok(ProgramRoot { id, output, typ, warnings: vec![] })
    }

    fn compile_callable(&mut self, v: Value, rt: GXHandle<X>) -> Result<Callable<X>> {
        let lb = v
            .downcast_ref::<LambdaDef<GXRt<X>>>()
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
        // a call is the delivery of its arguments: with none it would run
        // once, now, and never when called
        if ftype.args.is_empty() {
            bail!(
                "a callable needs an argument without a default to call it with \
                 (`|_: null| ..`)"
            )
        }
        let args: Box<[BindId]> = ftype.args.iter().map(|_| BindId::new()).collect();
        let argn = ftype.args.iter().zip(args.iter());
        let argn = argn
            .map(|(arg, id)| {
                genn::reference(&mut self.ctx.view(), *id, arg.typ.clone(), eid)
            })
            .collect::<smallvec::SmallVec<[_; 2]>>();
        let fnode = genn::constant(v.clone(), Type::Fn(lb.typ.clone()));
        let mut n = genn::apply(fnode, Scope::root(), argn, &ftype, eid)?;
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
        self.ctx.rt.updated.insert(eid, true);
        let cid = CallableId::new();
        let answer = ExprId::new();
        self.callables
            .insert(cid, CallableInt { expr: eid, answer, args, typ: ftype.clone() });
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
        crate::check_args(&c.typ, &self.ctx.env, &args)?;
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

    /// Delete a callable: its calls are made, with nothing to answer,
    /// next cycle, and its arguments' stored and pending values go.
    fn delete_callable(&mut self, id: CallableId, sets: &mut Vec<EmbedderWrite>) {
        if let Some(c) = self.callables.remove(&id) {
            for call in self.calls.iter_mut().filter(|call| call.expr == c.expr) {
                call.arg = None
            }
            for w in sets.iter_mut() {
                if let EmbedderWrite::Set(set) = w {
                    set.retain(|(id, _)| !c.args.contains(id));
                }
            }
            sets.retain(|w| !matches!(w, EmbedderWrite::Set(set) if set.is_empty()));
            for a in c.args.iter() {
                self.ctx.rt.forget_writes(*a);
                self.ctx.rt.store_remove(a);
            }
            if let Some(mut n) = self.nodes.remove(&c.expr) {
                n.delete(&mut self.ctx.view())
            }
        }
    }

    pub(super) async fn run(
        mut self,
        mut to_rt: tmpsc::UnboundedReceiver<ToGX<X>>,
        aborted: &Notify,
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
                    drain_ready(&mut self.ctx.rt.watches, &mut custom_tasks)
                };
                (var_watches) => {
                    drain_ready(&mut self.ctx.rt.var_watches, &mut tasks)
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
            // the send loop parks the input it serves in `sets`
            let ready = self.cycle_ready()
                || !tasks.is_empty()
                || !custom_tasks.is_empty()
                || !input.is_empty()
                || !sets.is_empty();
            // An idle waiter resolves once the runtime is idle, confirmed on
            // a second consecutive pass: the first may race an in-flight
            // spawned task.
            let mut idle_owed = false;
            if !ready {
                // a settle waiter waits out what is outstanding without
                // the grace loop: a finished task or a fire wakes it
                let settled =
                    !self.settle_waiters.is_empty() && !self.ctx.rt.outstanding();
                let waiter = self.trace.is_some()
                    || !self.idle_waiters.is_empty()
                    || self.notify_idle
                    || settled;
                if waiter && idle_passes == 0 {
                    idle_passes = 1;
                } else {
                    if self.notify_idle {
                        match self.sub.try_reserve() {
                            Ok(permit) => {
                                let mut batch = self.batch_pool.take();
                                batch.push(GXEvent::Idle);
                                permit.send(batch);
                                self.notify_idle = false;
                            }
                            Err(tmpsc::error::TrySendError::Full(())) => idle_owed = true,
                            Err(tmpsc::error::TrySendError::Closed(())) => {
                                self.notify_idle = false
                            }
                        }
                    }
                    // the verdict's Idle precedes its waiters' answers: an
                    // owed one answers them at the verdict after its send
                    if !idle_owed {
                        for tx in self.idle_waiters.drain(..) {
                            let _ = tx.send(());
                        }
                    }
                    if settled {
                        for tx in self.settle_waiters.drain(..) {
                            let _ = tx.send(());
                        }
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
                up = next_or_ended(&mut self.ctx.rt.watches) => {
                    if let Some(mut up) = up {
                        for v in up.drain(..) {
                            custom_tasks.push(v);
                        }
                    }
                    peek!(watches, tasks, var_watches, custom_tasks, input)
                },
                up = next_or_ended(&mut self.ctx.rt.var_watches) => {
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
                permit = reserve_or_never(&self.sub, idle_owed) => {
                    if let Ok(permit) = permit {
                        let mut batch = self.batch_pool.take();
                        batch.push(GXEvent::Idle);
                        permit.send(batch);
                    }
                    self.notify_idle = false;
                    continue 'main
                },
                _ = aborted.notified() => (),
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
            // CR claude for claude: [bug/medium] A runtime the activation budget aborted
            // can leave the shell's process hung forever instead of exiting.
            // graphix-shell/tests/script_exit.rs a_stopped_runtime_says_why hung in the
            // gate of 2026-10-24; the same test passed in the day's earlier gates. The
            // program: `let rec f = |n: i64| -> i64 select n { 0 => 0, k => k + f(k -
            // 1) }; let depth = 0; depth <- select depth { d if d < 100 => d + 10, _ =>
            // never() }; sys::exit(sys::time::after_idle(duration:300.ms, depth ~ 0));
            // f(depth)` under GRAPHIX_ACTIVATION_BUDGET=20, run as `graphix --no-netidx
            // --log-dir <dir>/log <dir>/main.gx` with stdin null and XDG_CACHE_HOME in a
            // tempdir. It was a warm run: the log said "Program image <path>" (loaded, not
            // written), then "ERROR [graphix_types::stack] activation budget (20) exceeded
            // by a recursion — aborting the runtime", and nothing more. stderr never got
            // the shell's GXHandle::stopped message ("a runaway recursion exceeded the
            // runtime's budget"), so the shell did not see the runtime stop. After 47
            // minutes the process had used 0 s of CPU: 18 threads, 15 tokio-rt-worker in
            // futex_wait, one tokio-rt-worker in do_epoll_wait, the main `graphix` thread
            // and `graphix-tokio` in futex_wait (/proc/<pid>/task/*/wchan; gdb could not
            // attach). A deadlock or a lost wakeup on the abort path, not a spin.
            // Candidates: this check and the `aborted.notified()` arms here and in the
            // batch send above (a notify_one with no waiter registered yet is stored, but
            // a second abort path or a send blocked on `sub.reserve()` could still miss
            // it), and the shell's wait for the runtime's end in script mode. Not
            // reproduced: 30 warm runs of the same program, same env, each exited within
            // 20 s. To catch it, loop the test (or the program under timeout) until one
            // hangs, then read the threads with gdb as root or `eu-stack -p`.
            // (rt-budget-hang-01)
            // 2026-10-24 claude: Not reproduced in about 2700 more runs: 400 and 1280 warm
            // runs at 16 and 32 parallel loops, and 1280 runs in cold_then_warm's exact
            // sequence (a fresh cache, one cold run, four warm) at 32 loops on 16 cores.
            // Ruled out: the eval pool (the hung process had no pool threads, so no fork
            // had run). The main thread waits for graphix-tokio, which was parked in
            // block_on, so Shell::run never returned; it returns when from_gx or the exit
            // request's sender closes, and both close only when GX::run returns and the GX
            // drops. So either the run loop never saw the abort (a budget abort sets
            // Control's Abort flag without notifying the run loop's Notify, which only
            // GXHandle::abort does) or the GX teardown blocked; no blocking Drop was found
            // on that path. Trap for the next one:
            // graphix-shell/tests/common/mod.rs::graphix now gives a run 120 s, then sends
            // it SIGABRT and fails naming the pid, so coredumpctl debug <pid> then thread
            // apply all bt shows every thread (checked on a run that never exits).
            // 2026-10-25 claude: 640 more runs of a_stopped_runtime_says_why's binary, 16
            // at a time at load average 40 to 60, each under XDG_CACHE_HOME of its own:
            // all passed, no hang, and the 120 s trap has caught nothing in today's gates.
            // 2026-10-26 claude: Not reproduced or changed in L4-b07. Re-read the run
            // loop's abort paths: a budget abort is raised only under with_control inside
            // a cycle (activation_born, budget_abort), the send loop breaks on
            // control.aborted() and the loop's top check returns, so the loop cannot miss
            // it; the new idle arm (reserve_or_never, rt-01) is also left by
            // aborted.notified() or the next pass's check. Unchecked: whether Shell::run's
            // load_env (Output::from_expr, Output::chosen through run_on_main) can wait on
            // the main thread while the main thread joins graphix-tokio.
            // 2026-10-27 claude: L4-b07 rerun: the run_on_main hypothesis is closed for
            // this program: its output is an i64, so every package's is_custom (gui, tui:
            // shows_as) refuses it before init_custom, and nothing reaches the main
            // thread; the one awaited package hook, tui's output_chosen, goes through
            // gx.with_ctx, whose request fails once the run loop returns and drops to_rt.
            // Also checked: the image receivers Shell::store awaits are all sent or
            // dropped inside GX::new, before start returns. 1240 more warm runs (40
            // serial, then 2 loops of 600 under the lane's capped wrapper, debug build at
            // cr26-staging 9022bfe18): all exited with the stopped message, none hung.
            // Still open: a deadlock on a lock inside the aborted cycle's unwind or the
            // GX's drop would look the same (every thread in futex_wait), and only a core
            // from the 120 s trap would tell.
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
                aborted,
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
