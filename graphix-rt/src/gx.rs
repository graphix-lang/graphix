use crate::{JitUnavailable, ProgramImage, RegistrationImage};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use enumflags2::BitFlags;
use futures::{FutureExt, Stream, StreamExt, stream::SelectAll};
use graphix_compiler::{
    BindId, CFlag, CustomBuiltinType, ExecState, FAILURE_TARGET, FusionStats, Late, Node,
    Rt, Saved, Scope, TagValue, View, WriteTarget, diagnostic_site,
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
    shown_type,
    typ::{FnType, Type, tvar::current_level},
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
    Ref, Settle, ToGX, TraceEvent, TraceSegment, rt::Via,
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

// CR claude for claude: [bug/medium] An empty statement (ExprKind::NoOp, from an empty
// line, a comment-only line or a trailing `;`) counts as an output here. In the REPL an
// empty line prints `-: _` and becomes the value display, so at a terminal the prompt
// does not come back until Ctrl-C and lines typed meanwhile are dropped. Piped, it
// replaces the value being shown. `let b = 1;` prints `-: _` and `x + 1;` shows
// nothing. Fix: drop NoOp statements from what GX::compile installs and returns, so the
// last real statement of a line decides the display, and have the shell change nothing
// when an input installs nothing. Classifying NoOp as non-output alone would leave `x +
// 1;` running unseen in the shell's `exprs`. probe:
// design/review-2026-11-16/repro/x-repl-empty-input.py Also reported as (rt-06).
// (x-repl-01)
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

/// The embedder's sources, which a held runtime does not poll.
async fn sources_unless<X: GXExt>(ext: &mut X, held: bool) -> Result<()> {
    match held {
        true => future::pending().await,
        false => ext.update_sources().await,
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

/// How many cycles in a row started by writes alone are a report
/// ([`GX::spinning`]): past any loop a test or a bench runs on purpose,
/// and about a second of a program that spins.
const SPIN_CYCLES: u32 = 1 << 20;

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

pub(crate) mod debug;

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
    /// Cycles in a row that only queued variable writes started
    /// ([`Self::spinning`]).
    spin: u32,
    sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    resolvers: Resolvers,
    batch_pool: Pool<Vec<GXEvent>>,
    flags: BitFlags<CFlag>,
    /// Pending `WaitIdle` requests, answered at the next idle verdict.
    idle_waiters: Vec<oneshot::Sender<()>>,
    /// A `NotifyIdle` is pending: the next idle verdict sends
    /// [`GXEvent::Idle`].
    notify_idle: bool,
    /// Pending `WaitSettled` requests, each answered at the first idle
    /// verdict with nothing its [`Settle`] names outstanding.
    settle_waiters: Vec<(Settle, oneshot::Sender<()>)>,
    /// The program compiled or restored at construction; a compile
    /// failure is kept for the embedder to report, the runtime starts.
    program: Option<Result<ProgramRoot, String>>,
    /// The regions fused when the program's compile or restore ended.
    program_fused: usize,
    /// The registration was restored from an image, not compiled.
    restored: bool,
    /// Active trace recording, if any. See [`GXHandle::trace_start`].
    trace: Option<TraceState>,
    /// The session scope for statement-at-a-time compiles: a top-level
    /// `catch(e) expr` advances it so later inputs compile under its
    /// coverage. File loads do not touch it.
    scope: Scope,
    /// The debugger sessions attached.
    debug: debug::Sessions,
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

    pub(super) fn fusion_stats(&self) -> FusionStats {
        self.ctx.fusion.stats()
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
        if !fusion_on(cfg.flags) {
            ctx.fusion.disable();
        }
        let mut t = Self {
            ctx,
            nodes: Roots::default(),
            callables: IntMap::default(),
            calls: VecDeque::new(),
            held_answers: Vec::new(),
            spin: 0,
            sub: cfg.sub,
            resolvers: std::sync::Arc::from(cfg.resolvers),
            batch_pool: Pool::new(10, 1000000),
            flags: cfg.flags,
            idle_waiters: Vec::new(),
            notify_idle: false,
            settle_waiters: Vec::new(),
            trace: None,
            scope: Scope::root(),
            debug: debug::Sessions::starting(cfg.paused),
            program: None,
            program_fused: 0,
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
            let loaded = t.load_program(&source, probes.as_ref(), 0).await;
            let warnings = t.ctx.env.recorded_warnings();
            match loaded {
                Ok(root) => {
                    t.program = Some(Ok(ProgramRoot { warnings, ..root }));
                    info!("program init time: {:?}", st.elapsed());
                    if let (Some(tx), Some(probes)) = (cfg.program_image, probes) {
                        let image = if fusion_on(t.flags) && !t.ctx.fusion.available() {
                            Err(anyhow::Error::new(JitUnavailable))
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
        t.program_fused = t.ctx.fusion.fused();
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
        self.ctx.rt.running = true;
        while let Some((i, init)) = due.pop() {
            let (id, n) = self.nodes.at_mut(i).expect("scheduled root");
            self.ctx.event.view = if init { View::Birth } else { View::Cycle };
            self.ctx.event.root = Some(id);
            // Only a FIRED production becomes an event.
            let tv = n.update(&mut self.ctx.view());
            if tv.is_fired() {
                let v = tv.value_cloned();
                if let Some(tr) = self.trace.as_mut() {
                    tr.record(self.ctx.rt.cycle, id, &v);
                }
                batch.push(GXEvent::Updated(id, v))
            }
            self.readers_in_other_roots(i);
            if self.ctx.rt.updated.len() != known {
                known = self.ctx.rt.updated.len();
                due = scheduled(self, Some(i));
            }
        }
        self.ctx.rt.running = false;
        self.ctx.event.root = None;
    }

    /// The readers in other roots of what root `i` fired: one in a root
    /// before it, which the cycle passed, reads the fire next cycle; one
    /// in a root after it runs this cycle.
    fn readers_in_other_roots(&mut self, i: u64) {
        let rt = &mut self.ctx.rt;
        for id in rt.produced.drain(..) {
            let (Some(readers), Some((tv, _))) = (rt.by_ref.get(&id), rt.store.get(&id))
            else {
                continue;
            };
            for (r, _) in readers.iter() {
                match self.nodes.seq(&r.root) {
                    Some(s) if s < i && r.late => {
                        rt.late.push((id, tv.clone(), r.clone()))
                    }
                    Some(s) if s > i => {
                        rt.updated.entry(r.root).or_default();
                    }
                    _ => (),
                }
            }
        }
    }

    /// Deliver last cycle's late fires ([`GXRt::late`]) to their readers:
    /// each reader's root runs and its boundary is marked, unless a write
    /// delivered the variable already this cycle.
    fn deliver_late(&mut self) {
        if self.ctx.rt.late.is_empty() {
            return;
        }
        let cycle = self.ctx.rt.cycle;
        let mut late = Late::default();
        for (id, tv, r) in self.ctx.rt.late.drain(..) {
            if self.ctx.event.variables.contains_key(&id) {
                continue;
            }
            self.ctx.rt.updated.entry(r.root).or_insert(false);
            r.mark(cycle);
            let fires = late.entry(id).or_default();
            // CR claude for claude: [perf/low] The late-fire dedupe is a linear scan
            // per entry, and late_fire's lookup is another linear find per read. The
            // seq machine's same-cycle handoffs queue about 1.5*N^2 late pc fires over
            // about 3N readers, so when the next cycle delivers no pc (the run waits on
            // a step) this loop is O(N^3): a 400-step seq with a final 1 ms wait costs
            // 129 ms per run against 15 ms without the wait. Fix: key Late by reader
            // per variable (Reader is Hash and Eq) so the dedupe and late_fire are each
            // one probe. probe:
            // design/review-2026-11-16/repro/x-perf-runtime-late-dedupe-cubic.sh
            // (x-perf-runtime-03)
            if !fires.iter().any(|(_, known)| *known == r) {
                fires.push((tv, r))
            }
        }
        if !late.is_empty() {
            self.ctx.event.late = Some(Arc::new(late));
        }
    }

    /// Deliver what waits for this cycle: the writes queued for it, what
    /// tasks and the embedder sent, the calls made, and last cycle's late
    /// fires. `answering` takes each call made that is owed an answer.
    fn deliver_pending(
        &mut self,
        tasks: &mut Vec<(BindId, Value)>,
        sets: &mut Vec<EmbedderWrite>,
        custom_tasks: &mut Vec<(BindId, Box<dyn CustomBuiltinType>)>,
        answering: &mut LPooled<Vec<(ExprId, ExprId)>>,
    ) {
        // A variable takes one delivery a cycle: what waited lands first,
        // each variable's oldest write, then this cycle's input, which waits
        // behind anything its variable has waiting or was given.
        let mut due: LPooled<Vec<(BindId, VarUpdate, Via)>> = LPooled::take();
        self.ctx.rt.var_updates.take(&mut due);
        let by_writes = tasks.is_empty()
            && sets.is_empty()
            && custom_tasks.is_empty()
            && self.ctx.rt.custom_updates.is_empty()
            && !due.is_empty()
            && due.iter().all(|(_, _, via)| !matches!(via, Via::Reply));
        self.spinning(by_writes, &due);
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
        self.calls_made(answering);
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
        self.deliver_late();
    }

    async fn do_cycle(
        &mut self,
        quiet: bool,
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
        let mut answering: LPooled<Vec<(ExprId, ExprId)>> = LPooled::take();
        let parked = match quiet {
            false => {
                self.deliver_pending(tasks, sets, custom_tasks, &mut answering);
                None
            }
            true => Some(self.debug.park(&mut self.ctx.rt.updated)),
        };
        let stepped = !quiet
            && (!self.ctx.event.variables.is_empty()
                || !self.ctx.event.custom.lock().is_empty()
                || self.ctx.rt.updated.keys().any(|id| !self.debug.owns(id)));
        let worked = !self.ctx.rt.updated.is_empty()
            || !self.ctx.event.variables.is_empty()
            || !self.ctx.event.custom.lock().is_empty();
        let control = self.ctx.control.clone();
        graphix_compiler::with_control(&control, || {
            self.update_nodes(&mut batch);
            if !quiet {
                for (answer, v) in self.held_answers.drain(..) {
                    batch.push(GXEvent::Updated(answer, v))
                }
            }
            // CR claude for claude: [bug/medium] The answer is held for exactly one
            // cycle. A variable takes one delivery per cycle, so a handler write queued
            // behind another write to the same variable lands after the answer.
            // call_answered promises that the writes its cycle queued come first. A
            // handler `|s: string| { v <- s; v <- "[s]!"; s }` answers between its two
            // writes. Number each write in Waiting and hold the answer until no write
            // queued at or before its cycle's end is still waiting. probe:
            // design/review-2026-11-16/repro/rt-handle-contracts.rs
            // (review_an_answer_comes_before_the_second_write) (rt-02)
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
        });
        if !self.debug.is_empty() {
            self.debug_cycle(&mut batch, quiet, stepped)
        }
        // before the send loop, which may compile roots for the next cycle
        self.ctx.event.clear();
        self.ctx.rt.updated.clear();
        if let Some(mut parked) = parked {
            self.ctx.rt.updated.extend(parked.drain(..))
        }
        if let Some(tr) = self.trace.as_mut() {
            tr.cycle_end(self.ctx.rt.cycle, worked);
        }
        self.ctx.rt.cycle += 1;
        self.send_batch(batch, sets, to_rt, input, aborted).await
    }

    /// Send `batch` to the subscriber. Input is served while it waits for
    /// room: a display's rebuild compiles through it while the shell waits
    /// on the display.
    async fn send_batch(
        &mut self,
        mut batch: GPooled<Vec<GXEvent>>,
        sets: &mut Vec<EmbedderWrite>,
        to_rt: &mut UnboundedReceiver<ToGX<X>>,
        input: &mut Vec<ToGX<X>>,
        aborted: &Notify,
    ) {
        let sub = self.sub.clone();
        let mut open = true;
        loop {
            // a stopped runtime sends what has room and waits for nothing
            if self.ctx.control.interrupted() {
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
                        // a subscriber gone after a stop is the stop
                        Err(_) if self.ctx.control.interrupted() => (),
                        Err(_) => error!("could not send batch"),
                    }
                    break;
                }
                n = to_rt.recv_many(input, INPUT_BATCH), if open => match n {
                    0 => open = false,
                    // a stopped runtime answers nothing more
                    _ if self.ctx.control.interrupted() => (),
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
                let patched =
                    self.ctx.rt.store_value(self.ctx.rt.var_of(id)).and_then(|cur| {
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
    fn wake_readers(&mut self, id: &BindId) {
        let cycle = self.ctx.rt.cycle;
        if let Some(readers) = self.ctx.rt.by_ref.get(id) {
            for (r, _) in readers.iter() {
                self.ctx.rt.updated.entry(r.root).or_insert(false);
                r.mark(cycle);
            }
        }
    }

    /// A cycle that nothing but queued variable writes started, or one that
    /// something else did. [`SPIN_CYCLES`] of the first kind in a row are
    /// reported once, with the variables that cycle's writes land in:
    /// a `<-` that feeds itself runs a cycle a write for as long as it
    /// fires, and says nothing. No intent is judged: a loop that long may
    /// be meant.
    fn spinning(&mut self, by_writes: bool, due: &[(BindId, VarUpdate, Via)]) {
        if !by_writes {
            self.spin = 0;
            return;
        }
        self.spin = self.spin.saturating_add(1);
        if self.spin != SPIN_CYCLES {
            return;
        }
        use std::fmt::Write;
        let mut written: LPooled<String> = LPooled::take();
        for (id, _, _) in due.iter().take(4) {
            let sep = if written.is_empty() { "" } else { ", " };
            let _ = match self.ctx.env.by_id.get(id) {
                Some(b) => write!(written, "{sep}`{}` (at {})", b.name, b.pos),
                None => write!(written, "{sep}a binding with no name"),
            };
        }
        error!(
            target: FAILURE_TARGET,
            "{SPIN_CYCLES} cycles in a row were started by writes alone, with no event \
             between them; this one writes {written}. A `<-` whose right side reads its \
             own target runs a cycle a write: clock it to an event (`on e {{ .. }}`, or \
             `x <- e ~ ..`)"
        )
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
                let Some(cur) = self.ctx.rt.store_value(self.ctx.rt.var_of(id)) else {
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
        self.ctx.view().deliver(id, TagValue::fired(v), graphix_compiler::Notify::Roots)
    }

    /// `id`'s delivery this cycle, or its wait behind what `id` has waiting
    /// or was given.
    fn busy(&self, id: &BindId) -> bool {
        self.ctx.event.variables.contains_key(id) || self.ctx.rt.var_updates.has(id)
    }

    fn write(&mut self, id: BindId, u: VarUpdate, via: Via) {
        let id = self.ctx.rt.aliases.var(id);
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
        for (id, _) in set.iter_mut() {
            *id = self.ctx.rt.aliases.var(*id)
        }
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
                ToGX::Debug(op) => self.debug_op(op, sets).await,
                ToGX::Delete { id } => {
                    if let Some(mut n) = self.nodes.remove(&id) {
                        n.delete(&mut self.ctx.view());
                    }
                    self.debug.forget(&id);
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
                    let _ = res.send((root, self.ctx.env.clone(), self.program_fused));
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
                        aliases_len: self.ctx.rt.aliases.len()
                            + self.ctx.rt.alias_reads.len(),
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
                ToGX::WaitSettled { until, res } => {
                    self.settle_waiters.retain(|(_, tx)| !tx.is_closed());
                    self.settle_waiters.push((until, res))
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
        !self.held_answers.is_empty()
            || !self.ctx.rt.updated.is_empty()
            || !self.ctx.rt.var_updates.is_empty()
            || !self.ctx.rt.custom_updates.is_empty()
            || !self.ctx.rt.late.is_empty()
    }

    async fn compile_root(&mut self, flags: BitFlags<CFlag>, text: ArcStr) -> Result<()> {
        let ori = Origin::new(None, Source::Unspecified, text);
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
        let load = self.ctx.env.block_types.next_load();
        let ori = Origin::new(None, Source::Unspecified, text).loaded(load);
        let exprs = expr::parser::parse(ori.clone())?;
        let exprs =
            join_in_order(exprs.iter().map(|e| e.resolve_modules(&self.resolvers, None)))
                .await?;
        self.prune_static_resolution();
        self.ctx.batch_connect_targets.clear();
        let scope = self.scope.clone();
        let saved = Saved::take(&mut self.ctx.cx);
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
                let typ = shown_type(&self.ctx.env, &self.scope, &n);
                self.ctx.rt.updated.insert(e.id, true);
                self.nodes.insert(e.id, n);
                CompExp { id: e.id, output, typ, rt: rt.clone() }
            })
            .collect::<SmallVec<[_; 1]>>();
        let fused = self.ctx.fusion.fused();
        Ok(CompRes { exprs: comp_exprs, env: self.ctx.env.clone(), fused })
    }

    async fn load_exprs(
        &self,
        source: &Load,
        resolvers: &Resolvers,
        probes: Option<&Probes>,
        load: u32,
    ) -> Result<(Origin, Arc<[Expr]>, Option<Sig>)> {
        let (ori, exprs, sig) = match source {
            Load::File(file) => {
                let overrides = resolvers.iter().find_map(|r| r.overrides());
                let root = RootFile::load(file, overrides.as_ref(), probes, load).await?;
                (root.ori, root.exprs, root.sig)
            }
            // CR claude for eric: [structure/low] A root fetched from netidx takes none
            // of `RootFile::load`'s steps. The `.gxi` beside it is never fetched (sig
            // `None`), so the program runs unchecked against an interface that the same
            // files on disk would be checked against, and a `#!` line is not stripped.
            // The netidx module resolver does fetch a module's interface. Decide
            // whether a netidx root has an interface; either way the root load should
            // be one function over a fetch (`RootFile::load` taking the transport), so
            // the two loaders cannot drift. Found by reading; not run. (x-dup-04)
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
                let ori = Origin::new(None, source, src).loaded(load);
                (ori.clone(), expr::parser::parse(ori)?, None)
            }
            Load::Text(src) => {
                let ori = Origin::new(None, Source::Program, src.clone()).loaded(load);
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
        let load = self.ctx.env.block_types.next_load();
        // a check runs nothing: everything it compiled is rolled back
        let saved = Saved::take(&mut self.ctx.cx);
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
                    let root =
                        RootFile::load(file, overrides.as_ref(), None, load).await?;
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
                        self.load_exprs(source, &resolvers_for_call, None, load).await?;
                    (ori, exprs, ModPath::root(), false, sig)
                }
            };
            let exprs = join_in_order(exprs.iter().map(|e| {
                e.resolve_modules_in_scope(&modules_at, &resolvers_for_call, None)
            }))
            .await?;
            info!("parse and resolve time: {:?}", st.elapsed());
            // CR claude for claude: [risk/low] `check`'s script path repeats
            // `load_program` (load the root, resolve modules, prune, `compile_script`,
            // warn of unused imports). It leaves out one step that every other compile
            // path makes right after pruning: `batch_connect_targets.clear()`
            // (`compile_root`, `compile`, `load_program`). A check therefore runs with
            // the previous batch's `<-` targets still in the batch set, and
            // `CompileCtx::rebinds` answers true for them where the build of the same
            // text answers false; `register_fn_params` then skips a forward resolution
            // the build makes. Pruning hides most of this today, since those ids have
            // already left `bind_to_lambda`. Fix: fold the clear into
            // `prune_static_resolution`, as one step that starts a batch for all four
            // paths, and share the script compile between `check` and `load_program`.
            // Found by reading; not run. (x-dup-03)
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
        let load = self.ctx.env.block_types.next_load();
        let ProgramRoot { id, output, typ, .. } =
            self.load_program(source, None, load).await?;
        let res = smallvec![CompExp { id, output, typ, rt: rt.clone() }];
        let fused = self.ctx.fusion.fused();
        Ok(CompRes { exprs: res, env: self.ctx.env.clone(), fused })
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
        load: u32,
    ) -> Result<ProgramRoot> {
        let scope = Scope::root();
        let st = Instant::now();
        let (ori, exprs, sig) =
            self.load_exprs(source, &self.resolvers, probes, load).await?;
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
        // CR claude for claude: [risk/low] Nothing decides this instance's parameters,
        // so a generic lambda's callable keeps an open type: `|x| x * x` gives fn(x:
        // '_0) -> '_0. check_args's is_a passes every value at an unbound variable
        // whatever its bound, so call("s") reaches the node-walk's `*` instead of being
        // refused. Refuse here when an argument type is undecided after the check,
        // naming the parameter to annotate. probe:
        // design/review-2026-11-16/repro/rt-handle-contracts.rs
        // (review_a_generic_callable) (rt-07)
        let callee = lb.typ.instantiate(current_level());
        let ftype = FnType {
            args: callee.args.iter().filter(|a| !a.has_default()).cloned().collect(),
            ..callee.clone()
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
        let fnode = genn::constant(v.clone(), Type::Fn(Arc::new(callee)));
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
        Ok(Ref {
            id: eid,
            bid: id,
            typ,
            last: self.ctx.rt.store_value(self.ctx.rt.var_of(id)),
            rt,
        })
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
                self.ctx.rt.store_remove(self.ctx.rt.var_of(*a));
            }
            if let Some(mut n) = self.nodes.remove(&c.expr) {
                n.delete(&mut self.ctx.view())
            }
        }
    }

    /// The idle verdict: `Idle` to a subscriber that asked for it, then
    /// the answers of the idle waiters, of the settle waiters `quiet`
    /// holds for and of the trace's. True when the `Idle` is owed, the
    /// subscriber's channel being full: the idle waiters are answered at
    /// the verdict after its send, so it precedes their answers.
    fn answer_idle(&mut self, quiet: impl Fn(&Settle) -> bool) -> bool {
        let mut idle_owed = false;
        if self.notify_idle {
            match self.sub.try_reserve() {
                Ok(permit) => {
                    let mut batch = self.batch_pool.take();
                    batch.push(GXEvent::Idle);
                    permit.send(batch);
                    self.notify_idle = false;
                }
                Err(tmpsc::error::TrySendError::Full(())) => idle_owed = true,
                Err(tmpsc::error::TrySendError::Closed(())) => self.notify_idle = false,
            }
        }
        if !idle_owed {
            for tx in self.idle_waiters.drain(..) {
                let _ = tx.send(());
            }
        }
        for (_, tx) in self.settle_waiters.extract_if(.., |(until, _)| quiet(until)) {
            let _ = tx.send(());
        }
        if let Some(tr) = self.trace.as_mut() {
            tr.resolve(self.ctx.rt.cycle.saturating_sub(1));
        }
        idle_owed
    }

    /// What is left of a runtime whose program ended itself
    /// (`Control::exit`): it runs no cycle, so nothing the program left
    /// queued or in flight is delivered and no write of its embedder's
    /// lands, and it answers its embedder as an idle runtime does until
    /// the embedder stops it.
    async fn run_exited(
        &mut self,
        to_rt: &mut UnboundedReceiver<ToGX<X>>,
        aborted: &Notify,
        sets: &mut Vec<EmbedderWrite>,
        input: &mut Vec<ToGX<X>>,
    ) {
        while !self.ctx.control.interrupted() {
            sets.clear();
            self.calls.clear();
            let idle_owed = self.answer_idle(|_| true);
            select! {
                permit = reserve_or_never(&self.sub, idle_owed) => {
                    if let Ok(permit) = permit {
                        let mut batch = self.batch_pool.take();
                        batch.push(GXEvent::Idle);
                        permit.send(batch);
                    }
                    self.notify_idle = false;
                    continue
                },
                _ = aborted.notified() => continue,
                n = to_rt.recv_many(input, INPUT_BATCH) => if n == 0 {
                    break
                },
            }
            let mut batch = self.batch_pool.take();
            self.process_input_batch(sets, input, &mut batch).await;
            if !batch.is_empty() {
                self.send_batch(batch, sets, to_rt, input, aborted).await
            }
        }
    }

    // XCR claude for claude: [risk/medium] A `<-` whose right side reads its own target
    // unsampled runs a cycle per write forever; the check accepts it by ruling (the
    // same shape is a legitimate loop) and this loop says nothing. In the probe `rows
    // <- array::map(rows, ..)` without its `edit ~` passes the check and prints 8,800
    // lines in five seconds; the admin port's ledger records three earlier sightings.
    // The runtime can tell without judging intent: count consecutive cycles that only
    // queued variable writes made ready, and past a threshold log once under
    // FAILURE_TARGET the variable that keeps the loop going. probe:
    // design/review-2026-11-16/repro/a-language-table-sort.gx (remove `edit ~` from the
    // `rows <-` line) (a-language-07)
    // 2026-11-27 claude: Eric ruled (2026-11-27, going through the language design
    // review): build it. The runtime counts consecutive cycles that only queued
    // variable writes made ready and, past a threshold, logs once under FAILURE_TARGET
    // the variable that keeps the loop going. The threshold is mine to pick and
    // measure.
    // 2026-11-27 claude: Addressed: GX::spinning counts cycles that only queued
    // variable writes started and logs once, at SPIN_CYCLES (2^20) in a row, the
    // variables that cycle's writes land in. What catches a regression: graphix-shell
    // tests/shell/swallowed_error_logs.rs a_connect_that_feeds_itself_is_reported_once
    // (a self-feeding connect is reported once and names `n`; a 300,000-step loop that
    // ends is not).
    pub(super) async fn run(
        mut self,
        to_rt: &mut tmpsc::UnboundedReceiver<ToGX<X>>,
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
            if self.ctx.control.interrupted() {
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
            // A held runtime starts no cycle of the program's: what waits
            // for one keeps waiting, and only what a debugger asks wakes it.
            let held = self.debug.held();
            // the send loop parks the input it serves in `sets`
            let ready = match held {
                true => !input.is_empty() || self.debug.awaited(),
                false => {
                    self.cycle_ready()
                        || !tasks.is_empty()
                        || !custom_tasks.is_empty()
                        || !input.is_empty()
                        || !sets.is_empty()
                        || self.debug.awaited()
                }
            };
            // An idle waiter resolves once the runtime is idle, confirmed on
            // a second consecutive pass: the first may race an in-flight
            // spawned task.
            let mut idle_owed = false;
            if !ready && !held {
                // a settle waiter waits out what is outstanding without
                // the grace loop: a finished task or a fire wakes it
                let replies = self.ctx.rt.replies_pending();
                let sources = self.ctx.rt.outstanding();
                let quiet = |until: &Settle| match until {
                    Settle::Replies => !replies,
                    Settle::Sources => !sources,
                };
                let settled = self.settle_waiters.iter().any(|(until, _)| quiet(until));
                let waiter = self.trace.is_some()
                    || !self.idle_waiters.is_empty()
                    || self.notify_idle
                    || settled;
                if waiter && idle_passes == 0 {
                    idle_passes = 1;
                } else {
                    idle_owed = self.answer_idle(quiet);
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
                r = sources_unless(&mut self.ctx.rt.ext, held) => {
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
            if self.ctx.control.interrupted() {
                return Ok(());
            }
            let mut batch = self.batch_pool.take();
            self.process_input_batch(&mut sets, &mut input, &mut batch).await;
            // a held runtime runs a cycle only for what a session waits on
            let quiet = self.debug.held();
            if quiet && !self.debug.awaited() {
                if !batch.is_empty() {
                    self.send_batch(batch, &mut sets, to_rt, &mut input, aborted).await
                }
                continue 'main;
            }
            let st = Instant::now();
            // CR claude for claude: [perf/low] This runs after every wake, whether or
            // not anything is ready. Each query that asks nothing of the program
            // (get_env, with_ctx, env_stats, compile_ref, a WaitIdle and its grace
            // pass) starts a cycle and sends an empty batch, and the TUI redraws at
            // every batch. Run the cycle only when the `ready` test holds, and send a
            // batch only when it is non-empty (a Delete's Env events still go out).
            // probe: design/review-2026-11-16/repro/rt-handle-contracts.rs
            // (review_a_query_runs_a_cycle: 10 get_env -> 12 cycles, 12 empty batches)
            // Also reported as (x-concurrency-03). (rt-04)
            self.do_cycle(
                quiet,
                &mut tasks,
                &mut sets,
                &mut custom_tasks,
                to_rt,
                &mut input,
                aborted,
                batch,
            )
            .await;
            if first_cycle {
                first_cycle = false;
                info!("first cycle time: {:?}", st.elapsed());
            }
            if let Some(code) = self.ctx.control.process_exit() {
                use std::io::Write;
                let _ = std::io::stdout().flush();
                let _ = std::io::stderr().flush();
                std::process::exit(code)
            }
            if self.ctx.control.exited() {
                self.run_exited(to_rt, aborted, &mut sets, &mut input).await;
                return Ok(());
            }
        }
    }
}
