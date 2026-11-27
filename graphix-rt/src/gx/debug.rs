//! The runtime's side of the debugger (`design/debugger.md`): the
//! sessions attached, what each watches, and the questions only the
//! runtime's thread can answer. Typed data in and out; the service
//! (`crate::debug`) owns the wire.

use super::{EmbedderWrite, GX, SETS};
use crate::{CompRes, GXEvent, GXExt, GXHandle, GXRt, debug::SessionId};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::{
    BindId, ExecState, Refs, Rt,
    env::{Bind, Env},
    expr::ExprId,
    is_lowered,
    seek::{BindSource, covering, in_file},
    typ::Type,
};
use netidx_core::path::Path as NPath;
use netidx_value::Value;
use nohash::{IntMap, IntSet};
use poolshark::{global::GPooled, local::LPooled};
use smallvec::SmallVec;
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
};
use tokio::sync::{mpsc::UnboundedSender, oneshot};
use triomphe::Arc;

/// How deep [`GX::why`] follows reads with no value.
const WHY_DEPTH: usize = 32;
/// The cycles the log keeps.
const LOG_CYCLES: usize = 256;
/// The deliveries the log keeps of one cycle.
const LOG_WIDTH: usize = 128;

/// What a session asks of the runtime's thread.
pub(crate) enum Op<X: GXExt> {
    Attach {
        session: SessionId,
        events: UnboundedSender<Fired>,
    },
    Detach {
        session: SessionId,
    },
    /// Compile `text` as a REPL input; what its expressions produce goes
    /// to the session, never to the embedder. An [`Op::Cycle`] sent after
    /// it waits out their first cycle.
    Eval {
        session: SessionId,
        text: ArcStr,
        rt: GXHandle<X>,
        res: oneshot::Sender<Result<CompRes<X>>>,
    },
    /// Send [`Fired::Cycled`] with `token` once the next cycle has run:
    /// after everything that cycle produced for the session, and after
    /// every write and compile asked before this.
    Cycle {
        session: SessionId,
        token: u64,
    },
    /// Write `v` to `id` as an embedder does, and [`Op::Cycle`] in one:
    /// the cycle that answers `token` is the one that delivers the write,
    /// unless the program is held, when the write waits.
    Write {
        session: SessionId,
        id: BindId,
        v: Value,
        token: u64,
    },
    /// Send [`Fired::Held`] with `token` once the program is held: at
    /// once if it is.
    WhenHeld {
        session: SessionId,
        token: u64,
    },
    /// Tell the session each delivery to `ids`; with `stop`, hold the
    /// program after the cycle of one that meets it.
    Watch {
        session: SessionId,
        watch: u64,
        ids: SmallVec<[BindId; 1]>,
        stop: Option<Stop>,
    },
    /// Hold the program: no cycle of its own starts until a session
    /// continues or steps it. Answers the cycle the next one would be.
    Pause {
        session: SessionId,
        res: oneshot::Sender<u64>,
    },
    Continue {
        session: SessionId,
    },
    /// Run until `cycles` more cycles have done the program's work, then
    /// hold it.
    Step {
        session: SessionId,
        cycles: u64,
    },
    /// The last `last` cycles that delivered something to a binding the
    /// program's author wrote, oldest first.
    Cycles {
        last: usize,
        res: oneshot::Sender<Vec<Logged>>,
    },
    Unwatch {
        session: SessionId,
        watch: u64,
        res: oneshot::Sender<bool>,
    },
    Why {
        id: BindId,
        res: oneshot::Sender<Option<Why>>,
    },
    /// The fused blocks that compute a place in a file whose path ends in
    /// `file`: one for each instance fused.
    FusedAt {
        file: PathBuf,
        line: i32,
        column: Option<i32>,
        res: oneshot::Sender<Vec<Fused>>,
    },
    Counts {
        res: oneshot::Sender<Counts>,
    },
}

/// What the runtime tells a session, as it happens.
#[derive(Clone)]
pub(crate) enum Fired {
    Value {
        expr: ExprId,
        value: Value,
    },
    /// The cycle an [`Op::Cycle`] waited for has run; `held` where the
    /// program was held, so that cycle delivered nothing of its own.
    Cycled {
        token: u64,
        held: bool,
    },
    Watch {
        watch: u64,
        bind: Arc<Bind>,
        cycle: u64,
        value: Option<Value>,
    },
    /// The program is held, as of `cycle`.
    Paused {
        cycle: u64,
        by: Paused,
        delivered: Option<Logged>,
    },
    /// The answer to an [`Op::WhenHeld`], after the [`Fired::Paused`] of
    /// the hold it waited for.
    Held {
        token: u64,
        cycle: u64,
    },
    Running,
}

/// Why the program is held.
#[derive(Clone, Copy)]
pub(crate) enum Paused {
    Asked,
    Stepped,
    Break { watch: u64 },
}

/// When a watch holds the program.
pub(crate) enum Stop {
    Fires,
    NoValue,
    Equals(Value),
}

impl Stop {
    fn again(&self) -> Self {
        match self {
            Stop::Fires => Stop::Fires,
            Stop::NoValue => Stop::NoValue,
            Stop::Equals(v) => Stop::Equals(v.clone()),
        }
    }

    fn met(&self, delivered: Option<&Value>) -> bool {
        match (self, delivered) {
            (Stop::Fires, Some(_)) | (Stop::NoValue, None) => true,
            (Stop::Equals(want), Some(v)) => want == v,
            (Stop::Fires, None) | (Stop::NoValue, Some(_)) | (Stop::Equals(_), None) => {
                false
            }
        }
    }
}

/// What one cycle delivered to the bindings the program's author wrote:
/// each with its value, or none where it went to no value. `more` counts
/// those past [`LOG_WIDTH`].
#[derive(Clone)]
pub(crate) struct Logged {
    pub(crate) cycle: u64,
    pub(crate) delivered: Vec<(Arc<Bind>, Option<Value>)>,
    pub(crate) more: usize,
}

/// A binding as the store holds it: its value, if it has one, and the
/// cycle of its last delivery, if it had any.
#[derive(Debug, Clone)]
pub(crate) struct Held {
    pub(crate) bind: Arc<Bind>,
    pub(crate) value: Option<Value>,
    pub(crate) written: Option<u64>,
}

pub(crate) struct Why {
    pub(crate) held: Held,
    pub(crate) source: Option<BindSource>,
    pub(crate) reads: Vec<Why>,
    pub(crate) again: bool,
}

/// A fused block as a box: its text, type and last output
/// ([`BindSource::fused`]), and the bindings it reads as they stand.
pub(crate) struct Fused {
    pub(crate) source: BindSource,
    pub(crate) inputs: Vec<Held>,
}

pub(crate) struct Counts {
    pub(crate) sessions: usize,
    pub(crate) watches: usize,
}

pub(crate) fn held<X: GXExt>(ctx: &ExecState<GXRt<X>>, bind: &Arc<Bind>) -> Held {
    let var = ctx.rt.var_of(bind.id);
    let entry = ctx.rt.store.get(&var.id());
    Held {
        bind: bind.clone(),
        value: entry.filter(|(tv, _)| !tv.is_bottom()).map(|(tv, _)| tv.value_cloned()),
        written: entry.map(|(_, at)| *at),
    }
}

struct Watch {
    n: u64,
    id: BindId,
    stop: Option<Stop>,
}

struct Session {
    events: UnboundedSender<Fired>,
    exprs: IntSet<ExprId>,
    /// The tokens of the [`Op::Cycle`]s the next cycle answers.
    awaited: SmallVec<[u64; 2]>,
    /// The tokens of the [`Op::WhenHeld`]s the next hold answers.
    waiting: SmallVec<[u64; 1]>,
    watches: SmallVec<[Watch; 4]>,
}

/// The sessions attached, and whether they hold the program. With none
/// a cycle pays one branch for them.
#[derive(Default)]
pub(super) struct Sessions {
    by_id: IntMap<SessionId, Session>,
    /// No cycle of the program's starts, unless `steps` are owed.
    held: bool,
    /// The cycles that do the program's work still to run before it is
    /// held again.
    steps: u64,
    /// The last cycles that delivered something, while a session is
    /// attached.
    log: VecDeque<Logged>,
}

impl Sessions {
    /// No session, the program held from its start where `held`.
    pub(super) fn starting(held: bool) -> Self {
        Self { held, ..Self::default() }
    }

    pub(super) fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    /// Whether the program is held now.
    pub(super) fn held(&self) -> bool {
        self.held && self.steps == 0
    }

    /// Whether a session waits for a cycle to end.
    pub(super) fn awaited(&self) -> bool {
        self.by_id.values().any(|s| !s.awaited.is_empty())
    }

    pub(super) fn owns(&self, id: &ExprId) -> bool {
        self.by_id.values().any(|s| s.exprs.contains(id))
    }

    /// `id` was deleted: no session's any more.
    pub(super) fn forget(&mut self, id: &ExprId) {
        for s in self.by_id.values_mut() {
            s.exprs.remove(id);
        }
    }

    /// Take out of `updated` the roots no session owns: a held cycle
    /// runs none of the program's.
    pub(super) fn park(
        &self,
        updated: &mut IntMap<ExprId, bool>,
    ) -> LPooled<Vec<(ExprId, bool)>> {
        let mut parked: LPooled<Vec<(ExprId, bool)>> = LPooled::take();
        updated.retain(|id, init| {
            let ours = self.owns(id);
            if !ours {
                parked.push((*id, *init))
            }
            ours
        });
        parked
    }

    /// Tell every session but `except`.
    fn tell(&self, except: Option<SessionId>, fired: Fired) {
        for (_, s) in self.by_id.iter().filter(|(id, _)| Some(**id) != except) {
            let _ = s.events.send(fired.clone());
        }
    }

    /// Hold the program as of `cycle`, telling every session but
    /// `except`, then answering those that wait for a hold.
    fn hold(
        &mut self,
        cycle: u64,
        by: Paused,
        delivered: Option<Logged>,
        except: Option<SessionId>,
    ) {
        self.held = true;
        self.steps = 0;
        self.tell(except, Fired::Paused { cycle, by, delivered });
        for s in self.by_id.values_mut() {
            for token in s.waiting.drain(..) {
                let _ = s.events.send(Fired::Held { token, cycle });
            }
        }
    }

    /// A session ended. The last one's leaves the program running: held
    /// with nobody to continue it, it would never run again.
    fn detach(&mut self, session: SessionId) {
        self.by_id.remove(&session);
        if self.by_id.is_empty() {
            self.held = false;
            self.steps = 0;
            self.log.clear()
        }
    }
}

fn is_fn(typ: &Type) -> bool {
    typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
}

/// Whether the program's author wrote `b`: no lowering made its name,
/// and it is none of a package's own, an instance of one of its
/// functions included.
pub(crate) fn authored(env: &Env, b: &Bind) -> bool {
    let package = NPath::parts(&b.scope.0).next();
    !is_lowered(&b.name) && !package.is_some_and(|p| env.package_roots.contains(p))
}

impl<X: GXExt> GX<X> {
    pub(super) async fn debug_op(&mut self, op: Op<X>, sets: &mut Vec<EmbedderWrite>) {
        match op {
            Op::Attach { session, events } => {
                let s = Session {
                    events,
                    exprs: IntSet::default(),
                    awaited: SmallVec::new(),
                    waiting: SmallVec::new(),
                    watches: SmallVec::new(),
                };
                self.debug.by_id.insert(session, s);
            }
            Op::Detach { session } => self.debug.detach(session),
            Op::Pause { session, res } => {
                let cycle = self.ctx.rt.cycle;
                if !self.debug.held() {
                    self.debug.hold(cycle, Paused::Asked, None, Some(session))
                }
                let _ = res.send(cycle);
            }
            Op::Write { session, id, v, token } => {
                if self.admits(id, &v) {
                    let mut one = SETS.take();
                    one.push((id, v));
                    sets.push(EmbedderWrite::Set(one))
                }
                if let Some(s) = self.debug.by_id.get_mut(&session) {
                    s.awaited.push(token)
                }
            }
            Op::WhenHeld { session, token } => {
                let (held, cycle) = (self.debug.held(), self.ctx.rt.cycle);
                if let Some(s) = self.debug.by_id.get_mut(&session) {
                    match held {
                        true => drop(s.events.send(Fired::Held { token, cycle })),
                        false => s.waiting.push(token),
                    }
                }
            }
            Op::Continue { session } => {
                if self.debug.held {
                    self.debug.held = false;
                    self.debug.steps = 0;
                    self.debug.tell(Some(session), Fired::Running)
                }
            }
            Op::Step { session, cycles } => {
                if self.debug.held() {
                    self.debug.tell(Some(session), Fired::Running)
                }
                self.debug.held = true;
                self.debug.steps = cycles.max(1)
            }
            Op::Cycles { last, res } => {
                let from = self.debug.log.len().saturating_sub(last);
                let _ = res.send(self.debug.log.iter().skip(from).cloned().collect());
            }
            Op::Eval { session, text, rt, res } => {
                let r = self.compile(rt, text).await;
                self.record_compiled(&r);
                if let (Ok(r), Some(s)) = (&r, self.debug.by_id.get_mut(&session)) {
                    s.exprs.extend(r.exprs.iter().map(|e| e.id))
                }
                let _ = res.send(r);
            }
            Op::Cycle { session, token } => {
                if let Some(s) = self.debug.by_id.get_mut(&session) {
                    s.awaited.push(token)
                }
            }
            Op::Watch { session, watch, ids, stop } => {
                if let Some(s) = self.debug.by_id.get_mut(&session) {
                    let mut stop = stop;
                    let last = ids.len().saturating_sub(1);
                    for (i, id) in ids.into_iter().enumerate() {
                        let stop = match i == last {
                            true => stop.take(),
                            false => stop.as_ref().map(Stop::again),
                        };
                        s.watches.push(Watch { n: watch, id, stop })
                    }
                }
            }
            Op::Unwatch { session, watch, res } => {
                let had = self.debug.by_id.get_mut(&session).is_some_and(|s| {
                    let n = s.watches.len();
                    s.watches.retain(|w| w.n != watch);
                    s.watches.len() != n
                });
                let _ = res.send(had);
            }
            Op::Why { id, res } => {
                let mut above: LPooled<IntSet<BindId>> = LPooled::take();
                let _ = res.send(self.why(id, 0, &mut above));
            }
            Op::FusedAt { file, line, column, res } => {
                let _ = res.send(self.fused_at(&file, line, column));
            }
            Op::Counts { res } => {
                let watches = self.debug.by_id.values().map(|s| s.watches.len()).sum();
                let _ = res.send(Counts { sessions: self.debug.by_id.len(), watches });
            }
        }
    }

    // CR claude for claude: [test-gap/medium] design/debugger.md says a debugger that
    // changes what a program computes is a bug, and nothing but one hand-written
    // program holds it to that (a_watched_program_computes_the_same in
    // stdlib/graphix-tests/tests/shared/debugger.rs). Add a fuzzer lane: run each
    // generated program alone and with a session attached that watches every binding
    // the program wrote, and compare the two traces per cycle, as Pair::Prune compares
    // a pruned walk with an unpruned one. Found by reading; not run. (debugger-02)
    /// Tell each session what the cycle that just ran produced for it,
    /// take its expressions' values out of the embedder's `batch`, and
    /// hold the program where a break or the last step owed says so.
    /// `quiet` is a held cycle, which ran nothing of the program's;
    /// `stepped` a cycle that did the program's work.
    pub(super) fn debug_cycle(
        &mut self,
        batch: &mut GPooled<Vec<GXEvent>>,
        quiet: bool,
        stepped: bool,
    ) {
        let cycle = self.ctx.rt.cycle;
        let logged = (!quiet).then(|| self.delivered(cycle)).flatten();
        let mut stop = None;
        let mut gone: SmallVec<[SessionId; 1]> = SmallVec::new();
        for (sid, s) in self.debug.by_id.iter_mut() {
            let mut alive = !s.events.is_closed();
            for e in batch.iter() {
                if let GXEvent::Updated(id, v) = e
                    && s.exprs.contains(id)
                {
                    let fired = Fired::Value { expr: *id, value: v.clone() };
                    alive &= s.events.send(fired).is_ok()
                }
            }
            for w in s.watches.iter() {
                let var = self.ctx.rt.var_of(w.id);
                let Some((tv, at)) = self.ctx.rt.store.get(&var.id()) else { continue };
                let Some(bind) = self.ctx.env.by_id.get(&w.id) else { continue };
                if *at == cycle {
                    let value = (!tv.is_bottom()).then(|| tv.value_cloned());
                    if w.stop.as_ref().is_some_and(|s| s.met(value.as_ref())) {
                        stop = stop.or(Some(Paused::Break { watch: w.n }))
                    }
                    let fired =
                        Fired::Watch { watch: w.n, bind: bind.clone(), cycle, value };
                    alive &= s.events.send(fired).is_ok()
                }
            }
            for token in s.awaited.drain(..) {
                alive &= s.events.send(Fired::Cycled { token, held: quiet }).is_ok()
            }
            if !alive {
                gone.push(*sid)
            }
        }
        batch.retain(|e| !matches!(e, GXEvent::Updated(id, _) if self.debug.owns(id)));
        for sid in gone {
            self.debug.detach(sid)
        }
        if quiet || self.debug.is_empty() {
            return;
        }
        if let Some(l) = &logged {
            if self.debug.log.len() == LOG_CYCLES {
                self.debug.log.pop_front();
            }
            self.debug.log.push_back(l.clone())
        }
        if self.debug.steps > 0 && stepped {
            self.debug.steps -= 1;
            if self.debug.steps == 0 {
                stop = stop.or(Some(Paused::Stepped))
            }
        }
        if let Some(by) = stop {
            self.debug.hold(cycle, by, logged, None)
        }
    }

    /// What this cycle delivered to the bindings the program's author
    /// wrote, functions aside, in the order they were introduced; `None`
    /// where nothing.
    fn delivered(&self, cycle: u64) -> Option<Logged> {
        if self.debug.is_empty() {
            return None;
        }
        let mut delivered = Vec::new();
        let mut more = 0;
        for (id, tv) in self.ctx.event.variables.own() {
            let tag = tv.tag();
            if !tag.triggers() {
                continue;
            }
            let Some(bind) = self.ctx.env.by_id.get(&id) else { continue };
            if !authored(&self.ctx.env, bind) || is_fn(&bind.typ) {
                continue;
            }
            match delivered.len() < LOG_WIDTH {
                true => {
                    let value = (!tag.is_bottom()).then(|| tv.value_cloned());
                    delivered.push((bind.clone(), value))
                }
                false => more += 1,
            }
        }
        delivered.sort_by_key(|(b, _)| b.id);
        (!delivered.is_empty()).then_some(Logged { cycle, delivered, more })
    }

    /// The fused blocks that compute a place ([`Op::FusedAt`]).
    fn fused_at(&self, file: &Path, line: i32, column: Option<i32>) -> Vec<Fused> {
        let mut blocks = Vec::new();
        let binds = self.ctx.env.by_id.iter().map(|(_, b)| b);
        let Some(ori) = binds.map(|b| &b.ori).find(|o| in_file(o, file)) else {
            return blocks;
        };
        let Ok(covering) = covering(ori, line, column) else { return blocks };
        let mut defs = Vec::new();
        for (_, n) in self.nodes.iter() {
            let mut refs = Refs::seeking_place(covering.clone(), defs);
            n.refs(&mut refs);
            defs = refs.take_defs();
        }
        for (_, n) in self.nodes.iter() {
            let mut refs = Refs::seeking_place(covering.clone(), defs.clone());
            n.refs(&mut refs);
            for source in refs.take_sources() {
                let inputs = self.written_reads(&source.reads);
                let held =
                    |id: &BindId| Some(held(&self.ctx, self.ctx.env.by_id.get(id)?));
                let inputs = inputs.iter().filter_map(held).collect();
                blocks.push(Fused { source, inputs })
            }
        }
        blocks
    }

    /// The `let` that binds `id`, and what its value reads.
    fn source_of(&self, id: BindId) -> Option<BindSource> {
        self.nodes.iter().find_map(|(_, n)| {
            let mut refs = Refs::seeking(id);
            n.refs(&mut refs);
            refs.take_sources().pop()
        })
    }

    /// The bindings among `reads` an author wrote and would ask about: no
    /// function, and for a name a lowering made, what it reads in turn.
    fn written_reads(&self, reads: &[BindId]) -> LPooled<Vec<BindId>> {
        let mut out: LPooled<Vec<BindId>> = LPooled::take();
        let mut lowered: LPooled<IntSet<BindId>> = LPooled::take();
        let mut todo: LPooled<Vec<BindId>> = reads.iter().rev().copied().collect();
        while let Some(id) = todo.pop() {
            let Some(bind) = self.ctx.env.by_id.get(&id) else { continue };
            if is_lowered(&bind.name) {
                if lowered.insert(id)
                    && let Some(s) = self.source_of(id)
                {
                    todo.extend(s.reads.iter().rev().copied())
                }
            } else if authored(&self.ctx.env, bind)
                && !is_fn(&bind.typ)
                && !out.contains(&id)
            {
                out.push(id)
            }
        }
        out
    }

    // CR claude for claude: [test-gap/low] No test reaches `again`. A `let` cannot read
    // itself (`let rec x: i64 = x + 1` is refused), so a cycle of reads would have to
    // pass through a name a lowering made, which `written_reads` splices in, and no
    // program was found that makes one. Either find the program and pin it, or prove
    // the reads of lets acyclic and replace the `above` set with the depth bound
    // alone. Found by reading; not run. (debugger-03)
    /// Why `id` stands as it does: its source and what that reads, each
    /// read with no value followed to its own source.
    fn why(
        &self,
        id: BindId,
        depth: usize,
        above: &mut LPooled<IntSet<BindId>>,
    ) -> Option<Why> {
        let bind = self.ctx.env.by_id.get(&id)?;
        let held = held(&self.ctx, bind);
        if depth > WHY_DEPTH || !above.insert(id) {
            return Some(Why { held, source: None, reads: Vec::new(), again: true });
        }
        let source =
            (depth == 0 || held.value.is_none()).then(|| self.source_of(id)).flatten();
        let mut reads = Vec::new();
        if let Some(s) = &source {
            let mut ids = self.written_reads(&s.reads);
            reads.extend(ids.drain(..).filter_map(|r| self.why(r, depth + 1, above)));
        }
        above.remove(&id);
        Some(Why { held, source, reads, again: false })
    }
}
