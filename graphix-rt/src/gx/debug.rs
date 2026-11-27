//! The runtime's side of the debugger (`design/debugger.md`): the
//! sessions attached, what each watches, and the questions only the
//! runtime's thread can answer. Typed data in and out; the service
//! (`crate::debug`) owns the wire.

use super::GX;
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
use std::path::{Path, PathBuf};
use tokio::sync::{mpsc::UnboundedSender, oneshot};
use triomphe::Arc;

/// How deep [`GX::why`] follows reads with no value.
const WHY_DEPTH: usize = 32;

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
    Watch {
        session: SessionId,
        watch: u64,
        ids: SmallVec<[BindId; 1]>,
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
pub(crate) enum Fired {
    Value { expr: ExprId, value: Value },
    Cycled { token: u64 },
    Watch { watch: u64, bind: Arc<Bind>, cycle: u64, value: Option<Value> },
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

struct Session {
    events: UnboundedSender<Fired>,
    exprs: IntSet<ExprId>,
    /// The tokens of the [`Op::Cycle`]s the next cycle answers.
    awaited: SmallVec<[u64; 2]>,
    watches: SmallVec<[(u64, BindId); 4]>,
}

/// The sessions attached. With none a cycle pays one branch for them.
#[derive(Default)]
pub(super) struct Sessions(IntMap<SessionId, Session>);

impl Sessions {
    pub(super) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn owns(&self, id: &ExprId) -> bool {
        self.0.values().any(|s| s.exprs.contains(id))
    }

    /// `id` was deleted: no session's any more.
    pub(super) fn forget(&mut self, id: &ExprId) {
        for s in self.0.values_mut() {
            s.exprs.remove(id);
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
    pub(super) async fn debug_op(&mut self, op: Op<X>) {
        match op {
            Op::Attach { session, events } => {
                let s = Session {
                    events,
                    exprs: IntSet::default(),
                    awaited: SmallVec::new(),
                    watches: SmallVec::new(),
                };
                self.debug.0.insert(session, s);
            }
            Op::Detach { session } => {
                self.debug.0.remove(&session);
            }
            Op::Eval { session, text, rt, res } => {
                let r = self.compile(rt, text).await;
                self.record_compiled(&r);
                if let (Ok(r), Some(s)) = (&r, self.debug.0.get_mut(&session)) {
                    s.exprs.extend(r.exprs.iter().map(|e| e.id))
                }
                let _ = res.send(r);
            }
            Op::Cycle { session, token } => {
                if let Some(s) = self.debug.0.get_mut(&session) {
                    s.awaited.push(token)
                }
            }
            Op::Watch { session, watch, ids } => {
                if let Some(s) = self.debug.0.get_mut(&session) {
                    s.watches.extend(ids.into_iter().map(|id| (watch, id)))
                }
            }
            Op::Unwatch { session, watch, res } => {
                let had = self.debug.0.get_mut(&session).is_some_and(|s| {
                    let n = s.watches.len();
                    s.watches.retain(|(w, _)| *w != watch);
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
                let watches = self.debug.0.values().map(|s| s.watches.len()).sum();
                let _ = res.send(Counts { sessions: self.debug.0.len(), watches });
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
    /// and take its expressions' values out of the embedder's `batch`.
    pub(super) fn debug_cycle(&mut self, batch: &mut GPooled<Vec<GXEvent>>) {
        let cycle = self.ctx.rt.cycle;
        let mut gone: SmallVec<[SessionId; 1]> = SmallVec::new();
        for (sid, s) in self.debug.0.iter_mut() {
            let mut alive = !s.events.is_closed();
            for e in batch.iter() {
                if let GXEvent::Updated(id, v) = e
                    && s.exprs.contains(id)
                {
                    let fired = Fired::Value { expr: *id, value: v.clone() };
                    alive &= s.events.send(fired).is_ok()
                }
            }
            for (watch, id) in s.watches.iter() {
                let var = self.ctx.rt.var_of(*id);
                let Some((tv, at)) = self.ctx.rt.store.get(&var.id()) else { continue };
                let Some(bind) = self.ctx.env.by_id.get(id) else { continue };
                if *at == cycle {
                    let value = (!tv.is_bottom()).then(|| tv.value_cloned());
                    let fired =
                        Fired::Watch { watch: *watch, bind: bind.clone(), cycle, value };
                    alive &= s.events.send(fired).is_ok()
                }
            }
            for token in s.awaited.drain(..) {
                alive &= s.events.send(Fired::Cycled { token }).is_ok()
            }
            if !alive {
                gone.push(*sid)
            }
        }
        batch.retain(|e| !matches!(e, GXEvent::Updated(id, _) if self.debug.owns(id)));
        for sid in gone {
            self.debug.0.remove(&sid);
        }
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
