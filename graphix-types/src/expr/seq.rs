//! AST-to-AST lowering of `seq` (`design/seq_blocks.md`) to its
//! machine (`ExprKind::SeqMachine`, `node/seq_machine.rs`).
//!
//! Lets, connects, expression steps, `{ … }` blocks, `until`, and
//! `try … with`. `catch` is refused in a seq body. The machine installs
//! one handler that resets and rethrows; each try-body step carries a
//! generated handler that jumps to the with body. Every statement is a
//! step; the analysis decides which cycle each step enters in. A call
//! consumes one argument snapshot per entry.

use super::{
    ApplyExpr, Arg, ArgKind, Attr, BinOp, BindExpr, CatchExpr, CatchRole, Decorations,
    Expr, ExprId, ExprKind, LambdaBody, LambdaExpr, ModPath, ModuleKind, Pattern,
    SelectExpr, SeqCaptureExpr, SeqExpr, SeqKind, SeqMachineExpr, SeqStep, SeqTrigger,
    StructurePattern, TryWithExpr, WrittenAt,
};
use crate::{
    BindId,
    env::Env,
    expr::{At, Name, Origin, OriginScope},
    ide::Lowered,
    stack::ensure_sufficient,
    typ::{TVar, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use indexmap::IndexMap;
use netidx_core::path::Path;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cell::RefCell;
use triomphe::Arc;

/// The label of a machine with no run in progress.
pub const IDLE: i64 = -1;

/// Where the rewrite sends a name. A cell (a `seq let` trigger's name, a
/// try's `e`) takes every use. A snapshot (a trigger's value), a capture
/// (a `seqq` request's external values) and an entry (a step's snapshot)
/// take only reads: a write, a `&` or a call's function reaches the
/// variable itself. A nested seq takes its own entries, and a `live`
/// neither. A local identifies a completed let for later steps' snapshots
/// and hides outer bindings of the same name.
/// A function is a local or outer name bound to a function: it stays
/// itself, and an issued call takes it at the call site, where the call
/// resolves it.
#[derive(Clone)]
enum Redirect {
    Cell(ArcStr),
    Snapshot(ArcStr),
    Capture(ArcStr),
    Entry(ArcStr),
    Local(ArcStr),
    Function(ArcStr),
    /// A pattern bind of a scrutinee read live: read live, as its source.
    Live(ArcStr),
}

impl Redirect {
    fn name(&self) -> &ArcStr {
        match self {
            Self::Cell(n)
            | Self::Snapshot(n)
            | Self::Capture(n)
            | Self::Entry(n)
            | Self::Local(n)
            | Self::Function(n)
            | Self::Live(n) => n,
        }
    }

    fn snapshot(&self) -> bool {
        matches!(self, Self::Snapshot(_) | Self::Capture(_) | Self::Entry(_))
    }
}

type Names = AHashMap<ArcStr, Redirect>;

#[derive(Clone, Copy)]
enum Rewrite<'a> {
    Bindings,
    Issue(Issue<'a>),
}

/// How a step issues its calls: on the machine's `pc`.
#[derive(Clone, Copy)]
struct Issue<'a> {
    pc: &'a str,
}

impl Rewrite<'_> {
    fn deferred(self) -> Self {
        match self {
            Self::Issue(_) => Self::Bindings,
            mode => mode,
        }
    }
}

/// A seq's parts, as `desugar` matched them.
#[derive(Clone, Copy)]
struct Parts<'a> {
    spec: &'a Expr,
    trigger: Option<&'a SeqTrigger>,
    abort: Option<&'a Expr>,
    flush: Option<&'a Expr>,
    body: &'a Arc<[Expr]>,
}

thread_local! {
    /// Under a language server's check, what each cell the lowering in
    /// progress sends a written name to stands for.
    static LOWERED: RefCell<Option<LPooled<Vec<(ArcStr, Lowered)>>>> =
        const { RefCell::new(None) };
}

/// The cell `cell` stands for `written`: read or written by the source at
/// `site`, or declared at `decl` when the seq declares the name.
fn note_lowered(
    cell: &str,
    written: &str,
    site: Option<SourcePosition>,
    decl: Option<(SourcePosition, &Arc<Origin>)>,
) {
    if cell != written {
        LOWERED.with_borrow_mut(|l| {
            if let Some(l) = l {
                let lowered = Lowered {
                    written: ArcStr::from(written),
                    decl: decl.map(|(pos, ori)| (pos, ori.clone())),
                    sites: site.into_iter().collect(),
                };
                l.push((ArcStr::from(cell), lowered))
            }
        })
    }
}

/// The seq `spec` lowered to its machine, every expression of which
/// comes from `spec`'s source. Under a language server, the cells it
/// sends written names to are told to the IDE ([`Env::note_lowered`]).
pub fn desugar(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    if !env.ide.is_lsp() {
        return lower(spec, env, scope);
    }
    let outer = LOWERED.replace(Some(LPooled::take()));
    let lowered = lower(spec, env, scope);
    let mut noted = LOWERED.replace(outer).unwrap_or_default();
    for (cell, l) in noted.drain(..) {
        env.note_lowered(cell, l)
    }
    lowered
}

fn lower(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    let _ori = OriginScope::enter(spec.ori.clone());
    let ExprKind::Seq(s) = &spec.kind else { bail!("BUG: seq lowering of a non-seq") };
    let SeqExpr { kind, trigger, abort, body } = &**s;
    let seq = Parts {
        spec,
        trigger: trigger.as_ref(),
        abort: abort.as_deref(),
        flush: kind.flush().map(|e| &**e),
        body,
    };
    match (kind, seq.trigger) {
        (SeqKind::Queued { .. }, Some(SeqTrigger::Bind(b))) => desugar_let(&seq, b),
        (SeqKind::Queued { .. }, _) => desugar_queued(&seq, env, scope),
        (SeqKind::Plain, _) => {
            let aliases = trigger_aliases(&seq, env, scope);
            desugar_plain(&seq, None, &aliases, env, scope)
        }
    }
}

/// What a `seqq` hands its machine: the credit clock an abort returns a
/// credit on, and the queue its flush empties.
struct Queue<'a> {
    clock: &'a str,
    /// The variable the flush is written to when the flush reads the run's
    /// request, a cycle late, and the count of requests it carries: the
    /// queue keeps those that arrived with the flush or after. `None` when
    /// the queue reads the flush event itself, in its own cycle.
    flush: Option<(&'a str, &'a str)>,
    /// The queue and the lets that read it: emitted after a flush the
    /// queue reads, before the events that read the run's request.
    lets: LPooled<Vec<Expr>>,
    /// The cells the lets bind to the run's captures.
    captures: LPooled<AHashSet<ArcStr>>,
}

fn refuse_rec(spec: &Expr, b: &BindExpr) -> Result<()> {
    if b.rec {
        return Err(
            anyhow!("a seq trigger cannot be rec: it has no self to recurse on").at(spec)
        );
    }
    Ok(())
}

/// `seqq let pat = e { body }` is `{ let seqbind = e; seqq seqbind { let
/// pat = seqbind; body } }`: a run destructures the request it dequeued,
/// and so does an abort or flush event. The request is never the body's
/// own name, which a body write would enqueue.
fn desugar_let(seq: &Parts, b: &BindExpr) -> Result<Expr> {
    let pos = seq.spec.pos;
    refuse_rec(seq.spec, b)?;
    let BindExpr { pattern, typ, value, .. } = b;
    let name = ArcStr::from(format_compact!("#seqbind{}", seq.spec.id.inner()).as_str());
    let destructure = let_pat(pos, pattern.clone(), typ.clone(), r#ref(pos, &name));
    let event = |e: &Expr| Arc::new(block(e.pos, [destructure.clone(), e.clone()]));
    let body = Arc::from_iter(
        std::iter::once(destructure.clone()).chain(seq.body.iter().cloned()),
    );
    let queued = ExprKind::Seq(Arc::new(SeqExpr {
        kind: SeqKind::Queued { flush: seq.flush.map(&event) },
        trigger: Some(SeqTrigger::Expr(Arc::new(r#ref(pos, &name)))),
        abort: seq.abort.map(&event),
        body,
    }))
    .to_expr(pos);
    Ok(block(pos, [let_bind(pos, &name, typ.clone(), value.clone()), queued]))
}

/// The other spellings the body, abort and flush clauses read the
/// trigger's variable by (`use m::t; seq t { .. m::t .. }`), keyed as
/// reads are.
fn trigger_aliases(seq: &Parts, env: &Env, scope: &ModPath) -> SmallVec<[ArcStr; 2]> {
    let mut out: SmallVec<[ArcStr; 2]> = SmallVec::new();
    let Some(SeqTrigger::Expr(e)) = seq.trigger else { return out };
    let ExprKind::Ref { name } = &e.kind else { return out };
    let Ok(Some((_, b))) = env.lookup_bind(scope, name) else { return out };
    let (id, key) = (b.id, ref_key(name));
    let stmts = seq.body.iter().chain(seq.abort).chain(seq.flush);
    for s in stmts {
        s.fold((), &mut |(), e| {
            if let ExprKind::Ref { name: n } = &e.kind
                && ref_key(n) != key
                && !out.iter().any(|o| o.as_str() == ref_key(n))
                && let Ok(Some((_, other))) = env.lookup_bind(scope, n)
                && other.id == id
            {
                out.push(ArcStr::from(ref_key(n)))
            }
        })
    }
    out
}

/// The trigger a machine filters, and the run's snapshot of it: the cell
/// declarations, the writes the start event makes into them, and the
/// body's names for them. A bare variable's reads see the value the run
/// started with; the names a `seq let` binds are the body's own.
fn trigger_snapshot(
    seq: &Parts,
    go: &str,
    decls: &mut SmallVec<[Expr; 2]>,
    writes: &mut SmallVec<[Expr; 2]>,
    visible: &mut Names,
    aliases: &[ArcStr],
) -> Result<Expr> {
    let Parts { spec, trigger, body, .. } = *seq;
    let (pos, id) = (spec.pos, spec.id.inner());
    let cell = |n: &str| ArcStr::from(format_compact!("#seqt{id}_{n}").as_str());
    match trigger {
        None => Ok(boolean(pos, true)),
        Some(SeqTrigger::Expr(e)) => {
            // the abort clause reads the run's trigger too
            let reads = |n: &str| {
                reads_name(body, n)
                    || seq.abort.is_some_and(|a| reads_name(std::slice::from_ref(a), n))
            };
            if let ExprKind::Ref { name } = &e.kind
                && let n = ArcStr::from(ref_key(name))
                && (reads(&n) || aliases.iter().any(|a| reads(a)))
            {
                let c = cell(&n.replace('/', "_"));
                decls.push(let_bind(e.pos, &c, None, never(e.pos)));
                writes.push(connect(pos, &c, r#ref(pos, go)));
                for a in aliases {
                    visible.insert(a.clone(), Redirect::Snapshot(c.clone()));
                }
                visible.insert(n, Redirect::Snapshot(c));
            }
            Ok((**e).clone())
        }
        Some(SeqTrigger::Bind(b)) => {
            refuse_rec(spec, b)?;
            let vpos = b.value.pos;
            let single = matches!(b.pattern, StructurePattern::Bind(_));
            let mut body: LPooled<Vec<Expr>> = LPooled::take();
            if !single {
                body.push(let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, go)));
            }
            b.pattern.with_bound_names(&mut |n| {
                let c = cell(n.as_str());
                note_lowered(&c, n, None, Some((n.pos_or(pos), &spec.ori)));
                decls.push(let_bind(n.pos_or(vpos), &c, None, never(vpos)));
                let v = if single { r#ref(pos, go) } else { r#ref(pos, n) };
                body.push(connect(pos, &c, v));
                visible.insert(n.name.clone(), Redirect::Cell(c));
            });
            writes.push(block(pos, body.drain(..)));
            Ok(match &b.typ {
                None => b.value.clone(),
                Some(t) => {
                    let v = format_compact!("#seqtv{id}");
                    block(
                        vpos,
                        [
                            let_bind(vpos, &v, Some(t.clone()), b.value.clone()),
                            r#ref(vpos, &v),
                        ],
                    )
                }
            })
        }
    }
}

fn desugar_plain(
    seq: &Parts,
    queue: Option<Queue>,
    aliases: &[ArcStr],
    env: &Env,
    scope_path: &ModPath,
) -> Result<Expr> {
    let Parts { spec, abort, flush, body, .. } = *seq;
    let manual = abort.is_some() || flush.is_some();
    let pos = spec.pos;
    let id = spec.id.inner();
    let mut steps: SmallVec<[&Expr; 8]> = SmallVec::new();
    for e in body.iter() {
        refuse_in_seq(e)?;
        if !matches!(e.kind, ExprKind::NoOp) {
            steps.push(e);
        }
    }
    if steps.is_empty() {
        return Err(anyhow!("a seq block must contain at least one step").at(spec));
    }
    let pc = format_compact!("#seqpc{id}");
    let idle = format_compact!("#seqidle{id}");
    let result = format_compact!("#seqr{id}");
    let go = format_compact!("#seqgo{id}");
    let aborted = format_compact!("#seqab{id}");

    let mut snapshot_decls: SmallVec<[Expr; 2]> = SmallVec::new();
    let mut snapshot_writes: SmallVec<[Expr; 2]> = SmallVec::new();
    let mut visible: LPooled<Names> = LPooled::take();
    let trig_expr = trigger_snapshot(
        seq,
        &go,
        &mut snapshot_decls,
        &mut snapshot_writes,
        &mut visible,
        aliases,
    )?;
    for n in fn_names(body, env, scope_path).drain() {
        visible.entry(n.clone()).or_insert(Redirect::Function(n));
    }
    let err_bind = literal!("e");
    let catch_node = {
        let mut abort_body: SmallVec<[Expr; 2]> = SmallVec::new();
        abort_body.push(connect(pos, &pc, integer(pos, IDLE)));
        if let Some(q) = &queue {
            abort_body.push(connect(pos, q.clock, boolean(pos, true)));
        }
        ExprKind::Catch(Arc::new(CatchExpr {
            bind: err_bind.clone().into(),
            constraint: None,
            handler: Arc::new(
                ExprKind::Rethrow(Arc::new(r#ref(pos, &err_bind))).to_expr(pos),
            ),
            role: CatchRole::Machine {
                action: Arc::new(block(pos, abort_body)),
                manual: manual.then(|| Arc::new(r#ref(pos, &aborted))),
                pc: ArcStr::from(pc.as_str()),
                clock: queue.as_ref().map(|q| ArcStr::from(q.clock)),
            },
        }))
        .to_expr(pos)
    };

    let no_captures = AHashSet::new();
    let mut machine = Machine {
        issue: Issue { pc: &pc },
        result: &result,
        id,
        env,
        scope_path,
        captures: queue.as_ref().map_or(&no_captures, |q| &q.captures),
        snapshots: 0,
        steps: LPooled::take(),
        scopes: LPooled::take(),
        scope: 0,
        decls: LPooled::take(),
    };
    machine.scopes.push(0);
    let mut sink = Sink::new();
    sink.push(Write::Result);
    machine.lower_stmts(&steps, &sink, &visible)?;
    let Machine { steps: mut built, scopes, decls: mut join_cells, .. } = machine;
    let labels: SmallVec<[ArcStr; 8]> = (0..built.len()).map(label).collect();

    let mut exprs: LPooled<Vec<Expr>> = LPooled::take();
    exprs.push(let_bind(
        pos,
        &pc,
        Some(Type::Primitive(Typ::I64.into())),
        integer(pos, IDLE),
    ));
    exprs.push(let_bind(pos, &idle, None, idle_of(pos, &pc)));
    exprs.push(let_bind(pos, &result, None, never(pos)));
    exprs.extend(snapshot_decls);
    exprs.extend(join_cells.drain(..));
    run_events(&mut exprs, spec, &idle, &aborted, abort, flush, queue, &visible);
    // the trigger is outside the machine's handler: a busy machine drops
    // a trigger, and an error the trigger raises is not the run's
    let filter = apply_filter(pos, trig_expr, lambda_sampling(pos, &idle));
    exprs.push(let_bind(pos, &go, None, filter));
    exprs.push(catch_node);
    if manual {
        exprs.push(ExprKind::SeqAbort(Arc::new(r#ref(pos, &aborted))).to_expr(pos));
    }
    exprs.push(connect(pos, &pc, sample(pos, r#ref(pos, &go), integer(pos, 0))));
    exprs.extend(snapshot_writes);
    let steps = built.drain(..).zip(labels).map(|(s, label)| SeqStep {
        label,
        scope: s.scope,
        until: s.until,
        value: s.value,
        items: Arc::from_iter(s.items.iter().map(strip_live)),
        next: s.next,
    });
    exprs.push(
        ExprKind::SeqMachine(Arc::new(SeqMachineExpr {
            id: spec.id,
            pc: Arc::new(r#ref(pos, &pc)),
            scopes: Arc::from_iter(scopes.iter().copied()),
            steps: Arc::from_iter(steps),
        }))
        .to_expr(pos),
    );
    exprs.push(r#ref(pos, &result));
    Ok(block(pos, exprs.drain(..)))
}

// XCR claude for claude: [bug/medium] A seqq flush empties the queue but not the requests
// still waiting for an absent capture (those `BinOp::BankedSample` banks until the
// capture arrives). In the probe, requests `A and `B arrive while `ready` is absent,
// the flush fires before `ready` does, and both still run once it arrives (prints "a 7"
// and "b 7"). "flush(e) also empties the queue" should drop them: the flush must reach
// the banked requests too. probe: ~/tmp/xcr-review-2026-10-28/1/p/flushwait.gx
// (seqq-flush-wait-01)
// 2026-10-29 claude: A flush now drops the requests still waiting for a capture as well
// as the queued ones. Every seqq with a flush counts its requests, and the queue is
// flushed by number (#flush_before: the flush sampling the count, the same cycle; a
// flush reading the run's request keeps its cycle-late path), and core queue keeps a
// floor: a value arriving later numbered below the latest flush_before is dropped, since
// the BankedSample pays requests in order. Pin:
// lang::seq_abort::seqq_flush_drops_a_request_waiting_for_a_capture (gives [17, 27, 87]
// without the floor, [87] with it). CLAUDE.md and queue's doc updated.
// 2026-10-29 claude: Review R2 (cr29, 2026-10-29): the floor breaks when the seqq is born
// again (a seqq in a seq step: every run is a rebirth). Queue::reborn kept pushed and
// floor while the BankedSample's bank is cleared and the prelude's requests counter
// starts again at 0, so request numbers and arrival numbers drift apart. Probes
// ~/tmp/cr29/scratch/review/r2/fw_nested.gx (run 1 banks, flushes, is aborted; run 2's
// request, made after the flush, is dropped: prints nothing, where fw_noflush.gx and
// fw_flush_empty.gx print r 27) and fw_reset.gx (run 2's waiting request survives the
// flush: r 17 then r 27, the original bug again).
// 2026-10-29 claude: Fixed for a rebirth: Queue::reborn (graphix-package-core) resets
// pushed and floor with the queue, in step with the prelude's requests counter and the
// BankedSample's bank, which a rebirth starts again; a seqq in a seq step is born again
// at each run through CallSite::update_call's reborn (a block step takes no FreshFrame;
// that is for a call step), so this is the path runs take under D4. Pins:
// lang::seq_abort::seqq_flush_in_a_step_drops_nothing_of_the_next_run (R2's fw_nested.gx:
// [27]; [] without the reset) and seqq_flush_in_a_step_drops_a_waiting_request_of_its_run
// (fw_reset.gx: [17, 37]; [17, 27] without it), all four modes. fw_reset_ctl, fw_noflush
// and fw_flush_empty print r 37, r 27, r 27 as before. queue's .gxi doc says a queue born
// again numbers from 0.
/// The prelude that makes a seq's `abort(..)` and `flush(..)` one event
/// bound to `aborted`, and a `seqq`'s queue. The queue takes the flush
/// event in the cycle it fires, so a request that arrives with it stays,
/// unless the flush reads the run's request, which the queue produces:
/// then the flush reaches the queue through a variable, a cycle late.
fn run_events(
    out: &mut Vec<Expr>,
    spec: &Expr,
    idle: &str,
    aborted: &str,
    abort: Option<&Expr>,
    flush: Option<&Expr>,
    queue: Option<Queue>,
    visible: &Names,
) {
    let (pos, id) = (spec.pos, spec.id.inner());
    let edge = format_compact!("#seqedge{id}");
    let armed = format_compact!("#seqarmed{id}");
    if abort.is_some() || flush.is_some() {
        out.push(let_bind(
            pos,
            &edge,
            None,
            apply_core(pos, "uniq", [(None, r#ref(pos, idle))]),
        ));
        out.push(let_bind(pos, &armed, None, boolean(pos, false)));
        out.push(connect(
            pos,
            &armed,
            ExprKind::Not { expr: Arc::new(r#ref(pos, &edge)) }.to_expr(pos),
        ));
    }
    let mut events: SmallVec<[Expr; 2]> = SmallVec::new();
    let flushed = format_compact!("#seqfl{id}");
    let flush_var = queue.as_ref().and_then(|q| q.flush);
    let push_flush = |out: &mut Vec<Expr>, events: &mut SmallVec<[Expr; 2]>| {
        if let Some(e) = flush {
            out.push(let_bind(e.pos, &flushed, None, rewrite(e, visible)));
            if let Some((v, count)) = flush_var {
                let mark = sample(e.pos, r#ref(e.pos, &flushed), r#ref(e.pos, count));
                out.push(connect(e.pos, v, mark));
            }
            events.push(run_event(&r#ref(e.pos, &flushed), &edge, &armed));
        }
    };
    match queue {
        Some(mut q) if flush_var.is_none() => {
            push_flush(out, &mut events);
            out.extend(q.lets.drain(..));
        }
        Some(mut q) => {
            out.extend(q.lets.drain(..));
            push_flush(out, &mut events);
        }
        None => push_flush(out, &mut events),
    }
    if let Some(e) = abort {
        events.insert(0, run_event(&rewrite(e, visible), &edge, &armed));
    }
    match events.len() {
        0 => (),
        1 => out.push(let_bind(pos, aborted, None, events.pop().unwrap())),
        _ => out.push(let_bind(
            pos,
            aborted,
            None,
            ExprKind::Any { args: Arc::from_iter(events) }.to_expr(pos),
        )),
    }
}

/// The attribute the lowering puts on a run event's select
/// ([`crate::CFlag::RunEvent`]); no source can spell it.
pub const RUN_EVENT: ArcStr = literal!("seq-run");

/// A run's abort event: `e` is an initial step, born
/// when a run starts and asleep between runs, and only its fires after
/// the entry cycle count. What stands at entry, and the fires it missed
/// asleep, belong to no run.
fn run_event(e: &Expr, edge: &str, armed: &str) -> Expr {
    let pos = e.pos;
    let mut live = select(
        pos,
        r#ref(pos, edge),
        [
            (pat_lit(Value::Bool(true)), never(pos)),
            (pat_lit(Value::Bool(false)), e.clone()),
        ],
    );
    live.dec = Some(Arc::new(Decorations {
        attrs: Arc::from_iter([Attr { name: RUN_EVENT, args: Arc::from_iter([]) }]),
        ..Decorations::default()
    }));
    apply_filter(pos, live, lambda_sampling(pos, armed))
}

/// The names a seq body passes to a call that are functions where the seq
/// starts: an outer function binding, or a name passed where the callee,
/// resolved now, declares a function. Each is a [`Redirect::Function`]
/// until something in the body binds its name; a `let` of the body to a
/// lambda literal makes one too ([`shadow_step`]).
fn fn_names(body: &[Expr], env: &Env, scope: &ModPath) -> LPooled<AHashSet<ArcStr>> {
    let mut fn_args: LPooled<AHashSet<ArcStr>> = LPooled::take();
    let mut args: LPooled<AHashSet<ArcStr>> = LPooled::take();
    for s in body {
        s.fold((), &mut |(), e| {
            if let ExprKind::Apply(a) = &e.kind {
                args.extend(a.args.iter().filter_map(|(_, a)| simple_ref_name(a)));
                fn_position_args(a, env, scope, &mut fn_args);
            }
        })
    }
    args.retain(|n| {
        fn_args.contains(n)
            || env
                .lookup_bind(scope, &ModPath::from([n.as_str()]))
                .ok()
                .flatten()
                .is_some_and(|(_, b)| is_function(env, b))
    });
    args
}

/// The names `a` passes where its callee, resolved now, declares a
/// function: each is one, whatever the check will make of its own binding.
fn fn_position_args(
    a: &ApplyExpr,
    env: &Env,
    scope: &ModPath,
    out: &mut AHashSet<ArcStr>,
) {
    if let ExprKind::Ref { name } = &a.function.kind
        && let Ok(Some((_, b))) = env.lookup_bind(scope, name)
        && let Some(Type::Fn(ref ft)) = b.typ.deref_cloned()
    {
        let mut positional = ft.args.iter().filter(|f| f.is_positional());
        for (label, arg) in a.args.iter() {
            let formal = match label {
                Some(l) => ft.args.iter().find(|f| f.label() == Some(l)),
                None => positional.next(),
            };
            if let Some(n) = simple_ref_name(arg)
                && formal
                    .is_some_and(|f| f.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))))
            {
                out.insert(n);
            }
        }
    }
}

/// Whether a binding is a function: a trait method, or of a function type.
fn is_function(env: &Env, bind: &crate::env::Bind) -> bool {
    env.trait_methods.get(&bind.id).is_some()
        || bind.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
}

/// Whether any of `stmts` reads the simple name `n`.
fn reads_name(stmts: &[Expr], n: &str) -> bool {
    stmts.iter().any(|s| {
        s.fold(false, &mut |found, e| {
            found || matches!(&e.kind, ExprKind::Ref { name } if ref_key(name) == n)
        })
    })
}

/// `catch` is refused anywhere in a seq body: an install cannot produce
/// the value the next step waits for. So is `#[parallel]`: a seq runs
/// serially inside, so nothing under it forks. So is a dynamic module: a
/// step's value is scoped away from the steps after it, which could not
/// name the module. A lambda literal is its own dynamic scope, so its
/// body and defaults are exempt.
fn refuse_in_seq(e: &Expr) -> Result<()> {
    let dynamic = |x: &Expr| {
        matches!(&x.kind, ExprKind::Module { value, .. }
            if matches!(**value, ModuleKind::Dynamic { .. }))
    };
    if let Some(m) = find_outside_lambdas(e, dynamic) {
        return Err(anyhow!(
            "a dynamic module is not allowed inside a seq: load it outside the seq"
        )
        .at(&m));
    }
    if let Some(c) = find_outside_lambdas(e, |x| matches!(x.kind, ExprKind::Catch(_))) {
        return Err(anyhow!(
            "catch is not allowed inside a seq: `?` aborts the run, \
             `try {{ .. }} with(e) {{ .. }}` handles it, and a catch around the \
             seq sees the abort"
        )
        .at(&c));
    }
    let parallel = |x: &Expr| {
        x.dec.as_ref().is_some_and(|d| d.attrs.iter().any(|a| a.name == "parallel"))
    };
    match find_outside_lambdas(e, parallel) {
        None => Ok(()),
        Some(p) => Err(anyhow!(
            "#[parallel] is not allowed inside a seq: a seq runs its steps serially, \
             so nothing under it forks"
        )
        .at(&p)),
    }
}

/// `Expr::fold` over every node that is not inside a lambda literal's
/// body or defaults, or a module's body: the lambda or module itself is
/// visited, its children are not.
fn fold_outside_lambdas<T>(e: &Expr, init: T, f: &mut impl FnMut(T, &Expr) -> T) -> T {
    ensure_sufficient(|| {
        let mut acc = Some(f(init, e));
        if !matches!(e.kind, ExprKind::Lambda(_) | ExprKind::Module { .. }) {
            e.for_each_child(&mut |c| {
                let v = acc.take().unwrap();
                acc = Some(fold_outside_lambdas(c, v, f));
            });
        }
        acc.unwrap()
    })
}

/// The first node satisfying `pred` that is not inside a lambda literal's
/// body or defaults.
fn find_outside_lambdas(e: &Expr, pred: impl Fn(&Expr) -> bool) -> Option<Expr> {
    fold_outside_lambdas(e, None, &mut |found: Option<Expr>, x| {
        found.or_else(|| pred(x).then(|| x.clone()))
    })
}

/// The variable a place expression is rooted at, if it names one.
fn place_root(e: &Expr) -> Option<&ModPath> {
    match &e.place()?.0.kind {
        ExprKind::Ref { name } => Some(name),
        _ => None,
    }
}

/// `value` carrying a statement's decorations too: an attribute on a
/// seq statement annotates its computation, where the compiler applies
/// or refuses it.
fn decorated(value: &Expr, dec: &Option<Arc<Decorations>>) -> Expr {
    let Some(dec) = dec else { return value.clone() };
    let mut value = value.clone();
    value.dec = Some(match &value.dec {
        None => dec.clone(),
        Some(own) => Arc::new(Decorations {
            comments: own.comments.iter().chain(dec.comments.iter()).cloned().collect(),
            attrs: dec.attrs.iter().chain(own.attrs.iter()).cloned().collect(),
            eol: own.eol.clone().or_else(|| dec.eol.clone()),
            below: own.below.iter().chain(dec.below.iter()).cloned().collect(),
        }),
    });
    value
}

/// A `use`, `type`, `trait`, `impl` or static `mod`: a statement with no
/// value.
fn is_declaration(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Use { .. }
        | ExprKind::TypeDef(_)
        | ExprKind::Trait(_)
        | ExprKind::Impl(_) => true,
        ExprKind::Module { value, .. } => !matches!(&**value, ModuleKind::Dynamic { .. }),
        _ => false,
    }
}

/// The `try` a statement is: bare, or a let's or a connect's value.
fn try_of(e: &Expr) -> Option<(&Expr, &TryWithExpr)> {
    let v = match &e.kind {
        ExprKind::Bind(b) => &b.value,
        ExprKind::Connect { value, .. } => value,
        _ => e,
    };
    match &v.kind {
        ExprKind::TryWith(t) => Some((v, t)),
        _ => None,
    }
}

/// What the last statement of a list writes with its value, in order:
/// bind a `let`'s pattern, write a connect's target, write a `try`'s
/// join cell, publish the block's result.
#[derive(Clone)]
enum Write {
    Let {
        pattern: StructurePattern,
        typ: Option<Type>,
    },
    Connect {
        name: ModPath,
        deref: bool,
    },
    /// A try's join cell, typed as the write it stands for.
    Join(ArcStr, Option<Type>),
    Result,
}

type Sink = SmallVec<[Write; 2]>;

/// The steps of a statement list that continue to whatever follows it.
type Tails = SmallVec<[usize; 2]>;

/// A step under construction; its label is `S{index}`.
struct StepBuild {
    scope: u32,
    until: bool,
    value: ArcStr,
    items: SmallVec<[Expr; 4]>,
    next: Option<u32>,
}

/// The steps under construction, in the order they are lowered, the
/// lexical scopes they compile in (each a parent index; 0 is the
/// machine's), and the cells the prelude declares: each try's `e` and
/// join cells.
struct Machine<'a> {
    issue: Issue<'a>,
    result: &'a str,
    id: u64,
    /// Where the seq's free names resolve: its captures are the ones that
    /// name a variable here.
    env: &'a Env,
    scope_path: &'a ModPath,
    /// A `seqq`'s capture cells, read like the variables they capture.
    captures: &'a AHashSet<ArcStr>,
    /// Entry snapshots made so far, for their names.
    snapshots: usize,
    steps: LPooled<Vec<StepBuild>>,
    scopes: LPooled<Vec<u32>>,
    scope: u32,
    decls: LPooled<Vec<Expr>>,
}

/// A step's entry snapshot, made before the step: what its text reads sent
/// to cells ([`Machine::snapshot`]), and each cell's read, which the step
/// takes once it is built ([`Machine::enter`]).
struct Snapshot {
    map: LPooled<Names>,
    reads: LPooled<Vec<(ArcStr, Expr)>>,
    n: usize,
    pos: SourcePosition,
}

fn label(k: usize) -> ArcStr {
    ArcStr::from(format_compact!("S{k}").as_str())
}

impl Machine<'_> {
    /// The step's entry snapshot of `e`'s external variables and completed
    /// local lets, `live` aside (`design/seq_blocks.md` §5). Each read goes
    /// to a cell filled at the
    /// step's entry, or when all named inputs are present, as an issued
    /// call takes its argument tuple. The references the
    /// step's `sink` writes through are read at its entry too, and so are a
    /// `seqq`'s capture cells, whose variables a callee's write makes live.
    fn snapshot(&mut self, e: &Expr, sink: &[Write], visible: &Names) -> Snapshot {
        let mut vars: LPooled<IndexMap<ArcStr, Expr>> = LPooled::take();
        let (env, scope_path, captures) = (self.env, self.scope_path, self.captures);
        let mut read = |name: &ModPath, at: &Expr| {
            let key = ref_key(name);
            if vars.contains_key(key) {
                return;
            }
            let outer = match redirect(name, visible) {
                Some(Redirect::Local(_)) => true,
                Some(_) => false,
                None => {
                    captures.contains(key)
                        || matches!(env.lookup_bind(scope_path, name),
                        Ok(Some((_, bind))) if !is_function(env, bind))
                }
            };
            if outer {
                let mut read = at.clone();
                read.kind = ExprKind::Ref { name: name.clone() };
                vars.insert(ArcStr::from(key), read);
            }
        };
        captured(e, false, &mut |r| {
            let (ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. }) =
                &r.kind
            else {
                return;
            };
            read(name, r)
        });
        for w in sink {
            if let Write::Connect { name, deref: true } = w {
                read(name, e)
            }
        }
        let n = self.snapshots;
        self.snapshots += 1;
        let id = self.id;
        let mut map = scope(visible);
        let mut reads: LPooled<Vec<(ArcStr, Expr)>> = LPooled::take();
        for (i, (k, read)) in vars.drain(..).enumerate() {
            let c = ArcStr::from(format_compact!("#seqs{id}_{n}_{i}").as_str());
            map.insert(k, Redirect::Entry(c.clone()));
            reads.push((c, read));
        }
        Snapshot { map, reads, n, pos: e.pos }
    }

    /// Put step `k`'s entry snapshot ahead of its value: the cells its items
    /// read. A name every read of which an inner binding shadows takes no
    /// cell, which would wait for it.
    fn enter(&mut self, k: usize, snap: Snapshot) {
        let Snapshot { reads: mut taken, n, pos, .. } = snap;
        let items = &mut self.steps[k].items;
        let mut used: LPooled<AHashSet<ArcStr>> = LPooled::take();
        for item in items.iter() {
            item.fold((), &mut |(), x| {
                if let ExprKind::Ref { name }
                | ExprKind::Connect { name, deref: true, .. } = &x.kind
                    && let Some(s) = simple_name(name)
                {
                    used.insert(ArcStr::from(s));
                }
            })
        }
        taken.retain(|(c, _)| used.contains(c));
        if taken.is_empty() {
            return;
        }
        let (pc, id) = (self.issue.pc, self.id);
        let input = format_compact!("#seqsin{id}_{n}");
        let snap = format_compact!("#seqs{id}_{n}");
        let tuple = ExprKind::Tuple {
            args: Arc::from_iter(
                std::iter::once(r#ref(pos, pc))
                    .chain(taken.iter().map(|(_, r)| r.clone())),
            ),
        }
        .to_expr(pos);
        let sampled = entry_snapshot(pos, pc, &input);
        let fields = taken.iter().enumerate().map(|(i, (c, _))| {
            let field =
                ExprKind::TupleRef { source: Arc::new(r#ref(pos, &snap)), field: i + 1 }
                    .to_expr(pos);
            let_bind(pos, c, None, field)
        });
        let lets =
            [let_bind(pos, &input, None, tuple), let_bind(pos, &snap, None, sampled)]
                .into_iter()
                .chain(fields);
        items.insert_many(0, lets);
    }

    /// A step: `let <value> = value; writes..`, the writes built over
    /// the value's name.
    fn step(
        &mut self,
        until: bool,
        value: Expr,
        writes: impl FnOnce(&str) -> LPooled<Vec<Expr>>,
    ) -> usize {
        let k = self.steps.len();
        let name = ArcStr::from(format_compact!("#seqv{}_{k}", self.id).as_str());
        let mut items: SmallVec<[Expr; 4]> = SmallVec::new();
        // an until's condition is a boolean
        let typ = until.then(|| Type::Primitive(Typ::Bool.into()));
        items.push(let_bind(value.pos, &name, typ, value));
        items.extend(writes(&name).drain(..));
        self.steps.push(StepBuild {
            scope: self.scope,
            until,
            value: name,
            items,
            next: None,
        });
        k
    }

    /// Lower `stmts` in a lexical scope of their own, below the current.
    // CR claude for claude: [perf/low] Each try body gets a lexical scope here, and
    // SeqMachine::compile compiles every step under its scope's chain of append_blocks.
    // So a step at try depth d resolves its generated names (the catch's pc connect,
    // the #seqe cells) d levels deep, at Env::chain_lookup's O(d^2) (t-misc-02). Nested
    // tries make the check cubic: 990 deep (a 25 KB file) checks in 45 s, 500 in 6.2 s.
    // Fix: t-misc-02's (scope, name) memo, or resolve the generated machine names
    // without the per-try scope chain. probe:
    // design/review-2026-10-26/repro/x-stack-seq-try-nesting.py (x-stack-06)
    // 2026-10-26 claude: Not done in batch L5-b04. Measured: the probe at D=500 checks
    // in 11.5 s (debug, loaded host). The fix that stays in the seq lowering would drop
    // the per-try lexical scopes by sending each try body's lets to cells the rewrite
    // scopes (as a seq let trigger's names are), so every step compiles in the
    // machine's scope; that is a larger change to lower_try and touches IDE sites
    // (x-lsp-probe-01). The other is t-misc-02's (scope, name) memo in
    // Env::chain_lookup, which belongs to that CR's lane.
    // 2026-10-27 claude: seq-try-depth-01's fix (eb9bf67e5) took the cubic factor out:
    // Env::chain_lookup no longer recomputes mod_root per level, so the nested-try
    // check went from 5.95 to 1.02 s at depth 500, and to 6.4 s at 993 (debug; 45 s at
    // 990 when this CR was written). What remains is quadratic. Each lookup still walks
    // its chain, hashing every level's path, and a step at depth d still resolves its
    // generated names d levels deep. Remaining: the per-try lexical scopes, or a
    // (scope, name) memo, as above.
    // 2026-10-27 codex: Reproduced the named nested-try shape with the worktree debug
    // binary: --check depths 250/500/993 took 0.481/1.602/8.368 s. After t-seq-06's
    // constant-size pc, they took 0.363/1.221/7.276 s: the scope-chain cost remains. No
    // scope rewrite applied. Investigated qualifying generated cells at compile time,
    // but CatchRole::Try captures and generated references both use lexical resolution,
    // while user lets must retain their try scopes and shadowing; dropping or
    // flattening the scope chain without rewriting those bindings would change lookup
    // and IDE sites. A safe implementation still needs a scoped-name rewrite preserving
    // those sites, or the general lookup memo owned by t-misc-02. No new pin; the
    // existing nested_trys_check_in_time budget is too loose to distinguish this
    // remaining cost. All 651 seq fixtures pass for the preceding pc change, which does
    // not resolve this CR. Leaving open.
    fn lower_scoped(
        &mut self,
        stmts: &[&Expr],
        sink: &Sink,
        visible: &Names,
    ) -> Result<Tails> {
        let parent = self.scope;
        self.scope = self.scopes.len() as u32;
        self.scopes.push(parent);
        let res = self.lower_stmts(stmts, sink, visible);
        self.scope = parent;
        res
    }

    fn patch(&mut self, tails: &[usize], next: usize) {
        for t in tails {
            self.steps[*t].next = Some(next as u32);
        }
    }

    /// Lower `stmts` in order, each statement's steps continuing to the
    /// next statement's first; the last writes `sink`. A `let`'s names
    /// are visible to the statements after it.
    fn lower_stmts(
        &mut self,
        stmts: &[&Expr],
        sink: &Sink,
        visible: &Names,
    ) -> Result<Tails> {
        let mut vis = scope(visible);
        let mut tails = Tails::new();
        let none = Sink::new();
        for (i, stmt) in stmts.iter().enumerate() {
            let first = self.steps.len();
            let sink = if i + 1 == stmts.len() { sink } else { &none };
            let next = ensure_sufficient(|| self.lower_stmt(stmt, sink, &vis))?;
            self.patch(&tails, first);
            tails = next;
            shadow_step(stmt, &mut vis);
            if let ExprKind::Bind(b) = &stmt.kind {
                b.pattern.with_names(&mut |n| {
                    vis.entry(n.clone()).or_insert_with(|| Redirect::Local(n.clone()));
                });
            }
        }
        Ok(tails)
    }

    fn lower_stmt(&mut self, stmt: &Expr, sink: &Sink, visible: &Names) -> Result<Tails> {
        let pos = stmt.pos;
        if let ExprKind::Until(cond) = &stmt.kind {
            if !sink.is_empty() {
                return Err(anyhow!(
                    "until has no value: the last statement of a seq, or of a try \
                     or with body whose value is used, must be an expression"
                )
                .at(stmt));
            }
            let cond = decorated(cond, &stmt.dec);
            let snap = self.snapshot(&cond, &[], visible);
            let value = guard(entry_fire(rewrite(&cond, &snap.map), self.issue.pc));
            let k = self.step(true, value, |_| LPooled::take());
            self.enter(k, snap);
            return Ok(Tails::from_iter([k]));
        }
        if let ExprKind::Bind(b) = &stmt.kind
            && b.rec
        {
            return Err(anyhow!("let rec is not a seq step").at(stmt));
        }
        if let Some((spec, t)) = try_of(stmt) {
            if stmt.dec.as_ref().is_some_and(|d| !d.attrs.is_empty()) {
                return Err(anyhow!(
                    "a try statement takes no attribute: annotate a statement in its body"
                )
                .at(stmt));
            }
            let mut sink = sink.clone();
            match &stmt.kind {
                ExprKind::Bind(b) => sink.insert(
                    0,
                    Write::Let { pattern: b.pattern.clone(), typ: b.typ.clone() },
                ),
                ExprKind::Connect { name, deref, .. } => {
                    sink.insert(0, Write::Connect { name: name.clone(), deref: *deref })
                }
                _ => (),
            }
            return self.lower_try(spec, t, &sink, visible);
        }
        if is_declaration(stmt) {
            return Err(declaration_refused(stmt));
        }
        let (k, snap) = match &stmt.kind {
            ExprKind::Bind(b) => {
                let value = decorated(&b.value, &stmt.dec);
                let snap = self.snapshot(&value, sink, visible);
                let snapped = &snap.map;
                let mut vis = scope(snapped);
                shadow_step(stmt, &mut vis);
                let result = self.result;
                let k = match self.bound_lambda(b, &value, snapped) {
                    Some((name, bind)) => {
                        let value = guard(entry_fire(r#ref(pos, &name), self.issue.pc));
                        let k = self
                            .step(false, value, |v| writes(sink, result, pos, v, &vis));
                        self.steps[k].items.insert(0, bind);
                        k
                    }
                    None => {
                        let value = self.stmt_value(&value, snapped)?;
                        self.step(false, value, |v| {
                            let mut out = writes(sink, result, pos, v, &vis);
                            out.insert(
                                0,
                                let_pat(
                                    pos,
                                    b.pattern.clone(),
                                    b.typ.clone(),
                                    r#ref(pos, v),
                                ),
                            );
                            out
                        })
                    }
                };
                (k, snap)
            }
            ExprKind::Connect { name, value, deref, .. } => {
                let value = decorated(value, &stmt.dec);
                let snap = self.snapshot(stmt, sink, visible);
                let value = self.stmt_value(&value, &snap.map)?;
                let target = rewrite_target(name, pos, *deref, &snap.map);
                let result = self.result;
                let k = self.step(false, value, |v| {
                    let mut out = writes(sink, result, pos, v, &snap.map);
                    out.insert(0, connect_path(pos, target, *deref, r#ref(pos, v)));
                    out
                });
                (k, snap)
            }
            _ => {
                let snap = self.snapshot(stmt, sink, visible);
                let value = self.stmt_value(stmt, &snap.map)?;
                let result = self.result;
                (
                    self.step(false, value, |v| writes(sink, result, pos, v, &snap.map)),
                    snap,
                )
            }
        };
        self.enter(k, snap);
        Ok(Tails::from_iter([k]))
    }

    /// Each try-body step carries a generated handler that captures the
    /// first error into the with body's cell (`CatchRole::Try`) and whose
    /// drain action jumps to the with body's first step. When the try's
    /// value is used, both bodies end by writing a join cell, and a join
    /// step reads it and writes `sink`.
    fn lower_try(
        &mut self,
        spec: &Expr,
        t: &TryWithExpr,
        sink: &Sink,
        visible: &Names,
    ) -> Result<Tails> {
        let pos = spec.pos;
        let body: SmallVec<[&Expr; 8]> =
            t.body.iter().filter(|e| !matches!(e.kind, ExprKind::NoOp)).collect();
        let handler: SmallVec<[&Expr; 8]> =
            t.handler.iter().filter(|e| !matches!(e.kind, ExprKind::NoOp)).collect();
        if body.is_empty() || handler.is_empty() {
            return Err(
                anyhow!("a try body and a with body each need a statement").at(spec)
            );
        }
        let e_cell = ArcStr::from(format_compact!("#seqe{}", spec.id.inner()).as_str());
        self.decls.push(let_bind(t.bind.pos_or(pos), &e_cell, None, never(pos)));
        // a try whose value only writes variables has each body write them,
        // typed as they are
        let direct = !sink.is_empty()
            && sink.iter().all(|w| {
                matches!(w, Write::Connect { name, .. }
                    if !simple_name(name).is_some_and(|n| try_binds(t, n)))
            });
        let join = (!sink.is_empty() && !direct).then(|| {
            let cell = ArcStr::from(format_compact!("#seqj{}", spec.id.inner()).as_str());
            let typ = match sink.first() {
                Some(Write::Let { typ, .. } | Write::Join(_, typ)) => typ.clone(),
                _ => None,
            };
            self.decls.push(let_bind(pos, &cell, typ.clone(), never(pos)));
            (cell, typ)
        });
        let mut branch = Sink::new();
        match &join {
            Some((c, typ)) => branch.push(Write::Join(c.clone(), typ.clone())),
            None if direct => branch.extend(sink.iter().cloned()),
            None => (),
        }
        let mark = self.steps.len();
        let mut tails = self.lower_scoped(&body, &branch, visible)?;
        let with_entry = self.steps.len() as i64;
        let caught = ArcStr::from(format_compact!("#seqtry{}", spec.id.inner()).as_str());
        // an inner try catches every error its body raises (a constraint
        // must hold them all), so none reaches this one
        let inner_try = |e: &Expr| matches!(&e.kind, ExprKind::Catch(c) if matches!(c.role, CatchRole::Try { .. }));
        for step in self.steps[mark..].iter_mut() {
            if step.items.iter().any(inner_try) {
                continue;
            }
            let jump = ExprKind::Catch(Arc::new(CatchExpr {
                bind: caught.clone().into(),
                constraint: t.constraint.clone(),
                handler: Arc::new(never(pos)),
                role: CatchRole::Try {
                    action: Arc::new(connect(
                        pos,
                        self.issue.pc,
                        integer(pos, with_entry),
                    )),
                    capture: e_cell.clone(),
                },
            }))
            .to_expr(pos);
            step.items.insert(0, jump);
        }
        let mut wvis = scope(visible);
        note_lowered(&e_cell, &t.bind, None, Some((t.bind.pos_or(spec.pos), &spec.ori)));
        wvis.insert(t.bind.name.clone(), Redirect::Cell(e_cell));
        tails.extend(self.lower_scoped(&handler, &branch, &wvis)?);
        match join {
            None => Ok(tails),
            Some((cell, _)) => {
                let value = guard(entry_fire(r#ref(pos, &cell), self.issue.pc));
                let snap = self.snapshot(&value, sink, visible);
                let result = self.result;
                let j =
                    self.step(false, value, |v| writes(sink, result, pos, v, &snap.map));
                self.enter(j, snap);
                self.patch(&tails, j);
                Ok(Tails::from_iter([j]))
            }
        }
    }

    /// A `let` of one name to a lambda literal: the name and its binding
    /// to the lambda itself, so calls through it resolve statically.
    fn bound_lambda(
        &self,
        b: &BindExpr,
        value: &Expr,
        visible: &Names,
    ) -> Option<(ArcStr, Expr)> {
        let name = b.pattern.single_bind()?;
        if !inline_lambda(value) {
            return None;
        }
        let lambda = rewrite_with(value, visible, Rewrite::Issue(self.issue));
        let bind = let_pat(value.pos, b.pattern.clone(), b.typ.clone(), lambda);
        Some((name.clone(), bind))
    }

    /// A statement's value. A block in statement position, or as a
    /// let's or a connect's right-hand side, is lowered as a block;
    /// anything else is issued.
    fn stmt_value(&self, e: &Expr, visible: &Names) -> Result<Expr> {
        ensure_sufficient(|| match &e.kind {
            ExprKind::Block { exprs } => self.lower_block(e, exprs, visible),
            _ => Ok(issue_expr(e, visible, self.issue)),
        })
    }

    /// A `{ … }` statement: every statement issued at entry, lets local to
    /// the block, connects clocked to the entry, and the value the last
    /// statement's once every statement has produced.
    fn lower_block(&self, spec: &Expr, exprs: &[Expr], visible: &Names) -> Result<Expr> {
        if spec.dec.as_ref().is_some_and(|d| !d.attrs.is_empty()) {
            return Err(anyhow!(
                "a `{{ .. }}` statement takes no attribute: annotate a statement in it"
            )
            .at(spec));
        }
        let pos = spec.pos;
        let mut vis = scope(visible);
        let mut body: LPooled<Vec<Expr>> = LPooled::take();
        let mut vals: LPooled<Vec<Expr>> = LPooled::take();
        for (i, s) in exprs.iter().enumerate() {
            if try_of(s).is_some() {
                return Err(
                    anyhow!("try is a seq statement; write it at the seq level").at(s)
                );
            }
            if let ExprKind::Until(_) = s.kind {
                return Err(anyhow!(
                    "until is a seq statement; write it at the seq level"
                )
                .at(s));
            }
            let v = format_compact!("#seqb{}_{i}", spec.id.inner());
            match &s.kind {
                ExprKind::NoOp => continue,
                ExprKind::Bind(b) if b.rec => {
                    let Some(name) = b.pattern.single_bind() else {
                        return Err(anyhow!("can't use rec on a complex pattern").at(s));
                    };
                    shadow_step(s, &mut vis);
                    let value = rewrite(&b.value, &vis);
                    let mut bind = ExprKind::Bind(Arc::new(BindExpr {
                        rec: true,
                        pattern: b.pattern.clone(),
                        typ: b.typ.clone(),
                        value,
                    }))
                    .to_expr(s.pos);
                    bind.dec = s.dec.clone();
                    body.push(bind);
                    let bound = issue_expr(&r#ref(s.pos, name), &vis, self.issue);
                    body.push(let_bind(s.pos, &v, None, bound));
                }
                ExprKind::Bind(b) => {
                    let value = decorated(&b.value, &s.dec);
                    match self.bound_lambda(b, &value, &vis) {
                        Some((name, bind)) => {
                            body.push(bind);
                            let value =
                                guard(entry_fire(r#ref(s.pos, &name), self.issue.pc));
                            body.push(let_bind(s.pos, &v, None, value));
                        }
                        None => {
                            let value = self.stmt_value(&value, &vis)?;
                            body.push(let_bind(s.pos, &v, None, value));
                            body.push(let_pat(
                                s.pos,
                                b.pattern.clone(),
                                b.typ.clone(),
                                r#ref(s.pos, &v),
                            ));
                        }
                    }
                    shadow_step(s, &mut vis);
                }
                ExprKind::Connect { name, value, deref, .. } => {
                    let value = self.stmt_value(&decorated(value, &s.dec), &vis)?;
                    body.push(let_bind(s.pos, &v, None, value));
                    body.push(connect_path(
                        s.pos,
                        rewrite_target(name, s.pos, *deref, &vis),
                        *deref,
                        // the value's first fire after entry: an answer, or the
                        // level as it stands at entry
                        apply_core(s.pos, "once", [(None, r#ref(s.pos, &v))]),
                    ));
                }
                _ if is_declaration(s) => {
                    body.push(rewrite(s, &vis));
                    shadow_step(s, &mut vis);
                    continue;
                }
                _ => body.push(let_bind(s.pos, &v, None, self.stmt_value(s, &vis)?)),
            }
            vals.push(r#ref(s.pos, &v));
        }
        let value = match vals.len() {
            0 => return Err(declaration_refused(spec)),
            1 => vals.pop().unwrap(),
            n => {
                let last = format_compact!("#seqb{}", spec.id.inner());
                select(
                    pos,
                    ExprKind::Tuple { args: Arc::from_iter(vals.drain(..)) }.to_expr(pos),
                    [(pat_last(n, &last), r#ref(pos, &last))],
                )
            }
        };
        body.push(value);
        Ok(block(pos, body.drain(..)))
    }
}

/// Whether the try `t` binds `n` where its bodies' last statements see it:
/// the with body's error, or a `let` of either body or of a try nested in
/// one.
fn try_binds(t: &TryWithExpr, n: &str) -> bool {
    let binds = |stmts: &[Expr]| {
        stmts.iter().any(|s| {
            let mut found = false;
            if let ExprKind::Bind(b) = &s.kind {
                b.pattern.with_names(&mut |m| found |= m == n);
            }
            found || try_of(s).is_some_and(|(_, t)| try_binds(t, n))
        })
    };
    ensure_sufficient(|| t.bind.name == n || binds(&t.body) || binds(&t.handler))
}

fn declaration_refused(e: &Expr) -> anyhow::Error {
    anyhow!(
        "a declaration is not a seq step: write it in a `{{ .. }}` statement with the \
         code that uses it, or outside the seq"
    )
    .at(e)
}

/// What a statement's completion writes of its value `v`.
fn writes(
    sink: &Sink,
    result: &str,
    pos: SourcePosition,
    v: &str,
    visible: &Names,
) -> LPooled<Vec<Expr>> {
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    for w in sink.iter() {
        out.push(match w {
            Write::Let { pattern, typ } => {
                let_pat(pos, pattern.clone(), typ.clone(), r#ref(pos, v))
            }
            Write::Connect { name, deref } => connect_path(
                pos,
                rewrite_target(name, pos, *deref, visible),
                *deref,
                r#ref(pos, v),
            ),
            Write::Join(cell, _) => connect(pos, cell, r#ref(pos, v)),
            Write::Result => connect(pos, result, r#ref(pos, v)),
        })
    }
    out
}

fn desugar_queued(seq: &Parts, env: &Env, scope: &ModPath) -> Result<Expr> {
    let Parts { spec, trigger, abort, flush, body } = *seq;
    let pos = spec.pos;
    let id = spec.id.inner();
    let flushed = format_compact!("#seqqflush{id}");
    let requests = format_compact!("#seqqrequests{id}");
    let dequeued = format_compact!("#seqqrun{id}");
    let request = format_compact!("#seqqrequest{id}");
    let clock = format_compact!("#seqqclock{id}");
    let activation = format_compact!("#seqqactivation{id}");
    let input = format_compact!("#seqqinput{id}");
    let result = format_compact!("#seqqresult{id}");
    // every spelling of the trigger's variable is the run's request
    let trigger_names: SmallVec<[ArcStr; 2]> = match trigger.map(|t| &t.expr().kind) {
        Some(ExprKind::Ref { name }) => std::iter::once(ArcStr::from(ref_key(name)))
            .chain(trigger_aliases(seq, env, scope))
            .collect(),
        _ => SmallVec::new(),
    };
    let is_trigger = |n: &str| trigger_names.iter().any(|t| t.as_str() == n);
    let fn_args = body.iter().fold(LPooled::<AHashSet<ArcStr>>::take(), |args, stmt| {
        stmt.fold(args, &mut |mut args, e| {
            if let ExprKind::Apply(a) = &e.kind {
                fn_position_args(a, env, scope, &mut args);
            }
            args
        })
    });
    let mut captures: LPooled<IndexMap<ArcStr, (Expr, ArcStr, BindId)>> = LPooled::take();
    for stmt in body.iter() {
        captured(stmt, true, &mut |e| {
            if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
                &e.kind
                && let Ok(Some((_, bind))) = env.lookup_bind(scope, name)
                && !is_function(env, bind)
                && !simple_name(name).is_some_and(|n| fn_args.contains(n))
            {
                let n = captures.len();
                captures
                    .entry(ArcStr::from(simple_name(name).unwrap_or(name.0.as_ref())))
                    .or_insert_with(|| {
                        let mut expr = e.clone();
                        expr.kind = ExprKind::Ref { name: name.clone() };
                        (
                            expr,
                            ArcStr::from(format_compact!("#seqqcap{id}_{n}").as_str()),
                            bind.id,
                        )
                    });
            }
        });
    }
    // a capture the body writes or takes a reference to stays live: find
    // them through a scoped rewrite that sends them to the capture's cell
    let cell_of: LPooled<Names> = captures
        .iter()
        .map(|(n, (_, c, _))| (n.clone(), Redirect::Cell(c.clone())))
        .collect();
    let live_cells = rewrite_stmts(body, &cell_of, Rewrite::Bindings).iter().fold(
        LPooled::<AHashSet<ArcStr>>::take(),
        |w, stmt| {
            stmt.fold(w, &mut |mut w, e| {
                let target = match &e.kind {
                    ExprKind::Connect { name, deref: false, .. } => Some(name),
                    ExprKind::ByRef(_, place) => place_root(place),
                    _ => None,
                };
                if let Some(n) = target.and_then(simple_name) {
                    w.insert(ArcStr::from(n));
                }
                w
            })
        },
    );
    // a run reads its own request: the trigger's name is never live
    let live_ids: LPooled<AHashSet<BindId>> = captures
        .iter()
        .filter(|(n, _)| !is_trigger(n))
        .filter_map(|(_, (_, c, id))| live_cells.contains(c).then_some(*id))
        .collect();
    let snapshots: LPooled<Names> = captures
        .iter()
        .filter(|(_, (_, _, id))| !live_ids.contains(id))
        .map(|(n, (_, c, _))| {
            let redirect = match is_trigger(n) {
                true => Redirect::Snapshot(c.clone()),
                false => Redirect::Capture(c.clone()),
            };
            (n.clone(), redirect)
        })
        .collect();
    let body = rewrite_stmts(body, &snapshots, Rewrite::Bindings);
    let used = body.iter().fold(LPooled::<AHashSet<ArcStr>>::take(), |u, stmt| {
        stmt.fold(u, &mut |mut u, e| {
            if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
                &e.kind
                && let Some(n) = simple_name(name)
            {
                u.insert(ArcStr::from(n));
            }
            u
        })
    });
    let captures: LPooled<Vec<_>> =
        captures.drain(..).filter(|(_, (_, c, _))| used.contains(c)).collect();
    let mut prelude: LPooled<Vec<Expr>> = LPooled::take();
    prelude.extend([
        let_bind(pos, &clock, None, never(pos)),
        let_bind(pos, &activation, None, boolean(pos, true)),
        let_bind(
            pos,
            &request,
            None,
            trigger.map_or_else(|| boolean(pos, true), |t| t.expr().clone()),
        ),
    ]);
    // a flush that reads the run's request reads what the queue produces,
    // so the queue cannot read the flush in the same cycle
    let flush_reads_request = flush.is_some_and(|f| {
        trigger_names.iter().any(|n| reads_name(std::slice::from_ref(f), n))
    });
    if flush_reads_request {
        prelude.push(let_bind(pos, &flushed, None, never(pos)));
    }
    if flush.is_some() {
        // the requests arrived so far; read in a cycle, those before it. A
        // flush drops every one before it, queued or still waiting for a
        // capture, which reaches the queue numbered below the flush
        prelude.push(let_bind(
            pos,
            &requests,
            None,
            ExprKind::Constant(Value::I64(0)).to_expr(pos),
        ));
        let next = ExprKind::BinOp {
            op: BinOp::Add,
            lhs: Arc::new(r#ref(pos, &requests)),
            rhs: Arc::new(ExprKind::Constant(Value::I64(1)).to_expr(pos)),
        }
        .to_expr(pos);
        prelude.push(connect(pos, &requests, sample(pos, r#ref(pos, &request), next)));
    }
    let mut qlets: LPooled<Vec<Expr>> = LPooled::take();
    let mut args: LPooled<Vec<Expr>> = LPooled::take();
    for (name, (expr, _, _)) in captures.iter() {
        if !is_trigger(name) {
            args.push(expr.clone());
        }
    }
    for (i, arg) in args.iter_mut().enumerate() {
        let name = format_compact!("#seqqseed{id}_{i}");
        prelude.push(let_bind(
            pos,
            &name,
            None,
            ExprKind::Any {
                args: Arc::from_iter([
                    arg.clone(),
                    sample(pos, r#ref(pos, &activation), arg.clone()),
                ]),
            }
            .to_expr(pos),
        ));
        *arg = apply_core(
            pos,
            "hold",
            [(Some(literal!("clock")), r#ref(pos, &name)), (None, r#ref(pos, &name))],
        );
    }
    let payload = if args.is_empty() {
        ExprKind::Constant(Value::Null).to_expr(pos)
    } else {
        ExprKind::Tuple { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
    };
    let dequeued_request = || {
        ExprKind::TupleRef { source: Arc::new(r#ref(pos, &input)), field: 0 }.to_expr(pos)
    };
    let mut queue_args: SmallVec<[(Option<ArcStr>, Expr); 3]> = SmallVec::new();
    queue_args.push((
        Some(literal!("clock")),
        ExprKind::Any {
            args: Arc::from_iter([r#ref(pos, &activation), r#ref(pos, &clock)]),
        }
        .to_expr(pos),
    ));
    if flush.is_some() {
        let before = match flush_reads_request {
            true => r#ref(pos, &flushed),
            false => sample(
                pos,
                r#ref(pos, &format_compact!("#seqfl{id}")),
                r#ref(pos, &requests),
            ),
        };
        queue_args.push((Some(literal!("flush_before")), before));
    }
    // every queued request is paid its captures, however many find one
    // absent
    queue_args.push((
        None,
        ExprKind::BinOp {
            op: BinOp::BankedSample,
            lhs: Arc::new(r#ref(pos, &request)),
            rhs: Arc::new(payload),
        }
        .to_expr(pos),
    ));
    qlets.push(let_bind(pos, &input, None, apply_core(pos, "queue", queue_args)));
    let mut field = 0;
    for (var, (live, name, _)) in captures.iter() {
        let capture = if is_trigger(var) {
            dequeued_request()
        } else {
            let payload =
                ExprKind::TupleRef { source: Arc::new(r#ref(pos, &input)), field: 1 }
                    .to_expr(pos);
            let snapshot =
                ExprKind::TupleRef { source: Arc::new(payload), field }.to_expr(pos);
            field += 1;
            ExprKind::SeqCapture(Arc::new(SeqCaptureExpr {
                machine: seq.spec.id,
                snapshot: Arc::new(snapshot),
                live: Arc::new(live.clone()),
            }))
            .to_expr(pos)
        };
        qlets.push(let_bind(pos, name, None, capture));
    }
    // an abort or flush event reads everything live but the trigger's
    // name, which is the request this run dequeued
    let mut run_names: LPooled<Names> = LPooled::take();
    if !trigger_names.is_empty() {
        for n in &trigger_names {
            run_names
                .insert(n.clone(), Redirect::Snapshot(ArcStr::from(dequeued.as_str())));
        }
        if abort.is_some() || flush.is_some() {
            qlets.push(let_bind(pos, &dequeued, None, dequeued_request()));
        }
    }
    let machine_trigger = SeqTrigger::Expr(Arc::new(r#ref(pos, &input)));
    let abort = abort.map(|e| rewrite(e, &run_names));
    let flush = flush.map(|e| rewrite(e, &run_names));
    let machine = Parts {
        spec,
        trigger: Some(&machine_trigger),
        abort: abort.as_ref(),
        flush: flush.as_ref(),
        body: &body,
    };
    let queue = Queue {
        clock: &clock,
        flush: flush_reads_request.then_some((flushed.as_str(), requests.as_str())),
        lets: qlets,
        captures: captures.iter().map(|(_, (_, cell, _))| cell.clone()).collect(),
    };
    prelude.push(let_bind(
        pos,
        &result,
        None,
        desugar_plain(&machine, Some(queue), &[], env, scope)?,
    ));
    prelude.push(connect(
        pos,
        &clock,
        sample(pos, r#ref(pos, &result), boolean(pos, true)),
    ));
    prelude.push(r#ref(pos, &result));
    Ok(block(pos, prelude.drain(..)))
}

fn boolean(pos: SourcePosition, value: bool) -> Expr {
    ExprKind::Constant(Value::Bool(value)).to_expr(pos)
}

fn apply_core(
    pos: SourcePosition,
    name: &str,
    args: impl IntoIterator<Item = (Option<ArcStr>, Expr)>,
) -> Expr {
    ExprKind::Apply(ApplyExpr {
        function: Arc::new(
            ExprKind::Ref { name: ModPath::from(["core", name]) }.to_expr(pos),
        ),
        args: Arc::from_iter(args),
    })
    .to_expr(pos)
}

fn idle_of(pos: SourcePosition, pc: &str) -> Expr {
    select(
        pos,
        r#ref(pos, pc),
        [
            (
                pat_lit(Value::I64(IDLE)),
                ExprKind::Constant(Value::Bool(true)).to_expr(pos),
            ),
            (pat_wild(), ExprKind::Constant(Value::Bool(false)).to_expr(pos)),
        ],
    )
}

fn lambda_sampling(pos: SourcePosition, level: &str) -> Expr {
    let x = literal!("x");
    ExprKind::Lambda(Arc::new(LambdaExpr {
        vargs_comments: Default::default(),
        below: Default::default(),
        args: Arc::from_iter([Arg {
            comments: Default::default(),
            kind: ArgKind::Positional,
            pattern: StructurePattern::Bind(x.clone().into()),
            constraint: None,
            pos: WrittenAt(pos),
        }]),
        vargs: None,
        rtype: None,
        constraints: Arc::from(Vec::<(TVar, Type)>::new()),
        throws: None,
        body: LambdaBody::Expr(sample(pos, r#ref(pos, &x), r#ref(pos, level))),
    }))
    .to_expr(pos)
}

fn apply_filter(pos: SourcePosition, trig: Expr, pred: Expr) -> Expr {
    apply_core(pos, "filter", [(None, trig), (None, pred)])
}

fn simple_ref_name(e: &Expr) -> Option<ArcStr> {
    match &e.kind {
        ExprKind::Ref { name } => simple_name(name).map(ArcStr::from),
        _ => None,
    }
}

fn simple_name(p: &ModPath) -> Option<&str> {
    if Path::levels(&p.0) == 1 { Path::parts(&p.0).next() } else { None }
}

/// What a read of `p` is keyed by in a [`Names`] map: its name, or the
/// path a qualified read spells.
fn ref_key(p: &ModPath) -> &str {
    simple_name(p).unwrap_or(p.0.as_ref())
}

fn redirect<'a>(p: &ModPath, map: &'a Names) -> Option<&'a Redirect> {
    map.get(ref_key(p))
}

/// A read of `p`.
fn rewrite_path(p: &ModPath, at: SourcePosition, map: &Names) -> ModPath {
    match redirect(p, map) {
        Some(r) => {
            note_lowered(r.name(), ref_key(p), Some(at), None);
            ModPath::from([r.name().as_str()])
        }
        None => p.clone(),
    }
}

/// A connect's target `p`: a write, or with `deref` a read of the
/// reference written through.
fn rewrite_target(p: &ModPath, at: SourcePosition, deref: bool, map: &Names) -> ModPath {
    match redirect(p, map) {
        Some(r) if deref => {
            note_lowered(r.name(), ref_key(p), Some(at), None);
            ModPath::from([r.name().as_str()])
        }
        Some(Redirect::Cell(c)) => {
            note_lowered(c, ref_key(p), Some(at), None);
            ModPath::from([c.as_str()])
        }
        _ => p.clone(),
    }
}

fn rewrite(e: &Expr, map: &Names) -> Expr {
    rewrite_with(e, map, Rewrite::Bindings)
}

/// A copy of `names` for a scope to shadow in.
fn scope(names: &Names) -> LPooled<Names> {
    let mut inner: LPooled<Names> = LPooled::take();
    inner.extend(names.iter().map(|(k, v)| (k.clone(), v.clone())));
    inner
}

/// `map` without the names `binds` reports, for a scope that binds them:
/// a copy only when it holds one.
fn unbound(
    map: &Names,
    binds: impl Fn(&mut dyn FnMut(&ArcStr)),
) -> Option<LPooled<Names>> {
    let mut held = false;
    binds(&mut |n| held |= map.contains_key(n));
    held.then(|| {
        let mut inner = scope(map);
        binds(&mut |n| {
            inner.remove(n);
        });
        inner
    })
}

/// The entries of `map` that `keep` holds: a copy only when it drops one.
fn only(map: &Names, keep: impl Fn(&Redirect) -> bool) -> Option<LPooled<Names>> {
    (!map.values().all(&keep)).then(|| {
        let mut kept = scope(map);
        kept.retain(|_, r| keep(r));
        kept
    })
}

/// A step's scrutinee. A step completes on a fired production after entry;
/// a call is re-issued at entry and answers fired, while a level read as
/// it stands is fired at entry here.
fn issue_expr(e: &Expr, map: &Names, issue: Issue) -> Expr {
    let issued = has_call(e);
    let e = rewrite_with(e, map, Rewrite::Issue(issue));
    match &e.kind {
        ExprKind::SeqGuard(_) => e,
        _ => guard(if issued { e } else { entry_fire(e, issue.pc) }),
    }
}

/// `e` fired at entry with its standing value, then tracked: `any(pc
/// ~! e, e)`. A compound `e` is bound once so its nodes are not
/// duplicated (a `?` inside it must raise once).
fn entry_fire(e: Expr, pc: &str) -> Expr {
    let pos = e.pos;
    let at_entry = |v: Expr| {
        ExprKind::BinOp {
            op: BinOp::StrictSample,
            lhs: Arc::new(r#ref(pos, pc)),
            rhs: Arc::new(v),
        }
        .to_expr(pos)
    };
    let any =
        |a: Expr, b: Expr| ExprKind::Any { args: Arc::from_iter([a, b]) }.to_expr(pos);
    match &e.kind {
        ExprKind::Ref { .. } | ExprKind::Constant(_) => any(at_entry(e.clone()), e),
        _ => {
            let v = format_compact!("#seqv{}", e.id.inner());
            block(
                pos,
                [
                    let_bind(pos, &v, None, e),
                    any(at_entry(r#ref(pos, &v)), r#ref(pos, &v)),
                ],
            )
        }
    }
}

/// The `until`s of the seq `spec`'s own body (a nested seq's are its own)
/// whose condition has no `live` part, reads through no reference and
/// calls nothing: read from the step's snapshot, it cannot change after
/// the step's entry. A `seqq let` has none of its own: it lowers to a
/// `seqq` over its body, whose untils they are.
pub fn fixed_untils(spec: &Expr) -> SmallVec<[Expr; 1]> {
    fn walk(e: &Expr, out: &mut SmallVec<[Expr; 1]>) {
        ensure_sufficient(|| match &e.kind {
            ExprKind::Seq(_) | ExprKind::Lambda(_) => (),
            ExprKind::Until(c) => {
                let mut moves = false;
                c.fold((), &mut |(), x| {
                    moves |= matches!(
                        x.kind,
                        ExprKind::Live(_) | ExprKind::Apply(_) | ExprKind::Deref(_)
                    )
                });
                if !moves {
                    out.push(e.clone())
                }
            }
            _ => e.for_each_child(&mut |c| walk(c, out)),
        })
    }
    let mut out = SmallVec::new();
    if let ExprKind::Seq(s) = &spec.kind
        && !matches!(
            (&s.kind, &s.trigger),
            (SeqKind::Queued { .. }, Some(SeqTrigger::Bind(_)))
        )
    {
        s.body.iter().for_each(|e| walk(e, &mut out));
    }
    out
}

/// Each expression in `e` that may read a variable of the seq's world: all
/// but a call's function named by a `Ref` ([`callee_ref`]), and, unless
/// `nested`, those under a `live` and in a nested seq, which takes its own
/// snapshots.
fn captured(e: &Expr, nested: bool, f: &mut impl FnMut(&Expr)) {
    ensure_sufficient(|| {
        if !nested && matches!(e.kind, ExprKind::Live(_) | ExprKind::Seq(_)) {
            return;
        }
        f(e);
        let callee = callee_ref(e);
        e.for_each_child(&mut |c| {
            if !callee.is_some_and(|k| std::ptr::eq(k, c)) {
                captured(c, nested, f)
            }
        });
    })
}

/// The function of a call `e` when a `Ref` names it: the call reads the
/// binding where it resolves it, never a snapshot or a capture of it.
fn callee_ref(e: &Expr) -> Option<&Expr> {
    match &e.kind {
        ExprKind::Apply(a) if matches!(a.function.kind, ExprKind::Ref { .. }) => {
            Some(&a.function)
        }
        _ => None,
    }
}

/// `e` with each `live(x)` of this seq's written as `x`, its reads having
/// been kept from the step's snapshot. A nested seq's body is its own.
fn strip_live(e: &Expr) -> Expr {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Live(x) => strip_live(x),
        ExprKind::Seq(_) => e.clone(),
        _ => e.map_children(&mut strip_live),
    })
}

/// Whether a step produces later rather than standing as a level: it
/// holds a call, or a nested seq, whose result cell stands from its
/// previous run. A call under `live` is not issued, so it does not count.
/// The rewrite keeps the answer, so it is the same before and after.
fn has_call(e: &Expr) -> bool {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Apply(_) | ExprKind::Seq(_) => true,
        ExprKind::Lambda(_) | ExprKind::Live(_) => false,
        _ => {
            let mut found = false;
            e.for_each_child(&mut |c| found = found || has_call(c));
            found
        }
    })
}

/// Whether `e` reads this seq's world live: it holds a `live(..)` of its
/// own, not one a lambda or a nested seq in it holds, or a pattern bind
/// of a scrutinee read live.
fn reads_live(e: &Expr, map: &Names) -> bool {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Live(_) => true,
        ExprKind::Ref { name } => matches!(redirect(name, map), Some(Redirect::Live(_))),
        ExprKind::Lambda(_) | ExprKind::Seq(_) => false,
        _ => {
            let mut found = false;
            e.for_each_child(&mut |c| found = found || reads_live(c, map));
            found
        }
    })
}

fn inline_lambda(mut e: &Expr) -> bool {
    while let ExprKind::ExplicitParens(inner) = &e.kind {
        e = inner;
    }
    matches!(e.kind, ExprKind::Lambda(_))
}

/// One call issued per entry over a snapshot of the arguments taken on
/// the entry event, and again at each fire of an argument read live;
/// only the call's own fired production is this invocation's answer. A
/// nullary call is issued on the entry clock as any other.
fn issue_call(spec: &Expr, mut call: ApplyExpr, map: &Names, issue: Issue) -> Expr {
    let pc = issue.pc;
    // XCR claude for claude: [bug/medium] [bug/medium] A nullary call is lowered as a
    // level read at entry, so the step completes on the callee's cell as it stands,
    // against seq_blocks.md §5 (a lambda instance's cell standing from the previous run
    // is no completion). A `|| timer(..) ~ runs` callee gives 1, 1, 1 over three runs
    // where `|u| ..` gives 1, 2, 3. `|| read_all(path)` returns the first run's file
    // forever. The same `load()` placed after another step never completes at all,
    // because its callee is born with `path` standing. Choose: issue it on the entry
    // clock with the guard on the call (`select any(pc, once(pc)) ~! pc { _ =>
    // guard(f()) }`), accepting the `|v| k` stall for a body over standing levels; or
    // document the level reading and its failure modes in §5 and §6.3. probe:
    // design/review-2026-10-26/repro/t-seq-nullary-call-level.gx (t-seq-03)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q15, A nullary
    // call in a step): yes, issue it on entry clock, just like a no argument function
    // call in a normal language. (the recommendation: issue it on the entry clock.)
    // 2026-10-27 claude: Built as ruled: issue_call no longer lowers a nullary call as
    // a level read (entry_fire); it is issued on the entry clock as any call is,
    // through the same entry snapshot (of pc alone). The probe's stale case now gives
    // 1, 2, 3 (was 1, 1, 1). Pin: lang::seq_calls::a_nullary_call_answers_each_run. One
    // consequence for Eric: a nullary callee whose body reads only standing values, the
    // probe's || read_all(path), answers the first run and stalls on later ones, as |u|
    // read_all(path) does (the |v| k rule: nothing fires the read again). Making a
    // nullary call re-run its body like a call in an ordinary language would need its
    // body born again at each issue.
    // 2026-10-29 claude: Eric ruled (2026-10-29, cr-discuss round 3 D4): (b), rebirth
    // the callee: a call a seq step issues runs its callee's body fresh each run as the
    // step's own nodes are (constants fire, builtins reborn, select entry history
    // reset, nested calls included), every call, not only nullary; state inside the
    // callee starts fresh, captured state outside is untouched; calls outside seq are
    // unchanged; needs a soak.
    // 2026-10-29 claude: Built as ruled (D4): a call a step issues is a fresh frame at
    // every run. SeqGuard's first update in a run (the issue) opens wake::FreshFrame:
    // every standing input of the call (arguments, function values, live reads, its
    // callee's captures, nested callees') is delivered fired for that update, and a
    // catch-up scope that owes everything standing delivers the inputs of a callee
    // bound as it runs. The step's View::Step rebirths the slept callee body as before
    // (builtins reborn, lets re-initialized, selects' entry history reset); what is
    // bound inside it (a catch binder, a nested seq's pc) still reads stale.
    // CachedArgsAsync::reborn drops its last answer, so a previous run's reply never
    // answers the issue. The probe's read_all(path), load() and load1(n) now answer 1,
    // 2, 3. Pins that fail without it:
    // lang::seq_calls::a_callee_reads_its_world_fresh_each_run (load() answers nothing
    // without the frame; nested(n) answers 1, 1, 1 without the async reborn),
    // a_callees_own_state_starts_fresh_each_run ([1010, 2111, 3212] without),
    // lang::seq::reentry_fired_only (its |v| k case answers [7] without);
    // a_call_outside_a_seq_resumes pins that a call outside a seq resumes. Needs a
    // soak.
    // 2026-10-29 claude: (cr29 fix G4, RV4 findings 2 and 4) A bound callee whose
    // function value is bottom at the issue makes up the birth it missed when the
    // function returns: CallSite's Absent records the view it was due and whether it
    // sat in an issued call's fresh frame (callsite.rs::Unborn), and that dispatch runs
    // under that view, inside a FreshFrame of its own over the callee's inputs and its
    // arguments (Absent::dispatch); a call inside the callee that meets a bottom
    // function at its birth is born the same way. RV4's p1_absent_fn now gives 1 2 3
    // (was 1 1 1) and p1c_absent_state (n, 1) at each run (was (n, n)). Pins:
    // lang::seq_calls::a_callee_bottom_at_the_issue_is_born_when_it_returns (each of
    // its three calls fails with the change off: g(n) and h(n) give [11, 22, 33],
    // load() gives [11, 21, 31]);
    // lang::seq_calls::a_callee_answers_a_captured_event_as_it_stands pins the ruled
    // answer of a captured event ([1050, 2150, 3150]; main's binary stalls after the
    // second run). Docs: book seq.md (also: a nullary call is issued, not read as a
    // level), design/seq_blocks.md, CLAUDE.md, the skill. Needs a soak.
    let pos = spec.pos;
    let input = format_compact!("#seqargs{}", spec.id.inner());
    let issued = format_compact!("#seqissued{}", spec.id.inner());
    let mut args: LPooled<Vec<Expr>> = LPooled::take();
    args.reserve(call.args.len() + 1);
    args.push(r#ref(pos, pc));
    call.args = Arc::from_iter(call.args.iter().map(|(label, arg)| {
        // a function stays at the call site, where the call resolves it: a
        // name bare, a lambda literal sampled at entry
        let value = if let ExprKind::Ref { name } = &arg.kind
            && let Some(Redirect::Function(_)) = redirect(name, map)
        {
            arg.clone()
        } else if reads_live(arg, map) {
            // read live, so each of its fires issues the call again
            arg.clone()
        } else if inline_lambda(arg) {
            ExprKind::BinOp {
                op: BinOp::StrictSample,
                lhs: Arc::new(r#ref(arg.pos, pc)),
                rhs: Arc::new(arg.clone()),
            }
            .to_expr(arg.pos)
        } else {
            let field = args.len();
            args.push(arg.clone());
            ExprKind::TupleRef { source: Arc::new(r#ref(arg.pos, &issued)), field }
                .to_expr(arg.pos)
        };
        (label.clone(), value)
    }));
    let input_tuple = if args.len() == 1 {
        args.pop().unwrap()
    } else {
        ExprKind::Tuple { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
    };
    let mut expr = spec.clone();
    expr.id = ExprId::new();
    expr.kind = ExprKind::Apply(call);
    let snapshot = entry_snapshot(pos, pc, &input);
    block(
        pos,
        [
            let_bind(pos, &input, None, input_tuple),
            select(pos, snapshot, [(pat_bind(&issued), guard(expr))]),
        ],
    )
}

/// `input` as it stands at the entry clock `pc`, or at its first arrival
/// if it was absent then, held until the next entry: `any(pc,
/// once(input)) ~! input`.
fn entry_snapshot(pos: SourcePosition, pc: &str, input: &str) -> Expr {
    let ready = apply_core(pos, "once", [(None, r#ref(pos, input))]);
    ExprKind::BinOp {
        op: BinOp::StrictSample,
        lhs: Arc::new(
            ExprKind::Any { args: Arc::from_iter([r#ref(pos, pc), ready]) }.to_expr(pos),
        ),
        rhs: Arc::new(r#ref(pos, input)),
    }
    .to_expr(pos)
}

/// Drop from `map` the names statement `e` binds for the statements
/// after it: a `let`'s, and a `use`'s (a glob may bind any name). A `let`
/// of one name to a lambda literal makes its name a function.
fn shadow_step(e: &Expr, map: &mut Names) {
    match &e.kind {
        ExprKind::Bind(b) => match b.pattern.single_bind() {
            Some(n) if inline_lambda(&b.value) => {
                map.insert(n.clone(), Redirect::Function(n.clone()));
            }
            _ => b.pattern.with_names(&mut |n| {
                map.remove(n);
            }),
        },
        ExprKind::Use { names, .. } => {
            for u in names.iter() {
                if u.is_glob() {
                    return map.clear();
                }
                if let Some(n) = u.bound_name() {
                    map.remove(n);
                }
            }
        }
        _ => (),
    }
}

/// One statement of a sequential scope rewritten, its names shadowing
/// the statements after it; a `let rec`'s names are its own value's too.
fn rewrite_step(x: &Expr, inner: &mut Names, mode: Rewrite<'_>) -> Expr {
    let rec = matches!(&x.kind, ExprKind::Bind(b) if b.rec);
    if rec {
        shadow_step(x, inner);
    }
    let r = rewrite_with(x, inner, mode);
    if !rec {
        shadow_step(x, inner);
    }
    r
}

/// `stmts` rewritten as one sequential scope.
fn rewrite_stmts(stmts: &[Expr], map: &Names, mode: Rewrite<'_>) -> Arc<[Expr]> {
    let mut inner = scope(map);
    Arc::from_iter(stmts.iter().map(|x| rewrite_step(x, &mut inner, mode)))
}

fn rewrite_with(e: &Expr, map: &Names, mode: Rewrite<'_>) -> Expr {
    ensure_sufficient(|| rewrite_with_inner(e, map, mode))
}

fn rewrite_with_inner(e: &Expr, map: &Names, mode: Rewrite<'_>) -> Expr {
    if map.is_empty() && !matches!(mode, Rewrite::Issue(_)) {
        return e.clone();
    }
    let rewrite = |e: &Expr, map: &Names| rewrite_with(e, map, mode);
    let kind = match &e.kind {
        // what a `live` reads is never a snapshot
        ExprKind::Live(x) => {
            let live = only(map, |r| !r.snapshot());
            let live = live.as_deref().unwrap_or(map);
            ExprKind::Live(Arc::new(rewrite_with(x, live, mode.deferred())))
        }
        ExprKind::Ref { name } => ExprKind::Ref { name: rewrite_path(name, e.pos, map) },
        // a module's body names nothing of the seq's: it is a scope of its own,
        // but a dynamic module's source is an expression here
        ExprKind::Module { name, value } => match &**value {
            ModuleKind::Dynamic { sandbox, sig, source } => ExprKind::Module {
                name: name.clone(),
                value: Arc::new(ModuleKind::Dynamic {
                    sandbox: sandbox.clone(),
                    sig: sig.clone(),
                    source: Arc::new(rewrite(source, map)),
                }),
            },
            _ => return e.clone(),
        },
        ExprKind::Connect { name, at, value, deref } => ExprKind::Connect {
            name: rewrite_target(name, e.pos, *deref, map),
            at: *at,
            value: Arc::new(rewrite(value, map)),
            deref: *deref,
        },
        ExprKind::Until(x) => {
            ExprKind::Until(Arc::new(rewrite_with(x, map, mode.deferred())))
        }
        ExprKind::TryWith(t) => {
            let with_map = unbound(map, |f| f(&t.bind.name));
            ExprKind::TryWith(Arc::new(TryWithExpr {
                body: rewrite_stmts(&t.body, map, mode),
                bind: t.bind.clone(),
                constraint: t.constraint.clone(),
                handler: rewrite_stmts(
                    &t.handler,
                    with_map.as_deref().unwrap_or(map),
                    mode,
                ),
            }))
        }
        // the trigger is the outer step's, issued with it; the clauses
        // and the body belong to the inner machine's own runs
        ExprKind::Seq(s) => {
            let SeqExpr { kind, trigger, abort, body } = &**s;
            let outer =
                only(map, |r| !matches!(r, Redirect::Entry(_) | Redirect::Capture(_)));
            let outer = outer.as_deref().unwrap_or(map);
            let trigger = trigger.as_ref().map(|t| t.map(|e| rewrite(e, outer)));
            let inner = match &trigger {
                Some(SeqTrigger::Bind(b)) => {
                    unbound(outer, |f| b.pattern.with_names(&mut |n| f(n)))
                }
                _ => None,
            };
            let inner = inner.as_deref().unwrap_or(outer);
            let clause =
                |e: &Arc<Expr>| Arc::new(rewrite_with(e, inner, mode.deferred()));
            let abort = abort.as_ref().map(clause);
            let kind = match kind {
                SeqKind::Plain => SeqKind::Plain,
                SeqKind::Queued { flush } => {
                    SeqKind::Queued { flush: flush.as_ref().map(clause) }
                }
            };
            let body = rewrite_stmts(body, inner, mode.deferred());
            let seq = ExprKind::Seq(Arc::new(SeqExpr { kind, trigger, abort, body }));
            // issued with its step, it answers only once this run's fires
            match mode {
                Rewrite::Issue(_) => ExprKind::SeqGuard(Arc::new(seq.to_expr(e.pos))),
                _ => seq,
            }
        }
        ExprKind::Qop { arg: x, written } => {
            let original = written.clone().unwrap_or_else(|| x.clone());
            let x = rewrite(x, map);
            let arg = match mode {
                // A `?` over a level fires at entry as it stands, so a
                // carried error raises at every entry, and tracks after.
                Rewrite::Issue(issue) if !has_call(&x) => {
                    Arc::new(entry_fire(x, issue.pc))
                }
                _ => Arc::new(x),
            };
            let written = (arg != original).then_some(original);
            ExprKind::Qop { arg, written }
        }
        // a reference is to the variable, never to a snapshot of it
        ExprKind::ByRef(m, x) => {
            let cells = only(map, |r| matches!(r, Redirect::Cell(_)));
            let cells = cells.as_deref().unwrap_or(map);
            ExprKind::ByRef(*m, Arc::new(rewrite_with(x, cells, mode.deferred())))
        }
        ExprKind::Block { exprs } => {
            ExprKind::Block { exprs: rewrite_stmts(exprs, map, mode) }
        }
        ExprKind::Apply(a) => {
            let function = match callee_ref(e) {
                Some(f) => {
                    let unsnapped = only(map, |r| !r.snapshot());
                    rewrite(f, unsnapped.as_deref().unwrap_or(map))
                }
                None => rewrite(&a.function, map),
            };
            let call = ApplyExpr {
                function: Arc::new(function),
                args: Arc::from_iter(
                    a.args.iter().map(|(n, v)| (n.clone(), rewrite(v, map))),
                ),
            };
            if let Rewrite::Issue(issue) = mode {
                return issue_call(e, call, map, issue);
            }
            ExprKind::Apply(call)
        }
        // A call-free scrutinee fires at entry, so a re-match re-emits a level
        // arm while an arm holding an issued call waits on its own completion.
        ExprKind::Select(s) => ExprKind::Select(SelectExpr {
            arg: Arc::new(match mode {
                Rewrite::Issue(issue) if !has_call(&s.arg) => {
                    entry_fire(rewrite(&s.arg, map), issue.pc)
                }
                _ => rewrite(&s.arg, map),
            }),
            arms: Arc::from_iter(s.arms.iter().map(|(p, b)| {
                let inner = if reads_live(&s.arg, map) {
                    let mut inner = scope(map);
                    p.structure_predicate.with_names(&mut |n| {
                        inner.insert(n.clone(), Redirect::Live(n.clone()));
                    });
                    Some(inner)
                } else {
                    unbound(map, |f| p.structure_predicate.with_names(&mut |n| f(n)))
                };
                let inner = inner.as_deref().unwrap_or(map);
                let guard = p.guard.as_ref().map(|g| rewrite(g, inner));
                let mut p = p.clone();
                p.guard = guard;
                (p, rewrite(b, inner))
            })),
        }),
        ExprKind::Catch(c) => {
            let inner = unbound(map, |f| f(&c.bind.name));
            let inner = inner.as_deref().unwrap_or(map);
            ExprKind::Catch(Arc::new(CatchExpr {
                bind: c.bind.clone(),
                constraint: c.constraint.clone(),
                handler: Arc::new(rewrite_with(&c.handler, inner, mode.deferred())),
                // a machine's and a try's roles are built after every rewrite
                role: c.role.clone(),
            }))
        }
        ExprKind::Lambda(l) => {
            let inner = unbound(map, |f| {
                l.args.iter().for_each(|a| a.pattern.with_names(&mut |n| f(n)))
            });
            let inner = inner.as_deref().unwrap_or(map);
            let args = Arc::from_iter(l.args.iter().map(|a| Arg {
                comments: a.comments.clone(),
                kind: match &a.kind {
                    ArgKind::Defaulted(d) => {
                        ArgKind::Defaulted(rewrite_with(d, map, mode.deferred()))
                    }
                    other => other.clone(),
                },
                pattern: a.pattern.clone(),
                constraint: a.constraint.clone(),
                pos: a.pos,
            }));
            let body = match &l.body {
                LambdaBody::Expr(b) => {
                    LambdaBody::Expr(rewrite_with(b, inner, mode.deferred()))
                }
                LambdaBody::Builtin(s) => LambdaBody::Builtin(s.clone()),
            };
            ExprKind::Lambda(Arc::new(LambdaExpr {
                vargs_comments: l.vargs_comments.clone(),
                below: l.below.clone(),
                args,
                vargs: l.vargs.clone(),
                rtype: l.rtype.clone(),
                constraints: l.constraints.clone(),
                throws: l.throws.clone(),
                body,
            }))
        }
        _ => return e.map_children(&mut |c| rewrite(c, map)),
    };
    Expr {
        id: ExprId::new(),
        ori: e.ori.clone(),
        pos: e.pos,
        kind,
        dec: e.dec.clone(),
        str_form: e.str_form,
        end: e.end,
    }
}

fn let_bind(pos: SourcePosition, name: &str, typ: Option<Type>, value: Expr) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr {
        rec: false,
        pattern: StructurePattern::Bind(Name::from(name)),
        typ,
        value,
    }))
    .to_expr(pos)
}

fn let_pat(
    pos: SourcePosition,
    pattern: StructurePattern,
    typ: Option<Type>,
    value: Expr,
) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr { rec: false, pattern, typ, value })).to_expr(pos)
}

fn connect(pos: SourcePosition, name: &str, value: Expr) -> Expr {
    connect_path(pos, ModPath::from([name]), false, value)
}

fn connect_path(pos: SourcePosition, name: ModPath, deref: bool, value: Expr) -> Expr {
    ExprKind::Connect { name, at: WrittenAt::NOWHERE, value: Arc::new(value), deref }
        .to_expr(pos)
}

fn r#ref(pos: SourcePosition, name: &str) -> Expr {
    ExprKind::Ref { name: ModPath::from([name]) }.to_expr(pos)
}

fn never(pos: SourcePosition) -> Expr {
    ExprKind::Never { typ: None, args: Arc::from(Vec::<Expr>::new()) }.to_expr(pos)
}

fn integer(pos: SourcePosition, value: i64) -> Expr {
    ExprKind::Constant(Value::I64(value)).to_expr(pos)
}

fn sample(pos: SourcePosition, lhs: Expr, rhs: Expr) -> Expr {
    ExprKind::BinOp { op: BinOp::Sample, lhs: Arc::new(lhs), rhs: Arc::new(rhs) }
        .to_expr(pos)
}

fn guard(e: Expr) -> Expr {
    let pos = e.pos;
    ExprKind::SeqGuard(Arc::new(e)).to_expr(pos)
}

fn select(
    pos: SourcePosition,
    arg: Expr,
    arms: impl IntoIterator<Item = (Pattern, Expr)>,
) -> Expr {
    ExprKind::Select(SelectExpr { arg: Arc::new(arg), arms: Arc::from_iter(arms) })
        .to_expr(pos)
}

fn block(pos: SourcePosition, exprs: impl IntoIterator<Item = Expr>) -> Expr {
    let mut exprs: LPooled<Vec<Expr>> = exprs.into_iter().collect();
    match exprs.len() {
        0 => never(pos),
        1 => exprs.pop().unwrap(),
        _ => ExprKind::Block { exprs: Arc::from_iter(exprs.drain(..)) }.to_expr(pos),
    }
}

fn pat_bind(name: &str) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Bind(Name::from(name)),
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}

fn pat_wild() -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Ignore,
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}

fn pat_lit(v: Value) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::literal(v),
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}

/// `(_, …, name)`: the last of `n` tuple elements.
fn pat_last(n: usize, name: &str) -> Pattern {
    let binds = (1..n)
        .map(|_| StructurePattern::Ignore)
        .chain([StructurePattern::Bind(Name::from(name))]);
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Tuple {
            all: None,
            binds: Arc::from_iter(binds),
        },
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::parser::{max_nesting, parse_one};

    /// Every expression of a lowered machine comes from the seq's
    /// source, whatever the thread built last.
    #[test]
    fn a_machine_has_the_seq_origin() {
        use crate::expr::{Origin, Source, parser::parse};
        let ori = |name: &str| Origin {
            parent: None,
            source: Source::Vfs(ArcStr::from(name)),
            text: ArcStr::from("seq { a <- 1; b <- a; let c = f(b); c }"),
        };
        let seq = parse(ori("seq")).expect("parses");
        let _elsewhere = OriginScope::enter(Arc::new(ori("elsewhere")));
        let lowered = desugar(&seq[0], &Env::default(), &ModPath::root()).unwrap();
        let foreign = lowered.fold(0, &mut |n, e| {
            n + (e.ori.source != Source::Vfs(ArcStr::from("seq"))) as usize
        });
        assert_eq!(foreign, 0);
    }

    /// The compiler asks each seq it compiles for its fixed untils, the
    /// source seq and every seq its lowering builds: a `seqq let`'s until
    /// is named once over both.
    #[test]
    fn a_seqq_let_names_a_fixed_until_once() {
        let seq = parse_one("seqq let c = t { until flag; c }").expect("parses");
        let lowered = desugar(&seq, &Env::default(), &ModPath::root()).expect("lowers");
        let named = lowered.fold(fixed_untils(&seq).len(), &mut |n, e| match &e.kind {
            ExprKind::Seq(_) => n + fixed_untils(e).len(),
            _ => n,
        });
        assert_eq!(named, 1, "{lowered}");
    }

    #[test]
    fn the_pc_type_has_constant_size() {
        for n in [1, 32] {
            let mut body: LPooled<String> = LPooled::take();
            for _ in 0..n {
                body.push_str("f(1); ");
            }
            let seq = parse_one(&format_compact!("seq {{ {body} }}")).expect("parses");
            let lowered = desugar(&seq, &Env::default(), &ModPath::root()).unwrap();
            let mut found = false;
            lowered.fold((), &mut |(), e| {
                if let ExprKind::Bind(b) = &e.kind
                    && b.pattern.single_bind().is_some_and(|n| n.starts_with("#seqpc"))
                {
                    assert_eq!(b.typ, Some(Type::Primitive(Typ::I64.into())));
                    found = true;
                }
            });
            assert!(found);
        }
    }

    /// A seq lowers to a flat list of steps however long it is: no
    /// step nests the next.
    #[test]
    fn a_long_seq_lowers_flat() {
        let n = 4 * max_nesting();
        let body = (0..n).map(|i| format!("a <- {i}")).collect::<Vec<_>>().join("; ");
        let seq = parse_one(&format_compact!("seq {{ {body} }}")).expect("parses");
        let lowered = std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(move || {
                desugar(&seq, &Env::default(), &ModPath::root()).expect("lowers")
            })
            .expect("spawn")
            .join()
            .expect("seq lowering overflowed the stack");
        let steps = lowered.fold(0, &mut |n, e| match &e.kind {
            ExprKind::SeqMachine(m) => n + m.steps.len(),
            _ => n,
        });
        assert_eq!(steps, n);
    }

    /// A step under nested trys carries one jump catch, its innermost
    /// try's: that try catches every error its body raises.
    #[test]
    fn nested_trys_put_one_catch_on_a_step() {
        let d = 40;
        let mut body = String::from("let v0 = f()?");
        for i in 1..=d {
            body = format!("let v{i} = try {{ {body}; 1 }} with(e) {{ 0 }}");
        }
        let seq = parse_one(&format!("seq {{ {body}; v{d} }}")).expect("parses");
        let lowered = desugar(&seq, &Env::default(), &ModPath::root()).expect("lowers");
        let tries = lowered.fold(0, &mut |n, e| match &e.kind {
            ExprKind::Catch(c) if matches!(c.role, CatchRole::Try { .. }) => n + 1,
            _ => n,
        });
        let steps = lowered.fold(0, &mut |n, e| match &e.kind {
            ExprKind::SeqMachine(m) => n + m.steps.len(),
            _ => n,
        });
        assert!(tries <= steps, "{tries} try catches over {steps} steps");
    }

    /// A function the body binds is passed bare to an issued call, though
    /// a callback elsewhere binds its name: only the callback's own body
    /// sees that binding.
    #[test]
    fn a_function_stays_at_the_call_site_past_an_unrelated_binding() {
        let seq = parse_one(
            "seq t { let inc = |x| x + 1; let a = array::map([t, 2], inc); \
             let b = array::map([3], |inc| inc); (a, b) }",
        )
        .expect("parses");
        let lowered = desugar(&seq, &Env::default(), &ModPath::root()).expect("lowers");
        let bare = lowered.fold(0, &mut |n, e| match &e.kind {
            ExprKind::Apply(a) if a.args.len() == 2 => {
                n + matches!(&a.args[1].1.kind,
                    ExprKind::Ref { name } if simple_name(name) == Some("inc"))
                    as usize
            }
            _ => n,
        });
        assert_eq!(bare, 1, "{lowered}");
    }

    fn names(entries: &[(&str, Redirect)]) -> Names {
        entries.iter().map(|(n, r)| (ArcStr::from(*n), r.clone())).collect()
    }

    fn cell(n: &str) -> Redirect {
        Redirect::Cell(ArcStr::from(n))
    }

    fn snapshot(n: &str) -> Redirect {
        Redirect::Snapshot(ArcStr::from(n))
    }

    /// The rewrite's scoping: a name bound inside the rewritten
    /// expression is not redirected where that binding is in scope, and
    /// a snapshot takes only reads, never a call's function.
    #[test]
    fn the_rewrite_respects_scopes() {
        let map = names(&[
            ("f", cell("cf")),
            ("x", cell("cx")),
            ("e", cell("ce")),
            ("t", snapshot("st")),
            ("c", snapshot("sc")),
        ]);
        let bindings = Rewrite::Bindings;
        for (src, expected, mode) in [
            (
                "{ let rec f = |n| f(n - 1); f(x) }",
                "{ let rec f = |n| f(n - 1); f(cx) }",
                bindings,
            ),
            ("{ let f = |n| f(n); f(x) }", "{ let f = |n| cf(n); f(cx) }", bindings),
            ("select x { f => f, _ => f }", "select cx { f => f, _ => cf }", bindings),
            ("|f, y| f + x", "|f, y| f + cx", bindings),
            (
                "seq { try { e } with(e) { e } }",
                "seq { try { ce } with(e) { e } }",
                bindings,
            ),
            ("t <- t + 1", "t <- st + 1", bindings),
            ("x <- x + 1", "cx <- cx + 1", bindings),
            ("{ let q = &t; *q <- 10 }", "{ let q = &t; *q <- 10 }", bindings),
            ("{ let q = &x; *q <- t }", "{ let q = &cx; *q <- st }", bindings),
            ("seq let (f, y) = t { f + x }", "seq let (f, y) = st { f + cx }", bindings),
            ("c(c, f(x))", "c(sc, cf(cx))", bindings),
            (
                "seq abort(g(c)) { h(c) }",
                "seq abort(g(sc)) { h(sc) }",
                Rewrite::Issue(Issue { pc: "pc" }),
            ),
        ] {
            let got = rewrite_with(&parse_one(src).expect("parses"), &map, mode);
            let want = parse_one(expected).expect("parses");
            assert_eq!(got.to_string(), want.to_string(), "{src}");
        }
    }

    #[test]
    fn nested_seq_keeps_request_and_takes_own_entries() {
        let map = names(&[
            ("request", snapshot("request_snapshot")),
            ("bound", cell("bound_cell")),
            ("captured", Redirect::Capture(literal!("captured_snapshot"))),
            ("entry", Redirect::Entry(literal!("entry_snapshot"))),
        ]);
        let source = parse_one("seq request { (bound, captured, entry) }").unwrap();
        let expected =
            parse_one("seq request_snapshot { (bound_cell, captured, entry) }").unwrap();
        assert_eq!(rewrite(&source, &map).to_string(), expected.to_string());
    }
}
