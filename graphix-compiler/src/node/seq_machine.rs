//! The machine a seq lowers to (`design/dependency_summaries.md` §3):
//! its steps, one awake at a time. A step enters when `pc` becomes its
//! label, or, when the analysis found that it reads nothing the steps
//! before it left pending (`Step::same_cycle`), in the cycle its
//! predecessor completes. A passed step sleeps; entering one is its
//! birth for the run (`View::Step`) with `Select`'s catch-up, so for
//! sleep and wake the machine is a select whose arms are its steps.

use super::{
    compile_block_children, compiler::compile, evaluation_order, typecheck_in_order,
    typecheck1_statements, wake::TrackedFires,
};
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, NodeView, Notify, Refs, Rt, Scope,
    TagValue, Update, View,
    explain::{self, Because, Explain, Note},
    expr::{Expr, ExprId, SeqCaptureExpr, SeqMachineExpr},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr},
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    typ::Type,
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt::Write,
    mem,
    sync::atomic::{AtomicBool, Ordering::Relaxed},
};
use triomphe::Arc;

#[derive(Debug)]
pub(crate) struct Step<R: Rt> {
    label: ArcStr,
    until: bool,
    /// The statement's items: its handlers, a lambda a step's `let`
    /// names, the `let` of its value, then what completion writes.
    pub(crate) nodes: Box<[Node<R>]>,
    catches: Box<[usize]>,
    /// The index of the value's `let` in `nodes`.
    value: usize,
    value_id: BindId,
    pub(crate) next: Option<usize>,
    /// Set by the analysis: `next` enters in the cycle this step
    /// completes, not the cycle after.
    pub(crate) same_cycle: AtomicBool,
}

impl<R: Rt> Step<R> {
    fn refs(&self, refs: &mut Refs) {
        for n in self.nodes.iter() {
            n.refs(refs)
        }
    }

    /// It holds an `await`. Such a step is entered the cycle after the
    /// step before it completes, never in that cycle: the fire that
    /// completed an `await` there is spent, and one entered beside it
    /// would read it live.
    pub(crate) fn awaits(&self) -> bool {
        let mut inputs = IntSet::default();
        self.nodes.iter().for_each(|n| awaited(n, &mut inputs));
        !inputs.is_empty()
    }

    /// A step whose value is the `let` at `value`, after every handler.
    fn new(
        label: ArcStr,
        until: bool,
        nodes: Box<[Node<R>]>,
        catches: Box<[usize]>,
        value: usize,
        next: Option<usize>,
        same_cycle: bool,
    ) -> Result<Self> {
        let value_id = match nodes.get(value).map(|n| n.view()) {
            Some(NodeView::Bind(b)) => b.pattern.single_bind_id(),
            _ => None,
        };
        let Some(value_id) = value_id else { bail!("BUG: a seq step without its value") };
        if catches.iter().any(|c| *c > value) {
            bail!("BUG: a seq step's handler after its value")
        }
        Ok(Self {
            label,
            until,
            nodes,
            catches,
            value,
            value_id,
            next,
            same_cycle: AtomicBool::new(same_cycle),
        })
    }
}

#[derive(Debug)]
pub struct SeqMachine<R: Rt> {
    spec: Arc<Expr>,
    /// The seq's id, which its `seqq` captures name.
    pub(crate) id: ExprId,
    pub(crate) pc: Node<R>,
    pub(crate) pc_id: BindId,
    pub(crate) steps: Box<[Step<R>]>,
    /// The awake step. A sleeping machine is reset.
    current: Option<usize>,
    /// A run is under way: a step has been entered since `pc` last was
    /// idle.
    running: bool,
    /// Built on the first update, when the steps are materialized.
    tracked: Option<Tracking>,
    /// `--expand`: the analysis prints the plan it decides.
    pub(crate) expand: bool,
}

#[derive(Debug)]
struct Tracking {
    fires: TrackedFires,
    /// The inputs the steps' own `let`s bind.
    lets: IntSet<BindId>,
    /// The inputs an `await` reads: a run is owed their fires from the
    /// cycle it was started in on, and none from before.
    awaited: IntSet<BindId>,
}

/// What the `await`s under `n` read.
fn awaited<R: Rt>(n: &Node<R>, out: &mut IntSet<BindId>) {
    crate::stack::ensure_sufficient(|| match n.view() {
        NodeView::CallSite(c) if c.is_await() => {
            let mut refs = Refs::default();
            c.arg_nodes().for_each(|a| a.refs(&mut refs));
            refs.with_refs(|id| {
                out.insert(id);
            })
        }
        _ => n.for_each_child(&mut |c| awaited(c, out)),
    })
}

fn pc_id<R: Rt>(pc: &Node<R>) -> Result<BindId> {
    match pc.view() {
        NodeView::Ref(r) => Ok(r.id),
        _ => bail!("BUG: a seq machine's pc is not a variable"),
    }
}

impl<R: Rt> SeqMachine<R> {
    fn with(
        spec: Arc<Expr>,
        id: ExprId,
        pc: Node<R>,
        pc_id: BindId,
        steps: Box<[Step<R>]>,
        expand: bool,
    ) -> Node<R> {
        Node::new(Self {
            spec,
            id,
            pc,
            pc_id,
            steps,
            current: None,
            running: false,
            tracked: None,
            expand,
        })
    }

    // CR claude for claude: [perf/low] One run of a seq whose N steps share a cycle
    // costs O(N^2): 400/800/1600/3200 steps take 18/74/292/1190 ms, while the same
    // chain of lets in a plain block takes under a millisecond. PAR and PRUNE off
    // change nothing. c-prune-wake-05's per-step input count no longer matters; the
    // step count does. The cause is not pinned here. The per-step work that grows with
    // N is this loop's transition: ctx.deliver of the pc, which every step reads, and
    // TrackedFires' enter, deliver, open, ran and deselect for each step. Fix: profile
    // a 3200-step run and make a transition cost O(the step's own inputs). probe:
    // design/review-2026-11-16/repro/x-rerun-1-seq-steps-quadratic.sh (x-rerun-1-04)
    fn update_serial(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let Self {
            spec: _,
            id: _,
            pc,
            pc_id,
            steps,
            current,
            running,
            tracked,
            expand: _,
        } = self;
        let Tracking { fires: tracked, lets, awaited: awaits } = tracked
            .get_or_insert_with(|| {
                let fires =
                    TrackedFires::new(&ctx.env, ctx.rt.cycle(), steps.len(), |i, r| {
                        steps[i].refs(r)
                    });
                let (mut lets, mut awaits) = (IntSet::default(), IntSet::default());
                for n in steps.iter().flat_map(|s| s.nodes.iter()) {
                    if let NodeView::Bind(b) = n.view() {
                        b.pattern.ids(&mut |id| {
                            lets.insert(id);
                        });
                    }
                    awaited(n, &mut awaits)
                }
                Tracking { fires, lets, awaited: awaits }
            });
        let at = pc.update(ctx);
        let at = at.is_fired().then(|| at.value_cloned());
        let target = match at {
            Some(Value::I64(crate::expr::seq::IDLE)) => Some(None),
            Some(Value::I64(k)) => {
                usize::try_from(k).ok().filter(|k| *k < steps.len()).map(Some)
            }
            _ => None,
        };
        let mut entered = false;
        match target {
            Some(None) => *running = false,
            // a run starts a cycle after the trigger that started it
            Some(Some(_)) if !mem::replace(running, true) => {
                tracked.forget_before(ctx, awaits, ctx.rt.cycle().saturating_sub(1))
            }
            _ => (),
        }
        match target {
            Some(None) => {
                if let Some(j) = current.take() {
                    deselect(ctx, tracked, j, &mut steps[j]);
                }
            }
            Some(Some(k)) if *current == Some(k) => (),
            Some(Some(k)) => {
                if let Some(j) = current.replace(k) {
                    deselect(ctx, tracked, j, &mut steps[j]);
                }
                entered = true;
            }
            None => (),
        }
        let mut evaluated: SmallVec<[usize; 4]> = SmallVec::new();
        while let Some(k) = *current {
            evaluated.push(k);
            if !evaluate(ctx, tracked, k, &mut steps[k], entered) {
                break;
            }
            deselect(ctx, tracked, k, &mut steps[k]);
            *current = None;
            let Some(n) = steps[k].next else {
                ctx.rt.set_var(*pc_id, Value::I64(crate::expr::seq::IDLE));
                break;
            };
            let at = Value::I64(n as i64);
            if !steps[k].same_cycle.load(Relaxed) {
                ctx.rt.set_var(*pc_id, at);
                break;
            }
            // CR claude for claude: [perf/medium] A seq run costs O(steps^2). Each
            // same-cycle handoff delivers pc through store_fire, which walks every
            // reader of pc (about three per step, registered while the steps sleep),
            // marks each one, and queues a late fire for every reader in a step already
            // run this cycle and now deselected; notify_set walks them again. Each
            // entered step also runs TrackedFires::enter, whose carried.retain scans
            // every carried let fire (the N chained lets of the last run). A chain of
            // 400 lets takes 15.4 ms per run and 1600 takes 281 ms, in about 3 cycles.
            // Fix: write a same-cycle handoff's pc without the reader scan, since the
            // entered step runs under View::Step and no Prune skips there, then deliver
            // the last pc once with the scan for readers outside the machine
            // (#seqidle); make enter walk per_arm[i] and take each input from carried.
            // probe:
            // design/review-2026-11-16/repro/x-perf-runtime-seq-steps-quadratic.sh Also
            // reported as (x-stack-03). (x-perf-runtime-02)
            ctx.deliver(*pc_id, TagValue::fired(at), Notify::Roots);
            *current = Some(n);
            entered = true;
        }
        tracked.observe_except(ctx, &evaluated, Some(&*lets));
        TagValue::phantom_ref()
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        m: &SeqMachineExpr,
    ) -> Result<Node<R>> {
        let pc = compile(ctx, flags, (*m.pc).clone(), scope, top_id)?;
        let pc_id = pc_id(&pc)?;
        let mut scopes: SmallVec<[Scope; 2]> = SmallVec::new();
        for (i, parent) in m.scopes.iter().enumerate() {
            let s = match i {
                0 => scope.clone(),
                _ => scopes[*parent as usize].append_place("seq", &spec, Some(i)),
            };
            scopes.push(s);
        }
        // a step's lets are visible to the steps after it, whose writes
        // write them
        let writers: SmallVec<[&Expr; 32]> =
            m.steps.iter().flat_map(|s| s.items.iter()).collect();
        let top = super::block_top(ctx, scope);
        ctx.written.enter(&writers, top);
        let steps = Self::compile_steps(ctx, flags, top_id, m, &scopes);
        ctx.written.exit();
        let steps = steps?;
        Ok(Self::with(
            spec,
            m.id,
            pc,
            pc_id,
            steps.into_boxed_slice(),
            flags.contains(CFlag::ExpandSeq),
        ))
    }

    fn compile_steps(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        top_id: ExprId,
        m: &SeqMachineExpr,
        scopes: &[Scope],
    ) -> Result<Vec<Step<R>>> {
        let mut steps = Vec::with_capacity(m.steps.len());
        for s in m.steps.iter() {
            let scope = &scopes[s.scope as usize];
            let (nodes, catches) = compile_block_children(
                ctx,
                flags | CFlag::SeqStep,
                scope,
                top_id,
                false,
                s.items.iter(),
                false,
            )?;
            let next = s.next.map(|n| n as usize);
            // the value's let, by the name the lowering gave it
            let value = s.items.iter().position(|e| match &e.kind {
                crate::expr::ExprKind::Bind(b) => {
                    b.pattern.single_bind().is_some_and(|n| *n == s.value)
                }
                _ => false,
            });
            let Some(value) = value else { bail!("BUG: a seq step without its value") };
            let step =
                Step::new(s.label.clone(), s.until, nodes, catches, value, next, false)?;
            steps.push(step);
        }
        Ok(steps)
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let id = ExprId::decode(buf)?;
        let pc = decode_node(ctx, buf)?;
        let pc_id = pc_id(&pc).map_err(|_| PackError::InvalidFormat)?;
        let n = crate::image::count_decode(buf)?;
        let mut steps = Vec::with_capacity(n);
        for _ in 0..n {
            let label = ArcStr::decode(buf)?;
            let until = bool::decode(buf)?;
            let nodes = decode_nodes(ctx, buf)?.into_boxed_slice();
            let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
            let value = u64::decode(buf)? as usize;
            let next = Option::<u64>::decode(buf)?.map(|n| n as usize);
            let same_cycle = bool::decode(buf)?;
            let step = Step::new(label, until, nodes, catches, value, next, same_cycle)
                .map_err(|_| PackError::InvalidFormat)?;
            steps.push(step);
        }
        Ok(Self::with(spec, id, pc, pc_id, steps.into_boxed_slice(), false))
    }
}

impl<R: Rt> SeqMachine<R> {
    /// Each step's boundary: `S0 -> S1 same`, `S1 -> S2 next`, `S2 -> end`.
    pub(crate) fn plan(&self) -> LPooled<String> {
        let mut out: LPooled<String> = LPooled::take();
        for (k, s) in self.steps.iter().enumerate() {
            if k > 0 {
                out.push_str(", ");
            }
            let _ = match s.next {
                None => write!(out, "{} -> end", s.label),
                Some(n) => {
                    let when = if s.same_cycle.load(Relaxed) { "same" } else { "next" };
                    write!(out, "{} -> {} {when}", s.label, self.steps[n].label)
                }
            };
        }
        out
    }
}

/// Put step `j` to sleep and re-collect what it reads, as a select
/// deselects an arm (`select.rs::deselect`).
fn deselect<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    tracked: &mut TrackedFires,
    j: usize,
    step: &mut Step<R>,
) {
    super::deselecting_arm(true, || {
        for n in step.nodes.iter_mut() {
            n.sleep(ctx)
        }
    });
    tracked.deselect(ctx, j, |r| step.refs(r));
}

/// Update step `k`, under the step view when it is being `entered` (a
/// run's entry is its constants' birth), with its catch-up deliveries; its completion writes run only in the cycle
/// it completes. Whether it completed.
fn evaluate<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    tracked: &mut TrackedFires,
    k: usize,
    step: &mut Step<R>,
    entered: bool,
) -> bool {
    let view = if entered { View::Step } else { View::Cycle };
    ctx.under(view, |ctx| {
        if entered {
            tracked.enter(k)
        }
        let injected = tracked.deliver(ctx, k);
        let scope = tracked.open(ctx, k);
        let mut done = false;
        for i in evaluation_order(step.nodes.len(), &step.catches) {
            if i > step.value && !done {
                continue;
            }
            step.nodes[i].update(ctx);
            if i == step.value {
                done = ctx.event.variables.get(&step.value_id).is_some_and(|tv| {
                    tv.is_fired()
                        && (!step.until || tv.value_cloned() == Value::Bool(true))
                });
            }
        }
        let changed = tracked.ran(ctx, k, scope);
        TrackedFires::restore(ctx.event, injected);
        tracked.evaluated(ctx, k, changed, |r| step.refs(r));
        done
    })
}

impl<R: Rt> SeqMachine<R> {
    /// Why the seq has no value: the step it is at has none, or it is
    /// not running, when it waits on what the statements `around` it
    /// read, its trigger among them.
    pub(crate) fn explain<'a>(
        &'a self,
        cx: &mut Explain<'a, R>,
        around: impl Iterator<Item = &'a Node<R>>,
        out: &mut Vec<Because>,
    ) {
        for s in self.steps.iter() {
            cx.lets(s.nodes.iter())
        }
        let mut parts = Vec::new();
        match self.current.and_then(|i| self.steps.get(i)) {
            Some(s) => {
                if let Some(n) = s.nodes.get(s.value) {
                    cx.explain(n, &mut parts)
                }
            }
            None => {
                let mut waits = Vec::new();
                for n in around {
                    if let NodeView::Bind(b) = n.view()
                        && b.spec.is_lowered()
                    {
                        cx.explain(&b.node, &mut waits)
                    }
                }
                let mut ids = Vec::new();
                explain::reads(&waits, &mut ids);
                parts.extend(ids.into_iter().map(Because::Reads))
            }
        }
        let note = Note::Seq { step: self.current, steps: self.steps.len() };
        out.push(Because::Of { spec: self.spec.clone(), note, parts })
    }
}

impl<R: Rt> Update<R> for SeqMachine<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.pc);
        self.steps.iter().flat_map(|s| s.nodes.iter()).for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.pc);
        self.steps.iter_mut().flat_map(|s| s.nodes.iter_mut()).for_each(f)
    }

    /// The awake step and the tracker exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() || self.tracked.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::SeqMachine, buf);
        self.spec.encode(buf)?;
        self.id.encode(buf)?;
        self.pc.image_encode(buf)?;
        encode_varint(self.steps.len() as u64, buf);
        for s in self.steps.iter() {
            s.label.encode(buf)?;
            s.until.encode(buf)?;
            encode_nodes(&s.nodes, buf)?;
            image::slice_encode(&s.catches, buf)?;
            (s.value as u64).encode(buf)?;
            s.next.map(|n| n as u64).encode(buf)?;
            s.same_cycle.load(Relaxed).encode(buf)?;
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let flags = crate::branch::ForkFlags { seq: true, ..ctx.fork };
        ctx.with_fork_flags(flags, |ctx| self.update_serial(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.pc.sleep(ctx);
        for s in self.steps.iter_mut() {
            for n in s.nodes.iter_mut() {
                n.sleep(ctx)
            }
        }
        if let Some(t) = &mut self.tracked {
            t.fires.all_fell_asleep(super::wake::stopped(ctx));
        }
        self.current = None;
        self.running = false;
    }

    fn refs(&self, refs: &mut Refs) {
        self.pc.refs(refs);
        for s in self.steps.iter() {
            s.refs(refs)
        }
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.pc.typecheck0(ctx)?;
        for s in self.steps.iter_mut() {
            typecheck_in_order(ctx, &mut s.nodes, &s.catches, false, |n, ctx| {
                n.typecheck0(ctx)
            })?;
        }
        Ok(())
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        self.pc.typecheck0_instance(ctx, types)?;
        for s in self.steps.iter_mut() {
            typecheck_in_order(ctx, &mut s.nodes, &s.catches, false, |n, ctx| {
                n.typecheck0_instance(ctx, types)
            })?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.pc.typecheck1(ctx)?;
        for s in self.steps.iter_mut() {
            typecheck1_statements(ctx, &mut s.nodes, &s.catches, false)?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::SeqMachine(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("emit_clif: a seq machine sequences across cycles")
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        for s in self.steps.iter_mut() {
            for n in s.nodes.iter_mut() {
                fusion::fuse(n, ctx)?;
            }
        }
        Ok(None)
    }
}

/// A `seqq` capture: the queued snapshot of a variable its body reads,
/// or the variable itself when the analysis finds that a step of its
/// machine writes it (`live`, set before the first update).
#[derive(Debug)]
pub struct SeqCapture<R: Rt> {
    spec: Arc<Expr>,
    pub(crate) machine: ExprId,
    pub(crate) snapshot: Node<R>,
    pub(crate) live: Node<R>,
    pub(crate) live_id: BindId,
    pub(crate) is_live: AtomicBool,
    /// The variable is a generalized binding, which nothing writes: the
    /// capture is typed by `live`'s read, which carries its scheme, never
    /// by the queue's one instance of it.
    pub(crate) scheme: bool,
}

impl<R: Rt> SeqCapture<R> {
    fn with(
        spec: Arc<Expr>,
        machine: ExprId,
        snapshot: Node<R>,
        live: Node<R>,
        live_id: BindId,
        is_live: bool,
        scheme: bool,
    ) -> Node<R> {
        Node::new(Self {
            spec,
            machine,
            snapshot,
            live,
            live_id,
            is_live: AtomicBool::new(is_live),
            scheme,
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        c: &SeqCaptureExpr,
    ) -> Result<Node<R>> {
        let snapshot = compile(ctx, flags, (*c.snapshot).clone(), scope, top_id)?;
        let live = compile(ctx, flags, (*c.live).clone(), scope, top_id)?;
        let live_id = pc_id(&live)?;
        let machine = c.machine;
        let scheme = super::bind::reads_generic(&ctx.env, &live);
        Ok(Self::with(spec, machine, snapshot, live, live_id, false, scheme))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let machine = ExprId::decode(buf)?;
        let snapshot = decode_node(ctx, buf)?;
        let live = decode_node(ctx, buf)?;
        let live_id = pc_id(&live).map_err(|_| PackError::InvalidFormat)?;
        let is_live = bool::decode(buf)?;
        let scheme = bool::decode(buf)?;
        Ok(Self::with(spec, machine, snapshot, live, live_id, is_live, scheme))
    }

    fn chosen(&mut self) -> &mut Node<R> {
        if self.is_live.load(Relaxed) { &mut self.live } else { &mut self.snapshot }
    }
}

impl<R: Rt> Update<R> for SeqCapture<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.snapshot);
        f(&self.live)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.snapshot);
        f(&mut self.live)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqCapture, buf);
        self.spec.encode(buf)?;
        self.machine.encode(buf)?;
        self.snapshot.image_encode(buf)?;
        self.live.image_encode(buf)?;
        self.is_live.load(Relaxed).encode(buf)?;
        self.scheme.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        self.chosen().update(ctx)
    }

    fn typ(&self) -> &Type {
        match self.scheme {
            true => self.live.typ(),
            false => self.snapshot.typ(),
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.snapshot.typecheck0(ctx)?;
        self.live.typecheck0(ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::SeqCapture(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("emit_clif: a seqq capture is chosen by the analysis")
    }
}
