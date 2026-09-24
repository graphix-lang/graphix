//! The machine a seq lowers to (`design/dependency_summaries.md` §3):
//! its steps, one awake at a time. A step enters when `pc` becomes its
//! label, or, when the analysis found that it reads nothing the steps
//! before it left pending (`Step::same_cycle`), in the cycle its
//! predecessor completes. A passed step sleeps; entering one wakes it
//! under the wake view with `Select`'s catch-up, so for sleep and wake
//! the machine is a select whose arms are its steps.

use super::{
    compile_block_children, compiler::compile, evaluation_order, typecheck_in_order,
    typecheck1_settled, wake::TrackedFires,
};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update,
    UserEvent,
    expr::{Expr, ExprId, SeqCaptureExpr, SeqMachineExpr},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr},
    },
    image::{
        self, ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, nodes_len, put_tag, tag_len,
        },
    },
    typ::Type,
};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Value;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt::Write,
    sync::atomic::{AtomicBool, Ordering::Relaxed},
};

#[derive(Debug)]
pub(crate) struct Step<R: Rt, E: UserEvent> {
    label: ArcStr,
    until: bool,
    /// The statement's items: its handlers, the `let` of its value, then
    /// what completion writes.
    pub(crate) nodes: Box<[Node<R, E>]>,
    catches: Box<[usize]>,
    /// The index of the value's `let` in `nodes`.
    value: usize,
    value_id: BindId,
    pub(crate) next: Option<usize>,
    /// Set by the analysis: `next` enters in the cycle this step
    /// completes, not the cycle after.
    pub(crate) same_cycle: AtomicBool,
}

impl<R: Rt, E: UserEvent> Step<R, E> {
    fn refs(&self, refs: &mut Refs) {
        for n in self.nodes.iter() {
            n.refs(refs)
        }
    }

    fn new(
        label: ArcStr,
        until: bool,
        nodes: Box<[Node<R, E>]>,
        catches: Box<[usize]>,
        next: Option<usize>,
        same_cycle: bool,
    ) -> Result<Self> {
        let value = catches.len();
        let value_id = match nodes.get(value).map(|n| n.view()) {
            Some(NodeView::Bind(b)) => b.pattern.single_bind_id(),
            _ => None,
        };
        let Some(value_id) = value_id else { bail!("BUG: a seq step without its value") };
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
pub struct SeqMachine<R: Rt, E: UserEvent> {
    spec: Expr,
    /// The seq's id, which its `seqq` captures name.
    pub(crate) id: u64,
    pub(crate) pc: Node<R, E>,
    pub(crate) pc_id: BindId,
    pub(crate) steps: Box<[Step<R, E>]>,
    /// The awake step. A sleeping machine is reset.
    current: Option<usize>,
    /// Built on the first update, when the steps are materialized: the
    /// catch-up tracker, and the inputs the steps' own `let`s bind.
    tracked: Option<(TrackedFires, IntSet<BindId>)>,
    /// `--expand`: the analysis prints the plan it decides.
    pub(crate) expand: bool,
}

fn pc_id<R: Rt, E: UserEvent>(pc: &Node<R, E>) -> Result<BindId> {
    match pc.view() {
        NodeView::Ref(r) => Ok(r.id),
        _ => bail!("BUG: a seq machine's pc is not a variable"),
    }
}

impl<R: Rt, E: UserEvent> SeqMachine<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        m: &SeqMachineExpr,
    ) -> Result<Node<R, E>> {
        let pc = compile(ctx, flags, (*m.pc).clone(), scope, top_id)?;
        let pc_id = pc_id(&pc)?;
        let mut scopes: SmallVec<[Scope; 2]> = SmallVec::new();
        for (i, parent) in m.scopes.iter().enumerate() {
            let s = match i {
                0 => scope.clone(),
                _ => scopes[*parent as usize].append_block("seq", ExprId::new().inner()),
            };
            scopes.push(s);
        }
        let mut steps = Vec::with_capacity(m.steps.len());
        for s in m.steps.iter() {
            let scope = &scopes[s.scope as usize];
            let (nodes, catches) =
                compile_block_children(ctx, flags, scope, top_id, false, s.items.iter())?;
            let next = s.next.map(|n| n as usize);
            steps.push(Step::new(s.label.clone(), s.until, nodes, catches, next, false)?);
        }
        Ok(Node::new(Self {
            spec,
            id: m.id,
            pc,
            pc_id,
            steps: steps.into_boxed_slice(),
            current: None,
            tracked: None,
            expand: flags.contains(CFlag::ExpandSeq),
        }))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let id = u64::decode(buf)?;
        let pc = decode_node(ctx, buf)?;
        let pc_id = pc_id(&pc).map_err(|_| PackError::InvalidFormat)?;
        let n = decode_varint(buf)? as usize;
        let mut steps = Vec::with_capacity(n.min(buf.len()));
        for _ in 0..n {
            let label = ArcStr::decode(buf)?;
            let until = bool::decode(buf)?;
            let nodes = decode_nodes(ctx, buf)?.into_boxed_slice();
            let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
            let next = Option::<u64>::decode(buf)?.map(|n| n as usize);
            let same_cycle = bool::decode(buf)?;
            let step = Step::new(label, until, nodes, catches, next, same_cycle)
                .map_err(|_| PackError::InvalidFormat)?;
            steps.push(step);
        }
        Ok(Node::new(Self {
            spec,
            id,
            pc,
            pc_id,
            steps: steps.into_boxed_slice(),
            current: None,
            tracked: None,
            expand: false,
        }))
    }
}

impl<R: Rt, E: UserEvent> SeqMachine<R, E> {
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
fn deselect<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    tracked: &mut TrackedFires,
    j: usize,
    step: &mut Step<R, E>,
) {
    let saved = ctx.deselecting_arm;
    ctx.deselecting_arm = true;
    for n in step.nodes.iter_mut() {
        n.sleep(ctx)
    }
    ctx.deselecting_arm = saved;
    tracked.refresh(&ctx.env, j, |r| step.refs(r));
}

/// Update step `k`, under the wake view when it is being `entered`, with
/// its catch-up deliveries; its completion writes run only in the cycle
/// it completes. Whether it completed.
fn evaluate<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    tracked: &mut TrackedFires,
    k: usize,
    step: &mut Step<R, E>,
    entered: bool,
) -> bool {
    let (init, wake) = (event.init, event.wake_init);
    if entered {
        event.init = true;
        event.wake_init = true;
    }
    let injected = tracked.deliver(ctx, event, k);
    let mut done = false;
    for i in evaluation_order(step.nodes.len(), &step.catches) {
        if i > step.value && !done {
            continue;
        }
        step.nodes[i].update(ctx, event);
        if i == step.value {
            done = event.variables.get(&step.value_id).is_some_and(|tv| {
                tv.is_fired() && (!step.until || tv.value_cloned() == Value::Bool(true))
            });
        }
    }
    TrackedFires::restore(event, injected);
    event.init = init;
    event.wake_init = wake;
    done
}

impl<R: Rt, E: UserEvent> Update<R, E> for SeqMachine<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.id.encoded_len()
            + self.pc.image_len()
            + varint_len(self.steps.len() as u64)
            + self
                .steps
                .iter()
                .map(|s| {
                    s.label.encoded_len()
                        + s.until.encoded_len()
                        + nodes_len(&s.nodes)
                        + image::slice_len(&s.catches)
                        + s.next.map(|n| n as u64).encoded_len()
                        + 1
                })
                .sum::<usize>()
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
            s.next.map(|n| n as u64).encode(buf)?;
            s.same_cycle.load(Relaxed).encode(buf)?;
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let Self { spec: _, id: _, pc, pc_id, steps, current, tracked, expand: _ } = self;
        let (tracked, lets) = tracked.get_or_insert_with(|| {
            let tracked =
                TrackedFires::new(&ctx.env, steps.len(), |i, r| steps[i].refs(r));
            let mut lets = IntSet::default();
            for n in steps.iter().flat_map(|s| s.nodes.iter()) {
                if let NodeView::Bind(b) = n.view() {
                    b.pattern.ids(&mut |id| {
                        lets.insert(ctx.env.facet_of(id));
                    });
                }
            }
            (tracked, lets)
        });
        let at = pc.update(ctx, event);
        let target = match at.is_fired().then(|| at.value_cloned()) {
            Some(Value::String(l)) if l == "Idle" => Some(None),
            Some(Value::String(l)) => steps.iter().position(|s| s.label == l).map(Some),
            _ => None,
        };
        tracked.observe(ctx, event);
        let mut entered = false;
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
            if !evaluate(ctx, event, tracked, k, &mut steps[k], entered) {
                break;
            }
            deselect(ctx, tracked, k, &mut steps[k]);
            *current = None;
            let Some(n) = steps[k].next else {
                ctx.rt.set_var(*pc_id, Value::String(literal!("Idle")));
                break;
            };
            let at = Value::String(steps[n].label.clone());
            if !steps[k].same_cycle.load(Relaxed) {
                ctx.rt.set_var(*pc_id, at);
                break;
            }
            event.variables.insert(*pc_id, TagValue::fired(at.clone()));
            ctx.rt.store_insert(*pc_id, TagValue::fired(at));
            ctx.rt.notify_set(*pc_id);
            *current = Some(n);
            entered = true;
        }
        tracked.observe_except(ctx, event, &evaluated, Some(lets));
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pc.delete(ctx);
        for s in self.steps.iter_mut() {
            for n in s.nodes.iter_mut() {
                n.delete(ctx)
            }
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pc.sleep(ctx);
        for s in self.steps.iter_mut() {
            for n in s.nodes.iter_mut() {
                n.sleep(ctx)
            }
        }
        self.current = None;
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pc.reset_replay(ctx);
        for s in self.steps.iter_mut() {
            for n in s.nodes.iter_mut() {
                n.reset_replay(ctx)
            }
        }
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

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.pc.typecheck0(ctx)?;
        for s in self.steps.iter_mut() {
            typecheck_in_order(ctx, &mut s.nodes, &s.catches, false, |n, ctx| {
                n.typecheck0(ctx)
            })?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.pc.typecheck1(ctx)?;
        for s in self.steps.iter_mut() {
            typecheck_in_order(ctx, &mut s.nodes, &s.catches, false, typecheck1_settled)?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqMachine(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("emit_clif: a seq machine sequences across cycles")
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
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
pub struct SeqCapture<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) machine: u64,
    pub(crate) snapshot: Node<R, E>,
    pub(crate) live: Node<R, E>,
    pub(crate) live_id: BindId,
    pub(crate) is_live: AtomicBool,
}

impl<R: Rt, E: UserEvent> SeqCapture<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        c: &SeqCaptureExpr,
    ) -> Result<Node<R, E>> {
        let snapshot = compile(ctx, flags, (*c.snapshot).clone(), scope, top_id)?;
        let live = compile(ctx, flags, (*c.live).clone(), scope, top_id)?;
        let live_id = pc_id(&live)?;
        let machine = c.machine;
        let is_live = AtomicBool::new(false);
        Ok(Node::new(Self { spec, machine, snapshot, live, live_id, is_live }))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let machine = u64::decode(buf)?;
        let snapshot = decode_node(ctx, buf)?;
        let live = decode_node(ctx, buf)?;
        let live_id = pc_id(&live).map_err(|_| PackError::InvalidFormat)?;
        let is_live = AtomicBool::new(bool::decode(buf)?);
        Ok(Node::new(Self { spec, machine, snapshot, live, live_id, is_live }))
    }

    fn chosen(&mut self) -> &mut Node<R, E> {
        if self.is_live.load(Relaxed) { &mut self.live } else { &mut self.snapshot }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for SeqCapture<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.machine.encoded_len()
            + self.snapshot.image_len()
            + self.live.image_len()
            + 1
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqCapture, buf);
        self.spec.encode(buf)?;
        self.machine.encode(buf)?;
        self.snapshot.image_encode(buf)?;
        self.live.image_encode(buf)?;
        self.is_live.load(Relaxed).encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        self.chosen().update(ctx, event)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.snapshot.delete(ctx);
        self.live.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.snapshot.sleep(ctx);
        self.live.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.snapshot.reset_replay(ctx);
        self.live.reset_replay(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.snapshot.refs(refs);
        self.live.refs(refs);
    }

    fn typ(&self) -> &Type {
        self.snapshot.typ()
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.snapshot.typecheck0(ctx)?;
        self.live.typecheck0(ctx)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.snapshot.typecheck1(ctx)?;
        self.live.typecheck1(ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqCapture(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("emit_clif: a seqq capture is chosen by the analysis")
    }
}
