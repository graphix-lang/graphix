use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BindMode, BuiltIn, CompileCtx, Effect, ExecCtx, InitFn, LambdaId,
    Node, Refs, Rt, Scope, TagValue, UserEvent,
    effects::{EffectKind, RecursionKind},
    env::Env,
    expr::{Arg, ArgKind, ExprId, StructurePattern, WrittenAt},
    image::{self, ImageBuf},
    node::{genn, lambda::LambdaDef},
    typ::{FnType, Type},
};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug, marker::PhantomData, sync::Arc as SArc};
use triomphe::Arc;

use crate::{seam_tick, seam_value};

#[derive(Debug)]
struct QueueEntry {
    /// The (BindId, Value) pairs for the args that fired in the originating
    /// cycle; on dispatch only these are written, so pred sees only the
    /// args that actually updated.
    updates: LPooled<Vec<(BindId, Value)>>,
}

#[derive(Debug, Default)]
struct QueueState {
    queue: VecDeque<QueueEntry>,
    pop_count: i64,
    /// BindId of the writable `#count` ref, or None if not provided.
    count_ref: Option<BindId>,
    /// Last value written through `count_ref` to avoid redundant writes.
    last_written_depth: i64,
}

impl QueueState {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            pop_count: 1,
            count_ref: None,
            last_written_depth: 0,
        }
    }

    fn depth(&self) -> i64 {
        self.queue.len() as i64
    }
}

type StateRef = Arc<Mutex<QueueState>>;

/// Per-call-site Apply impl for the wrapper lambda. Each invocation of the
/// wrapper at a user call site goes through this. Push/pop coordination is
/// done via `state` shared with the owning `QueueFn` node.
#[derive(Debug)]
struct WrapperApply<R: Rt, E: UserEvent> {
    state: StateRef,
    /// One bind per fn arg, owned by this call site. `pred` references these
    /// to read the args at invocation time. Indexed positionally (`arg_bids[i]`
    /// corresponds to `from[i]`).
    arg_bids: Arc<[BindId]>,
    /// Compiled call to `f` using `arg_bids` as inputs.
    pred: Node<R, E>,
    typ: Arc<FnType>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WrapperApply<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // the wrapper lambda is a runtime definition, built once a cycle ran
        let _ = buf;
        Err(PackError::Application(image::NOT_QUIESCENT))
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let mut delta: LPooled<Vec<(BindId, Value)>> = LPooled::take();
        for (i, n) in from.iter_mut().enumerate() {
            if let Some(v) = seam_tick(n.update(ctx)) {
                if let Some(bid) = self.arg_bids.get(i) {
                    delta.push((*bid, v.value_cloned()));
                }
            }
        }
        if !delta.is_empty() {
            let count_write = {
                let mut s = self.state.lock();
                // CR claude for eric: [bug] The immediate path inserts its args
                // straight into ctx.event.variables. A popped invocation that set_var
                // delivered to the same binds this cycle gets overwritten. This happens
                // when the trigger fires again in the cycle after a pop that emptied
                // the queue: it banks a pop, and a new call at this site in that cycle
                // replaces the released invocation. The released call never runs, or
                // with several args it merges into the new call. Both engines lose it.
                // probe: design/review-2026-10-05/repro/core-aux-04.gx (prints ran 1,
                // ran 3; ran 2 never runs). (core-aux-04)
                if s.pop_count > 0 {
                    s.pop_count -= 1;
                    drop(s);
                    for (bid, v) in delta.drain(..) {
                        ctx.rt.store_insert(bid, TagValue::fired(v.clone()));
                        ctx.event.variables.insert(bid, TagValue::fired(v));
                    }
                    None
                } else {
                    s.queue.push_back(QueueEntry { updates: delta });
                    let depth = s.depth();
                    if let Some(bid) = s.count_ref {
                        if depth != s.last_written_depth {
                            s.last_written_depth = depth;
                            Some((bid, depth))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            };
            if let Some((bid, depth)) = count_write {
                ctx.rt.set_var(bid, Value::I64(depth));
            }
        }
        // CR claude for eric: [bug] When the wrapped call delivers a bottom, seam_tick
        // reads it as "no tick" here and the wrapper rides its previous result STALE. A
        // consumer that fires on another input then pairs the old result with the new
        // input: `(qparse(s$), s)` prints (5, "x") where `parse(s$)` is bottom. This
        // breaks R3 in design/dense_delivery.md: a consumed bottom bottoms the
        // production, and only the three Held sites ride. opt.rs does the same in three
        // places: - HofState::tick_unary (line 325), behind map, flat_map, is_some_and
        // and is_none_or; - OptFilter::update (line 573); - OrElseShared::tick (line
        // 825), where last_f survives f's bottom, so or_else answers a null input with
        // f's value from before the bottom. The opt HOFs also ride a bottomed input
        // instead of bottoming (dense delta 13), and core::filter (lib.rs:1717) reads
        // its predicate the same way. Produce the bottom only where it is consumed:
        // tick_unary must not bottom on the callback while the current input is null.
        // probe: design/review-2026-10-05/repro/core-aux-08.gx (core-aux-08)
        match seam_tick(self.pred.update(ctx)).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.pred.typecheck0(ctx)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    // CR claude for eric: [bug] A deleted wrapper site leaves its QueueEntry values in
    // the shared queue. A later #trigger pops one, set_var writes binds nobody reads
    // (the value stays in the store), f never runs, the release is lost, and #count
    // keeps counting the dead entry. When array::map shrinks while removed slots hold
    // queued calls, the live calls need extra triggers, and with the feedback idiom
    // (output drives #trigger) the queue stalls for good. Tag each entry with its site,
    // drop this site's entries here and write the new depth through #count. This delete
    // also never removes arg_bids from the store. probe:
    // design/review-2026-10-05/repro/core-aux-05.gx (core-aux-05)
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.pred.sleep(ctx);
    }
}

/// The `queuefn` builtin. Constructs a wrapper LambdaDef the first time `f`
/// is known, emits it as the call site's value, and on `#trigger` updates
/// pops queued invocations.
#[derive(Debug)]
pub(crate) struct QueueFn<R: Rt, E: UserEvent> {
    state: StateRef,
    /// BindId holding the most recent value of `f`. The wrapper LambdaDef's
    /// preds reference this so they always call the current `f`.
    fid: BindId,
    /// Resolved fn type of `f`. Filled in during the CallSite typecheck
    /// phase.
    ftyp: Option<Arc<FnType>>,
    /// The wrapped LambdaDef value, emitted as the queuefn call site's
    /// output. Built lazily once `ftyp` and `f`'s value are known.
    lambda: Option<Value>,
    top_id: ExprId,
    scope: Scope,
    out: TagValue,
    /// `fn() ->` makes the PhantomData unconditionally Send + Sync.
    _phantom: PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for QueueFn<R, E> {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let pop_count = i64::decode(buf)?;
        let count_ref = Pack::decode(buf)?;
        let last_written_depth = i64::decode(buf)?;
        let fid = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let ftyp = Option::<FnType>::decode(buf)?.map(Arc::new);
        let scope = image::scope_decode(buf)?;
        ctx.record_ref(fid, top_id);
        let state = Arc::new(Mutex::new(QueueState {
            queue: VecDeque::new(),
            pop_count,
            count_ref,
            last_written_depth,
        }));
        Ok(Box::new(Self {
            state,
            fid,
            ftyp,
            lambda: None,
            top_id,
            scope,
            out: TagValue::phantom(),
            _phantom: PhantomData,
        }))
    }

    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "core_queuefn";
    const ORDERED: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 3 {
            bail!("queuefn: expected three arguments (#count, #trigger, f)")
        }
        let fid = BindId::new();
        ctx.record_ref(fid, top_id);
        let ftyp = resolved.and_then(|r| extract_fn_arg_type(&ctx.env, r, 2));
        Ok(Box::new(Self {
            state: Arc::new(Mutex::new(QueueState::new())),
            fid,
            ftyp,
            lambda: None,
            top_id,
            scope: scope.clone(),
            out: TagValue::phantom(),
            _phantom: PhantomData,
        }))
    }
}

impl<R: Rt, E: UserEvent> QueueFn<R, E> {
    fn build_lambda(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Result<Value> {
        let ftyp = self
            .ftyp
            .clone()
            .ok_or_else(|| anyhow::anyhow!("queuefn: fn type not resolved"))?;
        let id = LambdaId::new();
        // CR claude for eric: [bug] The wrapper is built from f's type alone. This
        // argspec makes every labeled formal ArgKind::Labeled, so f's defaults are
        // lost. build_wrapper_apply (line 460) binds only ftyp.args, so
        // WrapperApply::update (line 89) silently drops f's variadic arguments, both on
        // the immediate call and on a queued one. The checker types the wrapper exactly
        // as f, so these calls pass --check. Wrapping `|#scale: i64 = 10, x: i64|`,
        // `qf(5)` never produces (the only trace is an ERROR "expected default value"
        // in the log). A wrapped `max` answers `qm(1, 5)` with 1, a wrapped array::push
        // drops the pushed values, and a wrapped str::concat never produces, all with
        // nothing logged. probe: design/review-2026-10-05/repro/core-aux-06.gx
        // (core-aux-06)
        let argspec: Arc<[Arg]> = ftyp
            .args
            .iter()
            .enumerate()
            .map(|(i, a)| {
                let name: ArcStr = match a.label() {
                    Some(n) => n.clone(),
                    None => format_compact!("a{i}").as_str().into(),
                };
                Arg {
                    kind: match a.is_labeled() {
                        true => ArgKind::Labeled,
                        false => ArgKind::Positional,
                    },
                    pattern: StructurePattern::Bind(name.into()),
                    constraint: Some(a.typ.clone()),
                    pos: WrittenAt::NOWHERE,
                }
            })
            .collect::<Vec<_>>()
            .into();
        let lambda_typ = ftyp.clone();
        let state = self.state.clone();
        let fid = self.fid;
        let init: InitFn<R, E> = SArc::new(
            move |scope: &Scope,
                  ctx: &mut CompileCtx<R, E>,
                  args: &mut [Node<R, E>],
                  _mode: BindMode<'_>,
                  tid: ExprId| {
                build_wrapper_apply(
                    scope,
                    ctx,
                    args,
                    state.clone(),
                    fid,
                    lambda_typ.clone(),
                    tid,
                )
            },
        );
        let env = ctx.env.clone();
        let def = LambdaDef {
            id,
            env,
            scope: self.scope.clone(),
            argspec,
            typ: ftyp,
            init,
            check: Mutex::new(None),
            table: Default::default(),
            intrinsic_effect: Mutex::new(EffectKind::Async),
            stateless: std::sync::atomic::AtomicBool::new(false),
            recursion: Mutex::new(RecursionKind::NotRecursive),
            source: self.top_id,
            origin: graphix_compiler::node::lambda::DefOrigin::Runtime,
            level: 1,
        };
        Ok(ctx.wrap_lambda(def))
    }

    fn maybe_write_count(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let to_write = {
            let mut s = self.state.lock();
            if let Some(bid) = s.count_ref {
                let depth = s.depth();
                if depth != s.last_written_depth {
                    s.last_written_depth = depth;
                    Some((bid, depth))
                } else {
                    None
                }
            } else {
                None
            }
        };
        if let Some((bid, depth)) = to_write {
            ctx.rt.set_var(bid, Value::I64(depth));
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for QueueFn<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let s = self.state.lock();
        // the wrapper lambda and queued invocations exist only once a cycle ran
        if self.lambda.is_some() || !s.queue.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        s.pop_count.encode(buf)?;
        s.count_ref.encode(buf)?;
        s.last_written_depth.encode(buf)?;
        self.fid.encode(buf)?;
        self.top_id.encode(buf)?;
        self.ftyp.as_deref().cloned().encode(buf)?;
        image::scope_encode(&self.scope, buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        // from[0] = #count (a ref, possibly null)
        // from[1] = #trigger
        // from[2] = f
        // CR claude for eric: [bug] #count gets its own set_var for every depth change,
        // from each push (lines 107-121) and each pop, and the runtime delivers only
        // one write to a variable per cycle. So a burst that queues N calls reaches the
        // reader one step per cycle, and the count peaks after the queue has already
        // drained. A cycle that both pops and pushes leaves one more write in the
        // runtime's backlog, which every later cycle walks: 32000 such cycles take 12
        // s, against 1.1 s when push and pop alternate. Under GRAPHIX_PAR=force, 6
        // calls with no pop ended at depth 5, 2 and 1 in three runs. Sleep
        // (line 439) empties the queue without writing 0, and a reference that arrives
        // or moves is never written (line 363), so the target keeps an old depth.
        // queuefn_count_ref (lib_tests/core.rs:444) pins the lag; probe:
        // design/review-2026-10-05/repro/core-aux-10.sh (core-aux-10)
        if let Some(v) = seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned()) {
            let new_ref = match &v {
                Value::U64(b) => {
                    let outer = BindId::from(*b);
                    // CR claude for eric: [bug] This resolves a reference only through
                    // `byref_chain`, but a place reference (`&s.f`, `&a[i]`) is
                    // registered with `Rt::set_ref_path`, so `#count: &st.depth` writes
                    // the place's mirror cell, which no reader goes through, and
                    // `st.depth` stays -1 while `#count: &plain` counts. `resolve_ref`
                    // (buffer.rs:273) has the same gap: buffer::decode(b, [`U8(&st.a)])
                    // returns DecodeError("ref does not point to a let binding").
                    // ConnectDeref::resolve (node/mod.rs:1616) and Deref::address
                    // (node/bind.rs:1388) check `ref_path` first; share one resolver
                    // with them and write a place with `Rt::patch_var`. probe:
                    // design/review-2026-10-05/repro/x-builtin-effects-11.gx
                    // (x-builtin-effects-11)
                    ctx.env.byref_chain.get(&outer).copied().or(Some(outer))
                }
                _ => None,
            };
            let mut s = self.state.lock();
            s.count_ref = new_ref;
            s.last_written_depth = s.depth();
        }
        let mut new_lambda: Option<Value> = None;
        if let Some(tv) = seam_value(from[2].update(ctx)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            // A lazily-built instance never saw typecheck1; the runtime
            // `f` value carries the LambdaDef to derive the type from.
            if self.ftyp.is_none() {
                if let Some(def) = v.downcast_ref::<LambdaDef<R, E>>() {
                    self.ftyp = Some(Arc::new(def.typ.reset_tvars()));
                }
            }
            ctx.rt.store_insert(self.fid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.fid, TagValue::tagged(v, tag));
            if self.lambda.is_none() {
                match self.build_lambda(ctx) {
                    Ok(lv) => {
                        self.lambda = Some(lv.clone());
                        new_lambda = Some(lv);
                    }
                    Err(e) => {
                        return self.out.set(TagValue::fired(graphix_compiler::errf!(
                            "QueueFnErr",
                            "{e}"
                        )));
                    }
                }
            }
        }
        let trigger_fired = seam_tick(from[1].update(ctx)).is_some();
        if trigger_fired {
            let popped = {
                let mut s = self.state.lock();
                match s.queue.pop_front() {
                    Some(entry) => Some(entry),
                    None => {
                        s.pop_count += 1;
                        None
                    }
                }
            };
            if let Some(mut entry) = popped {
                for (bid, v) in entry.updates.drain(..) {
                    ctx.rt.set_var(bid, v);
                }
            }
        }
        self.maybe_write_count(ctx);
        let res = if ctx.event.init { self.lambda.clone() } else { new_lambda };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        if let Some(ft) = extract_fn_arg_type(&ctx.env, resolved, 2) {
            self.ftyp = Some(ft);
        } else {
            bail!("queuefn: third argument must be a function")
        }
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    // CR claude for eric: [bug] A deleted queuefn leaves behind what it built.
    // build_lambda's ctx.wrap_lambda (line 305) puts the wrapper def, which holds a
    // clone of the env, in ctx.lambda_defs, and nothing removes it (Lambda::delete
    // removes its own def). fid's store entry (line 376, holding f) is never
    // store_removed. WrapperApply::delete (line 146) never unbinds the qa{i} binds
    // genn::bind made (line 461) and never removes their store entries (line 101), both
    // of which genn::SynthCall::delete does. A queuefn made per collection slot,
    // activation or dynamic call grows memory without bound, about 20 KB per instance.
    // probe: design/review-2026-10-05/repro/core-aux-03.gx (RSS 325 MB to 2.9 GB in 18
    // s; flat with a plain lambda in its place). (core-aux-03)
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.fid, self.top_id);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        let mut s = self.state.lock();
        s.queue.clear();
        s.pop_count = 1;
        s.last_written_depth = 0;
    }
}

fn build_wrapper_apply<R: Rt, E: UserEvent>(
    scope: &Scope,
    ctx: &mut CompileCtx<R, E>,
    _args: &mut [Node<R, E>],
    state: StateRef,
    fid: BindId,
    ftyp: Arc<FnType>,
    tid: ExprId,
) -> Result<Box<dyn Apply<R, E>>> {
    let scope = scope.append(&format_compact!("qfn{}", LambdaId::new().inner()));
    let mut arg_bids: Vec<BindId> = Vec::with_capacity(ftyp.args.len());
    let mut arg_nodes: smallvec::SmallVec<[Node<R, E>; 2]> =
        smallvec::SmallVec::with_capacity(ftyp.args.len());
    for (i, a) in ftyp.args.iter().enumerate() {
        let (id, n) = genn::bind(
            ctx,
            &scope.lexical,
            &format_compact!("qa{i}"),
            a.typ.clone(),
            tid,
        );
        arg_bids.push(id);
        arg_nodes.push(n);
    }
    let fnode = genn::reference(ctx, fid, Type::Fn(ftyp.clone()), tid);
    let pred = genn::apply(fnode, scope, arg_nodes, &ftyp, tid);
    Ok(Box::new(WrapperApply {
        state,
        arg_bids: arg_bids.into(),
        pred,
        typ: ftyp,
        out: TagValue::phantom(),
    }))
}

/// The function type `ft.args[idx]` holds, through bindings and type
/// references: the signature's `Function` bound promises one.
fn extract_fn_arg_type(env: &Env, ft: &FnType, idx: usize) -> Option<Arc<FnType>> {
    let mut typ = ft.args.get(idx)?.typ.with_deref(|t| t.cloned())?;
    loop {
        typ = match &typ {
            Type::Fn(ft) => return Some(ft.clone()),
            Type::Ref(_) => typ.lookup_ref(env).ok()?.with_deref(|t| t.cloned())?,
            _ => return None,
        }
    }
}
