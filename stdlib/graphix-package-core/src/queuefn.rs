use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::SourcePosition;
use graphix_compiler::{
    effects::EffectKind,
    expr::{Arg, ExprId, StructurePattern},
    node::{genn, lambda::LambdaDef},
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, InitFn, LambdaId, Node, Refs, Rt, Scope,
    TypecheckPhase, UserEvent,
};
use netidx::subscriber::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{collections::VecDeque, fmt::Debug, marker::PhantomData, sync::Arc as SArc};
use triomphe::Arc;

#[derive(Debug)]
struct QueueEntry {
    /// The (BindId, Value) pairs for the args that fired in the originating
    /// cycle. On dispatch, only these bids are written via `ctx.rt.set_var`;
    /// args that did not fire then are not re-fired now. This matches normal
    /// call site semantics where pred sees only the args that actually
    /// updated this cycle.
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
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WrapperApply<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut delta: LPooled<Vec<(BindId, Value)>> = LPooled::take();
        for (i, n) in from.iter_mut().enumerate() {
            if let Some(v) = n.update(ctx, event) {
                if let Some(bid) = self.arg_bids.get(i) {
                    delta.push((*bid, v));
                }
            }
        }
        if !delta.is_empty() {
            let count_write = {
                let mut s = self.state.lock();
                if s.pop_count > 0 {
                    s.pop_count -= 1;
                    drop(s);
                    for (bid, v) in delta.drain(..) {
                        ctx.cached.insert(bid, v.clone());
                        event.variables.insert(bid, v);
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
        self.pred.update(ctx, event)
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        self.pred.typecheck(ctx)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
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
    /// `fn() ->` makes the PhantomData unconditionally Send + Sync, which we
    /// need because the trait bound on Apply doesn't propagate to R/E.
    _phantom: PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for QueueFn<R, E> {
    const NAME: &str = "core_queuefn";
    const NEEDS_CALLSITE: bool = true;
    const EFFECT: EffectKind = EffectKind::Async;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
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
        ctx.rt.ref_var(fid, top_id);
        let ftyp = resolved.and_then(|r| extract_fn_arg_type(r, 2));
        Ok(Box::new(Self {
            state: Arc::new(Mutex::new(QueueState::new())),
            fid,
            ftyp,
            lambda: None,
            top_id,
            scope: scope.clone(),
            _phantom: PhantomData,
        }))
    }
}

impl<R: Rt, E: UserEvent> QueueFn<R, E> {
    fn build_lambda(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Value> {
        let ftyp = self
            .ftyp
            .clone()
            .ok_or_else(|| anyhow::anyhow!("queuefn: fn type not resolved"))?;
        let id = LambdaId::new();
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
                    labeled: a.is_labeled().then_some(None),
                    pattern: StructurePattern::Bind(name),
                    constraint: Some(a.typ.clone()),
                    pos: SourcePosition::default(),
                }
            })
            .collect::<Vec<_>>()
            .into();
        let lambda_typ = ftyp.clone();
        let state = self.state.clone();
        let fid = self.fid;
        let init: InitFn<R, E> = SArc::new(
            move |scope: &Scope,
                  ctx: &mut ExecCtx<R, E>,
                  args: &mut [Node<R, E>],
                  _resolved: Option<&FnType>,
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
            needs_callsite: false,
            check: Mutex::new(None),
            // queuefn wraps a function in queue-based dispatch — each
            // call queues the predicate and dispatches across cycles.
            // The wrapper lambda is intrinsically async.
            intrinsic_effect: Mutex::new(EffectKind::Async),
        };
        Ok(ctx.wrap_lambda(def))
    }

    fn maybe_write_count(&mut self, ctx: &mut ExecCtx<R, E>) {
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        // from[0] = #count (a ref, possibly null)
        // from[1] = #trigger
        // from[2] = f
        if let Some(v) = from[0].update(ctx, event) {
            let new_ref = match &v {
                Value::U64(b) => {
                    let outer = BindId::from(*b);
                    ctx.env.byref_chain.get(&outer).copied().or(Some(outer))
                }
                _ => None,
            };
            let mut s = self.state.lock();
            s.count_ref = new_ref;
            s.last_written_depth = s.depth();
        }
        let mut new_lambda: Option<Value> = None;
        if let Some(v) = from[2].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
            if self.lambda.is_none() {
                match self.build_lambda(ctx) {
                    Ok(lv) => {
                        self.lambda = Some(lv.clone());
                        new_lambda = Some(lv);
                    }
                    Err(e) => {
                        return Some(graphix_compiler::errf!("QueueFnErr", "{e}"));
                    }
                }
            }
        }
        let trigger_fired = from[1].update(ctx, event).is_some();
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
        if event.init {
            self.lambda.clone()
        } else {
            new_lambda
        }
    }

    fn typecheck(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        if let TypecheckPhase::CallSite(resolved) = phase {
            if let Some(ft) = extract_fn_arg_type(resolved, 2) {
                self.ftyp = Some(ft);
            } else {
                bail!("queuefn: third argument must be a function")
            }
        }
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.fid, self.top_id);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        let mut s = self.state.lock();
        s.queue.clear();
        s.pop_count = 1;
        s.last_written_depth = 0;
    }
}

fn build_wrapper_apply<R: Rt, E: UserEvent>(
    scope: &Scope,
    ctx: &mut ExecCtx<R, E>,
    _args: &mut [Node<R, E>],
    state: StateRef,
    fid: BindId,
    ftyp: Arc<FnType>,
    tid: ExprId,
) -> Result<Box<dyn Apply<R, E>>> {
    let scope = scope.append(&format_compact!("qfn{}", LambdaId::new().inner()));
    let mut arg_bids: Vec<BindId> = Vec::with_capacity(ftyp.args.len());
    let mut arg_nodes: Vec<Node<R, E>> = Vec::with_capacity(ftyp.args.len());
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
    Ok(Box::new(WrapperApply { state, arg_bids: arg_bids.into(), pred, typ: ftyp }))
}

/// Extract the FnType from `ft.args[idx]`, expanding refs if needed.
fn extract_fn_arg_type(ft: &FnType, idx: usize) -> Option<Arc<FnType>> {
    let typ = ft.args.get(idx)?;
    match &typ.typ {
        Type::Fn(ft) => Some(ft.clone()),
        _ => None,
    }
}
