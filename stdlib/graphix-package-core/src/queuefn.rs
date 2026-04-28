use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::SourcePosition;
use graphix_compiler::{
    expr::{Arg, ExprId, StructurePattern},
    node::{genn, lambda::LambdaDef},
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, InitFn, LambdaId, Node, Refs, Rt, Scope,
    TypecheckPhase, UserEvent,
};
use netidx::subscriber::Value;
use parking_lot::Mutex;
use smallvec::SmallVec;
use std::{collections::VecDeque, fmt::Debug, marker::PhantomData, sync::Arc as SArc};
use triomphe::Arc;

// CR estokes: LPool these smallvecs, functions often have an unpredictable
// number of arguments and the top end is more than we'd want to SmallVec
#[derive(Debug)]
struct QueueEntry {
    /// Per-call-site arg bindings. When this entry is dispatched, the queuefn
    /// node writes the args into these bindings via `ctx.rt.set_var`, which
    /// causes the call site's `pred` to fire on the next cycle.
    arg_bids: SmallVec<[BindId; 4]>,
    args: SmallVec<[Value; 4]>,
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

// CR estokes: Why an SArc, do we need a weak reference to this?
type StateRef = SArc<Mutex<QueueState>>;

// CR estokes: SmallVec -> LPooled<Vec>
/// Per-call-site Apply impl for the wrapper lambda. Each invocation of the
/// wrapper at a user call site goes through this. Push/pop coordination is
/// done via `state` shared with the owning `QueueFn` node.
#[derive(Debug)]
struct WrapperApply<R: Rt, E: UserEvent> {
    state: StateRef,
    /// One bind per fn arg, owned by this call site. `pred` references these
    /// to read the args at invocation time.
    arg_bids: SmallVec<[BindId; 4]>,
    /// Compiled call to `f` using `arg_bids` as inputs.
    pred: Node<R, E>,
    /// Most-recently-seen value of each user-supplied arg expression. Used
    /// to assemble a complete arg tuple even when args fire in different
    /// cycles.
    // CR estokes: My instinct is that queuefn shouldn't change the semantics vs
    // callsite, and caching the args and waiting to update until we have them
    // all is a fundamental change in semantics. Moreover it creates complex
    // semantics for the user to know, because once we've accumulated a copy of
    // all the args their function will be presented with spurious updates for
    // args that didn't update, and depending on the semantics of the function
    // they are wrapping that might matter a lot.
    //
    // consider for example queueing a function with a #trigger or #clock
    // argument, it will totally break.
    //
    // So IMHO the goal should be that the function we are queueing should not
    // be able to observe that it's being queued, the timing and structure of the
    // arguments we pass should be exactly the same as if it wasn't queued, except
    // for the fact that arguments are only released on queuefn's trigger.
    //
    // We should add a function with a trigger or a clock to the queuefn test
    // suite and ensure it behaves correctly.
    cached_args: SmallVec<[Option<Value>; 4]>,
    typ: Arc<FnType>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WrapperApply<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut any_arg_fired = false;
        for (i, n) in from.iter_mut().enumerate() {
            if let Some(v) = n.update(ctx, event) {
                if i < self.cached_args.len() {
                    self.cached_args[i] = Some(v);
                }
                any_arg_fired = true;
            }
        }
        if any_arg_fired && self.cached_args.iter().all(|s| s.is_some()) {
            let count_write = {
                let mut s = self.state.lock();
                if s.pop_count > 0 {
                    s.pop_count -= 1;
                    drop(s);
                    for (bid, slot) in self.arg_bids.iter().zip(self.cached_args.iter()) {
                        if let Some(v) = slot {
                            ctx.cached.insert(*bid, v.clone());
                            event.variables.insert(*bid, v.clone());
                        }
                    }
                    None
                } else {
                    let args: SmallVec<[Value; 4]> =
                        self.cached_args.iter().filter_map(|s| s.clone()).collect();
                    // CR estokes: Can arg_bids ever change? If so probably only
                    // rarely, good candidate for an Arc
                    s.queue
                        .push_back(QueueEntry { arg_bids: self.arg_bids.clone(), args });
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
        for slot in self.cached_args.iter_mut() {
            *slot = None;
        }
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
            state: SArc::new(Mutex::new(QueueState::new())),
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
                let name: ArcStr = match a.label.as_ref() {
                    Some((n, _)) => n.clone(),
                    None => format_compact!("a{i}").as_str().into(),
                };
                Arg {
                    labeled: a.label.as_ref().map(|_| None),
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
            if let Some(entry) = popped {
                for (bid, v) in entry.arg_bids.iter().zip(entry.args.into_iter()) {
                    ctx.rt.set_var(*bid, v);
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
    args: &mut [Node<R, E>],
    state: StateRef,
    fid: BindId,
    ftyp: Arc<FnType>,
    tid: ExprId,
) -> Result<Box<dyn Apply<R, E>>> {
    let scope = scope.append(&format_compact!("qfn{}", LambdaId::new().inner()));
    let mut arg_bids: SmallVec<[BindId; 4]> = SmallVec::new();
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
    let cached_args: SmallVec<[Option<Value>; 4]> = args.iter().map(|_| None).collect();
    Ok(Box::new(WrapperApply { state, arg_bids, pred, cached_args, typ: ftyp }))
}

/// Extract the FnType from `ft.args[idx]`, expanding refs if needed.
fn extract_fn_arg_type(ft: &FnType, idx: usize) -> Option<Arc<FnType>> {
    let typ = ft.args.get(idx)?;
    match &typ.typ {
        Type::Fn(ft) => Some(ft.clone()),
        _ => None,
    }
}
