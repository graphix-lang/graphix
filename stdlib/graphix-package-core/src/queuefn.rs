use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BindId, BindMode, BuiltIn, CompileCtx, Effect, ExecCtx, InitFn, LambdaId,
    Node, Refs, Rt, Scope, Tag, TagValue, TagView, UserEvent,
    effects::{LambdaFacts, RecursionKind},
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

use crate::{bottom_of, seam_tick, seam_value};

use serde_derive::{Deserialize, Serialize};

netidx_core::atomic_id!(SiteId);

#[derive(Debug)]
struct QueueEntry {
    /// the call site that queued it
    site: SiteId,
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

    /// The `#count` write the depth needs, if it changed.
    fn count_write(&mut self) -> Option<(BindId, i64)> {
        let bid = self.count_ref?;
        let depth = self.depth();
        (depth != self.last_written_depth).then(|| {
            self.last_written_depth = depth;
            (bid, depth)
        })
    }
}

fn write_count<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    w: Option<(BindId, i64)>,
) {
    if let Some((r, depth)) = w {
        let v = Value::I64(depth);
        match ctx.write_target(r) {
            graphix_compiler::WriteTarget::Place(root, path) => {
                ctx.rt.patch_level(root, path, v)
            }
            graphix_compiler::WriteTarget::Bind(target) => ctx.rt.set_level(target, v),
        }
    }
}

type StateRef = Arc<Mutex<QueueState>>;

// CR claude for claude: [bug/medium] A callback that accumulates into an outer
// variable, `|x| { got <- x ~ array::push(got, x); x }`, loses its first write when
// the wrapper releases one call per tick: `got` reads [1], then [2], then [2, 3],
// where each release should see the write the previous one made. Found while pinning
// c-data-map-02; it reproduces at 9445a1d21, pruned or not, so it is no regression of
// the 10-19 fixes. Find whether the second release's `~` samples `got` before the
// first write lands or the instance reads it stale, and pin it in lang::queuefn.
// probe: design/review-2026-10-19/repro/queuefn-stale-accumulator.gx
// (queuefn-stale-read-01)
// 2026-10-19 claude: Neither: no read is stale. The trigger clk fires at birth (its
// initial 0), and the queuefn node updates before any wrapper site (the sites need
// its lambda), so that fire finds the queue empty and banks a second credit
// (pop_count 2) beside the initial one. q(1) and q(2) then both dispatch in cycle 0,
// each samples got = [], and their two writes land in turn: [1], then [2]; q(3),
// released at the next tick, reads [2]. With #trigger: skip(#n: 1, clk) the probe
// gives [1], [1, 2], [1, 2, 3] (/home/eric/tmp/cr-fix3/wt-stdlib/qacc2.gx). So the
// question is a rule: may a credit a trigger banks in a cycle release a call made in
// that same cycle (statement order puts the node first), and is a trigger's birth
// fire a credit at all? queuefn_trigger_before_fn relies on same-cycle banking (trigs
// and xs both deliver one a cycle). Two credits mean two calls in flight, which an
// accumulator into an outer variable cannot survive. Needs a ruling before a pin.
// 2026-10-24 claude: Eric ruled (2026-10-24): keep a site's queued calls across sleep
// (pause, like queue), and a trigger's birth fire is not a credit: only fires after
// the queuefn exists release calls.
/// Per-call-site Apply impl for the wrapper lambda. Each invocation of the
/// wrapper at a user call site goes through this. Push/pop coordination is
/// done via `state` shared with the owning `QueueFn` node.
#[derive(Debug)]
struct WrapperApply<R: Rt, E: UserEvent> {
    site: SiteId,
    state: StateRef,
    /// The call of `f`, through the queuefn's binding of it, over one
    /// binding per argument of this site's (`from[i]` feeds the `i`th).
    call: genn::Callback<R, E>,
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
        let mut tags: LPooled<Vec<Tag>> = LPooled::take();
        for (i, n) in from.iter_mut().enumerate() {
            let tv = n.update(ctx);
            tags.push(tv.tag());
            if let Some(v) = seam_tick(tv) {
                if let Some(bid) = self.call.args().get(i) {
                    delta.push((*bid, v.value_cloned()));
                }
            }
        }
        // a call with a bottom argument is neither queued nor dispatched;
        // an answer to a call queued earlier still delivers
        let bottom = bottom_of(tags.drain(..));
        if bottom.is_some() {
            delta.clear();
        }
        if !delta.is_empty() {
            let count_write = {
                let mut s = self.state.lock();
                if s.pop_count > 0 {
                    s.pop_count -= 1;
                    drop(s);
                    // a release delivering to these binds this cycle goes first;
                    // this call lands next cycle
                    let released = self
                        .call
                        .args()
                        .iter()
                        .any(|b| ctx.event.variables.contains_key(b));
                    for (bid, v) in delta.drain(..) {
                        if released {
                            ctx.rt.set_var(bid, v);
                        } else {
                            ctx.deliver(bid, TagValue::fired(v));
                        }
                    }
                    None
                } else {
                    s.queue.push_back(QueueEntry { site: self.site, updates: delta });
                    s.count_write()
                }
            };
            write_count(ctx, count_write);
        }
        match self.call.update(ctx).view() {
            TagView::Fired(v) => self.out.set(TagValue::fired(v.value_cloned())),
            _ if let Some(bottom) = bottom => bottom,
            TagView::Stale(_) => self.out.ride(),
            // the wrapped call's bottom is the wrapper's
            TagView::FreshBottom => self.out.set_bottom(true),
            TagView::StaleBottom => self.out.set_bottom(false),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.call.typecheck0(ctx)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.call.refs(refs)
    }

    /// The site's queued calls go with it, and so do its bindings.
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.delete(ctx);
        let count_write = {
            let mut s = self.state.lock();
            s.queue.retain(|e| e.site != self.site);
            s.count_write()
        };
        write_count(ctx, count_write);
    }

    /// The site's queued calls go with its sleep: a release to a sleeping
    /// site would run nothing.
    // CR claude for claude: [bug/medium] Dropping the site's queued calls at sleep loses
    // calls the program made just because the caller's arm paused: the later triggers
    // only bank credit. Under the rule that a wake resumes and never rebirths, a pause
    // should keep them. The doc's premise that a release to a sleeping site runs
    // nothing no longer holds either, since wake catch-up re-raises a binding written
    // while asleep. Decide whether a pop may target a sleeping site (catch-up delivers
    // its last release) or skips it until the wake, then keep the entries here and
    // update queuefn_sleeping_site_drops_its_calls. probe:
    // design/review-2026-10-19/repro/core-aux-queuefn-site-sleep-drops.gx (core-aux-02)
    // 2026-10-24 claude: Eric ruled (2026-10-24): keep a site's queued calls across
    // sleep (pause, like queue), and a trigger's birth fire is not a credit: only fires
    // after the queuefn exists release calls.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.sleep(ctx);
        let count_write = {
            let mut s = self.state.lock();
            s.queue.retain(|e| e.site != self.site);
            s.count_write()
        };
        write_count(ctx, count_write);
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
            table: Default::default(),
            shared: Default::default(),
            param_calls: Default::default(),
            facts: Mutex::new(LambdaFacts::ASYNC),
            recursion: Mutex::new(RecursionKind::NotRecursive),
            no_loop: Default::default(),
            source: self.top_id,
            origin: graphix_compiler::node::lambda::DefOrigin::Runtime,
            level: graphix_compiler::typ::tvar::Level::Def { depth: 1, owner: id },
        };
        Ok(ctx.wrap_lambda(def))
    }

    fn maybe_write_count(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let w = self.state.lock().count_write();
        write_count(ctx, w);
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
        if let Some(v) = seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned()) {
            let new_ref = graphix_compiler::reference::cell(&v);
            let mut s = self.state.lock();
            if s.count_ref != new_ref {
                // a reference that arrives or moves is told the depth
                s.count_ref = new_ref;
                s.last_written_depth = -1;
            }
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
            ctx.deliver(self.fid, TagValue::tagged(v, tag));
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
        let res = if ctx.event.born() { self.lambda.clone() } else { new_lambda };
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
        let Some(ft) = extract_fn_arg_type(&ctx.env, resolved, 2) else {
            bail!("queuefn: third argument must be a function")
        };
        self.ftyp = Some(ft);
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.fid, self.top_id);
        if let Some(def) =
            self.lambda.as_ref().and_then(|l| l.downcast_ref::<LambdaDef<R, E>>())
        {
            ctx.lambda_defs.remove(&def.id);
        }
    }

    // CR claude for claude: [bug/medium] This sleep is still the old arm-rewake restart:
    // it clears the shared queue and resets pop_count to 1, and WrapperApply::sleep
    // drops its site's queued calls. A call queued before the sleep never answers, and
    // a #trigger credit banked before it is lost. The wake-is-a-resume rule (CLAUDE.md;
    // design/async_sleep_outputs.md: a queue keeps its contents and credits) keeps
    // both, as core `queue` and `iterq` do. design/queue_fn.md still documents the
    // restart. Decide whether queuefn pauses like `queue`. If it does, keep the state
    // across sleep, add `reborn` on QueueFn and WrapperApply for a seq step's next run,
    // and update queue_fn.md. If it does not, record the exception in CLAUDE.md and
    // async_sleep_outputs.md. probe:
    // design/review-2026-10-19/repro/x-node-contract-queuefn-sleep-reset.gx
    // (x-node-contract-02)
    // 2026-10-19 claude: The node half is done under core-aux-01 (for claude, now XCR):
    // QueueFn::sleep is a pause and QueueFn::reborn resets queue, credit and #count,
    // design/queue_fn.md and the async_sleep_outputs.md table follow. What remains here
    // is WrapperApply::sleep dropping its site's calls, which is core-aux-02.
    // 2026-10-24 claude: Eric ruled (2026-10-24): keep a site's queued calls across
    // sleep (pause, like queue), and a trigger's birth fire is not a credit: only fires
    // after the queuefn exists release calls.
    /// A pause: the queued calls and the credit stay.
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let w = {
            let mut s = self.state.lock();
            s.queue.clear();
            s.pop_count = 1;
            s.count_write()
        };
        write_count(ctx, w);
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
    let names: LPooled<Vec<CompactString>> =
        (0..ftyp.args.len()).map(|i| format_compact!("qa{i}")).collect();
    let names: LPooled<Vec<&str>> = names.iter().map(|n| n.as_str()).collect();
    let call = genn::Callback::through(ctx, &scope, &ftyp, &names, fid, tid)?;
    Ok(Box::new(WrapperApply {
        site: SiteId::new(),
        state,
        call,
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
