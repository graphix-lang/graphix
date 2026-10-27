use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BindId, BindMode, BuiltIn, CompileCtx, Effect, ExecCtx, InitFn, LambdaId,
    Node, Refs, Rt, Scope, Tag, TagValue, TagView, UserEvent, WriteTarget,
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
use smallvec::SmallVec;
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
    /// A credit is spent on it: it waits only for its site's release in
    /// flight to land.
    paid: bool,
}

#[derive(Debug, Default)]
struct QueueState {
    queue: VecDeque<QueueEntry>,
    /// The sites asleep: their calls keep their place and none is released
    /// until the site wakes.
    asleep: SmallVec<[SiteId; 2]>,
    /// The sites whose last release has not landed yet. A site releases
    /// one call a cycle, since writes to its argument bindings made in one
    /// cycle land together and would merge two calls into one.
    in_flight: SmallVec<[SiteId; 2]>,
    pop_count: i64,
    /// The writable `#count` reference's cell and where a write through it
    /// lands, or None if not provided.
    count_ref: Option<(BindId, WriteTarget)>,
    /// Last value written through `count_ref` to avoid redundant writes.
    last_written_depth: i64,
}

impl QueueState {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            asleep: SmallVec::new(),
            in_flight: SmallVec::new(),
            pop_count: 1,
            count_ref: None,
            last_written_depth: 0,
        }
    }

    fn depth(&self) -> i64 {
        self.queue.len() as i64
    }

    /// A trigger's fire: the oldest unpaid call of a site awake, released
    /// now unless its site has a release in flight, when it is paid and
    /// waits; with none, a banked credit.
    fn trigger(&mut self) -> Option<QueueEntry> {
        let asleep = &self.asleep;
        let Some(i) =
            self.queue.iter().position(|e| !e.paid && !asleep.contains(&e.site))
        else {
            self.pop_count += 1;
            return None;
        };
        let site = self.queue[i].site;
        if self.in_flight.contains(&site) {
            self.queue[i].paid = true;
            return None;
        }
        self.in_flight.push(site);
        self.queue.remove(i)
    }

    /// `site` wakes: the credit banked pays its calls, oldest first, and
    /// what it released before its sleep has landed. True if it slept.
    fn wake(&mut self, site: SiteId) -> bool {
        let Some(i) = self.asleep.iter().position(|s| *s == site) else {
            return false;
        };
        self.asleep.swap_remove(i);
        self.landed(site);
        for e in self.queue.iter_mut().filter(|e| e.site == site && !e.paid) {
            if self.pop_count == 0 {
                break;
            }
            self.pop_count -= 1;
            e.paid = true;
        }
        true
    }

    fn landed(&mut self, site: SiteId) {
        self.in_flight.retain(|s| *s != site)
    }

    /// The oldest paid call of `site`, unless a release is in flight.
    fn release(&mut self, site: SiteId) -> Option<QueueEntry> {
        if self.in_flight.contains(&site) {
            return None;
        }
        let i = self.queue.iter().position(|e| e.site == site && e.paid)?;
        self.in_flight.push(site);
        self.queue.remove(i)
    }

    /// Forget `site`: its calls, its sleep and its release in flight.
    fn drop_site(&mut self, site: SiteId) {
        self.queue.retain(|e| e.site != site);
        self.asleep.retain(|s| *s != site);
        self.landed(site);
    }

    /// The `#count` write the depth needs, if it changed.
    fn count_write(&mut self) -> Option<(BindId, i64)> {
        let bid = self.count_ref.as_ref()?.0;
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
            WriteTarget::Place(root, path) => ctx.rt.patch_level(root, path, v),
            WriteTarget::Bind(target) => ctx.rt.set_level(target, v),
        }
    }
}

fn release<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<'_, R, E>, e: Option<QueueEntry>) {
    if let Some(mut e) = e {
        for (bid, v) in e.updates.drain(..) {
            ctx.rt.set_var(bid, v);
        }
    }
}

type StateRef = Arc<Mutex<QueueState>>;

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
        // a site's paid calls are released one a cycle, each once the one
        // before lands; at a wake the credit banked while it slept pays its
        // calls, and a call it makes meanwhile queues behind them
        // XCR claude for claude: [bug/medium] At a wake every paid entry is set_var'd in
        // this one cycle. Writes to different bindings land together, so two queued
        // calls whose fired arguments are disjoint reach f as one call, and with
        // overlapping deltas a later call's argument arrives early. A call made in this
        // cycle with leftover credit lands with them too (released = woke_any). The
        // wrapped function can see the queueing, against design/queue_fn.md. Release
        // the woken entries one per cycle, e.g. keep them queued as paid and have the
        // site set_var a private tick binding it reads, popping one per update. probe:
        // design/review-2026-10-26/repro/core-aux-queuefn-wake-merges-calls.gx
        // (core-aux-01)
        // 2026-10-26 claude: WrapperApply releases one call a cycle per site:
        // QueueState::in_flight holds a site whose release has not landed, a wake marks
        // the calls its banked credit covers paid, the site releases the next paid one
        // when the last lands, and a trigger or credit meeting a release in flight
        // queues the call paid. Pin:
        // lib_tests::core::queuefn_wake_releases_one_call_a_cycle (fails with the fix
        // undone: the queued calls merge).
        // XCR claude for claude: [bug/medium] The wake releases every queued
        // call the credit covers in one cycle. Each entry writes only the
        // arguments that fired for it, and the runtime queues writes per
        // variable, so the first release lands with arguments of later
        // ones. A call made in the wake cycle (the `woke_any` path) mixes
        // the same way. With q(1, 100) and q(2, 200) queued across a sleep,
        // the function sees (1, 200), which was never called, then (2,
        // 200). Fix: at the wake, release one entry and release the next
        // when its binds land, keeping later calls behind, so each delta
        // lands alone. probe:
        // design/review-2026-10-26/repro/x-async-io-queuefn-wake-mixes-calls.gx
        // (x-async-io-03)
        // 2026-10-26 claude: Fixed with core-aux-01: one release a cycle per
        // site (QueueState::in_flight, paid entries); a call made in the wake
        // cycle queues paid behind the released ones. Pin:
        // lib_tests::core::queuefn_wake_releases_one_call_a_cycle; the repro
        // prints (1, 100) then (2, 200).
        let landed = self.call.args().iter().any(|b| ctx.event.variables.contains_key(b));
        let (released, count_write) = {
            let mut s = self.state.lock();
            if !s.wake(self.site) && landed {
                s.landed(self.site)
            }
            let e = s.release(self.site);
            (e, s.count_write())
        };
        write_count(ctx, count_write);
        release(ctx, released);
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
            let mut s = self.state.lock();
            let paid = s.pop_count > 0;
            if paid {
                s.pop_count -= 1;
            }
            if paid && !s.in_flight.contains(&self.site) {
                // a release landing in these binds this cycle goes first, and
                // this call lands next cycle
                if landed {
                    s.in_flight.push(self.site);
                }
                drop(s);
                for (bid, v) in delta.drain(..) {
                    match landed {
                        true => ctx.rt.set_var(bid, v),
                        false => ctx.deliver(bid, TagValue::fired(v)),
                    }
                }
            } else {
                s.queue.push_back(QueueEntry { site: self.site, updates: delta, paid });
                let w = s.count_write();
                drop(s);
                write_count(ctx, w);
            }
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
        self.forget(ctx);
    }

    /// A pause: the site's calls keep their place, released once it wakes.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.sleep(ctx);
        let mut s = self.state.lock();
        if !s.asleep.contains(&self.site) {
            s.asleep.push(self.site)
        }
    }

    /// Born again, the site makes its calls afresh.
    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.forget(ctx);
    }
}

impl<R: Rt, E: UserEvent> WrapperApply<R, E> {
    fn forget(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let count_write = {
            let mut s = self.state.lock();
            s.drop_site(self.site);
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
        let count_ref = Option::<BindId>::decode(buf)?.map(|c| (c, ctx.write_target(c)));
        let last_written_depth = i64::decode(buf)?;
        let fid = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let ftyp = Option::<FnType>::decode(buf)?.map(Arc::new);
        let scope = image::scope_decode(buf)?;
        ctx.record_ref(fid, top_id);
        let state = Arc::new(Mutex::new(QueueState {
            queue: VecDeque::new(),
            asleep: SmallVec::new(),
            in_flight: SmallVec::new(),
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
        s.count_ref.as_ref().map(|(c, _)| *c).encode(buf)?;
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
            let new_ref =
                graphix_compiler::reference::cell(&v).map(|c| (c, ctx.write_target(c)));
            let mut s = self.state.lock();
            // XCR claude for claude: [bug/low] Only the cell is compared, and a moving
            // place (`&mut arr[i]`) keeps its cell and changes only its path. When i
            // moves, the new place is not told the depth until the depth next changes,
            // contradicting the comment below. Keep the resolved WriteTarget
            // (ctx.write_target) instead of the cell and reset last_written_depth when
            // it changes. probe:
            // design/review-2026-10-26/repro/core-aux-queuefn-count-move.gx
            // (core-aux-04)
            // 2026-10-26 claude: QueueState::count_ref keeps the cell and its resolved
            // WriteTarget; a target that changes (a moving place) resets
            // last_written_depth so the new place is told the depth. Pin:
            // lib_tests::core::queuefn_count_follows_a_moving_place (prints [2, -1]
            // with the fix undone).
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
        // a fire born with the queuefn is no credit
        let trigger_fired = seam_tick(from[1].update(ctx)).is_some() && !ctx.event.born();
        if trigger_fired {
            let popped = self.state.lock().trigger();
            release(ctx, popped);
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

    /// A pause: the queued calls and the credit stay.
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let w = {
            let mut s = self.state.lock();
            s.queue.clear();
            s.asleep.clear();
            s.in_flight.clear();
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
