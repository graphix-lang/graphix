use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BindId, BindMode, BuiltIn, CompileCtx, Effect, ExecCtx, InitFn, LambdaId,
    Node, Notify, Refs, Rt, Scope, Tag, TagValue, TagView, WriteTarget,
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
    /// lands (None while its place has no address), or None if not provided.
    count_ref: Option<(BindId, Option<WriteTarget>)>,
    /// Last value written through `count_ref` to avoid redundant writes.
    last_written_depth: i64,
}

impl QueueState {
    fn new() -> Self {
        Self::with(1, None, 0)
    }

    fn with(
        pop_count: i64,
        count_ref: Option<(BindId, Option<WriteTarget>)>,
        last_written_depth: i64,
    ) -> Self {
        Self {
            queue: VecDeque::new(),
            asleep: SmallVec::new(),
            in_flight: SmallVec::new(),
            pop_count,
            count_ref,
            last_written_depth,
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
        // CR claude for claude: [bug/medium] A call released in the cycle before its
        // site sleeps is lost. `release` writes the call's arguments with `set_var`;
        // they land while the site sleeps, and this wake counts the release as landed.
        // The entry has already left the queue and spent its credit, so `f` never runs
        // it and the wrapper never answers. A sleep is a pause: keep the released
        // `QueueEntry` in `in_flight` until its arguments land, and have
        // `WrapperApply::sleep` put one that has not landed back at the front of the
        // queue, paid, as `Queue::sleep` does with `InFlight`. The paid direct
        // `set_var` in `WrapperApply::update` needs the same. `Group` in the array
        // package has the same loss. probe:
        // design/review-2026-11-16/repro/x-node-contract-queuefn-release-lost-at-sleep.gx
        // (x-node-contract-01)
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

fn write_count<R: Rt>(ctx: &mut ExecCtx<'_, R>, w: Option<(BindId, i64)>) {
    if let Some((r, depth)) = w {
        let v = Value::I64(depth);
        match ctx.write_target(r) {
            Some(WriteTarget::Place(root, path)) => ctx.rt.patch_level(root, path, v),
            Some(WriteTarget::Bind(target)) => ctx.rt.set_level(target, v),
            None => (),
        }
    }
}

fn release<R: Rt>(ctx: &mut ExecCtx<'_, R>, e: Option<QueueEntry>) {
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
struct WrapperApply<R: Rt> {
    site: SiteId,
    state: StateRef,
    /// The call of `f`, through the queuefn's binding of it, over one
    /// binding per argument of this site's (`from[i]` feeds the `i`th).
    call: genn::Callback<R>,
    typ: Arc<FnType>,
    out: TagValue,
}

impl<R: Rt> Apply<R> for WrapperApply<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // the wrapper lambda is a runtime definition, built once a cycle ran
        let _ = buf;
        Err(PackError::Application(image::NOT_QUIESCENT))
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        // a site's paid calls are released one a cycle, each once the one
        // before lands; at a wake the credit banked while it slept pays its
        // calls, and a call it makes meanwhile queues behind them
        // CR claude for claude: [bug/high] A wrapper site called from a seq step
        // answers the previous run's call from the second run on: `q2(go)` over go = 1,
        // 10, 20 gives 2, 2, 11. The step's fresh frame (`wake::FreshFrame`) delivers,
        // fired, every binding the call references, and `genn::Callback::refs` names
        // this site's own argument bindings, which still hold the last run's arguments:
        // `landed` reads true, the new arguments are written for the next cycle, and
        // the call fires over the old ones, which completes the step. An unpaid call
        // does the same, and its real release lands in a passed step. Fix: mark the
        // callback's argument bindings bound in `genn::Callback::refs` (as the
        // collection nodes do for a slot's) so no frame replays them, have `reborn`
        // leave them empty, and pin a paid and an unpaid call across two runs. probe:
        // design/review-2026-11-16/repro/x-engine-seq-errors-queuefn-seq-step-stale-answer.gx
        // (x-engine-seq-errors-01)
        let landed = self.call.args().iter().any(|b| ctx.event.variables.contains_key(b));
        let (released, count_write) = {
            let mut s = self.state.lock();
            if !s.wake(self.site) && landed {
                s.landed(self.site)
            }
            // CR claude for claude: [perf/medium] Every wrapper call scans the whole
            // queue in QueueState::release looking for a paid call of its site. A
            // backlog that grows by one call a cycle therefore costs O(n) per call and
            // O(n²) in all, and a queue that backs up is what queuefn is for. A site
            // holds a paid call only while it has a release in flight or has just
            // woken: trigger, wake and the paid push below pay only then. So call
            // release only when wake returned true or a release landed, and the
            // steady-state call is O(1). probe:
            // design/review-2026-11-16/repro/core-aux-queuefn-backlog-quadratic.sh
            // (debug build: 191/390/1104 ms for 10k/20k/40k queued calls, against
            // 115/161/200 ms for the direct call). (core-aux-03)
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
            // CR claude for claude: [bug/high] Only arguments that fire reach the
            // site's bindings, so an argument that stands without firing never reaches
            // f. Cases: a fire dropped beside a bottom argument (q(x, y$) with y null
            // at first never answers when y arrives, while the direct call answers (5,
            // 7)); a constant or capture in a select branch entered later, or in a
            // collection slot added later (array::map(xs, |x| q(x, k)) stops answering
            // when xs grows); and a value a wake moved (f keeps the old k).
            // design/queue_fn.md says the wrapped fn cannot observe the queueing. The
            // direct call seeds a fresh formal with the standing value and stands a
            // moved one (callsite::publish_production, QuietAtRoot::Stand). Do the same
            // here: use ctx.stand for an argument that does not fire, when its binding
            // holds no value and always when the production is moved, and queue only
            // what fires. QueueFn::update already reads f with seam_value for this
            // reason. probe:
            // design/review-2026-11-16/repro/core-aux-queuefn-standing-arg-top-level.gx,
            // plus -standing-arg-never-reaches-f.gx, -standing-arg-new-slot.gx and
            // -moved-arg-not-stood.gx beside it. graphix-fuzz check AGREEs on the wrong
            // answer. (core-aux-02)
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
                        false => ctx.deliver(bid, TagValue::fired(v), Notify::Walk),
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
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
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
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.call.delete(ctx);
        self.forget(ctx);
    }

    /// A pause: the site's calls keep their place, released once it wakes.
    // CR claude for claude: [bug/high] A call released the cycle before this sleep is
    // lost. The release removed it from the queue and wrote its argument binds with
    // set_var; the write lands in the cycle the site is asleep, nothing sees it, and
    // QueueState::wake then clears in_flight as if it had landed, so f never runs for
    // that call. design/queue_fn.md says a sleeping site's calls keep their place. Keep
    // the released entry until the site sees it land and put it back, paid, at the
    // front of the queue here, as Queue::sleep takes back its InFlight. probe:
    // design/review-2026-11-16/repro/x-async-io-queuefn-release-lost-at-sleep.gx Also
    // reported as (x-engine-seq-errors-02). (x-async-io-01)
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        // CR claude for claude: [bug/high] A call the trigger releases into this site
        // is lost if the site sleeps before the release lands, and the trigger fire
        // that paid for it is used up. QueueFn::update runs QueueState::trigger above
        // the site, so the site can fall asleep in the release's cycle or the next. The
        // values then land in its bindings while it sleeps, and at the wake
        // QueueState::wake clears the in-flight mark while nothing delivers the call,
        // since a wake fires nothing. design/queue_fn.md says a sleeping site's calls
        // keep their place. Fix: keep the released entry in QueueState until the site's
        // update sees it land. Here, while the site is still in flight, push that entry
        // back at the head of the queue, marked paid, so the wake's release sends it
        // again. A direct call sent with set_var because a release landed beside it
        // needs the same. probe:
        // design/review-2026-11-16/repro/core-aux-queuefn-release-lost-at-sleep.gx
        // prints [10, 15]; with the trigger one cycle later it prints [10, 11, 15].
        // graphix-fuzz check AGREEs. (core-aux-01)
        self.call.sleep(ctx);
        let mut s = self.state.lock();
        if !s.asleep.contains(&self.site) {
            s.asleep.push(self.site)
        }
    }

    /// Born again, the site makes its calls afresh.
    // CR claude for claude: [bug/high] A wrapper site reborn by a seq step's next run
    // keeps its argument bindings and `out`. The run is a birth, so the wrapped call
    // fires `f` over the previous run's arguments. `seqq t { qf(t) }` answers each run
    // with the run before's result ([0, 0, 10, 20, ..] where [0, 10, 20, ..] is right).
    // With `#trigger: never()`, every later run answers at once with the first result
    // where its queued call should stall. Leave the site with no arguments until its
    // own call is dispatched or released: release the stored values of
    // `self.call.args()` (as `Callback::delete` does) and reset `out` to phantom (as
    // array::group's reborn does). No queuefn pin in lib_tests/core.rs runs under a
    // seq; add one with the fix. probe:
    // design/review-2026-11-16/repro/tests-lib-a-queuefn-seq-step-answers-previous-call.gx
    // (tests-lib-a-01)
    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.forget(ctx);
    }
}

impl<R: Rt> WrapperApply<R> {
    fn forget(&mut self, ctx: &mut ExecCtx<'_, R>) {
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
pub(crate) struct QueueFn<R: Rt> {
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
    _phantom: PhantomData<fn() -> R>,
}

impl<R: Rt> BuiltIn<R> for QueueFn<R> {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let pop_count = i64::decode(buf)?;
        let count_ref = Option::<BindId>::decode(buf)?.map(|c| (c, ctx.write_target(c)));
        let last_written_depth = i64::decode(buf)?;
        let fid = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let ftyp = Option::<FnType>::decode(buf)?.map(Arc::new);
        let scope = image::scope_decode(buf)?;
        ctx.record_ref(fid, top_id);
        let state = Arc::new(Mutex::new(QueueState::with(
            pop_count,
            count_ref,
            last_written_depth,
        )));
        Ok(Box::new(Self::with(state, fid, ftyp, top_id, scope)))
    }

    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "core_queuefn";
    const ORDERED: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        if from.len() != 3 {
            bail!("queuefn: expected three arguments (#count, #trigger, f)")
        }
        let fid = BindId::new();
        ctx.record_ref(fid, top_id);
        let ftyp = resolved.and_then(|r| extract_fn_arg_type(&ctx.env, r, 2));
        let state = Arc::new(Mutex::new(QueueState::new()));
        Ok(Box::new(Self::with(state, fid, ftyp, top_id, scope.clone())))
    }
}

impl<R: Rt> QueueFn<R> {
    fn with(
        state: StateRef,
        fid: BindId,
        ftyp: Option<Arc<FnType>>,
        top_id: ExprId,
        scope: Scope,
    ) -> Self {
        Self {
            state,
            fid,
            ftyp,
            lambda: None,
            top_id,
            scope,
            out: TagValue::phantom(),
            _phantom: PhantomData,
        }
    }

    fn build_lambda(&mut self, ctx: &mut ExecCtx<'_, R>) -> Result<Value> {
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
                    comments: Default::default(),
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
        let init: InitFn<R> = SArc::new(
            move |scope: &Scope,
                  ctx: &mut CompileCtx<R>,
                  args: &mut [Node<R>],
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
        let def = LambdaDef::runtime(
            id,
            ctx.env.lexical(),
            self.scope.clone(),
            argspec,
            ftyp,
            init,
            self.top_id,
            graphix_compiler::typ::tvar::Level::Def { depth: 1, owner: id },
        );
        Ok(ctx.wrap_lambda(def))
    }

    fn maybe_write_count(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let w = self.state.lock().count_write();
        write_count(ctx, w);
    }
}

impl<R: Rt> Apply<R> for QueueFn<R> {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        // from[0] = #count (a ref, possibly null)
        // from[1] = #trigger
        // from[2] = f
        if let Some(v) = seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned()) {
            let new_ref =
                graphix_compiler::reference::cell(&v).map(|c| (c, ctx.write_target(c)));
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
                if let Some(def) = v.downcast_ref::<LambdaDef<R>>() {
                    self.ftyp = Some(Arc::new(def.typ.reset_tvars()));
                }
            }
            ctx.deliver(self.fid, TagValue::tagged(v, tag), Notify::Walk);
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
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        let Some(ft) = extract_fn_arg_type(&ctx.env, resolved, 2) else {
            bail!("queuefn: third argument must be a function")
        };
        self.ftyp = Some(ft);
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        ctx.release_var(self.fid, self.top_id);
        if let Some(def) =
            self.lambda.as_ref().and_then(|l| l.downcast_ref::<LambdaDef<R>>())
        {
            ctx.lambda_defs.remove(&def.id);
        }
    }

    /// A pause: the queued calls and the credit stay.
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

fn build_wrapper_apply<R: Rt>(
    scope: &Scope,
    ctx: &mut CompileCtx<R>,
    _args: &mut [Node<R>],
    state: StateRef,
    fid: BindId,
    ftyp: Arc<FnType>,
    tid: ExprId,
) -> Result<Box<dyn Apply<R>>> {
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
