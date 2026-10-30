//! Wake catch-up fire tracking (`design/wake_catchup.md`), shared by
//! the nodes that pause bodies and wake them: `Select`, the seq machine,
//! a collection whose source went bottom and a call whose function did.

use super::{VarRead, read_var};
use crate::{BindId, Event, ExecCtx, Refs, Rt, TagValue, branch::Prior, env::Env};
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::mem;
use triomphe::Arc;

/// What an evaluation catching up owes the inputs it learns only as it
/// runs (a callee bound at its dispatch, a reference's new target): the
/// fire of one written from `unseen` on, while the evaluating arm slept,
/// unless the innermost enclosing select that tracks it has no fire bit
/// for it.
#[derive(Debug)]
pub(crate) struct CatchUp {
    /// The first cycle whose writes the evaluating arm did not see.
    unseen: u64,
    /// The inputs the arms of this scope's select read.
    tracked: Arc<IntSet<BindId>>,
    /// Their fires no arm evaluation has read.
    pending: Arc<IntSet<BindId>>,
    /// The pending fires this evaluation was owed: it read them, so it
    /// consumes their bits when it ends.
    taken: Mutex<IntSet<BindId>>,
    outer: Option<Arc<CatchUp>>,
    /// A call issued as a fresh frame ([`FreshFrame`]): every input that
    /// stands is owed, unless a select inside it tracks the input.
    fresh: bool,
}

impl CatchUp {
    /// Whether a fire of `id` no reader saw is this evaluation's: by the
    /// bit of the innermost select that tracks `id`, which it takes.
    fn owes(&self, id: &BindId) -> bool {
        let mut scope = Some(self);
        while let Some(s) = scope {
            if s.tracked.contains(id) {
                let owed = s.pending.contains(id);
                if owed {
                    s.taken.lock().insert(*id);
                }
                return owed;
            }
            if s.fresh {
                return true;
            }
            scope = s.outer.as_deref();
        }
        true
    }
}

/// A call a seq step issues, run as a fresh frame: everything it reads
/// that stands is delivered, fired, whether it reads it now (its
/// arguments, its callee's captures) or learns it as it runs (a callee
/// bound at its dispatch).
pub(crate) struct FreshFrame {
    outer: Option<Arc<CatchUp>>,
    injected: Injected,
}

impl FreshFrame {
    /// Open the frame around the update of the call `fill` collects.
    pub(crate) fn open<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        fill: impl FnOnce(&mut Refs),
    ) -> Self {
        let scope = CatchUp {
            unseen: 0,
            tracked: Arc::default(),
            pending: Arc::default(),
            taken: Mutex::default(),
            outer: ctx.catch_up.clone(),
            fresh: true,
        };
        let outer = mem::replace(&mut ctx.catch_up, Some(Arc::new(scope)));
        let injected = deliver_owed(ctx, fill);
        Self { outer, injected }
    }

    /// Close the frame once the call has updated.
    pub(crate) fn close<R: Rt>(self, ctx: &mut ExecCtx<'_, R>) {
        TrackedFires::restore(ctx.event, self.injected);
        ctx.catch_up = self.outer;
    }
}

/// Whether the evaluation runs inside a call a seq step issues.
pub(crate) fn in_fresh_frame<R: Rt>(ctx: &ExecCtx<'_, R>) -> bool {
    let mut scope = ctx.catch_up.as_deref();
    while let Some(s) = scope {
        if s.fresh {
            return true;
        }
        scope = s.outer.as_deref();
    }
    false
}

/// Whether the catch-up scope owes `id` a fire: it was written while the
/// arm slept, is not delivered this cycle, and no reader saw its fire.
pub(crate) fn owed<R: Rt>(ctx: &ExecCtx<'_, R>, id: &BindId) -> bool {
    let Some(scope) = &ctx.catch_up else { return false };
    !ctx.event.variables.contains_key(id)
        && ctx
            .rt
            .store_get(id)
            .is_some_and(|(tv, stamp)| *stamp >= scope.unseen && !tv.tag().is_bottom())
        && scope.owes(id)
}

/// Deliver, fired at its standing value, each input `fill` names that the
/// catch-up scope owes; the deliveries are for [`TrackedFires::restore`].
pub(crate) fn deliver_owed<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    fill: impl FnOnce(&mut Refs),
) -> Injected {
    let mut injected = Injected::new();
    if ctx.catch_up.is_none() {
        return injected;
    }
    let mut r = Refs::default();
    fill(&mut r);
    for id in r.refed.difference(&r.bound) {
        if owed(ctx, id) {
            inject(ctx, *id, &mut injected);
        }
    }
    injected
}

/// Deliver one catch-up fire of `id` at its current value into
/// `injected`, unless it fires live this cycle (a live fire delivers
/// itself), is bottom (a bottom input is bottom at the wake) or never
/// delivered.
pub(crate) fn inject<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    id: BindId,
    injected: &mut Injected,
) {
    let v = match read_var(ctx, &id) {
        Some(VarRead::Delivered(tv)) if tv.tag().triggers() => return,
        Some(VarRead::Delivered(tv) | VarRead::Standing(tv)) if !tv.tag().is_bottom() => {
            tv.value_cloned()
        }
        _ => return,
    };
    injected.push((id, ctx.event.variables.put(id, TagValue::fired(v))));
}

/// Wake-catch-up fire tracking (design/wake_catchup.md): one fire bit
/// per arm-body input, set when the input fires and consumed by the
/// arm evaluation that reads it, so a woken arm receives exactly the
/// fires no selected reader saw, once, at the current standing value.
/// A select gives each guard a consumer slot in the same tracker. The
/// scrutinee and pattern binds (of any enclosing select) are not delivered
/// by it. Survives sleep.
#[derive(Debug, Default)]
pub(crate) struct TrackedFires {
    /// Per arm: the body's free refs. Refreshed at each deselect, when
    /// the arm's subtree is fully materialized.
    per_arm: Vec<IntSet<BindId>>,
    /// Per arm: the inputs the pattern binds the body reads are facets
    /// of. Reading a facet consumes the input's fire; the facet itself
    /// is never delivered.
    consumes: Vec<IntSet<BindId>>,
    /// The union of `per_arm`'s keys.
    all: Inputs,
    /// Sound fires no arm evaluation has consumed yet.
    pending: Arc<IntSet<BindId>>,
    /// A seq step's `let` fires, the entry of the first later step to
    /// read them: only an entered step takes them.
    carried: IntSet<BindId>,
    /// Per arm: its refs were collected again after its first
    /// evaluation, which names what a node knows only once it ran (a
    /// `Deref`'s target).
    ran: Vec<bool>,
    /// Per arm: where it stands in time.
    clock: Vec<ArmClock>,
}

/// The inputs a tracker's arms read, each with how many arms read it,
/// and the set of them a catch-up scope shares.
#[derive(Debug, Default)]
struct Inputs {
    counts: IntMap<BindId, u32>,
    keys: Arc<IntSet<BindId>>,
}

impl Inputs {
    /// One more arm reads `id`.
    fn add(&mut self, id: BindId) {
        let n = self.counts.entry(id).or_default();
        *n += 1;
        if *n == 1 {
            Arc::make_mut(&mut self.keys).insert(id);
        }
    }

    /// One arm fewer reads `id`: whether no arm reads it now.
    fn drop_one(&mut self, id: &BindId) -> bool {
        let Some(n) = self.counts.get_mut(id) else { return false };
        *n -= 1;
        if *n > 0 {
            return false;
        }
        self.counts.remove(id);
        Arc::make_mut(&mut self.keys).remove(id);
        true
    }
}

/// An arm's place in time, as the arm stands.
#[derive(Debug, Clone, Copy)]
enum ArmClock {
    /// It ran in this cycle and has not fallen asleep since.
    Awake(u64),
    /// Asleep, it saw no write from this cycle on: the tracker's build
    /// cycle if it never ran.
    Asleep(u64),
}

impl ArmClock {
    /// The first cycle whose writes the arm did not see.
    fn unseen(self) -> u64 {
        match self {
            ArmClock::Awake(c) => c + 1,
            ArmClock::Asleep(c) => c,
        }
    }

    /// Stopped running in `cycle`: it stood selected, walked or not,
    /// until the cycle before.
    fn fell_asleep(&mut self, cycle: u64) {
        if let ArmClock::Awake(ran) = *self {
            *self = ArmClock::Asleep((ran + 1).max(cycle))
        }
    }
}

/// The catch-up deliveries made for one evaluation, with what each
/// overwrote, for [`TrackedFires::restore`].
pub(crate) type Injected = SmallVec<[(BindId, Prior<TagValue>); 4]>;

impl TrackedFires {
    /// An arm's tracked inputs into `inputs` and the pattern-bind inputs
    /// it consumes into `consumes`, both cleared first, from `r`: the
    /// arm's refs with its own binds in `bound`.
    fn arm_refs(
        env: &Env,
        r: &Refs,
        inputs: &mut IntSet<BindId>,
        consumes: &mut IntSet<BindId>,
    ) {
        inputs.clear();
        consumes.clear();
        for id in r.refed.difference(&r.bound).copied() {
            match env.pattern_inputs(id) {
                None => {
                    inputs.insert(id);
                }
                Some(ids) => consumes.extend(ids.iter().copied()),
            }
        }
    }

    /// A tracker over `n` arms, built in `cycle`; `fill(i, r)` collects
    /// arm `i`'s refs.
    pub(crate) fn new(
        env: &Env,
        cycle: u64,
        n: usize,
        mut fill: impl FnMut(usize, &mut Refs),
    ) -> Self {
        let mut t = TrackedFires {
            per_arm: (0..n).map(|_| IntSet::default()).collect(),
            consumes: (0..n).map(|_| IntSet::default()).collect(),
            all: Inputs::default(),
            pending: Arc::default(),
            carried: IntSet::default(),
            ran: vec![false; n],
            clock: vec![ArmClock::Asleep(cycle); n],
        };
        for i in 0..n {
            let mut r = Refs::default();
            fill(i, &mut r);
            Self::arm_refs(env, &r, &mut t.per_arm[i], &mut t.consumes[i]);
        }
        for id in t.per_arm.iter().flatten() {
            t.all.add(*id);
        }
        t
    }

    /// Re-collect arm `i`'s refs, at a deselect, when its subtree is
    /// fully materialized.
    // CR claude for claude: [perf/low] Every select deselect and every seq step passage
    // call this. It builds a fresh Refs and walks the arm's entire subtree with `refs`,
    // descending into every bound callee instance and every collection slot. It then
    // rebuilds the arm's input map and the select-wide `all` set. The arm's refs change
    // only when something under it binds or resizes, yet a state machine that switches
    // arms on each event repeats the full walk and rehash at every switch, on top of
    // the sleep walk. Fix: refresh arm i only when its subtree changed since its last
    // refresh. A dirty bit or generation that the arm's CallSites and collections bump
    // when they bind or resize would do it. (x-alloc-06)
    // 2026-10-16 claude: The dirty bit this asks for is the same mechanism as Eric's
    // generalized walk pruning (his 2026-10-14 note on x-perf-runtime-01: a NodeId per
    // node, BindId -> dependent NodeIds, a dirty bit per node). Readdressed to Eric so
    // both are designed once.
    // 2026-10-18 claude: Walk pruning is built (design/walk_pruning.md), but its marks
    // record deliveries, not structural change: a boundary is marked when something it
    // reads is written this cycle, never when a CallSite under it binds or a collection
    // resizes. So it does not give this refresh the generation it needs. That
    // generation is a separate, local fix (a counter the arm's binds and resizes bump),
    // no longer coupled to the pruning design; readdressed to claude.
    // 2026-10-20 claude: Looked at for this round and left: a generation that gates
    // refresh must move with every way an arm's refs grow, and not all of them pass
    // through a bind or a resize. Control::rebuilds (new this round, bumped by
    // ExecCtx::apply_deferred and CallSite::bind) covers binds, slot builds, lazily
    // decoded bodies and module recompiles, but a Deref's moving target is read only by
    // the refs walk (TrackedFires::evaluated takes it on the first run alone), and a
    // skipped refresh that misses a new input loses that input's catch-up, a semantics
    // change. Gating on rebuilds plus a per-arm count of the Deref retargets under it
    // would do; it wants a pin of a woken arm whose reference moved while it slept.
    // 2026-10-27 claude: Looked at for L2-b07 and left. Gating refresh on
    // Control::rebuilds alone would be sound only if every way an arm's refs grow bumps
    // it: CallSite::bind and ExecCtx::apply_deferred do; Bind::address's moved target
    // (a Deref retarget) does not and would have to; and every Update::refs or
    // Apply::refs reading run-time state must be on that list (28 impls: callsite,
    // collection slots, bind's Deref, array's group predicate, core's handler/queuefn,
    // http::serve and sys::net per-request instances). A refs shrink skipped is
    // harmless (a stale input only injects a fire no node reads). Wants that inventory
    // plus a pin of a woken arm whose Deref moved while it slept before it is safe to
    // land.
    pub(crate) fn refresh(&mut self, env: &Env, i: usize, fill: impl FnOnce(&mut Refs)) {
        let Self { per_arm, consumes, all, pending, carried, .. } = self;
        let mut r = Refs::default();
        fill(&mut r);
        let old = mem::take(&mut per_arm[i]);
        Self::arm_refs(env, &r, &mut per_arm[i], &mut consumes[i]);
        for id in per_arm[i].iter() {
            if !old.contains(id) {
                all.add(*id);
            }
        }
        for id in old.iter() {
            if !per_arm[i].contains(id) && all.drop_one(id) {
                if pending.contains(id) {
                    Arc::make_mut(pending).remove(id);
                }
                carried.remove(id);
            }
        }
    }

    /// Arm `i` was evaluated: the first time, add what its own nodes
    /// name only once they ran (a `Deref`'s target), callees left out.
    /// The arm read such an input live as it ran, so it consumes its fire.
    pub(crate) fn evaluated(
        &mut self,
        env: &Env,
        i: usize,
        fill: impl FnOnce(&mut Refs),
    ) {
        if std::mem::replace(&mut self.ran[i], true) {
            return;
        }
        let mut r = Refs::without_callees();
        fill(&mut r);
        let (mut inputs, mut consumed) = (IntSet::default(), IntSet::default());
        Self::arm_refs(env, &r, &mut inputs, &mut consumed);
        let Self { per_arm, consumes, all, pending, .. } = self;
        for id in inputs {
            if per_arm[i].insert(id) {
                all.add(id);
                if pending.contains(&id) {
                    Arc::make_mut(pending).remove(&id);
                }
            }
        }
        consumes[i].extend(consumed);
    }

    /// Record this cycle's sound fires of tracked inputs. Runs before
    /// routing, so the taken arm consumes same-cycle fires immediately
    /// and no-arm cycles accumulate them for a future waker.
    pub(crate) fn observe<R: Rt>(&mut self, ctx: &ExecCtx<'_, R>) {
        self.observe_except(ctx, &[], None)
    }

    /// [`Self::observe`] after the arms `evaluated` ran this cycle: the
    /// fires their evaluation made are recorded for the arms that did not
    /// run; what an evaluated arm reads it saw, except the `carried`
    /// inputs (a seq step's `let`s), whose fire is also the entry of the
    /// first arm to read them in a later cycle.
    pub(crate) fn observe_except<R: Rt>(
        &mut self,
        ctx: &ExecCtx<'_, R>,
        evaluated: &[usize],
        carried: Option<&IntSet<BindId>>,
    ) {
        let Self { all, pending, carried: carried_bits, per_arm, .. } = self;
        // CR claude for claude: [perf/low] Walk pruning, now built, does not shrink
        // this scan, contrary to the x-perf-runtime-03 note: a walked select still
        // read_vars every input of every arm. Pruning does make a sound filter
        // possible. In a Cycle view, an arm whose body Prune is not marked this cycle
        // had none of its tracked inputs delivered, since every this-cycle write marks
        // its readers' boundaries and their ancestors. Catch-up injections are unmarked
        // but happen only under Wake or Step. Visit only the per_arm maps of dirty arms
        // when the view is Cycle (any statement boundary of a seq step, the instance
        // body for Absent), and scan everything under other views. Cites
        // x-perf-runtime-03. Found by reading; not run. (c-prune-wake-06)
        // 2026-10-20 claude: Looked at for this round and left: the filter is sound
        // only where every fire of a tracked input marks the arm's boundary, and
        // several deliveries are raw event.variables inserts (a module's proxies, a seq
        // pc, a bind's republish) whose marking is not pinned; a missed fire here is a
        // lost catch-up, not a slower cycle. Safe way in: apply the filter only under
        // PruneMode::On, and under Audit scan everything and panic when a fired input
        // belongs to no dirty arm, so GRAPHIX_PRUNE=audit and the fuzzer's Pair::Prune
        // check it. Gain to expect: x-perf-runtime-05.py (d) puts the scan at about
        // 0.22 ms a cycle for 1600 idle arms, nothing measurable at a handful.
        // 2026-10-27 claude: Looked at again for L2-b07 and left. A further hole in the
        // filter's soundness argument: catch-up injections are not confined to Wake or
        // Step views. TrackedFires::deliver runs at every arm evaluation, a Cycle-view
        // one included (a guard flip or a scrutinee routing to an arm with a pending
        // bit), and wake::inject writes the event without marking any reader, so a
        // select nested in that arm observes the injected input as fired in a Cycle
        // view with no boundary of its arms marked; the filter would stop it recording
        // that bit for its other arms, a semantics change. Making inject mark its
        // readers would close that one (it only costs walks). With the scan's cost
        // about 0.2 ms a cycle at 1600 idle arms, the Audit check suggested on 10-20 is
        // still the way in.
        let seen: LPooled<IntSet<BindId>> =
            evaluated.iter().flat_map(|i| per_arm[*i].iter().copied()).collect();
        for id in all.keys.iter() {
            let carry = carried.is_some_and(|c| c.contains(id));
            if (carry || !seen.contains(id))
                && let Some(VarRead::Delivered(tv)) = read_var(ctx, id)
                && tv.tag().is_fired()
            {
                if carry {
                    carried_bits.insert(*id);
                } else if !pending.contains(id) {
                    Arc::make_mut(pending).insert(*id);
                }
            }
        }
    }

    /// Arm `i` runs: under a wake or step view, open the catch-up scope
    /// for what it learns as it runs; what to put back, for [`Self::ran`].
    pub(crate) fn open<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        i: usize,
    ) -> Option<Option<Arc<CatchUp>>> {
        if !ctx.event.init() {
            return None;
        }

        // an arm sleeps whenever the arm around it does
        let unseen = match &ctx.catch_up {
            Some(outer) => self.clock[i].unseen().min(outer.unseen),
            None => self.clock[i].unseen(),
        };
        let scope = CatchUp {
            unseen,
            tracked: self.all.keys.clone(),
            pending: self.pending.clone(),
            taken: Mutex::default(),
            outer: ctx.catch_up.clone(),
            fresh: false,
        };
        Some(mem::replace(&mut ctx.catch_up, Some(Arc::new(scope))))
    }

    /// Arm `i` ran this cycle: close the scope [`Self::open`] opened.
    pub(crate) fn ran<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        i: usize,
        outer: Option<Option<Arc<CatchUp>>>,
    ) {
        if let Some(outer) = outer
            && let Some(scope) = mem::replace(&mut ctx.catch_up, outer)
        {
            let taken = mem::take(&mut *scope.taken.lock());
            drop(scope);
            if !taken.is_empty() {
                let pending = Arc::make_mut(&mut self.pending);
                for id in taken.iter() {
                    pending.remove(id);
                }
            }
        }
        self.clock[i] = ArmClock::Awake(ctx.rt.cycle());
    }

    /// Arm `i` stops running in `cycle` (deselected, or put to sleep
    /// while selected): it was awake until the cycle before. An arm
    /// already asleep stays asleep since it fell.
    pub(crate) fn fell_asleep(&mut self, i: usize, cycle: u64) {
        self.clock[i].fell_asleep(cycle)
    }

    /// Every awake arm falls asleep in `cycle`: the node holding them sleeps.
    pub(crate) fn all_fell_asleep(&mut self, cycle: u64) {
        for c in self.clock.iter_mut() {
            c.fell_asleep(cycle)
        }
    }

    /// Arm `i` is entered: the carried fires it reads are its entry's.
    pub(crate) fn enter(&mut self, i: usize) {
        let Some(set) = self.per_arm.get(i) else { return };
        let Self { carried, pending, .. } = self;
        carried.retain(|id| {
            let read = set.contains(id);
            if read {
                Arc::make_mut(pending).insert(*id);
            }
            !read
        });
    }

    /// Consume the bits arm `i` reads, injecting one catch-up FIRED
    /// delivery at the current standing value for each input not
    /// delivered live this cycle. Returns the injected entries for
    /// [`Self::restore`]; a bottomed or vanished input spends its bit
    /// and injects nothing.
    pub(crate) fn deliver<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        i: usize,
    ) -> Injected {
        let mut injected = Injected::new();
        if self.pending.is_empty() {
            return injected;
        }
        let Self { per_arm, consumes, pending, .. } = self;
        let pending = Arc::make_mut(pending);
        if let Some(consumed) = consumes.get(i) {
            for id in consumed.iter() {
                pending.remove(id);
            }
        }
        let Some(set) = per_arm.get(i) else { return injected };
        let read: SmallVec<[BindId; 8]> = if pending.len() <= set.len() {
            pending.iter().filter(|id| set.contains(id)).copied().collect()
        } else {
            set.iter().filter(|id| pending.contains(id)).copied().collect()
        };
        for id in read {
            pending.remove(&id);
            inject(ctx, id, &mut injected);
        }
        injected
    }

    pub(crate) fn restore(event: &mut Event, injected: Injected) {
        for (id, prior) in injected {
            event.variables.put_back(id, prior)
        }
    }
}
