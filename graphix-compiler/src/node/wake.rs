//! Wake catch-up fire tracking (`design/wake_catchup.md`), shared by
//! the nodes that pause bodies and wake them: `Select`, the seq machine,
//! a collection whose source went bottom and a call whose function did.

use super::{VarRead, read_var};
use crate::{
    BindId, Event, ExecCtx, Refs, Rt, TagValue, UserEvent, branch::Prior, env::Env,
};
use nohash::{IntMap, IntSet};
use smallvec::SmallVec;
use std::mem;
use triomphe::Arc;

/// What an evaluation catching up owes the inputs it learns only as it
/// runs (a callee bound at its dispatch, a reference's new target): the
/// fire of one written after `since`, the last cycle the evaluating arm
/// was awake, unless an enclosing select tracks it, whose bits decide.
#[derive(Debug)]
pub(crate) struct CatchUp {
    since: u64,
    tracked: Arc<IntSet<BindId>>,
    outer: Option<Arc<CatchUp>>,
}

impl CatchUp {
    fn tracks(&self, id: &BindId) -> bool {
        let mut scope = Some(self);
        while let Some(s) = scope {
            if s.tracked.contains(id) {
                return true;
            }
            scope = s.outer.as_deref();
        }
        false
    }
}

/// Whether the catch-up scope owes `id` a fire: it was written after the
/// arm was last awake, untracked, and is not delivered this cycle.
pub(crate) fn owed<R: Rt, E: UserEvent>(ctx: &ExecCtx<'_, R, E>, id: &BindId) -> bool {
    let Some(scope) = &ctx.catch_up else { return false };
    // CR claude for claude: [bug/medium] [bug/medium] This refuses every input any arm
    // of an enclosing select tracks, leaving it to that select's bits, but
    // TrackedFires::deliver injects only the bits the waking arm names itself (callees
    // left out). So an input the waking arm reads only through a callee bound fresh at
    // its wake, and another arm of the same select names, has its pending fire
    // delivered to no one. Adding an arm that is never selected changes the result. The
    // rule is that the arm evaluation reading an input consumes its bit. Fix: for an
    // input the innermost scope's select tracks, owe it when its bit is pending and
    // consume the bit (the scope carries that select's pending set, or deliver_owed
    // asks the tracker). probe:
    // design/review-2026-10-26/repro/c-prune-wake-tracked-elsewhere.gx
    // (c-prune-wake-03)
    !scope.tracks(id)
        && !ctx.event.variables.contains_key(id)
        && ctx
            .rt
            .store_get(id)
            // XCR claude for claude: [bug/medium] [bug/medium] owed compares the store's
            // write stamp with the arm's last run, but writes that are not fires also
            // move that stamp: a `let` republishing quietly at its wake (Bind::update),
            // and a formal re-seeded standing (store_insert_standing). A callee bound
            // fresh at a wake then reads such an input FIRED although it never fired
            // after the arm's last run. So an edge consumer in it (count, `~`, a
            // connect) acts on a phantom event, depending on whether an unrelated arm
            // slept and woke. The ruling for this mechanism was a stamp of each
            // variable's last fire. Fix: keep a fire stamp that only a firing delivery
            // moves (or have the quiet republishes keep the entry's stamp) and compare
            // that here. probe:
            // design/review-2026-10-26/repro/c-prune-wake-owed-quiet-stamp.gx
            // (c-prune-wake-02)
            // 2026-10-26 claude: Fixed: a standing store entry keeps the stamp of the
            // entry it replaces when that one is from an earlier cycle
            // (branch::standing_stamp, shared by Rt::store_insert_standing's default
            // and RtView's), Rt::store_put is now exact (stamp and all, so a fork's
            // merge keeps it), and Bind's quiet publication (a let republishing at its
            // wake, a first quiet publish) stands instead of stamping a delivery. The
            // formal re-seeded standing (QuietAtRoot::Stand) gets the same through
            // store_insert_standing. The probe's last cycle is [] in every engine. Pin:
            // lang::select::quiet_republish_is_owed_no_fire (interp, jit, par, jit_par;
            // fails without the fix with a last [1, 1]).
            .is_some_and(|(tv, stamp)| *stamp > scope.since && !tv.tag().is_bottom())
}

/// Deliver, fired at its standing value, each input `fill` names that the
/// catch-up scope owes; the deliveries are for [`TrackedFires::restore`].
pub(crate) fn deliver_owed<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
pub(crate) fn inject<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
/// Guards, the scrutinee and pattern binds (of any enclosing select)
/// are not tracked. Survives sleep.
#[derive(Debug, Default)]
pub(crate) struct TrackedFires {
    /// Per arm: the body's free refs. Refreshed at each deselect, when
    /// the arm's subtree is fully materialized.
    per_arm: Vec<IntSet<BindId>>,
    /// Per arm: the inputs the pattern binds the body reads are facets
    /// of. Reading a facet consumes the input's fire; the facet itself
    /// is never delivered.
    consumes: Vec<IntSet<BindId>>,
    /// The union of `per_arm`'s keys, each with how many arms read it.
    all: IntMap<BindId, u32>,
    /// Sound fires no arm evaluation has consumed yet.
    pending: IntSet<BindId>,
    /// A seq step's `let` fires, the entry of the first later step to
    /// read them: only an entered step takes them.
    carried: IntSet<BindId>,
    /// Per arm: its refs were collected again after its first
    /// evaluation, which names what a node knows only once it ran (a
    /// `Deref`'s target).
    ran: Vec<bool>,
    /// Per arm: the last cycle it was awake.
    clock: Vec<ArmClock>,
    /// `all`'s keys for a catch-up scope, built when one opens after
    /// `all` changed.
    keys: Option<Arc<IntSet<BindId>>>,
}

/// An arm's last cycle awake, as the arm stands.
#[derive(Debug, Clone, Copy)]
enum ArmClock {
    /// It ran in this cycle and has not fallen asleep since.
    Awake(u64),
    /// It was last awake in this cycle, the tracker's build cycle if it
    /// never ran.
    Asleep(u64),
}

impl ArmClock {
    fn awake_until(self) -> u64 {
        match self {
            ArmClock::Awake(c) | ArmClock::Asleep(c) => c,
        }
    }

    /// Stopped running in `cycle`: it stood selected, walked or not,
    /// until the cycle before.
    fn fell_asleep(&mut self, cycle: u64) {
        if let ArmClock::Awake(ran) = *self {
            *self = ArmClock::Asleep(ran.max(cycle.saturating_sub(1)))
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
            all: IntMap::default(),
            pending: IntSet::default(),
            carried: IntSet::default(),
            ran: vec![false; n],
            // CR claude for claude: [bug/medium] A never-run arm's `since` is the build
            // cycle, and `owed` needs `stamp > since`. So a variable written in the
            // cycle the select was first updated (a let's birth, or a write landing as
            // the select is born) is not owed. A callee bound through a function value
            // at the arm's first entry misses that fire, while a static callee with the
            // same body catches it up through the bits: `select n { 1 => gs(n) }` gives
            // 1 and the `gd(n)` twin stays bottom. Fix: have TrackedFires::ran record
            // cycle + 1 and make owed test `stamp >= since`. Add a build-cycle twin to
            // first_entry_*_callee_catches_up, whose fire comes well after the build
            // cycle. probe:
            // design/review-2026-10-26/repro/tests-select-dynamic-callee-misses-build-cycle-fire.gx
            // Also reported as (x-refs-catchup-04). (tests-select-03)
            // 2026-10-26 claude: last_run is now ArmClock (x-prune-01): a never-run arm is
            // ArmClock::Asleep(build cycle), and TrackedFires::open takes no later than the
            // enclosing catch-up scope's since, so a nested arm's first entry inside a woken or
            // first-entered arm already counts from the enclosing arm's cycle; the top-level
            // build-cycle case this CR names is unchanged.
            clock: vec![ArmClock::Asleep(cycle); n],
            keys: None,
        };
        for i in 0..n {
            let mut r = Refs::default();
            fill(i, &mut r);
            Self::arm_refs(env, &r, &mut t.per_arm[i], &mut t.consumes[i]);
        }
        for id in t.per_arm.iter().flatten() {
            *t.all.entry(*id).or_default() += 1;
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
    pub(crate) fn refresh(&mut self, env: &Env, i: usize, fill: impl FnOnce(&mut Refs)) {
        self.keys = None;
        let Self { per_arm, consumes, all, pending, carried, .. } = self;
        let mut r = Refs::default();
        fill(&mut r);
        let old = mem::take(&mut per_arm[i]);
        Self::arm_refs(env, &r, &mut per_arm[i], &mut consumes[i]);
        // CR claude for claude: [perf/medium] refresh rebuilds `all` from every arm's
        // map and then retains `pending`, at every deselect. A seq machine deselects at
        // every step completion, so a run of S steps costs O(S^2). observe_except also
        // reads every tracked id with read_var on every update, whether or not anything
        // fired, and for each id scans `evaluated` for membership. The seq machine adds
        // a linear `steps.iter().position` label search on each pc fire. A chained seq
        // of 800 steps runs in 77 ms and 1600 in 248 ms (probe:
        // design/review-2026-10-12/repro/x-perf-runtime-03.py, -03b.py, -03c.py), and
        // an idle select whose 1600 arms read distinct variables costs about 0.28 us
        // per tracked input per cycle (-05.py with `d`). Fix: keep `all` as a reference
        // count per BindId and diff the refreshed arm's key set against its old one,
        // dropping pending only for keys that reach zero. In observe_except, walk the
        // event's delivered variables filtered by `all` when the event is the smaller
        // side; the store-stamped-this-cycle case read_var also reports must be made
        // explicit. Precompute the evaluated arms' membership set once per call. Map
        // label to step index once in the seq machine. (x-perf-runtime-03)
        // the input counts move by this arm's difference; an input no arm
        // reads any more drops its bits
        // 2026-10-15 claude: Done: all counts the arms reading each input and refresh
        // moves the counts by the refreshed arm's difference, dropping the bits of an
        // input no arm reads; the seq machine tries the current step's next before
        // searching the labels. The seq chain probe is now linear (quick build, pinned:
        // 800/1600/3200 steps 3.1/5.1/9.7 ms, from 11.9/50.8/131.7). Open:
        // observe_except still reads every tracked input each update
        // (x-perf-runtime-05.py d, 1600 arms: 0.22 ms a cycle, 0.06 shared). Walking
        // the event instead needs the store writes stamped this cycle with no event
        // entry made explicit (the ByRef mirror, a module's proxies, the seq pc) and
        // must keep a fork's read audit, so it is left.
        // 2026-10-16 claude: The open part (observe_except reading every tracked input
        // each update) would be answered by the same per-node dirty bits as Eric's walk
        // pruning on x-perf-runtime-01: an arm's tracked input that nothing set stays
        // clean. Readdressed to Eric with x-alloc-06 so the design is made once.
        // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no ruling
        // needed. Check whether walk pruning's marks now answer observe_except's
        // per-update reads; if they do, close.
        for id in per_arm[i].iter() {
            if !old.contains(id) {
                *all.entry(*id).or_default() += 1;
            }
        }
        for id in old.iter() {
            if !per_arm[i].contains(id)
                && let Some(n) = all.get_mut(id)
            {
                *n -= 1;
                if *n == 0 {
                    all.remove(id);
                    pending.remove(id);
                    carried.remove(id);
                }
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
        self.keys = None;
        let Self { per_arm, consumes, all, pending, .. } = self;
        for id in inputs {
            if per_arm[i].insert(id) {
                *all.entry(id).or_default() += 1;
                pending.remove(&id);
            }
        }
        consumes[i].extend(consumed);
    }

    /// Record this cycle's sound fires of tracked inputs. Runs before
    /// routing, so the taken arm consumes same-cycle fires immediately
    /// and no-arm cycles accumulate them for a future waker.
    pub(crate) fn observe<R: Rt, E: UserEvent>(&mut self, ctx: &ExecCtx<'_, R, E>) {
        self.observe_except(ctx, &[], None)
    }

    /// [`Self::observe`] after the arms `evaluated` ran this cycle: the
    /// fires their evaluation made are recorded for the arms that did not
    /// run; what an evaluated arm reads it saw, except the `carried`
    /// inputs (a seq step's `let`s), whose fire is also the entry of the
    /// first arm to read them in a later cycle.
    pub(crate) fn observe_except<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
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
        for id in all.keys() {
            let carry = carried.is_some_and(|c| c.contains(id));
            if (carry || !evaluated.iter().any(|i| per_arm[*i].contains(id)))
                && let Some(VarRead::Delivered(tv)) = read_var(ctx, id)
                && tv.tag().is_fired()
            {
                let bits = if carry { &mut *carried_bits } else { &mut *pending };
                bits.insert(*id);
            }
        }
    }

    /// Arm `i` runs: under a wake or step view, open the catch-up scope
    /// for what it learns as it runs; what to put back, for [`Self::ran`].
    pub(crate) fn open<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        i: usize,
    ) -> Option<Option<Arc<CatchUp>>> {
        if !ctx.event.init() {
            return None;
        }
        let all = &self.all;
        // CR claude for claude: [perf/medium] [perf/medium] In a seq machine every step
        // completion refreshes (keys = None), so each step entry, a Step view, rebuilds
        // here an IntSet of every input any step reads. deliver also scans the whole
        // pending set for the step's own bits. A run of N same-cycle steps costs O(N x
        // inputs): with 3 extra inputs a step the chain is about 4.5 times slower and
        // grows about 3x per doubling of N, and the pc reader cost of x-perf-runtime-04
        // does not depend on the input count. Fix: keep keys current in refresh and
        // evaluated (Arc::make_mut as the counts in `all` reach 1 or 0) instead of
        // dropping it, and in deliver iterate the smaller of pending and the arm's
        // per_arm set. probe:
        // design/review-2026-10-26/repro/c-prune-wake-seq-step-inputs.py
        // (c-prune-wake-05)
        let tracked = self
            .keys
            .get_or_insert_with(|| Arc::new(all.keys().copied().collect()))
            .clone();
        // an arm sleeps whenever the arm around it does
        let since = match &ctx.catch_up {
            Some(outer) => self.clock[i].awake_until().min(outer.since),
            None => self.clock[i].awake_until(),
        };
        let scope = CatchUp { since, tracked, outer: ctx.catch_up.clone() };
        Some(mem::replace(&mut ctx.catch_up, Some(Arc::new(scope))))
    }

    /// Arm `i` ran this cycle: close the scope [`Self::open`] opened.
    pub(crate) fn ran<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        i: usize,
        outer: Option<Option<Arc<CatchUp>>>,
    ) {
        if let Some(outer) = outer {
            ctx.catch_up = outer;
        }
        // XCR claude for claude: [bug/high] last_run records the last cycle a walk
        // evaluated the arm, and wake::owed treats anything written after it as a fire
        // missed in sleep. Walk pruning moves that cycle: a select nothing marks is not
        // walked while its arm stays selected, so at the arm's wake a write that landed
        // while the arm was awake is re-raised FIRED to a freshly bound callee (or a
        // Deref's new target), and pruned and unpruned runs disagree. Record the cycle
        // before the arm goes to sleep instead (deselect in select.rs and
        // seq_machine.rs, and the selected arm in Select::sleep and the machine's
        // reset), not on every walk, and pin the probe in lang::prune. probe:
        // design/review-2026-10-26/repro/x-prune-catchup-since-last-walk.gx
        // (GRAPHIX_PRUNE=on prints 0 1 0 101, off prints 0 1 0; graphix-fuzz check
        // reports a walk pruning divergence) (x-prune-01)
        // 2026-10-26 claude: Fixed: TrackedFires keeps an ArmClock per arm (Awake(last
        // run) | Asleep(last cycle awake)); a run sets Awake, and the arm's falling
        // asleep (select.rs deselect, a guard the chain passes by, Select::sleep and
        // the seq machine's deselect and sleep for every awake arm) records the cycle
        // before it, so 'since' is when the arm fell asleep whether or not pruning
        // walked it; TrackedFires::open also takes no later than the enclosing scope's
        // since (an arm sleeps whenever the arm around it does). The probe prints 0 1 0
        // in both prune modes. Pins: lang::prune::catch_up_counts_from_the_deselect
        // (this probe), catch_up_counts_from_the_enclosing_sleep (fails without
        // Select::sleep's stamp), guard_catch_up_counts_from_the_pass and
        // guard_catch_up_counts_from_the_enclosing_sleep (the guard tracker's two
        // stamps). No pin for the seq machine's stamps or the enclosing-scope min,
        // which I could not reach from a program.
        // XCR claude for claude: [bug/high] [bug/high] last_run is set at every
        // evaluation of the arm, so it is the last cycle a walk reached the arm, and
        // walk pruning changes it: a skipped statement leaves it behind where the
        // unpruned walk advances it. wake::owed compares input write stamps against it.
        // So a callee bound fresh at the arm's wake is caught up on an input written
        // while the arm was awake but skipped, only when pruning is on, and
        // graphix-fuzz check reports a walk pruning divergence. Fix: record the cycle
        // the arm stops running (its deselect, and the select's or machine's own sleep
        // while it is selected) instead of every evaluation, so `since` means 'fell
        // asleep' in both walks, as the ruling ('fired while it slept') asks. probe:
        // design/review-2026-10-26/repro/c-prune-wake-last-run-pruned.gx
        // (c-prune-wake-01)
        // 2026-10-26 claude: Fixed with x-prune-01 (ArmClock: since is the cycle before
        // the arm fell asleep, set at its deselect or its select's or machine's sleep,
        // not at every walk). graphix-fuzz check of the probe now AGREEs. Pin:
        // lang::prune::skipped_statement_owes_its_awake_fires_nothing (this probe on a
        // counter; fails without the fix pruned, prints (2, 1) last).
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
                pending.insert(*id);
            }
            !read
        });
    }

    /// Consume the bits arm `i` reads, injecting one catch-up FIRED
    /// delivery at the current standing value for each input not
    /// delivered live this cycle. Returns the injected entries for
    /// [`Self::restore`]; a bottomed or vanished input spends its bit
    /// and injects nothing.
    pub(crate) fn deliver<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        i: usize,
    ) -> Injected {
        let mut injected = Injected::new();
        if self.pending.is_empty() {
            return injected;
        }
        let Self { per_arm, consumes, pending, .. } = self;
        if let Some(consumed) = consumes.get(i) {
            for id in consumed.iter() {
                pending.remove(id);
            }
        }
        let Some(set) = per_arm.get(i) else { return injected };
        let read: SmallVec<[BindId; 8]> =
            pending.iter().filter(|id| set.contains(id)).copied().collect();
        for id in read {
            pending.remove(&id);
            inject(ctx, id, &mut injected);
        }
        injected
    }

    pub(crate) fn restore<E: UserEvent>(event: &mut Event<E>, injected: Injected) {
        for (id, prior) in injected {
            event.variables.put_back(id, prior)
        }
    }
}
