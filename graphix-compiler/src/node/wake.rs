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

/// Wake-catch-up fire tracking (design/wake_catchup.md): one fire bit
/// per arm-body input, set when the input fires and consumed by the
/// arm evaluation that reads it, so a woken arm receives exactly the
/// fires no selected reader saw, once, at the current standing value.
/// Guards, the scrutinee and pattern binds (of any enclosing select)
/// are not tracked. Survives sleep.
#[derive(Debug, Default)]
pub(crate) struct TrackedFires {
    /// Per arm: the body's free refs, keyed by the input they are
    /// tracked under (a destructuring `let`'s siblings share their
    /// group's representative, `Env::facet_of`). Refreshed at each
    /// deselect, when the arm's subtree is fully materialized.
    per_arm: Vec<IntMap<BindId, SmallVec<[BindId; 2]>>>,
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
        inputs: &mut IntMap<BindId, SmallVec<[BindId; 2]>>,
        consumes: &mut IntSet<BindId>,
    ) {
        inputs.clear();
        consumes.clear();
        for id in r.refed.difference(&r.bound).copied() {
            match env.pattern_inputs(id) {
                None => inputs.entry(env.facet_of(id)).or_default().push(id),
                Some(ids) => consumes.extend(ids.iter().map(|id| env.facet_of(*id))),
            }
        }
    }

    /// A tracker over `n` arms; `fill(i, r)` collects arm `i`'s refs.
    // CR claude for eric: [bug/medium] An arm's tracked inputs come from its
    // compile-time refs and are refreshed only at a deselect. A callee reached through
    // a function value has no instance before its first dispatch, so the variables its
    // body reads are untracked until the arm's first deselect. A fire of such a
    // variable while another arm was selected is therefore lost at the arm's first
    // entry. A statically resolved callee with the same body catches the fire up. In
    // the probe, `|x| a ~ x` called directly prints "static 6". The same lambda chosen
    // by a select prints nothing, on both engines, so the fuzzer cannot see it. The
    // wake rule ("the fork" in design/wake_catchup.md) prescribes the catch-up, so how
    // a callee resolves changes what a program outputs. A fix needs a way to learn, at
    // a dynamic callee's first dispatch, which of its body's inputs fired since the arm
    // last ran. One option is a per-variable fire epoch compared with the arm's last
    // evaluation. Another is to seed the bits from the function value's definition refs
    // when the arm reads a function-typed input. Pick one, then correct
    // lang::collection::collection_refs_reach_its_slots: its [9]-only expectation
    // encodes the lost catch-up. Also add a static twin of that pin. probe:
    // design/review-2026-10-12/repro/tests-lang-rest-b-02.gx (tests-lang-rest-b-02)
    pub(crate) fn new(
        env: &Env,
        n: usize,
        mut fill: impl FnMut(usize, &mut Refs),
    ) -> Self {
        let mut t = TrackedFires {
            per_arm: (0..n).map(|_| IntMap::default()).collect(),
            consumes: (0..n).map(|_| IntSet::default()).collect(),
            all: IntMap::default(),
            pending: IntSet::default(),
            carried: IntSet::default(),
            ran: vec![false; n],
        };
        for i in 0..n {
            let mut r = Refs::default();
            fill(i, &mut r);
            Self::arm_refs(env, &r, &mut t.per_arm[i], &mut t.consumes[i]);
        }
        for id in t.per_arm.iter().flat_map(|m| m.keys()) {
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
    pub(crate) fn refresh(&mut self, env: &Env, i: usize, fill: impl FnOnce(&mut Refs)) {
        let Self { per_arm, consumes, all, pending, carried, ran: _ } = self;
        let mut r = Refs::default();
        fill(&mut r);
        let old = mem::take(&mut per_arm[i]);
        Self::arm_refs(env, &r, &mut per_arm[i], &mut consumes[i]);
        // CR claude for eric: [perf/medium] refresh rebuilds `all` from every arm's
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
        for id in per_arm[i].keys() {
            if !old.contains_key(id) {
                *all.entry(*id).or_default() += 1;
            }
        }
        for id in old.keys() {
            if !per_arm[i].contains_key(id)
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
        let (mut inputs, mut consumed) = (IntMap::default(), IntSet::default());
        Self::arm_refs(env, &r, &mut inputs, &mut consumed);
        let Self { per_arm, consumes, all, pending, .. } = self;
        for (id, ids) in inputs {
            if let std::collections::hash_map::Entry::Vacant(e) = per_arm[i].entry(id) {
                e.insert(ids);
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
        for id in all.keys() {
            let carry = carried.is_some_and(|c| c.contains(id));
            if (carry || !evaluated.iter().any(|i| per_arm[*i].contains_key(id)))
                && let Some(VarRead::Delivered(tv)) = read_var(ctx, id)
                && tv.tag().is_fired()
            {
                let bits = if carry { &mut *carried_bits } else { &mut *pending };
                bits.insert(*id);
            }
        }
    }

    /// Arm `i` is entered: the carried fires it reads are its entry's.
    pub(crate) fn enter(&mut self, i: usize) {
        let Some(set) = self.per_arm.get(i) else { return };
        let Self { carried, pending, .. } = self;
        carried.retain(|id| {
            let read = set.contains_key(id);
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
        if let Some(consumed) = self.consumes.get(i) {
            for id in consumed.iter() {
                self.pending.remove(id);
            }
        }
        let Some(set) = self.per_arm.get(i) else { return injected };
        let keys: SmallVec<[BindId; 8]> =
            self.pending.iter().filter(|id| set.contains_key(id)).copied().collect();
        for key in keys {
            self.pending.remove(&key);
            for id in set[&key].iter().copied() {
                let standing = match read_var(ctx, &id) {
                    // a live fire delivers itself
                    Some(VarRead::Delivered(tv)) if tv.tag().triggers() => None,
                    Some(VarRead::Delivered(tv) | VarRead::Standing(tv))
                        if !tv.tag().is_bottom() =>
                    {
                        Some(tv.value_cloned())
                    }
                    _ => None,
                };
                if let Some(v) = standing {
                    let prior = ctx.event.variables.put(id, TagValue::fired(v));
                    injected.push((id, prior));
                }
            }
        }
        injected
    }

    pub(crate) fn restore<E: UserEvent>(event: &mut Event<E>, injected: Injected) {
        for (id, prior) in injected {
            event.variables.put_back(id, prior)
        }
    }
}
