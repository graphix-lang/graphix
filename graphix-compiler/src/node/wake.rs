//! Wake catch-up fire tracking (`design/wake_catchup.md`), shared by
//! the nodes that select among sleeping arms: `Select` and the seq
//! machine.

use super::{VarRead, read_var};
use crate::{BindId, Event, ExecCtx, Refs, Rt, Tag, TagValue, UserEvent, env::Env};
use nohash::{IntMap, IntSet};
use smallvec::SmallVec;

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
    /// The union of `per_arm`'s keys.
    all: IntSet<BindId>,
    /// Sound fires no arm evaluation has consumed yet, each with its
    /// tag: a real event, or a wake's own fire.
    pending: IntMap<BindId, Tag>,
}

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
            all: IntSet::default(),
            pending: IntMap::default(),
        };
        for i in 0..n {
            let mut r = Refs::default();
            fill(i, &mut r);
            Self::arm_refs(env, &r, &mut t.per_arm[i], &mut t.consumes[i]);
        }
        t.all.extend(t.per_arm.iter().flat_map(|m| m.keys().copied()));
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
    pub(crate) fn refresh(&mut self, env: &Env, i: usize, fill: impl FnOnce(&mut Refs)) {
        let Self { per_arm, consumes, all, pending } = self;
        let mut r = Refs::default();
        fill(&mut r);
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
        all.clear();
        all.extend(per_arm.iter().flat_map(|m| m.keys().copied()));
        pending.retain(|id, _| all.contains(id));
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
        let Self { all, pending, per_arm, .. } = self;
        // CR claude for claude: [bug/medium] A destructuring let's siblings are tracked
        // as one input under their representative (Env::facet_of, marked in
        // Bind::compile), but only the let's own publish delivers them together. A `<-`
        // or `&mut` write to one sibling fires that sibling alone. Writing the
        // representative while the arm sleeps sets the group's bit, and deliver then
        // injects FIRED into every sibling the woken arm reads, a phantom event.
        // Writing a non-representative sibling is never observed, because only the
        // representative is read here, so its catch-up is lost. With `let (a, b) = (0,
        // 0)` the outputs differ from `let a = 0; let b = 0` in both directions (probe:
        // design/review-2026-10-12/repro/c-bind-03.sh). Fix: in arm_refs, do not merge
        // a sibling into its group when it is a write target (ctx.connect_targets), or
        // observe each member id and set the group bit only for a joint publish. Add
        // pins for both directions beside let_sibling_binds_spent. (c-bind-03)
        for id in all.iter() {
            // CR claude for claude: [bug/medium] A carried `let` fire is re-recorded as
            // pending even when the step evaluated this cycle read it live. Because
            // TrackedFires::deliver runs on every evaluation of the current step, not
            // only at entry, a step that read the fire at its same-cycle entry and did
            // not complete gets it again next cycle as a phantom FIRED delivery. In the
            // probe, `a` fires once, but `until count(a) >= 2` completes one cycle
            // later, and `until a ~ (step > 3)` completes on step 4. This breaks the
            // "consumed by whichever step reads it, re-raised at its entry" rule of
            // design/seq_blocks.md §6.4. Fix: keep carried fires apart from ordinary
            // pending bits, for example a carried set that only a step being ENTERED
            // consumes (pass `entered` from seq_machine::evaluate into deliver), so the
            // step that read the fire live never sees it again. Pin the probe beside
            // seq_steps::let_fire_reaches_a_later_cycle. probe:
            // design/review-2026-10-12/repro/c-select-seq-02.gx (c-select-seq-02)
            if (carried.is_some_and(|c| c.contains(id))
                || !evaluated.iter().any(|i| per_arm[*i].contains_key(id)))
                && let Some(VarRead::Delivered(tv)) = read_var(ctx, id)
                && tv.tag().is_fired()
            {
                let tag = tv.tag().fresh_or_wake();
                let t = pending.entry(*id).or_insert(tag);
                if !tag.is_wake() {
                    *t = Tag::FIRED
                }
            }
        }
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
    ) -> SmallVec<[(BindId, Option<TagValue>); 4]> {
        let mut injected: SmallVec<[(BindId, Option<TagValue>); 4]> = SmallVec::new();
        if self.pending.is_empty() {
            return injected;
        }
        if let Some(consumed) = self.consumes.get(i) {
            for id in consumed.iter() {
                self.pending.remove(id);
            }
        }
        let Some(set) = self.per_arm.get(i) else { return injected };
        let keys: SmallVec<[(BindId, Tag); 8]> = self
            .pending
            .iter()
            .filter(|(id, _)| set.contains_key(id))
            .map(|(id, t)| (*id, *t))
            .collect();
        for (key, tag) in keys {
            self.pending.remove(&key);
            for id in set[&key].iter().copied() {
                let standing = match read_var(ctx, &id) {
                    // CR claude for claude: [bug/high] A live delivery tagged
                    // WAKE_FIRED also passes `triggers()`, so a pending real fire
                    // (Tag::FIRED) is spent here without being delivered as real. This
                    // happens when an enclosing arm's wake republishes a `let` that one
                    // of its constants fired over a stale input, in the same cycle this
                    // select wakes the arm that slept through that let's real fire. A
                    // `<-` target under the arm then keeps its last write, and the
                    // event is lost. The same event is caught up when the outer arm did
                    // not sleep, or when the let has no constant. Fix: skip the
                    // injection only when the live tag is at least as strong as the
                    // pending one. When the live tag is a wake's own and the pending
                    // one is FIRED, inject the live value as Tag::FIRED and record it
                    // for restore. Pin it in lang::select beside the wake pins. Probe:
                    // design/review-2026-10-12/repro/x-engine-firing-01.sh. In the
                    // sleep variant t stays 99; the awake and no-constant twins give 7
                    // and 14. (x-engine-firing-01)
                    Some(VarRead::Delivered(tv)) if tv.tag().triggers() => None,
                    Some(VarRead::Delivered(tv) | VarRead::Standing(tv))
                        if !tv.tag().is_bottom() =>
                    {
                        Some(tv.value_cloned())
                    }
                    _ => None,
                };
                if let Some(v) = standing {
                    let prev = ctx.event.variables.insert(id, TagValue::tagged(v, tag));
                    injected.push((id, prev));
                }
            }
        }
        injected
    }

    pub(crate) fn restore<E: UserEvent>(
        event: &mut Event<E>,
        injected: SmallVec<[(BindId, Option<TagValue>); 4]>,
    ) {
        for (id, prev) in injected {
            match prev {
                Some(tv) => {
                    // CR claude for claude: [risk/low] In a forked branch, `deliver`'s
                    // `prev` may be the parent layer's entry: `Layered::insert` returns
                    // a clone of it. Re-inserting it here leaves the branch an own
                    // entry that only copies the parent's. `merge_in_order` treats
                    // every own id as a delivery, so if two sibling branches both catch
                    // up the same id, the right one's copy is requeued as a write for
                    // the next cycle, a phantom fire. The module's primes are removed
                    // for the same reason. Fix: remember in `deliver` whether the
                    // previous value was the layer's own, and in that case restore it
                    // with `take_own` instead of re-inserting it. Not reproduced yet.
                    // (c-analysis-branch-06)
                    event.variables.insert(id, tv);
                }
                None => {
                    event.variables.remove(&id);
                }
            }
        }
    }
}
