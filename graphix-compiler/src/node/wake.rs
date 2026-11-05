//! Wake catch-up fire tracking (`design/wake_catchup.md`), shared by
//! the nodes that pause bodies and wake them: `Select`, the seq machine,
//! a collection whose source went bottom and a call whose function did.

use super::{VarRead, read_var};
use crate::{
    BindId, Event, ExecCtx, Refs, Rt, TagValue,
    branch::Prior,
    env::Env,
    prune::{BoundaryRef, Reader},
};
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
    ran: ArmMask,
    /// Per arm: where it stands in time.
    clock: Vec<ArmClock>,
}

/// One bit per arm; inline up to 64 arms.
#[derive(Debug, Default)]
pub(crate) struct ArmMask(SmallVec<[u64; 1]>);

impl ArmMask {
    pub(super) fn new(arms: usize) -> Self {
        let mut mask = Self::default();
        mask.clear(arms);
        mask
    }

    pub(super) fn clear(&mut self, arms: usize) {
        self.0.clear();
        self.0.resize(arms.div_ceil(64), 0);
    }

    /// Bit i = the i-th of `bits`.
    pub(super) fn of(bits: impl ExactSizeIterator<Item = bool>) -> Self {
        let mut mask = Self::new(bits.len());
        for (i, b) in bits.enumerate() {
            if b {
                mask.set(i)
            }
        }
        mask
    }

    pub(super) fn set(&mut self, i: usize) {
        self.0[i / 64] |= 1 << (i % 64);
    }

    /// Set bit `i`, saying whether it was set.
    pub(super) fn replace(&mut self, i: usize) -> bool {
        let was = self.get(i);
        self.set(i);
        was
    }

    pub(super) fn unset(&mut self, i: usize) {
        self.0[i / 64] &= !(1 << (i % 64));
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.iter().all(|w| *w == 0)
    }

    pub(super) fn get(&self, i: usize) -> bool {
        self.0.get(i / 64).is_some_and(|w| w & (1 << (i % 64)) != 0)
    }
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
            ran: ArmMask::new(n),
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
        if self.ran.replace(i) {
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

    // CR claude for claude: [risk/low] Only a select's arm bodies take a late fire they
    // slept through: a guard's reader has the select's own boundary, which tells no arm
    // apart, and the seq machine, a collection over a bottom source and a call over a bottom
    // function never call observe_late, so a late fire to a reader they hold asleep is lost
    // at the wake as it was for select arms before late-asleep-01. Unprobed. (late-asleep-02)
    /// Record each late fire ([`crate::Event::late`]) to a reader inside
    /// an arm `arm(i)` names by its boundary: the walk passed the arm when
    /// the fire was made, so the arm owes it if it does not run this cycle.
    pub(crate) fn observe_late<'a, R: Rt>(
        &mut self,
        ctx: &ExecCtx<'_, R>,
        arm: impl Fn(usize) -> Option<&'a BoundaryRef>,
    ) {
        let (Some(late), Some(root)) = (&ctx.event.late, ctx.event.root) else {
            return;
        };
        let Self { all, pending, per_arm, .. } = self;
        for (id, (_, readers)) in late.iter() {
            if !all.keys.contains(id)
                || pending.contains(id)
                || ctx.event.variables.contains_key(id)
            {
                continue;
            }
            let inside = |r: &Reader| {
                r.root == root
                    && per_arm.iter().enumerate().any(|(i, ids)| {
                        ids.contains(id)
                            && arm(i).is_some_and(|b| b.holds(r.boundary.as_ref()))
                    })
            };
            if readers.iter().any(inside) {
                if let crate::branch::RtView::Fork(f) = &ctx.rt {
                    f.note_read(*id);
                }
                Arc::make_mut(pending).insert(*id);
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
