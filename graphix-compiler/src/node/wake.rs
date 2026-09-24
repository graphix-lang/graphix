//! Wake catch-up fire tracking (`design/wake_catchup.md`), shared by
//! the nodes that select among sleeping arms: `Select` and the seq
//! machine.

use super::{VarRead, read_var};
use crate::{BindId, Event, ExecCtx, Refs, Rt, TagValue, UserEvent, env::Env};
use nohash::{IntMap, IntSet};
use smallvec::SmallVec;

/// Wake-catch-up fire tracking (design/wake_catchup.md): one fire bit
/// per arm-body input, set when the input fires and consumed by the
/// arm evaluation that reads it, so a woken arm receives exactly the
/// fires no selected reader saw, once, at the current standing value.
/// Guards, the scrutinee and pattern binds (of any enclosing select)
/// are not tracked. Survives sleep and `reset_replay`; frames excluded.
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
    /// Sound fires no arm evaluation has consumed yet.
    pending: IntSet<BindId>,
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
    pub(crate) fn new(
        env: &Env,
        n: usize,
        mut fill: impl FnMut(usize, &mut Refs),
    ) -> Self {
        let mut t = TrackedFires {
            per_arm: (0..n).map(|_| IntMap::default()).collect(),
            consumes: (0..n).map(|_| IntSet::default()).collect(),
            all: IntSet::default(),
            pending: IntSet::default(),
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
    pub(crate) fn refresh(&mut self, env: &Env, i: usize, fill: impl FnOnce(&mut Refs)) {
        let Self { per_arm, consumes, all, pending } = self;
        let mut r = Refs::default();
        fill(&mut r);
        Self::arm_refs(env, &r, &mut per_arm[i], &mut consumes[i]);
        all.clear();
        all.extend(per_arm.iter().flat_map(|m| m.keys().copied()));
        pending.retain(|id| all.contains(id));
    }

    /// Record this cycle's sound fires of tracked inputs. Runs before
    /// routing, so the taken arm consumes same-cycle fires immediately
    /// and no-arm cycles accumulate them for a future waker.
    pub(crate) fn observe<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<R, E>,
        event: &Event<E>,
    ) {
        self.observe_except(ctx, event, &[], None)
    }

    /// [`Self::observe`] after the arms `evaluated` ran this cycle: the
    /// fires their evaluation made are recorded for the arms that did not
    /// run; what an evaluated arm reads it saw, except the `carried`
    /// inputs (a seq step's `let`s), whose fire is also the entry of the
    /// first arm to read them in a later cycle.
    pub(crate) fn observe_except<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<R, E>,
        event: &Event<E>,
        evaluated: &[usize],
        carried: Option<&IntSet<BindId>>,
    ) {
        if ctx.frame_depth > 0 {
            return;
        }
        let Self { all, pending, per_arm, .. } = self;
        for id in all.iter() {
            if !pending.contains(id)
                && (carried.is_some_and(|c| c.contains(id))
                    || !evaluated.iter().any(|i| per_arm[*i].contains_key(id)))
                && let Some(VarRead::Delivered(tv)) = read_var(ctx, event, id)
                && tv.tag().is_fired()
                && !tv.tag().is_bottom()
            {
                pending.insert(*id);
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
        ctx: &ExecCtx<R, E>,
        event: &mut Event<E>,
        i: usize,
    ) -> SmallVec<[(BindId, Option<TagValue>); 4]> {
        let mut injected: SmallVec<[(BindId, Option<TagValue>); 4]> = SmallVec::new();
        if ctx.frame_depth > 0 || self.pending.is_empty() {
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
                let standing = match read_var(ctx, event, &id) {
                    Some(VarRead::Delivered(_)) => None,
                    Some(VarRead::Standing(tv)) if !tv.tag().is_bottom() => {
                        Some(tv.value_cloned())
                    }
                    _ => None,
                };
                if let Some(v) = standing {
                    let prev = event.variables.insert(id, TagValue::fired(v));
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
                    event.variables.insert(id, tv);
                }
                None => {
                    event.variables.remove(&id);
                }
            }
        }
    }
}
