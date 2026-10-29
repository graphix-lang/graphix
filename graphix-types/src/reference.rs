//! A reference's runtime form: an abstract value holding its cell, which
//! no number and no other type's value can pass for, and what keeps the
//! cell and the place it names alive while the reference does.

use crate::{BindId, abstract_value, list, stack::ensure_sufficient};
use anyhow::anyhow;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{
    Abstract, FromValue, Value, abstract_type::AbstractWrapper, trace::Edge,
};
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    mem,
    sync::LazyLock,
};
use triomphe::Arc;

/// The references alive in a runtime: a reference keeps its cell, and the
/// binding it names, until its last copy drops ([`Keep`]).
#[derive(Debug, Default)]
pub struct Lives(Mutex<LivesState>);

#[derive(Debug, Default)]
struct LivesState {
    /// Per binding a reference names, how many keeps name it.
    pins: IntMap<BindId, u32>,
    /// Per cell whose keep names a binding, that binding.
    targets: IntMap<BindId, BindId>,
    /// Bindings deleted while pinned: their values go with the last pin.
    orphaned: IntSet<BindId>,
    /// Cells, and orphans unpinned, whose values may go.
    released: Vec<BindId>,
    /// An orphan was made or lost a pin since the last collection.
    dirty: bool,
    /// Cycles until an orphan a collection kept is looked at again, and
    /// the wait after that one, doubled at each collection that keeps
    /// one with nothing changed.
    countdown: u32,
    wait: u32,
}

impl LivesState {
    fn unpin(&mut self, t: BindId) {
        if let Some(n) = self.pins.get_mut(&t) {
            *n -= 1;
            if *n == 0 {
                self.pins.remove(&t);
                if self.orphaned.remove(&t) {
                    self.released.push(t);
                }
            } else if self.orphaned.contains(&t) {
                self.dirty = true;
            }
        }
    }

    /// The keep of `cell` names `target`, in place of what it named.
    fn name(&mut self, cell: BindId, target: BindId) {
        let old = self.targets.insert(cell, target);
        if old != Some(target) {
            *self.pins.entry(target).or_default() += 1;
            if let Some(o) = old {
                self.unpin(o)
            }
        }
    }
}

impl Lives {
    /// What every reference value of the cell `cell`, naming `target`,
    /// shares: the last one to drop releases them.
    pub fn keep(lives: &Arc<Self>, cell: BindId, target: Option<BindId>) -> Arc<Keep> {
        if let Some(t) = target {
            lives.0.lock().name(cell, t);
        }
        Arc::new(Keep { cell, lives: lives.clone() })
    }

    pub fn capture(lives: &Arc<Self>, ids: impl Iterator<Item = BindId>) -> Captures {
        let ids: Box<[BindId]> = ids.collect();
        let mut st = lives.0.lock();
        for id in &ids {
            *st.pins.entry(*id).or_default() += 1;
        }
        Captures { ids, lives: lives.clone() }
    }

    /// The binding `id` is deleted: true when a reference still names it,
    /// whose value then goes with the last such reference.
    pub fn orphan_if_pinned(&self, id: BindId) -> bool {
        let mut st = self.0.lock();
        let pinned = st.pins.contains_key(&id);
        if pinned {
            st.orphaned.insert(id);
            st.dirty = true;
        }
        pinned
    }

    /// Release the orphans nothing outside the orphans keeps: every pin
    /// of each is held by a reference or a function value that only
    /// orphans' values, and the cells such references own, hold. Run at a
    /// cycle's start, before [`Lives::released`]. `stored` hands its
    /// callback a binding's stored value, and `captures` the captures of a
    /// function value. An allocation whose owners are not all counted, a
    /// pin not found and anything an abstract this walk cannot read holds
    /// keep what they reach, so a collection only ever releases too little.
    pub fn collect(
        &self,
        mut stored: impl FnMut(BindId, &mut dyn FnMut(&Value)),
        captures: impl Fn(&Abstract) -> Option<&Captures>,
    ) {
        let orphans: LPooled<Vec<(BindId, u32)>> = {
            let mut st = self.0.lock();
            if st.orphaned.is_empty() {
                return;
            }
            if !st.dirty && st.countdown > 0 {
                st.countdown -= 1;
                return;
            }
            st.orphaned
                .iter()
                .map(|id| (*id, st.pins.get(id).copied().unwrap_or(0)))
                .collect()
        };
        let mut g = Graph::default();
        let mut pending: LPooled<Vec<BindId>> =
            orphans.iter().map(|(id, _)| *id).collect();
        while let Some(id) = pending.pop() {
            g.walk_bind(id, &mut stored, &captures, self, &mut pending);
        }
        let live = g.live(&orphans);
        let mut st = self.0.lock();
        let mut kept = false;
        for (id, _) in orphans.iter() {
            if live.binds.contains(id) {
                kept = true;
            } else if st.orphaned.remove(id) {
                st.released.push(*id);
            }
        }
        // the cells of the references collected: their values may hold the
        // references themselves, and go before the keeps do
        for id in g.cells.iter() {
            if !live.binds.contains(id) {
                st.released.push(*id);
            }
        }
        st.wait = if !kept {
            0
        } else if st.dirty {
            1
        } else {
            (st.wait * 2).clamp(1, 1024)
        };
        st.countdown = st.wait;
        st.dirty = false;
    }

    /// How many reference cells are alive: what a leak of them shows.
    #[doc(hidden)]
    pub fn cells(&self) -> usize {
        self.0.lock().targets.len()
    }

    /// The cells and orphaned bindings released since the last call, whose
    /// stored values may go; the lock is not held while `f` runs.
    pub fn released(&self, mut f: impl FnMut(BindId)) {
        loop {
            let released = mem::take(&mut self.0.lock().released);
            if released.is_empty() {
                break;
            }
            for id in released {
                f(id)
            }
        }
    }
}

/// The allocations under the orphans' values, for [`Lives::collect`].
#[derive(Default)]
struct Graph {
    allocs: IntMap<usize, Alloc>,
    /// Per binding walked (an orphan, or a cell a reference owns), the
    /// allocations its value holds.
    binds: IntMap<BindId, SmallVec<[usize; 2]>>,
    /// Per binding, how many distinct allocations found pin it.
    found: IntMap<BindId, u32>,
    /// The cells of the keeps walked.
    cells: IntSet<BindId>,
}

#[derive(Default)]
struct Alloc {
    strong: usize,
    /// Edges to it from what this walk reached.
    internal: usize,
    children: SmallVec<[usize; 2]>,
    /// The cells it owns (a keep's).
    owns: SmallVec<[BindId; 1]>,
    pins: SmallVec<[BindId; 1]>,
}

#[derive(Default)]
struct Live {
    allocs: IntSet<usize>,
    binds: IntSet<BindId>,
}

impl Graph {
    fn walk_bind(
        &mut self,
        id: BindId,
        stored: &mut impl FnMut(BindId, &mut dyn FnMut(&Value)),
        captures: &impl Fn(&Abstract) -> Option<&Captures>,
        lives: &Lives,
        cells: &mut Vec<BindId>,
    ) {
        if self.binds.contains_key(&id) {
            return;
        }
        self.binds.insert(id, SmallVec::new());
        stored(id, &mut |v| {
            let mut stack: LPooled<Vec<(Option<usize>, Edge<'_>)>> = LPooled::take();
            v.edges(|e| stack.push((None, e)));
            while let Some((parent, e)) = stack.pop() {
                let a = e.id();
                match parent {
                    None => self.binds.get_mut(&id).unwrap().push(a),
                    Some(p) => self.allocs.get_mut(&p).unwrap().children.push(a),
                }
                if let Some(n) = self.allocs.get_mut(&a) {
                    n.internal += 1;
                    continue;
                }
                let mut n = Alloc { strong: e.strong(), internal: 1, ..Alloc::default() };
                match e.as_abstract() {
                    None => e.children(|c| stack.push((Some(a), c))),
                    Some(abs) => {
                        if let Some(r) = abs.downcast_ref::<Reference>() {
                            if let Some(k) = &r._keep {
                                let kid = Arc::as_ptr(k) as usize;
                                n.children.push(kid);
                                self.keep(kid, k, lives, cells);
                            }
                        } else if let Some(x) =
                            abs.downcast_ref::<abstract_value::GxAbstract>()
                        {
                            x.payload.edges(|c| stack.push((Some(a), c)));
                        } else if let Some(c) = captures(abs) {
                            n.pins.extend(c.ids.iter().copied());
                            for t in c.ids.iter() {
                                *self.found.entry(*t).or_default() += 1;
                            }
                        }
                    }
                }
                self.allocs.insert(a, n);
            }
        });
    }

    /// The keep `k` at address `kid`, reached once more.
    fn keep(
        &mut self,
        kid: usize,
        k: &Arc<Keep>,
        lives: &Lives,
        cells: &mut Vec<BindId>,
    ) {
        if let Some(n) = self.allocs.get_mut(&kid) {
            n.internal += 1;
            return;
        }
        let mut n = Alloc { strong: Arc::count(k), internal: 1, ..Alloc::default() };
        n.owns.push(k.cell);
        self.cells.insert(k.cell);
        cells.push(k.cell);
        if let Some(t) = lives.0.lock().targets.get(&k.cell).copied() {
            n.pins.push(t);
            *self.found.entry(t).or_default() += 1;
        }
        self.allocs.insert(kid, n);
    }

    /// What something outside the walk keeps: an allocation with an
    /// uncounted owner, an orphan with an uncounted pin, and all they
    /// reach.
    fn live(&self, orphans: &[(BindId, u32)]) -> Live {
        enum Item {
            Alloc(usize),
            Bind(BindId),
        }
        let mut live = Live::default();
        let mut work: LPooled<Vec<Item>> = LPooled::take();
        for (a, n) in self.allocs.iter() {
            if n.strong > n.internal {
                work.push(Item::Alloc(*a))
            }
        }
        for (id, pins) in orphans {
            if *pins > self.found.get(id).copied().unwrap_or(0) {
                work.push(Item::Bind(*id))
            }
        }
        while let Some(i) = work.pop() {
            match i {
                Item::Alloc(a) => {
                    if live.allocs.insert(a)
                        && let Some(n) = self.allocs.get(&a)
                    {
                        work.extend(n.children.iter().map(|c| Item::Alloc(*c)));
                        work.extend(n.owns.iter().chain(&n.pins).map(|b| Item::Bind(*b)));
                    }
                }
                Item::Bind(b) => {
                    if live.binds.insert(b)
                        && let Some(cs) = self.binds.get(&b)
                    {
                        work.extend(cs.iter().map(|c| Item::Alloc(*c)));
                    }
                }
            }
        }
        live
    }
}

#[derive(Debug)]
pub struct Captures {
    ids: Box<[BindId]>,
    lives: Arc<Lives>,
}

impl Drop for Captures {
    fn drop(&mut self) {
        let mut st = self.lives.0.lock();
        for id in &self.ids {
            st.unpin(*id);
        }
    }
}

/// Shared by every reference value of one cell.
pub struct Keep {
    cell: BindId,
    lives: Arc<Lives>,
}

impl Keep {
    /// The cell names `target` now, a place whose root moved: the keep
    /// pins it in place of the binding it named.
    pub fn retarget(&self, target: BindId) {
        self.lives.0.lock().name(self.cell, target)
    }
}

impl fmt::Debug for Keep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Keep").field("cell", &self.cell).finish()
    }
}

impl Drop for Keep {
    fn drop(&mut self) {
        let mut st = self.lives.0.lock();
        st.released.push(self.cell);
        if let Some(t) = st.targets.remove(&self.cell) {
            st.unpin(t)
        }
    }
}

/// A reference, known by its cell; one minted by a `&` keeps the cell and
/// what it names alive.
#[derive(Clone)]
struct Reference {
    cell: BindId,
    _keep: Option<Arc<Keep>>,
}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        self.cell == other.cell
    }
}

impl Eq for Reference {}

impl PartialOrd for Reference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Reference {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cell.cmp(&other.cell)
    }
}

impl Hash for Reference {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cell.hash(state)
    }
}

impl fmt::Debug for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&ref")
    }
}

impl Pack for Reference {
    fn encoded_len(&self) -> usize {
        self.cell.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.cell.encode(buf)
    }

    /// Data never carries a reference, so bytes holding one are forged or
    /// stale: only an image session reads them.
    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !crate::ids::in_session() {
            return Err(PackError::InvalidFormat);
        }
        Ok(Reference { cell: BindId::decode(buf)?, _keep: None })
    }
}

static WRAPPER: LazyLock<AbstractWrapper<Reference>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0x3e, 0x71, 0x0a, 0xd4, 0x5c, 0x28, 0x4f, 0x9b, 0x86, 0x1e, 0xb2, 0x47, 0x0d,
        0x93, 0xe6, 0x5a,
    ]);
    Abstract::register::<Reference>(id).expect("failed to register Reference")
});

/// Register the wrapper with netidx before any value decodes: a value
/// read before the first reference is made must still downcast.
#[doc(hidden)]
pub fn register() {
    LazyLock::force(&WRAPPER);
}

/// The reference whose cell is `cell`, kept alive by `keep`.
pub fn reference(cell: BindId, keep: Option<Arc<Keep>>) -> Value {
    WRAPPER.wrap(Reference { cell, _keep: keep })
}

/// The cell of `v`, if `v` is a reference.
pub fn cell(v: &Value) -> Option<BindId> {
    match v {
        Value::Abstract(a) => a.downcast_ref::<Reference>().map(|r| r.cell),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn captures_release_orphans_after_the_last_owner() {
        let lives = Arc::new(Lives::default());
        let id = BindId::new();
        let first = Lives::capture(&lives, [id].into_iter());
        let last = Lives::capture(&lives, [id].into_iter());
        assert!(lives.orphan_if_pinned(id));
        drop(first);
        lives.released(|_| panic!("a capture still owns the binding"));
        drop(last);
        let mut released = 0;
        lives.released(|got| {
            assert_eq!(got, id);
            released += 1;
        });
        assert_eq!(released, 1);
    }

    #[test]
    fn reference_decodes_only_under_an_image() {
        let mut buf = Vec::new();
        Reference { cell: BindId::new(), _keep: None }.encode(&mut buf).unwrap();
        assert!(matches!(
            Reference::decode(&mut &buf[..]),
            Err(PackError::InvalidFormat)
        ));
    }

    #[test]
    fn a_retargeted_keep_pins_its_new_binding_alone() {
        let lives = Arc::new(Lives::default());
        let (cell, a, b) = (BindId::new(), BindId::new(), BindId::new());
        let keep = Lives::keep(&lives, cell, Some(a));
        keep.retarget(b);
        let mut released = Vec::new();
        assert!(!lives.orphan_if_pinned(a));
        assert!(lives.orphan_if_pinned(b));
        lives.released(|id| released.push(id));
        assert!(released.is_empty());
        drop(keep);
        lives.released(|id| released.push(id));
        assert_eq!(released, [cell, b]);
    }
}

/// A reference read as its cell.
impl FromValue for BindId {
    fn from_value(v: Value) -> anyhow::Result<Self> {
        cell(&v)
            .ok_or_else(|| anyhow!("expected a reference, got {}", crate::typ::Naked(&v)))
    }
}

#[cfg(test)]
thread_local! {
    pub(crate) static ORDER_CHECKS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Whether ordering this value can reach a reference outside an `Ord` implementation.
/// The caller lends its core-trait hooks.
pub fn orderable(v: &Value) -> bool {
    #[cfg(test)]
    ORDER_CHECKS.set(ORDER_CHECKS.get() + 1);
    ensure_sufficient(|| {
        if cell(v).is_some() {
            return false;
        }
        if let Some(g) = abstract_value::get(v) {
            return g.has_order_impl() || orderable(&g.payload);
        }
        if list::split(v).is_some() {
            return list::Iter::new(v.clone()).all(|v| orderable(&v));
        }
        match v {
            Value::Array(a) => a.iter().all(orderable),
            Value::Map(m) => m.into_iter().all(|(k, v)| orderable(k) && orderable(v)),
            Value::Error(e) => orderable(e),
            _ => true,
        }
    })
}

#[cfg(test)]
mod order_check_test {
    use super::*;
    use crate::{env::Env, typ::Type};
    use triomphe::Arc as TArc;

    #[test]
    fn typed_map_cast_skips_reference_walk() {
        let env = Env::default();
        let target = Type::Map {
            key: TArc::new(Type::Primitive(netidx_value::Typ::I64.into())),
            value: TArc::new(Type::Primitive(netidx_value::Typ::I64.into())),
        };
        let value = Value::Map([(Value::I64(1), Value::I64(2))].into_iter().collect());
        ORDER_CHECKS.set(0);
        assert_eq!(target.cast_value(&env, value.clone()), value);
        assert_eq!(ORDER_CHECKS.get(), 0);
    }
}
