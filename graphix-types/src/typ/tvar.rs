use crate::{
    AddrMap, AddrSet, LambdaId,
    env::{Bind, Env},
    expr::{ModPath, Origin, TypeDefBody},
    image,
    stack::ensure_sufficient,
    typ::{
        Bound, FnType, Mutability, NodeKey, NodePairs, PRINT_FLAGS, PrintFlag, RefKey,
        TVarsSeen, Type,
        contains::{cell_admits, find_open_cell},
        node_addr,
        setops::{union_identical, written_identical},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags};
use netidx_core::pack::{self, Pack, PackError};
use nohash::IntSet;
use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    collections::hash_map::Entry,
    fmt::{self, Debug},
    hash::Hash,
    mem::{self, ManuallyDrop},
    ops::{ControlFlow, Deref},
    sync::{
        self,
        atomic::{self, AtomicU64},
    },
};
use triomphe::Arc;

image_id!(TVarId);

#[cfg(test)]
thread_local! {
    pub(crate) static MAY_BE_ERROR_NAMES: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    pub(super) static SPLIT_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    static ANY_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    pub(super) static NARROW_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    pub(super) static CHAIN_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    static CYCLE_CELLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Advanced by every change to a bound cell that can make it reach a cell
/// it did not (a binding replaced, a constraint or lower bound added, an
/// unbind): only such a change can make a type that reached no open cell
/// reach one, so a cell's [`TCell::ground`] holds while the epoch that
/// proved it stands. A normal form's write-back reaches only its source's
/// cells and leaves it.
static GROUND_EPOCH: AtomicU64 = AtomicU64::new(1);

fn ground_epoch() -> u64 {
    GROUND_EPOCH.load(atomic::Ordering::SeqCst)
}

/// A bound cell is about to change.
fn bound_cell_changes() {
    GROUND_EPOCH.fetch_add(1, atomic::Ordering::SeqCst);
}

// CR claude for claude: [structure/low] This file interleaves five mechanisms that
// share only `TCell`'s fields: the ground proofs and the `ReachOrder`, `CycleWalk` and
// `BarePath` walks, the task records (`OwnWrites`, `decided_of`), `Level`, the
// settled-⊥ diagnostics and `TVar`'s own operations. A cell mutation must pass three
// guards that live in three of them (an occurs walk, `decided`, `bound_cell_changes`),
// and three mutators were found skipping one (`merge_one`'s lower bounds,
// `add_cell_constraint`, `closes_cycle`). Move the reachability walks to `typ/reach.rs`
// and the task records to `typ/task.rs` as plain file moves, leaving here the cell and
// its mutators. Found by reading; not run. (t-tvar-a-08)
/// The cells one compile epoch made, and their one owner: every other
/// reference to one keeps nothing ([`CellLink::Epoch`]), so the cycles
/// their bounds form cost nothing. What keeps the epoch's work (the env
/// it made, the function values it made) keeps the arena, and the cells
/// go with it.
pub struct Arena {
    cells: Mutex<Vec<sync::Arc<CellLock>>>,
}

pub type ArenaRef = sync::Arc<Arena>;

impl Arena {
    pub fn new() -> ArenaRef {
        ArenaRef::new(Arena { cells: Mutex::new(Vec::new()) })
    }

    /// Drop the cells nothing links to: what counting references would
    /// free. A cell only the arena holds can never be reached again, since
    /// a new link is made from a held cell. Dropping one can leave others
    /// so, hence the passes.
    pub fn sweep(&self) {
        let mut cells = self.cells.lock();
        loop {
            let n = cells.len();
            cells.retain_mut(|c| sync::Arc::get_mut(c).is_none());
            if cells.len() == n {
                break;
            }
        }
    }
}

impl fmt::Debug for Arena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Arena({} cells)", self.cells.lock().len())
    }
}

thread_local! {
    /// The arena of the epoch this thread compiles in: the cells made here
    /// are its.
    static ARENA: RefCell<Option<ArenaRef>> = const { RefCell::new(None) };
}

/// The arena of the epoch this thread compiles in, if any.
pub fn current_arena() -> Option<ArenaRef> {
    ARENA.with_borrow(|a| a.clone())
}

/// This thread compiles in `arena` until the guard drops, which restores
/// the arena it compiled in before.
pub struct InArena(Option<ArenaRef>);

impl InArena {
    pub fn enter(arena: ArenaRef) -> Self {
        InArena(ARENA.with_borrow_mut(|a| a.replace(arena)))
    }
}

impl Drop for InArena {
    fn drop(&mut self) {
        let prev = self.0.take();
        ARENA.with_borrow_mut(|a| *a = prev)
    }
}

/// A cell, kept for as long as this is. A cell made outside any epoch is
/// its own (a written type's variable, as the parser makes it, which no
/// check bounds); a var links to it owning it. A cell an epoch made is its
/// arena's, and a var links to it keeping nothing.
#[derive(Clone)]
#[doc(hidden)]
pub enum CellRef {
    Own(sync::Arc<CellLock>),
    Epoch(sync::Arc<CellLock>),
}

impl Deref for CellRef {
    type Target = CellLock;

    fn deref(&self) -> &CellLock {
        match self {
            CellRef::Own(c) | CellRef::Epoch(c) => c,
        }
    }
}

impl CellRef {
    /// A new cell, the current epoch's where there is one.
    pub(crate) fn new(cell: CellLock) -> Self {
        let cell = sync::Arc::new(cell);
        match current_arena() {
            Some(a) => {
                a.cells.lock().push(cell.clone());
                CellRef::Epoch(cell)
            }
            None => CellRef::Own(cell),
        }
    }

    pub(crate) fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.data_ptr() == b.data_ptr()
    }

    /// How a var reaches this cell.
    pub(crate) fn link(&self) -> CellLink {
        match self {
            CellRef::Own(c) => CellLink::Own(c.clone()),
            CellRef::Epoch(c) => CellLink::Epoch(sync::Arc::downgrade(c)),
        }
    }
}

/// How a var reaches its cell: its own cell it keeps, an epoch's cell it
/// keeps nothing of.
#[derive(Debug)]
pub(crate) enum CellLink {
    Own(sync::Arc<CellLock>),
    Epoch(sync::Weak<CellLock>),
}

/// A link's cell for as long as this lives: borrowed where the link keeps
/// it, held where the link keeps nothing.
pub(crate) enum CellGuard<'a> {
    Borrowed(&'a sync::Arc<CellLock>),
    Held(sync::Arc<CellLock>),
}

impl Deref for CellGuard<'_> {
    type Target = CellLock;

    fn deref(&self) -> &CellLock {
        match self {
            CellGuard::Borrowed(c) => c,
            CellGuard::Held(c) => c,
        }
    }
}

impl CellGuard<'_> {
    pub(crate) fn to_ref(&self) -> CellRef {
        match self {
            CellGuard::Borrowed(c) => CellRef::Own((*c).clone()),
            CellGuard::Held(c) => CellRef::Epoch(c.clone()),
        }
    }
}

impl CellLink {
    /// The cell. An epoch's cell that is gone is a holder of its arena
    /// missing: a bug.
    pub(crate) fn get(&self) -> CellGuard<'_> {
        match self {
            CellLink::Own(c) => CellGuard::Borrowed(c),
            CellLink::Epoch(w) => CellGuard::Held(
                w.upgrade().expect("a cell is read after its epoch's arena was dropped"),
            ),
        }
    }

    /// The cell, kept for as long as the result is.
    pub(crate) fn upgrade(&self) -> CellRef {
        self.get().to_ref()
    }
}

/// Whether no open cell is reachable from `t` through the bindings,
/// constraints and lower bounds [`would_cycle_inner`] walks: then `t`
/// reaches no open cell's address. A cell proved ground at `epoch` is not
/// walked again; each bound cell walked is pushed to `walked`, and when
/// the answer is true every one of them reaches no open cell either.
fn reaches_no_open_cell(
    t: &Type,
    epoch: u64,
    seen: &mut AddrSet,
    walked: &mut Vec<CellRef>,
) -> bool {
    ensure_sufficient(|| {
        if let Some(node) = node_addr(t)
            && !seen.insert(node)
        {
            return true;
        }
        match t {
            Type::Ref(r) => {
                r.params.iter().all(|p| reaches_no_open_cell(p, epoch, seen, walked))
            }
            Type::TVar(tv) => {
                let cell = tv.cell();
                if !seen.insert(cell.data_ptr().addr()) {
                    return true;
                }
                let (binding, cons, below) = {
                    let c = cell.read();
                    match &c.binding {
                        None => return false,
                        Some(_) if c.ground == epoch => return true,
                        Some(b) => (b.clone(), c.constraints.clone(), c.lower.clone()),
                    }
                };
                walked.push(cell);
                reaches_no_open_cell(&binding, epoch, seen, walked)
                    && cons
                        .iter()
                        .chain(below.iter())
                        .all(|c| reaches_no_open_cell(c, epoch, seen, walked))
            }
            t => t
                .try_for_each_child(&mut |c| match reaches_no_open_cell(
                    c, epoch, seen, walked,
                ) {
                    true => ControlFlow::Continue(()),
                    false => ControlFlow::Break(()),
                })
                .is_continue(),
        }
    })
}

/// Mark each of `cells`, if bound, as reaching no open cell at `epoch`,
/// unless the epoch moved since the walk that proved them read it.
fn mark_ground(cells: &mut Vec<CellRef>, epoch: u64) {
    if ground_epoch() != epoch {
        return;
    }
    for cell in cells.drain(..) {
        let mut c = cell.0.write();
        if c.binding.is_some() && c.ground < epoch {
            c.ground = epoch;
            c.kept = Kept::default();
            c.resolved = None;
        }
    }
}

/// What [`TVar::ground_judgment`] found of a cell.
enum Ground {
    /// Not proved ground now.
    Unproved,
    /// Ground, and judged: whether the judgment found something. A
    /// [`TVar::ground_verdict`] keeps only what found nothing.
    Kept(bool),
    /// Ground, its binding not judged so.
    Unjudged { cell: CellRef, binding: Type, epoch: u64 },
}

/// Keep judgment `j` of `cell`'s ground binding, made at `epoch`, while
/// that proof stands.
fn keep_judgment(cell: &CellLock, epoch: u64, j: Judgment, found: bool) {
    if ground_epoch() != epoch {
        return;
    }
    let mut c = cell.0.write();
    if c.ground == epoch {
        match found {
            true => c.kept.found |= j,
            false => c.kept.clear |= j,
        }
    }
}

/// One numbering of cells by what they reach through the edges
/// [`would_cycle_inner`] walks (bindings, conjuncts, lower bounds). While
/// a cell's number stands, every cell it reaches holds a lower number of
/// the same numbering, standing too, and none of them has been written
/// since: a walk passes by such a cell unless the cell it looks for holds
/// a lower number of that numbering. So a chain of open cells is walked
/// once however many of its links ask, and a write to a link leaves the
/// links below it standing. Walks on several threads may number the same
/// cells: [`CycleWalk::number`], [`ReachOrder::take`] and
/// [`Numbering::met`] keep the invariant then.
#[derive(Debug, Default)]
pub(crate) struct ReachOrder(Mutex<Numbers>);

#[derive(Debug, Default)]
struct Numbers {
    next: u64,
    /// How many writes took a number.
    writes: u64,
    /// The numbers that stand no more, ascending and apart: from a
    /// written cell's number to the next number at a write, which every
    /// cell that reached it then lies in. Each range counts the writes
    /// joined in it, fewer in each range than in the one before: a range
    /// joins the one before it, and takes the numbers above it, once it
    /// counts as many, so a number given once falls to few such joins.
    written: SmallVec<[Written; 2]>,
}

#[derive(Debug, Clone, Copy)]
struct Written {
    from: u64,
    to: u64,
    writes: u32,
}

/// A cell's number in a [`ReachOrder`], taken from it by its next write
/// ([`CellLock::write`]).
#[derive(Debug, Clone)]
pub(crate) struct Place {
    by: Arc<ReachOrder>,
    n: u64,
}

impl Numbers {
    fn stands(&self, n: u64) -> bool {
        !self.written.iter().rev().take_while(|w| n < w.to).any(|w| w.from <= n)
    }
}

impl Place {
    /// Whether a cell that held `was` holds it still.
    fn same(was: Option<&Self>, now: Option<&Self>) -> bool {
        match (was, now) {
            (None, None) => true,
            (Some(w), Some(n)) => Arc::ptr_eq(&w.by, &n.by) && w.n == n.n,
            (None, Some(_)) | (Some(_), None) => false,
        }
    }
}

impl ReachOrder {
    /// The fewest cells a walk that met no numbering numbers.
    const MIN_NUMBERED: usize = 32;

    /// The first of `len` fresh numbers, each above every number given;
    /// none once a write took a number since `met` writes had
    /// ([`Self::standing`]): a cell the walk found standing may not be.
    fn take(&self, len: u64, met: Option<u64>) -> Option<u64> {
        let mut numbers = self.0.lock();
        if met.is_some_and(|writes| writes != numbers.writes) {
            return None;
        }
        let first = numbers.next;
        numbers.next += len;
        Some(first)
    }

    /// The cell numbered `n` was written.
    fn wrote(&self, n: u64) {
        let mut numbers = self.0.lock();
        let Numbers { next, writes, written } = &mut *numbers;
        *writes += 1;
        while written.last().is_some_and(|w| w.from >= n) {
            written.pop();
        }
        match written.last_mut() {
            Some(w) if w.to >= n => {
                w.to = *next;
                w.writes += 1
            }
            _ => written.push(Written { from: n, to: *next, writes: 1 }),
        }
        while let [.., before, last] = &mut written[..]
            && before.writes <= last.writes
        {
            before.to = *next;
            before.writes += last.writes;
            written.pop();
        }
    }

    /// How many writes have taken a number, where `n` stands.
    fn standing(&self, n: u64) -> Option<u64> {
        let numbers = self.0.lock();
        numbers.stands(n).then_some(numbers.writes)
    }
}

pub(super) fn would_cycle_inner(cell: &CellLock, t: &Type) -> bool {
    let mut walk = CycleWalk::new(cell, false);
    let found = walk.go(t, Toward::Any, false).is_break();
    if !found {
        walk.number()
    }
    found
}

/// [`would_cycle_inner`] toward the open cell `cell`: false at once
/// for a type that reaches no open cell, which its bound cells remember,
/// so a chain of bound cells is walked once however many cells ask.
pub(super) fn would_cycle_open(cell: &CellLock, t: &Type) -> bool {
    let epoch = ground_epoch();
    let mut walked: LPooled<Vec<CellRef>> = LPooled::take();
    if reaches_no_open_cell(t, epoch, &mut LPooled::take(), &mut walked) {
        mark_ground(&mut walked, epoch);
        return false;
    }
    would_cycle_inner(cell, t)
}

/// How a type reaches a cell ([`cycle_toward`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum Cycle {
    /// Through cells and unions alone: the cells are one type.
    Bare,
    /// Through a constructor's part (an element, a field, a payload, an
    /// argument, a referent, a parameter its typedef holds under one): no
    /// finite type holds itself there.
    Constructor,
}

/// The edges a walk takes from a cell, by the relation it checks. A
/// bind or a merge makes the cells one type, so it takes every edge. A
/// lower bound `t0 ⊇ t1` closes a cycle only down from `t1` (its binding
/// and lower bounds) or up from `t0` (its binding and conjuncts): a
/// contravariant part turns the walk round, and a part of unknown or no
/// variance (invariant, a typedef's parameter) takes every edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Toward {
    Any,
    Below,
    Above,
}

impl Toward {
    fn flip(self) -> Self {
        match self {
            Self::Any => Self::Any,
            Self::Below => Self::Above,
            Self::Above => Self::Below,
        }
    }

    /// The edges a walk this way takes from `cell`.
    fn edges(self, cell: &TCell) -> SmallVec<[Type; 4]> {
        let (up, down) = match self {
            Self::Any => (true, true),
            Self::Above => (true, false),
            Self::Below => (false, true),
        };
        let mut edges: SmallVec<[Type; 4]> = cell.binding.iter().cloned().collect();
        if up {
            edges.extend(cell.constraints.iter().cloned());
        }
        if down {
            edges.extend(cell.lower.iter().cloned());
        }
        edges
    }
}

/// What [`Type::open_over_any`] has done: the node pairs it walked whole
/// since it last bound a cell, and the cells it bound. `inside`: every
/// cell open inside a part that is `Any` is bound too
/// ([`Type::close_over_any`]); with `abstracts`, only the parameters of
/// the abstract types there ([`Type::widen_abstracts_over_any`]); with
/// `fns`, a part holding a function type there is refused.
#[derive(Default)]
struct AnyWalk {
    walked: NodePairs,
    binds: usize,
    inside: bool,
    abstracts: bool,
    fns: bool,
}

/// A cell a select arm's test holds inside an `Any` part, open once the
/// arm is checked, that cannot be `Any` ([`Type::close_over_any`]).
#[derive(Debug)]
pub struct OpenOverAny(pub TVar);

impl fmt::Display for OpenOverAny {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the type '{} is not decided over Any", ShortName(&self.0.name))
    }
}

impl std::error::Error for OpenOverAny {}

/// How `t` reaches the cell at `addr` by the edges `toward` takes, a
/// constructor path over a bare one.
pub(super) fn cycle_toward(cell: &CellLock, t: &Type, toward: Toward) -> Option<Cycle> {
    if !would_cycle_inner(cell, t) {
        return None;
    }
    let mut walk = CycleWalk::new(cell, true);
    match walk.go(t, toward, false) {
        ControlFlow::Break(()) => Some(Cycle::Constructor),
        ControlFlow::Continue(()) => walk.bare.then_some(Cycle::Bare),
    }
}

enum Visits<'a> {
    /// Each node and cell once, by its place on the walk's path, numbering
    /// the cells the walk leaves.
    Once(LPooled<AddrMap<u32>>, Numbering<'a>),
    /// Each node and cell once per direction and per being under a
    /// constructor: a visit in one state may reach the cell only in
    /// another.
    PerState(LPooled<AHashSet<(usize, u8)>>),
}

/// What a walk that ends without finding its cell numbers ([`ReachOrder`]).
struct Numbering<'a> {
    /// The cell the walk looks for, and the number it held when the walk
    /// began.
    target: Option<(&'a CellLock, Option<Place>)>,
    /// The nodes and cells the walk is inside, outermost first.
    path: SmallVec<[usize; 16]>,
    /// Each cell walked whose number did not stand, after every cell it
    /// reaches, with the number the walk read there.
    walked: SmallVec<[(CellRef, Option<Place>); 8]>,
    /// The numbering of the standing cells the walk met, and how many
    /// writes had taken a number of it at the first.
    met: Option<(Arc<ReachOrder>, u64)>,
    /// Whether the cells walked take numbers above those met: false once
    /// the walk meets a node or cell on its own path, or cells standing
    /// in two numberings.
    ordered: bool,
}

impl<'a> Numbering<'a> {
    fn toward(target: Option<&'a CellLock>) -> Self {
        Self {
            target: target.map(|cell| (cell, cell.read().place())),
            path: SmallVec::new(),
            walked: SmallVec::new(),
            met: None,
            ordered: true,
        }
    }

    /// The walk met a cell whose number `place` stands, at `writes` of
    /// its numbering; whether it may reach the target: the target holds a
    /// lower number of that numbering, or another thread's walk numbered
    /// it since this walk began.
    fn met(&mut self, place: &Place, writes: u64) -> bool {
        match &self.met {
            None => self.met = Some((place.by.clone(), writes)),
            Some((by, _)) if Arc::ptr_eq(by, &place.by) => (),
            Some(_) => self.ordered = false,
        }
        let Some((cell, was)) = &self.target else { return false };
        let below = |t: &Place| Arc::ptr_eq(&t.by, &place.by) && t.n < place.n;
        was.as_ref().is_some_and(below)
            || !Place::same(was.as_ref(), cell.read().place.as_deref())
    }
}

/// A walk from a type to a cell through bindings, conjuncts and lower
/// bounds. Untracked, it breaks at the first path and passes by a cell
/// whose number says it reaches no such path; tracking, it breaks at the
/// first path under a constructor, and it records a bare one.
struct CycleWalk<'a> {
    addr: usize,
    visits: Visits<'a>,
    bare: bool,
}

impl<'a> CycleWalk<'a> {
    fn new(cell: &'a CellLock, track: bool) -> Self {
        let visits = match track {
            false => Visits::Once(LPooled::take(), Numbering::toward(Some(cell))),
            true => Visits::PerState(LPooled::take()),
        };
        Self { addr: cell.data_ptr().addr(), visits, bare: false }
    }

    fn tracking(&self) -> bool {
        matches!(self.visits, Visits::PerState(_))
    }

    fn visit(&mut self, key: usize, toward: Toward, under: bool) -> bool {
        match &mut self.visits {
            Visits::Once(seen, numbering) => match seen.entry(key) {
                Entry::Vacant(e) => {
                    e.insert(numbering.path.len() as u32);
                    numbering.path.push(key);
                    true
                }
                Entry::Occupied(e) => {
                    if numbering.path.get(*e.get() as usize) == Some(&key) {
                        numbering.ordered = false
                    }
                    false
                }
            },
            Visits::PerState(seen) => seen.insert((key, toward as u8 * 2 + under as u8)),
        }
    }

    /// The walk leaves the node or cell it last visited.
    fn leave(&mut self) {
        if let Visits::Once(_, numbering) = &mut self.visits {
            numbering.path.pop();
        }
    }

    fn go(&mut self, t: &Type, toward: Toward, under: bool) -> ControlFlow<()> {
        ensure_sufficient(|| {
            let node = node_addr(t);
            if let Some(node) = node
                && !self.visit(node, toward, under)
            {
                return ControlFlow::Continue(());
            }
            self.parts(t, toward, under)?;
            if node.is_some() {
                self.leave()
            }
            ControlFlow::Continue(())
        })
    }

    fn parts(&mut self, t: &Type, toward: Toward, under: bool) -> ControlFlow<()> {
        // tracking, a constructor's parts are under it
        let parts_under = self.tracking();
        match t {
            Type::Set(ms) => ms.iter().try_for_each(|m| self.go(m, toward, under)),
            // a typedef body's free tvars are its params, so the params
            // reach every cell its expansion does
            Type::Ref(r) => {
                let def = parts_under.then(|| r.resolved()).flatten();
                r.params.iter().enumerate().try_for_each(|(i, p)| {
                    let guarded =
                        under || def.as_ref().is_some_and(|d| d.param_guarded(i));
                    self.go(p, Toward::Any, guarded)
                })
            }
            Type::TVar(tv) => self.cell(tv, toward, under),
            Type::Fn(ft) => {
                for a in ft.args.iter() {
                    self.go(&a.typ, toward.flip(), parts_under)?;
                }
                if let Some(v) = &ft.vargs {
                    self.go(v, toward.flip(), parts_under)?;
                }
                self.go(&ft.rtype, toward, parts_under)?;
                self.go(&ft.throws, toward, parts_under)
            }
            Type::Abstract { .. } | Type::App(..) | Type::ByRef(Mutability::Mut, _) => {
                t.try_for_each_child(&mut |c| self.go(c, Toward::Any, parts_under))
            }
            t => t.try_for_each_child(&mut |c| self.go(c, toward, parts_under)),
        }
    }

    fn cell(&mut self, tv: &TVar, toward: Toward, under: bool) -> ControlFlow<()> {
        let cell = tv.cell();
        let cell_addr = cell.data_ptr().addr();
        if cell_addr == self.addr {
            if under || !self.tracking() {
                return ControlFlow::Break(());
            }
            self.bare = true;
            return ControlFlow::Continue(());
        }
        if !self.visit(cell_addr, toward, under) {
            return ControlFlow::Continue(());
        }
        #[cfg(test)]
        CYCLE_CELLS.with(|n| n.set(n.get() + 1));
        let (edges, place) = {
            let c = cell.read();
            (toward.edges(&c), c.place())
        };
        // whether the cell's number stands, and it may then reach the target
        let stands = match (&mut self.visits, &place) {
            (Visits::Once(_, numbering), Some(place)) => {
                place.by.standing(place.n).map(|writes| numbering.met(place, writes))
            }
            _ => None,
        };
        if stands != Some(false) {
            edges.iter().try_for_each(|c| self.go(c, toward, under))?;
        }
        self.leave();
        if stands.is_none()
            && let Visits::Once(_, numbering) = &mut self.visits
        {
            numbering.walked.push((cell, place))
        }
        ControlFlow::Continue(())
    }

    /// Number the cells of a walk that reached its end: above the
    /// numbering it met, or anew where it met none and walked enough
    /// cells to be worth one. The thread wrote no cell since it read
    /// them, and no other thread writes a cell this one reads
    /// ([`cell_writes`]), but one may number it: a cell takes its number,
    /// the cells it reaches first, only while it holds the place the walk
    /// read there.
    fn number(&mut self) {
        let Visits::Once(_, numbering) = &mut self.visits else { return };
        if !numbering.ordered || numbering.walked.is_empty() {
            return;
        }
        let (by, met) = match numbering.met.take() {
            Some((by, writes)) => (by, Some(writes)),
            None if numbering.walked.len() >= ReachOrder::MIN_NUMBERED => {
                (Arc::new(ReachOrder::default()), None)
            }
            None => return,
        };
        let Some(mut n) = by.take(numbering.walked.len() as u64, met) else { return };
        for (cell, was) in numbering.walked.drain(..) {
            let mut cell = cell.0.write();
            if !Place::same(was.as_ref(), cell.place.as_deref()) {
                return;
            }
            let place = Place { by: by.clone(), n };
            match &mut cell.place {
                Some(held) => **held = place,
                None => cell.place = Some(Box::new(place)),
            }
            n += 1;
        }
    }
}

/// Where a cell belongs (`design/tvar_constraints.md`,
/// Generalization).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[doc(hidden)]
pub enum Level {
    /// The top level: shared by every use.
    Top,
    /// Owned by the definition `owner`, at its nesting depth (1 and up).
    Def { depth: u32, owner: LambdaId },
    /// A scheme's variable no definition owns; deepest of all.
    Generic,
    /// A variable a body's written type owns: generic until a cell of a
    /// definition or the top level merges into it, which claims it there.
    Written,
}

impl Level {
    #[doc(hidden)]
    pub const GENERIC: Self = Self::Generic;
    #[doc(hidden)]
    pub const TOP: Self = Self::Top;

    /// The nesting depth, deepest for [`Self::Generic`]: a cell claimed
    /// for a shallower level moves up.
    #[doc(hidden)]
    pub fn depth(&self) -> u32 {
        match self {
            Self::Top => 0,
            Self::Def { depth, .. } => *depth,
            Self::Generic | Self::Written => u32::MAX,
        }
    }

    /// Does no definition own the cell yet: a scheme's or a written one.
    #[doc(hidden)]
    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic | Self::Written)
    }

    /// The level of the definition `id` compiled now: one below the
    /// enclosing definition's.
    #[doc(hidden)]
    pub fn definition(id: LambdaId) -> Self {
        let depth = match current_level() {
            l if l.is_generic() => 1,
            l => l.depth() + 1,
        };
        Self::Def { depth, owner: id }
    }

    /// Does a call copy a cell at this level? A generic cell, and one a
    /// definition owns whose gate is not open: the definition's scheme.
    /// A top-level cell, and an open gate's, is shared.
    #[doc(hidden)]
    pub fn copied_by(&self, open: &IntSet<LambdaId>) -> bool {
        match self {
            Self::Generic | Self::Written => true,
            Self::Def { owner, .. } => !open.contains(owner),
            Self::Top => false,
        }
    }
}

impl Pack for Naming {
    fn encoded_len(&self) -> usize {
        1 + match self.by() {
            NamedBy::Taken(name) => name.encoded_len(),
            NamedBy::Own | NamedBy::CallCopy => 0,
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self.by() {
            NamedBy::Own => Ok(buf.put_u8(0)),
            NamedBy::CallCopy => Ok(buf.put_u8(1)),
            NamedBy::Taken(name) => {
                buf.put_u8(2);
                name.encode(buf)
            }
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(Self::OWN),
            1 => Ok(Self::call_copy()),
            2 => Ok(Self::taken(Pack::decode(buf)?)),
            _ => Err(PackError::UnknownTag),
        }
    }
}

impl Pack for Level {
    fn encoded_len(&self) -> usize {
        1 + match self {
            Self::Def { depth, owner } => depth.encoded_len() + owner.encoded_len(),
            Self::Top | Self::Generic | Self::Written => 0,
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Self::Top => Ok(buf.put_u8(0)),
            Self::Def { depth, owner } => {
                buf.put_u8(1);
                depth.encode(buf)?;
                owner.encode(buf)
            }
            Self::Generic => Ok(buf.put_u8(2)),
            Self::Written => Ok(buf.put_u8(3)),
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(Self::Top),
            1 => Ok(Self::Def { depth: u32::decode(buf)?, owner: Pack::decode(buf)? }),
            2 => Ok(Self::Generic),
            3 => Ok(Self::Written),
            _ => Err(PackError::UnknownTag),
        }
    }
}

thread_local! {
    static LEVEL: std::cell::Cell<Level> = const { std::cell::Cell::new(Level::GENERIC) };
    static TASK: std::cell::Cell<TaskId> = const { std::cell::Cell::new(TaskId::OUTSIDE) };
    /// The compile task whose foreign writes this thread records
    /// ([`OwnWrites`]).
    static OWN_WRITES: RefCell<Option<Record>> = const { RefCell::new(None) };
    static CELL_WRITES: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// How many writes this thread has made to cells and to vars' links: a
/// verdict this thread read from cells stands while the count does (a
/// compile task on another thread writes only cells it owns).
#[inline]
pub(super) fn cell_writes() -> u64 {
    CELL_WRITES.with(|n| n.get())
}

#[inline]
fn count_write() -> u64 {
    CELL_WRITES.with(|n| {
        n.set(n.get() + 1);
        n.get()
    })
}

/// A cell's lock: each write counts toward [`cell_writes`] and takes the
/// cell's [`Place`], whatever it changes.
#[derive(Debug, Default)]
pub struct CellLock(RwLock<TCell>);

impl CellLock {
    pub(crate) fn new(cell: TCell) -> Self {
        Self(RwLock::new(cell))
    }

    #[inline]
    pub(crate) fn read(&self) -> RwLockReadGuard<'_, TCell> {
        self.0.read()
    }

    #[inline]
    pub(crate) fn write(&self) -> RwLockWriteGuard<'_, TCell> {
        let writes = count_write();
        let mut cell = self.0.write();
        cell.written = writes;
        if let Some(place) = cell.place.take() {
            place.by.wrote(place.n)
        }
        cell
    }

    #[inline]
    pub(crate) fn data_ptr(&self) -> *mut TCell {
        self.0.data_ptr()
    }
}

/// A concurrent compile task: one started later is greater.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[doc(hidden)]
pub struct TaskId(u64);

impl TaskId {
    /// Outside every task.
    pub const OUTSIDE: Self = Self(0);
}

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

static TASKS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/// A fresh concurrent compile task's id.
#[doc(hidden)]
pub fn new_task() -> TaskId {
    TaskId(TASKS.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

/// Cells created while this lives belong to compile task `id`; dropping
/// it restores the enclosing task.
#[must_use]
#[doc(hidden)]
pub struct InTask(TaskId);

impl InTask {
    #[doc(hidden)]
    pub fn enter(id: TaskId) -> Self {
        Self(TASK.replace(id))
    }
}

/// The compile task this thread runs.
#[doc(hidden)]
pub fn current_task() -> TaskId {
    TASK.get()
}

impl Drop for InTask {
    fn drop(&mut self) {
        TASK.set(self.0)
    }
}

/// A compile's context on its thread: the task its cells belong to and
/// the level they take. A job on a shared pool runs under its spawner's
/// ([`Self::enter`]), never under the thread's: a worker blocked in a
/// join runs jobs stolen from any compile of its runtime, and a job that
/// ran under the waiting task's context would create cells as that
/// task's and have its decisions recorded as that task's foreign writes.
#[derive(Debug, Clone)]
#[doc(hidden)]
pub struct TaskContext {
    task: TaskId,
    level: Level,
    arena: Option<ArenaRef>,
}

impl TaskContext {
    /// The context this thread runs.
    #[doc(hidden)]
    pub fn current() -> Self {
        Self { task: TASK.get(), level: LEVEL.get(), arena: current_arena() }
    }

    /// This context as compile task `task`'s.
    #[doc(hidden)]
    pub fn with_task(self, task: TaskId) -> Self {
        Self { task, ..self }
    }

    /// Run as this context until the guard drops. The thread's record of
    /// foreign writes ([`OwnWrites`]) and the variables of the definitions
    /// it checks ([`Declared`]) are its waiting task's and are set aside
    /// meanwhile: a task's record is kept on the thread it runs whole on.
    #[doc(hidden)]
    pub fn enter(self) -> InContext {
        InContext {
            task: TASK.replace(self.task),
            level: LEVEL.replace(self.level),
            arena: ARENA.with_borrow_mut(|a| mem::replace(a, self.arena)),
            own: OWN_WRITES.take(),
            declared: DECLARED.take(),
        }
    }
}

/// The context a thread ran before [`TaskContext::enter`], restored when
/// this drops.
#[must_use]
#[doc(hidden)]
pub struct InContext {
    task: TaskId,
    level: Level,
    arena: Option<ArenaRef>,
    own: Option<Record>,
    declared: Declared,
}

impl Drop for InContext {
    fn drop(&mut self) {
        TASK.set(self.task);
        LEVEL.set(self.level);
        let arena = self.arena.take();
        ARENA.with_borrow_mut(|a| *a = arena);
        OWN_WRITES.set(self.own.take());
        DECLARED.set(mem::take(&mut self.declared));
    }
}

/// Whether an earlier compile task created `cell`: what such a task left
/// open, it decided open, and no settle of this one binds it.
pub(super) fn earlier_task(cell: &TCell) -> bool {
    cell.task < TASK.get()
}

/// A change to `cell` by the running compile task. A task changes only
/// cells it or a later task created: what an earlier one created (the
/// enclosing body's, a sibling's) the check settled, and a concurrent
/// sibling may be reading it.
pub(super) fn written(cell: &TCell) {
    written_of(cell.task, "cell", Some(cell_addr(cell)))
}

/// A rigid gate opened or closed on `cell` by the running task, reported
/// as [`written`] is, except by a top-level input's own check: inputs run
/// one at a time, so its hold of an earlier input's cell for one
/// comparison is seen by no other task.
fn gated(cell: &TCell) {
    if !in_input() {
        written(cell)
    }
}

/// A cell's identity while it lives, for naming what a write reached.
fn cell_addr(cell: &TCell) -> usize {
    cell as *const TCell as usize
}

/// A decision about a type (a binding, a conjunct, a merge, a mark) by
/// the running compile task, reported as [`written`] is. Under
/// [`OwnWrites`] a decision about what an earlier task created is
/// recorded and not made: every module of a run that would make it is
/// refused, whatever order they ran in, and none sees another's.
pub(super) fn decided(cell: &TCell) -> bool {
    decided_of(cell.task, "cell", Some(cell_addr(cell)), || None)
}

/// Whether `u` is a union that holds `t` as one of its members.
fn union_member(u: &Type, t: &Type) -> bool {
    matches!(u, Type::Set(s) if s.iter().any(|m| union_identical(m, t)))
}

/// [`decided`], where the decision would make the cell `would`.
pub(super) fn decided_as(cell: &TCell, would: &Type) -> bool {
    decided_of(cell.task, "cell", Some(cell_addr(cell)), || Some(would.clone()))
}

fn decided_of(
    owner: TaskId,
    what: &str,
    cell: Option<usize>,
    would: impl FnOnce() -> Option<Type>,
) -> bool {
    if owner >= TASK.get() {
        return true;
    }
    foreign_write(owner, what, cell, would);
    !OWN_WRITES.with_borrow(|r| r.as_ref().is_some_and(|r| r.task == TASK.get()))
}

fn written_of(owner: TaskId, what: &str, cell: Option<usize>) {
    if owner < TASK.get() {
        foreign_write(owner, what, cell, || None)
    }
}

/// The most cells a refusal names.
const MAX_FOREIGN: usize = 16;

/// A compile task's record of its writes outside itself ([`OwnWrites`]).
struct Record {
    task: TaskId,
    /// The task is a top-level input's ([`OwnWrites::enter_input`]).
    input: bool,
    /// The cells it wrote outside itself, in the order it first wrote
    /// them, or the one write that was no cell's.
    foreign: SmallVec<[ForeignWrite; 4]>,
}

/// While this lives, record whether the running compile task changes a
/// cell or var an earlier task created. A task runs whole on one
/// thread, and a task run while it waits keeps its own record.
#[must_use]
#[doc(hidden)]
pub struct OwnWrites(Option<Record>);

impl OwnWrites {
    #[doc(hidden)]
    pub fn enter(task: TaskId) -> Self {
        Self(OWN_WRITES.replace(Some(Record {
            task,
            input: false,
            foreign: SmallVec::new(),
        })))
    }

    /// [`Self::enter`] for a top-level input's task (a REPL input, a
    /// script, a dynamic load): see [`in_input`].
    #[doc(hidden)]
    pub fn enter_input(task: TaskId) -> Self {
        Self(OWN_WRITES.replace(Some(Record {
            task,
            input: true,
            foreign: SmallVec::new(),
        })))
    }

    /// The first write the task made outside itself.
    #[doc(hidden)]
    pub fn foreign(&self) -> Option<ForeignWrite> {
        OWN_WRITES.with_borrow(|r| r.as_ref().and_then(|r| r.foreign.first().cloned()))
    }

    /// The cells the task wrote outside itself, at most [`MAX_FOREIGN`],
    /// in the order it first wrote them.
    #[doc(hidden)]
    pub fn foreigns(&self) -> SmallVec<[ForeignWrite; 4]> {
        OWN_WRITES
            .with_borrow(|r| r.as_ref().map(|r| r.foreign.clone()).unwrap_or_default())
    }
}

/// Whether the running task is a top-level input's own check. There what
/// an earlier input left open, a function type's own quantifier included,
/// is one type, compared as itself and never as a fresh copy, so a
/// decision about it is recorded and not made.
pub(super) fn in_input() -> bool {
    OWN_WRITES
        .with_borrow(|r| r.as_ref().is_some_and(|r| r.input && r.task == TASK.get()))
}

/// A compile task's write to what an earlier task created: the cell, when
/// it was one.
#[derive(Clone)]
#[doc(hidden)]
pub struct ForeignWrite {
    cell: Option<usize>,
    /// What the write would have made the cell, where the write said.
    would: Option<Type>,
}

impl ForeignWrite {
    /// Whether `t` reaches the cell written.
    #[doc(hidden)]
    pub fn held_by(&self, t: &Type) -> bool {
        self.cell.is_some_and(|c| t.holds_cell(c))
    }

    /// `t` with the cell written made what the write would have made it:
    /// the type to declare for a binding holding it. `None` where the
    /// write did not say, or what it would have made is itself open.
    #[doc(hidden)]
    pub fn declared(&self, t: &Type) -> Option<Type> {
        let (cell, would) = (self.cell?, self.would.as_ref()?);
        let would = would.deref_cloned()?;
        t.with_cell_as(cell, &would)
    }
}

impl Drop for OwnWrites {
    fn drop(&mut self) {
        OWN_WRITES.set(self.0.take());
    }
}

#[cold]
fn foreign_write(
    owner: TaskId,
    what: &str,
    cell: Option<usize>,
    would: impl FnOnce() -> Option<Type>,
) {
    OWN_WRITES.with_borrow_mut(|r| {
        let Some(r) = r.as_mut().filter(|r| r.task == TASK.get()) else {
            return;
        };
        match r.foreign.iter_mut().find(|f| cell.is_some() && f.cell == cell) {
            Some(f) => {
                if f.would.is_none() {
                    f.would = would()
                }
            }
            None => {
                if r.foreign.len() == 1 && r.foreign[0].cell.is_none() && cell.is_some() {
                    r.foreign.clear()
                }
                if r.foreign.len() < MAX_FOREIGN
                    && (cell.is_some() || r.foreign.is_empty())
                {
                    r.foreign.push(ForeignWrite { cell, would: would() })
                }
            }
        }
    });
    if crate::dbgenv::graphix_task_audit() {
        eprintln!(
            "FOREIGN-WRITE by task {} to a {what} of task {owner}\n{}",
            TASK.get(),
            std::backtrace::Backtrace::force_capture()
        );
    }
}

/// The level a cell created now takes.
#[doc(hidden)]
pub fn current_level() -> Level {
    LEVEL.get()
}

/// Cells created while this lives take `level`; dropping it restores
/// the enclosing level.
#[must_use]
#[doc(hidden)]
pub struct AtLevel(Level);

impl AtLevel {
    #[doc(hidden)]
    pub fn enter(level: Level) -> Self {
        Self(LEVEL.replace(level))
    }
}

impl Drop for AtLevel {
    fn drop(&mut self) {
        LEVEL.set(self.0)
    }
}

/// Where each forwarding cell's chain of links ends, by cell address: the
/// address of [`TVar::open_cell`]'s answer, kept for chains longer than
/// [`ChainEnds::SHORT`]. Valid while no binding the chains pass changes,
/// so one copy's walk at most.
#[derive(Default)]
pub(super) struct ChainEnds(Option<LPooled<AHashMap<usize, Option<usize>>>>);

impl ChainEnds {
    pub(super) const SHORT: usize = 8;

    /// The address of `tv.open_cell()`.
    fn open_cell(&mut self, tv: &TVar) -> Option<usize> {
        let mut at = tv.clone();
        let mut walked = 0;
        let end = loop {
            let addr = at.cell_addr();
            if let Some(end) = self.0.as_ref().and_then(|m| m.get(&addr)) {
                break *end;
            }
            walked += 1;
            #[cfg(test)]
            CHAIN_STEPS.with(|n| n.set(n.get() + 1));
            let next = match &at.cell().read().binding {
                None => break Some(addr),
                Some(Type::TVar(next)) => next.clone(),
                Some(_) => break None,
            };
            at = next;
        };
        if walked > Self::SHORT {
            let memo = self.0.get_or_insert_with(LPooled::take);
            let mut at = tv.clone();
            for _ in 0..walked {
                memo.insert(at.cell_addr(), end);
                let next = match &at.cell().read().binding {
                    Some(Type::TVar(next)) => next.clone(),
                    _ => break,
                };
                at = next;
            }
        }
        end
    }
}

/// Where each chain of forwarding links longer than [`ChainEnds::SHORT`]
/// ends, by the address of its first cell, which it keeps alive, for walks
/// one owner makes while this thread writes no cell: forgotten at the next.
#[derive(Default)]
pub struct StandingEnds {
    at: u64,
    ends: Option<LPooled<AHashMap<usize, (CellRef, Option<usize>)>>>,
}

impl StandingEnds {
    /// The address of `tv.open_cell()`.
    pub(super) fn open_cell(&mut self, tv: &TVar) -> Option<usize> {
        let writes = cell_writes();
        if self.at != writes {
            if let Some(ends) = &mut self.ends {
                ends.clear()
            }
            self.at = writes;
        }
        let start = tv.cell();
        let addr = start.data_ptr().addr();
        if let Some((_, end)) = self.ends.as_ref().and_then(|e| e.get(&addr)) {
            return *end;
        }
        let mut walked = 0;
        let mut cell = start.clone();
        let end = loop {
            walked += 1;
            #[cfg(test)]
            CHAIN_STEPS.with(|n| n.set(n.get() + 1));
            let next = match &cell.read().binding {
                None => break Some(cell.data_ptr().addr()),
                Some(Type::TVar(next)) => next.cell(),
                Some(_) => break None,
            };
            cell = next;
        };
        if walked > ChainEnds::SHORT {
            self.ends.get_or_insert_with(LPooled::take).insert(addr, (start, end));
        }
        end
    }
}

/// A copy's walk state beside its images: where forwarding chains end.
pub(super) trait Forwards {
    fn ends(&mut self) -> &mut ChainEnds;

    /// The composites a copy's bindings have lowered ([`lower_in`]).
    fn lowered(&mut self) -> &mut Lowered;

    /// Enter (`true`) or leave the copy of a cell's binding, the one part
    /// of a copy whose composites many cells' bindings share.
    fn in_binding(&mut self, _entered: bool) {}
}

/// The composites of a type already lowered ([`lower_in`]), each with the
/// depth it was lowered to.
pub(super) type Lowered = LPooled<AHashSet<(usize, u32)>>;

/// What a walk rebuilds once, by address: each shared composite and each
/// bound cell's binding (a cell's address is another allocation, so the
/// two share the map); and the chain ends of its forwarding cells.
#[derive(Default)]
pub(super) struct CopyMemo {
    built: LPooled<AHashMap<usize, Option<Type>>>,
    ends: ChainEnds,
    lowered: Option<Lowered>,
}

impl Forwards for CopyMemo {
    fn ends(&mut self) -> &mut ChainEnds {
        &mut self.ends
    }

    fn lowered(&mut self) -> &mut Lowered {
        self.lowered.get_or_insert_with(LPooled::take)
    }
}

/// `rebuild` of `t`, made once per shared composite (`memo.built`).
fn once(
    t: &Type,
    memo: &mut CopyMemo,
    rebuild: impl FnOnce(&mut CopyMemo) -> Option<Type>,
) -> Option<Type> {
    let Some(n) = node_addr(t) else { return rebuild(memo) };
    if let Some(r) = memo.built.get(&n) {
        return r.clone();
    }
    let r = rebuild(memo);
    memo.built.insert(n, r.clone());
    r
}

/// The pairs of distinct open cells at one place in `a` and `b`, which
/// are equal ([`Type`]'s `PartialEq`, under which open cells are), into
/// `out`; false where a pair holds a rigid cell, which never merges.
fn open_pairs(a: &Type, b: &Type, out: &mut SmallVec<[(TVar, TVar); 2]>) -> bool {
    ensure_sufficient(|| match (a, b) {
        (Type::TVar(x), Type::TVar(y)) => match (x.binding(), y.binding()) {
            _ if x.cell_addr() == y.cell_addr() => true,
            (Some(a), Some(b)) => open_pairs(&a, &b, out),
            (None, None) if !x.is_rigid() && !y.is_rigid() => {
                out.push((x.clone(), y.clone()));
                true
            }
            _ => false,
        },
        (Type::TVar(x), b) => x.binding().is_some_and(|a| open_pairs(&a, b, out)),
        (a, Type::TVar(y)) => y.binding().is_some_and(|b| open_pairs(a, &b, out)),
        (a, b) => {
            let mut cs: SmallVec<[Type; 8]> = SmallVec::new();
            a.for_each_child(&mut |c| cs.push(c.clone()));
            let mut cs = cs.iter();
            let mut paired = true;
            b.for_each_child(&mut |c| {
                paired &= cs.next().is_some_and(|a| open_pairs(a, c, out))
            });
            paired
        }
    })
}

/// Lower every cell `t` reaches to at most `level`: what a cell at
/// `level` is bound to, or constrained by, belongs where the cell does.
/// A scheme's generic cell stays generic (a scheme bound into a cell is
/// still a scheme), and a cell already at or above `level`'s depth
/// reaches nothing deeper.
pub(super) fn lower(t: &Type, level: Level) {
    lower_in(t, level, &mut LPooled::take())
}

/// [`lower`], skipping the composites `seen` holds lowered to `level`'s
/// depth already: a walk that made them, and cells under them since, stand
/// lowered.
fn lower_in(t: &Type, level: Level, seen: &mut Lowered) {
    fn go(t: &Type, level: Level, seen: &mut Lowered) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if tv.level() != Level::Generic => tv.claim(level),
            Type::TVar(_) => (),
            t if node_addr(t).is_some_and(|n| !seen.insert((n, level.depth()))) => (),
            t => t.for_each_child(&mut |c| go(c, level, seen)),
        })
    }
    if !level.is_generic() {
        go(t, level, seen)
    }
}

/// How a copy of a type treats its cells: `Copy` makes a fresh cell per
/// cell at the source's level; `Instantiate` is a call's copy, a fresh cell at the
/// current level per cell a call copies ([`Level::copied_by`] the open
/// gates), every other cell shared; `Scheme` is `Instantiate` whose fresh
/// cells are generic, a reference's copy of the scheme it names; `Generic`
/// copies the generic cells alone, a stored scheme's, and shares every
/// live one.
#[derive(Clone, Copy)]
pub(crate) enum Fresh<'a> {
    Copy,
    Instantiate(&'a IntSet<LambdaId>),
    Scheme(&'a IntSet<LambdaId>),
    Generic,
}

/// Whose name a cell's handles carry: what a message may write a handle
/// as. One word: no name is the handles' own, the empty name a call's copy
/// (no variable is written so), any other the written variable that took
/// the copy.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Naming(Option<ArcStr>);

/// [`Naming`], read.
pub(crate) enum NamedBy<'a> {
    /// The name a handle carries is the cell's.
    Own,
    /// A call's copy of a cell ([`Fresh::Instantiate`]), or a copy of one:
    /// the name is the copied variable's, which the program wrote for
    /// another cell. A merge keeps it where no variable the program wrote
    /// names either cell.
    CallCopy,
    /// A call's copy a variable the program wrote took: every handle of
    /// the cell writes that variable's name, whatever name it carries.
    Taken(&'a ArcStr),
}

impl Naming {
    pub(crate) const OWN: Naming = Naming(None);

    pub(crate) fn call_copy() -> Self {
        Naming(Some(ArcStr::new()))
    }

    pub(crate) fn taken(name: ArcStr) -> Self {
        Naming(Some(name))
    }

    pub(crate) fn by(&self) -> NamedBy<'_> {
        match &self.0 {
            None => NamedBy::Own,
            Some(n) if n.is_empty() => NamedBy::CallCopy,
            Some(n) => NamedBy::Taken(n),
        }
    }

    /// What a cell copied from one named `self` and into one named `into`
    /// is: the copy of a call's copy stays one, and a written variable's
    /// name is the copy's too.
    fn copied_into(self, into: &Naming) -> Naming {
        match (self.by(), into.by()) {
            (NamedBy::CallCopy, _) | (_, NamedBy::CallCopy) => Self::call_copy(),
            (NamedBy::Taken(_), _) => self,
            (NamedBy::Own, _) => into.clone(),
        }
    }
}

/// The shared binding cell: aliased `TVar`s hold one `Arc` of this.
/// `constraints` is a conjunction — everything the cell is ever bound
/// to must be contained by every member (empty = unconstrained); each
/// bind site checks it where an `Env` exists.
#[derive(Debug)]
pub struct TCell {
    /// Written through [`Self::set_binding`] while a walk may watch.
    pub(crate) binding: Option<Type>,
    pub(crate) constraints: SmallVec<[Type; 1]>,
    /// What the cell is bounded by from below: everything it is ever
    /// bound to must contain every member, and a cell still open when it
    /// settles binds to their union ([`TVar::add_lower`]).
    pub(crate) lower: SmallVec<[Type; 1]>,
    /// What the checks learned of the cell ([`CellFact`]): every copy, merge
    /// and image carries the whole set.
    pub(crate) facts: BitFlags<CellFact>,
    /// Whose name the handles' names are ([`Naming`]). Copies and images
    /// carry it.
    pub(crate) naming: Naming,
    /// Nonzero while a declared (named) lambda tvar is inside its def's
    /// body check: a rigid unbound cell never binds, so the body must
    /// be well-typed for arbitrary 'a.
    pub(crate) rigid_gates: u32,
    /// Where the cell belongs ([`Level`]); everything it reaches is at
    /// its depth or shallower.
    pub(crate) level: Level,
    /// The compile task that created the cell ([`written`]).
    pub(crate) task: TaskId,
    /// The latest [`GROUND_EPOCH`] at which the cell, bound, was proved to
    /// reach no open cell; 0, never.
    pub(crate) ground: u64,
    /// The judgments of the ground binding the cell keeps while the proof
    /// at `ground` stands.
    pub(crate) kept: Kept,
    /// The ground binding resolved ([`TVar::ground_resolved`]), kept while
    /// the proof at `ground` stands and the binding is not written.
    pub(crate) resolved: Option<Box<Type>>,
    /// The cell's number by what it reaches, until its next write: boxed,
    /// so a cell no walk numbers pays one word for it.
    pub(crate) place: Option<Box<Place>>,
    /// The [`cell_writes`] count of the thread that last wrote the cell.
    pub(crate) written: u64,
    /// What the bounds solver keeps of the cell beside its bounds
    /// ([`super::bounds::Known`]), once it has something to keep.
    pub(crate) solving: Option<Box<super::bounds::Known>>,
}

// Every node-walked activation holds a cell.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(size_of::<TCell>() <= 216);

/// A judgment of a ground cell's binding the cell keeps
/// ([`TVar::ground_verdict`], [`TVar::ground_holds`]).
#[doc(hidden)]
#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Judgment {
    /// Whether a comparison of a value of it is ambiguous.
    Compared,
    /// Whether an ordering of a value of it is ambiguous.
    Ordered,
    /// [`Type::holds_fn`].
    HoldsFn,
    /// [`Type::holds_unknown`].
    HoldsUnknown,
    /// [`Type::holds_abstract`].
    HoldsAbstract,
    /// Whether a function or a type reference stands in it.
    MayHoldFn,
}

/// What a ground cell keeps of the judgments of its binding: those that
/// found nothing and those that found something.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Kept {
    clear: BitFlags<Judgment>,
    found: BitFlags<Judgment>,
}

/// A fact about a cell, beside its binding and constraints.
#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CellFact {
    /// A ⊥ was produced into the cell while it was unbound. Every other
    /// production binds a cell or bounds it from below, so one still open
    /// once its writers are checked, with no lower bound but ⊥, had only ⊥
    /// produced into it (`TVar::settle`).
    BottomFed,
    /// The cell is bound to ⊥ because something required it (`⊥ ⊇ 'x`:
    /// a `_` annotation, a `-> _` return): a fact the gate keeps, where a
    /// ⊥ nothing required is vacuous and reopens.
    BottomRequired,
    /// The end of the input that made the cell settled it
    /// ([`settle_input`](super::settle::settle_input)): what it holds is
    /// decided, so a ⊥ it holds is required of whatever a later input sends
    /// into it, and a dynamic load reopens nothing of it.
    Settled,
}

impl Default for TCell {
    fn default() -> Self {
        TCell {
            binding: None,
            constraints: SmallVec::new(),
            lower: SmallVec::new(),
            facts: BitFlags::empty(),
            naming: Naming::OWN,
            rigid_gates: 0,
            level: current_level(),
            task: TASK.get(),
            ground: 0,
            kept: Kept::default(),
            resolved: None,
            place: None,
            written: 0,
            solving: None,
        }
    }
}

/// An open rigid gate, holding the cell it counted on; dropping it
/// closes the gate. A merge may re-point the var to another cell before
/// the gate closes, and a rollback may undo the forward link the merge
/// left, so the close decrements this cell and not whatever the var
/// reads by then.
pub struct RigidGate {
    cell: CellRef,
}

impl Drop for RigidGate {
    fn drop(&mut self) {
        let mut cell = self.cell.write();
        gated(&cell);
        cell.rigid_gates = cell.rigid_gates.saturating_sub(1);
    }
}

/// `incoming` minus what `existing` (or an earlier incoming conjunct)
/// already holds, by strict identity: two conjuncts over distinct open
/// cells are different facts.
fn new_conjuncts(
    existing: &[Type],
    incoming: impl IntoIterator<Item = Type>,
) -> LPooled<Vec<Type>> {
    let mut out: LPooled<Vec<Type>> = LPooled::take();
    for c in incoming {
        if !existing.iter().chain(out.iter()).any(|e| union_identical(e, &c)) {
            out.push(c)
        }
    }
    out
}

impl TCell {
    fn bound(typ: Type) -> Self {
        TCell { binding: Some(typ), ..TCell::default() }
    }

    /// Bind the cell, or clear its binding with `None`.
    pub(crate) fn set_binding(&mut self, binding: Option<Type>) {
        self.binding = binding;
        self.resolved = None
    }

    fn place(&self) -> Option<Place> {
        self.place.as_deref().cloned()
    }

    /// Add `c` to the conjunction unless an identical member is present.
    /// The identity walk can reach this very cell, so this must not run
    /// under a held tvar/cell guard.
    pub(crate) fn add_constraint(&mut self, c: Type) {
        if !self.constraints.iter().any(|e| union_identical(e, &c)) {
            self.constraints.push(c)
        }
    }
}

/// The note a refusal over `types` ends with where they reach a cell an
/// input's end settled to ⊥: the binding a later input can name that
/// holds the cell, made first, to declare.
pub fn settled_note(env: &Env, types: &[&Type]) -> Option<CompactString> {
    let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    for t in types {
        t.reached_cells(&mut cells);
    }
    let settled: SmallVec<[usize; 4]> = cells
        .values()
        .filter(|tv| tv.settled_bottom())
        .map(|tv| tv.cell_addr())
        .collect();
    if settled.is_empty() {
        return None;
    }
    let b = (&env.lexical.binds)
        .into_iter()
        .flat_map(|(_, names)| names.into_iter().map(|(_, id)| *id))
        .filter_map(|id| env.by_id.get(&id))
        .filter(|b| {
            !crate::is_lowered(&b.name) && settled.iter().any(|c| b.typ.holds_cell(*c))
        })
        .min_by_key(|b| b.id)?;
    Some(format_compact!(
        "it holds the type of `{}` ({}), settled when its input ended; declare {}'s \
         type where it is bound",
        b.name,
        BoundAt(b, None),
        b.name
    ))
}

/// [`settled_note`] for a refusal of `container` to hold `contained`,
/// where a settled ⊥ is its cause ([`settled_cause`]); `exact` where the
/// two must be one type.
pub fn settled_cause_note(
    env: &Env,
    container: &Type,
    contained: &Type,
    exact: bool,
) -> Option<CompactString> {
    match settled_cause(container, contained, exact) {
        true => settled_note(env, &[container, contained]),
        false => None,
    }
}

/// Whether a ⊥ an input's end settled is where `container` fails to hold
/// `contained`: a settled part of the container where the other has a
/// value, or a settled part of either where the two must be one type (a
/// `&mut`'s referent, an abstract type's parameter, everywhere when
/// `exact`). A refusal elsewhere is not the settled ⊥'s, and declaring
/// its binding would not lift it. Through a union or a named type every
/// settled part counts.
fn settled_cause(container: &Type, contained: &Type, exact: bool) -> bool {
    ensure_sufficient(|| {
        let settled = |t: &Type| matches!(t, Type::TVar(tv) if tv.settled_bottom());
        match (settled(container), settled(contained)) {
            (true, true) => return false,
            (true, false) => {
                return contained.with_deref(|t| !matches!(t, Some(Type::Bottom)));
            }
            (false, true) => return exact,
            (false, false) => (),
        }
        if let Type::TVar(tv) = container {
            return tv.binding().is_some_and(|c| settled_cause(&c, contained, exact));
        }
        if let Type::TVar(tv) = contained {
            return tv.binding().is_some_and(|d| settled_cause(container, &d, exact));
        }
        let pairs = |a: &[Type], b: &[Type], exact: bool| {
            a.len() == b.len()
                && a.iter().zip(b.iter()).any(|(a, b)| settled_cause(a, b, exact))
        };
        match (container, contained) {
            (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::Error(a), Type::Error(b)) => settled_cause(a, b, exact),
            (Type::ByRef(m, a), Type::ByRef(_, b)) => {
                settled_cause(a, b, exact || *m == Mutability::Mut)
            }
            (Type::Map(m0), Type::Map(m1)) => {
                settled_cause(&m0.key, &m1.key, exact)
                    || settled_cause(&m0.value, &m1.value, exact)
            }
            (Type::Tuple(a), Type::Tuple(b)) => pairs(a, b, exact),
            (Type::Variant(t0, a, _), Type::Variant(t1, b, _)) => {
                t0 == t1 && pairs(a, b, exact)
            }
            (Type::Struct(a), Type::Struct(b)) => {
                a.len() == b.len()
                    && a.iter().zip(b.iter()).any(|((n0, a, _), (n1, b, _))| {
                        n0 == n1 && settled_cause(a, b, exact)
                    })
            }
            (
                Type::Abstract { id: i0, params: a },
                Type::Abstract { id: i1, params: b },
            ) => i0 == i1 && pairs(a, b, true),
            (Type::Fn(f), Type::Fn(g)) => {
                let (args, aligned) = f.align(g);
                aligned
                    && (args.iter().any(|(s, t)| settled_cause(&t.typ, &s.typ, exact))
                        || settled_cause(&f.rtype, &g.rtype, exact)
                        || settled_cause(&f.throws, &g.throws, exact))
            }
            // CR claude for claude: [bug/medium] Through a union this arm calls ANY
            // settled cell the cause, so the note "settled when its input ended;
            // declare e's type" attaches where another member fails. In the REPL, after
            // `let e = []` and `let v = select true { true => e, false => 1.5 }`, `v ==
            // "s"` and `let s: [string, i64] = v` both carry the note. Declaring `let
            // e: Array<i64> = []` leaves both refused: the f64 member is the cause. The
            // note should hold only where the relation would pass with the settled ⊥
            // cells open (test that on copies), or at least judge a union member by
            // member as the tuple arm does. probe:
            // design/review-2026-11-16/repro/x-diagnostics-settled-note-union.sh
            // (x-diagnostics-03)
            (Type::Set(_) | Type::Ref(_) | Type::App(..), _)
            | (_, Type::Set(_) | Type::Ref(_) | Type::App(..)) => {
                let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                container.reached_cells(&mut cells);
                contained.reached_cells(&mut cells);
                cells.values().any(|tv| tv.settled_bottom())
            }
            _ => false,
        }
    })
}

/// Where a binding is declared, for a refusal that names it: the line and
/// column, and, unless it is the origin given, where they are: a file, a
/// module or the text of a REPL input.
pub struct BoundAt<'a>(pub &'a Bind, pub Option<&'a Origin>);

impl fmt::Display for BoundAt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::expr::Source;
        let Bind { pos, ori, .. } = self.0;
        write!(f, "line {}, column {}", pos.line, pos.column)?;
        if self.1.is_some_and(|at| *at == **ori) {
            return Ok(());
        }
        match &ori.source {
            Source::Unspecified => {
                let line = usize::try_from(pos.line - 1).ok();
                match line.and_then(|i| ori.text.lines().nth(i)).map(str::trim) {
                    Some(l) if l.chars().count() > 40 => {
                        let l: String = l.chars().take(40).collect();
                        write!(f, " in `{l}..`")
                    }
                    Some(l) => write!(f, " in `{l}`"),
                    None => Ok(()),
                }
            }
            s => write!(f, " in {s}"),
        }
    }
}

/// A var's link to its binding cell, which other vars may share.
#[derive(Debug)]
pub struct TVarLink {
    pub(crate) id: TVarId,
    pub(crate) cell: CellLink,
    /// The [`cell_writes`] count of the thread that last wrote the link.
    written: u64,
}

#[derive(Debug)]
pub struct TVarInner {
    pub name: ArcStr,
    pub(crate) link: RwLock<TVarLink>,
}

#[derive(Clone)]
pub struct TVar(ManuallyDrop<Arc<TVarInner>>);

/// A cell's constraints hold types that hold cells, so teardown
/// recurses and must run inside the stack guard.
impl Drop for TVar {
    fn drop(&mut self) {
        ensure_sufficient(|| unsafe { ManuallyDrop::drop(&mut self.0) })
    }
}

// Cycle-guarded: a cell's conjuncts can reach the cell itself.
impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static DEBUGGING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
        }
        let addr = self.cell_addr();
        if !DEBUGGING.with_borrow_mut(|s| s.insert(addr)) {
            return f
                .debug_tuple("TVar")
                .field(&format_args!("'{}: …", self.name))
                .finish();
        }
        let r = ensure_sufficient(|| f.debug_tuple("TVar").field(&**self.0).finish());
        DEBUGGING.with_borrow_mut(|s| s.remove(&addr));
        r
    }
}

/// A generated cell's name: `_<id>`, the id short when it was minted
/// (counted from [`crate::ids::MINT_BASE`]) rather than its full width.
pub(crate) struct ShortName<'a>(pub(crate) &'a str);

impl fmt::Display for ShortName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.strip_prefix('_').and_then(|n| n.parse::<u64>().ok()) {
            Some(n) if n >= crate::ids::MINT_BASE => {
                write!(f, "_{}", n - crate::ids::MINT_BASE)
            }
            _ => write!(f, "{}", self.0),
        }
    }
}

/// Whether `name` is one the checker makes up: `_<id>`.
fn generated(name: &str) -> bool {
    name.strip_prefix('_').is_some_and(|n| n.bytes().all(|b| b.is_ascii_digit()))
}

impl TVar {
    /// Whether the checker made this variable up (an inferred cell), as
    /// opposed to one a program wrote.
    pub fn generated(&self) -> bool {
        generated(&self.name)
    }

    /// Whether the name is one no program writes as a variable: the
    /// receiver `self`, and the `#arg` of a trait written in argument
    /// position.
    pub(super) fn unwritten(&self) -> bool {
        &*self.name == "self" || self.name.starts_with('#')
    }

    /// Whether a signature printing cells as they stand
    /// ([`crate::print_standing`]) lists this quantifier in its header: a
    /// call still picks it, so it is open and generic, as it was where the
    /// message being printed was made ([`HeldQuantifiers`]).
    pub(super) fn heads_a_printed_signature(&self) -> bool {
        self.open_cell().is_some_and(|c| {
            c.level().is_generic() && !HELD.with_borrow(|h| h.contains(&c.cell_addr()))
        })
    }

    /// An open cell nothing names as a program writes it: the variable
    /// it wrote; a cell the checker made up has no name a program can
    /// write.
    pub fn written_open(&self) -> Option<Type> {
        match self.generated() {
            true => None,
            false => Some(Type::TVar(TVar::empty_named(self.name.clone()))),
        }
    }

    /// [`Self::written_open`] where the program wrote the variable for this
    /// cell, which a message may then name as one type: a call's copy of
    /// the variable its callee declares carries a name the program wrote
    /// for another cell, as every other copy of it does, unless a variable
    /// the program wrote took it ([`Naming`]).
    pub fn written_own(&self) -> Option<Type> {
        let end = self.open_cell().unwrap_or_else(|| self.clone());
        let naming = end.cell().read().naming.clone();
        match naming.by() {
            NamedBy::CallCopy => None,
            NamedBy::Taken(name) => Some(Type::TVar(TVar::empty_named(name.clone()))),
            NamedBy::Own => self.written_open(),
        }
    }
}

impl fmt::Display for TVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !PRINT_FLAGS.get().contains(PrintFlag::DerefTVars) {
            if &*self.name == "self" {
                return write!(f, "self");
            }
            if self.name.starts_with('#') {
                // A trait in argument position (`fn(s: Read)`) is a
                // quantifier whose one conjunct is the trait.
                let cons = self.cell_constraints();
                if let [c] = &cons[..] {
                    return write!(f, "{c}");
                }
            }
            write!(f, "'{}", ShortName(&printed_name(self)))
        } else {
            // A cell can be reachable from its own constraints; a
            // revisit on the print stack elides. Contents are cloned
            // out before recursing (never recurse under the cell guard).
            thread_local! {
                static PRINTING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
            }
            let _printing = Printing::enter(|| Type::TVar(self.clone()));
            let addr = self.cell_addr();
            if !PRINTING.with_borrow_mut(|s| s.insert(addr)) {
                return write!(f, "'{}: …", ShortName(&printed_name(self)));
            }
            let r = (|| {
                let (typ, cons, below) = {
                    let cell = self.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
                };
                // a generated cell's binding, or else what it settles to,
                // is all a reader needs of it
                let held = (typ.is_some() || !below.is_empty()) && prints_held(self);
                match (&typ, held, &below[..]) {
                    (Some(t), true, _) => return write!(f, "{t}"),
                    (None, true, [l]) => return write!(f, "{l}"),
                    (None, true, [l, ls @ ..]) => {
                        write!(f, "[{l}")?;
                        for l in ls {
                            write!(f, ", {l}")?
                        }
                        return write!(f, "]");
                    }
                    _ => {
                        // CR claude for claude: [readability/medium] A generated cell
                        // prints here as `'_<TVarId>: unbound`, and that id reaches
                        // refusals: the ?/$ operand (Strip::of), `f is fn(x: '_8144:
                        // unbound) -> ..` from a `<-`, let and compare mismatches, the
                        // destructuring-let misfit, ConcreteRequired, AmbiguousMember
                        // ("Array<'_8174: unbound> could go to ..") and printed
                        // patterns. 28 of the 308 refusal cases in the graphix-lang
                        // skill show one. The id is the global counter, so the CLI says
                        // '_8144 where the LSP says '_8239 for the same program, and
                        // the number names nothing the author wrote. Number generated
                        // cells per message ('_1, '_2 in first-met order, as
                        // Holders::number does for select) under
                        // format_with_flags(DerefTVars), or name them after their
                        // binding where the site knows it (Holders::name_cell). probe:
                        // design/review-2026-11-16/repro/x-diagnostics-cell-id-leak.sh
                        // (x-diagnostics-01)
                        let name = printed_name(self);
                        if let Some(Type::TVar(next)) = &typ
                            && printed_name(next) == name
                        {
                            return write!(f, "{next}");
                        }
                        write!(f, "'{}: ", ShortName(&name))?
                    }
                }
                let Some(t) = typ else {
                    write!(f, "unbound")?;
                    // a bound that differs from one printed by its cells'
                    // identity alone reads as that one written again
                    let once = cons.iter().enumerate().filter(|(i, c)| {
                        !cons[..*i].iter().any(|p| written_identical(p, c))
                    });
                    for (n, (_, c)) in once.enumerate() {
                        write!(f, "{}{c}", if n == 0 { " within " } else { " & " })?
                    }
                    for (i, t) in below.iter().enumerate() {
                        write!(f, "{}{t}", if i == 0 { " above " } else { " | " })?
                    }
                    return Ok(());
                };
                write!(f, "{t}")
            })();
            PRINTING.with_borrow_mut(|s| s.remove(&addr));
            r
        }
    }
}

impl Default for TVar {
    fn default() -> Self {
        let id = TVarId::new();
        let name = ArcStr::from(format_compact!("_{}", id.0).as_str());
        Self::from_parts(name, id, CellRef::new(CellLock::default()))
    }
}

impl Deref for TVar {
    type Target = TVarInner;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

/// Binding equality: two vars are equal when they stand for one cell
/// ([`TVar::end_cell`]) or those cells' bindings are equal, so two
/// distinct unbound cells compare equal. Use [`union_identical`] where
/// identity is meant.
impl PartialEq for TVar {
    fn eq(&self, other: &Self) -> bool {
        let (c0, c1) = (self.end_cell(), other.end_cell());
        CellRef::ptr_eq(&c0, &c1) || c0.read().binding == c1.read().binding
    }
}

impl Eq for TVar {}

impl PartialOrd for TVar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TVar {
    fn cmp(&self, other: &Self) -> Ordering {
        let (c0, c1) = (self.end_cell(), other.end_cell());
        if CellRef::ptr_eq(&c0, &c1) {
            Ordering::Equal
        } else {
            c0.read().binding.cmp(&c1.read().binding)
        }
    }
}

impl Hash for TVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.end_cell().read().binding.hash(state)
    }
}

impl Type {
    /// The conjunct a body adds to a variable its definition declares
    /// without the signature saying it: the operands of its operators are
    /// one type.
    pub(super) fn is_quiet_bound(&self) -> bool {
        matches!(self, Type::Bound(Bound::Singleton))
    }
}

impl TVar {
    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        match &Type::TVar(self.clone()).scope_refs(scope) {
            Type::TVar(tv) => tv.clone(),
            _ => unreachable!(),
        }
    }

    pub fn empty_named(name: ArcStr) -> Self {
        Self::from_parts(name, TVarId::new(), CellRef::new(CellLock::default()))
    }

    /// A fresh variable no definition owns yet, as written in source.
    pub fn empty_generic(name: ArcStr) -> Self {
        let tv = Self::empty_named(name);
        tv.set_level(Level::GENERIC);
        tv
    }

    /// The cell's level ([`TCell::level`]).
    #[doc(hidden)]
    pub fn level(&self) -> Level {
        self.cell().read().level
    }

    /// An empty cell of the same name and level.
    pub(crate) fn fresh_copy(&self) -> Self {
        let f = Self::empty_named(self.name.clone());
        f.set_level(self.level());
        f
    }

    /// Mark a generic cell a written type's own ([`Level::Written`]).
    #[doc(hidden)]
    pub fn own_written(&self) {
        if self.level() == Level::Generic {
            self.set_level(Level::Written)
        }
    }

    /// Mark the cell generic: its definition closed over it.
    pub(crate) fn generalize(&self) {
        self.set_level(Level::GENERIC)
    }

    pub(crate) fn set_level(&self, level: Level) {
        let cell = self.cell();
        let mut c = cell.write();
        if c.level != level {
            written(&c);
        }
        c.level = level
    }

    /// Claim the cell, generic or not, for `level` unless it already
    /// belongs at that depth or shallower.
    // CR claude for claude: [perf/low] `claim` takes the cell's write lock before it
    // compares levels, and `CellLock::write` counts a write and takes the cell's
    // `Place` whatever it changes. `lower_in` calls it for every non-generic variable
    // of every type bound, bounded or merged, so a bind marks each cell its type names
    // as written: their `ReachOrder` numbers and those above them fall, `written_since`
    // reports them changed, and a cell another task shares takes this thread's count in
    // `written`. Read the level first and return when the cell is already at that depth
    // or shallower. Found by reading; not measured. (t-tvar-a-06)
    pub(crate) fn claim(&self, level: Level) {
        let lowered = {
            let cell = self.cell();
            let mut c = cell.write();
            (c.level.depth() > level.depth()).then(|| {
                written(&c);
                c.level = level;
                (c.binding.clone(), c.constraints.clone(), c.lower.clone())
            })
        };
        if let Some((binding, cons, below)) = lowered {
            for t in binding.iter().chain(cons.iter()).chain(below.iter()) {
                lower(t, level)
            }
        }
    }

    pub fn named(name: ArcStr, typ: Type) -> Self {
        lower(&typ, current_level());
        Self::from_parts(
            name,
            TVarId::new(),
            CellRef::new(CellLock::new(TCell::bound(typ))),
        )
    }

    /// A wrapper over an existing cell, as an image restores it.
    pub(crate) fn from_parts(name: ArcStr, id: TVarId, cell: CellRef) -> Self {
        Self(ManuallyDrop::new(Arc::new(TVarInner {
            name,
            link: RwLock::new(TVarLink { id, cell: cell.link(), written: 0 }),
        })))
    }

    /// The same cell under another name.
    pub fn renamed(&self, name: ArcStr) -> Self {
        let (_, cell) = self.parts();
        Self::from_parts(name, TVarId::new(), cell)
    }

    /// Point this handle at `other`'s cell: a variable a parse wrote by a
    /// name before it knew which variable the name is, whose own cell
    /// holds nothing yet ([`super::fntyp::ParseDepth::quantifiers_end`]).
    pub(crate) fn point_at(&self, other: &TVar) {
        let (id, cell) = other.parts();
        let mut l = self.write();
        l.cell = cell.link();
        l.id = id;
    }

    /// The wrapper's id and its cell.
    pub(crate) fn parts(&self) -> (TVarId, CellRef) {
        let link = self.read();
        (link.id, link.cell.upgrade())
    }

    /// Identity of the wrapper: equal for clones of one `TVar`.
    pub(crate) fn wrapper_addr(&self) -> usize {
        Arc::as_ptr(&*self.0) as *const () as usize
    }

    /// The cell this var reads now.
    pub(crate) fn cell(&self) -> CellRef {
        self.read().cell.upgrade()
    }

    /// The cell's binding, cloned out (never recurse under the guard).
    pub fn binding(&self) -> Option<Type> {
        self.read().cell.get().read().binding.clone()
    }

    pub fn is_bound(&self) -> bool {
        self.read().cell.get().read().binding.is_some()
    }

    fn ground_judgment(&self, j: Judgment) -> Ground {
        let epoch = ground_epoch();
        let link = self.read();
        let cell = link.cell.get();
        let c = cell.read();
        match &c.binding {
            Some(_) if c.ground == epoch && c.kept.clear.contains(j) => {
                Ground::Kept(false)
            }
            Some(_) if c.ground == epoch && c.kept.found.contains(j) => {
                Ground::Kept(true)
            }
            Some(b) if c.ground == epoch => {
                Ground::Unjudged { cell: cell.to_ref(), binding: b.clone(), epoch }
            }
            _ => Ground::Unproved,
        }
    }

    /// `judge` of the cell's binding when it reaches no open cell
    /// ([`Self::ground_now`]), else `None`. A judgment that found nothing,
    /// and says it rests on the binding alone (`judge`'s flag), finds
    /// nothing again while the proof stands, so the cell keeps it and is
    /// not judged `j` again.
    pub(super) fn ground_verdict<R>(
        &self,
        j: Judgment,
        judge: impl FnOnce(&Type) -> (Option<R>, bool),
    ) -> Option<Option<R>> {
        match self.ground_judgment(j) {
            Ground::Kept(false) => Some(None),
            Ground::Unproved | Ground::Kept(true) => None,
            Ground::Unjudged { cell, binding, epoch } => {
                let (r, keep) = judge(&binding);
                if keep && r.is_none() {
                    keep_judgment(&cell, epoch, j, false)
                }
                Some(r)
            }
        }
    }

    /// [`Self::ground_verdict`] of a judgment that finds a yes or a no:
    /// the cell keeps either.
    #[doc(hidden)]
    pub fn ground_holds(
        &self,
        j: Judgment,
        judge: impl FnOnce(&Type) -> (bool, bool),
    ) -> Option<bool> {
        match self.ground_judgment(j) {
            Ground::Unproved => None,
            Ground::Kept(found) => Some(found),
            Ground::Unjudged { cell, binding, epoch } => {
                let (found, keep) = judge(&binding);
                if keep {
                    keep_judgment(&cell, epoch, j, found)
                }
                Some(found)
            }
        }
    }

    /// The cell's binding resolved ([`Type::resolve_tvars`]) when it
    /// reaches no open cell ([`Self::ground_now`]), else `None`: the form
    /// the cell keeps, or `resolve`'s of the binding, which the cell keeps
    /// while the proof stands when `resolve` says it holds no cell.
    pub(super) fn ground_resolved(
        &self,
        resolve: impl FnOnce(&Type) -> (Type, bool),
    ) -> Option<Type> {
        let epoch = ground_epoch();
        let (cell, binding) = {
            let link = self.read();
            let held = link.cell.get();
            let c = held.read();
            match (&c.binding, &c.resolved) {
                (Some(_), Some(r)) if c.ground == epoch => return Some((**r).clone()),
                (Some(b), None) if c.ground == epoch => (held.to_ref(), b.clone()),
                _ => return None,
            }
        };
        let (r, keep) = resolve(&binding);
        if keep && ground_epoch() == epoch {
            let mut c = cell.0.write();
            let written = c.binding.as_ref().map(Type::shared_identity);
            if c.ground == epoch && written == Some(binding.shared_identity()) {
                c.resolved = Some(Box::new(r.clone()))
            }
        }
        Some(r)
    }

    /// Whether the cell is bound and proved, at the current
    /// [`GROUND_EPOCH`], to reach no open cell. Reads the proof, walks
    /// nothing.
    pub(super) fn ground_now(&self) -> bool {
        let link = self.read();
        let cell = link.cell.get();
        let c = cell.read();
        c.binding.is_some() && c.ground == ground_epoch()
    }

    /// The name a definition whose body this thread checks declares the
    /// open cell this var stands for by ([`Declared`]).
    pub fn declared_name(&self) -> Option<ArcStr> {
        DECLARED.with_borrow(|d| d.name_of(self))
    }

    /// Whether the open cell this var stands for is a quantifier a
    /// definition whose body this thread checks declares: its bound is the
    /// signature's, which the body may not add to.
    pub(super) fn declared_here(&self) -> bool {
        DECLARED.with_borrow(|d| d.each_of(self, |frame| &frame.quantified, |_| true))
    }

    /// Whether this thread is checking the body of a definition that
    /// declares variables.
    pub(super) fn checking_a_body() -> bool {
        DECLARED.with_borrow(|d| d.0.is_some())
    }

    /// The open cell this var stands for: its own, or the end of a chain
    /// of bindings to vars (a merged cell's forward link); `None` when
    /// the chain ends bound.
    pub fn open_cell(&self) -> Option<TVar> {
        let mut tv = self.clone();
        loop {
            #[cfg(test)]
            CHAIN_STEPS.with(|n| n.set(n.get() + 1));
            let next = match &tv.cell().read().binding {
                None => None,
                Some(Type::TVar(next)) => Some(next.clone()),
                Some(_) => return None,
            };
            match next {
                None => return Some(tv),
                Some(next) => tv = next,
            }
        }
    }

    /// Bind the cell, replacing any binding.
    #[doc(hidden)]
    pub fn bind(&self, t: Type) {
        self.write_binding(t, true)
    }

    /// Bind the cell; `reaching` when `t` may reach an open cell a
    /// binding it replaces did not.
    fn write_binding(&self, t: Type, reaching: bool) {
        self.write_binding_in(t, reaching, &mut LPooled::take())
    }

    /// [`Self::write_binding`], lowering through `seen` ([`lower_in`]).
    fn write_binding_in(&self, t: Type, reaching: bool, seen: &mut Lowered) {
        super::bounds::old_binding(self, &t);
        lower_in(&t, self.level(), seen);
        let cell = self.cell();
        let mut c = cell.write();
        if decided_as(&c, &t) {
            if reaching && c.binding.is_some() {
                bound_cell_changes();
            }
            c.set_binding(Some(t))
        }
    }

    /// Add a conjunct to this var's cell constraints (deduped).
    // CR claude for claude: [bug/high] Nothing checks that a conjunct added here does
    // not reach the cell itself under a constructor: `narrow_cell` skips one that holds
    // the cell bare (`holds_cell`), and `require_bounds` and `add_body_bound` check
    // nothing. `Type::require_singleton` hands an open union member the known member
    // beside it (`[z, Array<z>]` gives z the conjunct `Array<z>`), and
    // `TypeRef::hold_bounds_of` a typedef parameter's bound over the same variable
    // (`T<'x, 'x>` over `type T<'p: Array<'q>, 'q>`). The definition is accepted, and a
    // call's check binds the cell to an array of itself and allocates until it is
    // killed (6 GB in under a second). Refuse a conjunct whose `cycle_toward` the cell
    // is `Cycle::Constructor`: as the infinite type from `add_body_bound`, and by
    // `mark_cycle_refused` where a bind hands it. probe:
    // design/review-2026-11-16/repro/t-tvar-a-self-conjunct-oom.sh (t-tvar-a-02)
    // 2026-11-21 claude: require_bounds and require_singleton are deleted (no bind
    // hands a predicate). Re-probed: no run grows; singleton_call is refused
    // ('[Array<..>, Array<Array<..>>] is more than one type, where Singleton asks for
    // one'), typedef_call is accepted (exit 0); both definitions still check where the
    // probe expects 'cannot infer a finite type here'.
    pub fn add_cell_constraint(&self, c: Type) {
        let cell = self.cell();
        let existing = cell.read().constraints.clone();
        let new = new_conjuncts(&existing, [c]);
        let level = cell.read().level;
        for c in new.iter() {
            lower(c, level)
        }
        if new.is_empty() {
            return;
        }
        let mut c = cell.write();
        if !decided(&c) {
            return;
        }
        if c.binding.is_some() {
            bound_cell_changes();
        }
        c.constraints.extend(new.iter().cloned())
    }

    /// The refusal of the bounds `needs` a body needs of the variable of its
    /// definition this cell is, which the variable's own bound does not
    /// imply, with the bound to write in the signature: its own conjuncts
    /// that none of `needs` implies, and `needs`.
    // CR claude for claude: [bug/medium] The bound to write is printed with
    // `PrintFlag::DerefTVars`, so one that names a variable reads `write 'a: Array<'d:
    // unbound within Any>`, which no program can write. And
    // `contains::undeclared_bound_note` passes a conjunct that holds the declared
    // variable itself, so the fix can read `write 'a: Array<'a: unbound>`, a bound the
    // check refuses ("'a's bound names 'a"). Print the bound as a program writes it
    // (`Type::written_form`), and give no such note for a conjunct that reaches the
    // declared cell. No pin covers a needed bound that names a variable. probe:
    // design/review-2026-11-16/repro/t-tvar-a-undeclared-bound-unwritable.sh
    // (t-tvar-a-05)
    pub(super) fn undeclared_bound(&self, env: &Env, needs: &[&Type]) -> CompactString {
        crate::format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
            let name = self.declared_name().unwrap_or_else(|| self.name.clone());
            let mut bound = CompactString::default();
            let mut within = CompactString::default();
            for own in self.cell_constraints().iter() {
                let said = matches!(own, Type::Any)
                    || needs.iter().any(|c| c.implies(env, own).unwrap_or(false));
                if !said {
                    bound.push_str(&format_compact!("{own} + "));
                }
            }
            for (i, c) in needs.iter().enumerate() {
                let sep = if i == 0 { "" } else { " + " };
                within.push_str(&format_compact!("{sep}{c}"));
            }
            bound.push_str(&within);
            format_compact!(
                "the body needs '{} within {within}, which the signature does not \
                 declare: write '{}: {bound}",
                ShortName(&name),
                ShortName(&name)
            )
        })
    }

    /// Add the conjunct `c` a body needs of the cell: the variable of a
    /// definition being checked takes none its signature does not declare,
    /// but `Singleton`, which its operators say of one type each call picks.
    pub fn add_body_bound(&self, env: &Env, c: Type) -> Result<()> {
        if !self.declared_here() {
            self.add_cell_constraint(c);
            return Ok(());
        }
        if self.conjunct_implies(env, &c)? {
            return Ok(());
        }
        match c.is_quiet_bound() {
            true => Ok(self.add_cell_constraint(c)),
            false => bail!("{}", self.undeclared_bound(env, &[&c])),
        }
    }

    /// The cell's constraint conjunction (cloned out).
    pub fn cell_constraints(&self) -> SmallVec<[Type; 1]> {
        self.read().cell.get().read().constraints.clone()
    }

    /// Whether one of the cell's conjuncts implies the bound `c`
    /// ([`Type::implies`]).
    pub(crate) fn conjunct_implies(&self, env: &Env, c: &Type) -> Result<bool> {
        for own in self.cell_constraints().iter() {
            if own.implies(env, c)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Whether an open cell may be bound to a type that holds an error: no
    /// conjunct of its own says `NoError`.
    pub fn may_hold_error(&self, env: &Env) -> Result<bool> {
        Ok(!self.conjunct_implies(env, &Type::Bound(Bound::NoError))?)
    }

    /// The cell's lower bounds (cloned out).
    pub fn cell_lower(&self) -> SmallVec<[Type; 1]> {
        self.read().cell.get().read().lower.clone()
    }

    /// Bound the open cell from below by `t` (deduped, and a bound that a
    /// union bound holds as a member is not kept beside it): whatever it
    /// binds to must contain `t`. The caller has checked the cell's
    /// conjuncts admit `t`; they are handed to `t`'s open cells as a
    /// binding's are.
    pub(crate) fn add_lower(&self, t: Type) {
        let cell = self.cell();
        // CR claude for claude: [risk/medium] `new_conjuncts` runs its identity walk
        // here under the read guards of both cells (the temporaries of `o_cell.read()`
        // and `s_cell.read()` live to the end of the statement), though the comment
        // above says the dedup is computed lock-free because the walk can re-enter
        // these cells. `add_lower`'s `held` and `TVar`'s `PartialEq` and `Ord` walk
        // under a cell's read guard the same way. A second read of a
        // `parking_lot::RwLock` on one thread blocks behind a queued writer, and
        // another compile task queues one on a shared cell at any time (`mark_ground`,
        // `CycleWalk::number`, every `claim`), so a parallel module check can deadlock.
        // Clone the bounds and bindings out before walking them, as `mine` and `theirs`
        // are. `TCell::add_constraint`'s doc forbids running it under a cell guard,
        // while its `&mut TCell` exists only under one. Found by reading; not run.
        // (t-tvar-a-04)
        // 2026-11-21 claude: moved here from `TVar::merge_one`, deleted with name
        // merging; the walk it named there is gone, and `held` below and `TVar`'s
        // `PartialEq` and `Ord` still walk under a cell's read guard.
        let held = |m: &Type| {
            cell.read().lower.iter().any(|e| union_identical(e, m) || union_member(e, m))
        };
        if held(&t) || matches!(&t, Type::Set(ms) if ms.iter().all(held)) {
            return;
        }
        lower(&t, cell.read().level);
        let mut c = cell.write();
        if decided_as(&c, &t) {
            if c.binding.is_some() {
                bound_cell_changes();
            }
            c.lower.retain(|e| !union_member(&t, e));
            c.lower.push(t)
        }
    }

    pub fn read<'a>(&'a self) -> RwLockReadGuard<'a, TVarLink> {
        self.link.read()
    }

    pub fn write<'a>(&'a self) -> RwLockWriteGuard<'a, TVarLink> {
        let writes = count_write();
        let mut link = self.link.write();
        link.written = writes;
        link
    }

    /// Whether the cell holds the `Concrete` conjunct.
    #[doc(hidden)]
    pub fn requires_concrete(&self) -> bool {
        self.cell()
            .read()
            .constraints
            .iter()
            .any(|c| matches!(c, Type::Bound(Bound::Concrete)))
    }

    /// Whether the cell holds the `Data` conjunct.
    // CR claude for claude: [structure/low] `requires_concrete`, `requires_data` and
    // `requires_function` are one body over three `Bound` variants, and `mark_settled`,
    // `mark_bottom_fed` and `mark_cycle_refused` one lock, already-held, `decided`, set
    // sequence over three `CellFact`s, the `decided` test written two ways. One
    // `requires(&self, b: Bound)` and one private `mark(&self, fact: CellFact)` say
    // each once. Found by reading; not run. (t-tvar-a-07)
    pub(crate) fn requires_data(&self) -> bool {
        self.cell()
            .read()
            .constraints
            .iter()
            .any(|c| matches!(c, Type::Bound(Bound::Data)))
    }

    pub(super) fn normalize_int(&self, cx: &mut super::normalize::NormCx) -> Self {
        // First visit only. Clone the binding out, normalize unlocked,
        // write back: the lock is non-reentrant.
        // A cell an earlier task owns keeps its binding as it stands: the
        // normal form means the same, and writing it would be a decision.
        // The normal form reaches only cells its source reached.
        if cx.visited.cells.insert(self.cell_addr())
            && !self.earlier_task()
            && let Some(t) = self.binding()
            && let Some(n) = t.normalize_int(cx)
        {
            self.write_binding(n, false);
        }
        self.clone()
    }

    /// Clear the binding, and the requirement a ⊥ binding carried; the
    /// constraints stay.
    pub fn unbind(&self) {
        let cell = self.cell();
        let mut c = cell.write();
        if c.binding.is_some() && !decided(&c) {
            return;
        }
        if c.binding.is_some() {
            bound_cell_changes();
        }
        c.set_binding(None);
        c.facts.remove(CellFact::BottomRequired);
    }

    /// Open a rigid gate on the open cell this var stands for
    /// ([`Self::open_cell`]: a merge re-points one var and leaves the
    /// others reading a cell that forwards), else on the cell it reads;
    /// see [`TCell::rigid_gates`].
    pub fn open_rigid(&self) -> RigidGate {
        let cell = match self.open_cell() {
            Some(open) => open.cell(),
            None => self.cell(),
        };
        {
            let mut c = cell.write();
            gated(&c);
            c.rigid_gates += 1;
        }
        RigidGate { cell }
    }

    /// Whether a compile task started before the running one created the
    /// cell.
    #[doc(hidden)]
    pub fn earlier_task(&self) -> bool {
        earlier_task(&self.cell().read())
    }

    #[doc(hidden)]
    pub fn is_rigid(&self) -> bool {
        self.read().cell.get().read().rigid_gates > 0
    }

    /// Whether the cell holds ⊥ as its input's end settled it.
    pub(super) fn settled_bottom(&self) -> bool {
        let cell = self.cell();
        let c = cell.read();
        matches!(c.binding, Some(Type::Bottom)) && c.facts.contains(CellFact::Settled)
    }

    /// Mark the cell settled by its input's end; see [`CellFact::Settled`].
    pub(super) fn mark_settled(&self) {
        let cell = self.cell();
        let mut c = cell.write();
        if !c.facts.contains(CellFact::Settled) && decided(&c) {
            c.facts |= CellFact::Settled;
        }
    }

    /// Whether a ⊥ was produced into the cell while it was open: a
    /// binding over ⊥ that took its type from its first use.
    #[doc(hidden)]
    pub fn bottom_fed(&self) -> bool {
        self.cell().read().facts.contains(CellFact::BottomFed)
    }

    pub(super) fn would_cycle(&self, t: &Type) -> bool {
        would_cycle_inner(&self.cell(), t)
    }

    /// True iff both vars share one binding cell (aliases of each other).
    pub fn same_cell(&self, other: &Self) -> bool {
        self.cell_addr() == other.cell_addr()
    }

    /// The cell this var stands for: its own, or the end of the forward
    /// links merges left ([`Self::open_cell`], bound or not).
    fn end_cell(&self) -> CellRef {
        let mut cell = self.cell();
        loop {
            let next = match &cell.read().binding {
                Some(Type::TVar(next)) => Some(next.cell()),
                _ => None,
            };
            match next {
                Some(next) => cell = next,
                None => return cell,
            }
        }
    }

    /// The address of [`Self::end_cell`].
    pub fn stands_for(&self) -> usize {
        self.end_cell().data_ptr().addr()
    }

    /// Identity of the shared binding cell, for set membership tests.
    #[doc(hidden)]
    pub fn cell_addr(&self) -> usize {
        self.read().cell.get().data_ptr().addr()
    }

    /// `self`'s cell under `like`'s name when a program wrote that name: a
    /// parameter's cell merged into a declared quantifier's keeps the
    /// quantifier's name in a copy, since a signature finds its
    /// quantifiers by name. Inferred names of one cell collapse to one.
    pub(crate) fn named_as(&self, like: &Self) -> Self {
        match self.name == like.name || like.name.starts_with('_') {
            true => self.clone(),
            false => self.renamed(like.name.clone()),
        }
    }

    /// A copy's image `r` of this var's binding, under this var's name
    /// ([`Self::named_as`]) when `r` is a var: the binding is a merge's
    /// forward link, and the var a second name for the cell it reaches.
    pub(crate) fn forwarded_as(&self, r: Type) -> Type {
        match &r {
            Type::TVar(v) => Type::TVar(v.named_as(self)),
            _ => r,
        }
    }

    /// A fresh cell standing for this one in a copy: the name, and the
    /// conjuncts, refusal and binding rebuilt through `walk`; memoized
    /// by cell in `fresh`, so a copy keeps the source's alias topology. A
    /// var whose cell a merge forwarded to a cell the copy holds an image
    /// of names that image, whether or not its own cell is copied.
    fn freshen<M: Forwards>(
        &self,
        fresh: &mut AHashMap<usize, TVar>,
        memo: &mut M,
        how: Fresh<'_>,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>, &mut M) -> Option<Type>,
    ) -> TVar {
        let addr = self.cell_addr();
        if let Some(f) = fresh.get(&addr) {
            return f.named_as(self);
        }
        if !fresh.is_empty()
            && matches!(self.read().cell.get().read().binding, Some(Type::TVar(_)))
            && let Some(end) = memo.ends().open_cell(self)
            && let Some(f) = fresh.get(&end)
        {
            return f.named_as(self);
        }
        let f = match how {
            Fresh::Copy => self.fresh_copy(),
            Fresh::Instantiate(open) if self.level().copied_by(open) => {
                let f = TVar::empty_named(self.name.clone());
                f.cell().write().naming = Naming::call_copy();
                f
            }
            Fresh::Scheme(open) if self.level().copied_by(open) => {
                TVar::empty_generic(self.name.clone())
            }
            Fresh::Generic if self.level().is_generic() => {
                TVar::empty_named(self.name.clone())
            }
            Fresh::Instantiate(_) | Fresh::Scheme(_) | Fresh::Generic => {
                return self.clone();
            }
        };
        self.copy_into(&f, fresh, memo, walk);
        f
    }

    /// Make the empty cell `f` this var's copy in `fresh`: the conjuncts,
    /// lower bounds and binding rebuilt through `walk`, the facts and
    /// [`TCell::naming`] kept.
    fn copy_into<M: Forwards>(
        &self,
        f: &TVar,
        fresh: &mut AHashMap<usize, TVar>,
        memo: &mut M,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>, &mut M) -> Option<Type>,
    ) {
        fresh.insert(self.cell_addr(), f.clone());
        for c in self.cell_constraints() {
            let c = walk(&c, fresh, memo).unwrap_or(c);
            f.add_cell_constraint(c);
        }
        for t in self.cell_lower() {
            f.add_lower(walk(&t, fresh, memo).unwrap_or(t));
        }
        // what the source's checks learned holds of every copy
        let (facts, naming) = {
            let cell = self.cell();
            let c = cell.read();
            (c.facts, c.naming.clone())
        };
        {
            let cell = f.cell();
            let mut c = cell.write();
            c.facts |= facts;
            c.naming = naming.copied_into(&c.naming);
        }
        if let Some(t) = self.binding() {
            memo.in_binding(true);
            let t = walk(&t, fresh, memo).unwrap_or(t);
            memo.in_binding(false);
            f.write_binding_in(t, true, memo.lowered());
        }
    }
}

/// A by-name substitution: the map, every name its types hold, read once,
/// when a quantified function type first asks, and the copy of each shared
/// composite, made once.
pub(super) struct Subst<'a> {
    known: &'a AHashMap<ArcStr, Type>,
    held: Option<LPooled<AHashSet<ArcStr>>>,
    ends: ChainEnds,
    built: Option<LPooled<AHashMap<usize, Option<Type>>>>,
    bindings: u32,
    lowered: Option<Lowered>,
}

impl Forwards for Subst<'_> {
    fn ends(&mut self) -> &mut ChainEnds {
        &mut self.ends
    }

    fn lowered(&mut self) -> &mut Lowered {
        self.lowered.get_or_insert_with(LPooled::take)
    }

    fn in_binding(&mut self, entered: bool) {
        match entered {
            true => self.bindings += 1,
            false => self.bindings -= 1,
        }
    }
}

impl<'a> Subst<'a> {
    pub(super) fn new(known: &'a AHashMap<ArcStr, Type>) -> Self {
        Self {
            known,
            held: None,
            ends: ChainEnds::default(),
            built: None,
            bindings: 0,
            lowered: None,
        }
    }

    fn held(&mut self) -> &AHashSet<ArcStr> {
        let known = self.known;
        self.held.get_or_insert_with(|| {
            let mut held: LPooled<AHashSet<ArcStr>> = LPooled::take();
            let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
            for t in known.values() {
                t.names_held(&mut held, &mut seen)
            }
            held
        })
    }
}

/// The cells of `ft`'s quantifiers `names`, where a position names them
/// (a nested function type quantifying a name hides it), else as
/// [`FnType::each_quantifier`] finds them.
fn quantifier_cells(ft: &FnType, names: &[ArcStr], out: &mut AHashMap<ArcStr, TVar>) {
    fn go(
        t: &Type,
        names: &[ArcStr],
        hidden: &mut SmallVec<[ArcStr; 4]>,
        out: &mut AHashMap<ArcStr, TVar>,
        seen: &mut AHashSet<usize>,
    ) -> ControlFlow<()> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if names.contains(&tv.name) && !hidden.contains(&tv.name) {
                    out.entry(tv.name.clone()).or_insert_with(|| tv.clone());
                }
                match out.len() == names.len() {
                    true => ControlFlow::Break(()),
                    false => ControlFlow::Continue(()),
                }
            }
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => {
                ControlFlow::Continue(())
            }
            Type::Fn(ft) => {
                let depth = hidden.len();
                hidden.extend(ft.quantifiers.iter().cloned());
                let r = ft.try_for_each_type(&mut |c| go(c, names, hidden, out, seen));
                hidden.truncate(depth);
                r
            }
            t => t.try_for_each_child(&mut |c| go(c, names, hidden, out, seen)),
        })
    }
    if names.is_empty() {
        return;
    }
    let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
    let _ =
        ft.try_for_each_type(&mut |t| go(t, names, &mut SmallVec::new(), out, &mut seen));
    if out.len() < names.len() {
        ft.each_quantifier(|q, tv| {
            if names.contains(q) {
                out.entry(q.clone()).or_insert_with(|| tv.clone());
            }
        })
    }
}

/// A by-name substitution `s` as it reads under the function type `ft`:
/// `ft`'s quantifiers shadow the names it gives, and each one a type
/// substituted under `ft` holds is renamed, to a name nothing there holds,
/// through a copy of its cell that `fresh` and the map give; with the
/// renames, `(old, new)`. `None` when `s` reads as it is.
fn under_quantifiers(
    ft: &FnType,
    s: &mut Subst,
    fresh: &mut AHashMap<usize, TVar>,
) -> Option<(LPooled<AHashMap<ArcStr, Type>>, SmallVec<[(ArcStr, ArcStr); 2]>)> {
    let known = s.known;
    if ft.quantifiers.is_empty() || known.is_empty() {
        return None;
    }
    let bound = |n: &ArcStr| ft.quantifiers.contains(n);
    let shadowed = ft.quantifiers.iter().any(|q| known.contains_key(q));
    if !shadowed && !ft.quantifiers.iter().any(|q| s.held().contains(q)) {
        return None;
    }
    let mut avoid: LPooled<AHashSet<ArcStr>> = LPooled::take();
    avoid.extend(ft.quantifiers.iter().cloned());
    let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
    ft.for_each_type(&mut |t| t.names_held(&mut avoid, &mut seen));
    let mut held: LPooled<AHashSet<ArcStr>> = LPooled::take();
    seen.clear();
    for (n, t) in known.iter() {
        if avoid.contains(n) && !bound(n) {
            t.names_held(&mut held, &mut seen)
        }
    }
    let captured: SmallVec<[ArcStr; 2]> =
        ft.quantifiers.iter().filter(|q| held.contains(*q)).cloned().collect();
    if captured.is_empty() && !shadowed {
        return None;
    }
    let mut inner: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
    inner.extend(
        known.iter().filter(|(n, _)| !bound(n)).map(|(n, t)| (n.clone(), t.clone())),
    );
    avoid.extend(held.drain());
    let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    quantifier_cells(ft, &captured, &mut cells);
    let mut renames: SmallVec<[(ArcStr, ArcStr); 2]> = SmallVec::new();
    let mut copies: SmallVec<[(TVar, TVar); 2]> = SmallVec::new();
    for q in captured {
        let name = name_apart(&q, |n| avoid.contains(n));
        avoid.insert(name.clone());
        if let Some(tv) = cells.get(&q) {
            let copy = match fresh.get(&tv.cell_addr()) {
                Some(f) => f.renamed(name.clone()),
                None => {
                    let f = tv.fresh_copy().renamed(name.clone());
                    fresh.insert(tv.cell_addr(), f.clone());
                    copies.push((tv.clone(), f.clone()));
                    f
                }
            };
            inner.insert(q.clone(), Type::TVar(copy));
        }
        renames.push((q, name));
    }
    let mut scoped = Subst::new(&inner);
    for (tv, copy) in copies {
        tv.copy_into(&copy, fresh, &mut scoped, |t, f, s| t.replace_tvars_int(s, f));
    }
    Some((inner, renames))
}

/// `q` numbered to a name not `taken`.
fn name_apart(q: &str, taken: impl Fn(&ArcStr) -> bool) -> ArcStr {
    (1u64..)
        .map(|i| ArcStr::from(format_compact!("{q}{i}").as_str()))
        .find(|n| !taken(n))
        .unwrap()
}

/// `copy`, a walk's image of `ft` that inlined bindings, with each of
/// `ft`'s quantifiers renamed apart where a cell of its name that an
/// inlined binding brought would stand for it by name, as
/// [`under_quantifiers`] renames one a substituted type holds; where the
/// quantifier's image stands in no position (it is held through a bound),
/// the cells that would stand for it are renamed instead. `image` is the
/// walk's image of a cell of `ft`, `None` where it keeps the cell.
pub(super) fn keep_quantifiers(
    ft: &FnType,
    mut copy: FnType,
    mut image: impl FnMut(&TVar) -> Option<Type>,
) -> FnType {
    if ft.quantifiers.is_empty() {
        return copy;
    }
    let mut own: SmallVec<[(ArcStr, TVar); 2]> = SmallVec::new();
    ft.each_quantifier(|q, tv| own.push((q.clone(), tv.clone())));
    let mut picked: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    copy.collect_tvars(&mut picked);
    for (q, tv) in own {
        let addr = match &image(&tv) {
            None => tv.cell_addr(),
            Some(Type::TVar(img)) => img.cell_addr(),
            Some(_) => continue,
        };
        if picked.get(&q).is_none_or(|p| p.cell_addr() == addr) {
            continue;
        }
        let mut avoid: LPooled<AHashSet<ArcStr>> = LPooled::take();
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        copy.for_each_type(&mut |t| t.names_held(&mut avoid, &mut seen));
        avoid.extend(copy.quantifiers.iter().cloned());
        let name = name_apart(&q, |n| avoid.contains(n));
        let rename = |copy: &FnType, pick: &dyn Fn(&TVar) -> bool| {
            let mut memo = CopyMemo::default();
            copy.cow_walk(|t| t.renamed_cells(pick, &name, &mut memo))
        };
        copy = match rename(&copy, &|c| c.cell_addr() == addr) {
            Some(mut renamed) => {
                renamed.quantifiers = Arc::from_iter(
                    copy.quantifiers
                        .iter()
                        .map(|n| if *n == q { name.clone() } else { n.clone() }),
                );
                renamed
            }
            None => {
                rename(&copy, &|c| c.name == q && c.cell_addr() != addr).unwrap_or(copy)
            }
        };
    }
    copy
}

thread_local! {
    static PRINT_DEPTH: Cell<u32> = const { Cell::new(0) };
    static MESSAGE_DEPTH: Cell<u32> = const { Cell::new(0) };
    static SHOWN: RefCell<Shown> = RefCell::new(Shown::default());
    /// The cells of the [`HeldQuantifiers`] being printed.
    static HELD: RefCell<SmallVec<[usize; 4]>> = RefCell::new(SmallVec::new());
    /// The variables of the definitions whose bodies this thread checks.
    static DECLARED: RefCell<Declared> = RefCell::new(Declared::default());
}

/// The variables the definitions whose bodies a thread checks declare,
/// innermost first, each holding its cell rigid meanwhile: a cell merged
/// into one (a call's result, an inferred argument) is that variable,
/// whatever handle a message reaches it by, and prints under its name
/// ([`printed_name`]).
#[derive(Debug, Clone, Default)]
pub(crate) struct Declared(Option<Arc<DeclaredFrame>>);

#[derive(Debug)]
struct DeclaredFrame {
    vars: SmallVec<[TVar; 2]>,
    /// The variables among `vars` the definition's signature declares with a
    /// bound of their own, as against those it only names.
    quantified: SmallVec<[TVar; 2]>,
    outer: Declared,
}

impl Declared {
    /// The variables of the definitions whose bodies this thread checks.
    pub(crate) fn current() -> Self {
        DECLARED.with_borrow(Self::clone)
    }

    /// `f`, naming the cells these variables declare by their names.
    pub(crate) fn printing<R>(&self, f: impl FnOnce() -> R) -> R {
        let _declared = Declaring::replace(self.clone());
        f()
    }

    /// Each variable of these whose cell is the open cell `tv` stands for,
    /// until `f` answers true; whether it did.
    fn each_of(
        &self,
        tv: &TVar,
        pick: fn(&DeclaredFrame) -> &[TVar],
        mut f: impl FnMut(&TVar) -> bool,
    ) -> bool {
        let Some(mut frame) = self.0.as_deref() else { return false };
        let Some(end) = tv.open_cell() else { return false };
        loop {
            for v in pick(frame).iter() {
                if v.open_cell().is_some_and(|c| c.same_cell(&end)) && f(v) {
                    return true;
                }
            }
            match frame.outer.0.as_deref() {
                Some(outer) => frame = outer,
                None => return false,
            }
        }
    }

    /// The name a variable of these declares the open cell `tv` stands for
    /// by, the least where two do.
    fn name_of(&self, tv: &TVar) -> Option<ArcStr> {
        let mut found: Option<ArcStr> = None;
        self.each_of(
            tv,
            |frame| &frame.vars,
            |v| {
                if found.as_ref().is_none_or(|f| v.name < *f) {
                    found = Some(v.name.clone())
                }
                false
            },
        );
        found
    }
}

/// The variables a definition whose body this thread checks declares
/// ([`Declared`]) until this drops, which restores the enclosing ones.
#[must_use]
#[doc(hidden)]
pub struct Declaring(Declared);

impl Declaring {
    #[doc(hidden)]
    pub fn enter(
        vars: impl IntoIterator<Item = TVar>,
        quantified: impl IntoIterator<Item = TVar>,
    ) -> Self {
        let vars: SmallVec<[TVar; 2]> = vars.into_iter().collect();
        let quantified = quantified.into_iter().collect();
        let outer = Declared::current();
        match vars.is_empty() {
            true => Self(outer),
            false => Self::replace(Declared(Some(Arc::new(DeclaredFrame {
                vars,
                quantified,
                outer,
            })))),
        }
    }

    fn replace(declared: Declared) -> Self {
        Self(DECLARED.replace(declared))
    }
}

impl Drop for Declaring {
    fn drop(&mut self) {
        DECLARED.set(mem::take(&mut self.0))
    }
}

/// What a refusal formatted after its check prints by as it stood when the
/// refusal was made: the quantifiers of its types no call picked (a
/// definition's own, read in its body, a `let rec`'s self-read, and one
/// instance's), which print where they occur though the definition's close
/// generalizes them, and the variables of the definitions being checked
/// ([`Declared`]), whose gates have closed by then. A type a tool shows
/// after the check holds the quantifiers of the definitions whose text it
/// is shown in ([`Self::resolving`]).
#[derive(Debug)]
pub struct HeldQuantifiers {
    held: SmallVec<[TVar; 2]>,
    declared: Declared,
}

impl HeldQuantifiers {
    pub(crate) fn of(types: [&Type; 2]) -> Self {
        let mut held = SmallVec::new();
        for t in types {
            t.each_quantifier(&mut |_, tv| {
                if let Some(c) = tv.open_cell()
                    && !c.level().is_generic()
                {
                    held.push(c)
                }
            })
        }
        Self { held, declared: Declared::current() }
    }

    /// `typ` resolved ([`Type::resolve_tvars`]), and what it prints by: the
    /// copies of the open cells of `held` it holds, each named by the name
    /// its handle in `held` carries when that is a declared name.
    pub fn resolving(typ: &Type, held: &[TVar]) -> (Type, Self) {
        let (typ, held) = typ.resolve_tvars_copying(held);
        let vars: SmallVec<[TVar; 2]> =
            held.iter().filter(|c| !c.name.starts_with('_')).cloned().collect();
        let declared = match vars.is_empty() {
            true => Declared::default(),
            false => Declared(Some(Arc::new(DeclaredFrame {
                vars,
                quantified: SmallVec::new(),
                outer: Declared::default(),
            }))),
        };
        (typ, Self { held, declared })
    }

    /// The copies of the held cells the resolved type holds.
    pub fn cells(&self) -> &[TVar] {
        &self.held
    }

    /// `f`, printing as the types stood where they were made.
    pub fn printing<R>(&self, f: impl FnOnce() -> R) -> R {
        struct Truncate(usize);
        impl Drop for Truncate {
            fn drop(&mut self) {
                HELD.with_borrow_mut(|h| h.truncate(self.0))
            }
        }
        self.declared.printing(|| {
            if self.held.is_empty() {
                return f();
            }
            let _truncate = Truncate(HELD.with_borrow_mut(|h| {
                let len = h.len();
                h.extend(self.held.iter().map(TVar::cell_addr));
                len
            }));
            f()
        })
    }
}

/// The names one message gave its named cells where cells print as they
/// stand ([`crate::print_standing`]), from its outermost
/// [`crate::format_with_flags`] to that one's end ([`Message`]): a cell a
/// printing signature's header lists until that signature ends
/// ([`quantifiers_apart`]), any other until the message does
/// ([`printed_name`]).
#[derive(Default)]
struct Shown {
    names: AHashMap<usize, ArcStr>,
    /// The cells the message printed as what they hold ([`prints_held`]).
    held: AHashSet<usize>,
    /// The names of `names`, each one cell's.
    taken: AHashSet<ArcStr>,
    /// The name a declared variable's open cell printed under, by the
    /// cell's end: each handle a copy made of it prints under that one.
    declared: AHashMap<usize, ArcStr>,
    /// Every name the message showed a cell under, and every name a type
    /// it printed holds once `unwalked` is walked: a name numbered apart
    /// is none of them.
    used: AHashSet<ArcStr>,
    /// The types the message printed whose names `used` lacks.
    unwalked: SmallVec<[Type; 2]>,
}

impl Shown {
    /// The capacity the tables keep from one message to the next.
    const KEPT: usize = 64;

    fn show(&mut self, addr: usize, name: ArcStr) {
        self.taken.insert(name.clone());
        self.used.insert(name.clone());
        self.names.insert(addr, name);
    }

    fn hide(&mut self, addr: usize) {
        if let Some(name) = self.names.remove(&addr) {
            self.taken.remove(&name);
        }
    }

    /// `name` unless a cell prints under it, then numbered apart.
    fn free_name(&mut self, name: &ArcStr) -> ArcStr {
        match self.taken.contains(name) {
            false => name.clone(),
            true => self.numbered_apart(name),
        }
    }

    /// `name` numbered apart from every name the message uses; a
    /// generated name as its short form, `_` and a number, which no
    /// generated name prints as.
    fn numbered_apart(&mut self, name: &ArcStr) -> ArcStr {
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        for t in self.unwalked.drain(..) {
            t.names_held(&mut self.used, &mut seen)
        }
        let taken = |n: &ArcStr| self.used.contains(n);
        match generated(name) {
            false => name_apart(name, taken),
            true => name_apart(&format_compact!("{}_", ShortName(name)), taken),
        }
    }

    fn clear(&mut self) {
        if self.names.capacity() > Self::KEPT
            || self.held.capacity() > Self::KEPT
            || self.used.capacity() > Self::KEPT
            || self.declared.capacity() > Self::KEPT
            || self.unwalked.capacity() > Self::KEPT
        {
            *self = Self::default()
        } else {
            self.names.clear();
            self.held.clear();
            self.taken.clear();
            self.declared.clear();
            self.used.clear();
            self.unwalked.clear()
        }
    }
}

/// A message being formatted ([`crate::format_with_flags`]). Until the
/// outermost one ends, each named cell its types show that no header
/// lists keeps one name ([`printed_name`]).
pub(crate) struct Message;

impl Message {
    pub(crate) fn enter() -> Self {
        MESSAGE_DEPTH.set(MESSAGE_DEPTH.get() + 1);
        Message
    }
}

impl Drop for Message {
    fn drop(&mut self) {
        let depth = MESSAGE_DEPTH.get() - 1;
        MESSAGE_DEPTH.set(depth);
        if depth == 0 {
            SHOWN.with_borrow_mut(|s| s.clear())
        }
    }
}

/// A type being printed as its cells stand ([`crate::print_standing`]).
pub(super) struct Printing;

impl Printing {
    /// `top` is the type being printed where no print encloses this one,
    /// walked for the names it holds only when a name numbers apart.
    pub(super) fn enter(top: impl FnOnce() -> Type) -> Option<Self> {
        if !crate::print_standing() {
            return None;
        }
        let depth = PRINT_DEPTH.get();
        if depth == 0 {
            SHOWN.with_borrow_mut(|s| s.unwalked.push(top()))
        }
        PRINT_DEPTH.set(depth + 1);
        Some(Printing)
    }
}

impl Drop for Printing {
    fn drop(&mut self) {
        PRINT_DEPTH.set(PRINT_DEPTH.get() - 1)
    }
}

/// The name `tv` prints under where cells print as they stand
/// ([`crate::print_standing`]), else its own: the one a printing signature
/// gave the cell its header lists ([`quantifiers_apart`]), and for any
/// other named cell, the one a definition being checked declares it by
/// ([`TVar::declared_name`]), else its own, unless the message being
/// printed showed another cell under it, then numbered apart, so two
/// instances' cells of one declared or inferred variable read as two. The
/// handles of one declared variable's open cell share its name.
// CR claude for claude: [bug/low] The first cell printed under a name takes it, so
// where a callee's variable and a variable the definition being checked declares are
// both `'a`, the program's own variable is the one numbered apart. `let f = 'a: Any |x:
// 'a| uniq(x)` is refused with "type mismatch 'a: unbound within Discernible does not
// contain 'a1: unbound within Any: the body needs 'a within Discernible .. write 'a:
// Discernible": the first `'a` is uniq's, `'a1` is the program's, and the note
// (`TVar::undeclared_bound`, formatted as a message of its own before `TypeMismatch`
// prints) calls the program's variable `'a` again. Reserve the names
// `Declared::current()` declares for their own cells when the outermost message starts,
// so a call's copy numbers apart, and format the note inside the mismatch's message.
// probe: design/review-2026-11-16/repro/t-tvar-b-declared-name-numbered-apart.gx
// (t-tvar-b-04)
pub(super) fn printed_name(tv: &TVar) -> ArcStr {
    if !crate::print_standing() || PRINT_DEPTH.get() == 0 {
        return tv.name.clone();
    }
    let addr = tv.cell_addr();
    SHOWN.with_borrow_mut(|s| match s.names.get(&addr) {
        Some(name) => name.clone(),
        None if tv.unwritten() => tv.name.clone(),
        None => {
            let declared = tv.declared_name();
            let end = declared.as_ref().map(|_| tv.stands_for());
            let name = match end.and_then(|e| s.declared.get(&e)) {
                Some(name) => name.clone(),
                None => {
                    let name = s.free_name(&declared.unwrap_or_else(|| tv.name.clone()));
                    if let Some(end) = end {
                        s.declared.insert(end, name.clone());
                    }
                    name
                }
            };
            s.show(addr, name.clone());
            name
        }
    })
}

/// Whether `tv`, whose cell holds a binding or lower bounds, prints as
/// what it holds rather than under a name: a generated variable does.
/// Where cells print as they stand ([`crate::print_standing`]) the first
/// handle of a cell the message prints decides for every other, so a
/// cell that a copy named after a callee's variable ([`TVar::named_as`])
/// never prints both bare and under that name.
fn prints_held(tv: &TVar) -> bool {
    if !crate::print_standing() || PRINT_DEPTH.get() == 0 {
        return tv.generated();
    }
    let addr = tv.cell_addr();
    SHOWN.with_borrow_mut(|s| {
        if s.held.contains(&addr) {
            return true;
        }
        if s.names.contains_key(&addr) {
            return false;
        }
        let held = tv.generated();
        if held {
            s.held.insert(addr);
        }
        held
    })
}

/// The cells a printing signature's header lists, hidden as it ends.
struct Listed(LPooled<Vec<usize>>);

impl Drop for Listed {
    fn drop(&mut self) {
        SHOWN.with_borrow_mut(|s| self.0.drain(..).for_each(|addr| s.hide(addr)))
    }
}

/// `f`, printing `ft`, with each quantifier `ft`'s header lists
/// ([`TVar::heads_a_printed_signature`]) printing under a name apart from
/// the other cells of its name `ft` holds and from every name the message
/// being printed shows another cell under, where cells print as they
/// stand ([`crate::print_standing`]).
pub(super) fn quantifiers_apart<R>(ft: &FnType, f: impl FnOnce() -> R) -> R {
    let Some(_printing) = Printing::enter(|| Type::Fn(Arc::new(ft.clone()))) else {
        return f();
    };
    if ft.quantifiers.is_empty() {
        return f();
    }
    let mut own: SmallVec<[(ArcStr, TVar); 2]> = SmallVec::new();
    ft.each_quantifier(|q, tv| {
        if tv.heads_a_printed_signature() {
            own.push((q.clone(), tv.clone()))
        }
    });
    if own.is_empty() {
        return f();
    }
    let mut listed = Listed(LPooled::take());
    for (q, tv) in own {
        let addr = tv.cell_addr();
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let alone = ft
            .try_for_each_type(&mut |t| t.holds_other_cell(&q, addr, &mut seen))
            .is_continue();
        SHOWN.with_borrow_mut(|s| {
            if s.names.contains_key(&addr) {
                return;
            }
            let name = match alone && !s.taken.contains(&q) {
                true => q,
                false => s.numbered_apart(&q),
            };
            s.show(addr, name);
            listed.0.push(addr)
        })
    }
    f()
}

// Each walk below spells out only the arms whose policy differs from
// plain recursion (chiefly whether `Ref` params are walked); the rest
// routes through `Type::try_for_each_child` / `Type::cow_children`.
impl Type {
    /// Whether `Function ⊇ self` holds as the type stands: a function
    /// type a generated call passes every argument of (no variadic, no
    /// defaulted label), through bindings and type references; an open
    /// cell is admitted, the terminal settle refuses it open.
    pub(crate) fn function_holds(&self, env: &Env, commit: bool) -> Result<bool> {
        ensure_sufficient(|| match self {
            Type::Fn(ft) => {
                Ok(ft.vargs.is_none() && !ft.args.iter().any(|a| a.has_default()))
            }
            Type::TVar(tv) => match tv.binding() {
                None => Ok(true),
                Some(b) => b.function_holds(env, commit),
            },
            Type::Ref(_) => match self.lookup_ref_with(env, commit)? {
                Some(t) => t.function_holds(env, commit),
                None => Ok(false),
            },
            _ => Ok(false),
        })
    }

    /// Whether `Singleton ⊇ self` holds as the type stands: one type, not
    /// a union of several, a primitive set of several included; bound
    /// cells judged by their bindings, type references by their
    /// expansions. An open cell, alone or a member of a union, is
    /// admitted: a bind of a `Singleton` cell narrows the open members of
    /// its binding ([`Self::require_singleton`]); two rigid ones are not,
    /// since they can't be made one.
    pub(crate) fn singleton_holds(&self, env: &Env, commit: bool) -> Result<bool> {
        let mut parts = UnionParts::new(Measure::Members);
        self.union_parts(Some((env, commit)), &mut parts)?;
        Ok(parts.known <= 1 && parts.rigid_apart().is_none())
    }

    /// Whether `OneNumber ⊇ self` holds as the type stands: at most one
    /// numeric type among its members, read as [`Self::singleton_holds`]
    /// reads them. Against a fixed signature (`sealed`) a rigid open
    /// member counts as the numeric types its caller may pick
    /// ([`UnionParts::count_rigid_numbers`]).
    pub(crate) fn one_number_holds(
        &self,
        env: &Env,
        commit: bool,
        sealed: bool,
    ) -> Result<bool> {
        let mut parts = UnionParts::new(Measure::Numbers);
        self.union_parts(Some((env, commit)), &mut parts)?;
        if sealed {
            parts.count_rigid_numbers(env)?
        }
        Ok(parts.known <= 1)
    }

    /// Whether a value of `self` may be an error as the type stands: a
    /// member is an `Error`, holds the primitive `error` or is `Any`.
    /// `NoError ⊇ self` is its negation; an open member may be anything
    /// its own conjuncts admit and is not counted, but against a fixed
    /// signature (`sealed`) a rigid one is its caller's pick: an error
    /// unless one of its conjuncts implies `NoError`.
    pub(crate) fn may_be_error(
        &self,
        env: &Env,
        commit: bool,
        sealed: bool,
    ) -> Result<bool> {
        self.may_be_error_past(env, (commit, sealed), &mut LPooled::take())
    }

    /// [`Self::may_be_error`], each name expanded once a walk: one met
    /// again was found to hold no error, or the walk would have ended.
    fn may_be_error_past(
        &self,
        env: &Env,
        (commit, sealed): (bool, bool),
        named: &mut LPooled<Vec<RefKey>>,
    ) -> Result<bool> {
        ensure_sufficient(|| match self {
            Type::Error(_) | Type::Any => Ok(true),
            Type::Primitive(p) => Ok(p.contains(netidx_value::Typ::Error)),
            Type::Set(ts) => {
                for t in ts.iter() {
                    if t.may_be_error_past(env, (commit, sealed), named)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.may_be_error_past(env, (commit, sealed), named),
                None if sealed && tv.is_rigid() => {
                    Ok(!tv.conjunct_implies(env, &Type::Bound(Bound::NoError))?)
                }
                None => Ok(false),
            },
            Type::Ref(tr) => {
                if let Some(r) = tr.resolve_peek(env) {
                    let k = (r.def_key(), tr.params.clone());
                    if named.contains(&k) {
                        return Ok(false);
                    }
                    named.push(k)
                }
                #[cfg(test)]
                MAY_BE_ERROR_NAMES.with(|n| n.set(n.get() + 1));
                match self.lookup_ref_with(env, commit)? {
                    Some(t) => t.may_be_error_past(env, (commit, sealed), named),
                    None => Ok(true),
                }
            }
            _ => Ok(false),
        })
    }

    /// The members of `self` read as a union into `parts`; a type
    /// reference counts as one member without `env`.
    fn union_parts(
        &self,
        env: Option<(&Env, bool)>,
        parts: &mut UnionParts,
    ) -> Result<()> {
        ensure_sufficient(|| match self {
            Type::Bottom => Ok(()),
            Type::Set(ts) => ts.iter().try_for_each(|t| t.union_parts(env, parts)),
            Type::TVar(tv) => match tv.binding() {
                None => Ok(parts.open.push(tv.clone())),
                Some(b) => b.union_parts(env, parts),
            },
            Type::Ref(_) if let Some((env, commit)) = env => {
                match self.lookup_ref_with(env, commit)? {
                    Some(t) => t.union_parts(Some((env, commit)), parts),
                    None => Ok(parts.member(self)),
                }
            }
            t => Ok(parts.member(t)),
        })
    }

    /// Bind each open cell of `self` to the part of `row` at its
    /// position: `self` an expression's type as an instance compiled it,
    /// `row` the same expression's type as its definition's check left
    /// it, read through the instance's cell map. Where the two differ in
    /// shape the instance's own knowledge stands. False when an open cell
    /// of `self` found no part of `row` to take.
    pub fn take_row(&self, row: &Type) -> bool {
        let mut seen: LPooled<AHashMap<(usize, usize), bool>> = LPooled::take();
        self.take_row_at(row, &mut seen)
    }

    /// A shared pair of nodes is taken once: what its first visit bound
    /// stands, so the walk is linear in a DAG, not in its paths.
    fn take_row_at(&self, row: &Type, seen: &mut AHashMap<(usize, usize), bool>) -> bool {
        ensure_sufficient(|| {
            let key = super::node_addr(self).zip(super::node_addr(row));
            if let Some(r) = key.and_then(|k| seen.get(&k)) {
                return *r;
            }
            let r = self.take_row_inner(row, seen);
            if let Some(k) = key {
                seen.insert(k, r);
            }
            r
        })
    }

    fn take_row_inner(
        &self,
        row: &Type,
        seen: &mut AHashMap<(usize, usize), bool>,
    ) -> bool {
        let each =
            |a: &[Type], b: &[Type], seen: &mut AHashMap<(usize, usize), bool>| match a
                .len()
                == b.len()
            {
                true => {
                    a.iter().zip(b).fold(true, |ok, (a, b)| a.take_row_at(b, seen) & ok)
                }
                false => !self.has_unbound(),
            };
        match (self, row) {
            (Type::TVar(tv), row) => match tv.binding() {
                Some(b) => b.take_row_at(row, seen),
                None => match row {
                    Type::TVar(r) if r.cell_addr() == tv.cell_addr() => true,
                    row if would_cycle_inner(&tv.cell(), row) => false,
                    row => {
                        tv.bind(row.clone());
                        true
                    }
                },
            },
            (t, Type::TVar(r)) => match r.binding() {
                Some(b) => t.take_row_at(&b, seen),
                None => match &r.cell_lower()[..] {
                    [lower] => t.take_row_at(lower, seen),
                    _ => !t.has_unbound(),
                },
            },
            (Type::App(c, x), Type::App(d, y)) => {
                c.take_row_at(d, seen) & x.take_row_at(y, seen)
            }
            (Type::Set(a), Type::Set(b)) | (Type::Tuple(a), Type::Tuple(b)) => {
                each(a, b, seen)
            }
            (Type::Variant(ta, a, _), Type::Variant(tb, b, _)) if ta == tb => {
                each(a, b, seen)
            }
            (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::Error(a), Type::Error(b)) => a.take_row_at(b, seen),
            (Type::ByRef(m0, a), Type::ByRef(m1, b)) if m0 == m1 => {
                a.take_row_at(b, seen)
            }
            (Type::Struct(a), Type::Struct(b))
                if a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(a, b)| a.0 == b.0) =>
            {
                a.iter()
                    .zip(b.iter())
                    .fold(true, |ok, ((_, a, _), (_, b, _))| a.take_row_at(b, seen) & ok)
            }
            (Type::Map(a), Type::Map(b)) => {
                a.key.take_row_at(&b.key, seen) & a.value.take_row_at(&b.value, seen)
            }
            (
                Type::Abstract { id: ia, params: a },
                Type::Abstract { id: ib, params: b },
            ) if ia == ib => each(a, b, seen),
            (Type::Ref(a), Type::Ref(b)) if a.name == b.name => {
                each(&a.params, &b.params, seen)
            }
            (Type::Fn(f), Type::Fn(g))
                if f.args.len() == g.args.len()
                    && f.vargs.is_some() == g.vargs.is_some() =>
            {
                let args = f.args.iter().zip(g.args.iter());
                let ok =
                    args.fold(true, |ok, (a, b)| a.typ.take_row_at(&b.typ, seen) & ok);
                let vargs = match (&f.vargs, &g.vargs) {
                    (Some(a), Some(b)) => a.take_row_at(b, seen),
                    _ => true,
                };
                ok & vargs
                    & f.rtype.take_row_at(&g.rtype, seen)
                    & f.throws.take_row_at(&g.throws, seen)
            }
            (t, _) => !t.has_unbound(),
        }
    }

    /// Whether `self` is concrete now: no open cell, and `Concrete ⊇ self`
    /// holds. The type a run-time read may cast to.
    pub fn is_concrete(&self, env: &Env) -> bool {
        !self.has_unbound() && self.concrete_holds(env)
    }

    /// Drop the conjuncts of every open cell the type reaches: for a
    /// copy the caller owns ([`Self::reset_tvars`]), whose heads unify
    /// whatever their bounds say.
    pub(crate) fn drop_bounds(&self) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.drop_bounds(),
                None => {
                    let cell = tv.cell();
                    cell.write().constraints.clear();
                }
            },
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| t.drop_bounds()),
            t => t.for_each_child(&mut |c| c.drop_bounds()),
        })
    }

    /// Claim every cell `self` reaches, through bindings, for `level`
    /// ([`TVar::claim`]), a generic one included, but a nested fn type's
    /// own quantifiers: the type of a binding something writes, whose
    /// writers share its one instance.
    #[doc(hidden)]
    pub fn claim_free(&self, level: Level) {
        fn go(
            t: &Type,
            level: Level,
            quantified: &mut SmallVec<[ArcStr; 4]>,
            seen: &mut AHashSet<usize>,
        ) {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    if !quantified.contains(&tv.name) && seen.insert(tv.cell_addr()) {
                        tv.claim(level);
                        if let Some(b) = tv.binding() {
                            go(&b, level, quantified, seen)
                        }
                    }
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
                Type::Fn(ft) => {
                    let outer = quantified.len();
                    quantified.extend(ft.quantifiers.iter().cloned());
                    ft.for_each_type(&mut |t| go(t, level, quantified, seen));
                    quantified.truncate(outer);
                }
                t => t.for_each_child(&mut |c| go(c, level, quantified, seen)),
            })
        }
        go(self, level, &mut SmallVec::new(), &mut LPooled::take())
    }

    /// This type with the cell at `addr` replaced by `with`, bound cells
    /// looked through; `None` where it holds none such.
    fn with_cell_as(&self, addr: usize, with: &Type) -> Option<Type> {
        fn go(t: &Type, addr: usize, with: &Type) -> Option<Type> {
            ensure_sufficient(|| match t {
                Type::TVar(tv) if tv.cell_addr() == addr => Some(with.clone()),
                Type::TVar(tv) => tv.binding().and_then(|b| go(&b, addr, with)),
                t => t.cow_children(&mut |c| go(c, addr, with)),
            })
        }
        go(self, addr, with)
    }

    /// Whether the cell at `addr` ([`ForeignWrite`]) is one of this type's,
    /// or one a cell it holds is bound to reaches.
    // CR claude for claude: [readability/low] Two relations share this name in one
    // file. This method asks whether the type reaches the cell at `addr` anywhere,
    // through bindings. `contains::holds_cell`, imported above and called by
    // `TVar::narrow_cell`, asks whether the cell is a member of the union `c`, so that
    // every type it takes is in `c`. A reader of `t.holds_cell(c)` beside
    // `holds_cell(env, &c, addr)` must open both to learn they differ. Rename this one
    // `reaches_cell`, the word `ForeignWrite::held_by`'s doc already uses. Found by
    // reading; not run. (t-tvar-b-05)
    fn holds_cell(&self, addr: usize) -> bool {
        fn go(t: &Type, addr: usize, seen: &mut AHashSet<usize>) -> bool {
            ensure_sufficient(|| match t {
                // an open cell reaches what flows into it
                Type::TVar(tv) => {
                    let cell = tv.cell_addr();
                    cell == addr
                        || (seen.insert(cell)
                            && match tv.binding() {
                                Some(b) => go(&b, addr, seen),
                                None => tv.cell_lower().iter().any(|l| go(l, addr, seen)),
                            })
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
                t => t
                    .try_for_each_child(&mut |c| match go(c, addr, seen) {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    })
                    .is_break(),
            })
        }
        go(self, addr, &mut LPooled::take())
    }

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        self.collect_tvars_seen(known, &mut TVarsSeen::take())
    }

    /// [`Self::collect_tvars`], each shared node (`seen`) walked once.
    pub(super) fn collect_tvars_seen(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut TVarsSeen,
    ) {
        #[cfg(test)]
        super::fntyp::TVAR_WALK_STEPS.with(|c| c.set(c.get() + 1));
        if let Some(n) = node_addr(self)
            && seen.nodes.insert(n, self.clone()).is_some()
        {
            return;
        }
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
            }
            // a nested fn type's own quantifiers are its own, whatever an
            // enclosing signature names alike; a node it shares with the
            // outside names them there, so it is walked apart
            Type::Fn(ft) if !ft.quantifiers.is_empty() => {
                super::fntyp::free_cells(ft, known)
            }
            Type::Fn(ft) => ft.collect_tvars_seen(known, seen),
            t => t.for_each_child(&mut |c| c.collect_tvars_seen(known, seen)),
        })
    }

    /// Every variable name `self` holds, bound or free: its cells', their
    /// conjuncts', lower bounds' and bindings', and its function types'
    /// quantifiers. Each cell and shared node (`seen`) is walked once.
    fn names_held(&self, out: &mut AHashSet<ArcStr>, seen: &mut AHashSet<usize>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                out.insert(tv.name.clone());
                if seen.insert(tv.cell_addr()) {
                    for t in tv.cell_constraints().iter().chain(tv.cell_lower().iter()) {
                        t.names_held(out, seen)
                    }
                    if let Some(b) = tv.binding() {
                        b.names_held(out, seen)
                    }
                }
            }
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            Type::Fn(ft) => {
                out.extend(ft.quantifiers.iter().cloned());
                ft.for_each_type(&mut |t| t.names_held(out, seen))
            }
            t => t.for_each_child(&mut |c| c.names_held(out, seen)),
        })
    }

    /// Whether a cell named `q` other than the one at `addr` is held,
    /// through bindings and bounds.
    fn holds_other_cell(
        &self,
        q: &ArcStr,
        addr: usize,
        seen: &mut AHashSet<usize>,
    ) -> ControlFlow<()> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if tv.name == *q && tv.cell_addr() != addr {
                    return ControlFlow::Break(());
                }
                if seen.insert(tv.cell_addr()) {
                    for t in tv.cell_constraints().iter().chain(tv.cell_lower().iter()) {
                        t.holds_other_cell(q, addr, seen)?
                    }
                    if let Some(b) = tv.binding() {
                        b.holds_other_cell(q, addr, seen)?
                    }
                }
                ControlFlow::Continue(())
            }
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => {
                ControlFlow::Continue(())
            }
            Type::Fn(ft) if ft.quantifiers.contains(q) => ControlFlow::Continue(()),
            t => t.try_for_each_child(&mut |c| c.holds_other_cell(q, addr, seen)),
        })
    }

    /// Self with each occurrence of a cell `pick` takes named `name`, its
    /// function types' included; `None` where none occurs. Each shared
    /// node (`memo`) is rebuilt once.
    pub(super) fn renamed_cells(
        &self,
        pick: &dyn Fn(&TVar) -> bool,
        name: &ArcStr,
        memo: &mut CopyMemo,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => pick(tv).then(|| Type::TVar(tv.renamed(name.clone()))),
            Type::Fn(ft) => once(self, memo, |memo| {
                ft.cow_walk(|t| t.renamed_cells(pick, name, memo))
                    .map(|f| Type::Fn(Arc::new(f)))
            }),
            t => once(t, memo, |memo| {
                t.cow_children(&mut |c| c.renamed_cells(pick, name, memo))
            }),
        })
    }

    /// [`Self::collect_tvars`] over positions alone: a nested signature's
    /// cells by its own positions, never its conjuncts, each shared node
    /// (`seen`) walked once, so the walk is linear in the type.
    pub(crate) fn collect_position_tvars(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut AHashSet<usize>,
    ) {
        #[cfg(test)]
        super::fntyp::TVAR_WALK_STEPS.with(|c| c.set(c.get() + 1));
        if let Some(n) = node_addr(self)
            && !seen.insert(n)
        {
            return;
        }
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
            }
            Type::Fn(ft) => {
                let mut inner: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                super::fntyp::position_cells(ft, &mut inner);
                for (name, tv) in inner.drain() {
                    if !ft.quantifiers.contains(&name) {
                        known.entry(name).or_insert(tv);
                    }
                }
            }
            t => t.for_each_child(&mut |c| c.collect_position_tvars(known, seen)),
        })
    }

    // CR claude for claude: [structure/low] The last arm of `check_tvars_declared`
    // repeats this body token for token: the `try_for_each_child` call that turns a
    // child's `Err` into a `Break` and back. The arm above it already calls this
    // function. Write the last arm as `t => t.check_parts_declared(declared)`. Found by
    // reading; not run. (t-tvar-b-07)
    fn check_parts_declared(&self, declared: &AHashSet<ArcStr>) -> Result<()> {
        match self.try_for_each_child(&mut |c| match c.check_tvars_declared(declared) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(e),
        }) {
            ControlFlow::Continue(()) => Ok(()),
            ControlFlow::Break(e) => Err(e),
        }
    }

    pub fn check_tvars_declared(&self, declared: &AHashSet<ArcStr>) -> Result<()> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if !declared.contains(&tv.name) {
                    bail!("undeclared type variable '{}'", tv.name)
                } else {
                    Ok(())
                }
            }
            // a fn type's own quantifiers are declared within it
            Type::Fn(ft) if !ft.quantifiers.is_empty() => {
                let mut inner: LPooled<AHashSet<ArcStr>> = LPooled::take();
                inner.extend(declared.iter().cloned());
                inner.extend(ft.quantifiers.iter().cloned());
                Type::Fn(ft.clone()).check_parts_declared(&inner)
            }
            t => match t.try_for_each_child(&mut |c| match c
                .check_tvars_declared(declared)
            {
                Ok(()) => ControlFlow::Continue(()),
                Err(e) => ControlFlow::Break(e),
            }) {
                ControlFlow::Continue(()) => Ok(()),
                ControlFlow::Break(e) => Err(e),
            },
        })
    }

    /// An open cell anywhere beneath, through bindings: a cell bound to
    /// an open cell is open. `Ref` params are walked (`Alias<'b>` with
    /// 'b open is open), expansions are not.
    pub fn has_unbound(&self) -> bool {
        self.open_beneath(&|_| true)
    }

    /// The lower bounds of the open cell `self` ends in, if it is one.
    pub fn open_lower(&self) -> SmallVec<[Type; 1]> {
        match self {
            Type::TVar(tv) => tv.open_cell().map(|c| c.cell_lower()).unwrap_or_default(),
            _ => SmallVec::new(),
        }
    }

    /// [`Self::has_unbound`] less the rigid cells: a declared variable in
    /// its own body never binds, so it stands as a constant.
    pub fn has_free(&self) -> bool {
        self.open_beneath(&|tv| !tv.is_rigid())
    }

    /// [`Self::has_free`] outside every function type.
    pub fn has_free_in_data(&self) -> bool {
        self.open_beneath_in(true, &|tv| !tv.is_rigid())
    }

    /// The first open cell `counts` beneath, through bindings.
    pub fn first_open(&self, counts: &dyn Fn(&TVar) -> bool) -> Option<TVar> {
        let found = std::cell::RefCell::new(None);
        self.open_beneath(&|tv| match counts(tv) {
            false => false,
            true => {
                *found.borrow_mut() = Some(tv.clone());
                true
            }
        });
        found.into_inner()
    }

    /// An open cell held rigid anywhere beneath, through bindings.
    pub fn has_rigid(&self) -> bool {
        self.open_beneath(&|tv| tv.is_rigid())
    }

    /// An open cell `counts` anywhere beneath, through bindings.
    fn open_beneath(&self, counts: &dyn Fn(&TVar) -> bool) -> bool {
        self.open_beneath_in(false, counts)
    }

    /// An open cell `counts` beneath, through bindings, outside every
    /// function type where `data`.
    fn open_beneath_in(&self, data: bool, counts: &dyn Fn(&TVar) -> bool) -> bool {
        fn go(
            t: &Type,
            data: bool,
            counts: &dyn Fn(&TVar) -> bool,
            seen: &mut AHashSet<usize>,
        ) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    seen.insert(tv.cell_addr())
                        && !tv.ground_now()
                        && match tv.binding() {
                            Some(b) => go(&b, data, counts, seen),
                            None => counts(tv),
                        }
                }
                Type::Fn(_) if data => false,
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
                t => t
                    .try_for_each_child(&mut |c| {
                        if go(c, data, counts, seen) {
                            ControlFlow::Break(())
                        } else {
                            ControlFlow::Continue(())
                        }
                    })
                    .is_break(),
            })
        }
        go(self, data, counts, &mut LPooled::take())
    }

    /// `self ⊇ pred` for a pattern's predicate over what reaches it: over
    /// a union, by member ([`Self::narrow_per_member`]). Whether some
    /// member, or `self`, contains it.
    pub fn narrow_pattern(&self, env: &Env, pred: &Type) -> Result<bool> {
        self.narrow_pattern_in(env, pred, &mut Narrowing::new())
    }

    fn narrow_pattern_in(
        &self,
        env: &Env,
        pred: &Type,
        walk: &mut Narrowing,
    ) -> Result<bool> {
        ensure_sufficient(|| {
            let Some(k) = walk.key(self, pred) else {
                return self.narrow_pattern_inner(env, pred, walk);
            };
            if walk.held.contains(k) {
                return Ok(true);
            }
            if walk.failed.contains(k) {
                return Ok(false);
            }
            let pair = walk.pair(env, self, pred);
            let depth = walk.open.len();
            if let Some(&at) = walk.open.get(&pair) {
                walk.assumed = walk.assumed.min(at);
                return Ok(true);
            }
            walk.open.insert(pair, depth);
            let outer = mem::replace(&mut walk.assumed, usize::MAX);
            let r = self.narrow_pattern_inner(env, pred, walk);
            walk.open.remove(&pair);
            let assumed = mem::replace(&mut walk.assumed, outer);
            if assumed < depth {
                walk.assumed = walk.assumed.min(assumed)
            }
            let r = r?;
            match r {
                true if assumed < depth => (),
                true => walk.held.insert(k, self, pred),
                false => walk.failed.insert(k, self, pred),
            }
            Ok(r)
        })
    }

    fn narrow_pattern_inner(
        &self,
        env: &Env,
        pred: &Type,
        walk: &mut Narrowing,
    ) -> Result<bool> {
        #[cfg(test)]
        NARROW_STEPS.with(|n| n.set(n.get() + 1));
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        crate::expr::union_members(env, self, &mut members)?;
        members.retain(|m| pred.could_match(env, m).unwrap_or(true));
        match members.len() {
            1 if members[0].needs_split(env, pred)? => {
                members[0].narrow_member(env, pred, walk)
            }
            0 | 1 => self.narrow_member(env, pred, walk),
            _ => pred.narrow_per_member_in(env, &members, walk).map(|()| true),
        }
    }

    /// `self ⊇ pred` for one member of what reaches a pattern: by
    /// [`Self::narrow_parts`] where a union stands under a constructor
    /// `pred` has, else by containment; then a cell of `pred` left open
    /// over an `Any` part is `Any` ([`Self::open_over_any`]).
    fn narrow_member(
        &self,
        env: &Env,
        pred: &Type,
        walk: &mut Narrowing,
    ) -> Result<bool> {
        let narrowed = if self.needs_split(env, pred)? {
            self.narrow_parts(env, pred, walk)?
        } else {
            walk.contains(env, self, pred)?
        };
        if narrowed && pred.has_unbound() {
            self.open_over_any(env, pred)?
        }
        Ok(narrowed)
    }

    /// Bind each cell of `pred` still open where `self`'s part is `Any`,
    /// written or named, to `Any`: `Any ⊇ 'a` binds nothing, but a pattern
    /// bind receives the scrutinee's value, whatever the arm body does
    /// with it.
    fn open_over_any(&self, env: &Env, pred: &Type) -> Result<()> {
        self.open_over_any_int(env, pred, &mut AnyWalk::default())
    }

    /// The parameters of each abstract type `self` holds as a part a
    /// pattern takes, at the widest instance's.
    fn widest_abstracts(&self, env: &Env, walk: &mut AnyWalk) -> Result<()> {
        ensure_sufficient(|| match &self.deref_cloned() {
            Some(Type::Abstract { id, params }) => {
                let widest = env.abstract_reps.get(id).map(|r| r.widest());
                for (i, p) in params.iter().enumerate() {
                    match widest.as_ref().and_then(|w| w.get(i)) {
                        Some(w) if !matches!(w, Type::Any) => {
                            w.decided_holds(env, p)?;
                        }
                        _ => p.any_beneath(env, walk)?,
                    }
                }
                Ok(())
            }
            Some(
                t @ (Type::Tuple(_)
                | Type::Struct(_)
                | Type::Variant(..)
                | Type::Array(_)
                | Type::List(_)
                | Type::Set(_)),
            ) => {
                let mut r = Ok(());
                let _ =
                    t.try_for_each_child(&mut |c| match c.widest_abstracts(env, walk) {
                        Ok(()) => ControlFlow::Continue(()),
                        Err(e) => {
                            r = Err(e);
                            ControlFlow::Break(())
                        }
                    });
                r
            }
            Some(_) | None => Ok(()),
        })
    }

    /// Bind each free cell `self` holds as data to `Any`; a declared
    /// variable is only asked whether its bound admits `Any`. A cell
    /// another compile task owns is left to that task's refusal of the
    /// write.
    fn any_beneath(&self, env: &Env, walk: &mut AnyWalk) -> Result<()> {
        if walk.fns && self.holds_fn(env) {
            bail!(
                "can't match on a type holding a function: a test tells a function from a \
                 value, not one signature from another"
            )
        }
        let mut left: LPooled<AddrSet> = LPooled::take();
        loop {
            let mut unseen = |tv: &TVar| !left.contains(&tv.cell_addr());
            let Some(tv) = find_open_cell(self, false, &mut unseen) else { break };
            let any = match tv.is_rigid() {
                true => cell_admits(&tv, env, &Type::Any),
                false => Type::TVar(tv.clone()).decided_holds(env, &Type::Any),
            };
            if !any.unwrap_or(false) {
                bail!(OpenOverAny(tv))
            }
            match tv.is_bound() {
                true => {
                    walk.binds += 1;
                    walk.walked.clear()
                }
                false => {
                    left.insert(tv.cell_addr());
                }
            }
        }
        Ok(())
    }

    /// [`Self::open_over_any`], each pair of shared nodes walked once
    /// while no cell is bound: a pair walked whole under the cells as they
    /// stand binds nothing when met again. A bound cell is the node it
    /// holds, so parts shared through cells are walked once too.
    fn open_over_any_int(
        &self,
        env: &Env,
        pred: &Type,
        walk: &mut AnyWalk,
    ) -> Result<()> {
        ensure_sufficient(|| {
            if walk.inside && matches!(self.head(env)?, Some(Type::Any)) {
                return match walk.abstracts {
                    true => pred.widest_abstracts(env, walk),
                    false => pred.any_beneath(env, walk),
                };
            }
            let Some(p) = pred.deref_cloned() else {
                if matches!(self.head(env)?, Some(Type::Any)) {
                    pred.decided_holds(env, &Type::Any)?;
                    walk.binds += 1;
                    walk.walked.clear()
                }
                return Ok(());
            };
            let Some(t) = self.deref_cloned() else { return Ok(()) };
            let key = walk.walked.key(&t, &p);
            if key.is_some_and(|k| walk.walked.contains(k)) {
                return Ok(());
            }
            let binds = walk.binds;
            t.open_over_any_inner(env, &p, walk)?;
            if let Some(k) = key.filter(|_| walk.binds == binds) {
                walk.walked.insert(k, &t, &p)
            }
            Ok(())
        })
    }

    fn open_over_any_inner(&self, env: &Env, p: &Type, walk: &mut AnyWalk) -> Result<()> {
        #[cfg(test)]
        ANY_STEPS.with(|n| n.set(n.get() + 1));
        if let Type::Set(qs) = p {
            return qs.iter().try_for_each(|q| self.open_over_any_int(env, q, walk));
        }
        let Some(t) = self.shape_under(env, p)? else { return Ok(()) };
        let mut each = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
            for (t, p) in parts {
                t.open_over_any_int(env, p, walk)?
            }
            Ok(())
        };
        match (&t, p) {
            (Type::Set(_), p) => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                crate::expr::union_members(env, &t, &mut members)?;
                for m in members.iter() {
                    if p.could_match(env, m)? {
                        m.open_over_any_int(env, p, walk)?
                    }
                }
                Ok(())
            }
            (Type::Tuple(ts), Type::Tuple(ps))
            | (Type::Variant(_, ts, _), Type::Variant(_, ps, _)) => {
                each(&mut ts.iter().zip(ps.iter()))
            }
            (Type::Struct(fs), Type::Struct(pfs)) => {
                each(&mut fs.iter().zip(pfs.iter()).map(|((_, t, _), (_, p, _))| (t, p)))
            }
            (Type::Array(e), Type::Array(pe)) | (Type::List(e), Type::List(pe)) => {
                e.open_over_any_int(env, pe, walk)
            }
            _ => Ok(()),
        }
    }

    /// Whether a union of `self` stands where `pred` has a constructor
    /// (a tuple position, a struct field, a variant payload, an array or
    /// list element), which containment would commit to its first member
    /// that holds the constructor, or at a part of a tuple, struct or
    /// variant where `pred` has an open cell that is one of the union's
    /// cells ([`Self::binds_beside`]), which containment would leave apart
    /// from the others.
    fn needs_split(&self, env: &Env, pred: &Type) -> Result<bool> {
        self.needs_split_int(env, pred, &mut NodePairs::default())
    }

    /// [`Self::needs_split`], each pair of shared nodes that needs none
    /// (`apart`) walked once, a bound cell as the node it holds. Exact:
    /// the walk assumes no pair in progress, so a pair's false holds
    /// wherever it is met, and a true ends it.
    fn needs_split_int(
        &self,
        env: &Env,
        pred: &Type,
        apart: &mut NodePairs,
    ) -> Result<bool> {
        ensure_sufficient(|| {
            let (Some(t), Some(p)) = (self.deref_cloned(), pred.deref_cloned()) else {
                return Ok(false);
            };
            let key = apart.key(&t, &p);
            if key.is_some_and(|k| apart.contains(k)) {
                return Ok(false);
            }
            let split = t.needs_split_inner(env, &p, apart)?;
            if let Some(k) = key.filter(|_| !split) {
                apart.insert(k, &t, &p)
            }
            Ok(split)
        })
    }

    fn needs_split_inner(
        &self,
        env: &Env,
        p: &Type,
        apart: &mut NodePairs,
    ) -> Result<bool> {
        #[cfg(test)]
        SPLIT_STEPS.with(|n| n.set(n.get() + 1));
        if let Type::Set(qs) = p {
            for q in qs.iter() {
                if self.needs_split_int(env, q, apart)? {
                    return Ok(true);
                }
            }
            return Ok(false);
        }
        let Some(t) = self.shape_under(env, p)? else { return Ok(false) };
        let mut any = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
            for (t, p) in parts {
                if t.binds_beside(p).is_some() || t.needs_split_int(env, p, apart)? {
                    return Ok(true);
                }
            }
            Ok(false)
        };
        match (&t, p) {
            (Type::Set(_), _) => Ok(true),
            (Type::Tuple(ts), Type::Tuple(ps))
            | (Type::Variant(_, ts, _), Type::Variant(_, ps, _)) => {
                any(&mut ts.iter().zip(ps.iter()))
            }
            (Type::Struct(fs), Type::Struct(pfs)) => {
                any(&mut fs.iter().zip(pfs.iter()).map(|((_, t, _), (_, p, _))| (t, p)))
            }
            // each of a slice's elements stands over the whole of a union
            // element type, where containment would pair it with a member
            (Type::Array(e), Type::Array(pe)) | (Type::List(e), Type::List(pe)) => {
                let union = matches!(e.head(env)?, Some(Type::Set(_)));
                match pe.deref_cloned() {
                    Some(Type::Set(_)) if union => Ok(true),
                    _ => e.needs_split_int(env, pe, apart),
                }
            }
            _ => Ok(false),
        }
    }

    /// The members of the union `self` stands for, where the open cell
    /// `pred` is one of them and not the only member: an open cell as it
    /// stands, a bound one as what it holds. The arms' predicates joined
    /// into that union typed the scrutinee, so each cell there is one
    /// arm's bind of this part, and receives what the others do; a member
    /// that is no cell is another arm's test, whose values reach `pred`'s
    /// arm unless an earlier arm takes them, which the select leaves out
    /// of what reaches it.
    fn binds_beside(&self, pred: &Type) -> Option<SmallVec<[Type; 8]>> {
        fn cells(
            t: &Type,
            c: &TVar,
            seen: &mut AddrSet,
            own: &mut bool,
            out: &mut SmallVec<[Type; 8]>,
        ) {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => match tv.open_cell() {
                    Some(o) => {
                        if seen.insert(o.cell_addr()) {
                            *own |= o.same_cell(c);
                            out.push(Type::TVar(o))
                        }
                    }
                    None => {
                        if seen.insert(tv.cell_addr()) {
                            out.push(t.clone())
                        }
                    }
                },
                Type::Set(ms) => ms.iter().for_each(|m| cells(m, c, seen, own, out)),
                t => {
                    if !out.contains(t) {
                        out.push(t.clone())
                    }
                }
            })
        }
        let Type::TVar(c) = pred else { return None };
        let c = c.open_cell()?;
        let t @ Type::Set(_) = self.deref_cloned()? else { return None };
        let mut own = false;
        let mut out: SmallVec<[Type; 8]> = SmallVec::new();
        cells(&t, &c, &mut LPooled::take(), &mut own, &mut out);
        (own && out.len() > 1).then_some(out)
    }

    /// `self` looked through its bindings and typedef names to what they
    /// stand for; `None` for an open cell. A chain of names ends: a
    /// typedef naming itself through names alone is refused.
    fn head(&self, env: &Env) -> Result<Option<Type>> {
        let mut t = self.deref_cloned();
        while let Some(named @ Type::Ref(_)) = &t {
            t = named.lookup_ref(env)?.deref_cloned()
        }
        Ok(t)
    }

    /// `self` looked through ([`Self::head`]) where `pred` is the same
    /// constructor with the same parts (arity, tag, field names), or a
    /// union where `pred` is a constructor; `None` anywhere else.
    // CR claude for claude: [structure/low] `open_over_any_inner`, `needs_split_inner`,
    // `narrow_parts` and `own_parts` each follow this call with the same match: tuple
    // with tuple and variant with variant zip their parts, struct with struct zips its
    // fields, array with array and list with list pair their elements. That is four
    // copies of a pairing this function has already validated (arity, tag, field
    // names), each ending in a catch-all, so a constructor added here and missed in one
    // of them is skipped silently. Return the pairs from here (the union as it stands,
    // else the paired parts, the element case marked) and let each caller loop over
    // them. Found by reading; not run. (t-tvar-b-06)
    fn shape_under(&self, env: &Env, p: &Type) -> Result<Option<Type>> {
        let Some(t) = self.head(env)? else { return Ok(None) };
        let same = match (&t, p) {
            (Type::Set(_), p) => matches!(
                p,
                Type::Tuple(_)
                    | Type::Struct(_)
                    | Type::Variant(..)
                    | Type::Array(_)
                    | Type::List(_)
            ),
            (Type::Tuple(ts), Type::Tuple(ps)) => ts.len() == ps.len(),
            (Type::Variant(tag, ts, _), Type::Variant(ptag, ps, _)) => {
                tag == ptag && ts.len() == ps.len()
            }
            (Type::Struct(fs), Type::Struct(pfs)) => {
                fs.len() == pfs.len()
                    && fs.iter().zip(pfs.iter()).all(|((n, ..), (pn, ..))| n == pn)
            }
            (Type::Array(_), Type::Array(_)) | (Type::List(_), Type::List(_)) => true,
            _ => false,
        };
        Ok(same.then_some(t))
    }

    /// `self ⊇ pred` part by part, where [`Self::needs_split`] holds: each
    /// part of `pred` narrows against its part of `self` as a pattern does
    /// against what reaches it ([`Self::narrow_pattern`]), so a union under
    /// a constructor gives a bind the union of its parts over every member
    /// its pattern can match. An open cell of `pred` that is one of the
    /// cells of the union at its part of a tuple, struct or variant
    /// narrows over that union's members ([`Self::binds_beside`],
    /// [`Self::narrow_per_member`]): the open ones are one cell, which
    /// takes what the others hold. An element's set (a slice's
    /// elements and rest) narrows member by member, as containment of a
    /// set does.
    fn narrow_parts(&self, env: &Env, pred: &Type, walk: &mut Narrowing) -> Result<bool> {
        ensure_sufficient(|| {
            let Some(p) = pred.deref_cloned() else {
                return walk.contains(env, self, pred);
            };
            let Some(t) = self.shape_under(env, &p)? else {
                return walk.contains(env, self, pred);
            };
            let mut all = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
                for (t, p) in parts {
                    let held = match t.binds_beside(p) {
                        Some(cells) => {
                            p.narrow_per_member_in(env, &cells, walk)?;
                            t.decided_holds(env, p)?
                        }
                        None => t.narrow_pattern_in(env, p, walk)?,
                    };
                    if !held {
                        return Ok(false);
                    }
                }
                Ok(true)
            };
            match (&t, &p) {
                (Type::Tuple(ts), Type::Tuple(ps))
                | (Type::Variant(_, ts, _), Type::Variant(_, ps, _)) => {
                    all(&mut ts.iter().zip(ps.iter()))
                }
                (Type::Struct(fs), Type::Struct(pfs)) => all(&mut fs
                    .iter()
                    .zip(pfs.iter())
                    .map(|((_, t, _), (_, p, _))| (t, p))),
                (Type::Array(e), Type::Array(pe)) | (Type::List(e), Type::List(pe)) => {
                    match pe.deref_cloned() {
                        Some(Type::Set(ref qs)) => {
                            for q in qs.iter() {
                                if !e.narrow_pattern_in(env, q, walk)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        }
                        _ => e.narrow_pattern_in(env, pe, walk),
                    }
                }
                _ => walk.contains(env, self, pred),
            }
        })
    }

    fn narrow_per_member_in(
        &self,
        env: &Env,
        members: &[Type],
        walk: &mut Narrowing,
    ) -> Result<()> {
        fn open(t: &Type, seen: &mut AddrSet, out: &mut SmallVec<[TVar; 8]>) {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    if seen.insert(tv.cell_addr()) {
                        match tv.binding() {
                            Some(b) => open(&b, seen, out),
                            None => out.push(tv.clone()),
                        }
                    }
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
                t => t.for_each_child(&mut |c| open(c, seen, out)),
            })
        }
        // the members of the union `t`, through its bindings and nested
        // unions, less the open cell `c`, the open cells among them apart:
        // whether `c` stood among them
        fn members_beside(
            c: &TVar,
            t: &Type,
            seen: &mut AddrSet,
            (open, rest): (&mut SmallVec<[TVar; 4]>, &mut SmallVec<[Type; 4]>),
        ) -> bool {
            ensure_sufficient(|| {
                if let Type::TVar(tv) = t {
                    if let Some(o) = tv.open_cell() {
                        let own = o.same_cell(c);
                        if !own && seen.insert(o.cell_addr()) {
                            open.push(o)
                        }
                        return own;
                    }
                    if !seen.insert(tv.cell_addr()) {
                        return false;
                    }
                }
                match &t.deref_cloned() {
                    Some(Type::Set(ms)) => ms.iter().fold(false, |own, m| {
                        members_beside(c, m, seen, (&mut *open, &mut *rest)) | own
                    }),
                    _ => {
                        rest.push(t.clone());
                        false
                    }
                }
            })
        }
        // whether `t` is one open cell and nothing else, `of`: that cell, or
        // a union of it alone, through its bindings and nested unions
        fn one_cell(t: &Type, seen: &mut AddrSet, of: &mut Option<TVar>) -> bool {
            ensure_sufficient(|| {
                if let Type::TVar(tv) = t {
                    if let Some(o) = tv.open_cell() {
                        return of.get_or_insert_with(|| o.clone()).same_cell(&o);
                    }
                    if !seen.insert(tv.cell_addr()) {
                        return true;
                    }
                }
                match &t.deref_cloned() {
                    Some(Type::Set(ms)) => ms.iter().all(|m| one_cell(m, seen, of)),
                    _ => false,
                }
            })
        }
        // maps to itself each open cell of `alt` that `m` holds at the same
        // part, alone there: it is its own part of `m`, where a copy would
        // merge back into it and leave it its lower bounds twice. A slice's
        // elements and rest that are one cell are that cell alone.
        fn own_parts(
            env: &Env,
            m: &Type,
            alt: &Type,
            seen: &mut AddrSet,
            known: &mut AHashMap<usize, TVar>,
        ) -> Result<()> {
            ensure_sufficient(|| {
                let mut a = None;
                if one_cell(alt, &mut LPooled::take(), &mut a)
                    && let Some(a) = a
                {
                    let mut held = None;
                    if one_cell(m, &mut LPooled::take(), &mut held)
                        && held.is_some_and(|h| h.same_cell(&a))
                    {
                        known.insert(a.cell_addr(), a);
                    }
                    return Ok(());
                }
                let Some(p) = alt.deref_cloned() else { return Ok(()) };
                if node_addr(&p).is_some_and(|n| !seen.insert(n)) {
                    return Ok(());
                }
                let Some(t) = m.shape_under(env, &p)? else { return Ok(()) };
                let mut each = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
                    for (t, p) in parts {
                        own_parts(env, t, p, seen, known)?
                    }
                    Ok(())
                };
                match (&t, &p) {
                    (Type::Tuple(ts), Type::Tuple(ps))
                    | (Type::Variant(_, ts, _), Type::Variant(_, ps, _)) => {
                        each(&mut ts.iter().zip(ps.iter()))
                    }
                    (Type::Struct(fs), Type::Struct(pfs)) => each(
                        &mut fs
                            .iter()
                            .zip(pfs.iter())
                            .map(|((_, t, _), (_, p, _))| (t, p)),
                    ),
                    (Type::Array(e), Type::Array(pe))
                    | (Type::List(e), Type::List(pe)) => {
                        own_parts(env, e, pe, seen, known)
                    }
                    _ => Ok(()),
                }
            })
        }
        let mut cells: SmallVec<[TVar; 8]> = SmallVec::new();
        open(self, &mut LPooled::take(), &mut cells);
        let mut parts: SmallVec<[Option<SmallVec<[(usize, Type); 4]>>; 8]> =
            cells.iter().map(|_| Some(SmallVec::new())).collect();
        let mut ends: SmallVec<[TVar; 8]> = SmallVec::new();
        let mut alts: SmallVec<[Type; 8]> = SmallVec::new();
        crate::expr::union_members(env, self, &mut alts)?;
        if alts.is_empty() {
            alts.push(self.clone())
        }
        // a member walked as one before it gives no part that one did not
        let mut walked: SmallVec<[Type; 8]> = SmallVec::new();
        let mut chain_ends = StandingEnds::default();
        for m in members {
            if m.walks_as_one_of(&walked, &mut chain_ends) {
                continue;
            }
            let mut met = false;
            for (alt_at, alt) in alts.iter().enumerate() {
                if !alt.could_match(env, m)? {
                    continue;
                }
                met = true;
                let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                own_parts(env, m, alt, &mut LPooled::take(), &mut known)?;
                // an open cell is copied before the vars that forward to it,
                // which then name its copy: no chain of links is copied
                open(alt, &mut LPooled::take(), &mut ends);
                for e in ends.drain(..) {
                    let _ = Type::TVar(e).reset_tvars_int(&mut known, Fresh::Copy);
                }
                // CR claude for claude: [perf/medium] Each member narrows against a
                // fresh copy of the predicate, so `Narrowing`'s memo, keyed by the
                // predicate's nodes, never hits beneath it: a constructor pattern
                // nested D deep over a recursive union two of whose members have its
                // shape narrows 2^D times. `type T = [(T, i64), ([T, null], string),
                // null]` under `select x { ((((v, _), _), _), _) => 1, _ => 0 }` takes
                // 2 s to check at D=16 and 30 s at D=20 (debug build), doubling per
                // level; `StructurePattern::meets` under `Select::check_undecided` then
                // walks the pattern once per member the same way. Key the memo by the
                // member and the node the copy was made from, or narrow each part once
                // against the union of the members' parts. probe:
                // design/review-2026-11-16/repro/c-pattern-nested-pattern-check-exponential.sh
                // (c-pattern-04)
                let copy = alt
                    .reset_tvars_int(&mut known, Fresh::Copy)
                    .unwrap_or_else(|| alt.clone());
                // read before the narrowing, which may move a cell of self the
                // member holds into its copy's; a cell of self an earlier
                // member's copy took forwards to that copy's, and its copy to
                // the copy of that
                let copies: SmallVec<[Option<TVar>; 8]> = cells
                    .iter()
                    .map(|c| match known.get(&c.cell_addr()) {
                        Some(f) => Some(f.clone()),
                        None => {
                            let end = c.open_cell()?;
                            let f = known.get(&end.cell_addr())?;
                            (!f.same_cell(&end)).then(|| {
                                let link = c.fresh_copy();
                                link.bind(Type::TVar(f.clone()));
                                link
                            })
                        }
                    })
                    .collect();
                if !m.narrow_member(env, &copy, walk)? {
                    continue;
                }
                // a copy left open took an open cell of the member's part,
                // whose values reach the arm as the member's others do
                let mut taken: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                super::settle::position_cells(m, &mut taken);
                for (f, p) in copies.iter().zip(parts.iter_mut()) {
                    let Some(f) = f else { continue };
                    match (f.binding(), p.as_mut()) {
                        (Some(t), Some(p)) => p.push((alt_at, t)),
                        (None, Some(p)) if taken.contains_key(&f.cell_addr()) => {
                            p.push((alt_at, Type::TVar(f.clone())))
                        }
                        (None, _) => *p = None,
                        (Some(_), None) => (),
                    }
                }
            }
            if met {
                walked.push(m.clone())
            }
        }
        for (c, p) in cells.iter().zip(parts.iter()) {
            if let Some(p) = p
                && !p.is_empty()
            {
                let cell = Type::TVar(c.clone());
                // a part that is the cell itself is a member's own part, which
                // holds the others as the arm's bind does: the open cells among
                // them are the cell, and what is left binds it
                let mut open: SmallVec<[TVar; 4]> = SmallVec::new();
                let mut rest: SmallVec<[Type; 4]> = SmallVec::new();
                let mut rest_alts: SmallVec<[usize; 4]> = SmallVec::new();
                let own = c.open_cell().is_some_and(|c| {
                    let mut seen: LPooled<AddrSet> = LPooled::take();
                    p.iter().fold(false, |own, (alt_at, t)| {
                        let own =
                            members_beside(&c, t, &mut seen, (&mut open, &mut rest))
                                | own;
                        rest_alts.resize(rest.len(), *alt_at);
                        own
                    })
                });
                let held: SmallVec<[(usize, &Type); 4]> = match own {
                    true => {
                        for o in open.drain(..) {
                            cell.decided_holds(env, &Type::TVar(o))?;
                        }
                        rest_alts.iter().copied().zip(rest.iter()).collect()
                    }
                    false => p.iter().map(|(a, t)| (*a, t)).collect(),
                };
                if held.is_empty() {
                    continue;
                }
                let tags: SmallVec<[u32; 4]> =
                    held.iter().map(|(a, _)| *a as u32).collect();
                let p: SmallVec<[&Type; 4]> = held.iter().map(|(_, t)| *t).collect();
                let joined = Type::union_exact_of_parts(env, &p, &tags)?;
                if joined.decided_holds(env, &cell)? && c.binding().is_none() {
                    joined.open_over_any(env, &cell)?
                }
            }
        }
        Ok(())
    }

    /// A copy of self with fresh type variable cells: unbound cells
    /// freshen unbound, a bound cell freshens to a fresh cell bound to
    /// the reset of its binding. self is not modified.
    pub fn reset_tvars(&self) -> Type {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Copy)
            .unwrap_or_else(|| self.clone())
    }

    /// A typedef's params and body over fresh cells this task owns, with
    /// their alias topology: what [`crate::env::Env::deftype`] aliases and
    /// freezes in place, so a definition built elsewhere is never written.
    pub fn reset_typedef_tvars(
        params: &[(TVar, Option<Type>)],
        body: &TypeDefBody,
    ) -> (Arc<[(TVar, Option<Type>)]>, TypeDefBody) {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let mut copy = |t: &Type| {
            t.reset_tvars_int(&mut known, Fresh::Copy).unwrap_or_else(|| t.clone())
        };
        let params = Arc::from_iter(params.iter().map(|(tv, bound)| {
            let tv = match &copy(&Type::TVar(tv.clone())) {
                Type::TVar(tv) => tv.clone(),
                _ => unreachable!("a variable copies to a variable"),
            };
            (tv, bound.as_ref().map(&mut copy))
        }));
        let body = match body {
            TypeDefBody::Alias(t) => TypeDefBody::Alias(copy(t)),
            TypeDefBody::Abstract(rep) => TypeDefBody::Abstract(rep.as_ref().map(copy)),
        };
        (params, body)
    }

    /// The freshening map is keyed by cell identity, not name, so an
    /// instance preserves the source's alias topology. `None` when no
    /// TVar is beneath.
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Option<Type> {
        self.reset_memo(known, &mut CopyMemo::default(), how)
    }

    /// [`Self::reset_tvars_int`], each shared composite copied once
    /// (`nodes`, by address), so a shared node stays shared.
    fn reset_memo(
        &self,
        known: &mut AHashMap<usize, TVar>,
        nodes: &mut CopyMemo,
        how: Fresh<'_>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                Some(Type::TVar(
                    tv.freshen(known, nodes, how, |t, k, n| t.reset_memo(k, n, how)),
                ))
            }
            // `cow_children` rebuilds a Ref through `with_params`, which
            // shares the resolution cell; commit copies rely on that.
            // A nested fn type is always fresh: its `lambda_ids` cell
            // must be the instance's own.
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.reset_tvars_int(known, how)))),
            t => once(t, nodes, |nodes| {
                t.cow_children(&mut |c| c.reset_memo(known, nodes, how))
            }),
        })
    }

    /// An instance's copy of a type its definition's check settled: the
    /// bindings resolved, and each open cell a closed gate owns copied
    /// once through `known` (the signature's, mapped already).
    #[doc(hidden)]
    pub fn instantiate_with(
        &self,
        known: &mut AHashMap<usize, TVar>,
        open: &IntSet<LambdaId>,
    ) -> Type {
        self.instantiate_int(known, Fresh::Instantiate(open))
            .unwrap_or_else(|| self.clone())
    }

    /// Self with every open cell `known` maps replaced by its image and
    /// every bound cell by its binding; any other cell is kept.
    #[doc(hidden)]
    pub fn rename_with(&self, known: &AHashMap<usize, TVar>) -> Type {
        self.rename_int(known).unwrap_or_else(|| self.clone())
    }

    /// Self with each open cell `image` maps, by its address, replaced by
    /// its image; every other cell, bound or open, is kept.
    pub(crate) fn swap_cells(&self, image: &dyn Fn(usize) -> Option<Type>) -> Type {
        self.swap_int(image).unwrap_or_else(|| self.clone())
    }

    fn swap_int(&self, image: &dyn Fn(usize) -> Option<Type>) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => tv.open_cell().and_then(|o| image(o.cell_addr())),
            Type::Fn(ft) => {
                ft.cow_walk(|t| t.swap_int(image)).map(|f| Type::Fn(Arc::new(f)))
            }
            t => t.cow_children(&mut |c| c.swap_int(image)),
        })
    }

    pub(super) fn rename_int(&self, known: &AHashMap<usize, TVar>) -> Option<Type> {
        self.rename_memo(known, &mut CopyMemo::default())
    }

    /// [`Self::rename_int`], each bound cell's binding rebuilt once
    /// (`memo`, by cell), so a shared binding stays shared.
    fn rename_memo(
        &self,
        known: &AHashMap<usize, TVar>,
        memo: &mut CopyMemo,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(t) => Some(tv.forwarded_as(match memo.built.get(&tv.cell_addr()) {
                    Some(r) => r.clone().unwrap_or(t),
                    None => {
                        let r = t.rename_memo(known, memo).unwrap_or(t);
                        memo.built.insert(tv.cell_addr(), Some(r.clone()));
                        r
                    }
                })),
                None => known.get(&tv.cell_addr()).map(|f| Type::TVar(f.named_as(tv))),
            },
            Type::Fn(ft) => ft.cow_walk(|t| t.rename_memo(known, memo)).map(|f| {
                let image = |tv: &TVar| Type::TVar(tv.clone()).rename_memo(known, memo);
                Type::Fn(Arc::new(keep_quantifiers(ft, f, image)))
            }),
            t => {
                once(t, memo, |memo| t.cow_children(&mut |c| c.rename_memo(known, memo)))
            }
        })
    }

    /// `None` when no TVar is beneath.
    /// The bindings resolved, each open cell freshened as `how` says.
    pub(super) fn instantiate_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Option<Type> {
        self.instantiate_memo(known, &mut CopyMemo::default(), how)
    }

    /// [`Self::instantiate_int`], each bound cell's binding rebuilt once
    /// (`memo`, by cell), so a shared binding stays shared.
    fn instantiate_memo(
        &self,
        known: &mut AHashMap<usize, TVar>,
        memo: &mut CopyMemo,
        how: Fresh<'_>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                Some(match tv.binding() {
                    Some(t) => tv.forwarded_as(match memo.built.get(&tv.cell_addr()) {
                        Some(r) => r.clone().unwrap_or(t),
                        None => {
                            let r = t.instantiate_memo(known, memo, how).unwrap_or(t);
                            memo.built.insert(tv.cell_addr(), Some(r.clone()));
                            r
                        }
                    }),
                    None => Type::TVar(tv.freshen(known, memo, how, |t, k, m| {
                        t.instantiate_memo(k, m, how)
                    })),
                })
            }
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.instantiate_how(known, how)))),
            t => once(t, memo, |memo| {
                t.cow_children(&mut |c| c.instantiate_memo(known, memo, how))
            }),
        })
    }

    /// This type as a program can write it: a cell `name` names is the
    /// type it gives, a bound cell its binding, an open cell the union of
    /// its lower bounds, and any other open cell [`TVar::written_open`].
    /// `None` where a cell has no such form, or holds itself.
    pub fn written_form(
        &self,
        name: &mut impl FnMut(&TVar) -> Option<Type>,
    ) -> Option<Type> {
        struct Walk {
            walking: LPooled<AHashSet<usize>>,
            written: LPooled<AHashMap<NodeKey, Option<Type>>>,
            unwritable: bool,
        }
        fn go(
            t: &Type,
            name: &mut impl FnMut(&TVar) -> Option<Type>,
            w: &mut Walk,
        ) -> Option<Type> {
            let key = t.node_key();
            if let Some(r) = key.and_then(|k| w.written.get(&k)) {
                return r.clone();
            }
            let r = ensure_sufficient(|| match t {
                Type::TVar(tv) => Some(match name(tv) {
                    Some(t) => t,
                    None if !w.walking.insert(tv.cell_addr()) => {
                        w.unwritable = true;
                        Type::Bottom
                    }
                    None => {
                        let r = match tv.binding() {
                            Some(b) => go(&b, name, w).unwrap_or(b),
                            None => {
                                let lower: SmallVec<[Type; 4]> = tv
                                    .cell_lower()
                                    .iter()
                                    .map(|l| go(l, name, w).unwrap_or_else(|| l.clone()))
                                    .collect();
                                match (lower.is_empty(), tv.written_open()) {
                                    (false, _) => Type::Set(Arc::from_iter(lower)),
                                    (true, Some(t)) => t,
                                    (true, None) => {
                                        w.unwritable = true;
                                        Type::Bottom
                                    }
                                }
                            }
                        };
                        w.walking.remove(&tv.cell_addr());
                        r
                    }
                }),
                t => t.cow_children(&mut |c| go(c, name, w)),
            });
            if let Some(k) = key {
                w.written.insert(k, r.clone());
            }
            r
        }
        let mut w = Walk {
            walking: LPooled::take(),
            written: LPooled::take(),
            unwritable: false,
        };
        let t = go(self, name, &mut w).unwrap_or_else(|| self.clone());
        (!w.unwritable).then(|| t.normalize())
    }

    /// A copy of self with every TVar named in `known` replaced by the
    /// corresponding type; any other TVar is freshened as
    /// [`Self::reset_tvars`] does, its conjuncts and binding rewritten
    /// the same way. TVar-free structure is returned shared. Capture
    /// avoiding under each function type it holds ([`under_quantifiers`]).
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        self.replace_tvars_copied(known, &mut LPooled::take())
    }

    /// [`Self::replace_tvars`], and in `copies` each cell it freshened by
    /// the address of the cell it copies.
    #[doc(hidden)]
    pub fn replace_tvars_copied(
        &self,
        known: &AHashMap<ArcStr, Self>,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Type {
        self.replace_tvars_int(&mut Subst::new(known), copies)
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn replace_tvars_int(
        &self,
        s: &mut Subst,
        fresh: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                Some(match s.known.get(&tv.name) {
                    Some(t) => t.clone(),
                    None => Type::TVar(tv.freshen(fresh, s, Fresh::Copy, |t, f, s| {
                        t.replace_tvars_int(s, f)
                    })),
                })
            }
            // a nested fn type's lambda_ids is the copy's own
            Type::Fn(ft) => {
                let scoped = under_quantifiers(ft, s, fresh);
                let mut f = match &scoped {
                    Some((inner, _)) => {
                        ft.replace_tvars_int(&mut Subst::new(inner), fresh)
                    }
                    None => ft.replace_tvars_int(s, fresh),
                }
                .unwrap_or_else(|| (**ft).clone());
                if let Some((_, renames)) = &scoped
                    && !renames.is_empty()
                {
                    let renamed =
                        |q: &ArcStr| match renames.iter().find(|(old, _)| old == q) {
                            Some((_, new)) => new.clone(),
                            None => q.clone(),
                        };
                    f.quantifiers = f.quantifiers.iter().map(renamed).collect();
                }
                f.lambda_ids = ft.lambda_ids.instantiate();
                Some(Type::Fn(Arc::new(f)))
            }
            // a union substituted into a union is one union: unflattened, a
            // non-regular typedef's unfoldings nest without end
            t => {
                let node = node_addr(t).filter(|_| s.bindings > 0);
                if let Some(r) = node.and_then(|n| s.built.as_ref()?.get(&n)) {
                    return r.clone();
                }
                let r = match t {
                    Type::Set(set) => {
                        Type::cow_slice(set, |c| c.replace_tvars_int(s, fresh))
                            .map(|set| Type::flatten_set(set.iter().cloned()))
                    }
                    t => t.cow_children(&mut |c| c.replace_tvars_int(s, fresh)),
                };
                if let Some(n) = node.filter(|_| r.is_some()) {
                    s.built.get_or_insert_with(LPooled::take).insert(n, r.clone());
                }
                r
            }
        })
    }

    /// Unbind any bound tvars, but do not unalias them. A cell its input's
    /// end settled stays as it was decided.
    #[doc(hidden)]
    pub fn unbind_tvars(&self) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) if tv.cell().read().facts.contains(CellFact::Settled) => (),
            Type::TVar(tv) => tv.unbind(),
            // Sig-level Ref params are concrete or rigid: nothing to unbind.
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_tvars()),
        })
    }

    /// This type with each ⊥ its input's end settled ([`CellFact::Settled`])
    /// replaced by the part of `written` at its place, children paired in
    /// order; `None` where it holds no such ⊥ or the two differ in arity on
    /// the way to one.
    #[doc(hidden)]
    pub fn fill_settled(&self, written: &Type) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                let (binding, settled) = {
                    let cell = tv.cell();
                    let c = cell.read();
                    (c.binding.clone(), c.facts.contains(CellFact::Settled))
                };
                match binding? {
                    Type::Bottom if settled => Some(written.clone()),
                    b => b.fill_settled(written),
                }
            }
            held => {
                let written = written.deref_cloned()?;
                let mut parts: SmallVec<[Type; 4]> = SmallVec::new();
                written.for_each_child(&mut |c| parts.push(c.clone()));
                let mut n = 0;
                held.for_each_child(&mut |_| n += 1);
                if n != parts.len() {
                    return None;
                }
                let mut parts = parts.iter();
                held.cow_children(&mut |c| c.fill_settled(parts.next()?))
            }
        })
    }
}

/// A narrowing's walk: the verdicts of the pairs of shared nodes it
/// narrowed where neither side reaches an open cell. Such a pair binds
/// nothing, so its verdict is the same on every path that meets it, and a
/// predicate that shares its parts is narrowed once per pair of nodes.
struct Narrowing {
    /// The names the narrowing met ([`Self::pair`]).
    refs: super::RefHist<AHashMap<super::RefPair, usize>>,
    /// Whether each shared node reaches an open cell ([`Self::open`]), the
    /// node held so its address is not recycled under its key.
    open_nodes: super::Lazy<AddrMap<bool>>,
    open_pins: super::Lazy<Vec<super::Held>>,
    held: NodePairs,
    failed: NodePairs,
    /// The pairs of shared nodes being narrowed ([`Self::pair`]), each
    /// with how many were open when it was: one met again inside itself
    /// holds, as a containment's does, so a narrowing over two recursive
    /// types ends.
    open: LPooled<AHashMap<(usize, usize), usize>>,
    /// The outermost open pair the narrowing at hand took to hold: a
    /// pair held on the word of one still open is not remembered.
    assumed: usize,
}

impl Narrowing {
    fn new() -> Self {
        Narrowing {
            refs: super::RefHist::new(),
            open_nodes: super::Lazy::new(),
            open_pins: super::Lazy::new(),
            held: NodePairs::default(),
            failed: NodePairs::default(),
            open: LPooled::take(),
            assumed: usize::MAX,
        }
    }

    fn key(&mut self, t: &Type, pred: &Type) -> Option<(NodeKey, NodeKey)> {
        let k = self.held.key(t, pred)?;
        (!self.open(t) && !self.open(pred)).then_some(k)
    }

    /// A pair [`Self::key`] keys, by its sides' structure
    /// ([`super::RefHist::param_id`]): a named type's unfolding is built
    /// anew at each visit, and is the pair the narrowing left.
    fn pair(&mut self, env: &Env, t: &Type, pred: &Type) -> (usize, usize) {
        (self.refs.param_id(t, env), self.refs.param_id(pred, env))
    }

    /// Whether `t` reaches an open cell through bindings. Within a
    /// narrowing a node that reaches none stays so (cells only bind).
    fn open(&mut self, t: &Type) -> bool {
        ensure_sufficient(|| {
            let key = t.shared_identity();
            if let Some(k) = key {
                if let Some(o) = self.open_nodes.get().and_then(|m| m.get(&k)) {
                    return *o;
                }
                self.open_pins.get_mut().push(super::Held::of(t));
                // a binding that reaches its own cell is open
                self.open_nodes.get_mut().insert(k, true);
            }
            let open = match t {
                Type::TVar(tv) if tv.ground_now() => false,
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => self.open(&b),
                    None => true,
                },
                t => {
                    t.try_for_each_child(&mut |p| match self.open(p) {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    }) == ControlFlow::Break(())
                }
            };
            if let Some(k) = key {
                self.open_nodes.get_mut().insert(k, open);
            }
            open
        })
    }

    /// `t ⊇ pred` over decided types ([`Type::decided_holds`]).
    fn contains(&mut self, env: &Env, t: &Type, pred: &Type) -> Result<bool> {
        t.decided_holds(env, pred)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arcstr::literal;

    fn array_of(tv: &TVar) -> Type {
        Type::Array(Arc::new(Type::TVar(tv.clone())))
    }

    /// `n` open cells, each bounded by an array of the next.
    fn chain_of_bounds(name: &str, n: usize) -> Vec<TVar> {
        let cells: Vec<TVar> =
            (0..n).map(|i| TVar::empty_named(arcstr::format!("{name}{i}"))).collect();
        for w in cells.windows(2) {
            w[0].add_cell_constraint(array_of(&w[1]))
        }
        cells
    }

    /// A var a merge left forwarding to another cell is that cell to an
    /// ordering, an equality and a hash, open or bound, so the two sort
    /// alike among the members of a union.
    #[test]
    fn a_forwarded_var_orders_as_the_cell_it_stands_for() {
        use netidx_value::Typ;
        use std::hash::{BuildHasher, RandomState};
        let cell = |n: &str| TVar::empty_named(ArcStr::from(n));
        let (a, b, c) = (cell("a"), cell("b"), cell("c"));
        a.bind(Type::TVar(b.clone()));
        let hasher = RandomState::new();
        for (x, y) in [(&a, &b), (&a, &c), (&c, &a)] {
            assert_eq!(x.cmp(y), Ordering::Equal);
            assert!(x == y);
            assert_eq!(hasher.hash_one(x), hasher.hash_one(y));
        }
        for members in [[&a, &c], [&c, &a], [&b, &a]] {
            assert!(members.map(array_of).is_sorted());
        }
        b.bind(Type::Primitive(Typ::I64.into()));
        let d = cell("d");
        d.bind(Type::Primitive(Typ::I64.into()));
        assert_eq!(a.cmp(&d), Ordering::Equal);
        assert!(a == d);
        assert_eq!(hasher.hash_one(&a), hasher.hash_one(&d));
        assert_eq!(c.cmp(&a), Ordering::Less);
        assert_eq!(a.cmp(&c), Ordering::Greater);
    }

    /// Two threads that read one chain of open cells, as two compile tasks
    /// read an earlier task's, each number what they walked: every link
    /// still answers for the links it reaches.
    #[test]
    fn two_threads_walking_one_chain_keep_one_order() {
        for round in 0..300 {
            let chain = chain_of_bounds("s", 200);
            let barrier = std::sync::Barrier::new(2);
            std::thread::scope(|s| {
                for _ in 0..2 {
                    let (chain, barrier) = (&chain, &barrier);
                    s.spawn(move || {
                        let own = TVar::empty_named(literal!("own"));
                        barrier.wait();
                        let from = Type::TVar(chain[0].clone());
                        assert!(!would_cycle_inner(&own.cell(), &from));
                    });
                }
            });
            for w in chain.windows(2) {
                let from = Type::TVar(w[0].clone());
                assert!(would_cycle_inner(&w[1].cell(), &from), "round {round}");
            }
        }
    }

    /// A cell another thread's walk numbered after this walk read its
    /// number may be reached by the numbered cells this walk meets: none
    /// is passed by.
    #[test]
    fn a_target_numbered_since_the_walk_began_is_not_passed_by() {
        let by = Arc::new(ReachOrder::default());
        let first = by.take(2, None).unwrap();
        let above = Place { by: by.clone(), n: first + 1 };
        let target = TVar::empty_named(literal!("t")).cell();
        let mut walk = Numbering::toward(Some(&target));
        assert!(!walk.met(&above, 0));
        target.0.write().place = Some(Box::new(Place { by, n: first }));
        assert!(walk.met(&above, 0));
        assert!(Numbering::toward(Some(&target)).met(&above, 0));
    }

    /// Threads that read one chain of open cells and write cells of their
    /// own above it, as compile tasks do over an earlier task's cells,
    /// each numbering what it walks: every occurs check answers as a walk
    /// that trusts no number does.
    #[test]
    fn threads_numbering_shared_cells_answer_as_a_fresh_walk_does() {
        let reaches = |from: &TVar, to: &TVar| {
            let mut seen: LPooled<AddrSet> = LPooled::take();
            reached_afresh(&Type::TVar(from.clone()), &mut seen);
            seen.contains(&to.cell_addr())
        };
        for round in 0..24u64 {
            let shared = chain_of_bounds("s", 64);
            let barrier = std::sync::Barrier::new(4);
            std::thread::scope(|s| {
                for thread in 0..4u64 {
                    let (shared, barrier) = (&shared, &barrier);
                    s.spawn(move || {
                        let mut seed = 0x9E3779B97F4A7C15u64 ^ (round * 4 + thread);
                        let mut rand = |n: usize| {
                            seed = seed
                                .wrapping_mul(6364136223846793005)
                                .wrapping_add(1442695040888963407);
                            (seed >> 33) as usize % n
                        };
                        let own = chain_of_bounds("o", 40);
                        let (o, n) = (own.len(), own.len() + shared.len());
                        let any = |i: usize| if i < o { &own[i] } else { &shared[i - o] };
                        barrier.wait();
                        for _ in 0..300 {
                            let (a, b) = (&own[rand(o)], any(rand(n)));
                            match rand(10) {
                                0 => a.add_cell_constraint(array_of(b)),
                                1 if !reaches(b, a) => a.add_lower(Type::TVar(b.clone())),
                                2 if !a.is_bound() && !reaches(b, a) => {
                                    a.bind(array_of(b))
                                }
                                4 => a.unbind(),
                                _ => (),
                            }
                            for _ in 0..4 {
                                let (from, to) = (any(rand(n)), any(rand(n)));
                                let found = would_cycle_inner(
                                    &to.cell(),
                                    &Type::TVar(from.clone()),
                                );
                                assert_eq!(found, reaches(from, to), "round {round}");
                            }
                        }
                    });
                }
            });
        }
    }

    /// The cells `t` reaches by the edges an occurs check walks, each
    /// walked afresh.
    fn reached_afresh(t: &Type, seen: &mut AddrSet) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if seen.insert(tv.cell_addr()) {
                    let edges = Toward::Any.edges(&tv.cell().read());
                    edges.iter().for_each(|e| reached_afresh(e, seen))
                }
            }
            t => t.for_each_child(&mut |c| reached_afresh(c, seen)),
        })
    }

    /// A copy of a chain of cells bound each to the next walks each link
    /// once to find where the chain ends, not once per cell that asks.
    #[test]
    fn a_copy_walks_a_forwarding_chain_once() {
        let n = 2000;
        let cells: Vec<TVar> =
            (0..n).map(|i| TVar::empty_named(arcstr::format!("a{i}"))).collect();
        for w in cells.windows(2) {
            w[0].bind(Type::TVar(w[1].clone()))
        }
        let head = Type::TVar(cells[0].clone());
        let end = |t: &Type| match t {
            Type::TVar(tv) => tv.open_cell().expect("open").cell_addr(),
            _ => panic!("a var"),
        };
        let copies = [(|t: &Type| t.reset_tvars()) as fn(&Type) -> Type, |t| {
            t.replace_tvars(&AHashMap::new())
        }];
        for copy in copies {
            CHAIN_STEPS.with(|s| s.set(0));
            let c = copy(&head);
            let steps = CHAIN_STEPS.with(|s| s.get());
            assert!(steps <= 2 * n, "{steps} links walked for a chain of {n}");
            assert_ne!(end(&c), cells[n - 1].cell_addr());
            assert!(matches!(&c, Type::TVar(tv) if tv.name == "a0"));
        }
    }

    /// The end of a long chain is read once while no cell is written, and
    /// again once one is: a bound end is none.
    #[test]
    fn the_end_of_a_long_chain_is_read_once_while_no_cell_is_written() {
        let n = 2000;
        let cells: Vec<TVar> =
            (0..n).map(|i| TVar::empty_named(arcstr::format!("a{i}"))).collect();
        for c in &cells {
            c.set_level(Level::TOP)
        }
        for w in cells.windows(2) {
            w[0].bind(Type::TVar(w[1].clone()))
        }
        let end = cells[n - 1].cell_addr();
        assert_eq!(cells[0].open_cell().unwrap().cell_addr(), end);
        let mut ends = StandingEnds::default();
        CHAIN_STEPS.with(|s| s.set(0));
        for _ in 0..100 {
            assert_eq!(ends.open_cell(&cells[0]), Some(end));
        }
        let steps = CHAIN_STEPS.with(|s| s.get());
        assert!(steps <= n + 8, "{steps} links walked for 100 reads of {n}");
        cells[n - 1].bind(Type::Primitive(netidx_value::Typ::I64.into()));
        assert_eq!(ends.open_cell(&cells[0]), None);
        assert_eq!(cells[0].open_cell(), None);
    }

    /// A copy from each cell of a chain, none of which it copies (a
    /// settle's witnesses, each its own copy), walks no link: a copy that
    /// holds no image yet has no image of the chain's end to name.
    #[test]
    fn copies_from_each_cell_of_a_chain_walk_no_link() {
        let n = 2000;
        let cells: Vec<TVar> =
            (0..n).map(|i| TVar::empty_named(arcstr::format!("a{i}"))).collect();
        for c in &cells {
            c.set_level(Level::TOP)
        }
        for w in cells.windows(2) {
            w[0].bind(Type::TVar(w[1].clone()))
        }
        CHAIN_STEPS.with(|s| s.set(0));
        for c in &cells {
            let t = Type::TVar(c.clone());
            let r = t.reset_tvars_int(&mut LPooled::take(), Fresh::Generic);
            assert!(matches!(&r, Some(Type::TVar(r)) if r.same_cell(c)));
        }
        let steps = CHAIN_STEPS.with(|s| s.get());
        assert!(steps <= 2 * n, "{steps} links walked for {n} copies");
    }

    /// `n` pairs, each of one bound cell twice, the cell holding the pair
    /// below it and the last one `leaf`.
    fn pairs_through_cells(n: usize, leaf: Type) -> Type {
        (0..n).fold(leaf, |t, i| {
            let part = TVar::empty_named(arcstr::format!("p{i}"));
            part.bind(t);
            Type::Tuple(Arc::from_iter([Type::TVar(part.clone()), Type::TVar(part)]))
        })
    }

    /// Whether a union stands under a constructor of a pair DAG that
    /// shares its parts through bound cells is asked once per pair of the
    /// nodes the cells hold.
    #[test]
    fn a_split_is_asked_of_a_pair_through_cells_once() {
        let n = 24;
        let leaf = Type::Primitive(netidx_value::Typ::I64.into());
        let t = pairs_through_cells(n, leaf.clone());
        let pred = pairs_through_cells(n, leaf);
        SPLIT_STEPS.with(|s| s.set(0));
        assert!(!t.needs_split(&Env::default(), &pred).unwrap());
        let steps = SPLIT_STEPS.with(|s| s.get());
        assert!(steps <= 4 * n + 128, "{steps} steps over a DAG of depth {n}");
    }

    /// A job a worker runs while its module task waits in a join (stolen
    /// work, possibly another compile's) runs under its spawner's context:
    /// its cells are its spawner's and its decisions are not the waiting
    /// module's. Under the waiting task's context the same decision is
    /// recorded as the module's foreign write and refuses it.
    #[test]
    fn a_stolen_job_runs_under_its_spawners_context() {
        let spawner = TaskContext::current();
        let old = TVar::empty_named(literal!("a"));
        let older = TVar::empty_named(literal!("c"));
        let module = new_task();
        let _task = InTask::enter(module);
        let writes = OwnWrites::enter(module);
        {
            let _cx = spawner.enter();
            assert_eq!(current_task(), TaskId::OUTSIDE);
            old.bind(Type::Bottom);
            assert!(old.is_bound());
        }
        assert_eq!(current_task(), module);
        assert!(writes.foreign().is_none());
        let writes = OwnWrites::enter(module);
        TVar::empty_named(literal!("b")).bind(Type::Bottom);
        assert!(writes.foreign().is_none(), "a cell of the module's own");
        let writes = OwnWrites::enter(module);
        older.bind(Type::Bottom);
        let w = writes.foreign().expect("an earlier task's cell, decided by the module");
        assert!(!older.is_bound(), "a decision about an earlier task's cell is not made");
        assert!(w.held_by(&Type::Tuple(Arc::from_iter([Type::TVar(older.clone())]))));
        assert!(!w.held_by(&Type::TVar(old.clone())), "the write names its own cell");
    }

    /// A task's decisions about earlier cells are recorded, each once with
    /// what it would have made the cell, and the type holding a cell is
    /// declared with that.
    #[test]
    fn foreign_decisions_record_what_they_would_make() {
        let i64 = Type::Primitive(netidx_value::Typ::I64.into());
        let string = Type::Primitive(netidx_value::Typ::String.into());
        let (a, b) = (TVar::empty_named(literal!("a")), TVar::empty_named(literal!("b")));
        let module = new_task();
        let _task = InTask::enter(module);
        let writes = OwnWrites::enter_input(module);
        a.bind(i64.clone());
        a.bind(string.clone());
        b.add_lower(string.clone());
        assert!(!a.is_bound() && !b.is_bound());
        let all = writes.foreigns();
        assert_eq!(all.len(), 2, "one record per cell");
        let held =
            Type::Tuple(Arc::from_iter([Type::TVar(a.clone()), Type::TVar(b.clone())]));
        let declared = all[0].declared(&held).expect("a's write said what it would make");
        let Type::Tuple(parts) = &declared else { panic!() };
        assert_eq!(parts[0], i64, "the first decision about a cell is the one kept");
        assert!(matches!(&parts[1], Type::TVar(v) if v.same_cell(&b)));
        assert!(
            all[1].declared(&Type::TVar(a.clone())).is_none(),
            "b's cell is not held"
        );
    }

    /// A copy knows what the source's checks learned of each cell.
    #[test]
    fn a_copy_keeps_its_cells_facts() {
        let tv = TVar::empty_named(literal!("a"));
        tv.bind(Type::Bottom);
        let all = CellFact::BottomRequired | CellFact::BottomFed;
        tv.cell().write().facts = all;
        let copy = Type::TVar(tv.clone()).reset_tvars();
        let Type::TVar(copy) = &copy else { panic!() };
        assert!(!copy.same_cell(&tv));
        assert_eq!(copy.cell().read().facts, all);
    }

    /// A load's reopening of its signature's cells leaves what an
    /// input's end settled as it was decided.
    #[test]
    fn unbinding_leaves_a_settled_cell() {
        let i64 = Type::Primitive(netidx_value::Typ::I64.into());
        let [settled, open] = ["s", "o"].map(|n| TVar::empty_named(n.into()));
        settled.bind(i64.clone());
        settled.mark_settled();
        open.bind(i64.clone());
        let t = Type::Tuple(Arc::from_iter([
            Type::TVar(settled.clone()),
            Type::TVar(open.clone()),
        ]));
        t.unbind_tvars();
        assert_eq!(settled.binding(), Some(i64));
        assert!(!open.is_bound());
    }

    /// A union's cells print as what they hold, so two cells bound to one
    /// type, or two names of one cell, print once.
    #[test]
    fn cells_bound_alike_print_once() {
        let i64 = Type::Primitive(netidx_value::Typ::I64.into());
        let string = Type::Primitive(netidx_value::Typ::String.into());
        let (a, b) =
            (TVar::empty_named(literal!("_1")), TVar::empty_named(literal!("_2")));
        a.bind(Type::Array(Arc::new(i64.clone())));
        b.bind(Type::Array(Arc::new(i64.clone())));
        let c = TVar::empty_named(literal!("_3"));
        let d = c.renamed(literal!("_4"));
        let shown = |members: Vec<Type>| {
            crate::format_with_flags(PrintFlag::DerefTVars, || {
                Type::Set(Arc::from_iter(members)).to_string()
            })
        };
        assert_eq!(
            shown(vec![Type::TVar(a.clone()), Type::TVar(b.clone())]),
            "Array<i64>"
        );
        assert_eq!(
            shown(vec![Type::TVar(a), string.clone(), Type::TVar(b)]),
            "[Array<i64>, string]"
        );
        assert_eq!(shown(vec![Type::TVar(c), Type::TVar(d)]), "'_3: unbound");
        assert_eq!(shown(vec![i64, string]), "[i64, string]");
    }

    /// A type as a program writes it: a bound cell its binding, an open one
    /// the union of its lower bounds, a named one its name, one the program
    /// wrote its variable; one the checker made up has no written form.
    #[test]
    fn written_form_writes_each_cell() {
        let i64 = Type::Primitive(netidx_value::Typ::I64.into());
        let (bound, below, named, open) = (
            TVar::empty_named(literal!("_1")),
            TVar::empty_named(literal!("_2")),
            TVar::empty_named(literal!("_3")),
            TVar::empty_named(literal!("_4")),
        );
        bound.bind(Type::Array(Arc::new(Type::TVar(named.clone()))));
        below.add_lower(i64.clone());
        below.add_lower(Type::Array(Arc::new(i64.clone())));
        let t = Type::Tuple(Arc::from_iter(
            [&bound, &below, &open].map(|tv| Type::TVar(tv.clone())),
        ));
        let x = Type::TVar(TVar::empty_named(literal!("x")));
        let mut name = |tv: &TVar| tv.same_cell(&named).then(|| x.clone());
        assert_eq!(t.written_form(&mut name), None);
        let t = Type::Tuple(Arc::from_iter(
            [&bound, &below].map(|tv| Type::TVar(tv.clone())),
        ));
        let written = t.written_form(&mut name).unwrap();
        assert_eq!(written.to_string(), "(Array<'x>, [i64, Array<i64>])");
        let a = Type::TVar(TVar::empty_named(literal!("a")));
        assert_eq!(a.written_form(&mut |_| None).unwrap().to_string(), "'a");
        let i64_null = Type::Primitive(netidx_value::Typ::I64 | netidx_value::Typ::Null);
        let both = Type::Set(Arc::from_iter([i64, i64_null]));
        assert_eq!(both.written_form(&mut |_| None).unwrap().to_string(), "[i64, null]");
    }

    /// A copy's nested fn type collects the lambdas checked against the
    /// copy alone.
    #[test]
    fn replaced_nested_fn_has_its_own_lambda_ids() {
        let t = crate::expr::parser::parse_type("{f: fn(x: i64) -> i64, g: 'a}").unwrap();
        let mut known: AHashMap<ArcStr, Type> = AHashMap::default();
        known.insert(literal!("a"), Type::Any);
        let copy = t.replace_tvars(&known);
        let ids = |t: &Type| match t {
            Type::Struct(fs) => match &fs[0].1 {
                Type::Fn(ft) => ft.lambda_ids.addr(),
                t => panic!("{t:?}"),
            },
            t => panic!("{t:?}"),
        };
        assert_ne!(ids(&t), ids(&copy));
    }

    /// Each scoping of a written type, a function type or one nested in a
    /// struct, has a node of its own, tvars or not.
    #[test]
    fn scoped_fn_types_have_their_own_lambda_ids() {
        let root = ModPath::root();
        let addr = |t: &Type| match t {
            Type::Fn(ft) => ft.lambda_ids.addr(),
            Type::Struct(fs) => match &fs[0].1 {
                Type::Fn(ft) => ft.lambda_ids.addr(),
                t => panic!("{t:?}"),
            },
            t => panic!("{t:?}"),
        };
        for src in [
            "fn(?#d: 'q, x: i64) -> string",
            "fn(x: i64) -> string",
            "{f: fn(?#d: 'q, x: i64) -> string}",
            "{f: fn(x: i64) -> string}",
        ] {
            let t = crate::expr::parser::parse_type(src).unwrap();
            let (a, b) = (t.scope_refs(&root), t.scope_refs(&root));
            assert_ne!(addr(&a), addr(&b), "{src}");
            assert_ne!(addr(&a), addr(&t), "{src}");
        }
    }
}

thread_local! {
    /// The cells whose contents the syntax codec is writing, innermost
    /// last.
    static WRITING: RefCell<SmallVec<[usize; 8]>> = RefCell::new(SmallVec::new());
}

/// What the syntax codec writes of `tv`'s cell, its bound and constraints;
/// `None` for a cell met inside its own contents, which has no syntax: the
/// parser refuses a bound that reaches its own variable.
fn cell_contents<R>(tv: &TVar, f: impl FnOnce(Option<&Type>, &[Type]) -> R) -> Option<R> {
    struct Writing;
    impl Drop for Writing {
        fn drop(&mut self) {
            WRITING.with_borrow_mut(|w| w.pop());
        }
    }
    let cell = tv.read().cell.upgrade();
    let key = cell.data_ptr().addr();
    if WRITING.with_borrow(|w| w.contains(&key)) {
        return None;
    }
    WRITING.with_borrow_mut(|w| w.push(key));
    let _writing = Writing;
    let cell = cell.read();
    Some(f(cell.binding.as_ref(), &cell.constraints))
}

/// Under an image session the wrapper and its cell are shared objects
/// ([`image::tvar_encode`]); the syntax codec writes the cell's
/// contents (`Option<Type>`, then `Vec<Type>`) and mints a fresh variable.
impl Pack for TVar {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() {
            return image::tvar_len(self);
        }
        self.name.encoded_len()
            + cell_contents(self, |bound, constraints| {
                1 + bound.map_or(0, |t| t.encoded_len())
                    + constraints
                        .iter()
                        .fold(pack::varint_len(constraints.len() as u64), |n, t| {
                            n + t.encoded_len()
                        })
            })
            .unwrap_or(0)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            return image::tvar_encode(self, buf);
        }
        self.name.encode(buf)?;
        cell_contents(self, |bound, constraints| {
            match bound {
                None => buf.put_u8(0),
                Some(t) => {
                    buf.put_u8(1);
                    t.encode(buf)?
                }
            }
            pack::encode_varint(constraints.len() as u64, buf);
            constraints.iter().try_for_each(|t| t.encode(buf))
        })
        .unwrap_or(Err(PackError::InvalidFormat))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            return image::tvar_decode(buf);
        }
        let name = <ArcStr as Pack>::decode(buf)?;
        let bound = <Option<Type> as Pack>::decode(buf)?;
        let constraints = <Vec<Type> as Pack>::decode(buf)?;
        // A fresh id is sound: the typechecker re-aliases same-named tvars
        // within a scope.
        let tv = TVar::empty_generic(name);
        if let Some(t) = bound {
            tv.bind(t)
        }
        {
            let cell = tv.cell();
            let mut cell = cell.write();
            for c in constraints {
                cell.add_constraint(c);
            }
        }
        Ok(tv)
    }
}

/// What a type read as a union counts of its members.
#[derive(Clone, Copy)]
enum Measure {
    /// Every member; a primitive set by its flags, `Any` as several.
    Members,
    /// The numeric members; a primitive set by its numeric flags, `Any`
    /// as several.
    Numbers,
}

/// Why a union's rigid member keeps it from being one type.
enum Apart {
    BesideKnown,
    TwoRigid,
}

/// A type read as a union ([`Type::union_parts`]): how many known members
/// it has by its measure, the first of them, and its open cells.
struct UnionParts {
    measure: Measure,
    known: usize,
    first: Option<Type>,
    open: SmallVec<[TVar; 2]>,
    /// The primitives and the other members counted so far: a member
    /// met twice is one.
    prims: BitFlags<netidx_value::Typ>,
    others: SmallVec<[Type; 2]>,
    /// The open cells, pair by pair, of members that differ in them
    /// alone (`[Array<'y>, Array<'z>]`): one member once they are one.
    same: SmallVec<[(TVar, TVar); 2]>,
}

impl UnionParts {
    fn new(measure: Measure) -> Self {
        Self {
            measure,
            known: 0,
            first: None,
            open: SmallVec::new(),
            prims: BitFlags::empty(),
            others: SmallVec::new(),
            same: SmallVec::new(),
        }
    }

    /// A rigid open member that cannot be made one type with the rest:
    /// beside a known member, or beside another rigid cell.
    fn rigid_apart(&self) -> Option<Apart> {
        let mut rigid = self.open.iter().filter(|tv| tv.is_rigid());
        let a = rigid.next()?;
        match self.known > 0 {
            true => Some(Apart::BesideKnown),
            false => {
                rigid.any(|b| b.cell_addr() != a.cell_addr()).then_some(Apart::TwoRigid)
            }
        }
    }

    /// Count each rigid open member, a fixed signature's variable, as the
    /// numeric types its caller may pick: those every type among its
    /// conjuncts holds as a member (any, where a member of the conjunct
    /// is `Any` or open), or one more where a conjunct implies
    /// `OneNumber` and several of them are new.
    fn count_rigid_numbers(&mut self, env: &Env) -> Result<()> {
        let one_number = Type::Bound(Bound::OneNumber);
        let open = mem::take(&mut self.open);
        for (i, tv) in open.iter().enumerate() {
            if !tv.is_rigid() || open[..i].iter().any(|o| o.same_cell(tv)) {
                continue;
            }
            let mut numbers = netidx_value::Typ::number();
            for c in tv.cell_constraints().iter() {
                if !(c.is_predicate() || c.is_trait_ref(env)) {
                    let mut own = UnionParts::new(Measure::Numbers);
                    c.union_parts(Some((env, false)), &mut own)?;
                    // `known` beyond the numeric primitives is an `Any` member
                    let named = (own.prims & netidx_value::Typ::number()).len();
                    if own.open.is_empty() && own.known == named {
                        numbers &= own.prims
                    }
                }
            }
            let new = (numbers & !self.prims).len();
            if new > 1 && tv.conjunct_implies(env, &one_number)? {
                self.known += 1
            } else {
                self.prims |= numbers;
                self.known += new
            }
        }
        self.open = open;
        Ok(())
    }

    /// Whether `t` is a member counted already: identical to one, or
    /// differing from one in open cells alone, whose pairs `same` records.
    fn same_member(&mut self, t: &Type) -> bool {
        if self.others.iter().any(|o| union_identical(o, t)) {
            return true;
        }
        let start = self.same.len();
        for o in self.others.iter() {
            if o == t && open_pairs(o, t, &mut self.same) {
                return true;
            }
            self.same.truncate(start);
        }
        false
    }

    fn member(&mut self, t: &Type) {
        let n = match (self.measure, t) {
            (measure, Type::Primitive(p)) => {
                let new = *p & !self.prims;
                self.prims |= *p;
                match measure {
                    Measure::Members => new.len(),
                    Measure::Numbers => (new & netidx_value::Typ::number()).len(),
                }
            }
            (_, Type::Any) => 2,
            (_, t) if self.same_member(t) => 0,
            (measure, t) => {
                self.others.push(t.clone());
                match measure {
                    Measure::Members => 1,
                    Measure::Numbers => 0,
                }
            }
        };
        self.known += n;
        if n > 0 && self.first.is_none() {
            self.first = Some(t.clone())
        }
    }
}
