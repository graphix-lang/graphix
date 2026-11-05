use crate::{
    AddrSet, LambdaId,
    dbgenv::graphix_dbg_bind,
    env::{Bind, Env},
    expr::{ModPath, Origin, TypeDefBody},
    image,
    stack::ensure_sufficient,
    typ::{
        Bound, FnType, Mutability, NodePairs, PRINT_FLAGS, PrintFlag, RefKey, Type,
        node_addr, setops::union_identical,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Error, Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags};
use netidx_core::pack::{self, Pack, PackError};
use nohash::IntSet;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::hash_map::Entry,
    fmt::{self, Debug},
    hash::Hash,
    mem::ManuallyDrop,
    ops::{ControlFlow, Deref},
    sync::atomic::{self, AtomicU64},
};
use triomphe::Arc;

image_id!(TVarId);

#[cfg(test)]
thread_local! {
    pub(crate) static MAY_BE_ERROR_NAMES: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    pub(super) static SPLIT_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
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

type CellRef = Arc<RwLock<TCell>>;

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
        let mut c = cell.write();
        if c.binding.is_some() {
            c.ground = c.ground.max(epoch);
        }
    }
}

pub(super) fn would_cycle_inner(addr: usize, t: &Type) -> bool {
    CycleWalk::new(addr, false).go(t, Toward::Any, false).is_break()
}

/// [`would_cycle_inner`] toward the open cell at `addr`: false at once
/// for a type that reaches no open cell, which its bound cells remember,
/// so a chain of bound cells is walked once however many cells ask.
pub(super) fn would_cycle_open(addr: usize, t: &Type) -> bool {
    let epoch = ground_epoch();
    let mut walked: LPooled<Vec<CellRef>> = LPooled::take();
    if reaches_no_open_cell(t, epoch, &mut LPooled::take(), &mut walked) {
        mark_ground(&mut walked, epoch);
        return false;
    }
    would_cycle_inner(addr, t)
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

/// The vars on the paths [`Cycle::Bare`] names from `t` to the cell at
/// `addr`, by the edges `toward` takes: each var `t` holds or reaches
/// through cells and unions alone whose cell reaches that cell so.
pub(super) fn bare_path(addr: usize, t: &Type, toward: Toward) -> LPooled<Vec<TVar>> {
    let mut walk = BarePath { addr, reaches: LPooled::take(), on: LPooled::take() };
    walk.go(t, toward);
    walk.on
}

struct BarePath {
    addr: usize,
    /// Per node or cell and direction, whether it reaches the cell; false
    /// while it is walked.
    reaches: LPooled<AHashMap<(usize, u8), bool>>,
    on: LPooled<Vec<TVar>>,
}

impl BarePath {
    fn go(&mut self, t: &Type, toward: Toward) -> bool {
        ensure_sufficient(|| {
            let key = match t {
                Type::TVar(tv) if tv.cell_addr() == self.addr => return true,
                Type::TVar(tv) => Some(tv.cell_addr()),
                t => node_addr(t),
            };
            if let Some(key) = key {
                match self.reaches.entry((key, toward as u8)) {
                    Entry::Occupied(e) => return *e.get(),
                    Entry::Vacant(e) => {
                        e.insert(false);
                    }
                }
            }
            let reaches = match t {
                Type::Set(ms) => ms.iter().fold(false, |r, m| self.go(m, toward) | r),
                Type::Ref(r) => {
                    let def = r.resolved();
                    r.params.iter().enumerate().fold(false, |reaches, (i, p)| {
                        let guarded = def.as_ref().is_some_and(|d| d.param_guarded(i));
                        (!guarded && self.go(p, Toward::Any)) | reaches
                    })
                }
                Type::TVar(tv) => {
                    let edges = toward.edges(&tv.cell().read());
                    let reaches = edges.iter().fold(false, |r, e| self.go(e, toward) | r);
                    if reaches {
                        self.on.push(tv.clone());
                    }
                    reaches
                }
                _ => false,
            };
            if reaches && let Some(key) = key {
                self.reaches.insert((key, toward as u8), true);
            }
            reaches
        })
    }
}

/// How `t` reaches the cell at `addr` by the edges `toward` takes, a
/// constructor path over a bare one.
pub(super) fn cycle_toward(addr: usize, t: &Type, toward: Toward) -> Option<Cycle> {
    if !would_cycle_inner(addr, t) {
        return None;
    }
    let mut walk = CycleWalk::new(addr, true);
    match walk.go(t, toward, false) {
        ControlFlow::Break(()) => Some(Cycle::Constructor),
        ControlFlow::Continue(()) => walk.bare.then_some(Cycle::Bare),
    }
}

enum Visits {
    /// Each node and cell once.
    Once(LPooled<AddrSet>),
    /// Each node and cell once per direction and per being under a
    /// constructor: a visit in one state may reach the cell only in
    /// another.
    PerState(LPooled<AHashSet<(usize, u8)>>),
}

/// A walk from a type to the cell at `addr` through bindings, conjuncts
/// and lower bounds. Untracked, it breaks at the first path; tracking,
/// at the first path under a constructor, and it records a bare one.
struct CycleWalk {
    addr: usize,
    visits: Visits,
    bare: bool,
}

impl CycleWalk {
    fn new(addr: usize, track: bool) -> Self {
        let visits = match track {
            false => Visits::Once(LPooled::take()),
            true => Visits::PerState(LPooled::take()),
        };
        Self { addr, visits, bare: false }
    }

    fn tracking(&self) -> bool {
        matches!(self.visits, Visits::PerState(_))
    }

    fn visit(&mut self, key: usize, toward: Toward, under: bool) -> bool {
        match &mut self.visits {
            Visits::Once(seen) => seen.insert(key),
            Visits::PerState(seen) => seen.insert((key, toward as u8 * 2 + under as u8)),
        }
    }

    fn go(&mut self, t: &Type, toward: Toward, under: bool) -> ControlFlow<()> {
        ensure_sufficient(|| {
            if let Some(node) = node_addr(t)
                && !self.visit(node, toward, under)
            {
                return ControlFlow::Continue(());
            }
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
                Type::TVar(tv) => {
                    let cell = tv.cell();
                    let cell_addr = cell.data_ptr().addr();
                    if cell_addr == self.addr {
                        if under || !parts_under {
                            return ControlFlow::Break(());
                        }
                        self.bare = true;
                        return ControlFlow::Continue(());
                    }
                    if !self.visit(cell_addr, toward, under) {
                        return ControlFlow::Continue(());
                    }
                    let edges = toward.edges(&cell.read());
                    edges.iter().try_for_each(|c| self.go(c, toward, under))
                }
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
                Type::Abstract { .. }
                | Type::App(..)
                | Type::ByRef(Mutability::Mut, _) => {
                    t.try_for_each_child(&mut |c| self.go(c, Toward::Any, parts_under))
                }
                t => t.try_for_each_child(&mut |c| self.go(c, toward, parts_under)),
            }
        })
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

    /// The definition that owns the cell.
    #[doc(hidden)]
    pub fn owner(&self) -> Option<LambdaId> {
        match self {
            Self::Def { owner, .. } => Some(*owner),
            Self::Top | Self::Generic | Self::Written => None,
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
    static OWN_WRITES: std::cell::Cell<Option<Record>> =
        const { std::cell::Cell::new(None) };
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
#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct TaskContext {
    task: TaskId,
    level: Level,
}

impl TaskContext {
    /// The context this thread runs.
    #[doc(hidden)]
    pub fn current() -> Self {
        Self { task: TASK.get(), level: LEVEL.get() }
    }

    /// This context as compile task `task`'s.
    #[doc(hidden)]
    pub fn with_task(self, task: TaskId) -> Self {
        Self { task, ..self }
    }

    /// Run as this context until the guard drops. The thread's record of
    /// foreign writes ([`OwnWrites`]) is its waiting task's and is set
    /// aside meanwhile: a task's record is kept on the thread it runs
    /// whole on.
    #[doc(hidden)]
    pub fn enter(self) -> InContext {
        InContext {
            task: TASK.replace(self.task),
            level: LEVEL.replace(self.level),
            own: OWN_WRITES.replace(None),
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
    own: Option<Record>,
}

impl Drop for InContext {
    fn drop(&mut self) {
        TASK.set(self.task);
        LEVEL.set(self.level);
        OWN_WRITES.set(self.own);
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
    decided_of(cell.task, "cell", Some(cell_addr(cell)))
}

fn decided_of(owner: TaskId, what: &str, cell: Option<usize>) -> bool {
    if owner >= TASK.get() {
        return true;
    }
    foreign_write(owner, what, cell);
    !OWN_WRITES.get().is_some_and(|r| r.task == TASK.get())
}

fn written_of(owner: TaskId, what: &str, cell: Option<usize>) {
    if owner < TASK.get() {
        foreign_write(owner, what, cell)
    }
}

/// A compile task's record of its writes outside itself ([`OwnWrites`]).
#[derive(Clone, Copy)]
struct Record {
    task: TaskId,
    /// The task is a top-level input's ([`OwnWrites::enter_input`]).
    input: bool,
    /// The first write it made outside itself.
    first: Option<ForeignWrite>,
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
        Self(OWN_WRITES.replace(Some(Record { task, input: false, first: None })))
    }

    /// [`Self::enter`] for a top-level input's task (a REPL input, a
    /// script, a dynamic load): see [`in_input`].
    #[doc(hidden)]
    pub fn enter_input(task: TaskId) -> Self {
        Self(OWN_WRITES.replace(Some(Record { task, input: true, first: None })))
    }

    /// The first write the task made outside itself.
    #[doc(hidden)]
    pub fn foreign(&self) -> Option<ForeignWrite> {
        OWN_WRITES.get().and_then(|r| r.first)
    }
}

/// Whether the running task is a top-level input's own check. There what
/// an earlier input left open, a function type's own quantifier included,
/// is one type, compared as itself and never as a fresh copy, so a
/// decision about it is recorded and not made.
pub(super) fn in_input() -> bool {
    OWN_WRITES.get().is_some_and(|r| r.input && r.task == TASK.get())
}

/// A compile task's write to what an earlier task created: the cell, when
/// it was one.
#[derive(Clone, Copy)]
#[doc(hidden)]
pub struct ForeignWrite {
    cell: Option<usize>,
}

impl ForeignWrite {
    /// Whether `t` reaches the cell written.
    #[doc(hidden)]
    pub fn held_by(&self, t: &Type) -> bool {
        self.cell.is_some_and(|c| t.holds_cell(c))
    }
}

impl Drop for OwnWrites {
    fn drop(&mut self) {
        OWN_WRITES.set(self.0)
    }
}

#[cold]
fn foreign_write(owner: TaskId, what: &str, cell: Option<usize>) {
    if let Some(r) = OWN_WRITES.get()
        && r.task == TASK.get()
        && r.first.is_none_or(|f| f.cell.is_none() && cell.is_some())
    {
        OWN_WRITES.set(Some(Record { first: Some(ForeignWrite { cell }), ..r }))
    }
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

/// `rebuild` of `t`, made once per shared composite (`memo`, by address;
/// a cell's address is another allocation, so a cell memo shares the map).
fn once(
    t: &Type,
    memo: &mut AHashMap<usize, Option<Type>>,
    rebuild: impl FnOnce(&mut AHashMap<usize, Option<Type>>) -> Option<Type>,
) -> Option<Type> {
    let Some(n) = node_addr(t) else { return rebuild(memo) };
    if let Some(r) = memo.get(&n) {
        return r.clone();
    }
    let r = rebuild(memo);
    memo.insert(n, r.clone());
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

// CR claude for claude: [bug/medium] A collection HOF's callback can leave its generic
// cells in an outer binding's type, with no rank-2 type anywhere: `type W<'t> =
// Abstract<'t>; let y = never(); let g = |x| { y <- array::map([x], |e| W(e)); x };
// let b = g("s"); let n = select y { [w, ..] => w.0 + 1, _ => 0 }` passes --check and
// the build refuses g("s") ("an instance at fn(x: string) -> string of a definition
// typed fn(x: i64) -> i64"); a named callback (`let mkw = |e| W(e)`, gen_map4) the
// same, main ec53d4846 the same. Under GRAPHIX_DBG_BIND the map site's 'b is bound to a
// copy of the callback's result W<'g> sharing 'g, the callback's generic cell (contains'
// TT-LEFTCOPY, TVar::copy), and this walk never lowers a generic cell, so y's type
// holds 'g and reaches g's parameter only through generic cells: g stays generic in
// 'x though y depends on it, and each call's copy of 'x decides nothing of y. A user
// HOF with written variables (`|g: fn(v: 'p) -> 'q, v: 'p| -> 'q g(v)`, gen_app2) is
// refused by the check and the build alike. Options: instantiate a callback's scheme
// where a call's formal contains it, as a reference's read copies a generalized
// binding; or lower the generic cells a non-generic cell's binding reaches where no
// let names them. Its rank-2 form is refused through callsite::stand_ins
// (rank2-escape-hof-01). probes: ~/ptmp/cr29/scratch/r9-R/{gen_map,gen_map2,gen_map4,
// gen_map5,gen_app2}.gx (callback-generic-leak-01)
// 2026-11-03 claude: Review r9 (R-r1-1, R-r1-2): 'Its rank-2 form is refused through
// callsite::stand_ins' holds only where CompileCtx::monomorphic_noting made the copy
// (stand_ins is now check_general's copied cells). A read of the let-bound lambda held
// by a written quantified type passes --check and the build or run-time bind refuses (a
// call argument ref1_arg, a let annotation ref2_let, a connect ref3_conn and c1, a
// block's value c5_block), and so does a select or array of lambda literals, which
// contains copies into its result cell (c4_sel, c4e_let, c4h_arr, a1_selarg, a3_appconn,
// e2_default_sel; their non-rank-2 analog c4b_norank2 is refused); main ec53d4846 the
// same. The second option here closes these too: with the callback's cell lowered the
// lambda's parameter cell is no longer generic, a copy shares it, and check_general's
// walk meets the quantifier through the outer binding. rank2-escape-hof-01 is a CR
// again, readdressed to eric with this one. probes:
// ~/ptmp/cr29/scratch/review-r9/R-r1-1/p/{ref1_arg,ref2_let,ref3_conn,c5_block,c4_sel,c4e_let,c4h_arr,a1_selarg,a3_appconn,e2_default_sel,c4b_norank2}.gx,
// review-r9/R-r1-2/p/c1.gx
// 2026-11-03 claude: Lane R salvage (branch cr46-R2): nothing refuses the rank-2 form now. The copied-cell walk is withdrawn for its cost
// (rank2-escape-hof-01's note of today: CompileCtx::monomorphic_noting and callsite::stand_ins, later check_general's copied cells, are gone), so this
// CR's 'Its rank-2 form is refused through callsite::stand_ins' and the note above's 'holds only where CompileCtx::monomorphic_noting made the copy' no
// longer hold: every rank-2 copy shape, map_conn and kin as well as the review r9 select, array and read copies, passes --check with main ec53d4846's
// verdict. This CR's second option remains the fix that closes them all.
// 2026-11-03 claude: Eric ruled (2026-11-03): D14 (b) in ~/ptmp/cr-discuss.md, under his rule for type holes (reject and require an annotation, then a
// --check census; back to him only if the stdlib, the book's examples or netidx-admin would need a new annotation): when a non-generic binding's type
// reaches a generic variable no let names, that variable stops being generic (ML's level rule); census and the must-reject lane before landing.
// 2026-11-04 claude: Review r14 D-r2-1 (cr49-D) found that this leak computes wrong values, not only late refusals, wherever no run-time bind stands
// between the leaked cell and its reader: in the REPL, p = { let y = never(); let g = |x| { y <- array::map([x], |e| W(e)); x }; (g, &y) }, then
// p.0("abc"), let k = |u| *p.1, let w: fn(u: i64) -> Array<W<i64>> = k makes w(0)[0]$.0 an i64 holding "abc" (+ 1 gives 1; d14k, d14i); through a function
// type alone, let h = { ..; let a = g0("abc"); |u| y0 } then let w: fn(u: i64) -> Array<W<i64>> = h gives the same (~/ptmp/cr29/scratch/r14-D/r3/h1.txt);
// and in a plain script, a static module whose .gxi says val k: fn(u: i64) -> Array<super::W<'b>> over the leak computes str::len of an i64 as 0 (D-r2-1
// sm3; dl3 its dynamic twin). main 968fa9bc0 and cr49-D alike. D9 (a)'s input-end settle settles top-level cells only, and the leaked cell is the
// callback's generic one, which each read copies: settling the open cells a binding's type holds outside function types would close d14k and d14i but not
// h1, so this CR's ruled fix, D14 (b) (lowering the cell to the top level, where the input-end settle then settles it), is the one that closes all three.
// probes ~/ptmp/cr29/scratch/review-r14/D-r2-1/{d14k,d14i}.txt, sm3/, dl3/.
// 2026-11-04 claude: Correction to the 2026-11-04 note above (review r14 final, D-r3-2): on cr49-D the REPL routes are refused, not computed: d14k and
// d14i are refused naming y, g and p, and h1 naming h (this input would decide a type of an earlier input), and since fix round 3 so are the load routes
// that read the program's leaked cell (ld3, dl6, dl4/main2: the load is an input under OwnWrites). Still computing wrong values on cr49-D as on main: the
// script route sm3, and dl3, where the leak is inside the loaded body and the program decides its val's 'b.
// 2026-11-04 claude: Pins for the previous note's claims: later_input_decides_no_leaked_callback_cell (the REPL route: d14k, d14i, h1) and
// load_decides_no_type_the_program_left_open (the load route: ld3, dl6, dl4/main2), both in lang::repl_inputs. Those cover the d14k family only. Review
// r14 D-r4-1 found a route still open, main's too (968fa9bc0 behaves the same): an unannotated writer of an earlier input's generic-function binding. With
// `let s = {g: |x| x}` and `let j = s.g`, then `let w2 = |f| { j <- f; null }` and `w2(|x: i64| -> i64 x + 1)` (mm3), or the same writer in a dynamic
// load's body (mm4) or in a later input's `mod m` with an interface (mm1, whose module task compares j's quantifier as a fresh copy since in_input is
// false there), writes a monomorphic function into j while j stays fn(x: 'C) -> 'C. `j("s")` then passes the check and fails its run-time bind naming no
// binding (no wrong value: the bind is the backstop). The direct `j <- |x: i64| -> i64 x + 1` and an annotated writer (mm2) are refused. It goes through
// because a call copies the earlier quantifier the writer's parameter type holds. probe:
// ~/ptmp/cr29/scratch/review-r14/D-r4-1/new/mm/{mm1,mm2,mm3,mm4}.txt (GRAPHIX_MODPATH=$PWD ../../r.sh lane FILE).
// 2026-11-05 claude: Lane Q (cr51-Q, Eric's 11-05 ruling: (e) plus the ML rule) closes this CR's probes as a side effect of the ML rule, not by D14 (b) as
// ruled: a lambda no binding names (the collection callback |e| W(e) here) takes its enclosing level's cells at its gate's close instead of generic ones
// (Lambda::named, DefGate::close), and a read of a generalized binding no binding names is one instance (Ref::named), so the callback leaves no generic
// cell in y's type and g is one type in x. gen_map, gen_map2, gen_map4, gen_map5 are now refused by --check (type mismatch i64 does not contain string, or
// cannot compute string + i64), as gen_app2 was; the review r9 rank-2 forms (ref1_arg .. e2_default_sel, c1) are refused by the new rule on written
// quantified function types; c4b_norank2 by the check. Left for D14's owner to decide whether this closes the CR or D14 (b) is still wanted for a shape
// the ML rule misses; no probe here still holds. Pins: lang::quantified::a_callbacks_result_is_one_instance,
// a_generic_binding_read_into_a_callbacks_result_is_one_instance.
/// Lower every cell `t` reaches to at most `level`: what a cell at
/// `level` is bound to, or constrained by, belongs where the cell does.
/// A scheme's generic cell stays generic (a scheme bound into a cell is
/// still a scheme), and a cell already at or above `level`'s depth
/// reaches nothing deeper.
pub(super) fn lower(t: &Type, level: Level) {
    fn go(t: &Type, level: Level, nodes: &mut AHashSet<usize>) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if tv.level() != Level::Generic => tv.claim(level),
            Type::TVar(_) => (),
            t if node_addr(t).is_some_and(|n| !nodes.insert(n)) => (),
            t => t.for_each_child(&mut |c| go(c, level, nodes)),
        })
    }
    if !level.is_generic() {
        go(t, level, &mut LPooled::take())
    }
}

/// How a copy of a type treats its cells: `Copy` makes a fresh cell per
/// cell at the source's level; `Instantiate` is a call's copy, a fresh
/// cell at the current level per cell a call copies
/// ([`Level::copied_by`] the open gates), every other cell shared;
/// `Scheme` is `Instantiate` whose fresh cells are generic, a
/// reference's copy of the scheme it names; `Generic` copies the generic
/// cells alone, a stored scheme's, and shares every live one.
#[derive(Clone, Copy)]
pub(crate) enum Fresh<'a> {
    Copy,
    Instantiate(&'a IntSet<LambdaId>),
    Scheme(&'a IntSet<LambdaId>),
    Generic,
}

/// The shared binding cell: aliased `TVar`s hold one `Arc` of this.
/// `constraints` is a conjunction — everything the cell is ever bound
/// to must be contained by every member (empty = unconstrained); each
/// bind site checks it where an `Env` exists.
#[derive(Debug)]
pub struct TCell {
    pub(crate) binding: Option<Type>,
    pub(crate) constraints: SmallVec<[Type; 1]>,
    /// What the cell is bounded by from below: everything it is ever
    /// bound to must contain every member, and a cell still open when it
    /// settles binds to their union ([`TVar::add_lower`]).
    pub(crate) lower: SmallVec<[Type; 1]>,
    /// What the checks learned of the cell ([`CellFact`]): every copy, merge
    /// and image carries the whole set.
    pub(crate) facts: BitFlags<CellFact>,
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
}

/// What [`Type::tvar_occurrences`] found: each variable met, once, and
/// how often each cell occurs, a shared node's counted per sight.
#[derive(Default)]
pub(crate) struct Occurrences {
    pub(crate) vars: Vec<TVar>,
    count: AHashMap<usize, u32>,
    /// The cells met, in order, a shared node's sights included.
    trail: Vec<usize>,
    /// Per shared node walked, the cells beneath it.
    nodes: AHashMap<usize, Arc<[usize]>>,
}

impl Occurrences {
    fn met(&mut self, cell: usize) {
        self.trail.push(cell);
        let n = self.count.entry(cell).or_default();
        *n = n.saturating_add(1);
    }

    /// Whether `cell` occurs more than once.
    pub(crate) fn many(&self, cell: usize) -> bool {
        self.count.get(&cell).is_some_and(|n| *n > 1)
    }
}

/// A fact about a cell, beside its binding and constraints.
#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CellFact {
    /// An occurs check refused to bind or link the cell (the only
    /// solution was an infinite type). A flagged cell still open at the
    /// terminal settle must error rather than default to ⊥.
    CycleRefused,
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
            rigid_gates: 0,
            level: current_level(),
            task: TASK.get(),
            ground: 0,
        }
    }
}

/// An open rigid gate, holding the cell it counted on; dropping it
/// closes the gate. A merge may re-point the var to another cell before
/// the gate closes, and a rollback may undo the forward link the merge
/// left, so the close decrements this cell and not whatever the var
/// reads by then.
pub struct RigidGate {
    cell: Arc<RwLock<TCell>>,
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
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                settled_cause(k0, k1, exact) || settled_cause(v0, v1, exact)
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

/// A var's link to its binding cell; aliased vars share one cell.
#[derive(Debug)]
pub struct TVarLink {
    pub(crate) id: TVarId,
    pub(crate) frozen: bool,
    pub(crate) cell: Arc<RwLock<TCell>>,
    /// The compile task that created the var ([`written`]).
    task: TaskId,
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

impl TVar {
    /// Whether the checker made this variable up (an inferred cell), as
    /// opposed to one a program wrote.
    fn generated(&self) -> bool {
        self.name.strip_prefix('_').is_some_and(|n| n.bytes().all(|b| b.is_ascii_digit()))
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
            write!(f, "'{}", ShortName(&self.name))
        } else {
            // A cell can be reachable from its own constraints; a
            // revisit on the print stack elides. Contents are cloned
            // out before recursing (never recurse under the cell guard).
            thread_local! {
                static PRINTING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
            }
            let addr = self.cell_addr();
            if !PRINTING.with_borrow_mut(|s| s.insert(addr)) {
                return write!(f, "'{}: …", ShortName(&self.name));
            }
            let r = (|| {
                let (typ, cons, below) = {
                    let cell = self.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
                };
                // a generated cell's binding, or else what it settles to,
                // is all a reader needs of it
                match (&typ, self.generated(), &below[..]) {
                    (Some(t), true, _) => return write!(f, "{t}"),
                    (None, true, [l]) => return write!(f, "{l}"),
                    (None, true, [l, ls @ ..]) => {
                        write!(f, "[{l}")?;
                        for l in ls {
                            write!(f, ", {l}")?
                        }
                        return write!(f, "]");
                    }
                    _ => write!(f, "'{}: ", ShortName(&self.name))?,
                }
                let Some(t) = typ else {
                    write!(f, "unbound")?;
                    for (i, c) in cons.iter().enumerate() {
                        write!(f, "{}{c}", if i == 0 { " within " } else { " & " })?
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
        Self::from_parts(name, id, false, Arc::new(RwLock::new(TCell::default())))
    }
}

impl Deref for TVar {
    type Target = TVarInner;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

/// Binding equality: two vars are equal when they share a cell or their
/// bindings are equal, so two distinct unbound cells compare equal. Use
/// [`union_identical`] where identity is meant.
impl PartialEq for TVar {
    fn eq(&self, other: &Self) -> bool {
        let (c0, c1) = (self.cell(), other.cell());
        Arc::ptr_eq(&c0, &c1) || c0.read().binding == c1.read().binding
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
        let (c0, c1) = (self.cell(), other.cell());
        if Arc::ptr_eq(&c0, &c1) {
            Ordering::Equal
        } else {
            c0.read().binding.cmp(&c1.read().binding)
        }
    }
}

impl Hash for TVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.cell().read().binding.hash(state)
    }
}

/// Hand a cell's bounds to the open cells of its binding `t`.
pub fn require_bounds(cons: &[Type], t: &Type) {
    handed_bounds(cons, t, &mut |tv, b| tv.add_cell_constraint(b))
}

/// Each conjunct the bounds `cons` hand an open cell of `t`, a type
/// bound where they hold: what [`require_bounds`] adds, and what an
/// interface's typedef parameter declares of the variables its argument
/// holds.
pub(crate) fn handed_bounds(cons: &[Type], t: &Type, hand: &mut dyn FnMut(&TVar, Type)) {
    for c in cons {
        match c {
            Type::Bound(Bound::Concrete)
            | Type::Bound(Bound::Data)
            | Type::Bound(Bound::JsonData)
            | Type::Bound(Bound::TomlData) => {
                let skips = |t: &Type| matches!(t, Type::Fn(_));
                t.require_everywhere(c, &skips, hand, &mut LPooled::take())
            }
            Type::Bound(Bound::Singleton) => t.require_singleton(hand),
            Type::Bound(Bound::OneNumber) => t.require_one_number(hand),
            Type::Bound(Bound::NoError) => t.require_no_error(hand),
            // what a comparison of the type reaches
            Type::Bound(Bound::Discernible) | Type::Bound(Bound::Ordered) => {
                let skips = |t: &Type| matches!(t, Type::Fn(_) | Type::ByRef(..));
                t.require_everywhere(c, &skips, hand, &mut LPooled::take())
            }
            _ => (),
        }
    }
}

/// How a merge treats the var being merged away.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Merge {
    /// Name aliasing: once per var (`frozen` gates it), and the var
    /// takes the survivor's id.
    Name,
    /// Unification of two cells: a rigid cell survives whichever side
    /// it is on.
    Cells,
}

impl TVar {
    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        match &Type::TVar(self.clone()).scope_refs(scope) {
            Type::TVar(tv) => tv.clone(),
            _ => unreachable!(),
        }
    }

    pub fn empty_named(name: ArcStr) -> Self {
        Self::from_parts(
            name,
            TVarId::new(),
            false,
            Arc::new(RwLock::new(TCell::default())),
        )
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
            false,
            Arc::new(RwLock::new(TCell::bound(typ))),
        )
    }

    /// A wrapper over an existing cell, as an image restores it.
    pub(crate) fn from_parts(
        name: ArcStr,
        id: TVarId,
        frozen: bool,
        cell: Arc<RwLock<TCell>>,
    ) -> Self {
        Self(ManuallyDrop::new(Arc::new(TVarInner {
            name,
            link: RwLock::new(TVarLink { id, frozen, cell, task: TASK.get() }),
        })))
    }

    /// The same cell under another name.
    pub fn renamed(&self, name: ArcStr) -> Self {
        let (_, frozen, cell) = self.parts();
        Self::from_parts(name, TVarId::new(), frozen, cell)
    }

    /// The wrapper's id and frozen flag, and its cell.
    pub(crate) fn parts(&self) -> (TVarId, bool, Arc<RwLock<TCell>>) {
        let link = self.read();
        (link.id, link.frozen, link.cell.clone())
    }

    /// Identity of the wrapper: equal for clones of one `TVar`.
    pub(crate) fn wrapper_addr(&self) -> usize {
        Arc::as_ptr(&*self.0) as *const () as usize
    }

    /// The cell this var reads now.
    pub(crate) fn cell(&self) -> Arc<RwLock<TCell>> {
        self.read().cell.clone()
    }

    /// The cell's binding, cloned out (never recurse under the guard).
    pub fn binding(&self) -> Option<Type> {
        self.read().cell.read().binding.clone()
    }

    pub fn is_bound(&self) -> bool {
        self.read().cell.read().binding.is_some()
    }

    // CR claude for claude: [perf/low] A chain of bounds naming the next
    // variable itself ('a{i}: 'a{i+1}) settles in quadratic time: the settle
    // binds 'a0 -> 'a1 -> .. cell to cell, then TVar::freshen (Type::reset_memo
    // in TVar::witness, from FnType::settle_terminal) asks this of every cell,
    // and each call walks the chain to its end, with no path compression.
    // 0.22 / 0.57 / 1.62 / 5.19 G instructions net of the stdlib at 1000 /
    // 2000 / 4000 / 8000 links, about 70 G at 30000; this walk is 960 M of
    // 2.96 G at 4000. Main 968fa9bc0 has it under its cubic part. A walk that
    // stops at a cell it already resolved, or compressing the chain where a
    // forward link may be rewritten, would make it linear. Found by review
    // r14 of lane K. probe: ~/ptmp/cr29/scratch/review-r14/K-r1-1/p/direct_8000.gx
    // (gen.py direct N OUT), profile callgrind/cg_direct4000.out.gz
    // (open-cell-chain-01)
    /// The open cell this var stands for: its own, or the end of a chain
    /// of bindings to vars (a merged cell's forward link); `None` when
    /// the chain ends bound.
    pub fn open_cell(&self) -> Option<TVar> {
        let mut tv = self.clone();
        loop {
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

    /// Bind the open, flexible, unbounded var `slot` holds to `t`, which
    /// is not a var, without an occurs walk of `t`'s whole depth, when
    /// `t` cannot reach the var's cell: nothing but `slot` holds the var
    /// and nothing but the var holds the cell, and either nothing but its
    /// owner holds `slot` (a type reaches a cell only through a clone of
    /// one of the three) or `t` reaches no open cell at all. False,
    /// binding nothing, when any of that fails.
    pub fn bind_unshared(slot: &Arc<Type>, t: &Type) -> bool {
        let Type::TVar(tv) = &**slot else { return false };
        if Arc::count(&tv.0) != 1
            || Arc::count(&tv.read().cell) != 1
            || matches!(t, Type::TVar(_))
            || tv.is_bound()
            || tv.is_rigid()
            || !tv.cell_constraints().is_empty()
            || !tv.cell_lower().is_empty()
        {
            return false;
        }
        let epoch = ground_epoch();
        let shared = Arc::count(slot) > 1;
        let mut walked: LPooled<Vec<CellRef>> = LPooled::take();
        if shared && !reaches_no_open_cell(t, epoch, &mut LPooled::take(), &mut walked) {
            return false;
        }
        tv.bind(t.clone());
        if shared {
            walked.push(tv.cell());
            mark_ground(&mut walked, epoch);
        }
        true
    }

    /// `e`, the refusal of the var `slot` holds against `t`, as the
    /// infinite type when `t` reaches the var's cell under a constructor.
    pub fn refusal_over(slot: &Type, t: &Type, e: Error) -> Error {
        let Type::TVar(tv) = slot else { return e };
        match cycle_toward(tv.cell_addr(), t, Toward::Any) {
            Some(Cycle::Constructor) => Error::new(super::contains::InfiniteType),
            Some(Cycle::Bare) | None => e,
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
        require_bounds(&self.cell_constraints(), &t);
        lower(&t, self.level());
        let cell = self.cell();
        let mut c = cell.write();
        if decided(&c) {
            if reaching && c.binding.is_some() {
                bound_cell_changes();
            }
            c.binding = Some(t)
        }
    }

    /// Add a conjunct to this var's cell constraints (deduped).
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
        let below = {
            let mut c = cell.write();
            if !decided(&c) {
                return;
            }
            if c.binding.is_some() {
                bound_cell_changes();
            }
            c.constraints.extend(new.iter().cloned());
            c.lower.clone()
        };
        // what bounds the cell from below holds its predicates, whichever
        // came first ([`Self::add_lower`])
        for t in below.iter() {
            require_bounds(&new, t)
        }
    }

    /// Narrow the cell by `c` unless a conjunct already is at least
    /// that narrow (a probe in `env`: `c` contains it).
    #[doc(hidden)]
    pub fn narrow_cell(&self, env: &Env, c: Type) -> Result<()> {
        for e in self.cell_constraints().iter() {
            if c.contains_with_flags(BitFlags::empty(), env, e)? {
                return Ok(());
            }
        }
        self.add_cell_constraint(c);
        Ok(())
    }

    /// The cell's constraint conjunction (cloned out).
    pub fn cell_constraints(&self) -> SmallVec<[Type; 1]> {
        self.read().cell.read().constraints.clone()
    }

    /// The cell's lower bounds (cloned out).
    pub fn cell_lower(&self) -> SmallVec<[Type; 1]> {
        self.read().cell.read().lower.clone()
    }

    /// Bound the open cell from below by `t` (deduped): whatever it binds
    /// to must contain `t`. The caller has checked the cell's conjuncts
    /// admit `t`; they are handed to `t`'s open cells as a binding's are.
    pub(crate) fn add_lower(&self, t: Type) {
        let cell = self.cell();
        if cell.read().lower.iter().any(|e| union_identical(e, &t)) {
            return;
        }
        require_bounds(&self.cell_constraints(), &t);
        lower(&t, cell.read().level);
        let mut c = cell.write();
        if decided(&c) {
            if c.binding.is_some() {
                bound_cell_changes();
            }
            c.lower.push(t)
        }
    }

    /// Drop each bound of the open cell that is a cell of `on`, each such
    /// member of a union lower bound, and each conjunct union holding one:
    /// merged with those cells, the cell is each of them, so `'r ⊇ [T,
    /// 'r]` holds by `T` and `'r ⊆ [T, 'r]` always.
    pub(super) fn drop_bounds_on(&self, on: &AddrSet) {
        let is_on = |t: &Type| match t {
            Type::TVar(v) => {
                on.contains(&v.cell_addr())
                    || v.open_cell().is_some_and(|o| on.contains(&o.cell_addr()))
            }
            _ => false,
        };
        let holds_on = |t: &Type| matches!(t, Type::Set(ms) if ms.iter().any(is_on));
        let cell = self.cell();
        let (lower, cons) = {
            let c = cell.read();
            (c.lower.clone(), c.constraints.clone())
        };
        if !lower.iter().chain(cons.iter()).any(|t| is_on(t) || holds_on(t)) {
            return;
        }
        let lower = lower
            .into_iter()
            .filter_map(|l| match &l {
                l if is_on(l) => None,
                Type::Set(ms) if ms.iter().any(is_on) => {
                    let mut rest: SmallVec<[Type; 4]> =
                        ms.iter().filter(|m| !is_on(m)).cloned().collect();
                    match rest.len() {
                        0 | 1 => rest.pop(),
                        _ => Some(Type::Set(Arc::from_iter(rest.drain(..)))),
                    }
                }
                _ => Some(l),
            })
            .collect();
        let cons = cons.into_iter().filter(|c| !is_on(c) && !holds_on(c)).collect();
        let mut c = cell.write();
        if decided(&c) {
            c.lower = lower;
            c.constraints = cons;
        }
    }

    pub fn read<'a>(&'a self) -> RwLockReadGuard<'a, TVarLink> {
        self.link.read()
    }

    pub fn write<'a>(&'a self) -> RwLockWriteGuard<'a, TVarLink> {
        self.link.write()
    }

    /// Make self an alias for other; self's constraints merge into the
    /// shared cell. A var of an earlier compile task keeps its name: the
    /// other takes it, or, named already, the two merge as cells.
    pub fn alias(&self, other: &Self) {
        self.merge_into(other, Merge::Name)
    }

    /// Merge self's cell into other's for a unification-driven merge:
    /// bypasses the `frozen` gate [`Self::alias`] honors (frozen means
    /// name-aliasing already happened) but keeps its occurs checks.
    /// A rigid cell is the survivor whichever side it is on: its gate
    /// counts on that cell, and a var re-pointed away from it would
    /// read as free for the rest of the def's check and take a binding.
    /// Otherwise the older compile task's cell survives, and two cells of
    /// earlier tasks stay apart ([`written`]).
    pub(super) fn alias_cells(&self, other: &Self) {
        self.merge_into(other, Merge::Cells)
    }

    fn merge_into(&self, other: &Self, how: Merge) {
        let mut absorbed: SmallVec<[(TVar, TVar); 2]> = SmallVec::new();
        self.merge_one(other, how, &mut absorbed);
        while let Some((o, into)) = absorbed.pop() {
            o.merge_one(&into, Merge::Cells, &mut absorbed);
            if !o.same_cell(&into) {
                let cell = into.cell();
                let mut c = cell.write();
                if decided(&c) {
                    c.lower.push(Type::TVar(o))
                }
            }
        }
    }

    /// One merge of [`Self::merge_into`]; each open cell it would move below
    /// a rigid survivor goes on top of `absorbed`, the first on top, with
    /// that survivor, to merge into it next.
    fn merge_one(
        &self,
        other: &Self,
        how: Merge,
        absorbed: &mut SmallVec<[(TVar, TVar); 2]>,
    ) {
        if how == Merge::Cells
            && !other.is_rigid()
            && (self.is_rigid() || self.cell().read().task < other.cell().read().task)
        {
            return other.merge_one(self, how, absorbed);
        }
        let (s_cell, o_cell) = (self.cell(), other.cell());
        let earlier = (earlier_task(&s_cell.read()), earlier_task(&o_cell.read()));
        match earlier {
            // two cells earlier tasks made stay apart; the unification this
            // check needed of them is a decision it may not make
            (true, true) => {
                if !Arc::ptr_eq(&s_cell, &o_cell) {
                    written_of(
                        s_cell.read().task,
                        "cell",
                        Some(s_cell.data_ptr().addr()),
                    );
                }
                return;
            }
            (true, false) if how == Merge::Name => {
                let frozen = other.read().frozen;
                return match frozen {
                    false => other.merge_one(self, Merge::Name, absorbed),
                    true => self.merge_one(other, Merge::Cells, absorbed),
                };
            }
            _ => (),
        }
        let same = Arc::ptr_eq(&s_cell, &o_cell);
        if how == Merge::Name && self.read().frozen {
            return;
        }
        if !same {
            // Occurs check: a merged cell reachable from its own contents
            // is an infinite type every later walk loops on. Skipping the
            // merge only keeps inference looser.
            let (s_addr, o_addr) = (s_cell.data_ptr().addr(), o_cell.data_ptr().addr());
            let scons = s_cell.read().constraints.clone();
            if would_cycle_inner(s_addr, &Type::TVar(other.clone()))
                || scons.iter().any(|c| would_cycle_inner(o_addr, c))
            {
                if graphix_dbg_bind() {
                    eprintln!(
                        "ALIAS-REFUSE {}({s_addr:x}) -> {}({o_addr:x}): the merge closes a cycle",
                        self.name, other.name
                    );
                }
                self.mark_cycle_refused();
                other.mark_cycle_refused();
                return;
            }
            // A bound cell's binding would be lost for this var.
            if s_cell.read().binding.is_some() {
                if graphix_dbg_bind() {
                    eprintln!("ALIAS-REFUSE {}({s_addr:x}): bound", self.name);
                }
                return;
            }
        }
        // Dedup computed lock-free: the identity walk can re-enter these cells.
        let (mut to_add, facts) = if same {
            (LPooled::take(), BitFlags::empty())
        } else {
            let mine = s_cell.read().constraints.clone();
            let theirs = o_cell.read().constraints.clone();
            let conjuncts = new_conjuncts(&theirs, mine);
            let s = s_cell.read();
            (conjuncts, s.facts)
        };
        let mut below = match same {
            true => LPooled::take(),
            false => new_conjuncts(&o_cell.read().lower, s_cell.read().lower.clone()),
        };
        // a rigid cell holds only itself: an open cell below it is it
        let mut absorb: SmallVec<[TVar; 2]> = SmallVec::new();
        if o_cell.read().rigid_gates > 0 {
            below.retain(|l| match l {
                Type::TVar(v) => match v.open_cell() {
                    Some(o)
                        if !o.is_rigid()
                            && !Arc::ptr_eq(&o.cell(), &s_cell)
                            && !Arc::ptr_eq(&o.cell(), &o_cell) =>
                    {
                        absorb.push(o);
                        false
                    }
                    _ => true,
                },
                _ => true,
            });
        }
        let oid = other.read().id;
        if !same {
            let s_task = s_cell.read().task;
            let o_task = o_cell.read().task;
            let var_task = self.read().task;
            if !decided_of(s_task, "cell", Some(s_cell.data_ptr().addr()))
                || (!to_add.is_empty()
                    && !decided_of(o_task, "cell", Some(o_cell.data_ptr().addr())))
                || !decided_of(var_task, "var", None)
            {
                return;
            }
        }
        let mut s = self.write();
        if same && how == Merge::Name && (!s.frozen || s.id != oid) {
            written_of(s.task, "var", None);
        }
        if how == Merge::Name {
            s.frozen = true;
            s.id = oid;
        }
        if same {
            return;
        }
        if graphix_dbg_bind() && how == Merge::Cells {
            eprintln!(
                "CELL-MERGE '{}({:x}) <=> '{}({:x})",
                self.name,
                s_cell.data_ptr().addr(),
                other.name,
                o_cell.data_ptr().addr()
            );
        }
        let level = s_cell.read().level;
        let moved: LPooled<Vec<Type>> =
            to_add.iter().chain(below.iter()).cloned().collect();
        {
            let mut oc = o_cell.write();
            if oc.binding.is_some() && !(to_add.is_empty() && below.is_empty()) {
                bound_cell_changes();
            }
            oc.constraints.extend(to_add.drain(..));
            oc.lower.extend(below.drain(..));
            if !earlier_task(&oc) {
                oc.facts |= facts;
            } else if !oc.facts.contains(facts) {
                written_of(oc.task, "cell", Some(cell_addr(&oc)));
            }
        }
        // Forward-link the abandoned cell: other TVars may share it and
        // must follow the merge. The occurs check above guarantees the
        // link closes no cycle.
        {
            let mut sc = s_cell.write();
            sc.binding = Some(Type::TVar(other.clone()));
        }
        s.cell = o_cell;
        drop(s);
        lower(&Type::TVar(other.clone()), level);
        let at = other.level();
        for t in moved.iter() {
            lower(t, at)
        }
        absorbed.extend(absorb.into_iter().rev().map(|o| (o, other.clone())));
    }

    /// [`Self::alias`] by an enclosing scope's same-named var: one a nested
    /// fn type named alike already (its own quantifiers never reach here)
    /// is the enclosing scope's, and takes its name again.
    fn alias_enclosing(&self, other: &Self) {
        let (id, cell) = {
            let o = other.read();
            (o.id, o.cell.clone())
        };
        let renamed = {
            let mut l = self.write();
            let renamed = l.frozen && l.id != id && decided_of(l.task, "var", None);
            if renamed {
                l.frozen = false;
            }
            renamed
        };
        // a sibling of the nested group merged first: this var's cell
        // already forwards to the enclosing one
        if renamed
            && let Some(end) = self.open_cell()
            && Arc::ptr_eq(&end.cell(), &cell)
        {
            let mut l = self.write();
            l.cell = cell;
            l.id = id;
            l.frozen = true;
            return;
        }
        self.alias(other)
    }

    pub fn freeze(&self) {
        let mut l = self.write();
        if !l.frozen && !decided_of(l.task, "var", None) {
            return;
        }
        l.frozen = true;
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
    pub(crate) fn requires_data(&self) -> bool {
        self.cell()
            .read()
            .constraints
            .iter()
            .any(|c| matches!(c, Type::Bound(Bound::Data)))
    }

    /// Whether the cell holds the `Function` conjunct.
    pub(crate) fn requires_function(&self) -> bool {
        self.cell()
            .read()
            .constraints
            .iter()
            .any(|c| matches!(c, Type::Bound(Bound::Function)))
    }

    /// Bind self to `binding` (other's, read by the caller), merging
    /// other's constraints into self's cell.
    pub(super) fn copy(&self, other: &Self, binding: Type) {
        let s_cell = self.cell();
        // Occurs check as in [`Self::alias`].
        if would_cycle_inner(s_cell.data_ptr().addr(), &Type::TVar(other.clone())) {
            self.mark_cycle_refused();
            return;
        }
        let o_cell = other.cell();
        if Arc::ptr_eq(&s_cell, &o_cell) {
            return;
        }
        let ocons = o_cell.read().constraints.clone();
        if graphix_dbg_bind() {
            eprintln!(
                "COPY '{}({:x}) <= '{}: {binding:?}",
                self.name,
                s_cell.data_ptr().addr(),
                other.name
            );
        }
        if let Some(id) = crate::dbgenv::graphix_dbg_bind_bt_id()
            && id == self.read().id.inner().to_string()
        {
            eprintln!(
                "BT for write to '{}({}):\n{}",
                self.name,
                id,
                std::backtrace::Backtrace::force_capture()
            );
        }
        let existing = s_cell.read().constraints.clone();
        let mut to_add = new_conjuncts(&existing, ocons);
        let level = s_cell.read().level;
        lower(&binding, level);
        for c in to_add.iter() {
            lower(c, level)
        }
        let cons = {
            let mut sc = s_cell.write();
            if !decided(&sc) {
                return;
            }
            if sc.binding.is_some() {
                bound_cell_changes();
            }
            sc.binding = Some(binding.clone());
            sc.constraints.extend(to_add.drain(..));
            sc.constraints.clone()
        };
        require_bounds(&cons, &binding);
    }

    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut super::normalize::NormCx::take())
    }

    pub(super) fn normalize_int(&self, cx: &mut super::normalize::NormCx) -> Self {
        // First visit only. Clone the binding out, normalize unlocked,
        // write back: the lock is non-reentrant.
        // A cell an earlier task owns keeps its binding as it stands: the
        // normal form means the same, and writing it would be a decision.
        // The normal form reaches only cells its source reached.
        if cx.cells.insert(self.cell_addr())
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
        c.binding = None;
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
        self.read().cell.read().rigid_gates > 0
    }

    /// Bind the open cell to ⊥ as a requirement; see
    /// [`CellFact::BottomRequired`].
    pub(super) fn bind_bottom_required(&self) {
        self.bind(Type::Bottom);
        let cell = self.cell();
        let mut c = cell.write();
        if decided(&c) {
            c.facts |= CellFact::BottomRequired;
        }
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

    /// Record a ⊥ produced into the open cell; see [`CellFact::BottomFed`].
    pub(super) fn mark_bottom_fed(&self) {
        if graphix_dbg_bind() {
            eprintln!("BOTTOM-FED '{}({:x})", self.name, self.cell_addr());
        }
        let cell = self.cell();
        let mut c = cell.write();
        if !c.facts.contains(CellFact::BottomFed) && !decided(&c) {
            return;
        }
        c.facts |= CellFact::BottomFed;
    }

    /// Record an occurs-check refusal; see [`CellFact::CycleRefused`].
    pub(super) fn mark_cycle_refused(&self) {
        if graphix_dbg_bind() {
            eprintln!("CYCLE-REFUSED '{}({:x})", self.name, self.cell_addr());
        }
        if crate::dbgenv::graphix_dbg_cycle_bt() {
            eprintln!(
                "CYCLE-REFUSED '{}({:x})\n{}",
                self.name,
                self.cell_addr(),
                std::backtrace::Backtrace::force_capture()
            );
        }
        let cell = self.cell();
        let mut c = cell.write();
        if !c.facts.contains(CellFact::CycleRefused) && !decided(&c) {
            return;
        }
        c.facts |= CellFact::CycleRefused;
    }

    pub(super) fn would_cycle(&self, t: &Type) -> bool {
        would_cycle_inner(self.cell_addr(), t)
    }

    /// True iff both vars share one binding cell (aliases of each other).
    pub fn same_cell(&self, other: &Self) -> bool {
        self.cell_addr() == other.cell_addr()
    }

    /// The cell this var stands for: its own, or the end of the forward
    /// links merges left ([`Self::open_cell`], bound or not).
    pub fn stands_for(&self) -> usize {
        let mut tv = self.clone();
        loop {
            let next = match &tv.cell().read().binding {
                Some(Type::TVar(next)) => next.clone(),
                _ => return tv.cell_addr(),
            };
            tv = next;
        }
    }

    /// Identity of the shared binding cell, for set membership tests.
    #[doc(hidden)]
    pub fn cell_addr(&self) -> usize {
        self.read().cell.data_ptr().addr()
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
    fn freshen<M>(
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
        if matches!(self.read().cell.read().binding, Some(Type::TVar(_)))
            && let Some(end) = self.open_cell()
            && let Some(f) = fresh.get(&end.cell_addr())
        {
            return f.named_as(self);
        }
        let f = match how {
            Fresh::Copy => self.fresh_copy(),
            Fresh::Instantiate(open) if self.level().copied_by(open) => {
                TVar::empty_named(self.name.clone())
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
    /// lower bounds and binding rebuilt through `walk`, the facts kept.
    fn copy_into<M>(
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
        let facts = self.cell().read().facts;
        f.cell().write().facts |= facts;
        if let Some(t) = self.binding() {
            f.bind(walk(&t, fresh, memo).unwrap_or(t));
        }
    }
}

/// A by-name substitution: the map, and every name its types hold, read
/// once, when a quantified function type first asks.
pub(super) struct Subst<'a> {
    known: &'a AHashMap<ArcStr, Type>,
    held: Option<LPooled<AHashSet<ArcStr>>>,
}

impl<'a> Subst<'a> {
    pub(super) fn new(known: &'a AHashMap<ArcStr, Type>) -> Self {
        Self { known, held: None }
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
        let name = (1u64..)
            .map(|i| ArcStr::from(format_compact!("{q}{i}").as_str()))
            .find(|n| !avoid.contains(n))
            .unwrap();
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
    /// reads them.
    pub(crate) fn one_number_holds(&self, env: &Env, commit: bool) -> Result<bool> {
        let mut parts = UnionParts::new(Measure::Numbers);
        self.union_parts(Some((env, commit)), &mut parts)?;
        Ok(parts.known <= 1)
    }

    /// Whether a value of `self` may be an error as the type stands: a
    /// member is an `Error`, holds the primitive `error` or is `Any`.
    /// `NoError ⊇ self` is its negation; an open member may be anything
    /// its own conjuncts admit and is not counted.
    pub(crate) fn may_be_error(&self, env: &Env, commit: bool) -> Result<bool> {
        self.may_be_error_past(env, commit, &mut LPooled::take())
    }

    /// [`Self::may_be_error`], each name expanded once a walk: one met
    /// again was found to hold no error, or the walk would have ended.
    fn may_be_error_past(
        &self,
        env: &Env,
        commit: bool,
        named: &mut LPooled<Vec<RefKey>>,
    ) -> Result<bool> {
        ensure_sufficient(|| match self {
            Type::Error(_) | Type::Any => Ok(true),
            Type::Primitive(p) => Ok(p.contains(netidx_value::Typ::Error)),
            Type::Set(ts) => {
                for t in ts.iter() {
                    if t.may_be_error_past(env, commit, named)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::TVar(tv) => match tv.binding() {
                None => Ok(false),
                Some(b) => b.may_be_error_past(env, commit, named),
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
                    Some(t) => t.may_be_error_past(env, commit, named),
                    None => Ok(true),
                }
            }
            _ => Ok(false),
        })
    }

    /// `self` without the members a value of which may be an error, its
    /// type references expanded; `None` when nothing is left or `Any`
    /// is a member.
    pub(crate) fn without_errors(&self, env: &Env) -> Result<Option<Type>> {
        fn go(t: &Type, env: &Env, out: &mut LPooled<Vec<Type>>) -> Result<bool> {
            ensure_sufficient(|| match t {
                Type::Any => Ok(false),
                Type::Error(_) | Type::Bottom => Ok(true),
                Type::Primitive(p) => {
                    let mut p = *p;
                    p.remove(netidx_value::Typ::Error);
                    if !p.is_empty() {
                        out.push(Type::Primitive(p))
                    }
                    Ok(true)
                }
                Type::Set(ts) => {
                    for t in ts.iter() {
                        if !go(t, env, out)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => go(&b, env, out),
                    None => Ok({
                        out.push(t.clone());
                        true
                    }),
                },
                Type::Ref(_) if t.may_be_error(env, false)? => {
                    go(&t.lookup_ref(env)?, env, out)
                }
                t => Ok({
                    out.push(t.clone());
                    true
                }),
            })
        }
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        if !go(self, env, &mut out)? || out.is_empty() {
            return Ok(None);
        }
        Ok(Some(match out.len() {
            1 => out.pop().unwrap(),
            _ => Type::Set(Arc::from_iter(out.drain(..))).normalize(),
        }))
    }

    /// The open members of a `NoError` cell's binding are each `NoError`.
    fn require_no_error(&self, hand: &mut dyn FnMut(&TVar, Type)) {
        let mut parts = UnionParts::new(Measure::Members);
        if self.union_parts(None, &mut parts).is_ok() {
            for tv in parts.open.iter() {
                hand(tv, Type::Bound(Bound::NoError))
            }
        }
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

    /// Narrow the open members of a `Singleton` cell's binding: beside one
    /// known member each must be within it; with none, each is a
    /// `Singleton` itself (two open members may still differ).
    fn require_singleton(&self, hand: &mut dyn FnMut(&TVar, Type)) {
        let mut parts = UnionParts::new(Measure::Members);
        if self.union_parts(None, &mut parts).is_err() {
            return;
        }
        parts.merge_open();
        if let Some(narrow) = parts.narrowing() {
            for tv in parts.open.iter() {
                hand(tv, narrow.clone())
            }
        }
    }

    /// The open members of a `OneNumber` cell's binding are each
    /// `OneNumber` where no member is numeric yet; beside a numeric
    /// member, one may still bind another numeric type (no upper bound
    /// says "not numeric, or this number").
    fn require_one_number(&self, hand: &mut dyn FnMut(&TVar, Type)) {
        let mut parts = UnionParts::new(Measure::Numbers);
        if self.union_parts(None, &mut parts).is_err() || parts.known > 0 {
            return;
        }
        for tv in parts.open.iter() {
            hand(tv, Type::Bound(Bound::OneNumber))
        }
    }

    /// An open member of a `OneNumber` cell's binding beside a numeric one,
    /// or two open members beside none: one may yet be another numeric type
    /// than the rest, which no bound on either can rule out, so the
    /// definition that leaves them open is refused (annotate it).
    pub fn one_number_open(&self) -> bool {
        let mut parts = UnionParts::new(Measure::Numbers);
        if self.union_parts(None, &mut parts).is_err() {
            return false;
        }
        match (parts.known, parts.open.split_first()) {
            (_, None) => false,
            (0, Some((first, rest))) => {
                rest.iter().any(|tv| tv.cell_addr() != first.cell_addr())
            }
            (_, Some(_)) => true,
        }
    }

    /// [`Self::require_singleton`] for an operand whose type must be one
    /// type: an open cell's existing conjuncts may already say so.
    pub fn narrow_singleton(&self, env: &Env) -> Result<()> {
        let mut parts = UnionParts::new(Measure::Members);
        self.union_parts(Some((env, false)), &mut parts)?;
        match parts.rigid_apart() {
            None => (),
            Some(Apart::TwoRigid) => bail!(
                "{} must be one type, but its type variables may differ",
                self.resolve_tvars()
            ),
            Some(Apart::BesideKnown(tv)) => bail!(
                "{} must be one type, but the declared variable '{} may differ from \
                 its other members",
                self.resolve_tvars(),
                ShortName(&tv.name)
            ),
        }
        parts.merge_open();
        match parts.narrowing() {
            None => Ok(()),
            Some(narrow) => parts
                .open
                .drain(..)
                .try_for_each(|tv| tv.narrow_cell(env, narrow.clone())),
        }
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
                    row if would_cycle_inner(tv.cell_addr(), row) => false,
                    row => {
                        tv.bind(row.clone());
                        true
                    }
                },
            },
            (t, Type::TVar(r)) => match r.binding() {
                Some(b) => t.take_row_at(&b, seen),
                None => !t.has_unbound(),
            },
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
            (Type::Map { key: ka, value: va }, Type::Map { key: kb, value: vb }) => {
                ka.take_row_at(kb, seen) & va.take_row_at(vb, seen)
            }
            (
                Type::Abstract { id: ia, params: a },
                Type::Abstract { id: ib, params: b },
            ) if ia == ib => each(a, b, seen),
            (Type::Ref(a), Type::Ref(b)) if a.name == b.name => {
                each(&a.params, &b.params, seen)
            }
            (t, _) => !t.has_unbound(),
        }
    }

    /// Whether `self` is concrete now: no open cell, and `Concrete ⊇ self`
    /// holds. The type a run-time read may cast to.
    pub fn is_concrete(&self, env: &Env) -> bool {
        !self.has_unbound() && self.concrete_holds(env)
    }

    /// Hand `bound` to every open cell the type reaches, never under a
    /// part `skips` names; each cell and shared node once.
    fn require_everywhere(
        &self,
        bound: &Type,
        skips: &dyn Fn(&Type) -> bool,
        hand: &mut dyn FnMut(&TVar, Type),
        seen: &mut AHashSet<usize>,
    ) {
        ensure_sufficient(|| match self {
            t if skips(t) => (),
            Type::TVar(tv) if !seen.insert(tv.cell_addr()) => (),
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.require_everywhere(bound, skips, hand, seen),
                None => hand(tv, bound.clone()),
            },
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            t => {
                t.for_each_child(&mut |c| c.require_everywhere(bound, skips, hand, seen))
            }
        })
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

    /// Every type variable the structure holds, bindings not entered,
    /// with how often each cell occurs; a shared node is walked once.
    pub(crate) fn tvar_occurrences(&self, out: &mut Occurrences) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                out.vars.push(tv.clone());
                out.met(tv.cell_addr());
            }
            t => {
                if let Some(n) = super::node_addr(t) {
                    if let Some(cells) = out.nodes.get(&n).cloned() {
                        for c in cells.iter() {
                            out.met(*c)
                        }
                        return;
                    }
                    let start = out.trail.len();
                    match t {
                        Type::Fn(ft) => {
                            ft.for_each_part(&mut |t, _| t.tvar_occurrences(out))
                        }
                        t => t.for_each_child(&mut |c| c.tvar_occurrences(out)),
                    }
                    let cells: Arc<[usize]> =
                        Arc::from_iter(out.trail[start..].iter().copied());
                    out.nodes.insert(n, cells);
                    return;
                }
                t.for_each_child(&mut |c| c.tvar_occurrences(out))
            }
        })
    }

    /// Alias type variables with the same name to each other.
    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(e) => {
                    let v = e.get();
                    v.freeze();
                    tv.alias_enclosing(v);
                }
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                }
            },
            // a nested fn type's own quantifiers are its own, whatever an
            // enclosing signature names alike
            Type::Fn(ft) if !ft.quantifiers.is_empty() => {
                let outer: SmallVec<[(ArcStr, Option<TVar>); 4]> =
                    ft.quantifiers.iter().map(|q| (q.clone(), known.remove(q))).collect();
                ft.alias_tvars(known);
                for (q, tv) in outer {
                    known.remove(&q);
                    if let Some(tv) = tv {
                        known.insert(q, tv);
                    }
                }
            }
            Type::Fn(ft) => ft.alias_tvars(known),
            t => t.for_each_child(&mut |c| c.alias_tvars(known)),
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

    /// Whether the cell at `addr` ([`ForeignWrite`]) is one of this type's,
    /// or one a cell it holds is bound to reaches.
    fn holds_cell(&self, addr: usize) -> bool {
        fn go(t: &Type, addr: usize, seen: &mut AHashSet<usize>) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    let cell = tv.cell_addr();
                    cell == addr
                        || (seen.insert(cell)
                            && tv.binding().is_some_and(|b| go(&b, addr, seen)))
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
        self.collect_tvars_seen(known, &mut LPooled::take())
    }

    /// [`Self::collect_tvars`], each shared node (`seen`) walked once.
    /// `seen` holds each node, so a normalized conjunct dropped mid-walk
    /// cannot lend its address to a node never walked.
    pub(super) fn collect_tvars_seen(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut AHashMap<usize, Type>,
    ) {
        if let Some(n) = node_addr(self)
            && seen.insert(n, self.clone()).is_some()
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
                let mut inner: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                ft.collect_tvars(&mut inner);
                for (name, tv) in inner.drain() {
                    if !ft.quantifiers.contains(&name) {
                        known.entry(name).or_insert(tv);
                    }
                }
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

    /// [`Self::collect_tvars`] over positions alone: a nested signature's
    /// cells by its own positions, never its conjuncts, each shared node
    /// (`seen`) walked once, so the walk is linear in the type.
    pub(crate) fn collect_position_tvars(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut AHashSet<usize>,
    ) {
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

    /// An open cell held rigid anywhere beneath, through bindings.
    pub fn has_rigid(&self) -> bool {
        self.open_beneath(&|tv| tv.is_rigid())
    }

    /// An open cell `counts` anywhere beneath, through bindings.
    fn open_beneath(&self, counts: &dyn Fn(&TVar) -> bool) -> bool {
        fn go(
            t: &Type,
            counts: &dyn Fn(&TVar) -> bool,
            seen: &mut AHashSet<usize>,
        ) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    seen.insert(tv.cell_addr())
                        && match tv.binding() {
                            Some(b) => go(&b, counts, seen),
                            None => counts(tv),
                        }
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
                t => t
                    .try_for_each_child(&mut |c| {
                        if go(c, counts, seen) {
                            ControlFlow::Break(())
                        } else {
                            ControlFlow::Continue(())
                        }
                    })
                    .is_break(),
            })
        }
        go(self, counts, &mut LPooled::take())
    }

    /// `self ⊇ pred` for a pattern's predicate over what reaches it: over
    /// a union, by member ([`Self::narrow_per_member`]). Whether some
    /// member, or `self`, contains it.
    pub fn narrow_pattern(&self, env: &Env, pred: &Type) -> Result<bool> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        crate::expr::union_members(env, self, &mut members)?;
        members.retain(|m| pred.could_match(env, m).unwrap_or(true));
        match members.len() {
            1 if members[0].needs_split(env, pred)? => {
                members[0].narrow_member(env, pred)
            }
            0 | 1 => self.narrow_member(env, pred),
            _ => pred.narrow_per_member(env, &members).map(|()| true),
        }
    }

    /// `self ⊇ pred` for one member of what reaches a pattern: by
    /// [`Self::narrow_parts`] where a union stands under a constructor
    /// `pred` has, else by containment; then a cell of `pred` left open
    /// over an `Any` part is `Any` ([`Self::open_over_any`]).
    fn narrow_member(&self, env: &Env, pred: &Type) -> Result<bool> {
        let narrowed = if self.needs_split(env, pred)? {
            self.narrow_parts(env, pred)?
        } else {
            self.contains(env, pred)?
        };
        if narrowed && pred.has_unbound() {
            self.open_over_any(env, pred)?
        }
        Ok(narrowed)
    }

    /// Bind each cell of `pred` still open where `self`'s part is `Any`
    /// to `Any`: `Any ⊇ 'a` binds nothing, but a pattern bind receives
    /// the scrutinee's value, whatever the arm body does with it.
    fn open_over_any(&self, env: &Env, pred: &Type) -> Result<()> {
        ensure_sufficient(|| {
            if let Type::TVar(tv) = pred {
                return match tv.binding() {
                    Some(b) => self.open_over_any(env, &b),
                    None if self.with_deref(|t| matches!(t, Some(Type::Any))) => {
                        pred.contains(env, &Type::Any).map(|_| ())
                    }
                    None => Ok(()),
                };
            }
            let Some(p) = pred.deref_cloned() else { return Ok(()) };
            if let Type::Set(qs) = &p {
                return qs.iter().try_for_each(|q| self.open_over_any(env, q));
            }
            let Some(t) = self.shape_under(env, &p)? else { return Ok(()) };
            let each = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
                for (t, p) in parts {
                    t.open_over_any(env, p)?
                }
                Ok(())
            };
            match (&t, &p) {
                (Type::Set(_), p) => {
                    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                    crate::expr::union_members(env, &t, &mut members)?;
                    for m in members.iter() {
                        if p.could_match(env, m)? {
                            m.open_over_any(env, p)?
                        }
                    }
                    Ok(())
                }
                (Type::Tuple(ts), Type::Tuple(ps))
                | (Type::Variant(_, ts, _), Type::Variant(_, ps, _)) => {
                    each(&mut ts.iter().zip(ps.iter()))
                }
                (Type::Struct(fs), Type::Struct(pfs)) => each(
                    &mut fs.iter().zip(pfs.iter()).map(|((_, t, _), (_, p, _))| (t, p)),
                ),
                (Type::Array(e), Type::Array(pe)) | (Type::List(e), Type::List(pe)) => {
                    e.open_over_any(env, pe)
                }
                _ => Ok(()),
            }
        })
    }

    /// Whether a union of `self` stands where `pred` has a constructor
    /// (a tuple position, a struct field, a variant payload, an array or
    /// list element), which containment would commit to its first member
    /// that holds the constructor.
    fn needs_split(&self, env: &Env, pred: &Type) -> Result<bool> {
        self.needs_split_int(env, pred, &mut NodePairs::default())
    }

    /// [`Self::needs_split`], each pair of shared nodes that needs none
    /// (`apart`) walked once. Exact: the walk assumes no pair in progress,
    /// so a pair's false holds wherever it is met, and a true ends it.
    fn needs_split_int(
        &self,
        env: &Env,
        pred: &Type,
        apart: &mut NodePairs,
    ) -> Result<bool> {
        ensure_sufficient(|| {
            let key = apart.key(self, pred);
            if key.is_some_and(|k| apart.contains(k)) {
                return Ok(false);
            }
            let split = self.needs_split_inner(env, pred, apart)?;
            if let Some(k) = key.filter(|_| !split) {
                apart.insert(k, self, pred)
            }
            Ok(split)
        })
    }

    fn needs_split_inner(
        &self,
        env: &Env,
        pred: &Type,
        apart: &mut NodePairs,
    ) -> Result<bool> {
        #[cfg(test)]
        SPLIT_STEPS.with(|n| n.set(n.get() + 1));
        let Some(p) = pred.deref_cloned() else { return Ok(false) };
        if let Type::Set(qs) = &p {
            for q in qs.iter() {
                if self.needs_split_int(env, q, apart)? {
                    return Ok(true);
                }
            }
            return Ok(false);
        }
        let Some(t) = self.shape_under(env, &p)? else { return Ok(false) };
        let mut any = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
            for (t, p) in parts {
                if t.needs_split_int(env, p, apart)? {
                    return Ok(true);
                }
            }
            Ok(false)
        };
        match (&t, &p) {
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
                let union = match e.deref_cloned() {
                    Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(env)?, Type::Set(_)),
                    Some(t) => matches!(t, Type::Set(_)),
                    None => false,
                };
                match pe.deref_cloned() {
                    Some(Type::Set(_)) if union => Ok(true),
                    _ => e.needs_split_int(env, pe, apart),
                }
            }
            _ => Ok(false),
        }
    }

    /// `self` looked through (bindings, a typedef) where `pred` is the
    /// same constructor with the same parts (arity, tag, field names), or a
    /// union where `pred` is a constructor; `None` anywhere else.
    fn shape_under(&self, env: &Env, p: &Type) -> Result<Option<Type>> {
        let t = match self.deref_cloned() {
            Some(t @ Type::Ref(_)) => t.lookup_ref(env)?,
            Some(t) => t,
            None => return Ok(None),
        };
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
    /// its pattern can match. An element's set (a slice's elements and
    /// rest) narrows member by member, as containment of a set does.
    fn narrow_parts(&self, env: &Env, pred: &Type) -> Result<bool> {
        ensure_sufficient(|| {
            let Some(p) = pred.deref_cloned() else { return self.contains(env, pred) };
            let Some(t) = self.shape_under(env, &p)? else {
                return self.contains(env, pred);
            };
            let all = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
                for (t, p) in parts {
                    if !t.narrow_pattern(env, p)? {
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
                        Some(Type::Set(ref qs)) => all(&mut qs.iter().map(|q| (&**e, q))),
                        _ => e.narrow_pattern(env, pe),
                    }
                }
                _ => self.contains(env, pred),
            }
        })
    }

    /// `members ⊇ self` for a select arm's predicate over what reaches the
    /// arm, `members` being the members of that union it can match: each
    /// open cell of self takes the union of its parts over every member and
    /// each alternative of self that matches it, where containment against
    /// the union commits it to the first member. A cell a matching
    /// alternative leaves open stays open.
    pub fn narrow_per_member(&self, env: &Env, members: &[Type]) -> Result<()> {
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
                t => t.for_each_child(&mut |c| open(c, seen, out)),
            })
        }
        let mut cells: SmallVec<[TVar; 8]> = SmallVec::new();
        open(self, &mut LPooled::take(), &mut cells);
        let mut parts: SmallVec<[Option<SmallVec<[Type; 4]>>; 8]> =
            cells.iter().map(|_| Some(SmallVec::new())).collect();
        let mut alts: SmallVec<[Type; 8]> = SmallVec::new();
        crate::expr::union_members(env, self, &mut alts)?;
        if alts.is_empty() {
            alts.push(self.clone())
        }
        for m in members {
            for alt in alts.iter() {
                if !alt.could_match(env, m)? {
                    continue;
                }
                let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                let copy = alt
                    .reset_tvars_int(&mut known, Fresh::Copy)
                    .unwrap_or_else(|| alt.clone());
                if !m.narrow_member(env, &copy)? {
                    continue;
                }
                // a copy left open took an open cell of the member's part,
                // whose values reach the arm as the member's others do
                let mut taken: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                super::settle::position_cells(m, &mut taken);
                for (c, p) in cells.iter().zip(parts.iter_mut()) {
                    let Some(f) = known.get(&c.cell_addr()) else { continue };
                    match (f.binding(), p.as_mut()) {
                        (Some(t), Some(p)) => p.push(t.resolve_tvars()),
                        (None, Some(p)) if taken.contains_key(&f.cell_addr()) => {
                            p.push(Type::TVar(f.clone()))
                        }
                        (None, _) => *p = None,
                        (Some(_), None) => (),
                    }
                }
            }
        }
        for (c, p) in cells.iter().zip(parts.iter()) {
            if let Some(p) = p
                && !p.is_empty()
            {
                let p: LPooled<Vec<&Type>> = p.iter().collect();
                Type::union_exact(env, &p)?.contains(env, &Type::TVar(c.clone()))?;
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

    /// `self ⊇ t` with each of `held`, open cells, one type equal only to
    /// itself, whatever it becomes: over copies of both, each of `held`
    /// copied to one fresh cell held rigid, so it decides no cell.
    #[doc(hidden)]
    pub fn contains_holding(&self, env: &Env, t: &Type, held: &[TVar]) -> Result<bool> {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let gates: SmallVec<[RigidGate; 4]> = held
            .iter()
            .map(|tv| {
                let c = TVar::empty_named(tv.name.clone());
                let gate = c.open_rigid();
                known.insert(tv.cell_addr(), c);
                gate
            })
            .collect();
        let outer = self.reset_tvars_int(&mut known, Fresh::Copy);
        let inner = t.reset_tvars_int(&mut known, Fresh::Copy);
        let outer = outer.as_ref().unwrap_or(self);
        let inner = inner.as_ref().unwrap_or(t);
        let r = outer.contains(env, inner);
        drop(gates);
        r
    }

    /// `self ⊇ t` over a copy of self's cells but those `known` maps
    /// already (to themselves), so binding a copy to a part of `t` binds
    /// none of self's cells; a copy still open after, a part an open cell
    /// of `t` took, is its source cell again, unless that cell is held
    /// rigid, which no pattern decides. `false` when one cannot be.
    #[doc(hidden)]
    pub fn contains_through_copy(
        &self,
        env: &Env,
        known: &mut AHashMap<usize, TVar>,
        t: &Type,
    ) -> Result<bool> {
        let mut sources: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut sources);
        let copy =
            self.reset_tvars_int(known, Fresh::Copy).unwrap_or_else(|| self.clone());
        let mut pairs: LPooled<Vec<(TVar, TVar)>> = sources
            .drain()
            .filter_map(|(addr, s)| {
                let c = known.get(&addr)?;
                (!c.same_cell(&s)).then(|| (s, c.clone()))
            })
            .collect();
        pairs.sort_by_key(|(_, c)| c.read().id);
        let _ = copy.contains(env, t);
        for (s, c) in
            pairs.iter().filter(|(_, c)| c.open_cell().is_some_and(|o| !o.is_rigid()))
        {
            if !Type::TVar(s.clone()).contains(env, &Type::TVar(c.clone()))? {
                return Ok(false);
            }
        }
        Ok(true)
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
        self.reset_memo(known, &mut LPooled::take(), how)
    }

    /// [`Self::reset_tvars_int`], each shared composite copied once
    /// (`nodes`, by address), so a shared node stays shared.
    fn reset_memo(
        &self,
        known: &mut AHashMap<usize, TVar>,
        nodes: &mut AHashMap<usize, Option<Type>>,
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
        self.rename_memo(known, &mut LPooled::take())
    }

    /// [`Self::rename_int`], each bound cell's binding rebuilt once
    /// (`memo`, by cell), so a shared binding stays shared.
    fn rename_memo(
        &self,
        known: &AHashMap<usize, TVar>,
        memo: &mut AHashMap<usize, Option<Type>>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(t) => Some(tv.forwarded_as(match memo.get(&tv.cell_addr()) {
                    Some(r) => r.clone().unwrap_or(t),
                    None => {
                        let r = t.rename_memo(known, memo).unwrap_or(t);
                        memo.insert(tv.cell_addr(), Some(r.clone()));
                        r
                    }
                })),
                None => known.get(&tv.cell_addr()).map(|f| Type::TVar(f.named_as(tv))),
            },
            Type::Fn(ft) => {
                ft.cow_walk(|t| t.rename_memo(known, memo)).map(|f| Type::Fn(Arc::new(f)))
            }
            t => {
                once(t, memo, |memo| t.cow_children(&mut |c| c.rename_memo(known, memo)))
            }
        })
    }

    // CR claude for claude: [risk/low] A walk that inlines a bound cell's binding under a
    // quantified function type (instantiate_memo here, rename_memo, resolve_tvars) can
    // put a cell named like that function type's quantifier inside it, which every
    // by-name reading of the quantifier (FnType::each_quantifier, collect_tvars'
    // first-seen pick) may take for the quantifier: the capture typedef-capture-01 closed
    // for replace_tvars (under_quantifiers), by binding instead of by substitution. A
    // forwarding binding keeps the occurrence's name (TVar::forwarded_as), so only a
    // structured binding (`'_7 := Array<'a>` under `fn<'a: ..>`) brings the name in. Not
    // reproduced: ~/ptmp/cr29/scratch/r10-R/{t_inst_a..d,t_res_c..h}.gx (a declared
    // quantifier beside a parameter bound to an outer 'a, a body annotation's free
    // variable, a callee instanced under a rank-2 argument) run alike with the
    // quantifier named 'a and 'b, under GRAPHIX_NO_SUBST=1 too. If one shows: rename the
    // quantifier as under_quantifiers does when an inlined binding holds its name.
    // (resolve-capture-01)
    /// `None` when no TVar is beneath.
    /// The bindings resolved, each open cell freshened as `how` says.
    pub(super) fn instantiate_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Option<Type> {
        self.instantiate_memo(known, &mut LPooled::take(), how)
    }

    /// [`Self::instantiate_int`], each bound cell's binding rebuilt once
    /// (`memo`, by cell), so a shared binding stays shared.
    fn instantiate_memo(
        &self,
        known: &mut AHashMap<usize, TVar>,
        memo: &mut AHashMap<usize, Option<Type>>,
        how: Fresh<'_>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                Some(match tv.binding() {
                    Some(t) => tv.forwarded_as(match memo.get(&tv.cell_addr()) {
                        Some(r) => r.clone().unwrap_or(t),
                        None => {
                            let r = t.instantiate_memo(known, memo, how).unwrap_or(t);
                            memo.insert(tv.cell_addr(), Some(r.clone()));
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
        fn go(
            t: &Type,
            name: &mut impl FnMut(&TVar) -> Option<Type>,
            walking: &mut AHashSet<usize>,
            unwritable: &mut bool,
        ) -> Option<Type> {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => Some(match name(tv) {
                    Some(t) => t,
                    None if !walking.insert(tv.cell_addr()) => {
                        *unwritable = true;
                        Type::Bottom
                    }
                    None => {
                        let r = match tv.binding() {
                            Some(b) => go(&b, name, walking, unwritable).unwrap_or(b),
                            None => {
                                let lower: SmallVec<[Type; 4]> = tv
                                    .cell_lower()
                                    .iter()
                                    .map(|l| {
                                        go(l, name, walking, unwritable)
                                            .unwrap_or_else(|| l.clone())
                                    })
                                    .collect();
                                match (lower.is_empty(), tv.written_open()) {
                                    (false, _) => Type::Set(Arc::from_iter(lower)),
                                    (true, Some(t)) => t,
                                    (true, None) => {
                                        *unwritable = true;
                                        Type::Bottom
                                    }
                                }
                            }
                        };
                        walking.remove(&tv.cell_addr());
                        r
                    }
                }),
                t => t.cow_children(&mut |c| go(c, name, walking, unwritable)),
            })
        }
        let mut unwritable = false;
        let t = go(self, name, &mut LPooled::take(), &mut unwritable)
            .unwrap_or_else(|| self.clone());
        (!unwritable).then(|| t.normalize())
    }

    /// A copy of self with every TVar named in `known` replaced by the
    /// corresponding type; any other TVar is freshened as
    /// [`Self::reset_tvars`] does, its conjuncts and binding rewritten
    /// the same way. TVar-free structure is returned shared. Capture
    /// avoiding under each function type it holds ([`under_quantifiers`]).
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        self.replace_tvars_int(&mut Subst::new(known), &mut LPooled::take())
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
            Type::Set(set) => Type::cow_slice(set, |c| c.replace_tvars_int(s, fresh))
                .map(|set| Type::flatten_set(set.iter().cloned())),
            t => t.cow_children(&mut |c| c.replace_tvars_int(s, fresh)),
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

    /// Reopen every cell bound to ⊥: at a definition's gate that is a
    /// vacuous fact (`throws := ⊥`, the body observed nothing). Every other
    /// binding stays, shared with the cells it relates.
    #[doc(hidden)]
    pub fn unbind_vacuous_tvars(&self) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                let required = tv.cell().read().facts.contains(CellFact::BottomRequired);
                debug_assert!(!required || tv.binding() == Some(Type::Bottom));
                if !required && tv.binding().is_some_and(|t| t == Type::Bottom) {
                    tv.unbind()
                }
            }
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_vacuous_tvars()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arcstr::literal;

    /// A chain of open cells, each the lower bound of the next, merged
    /// below a rigid cell: every one becomes that cell, one merge after
    /// another, never a recursion as deep as the chain.
    #[test]
    fn a_chain_below_a_rigid_cell_merges_into_it_flat() {
        let n = 4000;
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(move || {
                let rigid = TVar::empty_named(literal!("c"));
                let _gate = rigid.open_rigid();
                let cells: Vec<TVar> =
                    (0..n).map(|i| TVar::empty_named(arcstr::format!("_{i}"))).collect();
                for w in cells.windows(2) {
                    w[1].add_lower(Type::TVar(w[0].clone()))
                }
                cells[n - 1].alias_cells(&rigid);
                assert!(cells.iter().all(|c| c.same_cell(&rigid)));
            })
            .expect("spawn")
            .join()
            .expect("the merges")
    }

    // Open cells below a merged cell go into the rigid survivor in their
    // order there: `x ⊇ o1, o2` with `o1 ⊇ Array<o2>` merges o1 first, so o2,
    // which the survivor then reaches through o1's bound, is refused by the
    // occurs check and stays a lower bound.
    #[test]
    fn cells_below_a_rigid_cell_merge_into_it_in_order() {
        let rigid = TVar::empty_named(literal!("c"));
        let _gate = rigid.open_rigid();
        let [x, o1, o2] = ["x", "o1", "o2"].map(|n| TVar::empty_named(n.into()));
        o1.add_lower(Type::Array(Arc::new(Type::TVar(o2.clone()))));
        x.add_lower(Type::TVar(o1.clone()));
        x.add_lower(Type::TVar(o2.clone()));
        x.alias_cells(&rigid);
        assert!(x.same_cell(&rigid) && o1.same_cell(&rigid));
        assert!(!o2.same_cell(&rigid));
    }

    #[test]
    fn alias_to_itself_is_a_no_op() {
        let tv = TVar::empty_named(literal!("a"));
        tv.alias(&tv);
        tv.alias_cells(&tv);
        assert!(!tv.is_bound());
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

    /// A copy knows what the source's checks learned of each cell.
    #[test]
    fn a_copy_keeps_its_cells_facts() {
        let tv = TVar::empty_named(literal!("a"));
        tv.bind(Type::Bottom);
        let all = CellFact::BottomRequired | CellFact::BottomFed | CellFact::CycleRefused;
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
        let (c, d) =
            (TVar::empty_named(literal!("_3")), TVar::empty_named(literal!("_4")));
        d.alias(&c);
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
    let cell = tv.read().cell.clone();
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
enum Apart<'a> {
    BesideKnown(&'a TVar),
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
    fn rigid_apart(&self) -> Option<Apart<'_>> {
        let mut rigid = self.open.iter().filter(|tv| tv.is_rigid());
        let a = rigid.next()?;
        match self.known > 0 {
            true => Some(Apart::BesideKnown(a)),
            false => {
                rigid.any(|b| b.cell_addr() != a.cell_addr()).then_some(Apart::TwoRigid)
            }
        }
    }

    /// Members that differ in open cells alone are one member: merge
    /// those cells. Beside no known member, the open members must be one
    /// type: merge them into one cell.
    fn merge_open(&self) {
        for (a, b) in self.same.iter() {
            if a.cell_addr() != b.cell_addr() {
                a.alias_cells(b)
            }
        }
        if self.known == 0
            && let Some((first, rest)) = self.open.split_first()
        {
            for tv in rest {
                if tv.cell_addr() != first.cell_addr() {
                    tv.alias_cells(first)
                }
            }
        }
    }

    /// What each open member is narrowed to for the union to be one
    /// type: `None` when it has several known members already.
    fn narrowing(&self) -> Option<Type> {
        match (&self.first, self.known) {
            (Some(k), 1) => Some(k.clone()),
            (_, 0) => Some(Type::Bound(Bound::Singleton)),
            _ => None,
        }
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
