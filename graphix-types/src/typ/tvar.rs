use crate::{
    LambdaId,
    dbgenv::graphix_dbg_bind,
    env::Env,
    expr::{ModPath, TypeDefBody},
    image,
    stack::ensure_sufficient,
    typ::{PRINT_FLAGS, PrintFlag, Type, node_addr, setops::union_identical},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
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
};
use triomphe::Arc;

image_id!(TVarId);

pub(super) fn would_cycle_inner(addr: usize, t: &Type) -> bool {
    would_cycle_seen(addr, t, &mut LPooled::take())
}

// Conjunct graphs can be cyclic; a revisited cell adds no
// reachability, so it answers false.
fn would_cycle_seen(addr: usize, t: &Type, seen: &mut IntSet<usize>) -> bool {
    ensure_sufficient(|| would_cycle_seen_inner(addr, t, seen))
}

fn would_cycle_seen_inner(addr: usize, t: &Type, seen: &mut IntSet<usize>) -> bool {
    // `seen` is a true visited set holding both cell and composite
    // node addresses; the answer depends only on reachable leaves.
    if let Some(node) = node_addr(t)
        && !seen.insert(node)
    {
        return false;
    }
    match t {
        // Expansion reaches every cell a Ref's params hold; a typedef
        // body's free tvars are its params, so the params cover it.
        Type::Ref(r) => r.params.iter().any(|p| would_cycle_seen(addr, p, seen)),
        Type::TVar(t) => {
            let cell = t.cell();
            let cell_addr = cell.data_ptr().addr();
            cell_addr == addr || {
                if !seen.insert(cell_addr) {
                    return false;
                }
                let (binding, cons) = {
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                binding.is_some_and(|b| would_cycle_seen(addr, &b, seen))
                    || cons.iter().any(|c| would_cycle_seen(addr, c, seen))
            }
        }
        t => t
            .try_for_each_child(&mut |c| {
                if would_cycle_seen(addr, c, seen) {
                    ControlFlow::Break(())
                } else {
                    ControlFlow::Continue(())
                }
            })
            .is_break(),
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
            Self::Generic => u32::MAX,
        }
    }

    /// The definition that owns the cell.
    #[doc(hidden)]
    pub fn owner(&self) -> Option<LambdaId> {
        match self {
            Self::Def { owner, .. } => Some(*owner),
            Self::Top | Self::Generic => None,
        }
    }

    fn is_generic(&self) -> bool {
        matches!(self, Self::Generic)
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
    pub(super) fn copied_by(&self, open: &IntSet<LambdaId>) -> bool {
        match self {
            Self::Generic => true,
            Self::Def { owner, .. } => !open.contains(owner),
            Self::Top => false,
        }
    }
}

impl Pack for Level {
    fn encoded_len(&self) -> usize {
        1 + match self {
            Self::Def { depth, owner } => depth.encoded_len() + owner.encoded_len(),
            Self::Top | Self::Generic => 0,
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
            _ => Err(PackError::UnknownTag),
        }
    }
}

thread_local! {
    static LEVEL: std::cell::Cell<Level> = const { std::cell::Cell::new(Level::GENERIC) };
    static TASK: std::cell::Cell<TaskId> = const { std::cell::Cell::new(TaskId::OUTSIDE) };
    /// The compile task whose foreign writes this thread records
    /// ([`OwnWrites`]), and the first it made.
    static OWN_WRITES: std::cell::Cell<Option<(TaskId, Option<ForeignWrite>)>> =
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
    own: Option<(TaskId, Option<ForeignWrite>)>,
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
    !OWN_WRITES.get().is_some_and(|(task, _)| task == TASK.get())
}

fn written_of(owner: TaskId, what: &str, cell: Option<usize>) {
    if owner < TASK.get() {
        foreign_write(owner, what, cell)
    }
}

/// While this lives, record whether the running compile task changes a
/// cell or var an earlier task created. A task runs whole on one
/// thread, and a task run while it waits keeps its own record.
#[must_use]
#[doc(hidden)]
pub struct OwnWrites(Option<(TaskId, Option<ForeignWrite>)>);

impl OwnWrites {
    #[doc(hidden)]
    pub fn enter(task: TaskId) -> Self {
        Self(OWN_WRITES.replace(Some((task, None))))
    }

    /// The first write the task made outside itself.
    #[doc(hidden)]
    pub fn foreign(self) -> Option<ForeignWrite> {
        OWN_WRITES.get().and_then(|(_, w)| w)
    }
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
    if let Some((task, first)) = OWN_WRITES.get()
        && task == TASK.get()
        && first.is_none_or(|f| f.cell.is_none() && cell.is_some())
    {
        OWN_WRITES.set(Some((task, Some(ForeignWrite { cell }))))
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

// XCR claude for claude: [bug/high] lower never claims a generic cell, on the assumption
// that every generic cell is a scheme's. Cells born generic as scratch also become live
// bindings' types: a let annotation's free variables (`let r: ['a, null] = null`,
// `never<['a, null]>()`), whose cells node::lambda::body_annotation only aliases, and a
// let pattern's predicate over a ⊥ initializer (`let (a, b) = never()`, node::bind). A
// function that writes such a binding (`let f = |x| { r <- x; x }`) merges x's cell
// with that generic cell and is generalized over it. So `(f(1), f("a"))` passes
// --check, and the build either refuses it with the internal "an instance at .. of a
// definition typed .." error (graphix-fuzz: DIVERGENCE) or runs it with r holding an
// i64 and then a string. Fix: claim those cells for current_level() where they become a
// binding's type (body_annotation, the ⊥-initializer predicate). Also let merge_into
// give the merged cell the shallower level of the two (TVar::claim, which takes generic
// cells), so a live cell never ends up generic. probe:
// design/review-2026-10-26/repro/t-tvar-annotation-var-generic.gx (t-tvar-01)
// 2026-10-26 claude: Bind::compile claims the type of each name its block writes (a <-
// target or &mut referent, the ids it hands mark_connect_target) for the binding's
// level with Type::claim_free, which claims every cell the type reaches through
// bindings, generic ones included, except a nested fn type's own quantifiers. An
// annotation's variables, never<T>'s and a ⊥ initializer's pattern cells are then the
// binding's, so a definition writing it shares them and is not generalized over them;
// an unwritten annotated lambda stays generic. The repro and its four variants are
// refused at the check. Not done: the merge_into backstop (a merge keeping the
// shallower level through claim): no case is known to reach it after this change, and
// it would reach every scheme merge. Pins: lang::types::written_annotation_variable_is_monomorphic,
// written_never_annotation_is_monomorphic, written_bottom_pattern_is_monomorphic,
// written_body_annotation_variable_is_monomorphic,
// unwritten_annotated_lambda_stays_generic.
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
/// A generic cell stays generic (a scheme bound into a cell is still a
/// scheme), and a cell already at or above `level`'s depth reaches
/// nothing deeper.
pub(super) fn lower(t: &Type, level: Level) {
    fn go(t: &Type, level: Level, nodes: &mut AHashSet<usize>) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if !tv.level().is_generic() => tv.claim(level),
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
    /// What the checks learned of the cell ([`CellFact`]): every copy, merge
    /// and image carries the whole set.
    pub(crate) facts: BitFlags<CellFact>,
    /// Nonzero while a declared (named) lambda tvar is inside its def's
    /// body check: a rigid unbound cell never binds, so the body must
    /// be well-typed for arbitrary 'a.
    pub(crate) rigid_gates: u32,
    /// The rigid gates a call opened on a rank-2 formal's quantifiers: a callee
    /// checked against the declared bound alone, so a unification may not
    /// add a conjunct the bound does not imply.
    pub(crate) quantifier_gates: u32,
    /// Where the cell belongs ([`Level`]); everything it reaches is at
    /// its depth or shallower.
    pub(crate) level: Level,
    /// The compile task that created the cell ([`written`]).
    pub(crate) task: TaskId,
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
    /// production binds a cell, so one still open once its writers are
    /// checked had only ⊥ produced into it (`TVar::settle`).
    BottomFed,
    /// The cell is bound to ⊥ because something required it (`⊥ ⊇ 'x`:
    /// a `_` annotation, a `-> _` return): a fact the gate keeps, where a
    /// ⊥ nothing required is vacuous and reopens.
    BottomRequired,
    /// The cell is a quantifier of a rank-2 formal: wherever the formal is
    /// passed on, in its definition's check or an instance's elaboration,
    /// it is any instance of itself, copied fresh for each use.
    FormalQuantifier,
}

impl Default for TCell {
    fn default() -> Self {
        TCell {
            binding: None,
            constraints: SmallVec::new(),
            facts: BitFlags::empty(),
            rigid_gates: 0,
            quantifier_gates: 0,
            level: current_level(),
            task: TASK.get(),
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
    quantifier: bool,
}

impl Drop for RigidGate {
    fn drop(&mut self) {
        let mut cell = self.cell.write();
        written(&cell);
        cell.rigid_gates = cell.rigid_gates.saturating_sub(1);
        if self.quantifier {
            cell.quantifier_gates = cell.quantifier_gates.saturating_sub(1);
        }
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
            static DEBUGGING: RefCell<IntSet<usize>> = RefCell::new(IntSet::default());
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
                static PRINTING: RefCell<IntSet<usize>> = RefCell::new(IntSet::default());
            }
            let addr = self.cell_addr();
            if !PRINTING.with_borrow_mut(|s| s.insert(addr)) {
                return write!(f, "'{}: …", ShortName(&self.name));
            }
            let r = (|| {
                let (typ, cons) = {
                    let cell = self.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                // a generated cell's binding is all a reader needs of it
                match (&typ, self.generated()) {
                    (Some(t), true) => return write!(f, "{t}"),
                    _ => write!(f, "'{}: ", ShortName(&self.name))?,
                }
                match typ {
                    Some(t) => write!(f, "{t}"),
                    None if cons.is_empty() => write!(f, "unbound"),
                    None => {
                        write!(f, "unbound within ")?;
                        for (i, c) in cons.iter().enumerate() {
                            if i > 0 {
                                write!(f, " & ")?
                            }
                            write!(f, "{c}")?
                        }
                        Ok(())
                    }
                }
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
            Type::Concrete => t.require_everywhere(c, false, hand, &mut LPooled::take()),
            Type::Data => t.require_everywhere(c, false, hand, &mut LPooled::take()),
            Type::Singleton => t.require_singleton(hand),
            Type::OneNumber => t.require_one_number(hand),
            Type::NoError => t.require_no_error(hand),
            Type::Discernible | Type::Ordered => t.require_compared(c, hand),
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
                (c.binding.clone(), c.constraints.clone())
            })
        };
        if let Some((binding, cons)) = lowered {
            for t in binding.iter().chain(cons.iter()) {
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

    /// Bind an open, flexible, unconstrained cell to `t`, which is not a
    /// var and does not hold this one, when nothing but this var holds
    /// the cell: every way a type reaches a cell is a clone of a var
    /// sharing it, so `t` cannot reach it and the occurs check would find
    /// nothing. False, binding nothing, when any of that fails.
    pub fn bind_unshared(&self, t: &Type) -> bool {
        let unshared = Arc::count(&self.0) == 1 && Arc::count(&self.read().cell) == 1;
        if !unshared
            || matches!(t, Type::TVar(_))
            || self.is_bound()
            || self.is_rigid()
            || !self.cell_constraints().is_empty()
        {
            return false;
        }
        self.bind(t.clone());
        true
    }

    /// Bind the cell, replacing any binding.
    #[doc(hidden)]
    pub fn bind(&self, t: Type) {
        require_bounds(&self.cell_constraints(), &t);
        lower(&t, self.level());
        let cell = self.cell();
        let mut c = cell.write();
        if decided(&c) {
            c.binding = Some(t)
        }
    }

    /// Add a conjunct to this var's cell constraints (deduped).
    pub fn add_cell_constraint(&self, c: Type) {
        let cell = self.cell();
        let existing = cell.read().constraints.clone();
        let mut new = new_conjuncts(&existing, [c]);
        let level = cell.read().level;
        for c in new.iter() {
            lower(c, level)
        }
        if !new.is_empty() {
            let mut c = cell.write();
            if decided(&c) {
                c.constraints.extend(new.drain(..));
            }
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
        if how == Merge::Cells
            && !other.is_rigid()
            && (self.is_rigid() || self.cell().read().task < other.cell().read().task)
        {
            return other.merge_into(self, how);
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
                    false => other.merge_into(self, Merge::Name),
                    true => self.merge_into(other, Merge::Cells),
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
        {
            let mut oc = o_cell.write();
            oc.constraints.extend(to_add.drain(..));
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
        self.cell().read().constraints.iter().any(|c| matches!(c, Type::Concrete))
    }

    /// Whether the cell holds the `Data` conjunct.
    pub(crate) fn requires_data(&self) -> bool {
        self.cell().read().constraints.iter().any(|c| matches!(c, Type::Data))
    }

    /// Whether the cell holds the `Function` conjunct.
    pub(crate) fn requires_function(&self) -> bool {
        self.cell().read().constraints.iter().any(|c| matches!(c, Type::Function))
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
        if cx.cells.insert(self.cell_addr())
            && !self.earlier_task()
            && let Some(t) = self.binding()
            && let Some(n) = t.normalize_int(cx)
        {
            self.bind(n);
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
        c.binding = None;
        c.facts.remove(CellFact::BottomRequired);
    }

    /// Open a rigid gate on the cell this var reads now; see
    /// [`TCell::rigid_gates`].
    pub fn open_rigid(&self) -> RigidGate {
        self.open_gate(false)
    }

    /// [`Self::open_rigid`] for a rank-2 formal's quantifier; see
    /// [`TCell::quantifier_gates`].
    pub fn open_quantifier_rigid(&self) -> RigidGate {
        self.open_gate(true)
    }

    fn open_gate(&self, quantifier: bool) -> RigidGate {
        let cell = self.cell();
        {
            let mut c = cell.write();
            written(&c);
            c.rigid_gates += 1;
            if quantifier {
                c.quantifier_gates += 1;
            }
        }
        RigidGate { cell, quantifier }
    }

    /// Whether a rank-2 formal's quantifier holds this cell rigid.
    pub fn is_quantifier_rigid(&self) -> bool {
        self.read().cell.read().quantifier_gates > 0
    }

    pub(crate) fn earlier_task(&self) -> bool {
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

    /// Mark the cell a quantifier of a rank-2 formal; see
    /// [`CellFact::FormalQuantifier`].
    pub fn mark_formal_quantifier(&self) {
        let cell = self.cell();
        let mut c = cell.write();
        if decided(&c) {
            c.facts |= CellFact::FormalQuantifier;
        }
    }

    /// Whether the cell is a quantifier of a rank-2 formal.
    pub(crate) fn is_formal_quantifier(&self) -> bool {
        self.cell().read().facts.contains(CellFact::FormalQuantifier)
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

    /// Identity of the shared binding cell, for set membership tests.
    #[doc(hidden)]
    pub fn cell_addr(&self) -> usize {
        self.read().cell.data_ptr().addr()
    }

    /// A fresh cell standing for this one in a copy: the name, and the
    /// conjuncts, refusal and binding rebuilt through `walk`; memoized
    /// by cell in `fresh`, so a copy keeps the source's alias topology.
    fn freshen<M>(
        &self,
        fresh: &mut AHashMap<usize, TVar>,
        memo: &mut M,
        how: Fresh<'_>,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>, &mut M) -> Option<Type>,
    ) -> TVar {
        let addr = self.cell_addr();
        if let Some(f) = fresh.get(&addr) {
            return f.clone();
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
        fresh.insert(addr, f.clone());
        for c in self.cell_constraints() {
            let c = walk(&c, fresh, memo).unwrap_or(c);
            f.add_cell_constraint(c);
        }
        // what the source's checks learned holds of every copy
        let facts = self.cell().read().facts;
        f.cell().write().facts |= facts;
        if let Some(t) = self.binding() {
            f.bind(walk(&t, fresh, memo).unwrap_or(t));
        }
        f
    }
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
        ensure_sufficient(|| match self {
            Type::Error(_) | Type::Any => Ok(true),
            Type::Primitive(p) => Ok(p.contains(netidx_value::Typ::Error)),
            Type::Set(ts) => {
                for t in ts.iter() {
                    if t.may_be_error(env, commit)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::TVar(tv) => match tv.binding() {
                None => Ok(false),
                Some(b) => b.may_be_error(env, commit),
            },
            Type::Ref(_) => match self.lookup_ref_with(env, commit)? {
                Some(t) => t.may_be_error(env, commit),
                None => Ok(true),
            },
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
                hand(tv, Type::NoError)
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
            hand(tv, Type::OneNumber)
        }
    }

    /// An open member of a `OneNumber` cell's binding beside a numeric one,
    /// or two open members beside none: one may yet be another numeric type
    /// than the rest, which no bound on either can rule out, so the
    /// definition that leaves them open is refused (annotate it).
    // XCR claude for claude: [bug/medium] This needs a known numeric member. Two open
    // members with none known each get OneNumber alone (require_one_number), yet they
    // may bind two different numeric types. `let g = |b, y, z| f(select b { true => z,
    // false => y })` over a `'a: [Number, null] + OneNumber` f passes the check, and
    // `g(false, 2.5, 1)` is refused by the build with the internal 'an instance at
    // fn(x: [i64, f64]) ..' error (graphix-fuzz: DIVERGENCE). Fix: also return true
    // when no member is numeric and two or more distinct open cells remain, so
    // site_bounds refuses at the definition as it does for one open member beside a
    // numeric one. probe:
    // design/review-2026-10-26/repro/x-typecheck-generics-onenumber-two-open.gx
    // (x-typecheck-generics-03)
    // 2026-10-26 claude: one_number_open also holds beside no numeric member when two
    // distinct open cells remain, so site_bounds refuses the definition (annotate it)
    // as it does one open member beside a numeric one; one cell met twice is one
    // member. The repro is refused at the check. Pins:
    // lang::types::one_number_two_open_members_refused,
    // one_number_one_open_member_twice.
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

    /// Hand `bound` to every open cell the type reaches, a function's
    /// parts only when `in_fns`; each cell and shared node once.
    fn require_everywhere(
        &self,
        bound: &Type,
        in_fns: bool,
        hand: &mut dyn FnMut(&TVar, Type),
        seen: &mut AHashSet<usize>,
    ) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) if !seen.insert(tv.cell_addr()) => (),
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.require_everywhere(bound, in_fns, hand, seen),
                None => hand(tv, bound.clone()),
            },
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            Type::Fn(ft) if in_fns => ft.for_each_part(&mut |t, _| {
                t.require_everywhere(bound, in_fns, hand, seen)
            }),
            Type::Fn(_) => (),
            t => {
                t.for_each_child(&mut |c| c.require_everywhere(bound, in_fns, hand, seen))
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
    // XCR claude for claude: [perf/low] holds_cell enters every binding and child with
    // no seen set. node::module::decided_binds calls it, through ForeignWrite::held_by,
    // on the type of every binding in Env::by_id. A pair DAG anywhere in scope
    // therefore makes a module's foreign-write refusal exponential: 1.2 s at depth 26
    // and 4.7 s at depth 28, against 41 ms for the annotated program. Add a seen set of
    // cells and shared nodes, as open_beneath has. Name the cell by TVar::cell_addr
    // (Arc::as_ptr), not data_ptr, so ForeignWrite uses the same address convention as
    // every other walk. probe:
    // design/review-2026-10-26/repro/t-tvar-held-by-dag-exponential.sh (t-tvar-05)
    // 2026-10-26 claude: holds_cell walks through a seen set of cells and shared nodes
    // (as open_beneath does), and every cell address in tvar.rs (TVar::cell_addr, the
    // occurs check, ForeignWrite, holds_cell) is now the TCell's (data_ptr), one
    // convention. The repro at D=28 refuses in 0.12 s (was 4.7 s). Pin:
    // graphix-shell/tests/deep_nesting.rs
    // a_foreign_write_refusal_beside_a_shared_type_dag_is_linear (depth 60; runs past
    // its 15 s budget with the memo undone).
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
    pub(super) fn collect_tvars_seen(
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

    /// [`Self::collect_tvars`] over positions alone: a nested signature's
    /// cells by its own positions, never its conjuncts, each shared node
    /// (`seen`) walked once, so the walk is linear in the type.
    // XCR claude for claude: [perf/medium] This has no shared-node memo, so over a type
    // DAG it visits every path, contrary to its doc. FnType::for_each_part calls it on
    // every walk, and settle_terminal, inner_quantifiers and site_bounds' collect_tvars
    // go through for_each_part. A generic call chain h -> g -> f, where f returns a DAG
    // of nested pair lets, elaborates in 0.58 s at depth 20, 2.1 s at 22 and 7.5 s at
    // 24, while --check stays at 0.08 s. The deep_nesting pins call f directly and
    // never reach this walk. Fix: skip a node_addr already walked, as
    // collect_tvars_seen does, and pin the chain in
    // graphix-shell/tests/deep_nesting.rs. probe:
    // design/review-2026-10-26/repro/x-typecheck-generics-dag-position-walk.sh
    // (x-typecheck-generics-07)
    // 2026-10-26 claude: collect_position_tvars takes a seen set of shared nodes,
    // threaded from position_cells and for_each_part. The repro is flat (depth
    // 20/22/24: 91/85/82 ms; depth 60 under 0.1 s). Pin: deep_nesting.rs
    // shared_type_dags_through_generic_calls_build_in_linear_time, its h -> g -> f
    // program (fails its 15 s budget with the memo undone).
    // XCR claude for claude: [perf/medium] This walk has no visited set, so it walks a
    // shared type DAG as a tree. A pair DAG built through a generic constructor (`let
    // mk = |x: 'a| -> ('a, 'a) (x, x)`, then x1 = mk(x0) .. x24 = mk(x23) inside a
    // definition) checks in 40 ms but builds in about 10 s at depth 24, x4 per two
    // levels. gdb samples sit here, under FnType::collect_tvars and position_cells from
    // settle_terminal. The inline-tuple pin in deep_nesting.rs does not reach this
    // path. Fix: skip a node whose node_addr was already seen, as collect_tvars_seen
    // does, with the set threaded in from position_cells and collect_tvars, and pin the
    // generic-constructor shape. probe:
    // design/review-2026-10-26/repro/t-typ-mod-generic-pair-dag-exponential.sh
    // (t-typ-mod-02)
    // 2026-10-26 claude: collect_position_tvars memoized (see x-typecheck-generics-07);
    // the generic-constructor shape then still doubled per level in Type::pre_unify_arg
    // (gdb), which now pre-unifies each pair of shared nodes once (pre_unify_seen): a
    // repeat of a pair that already pre-unified adds nothing the argument's own check
    // does not decide. n=24 106 ms (was 12.5 s), n=60 116 ms. Pin: deep_nesting.rs
    // shared_type_dags_through_generic_calls_build_in_linear_time, its mk program
    // (fails its budget with either memo undone).
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
        self.open_beneath(true)
    }

    /// [`Self::has_unbound`] less the rigid cells: a declared variable in
    /// its own body never binds, so it stands as a constant.
    pub fn has_free(&self) -> bool {
        self.open_beneath(false)
    }

    fn open_beneath(&self, rigid: bool) -> bool {
        fn go(t: &Type, rigid: bool, seen: &mut AHashSet<usize>) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    seen.insert(tv.cell_addr())
                        && match tv.binding() {
                            Some(b) => go(&b, rigid, seen),
                            None => rigid || !tv.is_rigid(),
                        }
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
                t => t
                    .try_for_each_child(&mut |c| {
                        if go(c, rigid, seen) {
                            ControlFlow::Break(())
                        } else {
                            ControlFlow::Continue(())
                        }
                    })
                    .is_break(),
            })
        }
        go(self, rigid, &mut LPooled::take())
    }

    /// `self ⊇ pred` for a pattern's predicate over what reaches it: over
    /// a union, by member ([`Self::narrow_per_member`]). Whether some
    /// member, or `self`, contains it.
    // XCR claude for claude: [bug/high] A predicate cell standing over an `Any` part of
    // self stays open here: `Any ⊇ 'a` binds nothing, and a plain bind's cell is not
    // fresh, so bind_narrowed never checks it. The arm body's uses then decide the
    // bind's type. `(x, 1)` over (Any, i64) with `str::len(x)` passes the check, and
    // the JIT panics on an i64 in a String slot. The same happens through a written
    // union (`[`A(Any), `B(Any)] as `A(x)`, `[Array<Any>, null] as [x, ..]`), which
    // narrows here at compile. `let (x, _) = v` over the same type gives x Any and
    // refuses the use. Fix: after the containment, bind every cell of pred that is
    // still open and whose part of self is Any to Any: a pattern bind receives the
    // scrutinee's value. probe:
    // design/review-2026-10-26/repro/c-pattern-bind-over-any-open.gx (c-pattern-03)
    // 2026-10-26 claude: narrow_member (every path of narrow_pattern and each member
    // narrow_per_member takes) ends with Type::open_over_any: a cell of pred still
    // open where self's part is Any is bound by 'a ⊇ Any, through tuple positions,
    // struct fields, variant payloads, array and list elements (a slice's element set
    // member by member) and a union's members pred could match; the written-union
    // narrowing at compile goes through the same narrow_pattern. All four repro forms
    // are refused at the check (string does not contain Any). Pins:
    // lang::select::bind_over_any_tuple_position_refused,
    // bind_over_any_element_refused, bind_over_any_written_union_refused,
    // bind_over_any_written_element_refused, and bind_over_any_is_any for the accepted
    // use.
    pub fn narrow_pattern(&self, env: &Env, pred: &Type) -> Result<bool> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        crate::expr::union_members(env, self, &mut members)?;
        members.retain(|m| pred.could_match(env, m).unwrap_or(true));
        match members.len() {
            // XCR claude for claude: [bug/critical] With one member this checks `self ⊇
            // pred` through contains, which commits a union NESTED under a constructor
            // (an array element, a tuple position, a struct field, a variant payload)
            // to its first member that holds the pattern's shape. The arm's binds and
            // its run-time test then carry that member alone. Over Array<[(i64, i64),
            // (string, string)]>, `[(a, _)]` types `a` as i64 and tests (i64, i64), so
            // ("s", "t") falls to the next arm, and without a fallback the select is
            // refused. Under a slice with a rest, whose run-time test admits the whole
            // element union, a string reaches an i64 bind and the JIT panics. CLAUDE.md
            // says a bind narrows to the union of its parts over every member the arm
            // can match. Fix: where self holds a union under a constructor that pred
            // also has, narrow that part per member (narrow_per_member on the
            // sub-part), as the top level does, and pin each position. probe:
            // design/review-2026-10-26/repro/c-pattern-nested-union-first-member.gx
            // (c-pattern-01)
            // 2026-10-26 claude: narrow_pattern narrows a member that holds a union
            // under a constructor pred has part by part (Type::needs_split,
            // narrow_parts): each part narrows as a pattern does against what reaches
            // it, an element's set (a slice's elements and rest) member by member, and
            // narrow_per_member narrows each member the same way (narrow_member), so a
            // bind takes the union of its parts over every member at any depth. Whole
            // captures under such a position are typed by member through
            // Type::split_members (tuple, struct and variant unions split, an element
            // union kept whole) in StructPatternNode::wholes_by_member, so they still
            // fit the scrutinee's type (capture_keeps_its_member). The repro's
            // irrefutable arms now make its `_` arms dead (refused); without them it
            // prints every member's arm in both engines, and the slice form is
            // refused at the check. Pins: lang::select::nested_union_binds_take_every_member,
            // nested_union_slice_bind_is_the_union,
            // nested_whole_capture_takes_every_member. Still open: a let pattern over a
            // nested union (`let {f: (a, _)} = x` with x: {f: P}) is refused as
            // refutable, the same first-member commit on node::bind's shape check
            // (pre-existing, not touched).
            // 2026-10-26 claude: Rebased over the whole-capture rework
            // (StructPatternNode::member_parts, wholes_by_member over raw members):
            // Type::split_members is gone, and wholes_by_member now runs whenever a member
            // reaches the arm, so a nested whole capture over one reaching member takes its
            // part of each member under the constructor too
            // (nested_whole_capture_takes_every_member). hold_slice_members holds an element
            // name to the union of its parts over the element's members, not to each in turn,
            // so an or-name an element binds is no longer bound to its first member's part
            // ([`A(y) | `B(y), ..] in nested_union_binds_take_every_member).
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
        if narrowed {
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
        ensure_sufficient(|| {
            let Some(p) = pred.deref_cloned() else { return Ok(false) };
            if let Type::Set(qs) = &p {
                for q in qs.iter() {
                    if self.needs_split(env, q)? {
                        return Ok(true);
                    }
                }
                return Ok(false);
            }
            let Some(t) = self.shape_under(env, &p)? else { return Ok(false) };
            let any = |parts: &mut dyn Iterator<Item = (&Type, &Type)>| {
                for (t, p) in parts {
                    if t.needs_split(env, p)? {
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
                (Type::Struct(fs), Type::Struct(pfs)) => any(&mut fs
                    .iter()
                    .zip(pfs.iter())
                    .map(|((_, t, _), (_, p, _))| (t, p))),
                (Type::Array(e), Type::Array(pe)) | (Type::List(e), Type::List(pe)) => {
                    e.needs_split(env, pe)
                }
                _ => Ok(false),
            }
        })
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
        fn open(t: &Type, seen: &mut IntSet<usize>, out: &mut SmallVec<[TVar; 8]>) {
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
                for (c, p) in cells.iter().zip(parts.iter_mut()) {
                    let Some(f) = known.get(&c.cell_addr()) else { continue };
                    match (f.binding(), p.as_mut()) {
                        (Some(t), Some(p)) => p.push(t.resolve_tvars()),
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

    /// Self with each open cell `known` maps replaced by its image;
    /// every other cell, bound or open, is kept.
    pub(crate) fn swap_cells(&self, known: &AHashMap<usize, TVar>) -> Type {
        self.swap_int(known).unwrap_or_else(|| self.clone())
    }

    fn swap_int(&self, known: &AHashMap<usize, TVar>) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => tv
                .open_cell()
                .and_then(|o| known.get(&o.cell_addr()))
                .map(|f| Type::TVar(f.clone())),
            Type::Fn(ft) => {
                ft.cow_walk(|t| t.swap_int(known)).map(|f| Type::Fn(Arc::new(f)))
            }
            t => t.cow_children(&mut |c| c.swap_int(known)),
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
                Some(t) => Some(match memo.get(&tv.cell_addr()) {
                    Some(r) => r.clone().unwrap_or(t),
                    None => {
                        let r = t.rename_memo(known, memo).unwrap_or(t);
                        memo.insert(tv.cell_addr(), Some(r.clone()));
                        r
                    }
                }),
                None => known.get(&tv.cell_addr()).map(|f| Type::TVar(f.clone())),
            },
            Type::Fn(ft) => {
                ft.cow_walk(|t| t.rename_memo(known, memo)).map(|f| Type::Fn(Arc::new(f)))
            }
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
                    Some(t) => match memo.get(&tv.cell_addr()) {
                        Some(r) => r.clone().unwrap_or(t),
                        None => {
                            let r = t.instantiate_memo(known, memo, how).unwrap_or(t);
                            memo.insert(tv.cell_addr(), Some(r.clone()));
                            r
                        }
                    },
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

    /// A copy of self with every TVar named in `known` replaced by the
    /// corresponding type; any other TVar is freshened as
    /// [`Self::reset_tvars`] does, its conjuncts and binding rewritten
    /// the same way. TVar-free structure is returned shared.
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        self.replace_tvars_int(known, &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn replace_tvars_int(
        &self,
        known: &AHashMap<ArcStr, Self>,
        fresh: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => Some(match known.get(&tv.name) {
                Some(t) => t.clone(),
                None => {
                    Type::TVar(tv.freshen(fresh, &mut (), Fresh::Copy, |t, f, ()| {
                        t.replace_tvars_int(known, f)
                    }))
                }
            }),
            // a nested fn type's lambda_ids is the copy's own
            Type::Fn(ft) => {
                let mut f =
                    ft.replace_tvars_int(known, fresh).unwrap_or_else(|| (**ft).clone());
                f.lambda_ids = ft.lambda_ids.instantiate();
                Some(Type::Fn(Arc::new(f)))
            }
            // a union substituted into a union is one union: unflattened, a
            // non-regular typedef's unfoldings nest without end
            Type::Set(s) => Type::cow_slice(s, |c| c.replace_tvars_int(known, fresh))
                .map(|s| Type::flatten_set(s.iter().cloned())),
            t => t.cow_children(&mut |c| c.replace_tvars_int(known, fresh)),
        })
    }

    /// Unbind any bound tvars, but do not unalias them.
    #[doc(hidden)]
    pub fn unbind_tvars(&self) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => tv.unbind(),
            // Sig-level Ref params are concrete or rigid: nothing to unbind.
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_tvars()),
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

/// What the syntax codec writes of `tv`'s cell: its bound and constraints,
/// or nothing inside that cell's own contents, where a quantifier that
/// its constraint names (`'a: [i64, Array<'a>]`) is the name alone and the
/// typechecker re-aliases it.
fn cell_contents<R>(tv: &TVar, f: impl FnOnce(Option<&Type>, &[Type]) -> R) -> R {
    struct Writing;
    impl Drop for Writing {
        fn drop(&mut self) {
            WRITING.with_borrow_mut(|w| w.pop());
        }
    }
    let cell = tv.read().cell.clone();
    let key = cell.data_ptr().addr();
    if WRITING.with_borrow(|w| w.contains(&key)) {
        return f(None, &[]);
    }
    WRITING.with_borrow_mut(|w| w.push(key));
    let _writing = Writing;
    let cell = cell.read();
    f(cell.binding.as_ref(), &cell.constraints)
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
            (_, 0) => Some(Type::Singleton),
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
            // XCR claude for claude: [bug/medium] `others.contains` compares with Type's
            // PartialEq, under which two distinct unbound cells are equal. [Array<'y>,
            // Array<'z>] therefore counts as one member and Singleton admits it in a
            // generic definition's check. The instance at 'y = i64, 'z = string then
            // refuses: `let k = |b, y, z| h(select b { true => [y], false => [z] })`
            // with `h = 'a: Singleton |x: 'a| -> 'a x` passes --check, and the build of
            // k(false, 1, "a") fails with "an instance at .. of a definition typed .."
            // (graphix-fuzz: DIVERGENCE). Dedup by union_identical instead, as
            // new_conjuncts does. Either refuse members that differ only in open cells,
            // or unify them as merge_open unifies bare open members. probe:
            // design/review-2026-10-26/repro/t-tvar-singleton-open-members-equal.gx
            // (t-tvar-02)
            // 2026-10-26 claude: UnionParts::member counts a member as met when it is
            // union_identical to one counted, or equal to one under PartialEq with
            // every differing place a pair of distinct non-rigid open cells
            // (open_pairs, kept in UnionParts::same); merge_open merges those pairs, so
            // the members become one and their writers must agree. A pair with a rigid
            // cell counts as a second member. The repro is refused at the check (i64
            // does not contain string); k(false, 1, 2) stays accepted; two declared
            // variables are refused at the definition. Pins:
            // lang::types::singleton_members_apart_in_open_cells_merge,
            // singleton_members_apart_in_open_cells_one_type,
            // singleton_members_apart_in_declared_variables_refused.
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
