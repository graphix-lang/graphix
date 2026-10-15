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
            let cell_addr = Arc::as_ptr(&cell).addr();
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
    /// ([`OwnWrites`]), and whether it made one.
    static OWN_WRITES: std::cell::Cell<Option<(TaskId, bool)>> =
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

impl Drop for InTask {
    fn drop(&mut self) {
        TASK.set(self.0)
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
    written_of(cell.task, "cell")
}

/// A decision about a type (a binding, a conjunct, a merge, a mark) by
/// the running compile task, reported as [`written`] is. Under
/// [`OwnWrites`] a decision about what an earlier task created is
/// recorded and not made: every module of a run that would make it is
/// refused, whatever order they ran in, and none sees another's.
pub(super) fn decided(cell: &TCell) -> bool {
    decided_of(cell.task, "cell")
}

fn decided_of(owner: TaskId, what: &str) -> bool {
    if owner >= TASK.get() {
        return true;
    }
    foreign_write(owner, what);
    !OWN_WRITES.get().is_some_and(|(task, _)| task == TASK.get())
}

fn written_of(owner: TaskId, what: &str) {
    if owner < TASK.get() {
        foreign_write(owner, what)
    }
}

/// While this lives, record whether the running compile task changes a
/// cell or var an earlier task created. A task runs whole on one
/// thread, and a task run while it waits keeps its own record.
#[must_use]
#[doc(hidden)]
pub struct OwnWrites(Option<(TaskId, bool)>);

impl OwnWrites {
    #[doc(hidden)]
    pub fn enter(task: TaskId) -> Self {
        Self(OWN_WRITES.replace(Some((task, false))))
    }

    /// Whether the task wrote outside itself.
    #[doc(hidden)]
    pub fn foreign(self) -> bool {
        OWN_WRITES.get().is_some_and(|(_, w)| w)
    }
}

impl Drop for OwnWrites {
    fn drop(&mut self) {
        OWN_WRITES.set(self.0)
    }
}

#[cold]
fn foreign_write(owner: TaskId, what: &str) {
    if let Some((task, _)) = OWN_WRITES.get()
        && task == TASK.get()
    {
        OWN_WRITES.set(Some((task, true)))
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

/// Lower every cell `t` reaches to at most `level`: what a cell at
/// `level` is bound to, or constrained by, belongs where the cell does.
/// A generic cell stays generic (a scheme bound into a cell is still a
/// scheme), and a cell already at or above `level`'s depth reaches
/// nothing deeper.
pub(super) fn lower(t: &Type, level: Level) {
    if level.is_generic() {
        return;
    }
    ensure_sufficient(|| match t {
        Type::TVar(tv) if !tv.level().is_generic() => tv.claim(level),
        Type::TVar(_) => (),
        t => t.for_each_child(&mut |c| lower(c, level)),
    })
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
    // CR claude for claude: [risk/low] `bottom_required` only means something when
    // `binding` is `Some(Bottom)`, and two writers of a cell drop it. The session image
    // (`cell_encode`/`cell_decode` in image/mod.rs) encodes `cycle_refused`,
    // `bottom_fed` and `level` but not this flag. `TVar::freshen` carries the other two
    // flags into a copy but not this one. A restored or copied cell bound to ⊥ because
    // a `_` or `-> _` required it then reads as a vacuous ⊥, which
    // `Type::unbind_vacuous_tvars` reopens. Today that runs only at a definition's own
    // gate, so a warm start would type differently only if a later definition's
    // signature shared such a cell. Fold the flag into the binding, e.g. `enum Binding
    // { Open, Bound(Type), RequiredBottom }`, so the image codec and the copy walk have
    // to handle it and `unbind_vacuous_tvars` matches on it. At minimum, encode it and
    // copy it beside `bottom_fed`. (x-invalid-states-04)
    // 2026-10-14 claude: the image and the copy now carry it (CellFact, with
    // t-tvar-09 and t-image-04; pins image::decode_tests::a_cell_keeps_its_facts,
    // typ::tvar::tests::a_copy_keeps_its_cells_facts). A BottomRequired beside a
    // binding other than ⊥ is still representable.
    BottomRequired,
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
fn require_bounds(cons: &[Type], t: &Type) {
    for c in cons {
        match c {
            Type::Concrete => t.require_concrete(),
            Type::Singleton => t.require_singleton(),
            Type::OneNumber => t.require_one_number(),
            Type::Discernible | Type::Ordered => t.require_compared(c),
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
                    written_of(s_cell.read().task, "cell");
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
            let (s_addr, o_addr) =
                (Arc::as_ptr(&s_cell).addr(), Arc::as_ptr(&o_cell).addr());
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
            if !decided_of(s_task, "cell")
                || (!to_add.is_empty() && !decided_of(o_task, "cell"))
                || !decided_of(var_task, "var")
            {
                return;
            }
        }
        let mut s = self.write();
        if same && how == Merge::Name && (!s.frozen || s.id != oid) {
            written_of(s.task, "var");
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
                Arc::as_ptr(&s_cell).addr(),
                other.name,
                Arc::as_ptr(&o_cell).addr()
            );
        }
        let level = s_cell.read().level;
        {
            let mut oc = o_cell.write();
            oc.constraints.extend(to_add.drain(..));
            if !earlier_task(&oc) {
                oc.facts |= facts;
            } else if !oc.facts.contains(facts) {
                written_of(oc.task, "cell");
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
        // CR claude for claude: [risk/low] `lower` skips a generic cell, which is right
        // for a binding (a scheme bound into a cell is still a scheme). A merge is
        // different: it welds two cells into one, and the survivor must take the
        // shallower level. When `other` is generic (a pattern-predicate cell, a frozen
        // annotation var, a let-rec placeholder, a trait quantifier) and self's cell is
        // Top or Def, the merged cell stays generic. Level::copied_by then copies it at
        // every call, making polymorphic a cell the environment owns, and the value
        // restriction no longer holds for it. No route to it is known: the direction
        // rules usually keep the generic side as the abandoned cell. When `level` is
        // not generic, call `other.claim(level)` here (claim lowers generic cells too)
        // instead of `lower`. (t-tvar-07)
        // 2026-10-14 claude: tried `other.claim(level)` here; it breaks
        // lang::traits::core_display_nested_same_tag (`impl<'a> Display for Box<'a>`
        // over Box(Box(Box(1))) prints "<Box(Box(1))>"): the impl's quantifier welds
        // with the method's parameter cell and must stay generic for each call's
        // dispatch. A rule has to tell that weld from an environment-owned one.
        lower(&Type::TVar(other.clone()), level);
    }

    pub fn freeze(&self) {
        let mut l = self.write();
        if !l.frozen && !decided_of(l.task, "var") {
            return;
        }
        l.frozen = true;
    }

    /// Whether the cell holds the `Concrete` conjunct.
    #[doc(hidden)]
    pub fn requires_concrete(&self) -> bool {
        self.cell().read().constraints.iter().any(|c| matches!(c, Type::Concrete))
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
        if would_cycle_inner(Arc::as_ptr(&s_cell).addr(), &Type::TVar(other.clone())) {
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
                Arc::as_ptr(&s_cell).addr(),
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

    /// Clear the binding; the constraints stay.
    pub fn unbind(&self) {
        let cell = self.cell();
        let mut c = cell.write();
        if c.binding.is_some() && !decided(&c) {
            return;
        }
        c.binding = None
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
    pub(super) fn is_quantifier_rigid(&self) -> bool {
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
    /// [`TCell::bottom_required`].
    pub(super) fn bind_bottom_required(&self) {
        self.bind(Type::Bottom);
        let cell = self.cell();
        let mut c = cell.write();
        if decided(&c) {
            c.facts |= CellFact::BottomRequired;
        }
    }

    /// Whether a ⊥ was produced into the cell while it was open: a
    /// binding over ⊥ that took its type from its first use.
    #[doc(hidden)]
    pub fn bottom_fed(&self) -> bool {
        self.cell().read().facts.contains(CellFact::BottomFed)
    }

    /// Record a ⊥ produced into the open cell; see [`TCell::bottom_fed`].
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

    /// Record an occurs-check refusal; see [`TCell::cycle_refused`].
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
        Arc::as_ptr(&self.read().cell).addr()
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
        Ok(parts.known <= 1 && !parts.rigid_apart())
    }

    /// Whether `OneNumber ⊇ self` holds as the type stands: at most one
    /// numeric type among its members, read as [`Self::singleton_holds`]
    /// reads them.
    pub(crate) fn one_number_holds(&self, env: &Env, commit: bool) -> Result<bool> {
        let mut parts = UnionParts::new(Measure::Numbers);
        self.union_parts(Some((env, commit)), &mut parts)?;
        Ok(parts.known <= 1)
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
    pub(crate) fn require_singleton(&self) {
        let mut parts = UnionParts::new(Measure::Members);
        if self.union_parts(None, &mut parts).is_err() {
            return;
        }
        parts.merge_open();
        if let Some(narrow) = parts.narrowing() {
            for tv in parts.open.drain(..) {
                tv.add_cell_constraint(narrow.clone())
            }
        }
    }

    /// The open members of a `OneNumber` cell's binding are each
    /// `OneNumber` where no member is numeric yet; beside a numeric
    /// member, one may still bind another numeric type (no upper bound
    /// says "not numeric, or this number").
    pub(crate) fn require_one_number(&self) {
        let mut parts = UnionParts::new(Measure::Numbers);
        // CR claude for eric: [bug/medium] When the binding already has a numeric
        // member, the open members get no conjunct at all. So `'a: [Number, null] +
        // OneNumber` bound to `[i64, 'y]` leaves 'y free. A generic caller (`let g =
        // |b, y| f(select b { true => 1, false => y })`) then generalizes with an
        // unconstrained 'y, and `g(false, 2.5)` passes the check. The build refuses it:
        // the instance of f at fn(x: [i64, f64]) is refused. So --check and the LSP
        // accept what elaboration refuses, which CLAUDE.md calls a type-system bug
        // (graphix-fuzz check reports the DIVERGENCE). A statement-settle recheck is
        // not enough, because 'y is still open when g settles: the constraint has to
        // reach g's signature. Options: narrow the open members to the known numeric
        // member plus the non-numeric types (needs a way to say "not a number"), refuse
        // such a binding at the definition, or let a bound name a compound type. Needs
        // a rule. Probe: design/review-2026-10-12/repro/t-tvar-01.gx (t-tvar-01)
        if self.union_parts(None, &mut parts).is_err() || parts.known > 0 {
            return;
        }
        for tv in parts.open.drain(..) {
            tv.add_cell_constraint(Type::OneNumber)
        }
    }

    /// [`Self::require_singleton`] for an operand whose type must be one
    /// type: an open cell's existing conjuncts may already say so.
    pub fn narrow_singleton(&self, env: &Env) -> Result<()> {
        let mut parts = UnionParts::new(Measure::Members);
        self.union_parts(Some((env, false)), &mut parts)?;
        if parts.rigid_apart() {
            bail!(
                "{} must be one type, but its type variables may differ",
                self.resolve_tvars()
            )
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
        ensure_sufficient(|| self.take_row_inner(row))
    }

    fn take_row_inner(&self, row: &Type) -> bool {
        let each = |a: &[Type], b: &[Type]| match a.len() == b.len() {
            true => a.iter().zip(b).fold(true, |ok, (a, b)| a.take_row(b) & ok),
            false => !self.has_unbound(),
        };
        match (self, row) {
            (Type::TVar(tv), row) => match tv.binding() {
                Some(b) => b.take_row(row),
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
                Some(b) => t.take_row(&b),
                None => !t.has_unbound(),
            },
            (Type::Set(a), Type::Set(b)) | (Type::Tuple(a), Type::Tuple(b)) => each(a, b),
            (Type::Variant(ta, a, _), Type::Variant(tb, b, _)) if ta == tb => each(a, b),
            (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::Error(a), Type::Error(b)) => a.take_row(b),
            (Type::ByRef(m0, a), Type::ByRef(m1, b)) if m0 == m1 => a.take_row(b),
            (Type::Struct(a), Type::Struct(b))
                if a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(a, b)| a.0 == b.0) =>
            {
                a.iter()
                    .zip(b.iter())
                    .fold(true, |ok, ((_, a, _), (_, b, _))| a.take_row(b) & ok)
            }
            (Type::Map { key: ka, value: va }, Type::Map { key: kb, value: vb }) => {
                ka.take_row(kb) & va.take_row(vb)
            }
            (
                Type::Abstract { id: ia, params: a },
                Type::Abstract { id: ib, params: b },
            ) if ia == ib => each(a, b),
            (Type::Ref(a), Type::Ref(b)) if a.name == b.name => {
                each(&a.params, &b.params)
            }
            (t, _) => !t.has_unbound(),
        }
    }

    /// Whether `self` is concrete now: no open cell, and `Concrete ⊇ self`
    /// holds. The type a run-time read may cast to.
    pub fn is_concrete(&self) -> bool {
        !self.has_unbound() && self.concrete_holds()
    }

    /// Whether `Concrete ⊇ self` holds as the type stands: no ⊥ and no
    /// reference anywhere (a value read from data can't be one), bound
    /// cells judged by their bindings, open cells admitted (a bind hands
    /// them the conjunct).
    pub(crate) fn concrete_holds(&self) -> bool {
        ensure_sufficient(|| match self {
            // CR claude for eric: [bug/low] The Type::Fn arm below walks the signature
            // with for_each_part, throws slot included. A fn type that throws nothing
            // has `throws: ⊥`, and this arm reads that as a ⊥ value. So `fn(x: i64) ->
            // i64` fails Concrete (a type-directed read is refused with "'b: unbound
            // within Concrete"), while `fn(x: i64) -> i64 throws `E` holds and the read
            // fails at run time with InvalidCast "not a function" (probe:
            // design/review-2026-10-12/repro/t-tvar-10.gx). Every Concrete target in
            // the stdlib is a data read, and data cannot hold a function, just as it
            // cannot hold a reference. The suggested rule is that Concrete refuses
            // every Type::Fn, in concrete_holds and require_concrete together, with the
            // must-reject family updated to match. The alternative, skipping the throws
            // slot, admits targets that can only fail at run time. (t-tvar-10)
            Type::Bottom | Type::ByRef(..) => false,
            Type::TVar(tv) => tv.binding().is_none_or(|b| b.concrete_holds()),
            Type::Fn(ft) => {
                let mut holds = true;
                ft.for_each_part(&mut |t, _| holds &= t.concrete_holds());
                holds
            }
            t => {
                let mut holds = true;
                t.for_each_child(&mut |c| holds &= c.concrete_holds());
                holds
            }
        })
    }

    /// Hand the `Concrete` conjunct to every open cell the type reaches:
    /// a concrete cell's binding is concrete all the way down.
    pub(crate) fn require_concrete(&self) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.require_concrete(),
                None => tv.add_cell_constraint(Type::Concrete),
            },
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| t.require_concrete()),
            t => t.for_each_child(&mut |c| c.require_concrete()),
        })
    }

    /// Every type variable the structure holds, bindings not entered.
    pub(crate) fn tvar_occurrences(&self, out: &mut Vec<TVar>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => out.push(tv.clone()),
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| t.tvar_occurrences(out)),
            t => t.for_each_child(&mut |c| c.tvar_occurrences(out)),
        })
    }

    /// Alias type variables with the same name to each other.
    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(e) => {
                    let v = e.get();
                    v.freeze();
                    tv.alias(v);
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

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
            }
            // a nested fn type's own quantifiers are its own, whatever an
            // enclosing signature names alike
            Type::Fn(ft) if !ft.quantifiers.is_empty() => {
                let mut inner: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                ft.collect_tvars(&mut inner);
                for (name, tv) in inner.drain() {
                    if !ft.quantifiers.contains(&name) {
                        known.entry(name).or_insert(tv);
                    }
                }
            }
            Type::Fn(ft) => ft.collect_tvars(known),
            t => t.for_each_child(&mut |c| c.collect_tvars(known)),
        })
    }

    /// [`Self::collect_tvars`] over positions alone: a nested signature's
    /// cells by its own positions, never its conjuncts, so the walk is
    /// linear in the type.
    pub(crate) fn collect_position_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
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
            t => t.for_each_child(&mut |c| c.collect_position_tvars(known)),
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
    pub(crate) fn has_free(&self) -> bool {
        self.open_beneath(false)
    }

    fn open_beneath(&self, rigid: bool) -> bool {
        fn go(t: &Type, rigid: bool, seen: &mut IntSet<usize>) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    seen.insert(tv.cell_addr())
                        && match tv.binding() {
                            Some(b) => go(&b, rigid, seen),
                            None => rigid || !tv.is_rigid(),
                        }
                }
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
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                Some(Type::TVar(
                    tv.freshen(known, &mut (), how, |t, k, ()| t.reset_tvars_int(k, how)),
                ))
            }
            // `cow_children` rebuilds a Ref through `with_params`, which
            // shares the resolution cell; commit copies rely on that.
            // A nested fn type is always fresh: its `lambda_ids` cell
            // must be the instance's own.
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.reset_tvars_int(known, how)))),
            t => t.cow_children(&mut |c| c.reset_tvars_int(known, how)),
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
        self.instantiate_int(known, open).unwrap_or_else(|| self.clone())
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

    fn rename_int(&self, known: &AHashMap<usize, TVar>) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(t) => Some(t.rename_int(known).unwrap_or(t)),
                None => known.get(&tv.cell_addr()).map(|f| Type::TVar(f.clone())),
            },
            Type::Fn(ft) => {
                ft.cow_walk(|t| t.rename_int(known)).map(|f| Type::Fn(Arc::new(f)))
            }
            t => t.cow_children(&mut |c| c.rename_int(known)),
        })
    }

    /// `None` when no TVar is beneath.
    pub(super) fn instantiate_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        open: &IntSet<LambdaId>,
    ) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => Some(match tv.binding() {
                Some(t) => t.instantiate_int(known, open).unwrap_or(t),
                None => Type::TVar(tv.freshen(
                    known,
                    &mut (),
                    Fresh::Instantiate(open),
                    |t, k, ()| t.instantiate_int(k, open),
                )),
            }),
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.instantiate_with(known, open)))),
            t => t.cow_children(&mut |c| c.instantiate_int(known, open)),
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
    let key = Arc::as_ptr(&cell) as usize;
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

/// A type read as a union ([`Type::union_parts`]): how many known members
/// it has by its measure, the first of them, and its open cells.
struct UnionParts {
    measure: Measure,
    known: usize,
    first: Option<Type>,
    open: SmallVec<[TVar; 2]>,
}

impl UnionParts {
    fn new(measure: Measure) -> Self {
        Self { measure, known: 0, first: None, open: SmallVec::new() }
    }

    /// Whether a rigid open member cannot be made one type with the
    /// rest: beside a known member, or beside another rigid cell.
    fn rigid_apart(&self) -> bool {
        let mut rigid = self.open.iter().filter(|tv| tv.is_rigid());
        match rigid.next() {
            None => false,
            Some(_) if self.known > 0 => true,
            Some(a) => rigid.any(|b| b.cell_addr() != a.cell_addr()),
        }
    }

    /// Beside no known member, the open members must be one type: merge
    /// them into one cell.
    fn merge_open(&self) {
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

    fn member(&mut self, t: &Type) {
        let n = match (self.measure, t) {
            (Measure::Members, Type::Primitive(p)) => p.len(),
            (Measure::Numbers, Type::Primitive(p)) => {
                (*p & netidx_value::Typ::number()).len()
            }
            (_, Type::Any) => 2,
            (Measure::Members, _) => 1,
            (Measure::Numbers, _) => 0,
        };
        self.known += n;
        if n > 0 && self.first.is_none() {
            self.first = Some(t.clone())
        }
    }
}
