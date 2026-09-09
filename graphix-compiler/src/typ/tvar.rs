use crate::{
    expr::ModPath,
    typ::{FnType, PRINT_FLAGS, PrintFlag, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use poolshark::local::LPooled;
use std::{
    cmp::{Eq, PartialEq},
    collections::hash_map::Entry,
    fmt::{self, Debug},
    hash::Hash,
    ops::{ControlFlow, Deref},
};
use triomphe::Arc;

atomic_id!(TVarId);

pub(super) fn would_cycle_inner(addr: usize, t: &Type) -> bool {
    use poolshark::local::LPooled;
    let mut seen: LPooled<nohash::IntSet<usize>> = LPooled::take();
    would_cycle_seen(addr, t, &mut seen)
}

// Conjunct graphs can be cyclic; a revisited cell adds no
// reachability, so it answers false.
fn would_cycle_seen(addr: usize, t: &Type, seen: &mut nohash::IntSet<usize>) -> bool {
    crate::stack::ensure_sufficient(|| would_cycle_seen_inner(addr, t, seen))
}

fn would_cycle_seen_inner(
    addr: usize,
    t: &Type,
    seen: &mut nohash::IntSet<usize>,
) -> bool {
    // `seen` is a true visited set holding both cell and composite
    // node addresses; the answer depends only on reachable leaves.
    let node = match t {
        Type::Set(a) | Type::Tuple(a) | Type::Variant(_, a) => {
            Some((**a).as_ptr().addr())
        }
        Type::Abstract { params: a, .. } => Some((**a).as_ptr().addr()),
        Type::Struct(a) => Some((**a).as_ptr().addr()),
        Type::Fn(f) => Some((&**f as *const FnType).addr()),
        Type::Array(a) | Type::List(a) | Type::Error(a) | Type::ByRef(a) => {
            Some((&**a as *const Type).addr())
        }
        // Map carries two Arcs, so its children dedup individually.
        Type::Map { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Primitive(_)
        | Type::Any
        | Type::Bottom
        | Type::Ref(_)
        | Type::TVar(_) => None,
    };
    if let Some(node) = node
        && !seen.insert(node)
    {
        return false;
    }
    match t {
        // Expansion reaches every cell a Ref's params hold; a typedef
        // body's free tvars are its params, so the params cover it.
        Type::Ref(r) => r.params.iter().any(|p| would_cycle_seen(addr, p, seen)),
        Type::TVar(t) => {
            Arc::as_ptr(&t.read().typ).addr() == addr || {
                let cell = t.read().typ.clone();
                if !seen.insert(Arc::as_ptr(&cell).addr()) {
                    return false;
                }
                let cell = cell.read();
                let in_bind = match &cell.typ {
                    None => false,
                    Some(t) => would_cycle_seen(addr, t, seen),
                };
                in_bind
                    || cell.constraints.iter().any(|c| would_cycle_seen(addr, c, seen))
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

/// The shared binding cell: aliased `TVar`s hold one `Arc` of this.
/// `constraints` is a conjunction — everything the cell is ever bound
/// to must be contained by every member (empty = unconstrained); each
/// bind site checks it where an `Env` exists.
#[derive(Debug, Default)]
pub struct TCell {
    pub(crate) typ: Option<Type>,
    pub(crate) constraints: smallvec::SmallVec<[Type; 1]>,
    /// An occurs check refused to bind or link this cell (the only
    /// solution was an infinite type). A flagged cell still open at
    /// the terminal settle must error rather than default to ⊥.
    pub(crate) cycle_refused: bool,
    /// Nonzero while a declared (named) lambda tvar is inside its def's
    /// body check: a rigid unbound cell never binds, so the body must
    /// be well-typed for arbitrary 'a.
    pub(crate) rigid_gates: u32,
}

impl TCell {
    fn bound(typ: Type) -> Self {
        TCell {
            typ: Some(typ),
            constraints: smallvec::SmallVec::new(),
            rigid_gates: 0,
            cycle_refused: false,
        }
    }

    /// Add `c` to the conjunction unless an equal member is present.
    /// The eq walk can reach this very cell, so this must not run
    /// under a held tvar/cell guard.
    pub(crate) fn add_constraint(&mut self, c: Type) {
        if !self.constraints.iter().any(|e| e == &c) {
            self.constraints.push(c)
        }
    }
}

#[derive(Debug)]
pub struct TVarInnerInner {
    pub(crate) id: TVarId,
    pub(crate) frozen: bool,
    pub(crate) typ: Arc<RwLock<TCell>>,
}

#[derive(Debug)]
pub struct TVarInner {
    pub name: ArcStr,
    pub(crate) typ: RwLock<TVarInnerInner>,
}

#[derive(Clone)]
pub struct TVar(std::mem::ManuallyDrop<Arc<TVarInner>>);

/// A cell's constraints hold types that hold cells, so teardown
/// recurses and must run inside the stack guard.
impl Drop for TVar {
    fn drop(&mut self) {
        crate::stack::ensure_sufficient(|| unsafe {
            std::mem::ManuallyDrop::drop(&mut self.0)
        })
    }
}

// Cycle-guarded: a cell's conjuncts can reach the cell itself.
impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static DEBUGGING: std::cell::RefCell<nohash::IntSet<usize>> =
                std::cell::RefCell::new(nohash::IntSet::default());
        }
        let addr = self.cell_addr();
        if !DEBUGGING.with_borrow_mut(|s| s.insert(addr)) {
            return f
                .debug_tuple("TVar")
                .field(&format_args!("'{}: …", self.name))
                .finish();
        }
        let r = f.debug_tuple("TVar").field(&self.0).finish();
        DEBUGGING.with_borrow_mut(|s| s.remove(&addr));
        r
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
            write!(f, "'{}", self.name)
        } else {
            // A cell can be reachable from its own constraints; a
            // revisit on the print stack elides. Contents are cloned
            // out before recursing (never recurse under the cell guard).
            thread_local! {
                static PRINTING: std::cell::RefCell<nohash::IntSet<usize>> =
                    std::cell::RefCell::new(nohash::IntSet::default());
            }
            let addr = self.cell_addr();
            if !PRINTING.with_borrow_mut(|s| s.insert(addr)) {
                return write!(f, "'{}: …", self.name);
            }
            let r = (|| {
                write!(f, "'{}: ", self.name)?;
                let (typ, cons) = {
                    let cell = self.read().typ.clone();
                    let cell = cell.read();
                    (cell.typ.clone(), cell.constraints.clone())
                };
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
        Self::empty_named(ArcStr::from(format_compact!("_{}", TVarId::new().0).as_str()))
    }
}

impl Deref for TVar {
    type Target = TVarInner;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl PartialEq for TVar {
    fn eq(&self, other: &Self) -> bool {
        let t0 = self.read();
        let t1 = other.read();
        Arc::ptr_eq(&t0.typ, &t1.typ) || {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            t0.typ == t1.typ
        }
    }
}

impl Eq for TVar {}

impl PartialOrd for TVar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let t0 = self.read();
        let t1 = other.read();
        if Arc::ptr_eq(&t0.typ, &t1.typ) {
            Some(std::cmp::Ordering::Equal)
        } else {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            t0.typ.partial_cmp(&t1.typ)
        }
    }
}

impl Ord for TVar {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let t0 = self.read();
        let t1 = other.read();
        if Arc::ptr_eq(&t0.typ, &t1.typ) {
            std::cmp::Ordering::Equal
        } else {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            t0.typ.cmp(&t1.typ)
        }
    }
}

impl std::hash::Hash for TVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let t = self.read();
        let inner = t.typ.read();
        inner.typ.hash(state);
    }
}

impl TVar {
    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        match Type::TVar(self.clone()).scope_refs(scope) {
            Type::TVar(tv) => tv,
            _ => unreachable!(),
        }
    }

    pub fn empty_named(name: ArcStr) -> Self {
        Self(std::mem::ManuallyDrop::new(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(TCell::default())),
            }),
        })))
    }

    pub fn named(name: ArcStr, typ: Type) -> Self {
        Self(std::mem::ManuallyDrop::new(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(TCell::bound(typ))),
            }),
        })))
    }

    /// Add a conjunct to this var's cell constraints (deduped).
    pub fn add_cell_constraint(&self, c: Type) {
        let cell = self.read().typ.clone();
        let existing = cell.read().constraints.clone();
        if !existing.iter().any(|e| e == &c) {
            cell.write().constraints.push(c);
        }
    }

    /// The cell's constraint conjunction (cloned out).
    pub fn cell_constraints(&self) -> smallvec::SmallVec<[Type; 1]> {
        self.read().typ.read().constraints.clone()
    }

    pub fn read<'a>(&'a self) -> RwLockReadGuard<'a, TVarInnerInner> {
        self.typ.read()
    }

    pub fn write<'a>(&'a self) -> RwLockWriteGuard<'a, TVarInnerInner> {
        self.typ.write()
    }

    /// Make self an alias for other; self's constraints merge into the
    /// shared cell.
    pub fn alias(&self, other: &Self) {
        // Occurs check: a merged cell reachable from its own contents
        // is an infinite type every later walk loops on. Skipping the
        // merge only keeps inference looser.
        {
            let self_addr = Arc::as_ptr(&self.read().typ).addr();
            let other_addr = Arc::as_ptr(&other.read().typ).addr();
            if self_addr != other_addr {
                if would_cycle_inner(self_addr, &Type::TVar(other.clone())) {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!(
                            "ALIAS-REFUSE-1 {}({:x}) -> {}({:x}): other reaches self; other bound={:?} cons={:?}",
                            self.name,
                            self_addr,
                            other.name,
                            other_addr,
                            other.read().typ.read().typ,
                            other.read().typ.read().constraints
                        );
                    }
                    self.mark_cycle_refused();
                    other.mark_cycle_refused();
                    return;
                }
                let scons = self.read().typ.read().constraints.clone();
                if scons.iter().any(|c| would_cycle_inner(other_addr, c)) {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!(
                            "ALIAS-REFUSE-2 {}({:x}) -> {}({:x}): self cons reach other; cons={scons:?}",
                            self.name, self_addr, other.name, other_addr
                        );
                    }
                    self.mark_cycle_refused();
                    other.mark_cycle_refused();
                    return;
                }
            }
        }
        // Dedup computed lock-free: the eq walk can re-enter these cells.
        let mut to_add = {
            let s_cell = self.read().typ.clone();
            let o_cell = other.read().typ.clone();
            if Arc::ptr_eq(&s_cell, &o_cell) {
                LPooled::take()
            } else {
                let mine = s_cell.read().constraints.clone();
                let theirs = o_cell.read().constraints.clone();
                let mut to_add: LPooled<Vec<Type>> = LPooled::take();
                for c in mine {
                    if !theirs.iter().any(|e| e == &c) && !to_add.iter().any(|e| e == &c)
                    {
                        to_add.push(c)
                    }
                }
                to_add
            }
        };
        let mut s = self.write();
        if !s.frozen {
            s.frozen = true;
            let o = other.read();
            s.id = o.id;
            if !Arc::ptr_eq(&s.typ, &o.typ) {
                {
                    let mut oc = o.typ.write();
                    for c in to_add.drain(..) {
                        oc.constraints.push(c);
                    }
                }
                // Forward-link the abandoned cell: other TVars may share
                // it and must follow the merge. The occurs check above
                // guarantees the link closes no cycle.
                {
                    let mut sc = s.typ.write();
                    if sc.typ.is_none() {
                        sc.typ = Some(Type::TVar(other.clone()));
                    }
                }
                s.typ = Arc::clone(&o.typ);
            }
        }
    }

    pub fn freeze(&self) {
        self.write().frozen = true;
    }

    /// Merge self's cell into other's for a unification-driven merge:
    /// bypasses the `frozen` gate [`Self::alias`] honors (frozen means
    /// name-aliasing already happened) but keeps its occurs checks.
    pub(super) fn alias_cells(&self, other: &Self) {
        {
            let self_addr = Arc::as_ptr(&self.read().typ).addr();
            let other_addr = Arc::as_ptr(&other.read().typ).addr();
            if self_addr == other_addr {
                return;
            }
            if would_cycle_inner(self_addr, &Type::TVar(other.clone())) {
                self.mark_cycle_refused();
                other.mark_cycle_refused();
                return;
            }
            let scons = self.read().typ.read().constraints.clone();
            if scons.iter().any(|c| would_cycle_inner(other_addr, c)) {
                self.mark_cycle_refused();
                other.mark_cycle_refused();
                return;
            }
        }
        let mut to_add = {
            let s_cell = self.read().typ.clone();
            let o_cell = other.read().typ.clone();
            if Arc::ptr_eq(&s_cell, &o_cell) {
                LPooled::take()
            } else {
                let mine = s_cell.read().constraints.clone();
                let theirs = o_cell.read().constraints.clone();
                let mut to_add: LPooled<Vec<Type>> = LPooled::take();
                for c in mine {
                    if !theirs.iter().any(|e| e == &c) && !to_add.iter().any(|e| e == &c)
                    {
                        to_add.push(c)
                    }
                }
                to_add
            }
        };
        let mut s = self.write();
        let o = other.read();
        if !Arc::ptr_eq(&s.typ, &o.typ) {
            if crate::dbgenv::graphix_dbg_bind() {
                eprintln!(
                    "CELL-MERGE '{}({:x}) <=> '{}({:x})",
                    self.name,
                    Arc::as_ptr(&s.typ).addr(),
                    other.name,
                    Arc::as_ptr(&o.typ).addr()
                );
            }
            {
                let mut oc = o.typ.write();
                for c in to_add.drain(..) {
                    oc.constraints.push(c);
                }
            }
            // Forward-link as in [`Self::alias`].
            {
                let mut sc = s.typ.write();
                if sc.typ.is_none() {
                    sc.typ = Some(Type::TVar(other.clone()));
                }
            }
            s.typ = Arc::clone(&o.typ);
        }
    }

    /// Copy self's binding from other, merging constraint lists.
    pub fn copy(&self, other: &Self) {
        // Occurs check as in [`Self::alias`].
        {
            let self_addr = Arc::as_ptr(&self.read().typ).addr();
            if would_cycle_inner(self_addr, &Type::TVar(other.clone())) {
                self.mark_cycle_refused();
                return;
            }
        }
        let s = self.read();
        let o = other.read();
        if Arc::ptr_eq(&s.typ, &o.typ) {
            return;
        }
        let (typ, ocons) = {
            let oc = o.typ.read();
            (oc.typ.clone(), oc.constraints.clone())
        };
        if crate::dbgenv::graphix_dbg_bind() {
            eprintln!(
                "COPY '{}({:x}) <= '{}: {:?}",
                self.name,
                Arc::as_ptr(&s.typ).addr(),
                other.name,
                typ
            );
        }
        if let Some(id) = crate::dbgenv::graphix_dbg_bind_bt_id() {
            if id == s.id.inner().to_string() {
                eprintln!(
                    "BT for write to '{}({}):\n{}",
                    self.name,
                    id,
                    std::backtrace::Backtrace::force_capture()
                );
            }
        }
        let existing = s.typ.read().constraints.clone();
        let mut to_add: LPooled<Vec<Type>> = LPooled::take();
        for c in ocons {
            if !existing.iter().any(|e| e == &c) && !to_add.iter().any(|e| e == &c) {
                to_add.push(c)
            }
        }
        let mut sc = s.typ.write();
        sc.typ = typ;
        for c in to_add.drain(..) {
            sc.constraints.push(c);
        }
    }

    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut super::normalize::NormCx::take())
    }

    pub(super) fn normalize_int(&self, cx: &mut super::normalize::NormCx) -> Self {
        // First visit only. Clone the binding out, normalize unlocked,
        // write back: the lock is non-reentrant.
        if cx.cells.insert(self.cell_addr()) {
            let bound = self.read().typ.read().typ.clone();
            if let Some(t) = bound
                && let Some(n) = t.normalize_int(cx)
            {
                self.read().typ.write().typ = Some(n);
            }
        }
        self.clone()
    }

    /// Clear the binding; the constraints stay.
    pub fn unbind(&self) {
        self.read().typ.write().typ = None
    }

    /// Mark this var's shared cell rigid; see [`TCell::rigid_gates`].
    pub fn set_rigid(&self) {
        self.read().typ.write().rigid_gates += 1
    }

    /// Clear one gate's rigidity claim.
    pub fn clear_rigid(&self) {
        let tv = self.read();
        let mut cell = tv.typ.write();
        cell.rigid_gates = cell.rigid_gates.saturating_sub(1);
    }

    pub(crate) fn is_rigid(&self) -> bool {
        self.read().typ.read().rigid_gates > 0
    }

    /// Record an occurs-check refusal; see [`TCell::cycle_refused`].
    pub(super) fn mark_cycle_refused(&self) {
        if crate::dbgenv::graphix_dbg_bind() {
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
        self.read().typ.write().cycle_refused = true;
    }

    pub(super) fn would_cycle(&self, t: &Type) -> bool {
        let addr = Arc::as_ptr(&self.read().typ).addr();
        would_cycle_inner(addr, t)
    }

    pub(super) fn addr(&self) -> usize {
        Arc::as_ptr(&self.0).addr()
    }

    pub(super) fn inner_addr(&self) -> usize {
        Arc::as_ptr(&self.read().typ).addr()
    }

    /// True iff both vars share one binding cell (aliases of each other).
    pub fn same_cell(&self, other: &Self) -> bool {
        self.inner_addr() == other.inner_addr()
    }

    /// Identity of the shared binding cell, for set membership tests.
    pub(crate) fn cell_addr(&self) -> usize {
        self.inner_addr()
    }
}

// Each walk below spells out only the arms whose policy differs from
// plain recursion (chiefly whether `Ref` params are walked); the rest
// routes through `Type::try_for_each_child` / `Type::cow_children`.
impl Type {
    pub fn unfreeze_tvars(&self) {
        match self {
            Type::TVar(tv) => tv.write().frozen = false,
            Type::Fn(ft) => ft.unfreeze_tvars(),
            t => t.for_each_child(&mut |c| c.unfreeze_tvars()),
        }
    }

    /// Alias type variables with the same name to each other.
    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
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
            Type::Fn(ft) => ft.alias_tvars(known),
            t => t.for_each_child(&mut |c| c.alias_tvars(known)),
        }
    }

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
            Type::TVar(tv) => {
                known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
            }
            Type::Fn(ft) => ft.collect_tvars(known),
            t => t.for_each_child(&mut |c| c.collect_tvars(known)),
        }
    }

    pub fn check_tvars_declared(&self, declared: &AHashSet<ArcStr>) -> Result<()> {
        match self {
            Type::TVar(tv) => {
                if !declared.contains(&tv.name) {
                    bail!("undeclared type variable '{}'", tv.name)
                } else {
                    Ok(())
                }
            }
            // Nested fn types quantify their own tvars.
            Type::Fn(_) => Ok(()),
            t => match t.try_for_each_child(&mut |c| match c
                .check_tvars_declared(declared)
            {
                Ok(()) => ControlFlow::Continue(()),
                Err(e) => ControlFlow::Break(e),
            }) {
                ControlFlow::Continue(()) => Ok(()),
                ControlFlow::Break(e) => Err(e),
            },
        }
    }

    pub fn has_unbound(&self) -> bool {
        match self {
            Type::TVar(tv) => tv.read().typ.read().typ.is_none(),
            // Ref params are walked: `Alias<'b>` with 'b unbound is open.
            t => t
                .try_for_each_child(&mut |c| {
                    if c.has_unbound() {
                        ControlFlow::Break(())
                    } else {
                        ControlFlow::Continue(())
                    }
                })
                .is_break(),
        }
    }

    /// Bind all unbound type variables to the specified type.
    pub fn bind_as(&self, t: &Self) {
        match self {
            Type::TVar(tv) => {
                let tv = tv.read();
                let mut tv = tv.typ.write();
                // A rigid cell is an enclosing def's declared
                // universal, not a leftover.
                if tv.typ.is_none() && tv.rigid_gates == 0 {
                    tv.typ = Some(t.clone());
                }
            }
            Type::Ref(_) => (),
            s => s.for_each_child(&mut |c| c.bind_as(t)),
        }
    }

    /// A copy of self with fresh type variable cells: unbound cells
    /// freshen unbound, a bound cell freshens to a fresh cell bound to
    /// the reset of its binding. self is not modified.
    pub fn reset_tvars(&self) -> Type {
        use poolshark::local::LPooled;
        self.reset_tvars_int(&mut LPooled::take()).unwrap_or_else(|| self.clone())
    }

    /// The freshening map is keyed by cell identity, not name, so an
    /// instance preserves the source's alias topology. `None` when no
    /// TVar is beneath.
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        match self {
            Type::TVar(tv) => Some({
                // The fresh cell carries the source's constraints.
                let addr = tv.cell_addr();
                if let Some(fresh) = known.get(&addr) {
                    return Some(Type::TVar(fresh.clone()));
                }
                let fresh = TVar::empty_named(tv.name.clone());
                known.insert(addr, fresh.clone());
                for c in tv.cell_constraints() {
                    let c = c.reset_tvars_int(known).unwrap_or_else(|| c.clone());
                    fresh.add_cell_constraint(c);
                }
                // A var whose only solution was infinite in the def is
                // infinite in every instance.
                if tv.read().typ.read().cycle_refused {
                    fresh.read().typ.write().cycle_refused = true;
                }
                // A bound source cell is a solved fact the fresh cell
                // must carry. Clone the binding out before recursing.
                let bound = tv.read().typ.read().typ.clone();
                if let Some(t) = bound {
                    let t = t.reset_tvars_int(known).unwrap_or(t);
                    fresh.read().typ.write().typ = Some(t);
                }
                Type::TVar(fresh)
            }),
            // `cow_children` rebuilds a Ref through `with_params`, which
            // shares the resolution cell; commit copies rely on that.
            // A nested fn type is always fresh: its `lambda_ids` cell
            // must be the instance's own.
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.reset_tvars_int(known)))),
            t => t.cow_children(&mut |c| c.reset_tvars_int(known)),
        }
    }

    /// A copy of self with every TVar named in `known` replaced by the
    /// corresponding type; other TVars become fresh uniquely named
    /// TVars. TVar-free structure is returned shared.
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        use poolshark::local::LPooled;
        self.replace_tvars_int(known, &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn replace_tvars_int(
        &self,
        known: &AHashMap<ArcStr, Self>,
        renamed: &mut AHashMap<ArcStr, TVar>,
    ) -> Option<Type> {
        match self {
            Type::TVar(tv) => Some(match known.get(&tv.name) {
                Some(t) => t.clone(),
                None => {
                    let fresh =
                        renamed.entry(tv.name.clone()).or_insert_with(TVar::default);
                    Type::TVar(fresh.clone())
                }
            }),
            t => t.cow_children(&mut |c| c.replace_tvars_int(known, renamed)),
        }
    }

    /// Unbind any bound tvars, but do not unalias them.
    pub(crate) fn unbind_tvars(&self) {
        match self {
            Type::TVar(tv) => tv.unbind(),
            // Sig-level Ref params are concrete or rigid: nothing to unbind.
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_tvars()),
        }
    }

    /// [`Self::unbind_tvars`], except a cell whose binding is fully
    /// closed stays bound: a closed def-body inference is a solved
    /// fact. Partial bindings snapshot mid-solve state and still unbind.
    pub(crate) fn unbind_open_tvars(&self) {
        match self {
            Type::TVar(tv) => {
                // Bottom is a vacuous fact (`throws := ⊥` means the
                // body observed nothing); Any is not.
                let bound = tv.read().typ.read().typ.clone();
                if let Some(t) = bound
                    && (t == Type::Bottom || t.resolve_tvars().has_unbound())
                {
                    // A partial inference is still a fact about shape;
                    // it survives as a constraint. Bottom bounds nothing.
                    if t != Type::Bottom {
                        tv.add_cell_constraint(t);
                    }
                    tv.unbind()
                }
            }
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_open_tvars()),
        }
    }
}
