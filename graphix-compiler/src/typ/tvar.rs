use crate::{
    expr::ModPath,
    typ::{FnType, PRINT_FLAGS, PrintFlag, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
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

// `seen` holds cell addrs already on the walk: conjunct graphs can be
// CYCLIC (phase C parser seeding aliases quantifier names across
// nested fn types), and an unguarded walk looped cell → conjunct →
// same cell forever whenever the target addr wasn't in the cycle. A
// revisited cell adds no reachability, so it answers false.
fn would_cycle_seen(addr: usize, t: &Type, seen: &mut nohash::IntSet<usize>) -> bool {
    // The query is a pure existence check over an immutable snapshot, so
    // `seen` is a true visited set (never removed): a fully-explored
    // subtree that didn't contain `addr` never needs re-exploring. It
    // holds BOTH cell addresses and composite node addresses — the same
    // shared composite reached along many paths was re-scanned per path
    // (tree-cost over the DAG; part of the 2026-07-13 widget-type
    // compile blowup). Variant-blind address dedup is sound here: the
    // answer depends only on the leaves reachable from the allocation.
    let node = match t {
        Type::Set(a) | Type::Tuple(a) | Type::Variant(_, a) => {
            Some((**a).as_ptr().addr())
        }
        Type::Abstract { params: a, .. } => Some((**a).as_ptr().addr()),
        Type::Struct(a) => Some((**a).as_ptr().addr()),
        Type::Fn(f) => Some((&**f as *const FnType).addr()),
        Type::Array(a) | Type::Error(a) | Type::ByRef(a) => {
            Some((&**a as *const Type).addr())
        }
        // Map carries TWO Arcs; a single-address key could alias a
        // different (key, value) pairing and skip the value — its
        // children dedup individually instead.
        Type::Map { .. }
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
        // Ref PARAMS are skipped (preserved from the pre-walker code;
        // the Ref-arm policy survey is review A4/C8): the reachability
        // this check guards against is what the cell-graph walks
        // (normalize / Display / reset) traverse, and those never
        // expand a name. Fn signatures go through the default walker —
        // constraint TYPES live in the signature cells' conjunctions
        // (phase C), and the `TVar` arm walks each reachable cell's
        // conjuncts, so the component walk covers everything the
        // retired list walk reached.
        Type::Ref(_) => false,
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

/// The SHARED binding cell: aliased `TVar`s hold one `Arc` of this, so
/// the constraint travels with the binding — every alias sees, and
/// alias-time merging writes into, the same cell.
///
/// `constraints` is a CONJUNCTION: everything this cell is ever bound
/// to must be contained by EVERY member (empty = unconstrained).
/// Conjunction rather than a single intersected type keeps alias-time
/// merging infallible (list concatenation, no `Env` needed to resolve
/// Refs); each bind site checks the conjuncts where an `Env` exists,
/// so a genuinely unsatisfiable merge errors at the first bind with
/// the violated constraint named.
#[derive(Debug, Default)]
pub struct TCell {
    pub(crate) typ: Option<Type>,
    pub(crate) constraints: smallvec::SmallVec<[Type; 1]>,
    /// An occurs check refused to bind or link this cell: the type it
    /// was being equated with reaches the cell itself, so the only
    /// solution is an INFINITE type (`let rec f = |n, acc| f` — f's
    /// return type is f's own type). The refusal is silent at the
    /// bind site (later writers may still refine the cell), but a
    /// flagged cell that reaches the terminal settle still open and
    /// unconstrained must ERROR rather than default to ⊥ — the ⊥ is
    /// vacuous in unions, so the value's type collapses to the OTHER
    /// members and the kernel compares a Fn element's payload bits as
    /// a scalar (jul18c fleet divergence, both findings).
    pub(crate) cycle_refused: bool,
    /// RIGID: a DECLARED (user-annotated, named) lambda tvar during its
    /// def's body check. A rigid unbound cell never binds — the body
    /// must be well-typed for ARBITRARY 'a (within the constraints), so
    /// `contains('a, T)` is false for any concrete T and
    /// `contains(T, 'a)` holds only when T contains a constraint
    /// conjunct. Without this, the def check bound 'a to the body's
    /// concrete type, `unbind_tvars` discarded it, and every callsite
    /// re-instantiated 'a from its args alone — the signature became a
    /// lie the JIT trusted (soak jul09c, rigid_tvar_body_escape).
    /// Instantiation (`reset_tvars`) mints fresh cells, so callsite
    /// unification is unaffected. Anonymous `'_N` inference cells are
    /// never rigid — unannotated arg/rtype inference still binds.
    /// A COUNTER, not a bool: nested def gates can share a cell (an
    /// inner lambda annotated with the enclosing lambda's 'a), and the
    /// inner gate's exit must not un-rigidify the outer's still-open
    /// gate.
    pub(crate) rigid: u32,
}

impl TCell {
    fn bound(typ: Type) -> Self {
        TCell {
            typ: Some(typ),
            constraints: smallvec::SmallVec::new(),
            rigid: 0,
            cycle_refused: false,
        }
    }

    /// Add `c` to the conjunction unless an equal member is present.
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
pub struct TVar(Arc<TVarInner>);

// Manual, cycle-guarded: since cells became the constraint store
// (phase C) a cell's conjuncts can reach the cell itself (parser
// seeding aliases quantifier names across nested fn types), and the
// derived impl recursed cell → constraints → conjunct → same cell
// until the stack blew — first seen as proptest overflowing while
// REPORTING a failing case. Same shape as Display's PRINTING guard.
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
            write!(f, "'{}", self.name)
        } else {
            // Cycle guard: a cell can be reachable from its own
            // CONSTRAINTS (name-aliased fn-signature cells merge when
            // a polymorphic builtin is used as a first-class value —
            // soak jul06h), and the deref print recurses through both
            // the binding and the constraint list — printing a type
            // ERROR then overflowed the stack before any message got
            // out. Track the cells on the current print stack; a
            // revisit elides. Cell contents are cloned OUT before the
            // recursive writes (never recurse under the cell guard).
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
        // Mirror PartialEq: hash the inner Type. Same lock-pattern.
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
        Self(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(TCell::default())),
            }),
        }))
    }

    pub fn named(name: ArcStr, typ: Type) -> Self {
        Self(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(TCell::bound(typ))),
            }),
        }))
    }

    /// Add a conjunct to this var's CELL constraints (deduped).
    pub fn add_cell_constraint(&self, c: Type) {
        let cell = self.read().typ.clone();
        cell.write().add_constraint(c);
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

    /// make self an alias for other. Self's constraints MERGE into the
    /// now-shared cell (a conjunction — both vars' obligations apply to
    /// whatever the cell is ever bound to).
    pub fn alias(&self, other: &Self) {
        // Occurs check on the MERGE: if other's cell (through its
        // binding or constraints) reaches self's cell — or self's
        // constraints (about to move into other's cell) reach other's —
        // the merged cell becomes reachable from its own contents: an
        // infinite type that every later walk (contains deref,
        // normalize, resolve_tvars, Display) recurses on forever.
        // Cell merges had no such check while every BIND site did —
        // the cycle vector behind soak jul06h's compile wedge
        // (name-aliased fn-signature cells entangled by a polymorphic
        // builtin used as a first-class value). Skip the merge instead:
        // unlinked cells only keep inference looser, which at worst
        // rejects — never hangs.
        {
            let self_addr = Arc::as_ptr(&self.read().typ).addr();
            let other_addr = Arc::as_ptr(&other.read().typ).addr();
            if self_addr != other_addr {
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
        }
        let mut s = self.write();
        if !s.frozen {
            s.frozen = true;
            let o = other.read();
            s.id = o.id;
            if !Arc::ptr_eq(&s.typ, &o.typ) {
                let mine = s.typ.read().constraints.clone();
                {
                    let mut oc = o.typ.write();
                    for c in mine {
                        oc.add_constraint(c);
                    }
                }
                // FORWARD-LINK the abandoned cell before moving off it:
                // other TVar structs may share the old allocation, and
                // without the link they orphan — later facts fork
                // between the allocations and the orphan terminal-
                // settles ⊥ (jul17c katana divergence 000001: a rec
                // def's return-cell copy severed from the acc chain, so
                // the elem union flattened `[i64, ⊥]` → i64 and the
                // kernel compared a Fn element's payload bits as i64).
                // The occurs check above guarantees `other` doesn't
                // reach this cell, so the link can't close a cycle. A
                // cell that already holds a binding keeps it — the
                // link only rescues cells that would otherwise stay
                // open forever.
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

    /// Merge self's CELL into other's, bypassing the `frozen` gate
    /// that [`Self::alias`] honors — but keeping its occurs checks.
    /// `frozen` means "name-aliasing already happened for this var"
    /// (each var joins a name group once); a UNIFICATION-driven merge
    /// is a different act: two already-settled groups whose types are
    /// being equated must share one cell or later facts fork between
    /// them (the instantiation copy-skew family,
    /// site_recheck_strictness.md — the vacuous frozen×frozen
    /// unification was how `elem := null` and `cmp := i64` never met).
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
        let mut s = self.write();
        let o = other.read();
        if !Arc::ptr_eq(&s.typ, &o.typ) {
            if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
                eprintln!(
                    "CELL-MERGE '{}({:x}) <=> '{}({:x})",
                    self.name,
                    Arc::as_ptr(&s.typ).addr(),
                    other.name,
                    Arc::as_ptr(&o.typ).addr()
                );
            }
            let mine = s.typ.read().constraints.clone();
            {
                let mut oc = o.typ.write();
                for c in mine {
                    oc.add_constraint(c);
                }
            }
            // Same forward-link as [`Self::alias`] — sharers of the
            // abandoned cell must follow the merge.
            {
                let mut sc = s.typ.write();
                if sc.typ.is_none() {
                    sc.typ = Some(Type::TVar(other.clone()));
                }
            }
            s.typ = Arc::clone(&o.typ);
        }
    }

    /// copy self's binding from other, MERGING constraint lists (self
    /// keeps its own obligations — the bind-site check has already
    /// verified the incoming binding against them).
    pub fn copy(&self, other: &Self) {
        // Same occurs check as [`Self::alias`]: a binding that reaches
        // self's cell would make the cell cyclic. Skip rather than
        // hang every later walk.
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
        if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
            eprintln!(
                "COPY '{}({:x}) <= '{}: {:?}",
                self.name,
                Arc::as_ptr(&s.typ).addr(),
                other.name,
                typ
            );
        }
        if let Ok(id) = std::env::var("GRAPHIX_DBG_BIND_BT") {
            if id == s.id.inner().to_string() {
                eprintln!(
                    "BT for write to '{}({}):\n{}",
                    self.name,
                    id,
                    std::backtrace::Backtrace::force_capture()
                );
            }
        }
        let mut sc = s.typ.write();
        sc.typ = typ;
        for c in ocons {
            sc.add_constraint(c);
        }
    }

    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut super::normalize::NormCx::take())
    }

    pub(super) fn normalize_int(&self, cx: &mut super::normalize::NormCx) -> Self {
        // First visit only (`cx.cells` is keyed by cell address — the
        // walk reaches one cell along many paths through aliases and
        // shared subterms, and re-walking is exponential; see
        // `Type::normalize`). Clone the binding out, normalize
        // UNLOCKED, write back — never recurse under the cell's write
        // guard: parking_lot locks are non-reentrant, so recursing
        // under the guard deadlocked the whole compile (soak jul06h,
        // the other half of the same wedge).
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

    /// Clear the binding. The CONSTRAINTS stay — an unbound cell
    /// returns to constrained-unbound, not to unconstrained.
    pub fn unbind(&self) {
        self.read().typ.write().typ = None
    }

    /// Mark this var's shared cell RIGID — see [`TCell::rigid`]. Set on
    /// DECLARED (named) signature tvars at the lambda def gate; every
    /// same-named signature occurrence shares the cell via
    /// `alias_tvars`, so marking the representative marks them all.
    pub fn set_rigid(&self) {
        self.read().typ.write().rigid += 1
    }

    /// Clear one gate's rigidity claim when it exits — see `set_rigid`.
    pub fn clear_rigid(&self) {
        let tv = self.read();
        let mut cell = tv.typ.write();
        cell.rigid = cell.rigid.saturating_sub(1);
    }

    pub(crate) fn is_rigid(&self) -> bool {
        self.read().typ.read().rigid > 0
    }

    /// Record an occurs-check refusal on this cell — see
    /// [`TCell::cycle_refused`].
    pub(super) fn mark_cycle_refused(&self) {
        if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
            eprintln!("CYCLE-REFUSED '{}({:x})", self.name, self.cell_addr());
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

// The structural walks below are each written as their INTERESTING
// arms (TVar, plus any arm whose traversal policy differs from plain
// recursion — chiefly whether `Ref` params are walked), with all other
// structure routed through the shared child walkers
// (`Type::try_for_each_child` / `Type::cow_children`), so the
// exhaustive Type match lives in one place per walk kind. Every
// divergence from the default is an explicit override; the skips are
// preserved verbatim from the pre-walker code (the policy survey is
// review A4/C8, design/code_review_2026_07_19.md).
impl Type {
    pub fn unfreeze_tvars(&self) {
        match self {
            Type::TVar(tv) => tv.write().frozen = false,
            // FnType adds the guarded sig-cell constraint walk.
            Type::Fn(ft) => ft.unfreeze_tvars(),
            t => t.for_each_child(&mut |c| c.unfreeze_tvars()),
        }
    }

    /// alias type variables with the same name to each other
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
            // FnType adds the guarded sig-cell constraint walk.
            Type::Fn(ft) => ft.alias_tvars(known),
            t => t.for_each_child(&mut |c| c.alias_tvars(known)),
        }
    }

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
            Type::TVar(tv) => {
                known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
            }
            // FnType adds the guarded sig-cell constraint walk.
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
            // Nested fn types quantify their own tvars — not checked
            // against the enclosing declaration set.
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
            // Ref PARAMS are skipped (preserved from the pre-walker
            // code; review A4/C8): a `Ref` with an unbound param
            // answers FALSE. The consumers are the closedness tests
            // (`constrain_known`, `unbind_open_tvars`), where this
            // makes an inferred `Alias<'open>` binding read as closed.
            Type::Ref(_) => false,
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

    /// bind all unbound type variables to the specified type
    pub fn bind_as(&self, t: &Self) {
        match self {
            Type::TVar(tv) => {
                let tv = tv.read();
                let mut tv = tv.typ.write();
                // A RIGID cell is an ENCLOSING def's declared universal
                // reached through an inner binding (a nested lambda's
                // gate runs inside the outer's, and its constrain_known
                // closes leftover cells with `bind_as(Any)`). It is not
                // an unconstrained leftover — binding it here stomped
                // the outer contract to Any and the outer's rigid
                // rtype check then refused its own body.
                if tv.typ.is_none() && tv.rigid == 0 {
                    tv.typ = Some(t.clone());
                }
            }
            // Ref PARAMS are skipped (preserved; review A4/C8).
            Type::Ref(_) => (),
            s => s.for_each_child(&mut |c| c.bind_as(t)),
        }
    }

    /// Return a copy of self with fresh, unaliased type variable
    /// cells: unbound (quantified) cells freshen unbound, while a
    /// bound cell freshens to a fresh cell bound to the reset of its
    /// binding — solved def-body facts survive instantiation. self
    /// will not be modified.
    pub fn reset_tvars(&self) -> Type {
        use poolshark::local::LPooled;
        self.reset_tvars_int(&mut LPooled::take()).unwrap_or_else(|| self.clone())
    }

    /// The freshening map is keyed by CELL identity, not name, so an
    /// instance preserves the source's alias topology — `|a| a + a`
    /// shares one cell between `'a` and the rtype, and every instance
    /// must too (or a narrowing through one leaf silently stops
    /// reaching the others).
    ///
    /// `None` = no TVar anywhere beneath — the caller keeps the
    /// original (shared); TVar-free structure has nothing to freshen.
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        match self {
            Type::TVar(tv) => Some({
                // The fresh cell CARRIES the source's constraint
                // conjunction — instantiation freshens the binding
                // slot, never the obligations.
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
                // `cycle_refused` rides the copy: the instance preserves
                // the source's alias topology, so a var whose only
                // solution was infinite in the def is infinite in every
                // instance too. Without the carry the def-gate refusal
                // was recorded on the def cell while the INSTANCE cell
                // reached the terminal settle unflagged and ⊥-settled —
                // the exact lie this flag exists to reject (jul18c).
                if tv.read().typ.read().cycle_refused {
                    fresh.read().typ.write().cycle_refused = true;
                }
                // A BOUND source cell is a solved fact (def-body
                // inference — instances never write def cells, see the
                // #18 note in lambda.rs InitFn), not a quantified
                // variable: the fresh cell must carry the binding or
                // every instance forgets it. `|n: i64| error(f64:0.)`
                // binds `'a := f64` inside the def's rtype constraint;
                // resetting that to unbound left the site's rtype to
                // terminal-settle as ⊥ and fusion froze the return of
                // `[i64, Error<f64>]` as bare i64 — the error arm's
                // payload pointer marshalled as a scalar
                // (soak-jul06c B5). Clone the binding out before
                // recursing (lock discipline), through the same `known`
                // map so alias topology is preserved.
                let bound = tv.read().typ.read().typ.clone();
                if let Some(t) = bound {
                    let t = t.reset_tvars_int(known).unwrap_or(t);
                    fresh.read().typ.write().typ = Some(t);
                }
                Type::TVar(fresh)
            }),
            // `cow_children`'s Ref arm rebuilds through `with_params`
            // (SHARES the resolution cell) — load-bearing: expand_ref's
            // commit copies come through here and must keep their
            // seeded resolutions.
            t => t.cow_children(&mut |c| c.reset_tvars_int(known)),
        }
    }

    /// return a copy of self with every TVar named in known replaced
    /// with the corresponding type. TVars not in known are replaced with
    /// fresh TVars using unique names to avoid entanglement with the caller's
    /// TVars that happen to share the same name. TVar-free structure is
    /// returned SHARED, not copied.
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        use poolshark::local::LPooled;
        self.replace_tvars_int(known, &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` = no TVar anywhere beneath — the caller keeps the
    /// original (shared).
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
            // Ref PARAMS are skipped (preserved; review A4/C8) —
            // structural sig-level Ref params are concrete or
            // DECLARED (rigid-gated, so never bound during the def
            // check), leaving nothing to unbind in practice.
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_tvars()),
        }
    }

    /// [`Self::unbind_tvars`], except a cell whose binding is fully
    /// CLOSED (no unbound interior anywhere beneath, through bound
    /// cells — `constrain_known`'s closedness test, taken deep) stays
    /// BOUND. A closed def-body inference is a SOLVED fact: re-opening
    /// it downgraded the fact to an upper-bound cell constraint that
    /// the first consumer to unify could narrow below the def's actual
    /// delivery — first-writer-wins, with the winner decided by
    /// typecheck order and therefore by env contents (the select-union
    /// callback + push_front class: the shell rejected what the fuzz
    /// driver accepted, same build). Partial (open-interior) bindings
    /// still unbind — they snapshot mid-solve state an enclosing gate
    /// may revise.
    pub(crate) fn unbind_open_tvars(&self) {
        match self {
            Type::TVar(tv) => {
                // Bottom/Any are VACUOUS facts (`constrain_known`
                // skips them too): an inferred `throws := ⊥` means
                // only "this body observed nothing" — the declared
                // signature's 'e must stay a variable.
                let bound = tv.read().typ.read().typ.clone();
                if let Some(t) = bound
                    && (t == Type::Bottom
                        || t == Type::Any
                        || t.resolve_tvars().has_unbound())
                {
                    tv.unbind()
                }
            }
            // Ref PARAMS are skipped (preserved; review A4/C8) — same
            // rationale as `unbind_tvars`.
            Type::Ref(_) => (),
            t => t.for_each_child(&mut |c| c.unbind_open_tvars()),
        }
    }
}
