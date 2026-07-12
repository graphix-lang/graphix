use crate::{
    expr::ModPath,
    typ::{FnType, PRINT_FLAGS, PrintFlag, Type, TypeRef},
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
    ops::Deref,
};
use triomphe::Arc;

atomic_id!(TVarId);

pub(super) fn would_cycle_inner(addr: usize, t: &Type) -> bool {
    match t {
        Type::Primitive(_) | Type::Any | Type::Bottom | Type::Ref(TypeRef { .. }) => {
            false
        }
        Type::TVar(t) => {
            Arc::as_ptr(&t.read().typ).addr() == addr || {
                let cell = t.read().typ.clone();
                let cell = cell.read();
                let in_bind = match &cell.typ {
                    None => false,
                    Some(t) => would_cycle_inner(addr, t),
                };
                in_bind || cell.constraints.iter().any(|c| would_cycle_inner(addr, c))
            }
        }
        Type::Abstract { id: _, params } => {
            params.iter().any(|t| would_cycle_inner(addr, t))
        }
        Type::Error(t) => would_cycle_inner(addr, t),
        Type::Array(a) => would_cycle_inner(addr, &**a),
        Type::Map { key, value } => {
            would_cycle_inner(addr, &**key) || would_cycle_inner(addr, &**value)
        }
        Type::ByRef(t) => would_cycle_inner(addr, t),
        Type::Tuple(ts) => ts.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Variant(_, ts) => ts.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Struct(ts) => ts.iter().any(|(_, t)| would_cycle_inner(addr, t)),
        Type::Set(s) => s.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Fn(f) => {
            let FnType {
                args,
                vargs,
                rtype,
                constraints,
                throws,
                explicit_throws: _,
                lambda_ids: _,
            } = &**f;
            args.iter().any(|t| would_cycle_inner(addr, &t.typ))
                || match vargs {
                    None => false,
                    Some(t) => would_cycle_inner(addr, t),
                }
                || would_cycle_inner(addr, rtype)
                || constraints.read().iter().any(|a| {
                    Arc::as_ptr(&a.0.read().typ).addr() == addr
                        || would_cycle_inner(addr, &a.1)
                })
                || would_cycle_inner(addr, &throws)
        }
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
        TCell { typ: Some(typ), constraints: smallvec::SmallVec::new(), rigid: 0 }
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

#[derive(Debug, Clone)]
pub struct TVar(Arc<TVarInner>);

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
                    return;
                }
                let scons = self.read().typ.read().constraints.clone();
                if scons.iter().any(|c| would_cycle_inner(other_addr, c)) {
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
                s.typ = Arc::clone(&o.typ);
            }
        }
    }

    pub fn freeze(&self) {
        self.write().frozen = true;
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
        use poolshark::local::LPooled;
        self.normalize_int(&mut LPooled::take())
    }

    pub(super) fn normalize_int(&self, seen: &mut nohash::IntSet<usize>) -> Self {
        // First visit only (`seen` is keyed by cell address — the walk
        // reaches one cell along many paths through aliases and shared
        // subterms, and re-walking is exponential; see
        // `Type::normalize`). Clone the binding out, normalize
        // UNLOCKED, write back — never recurse under the cell's write
        // guard: parking_lot locks are non-reentrant, so recursing
        // under the guard deadlocked the whole compile (soak jul06h,
        // the other half of the same wedge).
        if seen.insert(self.cell_addr()) {
            let bound = self.read().typ.read().typ.clone();
            if let Some(t) = bound {
                let n = t.normalize_int(seen);
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

impl Type {
    pub fn unfreeze_tvars(&self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref(TypeRef { params, .. }) => {
                for t in params.iter() {
                    t.unfreeze_tvars();
                }
            }
            Type::Error(t) => t.unfreeze_tvars(),
            Type::Array(t) => t.unfreeze_tvars(),
            Type::Map { key, value } => {
                key.unfreeze_tvars();
                value.unfreeze_tvars();
            }
            Type::ByRef(t) => t.unfreeze_tvars(),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::TVar(tv) => tv.write().frozen = false,
            Type::Fn(ft) => ft.unfreeze_tvars(),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.unfreeze_tvars()
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.unfreeze_tvars()
                }
            }
        }
    }

    /// alias type variables with the same name to each other
    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref(TypeRef { params, .. }) => {
                for t in params.iter() {
                    t.alias_tvars(known);
                }
            }
            Type::Error(t) => t.alias_tvars(known),
            Type::Array(t) => t.alias_tvars(known),
            Type::Map { key, value } => {
                key.alias_tvars(known);
                value.alias_tvars(known);
            }
            Type::ByRef(t) => t.alias_tvars(known),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(e) => {
                    let v = e.get();
                    v.freeze();
                    tv.alias(v);
                }
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                    ()
                }
            },
            Type::Fn(ft) => ft.alias_tvars(known),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.alias_tvars(known)
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.alias_tvars(known)
                }
            }
        }
    }

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref(TypeRef { params, .. }) => {
                for t in params.iter() {
                    t.collect_tvars(known);
                }
            }
            Type::Error(t) => t.collect_tvars(known),
            Type::Array(t) => t.collect_tvars(known),
            Type::Map { key, value } => {
                key.collect_tvars(known);
                value.collect_tvars(known);
            }
            Type::ByRef(t) => t.collect_tvars(known),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(_) => (),
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                    ()
                }
            },
            Type::Fn(ft) => ft.collect_tvars(known),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.collect_tvars(known)
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.collect_tvars(known)
                }
            }
        }
    }

    /// [`collect_tvars`](Self::collect_tvars), except NESTED `Fn`
    /// subtrees are skipped: yields the tvars this type quantifies
    /// DIRECTLY, excluding a fn-typed member's own quantifiers. The
    /// def gate uses it to scope promotion obligations to the def's
    /// own declared tvars — a body operation on a PARAM's quantified
    /// cell (`|f: fn<'a>(x: 'a) -> 'a, y| f(y) + 1`) must not
    /// constrain the param's 'a (the 5634fbdc poisoned-conjunct
    /// class).
    pub(crate) fn collect_tvars_no_fn(&self, known: &mut AHashMap<ArcStr, TVar>) {
        match self {
            Type::Fn(_) => (),
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref(TypeRef { params, .. }) => {
                for t in params.iter() {
                    t.collect_tvars_no_fn(known);
                }
            }
            Type::Error(t) | Type::Array(t) | Type::ByRef(t) => {
                t.collect_tvars_no_fn(known)
            }
            Type::Map { key, value } => {
                key.collect_tvars_no_fn(known);
                value.collect_tvars_no_fn(known);
            }
            Type::Tuple(ts) | Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.collect_tvars_no_fn(known)
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.collect_tvars_no_fn(known)
                }
            }
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(_) => (),
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                }
            },
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.collect_tvars_no_fn(known)
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.collect_tvars_no_fn(known)
                }
            }
        }
    }

    pub fn check_tvars_declared(&self, declared: &AHashSet<ArcStr>) -> Result<()> {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => Ok(()),
            Type::Ref(TypeRef { params, .. }) => {
                params.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Error(t) => t.check_tvars_declared(declared),
            Type::Array(t) => t.check_tvars_declared(declared),
            Type::Map { key, value } => {
                key.check_tvars_declared(declared)?;
                value.check_tvars_declared(declared)
            }
            Type::ByRef(t) => t.check_tvars_declared(declared),
            Type::Tuple(ts) => {
                ts.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Struct(ts) => {
                ts.iter().try_for_each(|(_, t)| t.check_tvars_declared(declared))
            }
            Type::Variant(_, ts) => {
                ts.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::TVar(tv) => {
                if !declared.contains(&tv.name) {
                    bail!("undeclared type variable '{}'", tv.name)
                } else {
                    Ok(())
                }
            }
            Type::Set(s) => s.iter().try_for_each(|t| t.check_tvars_declared(declared)),
            Type::Abstract { id: _, params } => {
                params.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Fn(_) => Ok(()),
        }
    }

    pub fn has_unbound(&self) -> bool {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => false,
            Type::Ref(TypeRef { .. }) => false,
            Type::Error(e) => e.has_unbound(),
            Type::Array(t0) => t0.has_unbound(),
            Type::Map { key, value } => key.has_unbound() || value.has_unbound(),
            Type::ByRef(t0) => t0.has_unbound(),
            Type::Tuple(ts) => ts.iter().any(|t| t.has_unbound()),
            Type::Struct(ts) => ts.iter().any(|(_, t)| t.has_unbound()),
            Type::Variant(_, ts) => ts.iter().any(|t| t.has_unbound()),
            Type::TVar(tv) => tv.read().typ.read().typ.is_none(),
            Type::Set(s) => s.iter().any(|t| t.has_unbound()),
            Type::Abstract { id: _, params } => params.iter().any(|t| t.has_unbound()),
            Type::Fn(ft) => ft.has_unbound(),
        }
    }

    /// bind all unbound type variables to the specified type
    pub fn bind_as(&self, t: &Self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref(TypeRef { .. }) => (),
            Type::Error(t0) => t0.bind_as(t),
            Type::Array(t0) => t0.bind_as(t),
            Type::Map { key, value } => {
                key.bind_as(t);
                value.bind_as(t);
            }
            Type::ByRef(t0) => t0.bind_as(t),
            Type::Tuple(ts) => {
                for elt in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Struct(ts) => {
                for (_, elt) in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Variant(_, ts) => {
                for elt in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::TVar(tv) => {
                let tv = tv.read();
                let mut tv = tv.typ.write();
                // A RIGID cell is an ENCLOSING def's declared universal
                // reached through an inner binding (a nested lambda's
                // gate runs inside the outer's, and its constrain_known
                // closes leftover cells with `bind_as(Any)`). It is not
                // an unconstrained leftover — binding it here stomped
                // the outer contract to Any and the outer's rigid
                // rtype check then refused its own body (sync-subset
                // P4, the in-language map).
                if tv.typ.is_none() && tv.rigid == 0 {
                    tv.typ = Some(t.clone());
                }
            }
            Type::Set(s) => {
                for elt in s.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Fn(ft) => ft.bind_as(t),
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.bind_as(t)
                }
            }
        }
    }

    /// Return a copy of self with fresh, unaliased type variable
    /// cells: unbound (quantified) cells freshen unbound, while a
    /// bound cell freshens to a fresh cell bound to the reset of its
    /// binding — solved def-body facts survive instantiation. self
    /// will not be modified.
    pub fn reset_tvars(&self) -> Type {
        use poolshark::local::LPooled;
        self.reset_tvars_int(&mut LPooled::take())
    }

    /// The freshening map is keyed by CELL identity, not name, so an
    /// instance preserves the source's alias topology — `|a| a + a`
    /// shares one cell between `'a` and the rtype, and every instance
    /// must too (or a narrowing through one leaf silently stops
    /// reaching the others).
    pub(super) fn reset_tvars_int(&self, known: &mut AHashMap<usize, TVar>) -> Type {
        match self {
            Type::Bottom => Type::Bottom,
            Type::Any => Type::Any,
            Type::Primitive(p) => Type::Primitive(*p),
            Type::Ref(TypeRef { scope, name, params, .. }) => Type::Ref(TypeRef {
                scope: scope.clone(),
                name: name.clone(),
                params: Arc::from_iter(params.iter().map(|t| t.reset_tvars_int(known))),
                ..Default::default()
            }),
            Type::Error(t0) => Type::Error(Arc::new(t0.reset_tvars_int(known))),
            Type::Array(t0) => Type::Array(Arc::new(t0.reset_tvars_int(known))),
            Type::Map { key, value } => {
                let key = Arc::new(key.reset_tvars_int(known));
                let value = Arc::new(value.reset_tvars_int(known));
                Type::Map { key, value }
            }
            Type::ByRef(t0) => Type::ByRef(Arc::new(t0.reset_tvars_int(known))),
            Type::Tuple(ts) => {
                Type::Tuple(Arc::from_iter(ts.iter().map(|t| t.reset_tvars_int(known))))
            }
            Type::Struct(ts) => Type::Struct(Arc::from_iter(
                ts.iter().map(|(n, t)| (n.clone(), t.reset_tvars_int(known))),
            )),
            Type::Variant(tag, ts) => Type::Variant(
                tag.clone(),
                Arc::from_iter(ts.iter().map(|t| t.reset_tvars_int(known))),
            ),
            Type::TVar(tv) => {
                // The fresh cell CARRIES the source's constraint
                // conjunction — instantiation freshens the binding
                // slot, never the obligations.
                let addr = tv.cell_addr();
                if let Some(fresh) = known.get(&addr) {
                    return Type::TVar(fresh.clone());
                }
                let fresh = TVar::empty_named(tv.name.clone());
                known.insert(addr, fresh.clone());
                for c in tv.cell_constraints() {
                    let c = c.reset_tvars_int(known);
                    fresh.add_cell_constraint(c);
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
                    let t = t.reset_tvars_int(known);
                    fresh.read().typ.write().typ = Some(t);
                }
                Type::TVar(fresh)
            }
            Type::Set(s) => {
                Type::Set(Arc::from_iter(s.iter().map(|t| t.reset_tvars_int(known))))
            }
            Type::Fn(fntyp) => Type::Fn(Arc::new(fntyp.reset_tvars_int(known))),
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(params.iter().map(|t| t.reset_tvars_int(known))),
            },
        }
    }

    /// return a copy of self with every TVar named in known replaced
    /// with the corresponding type. TVars not in known are replaced with
    /// fresh TVars using unique names to avoid entanglement with the caller's
    /// TVars that happen to share the same name.
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Self>) -> Type {
        use poolshark::local::LPooled;
        self.replace_tvars_int(known, &mut LPooled::take())
    }

    pub(super) fn replace_tvars_int(
        &self,
        known: &AHashMap<ArcStr, Self>,
        renamed: &mut AHashMap<ArcStr, TVar>,
    ) -> Type {
        match self {
            Type::TVar(tv) => match known.get(&tv.name) {
                Some(t) => t.clone(),
                None => {
                    let fresh =
                        renamed.entry(tv.name.clone()).or_insert_with(TVar::default);
                    Type::TVar(fresh.clone())
                }
            },
            Type::Bottom => Type::Bottom,
            Type::Any => Type::Any,
            Type::Primitive(p) => Type::Primitive(*p),
            Type::Ref(TypeRef { scope, name, params, .. }) => Type::Ref(TypeRef {
                scope: scope.clone(),
                name: name.clone(),
                params: Arc::from_iter(
                    params.iter().map(|t| t.replace_tvars_int(known, renamed)),
                ),
                ..Default::default()
            }),
            Type::Error(t0) => {
                Type::Error(Arc::new(t0.replace_tvars_int(known, renamed)))
            }
            Type::Array(t0) => {
                Type::Array(Arc::new(t0.replace_tvars_int(known, renamed)))
            }
            Type::Map { key, value } => {
                let key = Arc::new(key.replace_tvars_int(known, renamed));
                let value = Arc::new(value.replace_tvars_int(known, renamed));
                Type::Map { key, value }
            }
            Type::ByRef(t0) => {
                Type::ByRef(Arc::new(t0.replace_tvars_int(known, renamed)))
            }
            Type::Tuple(ts) => Type::Tuple(Arc::from_iter(
                ts.iter().map(|t| t.replace_tvars_int(known, renamed)),
            )),
            Type::Struct(ts) => Type::Struct(Arc::from_iter(
                ts.iter().map(|(n, t)| (n.clone(), t.replace_tvars_int(known, renamed))),
            )),
            Type::Variant(tag, ts) => Type::Variant(
                tag.clone(),
                Arc::from_iter(ts.iter().map(|t| t.replace_tvars_int(known, renamed))),
            ),
            Type::Set(s) => Type::Set(Arc::from_iter(
                s.iter().map(|t| t.replace_tvars_int(known, renamed)),
            )),
            Type::Fn(fntyp) => {
                Type::Fn(Arc::new(fntyp.replace_tvars_int(known, renamed)))
            }
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(
                    params.iter().map(|t| t.replace_tvars_int(known, renamed)),
                ),
            },
        }
    }

    /// Unbind any bound tvars, but do not unalias them.
    pub(crate) fn unbind_tvars(&self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) | Type::Ref(TypeRef { .. }) => {
                ()
            }
            Type::Error(t0) => t0.unbind_tvars(),
            Type::Array(t0) => t0.unbind_tvars(),
            Type::Map { key, value } => {
                key.unbind_tvars();
                value.unbind_tvars();
            }
            Type::ByRef(t0) => t0.unbind_tvars(),
            Type::Tuple(ts)
            | Type::Variant(_, ts)
            | Type::Set(ts)
            | Type::Abstract { id: _, params: ts } => {
                for t in ts.iter() {
                    t.unbind_tvars()
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.unbind_tvars()
                }
            }
            Type::TVar(tv) => tv.unbind(),
            Type::Fn(fntyp) => fntyp.unbind_tvars(),
        }
    }
}
