use crate::{
    PrintFlag,
    env::Env,
    format_with_flags,
    typ::{AndAc, FnType, RefHist, TVar, Type, TypeRef, tvar::would_cycle_inner},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::{BitFlags, bitflags};
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::fmt::Debug;
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum ContainsFlags {
    AliasTVars,
    InitTVars,
    /// Enforce rigid (declared) tvar semantics; see `TCell::rigid`.
    /// Set only on the def gate's acceptance checks — elsewhere rigid
    /// cells behave like ordinary unbound cells.
    RigidCheck,
}

/// The infinite-type rejection wording, shared by
/// [`TVar::settle_or_bottom`] and [`Type::contains_mismatch`].
pub(crate) const INFINITE_TYPE_MSG: &str = "cannot infer a finite type here: unification requires a type that \
     contains itself (e.g. a function that returns itself); declare a \
     named recursive type and annotate the binding";

/// Is `a` an open cell that `b` reaches (`'r ⊇ fn(..) -> 'r`)?
fn is_unbound_tvar(t: &Type) -> bool {
    matches!(t, Type::TVar(tv) if tv.read().typ.read().typ.is_none())
}

fn open_cell_reaches(a: &Type, b: &Type) -> bool {
    match a {
        Type::TVar(tv) => {
            let open = tv.read().typ.read().typ.is_none();
            open && tv.would_cycle(b)
        }
        _ => false,
    }
}

/// Does `t` reach a cell that is open, unconstrained and
/// `cycle_refused`? Pure read through bindings and constraints.
fn type_has_refused_open_cell(t: &Type) -> bool {
    fn walk(t: &Type, visited: &mut LPooled<nohash::IntSet<usize>>) -> bool {
        match t {
            Type::TVar(tv) => {
                if !visited.insert(tv.cell_addr()) {
                    return false;
                }
                let (bound, cons, refused) = {
                    let g = tv.read();
                    let cell = g.typ.read();
                    (cell.typ.clone(), cell.constraints.clone(), cell.cycle_refused)
                };
                if bound.is_none() && cons.is_empty() && refused {
                    return true;
                }
                if let Some(b) = &bound
                    && walk(b, visited)
                {
                    return true;
                }
                cons.iter().any(|c| walk(c, visited))
            }
            Type::Fn(ft) => {
                ft.args.iter().any(|a| walk(&a.typ, visited))
                    || ft.vargs.as_ref().is_some_and(|v| walk(v, visited))
                    || walk(&ft.rtype, visited)
                    || walk(&ft.throws, visited)
            }
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found |= walk(c, visited));
                found
            }
        }
    }
    walk(t, &mut LPooled::take())
}

/// True iff binding `t` into the cell would satisfy every conjunct of
/// the cell's constraints. A pure probe.
fn cell_constraints_ok(
    tv: &crate::typ::TVar,
    env: &Env,
    hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
    t: &Type,
) -> Result<bool> {
    let cons = tv.read().typ.read().constraints.clone();
    for c in cons.iter() {
        if !c.contains_int(BitFlags::empty(), env, hist, t)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Weld the tvar cells of two loosely-equal types by position: two
/// open cells that compared equal must share fate, or the discarded
/// side's later binding never reaches the survivor. The alias carries
/// the occurs check.
fn link_equal(t0: &Type, t1: &Type) {
    crate::stack::ensure_sufficient(|| link_equal_inner(t0, t1))
}

fn link_equal_inner(t0: &Type, t1: &Type) {
    match (t0, t1) {
        (Type::TVar(a), Type::TVar(b)) => {
            if a.same_cell(b) {
                return;
            }
            let ab = a.read().typ.read().typ.clone();
            let bb = b.read().typ.read().typ.clone();
            match (ab, bb) {
                (None, None) => {
                    let af = a.read().frozen;
                    let bf = b.read().frozen;
                    if af && bf {
                        a.alias_cells(b)
                    } else if af {
                        b.alias(a)
                    } else {
                        a.alias(b)
                    }
                }
                (Some(x), Some(y)) => link_equal(&x, &y),
                // Unreachable under an eq-true verdict.
                _ => (),
            }
        }
        (Type::Fn(f0), Type::Fn(f1)) => {
            for (a, b) in f0.args.iter().zip(f1.args.iter()) {
                link_equal(&a.typ, &b.typ);
            }
            if let (Some(a), Some(b)) = (&f0.vargs, &f1.vargs) {
                link_equal(a, b);
            }
            link_equal(&f0.rtype, &f1.rtype);
            link_equal(&f0.throws, &f1.throws);
        }
        (Type::Ref(r0), Type::Ref(r1)) => {
            for (a, b) in r0.params.iter().zip(r1.params.iter()) {
                link_equal(a, b);
            }
        }
        (Type::Set(a), Type::Set(b))
        | (Type::Tuple(a), Type::Tuple(b))
        | (Type::Variant(_, a), Type::Variant(_, b))
        | (Type::Abstract { params: a, .. }, Type::Abstract { params: b, .. }) => {
            for (x, y) in a.iter().zip(b.iter()) {
                link_equal(x, y);
            }
        }
        (Type::Struct(a), Type::Struct(b)) => {
            for ((_, x), (_, y)) in a.iter().zip(b.iter()) {
                link_equal(x, y);
            }
        }
        (Type::Array(a), Type::Array(b))
        | (Type::List(a), Type::List(b))
        | (Type::Error(a), Type::Error(b))
        | (Type::ByRef(a), Type::ByRef(b)) => link_equal(a, b),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            link_equal(k0, k1);
            link_equal(v0, v1);
        }
        (Type::App(c0, a0), Type::App(c1, a1)) => {
            link_equal(c0, c1);
            link_equal(a0, a1);
        }
        _ => (),
    }
}

impl crate::typ::TVar {
    /// Bind a constrained-unbound cell to its conjunction's witness,
    /// the narrowest conjunct every other conjunct contains. Bound and
    /// unconstrained cells are untouched. No witness is a type error.
    pub fn settle(&self, env: &Env) -> Result<()> {
        let cons = {
            let tv = self.read();
            let cell = tv.typ.read();
            if cell.typ.is_some() || cell.constraints.is_empty() {
                return Ok(());
            }
            cell.constraints.clone()
        };
        let mut hist = RefHist::new(LPooled::take());
        let mut witness = None;
        let addr = self.cell_addr();
        let mut all_self_referential = true;
        let mut has_trait = false;
        'cand: for c in cons.iter() {
            // A trait conjunct is a predicate, not a binding.
            if c.is_trait_ref(env) {
                has_trait = true;
                continue;
            }
            // A conjunct reaching this cell has no finite witness.
            if would_cycle_inner(addr, c) {
                continue;
            }
            all_self_referential = false;
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    continue 'cand;
                }
            }
            witness = Some(c.clone());
            break;
        }
        // No finite witness: leave the cell open for writers to refine.
        if witness.is_none() && (all_self_referential || has_trait) {
            return Ok(());
        }
        match witness {
            Some(w) => {
                // A private copy: binding the store's type verbatim
                // would alias its interior cells into live inference.
                let w = w.reset_tvars();
                if crate::dbgenv::graphix_dbg_bind() {
                    eprintln!("SETTLE '{}({:x}) := {w:?}", self.name, self.cell_addr());
                }
                self.read().typ.write().typ = Some(w);
                Ok(())
            }
            None => {
                format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                    let mut cs: LPooled<String> = LPooled::take();
                    for (i, c) in cons.iter().enumerate() {
                        use std::fmt::Write;
                        if i > 0 {
                            cs.push_str(" & ");
                        }
                        write!(cs, "{c}")?;
                    }
                    bail!("unsatisfiable constraints on '{}: {}", self.name, &*cs)
                })
            }
        }
    }

    /// [`Self::settle`], but an unconstrained unbound cell binds to ⊥:
    /// nothing produced or bounded it. Only the terminal walk uses this
    /// (an earlier call would foreclose writers not yet typechecked).
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        {
            let tv = self.read();
            let cell = tv.typ.read();
            if cell.typ.is_some() {
                return Ok(());
            }
            if cell.constraints.is_empty() {
                // The only solution was infinite; ⊥ would be a lie.
                if cell.cycle_refused {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!(
                            "SETTLE-INFINITE '{}({:x})",
                            self.name,
                            self.cell_addr()
                        );
                    }
                    bail!("{INFINITE_TYPE_MSG}")
                }
                drop(cell);
                if crate::dbgenv::graphix_dbg_bind() {
                    eprintln!("SETTLE-BOTTOM '{}({:x})", self.name, self.cell_addr());
                }
                if crate::dbgenv::graphix_dbg_bind_bt() {
                    eprintln!("{}", std::backtrace::Backtrace::force_capture());
                }
                tv.typ.write().typ = Some(Type::Bottom);
                return Ok(());
            }
        }
        self.settle(env)
    }
}

/// Record which settle-set members `t` references, descending through
/// non-member cells' bindings and constraints but stopping at members
/// (their reach is their own edge list).
fn settle_refs(
    t: &Type,
    index: &AHashMap<usize, usize>,
    visited: &mut AHashSet<usize>,
    out: &mut SmallVec<[usize; 4]>,
) {
    match t {
        Type::TVar(tv) => {
            let addr = tv.cell_addr();
            if let Some(&i) = index.get(&addr) {
                out.push(i);
            } else if visited.insert(addr) {
                let (bound, cons) = {
                    let g = tv.read();
                    let cell = g.typ.read();
                    (cell.typ.clone(), cell.constraints.clone())
                };
                if let Some(b) = &bound {
                    settle_refs(b, index, visited, out);
                }
                for c in cons.iter() {
                    settle_refs(c, index, visited, out);
                }
            }
        }
        Type::Fn(ft) => {
            for arg in ft.args.iter() {
                settle_refs(&arg.typ, index, visited, out);
            }
            if let Some(vargs) = &ft.vargs {
                settle_refs(vargs, index, visited, out);
            }
            settle_refs(&ft.rtype, index, visited, out);
            ft.for_each_sig_constraint(&mut |c| settle_refs(c, index, visited, out));
            settle_refs(&ft.throws, index, visited, out);
        }
        t => t.for_each_child(&mut |c| settle_refs(c, index, visited, out)),
    }
}

impl FnType {
    /// Terminal settle of a call site's resolved signature in
    /// dependency order: a member settles only after every member its
    /// binding or constraints reach. Ordering keys are (name, TVarId)
    /// only — `cell_addr` is ASLR-dependent and used for identity alone.
    /// `defaulted` cells are ordered but not settled.
    pub fn settle_terminal(
        &self,
        env: &Env,
        rtype_cell: Option<&TVar>,
        defaulted: &AHashSet<usize>,
    ) -> Result<()> {
        let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut tvs);
        let mut nodes: LPooled<Vec<(ArcStr, TVar)>> = tvs.drain().collect();
        if let Some(tv) = rtype_cell {
            if !nodes.iter().any(|(_, n)| n.cell_addr() == tv.cell_addr()) {
                nodes.push((tv.name.clone(), tv.clone()));
            }
        }
        nodes.sort_by(|a, b| {
            a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id))
        });
        let mut index: LPooled<AHashMap<usize, usize>> = LPooled::take();
        for (i, (_, tv)) in nodes.iter().enumerate() {
            index.insert(tv.cell_addr(), i);
        }
        let mut edges: LPooled<Vec<SmallVec<[usize; 4]>>> = LPooled::take();
        for (_, tv) in nodes.iter() {
            let mut out: SmallVec<[usize; 4]> = SmallVec::new();
            let mut visited: LPooled<AHashSet<usize>> = LPooled::take();
            let (bound, cons) = {
                let g = tv.read();
                let cell = g.typ.read();
                (cell.typ.clone(), cell.constraints.clone())
            };
            if let Some(b) = &bound {
                settle_refs(b, &index, &mut visited, &mut out);
            }
            for c in cons.iter() {
                settle_refs(c, &index, &mut visited, &mut out);
            }
            out.sort_unstable();
            out.dedup();
            edges.push(out);
        }
        fn visit(
            i: usize,
            edges: &[SmallVec<[usize; 4]>],
            seen: &mut [bool],
            order: &mut LPooled<Vec<usize>>,
        ) {
            if seen[i] {
                return;
            }
            seen[i] = true;
            for &j in edges[i].iter() {
                visit(j, edges, seen, order);
            }
            order.push(i);
        }
        let mut seen: LPooled<Vec<bool>> = LPooled::take();
        seen.resize(nodes.len(), false);
        let mut order: LPooled<Vec<usize>> = LPooled::take();
        for i in 0..nodes.len() {
            visit(i, &edges, &mut seen, &mut order);
        }
        for i in order.drain(..) {
            let tv = &nodes[i].1;
            if !defaulted.contains(&tv.cell_addr()) {
                tv.settle_or_bottom(env)?;
            }
        }
        Ok(())
    }
}

/// A non-containment report that formats lazily: callers probe these
/// errors, and rendering a large type eagerly is tree-cost.
#[derive(Debug)]
pub struct TypeMismatch {
    expected: Type,
    actual: Type,
}

impl std::fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
            write!(f, "type mismatch {} does not contain {}", self.expected, self.actual)
        })
    }
}

impl std::error::Error for TypeMismatch {}

/// Both nodes wrap the same content allocation, so containment holds
/// reflexively. `TVar`/`Ref` keep their full arms: cheap and delicate.
fn same_content(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Set(x), Type::Set(y)) | (Type::Tuple(x), Type::Tuple(y)) => {
            (**x).as_ptr() == (**y).as_ptr()
        }
        (Type::Struct(x), Type::Struct(y)) => (**x).as_ptr() == (**y).as_ptr(),
        (Type::Variant(t0, x), Type::Variant(t1, y)) => {
            t0 == t1 && (**x).as_ptr() == (**y).as_ptr()
        }
        (Type::Fn(x), Type::Fn(y)) => Arc::ptr_eq(x, y),
        (Type::Array(x), Type::Array(y))
        | (Type::List(x), Type::List(y))
        | (Type::Error(x), Type::Error(y))
        | (Type::ByRef(x), Type::ByRef(y)) => Arc::ptr_eq(x, y),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            Arc::ptr_eq(k0, k1) && Arc::ptr_eq(v0, v1)
        }
        _ => false,
    }
}

impl Type {
    pub fn check_contains(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = RefHist::new(LPooled::take());
        let ok = self.contains_int(
            ContainsFlags::AliasTVars | ContainsFlags::InitTVars,
            env,
            &mut hist,
            t,
        )?;
        if crate::dbgenv::graphix_dbg_bind() {
            eprintln!("CHK-CONTAINS {self} >= {t} -> {ok}");
        }
        if ok { Ok(()) } else { Err(self.contains_mismatch(t)) }
    }

    fn contains_mismatch(&self, t: &Self) -> anyhow::Error {
        // A refused open cell on either side is the infinite type,
        // surfacing at a consumer; report it as the settle path does.
        if type_has_refused_open_cell(self)
            || type_has_refused_open_cell(t)
            || open_cell_reaches(self, t)
            || open_cell_reaches(t, self)
        {
            return anyhow::anyhow!("{INFINITE_TYPE_MSG}");
        }
        anyhow::Error::new(TypeMismatch { expected: self.clone(), actual: t.clone() })
    }

    /// [`Self::check_contains`] with rigid enforcement; the def gate's
    /// acceptance checks only.
    pub fn check_contains_rigid(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = RefHist::new(LPooled::take());
        let ok = self.contains_int(
            ContainsFlags::AliasTVars
                | ContainsFlags::InitTVars
                | ContainsFlags::RigidCheck,
            env,
            &mut hist,
            t,
        )?;
        if ok { Ok(()) } else { Err(self.contains_mismatch(t)) }
    }

    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        crate::stack::ensure_sufficient(|| self.contains_int_inner(flags, env, hist, t))
    }

    fn contains_int_inner(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        if (self as *const Type) == (t as *const Type) || same_content(self, t) {
            return Ok(true);
        }
        // Flagged calls may bind cells, so they invalidate the memo.
        if flags.is_empty() {
            if let Some(r) = hist.probe_get(self, t) {
                return Ok(r);
            }
            let r = self.contains_dispatch(flags, env, hist, t)?;
            hist.probe_put(self, t, r);
            return Ok(r);
        }
        hist.note_commit();
        self.contains_dispatch(flags, env, hist, t)
    }

    fn contains_dispatch(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        // A trait in type position is a predicate, reached only as a
        // cell conjunct.
        if let Self::Ref(tr) = self
            && let Some(tid) = env.trait_of_ref(tr)
        {
            return Self::trait_contains(tid, flags, env, hist, t);
        }
        if let Self::Ref(tr) = t
            && let Some(tid) = env.trait_of_ref(tr)
        {
            return Ok(match self {
                Self::Any => true,
                Self::Ref(tr0) => env.trait_of_ref(tr0) == Some(tid),
                _ => false,
            });
        }
        match (self, t) {
            // A constructor application decomposes a reference by
            // name, ahead of the expansion arm.
            (Self::App(..), Self::Ref(_)) | (Self::Ref(_), Self::App(..)) => {
                self.app_contains(flags, env, hist, t)
            }
            // A cell bound to a reference meets a reference by name
            // before either expands.
            (Self::Ref(_), Self::TVar(_)) if t.ref_behind().is_some() => {
                let behind = t.ref_behind().expect("checked");
                self.contains_int(flags, env, hist, &behind)
            }
            (Self::TVar(_), Self::Ref(_)) if self.ref_behind().is_some() => {
                let behind = self.ref_behind().expect("checked");
                behind.contains_int(flags, env, hist, t)
            }
            (Self::Hole, Self::Hole) => Ok(true),
            (Self::Hole, Self::TVar(tv)) => {
                let bound = tv.read().typ.read().typ.clone();
                match bound {
                    Some(b) => Self::Hole.contains_int(flags, env, hist, &b),
                    None => Ok(false),
                }
            }
            (Self::TVar(tv), Self::Hole) => {
                let bound = tv.read().typ.read().typ.clone();
                match bound {
                    Some(b) => b.contains_int(flags, env, hist, &Self::Hole),
                    None => Ok(false),
                }
            }
            (Self::Hole, _) | (_, Self::Hole) => Ok(false),
            // Two filled cells can hold different defs for one name;
            // disagreement falls through to the expansion arm.
            (Self::Ref(tr0), Self::Ref(tr1))
                if tr0.scope == tr1.scope
                    && tr0.name == tr1.name
                    && tr0.cells_agree(tr1) =>
            {
                Ok(tr0.params.len() == tr1.params.len()
                    && tr0
                        .params
                        .iter()
                        .zip(tr1.params.iter())
                        .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (t0 @ Self::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Self::Ref(TypeRef { .. })) => {
                let t0_id = hist.ref_id(t0, env);
                let t1_id = hist.ref_id(t1, env);
                let raw = flags.is_empty();
                let t0 = hist.expand_ref(t0, t0_id, env, raw)?;
                let t1 = hist.expand_ref(t1, t1_id, env, raw)?;
                match hist.get(&(t0_id, t1_id)) {
                    Some(r) => {
                        if crate::dbgenv::graphix_dbg_bind() && !raw {
                            eprintln!("REF-MEMO-HIT ({t0_id:?},{t1_id:?}) -> {r}");
                        }
                        Ok(*r)
                    }
                    None => {
                        hist.insert((t0_id, t1_id), true);
                        let r = t0.contains_int(flags, env, hist, &t1);
                        hist.remove(&(t0_id, t1_id));
                        r
                    }
                }
            }
            // ⊥ fits whatever the cell becomes; binding would only
            // foreclose its writers. An unrefined cell settles to ⊥.
            (Self::TVar(_), Self::Bottom) => Ok(true),
            // ⊥ ⊇ 'r has one solution, so an open cell commits under
            // InitTVars; a bound cell answers for its binding.
            (Self::Bottom, Self::TVar(t0)) => {
                let bound = t0.read().typ.read().typ.clone();
                match bound {
                    Some(b) => Self::Bottom.contains_int(flags, env, hist, &b),
                    None => {
                        if flags.contains(ContainsFlags::InitTVars) {
                            t0.read().typ.write().typ = Some(Self::Bottom);
                        }
                        Ok(true)
                    }
                }
            }
            (Self::Bottom, Self::Bottom) => Ok(true),
            (Self::Bottom, _) => Ok(false),
            (_, Self::Bottom) => Ok(true),
            (Self::TVar(t0), Self::Any) => {
                // Clone the binding out before recursing, here and in
                // every deref arm: the walk can revisit this cell and
                // write-lock it, and the locks are non-reentrant.
                let bound = t0.read().typ.read().typ.clone();
                if let Some(t0) = bound {
                    return t0.contains_int(flags, env, hist, t);
                }
                // A rigid cell contains only itself and Bottom.
                if flags.contains(ContainsFlags::RigidCheck) && t0.is_rigid() {
                    return Ok(false);
                }
                if !cell_constraints_ok(t0, env, hist, &Self::Any)? {
                    return Ok(false);
                }
                // A rigid cell is never written outside the acceptance
                // judgment.
                if flags.contains(ContainsFlags::InitTVars) && !t0.is_rigid() {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!("BIND lhs '{}({:x}) := Any", t0.name, t0.cell_addr());
                    }
                    t0.read().typ.write().typ = Some(Self::Any);
                }
                Ok(true)
            }
            (Self::Any, _) => Ok(true),
            (
                Self::Abstract { id: id0, params: p0 },
                Self::Abstract { id: id1, params: p1 },
            ) => {
                if id0 != id1 {
                    return Ok(false);
                }
                Ok(p0.len() == p1.len()
                    && p0
                        .iter()
                        .zip(p1.iter())
                        .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (Self::Primitive(p0), Self::Primitive(p1)) => Ok(p0.contains(*p1)),
            (
                Self::Primitive(p),
                Self::Array(_) | Self::Tuple(_) | Self::Struct(_) | Self::Variant(_, _),
            ) => Ok(p.contains(Typ::Array)),
            (Self::Array(t0), Self::Array(t1)) => t0.contains_int(flags, env, hist, t1),
            (Self::List(t0), Self::List(t1)) => t0.contains_int(flags, env, hist, t1),
            (
                Self::List(_),
                Self::Primitive(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Struct(_)
                | Self::Variant(_, _)
                | Self::Error(_)
                | Self::Map { .. },
            )
            | (
                Self::Primitive(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Struct(_)
                | Self::Variant(_, _)
                | Self::Error(_)
                | Self::Map { .. },
                Self::List(_),
            ) => Ok(false),
            (Self::Array(t0), Self::Primitive(p)) if *p == BitFlags::from(Typ::Array) => {
                t0.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                Ok(k0.contains_int(flags, env, hist, k1)?
                    && v0.contains_int(flags, env, hist, v1)?)
            }
            (Self::Primitive(p), Self::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Self::Map { key, value }, Self::Primitive(p))
                if *p == BitFlags::from(Typ::Map) =>
            {
                Ok(key.contains_int(flags, env, hist, &Type::Any)?
                    && value.contains_int(flags, env, hist, &Type::Any)?)
            }
            (Self::Primitive(p0), Self::Error(_)) => Ok(p0.contains(Typ::Error)),
            (Self::Error(e), Self::Primitive(p)) if *p == BitFlags::from(Typ::Error) => {
                e.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Error(e0), Self::Error(e1)) => e0.contains_int(flags, env, hist, e1),
            (Self::Tuple(t0), Self::Tuple(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
            (Self::Tuple(t0), Self::Tuple(t1)) => Ok(t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Struct(t0), Self::Struct(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
            (Self::Struct(t0), Self::Struct(t1)) => {
                Ok(t0.len() == t1.len() && {
                    // Struct fields are sorted by name.
                    t0.iter()
                        .zip(t1.iter())
                        .map(|((n0, t0), (n1, t1))| {
                            Ok(n0 == n1 && t0.contains_int(flags, env, hist, t1)?)
                        })
                        .collect::<Result<AndAc>>()?
                        .0
                })
            }
            (Self::Variant(tg0, t0), Self::Variant(tg1, t1))
                if tg0.as_ptr() == tg1.as_ptr() && Arc::ptr_eq(t0, t1) =>
            {
                Ok(true)
            }
            (Self::Variant(tg0, t0), Self::Variant(tg1, t1)) => Ok(tg0 == tg1
                && t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::ByRef(t0), Self::ByRef(t1)) => t0.contains_int(flags, env, hist, t1),
            // Two vars sharing one cell are already unified; the cycle
            // guard below would otherwise poison both.
            (Self::TVar(t0), Self::TVar(t1))
                if t0.addr() == t1.addr()
                    || t0.read().id == t1.read().id
                    || t0.same_cell(t1) =>
            {
                Ok(true)
            }
            (tt0 @ Self::TVar(t0), tt1 @ Self::TVar(t1)) => {
                #[derive(Debug)]
                enum Act {
                    RightCopy,
                    RightAlias,
                    LeftAlias,
                    LeftCopy,
                    CellMerge,
                }
                // Recursion happens outside the guard block.
                enum ActOrRecurse {
                    Act(Act, Option<Type>),
                    Recurse(Type, Type),
                    Memo(Type, Type, usize, usize),
                    Refuse,
                    /// Two declared vars of the def under check (rigid
                    /// cells exist only inside its gate): the body must
                    /// not require them equal.
                    Distinct,
                }
                let act = {
                    let t0 = t0.read();
                    let t1 = t1.read();
                    let addr0 = Arc::as_ptr(&t0.typ).addr();
                    let addr1 = Arc::as_ptr(&t1.typ).addr();
                    if addr0 == addr1 {
                        return Ok(true);
                    }
                    let cyc0 = would_cycle_inner(addr0, tt1);
                    let cyc1 = would_cycle_inner(addr1, tt0);
                    let t0i = t0.typ.read();
                    let t1i = t1.typ.read();
                    match (&t0i.typ, &t1i.typ) {
                        // An open cell meeting a bound cell whose binding
                        // reaches it is the μ-shape through a binding: a
                        // copy would bind the infinite type, so walk the
                        // binding, where `'r ⊇ [T, 'r]` collapses.
                        (None, Some(b)) if cyc0 => {
                            ActOrRecurse::Recurse(tt0.clone(), b.clone())
                        }
                        (Some(b), None) if cyc1 => {
                            ActOrRecurse::Recurse(b.clone(), tt1.clone())
                        }
                        // Two bound cells decide by walking the bindings;
                        // every bind is occurs-checked, so the walk
                        // bottoms out (the pair memo bounds it otherwise).
                        (Some(b0), Some(b1)) if cyc0 || cyc1 => {
                            ActOrRecurse::Memo(b0.clone(), b1.clone(), addr0, addr1)
                        }
                        _ if cyc0 || cyc1 => ActOrRecurse::Refuse,
                        (Some(t0), Some(t1)) => {
                            ActOrRecurse::Recurse(t0.clone(), t1.clone())
                        }
                        (None, None) if t0i.rigid_gates > 0 && t1i.rigid_gates > 0 => {
                            ActOrRecurse::Distinct
                        }
                        (None, None) => {
                            if t0.frozen && t1.frozen {
                                // Merge the cells: `frozen` gates
                                // name-aliasing, not unification.
                                ActOrRecurse::Act(Act::CellMerge, None)
                            } else if t0.frozen {
                                ActOrRecurse::Act(Act::RightAlias, None)
                            } else {
                                ActOrRecurse::Act(Act::LeftAlias, None)
                            }
                        }
                        // A rigid receiver must not bind; re-verdict
                        // against the bare var in the arms below.
                        (Some(b), None)
                            if flags.contains(ContainsFlags::RigidCheck)
                                && t1i.rigid_gates > 0 =>
                        {
                            ActOrRecurse::Recurse(b.clone(), tt1.clone())
                        }
                        (None, Some(b))
                            if flags.contains(ContainsFlags::RigidCheck)
                                && t0i.rigid_gates > 0 =>
                        {
                            ActOrRecurse::Recurse(tt0.clone(), b.clone())
                        }
                        (Some(b), None) => {
                            if crate::dbgenv::graphix_dbg_bind() {
                                eprintln!(
                                    "TT-RIGHTCOPY '{} <= '{}",
                                    t1.id.inner(),
                                    t0.id.inner()
                                );
                            }
                            ActOrRecurse::Act(Act::RightCopy, Some(b.clone()))
                        }
                        (None, Some(b)) => {
                            if crate::dbgenv::graphix_dbg_bind() {
                                eprintln!(
                                    "TT-LEFTCOPY '{} <= '{}",
                                    t0.id.inner(),
                                    t1.id.inner()
                                );
                            }
                            ActOrRecurse::Act(Act::LeftCopy, Some(b.clone()))
                        }
                    }
                };
                let (act, bound) = match act {
                    ActOrRecurse::Distinct => return Ok(false),
                    ActOrRecurse::Refuse => {
                        if crate::dbgenv::graphix_dbg_cycle_bt() {
                            eprintln!(
                                "CYCLE-REFUSED-PAIR ({:x},{:x})\n{}",
                                t0.cell_addr(),
                                t1.cell_addr(),
                                std::backtrace::Backtrace::force_capture()
                            );
                        }
                        t0.mark_cycle_refused();
                        t1.mark_cycle_refused();
                        return Ok(true);
                    }
                    ActOrRecurse::Recurse(a, b) => {
                        return a.contains_int(flags, env, hist, &b);
                    }
                    ActOrRecurse::Memo(a, b, addr0, addr1) => {
                        let key = (Some(addr0), Some(addr1));
                        if let Some(r) = hist.get(&key) {
                            return Ok(*r);
                        }
                        hist.insert(key, true);
                        let r = a.contains_int(flags, env, hist, &b);
                        hist.remove(&key);
                        return r;
                    }
                    ActOrRecurse::Act(act, bound) => (act, bound),
                };
                // The receiver's constraints must admit the binding.
                match act {
                    Act::RightCopy
                        if flags.contains(ContainsFlags::InitTVars) && !t1.is_rigid() =>
                    {
                        let b = bound.as_ref().expect("copy without binding");
                        if !cell_constraints_ok(t1, env, hist, b)? {
                            return Ok(false);
                        }
                        t1.copy(t0)
                    }
                    Act::RightAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        if crate::dbgenv::graphix_dbg_bind() {
                            eprintln!(
                                "RALIAS '{}({:x}) -> '{}({:x})",
                                t1.name,
                                t1.cell_addr(),
                                t0.name,
                                t0.cell_addr()
                            );
                        }
                        t1.alias(t0)
                    }
                    Act::LeftAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        if crate::dbgenv::graphix_dbg_bind() {
                            eprintln!(
                                "LALIAS '{}({:x}) -> '{}({:x})",
                                t0.name,
                                t0.cell_addr(),
                                t1.name,
                                t1.cell_addr()
                            );
                        }
                        t0.alias(t1)
                    }
                    Act::LeftCopy
                        if flags.contains(ContainsFlags::InitTVars) && !t0.is_rigid() =>
                    {
                        let b = bound.as_ref().expect("copy without binding");
                        if !cell_constraints_ok(t0, env, hist, b)? {
                            return Ok(false);
                        }
                        t0.copy(t1)
                    }
                    Act::CellMerge if flags.contains(ContainsFlags::AliasTVars) => {
                        t0.alias_cells(t1)
                    }
                    Act::RightCopy
                    | Act::RightAlias
                    | Act::LeftAlias
                    | Act::LeftCopy
                    | Act::CellMerge => (),
                }
                Ok(true)
            }
            (Self::TVar(t0), t1) if !t0.would_cycle(t1) => {
                let bound = t0.read().typ.read().typ.clone();
                if let Some(t0) = bound {
                    return t0.contains_int(flags, env, hist, t1);
                }
                // A rigid tvar contains only itself and Bottom.
                if flags.contains(ContainsFlags::RigidCheck) && t0.is_rigid() {
                    return Ok(false);
                }
                // A constraint violation fails here, at the site that
                // tried it.
                if !cell_constraints_ok(t0, env, hist, t1)? {
                    return Ok(false);
                }
                if flags.contains(ContainsFlags::InitTVars) && !t0.is_rigid() {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!(
                            "BIND lhs '{}({:x}) := {t1:?}",
                            t0.name,
                            t0.cell_addr()
                        );
                    }
                    t0.read().typ.write().typ = Some(t1.clone());
                }
                Ok(true)
            }
            (t0, Self::TVar(t1)) if !t1.would_cycle(t0) => {
                let bound = t1.read().typ.read().typ.clone();
                if let Some(t1) = bound {
                    return t0.contains_int(flags, env, hist, &t1);
                }
                // t0 contains an arbitrary 'a only when it contains one
                // of the cell's conjuncts ('a ⊆ C ⊆ t0).
                if flags.contains(ContainsFlags::RigidCheck) && t1.is_rigid() {
                    let cons = t1.read().typ.read().constraints.clone();
                    for c in cons.iter() {
                        // A probe: a flagged check would alias live
                        // cells into the constraint store.
                        if t0.contains_int(BitFlags::empty(), env, hist, c)? {
                            return Ok(true);
                        }
                    }
                    return Ok(false);
                }
                if !cell_constraints_ok(t1, env, hist, t0)? {
                    return Ok(false);
                }
                if flags.contains(ContainsFlags::InitTVars) && !t1.is_rigid() {
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!(
                            "BIND rhs '{}({:x}) := {t0:?}",
                            t1.name,
                            t1.cell_addr()
                        );
                    }
                    t1.read().typ.write().typ = Some(t0.clone());
                }
                Ok(true)
            }
            (Self::Set(s0), Self::Set(s1)) if Arc::ptr_eq(s0, s1) => Ok(true),
            (t0 @ Self::Set(_), t1 @ Self::Set(_)) if t0 == t1 => {
                if flags.contains(ContainsFlags::InitTVars) {
                    link_equal(t0, t1);
                }
                Ok(true)
            }
            // A set with a bare unbound tvar member binds the tvar to
            // the residue (the rhs members no concrete lhs member
            // covers) in one act; per-member it would capture greedily.
            (t0 @ Self::Set(s0), Self::Set(s1))
                if s0.iter().any(
                    |m| matches!(m, Self::TVar(tv) if tv.read().typ.read().typ.is_none()),
                ) =>
            {
                let probe = BitFlags::empty();
                let mut residue: LPooled<Vec<Type>> = LPooled::take();
                for m in s1.iter() {
                    // An rhs member equal to the whole lhs set is
                    // covered reflexively; as residue it would close a
                    // cycle.
                    let reflexive = m.with_deref(|md| match md {
                        Some(md) if t0 == md => {
                            if flags.contains(ContainsFlags::InitTVars) {
                                link_equal(t0, md);
                            }
                            true
                        }
                        _ => false,
                    });
                    if reflexive {
                        continue;
                    }
                    // An rhs member that is one of the lhs's own cells
                    // is covered reflexively too.
                    let own_cell = match m {
                        Self::TVar(mtv) => s0.iter().any(|c| match c {
                            Self::TVar(ctv) => {
                                Arc::ptr_eq(&ctv.read().typ, &mtv.read().typ)
                            }
                            _ => false,
                        }),
                        _ => false,
                    };
                    if own_cell {
                        continue;
                    }
                    // A free rhs member is residue too: the coverage
                    // loop would bind it greedily; in the residue it
                    // aliases with the bare lhs member.
                    if is_unbound_tvar(&m) {
                        residue.push(m.clone());
                        continue;
                    }
                    let mut covered = false;
                    for c in s0.iter().filter(|c| !is_unbound_tvar(c)) {
                        if c.contains_int(probe, env, hist, m)? {
                            if !c.contains_int(flags, env, hist, m)? {
                                return Ok(false);
                            }
                            covered = true;
                            break;
                        }
                    }
                    if !covered {
                        residue.push(m.clone());
                    }
                }
                if residue.is_empty() {
                    return Ok(true);
                }
                let target = if residue.len() == 1 {
                    residue[0].clone()
                } else {
                    Type::Set(Arc::from_iter(residue.drain(..)))
                };
                let target = target.normalize();
                match s0.iter().find(|m| is_unbound_tvar(m)) {
                    Some(tv_m) => tv_m.contains_int(flags, env, hist, &target),
                    None => Ok(false),
                }
            }
            // Member-wise equality pre-pass: only the residue takes the
            // general per-member walk (which is O(|s0|·|s1|) per level).
            (t0 @ Self::Set(s0), Self::Set(s1)) => {
                for m in s1.iter() {
                    match s0.iter().find(|c| *c == m) {
                        Some(c) => {
                            if flags.contains(ContainsFlags::InitTVars) {
                                link_equal(c, m);
                            }
                        }
                        None => {
                            if !t0.contains_int(flags, env, hist, m)? {
                                return Ok(false);
                            }
                        }
                    }
                }
                Ok(true)
            }
            (t0, Self::Set(s)) => Ok(s
                .iter()
                .map(|t1| t0.contains_int(flags, env, hist, t1))
                .collect::<Result<AndAc>>()?
                .0),
            (Self::Set(s), t) => {
                let probe = BitFlags::empty();
                let whole_ok =
                    s.iter().fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                        Ok(acc? || t0.contains_int(probe, env, hist, t)?)
                    })?;
                let prims_ok =
                    t.iter_prims().fold(Ok::<_, anyhow::Error>(true), |acc, t1| {
                        Ok(acc?
                            && s.iter().fold(
                                Ok::<_, anyhow::Error>(false),
                                |acc, t0| {
                                    Ok(acc? || t0.contains_int(probe, env, hist, &t1)?)
                                },
                            )?)
                    })?;
                // Structural members first: a bare unbound TVar admits
                // anything, so it is the fallback (`['b, Array<'b>]`
                // must bind an array through `Array<'b>`).
                let members = || {
                    s.iter()
                        .filter(|t0| !is_unbound_tvar(t0))
                        .chain(s.iter().filter(|t0| is_unbound_tvar(t0)))
                };
                if crate::dbgenv::graphix_dbg_bind() {
                    eprintln!(
                        "SET-T {} >= {t} whole={whole_ok} prims={prims_ok}",
                        Self::Set(s.clone())
                    );
                }
                match (whole_ok, prims_ok) {
                    (false, false) => Self::set_covers_by_distribution(env, hist, s, t),
                    // Prims first: the narrowest TVar bindings.
                    (_, true) => Ok(t.iter_prims().fold(
                        Ok::<_, anyhow::Error>(true),
                        |acc, t1| {
                            Ok(acc?
                                && members().fold(
                                    Ok::<_, anyhow::Error>(false),
                                    |acc, t0| {
                                        Ok(acc?
                                            || t0.contains_int(flags, env, hist, &t1)?)
                                    },
                                )?)
                        },
                    )?),
                    (true, false) => Ok(members()
                        .fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                            Ok(acc? || t0.contains_int(flags, env, hist, t)?)
                        })?),
                }
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                let same = Arc::ptr_eq(f0, f1);
                let r = same || f0.contains_int(flags, env, hist, f1)?;
                if r && !same && flags.contains(ContainsFlags::InitTVars) {
                    f0.lambda_ids.link(&f1.lambda_ids);
                }
                Ok(r)
            }
            (Self::App(..), _) | (_, Self::App(..)) => {
                self.app_contains(flags, env, hist, t)
            }
            (Self::Abstract { .. }, _) | (_, Self::Abstract { .. }) => Ok(false),
            (_, Self::Any)
            | (_, Self::TVar(_))
            | (Self::TVar(_), _)
            | (Self::Fn(_), _)
            | (Self::ByRef(_), _)
            | (_, Self::ByRef(_))
            | (_, Self::Fn(_))
            | (Self::Tuple(_), Self::Array(_))
            | (Self::Tuple(_), Self::Primitive(_))
            | (Self::Tuple(_), Self::Struct(_))
            | (Self::Tuple(_), Self::Variant(_, _))
            | (Self::Tuple(_), Self::Error(_))
            | (Self::Tuple(_), Self::Map { .. })
            | (Self::Array(_), Self::Primitive(_))
            | (Self::Array(_), Self::Tuple(_))
            | (Self::Array(_), Self::Struct(_))
            | (Self::Array(_), Self::Variant(_, _))
            | (Self::Array(_), Self::Error(_))
            | (Self::Array(_), Self::Map { .. })
            | (Self::Struct(_), Self::Array(_))
            | (Self::Struct(_), Self::Primitive(_))
            | (Self::Struct(_), Self::Tuple(_))
            | (Self::Struct(_), Self::Variant(_, _))
            | (Self::Struct(_), Self::Error(_))
            | (Self::Struct(_), Self::Map { .. })
            | (Self::Variant(_, _), Self::Array(_))
            | (Self::Variant(_, _), Self::Struct(_))
            | (Self::Variant(_, _), Self::Primitive(_))
            | (Self::Variant(_, _), Self::Tuple(_))
            | (Self::Variant(_, _), Self::Error(_))
            | (Self::Variant(_, _), Self::Map { .. })
            | (Self::Error(_), Self::Array(_))
            | (Self::Error(_), Self::Primitive(_))
            | (Self::Error(_), Self::Struct(_))
            | (Self::Error(_), Self::Variant(_, _))
            | (Self::Error(_), Self::Tuple(_))
            | (Self::Error(_), Self::Map { .. })
            | (Self::Map { .. }, Self::Array(_))
            | (Self::Map { .. }, Self::Primitive(_))
            | (Self::Map { .. }, Self::Struct(_))
            | (Self::Map { .. }, Self::Variant(_, _))
            | (Self::Map { .. }, Self::Tuple(_))
            | (Self::Map { .. }, Self::Error(_)) => Ok(false),
        }
    }

    /// The distribution law for product heads: a set whose members
    /// split one argument position of a constructor across same-shaped
    /// alternatives covers the pooled constructor —
    /// `` [`T(A), `T(B)] ⊇ `T([A, B]) `` — provided every candidate
    /// covers every other position in full. A pure probe over cell-free
    /// operands; commits nothing.
    fn set_covers_by_distribution(
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        s: &Arc<[Type]>,
        t: &Self,
    ) -> Result<bool> {
        let t_id = hist.ref_id(t, env);
        if let Some(id) = t_id {
            if hist.distribution_probes_in_progress.contains(&id) {
                return Ok(false);
            }
            hist.distribution_probes_in_progress.push(id);
        }
        let r = Self::set_covers_by_distribution_inner(env, hist, s, t);
        if t_id.is_some() {
            hist.distribution_probes_in_progress.pop();
        }
        r
    }

    fn set_covers_by_distribution_inner(
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        s: &Arc<[Type]>,
        t: &Self,
    ) -> Result<bool> {
        fn head(env: &Env, t: &Type) -> Type {
            let mut cur = t.clone();
            for _ in 0..64 {
                cur = match &cur {
                    Type::TVar(_) => match cur.deref_cloned() {
                        Some(next) => next,
                        None => break,
                    },
                    // An unresolvable ref does not distribute; not an error.
                    Type::Ref(_) => match cur.lookup_ref(env) {
                        Ok(next) => next,
                        Err(_) => break,
                    },
                    _ => break,
                }
            }
            cur
        }
        let t = head(env, t);
        if t.has_unbound() {
            return Ok(false);
        }
        let mut targs: LPooled<Vec<Type>> = LPooled::take();
        match &t {
            Type::Variant(_, args) => targs.extend(args.iter().cloned()),
            Type::Tuple(args) => targs.extend(args.iter().cloned()),
            Type::Struct(flds) => targs.extend(flds.iter().map(|(_, t)| t.clone())),
            _ => return Ok(false),
        }
        let mut cands: LPooled<Vec<LPooled<Vec<Type>>>> = LPooled::take();
        for m in s.iter() {
            let m = head(env, m);
            let args: Option<LPooled<Vec<Type>>> = match (&t, &m) {
                (Type::Variant(tt, ta), Type::Variant(mt, ma))
                    if tt == mt && ta.len() == ma.len() =>
                {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Tuple(ta), Type::Tuple(ma)) if ta.len() == ma.len() => {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Struct(tf), Type::Struct(mf))
                    if tf.len() == mf.len()
                        && tf.iter().zip(mf.iter()).all(|((a, _), (b, _))| a == b) =>
                {
                    Some(mf.iter().map(|(_, t)| t.clone()).collect())
                }
                _ => None,
            };
            // Open cells in a candidate are fine (the probe accepts
            // through them without binding); only the scrutinee side
            // must be cell-free.
            if let Some(args) = args {
                cands.push(args);
            }
        }
        if cands.is_empty() {
            return Ok(false);
        }
        let probe = BitFlags::empty();
        let mut distributing: Option<usize> = None;
        for j in 0..targs.len() {
            let mut full = true;
            for c in cands.iter() {
                full &= c[j].contains_int(probe, env, hist, &targs[j])?;
                if !full {
                    break;
                }
            }
            if !full {
                if distributing.is_some() {
                    return Ok(false);
                }
                distributing = Some(j);
            }
        }
        match distributing {
            None => Ok(true),
            Some(j) => {
                let pool = Type::Set(Arc::from_iter(cands.iter().map(|c| c[j].clone())))
                    .normalize();
                pool.contains_int(probe, env, hist, &targs[j])
            }
        }
    }

    /// A constructor application (`self<'a>`, `'c<'b>`) against another
    /// type, either way round. A bound constructor is its filled type;
    /// an open one meets the other side decomposed on its outermost
    /// form and binds the constructor variable by name, discharging its
    /// bound through `find_impl`. A type with no last parameter is not
    /// a constructor.
    fn app_contains(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        match (self, t) {
            (Self::App(c0, a0), Self::App(c1, a1)) => {
                match (Self::app_filled(c0, a0), Self::app_filled(c1, a1)) {
                    (Some(f0), Some(f1)) => f0.contains_int(flags, env, hist, &f1),
                    (Some(f0), None) => f0.contains_int(flags, env, hist, t),
                    (None, Some(f1)) => self.contains_int(flags, env, hist, &f1),
                    (None, None) => Ok(c0.contains_int(flags, env, hist, c1)?
                        && a0.contains_int(flags, env, hist, a1)?),
                }
            }
            (Self::App(c, a), t1) => match Self::app_filled(c, a) {
                Some(filled) => filled.contains_int(flags, env, hist, t1),
                None => match Self::app_split_for(c, t1, env)? {
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && a.contains_int(flags, env, hist, &last)?)
                    }
                    None => Ok(false),
                },
            },
            (t0, Self::App(c, a)) => match Self::app_filled(c, a) {
                Some(filled) => t0.contains_int(flags, env, hist, &filled),
                None => match Self::app_split_for(c, t0, env)? {
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && last.contains_int(flags, env, hist, a)?)
                    }
                    None => Ok(false),
                },
            },
            _ => unreachable!("app_contains without an application"),
        }
    }

    /// Bind an open constructor variable to a constructor by name (the
    /// general walk would expand the reference and lose the name every
    /// later lookup keys on). Anything but an open, non-rigid variable
    /// takes the general walk.
    fn bind_ctor(
        c: &Self,
        ctor: &Self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
    ) -> Result<bool> {
        if let Self::TVar(cv) = c
            && cv.read().typ.read().typ.is_none()
            && !(flags.contains(ContainsFlags::RigidCheck) && cv.is_rigid())
        {
            if !cell_constraints_ok(cv, env, hist, ctor)? {
                return Ok(false);
            }
            if flags.contains(ContainsFlags::InitTVars) && !cv.is_rigid() {
                if crate::dbgenv::graphix_dbg_bind() {
                    eprintln!("BIND ctor '{}({:x}) := {ctor:?}", cv.name, cv.cell_addr());
                }
                cv.read().typ.write().typ = Some(ctor.clone());
            }
            return Ok(true);
        }
        c.contains_int(flags, env, hist, ctor)
    }

    /// Does `t` implement the trait `tid`? ⊥ implements everything,
    /// `Any` nothing, a union iff every member does, an open cell iff
    /// it could still become an implementor (a rigid cell only through
    /// its own conjuncts), a typedef by its expansion, anything
    /// structural by the impl table.
    fn trait_contains(
        tid: crate::typ::TraitId,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        // The core traits have a structural default for every type.
        if crate::node::coretraits::CoreTrait::of_id(tid).is_some() {
            return Ok(true);
        }
        match t {
            Self::Bottom => Ok(true),
            Self::Any => Ok(false),
            Self::TVar(tv) => {
                let bound = tv.read().typ.read().typ.clone();
                if let Some(b) = bound {
                    return Self::trait_contains(tid, flags, env, hist, &b);
                }
                if flags.contains(ContainsFlags::RigidCheck) && tv.is_rigid() {
                    let cons = tv.read().typ.read().constraints.clone();
                    return Ok(cons.iter().any(
                        |c| matches!(c, Self::Ref(r) if env.trait_of_ref(r) == Some(tid)),
                    ));
                }
                Ok(true)
            }
            Self::Set(ts) => {
                for m in ts.iter() {
                    if !Self::trait_contains(tid, flags, env, hist, m)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Self::Primitive(p) if p.len() > 1 => {
                for m in p.iter() {
                    if env.find_impl(tid, &Self::Primitive(m.into()))?.is_none() {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Self::Ref(tr) => match env.trait_of_ref(tr) {
                Some(o) => Ok(o == tid),
                // A constructor trait's reference is matched by name,
                // never expanded.
                None if env.trait_def(tid).is_some_and(|d| d.hole) => {
                    Ok(env.find_impl(tid, t)?.is_some())
                }
                None => {
                    let e = t.lookup_ref(env)?;
                    Self::trait_contains(tid, flags, env, hist, &e)
                }
            },
            t => Ok(env.find_impl(tid, t)?.is_some()),
        }
    }

    /// Is this a reference to a trait (in `env`)?
    pub fn is_trait_ref(&self, env: &Env) -> bool {
        matches!(self, Self::Ref(tr) if env.trait_of_ref(tr).is_some())
    }

    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        let r = self.contains_int(
            ContainsFlags::AliasTVars | ContainsFlags::InitTVars,
            env,
            &mut RefHist::new(LPooled::take()),
            t,
        );
        if crate::dbgenv::graphix_dbg_bind() {
            eprintln!("CONTAINS {self} >= {t} -> {r:?}");
        }
        r
    }

    pub fn contains_with_flags(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        t: &Self,
    ) -> Result<bool> {
        self.contains_int(flags, env, &mut RefHist::new(LPooled::take()), t)
    }
}
