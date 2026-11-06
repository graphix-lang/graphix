//! The settle phase: open cells that inference left constrained bind to
//! their conjunction's witness, and a call site's terminal settle binds
//! what nothing produced or bounded to ⊥.

use crate::{
    AddrSet, PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_bind_bt_id},
    env::Env,
    format_with_flags,
    typ::{
        Bound, FnType, TVar, Type,
        contains::{ContainsHist, InfiniteType},
        setops::strictly_identical,
        tvar::{CellFact, ConjunctsPassed, Fresh, Level, would_cycle_open},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    error::Error,
    fmt::{self, Write},
    mem,
};

#[derive(Debug)]
pub struct ConcreteRequired(pub TVar);

impl fmt::Display for ConcreteRequired {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "the type '{} must be fully known here, a type-directed operation reads it: \
             annotate it",
            super::tvar::ShortName(&self.0.name)
        )
    }
}

impl Error for ConcreteRequired {}

impl ConcreteRequired {
    pub fn unread_callback(&self) -> anyhow::Error {
        anyhow::anyhow!(
            "the quantifier '{} must be fully known but is unread through an optional \
             callback at this call: pass a typed handler or a typed null",
            super::tvar::ShortName(&self.0.name)
        )
    }
}

impl TVar {
    /// The union of the cell's lower bounds, else the narrowest conjunct
    /// every other conjunct contains, its generic cells copied (a stored
    /// scheme's cells must not join live inference) and its live cells
    /// shared, so it keeps its relations. `Err(true)` when there is none,
    /// `Err(false)` when no conjunct could be one (all traits or
    /// self-referential). Bound and unbounded cells have none.
    pub(crate) fn witness(&self, env: &Env) -> Result<std::result::Result<Type, bool>> {
        let (cons, below) = {
            let cell = self.cell();
            let cell = cell.read();
            if cell.binding.is_some()
                || (cell.constraints.is_empty() && cell.lower.is_empty())
            {
                return Ok(Err(false));
            }
            (cell.constraints.clone(), cell.lower.clone())
        };
        let addr = self.cell_addr();
        if !below.is_empty() {
            if below.iter().any(|l| would_cycle_open(addr, l)) {
                return Ok(Err(false));
            }
            let below: LPooled<Vec<&Type>> = below.iter().collect();
            let w = Type::union(env, &below)?;
            let mut hist = ContainsHist::new();
            for c in cons.iter() {
                if !c.contains_int(BitFlags::empty(), env, &mut hist, &w)? {
                    return Ok(Err(true));
                }
            }
            return Ok(Ok(w));
        }
        let mut hist = ContainsHist::new();
        let mut candidates = false;
        'cand: for c in cons.iter() {
            // A trait conjunct is a predicate, not a binding, and a
            // conjunct reaching this cell has no finite witness.
            if c.is_predicate() || c.is_trait_ref(env) || would_cycle_open(addr, c) {
                continue;
            }
            candidates = true;
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    continue 'cand;
                }
            }
            return Ok(Ok(c
                .reset_tvars_int(&mut LPooled::take(), Fresh::Generic)
                .unwrap_or_else(|| c.clone())));
        }
        Ok(Err(candidates))
    }

    /// Bind an unbound cell once its writers are checked: to ⊥ when only
    /// ⊥ was produced into it ([`CellFact::BottomFed`], and no lower bound
    /// but ⊥), whatever constrains it, else as [`Self::settle_witness`].
    ///
    /// [`CellFact::BottomFed`]: super::tvar::CellFact::BottomFed
    pub fn settle(&self, env: &Env) -> Result<()> {
        if self.cell().read().facts.contains(CellFact::BottomFed)
            && self
                .cell_lower()
                .iter()
                .all(|l| l.with_deref(|l| matches!(l, Some(Type::Bottom))))
        {
            return self.settle_bottom();
        }
        self.settle_witness(env)
    }

    /// Bind a constrained-unbound cell to its conjunction's [witness].
    /// Bound and unconstrained cells are untouched. No witness is a type
    /// error, unless no conjunct could be one: then the cell stays open
    /// for its writers to refine. The only settle before a cell's writers
    /// are checked, where a ⊥-fed cell may still take a writer's type.
    ///
    /// [witness]: Self::witness
    pub fn settle_witness(&self, env: &Env) -> Result<()> {
        if self.earlier_task() {
            return Ok(());
        }
        match self.witness(env)? {
            Ok(w) => {
                if graphix_dbg_bind() {
                    eprintln!("SETTLE '{}({:x}) := {w:?}", self.name, self.cell_addr());
                }
                self.bind(w);
                Ok(())
            }
            Err(false) => Ok(()),
            Err(true) => {
                let cons = self.cell_constraints();
                format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                    let mut cs: LPooled<String> = LPooled::take();
                    for (i, c) in cons.iter().enumerate() {
                        if i > 0 {
                            cs.push_str(" & ");
                        }
                        write!(cs, "{c}")?;
                    }
                    let name = super::tvar::ShortName(&self.name);
                    bail!("unsatisfiable constraints on '{name}: {}", &*cs)
                })
            }
        }
    }

    /// [`Self::settle`], but a cell still unbound binds to ⊥: nothing was
    /// produced into it, and what bounds it (a trait a dispatch on it
    /// needs, a predicate) has no witness, so a dispatch on it is a
    /// standing bottom. A cell a type-directed operation reads or calls is
    /// refused instead. Only the terminal walk uses this (an earlier call
    /// would foreclose writers not yet typechecked).
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        if !self.cell_constraints().is_empty() || !self.cell_lower().is_empty() {
            self.settle(env)?;
        }
        self.settle_bottom()
    }

    /// Bind an open cell whose values are all built: to what bounds it
    /// from below, else to its conjunction's witness, else to ⊥, since
    /// nothing of it was built ([`settle_input`]).
    fn settle_built(&self, env: &Env) -> Result<()> {
        let fed = self.cell().read().facts.contains(CellFact::BottomFed)
            && self
                .cell_lower()
                .iter()
                .all(|l| l.with_deref(|l| matches!(l, Some(Type::Bottom))));
        if !fed && let Ok(w) = self.witness(env)? {
            self.bind(w);
            return Ok(());
        }
        if self.cell().read().facts.contains(CellFact::CycleRefused) {
            bail!(InfiniteType)
        }
        if graphix_dbg_bind() {
            eprintln!("SETTLE-BUILT '{}({:x})", self.name, self.cell_addr());
        }
        self.bind(Type::Bottom);
        Ok(())
    }

    fn not_concrete(&self) -> anyhow::Error {
        ConcreteRequired(self.clone()).into()
    }

    fn not_function(&self) -> anyhow::Error {
        anyhow::anyhow!(
            "the type '{} must be a function here, an operation reads its signature: \
             annotate it",
            super::tvar::ShortName(&self.name)
        )
    }

    /// Bind an unbound cell nothing will read to ⊥, whatever its bounds; a
    /// bound one is untouched.
    pub fn settle_unread(&self) {
        let cell = self.cell();
        let mut cell = cell.write();
        if cell.binding.is_none() && !super::tvar::earlier_task(&cell) {
            cell.binding = Some(Type::Bottom);
        }
    }

    /// Bind an unbound cell to ⊥; a bound one is untouched.
    fn settle_bottom(&self) -> Result<()> {
        let cell = self.cell();
        let mut cell = cell.write();
        if cell.binding.is_some() || super::tvar::earlier_task(&cell) {
            return Ok(());
        }
        if cell.constraints.iter().any(|c| matches!(c, Type::Bound(Bound::Concrete))) {
            drop(cell);
            return Err(self.not_concrete());
        }
        if cell.constraints.iter().any(|c| matches!(c, Type::Bound(Bound::Function))) {
            drop(cell);
            return Err(self.not_function());
        }
        // The only solution was infinite; ⊥ would be a lie.
        if cell.facts.contains(CellFact::CycleRefused) {
            if graphix_dbg_bind() {
                eprintln!("SETTLE-INFINITE '{}({:x})", self.name, self.cell_addr());
            }
            bail!(InfiniteType)
        }
        if graphix_dbg_bind() {
            eprintln!("SETTLE-BOTTOM '{}({:x})", self.name, self.cell_addr());
        }
        if graphix_dbg_bind_bt_id().is_some() {
            eprintln!("{}", std::backtrace::Backtrace::force_capture());
        }
        cell.binding = Some(Type::Bottom);
        Ok(())
    }
}

impl Type {
    /// The open cells `self` holds as positions, when each has a lower
    /// bound: a reading of the type ([`Self::frozen`] reads them as their
    /// bounds) that an operation reading it decides by settling them.
    /// `None` when there is none or one has no lower bound.
    pub(crate) fn lowered_cells(&self) -> Option<LPooled<Vec<TVar>>> {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        position_cells(self, &mut cells);
        let mut open: LPooled<Vec<TVar>> = LPooled::take();
        for tv in cells.values().filter(|tv| !tv.is_bound() && !tv.is_rigid()) {
            if tv.cell_lower().is_empty() {
                return None;
            }
            open.push(tv.clone());
        }
        (!open.is_empty()).then_some(open)
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
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            let addr = tv.cell_addr();
            if let Some(&i) = index.get(&addr) {
                out.push(i);
            } else if visited.insert(addr) {
                let (bound, cons, below) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
                };
                for t in bound.iter().chain(cons.iter()).chain(below.iter()) {
                    settle_refs(t, index, visited, out);
                }
            }
        }
        // a shared node's address is another allocation than any cell's
        t if t.shared_identity().is_some_and(|n| !visited.insert(n)) => (),
        Type::Fn(ft) => ft.for_each_type(&mut |t| settle_refs(t, index, visited, out)),
        t => t.for_each_child(&mut |c| settle_refs(c, index, visited, out)),
    })
}

impl FnType {
    /// Terminal settle of a call site's resolved signature in
    /// dependency order: a member settles only after every member its
    /// binding or constraints reach. Ordering keys are (name, TVarId)
    /// only — `cell_addr` is ASLR-dependent and used for identity alone.
    /// `exempt` cells, and every cell in `kept` (what the enclosing
    /// definitions' signatures reach now), are ordered but not settled.
    pub fn settle_terminal(
        &self,
        env: &Env,
        rtype_cell: Option<&TVar>,
        exempt: &AHashSet<usize>,
        kept: &[&AHashSet<usize>],
    ) -> Result<()> {
        let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut tvs);
        // the cells a binding holds are positions this site reads too
        let mut positional: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.for_each_part(&mut |t, constraint| {
            if !constraint {
                position_cells(t, &mut positional)
            }
        });
        let mut seen: LPooled<AHashSet<usize>> =
            tvs.values().map(|tv| tv.cell_addr()).collect();
        let rtype_cell = rtype_cell.filter(|tv| seen.insert(tv.cell_addr()));
        let under = positional.values().filter(|tv| seen.insert(tv.cell_addr()));
        let named =
            rtype_cell.into_iter().chain(under).map(|tv| (tv.name.clone(), tv.clone()));
        let nodes = super::fntyp::sorted_tvars(tvs.drain().chain(named));
        let mut index: LPooled<AHashMap<usize, usize>> = LPooled::take();
        for (i, (_, tv)) in nodes.iter().enumerate() {
            index.insert(tv.cell_addr(), i);
        }
        let mut edges: LPooled<Vec<SmallVec<[usize; 4]>>> = LPooled::take();
        for (_, tv) in nodes.iter() {
            let mut out: SmallVec<[usize; 4]> = SmallVec::new();
            let mut visited: LPooled<AHashSet<usize>> = LPooled::take();
            let (bound, cons, below) = {
                let cell = tv.cell();
                let cell = cell.read();
                (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
            };
            for t in bound.iter().chain(cons.iter()).chain(below.iter()) {
                settle_refs(t, &index, &mut visited, &mut out);
            }
            out.sort_unstable();
            out.dedup();
            edges.push(out);
        }
        // a post-order over the dependency edges, on a stack of its own:
        // the program sets how long a chain of bounds is
        let mut seen: LPooled<Vec<bool>> = LPooled::take();
        seen.resize(nodes.len(), false);
        let mut order: LPooled<Vec<usize>> = LPooled::take();
        let mut stack: LPooled<Vec<(usize, usize)>> = LPooled::take();
        for root in 0..nodes.len() {
            if seen[root] {
                continue;
            }
            seen[root] = true;
            stack.push((root, 0));
            while let Some(&(i, k)) = stack.last() {
                match edges[i].get(k) {
                    Some(&j) => {
                        stack.last_mut().expect("nonempty").1 += 1;
                        if !seen[j] {
                            seen[j] = true;
                            stack.push((j, 0));
                        }
                    }
                    None => {
                        order.push(i);
                        stack.pop();
                    }
                }
            }
        }
        // an enclosing definition's signature is settled by each call,
        // what it reaches as it stands now; a quantifier of a function
        // type the signature holds is its callers' to pick
        let mut quantifiers: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.inner_quantifiers(&mut quantifiers);
        let owned: LPooled<Vec<TVar>> = quantifiers.values().cloned().collect();
        for tv in owned.iter() {
            for c in tv.cell_constraints().iter() {
                reached_cells(c, &mut quantifiers)
            }
        }
        // a `Concrete` or `Function` cell met only inside a conjunct is no
        // position of the signature: nothing reads it, so it is left as
        // it stands
        for i in order.drain(..) {
            let tv = &nodes[i].1;
            let addr = tv.cell_addr();
            if exempt.contains(&addr)
                || quantifiers.contains_key(&addr)
                || kept.iter().any(|k| k.contains(&addr))
                || (!positional.contains_key(&addr)
                    && (tv.requires_concrete() || tv.requires_function()))
            {
                continue;
            }
            tv.settle_or_bottom(env)?;
        }
        Ok(())
    }
}

impl FnType {
    /// Every cell the signature reaches, through bindings and conjuncts,
    /// by address.
    #[doc(hidden)]
    pub fn reached_cells(&self, out: &mut AHashMap<usize, TVar>) {
        let mut nodes: LPooled<AddrSet> = LPooled::take();
        self.for_each_type(&mut |t| reached(t, out, &mut nodes))
    }
}

/// The end of the check of an input (a REPL input, a script, a dynamic
/// load), over `types`, the types of what it made that a later input can
/// reach: every open top-level cell of the input's own they reach settles
/// as a terminal settle does, to ⊥ where nothing bounds it, and is
/// [`CellFact::Settled`]. A scheme's cells stay open, as do a quantifier of
/// a function type they hold and what its bound reaches: each use copies
/// them. Each part is walked once, whichever type reaches it first; a
/// refusal comes with the index of that type.
pub fn settle_input(
    env: &Env,
    types: &[&Type],
) -> std::result::Result<(), (usize, anyhow::Error)> {
    let mut seen: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    let mut nodes: LPooled<AddrSet> = LPooled::take();
    let mut reached: LPooled<Vec<(usize, TVar)>> = LPooled::take();
    let mut walked: LPooled<AHashSet<usize>> = LPooled::take();
    let mut quantifiers: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    for (i, t) in types.iter().enumerate() {
        reach_new(t, i, &mut seen, &mut nodes, &mut reached);
        t.quantifiers_seen(&mut walked, &mut |_, tv| {
            if let Some(open) = tv.open_cell() {
                quantifiers.insert(open.cell_addr(), open);
            }
        });
    }
    let owned: LPooled<Vec<TVar>> = quantifiers.values().cloned().collect();
    for tv in owned.iter() {
        for c in tv.cell_constraints().iter() {
            reached_cells(c, &mut quantifiers)
        }
    }
    let top = |tv: &TVar| {
        tv.level() == Level::Top
            && !tv.earlier_task()
            && !quantifiers.contains_key(&tv.cell_addr())
    };
    for (i, tv) in reached.iter().filter(|(_, tv)| top(tv)) {
        if !tv.is_bound() && !tv.is_rigid() {
            tv.settle_built(env).map_err(|e| (*i, e))?
        }
    }
    reached.iter().filter(|(_, tv)| top(tv)).for_each(|(_, tv)| tv.mark_settled());
    Ok(())
}

/// [`reached`], each cell met for the first time pushed with `owner`.
fn reach_new(
    t: &Type,
    owner: usize,
    seen: &mut AHashMap<usize, TVar>,
    nodes: &mut AddrSet,
    out: &mut Vec<(usize, TVar)>,
) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if seen.insert(tv.cell_addr(), tv.clone()).is_none() {
                out.push((owner, tv.clone()));
                let (bound, cons, below) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
                };
                for t in bound.iter().chain(cons.iter()).chain(below.iter()) {
                    reach_new(t, owner, seen, nodes, out)
                }
            }
        }
        t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
        Type::Fn(ft) => ft.for_each_type(&mut |t| reach_new(t, owner, seen, nodes, out)),
        t => t.for_each_child(&mut |c| reach_new(c, owner, seen, nodes, out)),
    })
}

impl Type {
    /// [`FnType::reached_cells`] for a type.
    #[doc(hidden)]
    pub fn reached_cells(&self, out: &mut AHashMap<usize, TVar>) {
        reached_cells(self, out)
    }

    /// The type variable, first by name, that stands in this type outside
    /// every function type, bound or not (a declaration's variable, whatever
    /// a use has bound it to): a typedef as its expansion, an abstract
    /// type's parameters included. A typedef met again inside itself over
    /// other parameters, or one that does not resolve, stands its
    /// parameters there.
    #[doc(hidden)]
    pub fn variable_outside_functions(&self, env: &Env) -> Option<TVar> {
        let mut out: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        OutsideFunctions { env, expanding: Expanding::default(), nodes: LPooled::take() }
            .walk(self, &mut out);
        out.values().min_by(|a, b| a.name.cmp(&b.name)).cloned()
    }
}

/// A union a written function type holds, under any constructor or
/// typedef's expansion, with two or more of the variables `own` admits as
/// members of their own: which of them a value takes, no type says
/// ([`Type::variable_union`]).
#[derive(Debug)]
pub struct VariableUnion {
    pub union: Type,
    pub vars: SmallVec<[ArcStr; 4]>,
}

impl fmt::Display for VariableUnion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is a union of the type variables ", self.union)?;
        for (i, v) in self.vars.iter().enumerate() {
            let sep = match i {
                0 => "",
                i if i + 1 == self.vars.len() => " and ",
                _ => ", ",
            };
            write!(f, "{sep}'{v}")?
        }
        write!(
            f,
            " in a function's signature: no value's type says which of them it takes; keep \
             one variable there, or write the types in their place (a union of \
             written types, such as [i64, string])"
        )
    }
}

impl Error for VariableUnion {}

impl Type {
    /// Refuse the first union a function type this written type holds
    /// (itself included) has in a part, its parameters, result, throws and
    /// bounds, under any constructor or typedef's expansion, two or more of
    /// whose members are distinct variables `own` admits.
    #[doc(hidden)]
    pub fn variable_union(
        &self,
        env: &Env,
        own: &dyn Fn(&TVar) -> bool,
    ) -> Result<(), VariableUnion> {
        if !may_unite(self, own, &ConjunctsPassed::take()) {
            return Ok(());
        }
        let passed = ConjunctsPassed::take();
        let mut walk = VariableUnions {
            env,
            own,
            passed: &passed,
            in_fn: false,
            expanding: Expanding::default(),
            nodes: LPooled::take(),
        };
        match walk.walk(self) {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }
}

/// [`Type::variable_union`]'s walk: each shared composite once.
struct VariableUnions<'a> {
    env: &'a Env,
    own: &'a dyn Fn(&TVar) -> bool,
    passed: &'a ConjunctsPassed,
    /// Whether the walk is inside a function type.
    in_fn: bool,
    expanding: Expanding,
    nodes: LPooled<AddrSet>,
}

impl VariableUnions<'_> {
    fn walk(&mut self, t: &Type) -> Option<VariableUnion> {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(_) => None,
            t if t.shared_identity().is_some_and(|n| !self.nodes.insert(n)) => None,
            Type::Fn(ft) => {
                let outside = !mem::replace(&mut self.in_fn, true);
                let mut found = None;
                let passed = self.passed;
                ft.for_each_part_once(passed, &mut |t, _| {
                    if found.is_none() {
                        found = self.walk(t)
                    }
                });
                if outside {
                    self.in_fn = false
                }
                found
            }
            Type::Ref(tr) => {
                if !tr.params.iter().any(|p| may_unite(p, self.own, self.passed)) {
                    if bare_vars(&tr.params, self.own) < 2 {
                        return None;
                    }
                    // only a union of the body's own parameters can become one
                    if let Some(def) = tr.resolve_peek(self.env) {
                        let param =
                            |tv: &TVar| def.params.iter().any(|(p, _)| p.name == tv.name);
                        if !may_unite(&def.typ, &param, &ConjunctsPassed::take()) {
                            return None;
                        }
                    }
                }
                match t.lookup_ref_probe(self.env) {
                    Some((key, whole)) => match self.expanding.meets(key, &tr.params) {
                        Meeting::Again => None,
                        Meeting::Grown => tr.params.iter().find_map(|p| self.walk(p)),
                        Meeting::Fresh => {
                            self.expanding.0.push((key, tr.params.clone()));
                            let found = self.walk(&whole);
                            self.expanding.0.pop();
                            found
                        }
                    },
                    None => tr.params.iter().find_map(|p| self.walk(p)),
                }
            }
            t => {
                if self.in_fn
                    && let Type::Set(ms) = t
                    && bare_vars(ms, self.own) > 1
                {
                    let mut vars: SmallVec<[ArcStr; 4]> = SmallVec::new();
                    for m in ms.iter() {
                        if let Type::TVar(tv) = m
                            && (self.own)(tv)
                            && !vars.contains(&tv.name)
                        {
                            vars.push(tv.name.clone())
                        }
                    }
                    return Some(VariableUnion { union: t.clone(), vars });
                }
                let mut found = None;
                t.for_each_child(&mut |c| {
                    if found.is_none() {
                        found = self.walk(c)
                    }
                });
                found
            }
        })
    }
}

/// How many distinct variables `own` admits `ts` holds as members of
/// their own (a union's members, a typedef's parameters).
fn bare_vars(ts: &[Type], own: &dyn Fn(&TVar) -> bool) -> usize {
    let mut vars: SmallVec<[&ArcStr; 4]> = SmallVec::new();
    for t in ts {
        if let Type::TVar(tv) = t
            && own(tv)
            && !vars.contains(&&tv.name)
        {
            vars.push(&tv.name)
        }
    }
    vars.len()
}

/// Whether `t`, as written, holds a union of two of the variables `own`
/// admits, or a typedef applied to two of them, the only ways a
/// [`VariableUnions`] walk can find one: a typedef's body reaches no
/// variable its parameters do not hold.
fn may_unite(t: &Type, own: &dyn Fn(&TVar) -> bool, passed: &ConjunctsPassed) -> bool {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(_) => false,
        Type::Set(ms) if bare_vars(ms, own) > 1 => true,
        Type::Ref(tr) if bare_vars(&tr.params, own) > 1 => true,
        Type::Fn(ft) => {
            let mut any = false;
            ft.for_each_part_once(passed, &mut |c, _| {
                any = any || may_unite(c, own, passed)
            });
            any
        }
        t => {
            let mut any = false;
            t.for_each_child(&mut |c| any = any || may_unite(c, own, passed));
            any
        }
    })
}

/// The typedefs a walk is expanding, by key, with their parameters.
#[derive(Default)]
struct Expanding(SmallVec<[(usize, triomphe::Arc<[Type]>); 4]>);

/// How a walk meets a reference to a typedef ([`Expanding::meets`]).
enum Meeting {
    /// Not being expanded.
    Fresh,
    /// Inside its own expansion over its own parameters.
    Again,
    /// Inside its own expansion over other parameters.
    Grown,
}

impl Expanding {
    fn meets(&self, key: usize, params: &[Type]) -> Meeting {
        match self.0.iter().find(|(k, _)| *k == key) {
            None => Meeting::Fresh,
            Some((_, p))
                if p.len() == params.len()
                    && p.iter()
                        .zip(params.iter())
                        .all(|(p, q)| strictly_identical(p, q)) =>
            {
                Meeting::Again
            }
            Some(_) => Meeting::Grown,
        }
    }
}

/// [`Type::variable_outside_functions`]' walk: each shared composite once.
struct OutsideFunctions<'a> {
    env: &'a Env,
    expanding: Expanding,
    nodes: LPooled<AddrSet>,
}

impl OutsideFunctions<'_> {
    fn walk(&mut self, t: &Type, out: &mut AHashMap<usize, TVar>) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                out.insert(tv.cell_addr(), tv.clone());
            }
            t if t.shared_identity().is_some_and(|n| !self.nodes.insert(n)) => (),
            Type::Fn(_) => (),
            Type::Ref(tr) => match t.lookup_ref_probe(self.env) {
                Some((key, whole)) => match self.expanding.meets(key, &tr.params) {
                    Meeting::Again => (),
                    Meeting::Grown => tr.params.iter().for_each(|p| self.walk(p, out)),
                    Meeting::Fresh => {
                        self.expanding.0.push((key, tr.params.clone()));
                        self.walk(&whole, out);
                        self.expanding.0.pop();
                    }
                },
                None => tr.params.iter().for_each(|p| self.walk(p, out)),
            },
            t => t.for_each_child(&mut |c| self.walk(c, out)),
        })
    }
}

fn reached_cells(t: &Type, out: &mut AHashMap<usize, TVar>) {
    reached(t, out, &mut LPooled::take())
}

/// `reached_cells`, each shared composite walked once (`nodes`).
fn reached(t: &Type, out: &mut AHashMap<usize, TVar>, nodes: &mut AddrSet) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if out.insert(tv.cell_addr(), tv.clone()).is_none() {
                let (bound, cons, below) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
                };
                for t in bound.iter().chain(cons.iter()).chain(below.iter()) {
                    reached(t, out, nodes)
                }
            }
        }
        t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
        Type::Fn(ft) => ft.for_each_type(&mut |t| reached(t, out, nodes)),
        t => t.for_each_child(&mut |c| reached(c, out, nodes)),
    })
}

/// The cells `t` holds as positions, through bindings, never through
/// conjuncts.
#[doc(hidden)]
pub fn position_cells(t: &Type, out: &mut AHashMap<usize, TVar>) {
    fn go(t: &Type, out: &mut AHashMap<usize, TVar>, nodes: &mut AHashSet<usize>) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if out.insert(tv.cell_addr(), tv.clone()).is_none()
                    && let Some(b) = tv.binding()
                {
                    go(&b, out, nodes)
                }
            }
            t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
            Type::Fn(ft) => ft.for_each_part(&mut |t, constraint| {
                if !constraint {
                    go(t, out, nodes)
                }
            }),
            t => t.for_each_child(&mut |c| go(c, out, nodes)),
        })
    }
    go(t, out, &mut LPooled::take())
}

impl Type {
    /// This type fixed for a value to carry: every cell resolved, an open
    /// one to its bounds' witness, else ⊥ (a value minted where the check
    /// left a parameter undecided carries no cell a later check could
    /// still decide).
    pub fn frozen(&self, env: &Env) -> Type {
        /// Each cell's fixed type, by address, with the cell held so a
        /// witness's fresh cell keeps its address for the walk.
        type Memo = AHashMap<usize, (TVar, Type)>;
        /// `None` where nothing beneath holds a cell.
        fn go(t: &Type, env: &Env, memo: &mut Memo) -> Option<Type> {
            crate::stack::ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    let addr = tv.cell_addr();
                    if let Some((_, t)) = memo.get(&addr) {
                        return Some(t.clone());
                    }
                    // a cell met again inside its own binding is ⊥ there
                    memo.insert(addr, (tv.clone(), Type::Bottom));
                    let fixed = |t: Type, memo: &mut Memo| go(&t, env, memo).unwrap_or(t);
                    let r = match tv.binding() {
                        Some(b) => fixed(b, memo),
                        None => match tv.witness(env) {
                            Ok(Ok(w)) => fixed(w, memo),
                            _ => Type::Bottom,
                        },
                    };
                    memo.insert(addr, (tv.clone(), r.clone()));
                    Some(r)
                }
                t => t.cow_children(&mut |c| go(c, env, memo)),
            })
        }
        go(self, env, &mut LPooled::take()).unwrap_or_else(|| self.clone())
    }
}
