//! The settle phase: open cells that inference left constrained bind to
//! their conjunction's witness, and a call site's terminal settle binds
//! what nothing produced or bounded to ⊥.

use crate::{
    AddrSet, PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_bind_bt_id},
    env::Env,
    format_with_flags,
    typ::{
        Bound, FnType, TVar, Type, Written,
        contains::{ContainsHist, InfiniteType},
        setops::strictly_identical,
        tvar::{CellFact, Fresh, Level, would_cycle_open},
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
    /// self-referential, or a set only `Singleton` refuses: a member would
    /// do, and no walk here can choose it). Bound and unbounded cells have
    /// none.
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
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    // `Singleton` refuses the set, not its members: one of
                    // them may still fit
                    candidates |= !matches!(o, Type::Bound(Bound::Singleton));
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
        let mut more: LPooled<Vec<TVar>> = LPooled::take();
        more.extend(rtype_cell.filter(|tv| seen.insert(tv.cell_addr())).cloned());
        more.extend(
            positional.values().filter(|tv| seen.insert(tv.cell_addr())).cloned(),
        );
        // a cell only a bound of a bound names is the site's to settle too
        let mut todo: LPooled<Vec<TVar>> = LPooled::take();
        todo.extend(tvs.values().chain(more.iter()).cloned());
        let mut walked: LPooled<AHashSet<usize>> = LPooled::take();
        let mut nodes: LPooled<AHashSet<usize>> = LPooled::take();
        while let Some(tv) = todo.pop() {
            for c in tv.cell_constraints().iter() {
                walk_positions(c, &mut nodes, &mut |tv| {
                    let addr = tv.cell_addr();
                    let first = walked.insert(addr);
                    if first && seen.insert(addr) {
                        todo.push(tv.clone());
                        more.push(tv.clone());
                    }
                    first
                })
            }
        }
        let named = more.drain(..).map(|tv| (tv.name.clone(), tv));
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
        self.variables_outside_functions(env, &mut out);
        out.values().min_by(|a, b| a.name.cmp(&b.name)).cloned()
    }

    /// Every type variable [`Self::variable_outside_functions`] finds,
    /// keyed by its cell.
    #[doc(hidden)]
    pub fn variables_outside_functions(
        &self,
        env: &Env,
        out: &mut AHashMap<usize, TVar>,
    ) {
        OutsideFunctions { env, expanding: Expanding::default(), nodes: LPooled::take() }
            .walk(self, out);
    }
}

/// A union written in a function type with two or more of the variables
/// `own` admits as members of their own, or two members of one constructor
/// that hold two distinct such variables: which of them a value takes, no
/// type says ([`Type::variable_union`]).
#[derive(Debug)]
pub struct VariableUnion {
    pub union: Type,
    pub vars: SmallVec<[ArcStr; 4]>,
    /// The two members of one constructor that hold `vars`, where the
    /// union's members are not the variables themselves.
    pub members: Option<(Type, Type)>,
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
        match &self.members {
            None => write!(
                f,
                " in a function's signature: no value's type says which of them it takes; \
                 keep one variable there, or write the types in their place (a union of \
                 written types, such as [i64, string])"
            ),
            Some((a, b)) => write!(
                f,
                ", held by its members {a} and {b} under one constructor, in a function's \
                 signature: no value's type says which member it takes; write one variable \
                 in both, or a written type in place of one of them"
            ),
        }
    }
}

impl Error for VariableUnion {}

impl VariableUnion {
    /// The written union `union` refused where two or more of its members
    /// are distinct variables `own` admits, or two members of one
    /// constructor ([`same_constructor`]) hold two distinct ones.
    #[doc(hidden)]
    pub fn of(union: &Type, own: &dyn Fn(&TVar) -> bool) -> Option<Self> {
        let Type::Set(ms) = union else { return None };
        let mut vars: SmallVec<[&TVar; 4]> = SmallVec::new();
        for m in ms.iter() {
            if let Type::TVar(tv) = m
                && own(tv)
                && !vars.iter().any(|v| v.name == tv.name)
            {
                vars.push(tv)
            }
        }
        if vars.len() >= 2 {
            let mut vars: SmallVec<[&ArcStr; 4]> =
                vars.iter().map(|tv| &tv.name).collect();
            vars.sort();
            return Some(VariableUnion {
                union: union.clone(),
                vars: vars.into_iter().cloned().collect(),
                members: None,
            });
        }
        let held: SmallVec<[(&Type, LPooled<Vec<ArcStr>>); 4]> = ms
            .iter()
            .filter(|m| !matches!(m, Type::TVar(_)))
            .map(|m| {
                let mut vs = LPooled::take();
                own_vars(m, own, &mut vs);
                (m, vs)
            })
            .filter(|(_, vs)| !vs.is_empty())
            .collect();
        for (i, (a, avs)) in held.iter().enumerate() {
            for (b, bvs) in held[i + 1..].iter() {
                if !same_constructor(a, b) {
                    continue;
                }
                let distinct = avs
                    .iter()
                    .find(|v| !bvs.contains(v))
                    .zip(bvs.iter().find(|w| !avs.contains(w)))
                    .or_else(|| {
                        avs.iter()
                            .find_map(|v| bvs.iter().find(|w| *w != v).map(|w| (v, w)))
                    });
                if let Some((v, w)) = distinct {
                    let (v, w) = if v <= w { (v, w) } else { (w, v) };
                    return Some(VariableUnion {
                        union: union.clone(),
                        vars: [v.clone(), w.clone()].into_iter().collect(),
                        members: Some(((*a).clone(), (*b).clone())),
                    });
                }
            }
        }
        None
    }
}

/// The distinct variables `own` admits that the written type `t` names,
/// through every constructor, no typedef looked through.
fn own_vars(t: &Type, own: &dyn Fn(&TVar) -> bool, out: &mut Vec<ArcStr>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if own(tv) && !out.contains(&tv.name) {
                out.push(tv.name.clone())
            }
        }
        t => t.for_each_child(&mut |c| own_vars(c, own, out)),
    })
}

// CR claude for claude: [bug/low] A written union of two function types whose
// variables pair by renaming merges into one member as the parser flattens it
// (Type::flatten_set -> union_identical, which pairs a function type's cells as its
// own quantifiers), though in a signature they are the definition's: `|k: [fn(x:
// 'c) -> i64, fn(x: 'd) -> i64], y: 'c, z: 'd| -> i64 0` keeps `fn(x: 'd) -> i64`
// alone, so VariableUnion::of never sees the two members the source writes, and
// `f(|x: i64| 0, 1, "s")` is refused naming nothing ("'d: i64 does not contain
// string"); `Array<fn(x: 'c) -> i64>` beside `Array<fn(x: 'd) -> i64>` merges alike.
// Main 3cfcd2407 alike. A fix keeps a written union's members apart where their
// cells are a definition's (or judges the union before it flattens). Probe:
// ~/ptmp/cr29/scratch/r19/19g/fnmerge.gx (formal-union-fn-merge-01)
// 2026-11-10 claude: D8 follow-up 2 was ruled (c) on 2026-11-10
// (formal-union-order-01): the order-dependent pick among a union's undecided
// members is refused where contains makes it, which may make Type::variable_union
// redundant. Re-check this CR against that build: whether the merged member still
// hides two written members from the new refusal.

// CR claude for claude: [structure/low] Three copies of one judgment: this same_constructor
// repeats callsite.rs's same_constructor arm for arm (adding Abstract, Ref by name and App), and
// setops.rs's same_shape repeats the tuple, struct, variant, array, list and map arms, differing in
// ByRef mutability and in leaving out Error and Fn. They already disagree on `&T` against `&mut T`.
// One helper in graphix-types with the variants it needs, called from all three.
// (one-constructor-dup-01)

/// Whether two written union members are built by one constructor, so a
/// value of the one may be a value of the other: two arrays, lists, maps,
/// errors, references or function types; tuples of one arity; structs of
/// one field set; variants of one tag and arity; one abstract type; one
/// typedef by its written name; one constructor variable applied (`self<'c>`).
fn same_constructor(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Array(_), Type::Array(_))
        | (Type::List(_), Type::List(_))
        | (Type::Map(_), Type::Map(_))
        | (Type::Error(_), Type::Error(_))
        | (Type::ByRef(..), Type::ByRef(..))
        | (Type::Fn(_), Type::Fn(_)) => true,
        (Type::Tuple(a), Type::Tuple(b)) => a.len() == b.len(),
        (Type::Struct(a), Type::Struct(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.0 == y.0)
        }
        (Type::Variant(t, a, _), Type::Variant(u, b, _)) => t == u && a.len() == b.len(),
        (Type::Abstract { id: a, .. }, Type::Abstract { id: b, .. }) => a == b,
        (Type::Ref(a), Type::Ref(b)) => a.scope == b.scope && a.name == b.name,
        (Type::App(f, _), Type::App(g, _)) => {
            matches!((&**f, &**g), (Type::TVar(f), Type::TVar(g)) if f.name == g.name)
        }
        _ => false,
    }
}

impl Type {
    /// Refuse the first union written in a function type this written type
    /// holds (itself included), in a part, its parameters, result, throws
    /// and its variables' bounds, under any constructor, two or more of
    /// whose members are distinct variables `own` admits
    /// ([`Type::for_each_written`]). A typedef is not looked through.
    #[doc(hidden)]
    pub fn variable_union(
        &self,
        own: &dyn Fn(&TVar) -> bool,
    ) -> Result<(), VariableUnion> {
        self.variable_union_in(false, own)
    }

    /// [`Self::variable_union`] of a written part of a function type: a
    /// parameter's, the result's, the throws' or a bound's type.
    #[doc(hidden)]
    pub fn signature_variable_union(
        &self,
        own: &dyn Fn(&TVar) -> bool,
    ) -> Result<(), VariableUnion> {
        self.variable_union_in(true, own)
    }

    fn variable_union_in(
        &self,
        in_fn: bool,
        own: &dyn Fn(&TVar) -> bool,
    ) -> Result<(), VariableUnion> {
        let mut res = Ok(());
        self.for_each_written(in_fn, &mut |w| {
            if res.is_ok()
                && let Written::Union(u) = w
                && let Some(e) = VariableUnion::of(u, own)
            {
                res = Err(e)
            }
        });
        res
    }
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
    walk_positions(t, &mut LPooled::take(), &mut |tv| {
        out.insert(tv.cell_addr(), tv.clone()).is_none()
    })
}

/// Visits the cells `t` holds as positions, entering a cell's binding
/// only when `first` says the cell is met for the first time; `nodes`
/// holds the shared nodes already walked.
fn walk_positions(
    t: &Type,
    nodes: &mut AHashSet<usize>,
    first: &mut dyn FnMut(&TVar) -> bool,
) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if first(tv)
                && let Some(b) = tv.binding()
            {
                walk_positions(&b, nodes, first)
            }
        }
        t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
        Type::Fn(ft) => ft.for_each_part(&mut |t, constraint| {
            if !constraint {
                walk_positions(t, nodes, first)
            }
        }),
        t => t.for_each_child(&mut |c| walk_positions(c, nodes, first)),
    })
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

#[cfg(test)]
mod test {
    use super::*;

    /// A call's terminal settle decides `'a4`, which only the bound of a
    /// bound names: the signature holds no open cell after it.
    #[test]
    fn a_terminal_settle_decides_a_bound_of_a_bound() {
        let (env, t) = Env::typed_at_root(
            &[],
            "fn<'a0: [Array<'a1>, null], 'a1: [Array<'a2>, null], 'a2: [Array<'a3>, null], \
             'a3: [Array<'a4>, null], 'a4: Number>(x: ('a0, 'a1)) -> i64",
        )
        .unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        ft.settle_terminal(&env, None, &AHashSet::default(), &[]).unwrap();
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars_through_bounds(&mut cells);
        let mut open: Vec<&ArcStr> =
            cells.iter().filter(|(_, tv)| !tv.is_bound()).map(|(n, _)| n).collect();
        open.sort();
        assert_eq!(cells.len(), 5, "{cells:?}");
        assert!(open.is_empty(), "open after the settle: {open:?}");
    }
}
