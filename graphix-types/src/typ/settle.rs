//! The settle phase: open cells that inference left constrained bind to
//! their conjunction's witness, and a call site's terminal settle binds
//! what nothing produced or bounded to ⊥.

use crate::{
    AddrSet, PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_bind_bt_id},
    env::Env,
    format_with_flags,
    typ::{
        AppKey, Bound, FnType, TVar, Type, TypeRef, Written,
        contains::{ContainsHist, InfiniteType},
        setops::{Welds, apply_welds, lower_collapses, strictly_identical},
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
    ops::ControlFlow,
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

/// `below` less each decided bound holding ⊥ in a part that another decided
/// bound contains: it adds no value to a witness (`Array<_>` beside
/// `Array<i64>`). Of two that contain each other the first stays.
fn joined_lower<'a>(
    env: &Env,
    hist: &mut ContainsHist,
    below: &'a [Type],
) -> Result<LPooled<Vec<&'a Type>>> {
    let holds_bottom = |l: &Type| {
        l.has_bottom()
            && !l.with_deref(|l| matches!(l, Some(Type::Bottom)))
            && !l.has_unbound()
    };
    let mut kept: LPooled<Vec<&Type>> = LPooled::take();
    let empty = BitFlags::empty();
    for (i, l) in below.iter().enumerate() {
        let mut contained = false;
        if below.len() > 1 && holds_bottom(l) {
            for (j, k) in below.iter().enumerate() {
                if j != i
                    && !k.has_unbound()
                    && k.contains_int(empty, env, hist, l)?
                    && (j < i || !l.contains_int(empty, env, hist, k)?)
                {
                    contained = true;
                    break;
                }
            }
        }
        if !contained {
            kept.push(l)
        }
    }
    Ok(kept)
}

/// What an open cell settles to ([`TVar::own_witness`]).
enum Witness {
    /// The union of its lower bounds.
    Lower(Type),
    /// A conjunct every other conjunct contains.
    Conjunct(Type),
}

/// Whether a conjunct of `holder`, or of an open cell one holds, holds
/// `tv`'s cell.
fn conjuncts_reach(holder: &TVar, tv: &TVar, seen: &mut AddrSet) -> bool {
    crate::stack::ensure_sufficient(|| {
        holder.cell_constraints().iter().any(|c| {
            let found = super::contains::find_open_cell(c, true, &mut |o| {
                o.same_cell(tv)
                    || (seen.insert(o.cell_addr()) && conjuncts_reach(o, tv, seen))
            });
            found.is_some()
        })
    })
}

/// Put `tvs` in the order a run of settles takes them: a cell after those
/// whose conjuncts reach it, since a settle at a cell's lower bounds puts
/// the open cells its conjuncts hold above what it took
/// ([`TVar::settle_witness`]); cells neither of which reaches the other
/// stand by name.
pub fn settle_order(tvs: &mut [TVar]) {
    if tvs.len() < 2 {
        return;
    }
    tvs.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.read().id.cmp(&b.read().id)));
    for i in 0..tvs.len() {
        let left = &tvs[i..];
        let reached = |k: usize| {
            left.iter().enumerate().any(|(j, o)| {
                j != k && conjuncts_reach(o, &left[k], &mut LPooled::take())
            })
        };
        if let Some(k) = (0..left.len()).find(|&k| !reached(k)) {
            tvs[i..=i + k].rotate_right(1)
        }
    }
}

impl Witness {
    /// The witness with a conjunct's generic cells copied and its live
    /// cells shared.
    fn shared(self) -> Type {
        match self {
            Witness::Lower(w) => w,
            Witness::Conjunct(c) => {
                c.reset_tvars_int(&mut LPooled::take(), Fresh::Generic).unwrap_or(c)
            }
        }
    }
}

impl TVar {
    /// [`Self::own_witness`], a conjunct's generic cells copied (a stored
    /// scheme's cells must not join live inference) and its live cells
    /// shared, so it keeps its relations.
    pub(crate) fn witness(&self, env: &Env) -> Result<std::result::Result<Type, bool>> {
        Ok(self.own_witness(env, &mut ContainsHist::new())?.map(Witness::shared))
    }

    /// The union of the cell's lower bounds, else the narrowest conjunct
    /// every other conjunct contains, its cells its own. `Err(true)` when
    /// there is none, `Err(false)` when no conjunct could be one (all
    /// traits or self-referential, or a set only `Singleton` refuses: a
    /// member would do, and no walk here can choose it). Bound and
    /// unbounded cells have none.
    fn own_witness(
        &self,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<std::result::Result<Witness, bool>> {
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
        let cell = self.cell();
        if !below.is_empty() {
            if below.iter().any(|l| would_cycle_open(&cell, l)) {
                return Ok(Err(false));
            }
            let below = joined_lower(env, hist, &below)?;
            let w = Type::union(env, &below)?;
            for c in cons.iter() {
                if !c.contains_int(BitFlags::empty(), env, hist, &w)? {
                    return Ok(Err(true));
                }
            }
            return Ok(Ok(Witness::Lower(w)));
        }
        let mut candidates = false;
        'cand: for c in cons.iter() {
            // A trait conjunct is a predicate, not a binding, and a
            // conjunct reaching this cell has no finite witness.
            if c.is_predicate() || c.is_trait_ref(env) || would_cycle_open(&cell, c) {
                continue;
            }
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, hist, c)? {
                    // `Singleton` refuses the set, not its members: one of
                    // them may still fit
                    candidates |= !matches!(o, Type::Bound(Bound::Singleton));
                    continue 'cand;
                }
            }
            return Ok(Ok(Witness::Conjunct(c.clone())));
        }
        Ok(Err(candidates))
    }

    /// Make one cell of the open cells that alone tell two of this cell's
    /// lower bounds apart ([`lower_collapses`]): a settle's [witness] then
    /// holds those bounds once, where their union would be two members
    /// nothing decides between.
    ///
    /// [witness]: Self::witness
    pub(super) fn weld_lower(&self) {
        if self.is_bound() {
            return;
        }
        let below = self.cell_lower();
        let undecided: LPooled<Vec<&Type>> =
            below.iter().filter(|l| l.has_unbound()).collect();
        let mut welds = Welds::new();
        for (i, l) in undecided.iter().enumerate().skip(1) {
            if undecided[..i].iter().any(|k| lower_collapses(k, l, &mut welds)) {
                apply_welds(&mut welds)
            }
        }
    }

    /// Bind an unbound cell once its writers are checked: to ⊥ when only
    /// ⊥ was produced into it ([`CellFact::BottomFed`], and no lower bound
    /// but ⊥), whatever constrains it, else as [`Self::settle_witness`].
    ///
    /// [`CellFact::BottomFed`]: super::tvar::CellFact::BottomFed
    pub fn settle(&self, env: &Env) -> Result<()> {
        self.settle_in(env, &mut ContainsHist::new())
    }

    /// [`Self::settle`] in `hist`, a walk earlier settles may share.
    fn settle_in(&self, env: &Env, hist: &mut ContainsHist) -> Result<()> {
        if self.cell().read().facts.contains(CellFact::BottomFed) && !self.above_a_type()
        {
            return self.settle_bottom();
        }
        self.settle_witness_in(env, hist)
    }

    /// Whether a lower bound of the cell is more than ⊥.
    fn above_a_type(&self) -> bool {
        let below = self.cell_lower();
        below.iter().any(|l| !l.with_deref(|l| matches!(l, Some(Type::Bottom))))
    }

    /// Bind a constrained-unbound cell to its conjunction's [witness].
    /// Bound and unconstrained cells are untouched. No witness is a type
    /// error, unless no conjunct could be one: then the cell stays open
    /// for its writers to refine. The only settle before a cell's writers
    /// are checked, where a ⊥-fed cell may still take a writer's type.
    ///
    /// [witness]: Self::witness
    pub fn settle_witness(&self, env: &Env) -> Result<()> {
        self.settle_witness_in(env, &mut ContainsHist::new())
    }

    fn settle_witness_in(&self, env: &Env, hist: &mut ContainsHist) -> Result<()> {
        if self.earlier_task() {
            return Ok(());
        }
        self.weld_lower();
        let settled = match self.own_witness(env, hist)? {
            Ok(w) => self.settle_at(env, hist, w)?,
            Err(unsatisfiable) => !unsatisfiable,
        };
        if settled {
            return Ok(());
        }
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

    /// Bind the cell to its witness `w` as a containment binds a cell: at
    /// the union of its lower bounds its conjuncts commit over it first
    /// ([`Self::commit_conjuncts`]), so a cell a conjunct holds stands
    /// above what this one took. False, the cell left open, where a
    /// conjunct does not hold it.
    fn settle_at(&self, env: &Env, hist: &mut ContainsHist, w: Witness) -> Result<bool> {
        if let Witness::Lower(w) = &w
            && !self.commit_conjuncts_in(env, hist, w)?
        {
            return Ok(false);
        }
        self.bind_witness(w.shared());
        Ok(true)
    }

    /// Bind the cell to its [own witness], after the open cells its
    /// conjuncts hold that `seen` does not, so its witness is decided
    /// where theirs are, and a cell `seen` holds is in it as itself; a cell
    /// with no witness stays open.
    ///
    /// [own witness]: Self::own_witness
    pub(super) fn settle_bounds_first(&self, env: &Env, seen: &mut AddrSet) {
        if self.is_bound() || !seen.insert(self.cell_addr()) {
            return;
        }
        crate::stack::ensure_sufficient(|| {
            for c in self.cell_constraints().iter() {
                super::contains::find_open_cell(c, false, &mut |o| {
                    o.settle_bounds_first(env, seen);
                    false
                });
            }
        });
        if self.earlier_task() {
            return;
        }
        self.weld_lower();
        if let Some(w) = self.bounds_witness(env) {
            self.bind_witness(w)
        }
    }

    /// What [`Self::settle_bounds_first`] binds the cell to, its
    /// [own witness]; `None` where it leaves the cell open.
    ///
    /// [own witness]: Self::own_witness
    pub(super) fn bounds_witness(&self, env: &Env) -> Option<Type> {
        match self.own_witness(env, &mut ContainsHist::new()) {
            Ok(Ok(Witness::Lower(w) | Witness::Conjunct(w))) => Some(w),
            _ => None,
        }
    }

    fn bind_witness(&self, w: Type) {
        if graphix_dbg_bind() {
            eprintln!("SETTLE '{}({:x}) := {w:?}", self.name, self.cell_addr());
        }
        self.bind(w)
    }

    /// [`Self::settle`], but a cell still unbound binds to ⊥: nothing was
    /// produced into it, and what bounds it (a trait a dispatch on it
    /// needs, a predicate) has no witness, so a dispatch on it is a
    /// standing bottom. A cell a type-directed operation reads or calls is
    /// refused instead. Only the terminal walk uses this (an earlier call
    /// would foreclose writers not yet typechecked).
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        self.settle_or_bottom_in(env, &mut ContainsHist::new())
    }

    /// [`Self::settle_or_bottom`] in `hist`, a walk earlier settles may
    /// share.
    fn settle_or_bottom_in(&self, env: &Env, hist: &mut ContainsHist) -> Result<()> {
        if !self.cell_constraints().is_empty() || !self.cell_lower().is_empty() {
            self.settle_in(env, hist)?;
        }
        self.settle_bottom()
    }

    /// Bind an open cell whose values are all built: to what bounds it
    /// from below, else to its conjunction's witness, else to ⊥, since
    /// nothing of it was built ([`settle_input`]).
    fn settle_built(&self, env: &Env) -> Result<()> {
        let fed = self.cell().read().facts.contains(CellFact::BottomFed)
            && !self.above_a_type();
        if !fed {
            self.weld_lower();
            if let Ok(w) = self.witness(env)? {
                self.bind(w);
                return Ok(());
            }
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
            cell.set_binding(Some(Type::Bottom));
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
        cell.set_binding(Some(Type::Bottom));
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
/// (their reach is their own edge list) and at cells proved ground, which
/// reach no open member.
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
            } else if !tv.ground_now() && visited.insert(addr) {
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
    /// binding, constraints or lower bounds reach, but for one that waits
    /// for it ([`TerminalRun::waits`]). Ordering keys are (name, TVarId)
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
        // the cells a binding holds are positions this site reads too; a
        // cell proved ground holds no open one, so it is passed by
        let mut positional: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let mut nodes: LPooled<AHashSet<usize>> = LPooled::take();
        self.for_each_part(&mut |t, constraint| {
            if !constraint {
                walk_positions(t, &mut nodes, &mut |tv| {
                    !tv.ground_now()
                        && positional.insert(tv.cell_addr(), tv.clone()).is_none()
                })
            }
        });
        let mut seen: LPooled<AHashSet<usize>> =
            tvs.values().map(|tv| tv.cell_addr()).collect();
        let mut more: LPooled<Vec<TVar>> = LPooled::take();
        more.extend(rtype_cell.filter(|tv| seen.insert(tv.cell_addr())).cloned());
        more.extend(
            positional.values().filter(|tv| seen.insert(tv.cell_addr())).cloned(),
        );
        // a cell only a bound of a bound names is the site's to settle too,
        // and so is one a lower bound holds: a position of the witness its
        // cell settles to
        let mut todo: LPooled<Vec<TVar>> = LPooled::take();
        todo.extend(tvs.values().chain(more.iter()).cloned());
        let mut walked: LPooled<AHashSet<usize>> = LPooled::take();
        nodes.clear();
        while let Some(tv) = todo.pop() {
            let below = match exempt.contains(&tv.cell_addr()) {
                true => SmallVec::new(),
                false => tv.cell_lower(),
            };
            for c in tv.cell_constraints().iter().chain(below.iter()) {
                walk_positions(c, &mut nodes, &mut |tv| {
                    let addr = tv.cell_addr();
                    let first = !tv.ground_now() && walked.insert(addr);
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
        let mut holders: LPooled<Vec<SmallVec<[usize; 4]>>> = LPooled::take();
        holders.resize(nodes.len(), SmallVec::new());
        for (i, (_, tv)) in nodes.iter().enumerate() {
            let mut out: SmallVec<[usize; 4]> = SmallVec::new();
            let mut visited: LPooled<AHashSet<usize>> = LPooled::take();
            let (bound, cons, below) = {
                let cell = tv.cell();
                let cell = cell.read();
                (cell.binding.clone(), cell.constraints.clone(), cell.lower.clone())
            };
            for t in cons.iter() {
                settle_refs(t, &index, &mut visited, &mut out);
            }
            if bound.is_none() {
                for &j in out.iter() {
                    if j != i && holders[j].last() != Some(&i) {
                        holders[j].push(i)
                    }
                }
            }
            for t in bound.iter().chain(below.iter()) {
                settle_refs(t, &index, &mut visited, &mut out);
            }
            out.sort_unstable();
            out.dedup();
            edges.push(out);
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
        let left = |tv: &TVar| {
            let addr = tv.cell_addr();
            exempt.contains(&addr)
                || quantifiers.contains_key(&addr)
                || kept.iter().any(|k| k.contains(&addr))
                || (!positional.contains_key(&addr)
                    && (tv.requires_concrete() || tv.requires_function()))
        };
        let mut run = TerminalRun {
            env,
            nodes: &nodes,
            edges: &edges,
            holders: &holders,
            left: &left,
            turns: LPooled::take(),
            feeds: LPooled::take(),
            passed: LPooled::take(),
            hist: ContainsHist::new(),
        };
        run.turns.resize(nodes.len(), Turn::Unmet);
        run.feeds.resize(nodes.len(), Feeds::Unasked);
        run.passed.resize(nodes.len(), 0);
        for i in 0..nodes.len() {
            run.take(i)?
        }
        for i in 0..nodes.len() {
            if run.turns[i] == Turn::Waiting {
                run.settle(i)?
            }
        }
        Ok(())
    }
}

/// Where a cell stands in a terminal settle's run.
#[derive(Clone, Copy, PartialEq)]
enum Turn {
    Unmet,
    /// the cells it reaches are being taken
    Met,
    /// left open for a cell whose conjuncts reach it
    /// ([`TerminalRun::waits`])
    Waiting,
    Settled,
}

/// Whether a cell's settle may put the cells its conjuncts hold above a
/// type ([`TerminalRun::feeds`]).
#[derive(Clone, Copy)]
enum Feeds {
    Unasked,
    Asked,
    No,
    Yes,
}

/// A terminal settle's run over its cells, `nodes`: a cell settles after
/// the cells it reaches, but for one that [waits](Self::waits).
struct TerminalRun<'a> {
    env: &'a Env,
    nodes: &'a [(ArcStr, TVar)],
    /// for each cell, the cells its binding, conjuncts and lower bounds
    /// reach
    edges: &'a [SmallVec<[usize; 4]>],
    /// for each cell, the open cells whose conjuncts reach it
    holders: &'a [SmallVec<[usize; 4]>],
    /// the cells the site orders and does not settle
    left: &'a dyn Fn(&TVar) -> bool,
    turns: LPooled<Vec<Turn>>,
    feeds: LPooled<Vec<Feeds>>,
    /// for each cell, how many of its `holders`, from the first, feed it
    /// no more: one that does not [feed](Self::feeds) never will again
    passed: LPooled<Vec<usize>>,
    /// the walk the run's settles share, so a probe's verdict over a cell
    /// whose reach no settle wrote is not walked again
    hist: ContainsHist,
}

#[cfg(test)]
thread_local! {
    static FEEDS_ASKED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

impl TerminalRun<'_> {
    fn take(&mut self, i: usize) -> Result<()> {
        if self.turns[i] != Turn::Unmet {
            return Ok(());
        }
        self.turns[i] = Turn::Met;
        crate::stack::ensure_sufficient(|| {
            let edges = self.edges;
            for &j in edges[i].iter() {
                self.take(j)?
            }
            if self.waits(i) {
                self.turns[i] = Turn::Waiting;
                return Ok(());
            }
            self.settle(i)
        })
    }

    /// Settle cell `i`, then the cells that waited for it alone.
    fn settle(&mut self, i: usize) -> Result<()> {
        self.turns[i] = Turn::Settled;
        let tv = &self.nodes[i].1;
        if !(self.left)(tv) {
            tv.settle_or_bottom_in(self.env, &mut self.hist)?
        }
        crate::stack::ensure_sufficient(|| {
            let edges = self.edges;
            for &j in edges[i].iter() {
                if self.turns[j] == Turn::Waiting && !self.waits(j) {
                    self.settle(j)?
                }
            }
            Ok(())
        })
    }

    /// Whether cell `i` settles after a cell whose conjuncts reach it. A
    /// cell that stands above a type commits its conjuncts over it when it
    /// settles ([`TVar::settle_witness`]), which puts the cells they hold
    /// above their part of it, and a cell settled before that has no part.
    /// So a cell that would settle to ⊥ or to a type holding an open cell
    /// (it is open, above no type, and a ⊥ was produced into it or its
    /// conjuncts are no closed type) waits for the cells that may
    /// [feed](Self::feeds) it. Any other settles first, so its holder
    /// holds it decided.
    fn waits(&mut self, i: usize) -> bool {
        let tv = &self.nodes[i].1;
        if self.holders[i].is_empty() || tv.is_bound() || tv.above_a_type() {
            return false;
        }
        if !tv.bottom_fed() {
            let mut types = tv.cell_constraints();
            types.retain(|c| !c.is_predicate() && !c.is_trait_ref(self.env));
            if !types.is_empty() && !types.iter().any(|c| c.has_unbound()) {
                return false;
            }
        }
        let holders = self.holders;
        while let Some(&h) = holders[i].get(self.passed[i]) {
            if self.feeds(h) {
                return true;
            }
            self.passed[i] += 1
        }
        false
    }

    /// Whether cell `h`, not yet settled, may put the cells its conjuncts
    /// hold above a type when it settles: it stands above one, or a cell
    /// whose conjuncts reach it may put it above one.
    fn feeds(&mut self, h: usize) -> bool {
        #[cfg(test)]
        FEEDS_ASKED.with(|n| n.set(n.get() + 1));
        if self.turns[h] == Turn::Settled {
            return false;
        }
        match self.feeds[h] {
            Feeds::Yes => true,
            Feeds::No | Feeds::Asked => false,
            Feeds::Unasked => {
                self.feeds[h] = Feeds::Asked;
                let tv = &self.nodes[h].1;
                let yes = !tv.is_bound()
                    && !(self.left)(tv)
                    && (tv.above_a_type()
                        || crate::stack::ensure_sufficient(|| {
                            let holders = self.holders;
                            holders[h].iter().any(|&g| self.feeds(g))
                        }));
                self.feeds[h] = if yes { Feeds::Yes } else { Feeds::No };
                yes
            }
        }
    }
}

impl FnType {
    /// Every cell the signature reaches, through bindings and conjuncts,
    /// by address.
    /// [`FnType::reached_cells`] for a type.
    #[doc(hidden)]
    pub fn reached_cells(&self, out: &mut AHashMap<usize, TVar>) {
        let mut nodes: LPooled<AddrSet> = LPooled::take();
        self.for_each_type(&mut |t| reached(t, out, &mut nodes))
    }

    /// The decided lower bounds of this site's open cells whose conjuncts
    /// reach a cell of `kept` (a definition's signature, left open for its
    /// callers), passed on to the cells those conjuncts hold, as a settle
    /// of the cell does ([`TVar::commit_conjuncts`]): a caller of the
    /// definition then meets a kept cell above what the site put there, not
    /// above nothing until the site settles. Each cell passes before the
    /// cells its conjuncts hold, so a chain passes down it. An error where
    /// a conjunct does not hold a lower bound: the signature stands above
    /// what its own bound cannot take.
    pub fn lower_into_kept(&self, env: &Env, kept: &AHashMap<usize, TVar>) -> Result<()> {
        fn children(tv: &TVar) -> SmallVec<[TVar; 2]> {
            let mut out = SmallVec::new();
            for c in tv.cell_constraints().iter() {
                super::contains::find_open_cell(c, false, &mut |o| {
                    out.push(o.clone());
                    false
                });
            }
            out
        }
        fn reaches(
            tv: &TVar,
            kept: &AHashMap<usize, TVar>,
            memo: &mut AHashMap<usize, bool>,
        ) -> bool {
            let addr = tv.cell_addr();
            if kept.contains_key(&addr) {
                return true;
            }
            if let Some(r) = memo.get(&addr) {
                return *r;
            }
            memo.insert(addr, false);
            let r = crate::stack::ensure_sufficient(|| {
                children(tv).iter().any(|c| reaches(c, kept, memo))
            });
            memo.insert(addr, r);
            r
        }
        fn postorder(tv: &TVar, seen: &mut AHashSet<usize>, out: &mut Vec<TVar>) {
            if seen.insert(tv.cell_addr()) {
                crate::stack::ensure_sufficient(|| {
                    for c in children(tv).iter() {
                        postorder(c, seen, out)
                    }
                });
                out.push(tv.clone())
            }
        }
        if self.quantifiers.is_empty() {
            return Ok(());
        }
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut cells);
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let mut order: LPooled<Vec<TVar>> = LPooled::take();
        let named = cells.drain().map(|(_, tv)| (tv.name.clone(), tv));
        for (_, tv) in super::fntyp::sorted_tvars(named).iter() {
            postorder(tv, &mut seen, &mut order)
        }
        let mut memo: LPooled<AHashMap<usize, bool>> = LPooled::take();
        for tv in order.iter().rev() {
            if !tv.is_bound()
                && !tv.is_rigid()
                && !kept.contains_key(&tv.cell_addr())
                && reaches(tv, kept, &mut memo)
            {
                for l in tv.cell_lower().iter().filter(|l| !l.has_unbound()) {
                    if !tv.commit_conjuncts(env, l)? {
                        let shown = crate::format_with_flags(
                            crate::PrintFlag::DerefTVars,
                            || compact_str::format_compact!("{}", Type::TVar(tv.clone())),
                        );
                        bail!(
                            "{shown}: the body puts {l} above it, which the variables \
                             the definition declares cannot hold, since each call picks \
                             them anew: annotate the parameter with a type holding {l}, \
                             or declare no variable"
                        )
                    }
                }
            }
        }
        Ok(())
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
        t.quantifiers_seen(true, &mut walked, &mut |_, tv| {
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
    /// constructor ([`Type::shares_constructor`]) hold two distinct ones.
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
                if !a.shares_constructor(b) {
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

/// How a type holds one occurrence of a variable.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[doc(hidden)]
pub struct Held {
    /// Under a function type, or a typedef application the walk does not
    /// expand, whose expansion may hold it under one.
    pub function: bool,
    /// Under a writable reference, a nominal type or a constructor
    /// application, which hold it exactly.
    pub exact: bool,
    /// In a union beside another member holding an open cell, told only
    /// by a walk that [`DataPositions::telling_members`]: which member
    /// takes a type depends on what the cells are.
    pub member: bool,
}

impl Held {
    /// As data: a value of a wider type is well typed there.
    pub fn data(&self) -> bool {
        !self.function && !self.exact
    }
}

/// A walk of every type variable under a type, through bindings, with how
/// each occurrence is held. A shared node is walked once for each way it
/// is held, and a typedef application by its [`AppKey`] once for each
/// way it is held, then again only where its walk does not
/// [`Unrolling::passes`]; each is kept so its keys stay its own. A caller
/// takes a variable told once as held under a function for held so,
/// whatever else is told of it: an application holding no other variable
/// is passed by.
#[doc(hidden)]
pub struct DataPositions<'a> {
    env: &'a Env,
    members: bool,
    /// The applications under expansion, each as it is held, for a walk
    /// [`Self::passing_repeats`].
    repeats: Option<SmallVec<[(AppKey, Held); 4]>>,
    unrolling: Unrolling,
    seen: LPooled<AHashMap<(usize, Held), Type>>,
    applied: LPooled<AHashMap<(AppKey, Held), (Type, Expanded)>>,
    /// The cells told as held under a function, each with every cell
    /// under its binding.
    sunk: LPooled<AddrSet>,
}

impl<'a> DataPositions<'a> {
    pub fn new(env: &'a Env) -> Self {
        DataPositions {
            env,
            members: false,
            repeats: None,
            unrolling: Unrolling::default(),
            seen: LPooled::take(),
            applied: LPooled::take(),
            sunk: LPooled::take(),
        }
    }

    /// Tell [`Held::member`] too.
    pub fn telling_members(self) -> Self {
        DataPositions { members: true, ..self }
    }

    /// Pass by an application met inside its own expansion, held as that
    /// expansion is: it holds its variables as the expansion told them.
    /// Any other application of a definition under expansion is still
    /// taken for holding them under a function.
    pub fn passing_repeats(self) -> Self {
        DataPositions { repeats: Some(SmallVec::new()), ..self }
    }

    fn beneath_function(&mut self, t: &Type, held: Held, f: &mut dyn FnMut(&TVar, Held)) {
        let held = Held { function: true, ..held };
        t.for_each_child(&mut |c| self.walk(c, held, f))
    }

    /// Whether every variable `t` writes is told as held under a function.
    fn all_sunk(&self, t: &Type) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => self.sunk.contains(&tv.cell_addr()),
            t => t
                .try_for_each_child(&mut |c| match self.all_sunk(c) {
                    true => ControlFlow::Continue(()),
                    false => ControlFlow::Break(()),
                })
                .is_continue(),
        })
    }

    pub fn walk(&mut self, t: &Type, held: Held, f: &mut dyn FnMut(&TVar, Held)) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                f(tv, held);
                if held.function {
                    self.sunk.insert(tv.cell_addr());
                }
                if self.seen.insert((tv.cell_addr(), held), t.clone()).is_none()
                    && let Some(b) = tv.binding()
                {
                    self.walk(&b, held, f)
                }
            }
            t if t
                .shared_identity()
                .is_some_and(|n| self.seen.insert((n, held), t.clone()).is_some()) => {}
            Type::Ref(tr)
                if let Some(key) = tr.probe_def_key(self.env)
                    && !self.unrolling.is_open(key) =>
            {
                if self.all_sunk(t) {
                    return;
                }
                let app = (tr.app_key(key), held);
                if let Some((_, expanded)) = self.applied.get(&app)
                    && self.unrolling.passes(expanded)
                {
                    return;
                }
                match t.lookup_ref_probe(self.env) {
                    Some((_, whole)) => {
                        self.unrolling.enter(key);
                        if let Some(open) = &mut self.repeats {
                            open.push(app.clone())
                        }
                        self.walk(&whole, held, f);
                        if let Some(open) = &mut self.repeats {
                            open.pop();
                        }
                        let expanded = self.unrolling.leave(tr);
                        self.applied.insert(app, (t.clone(), expanded));
                    }
                    None => self.beneath_function(t, held, f),
                }
            }
            Type::Ref(tr)
                if let Some(open) = &self.repeats
                    && let Some(key) = tr.probe_def_key(self.env)
                    && open.contains(&(tr.app_key(key), held)) => {}
            Type::Fn(_) | Type::Ref(_) => self.beneath_function(t, held, f),
            Type::ByRef(super::Mutability::Mut, _)
            | Type::Abstract { .. }
            | Type::App(..) => {
                let held = Held { exact: true, ..held };
                t.for_each_child(&mut |c| self.walk(c, held, f))
            }
            Type::Set(ms)
                if self.members
                    && !held.member
                    && ms.iter().filter(|m| m.has_unbound()).nth(1).is_some() =>
            {
                let held = Held { member: true, ..held };
                ms.iter().for_each(|m| self.walk(m, held, f))
            }
            t => t.for_each_child(&mut |c| self.walk(c, held, f)),
        })
    }
}

/// The definitions an application's walk expanded at an application
/// holding a variable ([`Unrolling`]).
#[doc(hidden)]
pub type Expanded = SmallVec<[usize; 4]>;

/// The typedef applications a walk is expanding, innermost last, for a
/// walk that stops at a definition met inside its own expansion and takes
/// the stop for whatever the variables it holds may be. Each keeps the
/// definitions its walk has expanded at an application holding a
/// variable: where one of those is open, a walk of the application stops
/// there instead, so one walked before finds what it found then only
/// where none of them is ([`Self::passes`]).
#[doc(hidden)]
#[derive(Default)]
pub struct Unrolling(SmallVec<[(usize, Expanded); 4]>);

impl Unrolling {
    /// Whether the walk is inside an application of `def`.
    pub fn is_open(&self, def: usize) -> bool {
        self.0.iter().any(|(d, _)| *d == def)
    }

    /// Whether an application whose walk expanded `expanded` finds here
    /// what it found then; where it does, the applications around it
    /// count those as expanded too.
    pub fn passes(&mut self, expanded: &Expanded) -> bool {
        if expanded.iter().any(|d| self.is_open(*d)) {
            return false;
        }
        if let Some((_, outer)) = self.0.last_mut() {
            union_into(outer, expanded)
        }
        true
    }

    pub fn enter(&mut self, def: usize) {
        self.0.push((def, SmallVec::new()))
    }

    /// Leave `tr`, the application entered last: what its walk expanded.
    pub fn leave(&mut self, tr: &TypeRef) -> Expanded {
        let (def, mut expanded) = self.0.pop().expect("an application was entered");
        if !tr.params.iter().all(|p| p.tvar_free()) {
            union_into(&mut expanded, &[def])
        }
        if let Some((_, outer)) = self.0.last_mut() {
            union_into(outer, &expanded)
        }
        expanded
    }
}

fn union_into(into: &mut Expanded, from: &[usize]) {
    for d in from {
        if !into.contains(d) {
            into.push(*d)
        }
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

/// Which of some cells a set of types reaches, as [`Type::reached_cells`]
/// reaches cells: breadth first, each cell and shared node once, until
/// every one is found.
#[doc(hidden)]
pub struct Reaching(LPooled<AHashSet<usize>>);

impl Reaching {
    /// Which of `cells`, by address, `types` reach.
    pub fn of<'a>(
        cells: impl IntoIterator<Item = usize>,
        types: impl IntoIterator<Item = &'a Type>,
    ) -> Self {
        let mut left: LPooled<AHashSet<usize>> = cells.into_iter().collect();
        let mut walked: LPooled<AHashSet<usize>> = LPooled::take();
        let mut nodes: LPooled<AddrSet> = LPooled::take();
        let mut next: LPooled<Vec<Type>> = types.into_iter().cloned().collect();
        let mut i = 0;
        while i < next.len() && !left.is_empty() {
            let t = next[i].clone();
            i += 1;
            match &t {
                Type::TVar(tv) => {
                    let addr = tv.cell_addr();
                    if walked.insert(addr) {
                        left.remove(&addr);
                        let cell = tv.cell();
                        let cell = cell.read();
                        next.extend(cell.binding.iter().cloned());
                        next.extend(cell.constraints.iter().cloned());
                        next.extend(cell.lower.iter().cloned());
                    }
                }
                t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
                Type::Fn(ft) => ft.for_each_type(&mut |t| next.push(t.clone())),
                t => t.for_each_child(&mut |c| next.push(c.clone())),
            }
        }
        Self(left)
    }

    /// Whether the types reach the cell at `addr`, one of those asked.
    pub fn reached(&self, addr: usize) -> bool {
        !self.0.contains(&addr)
    }
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
    use crate::typ::tvar::AtLevel;
    use compact_str::format_compact;

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

    fn open(name: &'static str) -> TVar {
        TVar::empty_named(ArcStr::from(name))
    }

    fn array_of(tv: &TVar) -> Type {
        Type::Array(triomphe::Arc::new(Type::TVar(tv.clone())))
    }

    /// Two lower bounds apart only by open cells nothing bounds settle to
    /// one of them, those cells one cell; reading the witness welds
    /// nothing.
    #[test]
    fn a_settle_welds_lower_bounds_apart_by_unbounded_cells() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let (p, a, b) = (open("p"), open("a"), open("b"));
        p.add_lower(array_of(&a));
        p.add_lower(array_of(&b));
        let w = p.witness(&env).unwrap().unwrap();
        assert!(matches!(&w, Type::Set(ms) if ms.len() == 2), "{w}");
        assert!(!a.same_cell(&b));
        p.settle_witness(&env).unwrap();
        assert!(a.same_cell(&b));
        let bound = p.binding().unwrap();
        assert!(strictly_identical(&bound, &array_of(&a)), "{bound}");
    }

    /// A cell with a bound of its own, a cell held rigid and a decided
    /// type each keep their lower bound a member of its own.
    #[test]
    fn a_settle_keeps_lower_bounds_apart_by_more_than_unbounded_cells() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, i64_t) = Env::typed_at_root(&[], "i64").unwrap();
        let (_, number) = Env::typed_at_root(&[], "Number").unwrap();
        let bounded = open("b");
        bounded.add_cell_constraint(number);
        let rigid = open("r");
        let _gate = rigid.open_rigid();
        let others = [Type::TVar(bounded), Type::TVar(rigid), i64_t];
        for other in others {
            let (p, a) = (open("p"), open("a"));
            p.add_lower(array_of(&a));
            p.add_lower(Type::Array(triomphe::Arc::new(other.clone())));
            p.settle_witness(&env).unwrap();
            let bound = p.binding().unwrap();
            assert!(matches!(&bound, Type::Set(ms) if ms.len() == 2), "{bound}");
            assert!(!a.is_bound(), "{other}");
        }
    }

    /// A settle of a bound cell compares none of its lower bounds, and one
    /// of an open cell only those that hold an open cell.
    #[test]
    fn a_settle_compares_only_undecided_lower_bounds_of_an_open_cell() {
        use crate::typ::setops::IDENTITY_STEPS;
        let steps = |f: &dyn Fn()| {
            IDENTITY_STEPS.with(|s| s.set(0));
            f();
            IDENTITY_STEPS.with(|s| s.get())
        };
        let _level = AtLevel::enter(Level::TOP);
        let (env, i64_t) = Env::typed_at_root(&[], "i64").unwrap();
        let (_, string_t) = Env::typed_at_root(&[], "string").unwrap();
        let array = |t: &Type| Type::Array(triomphe::Arc::new(t.clone()));
        let (p, a) = (open("p"), open("a"));
        p.add_lower(array_of(&a));
        p.add_lower(array(&i64_t));
        p.add_lower(array(&string_t));
        assert_eq!(steps(&|| p.weld_lower()), 0);
        p.add_lower(array(&array_of(&open("b"))));
        assert!(steps(&|| p.weld_lower()) > 0);
        p.settle_witness(&env).unwrap();
        assert!(p.is_bound());
        assert_eq!(steps(&|| p.settle_witness(&env).unwrap()), 0);
        assert_eq!(steps(&|| p.weld_lower()), 0);
    }

    /// A decided lower bound holding ⊥ in a part is no member of the
    /// witness where another decided bound contains it; one nothing
    /// contains, and one that holds an open cell, stay members.
    #[test]
    fn a_witness_leaves_out_a_bound_holding_bottom_that_another_contains() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, i64_t) = Env::typed_at_root(&[], "i64").unwrap();
        let array = |t: &Type| Type::Array(triomphe::Arc::new(t.clone()));
        let witness = |below: &[Type]| {
            let p = open("p");
            below.iter().for_each(|l| p.add_lower(l.clone()));
            p.witness(&env).unwrap().unwrap()
        };
        let settled = open("a");
        settled.settle_unread();
        for empty in [array(&Type::Bottom), array_of(&settled)] {
            for below in [[empty.clone(), array(&i64_t)], [array(&i64_t), empty.clone()]]
            {
                let w = witness(&below);
                assert!(strictly_identical(&w, &array(&i64_t)), "{w}");
            }
            let w = witness(&[empty.clone(), array(&array(&Type::Bottom))]);
            assert!(strictly_identical(&w, &array(&array(&Type::Bottom))), "{w}");
            let w = witness(&[empty, i64_t.clone()]);
            assert!(matches!(&w, Type::Set(ms) if ms.len() == 2), "{w}");
        }
        let w = witness(&[array_of(&open("b")), array(&i64_t)]);
        assert!(matches!(&w, Type::Set(ms) if ms.len() == 2), "{w}");
    }

    /// A call's terminal settle decides the cell a lower bound of its
    /// signature's cell holds: the witness holds no open cell after it.
    #[test]
    fn a_terminal_settle_decides_a_cell_a_lower_bound_holds() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, t) = Env::typed_at_root(&[], "fn(x: 'p) -> i64").unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars(&mut cells);
        let p = cells.get("p").unwrap().clone();
        p.set_level(Level::TOP);
        let (a, b) = (open("a"), open("b"));
        p.add_lower(array_of(&b));
        b.add_lower(array_of(&a));
        ft.settle_terminal(&env, None, &AHashSet::default(), &[]).unwrap();
        assert!(matches!(a.binding(), Some(Type::Bottom)), "{a:?}");
        assert!(!p.binding().unwrap().has_unbound(), "{p:?}");
    }

    /// A call's terminal settle takes a cell that stands above a type
    /// before the cells its bound holds, down a chain: each takes its part
    /// of what the cell before it settled to.
    #[test]
    fn a_terminal_settle_takes_a_cell_above_a_type_before_its_bounds_cells() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, t) = Env::typed_at_root(
            &[],
            "fn<'p: ['a, null], 'a: ['b, null]>(x: 'p, y: 'b) -> i64",
        )
        .unwrap();
        let (_, i64_t) = Env::typed_at_root(&[], "i64").unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars_through_bounds(&mut cells);
        for tv in cells.values() {
            tv.set_level(Level::TOP)
        }
        cells["p"].add_lower(i64_t.clone());
        cells["b"].mark_bottom_fed();
        ft.settle_terminal(&env, None, &AHashSet::default(), &[]).unwrap();
        for name in ["p", "a", "b"] {
            let bound = cells[name].binding().unwrap();
            assert!(strictly_identical(&bound, &i64_t), "'{name}: {bound}");
        }
    }

    /// A cell that waits asks each cell whose conjuncts reach it whether it
    /// may still feed it once, however many of them settle before the last:
    /// a call's terminal settle is linear in its cells.
    #[test]
    fn a_waiting_cell_asks_each_holder_once() {
        const HOLDERS: usize = 200;
        let _level = AtLevel::enter(Level::TOP);
        let mut sig: LPooled<String> = LPooled::take();
        sig.push_str("fn<'x: string");
        for i in 0..HOLDERS {
            write!(sig, ", 'a{i}: ['b{i}, null], 'b{i}: [Array<'x>, bool]").unwrap()
        }
        sig.push_str(">(x: 'x");
        for i in 0..HOLDERS {
            write!(sig, ", p{i}: 'a{i}").unwrap()
        }
        sig.push_str(") -> i64");
        let (env, t) = Env::typed_at_root(&[], &sig).unwrap();
        let (_, bool_t) = Env::typed_at_root(&[], "bool").unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars_through_bounds(&mut cells);
        for tv in cells.values() {
            tv.set_level(Level::TOP)
        }
        for i in 0..HOLDERS {
            cells[format_compact!("a{i}").as_str()].add_lower(bool_t.clone())
        }
        cells["x"].mark_bottom_fed();
        FEEDS_ASKED.with(|n| n.set(0));
        ft.settle_terminal(&env, None, &AHashSet::default(), &[]).unwrap();
        let asked = FEEDS_ASKED.with(|n| n.get());
        assert!(asked <= 8 * HOLDERS, "{asked} asks for {HOLDERS} holders");
        assert!(matches!(cells["x"].binding(), Some(Type::Bottom)), "{:?}", cells["x"]);
        for i in 0..HOLDERS {
            let bound = cells[format_compact!("b{i}").as_str()].binding().unwrap();
            assert!(strictly_identical(&bound, &bool_t), "'b{i}: {bound}");
        }
    }

    /// A run of settles takes a cell after the cells whose bounds reach
    /// it, whatever their names.
    #[test]
    fn a_run_of_settles_takes_a_bounds_holder_first() {
        let (_, t) = Env::typed_at_root(
            &[],
            "fn<'m: ['z, null], 'z: ['a, null]>(x: 'a, y: 'm, z: 'z, w: 'b) -> i64",
        )
        .unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars(&mut cells);
        let mut tvs: Vec<TVar> = cells.values().cloned().collect();
        settle_order(&mut tvs);
        let names: Vec<&str> = tvs.iter().map(|tv| tv.name.as_str()).collect();
        assert_eq!(names, ["b", "m", "z", "a"]);
    }

    /// A settle at a cell's lower bounds binds it as a containment does:
    /// the open cell its bound holds stands above what the cell took.
    #[test]
    fn a_settle_at_lower_bounds_commits_the_conjuncts() {
        let _level = AtLevel::enter(Level::TOP);
        let (env, t) =
            Env::typed_at_root(&[], "fn<'p: ['a, null]>(x: 'p, y: 'a) -> i64").unwrap();
        let (_, i64_t) = Env::typed_at_root(&[], "i64").unwrap();
        let Type::Fn(ft) = &t else { panic!("{t} is no function type") };
        ft.alias_tvars(&mut LPooled::take());
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ft.collect_tvars(&mut cells);
        let (p, a) = (cells.get("p").unwrap().clone(), cells.get("a").unwrap().clone());
        p.set_level(Level::TOP);
        a.set_level(Level::TOP);
        p.add_lower(i64_t.clone());
        assert!(a.cell_lower().is_empty(), "{a:?}");
        p.settle_witness(&env).unwrap();
        assert!(strictly_identical(&p.binding().unwrap(), &i64_t), "{p:?}");
        let below = a.cell_lower();
        assert!(matches!(&below[..], [l] if strictly_identical(l, &i64_t)), "{a:?}");
    }
}
