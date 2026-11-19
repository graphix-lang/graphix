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
        setops::{Welds, apply_welds, lower_collapses, strictly_identical},
        tvar::{CellFact, Fresh, Level, would_cycle_open},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    error::Error,
    fmt::{self, Write},
};

#[derive(Debug)]
pub(crate) struct ConcreteRequired(pub TVar);

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

impl ConcreteRequired {}

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
pub(crate) fn settle_order(tvs: &mut [TVar]) {
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
        // CR claude for eric: [bug/medium] The witness search tries each conjunct and
        // never their meet, so two bounds that overlap with neither holding the other
        // report "unsatisfiable constraints" though a type satisfies both. `let p = |s:
        // string| { let v = str::parse(s)$; f(v); g(v); v }; p("1")` over `f: 'a:
        // Number` and `g: 'b: [i64, string]` is refused ("unsatisfiable constraints on
        // '_N: Concrete & NoError & Number & [i64, string]") where `i64` fits; with
        // `g`'s bound `[Number, string]` it is accepted and reads a `Number`. Either
        // such a cell settles to the meet of its type conjuncts, or the refusal says
        // the bounds overlap and names the annotation: which is the rule is yours.
        // probe:
        // design/review-2026-11-16/repro/t-settle-overlapping-bounds-unsatisfiable.gx
        // (t-settle-06)
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
            // CR claude for claude: [bug/low] Where a generated cell's lower bound
            // fails a marker bound, this is the whole refusal: `Box(&x) < Box(&x)` over
            // `type Box<'a> = Abstract<'a>` says "unsatisfiable constraints on '_8192:
            // Ordered", a variable the program never wrote and no cause, where the same
            // order over a tuple names the reference. Name the lower bound the conjunct
            // refuses and why, as a bound cell's refusal does (`Type::not_discernible`
            // for `Ordered` and `Discernible`, `unmet_bound_note`'s wording for a typed
            // bound). The conjuncts are joined here and in `unmet_bound_note` without
            // the `written_identical` filter `TVar`'s Display applies, so a doubled
            // bound still prints twice in both (that part by reading). probe:
            // design/review-2026-11-16/repro/t-cast-ordered-abstract-param-message.gx
            // (t-cast-09)
            // CR claude for claude: [bug/medium] This prints the cell's conjuncts
            // alone. Where the cell fails because its conjuncts refuse its lower bounds
            // together, the text names a conjunction that is satisfiable and a
            // generated variable, and omits what conflicts: `type B<'p> = Abstract<'p>;
            // let b = B(1); let r = b.0 + b.0; b <- B(1.5)` answers "unsatisfiable
            // constraints on '_8191: Number & Singleton" at `B(1)`, with no word of the
            // i64 and the f64 built into the payload, the operator or the write. When
            // `cell_lower()` is not empty, print the bounds the conjuncts refuse and
            // the binding to declare, as a refused write does. probe:
            // design/review-2026-11-16/repro/t-tvar-b-unsatisfiable-names-no-lower-bound.gx
            // (t-tvar-b-03)
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
        // CR claude for claude: [bug/high] An input's end binds a `Concrete` or
        // `Function` cell to bottom here, where a call's terminal settle
        // (`settle_bottom`) refuses it. A function a REPL input leaves at one type
        // (`let fs = [|| str::parse("42"), || str::parse("43")]`) is then callable by a
        // later input at result type `_`: `let e: Error<`ParseError(string)> =
        // fs[0]$()` is accepted and holds 42, `e.0` is null, and a select over the call
        // reports a coverage hole in the check, while the same statements as one script
        // are refused ("the type 'b must be fully known here"). Refuse the cell as
        // `settle_bottom` does (this tail repeats that function's but for its two
        // predicate tests), so the input is refused naming the binding; census both
        // repos, pin it in `lang::repl_inputs`, and fix "nothing of it having been
        // built" in design/tvar_constraints.md, which a function's result cell
        // contradicts. probe:
        // design/review-2026-11-16/repro/t-settle-input-end-settles-concrete-to-bottom.sh
        // (t-settle-01)
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

impl FnType {}

#[cfg(test)]
thread_local! {
    static FEEDS_ASKED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
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
    // CR claude for claude: [bug/medium] A cell this loop binds to a conjunct's witness
    // takes fresh copies of the conjunct's generic cells (`Witness::shared`), and the
    // copies are in no list this function walks: they stay open and are never marked
    // `Settled`. After `let f = 'a0: Array<'a1> |x0: 'a0| x0` and `let fs = [|w| f(w),
    // |w| f(w)]` the REPL prints `fs` as `Array<fn(w: Array<'a1: unbound>) ->
    // Array<'a1: unbound>>`, where `[|w: Array<'e>| w, |w| w]` prints `'e: _`, and a
    // later use is refused as deciding an earlier input's type, not as a settled
    // binding's. Make the loop a worklist: run `reach_new` over each cell it binds, so
    // every cell the settle introduces is settled and marked. probe:
    // design/review-2026-11-16/repro/t-settle-input-end-leaves-witness-copy-open.sh
    // (t-settle-03)
    for (i, tv) in reached.iter().filter(|(_, tv)| top(tv)) {
        if !tv.is_bound() && !tv.is_rigid() {
            tv.settle_built(env).map_err(|e| (*i, e))?
        }
    }
    reached.iter().filter(|(_, tv)| top(tv)).for_each(|(_, tv)| tv.mark_settled());
    Ok(())
}

/// [`reached`], each cell met for the first time pushed with `owner`.
// CR claude for claude: [structure/low] `reach_new`, `reached` and `settle_refs` are
// one recursion written three times (a cell's binding, conjuncts and lower bounds; a
// shared node once; a function type by `for_each_type`), apart only in what they do at
// a cell's first visit, and `Reaching::of` is its worklist form. `walk_positions`
// already takes that difference as a `first` closure. Write one walk taking `first` and
// express the three through it, so a change to what a cell reaches is made once. Found
// by reading; not run. (t-settle-08)
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

// CR claude for claude: [structure/low] From `variable_outside_functions` to
// `Unrolling` this file holds walks no settle calls and that call no settle:
// `VariableUnion`, `OutsideFunctions`, `Held`, `DataPositions` and `Unrolling` serve
// signature checks and a call's widening in callsite.rs, lambda.rs, traits.rs and
// module.rs, and the module's header names only the settle phase. Move them to a module
// of their own (typ/positions.rs), so settle.rs is the settle and each is found by its
// file's name. Found by reading; not run. (t-settle-09)
// 2026-11-19 claude: Moved here from above VariableUnion, which is deleted with its
// rule (Eric ruled 11-19: drop the variable-union rule); the rest of the CR stands.
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
    pub(crate) fn variables_outside_functions(
        &self,
        env: &Env,
        out: &mut AHashMap<usize, TVar>,
    ) {
        OutsideFunctions { env, expanding: Expanding::default(), nodes: LPooled::take() }
            .walk(self, out);
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
    use crate::typ::tvar::AtLevel;
    use arcstr::ArcStr;

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
