//! What an input's end settles: the open cells of what it made that a later
//! input can reach, decided as the check decides a cell and marked settled.

use crate::{
    AddrSet,
    env::Env,
    typ::{
        Bound, FnType, TVar, Type,
        setops::strictly_identical,
        tvar::{Fresh, Level, would_cycle_open},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use poolshark::local::LPooled;
use smallvec::SmallVec;

/// `below` less each decided bound holding ⊥ in a part that another decided
/// bound contains: it adds no value to a witness (`Array<_>` beside
/// `Array<i64>`). Of two that contain each other the first stays.
fn joined_lower<'a>(env: &Env, below: &'a [Type]) -> Result<LPooled<Vec<&'a Type>>> {
    let holds_bottom = |l: &Type| {
        l.has_bottom()
            && !l.with_deref(|l| matches!(l, Some(Type::Bottom)))
            && !l.has_unbound()
    };
    let mut kept: LPooled<Vec<&Type>> = LPooled::take();
    for (i, l) in below.iter().enumerate() {
        let mut contained = false;
        if below.len() > 1 && holds_bottom(l) {
            for (j, k) in below.iter().enumerate() {
                if j != i
                    && !k.has_unbound()
                    && k.decided_holds(env, l)?
                    && (j < i || !l.decided_holds(env, k)?)
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
        Ok(self.own_witness(env)?.map(Witness::shared))
    }

    /// The union of the cell's lower bounds, else the narrowest conjunct
    /// every other conjunct contains, its cells its own. `Err(true)` when
    /// there is none, `Err(false)` when no conjunct could be one (all
    /// traits or self-referential, or a set only `Singleton` refuses: a
    /// member would do, and no walk here can choose it). Bound and
    /// unbounded cells have none.
    fn own_witness(&self, env: &Env) -> Result<std::result::Result<Witness, bool>> {
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
            let below = joined_lower(env, &below)?;
            let w = Type::union(env, &below)?;
            for c in cons.iter() {
                if !c.decided_holds(env, &w)? {
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
                if !o.decided_holds(env, c)? {
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
            super::bounds::settle_left(env, tv).map_err(|e| (*i, e))?
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
