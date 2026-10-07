//! The settle phase: open cells that inference left constrained bind to
//! their conjunction's witness, and a call site's terminal settle binds
//! what nothing produced or bounded to ⊥.

use crate::{
    PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_bind_bt_id},
    env::Env,
    format_with_flags,
    typ::{
        FnType, TVar, Type,
        contains::{ContainsHist, INFINITE_TYPE_MSG},
        tvar::would_cycle_inner,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::fmt::Write;

impl TVar {
    /// The narrowest conjunct every other conjunct contains, as a
    /// private copy (the store's type verbatim would alias its interior
    /// cells into live inference). `Err(true)` when there is none,
    /// `Err(false)` when no conjunct could be one (all traits or
    /// self-referential). Bound and unconstrained cells have none.
    pub(crate) fn witness(&self, env: &Env) -> Result<std::result::Result<Type, bool>> {
        let cons = {
            let cell = self.cell();
            let cell = cell.read();
            if cell.binding.is_some() || cell.constraints.is_empty() {
                return Ok(Err(false));
            }
            cell.constraints.clone()
        };
        let mut hist = ContainsHist::new();
        let mut candidates = false;
        let addr = self.cell_addr();
        'cand: for c in cons.iter() {
            // A trait conjunct is a predicate, not a binding, and a
            // conjunct reaching this cell has no finite witness.
            if c.is_predicate() || c.is_trait_ref(env) || would_cycle_inner(addr, c) {
                continue;
            }
            candidates = true;
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    continue 'cand;
                }
            }
            return Ok(Ok(c.reset_tvars()));
        }
        Ok(Err(candidates))
    }

    /// Bind an unbound cell once its writers are checked: to ⊥ when only
    /// ⊥ was produced into it ([`TCell::bottom_fed`]), whatever bounds
    /// it, else as [`Self::settle_witness`].
    ///
    /// [`TCell::bottom_fed`]: super::tvar::TCell::bottom_fed
    pub fn settle(&self, env: &Env) -> Result<()> {
        if self.cell().read().bottom_fed {
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
                    bail!("unsatisfiable constraints on '{}: {}", self.name, &*cs)
                })
            }
        }
    }

    /// [`Self::settle`], but an unconstrained unbound cell binds to ⊥:
    /// nothing produced or bounded it. Only the terminal walk uses this
    /// (an earlier call would foreclose writers not yet typechecked).
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        match self.cell_constraints().is_empty() {
            true => self.settle_bottom(),
            false => {
                self.settle(env)?;
                if !self.is_bound() && self.requires_concrete() {
                    return Err(self.not_concrete());
                }
                if !self.is_bound() && self.requires_function() {
                    return Err(self.not_function());
                }
                Ok(())
            }
        }
    }

    fn not_concrete(&self) -> anyhow::Error {
        anyhow::anyhow!(
            "the type '{} must be fully known here, a type-directed operation reads it: \
             annotate it",
            self.name
        )
    }

    fn not_function(&self) -> anyhow::Error {
        anyhow::anyhow!(
            "the type '{} must be a function here, an operation reads its signature: \
             annotate it",
            self.name
        )
    }

    /// Bind an unbound cell to ⊥; a bound one is untouched.
    fn settle_bottom(&self) -> Result<()> {
        let cell = self.cell();
        let mut cell = cell.write();
        if cell.binding.is_some() || super::tvar::earlier_task(&cell) {
            return Ok(());
        }
        if cell.constraints.iter().any(|c| matches!(c, Type::Concrete)) {
            drop(cell);
            return Err(self.not_concrete());
        }
        if cell.constraints.iter().any(|c| matches!(c, Type::Function)) {
            drop(cell);
            return Err(self.not_function());
        }
        // The only solution was infinite; ⊥ would be a lie.
        if cell.cycle_refused {
            if graphix_dbg_bind() {
                eprintln!("SETTLE-INFINITE '{}({:x})", self.name, self.cell_addr());
            }
            bail!("{INFINITE_TYPE_MSG}")
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
                let (bound, cons) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                for t in bound.iter().chain(cons.iter()) {
                    settle_refs(t, index, visited, out);
                }
            }
        }
        Type::Fn(ft) => ft.for_each_part(&mut |t, _| settle_refs(t, index, visited, out)),
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
            let (bound, cons) = {
                let cell = tv.cell();
                let cell = cell.read();
                (cell.binding.clone(), cell.constraints.clone())
            };
            for t in bound.iter().chain(cons.iter()) {
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
        self.for_each_part(&mut |t, _| reached_cells(t, out))
    }
}

fn reached_cells(t: &Type, out: &mut AHashMap<usize, TVar>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if out.insert(tv.cell_addr(), tv.clone()).is_none() {
                let (bound, cons) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                for t in bound.iter().chain(cons.iter()) {
                    reached_cells(t, out)
                }
            }
        }
        Type::Fn(ft) => ft.reached_cells(out),
        t => t.for_each_child(&mut |c| reached_cells(c, out)),
    })
}

/// The cells `t` holds as positions, through bindings, never through
/// conjuncts.
#[doc(hidden)]
pub fn position_cells(t: &Type, out: &mut AHashMap<usize, TVar>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if out.insert(tv.cell_addr(), tv.clone()).is_none()
                && let Some(b) = tv.binding()
            {
                position_cells(&b, out)
            }
        }
        Type::Fn(ft) => ft.for_each_part(&mut |t, constraint| {
            if !constraint {
                position_cells(t, out)
            }
        }),
        t => t.for_each_child(&mut |c| position_cells(c, out)),
    })
}
