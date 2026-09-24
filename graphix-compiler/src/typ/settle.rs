//! The settle phase: open cells that inference left constrained bind to
//! their conjunction's witness, and a call site's terminal settle binds
//! what nothing produced or bounded to ⊥.

use crate::{
    PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_bind_bt},
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
    /// Bind a constrained-unbound cell to its conjunction's witness,
    /// the narrowest conjunct every other conjunct contains. Bound and
    /// unconstrained cells are untouched. No witness is a type error,
    /// unless no conjunct could be one (all traits or self-referential):
    /// then the cell stays open for its writers to refine.
    pub fn settle(&self, env: &Env) -> Result<()> {
        let cons = {
            let cell = self.cell();
            let cell = cell.read();
            if cell.binding.is_some() || cell.constraints.is_empty() {
                return Ok(());
            }
            cell.constraints.clone()
        };
        let mut hist = ContainsHist::new();
        let mut witness = None;
        let mut candidates = false;
        let addr = self.cell_addr();
        'cand: for c in cons.iter() {
            // A trait conjunct is a predicate, not a binding, and a
            // conjunct reaching this cell has no finite witness.
            if c.is_trait_ref(env) || would_cycle_inner(addr, c) {
                continue;
            }
            candidates = true;
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    continue 'cand;
                }
            }
            witness = Some(c.clone());
            break;
        }
        match witness {
            // A private copy: binding the store's type verbatim would
            // alias its interior cells into live inference.
            Some(w) => {
                let w = w.reset_tvars();
                if graphix_dbg_bind() {
                    eprintln!("SETTLE '{}({:x}) := {w:?}", self.name, self.cell_addr());
                }
                self.bind(w);
                Ok(())
            }
            None if !candidates => Ok(()),
            None => format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                let mut cs: LPooled<String> = LPooled::take();
                for (i, c) in cons.iter().enumerate() {
                    if i > 0 {
                        cs.push_str(" & ");
                    }
                    write!(cs, "{c}")?;
                }
                bail!("unsatisfiable constraints on '{}: {}", self.name, &*cs)
            }),
        }
    }

    /// [`Self::settle`], but an unconstrained unbound cell binds to ⊥:
    /// nothing produced or bounded it. Only the terminal walk uses this
    /// (an earlier call would foreclose writers not yet typechecked).
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        {
            let cell = self.cell();
            let mut cell = cell.write();
            if cell.binding.is_some() {
                return Ok(());
            }
            if cell.constraints.is_empty() {
                // The only solution was infinite; ⊥ would be a lie.
                if cell.cycle_refused {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "SETTLE-INFINITE '{}({:x})",
                            self.name,
                            self.cell_addr()
                        );
                    }
                    bail!("{INFINITE_TYPE_MSG}")
                }
                if graphix_dbg_bind() {
                    eprintln!("SETTLE-BOTTOM '{}({:x})", self.name, self.cell_addr());
                }
                if graphix_dbg_bind_bt() {
                    eprintln!("{}", std::backtrace::Backtrace::force_capture());
                }
                cell.binding = Some(Type::Bottom);
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
    /// `defaulted` cells are ordered but not settled.
    pub fn settle_terminal(
        &self,
        env: &Env,
        rtype_cell: Option<&TVar>,
        defaulted: &AHashSet<usize>,
    ) -> Result<()> {
        let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut tvs);
        let rtype_cell = rtype_cell
            .filter(|tv| !tvs.values().any(|n| n.cell_addr() == tv.cell_addr()))
            .map(|tv| (tv.name.clone(), tv.clone()));
        let nodes = super::fntyp::sorted_tvars(tvs.drain().chain(rtype_cell));
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
