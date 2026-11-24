//! A binding's type as its cells' bounds read: what a hover, an outline or
//! a completion shows of a type the check left open.
//!
//! The check keeps each open cell with the types it must contain (lower
//! bounds) and the types it must fit (upper bounds, a cell among them where
//! one cell flows into another), and a scheme's cells are never decided.
//! [`Type::read_off`] reads a cell by where it stands in the type:
//!
//! - cells that flow into each other are one cell;
//! - one that only produces (it stands in a result) is what flows into it:
//!   the union of its lower bounds and of the variables flowing in, or
//!   nothing (bottom) where nothing does;
//! - one that only consumes (it stands in an argument) is the variable it
//!   flows into where that stands in a result, else the one type it must
//!   fit, else a variable;
//! - the rest are variables, each with the bounds of its cell and of the
//!   cells it flows into. Cells flowing into one `Singleton` cell are one
//!   variable.
//!
//! A `Singleton` cell with a lower bound is that bound, since it stands for
//! one type. A variable the program declared, or the definition the type is
//! shown in holds, stands for itself.

use crate::{
    stack::ensure_sufficient,
    typ::{Bound, Mutability, TVar, Type},
};
use ahash::{AHashMap, AHashSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;

/// How many times the reading rewrites the type before it takes what it has.
const ROUNDS: usize = 32;

/// Which ways a cell stands in a type.
#[derive(Clone, Copy, Default)]
struct Stands {
    produces: bool,
    consumes: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Pol {
    Produces,
    Consumes,
    Both,
}

impl Pol {
    fn flip(self) -> Self {
        match self {
            Pol::Produces => Pol::Consumes,
            Pol::Consumes => Pol::Produces,
            Pol::Both => Pol::Both,
        }
    }
}

/// An open cell's bounds, with the cells that flow into it and out of it.
struct Node {
    cell: TVar,
    lower: SmallVec<[Type; 1]>,
    upper: SmallVec<[Type; 2]>,
    flows_to: SmallVec<[usize; 2]>,
    flows_from: SmallVec<[usize; 2]>,
    singleton: bool,
}

/// What a bound or a binding is: an open cell, or a type.
enum Peeled {
    Cell(TVar),
    Type(Type),
}

fn bound_of(t: &Type) -> Type {
    let mut t = t.clone();
    while let Type::TVar(tv) = &t
        && let Some(b) = tv.binding()
    {
        t = b
    }
    t
}

fn is_singleton(tv: &TVar) -> bool {
    tv.cell_above().iter().any(|c| matches!(c, Type::Bound(Bound::Singleton)))
}

/// The one type a `Singleton` cell stands for where a lower bound decides
/// it.
fn collapsed(tv: &TVar) -> Option<Type> {
    if !is_singleton(tv) {
        return None;
    }
    let mut lows: SmallVec<[Type; 1]> = SmallVec::new();
    for l in tv.cell_lower() {
        match bound_of(&l) {
            Type::TVar(_) => return None,
            Type::Bottom => (),
            l => lows.push(l),
        }
    }
    if lows.is_empty() {
        return None;
    }
    match Type::flatten_set(lows) {
        Type::Set(_) | Type::Bottom => None,
        t => Some(t),
    }
}

fn peel(t: &Type) -> Peeled {
    let t = bound_of(t);
    match &t {
        Type::TVar(tv) => match collapsed(tv) {
            Some(c) => Peeled::Type(c),
            None => Peeled::Cell(tv.clone()),
        },
        _ => Peeled::Type(t),
    }
}

fn open_cells(t: &Type, f: &mut impl FnMut(&TVar)) {
    fn go(t: &Type, f: &mut impl FnMut(&TVar), seen: &mut AHashSet<usize>) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => go(&b, f, seen),
                None => {
                    if seen.insert(tv.cell_addr()) {
                        f(tv)
                    }
                }
            },
            t => t.for_each_child(&mut |c| go(c, f, seen)),
        })
    }
    go(t, f, &mut LPooled::take())
}

/// The cells the type reaches through their bounds, with how they flow.
#[derive(Default)]
struct Graph {
    nodes: AHashMap<usize, Node>,
    /// The `Singleton` cells a lower bound decides, with their types.
    collapsed: AHashMap<usize, Type>,
}

impl Graph {
    fn explore(&mut self, t: &Type) {
        let mut todo: LPooled<Vec<TVar>> = LPooled::take();
        open_cells(t, &mut |c| todo.push(c.clone()));
        while let Some(tv) = todo.pop() {
            let addr = tv.cell_addr();
            if self.nodes.contains_key(&addr) {
                continue;
            }
            if let Some(t) = collapsed(&tv) {
                open_cells(&t, &mut |c| todo.push(c.clone()));
                self.collapsed.insert(addr, t);
                continue;
            }
            let mut node = Node {
                singleton: is_singleton(&tv),
                cell: tv.clone(),
                lower: SmallVec::new(),
                upper: SmallVec::new(),
                flows_to: SmallVec::new(),
                flows_from: SmallVec::new(),
            };
            for l in tv.cell_lower() {
                match peel(&l) {
                    Peeled::Cell(u) => {
                        node.flows_from.push(u.cell_addr());
                        todo.push(u)
                    }
                    Peeled::Type(Type::Bottom) => (),
                    Peeled::Type(t) => {
                        open_cells(&t, &mut |c| todo.push(c.clone()));
                        node.lower.push(t)
                    }
                }
            }
            for u in tv.cell_above() {
                match peel(&u) {
                    Peeled::Cell(u) => {
                        node.flows_to.push(u.cell_addr());
                        todo.push(u)
                    }
                    Peeled::Type(t) => {
                        open_cells(&t, &mut |c| todo.push(c.clone()));
                        node.upper.push(t)
                    }
                }
            }
            self.nodes.insert(addr, node);
        }
        let addrs: LPooled<Vec<usize>> = self.nodes.keys().copied().collect();
        for a in addrs.iter() {
            let tos: SmallVec<[usize; 2]> = self.nodes[a].flows_to.clone();
            for b in tos {
                if let Some(n) = self.nodes.get_mut(&b)
                    && !n.flows_from.contains(a)
                {
                    n.flows_from.push(*a)
                }
            }
            let froms: SmallVec<[usize; 2]> = self.nodes[a].flows_from.clone();
            for b in froms {
                if let Some(n) = self.nodes.get_mut(&b)
                    && !n.flows_to.contains(a)
                {
                    n.flows_to.push(*a)
                }
            }
        }
    }

    /// The cells that flow into each other are one: each cycle's cells are
    /// mapped to the cell that stands for it, the one the program declared
    /// where the cycle holds one, and its node holds what theirs did.
    fn merge_cycles(&mut self, prefer: &AHashSet<usize>) -> AHashMap<usize, Type> {
        let addrs: LPooled<Vec<usize>> = self.nodes.keys().copied().collect();
        let mut closures: AHashMap<usize, LPooled<AHashSet<usize>>> = AHashMap::default();
        for a in addrs.iter() {
            let c: LPooled<AHashSet<usize>> = self.above(*a).iter().copied().collect();
            closures.insert(*a, c);
        }
        let mut rep_of: AHashMap<usize, usize> = AHashMap::default();
        for a in addrs.iter() {
            if rep_of.contains_key(a) {
                continue;
            }
            let mut group: SmallVec<[usize; 4]> = addrs
                .iter()
                .copied()
                .filter(|b| closures[a].contains(b) && closures[b].contains(a))
                .collect();
            group.sort_by_key(|b| (!prefer.contains(b), *b));
            for b in group.iter() {
                rep_of.insert(*b, group[0]);
            }
        }
        let mut alias = self.collapsed.clone();
        let cycles = rep_of.iter().filter(|(a, r)| a != r).count();
        for (a, r) in rep_of.iter() {
            if a != r {
                alias.insert(*a, Type::TVar(self.nodes[r].cell.clone()));
            }
        }
        if cycles == 0 {
            return alias;
        }
        let mut merged: AHashMap<usize, Node> = AHashMap::default();
        for (a, n) in self.nodes.drain() {
            let r = rep_of[&a];
            let target = merged.entry(r).or_insert_with(|| Node {
                cell: n.cell.clone(),
                lower: SmallVec::new(),
                upper: SmallVec::new(),
                flows_to: SmallVec::new(),
                flows_from: SmallVec::new(),
                singleton: false,
            });
            if a == r {
                target.cell = n.cell.clone();
            }
            target.singleton |= n.singleton;
            for t in n.lower.iter() {
                let t = substitute(t, &alias);
                if !target.lower.contains(&t) {
                    target.lower.push(t)
                }
            }
            for t in n.upper.iter() {
                let t = substitute(t, &alias);
                if !target.upper.contains(&t) {
                    target.upper.push(t)
                }
            }
            for b in n.flows_to.iter().map(|b| rep_of[b]).filter(|b| *b != r) {
                if !target.flows_to.contains(&b) {
                    target.flows_to.push(b)
                }
            }
            for b in n.flows_from.iter().map(|b| rep_of[b]).filter(|b| *b != r) {
                if !target.flows_from.contains(&b) {
                    target.flows_from.push(b)
                }
            }
        }
        self.nodes = merged;
        alias
    }

    /// `start` and every cell reached by `step`, in the order met.
    fn closure(
        &self,
        start: usize,
        step: impl Fn(&Node) -> &[usize],
    ) -> LPooled<Vec<usize>> {
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let mut order: LPooled<Vec<usize>> = LPooled::take();
        let mut todo: LPooled<Vec<usize>> = LPooled::take();
        todo.push(start);
        while let Some(a) = todo.pop() {
            if !seen.insert(a) {
                continue;
            }
            order.push(a);
            if let Some(n) = self.nodes.get(&a) {
                todo.extend(step(n).iter().copied())
            }
        }
        order
    }

    /// The cells flowing into `a`, itself included.
    fn below(&self, a: usize) -> LPooled<Vec<usize>> {
        self.closure(a, |n| &n.flows_from)
    }

    /// The cells `a` flows into, itself included.
    fn above(&self, a: usize) -> LPooled<Vec<usize>> {
        self.closure(a, |n| &n.flows_to)
    }
}

/// How the cells of `t` stand, and the cells that name a variable the
/// program declared.
fn stands(
    t: &Type,
    pol: Pol,
    out: &mut AHashMap<usize, (TVar, Stands)>,
    declared: &mut AHashSet<usize>,
) {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => stands(&b, pol, out, declared),
            None => {
                let e = out
                    .entry(tv.cell_addr())
                    .or_insert_with(|| (tv.clone(), Stands::default()));
                e.1.produces |= pol != Pol::Consumes;
                e.1.consumes |= pol != Pol::Produces;
            }
        },
        Type::Fn(ft) => {
            ft.each_quantifier(|_, tv| {
                declared.extend(tv.open_cell().map(|c| c.cell_addr()))
            });
            for a in ft.args.iter() {
                stands(&a.typ, pol.flip(), out, declared)
            }
            if let Some(v) = &ft.vargs {
                stands(v, pol.flip(), out, declared)
            }
            stands(&ft.rtype, pol, out, declared);
            stands(&ft.throws, pol, out, declared)
        }
        Type::ByRef(Mutability::Mut, t) => stands(t, Pol::Both, out, declared),
        Type::Abstract { params, .. } => {
            for p in params.iter() {
                stands(p, Pol::Both, out, declared)
            }
        }
        t => t.for_each_child(&mut |c| stands(c, pol, out, declared)),
    })
}

/// `t` with each cell of `subst` replaced by its type, and what that holds
/// replaced in turn.
fn substitute(t: &Type, subst: &AHashMap<usize, Type>) -> Type {
    fn go(
        t: &Type,
        subst: &AHashMap<usize, Type>,
        active: &mut AHashSet<usize>,
    ) -> Option<Type> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => go(&b, subst, active),
                None => {
                    let addr = tv.cell_addr();
                    let r = subst.get(&addr)?;
                    if !active.insert(addr) {
                        return None;
                    }
                    let r = go(r, subst, active).unwrap_or_else(|| r.clone());
                    active.remove(&addr);
                    Some(r)
                }
            },
            Type::Set(ms) => {
                let ms = Type::cow_slice(ms, |m| go(m, subst, active))?;
                Some(Type::flatten_set(ms.iter().cloned()))
            }
            t => t.cow_children(&mut |c| go(c, subst, active)),
        })
    }
    go(t, subst, &mut LPooled::take()).unwrap_or_else(|| t.clone())
}

type Standing = AHashMap<usize, (TVar, Stands)>;

/// What each round of the reading knows.
struct Reading {
    graph: Graph,
    /// The cells that stand in a type: those it holds, and the declared
    /// variables it names.
    standing: Standing,
    declared: AHashSet<usize>,
    /// The cells merged into a cell, by the cell.
    merged: AHashMap<usize, SmallVec<[usize; 2]>>,
    /// The cells already read as a type.
    read: AHashSet<usize>,
}

impl Reading {
    fn stands(&mut self, t: &Type) {
        self.standing.clear();
        stands(t, Pol::Produces, &mut self.standing, &mut self.declared);
    }

    /// Whether the cell is a variable of the type: a cell that has its own
    /// meaning where it stands, which what flows into it does not decide.
    fn is_variable(&self, addr: usize) -> bool {
        self.declared.contains(&addr)
            || self.standing.get(&addr).is_some_and(|(_, s)| s.consumes)
    }

    /// What flows into the cell: the lower bounds, and the variables
    /// flowing in.
    fn flows_in(&self, addr: usize) -> Option<Type> {
        let mut members: SmallVec<[Type; 4]> = SmallVec::new();
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let mut todo: LPooled<Vec<usize>> = LPooled::take();
        todo.push(addr);
        while let Some(a) = todo.pop() {
            if !seen.insert(a) {
                continue;
            }
            let Some(n) = self.graph.nodes.get(&a) else { continue };
            if a != addr && self.is_variable(a) {
                members.push(Type::TVar(n.cell.clone()));
                continue;
            }
            members.extend(n.lower.iter().cloned());
            todo.extend(n.flows_from.iter().copied())
        }
        match members.is_empty() {
            true => None,
            false => Some(Type::flatten_set(members)),
        }
    }

    /// The first cell the cell flows into that is a variable of the type
    /// the cell's values reach.
    fn flows_into_variable(&self, addr: usize) -> Option<usize> {
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let mut todo: LPooled<Vec<usize>> = LPooled::take();
        todo.push(addr);
        while let Some(a) = todo.pop() {
            if !seen.insert(a) {
                continue;
            }
            let Some(n) = self.graph.nodes.get(&a) else { continue };
            let reached = self.standing.get(&a).is_some_and(|(_, s)| s.produces)
                || self.declared.contains(&a);
            if a != addr && reached {
                return Some(a);
            }
            todo.extend(n.flows_to.iter().copied())
        }
        None
    }

    /// The one type the cell must fit, where the types of its upper bounds
    /// and the upper bounds of the cells it flows into agree.
    fn fits(&self, addr: usize) -> Option<Type> {
        let mut found: SmallVec<[Type; 2]> = SmallVec::new();
        for a in self.graph.above(addr).iter() {
            let Some(n) = self.graph.nodes.get(a) else { continue };
            for u in n.upper.iter() {
                if !matches!(u, Type::Bound(_)) && !found.contains(u) {
                    found.push(u.clone())
                }
            }
        }
        match &found[..] {
            [one] => Some(one.clone()),
            _ => None,
        }
    }

    /// The bounds a variable standing for the cell carries.
    fn bounds(&self, addr: usize) -> SmallVec<[Type; 2]> {
        let mut found: SmallVec<[Type; 2]> = SmallVec::new();
        let own = self.merged.get(&addr).map(|m| &m[..]).unwrap_or(&[]);
        for start in std::iter::once(addr).chain(own.iter().copied()) {
            for a in self.graph.above(start).iter() {
                let Some(n) = self.graph.nodes.get(a) else { continue };
                for u in n.upper.iter() {
                    if !u.has_unbound() && !found.contains(u) {
                        found.push(u.clone())
                    }
                }
            }
        }
        found
    }

    /// The cells of the type that flow into one `Singleton` cell are one
    /// variable.
    fn merge_singletons(&mut self, subst: &mut AHashMap<usize, Type>) {
        let singles: LPooled<Vec<usize>> = self
            .graph
            .nodes
            .iter()
            .filter(|(_, n)| n.singleton)
            .map(|(a, _)| *a)
            .collect();
        for s in singles.iter() {
            let mut into: SmallVec<[usize; 4]> = self
                .graph
                .below(*s)
                .iter()
                .copied()
                .filter(|a| self.standing.contains_key(a) && !subst.contains_key(a))
                .collect();
            into.sort_by_key(|a| !self.declared.contains(a));
            if let [rep, rest @ ..] = &into[..] {
                let Some(cell) = self.graph.nodes.get(rep).map(|n| n.cell.clone()) else {
                    continue;
                };
                for m in rest {
                    if !self.declared.contains(m) {
                        subst.insert(*m, Type::TVar(cell.clone()));
                        self.merged.entry(*rep).or_default().push(*m);
                    }
                }
            }
        }
    }

    /// The cells that only produce and are not read yet.
    fn producers(&self, subst: &AHashMap<usize, Type>) -> LPooled<Vec<usize>> {
        self.standing
            .iter()
            .filter(|(a, (_, s))| {
                !s.consumes
                    && !self.declared.contains(*a)
                    && !subst.contains_key(*a)
                    && !self.read.contains(*a)
            })
            .map(|(a, _)| *a)
            .collect()
    }

    /// The type each cell that only produces is, by what flows into it.
    fn read_producers(&mut self, subst: &mut AHashMap<usize, Type>) {
        for a in self.producers(subst).iter() {
            if let Some(r) = self.flows_in(*a) {
                subst.insert(*a, r);
                self.read.insert(*a);
            }
        }
    }

    /// A cell that only produces and that nothing flows into is nothing,
    /// once no consumer's reading can bring a variable that does.
    fn empty_producers(&mut self, top: Option<usize>, subst: &mut AHashMap<usize, Type>) {
        for a in self.producers(subst).iter() {
            let r = match top == Some(*a) {
                true => self.fits(*a).unwrap_or(Type::Bottom),
                false => Type::Bottom,
            };
            subst.insert(*a, r);
            self.read.insert(*a);
        }
    }

    /// The type each cell that only consumes is: the variable it flows
    /// into, or what it must fit.
    fn read_consumers(&mut self, subst: &mut AHashMap<usize, Type>) {
        let consumers: LPooled<Vec<usize>> = self
            .standing
            .iter()
            .filter(|(a, (_, s))| {
                !s.produces && !self.declared.contains(*a) && !self.read.contains(*a)
            })
            .map(|(a, _)| *a)
            .collect();
        for a in consumers.iter() {
            let into = self
                .flows_into_variable(*a)
                .and_then(|u| Some((u, self.graph.nodes.get(&u)?.cell.clone())));
            let r = match into {
                Some((u, cell)) => {
                    self.merged.entry(u).or_default().push(*a);
                    Some(Type::TVar(cell))
                }
                None => self.fits(*a),
            };
            if let Some(r) = r {
                subst.insert(*a, r);
                self.read.insert(*a);
            }
        }
    }
}

impl Type {
    /// This type with its open cells read by the bounds they hold
    /// ([the module](self)); `held` are the cells of the definition the
    /// type is shown in, which stand for themselves.
    pub fn read_off(&self, held: &[TVar]) -> Type {
        let mut reading = Reading {
            graph: Graph::default(),
            standing: Standing::default(),
            declared: held
                .iter()
                .filter_map(TVar::open_cell)
                .map(|c| c.cell_addr())
                .collect(),
            merged: AHashMap::default(),
            read: AHashSet::default(),
        };
        reading.stands(self);
        if reading.standing.is_empty() {
            return self.clone();
        }
        reading.graph.explore(self);
        let alias = reading.graph.merge_cycles(&reading.declared);
        let mut cur = substitute(self, &alias);
        let top = match &bound_of(&cur) {
            Type::TVar(tv) => Some(tv.cell_addr()),
            _ => None,
        };
        for _ in 0..ROUNDS {
            reading.stands(&cur);
            let mut subst: AHashMap<usize, Type> = AHashMap::default();
            reading.merge_singletons(&mut subst);
            reading.read_producers(&mut subst);
            if subst.is_empty() {
                reading.read_consumers(&mut subst);
            }
            if subst.is_empty() {
                reading.empty_producers(top, &mut subst);
            }
            if subst.is_empty() {
                break;
            }
            cur = substitute(&cur, &subst);
            reading.graph.explore(&cur);
            let alias = reading.graph.merge_cycles(&reading.declared);
            cur = substitute(&cur, &alias);
        }
        reading.stands(&cur);
        let mut variables: AHashMap<usize, Type> = AHashMap::default();
        for (addr, (cell, _)) in reading.standing.iter() {
            if reading.declared.contains(addr) || !cell.is_open() {
                continue;
            }
            let var = TVar::default();
            var.set_level(cell.level());
            for b in reading.bounds(*addr) {
                var.add_cell_constraint(b)
            }
            variables.insert(*addr, Type::TVar(var));
        }
        substitute(&cur, &variables)
    }
}
