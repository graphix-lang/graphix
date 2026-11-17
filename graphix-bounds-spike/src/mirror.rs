//! The prototype's solver calls, replayed into the solver ported onto the
//! tree's types (`graphix_types::typ::bounds`), so the port is judged by
//! every program the prototype is. Reads stay with the prototype; only its
//! outermost calls are replayed. A program that uses something not replayed
//! yet is not compared.

use crate::{
    solve::{Arg, Kind, Solver},
    ty::{FnTy, Ty, Var},
};
use arcstr::ArcStr;
use graphix_types::{
    LambdaId,
    env::Env,
    expr::WrittenAt,
    typ::{
        Bound, FnArgKind, FnArgType, FnType, MapType, Mutability, TVar, Type,
        bounds::{Solving, union_of},
        tvar::{AtLevel, Level, RigidGate},
    },
};
use std::collections::HashMap;
use triomphe::Arc;

#[derive(Default)]
pub struct Mirror {
    env: Env,
    port: Solving,
    cells: HashMap<Var, TVar>,
    /// The declared variables' gates: a declared variable stays one for
    /// the whole program here, as `Ty::Rigid` does.
    gates: Vec<RigidGate>,
    /// A definition per depth, to own the cells at that depth.
    owners: Vec<LambdaId>,
    /// How many cells the port had met when the prototype had so many
    /// variables.
    marks: Vec<(usize, usize)>,
    /// The port's first refusal.
    pub refused: Option<String>,
    /// The first thing the program used that is not replayed.
    pub skipped: Option<String>,
}

impl Mirror {
    fn level(&mut self, depth: u32) -> Level {
        while self.owners.len() < depth as usize {
            self.owners.push(LambdaId::new())
        }
        match depth {
            0 => Level::TOP,
            d => Level::Def { depth: d, owner: self.owners[d as usize - 1] },
        }
    }

    fn skip(&mut self, why: impl Into<String>) {
        self.skipped.get_or_insert_with(|| why.into());
    }

    /// The program used `what`, which is not replayed.
    pub fn skipped(&mut self, what: &str) {
        self.skip(what)
    }

    /// The port's verdict on the program: `ok`, `no` and why, or `skip`
    /// and what was not replayed. Where the prototype refused before the
    /// input's end, the port is given the end to refuse at.
    pub fn verdict(&mut self, s: &Solver) -> String {
        if self.live() {
            self.finish(s)
        }
        match (&self.skipped, &self.refused) {
            (Some(why), _) => format!("skip {why}"),
            (None, None) if s.refused_outside => "skip refused outside the solver".into(),
            (None, Some(why)) => format!("no {}", why.replace(['\n', '\t'], " ")),
            (None, None) => "ok".into(),
        }
    }

    fn live(&self) -> bool {
        self.skipped.is_none() && self.refused.is_none()
    }

    fn done(&mut self, s: &Solver, r: anyhow::Result<()>) {
        if let Err(e) = r {
            self.refused.get_or_insert_with(|| e.to_string());
        }
        self.marks.push((s.vars.len(), self.port.mark()))
    }

    /// The cell that stands for the prototype's `v`, made where it is
    /// first met, with what was declared of it.
    fn cell(&mut self, s: &Solver, v: Var, rigid: bool) -> TVar {
        if let Some(tv) = self.cells.get(&v) {
            return tv.clone();
        }
        let info = &s.vars[v.0 as usize];
        // one the prototype made and bounded on its own has no twin here
        if !info.lower.is_empty() {
            self.skip("a variable of the prototype's own")
        }
        let tv = {
            let _at = AtLevel::enter(self.level(info.level));
            TVar::empty_named(ArcStr::from(format!("v{}", v.0)))
        };
        self.cells.insert(v, tv.clone());
        if info.kind == Kind::Join && !rigid {
            tv.set_joins()
        }
        if info.singleton {
            self.port.declare(&tv, Type::Bound(Bound::Singleton))
        }
        for u in info.upper.clone() {
            let u = self.typ(s, &u);
            self.port.declare(&tv, u)
        }
        self.port.track(&tv);
        if rigid {
            self.gates.push(tv.open_rigid())
        }
        tv
    }

    fn fn_typ(&mut self, s: &Solver, ft: &FnTy) -> FnType {
        let args = ft.args.iter().map(|a| FnArgType {
            kind: match &a.label {
                None => FnArgKind::Positional { name: None },
                Some(name) => {
                    FnArgKind::Labeled { name: name.clone(), has_default: a.optional }
                }
            },
            typ: self.typ(s, &a.ty),
        });
        let args: Vec<FnArgType> = args.collect();
        FnType {
            args: Arc::from(args),
            vargs: ft.vargs.as_ref().map(|t| self.typ(s, t)),
            rtype: self.typ(s, &ft.ret),
            throws: self.typ(s, &ft.throws),
            explicit_throws: false,
            quantifiers: Arc::from(Vec::<ArcStr>::new()),
            lambda_ids: Default::default(),
        }
    }

    /// The prototype's type as the tree's.
    fn typ(&mut self, s: &Solver, t: &Ty) -> Type {
        let all = |m: &mut Self, ts: &[Ty]| -> Arc<[Type]> {
            let ts: Vec<Type> = ts.iter().map(|t| m.typ(s, t)).collect();
            Arc::from(ts)
        };
        match t {
            Ty::Bot => Type::Bottom,
            Ty::Any => Type::Any,
            Ty::Prim(p) => Type::Primitive(*p),
            Ty::Array(t) => Type::Array(Arc::new(self.typ(s, t))),
            Ty::List(t) => Type::List(Arc::new(self.typ(s, t))),
            Ty::Error(t) => Type::Error(Arc::new(self.typ(s, t))),
            Ty::Map(k, v) => Type::Map(Arc::new(MapType {
                key: self.typ(s, k),
                value: self.typ(s, v),
            })),
            Ty::Ref(m, t) => {
                let m = if *m { Mutability::Mut } else { Mutability::Shared };
                Type::ByRef(m, Arc::new(self.typ(s, t)))
            }
            Ty::Tuple(ts) => Type::Tuple(all(self, ts)),
            Ty::Variant(tag, ts) => {
                Type::Variant(tag.clone(), all(self, ts), WrittenAt::default())
            }
            Ty::Struct(fs) => {
                let fs: Vec<(ArcStr, Type, WrittenAt)> = fs
                    .iter()
                    .map(|(n, t)| (n.clone(), self.typ(s, t), WrittenAt::default()))
                    .collect();
                Type::Struct(Arc::from(fs))
            }
            Ty::Fn(ft) => Type::Fn(Arc::new(self.fn_typ(s, ft))),
            Ty::Union(ts) => {
                let ts: Vec<Type> = ts.iter().map(|t| self.typ(s, t)).collect();
                union_of(ts)
            }
            Ty::Var(v) => match &s.vars[v.0 as usize].fixed {
                Some(t) if !self.cells.contains_key(v) => self.typ(s, &t.clone()),
                _ => Type::TVar(self.cell(s, *v, false)),
            },
            Ty::Rigid(v) => Type::TVar(self.cell(s, *v, true)),
            Ty::Named(..) => {
                self.skip("a recursive typedef");
                Type::Bottom
            }
            Ty::Abs(..) => {
                self.skip("an abstract type");
                Type::Bottom
            }
            Ty::Trait(_) | Ty::Method(_) => {
                self.skip("a trait");
                Type::Bottom
            }
        }
    }

    pub fn constrain(&mut self, s: &Solver, a: &Ty, b: &Ty) {
        let (a, b) = (self.typ(s, a), self.typ(s, b));
        if self.live() {
            let r = self.port.constrain(&self.env, &a, &b);
            self.done(s, r)
        }
    }

    /// A call of a function whose type is known: its arguments fit its
    /// parameters, and its result goes to `out`.
    pub fn call(&mut self, s: &Solver, ft: &FnTy, args: &[Arg], out: &Ty) {
        let f = Type::Fn(Arc::new(self.fn_typ(s, ft)));
        let args = args.iter().map(|(label, t, _)| FnArgType {
            kind: match label {
                None => FnArgKind::Positional { name: None },
                Some(name) => {
                    FnArgKind::Labeled { name: name.clone(), has_default: false }
                }
            },
            typ: self.typ(s, t),
        });
        let args: Vec<FnArgType> = args.collect();
        let wanted = Type::Fn(Arc::new(FnType {
            args: Arc::from(args),
            vargs: None,
            rtype: self.typ(s, out),
            throws: Type::Any,
            explicit_throws: false,
            quantifiers: Arc::from(Vec::<ArcStr>::new()),
            lambda_ids: Default::default(),
        }));
        if self.live() {
            let r = self.port.constrain(&self.env, &f, &wanted);
            self.done(s, r)
        }
    }

    pub fn quantify(&mut self, s: &Solver, t: &Ty, depth: u32) {
        // a scheme the port cannot read yet is one it is never asked to
        // copy: a use of it is not replayed either
        let before = self.skipped.clone();
        let met: Vec<Var> = self.cells.keys().copied().collect();
        let t = self.typ(s, t);
        if before.is_none() && self.skipped.is_some() {
            self.skipped = None;
            self.cells.retain(|v, _| met.contains(v));
            return;
        }
        if self.live() {
            self.port.quantify(&t, depth);
            self.done(s, Ok(()))
        }
    }

    pub fn generalize(&mut self, s: &Solver, t: &Ty, depth: u32, mark: usize) {
        let t = self.typ(s, t);
        if self.live() {
            let known = self.marks.iter().rev().find(|(vars, _)| *vars <= mark);
            let mark = known.map(|(_, cells)| *cells).unwrap_or(0);
            let r = self.port.generalize(&self.env, &t, depth, mark);
            self.done(s, r)
        }
    }

    /// A use of a scheme: the port's copy stands for the prototype's, cell
    /// for variable.
    pub fn instantiate(&mut self, s: &Solver, t: &Ty, above: u32, at: u32, made: &Ty) {
        let t = self.typ(s, t);
        if self.live() {
            let at = self.level(at);
            let copy = self.port.instantiate(&t, above, at);
            self.align(s, made, &copy);
            self.done(s, Ok(()))
        }
    }

    pub fn finish(&mut self, s: &Solver) {
        if self.live() {
            let r = self.port.finish(&self.env);
            self.done(s, r)
        }
    }

    /// Pair each variable of `made` the mirror has not met with the cell
    /// at its place in `copy`.
    fn align(&mut self, s: &Solver, made: &Ty, copy: &Type) {
        let made = s.shallow(made);
        let zip = |m: &mut Self, xs: &[Ty], ys: &[Type]| match xs.len() == ys.len() {
            true => xs.iter().zip(ys.iter()).for_each(|(x, y)| m.align(s, x, y)),
            false => m.skip("an instance of another shape"),
        };
        match (&made, copy) {
            (Ty::Var(v) | Ty::Rigid(v), Type::TVar(tv)) => {
                if !self.cells.contains_key(v) {
                    self.cells.insert(*v, tv.clone());
                }
            }
            (Ty::Array(x), Type::Array(y))
            | (Ty::List(x), Type::List(y))
            | (Ty::Error(x), Type::Error(y))
            | (Ty::Ref(_, x), Type::ByRef(_, y)) => self.align(s, x, y),
            (Ty::Map(k, v), Type::Map(m)) => {
                self.align(s, k, &m.key);
                self.align(s, v, &m.value)
            }
            (Ty::Tuple(xs), Type::Tuple(ys))
            | (Ty::Variant(_, xs), Type::Variant(_, ys, _)) => zip(self, xs, ys),
            (Ty::Struct(xs), Type::Struct(ys)) if xs.len() == ys.len() => {
                xs.iter().zip(ys.iter()).for_each(|(x, y)| self.align(s, &x.1, &y.1))
            }
            (Ty::Fn(f), Type::Fn(g)) if f.args.len() == g.args.len() => {
                f.args
                    .iter()
                    .zip(g.args.iter())
                    .for_each(|(x, y)| self.align(s, &x.ty, &y.typ));
                if let (Some(x), Some(y)) = (&f.vargs, &g.vargs) {
                    self.align(s, x, y)
                }
                self.align(s, &f.ret, &g.rtype);
                self.align(s, &f.throws, &g.throws)
            }
            (Ty::Union(xs), Type::Set(ys)) => {
                // the members are in each side's own order: the one cell
                // among them is paired, and a union with more is not
                let open = |m: &Self, t: &Ty| match t {
                    Ty::Var(v) => !m.cells.contains_key(v),
                    _ => false,
                };
                let new: Vec<&Ty> = xs.iter().filter(|x| open(self, x)).collect();
                let cells: Vec<&Type> = ys
                    .iter()
                    .filter(|y| matches!(y, Type::TVar(tv) if !tv.is_bound()))
                    .collect();
                match (&new[..], &cells[..]) {
                    ([], _) => (),
                    ([x], [y]) => self.align(s, x, y),
                    _ => self.skip("an instance with a union of variables"),
                }
            }
            _ => (),
        }
    }
}
