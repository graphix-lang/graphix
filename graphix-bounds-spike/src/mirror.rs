//! The prototype's solver calls, replayed into the solver ported onto the
//! tree's types (`graphix_types::typ::bounds`), so the port is judged by
//! every program the prototype is. Reads stay with the prototype; only its
//! outermost calls are replayed. A program that uses something not replayed
//! yet is not compared.

use crate::{
    select::Pat,
    solve::{Arg, Kind, Solver, Watch},
    ty::{FnTy, Ty, Var},
};
use ahash::AHashMap;
use arcstr::ArcStr;
use graphix_types::{
    LambdaId,
    env::Env,
    env::{ImplDef, TraitMethodSig},
    expr::{ModPath, TraitMethod, TypeDefBody, WrittenAt, parser::parse_type},
    typ::{
        Bound, FnArgKind, FnArgType, FnType, MapType, Mutability, TVar, Type,
        bounds::{self, Solving, Use, union_of},
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
    /// The prototype's recursive typedefs the mirror's environment has.
    defs: Vec<u32>,
    /// The prototype's abstract types it has, each as `A<its place here>`.
    abs: Vec<ArcStr>,
    /// The prototype's traits it has, each as `R<its place here>`.
    traits: Vec<ArcStr>,
    /// A typedef's body is being translated: a name in it is as written.
    in_body: bool,
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
        if std::env::var_os("BOUNDS_DEBUG").is_some() {
            eprintln!(
                "cell v{} rigid={rigid} upper={} lower={}",
                v.0,
                info.upper.len(),
                info.lower.len()
            );
            if std::env::var_os("BOUNDS_TRACE").is_some() && rigid {
                eprintln!("{}", std::backtrace::Backtrace::force_capture())
            }
        }
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

    /// Define `name` in the mirror's environment: `params` formals, named
    /// `p0` on, and the body `body` gives once the prototype's variables
    /// `vars` stand for them.
    fn define(
        &mut self,
        s: &Solver,
        name: &str,
        vars: &[Var],
        params: usize,
        body: impl FnOnce(&mut Self, &Solver) -> TypeDefBody,
    ) {
        let formals: Vec<(TVar, Option<Type>)> = (0..params)
            .map(|i| (TVar::empty_named(ArcStr::from(format!("p{i}"))), None))
            .collect();
        for (v, (tv, _)) in vars.iter().zip(formals.iter()) {
            self.cells.insert(*v, tv.clone());
        }
        let outer = std::mem::replace(&mut self.in_body, true);
        let body = body(self, s);
        self.in_body = outer;
        let made = self.env.deftype(
            &ModPath::root(),
            name,
            Arc::from(formals),
            &body,
            true,
            None,
            Default::default(),
            Default::default(),
            Arc::new(Default::default()),
        );
        match made {
            Ok(_) => self.env.seed_typedef_refs(),
            Err(_) => self.skip("a typedef the environment refuses"),
        }
    }

    /// The name `name` the mirror's environment defines, at `args`.
    fn applied(&mut self, s: &Solver, name: &str, args: &[Ty]) -> Type {
        let formals: Vec<String> = (0..args.len()).map(|i| format!("'q{i}")).collect();
        let written = match args.len() {
            0 => name.to_string(),
            _ => format!("{name}<{}>", formals.join(", ")),
        };
        let Ok(t) = parse_type(&written) else {
            self.skip("a typedef's name");
            return Type::Bottom;
        };
        let at: AHashMap<ArcStr, Type> = args
            .iter()
            .enumerate()
            .map(|(i, a)| (ArcStr::from(format!("q{i}")), self.typ(s, a)))
            .collect();
        match self.in_body {
            true => t.replace_tvars(&at),
            false => {
                let t = t.scope_refs(&ModPath::root()).replace_tvars(&at);
                t.seed_refs(&self.env);
                t
            }
        }
    }

    /// A recursive typedef of the prototype's at `args`: a name the
    /// mirror's environment defines, as `T<id>`.
    fn named(&mut self, s: &Solver, id: u32, args: &[Ty]) -> Type {
        if !self.defs.contains(&id) {
            let def = &s.defs[id as usize];
            if matches!(def.body, Ty::Bot) {
                self.skip("a typedef still being read");
                return Type::Bottom;
            }
            self.defs.push(id);
            let (vars, body) = (def.params.clone(), def.body.clone());
            self.define(s, &format!("T{id}"), &vars, vars.len(), |m, s| {
                TypeDefBody::Alias(m.typ(s, &body))
            })
        }
        self.applied(s, &format!("T{id}"), args)
    }

    /// An abstract type of the prototype's at `args`: a name the mirror's
    /// environment defines, as `A<n>`, over what the prototype knows it is
    /// made of.
    fn abstract_type(&mut self, s: &Solver, tag: &ArcStr, args: &[Ty]) -> Type {
        let n = match self.abs.iter().position(|t| t == tag) {
            Some(n) => n,
            None => {
                let n = self.abs.len();
                self.abs.push(tag.clone());
                match s.reps.get(tag) {
                    Some(def) if matches!(def.body, Ty::Bot) => {
                        self.skip("an abstract type still being read")
                    }
                    Some(def) => {
                        let (vars, body) = (def.params.clone(), def.body.clone());
                        self.define(s, &format!("A{n}"), &vars, vars.len(), |m, s| {
                            TypeDefBody::Abstract(Some(m.typ(s, &body)))
                        })
                    }
                    None => self.define(s, &format!("A{n}"), &[], args.len(), |_, _| {
                        TypeDefBody::Abstract(None)
                    }),
                }
                n
            }
        };
        self.applied(s, &format!("A{n}"), args)
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
            Ty::Named(id, args) => self.named(s, *id, args),
            Ty::Abs(tag, args) => self.abstract_type(s, tag, args),
            Ty::Trait(name) => match self.traits.iter().position(|t| t == name) {
                Some(n) => self.applied(s, &format!("R{n}"), &[]),
                None => {
                    self.skip("a trait the mirror has not met");
                    Type::Bottom
                }
            },
            Ty::Method(_) => {
                self.skip("a method of a constructor trait");
                Type::Bottom
            }
        }
    }

    pub fn constrain(&mut self, s: &Solver, a: &Ty, b: &Ty) {
        let (a, b) = (self.typ(s, a), self.typ(s, b));
        if std::env::var_os("BOUNDS_DEBUG").is_some() {
            eprintln!("constrain {a} <= {b}")
        }
        if self.live() {
            let r = self.port.constrain(&self.env, &a, &b);
            self.done(s, r)
        }
    }

    /// A call of a function whose type is known.
    pub fn call(&mut self, s: &Solver, ft: &FnTy, args: &[Arg], out: &Ty) {
        let ft = self.fn_typ(s, ft);
        let args: Vec<_> =
            args.iter().map(|(label, t, _)| (label.clone(), self.typ(s, t))).collect();
        let out = self.typ(s, out);
        if std::env::var_os("BOUNDS_DEBUG").is_some() {
            let args: Vec<String> = args.iter().map(|(_, t)| t.to_string()).collect();
            eprintln!(
                "call {} ({}) -> {out}",
                Type::Fn(Arc::new(ft.clone())),
                args.join(", ")
            )
        }
        if self.live() {
            let r = self.port.call(&self.env, &ft, &args, &out);
            self.done(s, r)
        }
    }

    /// The prototype's pattern as the port's.
    fn pat(&mut self, s: &Solver, p: &Pat) -> bounds::Pat {
        let named = |m: &mut Self, v: &Option<Var>| v.map(|v| m.cell(s, v, false));
        let all = |m: &mut Self, ps: &[Pat]| -> Vec<bounds::Pat> {
            ps.iter().map(|p| m.pat(s, p)).collect()
        };
        match p {
            Pat::Any(v) => bounds::Pat::Any(named(self, v)),
            Pat::Test(t, p) => {
                bounds::Pat::Test(self.typ(s, t), Box::new(self.pat(s, p)))
            }
            Pat::Lit(t, whole) => bounds::Pat::Lit(self.typ(s, t), *whole),
            Pat::Tuple(v, ps) => bounds::Pat::Tuple(named(self, v), all(self, ps)),
            Pat::Variant(v, tag, ps) => {
                bounds::Pat::Variant(named(self, v), tag.clone(), all(self, ps))
            }
            Pat::Struct(v, fs, exhaustive) => {
                let v = named(self, v);
                let fs = fs.iter().map(|(n, p)| (n.clone(), self.pat(s, p))).collect();
                bounds::Pat::Struct(v, fs, *exhaustive)
            }
            Pat::Elems { all: v, list, elems, open } => bounds::Pat::Elems {
                all: named(self, v),
                list: *list,
                elems: all(self, elems),
                open: open.as_ref().map(|o| named(self, o)),
            },
            Pat::Or(ps) => bounds::Pat::Or(all(self, ps)),
            Pat::Abs(v, tag, p) => {
                // the type at whatever parameters, to learn which it is
                let params = s.reps.get(tag).map(|d| d.params.len());
                let any = vec![Ty::Any; params.unwrap_or(0)];
                let known = params.is_some() || self.abs.contains(tag);
                let whole = self.abstract_type(s, tag, &any);
                match (known, whole.lookup_ref(&self.env)) {
                    (true, Ok(Type::Abstract { id, .. })) => {
                        bounds::Pat::Abs(named(self, v), id, Box::new(self.pat(s, p)))
                    }
                    _ => {
                        self.skip("an abstract type the mirror has not met");
                        bounds::Pat::Any(None)
                    }
                }
            }
            Pat::Whole(v, p) => {
                bounds::Pat::Whole(self.cell(s, *v, false), Box::new(self.pat(s, p)))
            }
        }
    }

    /// The program declares the trait `name`: the mirror's environment
    /// declares it as `R<n>`, with the methods as written.
    pub fn trait_declared(&mut self, name: &str, methods: &[TraitMethod]) {
        if self.traits.iter().any(|t| t == name) {
            return;
        }
        let sigs = methods.iter().map(|m| TraitMethodSig {
            name: m.name.name.clone(),
            pos: Default::default(),
            typ: m.typ.clone(),
            self_index: m.self_index,
            requires: Arc::from(Vec::<ArcStr>::new()),
        });
        let sigs: Vec<TraitMethodSig> = sigs.collect();
        let n = self.traits.len();
        let made = self.env.deftrait(
            &ModPath::root(),
            &ArcStr::from(format!("R{n}")),
            sigs.into_iter(),
            None,
            Default::default(),
            Arc::new(Default::default()),
        );
        match made {
            Ok(_) => self.traits.push(ArcStr::from(name)),
            // one the environment cannot hold is one no program is
            // compared over
            Err(_) => (),
        }
    }

    /// The program implements the trait `name` for `target`.
    pub fn implemented(&mut self, s: &Solver, name: &ArcStr, target: &Ty) {
        let Some(n) = self.traits.iter().position(|t| t == name) else { return };
        let before = self.skipped.clone();
        let met: Vec<Var> = self.cells.keys().copied().collect();
        let target = self.typ(s, target);
        if before.is_none() && self.skipped.is_some() {
            self.skipped = None;
            self.cells.retain(|v, _| met.contains(v));
            return;
        }
        let Ok(Some(trait_id)) = self
            .env
            .lookup_trait(&ModPath::root(), &ModPath::from_iter([format!("R{n}")]))
        else {
            return;
        };
        let mut params = AHashMap::default();
        target.collect_tvars(&mut params);
        let _ = self.env.register_impl(Arc::new(ImplDef {
            trait_id,
            target,
            params: Arc::from_iter(params.into_values()),
            scope: ModPath::root(),
            methods: Default::default(),
            declared: false,
            pos: Default::default(),
            ori: Arc::new(Default::default()),
        }));
    }

    /// A `?` or `$` whose operand is not decided yet.
    pub fn hold_strip(
        &mut self,
        s: &Solver,
        operand: &Ty,
        out: &Ty,
        raised: &Option<Ty>,
    ) {
        let (operand, out) = (self.typ(s, operand), self.typ(s, out));
        let raised = raised.as_ref().map(|t| self.typ(s, t));
        if self.live() {
            self.port.hold_strip(operand, out, raised);
            self.done(s, Ok(()))
        }
    }

    /// A use of whatever `t` turns out to be.
    pub fn watch(&mut self, s: &Solver, t: &Ty, w: &Watch) {
        let u = match w {
            Watch::Field { field, out } => {
                Use::Field { field: field.clone(), out: self.typ(s, out) }
            }
            Watch::Index { i, out } => Use::Index { i: *i, out: self.typ(s, out) },
            Watch::Call { args, out } => Use::Call {
                args: args.iter().map(|(l, t, _)| (l.clone(), self.typ(s, t))).collect(),
                out: self.typ(s, out),
            },
            Watch::With { fields, out } => Use::With {
                fields: fields.iter().map(|(n, t)| (n.clone(), self.typ(s, t))).collect(),
                out: self.typ(s, out),
            },
            Watch::Select(id) => {
                let arms = s.selects[*id].arms.iter().map(|a| bounds::Arm {
                    pat: self.pat(s, &a.pat),
                    guarded: a.guarded,
                });
                let arms: Vec<bounds::Arm> = arms.collect();
                let at = ArcStr::from(s.selects[*id].at.as_str());
                Use::Select(self.port.select(arms, at))
            }
            Watch::Method { .. } => return self.skip("calling a method"),
        };
        let t = self.typ(s, t);
        if self.live() {
            let r = self.port.watch(&self.env, &t, u);
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

    /// A use of a scheme: each copy the port made stands for the copy the
    /// prototype made of the same variable.
    pub fn instantiate(&mut self, s: &Solver, t: &Ty, above: u32, at: u32, made: &Ty) {
        let t = self.typ(s, t);
        if self.live() {
            let at = self.level(at);
            let mut copies = AHashMap::default();
            let copy = self.port.instantiate_into(&t, above, at, &mut copies);
            if std::env::var_os("BOUNDS_DEBUG").is_some() {
                eprintln!("instantiate {t} => {copy}")
            }
            for (of, copied) in s.copied.iter() {
                let cell = self.cells.get(of).and_then(|c| copies.get(&c.cell_addr()));
                if let Some(cell) = cell.cloned() {
                    self.cells.entry(*copied).or_insert(cell);
                }
            }
            // a variable the result shows that only one side copied is
            // paired by its place
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
                // the members are in each side's own order: each is paired
                // with the one of its kind, and the one new variable among
                // them with the one cell
                let kin = |x: &Ty, y: &Type| match (x, y) {
                    (Ty::Array(_), Type::Array(_))
                    | (Ty::List(_), Type::List(_))
                    | (Ty::Error(_), Type::Error(_))
                    | (Ty::Map(..), Type::Map(_))
                    | (Ty::Fn(_), Type::Fn(_))
                    | (Ty::Ref(..), Type::ByRef(..)) => true,
                    (Ty::Tuple(a), Type::Tuple(b)) => a.len() == b.len(),
                    (Ty::Variant(t, a), Type::Variant(u, b, _)) => {
                        t == u && a.len() == b.len()
                    }
                    (Ty::Struct(a), Type::Struct(b)) => {
                        a.len() == b.len()
                            && a.iter().zip(b.iter()).all(|(x, y)| x.0 == y.0)
                    }
                    _ => false,
                };
                for x in xs.iter() {
                    let mut of_kind = ys.iter().filter(|y| kin(x, y));
                    match (of_kind.next(), of_kind.next()) {
                        (Some(y), None) => self.align(s, x, y),
                        (Some(_), Some(_)) => {
                            let mut new = false;
                            x.each(&mut |t| {
                                new |=
                                    matches!(t, Ty::Var(v) if !self.cells.contains_key(v))
                            });
                            if new {
                                self.skip("an instance with a union of one kind")
                            }
                        }
                        (None, _) => (),
                    }
                }
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
