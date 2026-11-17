use crate::{
    solve::{Def, Kind, Solver, Watch},
    ty::{FnArg, FnTy, Ty, Var, union},
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_types::{
    expr::{
        ApplyExpr, ArgKind, BinOp, BindExpr, Expr, ExprKind, LambdaBody, LambdaExpr,
        ModPath, Origin, SigKind, StructurePattern, TypeDefBody, TypeDefExpr, parser,
    },
    typ::{Bound, FnArgKind, FnType, Mutability, TVar, Type},
};
use netidx_core::path::Path;
use netidx_value::Typ;
use std::{collections::HashMap, fmt, fs, path::PathBuf, rc::Rc};

/// A construct outside the prototype's subset: the program is not judged.
#[derive(Debug)]
pub struct Unsupported(pub String);

impl fmt::Display for Unsupported {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unsupported: {}", self.0)
    }
}

impl std::error::Error for Unsupported {}

fn unsupported<T>(what: impl Into<String>) -> Result<T> {
    Err(Unsupported(what.into()).into())
}

#[derive(Clone)]
enum Binding {
    Mono(Ty),
    /// A scheme: the variables deeper than the level are its own.
    Poly(u32, Ty),
}

struct TypeDef {
    module: String,
    params: Vec<ArcStr>,
    body: Type,
    recursive: Option<u32>,
}

/// Where a written type's variables come from.
struct Scope {
    tvs: HashMap<ArcStr, Ty>,
    level: u32,
    rigid: bool,
}

pub struct Checker {
    pub s: Solver,
    stdlib: PathBuf,
    scopes: Vec<HashMap<ArcStr, Binding>>,
    tvs: Vec<Scope>,
    uses: Vec<String>,
    modules: HashMap<String, bool>,
    vals: HashMap<String, (Type, String)>,
    schemes: HashMap<String, Ty>,
    typedefs: HashMap<String, TypeDef>,
    expanding: Vec<String>,
}

fn parts(p: &ModPath) -> Vec<String> {
    Path::parts(&p.0).map(String::from).collect()
}

fn value_of(e: &Expr) -> &Expr {
    match &e.kind {
        ExprKind::ExplicitParens(e) => value_of(e),
        ExprKind::Block { exprs } if !exprs.is_empty() => value_of(&exprs[exprs.len() - 1]),
        _ => e,
    }
}

impl Checker {
    pub fn new(stdlib: PathBuf) -> Self {
        Checker {
            s: Solver::default(),
            stdlib,
            scopes: vec![HashMap::new()],
            tvs: vec![Scope { tvs: HashMap::new(), level: 0, rigid: false }],
            uses: vec!["core".into()],
            modules: HashMap::new(),
            vals: HashMap::new(),
            schemes: HashMap::new(),
            typedefs: HashMap::new(),
            expanding: vec![],
        }
    }

    /// Check a program; the type of its last expression, every variable fixed.
    pub fn program(&mut self, text: &str) -> Result<Ty> {
        let exprs = parser::parse(Origin::unspecified(text))?;
        let mut last = Ty::Bot;
        for e in exprs.iter() {
            last = self.infer(e, 0)?
        }
        self.s.finish()?;
        Ok(self.s.deep(&last))
    }

    fn load(&mut self, module: &str) -> bool {
        if let Some(found) = self.modules.get(module) {
            return *found;
        }
        let mut segs = module.split("::");
        let pkg = segs.next().unwrap();
        let rest: Vec<&str> = segs.collect();
        let dir = self.stdlib.join(format!("graphix-package-{pkg}/src/graphix"));
        let file = match rest.as_slice() {
            [] => dir.join("mod.gxi"),
            rest => {
                let flat = dir.join(format!("{}.gxi", rest.join("/")));
                match flat.exists() {
                    true => flat,
                    false => dir.join(format!("{}/mod.gxi", rest.join("/"))),
                }
            }
        };
        let sig = fs::read_to_string(&file)
            .ok()
            .and_then(|text| parser::parse_sig(Origin::unspecified(&text)).ok());
        let found = sig.is_some();
        self.modules.insert(module.to_string(), found);
        for item in sig.iter().flat_map(|s| s.items.iter()) {
            match &item.kind {
                SigKind::Bind(b) => {
                    let name = format!("{module}::{}", b.name.name);
                    self.vals.insert(name, (b.typ.clone(), module.to_string()));
                }
                SigKind::TypeDef(td) => self.typedef(module, td),
                _ => (),
            }
        }
        found
    }

    fn typedef(&mut self, module: &str, td: &TypeDefExpr) {
        if let TypeDefBody::Alias(body) = &td.body {
            let name = match module {
                "" => td.name.name.to_string(),
                m => format!("{m}::{}", td.name.name),
            };
            let params = td.params.iter().map(|(tv, _)| tv.name.clone()).collect();
            let def = TypeDef {
                module: module.to_string(),
                params,
                body: body.clone(),
                recursive: None,
            };
            self.typedefs.insert(name, def);
        }
    }

    /// The full names `path` may stand for, read in `module`, nearest first.
    fn candidates(&self, module: &str, path: &[String]) -> Vec<String> {
        let name = path.join("::");
        let mut out = vec![name.clone()];
        if !module.is_empty() {
            out.push(format!("{module}::{name}"))
        }
        out.extend(self.uses.iter().rev().map(|u| format!("{u}::{name}")));
        out
    }

    fn global(&mut self, path: &[String], lvl: u32) -> Result<Option<Ty>> {
        for full in self.candidates("", path) {
            if let Some((module, _)) = full.rsplit_once("::") {
                self.load(module);
            }
            if !self.schemes.contains_key(&full) {
                let Some((typ, module)) = self.vals.get(&full).cloned() else { continue };
                self.tvs.push(Scope { tvs: HashMap::new(), level: 1, rigid: false });
                let t = self.conv(&typ, &module);
                self.tvs.pop();
                self.schemes.insert(full.clone(), t?);
            }
            let t = self.schemes[&full].clone();
            return Ok(Some(self.s.instantiate(&t, 0, lvl)));
        }
        Ok(None)
    }

    fn tvar(&mut self, tv: &TVar, module: &str) -> Result<Ty> {
        if let Some(t) = self.tvs.iter().rev().find_map(|s| s.tvs.get(&tv.name)) {
            return Ok(t.clone());
        }
        let (level, rigid) = {
            let s = self.tvs.last().unwrap();
            (s.level, s.rigid)
        };
        let v = self.s.fresh(level, Kind::Single);
        let t = if rigid { Ty::Rigid(v) } else { Ty::Var(v) };
        self.tvs.last_mut().unwrap().tvs.insert(tv.name.clone(), t.clone());
        for c in tv.cell_constraints() {
            self.bound(v, &c, module)?
        }
        Ok(t)
    }

    fn bound(&mut self, v: Var, c: &Type, module: &str) -> Result<()> {
        match c {
            Type::Bound(Bound::Singleton) => self.s.vars[v.0 as usize].singleton = true,
            Type::Bound(_) => (),
            c => {
                let c = self.conv(c, module)?;
                self.s.vars[v.0 as usize].upper.push(c)
            }
        }
        Ok(())
    }

    fn named(&mut self, path: &[String], params: &[Type], module: &str) -> Result<Ty> {
        let mut args = vec![];
        for p in params {
            args.push(self.conv(p, module)?)
        }
        for full in self.candidates(module, path) {
            if let Some((m, _)) = full.rsplit_once("::") {
                self.load(m);
            }
            let Some(def) = self.typedefs.get(&full) else { continue };
            if def.params.len() != args.len() {
                bail!("{full} takes {} type arguments", def.params.len())
            }
            if let Some(id) = def.recursive {
                return Ok(Ty::Named(id, args.into()));
            }
            if self.expanding.contains(&full) {
                let id = self.s.defs.len() as u32;
                self.s.defs.push(Def { params: vec![], body: Ty::Bot });
                self.typedefs.get_mut(&full).unwrap().recursive = Some(id);
                return Ok(Ty::Named(id, args.into()));
            }
            let (names, body, home) = (def.params.clone(), def.body.clone(), def.module.clone());
            let expand = |c: &mut Self, args: Vec<Ty>| -> Result<Ty> {
                let tvs = names.iter().cloned().zip(args).collect();
                c.tvs.push(Scope { tvs, level: 0, rigid: false });
                c.expanding.push(full.clone());
                let t = c.conv(&body, &home);
                c.expanding.pop();
                c.tvs.pop();
                t
            };
            let t = expand(self, args)?;
            if let Some(id) = self.typedefs[&full].recursive
                && self.s.defs[id as usize].params.len() != names.len()
            {
                let params: Vec<Var> =
                    names.iter().map(|_| self.s.fresh(0, Kind::Single)).collect();
                self.s.defs[id as usize].params = params.clone();
                let body = expand(self, params.iter().map(|v| Ty::Var(*v)).collect())?;
                self.s.defs[id as usize].body = body
            }
            return Ok(t);
        }
        unsupported(format!("type {}", path.join("::")))
    }

    fn conv(&mut self, t: &Type, module: &str) -> Result<Ty> {
        let all = |c: &mut Self, ts: &[Type]| -> Result<Rc<[Ty]>> {
            ts.iter().map(|t| c.conv(t, module)).collect()
        };
        Ok(match t {
            Type::Bottom => Ty::Bot,
            Type::Any => Ty::Any,
            Type::Primitive(p) => Ty::Prim(*p),
            Type::Ref(r) => self.named(&parts(&r.name), &r.params, module)?,
            Type::Fn(ft) => Ty::Fn(Rc::new(self.conv_fn(ft, module)?)),
            Type::Set(ts) => union(all(self, ts)?.iter().cloned()),
            Type::TVar(tv) => self.tvar(tv, module)?,
            Type::Error(t) => Ty::Error(Rc::new(self.conv(t, module)?)),
            Type::Array(t) => Ty::Array(Rc::new(self.conv(t, module)?)),
            Type::List(t) => Ty::List(Rc::new(self.conv(t, module)?)),
            Type::ByRef(m, t) => {
                Ty::Ref(matches!(m, Mutability::Mut), Rc::new(self.conv(t, module)?))
            }
            Type::Tuple(ts) => Ty::Tuple(all(self, ts)?),
            Type::Variant(tag, ts, _) => Ty::Variant(tag.clone(), all(self, ts)?),
            Type::Struct(fs) => {
                let mut out = vec![];
                for (n, t, _) in fs.iter() {
                    out.push((n.clone(), self.conv(t, module)?))
                }
                out.sort_by(|a, b| a.0.cmp(&b.0));
                Ty::Struct(out.into())
            }
            Type::Map(m) => Ty::Map(
                Rc::new(self.conv(&m.key, module)?),
                Rc::new(self.conv(&m.value, module)?),
            ),
            Type::Abstract { .. } => unsupported("abstract type")?,
            Type::App(..) => unsupported("type application")?,
            Type::Hole => unsupported("type hole")?,
            Type::Bound(b) => unsupported(format!("bound {b:?} as a type"))?,
        })
    }

    fn conv_fn(&mut self, ft: &FnType, module: &str) -> Result<FnTy> {
        let mut args = vec![];
        for a in ft.args.iter() {
            let (label, optional) = match &a.kind {
                FnArgKind::Positional { .. } => (None, false),
                FnArgKind::Labeled { name, has_default } => (Some(name.clone()), *has_default),
            };
            args.push(FnArg { label, optional, ty: self.conv(&a.typ, module)? })
        }
        let vargs = match &ft.vargs {
            Some(t) => Some(self.conv(t, module)?),
            None => None,
        };
        Ok(FnTy {
            args,
            vargs,
            ret: self.conv(&ft.rtype, module)?,
            throws: self.conv(&ft.throws, module)?,
        })
    }

    fn lookup(&mut self, name: &ModPath, lvl: u32) -> Result<Ty> {
        let path = parts(name);
        let local = match path.as_slice() {
            [n] => self.scopes.iter().rev().find_map(|s| s.get(n.as_str())).cloned(),
            _ => None,
        };
        match local {
            Some(Binding::Mono(t)) => Ok(t),
            Some(Binding::Poly(l, t)) => Ok(self.s.instantiate(&t, l, lvl)),
            None => match self.global(&path, lvl)? {
                Some(t) => Ok(t),
                None => unsupported(format!("name {}", path.join("::"))),
            },
        }
    }

    fn bind(&mut self, name: &ArcStr, b: Binding) {
        self.scopes.last_mut().unwrap().insert(name.clone(), b);
    }

    fn equal(&mut self, t: &Ty, v: &Ty) -> Result<()> {
        self.s.constrain(t, v)?;
        self.s.constrain(v, t)
    }

    fn number(&mut self) -> Result<Ty> {
        self.named(&["Number".to_string()], &[], "")
    }

    fn infer_all(&mut self, es: &[Expr], lvl: u32) -> Result<Vec<Ty>> {
        es.iter().map(|e| self.infer(e, lvl)).collect()
    }

    /// The type of a collection literal's elements: a variable they all
    /// flow into, which may join them.
    fn joined(&mut self, es: &[Expr], lvl: u32) -> Result<Ty> {
        let elem = self.s.fresh_ty(lvl, Kind::Join);
        for t in self.infer_all(es, lvl)? {
            self.s.constrain(&t, &elem)?
        }
        Ok(elem)
    }

    fn let_bind(&mut self, b: &BindExpr, lvl: u32) -> Result<Ty> {
        let value = value_of(&b.value);
        match &b.pattern {
            StructurePattern::Bind(name) => {
                let name = &name.name;
                if let ExprKind::Ref { name: other } = &value.kind
                    && let [n] = parts(other).as_slice()
                    && let Some(Binding::Poly(l, t)) =
                        self.scopes.iter().rev().find_map(|s| s.get(n.as_str())).cloned()
                    && b.typ.is_none()
                {
                    self.bind(name, Binding::Poly(l, t.clone()));
                    return Ok(self.s.instantiate(&t, l, lvl));
                }
                if let ExprKind::Lambda(l) = &value.kind {
                    let me = self.s.fresh_ty(lvl + 1, Kind::Single);
                    if b.rec {
                        self.bind(name, Binding::Mono(me.clone()))
                    }
                    let t = self.lambda(l, lvl + 1, true)?;
                    self.s.constrain(&t, &me)?;
                    let t = match &b.typ {
                        None => t,
                        Some(written) => {
                            self.tvs.push(Scope {
                                tvs: HashMap::new(),
                                level: lvl + 1,
                                rigid: false,
                            });
                            let w = self.conv(written, "");
                            self.tvs.pop();
                            let w = w?;
                            self.s.constrain(&t, &w)?;
                            w
                        }
                    };
                    self.s.generalize(&t, lvl)?;
                    self.bind(name, Binding::Poly(lvl, t.clone()));
                    return Ok(self.s.instantiate(&t, lvl, lvl));
                }
                let t = self.infer(&b.value, lvl)?;
                let held = match &b.typ {
                    Some(written) => self.conv(written, "")?,
                    None => self.s.fresh_ty(lvl, Kind::Single),
                };
                self.s.constrain(&t, &held)?;
                self.bind(name, Binding::Mono(held.clone()));
                Ok(held)
            }
            StructurePattern::Ignore => self.infer(&b.value, lvl),
            StructurePattern::Tuple { all: None, binds } => {
                let t = self.infer(&b.value, lvl)?;
                if let Some(written) = &b.typ {
                    let w = self.conv(written, "")?;
                    self.s.constrain(&t, &w)?
                }
                for (i, p) in binds.iter().enumerate() {
                    match p {
                        StructurePattern::Ignore => (),
                        StructurePattern::Bind(name) => {
                            let out = self.s.fresh_ty(lvl, Kind::Single);
                            self.s.watch(&t, Watch::Index { i, out: out.clone() })?;
                            self.bind(&name.name, Binding::Mono(out))
                        }
                        _ => unsupported("nested let pattern")?,
                    }
                }
                Ok(t)
            }
            _ => unsupported("let pattern"),
        }
    }

    fn lambda(&mut self, l: &LambdaExpr, lvl: u32, generic: bool) -> Result<Ty> {
        self.scopes.push(HashMap::new());
        self.tvs.push(Scope { tvs: HashMap::new(), level: lvl, rigid: generic });
        let r = self.lambda_in(l, lvl);
        self.tvs.pop();
        self.scopes.pop();
        r
    }

    fn lambda_in(&mut self, l: &LambdaExpr, lvl: u32) -> Result<Ty> {
        if l.vargs.is_some() {
            unsupported("variadic lambda")?
        }
        for (tv, c) in l.constraints.iter() {
            match self.tvar(tv, "")? {
                Ty::Var(v) | Ty::Rigid(v) => self.bound(v, c, "")?,
                _ => (),
            }
        }
        let mut args = vec![];
        for a in l.args.iter() {
            let name = match &a.pattern {
                StructurePattern::Bind(n) => Some(n.name.clone()),
                StructurePattern::Ignore => None,
                _ => unsupported("argument pattern")?,
            };
            let ty = match &a.constraint {
                Some(t) => self.conv(t, "")?,
                None => self.s.fresh_ty(lvl, Kind::Single),
            };
            let (label, optional) = match &a.kind {
                ArgKind::Positional => (None, false),
                ArgKind::Labeled => (name.clone(), false),
                ArgKind::Defaulted(d) => {
                    let td = self.infer(d, lvl)?;
                    self.s.constrain(&td, &ty)?;
                    (name.clone(), true)
                }
            };
            if let Some(name) = &name {
                self.bind(name, Binding::Mono(ty.clone()))
            }
            args.push(FnArg { label, optional, ty })
        }
        let body = match &l.body {
            LambdaBody::Expr(e) => self.infer(e, lvl)?,
            LambdaBody::Builtin(_) => unsupported("builtin body")?,
        };
        let ret = match &l.rtype {
            None => body,
            Some(t) => {
                let ret = self.conv(t, "")?;
                self.s.constrain(&body, &ret)?;
                ret
            }
        };
        Ok(Ty::Fn(Rc::new(FnTy { args, vargs: None, ret, throws: Ty::Bot })))
    }

    fn apply(&mut self, a: &ApplyExpr, lvl: u32) -> Result<Ty> {
        let f = self.infer(&a.function, lvl)?;
        let mut args = vec![];
        for (label, e) in a.args.iter() {
            args.push((label.clone(), self.infer(e, lvl)?))
        }
        let out = self.s.fresh_ty(lvl, Kind::Single);
        match self.s.shallow(&f) {
            Ty::Fn(ft) => self.s.call(&ft, &args, &out)?,
            Ty::Var(_) if args.iter().all(|(l, _)| l.is_none()) => {
                let args = args
                    .into_iter()
                    .map(|(_, ty)| FnArg { label: None, optional: false, ty })
                    .collect();
                let want = FnTy { args, vargs: None, ret: out.clone(), throws: Ty::Any };
                self.s.constrain(&f, &Ty::Fn(Rc::new(want)))?
            }
            f => self.s.watch(&f, Watch::Call { args, out: out.clone() })?,
        }
        Ok(out)
    }

    fn binop(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr, lvl: u32) -> Result<Ty> {
        let (l, r) = (self.infer(lhs, lvl)?, self.infer(rhs, lvl)?);
        let bool = Ty::prim(Typ::Bool);
        let same = |c: &mut Self, singleton: bool| -> Result<Ty> {
            let v = c.s.fresh(lvl, Kind::Single);
            c.s.vars[v.0 as usize].singleton = singleton;
            let v = Ty::Var(v);
            c.equal(&l, &v)?;
            c.equal(&r, &v)?;
            Ok(v)
        };
        use BinOp::*;
        Ok(match op {
            Eq | Ne | Lt | Gt | Lte | Gte => {
                same(self, false).map_err(|e| anyhow!("cannot compare: {e}"))?;
                bool
            }
            And | Or => {
                self.s.constrain(&l, &bool)?;
                self.s.constrain(&r, &bool)?;
                bool
            }
            Add | Sub | Mul | Div | Mod | CheckedAdd | CheckedSub | CheckedMul
            | CheckedDiv | CheckedMod => {
                let v = same(self, true).map_err(|e| anyhow!("cannot compute: {e}"))?;
                let number = self.number()?;
                self.s.constrain(&v, &number).map_err(|e| anyhow!("cannot compute: {e}"))?;
                match op.is_checked() {
                    false => v,
                    true => {
                        let payload =
                            Ty::Variant("ArithError".into(), [Ty::prim(Typ::String)].into());
                        union([v, Ty::Error(Rc::new(payload))])
                    }
                }
            }
            Sample | StrictSample | BankedSample => r,
        })
    }

    pub fn infer(&mut self, e: &Expr, lvl: u32) -> Result<Ty> {
        Ok(match &e.kind {
            ExprKind::NoOp => Ty::Bot,
            ExprKind::Constant(v) => Ty::prim(Typ::get(v)),
            ExprKind::ExplicitParens(e) => self.infer(e, lvl)?,
            ExprKind::Block { exprs } => {
                self.scopes.push(HashMap::new());
                let ts = self.infer_all(exprs, lvl);
                self.scopes.pop();
                ts?.pop().unwrap_or(Ty::Bot)
            }
            ExprKind::Use { names, .. } => {
                for item in names.iter() {
                    self.uses.push(parts(&item.path).join("::"))
                }
                Ty::Bot
            }
            ExprKind::TypeDef(td) => match &td.body {
                TypeDefBody::Alias(_) => {
                    self.typedef("", td);
                    Ty::Bot
                }
                TypeDefBody::Abstract(_) => unsupported("abstract type")?,
            },
            ExprKind::Bind(b) => self.let_bind(b, lvl)?,
            ExprKind::Ref { name } => self.lookup(name, lvl)?,
            ExprKind::Lambda(l) => self.lambda(l, lvl, false)?,
            ExprKind::Apply(a) => self.apply(a, lvl)?,
            ExprKind::Connect { name, value, deref: false, .. } => {
                let target = self.lookup(name, lvl)?;
                let v = self.infer(value, lvl)?;
                self.s.constrain(&v, &target)?;
                Ty::Bot
            }
            ExprKind::StringInterpolate { args } => {
                self.infer_all(args, lvl)?;
                Ty::prim(Typ::String)
            }
            ExprKind::StructRef { source, field } => {
                let (t, out) = (self.infer(source, lvl)?, self.s.fresh_ty(lvl, Kind::Single));
                self.s.watch(&t, Watch::Field { field: field.name.clone(), out: out.clone() })?;
                out
            }
            ExprKind::TupleRef { source, field } => {
                let (t, out) = (self.infer(source, lvl)?, self.s.fresh_ty(lvl, Kind::Single));
                self.s.watch(&t, Watch::Index { i: *field, out: out.clone() })?;
                out
            }
            ExprKind::Never { typ, args } => {
                self.infer_all(args, lvl)?;
                match typ {
                    Some(t) => self.conv(t, "")?,
                    None => Ty::Bot,
                }
            }
            ExprKind::Any { args } => self.joined(args, lvl)?,
            ExprKind::Array { args } => Ty::Array(Rc::new(self.joined(args, lvl)?)),
            ExprKind::List { args } => Ty::List(Rc::new(self.joined(args, lvl)?)),
            ExprKind::Tuple { args } => Ty::Tuple(self.infer_all(args, lvl)?.into()),
            ExprKind::Variant { tag, args } => {
                Ty::Variant(tag.clone(), self.infer_all(args, lvl)?.into())
            }
            ExprKind::Struct(s) => {
                let mut fs = vec![];
                for (n, e) in s.args.iter() {
                    fs.push((n.clone(), self.infer(e, lvl)?))
                }
                fs.sort_by(|a, b| a.0.cmp(&b.0));
                Ty::Struct(fs.into())
            }
            ExprKind::BinOp { op, lhs, rhs } => self.binop(*op, lhs, rhs, lvl)?,
            ExprKind::Not { expr } => {
                let t = self.infer(expr, lvl)?;
                self.s.constrain(&t, &Ty::prim(Typ::Bool))?;
                Ty::prim(Typ::Bool)
            }
            ExprKind::Neg(e) => {
                let (t, number) = (self.infer(e, lvl)?, self.number()?);
                self.s.constrain(&t, &number)?;
                t
            }
            ExprKind::Select(_) => unsupported("select")?,
            ExprKind::Qop { .. } | ExprKind::OrNever(_) => unsupported("? or $")?,
            ExprKind::TypeCast { .. } => unsupported("cast")?,
            ExprKind::Module { .. } => unsupported("module")?,
            ExprKind::Trait(_) | ExprKind::Impl(_) => unsupported("trait")?,
            ExprKind::Seq(_) | ExprKind::SeqMachine(_) => unsupported("seq")?,
            ExprKind::Catch(_) | ExprKind::TryWith(_) => unsupported("catch")?,
            ExprKind::ByRef(..) | ExprKind::Deref(_) => unsupported("reference")?,
            ExprKind::Connect { .. } => unsupported("write through a reference")?,
            ExprKind::ArrayRef { .. } | ExprKind::ArraySlice { .. } => unsupported("index")?,
            ExprKind::MapRef { .. } | ExprKind::Map { .. } => unsupported("map")?,
            ExprKind::StructWith(_) => unsupported("struct update")?,
            ExprKind::Construct { .. } => unsupported("abstract constructor")?,
            _ => unsupported("expression kind")?,
        })
    }
}
