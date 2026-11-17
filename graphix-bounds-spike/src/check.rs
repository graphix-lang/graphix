use crate::{
    select::{Arm, FALSE, Pat, TRUE, unsplit},
    solve::{Def, Kind, Solver, Watch},
    ty::{FnArg, FnTy, Ty, Var, union},
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_types::{
    expr::{
        ApplyExpr, ArgKind, BinOp, BindExpr, Expr, ExprKind, LambdaBody, LambdaExpr, ModuleKind,
        ModPath, Origin, Pattern, SelectExpr, SigKind, StructurePattern, TypeDefBody,
        TypeDefExpr, parser,
    },
    typ::{Bound, FnArgKind, FnType, Mutability, TVar, Type},
};
use netidx_core::path::Path;
use netidx_value::{Typ, Value};
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
    /// What the name stands for; none for `type T;`.
    body: Option<Type>,
    recursive: Option<u32>,
    /// The name an abstract type is known by everywhere.
    nominal: Option<ArcStr>,
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
    /// Where the program's own `mod name;` files are.
    dir: Option<PathBuf>,
    /// The package the program is the root of, if it is one: its modules
    /// are `root::name`.
    root: Option<String>,
    /// The module being checked, outermost first.
    here: Vec<String>,
    /// What the program's own modules bind, by full name.
    mods: HashMap<String, Binding>,
    /// A name a `use` brought in, and what it stands for.
    aliases: HashMap<String, String>,
    /// What each interface file `use`s.
    mod_uses: HashMap<String, Vec<String>>,
    /// The module and position of the expression last entered: where a
    /// refusal or an unsupported construct is.
    pub last: (String, i32, i32),
    /// The names the pattern being built has bound so far.
    in_pattern: Vec<(ArcStr, Var)>,
}

/// A `use` path read in `module`: `self::` is that module, `super::` the
/// one around it.
fn used(path: &[String], module: &str) -> String {
    let around = module.rsplit_once("::").map(|(m, _)| m).unwrap_or("");
    let base = match path[0].as_str() {
        "self" => Some(module),
        "super" => Some(around),
        _ => None,
    };
    match base {
        Some(b) => std::iter::once(b.to_string())
            .filter(|b| !b.is_empty())
            .chain(path[1..].iter().cloned())
            .collect::<Vec<_>>()
            .join("::"),
        None => path.join("::"),
    }
}

fn parts(p: &ModPath) -> Vec<String> {
    Path::parts(&p.0).map(String::from).collect()
}

fn holds_tvar(t: &Type) -> bool {
    match t {
        Type::TVar(_) => true,
        Type::Ref(r) => r.params.iter().any(holds_tvar),
        Type::Fn(ft) => {
            ft.args.iter().any(|a| holds_tvar(&a.typ))
                || ft.vargs.iter().any(holds_tvar)
                || holds_tvar(&ft.rtype)
                || holds_tvar(&ft.throws)
        }
        Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.iter().any(holds_tvar),
        Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => holds_tvar(t),
        Type::Struct(fs) => fs.iter().any(|(_, t, _)| holds_tvar(t)),
        Type::Map(m) => holds_tvar(&m.key) || holds_tvar(&m.value),
        _ => false,
    }
}

/// Whether a lambda's own annotations name a type variable.
fn declares(l: &LambdaExpr) -> bool {
    !l.constraints.is_empty()
        || l.args.iter().any(|a| a.constraint.iter().any(holds_tvar))
        || l.rtype.iter().any(holds_tvar)
}

fn value_of(e: &Expr) -> &Expr {
    match &e.kind {
        ExprKind::ExplicitParens(e) => value_of(e),
        ExprKind::Block { exprs } if !exprs.is_empty() => value_of(&exprs[exprs.len() - 1]),
        _ => e,
    }
}

impl Checker {
    pub fn new(stdlib: PathBuf, dir: Option<PathBuf>, root: Option<String>) -> Self {
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
            dir,
            here: root.iter().cloned().collect(),
            root,
            mods: HashMap::new(),
            aliases: HashMap::new(),
            mod_uses: HashMap::new(),
            last: (String::new(), 0, 0),
            in_pattern: vec![],
        }
    }

    /// Check a program; the type of its last expression, every variable fixed.
    /// `sig` is the text of the interface file beside the program, if it
    /// has one (a package's root).
    pub fn program(&mut self, text: &str, sig: Option<&str>) -> Result<Ty> {
        let exprs = parser::parse(Origin::unspecified(text))?;
        let mut last = Ty::Bot;
        match sig {
            None => {
                for e in exprs.iter() {
                    last = self.infer(e, 0)?
                }
            }
            Some(sig) => {
                let sig = parser::parse_sig(Origin::unspecified(sig))?;
                let prefix = self.here.join("::");
                self.module(&exprs, Some(&sig.items), &prefix, 0)?
            }
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
        // one of the program's own modules, by its interface file
        let inside = match &self.root {
            Some(root) => module.strip_prefix(root.as_str()).map(|m| m.trim_start_matches("::")),
            None => Some(module),
        };
        let own = self.dir.as_ref().zip(inside.filter(|m| !m.is_empty())).map(|(d, module)| {
            let path = module.replace("::", "/");
            let flat = d.join(format!("{path}.gxi"));
            if flat.exists() { flat } else { d.join(format!("{path}/mod.gxi")) }
        });
        let file = match own {
            Some(own) if own.exists() => own,
            _ => file,
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
                SigKind::Use { names, .. } => {
                    let uses = names.iter().map(|i| used(&parts(&i.path), module));
                    self.mod_uses.entry(module.to_string()).or_default().extend(uses)
                }
                _ => (),
            }
        }
        found
    }

    fn typedef(&mut self, module: &str, td: &TypeDefExpr) {
        let name = match module {
            "" => td.name.name.to_string(),
            m => format!("{m}::{}", td.name.name),
        };
        let (body, nominal) = match &td.body {
            TypeDefBody::Alias(body) => (Some(body.clone()), None),
            TypeDefBody::Abstract(rep) => (rep.clone(), Some(ArcStr::from(name.as_str()))),
        };
        let params = td.params.iter().map(|(tv, _)| tv.name.clone()).collect();
        let def = TypeDef { module: module.to_string(), params, body, recursive: None, nominal };
        self.typedefs.insert(name, def);
    }

    /// The full names `path` may stand for, read in `module`, nearest first.
    fn candidates(&self, module: &str, path: &[String]) -> Vec<String> {
        let here = self.here.join("::");
        let module = if module.is_empty() { here.as_str() } else { module };
        let mut names = vec![];
        if let Some(full) = self.aliases.get(&path[0]) {
            names.push(std::iter::once(full.clone()).chain(path[1..].iter().cloned()).collect::<Vec<_>>().join("::"))
        }
        names.push(path.join("::"));
        let empty = vec![];
        let theirs = self.mod_uses.get(module).unwrap_or(&empty);
        let mut out = vec![];
        for name in names {
            // the module itself, then each module around it
            let mut around = module;
            while !around.is_empty() {
                out.push(format!("{around}::{name}"));
                around = around.rsplit_once("::").map(|(m, _)| m).unwrap_or("")
            }
            out.push(name.clone());
            for u in theirs.iter().rev() {
                out.push(format!("{u}::{name}"));
                if u.rsplit("::").next() == Some(path[0].as_str()) {
                    let rest: Vec<&str> = std::iter::once(u.as_str()).chain(path[1..].iter().map(|s| s.as_str())).collect();
                    out.push(rest.join("::"))
                }
            }
            out.extend(self.uses.iter().rev().map(|u| format!("{u}::{name}")));
        }
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
                let t = t?;
                self.s.quantify(&t, 0);
                self.schemes.insert(full.clone(), t);
            }
            let t = self.schemes[&full].clone();
            return Ok(Some(self.s.instantiate(&t, 0, lvl)));
        }
        Ok(None)
    }

    /// The variable a written name stands for; `own` where the innermost
    /// definition declares it, whatever an enclosing one calls by that name.
    fn tvar(&mut self, tv: &TVar, module: &str, own: bool) -> Result<Ty> {
        let found = match own {
            true => self.tvs.last().unwrap().tvs.get(&tv.name),
            false => self.tvs.iter().rev().find_map(|s| s.tvs.get(&tv.name)),
        };
        if let Some(t) = found {
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
        self.named_at(path, Some(args), module, 0)
    }

    /// The type a name stands for at `args`; with none given, at a fresh
    /// variable per parameter.
    fn named_at(&mut self, path: &[String], args: Option<Vec<Ty>>, module: &str, lvl: u32) -> Result<Ty> {
        for full in self.candidates(module, path) {
            if let Some((m, _)) = full.rsplit_once("::") {
                self.load(m);
            }
            let Some(def) = self.typedefs.get(&full) else { continue };
            let n = def.params.len();
            let args = match &args {
                Some(args) => args.clone(),
                None => (0..n).map(|_| self.s.fresh_ty(lvl, Kind::Single)).collect(),
            };
            let def = &self.typedefs[&full];
            if def.params.len() != args.len() {
                bail!("{full} takes {} type arguments", def.params.len())
            }
            if let Some(tag) = def.nominal.clone() {
                // what it is made of, once: seen wherever a definition with
                // a representation is
                if let (Some(body), false) = (def.body.clone(), self.s.reps.contains_key(&tag)) {
                    let (names, home) = (def.params.clone(), def.module.clone());
                    let params: Vec<Var> = names.iter().map(|_| self.s.fresh(0, Kind::Single)).collect();
                    self.s.reps.insert(tag.clone(), Def { params: params.clone(), body: Ty::Bot });
                    let tvs = names.into_iter().zip(params.iter().map(|v| Ty::Var(*v))).collect();
                    self.tvs.push(Scope { tvs, level: 0, rigid: false });
                    let rep = self.conv(&body, &home);
                    self.tvs.pop();
                    self.s.reps.get_mut(&tag).unwrap().body = rep?
                }
                return Ok(Ty::Abs(tag, args.into()));
            }
            let def = &self.typedefs[&full];
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
                let t = body.as_ref().map(|b| c.conv(b, &home)).transpose();
                c.expanding.pop();
                c.tvs.pop();
                Ok(t?.unwrap_or(Ty::Bot))
            };
            let t = expand(self, args)?;
            if let Some(id) = self.typedefs[&full].recursive
                && matches!(self.s.defs[id as usize].body, Ty::Bot)
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
            Type::TVar(tv) => self.tvar(tv, module, false)?,
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
        let local = local
            .or_else(|| self.candidates("", &path).iter().find_map(|c| self.mods.get(c).cloned()));
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

    /// Check a module's body; with an interface, each `val` it declares must
    /// hold what the body binds, and only those names are seen outside.
    fn module(
        &mut self,
        exprs: &[Expr],
        sig: Option<&[graphix_types::expr::SigItem]>,
        prefix: &str,
        lvl: u32,
    ) -> Result<()> {
        for item in sig.into_iter().flatten() {
            match &item.kind {
                SigKind::Use { names, .. } => {
                    for i in names.iter() {
                        let path = parts(&i.path);
                        let full = used(&path, prefix);
                        self.uses.push(full.clone());
                        self.aliases.insert(path[path.len() - 1].clone(), full);
                    }
                }
                SigKind::TypeDef(td) => self.typedef(prefix, td),
                _ => (),
            }
        }
        self.infer_all(exprs, lvl)?;
        for item in sig.into_iter().flatten() {
            if let SigKind::Module(name) = &item.kind {
                let child = ExprKind::Module {
                    name: name.clone(),
                    value: ModuleKind::Unresolved { from_interface: true }.into(),
                };
                self.infer(&child.to_expr(Default::default()), lvl)?;
            }
        }
        let scope = self.scopes.last().unwrap().clone();
        let full = |n: &str| match prefix {
            "" => n.to_string(),
            p => format!("{p}::{n}"),
        };
        let Some(items) = sig else {
            for (n, b) in scope {
                self.mods.insert(full(&n), b);
            }
            return Ok(());
        };
        for item in items.iter() {
            if let SigKind::Bind(val) = &item.kind {
                let made = match scope.get(&val.name.name) {
                    Some(Binding::Mono(t)) => t.clone(),
                    Some(Binding::Poly(l, t)) => self.s.instantiate(t, *l, lvl + 1),
                    None => bail!("the interface declares {}, which the module does not bind", val.name.name),
                };
                // against the declaration its variables are rigid; outside
                // the module they are a scheme's
                let declared = |c: &mut Self, rigid: bool| {
                    c.tvs.push(Scope { tvs: HashMap::new(), level: lvl + 1, rigid });
                    let t = c.conv(&val.typ, prefix);
                    c.tvs.pop();
                    t
                };
                let held = declared(self, true)?;
                self.s.constrain(&made, &held)?;
                let t = declared(self, false)?;
                self.s.quantify(&t, lvl);
                self.mods.insert(full(&val.name.name), Binding::Poly(lvl, t));
            }
        }
        Ok(())
    }

    /// The error an operation that can fail returns.
    fn failure(&self, tag: &str) -> Ty {
        Ty::Error(Rc::new(Ty::Variant(tag.into(), [Ty::prim(Typ::String)].into())))
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
                if let ExprKind::Lambda(_) = &value.kind {
                    let mark = self.s.vars.len();
                    let me = self.s.fresh_ty(lvl + 1, Kind::Single);
                    if b.rec {
                        self.bind(name, Binding::Mono(me.clone()))
                    }
                    let t = self.definition(&b.value, lvl + 1)?;
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
                            self.s.generalize(&t, lvl, mark)?;
                            let t = self.s.instantiate(&t, lvl, lvl + 1);
                            self.s.constrain(&t, &w)?;
                            w
                        }
                    };
                    self.s.generalize(&t, lvl, mark)?;
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
            pattern => {
                let t = self.infer(&b.value, lvl)?;
                let t = match &b.typ {
                    None => t,
                    Some(written) => {
                        let w = self.conv(written, "")?;
                        self.s.constrain(&t, &w)?;
                        w
                    }
                };
                self.destructure(pattern, &t, lvl)?;
                Ok(t)
            }
        }
    }

    /// Bind a pattern's names to the parts of a value: a select of one arm,
    /// so the pattern must take every value.
    fn destructure(&mut self, p: &StructurePattern, t: &Ty, lvl: u32) -> Result<()> {
        self.in_pattern.clear();
        let pat = self.structure(p, lvl)?;
        if let (Ty::Var(_), Some(shape)) = (self.s.shallow(t), pat.admits()) {
            self.s.constrain(t, &unsplit(&shape))?
        }
        let id = self.s.select(vec![Arm { pat, guarded: false }]);
        self.s.watch(t, Watch::Select(id))
    }

    /// The lambda a generic `let` names, past grouping parentheses and the
    /// statements of a block it ends, which are checked on the way.
    fn definition(&mut self, e: &Expr, lvl: u32) -> Result<Ty> {
        match &e.kind {
            ExprKind::ExplicitParens(e) => self.definition(e, lvl),
            ExprKind::Block { exprs } if !exprs.is_empty() => {
                self.scopes.push(HashMap::new());
                let (last, before) = exprs.split_last().unwrap();
                let r = self.infer_all(before, lvl).and_then(|_| self.definition(last, lvl));
                self.scopes.pop();
                r
            }
            ExprKind::Lambda(l) => self.lambda(l, lvl, true),
            _ => self.infer(e, lvl),
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
            match self.tvar(tv, "", true)? {
                Ty::Var(v) | Ty::Rigid(v) => self.bound(v, c, "")?,
                _ => (),
            }
        }
        let mut args = vec![];
        for a in l.args.iter() {
            let name = match &a.pattern {
                StructurePattern::Bind(n) => Some(n.name.clone()),
                _ => None,
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
            match (&name, &a.pattern) {
                (Some(name), _) => self.bind(name, Binding::Mono(ty.clone())),
                (None, StructurePattern::Ignore) => (),
                (None, pattern) => self.destructure(pattern, &ty, lvl)?,
            }
            args.push(FnArg { label, optional, ty })
        }
        // a builtin's type is what its signature says
        let body = match (&l.body, &l.rtype) {
            (LambdaBody::Expr(e), _) => self.infer(e, lvl)?,
            (LambdaBody::Builtin(_), Some(_)) => Ty::Bot,
            (LambdaBody::Builtin(_), None) => unsupported("builtin with no return type")?,
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
            let at = Some((e.pos.line as u32, e.pos.column as u32));
            args.push((label.clone(), self.infer(e, lvl)?, at))
        }
        let out = self.s.fresh_ty(lvl, Kind::Single);
        match self.s.shallow(&f) {
            Ty::Fn(ft) => self.s.call(&ft, &args, &out)?,
            Ty::Var(_) if args.iter().all(|(l, ..)| l.is_none()) => {
                let args = args
                    .into_iter()
                    .map(|(_, ty, _)| FnArg { label: None, optional: false, ty })
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

    fn structure(&mut self, p: &StructurePattern, lvl: u32) -> Result<Pat> {
        // a name the pattern already binds (in another alternative) is that name
        let named = |c: &mut Self, name: &Option<graphix_types::expr::Name>| {
            name.as_ref().map(|n| match c.in_pattern.iter().find(|(m, _)| m == &n.name) {
                Some((_, v)) => *v,
                None => {
                    let v = c.s.fresh(lvl, Kind::Join);
                    c.bind(&n.name, Binding::Mono(Ty::Var(v)));
                    c.in_pattern.push((n.name.clone(), v));
                    v
                }
            })
        };
        let subs = |c: &mut Self, ps: &[StructurePattern]| -> Result<Vec<Pat>> {
            ps.iter().map(|p| c.structure(p, lvl)).collect()
        };
        Ok(match p {
            StructurePattern::Ignore => Pat::Any(None),
            StructurePattern::Bind(n) => Pat::Any(named(self, &Some(n.clone()))),
            StructurePattern::Literal(Value::Bool(b), _) => {
                Pat::Variant(None, if *b { TRUE } else { FALSE }.into(), vec![])
            }
            StructurePattern::Literal(v, _) => {
                Pat::Lit(Ty::prim(Typ::get(v)), matches!(v, Value::Null))
            }
            StructurePattern::Tuple { all, binds } => {
                Pat::Tuple(named(self, all), subs(self, binds)?)
            }
            StructurePattern::Variant { all, tag, binds } => {
                Pat::Variant(named(self, all), tag.clone(), subs(self, binds)?)
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                let all = named(self, all);
                let mut fs = vec![];
                for (n, p, _) in binds.iter() {
                    fs.push((n.clone(), self.structure(p, lvl)?))
                }
                Pat::Struct(all, fs, *exhaustive)
            }
            StructurePattern::Slice { list, all, binds } => Pat::Elems {
                all: named(self, all),
                list: *list,
                elems: subs(self, binds)?,
                open: None,
            },
            StructurePattern::SlicePrefix { list, all, prefix, tail } => Pat::Elems {
                all: named(self, all),
                list: *list,
                elems: subs(self, prefix)?,
                open: Some(named(self, tail)),
            },
            StructurePattern::SliceSuffix { all, head, suffix } => Pat::Elems {
                all: named(self, all),
                list: false,
                elems: subs(self, suffix)?,
                open: Some(named(self, head)),
            },
            StructurePattern::Or(ps) => Pat::Or(subs(self, ps)?),
            StructurePattern::Abstract { all, name, bind, .. } => {
                let all = named(self, all);
                match self.named_at(&parts(name), None, "", lvl)? {
                    Ty::Abs(tag, _) => Pat::Abs(all, tag, Box::new(self.structure(bind, lvl)?)),
                    _ => bail!("{} is not an abstract type", parts(name).join("::")),
                }
            }
        })
    }

    fn pattern(&mut self, p: &Pattern, lvl: u32) -> Result<Arm> {
        self.in_pattern.clear();
        let inner = self.structure(&p.structure_predicate, lvl)?;
        let pat = match &p.type_predicate {
            None => inner,
            Some(t) => {
                let t = self.conv(t, "")?;
                if !self.s.is_ground(&t) {
                    unsupported("a test that holds a variable")?
                }
                if let Pat::Any(Some(v)) = &inner {
                    self.s.constrain(&Ty::Var(*v), &t)?
                }
                Pat::Test(t, Box::new(inner))
            }
        };
        Ok(Arm { pat, guarded: p.guard.is_some() })
    }

    fn select(&mut self, sel: &SelectExpr, lvl: u32) -> Result<Ty> {
        let scrutinee = self.infer(&sel.arg, lvl)?;
        let result = self.s.fresh_ty(lvl, Kind::Join);
        let mut arms = vec![];
        for (p, body) in sel.arms.iter() {
            self.scopes.push(HashMap::new());
            let arm = (|| {
                let arm = self.pattern(p, lvl)?;
                if let Some(g) = &p.guard {
                    let t = self.infer(g, lvl)?;
                    self.s.constrain(&t, &Ty::prim(Typ::Bool))?
                }
                let t = self.infer(body, lvl)?;
                self.s.constrain(&t, &result)?;
                Ok::<_, anyhow::Error>(arm)
            })();
            self.scopes.pop();
            arms.push(arm?)
        }
        // with no arm that admits everything, the arms' tests bound an
        // undeclared scrutinee from above
        let admitted: Option<Vec<Ty>> =
            arms.iter().filter(|a| !a.guarded).map(|a| a.pat.admits()).collect();
        if let (Ty::Var(_), Some(ts)) = (self.s.shallow(&scrutinee), admitted)
            && arms.iter().all(|a| !a.guarded)
        {
            self.s.constrain(&scrutinee, &unsplit(&union(ts)))?
        }
        let id = self.s.select(arms);
        self.s.watch(&scrutinee, Watch::Select(id))?;
        Ok(result)
    }

    fn stripped(&mut self, arg: &Expr, lvl: u32) -> Result<Ty> {
        let t = self.infer(arg, lvl)?;
        let t = self.s.shallow(&t);
        let known = match &t {
            Ty::Var(_) => false,
            Ty::Union(ms) => ms.iter().any(|m| matches!(m, Ty::Error(_))) || self.s.is_ground(&t),
            _ => true,
        };
        match known {
            true => Ok(self.s.strip(&t)),
            false => {
                let out = self.s.fresh_ty(lvl, Kind::Join);
                self.s.strips.push((t, out.clone()));
                Ok(out)
            }
        }
    }

    pub fn infer(&mut self, e: &Expr, lvl: u32) -> Result<Ty> {
        let outer = self.s.at.replace((e.pos.line as u32, e.pos.column as u32));
        self.last = (self.here.join("::"), e.pos.line, e.pos.column);
        let t = self.infer_at(e, lvl);
        self.s.at = outer;
        t
    }

    fn infer_at(&mut self, e: &Expr, lvl: u32) -> Result<Ty> {
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
                    let path = parts(&item.path);
                    let full = match self.aliases.get(&path[0]) {
                        Some(a) => std::iter::once(a.clone()).chain(path[1..].iter().cloned()).collect::<Vec<_>>().join("::"),
                        None => used(&path, &self.here.join("::")),
                    };
                    let name = match &item.rename {
                        Some(n) => n.name.to_string(),
                        None => path[path.len() - 1].clone(),
                    };
                    self.uses.push(full.clone());
                    self.aliases.insert(name, full);
                }
                Ty::Bot
            }
            ExprKind::Module { name, value } => {
                let (exprs, sig) = match &**value {
                    ModuleKind::Resolved { exprs, sig, .. } => (exprs.clone(), sig.clone()),
                    ModuleKind::Dynamic { .. } => unsupported("dynamic module")?,
                    ModuleKind::Unresolved { .. } => {
                        let mut dir = self.dir.clone().ok_or_else(|| Unsupported("module file".into()))?;
                        self.here.iter().skip(self.root.iter().count()).for_each(|m| dir.push(m));
                        let flat = dir.join(format!("{}.gx", name.name));
                        let file = if flat.exists() { flat } else { dir.join(format!("{}/mod.gx", name.name)) };
                        let text = fs::read_to_string(&file).map_err(|_| Unsupported(format!("module file {}", name.name)))?;
                        let sig = fs::read_to_string(file.with_extension("gxi"))
                            .ok()
                            .map(|text| parser::parse_sig(Origin::unspecified(&text)))
                            .transpose()?;
                        (parser::parse(Origin::unspecified(&text))?, sig)
                    }
                };
                self.scopes.push(HashMap::new());
                self.here.push(name.name.to_string());
                let prefix = self.here.join("::");
                let r = self.module(&exprs, sig.as_ref().map(|s| &*s.items), &prefix, lvl);
                self.here.pop();
                self.scopes.pop();
                r?;
                Ty::Bot
            }
            ExprKind::TypeDef(td) => {
                self.typedef(&self.here.join("::"), td);
                Ty::Bot
            }
            ExprKind::Construct { name, arg } => {
                let (t, arg) = (self.named_at(&parts(name), None, "", lvl)?, self.infer(arg, lvl)?);
                let rep = match &t {
                    Ty::Abs(tag, args) => self.s.rep(tag, args),
                    _ => None,
                };
                match rep {
                    Some(rep) => self.s.constrain(&arg, &rep)?,
                    None => bail!("{} has no constructor here", parts(name).join("::")),
                }
                t
            }
            ExprKind::ArrayRef { source, i } => {
                let (t, _) = (self.infer(source, lvl)?, self.infer(i, lvl)?);
                let elem = self.s.fresh_ty(lvl, Kind::Join);
                self.s.constrain(&t, &Ty::Array(Rc::new(elem.clone())))?;
                union([elem, self.failure("ArrayIndexError")])
            }
            ExprKind::ArraySlice { source, start, end } => {
                let t = self.infer(source, lvl)?;
                for e in start.iter().chain(end.iter()) {
                    self.infer(e, lvl)?;
                }
                let elem = self.s.fresh_ty(lvl, Kind::Join);
                let array = Ty::Array(Rc::new(elem));
                self.s.constrain(&t, &array)?;
                union([array, self.failure("ArrayIndexError")])
            }
            ExprKind::Map { args } => {
                let (k, v) = (self.s.fresh_ty(lvl, Kind::Join), self.s.fresh_ty(lvl, Kind::Join));
                for (ke, ve) in args.iter() {
                    let (kt, vt) = (self.infer(ke, lvl)?, self.infer(ve, lvl)?);
                    self.s.constrain(&kt, &k)?;
                    self.s.constrain(&vt, &v)?
                }
                Ty::Map(Rc::new(k), Rc::new(v))
            }
            ExprKind::MapRef { source, key } => {
                let (t, kt) = (self.infer(source, lvl)?, self.infer(key, lvl)?);
                let v = self.s.fresh_ty(lvl, Kind::Join);
                self.s.constrain(&t, &Ty::Map(Rc::new(kt), Rc::new(v.clone())))?;
                union([v, self.failure("MapKeyError")])
            }
            ExprKind::StructWith(w) => {
                let t = self.infer(&w.source, lvl)?;
                for (_, e) in w.replace.iter() {
                    self.infer(e, lvl)?;
                }
                t
            }
            ExprKind::TypeCast { expr, typ } => {
                self.infer(expr, lvl)?;
                union([self.conv(typ, "")?, self.failure("InvalidCast")])
            }
            ExprKind::ByRef(m, e) => {
                Ty::Ref(matches!(m, Mutability::Mut), Rc::new(self.infer(e, lvl)?))
            }
            ExprKind::Deref(e) => {
                let (t, out) = (self.infer(e, lvl)?, self.s.fresh_ty(lvl, Kind::Single));
                self.s.constrain(&t, &Ty::Ref(false, Rc::new(out.clone())))?;
                out
            }
            ExprKind::Connect { name, value, deref: true, .. } => {
                let (target, v) = (self.lookup(name, lvl)?, self.infer(value, lvl)?);
                let held = self.s.fresh_ty(lvl, Kind::Single);
                self.s.constrain(&target, &Ty::Ref(true, Rc::new(held.clone())))?;
                self.s.constrain(&v, &held)?;
                Ty::Bot
            }
            ExprKind::Bind(b) => self.let_bind(b, lvl)?,
            ExprKind::Ref { name } => self.lookup(name, lvl)?,
            // a lambda that declares variables is checked as a definition,
            // where they are rigid, and stands as its one instance
            ExprKind::Lambda(l) if declares(l) => {
                let mark = self.s.vars.len();
                let t = self.lambda(l, lvl + 1, true)?;
                self.s.generalize(&t, lvl, mark)?;
                self.s.instantiate(&t, lvl, lvl)
            }
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
            ExprKind::Select(sel) => self.select(sel, lvl)?,
            ExprKind::Qop { arg, .. } => self.stripped(arg, lvl)?,
            ExprKind::OrNever(arg) => self.stripped(arg, lvl)?,
            ExprKind::Trait(_) | ExprKind::Impl(_) => unsupported("trait")?,
            ExprKind::Seq(_) | ExprKind::SeqMachine(_) => unsupported("seq")?,
            ExprKind::Catch(_) | ExprKind::TryWith(_) => unsupported("catch")?,
            _ => unsupported("expression kind")?,
        })
    }
}
