use crate::{
    expr::{
        parser, ApplyExpr, BindExpr, CouldNotResolve, Expr, ExprId, ExprKind, LambdaExpr,
        ModPath, ModuleKind, Origin, Pattern, SelectExpr, Sig, SigItem, SigKind, Source,
        StructExpr, StructWithExpr, StructurePattern, TryCatchExpr,
    },
    format_with_flags, PrintFlag,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use futures::future::try_join_all;
use fxhash::{FxHashMap, FxHashSet};
use log::info;
use netidx::{
    path::Path,
    subscriber::{Event, Subscriber},
    utils::Either,
};
use netidx_value::Value;
use poolshark::local::LPooled;
use std::{path::PathBuf, pin::Pin, str::FromStr, time::Duration};
use tokio::{join, task, time::Instant, try_join};
use triomphe::Arc;

#[derive(Debug, Clone)]
pub enum ModuleResolver {
    VFS(FxHashMap<Path, ArcStr>),
    Files(PathBuf),
    Netidx { subscriber: Subscriber, base: Path, timeout: Option<Duration> },
}

impl ModuleResolver {
    /// Parse a comma separated list of module resolvers. Netidx
    /// resolvers are of the form, netidx:/path/in/netidx, and
    /// filesystem resolvers are of the form file:/path/in/fs
    ///
    /// This format is intended to be used in an environment variable,
    /// for example.
    pub fn parse_env(
        subscriber: Subscriber,
        timeout: Option<Duration>,
        s: &str,
    ) -> Result<Vec<Self>> {
        let mut res = vec![];
        for l in escaping::split(s, '\\', ',') {
            let l = l.trim();
            if let Some(s) = l.strip_prefix("netidx:") {
                let base = Path::from_str(s);
                let r = Self::Netidx { subscriber: subscriber.clone(), timeout, base };
                res.push(r);
            } else if let Some(s) = l.strip_prefix("file:") {
                let base = PathBuf::from_str(s)?;
                let r = Self::Files(base);
                res.push(r);
            } else {
                bail!("expected netidx: or file:")
            }
        }
        Ok(res)
    }
}

enum Resolution {
    Resolved { interface: Option<Origin>, implementation: Origin },
    TryNextMethod,
}

fn resolve_from_vfs(
    scope: &ModPath,
    parent: &Arc<Origin>,
    name: &Path,
    vfs: &FxHashMap<Path, ArcStr>,
) -> Resolution {
    macro_rules! ori {
        ($s:expr) => {
            Origin {
                parent: Some(parent.clone()),
                source: Source::Internal(name.clone().into()),
                text: $s.clone(),
            }
        };
    }
    let scoped_intf = scope.append(&format_compact!("{name}.gxi"));
    let scoped_impl = scope.append(&format_compact!("{name}.gx"));
    let implementation = match vfs.get(&scoped_impl) {
        Some(s) => ori!(s),
        None => {
            // try {name}/mod.gx fallback (consistent with file resolver)
            let mod_impl = scope.append(&format_compact!("{name}/mod.gx"));
            match vfs.get(&mod_impl) {
                Some(s) => ori!(s),
                None => return Resolution::TryNextMethod,
            }
        }
    };
    let interface = vfs
        .get(&scoped_intf)
        .or_else(|| {
            let mod_intf = scope.append(&format_compact!("{name}/mod.gxi"));
            vfs.get(&mod_intf)
        })
        .map(|s| ori!(s));
    Resolution::Resolved { interface, implementation }
}

async fn resolve_from_files(
    parent: &Arc<Origin>,
    name: &Path,
    base: &PathBuf,
    errors: &mut Vec<anyhow::Error>,
) -> Resolution {
    macro_rules! ori {
        ($s:expr, $path:expr) => {
            Origin {
                parent: Some(parent.clone()),
                source: Source::File($path),
                text: ArcStr::from($s),
            }
        };
    }
    let mut impl_path = base.clone();
    for part in Path::parts(&name) {
        impl_path.push(part);
    }
    impl_path.set_extension("gx");
    let mut intf_path = impl_path.with_extension("gxi");
    let implementation = match tokio::fs::read_to_string(&impl_path).await {
        Ok(s) => ori!(s, impl_path),
        Err(_) => {
            impl_path.set_extension("");
            impl_path.push("mod.gx");
            intf_path.set_extension("");
            intf_path.push("mod.gxi");
            match tokio::fs::read_to_string(&impl_path).await {
                Ok(s) => ori!(s, impl_path.clone()),
                Err(e) => {
                    errors.push(anyhow::Error::from(e));
                    return Resolution::TryNextMethod;
                }
            }
        }
    };
    let interface = match tokio::fs::read_to_string(&intf_path).await {
        Ok(s) => Some(ori!(s, intf_path)),
        Err(_) => None,
    };
    Resolution::Resolved { interface, implementation }
}

async fn resolve_from_netidx(
    parent: &Arc<Origin>,
    name: &Path,
    subscriber: &Subscriber,
    base: &Path,
    timeout: &Option<Duration>,
    errors: &mut Vec<anyhow::Error>,
) -> Resolution {
    macro_rules! ori {
        ($v:expr, $p:expr) => {
            match $v.last() {
                Event::Update(Value::String(text)) => Origin {
                    parent: Some(parent.clone()),
                    source: Source::Netidx($p.clone()),
                    text,
                },
                Event::Unsubscribed | Event::Update(_) => {
                    errors.push(anyhow!("expected string"));
                    return Resolution::TryNextMethod;
                }
            }
        };
    }
    let impl_path = base.append(&format_compact!("{name}.gx"));
    let intf_path = base.append(&format_compact!("{name}.gxi"));
    let impl_sub = subscriber.subscribe_nondurable_one(impl_path.clone(), *timeout);
    let intf_sub = subscriber.subscribe_nondurable_one(intf_path.clone(), *timeout);
    let (impl_sub, intf_sub) = join!(impl_sub, intf_sub);
    let implementation = match impl_sub {
        Ok(v) => ori!(v, impl_path),
        Err(e) => {
            errors.push(e);
            return Resolution::TryNextMethod;
        }
    };
    let interface = match intf_sub {
        Ok(v) => Some(ori!(v, intf_path)),
        Err(_) => None,
    };
    Resolution::Resolved { interface, implementation }
}

// add modules that are only mentioned in the interface to the implementation
// keep their relative location and order intact
fn add_interface_modules(exprs: Arc<[Expr]>, sig: &Sig) -> Arc<[Expr]> {
    let mut in_sig: LPooled<FxHashSet<&ArcStr>> = LPooled::take();
    let mut after_bind: LPooled<FxHashMap<&ArcStr, &ArcStr>> = LPooled::take();
    let mut after_td: LPooled<FxHashMap<&ArcStr, &ArcStr>> = LPooled::take();
    let mut after_mod: LPooled<FxHashMap<&ArcStr, &ArcStr>> = LPooled::take();
    let mut after_use: LPooled<FxHashMap<&ModPath, &ArcStr>> = LPooled::take();
    let mut first: Option<&ArcStr> = None;
    let mut last: Option<&SigItem> = None;
    for si in &*sig.items {
        if let SigKind::Module(name) = &si.kind {
            in_sig.insert(name);
            match last {
                None => first = Some(name),
                Some(si) => {
                    match &si.kind {
                        SigKind::Bind(v) => after_bind.insert(&v.name, name),
                        SigKind::Module(m) => after_mod.insert(m, name),
                        SigKind::TypeDef(td) => after_td.insert(&td.name, name),
                        SigKind::Use(n) => after_use.insert(n, name),
                    };
                }
            }
        }
        last = Some(si);
    }
    for e in &*exprs {
        if let ExprKind::Module { name, .. } = &e.kind {
            in_sig.remove(&name);
        }
    }
    if in_sig.is_empty() {
        drop(in_sig);
        drop(after_bind);
        drop(after_td);
        drop(after_mod);
        drop(after_use);
        return exprs;
    }
    let synth = |name: &ArcStr| {
        ExprKind::Module {
            name: name.clone(),
            value: ModuleKind::Unresolved { from_interface: true },
        }
        .to_expr_nopos()
    };
    let mut res: LPooled<Vec<Expr>> = LPooled::take();
    if let Some(name) = first.take() {
        res.push(synth(name));
    }
    let mut iter = exprs.iter();
    loop {
        match res.last().map(|e| &e.kind) {
            Some(ExprKind::Bind(v)) => match &v.pattern {
                StructurePattern::Bind(n) => {
                    if let Some(name) = after_bind.remove(n) {
                        in_sig.remove(name);
                        res.push(synth(name));
                        continue;
                    }
                }
                _ => (),
            },
            Some(ExprKind::TypeDef(td)) => {
                if let Some(name) = after_td.remove(&td.name) {
                    in_sig.remove(name);
                    res.push(synth(name));
                    continue;
                }
            }
            Some(ExprKind::Module { name, .. }) => {
                if let Some(name) = after_mod.remove(name) {
                    in_sig.remove(name);
                    res.push(synth(name));
                    continue;
                }
            }
            Some(ExprKind::Use { name }) => {
                if let Some(name) = after_use.remove(name) {
                    in_sig.remove(name);
                    res.push(synth(name));
                    continue;
                }
            }
            _ => (),
        };
        match iter.next() {
            None => break,
            Some(e) => res.push(e.clone()),
        }
    }
    for name in in_sig.drain() {
        res.push(synth(name));
    }
    Arc::from_iter(res.drain(..))
}

async fn resolve(
    scope: ModPath,
    prepend: Option<Arc<ModuleResolver>>,
    resolvers: Arc<[ModuleResolver]>,
    id: ExprId,
    parent: Arc<Origin>,
    pos: SourcePosition,
    name: ArcStr,
    from_interface: bool,
) -> Result<Expr> {
    macro_rules! check {
        ($res:expr) => {
            match $res {
                Resolution::TryNextMethod => continue,
                Resolution::Resolved { interface, implementation } => {
                    (interface, implementation)
                }
            }
        };
    }
    let ts = Instant::now();
    let name = Path::from(name);
    let mut errors: LPooled<Vec<anyhow::Error>> = LPooled::take();
    for r in prepend.iter().map(|r| r.as_ref()).chain(resolvers.iter()) {
        let (interface, implementation) = match r {
            ModuleResolver::VFS(vfs) => {
                check!(resolve_from_vfs(&scope, &parent, &name, vfs))
            }
            ModuleResolver::Files(base) => {
                check!(resolve_from_files(&parent, &name, base, &mut errors).await)
            }
            ModuleResolver::Netidx { subscriber, base, timeout } => {
                let r = resolve_from_netidx(
                    &parent,
                    &name,
                    subscriber,
                    base,
                    timeout,
                    &mut errors,
                )
                .await;
                check!(r)
            }
        };
        let exprs = task::spawn_blocking({
            let ori = implementation.clone();
            move || parser::parse(ori)
        });
        let sig = match &interface {
            None => None,
            Some(ori) => {
                let ori = ori.clone();
                let sig = task::spawn_blocking(move || parser::parse_sig(ori))
                    .await?
                    .with_context(|| format!("parsing file {interface:?}"))?;
                Some(sig)
            }
        };
        let exprs =
            exprs.await?.with_context(|| format!("parsing file {implementation:?}"))?;
        let exprs = match &sig {
            Some(sig) => add_interface_modules(exprs, &sig),
            None => exprs,
        };
        let value = ModuleKind::Resolved { exprs, sig, from_interface };
        let kind = ExprKind::Module { name: name.clone().into(), value };
        format_with_flags(PrintFlag::NoSource | PrintFlag::NoParents, || {
            info!(
                "load and parse {implementation:?} and {interface:?} {:?}",
                ts.elapsed()
            )
        });
        return Ok(Expr { id, ori: Arc::new(implementation), pos, kind });
    }
    bail!("module {name} could not be found {errors:?}")
}

impl Expr {
    pub fn has_unresolved_modules(&self) -> bool {
        self.fold(false, &mut |acc, e| {
            acc || match &e.kind {
                ExprKind::Module { value: ModuleKind::Unresolved { .. }, .. } => true,
                _ => false,
            }
        })
    }

    /// Resolve external modules referenced in the expression using
    /// the resolvers list. Each resolver will be tried in order,
    /// until one succeeds. If no resolver succeeds then an error will
    /// be returned.
    pub async fn resolve_modules<'a>(
        &'a self,
        resolvers: &'a Arc<[ModuleResolver]>,
    ) -> Result<Expr> {
        self.resolve_modules_int(&ModPath::root(), &None, resolvers).await
    }

    async fn resolve_modules_int<'a>(
        &'a self,
        scope: &ModPath,
        prepend: &'a Option<Arc<ModuleResolver>>,
        resolvers: &'a Arc<[ModuleResolver]>,
    ) -> Result<Expr> {
        if self.has_unresolved_modules() {
            self.resolve_modules_inner(scope, prepend, resolvers).await
        } else {
            Ok(self.clone())
        }
    }

    fn resolve_modules_inner<'a>(
        &'a self,
        scope: &'a ModPath,
        prepend: &'a Option<Arc<ModuleResolver>>,
        resolvers: &'a Arc<[ModuleResolver]>,
    ) -> Pin<Box<dyn Future<Output = Result<Expr>> + Send + Sync + 'a>> {
        macro_rules! subexprs {
            ($args:expr) => {{
                try_join_all($args.iter().map(|e| async {
                    e.resolve_modules_int(scope, prepend, resolvers).await
                }))
                .await?
            }};
        }
        macro_rules! subtuples {
            ($args:expr) => {{
                try_join_all($args.iter().map(|(k, e)| async {
                    Ok::<_, anyhow::Error>((
                        k.clone(),
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    ))
                }))
                .await?
            }};
        }
        macro_rules! expr {
            ($kind:expr) => {
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: $kind,
                })
            };
        }
        macro_rules! only_args {
            ($kind:ident, $args:expr) => {
                Box::pin(async move {
                    let args = Arc::from(subexprs!($args));
                    expr!(ExprKind::$kind { args })
                })
            };
        }
        macro_rules! bin_op {
            ($kind:ident, $lhs:expr, $rhs:expr) => {
                Box::pin(async move {
                    let (lhs, rhs) = try_join!(
                        $lhs.resolve_modules_int(scope, prepend, resolvers),
                        $rhs.resolve_modules_int(scope, prepend, resolvers)
                    )?;
                    expr!(ExprKind::$kind { lhs: Arc::from(lhs), rhs: Arc::from(rhs) })
                })
            };
        }
        if !self.has_unresolved_modules() {
            return Box::pin(async { Ok(self.clone()) });
        }
        match self.kind.clone() {
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Use { .. }
            | ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::TypeDef { .. } => Box::pin(async move { Ok(self.clone()) }),
            ExprKind::Module {
                value: ModuleKind::Unresolved { from_interface },
                name,
            } => {
                let (id, pos, prepend, resolvers) =
                    (self.id, self.pos, prepend.clone(), Arc::clone(resolvers));
                Box::pin(async move {
                    let e = resolve(
                        scope.clone(),
                        prepend.clone(),
                        resolvers.clone(),
                        id,
                        self.ori.clone(),
                        pos,
                        name.clone(),
                        from_interface,
                    )
                    .await
                    .with_context(|| CouldNotResolve(name.clone()))?;
                    let scope = ModPath(scope.append(&*name));
                    e.resolve_modules_int(&scope, &prepend, &resolvers).await
                })
            }
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, from_interface },
                name,
            } => Box::pin(async move {
                let prepend = match &self.ori.source {
                    Source::Unspecified | Source::Internal(_) => None,
                    Source::File(p) => {
                        p.parent().map(|p| Arc::new(ModuleResolver::Files(p.into())))
                    }
                    Source::Netidx(p) => resolvers.iter().find_map(|m| match m {
                        ModuleResolver::Netidx { subscriber, timeout, .. } => {
                            Some(Arc::new(ModuleResolver::Netidx {
                                subscriber: subscriber.clone(),
                                base: p.clone(),
                                timeout: *timeout,
                            }))
                        }
                        ModuleResolver::Files(_) | ModuleResolver::VFS(_) => None,
                    }),
                };
                let exprs = try_join_all(exprs.iter().map(|e| async {
                    e.resolve_modules_int(&scope, &prepend, resolvers).await
                }))
                .await?;
                expr!(ExprKind::Module {
                    value: ModuleKind::Resolved {
                        exprs: Arc::from(exprs),
                        sig,
                        from_interface
                    },
                    name,
                })
            }),
            ExprKind::Module {
                name,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                expr!(ExprKind::Module {
                    name,
                    value: ModuleKind::Dynamic { sandbox, sig, source },
                })
            }),
            ExprKind::ExplicitParens(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::ExplicitParens(Arc::new(e)))
            }),
            ExprKind::Do { exprs } => Box::pin(async move {
                let exprs = Arc::from(subexprs!(exprs));
                expr!(ExprKind::Do { exprs })
            }),
            ExprKind::Bind(b) => Box::pin(async move {
                let BindExpr { rec, pattern, typ, value } = &*b;
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Bind(Arc::new(BindExpr {
                    rec: *rec,
                    pattern: pattern.clone(),
                    typ: typ.clone(),
                    value,
                })))
            }),
            ExprKind::StructWith(StructWithExpr { source, replace }) => {
                Box::pin(async move {
                    expr!(ExprKind::StructWith(StructWithExpr {
                        source: Arc::new(
                            source.resolve_modules_int(scope, prepend, resolvers).await?,
                        ),
                        replace: Arc::from(subtuples!(replace)),
                    }))
                })
            }
            ExprKind::Connect { name, value, deref } => Box::pin(async move {
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Connect { name, value: Arc::new(value), deref })
            }),
            ExprKind::Lambda(l) => Box::pin(async move {
                let LambdaExpr { args, vargs, rtype, constraints, throws, body } = &*l;
                let body = match body {
                    Either::Right(s) => Either::Right(s.clone()),
                    Either::Left(e) => Either::Left(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    ),
                };
                let l = LambdaExpr {
                    args: args.clone(),
                    vargs: vargs.clone(),
                    rtype: rtype.clone(),
                    throws: throws.clone(),
                    constraints: constraints.clone(),
                    body,
                };
                expr!(ExprKind::Lambda(Arc::new(l)))
            }),
            ExprKind::TypeCast { expr, typ } => Box::pin(async move {
                let expr = expr.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::TypeCast { expr: Arc::new(expr), typ })
            }),
            ExprKind::Apply(ApplyExpr { args, function }) => Box::pin(async move {
                expr!(ExprKind::Apply(ApplyExpr {
                    args: Arc::from(subtuples!(args)),
                    function
                }))
            }),
            ExprKind::Any { args } => only_args!(Any, args),
            ExprKind::Array { args } => only_args!(Array, args),
            ExprKind::Map { args } => Box::pin(async move {
                let args = Arc::from(subtuples!(args));
                expr!(ExprKind::Map { args })
            }),
            ExprKind::MapRef { source, key } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let key =
                    Arc::new(key.resolve_modules_inner(scope, prepend, resolvers).await?);
                expr!(ExprKind::MapRef { source, key })
            }),
            ExprKind::Tuple { args } => only_args!(Tuple, args),
            ExprKind::StringInterpolate { args } => only_args!(StringInterpolate, args),
            ExprKind::Struct(StructExpr { args }) => Box::pin(async move {
                let args = Arc::from(subtuples!(args));
                expr!(ExprKind::Struct(StructExpr { args }))
            }),
            ExprKind::ArrayRef { source, i } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let i = Arc::new(i.resolve_modules_int(scope, prepend, resolvers).await?);
                expr!(ExprKind::ArrayRef { source, i })
            }),
            ExprKind::ArraySlice { source, start, end } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let start = match start {
                    None => None,
                    Some(e) => Some(Arc::new(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    )),
                };
                let end = match end {
                    None => None,
                    Some(e) => Some(Arc::new(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    )),
                };
                expr!(ExprKind::ArraySlice { source, start, end })
            }),
            ExprKind::Variant { tag, args } => Box::pin(async move {
                let args = Arc::from(subexprs!(args));
                expr!(ExprKind::Variant { tag, args })
            }),
            ExprKind::Select(SelectExpr { arg, arms }) => Box::pin(async move {
                let arg =
                    Arc::new(arg.resolve_modules_int(scope, prepend, resolvers).await?);
                let arms = try_join_all(arms.iter().map(|(p, e)| async {
                    let p = match &p.guard {
                        None => p.clone(),
                        Some(e) => {
                            let e =
                                e.resolve_modules_int(scope, prepend, resolvers).await?;
                            Pattern {
                                guard: Some(e),
                                type_predicate: p.type_predicate.clone(),
                                structure_predicate: p.structure_predicate.clone(),
                            }
                        }
                    };
                    let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                    Ok::<_, anyhow::Error>((p, e))
                }))
                .await?;
                expr!(ExprKind::Select(SelectExpr { arg, arms: Arc::from(arms) }))
            }),
            ExprKind::Qop(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Qop(Arc::new(e)))
            }),
            ExprKind::OrNever(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::OrNever(Arc::new(e)))
            }),
            ExprKind::TryCatch(tc) => Box::pin(async move {
                let exprs = try_join_all(tc.exprs.iter().map(|e| async {
                    e.resolve_modules_int(&scope, &prepend, resolvers).await
                }))
                .await?;
                let handler =
                    tc.handler.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::TryCatch(Arc::new(TryCatchExpr {
                    bind: tc.bind.clone(),
                    constraint: tc.constraint.clone(),
                    handler: Arc::new(handler),
                    exprs: Arc::from_iter(exprs),
                })))
            }),
            ExprKind::ByRef(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::ByRef(Arc::new(e)))
            }),
            ExprKind::Deref(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Deref(Arc::new(e)))
            }),
            ExprKind::Not { expr: e } => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Not { expr: Arc::new(e) })
            }),
            ExprKind::Add { lhs, rhs } => bin_op!(Add, lhs, rhs),
            ExprKind::Sub { lhs, rhs } => bin_op!(Sub, lhs, rhs),
            ExprKind::Mul { lhs, rhs } => bin_op!(Mul, lhs, rhs),
            ExprKind::Div { lhs, rhs } => bin_op!(Div, lhs, rhs),
            ExprKind::Mod { lhs, rhs } => bin_op!(Mul, lhs, rhs),
            ExprKind::And { lhs, rhs } => bin_op!(And, lhs, rhs),
            ExprKind::Or { lhs, rhs } => bin_op!(Or, lhs, rhs),
            ExprKind::Eq { lhs, rhs } => bin_op!(Eq, lhs, rhs),
            ExprKind::Ne { lhs, rhs } => bin_op!(Ne, lhs, rhs),
            ExprKind::Gt { lhs, rhs } => bin_op!(Gt, lhs, rhs),
            ExprKind::Lt { lhs, rhs } => bin_op!(Lt, lhs, rhs),
            ExprKind::Gte { lhs, rhs } => bin_op!(Gte, lhs, rhs),
            ExprKind::Lte { lhs, rhs } => bin_op!(Lte, lhs, rhs),
            ExprKind::Sample { lhs, rhs } => bin_op!(Sample, lhs, rhs),
        }
    }
}
