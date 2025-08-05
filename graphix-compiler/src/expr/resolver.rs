use crate::{
    expr::{
        parser, Bind, Expr, ExprId, ExprKind, Lambda, ModPath, ModuleKind, Origin,
        Pattern, Source,
    },
    format_with_flags, PrintFlag,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use futures::future::try_join_all;
use fxhash::FxHashMap;
use log::info;
use netidx::{
    path::Path,
    subscriber::{Event, Subscriber},
    utils::{self, Either},
};
use netidx_value::Value;
use std::{path::PathBuf, pin::Pin, str::FromStr, time::Duration};
use tokio::{task, time::Instant, try_join};
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
        for l in utils::split_escaped(s, '\\', ',') {
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

impl Expr {
    pub fn has_unresolved_modules(&self) -> bool {
        self.fold(false, &mut |acc, e| {
            acc || match &e.kind {
                ExprKind::Module { value: ModuleKind::Unresolved, .. } => true,
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
        macro_rules! only_args {
            ($kind:ident, $args:expr) => {
                Box::pin(async move {
                    let args = Arc::from(subexprs!($args));
                    Ok(Expr {
                        id: self.id,
                        ori: self.ori.clone(),
                        pos: self.pos,
                        kind: ExprKind::$kind { args },
                    })
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
                    Ok(Expr {
                        id: self.id,
                        ori: self.ori.clone(),
                        pos: self.pos,
                        kind: ExprKind::$kind {
                            lhs: Arc::from(lhs),
                            rhs: Arc::from(rhs),
                        },
                    })
                })
            };
        }
        async fn resolve(
            scope: ModPath,
            prepend: Option<Arc<ModuleResolver>>,
            resolvers: Arc<[ModuleResolver]>,
            id: ExprId,
            parent: Arc<Origin>,
            pos: SourcePosition,
            export: bool,
            name: ArcStr,
        ) -> Result<Expr> {
            let jh = task::spawn(async move {
                let ts = Instant::now();
                let name_rel = name.trim_start_matches(Path::SEP);
                let name_mod = Path::from(name.clone()).append("mod.gx");
                let name_mod = name_mod.trim_start_matches(Path::SEP);
                let mut errors = vec![];
                for r in prepend.iter().map(|r| r.as_ref()).chain(resolvers.iter()) {
                    let ori = match r {
                        ModuleResolver::VFS(vfs) => {
                            let scoped = scope.append(&*name);
                            match vfs.get(&scoped) {
                                Some(s) => Origin {
                                    parent: Some(parent.clone()),
                                    source: Source::Internal(name.clone()),
                                    text: s.clone(),
                                },
                                None => continue,
                            }
                        }
                        ModuleResolver::Files(base) => {
                            let full_path = base
                                .join(name_rel)
                                .with_extension("gx")
                                .canonicalize()?;
                            match tokio::fs::read_to_string(&full_path).await {
                                Ok(s) => Origin {
                                    parent: Some(parent.clone()),
                                    source: Source::File(full_path),
                                    text: ArcStr::from(s),
                                },
                                Err(_) => {
                                    let full_path = base.join(name_mod).canonicalize()?;
                                    match tokio::fs::read_to_string(&full_path).await {
                                        Ok(s) => Origin {
                                            parent: Some(parent.clone()),
                                            source: Source::File(full_path),
                                            text: ArcStr::from(s),
                                        },
                                        Err(e) => {
                                            errors.push(anyhow::Error::from(e));
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        ModuleResolver::Netidx { subscriber, base, timeout } => {
                            let full_path = base.append(name_rel);
                            let source = Source::Netidx(full_path.clone());
                            let sub = subscriber
                                .subscribe_nondurable_one(full_path, *timeout)
                                .await;
                            match sub {
                                Err(e) => {
                                    errors.push(e);
                                    continue;
                                }
                                Ok(v) => match v.last() {
                                    Event::Update(Value::String(text)) => Origin {
                                        parent: Some(parent.clone()),
                                        source,
                                        text,
                                    },
                                    Event::Unsubscribed | Event::Update(_) => {
                                        errors.push(anyhow!("expected string"));
                                        continue;
                                    }
                                },
                            }
                        }
                    };
                    let value = ModuleKind::Resolved(
                        parser::parse(ori.clone())
                            .with_context(|| format!("parsing file {ori:?}"))?,
                    );
                    let kind = ExprKind::Module { name, export, value };
                    format_with_flags(PrintFlag::NoSource | PrintFlag::NoParents, || {
                        info!("load and parse {ori} {:?}", ts.elapsed())
                    });
                    return Ok(Expr { id, ori: Arc::new(ori), pos, kind });
                }
                bail!("module {name} could not be found {errors:?}")
            });
            jh.await?
        }
        if !self.has_unresolved_modules() {
            return Box::pin(async { Ok(self.clone()) });
        }
        match self.kind.clone() {
            ExprKind::Module { value: ModuleKind::Unresolved, export, name } => {
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
                        export,
                        name.clone(),
                    )
                    .await?;
                    let scope = ModPath(scope.append(&*name));
                    e.resolve_modules_int(&scope, &prepend, &resolvers).await
                })
            }
            ExprKind::Constant(_)
            | ExprKind::Use { .. }
            | ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::TypeDef { .. } => Box::pin(async move { Ok(self.clone()) }),
            ExprKind::Module { value: ModuleKind::Inline(exprs), export, name } => {
                Box::pin(async move {
                    let scope = ModPath(scope.append(&*name));
                    let exprs = try_join_all(exprs.iter().map(|e| async {
                        e.resolve_modules_int(&scope, prepend, resolvers).await
                    }))
                    .await?;
                    Ok(Expr {
                        id: self.id,
                        ori: self.ori.clone(),
                        pos: self.pos,
                        kind: ExprKind::Module {
                            value: ModuleKind::Inline(Arc::from(exprs)),
                            name,
                            export,
                        },
                    })
                })
            }
            ExprKind::Module { value: ModuleKind::Resolved(exprs), export, name } => {
                Box::pin(async move {
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
                    Ok(Expr {
                        id: self.id,
                        ori: self.ori.clone(),
                        pos: self.pos,
                        kind: ExprKind::Module {
                            value: ModuleKind::Resolved(Arc::from(exprs)),
                            name,
                            export,
                        },
                    })
                })
            }
            ExprKind::Module {
                name,
                export,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Module {
                        name,
                        export,
                        value: ModuleKind::Dynamic { sandbox, sig, source },
                    },
                })
            }),
            ExprKind::Do { exprs } => Box::pin(async move {
                let exprs = Arc::from(subexprs!(exprs));
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Do { exprs },
                })
            }),
            ExprKind::Bind(b) => Box::pin(async move {
                let Bind { doc, pattern, typ, export, value } = &*b;
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Bind(Arc::new(Bind {
                        doc: doc.clone(),
                        pattern: pattern.clone(),
                        typ: typ.clone(),
                        export: *export,
                        value,
                    })),
                })
            }),
            ExprKind::StructWith { source, replace } => Box::pin(async move {
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::StructWith {
                        source: Arc::new(
                            source.resolve_modules_int(scope, prepend, resolvers).await?,
                        ),
                        replace: Arc::from(subtuples!(replace)),
                    },
                })
            }),
            ExprKind::Connect { name, value, deref } => Box::pin(async move {
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Connect { name, value: Arc::new(value), deref },
                })
            }),
            ExprKind::Lambda(l) => Box::pin(async move {
                let Lambda { args, vargs, rtype, constraints, body } = &*l;
                let body = match body {
                    Either::Right(s) => Either::Right(s.clone()),
                    Either::Left(e) => Either::Left(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    ),
                };
                let l = Lambda {
                    args: args.clone(),
                    vargs: vargs.clone(),
                    rtype: rtype.clone(),
                    constraints: constraints.clone(),
                    body,
                };
                let kind = ExprKind::Lambda(Arc::new(l));
                Ok(Expr { id: self.id, ori: self.ori.clone(), pos: self.pos, kind })
            }),
            ExprKind::TypeCast { expr, typ } => Box::pin(async move {
                let expr = expr.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::TypeCast { expr: Arc::new(expr), typ },
                })
            }),
            ExprKind::Apply { args, function } => Box::pin(async move {
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Apply { args: Arc::from(subtuples!(args)), function },
                })
            }),
            ExprKind::Any { args } => only_args!(Any, args),
            ExprKind::Array { args } => only_args!(Array, args),
            ExprKind::Tuple { args } => only_args!(Tuple, args),
            ExprKind::StringInterpolate { args } => only_args!(StringInterpolate, args),
            ExprKind::Struct { args } => Box::pin(async move {
                let args = Arc::from(subtuples!(args));
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Struct { args },
                })
            }),
            ExprKind::ArrayRef { source, i } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let i = Arc::new(i.resolve_modules_int(scope, prepend, resolvers).await?);
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::ArrayRef { source, i },
                })
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
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::ArraySlice { source, start, end },
                })
            }),
            ExprKind::Variant { tag, args } => Box::pin(async move {
                let args = Arc::from(subexprs!(args));
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Variant { tag, args },
                })
            }),
            ExprKind::Select { arg, arms } => Box::pin(async move {
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
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Select { arg, arms: Arc::from(arms) },
                })
            }),
            ExprKind::Qop(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Qop(Arc::new(e)),
                })
            }),
            ExprKind::ByRef(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::ByRef(Arc::new(e)),
                })
            }),
            ExprKind::Deref(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Deref(Arc::new(e)),
                })
            }),
            ExprKind::Not { expr: e } => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: ExprKind::Not { expr: Arc::new(e) },
                })
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
