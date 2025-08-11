use super::{compiler::compile, Nop};
use crate::{
    env::LambdaDef,
    expr::{self, Arg, Expr, ExprId, ModPath},
    format_with_flags,
    node::pattern::StructPatternNode,
    trace,
    typ::{FnArgType, FnType, Type},
    wrap, Apply, Event, ExecCtx, InitFn, LambdaId, Node, PrintFlag, Refs, Rt, Update,
    UserEvent, KNOWN,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx::{subscriber::Value, utils::Either};
use parking_lot::RwLock;
use smallvec::{smallvec, SmallVec};
use std::sync::Arc as SArc;
use triomphe::Arc;

#[derive(Debug)]
struct GXLambda<R: Rt, E: UserEvent> {
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for GXLambda<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            if let Some(v) = arg.update(ctx, event) {
                pat.bind(&v, &mut |id, v| {
                    ctx.cached.insert(id, v.clone());
                    event.variables.insert(id, v);
                })
            }
        }
        self.body.update(ctx, event)
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        if trace() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("lambda tc typ {}", self.typ);
            });
        }
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck(ctx))?;
            wrap!(arg, typ.check_contains(&ctx.env, &arg.typ()))?;
        }
        if trace() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("body {}", self.body.typ());
                eprintln!("body spec {}", self.body.spec());
            });
        }
        wrap!(self.body, self.body.typecheck(ctx)).map_err(|e| {
            eprintln!("failing spec: {}", self.body.spec());
            e
        })?;
        if trace() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("body {}", self.body.typ());
                eprintln!("body spec {}", self.body.spec());
            });
        }
        wrap!(self.body, self.typ.rtype.check_contains(&ctx.env, &self.body.typ()))?;
        for (tv, tc) in self.typ.constraints.read().iter() {
            tc.check_contains(&ctx.env, &Type::TVar(tv.clone()))?
        }
        Ok(())
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        for pat in &self.args {
            pat.ids(&mut |id| {
                refs.bound.insert(id);
            })
        }
        self.body.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.delete(ctx);
        for n in &self.args {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.sleep(ctx);
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    pub(super) fn new(
        ctx: &mut ExecCtx<R, E>,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &ModPath,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        if args.len() != argspec.len() {
            bail!("arity mismatch, expected {} arguments", argspec.len())
        }
        let mut argpats = vec![];
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            let pattern = StructPatternNode::compile(ctx, &atyp.typ, &a.pattern, &scope)?;
            if pattern.is_refutable() {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let body = compile(ctx, body, &scope, tid)?;
        Ok(Self { args: Box::from(argpats), typ, body })
    }
}

#[derive(Debug)]
struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.apply.update(ctx, from, event)
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        if args.len() < self.typ.args.len()
            || (args.len() > self.typ.args.len() && self.typ.vargs.is_none())
        {
            let vargs = if self.typ.vargs.is_some() { "at least " } else { "" };
            bail!(
                "expected {}{} arguments got {}",
                vargs,
                self.typ.args.len(),
                args.len()
            )
        }
        for i in 0..args.len() {
            wrap!(args[i], args[i].typecheck(ctx))?;
            let atyp = if i < self.typ.args.len() {
                &self.typ.args[i].typ
            } else {
                self.typ.vargs.as_ref().unwrap()
            };
            wrap!(args[i], atyp.check_contains(&ctx.env, &args[i].typ()))?
        }
        for (tv, tc) in self.typ.constraints.read().iter() {
            tc.check_contains(&ctx.env, &Type::TVar(tv.clone()))?
        }
        self.apply.typecheck(ctx, args)?;
        Ok(())
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.apply.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.sleep(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct Lambda<R: Rt, E: UserEvent> {
    top_id: ExprId,
    spec: Expr,
    def: SArc<LambdaDef<R, E>>,
    typ: Type,
}

impl<R: Rt, E: UserEvent> Lambda<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        l: &expr::Lambda,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let mut s: SmallVec<[&ArcStr; 16]> = smallvec![];
        for a in l.args.iter() {
            a.pattern.with_names(&mut |n| s.push(n));
        }
        let len = s.len();
        s.sort();
        s.dedup();
        if len != s.len() {
            bail!("arguments must have unique names");
        }
        let id = LambdaId::new();
        let scope = ModPath(scope.0.append(&format_compact!("fn{}", id.0)));
        let _scope = scope.clone();
        let env = ctx.env.clone();
        let _env = ctx.env.clone();
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(typ)) => Some(Some(typ.scope_refs(&scope))),
        };
        let rtype = l.rtype.as_ref().map(|t| t.scope_refs(&scope));
        let argspec = l
            .args
            .iter()
            .map(|a| match &a.constraint {
                None => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: None,
                },
                Some(typ) => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: Some(typ.scope_refs(&scope)),
                },
            })
            .collect::<SmallVec<[_; 16]>>();
        let argspec = Arc::from_iter(argspec);
        let constraints = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = tv.scope_refs(&scope);
                let tc = tc.scope_refs(&scope);
                Ok((tv, tc))
            })
            .collect::<Result<SmallVec<[_; 4]>>>()?;
        let constraints = Arc::new(RwLock::new(constraints.into_iter().collect()));
        let typ = match &l.body {
            Either::Left(_) => {
                let args = Arc::from_iter(argspec.iter().map(|a| FnArgType {
                    label: a.labeled.as_ref().and_then(|dv| {
                        a.pattern.single_bind().map(|n| (n.clone(), dv.is_some()))
                    }),
                    typ: match a.constraint.as_ref() {
                        Some(t) => t.clone(),
                        None => Type::empty_tvar(),
                    },
                }));
                let vargs = match vargs {
                    Some(Some(t)) => Some(t.clone()),
                    Some(None) => Some(Type::empty_tvar()),
                    None => None,
                };
                let rtype = rtype.clone().unwrap_or_else(|| Type::empty_tvar());
                Arc::new(FnType { constraints, args, vargs, rtype })
            }
            Either::Right(builtin) => match ctx.builtins.get(builtin.as_str()) {
                None => bail!("unknown builtin function {builtin}"),
                Some((styp, _)) => {
                    if !ctx.builtins_allowed {
                        bail!("defining builtins is not allowed in this context")
                    }
                    Arc::new(styp.clone().scope_refs(&_scope))
                }
            },
        };
        KNOWN.with_borrow_mut(|known| {
            known.clear();
            typ.alias_tvars(known);
        });
        let _typ = typ.clone();
        let _argspec = argspec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |ctx, args, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    let apply = GXLambda::new(
                        ctx,
                        _typ.clone(),
                        _argspec.clone(),
                        args,
                        &_scope,
                        tid,
                        body.clone(),
                    );
                    apply.map(|a| {
                        let f: Box<dyn Apply<R, E>> = Box::new(a);
                        f
                    })
                }
                Either::Right(builtin) => match ctx.builtins.get(&*builtin) {
                    None => bail!("unknown builtin function {builtin}"),
                    Some((_, init)) => {
                        let init = SArc::clone(init);
                        init(ctx, &_typ, &_scope, args, tid).map(|apply| {
                            let f: Box<dyn Apply<R, E>> =
                                Box::new(BuiltInLambda { typ: _typ.clone(), apply });
                            f
                        })
                    }
                },
            })
        });
        let def =
            SArc::new(LambdaDef { id, typ: typ.clone(), env, argspec, init, scope });
        ctx.env.lambdas.insert_cow(id, SArc::downgrade(&def));
        Ok(Box::new(Self { spec, def, typ: Type::Fn(typ), top_id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda<R, E> {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        if event.init {
            Some(Value::U64(self.def.id.0))
        } else {
            None
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.lambdas.remove_cow(&self.def.id);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let mut faux_args: Vec<Node<R, E>> = self
            .def
            .argspec
            .iter()
            .zip(self.def.typ.args.iter())
            .map(|(a, at)| match &a.labeled {
                Some(Some(e)) => ctx.with_restored(self.def.env.clone(), |ctx| {
                    compile(ctx, e.clone(), &self.def.scope, self.top_id)
                }),
                Some(None) | None => {
                    let n: Node<R, E> = Box::new(Nop { typ: at.typ.clone() });
                    Ok(n)
                }
            })
            .collect::<Result<_>>()?;
        let mut f = wrap!(self, (self.def.init)(ctx, &faux_args, ExprId::new()))?;
        let res = wrap!(self, f.typecheck(ctx, &mut faux_args));
        f.typ().constrain_known();
        self.typ.unbind_tvars();
        f.delete(ctx);
        res
    }
}
