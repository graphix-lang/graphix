use super::{compiler::compile, Nop};
use crate::{
    env::{Bind, LambdaDef},
    expr::{self, Arg, Expr, ExprId},
    node::pattern::StructPatternNode,
    typ::{FnArgType, FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, InitFn, LambdaId, Node, Refs, Rt, Scope,
    Update, UserEvent,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx::{subscriber::Value, utils::Either};
use parking_lot::RwLock;
use poolshark::local::LPooled;
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
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck(ctx))?;
            wrap!(arg, typ.check_contains(&ctx.env, &arg.typ()))?;
        }
        wrap!(self.body, self.body.typecheck(ctx))?;
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
        flags: BitFlags<CFlag>,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        if args.len() != argspec.len() {
            bail!("arity mismatch, expected {} arguments", argspec.len())
        }
        let mut argpats = vec![];
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            let pattern = StructPatternNode::compile(ctx, &atyp.typ, &a.pattern, scope)?;
            if pattern.is_refutable() {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let body = compile(ctx, flags, body, &scope, tid)?;
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
    flags: BitFlags<CFlag>,
    typ: Type,
}

impl<R: Rt, E: UserEvent> Lambda<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        l: &expr::LambdaExpr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let mut s: LPooled<Vec<&ArcStr>> = LPooled::take();
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
        let scope = scope.append(&format_compact!("fn{}", id.0));
        let _scope = scope.clone();
        let env = ctx.env.clone();
        let _env = ctx.env.clone();
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(typ)) => Some(Some(typ.scope_refs(&scope.lexical))),
        };
        let rtype = l.rtype.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let throws = l.throws.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let mut argspec = l
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
                    constraint: Some(typ.scope_refs(&scope.lexical)),
                },
            })
            .collect::<LPooled<Vec<_>>>();
        let argspec = Arc::from_iter(argspec.drain(..));
        let constraints = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = tv.scope_refs(&scope.lexical);
                let tc = tc.scope_refs(&scope.lexical);
                Ok((tv, tc))
            })
            .collect::<Result<LPooled<Vec<_>>>>()?;
        let constraints = Arc::new(RwLock::new(constraints));
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
                let throws = throws.clone().unwrap_or_else(|| Type::empty_tvar());
                Arc::new(FnType { constraints, args, vargs, rtype, throws })
            }
            Either::Right(builtin) => match ctx.builtins.get(builtin.as_str()) {
                None => bail!("unknown builtin function {builtin}"),
                Some((styp, _)) => {
                    if !ctx.builtins_allowed {
                        bail!("defining builtins is not allowed in this context")
                    }
                    Arc::new(styp.clone().scope_refs(&_scope.lexical))
                }
            },
        };
        typ.alias_tvars(&mut LPooled::take());
        let _typ = typ.clone();
        let _argspec = argspec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, tid, tcerr| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    let scope = Scope {
                        dynamic: scope.dynamic.clone(),
                        lexical: _scope.lexical.clone(),
                    };
                    let apply = GXLambda::new(
                        ctx,
                        flags,
                        _typ.clone(),
                        _argspec.clone(),
                        args,
                        &scope,
                        tid,
                        body.clone(),
                    );
                    apply.and_then(|a| {
                        let mut f: Box<dyn Apply<R, E>> = Box::new(a);
                        if tcerr {
                            f.typecheck(ctx, args).map(|()| f)
                        } else {
                            let _ = f.typecheck(ctx, args);
                            Ok(f)
                        }
                    })
                }
                Either::Right(builtin) => match ctx.builtins.get(&*builtin) {
                    None => bail!("unknown builtin function {builtin}"),
                    Some((_, init)) => {
                        let init = SArc::clone(init);
                        init(ctx, &_typ, &_scope, args, tid).and_then(|apply| {
                            let mut f: Box<dyn Apply<R, E>> =
                                Box::new(BuiltInLambda { typ: _typ.clone(), apply });
                            if tcerr {
                                f.typecheck(ctx, args).map(|()| f)
                            } else {
                                let _ = f.typecheck(ctx, args);
                                Ok(f)
                            }
                        })
                    }
                },
            })
        });
        let def =
            SArc::new(LambdaDef { id, typ: typ.clone(), env, argspec, init, scope });
        ctx.env.lambdas.insert_cow(id, SArc::downgrade(&def));
        Ok(Box::new(Self { spec, def, typ: Type::Fn(typ), top_id, flags }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda<R, E> {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        event.init.then(|| Value::U64(self.def.id.0))
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
        let mut faux_args: LPooled<Vec<Node<R, E>>> = self
            .def
            .argspec
            .iter()
            .zip(self.def.typ.args.iter())
            .map(|(a, at)| match &a.labeled {
                Some(Some(e)) => ctx.with_restored(self.def.env.clone(), |ctx| {
                    compile(ctx, self.flags, e.clone(), &self.def.scope, self.top_id)
                }),
                Some(None) | None => {
                    let n: Node<R, E> = Box::new(Nop { typ: at.typ.clone() });
                    Ok(n)
                }
            })
            .collect::<Result<_>>()?;
        let faux_id = BindId::new();
        ctx.env.by_id.insert_cow(
            faux_id,
            Bind {
                doc: None,
                export: false,
                id: faux_id,
                name: "faux".into(),
                scope: self.def.scope.lexical.clone(),
                typ: Type::empty_tvar(),
            },
        );
        let prev_catch =
            ctx.env.catch.insert_cow(self.def.scope.dynamic.clone(), faux_id);
        let res = wrap!(
            self,
            (self.def.init)(&self.def.scope, ctx, &mut faux_args, ExprId::new(), true)
        );
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            f.delete(ctx);
            let inferred_throws = ctx.env.by_id[&faux_id]
                .typ
                .with_deref(|t| t.cloned())
                .unwrap_or(Type::Bottom);
            wrap!(self, ftyp.throws.check_contains(&ctx.env, &inferred_throws))?;
            ftyp.constrain_known();
            Ok(())
        });
        ctx.env.by_id.remove_cow(&faux_id);
        match prev_catch {
            Some(id) => ctx.env.catch.insert_cow(self.def.scope.dynamic.clone(), id),
            None => ctx.env.catch.remove_cow(&self.def.scope.dynamic),
        };
        self.typ.unbind_tvars();
        res
    }
}
