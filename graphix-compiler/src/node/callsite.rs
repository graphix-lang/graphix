use super::{compiler::compile, error::ECHAIN, Nop};
use crate::{
    deref_typ,
    env::LambdaDef,
    expr::{Expr, ExprId},
    typ::{FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, LambdaId, Node, PrintFlag, Refs, Rt,
    Scope, Update, UserEvent,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use netidx::subscriber::Value;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, mem, sync::Arc};
use triomphe::Arc as TArc;

fn compile_apply_args<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<(Vec<Node<R, E>>, FxHashMap<ArcStr, (Option<Node<R, E>>, bool)>)> {
    let mut named: FxHashMap<ArcStr, (Option<Node<R, E>>, bool)> = FxHashMap::default();
    let mut nodes: Vec<Node<R, E>> = vec![];
    for (name, expr) in args.iter() {
        let n = compile(ctx, flags, expr.clone(), scope, top_id)?;
        match name {
            None => nodes.push(n),
            Some(k) => match named.entry(k.clone()) {
                Entry::Occupied(_) => bail!("duplicate named argument {k}"),
                Entry::Vacant(e) => {
                    e.insert((Some(n), false));
                }
            },
        }
    }
    Ok((nodes, named))
}

fn is_arith_error(t: &Type) -> bool {
    t.with_deref(|t| {
        t.map(|t| match t {
            Type::Error(e) => match &**e {
                Type::Ref { scope: _, name, params } => {
                    *name == *ECHAIN && params.len() == 1 && is_arith_error(&params[0])
                }
                Type::Variant(name, param) => {
                    &**name == "ArithError"
                        && param.len() == 1
                        && param[0] == Type::Primitive(Typ::String.into())
                }
                _ => false,
            },
            _ => false,
        })
        .unwrap_or(false)
    })
}

#[derive(Debug)]
pub(crate) struct CallSite<R: Rt, E: UserEvent> {
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<FnType>,
    pub(super) rtype: Type,
    pub(super) fnode: Node<R, E>,
    pub(super) named_args: FxHashMap<ArcStr, (Option<Node<R, E>>, bool)>,
    pub(super) args: Vec<Node<R, E>>,
    pub(super) function: Option<(LambdaId, Box<dyn Apply<R, E>>)>,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &TArc<[(Option<ArcStr>, Expr)]>,
        f: &TArc<Expr>,
    ) -> Result<Node<R, E>> {
        let fnode = compile(ctx, flags, (**f).clone(), scope, top_id)?;
        let spec = TArc::new(spec);
        let (args, named_args) = compile_apply_args(ctx, flags, scope, top_id, args)?;
        let site = Self {
            spec,
            ftype: None,
            rtype: Type::empty_tvar(),
            named_args,
            args,
            fnode,
            function: None,
            flags,
            top_id,
            scope: scope.clone(),
        };
        Ok(Box::new(site))
    }

    fn bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: Scope,
        flags: BitFlags<CFlag>,
        f: Arc<LambdaDef<R, E>>,
        event: &mut Event<E>,
        set: &mut Vec<BindId>,
    ) -> Result<()> {
        let mut flags = flags;
        // we already warned about this
        flags.remove(CFlag::WarnUnhandled);
        macro_rules! compile_default {
            ($i:expr, $f:expr) => {{
                match &$f.argspec[$i].labeled {
                    None | Some(None) => bail!("expected default value"),
                    Some(Some(expr)) => ctx.with_restored($f.env.clone(), |ctx| {
                        let scope = Scope {
                            dynamic: scope.dynamic.clone(),
                            lexical: $f.scope.lexical.clone(),
                        };
                        let n = compile(ctx, flags, expr.clone(), &scope, self.top_id)?;
                        let mut refs = Refs::default();
                        n.refs(&mut refs);
                        refs.with_external_refs(|id| {
                            if let Some(v) = ctx.cached.get(&id) {
                                if let Entry::Vacant(e) = event.variables.entry(id) {
                                    e.insert(v.clone());
                                    set.push(id);
                                }
                            }
                        });
                        Ok::<_, anyhow::Error>(n)
                    })?,
                }
            }};
        }
        let ftype = match &self.function {
            Some((_, f)) => &f.typ(),
            None => match self.ftype.as_ref() {
                Some(ftype) => ftype,
                None => {
                    let ftype = &*f.typ;
                    self.ftype = Some(ftype.clone());
                    for (i, arg) in ftype.args.iter().enumerate() {
                        if let Some((name, default)) = &arg.label {
                            match self.named_args.get_mut(name) {
                                None if !*default => {
                                    bail!("BUG: in bind missing required argument {name}")
                                }
                                None => {
                                    self.args.insert(i, Nop::new(arg.typ.clone()));
                                    self.named_args.insert(name.clone(), (None, true));
                                }
                                Some((n, _)) => {
                                    if let Some(n) = n.take() {
                                        self.args.insert(i, n)
                                    }
                                }
                            }
                        }
                    }
                    ftype
                }
            },
        };
        for arg in ftype.args.iter() {
            if let Some((name, _)) = &arg.label {
                let (n, is_default) = self.named_args.get_mut(name).unwrap();
                if *is_default {
                    let mut n = self.args.remove(0);
                    n.delete(ctx);
                } else {
                    *n = Some(self.args.remove(0));
                }
            }
        }
        let mut labeled: LPooled<FxHashSet<ArcStr>> = LPooled::take();
        for (i, arg) in f.typ.args.iter().enumerate() {
            if let Some((name, _)) = &arg.label {
                labeled.insert(name.clone());
                match self.named_args.entry(name.clone()) {
                    Entry::Occupied(mut e) => match e.get_mut().0.take() {
                        Some(n) => self.args.insert(i, n),
                        None => self.args.insert(i, compile_default!(i, f)),
                    },
                    Entry::Vacant(e) => {
                        e.insert((None, true));
                        self.args.insert(i, compile_default!(i, f))
                    }
                }
            }
        }
        self.named_args.retain(|name, (n, _)| {
            let keep = labeled.contains(name);
            if !keep && let Some(n) = n {
                n.delete(ctx)
            }
            keep
        });
        let rf = (f.init)(&scope, ctx, &mut self.args, self.top_id, false)?;
        self.function = Some((f.id, rf));
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        let bound = match (&self.function, self.fnode.update(ctx, event)) {
            (_, None) => false,
            (Some((cid, _)), Some(Value::U64(id))) if cid.0 == id => false,
            (_, Some(Value::U64(id))) => match ctx.env.lambdas.get(&LambdaId(id)) {
                None => panic!("no such function {id:?}"),
                Some(lb) => match lb.upgrade() {
                    None => panic!("function {id:?} is no longer callable"),
                    Some(lb) => {
                        let scope = self.scope.clone();
                        self.bind(ctx, scope, self.flags, lb, event, &mut set)
                            .expect("failed to bind to lambda");
                        true
                    }
                },
            },
            (_, Some(v)) => panic!("invalid function {v}"),
        };
        match &mut self.function {
            None => None,
            Some((_, f)) if !bound => f.update(ctx, &mut self.args, event),
            Some((_, f)) => {
                let init = mem::replace(&mut event.init, true);
                let mut refs = Refs::default();
                f.refs(&mut refs);
                refs.with_external_refs(|id| {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        if let Some(v) = ctx.cached.get(&id) {
                            e.insert(v.clone());
                            set.push(id);
                        }
                    }
                });
                let res = f.update(ctx, &mut self.args, event);
                event.init = init;
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                res
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            spec: _,
            rtype: _,
            ftype: _,
            fnode,
            named_args: _,
            args,
            function,
            flags: _,
            top_id: _,
            scope: _,
        } = self;
        if let Some((_, f)) = function {
            f.delete(ctx)
        }
        fnode.delete(ctx);
        for n in args {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            spec: _,
            rtype: _,
            ftype: _,
            fnode,
            named_args: _,
            args,
            function,
            flags: _,
            top_id: _,
            scope: _,
        } = self;
        if let Some((_, f)) = function {
            f.sleep(ctx)
        }
        fnode.sleep(ctx);
        for n in args {
            n.sleep(ctx)
        }
    }

    fn typ(&self) -> &Type {
        &self.rtype
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck(ctx))?;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(),
                    Some(Type::Fn(ftype)) => Ok(ftype.clone())
                )?;
                let ftype = ftype.reset_tvars();
                ftype.alias_tvars(&mut LPooled::take());
                self.ftype = Some(ftype.clone());
                let ftype = self.ftype.as_ref().unwrap();
                let args_len = self.args.len() + self.named_args.len();
                if ftype.args.len() < args_len && ftype.vargs.is_none() {
                    bail!(
                        "too many arguments, expected {}, received {}",
                        ftype.args.len(),
                        args_len
                    )
                }
                let mut labeled: LPooled<FxHashSet<ArcStr>> = LPooled::take();
                for (i, arg) in ftype.args.iter().enumerate() {
                    if let Some((name, default)) = &arg.label {
                        labeled.insert(name.clone());
                        match self.named_args.get_mut(name) {
                            None if !*default => {
                                bail!("missing required argument {name}")
                            }
                            None => {
                                self.args.insert(i, Nop::new(arg.typ.clone()));
                                self.named_args.insert(name.clone(), (None, true));
                            }
                            Some((n, _)) => {
                                if let Some(n) = n.take() {
                                    self.args.insert(i, n)
                                }
                            }
                        }
                    }
                    if i >= self.args.len() {
                        bail!("missing required argument")
                    }
                }
                for name in self.named_args.keys() {
                    if !labeled.contains(name) {
                        bail!("unknown labeled argument {name}")
                    }
                }
                ftype
            }
        };
        for (n, arg) in self.args.iter_mut().zip(ftype.args.iter()) {
            // associate the fntype arg with the arg before typechecking the arg
            arg.typ.contains(&ctx.env, n.typ())?;
            wrap!(n, n.typecheck(ctx))?;
            wrap!(n, arg.typ.check_contains(&ctx.env, n.typ()))?;
        }
        if self.args.len() > ftype.args.len()
            && let Some(typ) = &ftype.vargs
        {
            for n in &mut self.args[ftype.args.len()..] {
                // associate the fntype arg with the arg before typechecking the arg
                typ.contains(&ctx.env, n.typ())?;
                wrap!(n, n.typecheck(ctx))?;
                wrap!(n, typ.check_contains(&ctx.env, n.typ()))?
            }
        }
        for (tv, tc) in ftype.constraints.read().iter() {
            wrap!(self, tc.check_contains(&ctx.env, &Type::TVar(tv.clone())))?;
        }
        if let Some(t) = ftype.throws.with_deref(|t| t.cloned()) {
            match ctx.env.lookup_catch(&self.scope.dynamic) {
                Ok(id) => {
                    if let Some(bind) = ctx.env.by_id.get(&id)
                        && let Type::TVar(tv) = &bind.typ
                    {
                        let tv = tv.read();
                        let mut ty = tv.typ.write();
                        *ty = match &*ty {
                            None => Some(t),
                            Some(inner) => Some(inner.union(&ctx.env, &t)?),
                        };
                    }
                }
                Err(_) if is_arith_error(&t) => {
                    if self
                        .flags
                        .contains(CFlag::WarnUnhandledArith | CFlag::WarningsAreErrors)
                    {
                        bail!(
                            "ERROR: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
                        )
                    }
                    if self.flags.contains(CFlag::WarnUnhandledArith) {
                        eprintln!(
                            "WARNING: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
                        )
                    }
                }
                Err(_) if t != Type::Bottom => {
                    if self
                        .flags
                        .contains(CFlag::WarnUnhandled | CFlag::WarningsAreErrors)
                    {
                        bail!(
                            "ERROR: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
                        )
                    }
                    if self.flags.contains(CFlag::WarnUnhandled) {
                        eprintln!(
                            "WARNING: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
                        )
                    }
                }
                Err(_) => (),
            }
        }
        wrap!(self.fnode, self.rtype.check_contains(&ctx.env, &ftype.rtype))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        let Self {
            spec: _,
            rtype: _,
            ftype: _,
            fnode,
            named_args: _,
            args,
            function,
            flags: _,
            top_id: _,
            scope: _,
        } = self;
        if let Some((_, fun)) = function {
            fun.refs(refs)
        }
        fnode.refs(refs);
        for n in args {
            n.refs(refs)
        }
    }
}
