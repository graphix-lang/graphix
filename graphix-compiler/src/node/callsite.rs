use super::{bind::Ref, compiler::compile, Nop, NOP};
use crate::{
    deref_typ,
    expr::{ErrorContext, Expr, ExprId},
    node::lambda::LambdaDef,
    typ::{FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, LambdaId, Node, PrintFlag, Refs, Rt,
    Scope, TypecheckPhase, Update, UserEvent,
};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use netidx::subscriber::Value;
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, mem};
use triomphe::Arc as TArc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ArgKey {
    Positional(usize),
    Named(ArcStr),
}

#[derive(Debug)]
pub(crate) struct Arg<R: Rt, E: UserEvent> {
    pub id: BindId,
    pub node: Option<Node<R, E>>,
    pub is_default: bool,
}

fn compile_apply_args<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<FxHashMap<ArgKey, Arg<R, E>>> {
    let mut res = FxHashMap::default();
    let mut pos = 0;
    for (name, expr) in args.iter() {
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match name {
            None => {
                res.insert(
                    ArgKey::Positional(pos),
                    Arg { id: BindId::new(), node, is_default: false },
                );
                pos += 1;
            }
            Some(k) => match res.entry(ArgKey::Named(k.clone())) {
                Entry::Occupied(_) => bail!("duplicate named argument {k}"),
                Entry::Vacant(e) => {
                    e.insert(Arg { id: BindId::new(), node, is_default: false });
                }
            },
        }
    }
    Ok(res)
}

#[derive(Debug)]
pub(crate) struct CallSite<R: Rt, E: UserEvent> {
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<FnType>,
    pub(super) resolved_ftype: Option<FnType>,
    pub(super) rtype: Type,
    pub(super) fnode: Node<R, E>,
    pub(super) args: FxHashMap<ArgKey, Arg<R, E>>,
    pub(super) arg_refs: Vec<Node<R, E>>,
    pub(super) function: Option<(Value, Box<dyn Apply<R, E>>)>,
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
        let args = compile_apply_args(ctx, flags, scope, top_id, args)?;
        let site = Self {
            spec,
            ftype: None,
            resolved_ftype: None,
            rtype: Type::empty_tvar(),
            fnode,
            args,
            arg_refs: Vec::new(),
            function: None,
            flags,
            top_id,
            scope: scope.clone(),
        };
        Ok(Box::new(site))
    }

    fn make_ref(&self, id: BindId, typ: Type) -> Node<R, E> {
        Box::new(Ref { spec: NOP.clone(), typ, id, top_id: self.top_id })
    }

    fn bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: Scope,
        flags: BitFlags<CFlag>,
        fv: Value,
        f: &LambdaDef<R, E>,
        event: &mut Event<E>,
        set: &mut Vec<BindId>,
    ) -> Result<()> {
        // Resolve TVars now — after all type checking has completed
        if self.resolved_ftype.is_none() {
            if let Some(ftype) = &self.ftype {
                self.resolved_ftype = Some(ftype.resolve_tvars());
            }
        }
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
        // Clean up previous binding
        if let Some((_, mut old_f)) = self.function.take() {
            old_f.delete(ctx);
        }
        for mut n in self.arg_refs.drain(..) {
            n.delete(ctx);
        }
        // Remove and delete default args from previous bind
        self.args.retain(|_, arg| {
            if arg.is_default {
                if let Some(mut n) = arg.node.take() {
                    n.delete(ctx);
                }
                false
            } else {
                true
            }
        });
        // Build arg_refs in function-signature order
        let mut pos_idx = 0;
        for (i, farg) in f.typ.args.iter().enumerate() {
            if let Some((name, default)) = &farg.label {
                match self.args.get(&ArgKey::Named(name.clone())) {
                    Some(arg) => {
                        let typ = arg
                            .node
                            .as_ref()
                            .map(|n| n.typ().clone())
                            .unwrap_or_else(|| farg.typ.clone());
                        self.arg_refs.push(self.make_ref(arg.id, typ));
                    }
                    None if *default => {
                        let id = BindId::new();
                        let default_node = compile_default!(i, f);
                        let typ = default_node.typ().clone();
                        self.args.insert(
                            ArgKey::Named(name.clone()),
                            Arg { id, node: Some(default_node), is_default: true },
                        );
                        self.arg_refs.push(self.make_ref(id, typ));
                    }
                    None => bail!("BUG: in bind missing required argument {name}"),
                }
            } else {
                // Positional argument - find the pos_idx'th positional arg
                let key = loop {
                    let candidate = ArgKey::Positional(pos_idx);
                    pos_idx += 1;
                    if self.args.contains_key(&candidate) {
                        break candidate;
                    }
                    if pos_idx > self.args.len() + f.typ.args.len() {
                        bail!("missing required positional argument {i}")
                    }
                };
                let arg = &self.args[&key];
                let typ = arg
                    .node
                    .as_ref()
                    .map(|n| n.typ().clone())
                    .unwrap_or_else(|| farg.typ.clone());
                self.arg_refs.push(self.make_ref(arg.id, typ));
            }
        }
        // Handle vargs - remaining positional args
        if f.typ.vargs.is_some() {
            loop {
                let key = ArgKey::Positional(pos_idx);
                pos_idx += 1;
                match self.args.get(&key) {
                    Some(arg) => {
                        let typ = arg
                            .node
                            .as_ref()
                            .map(|n| n.typ().clone())
                            .unwrap_or_else(|| Type::Bottom);
                        self.arg_refs.push(self.make_ref(arg.id, typ));
                    }
                    None => break,
                }
            }
        }
        // Ensure all arg values are available for the init cycle.
        // Defaults need to be updated for the first time (with init=true
        // since Constant only fires on init); existing args may not have
        // changed this cycle but their cached values must be visible to
        // the newly bound function body.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default {
                if let Some(ref mut node) = arg.node {
                    if let Some(v) = node.update(ctx, event) {
                        ctx.cached.insert(arg.id, v.clone());
                        event.variables.insert(arg.id, v);
                        set.push(arg.id);
                    }
                }
            } else if let Entry::Vacant(e) = event.variables.entry(arg.id) {
                if let Some(v) = ctx.cached.get(&arg.id) {
                    e.insert(v.clone());
                    set.push(arg.id);
                }
            }
        }
        event.init = prev_init;
        let mut rf = (f.init)(
            &scope,
            ctx,
            &mut self.arg_refs,
            self.resolved_ftype.as_ref(),
            self.top_id,
        )?;
        let _ = rf.typecheck(ctx, &mut self.arg_refs, TypecheckPhase::Lambda);
        self.function = Some((fv, rf));
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        // Update all arg nodes every cycle, publishing values via bind IDs
        for arg in self.args.values_mut() {
            if let Some(ref mut node) = arg.node {
                if let Some(v) = node.update(ctx, event) {
                    ctx.cached.insert(arg.id, v.clone());
                    event.variables.insert(arg.id, v);
                    set.push(arg.id);
                }
            }
        }
        let bound = match (&self.function, self.fnode.update(ctx, event)) {
            (_, None) => false,
            (Some((fv, _)), Some(v)) if fv == &v => false,
            (_, Some(v)) => match v.downcast_ref::<LambdaDef<R, E>>() {
                None => panic!("value {v:?} is not a function"),
                Some(lb) => {
                    let scope = self.scope.clone();
                    self.bind(ctx, scope, self.flags, v.clone(), lb, event, &mut set)
                        .expect("failed to bind to lambda");
                    true
                }
            },
        };
        match &mut self.function {
            None => {
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                None
            }
            Some((_, f)) if !bound => {
                let res = f.update(ctx, &mut self.arg_refs, event);
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                res
            }
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
                let res = f.update(ctx, &mut self.arg_refs, event);
                event.init = init;
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                res
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, mut f)) = self.function.take() {
            f.delete(ctx)
        }
        self.fnode.delete(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.delete(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, f)) = &mut self.function {
            f.sleep(ctx)
        }
        self.fnode.sleep(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.sleep(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.sleep(ctx);
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
                if ftype.args.len() < self.args.len() && ftype.vargs.is_none() {
                    bail!(
                        "too many arguments, expected {}, received {}",
                        ftype.args.len(),
                        self.args.len()
                    )
                }
                let mut labeled: LPooled<FxHashSet<ArcStr>> = LPooled::take();
                for arg in ftype.args.iter() {
                    if let Some((name, default)) = &arg.label {
                        labeled.insert(name.clone());
                        match self.args.get(&ArgKey::Named(name.clone())) {
                            None if !*default => {
                                bail!("missing required argument {name}")
                            }
                            None => {
                                // Will be filled with default at bind time; insert placeholder
                                self.args.insert(
                                    ArgKey::Named(name.clone()),
                                    Arg {
                                        id: BindId::new(),
                                        node: Some(Nop::new(arg.typ.clone())),
                                        is_default: true,
                                    },
                                );
                            }
                            Some(_) => {}
                        }
                    }
                }
                for key in self.args.keys() {
                    if let ArgKey::Named(name) = key {
                        if !labeled.contains(name) {
                            bail!("unknown labeled argument {name}")
                        }
                    }
                }
                // Check we have enough positional args
                let n_positional_required =
                    ftype.args.iter().filter(|a| a.label.is_none()).count();
                let n_positional_provided = self
                    .args
                    .keys()
                    .filter(|k| matches!(k, ArgKey::Positional(_)))
                    .count();
                if n_positional_provided < n_positional_required {
                    bail!("missing required argument")
                }
                ftype
            }
        };
        let mut hof_idmap: LPooled<FxHashMap<LambdaId, usize>> = LPooled::take();
        // Typecheck positional args in order
        let mut pos_idx = 0;
        for (i, farg) in ftype.args.iter().enumerate() {
            let key = if let Some((name, _)) = &farg.label {
                ArgKey::Named(name.clone())
            } else {
                let key = loop {
                    let candidate = ArgKey::Positional(pos_idx);
                    pos_idx += 1;
                    if self.args.contains_key(&candidate) {
                        break candidate;
                    }
                    bail!("missing required positional argument {i}")
                };
                key
            };
            if let Some(arg) = self.args.get_mut(&key) {
                if let Some(ref mut n) = arg.node {
                    farg.typ.contains(&ctx.env, n.typ())?;
                    wrap!(n, n.typecheck(ctx))?;
                    wrap!(n, farg.typ.check_contains(&ctx.env, n.typ()))?;
                    match deref_typ!("arg", ctx, n.typ(), Some(t) => Ok(Some(t.clone())), None => Ok(None))
                    {
                        Ok(Some(Type::Fn(ft))) => {
                            if !TArc::ptr_eq(&ftype.lambda_ids, &ft.lambda_ids) {
                                let ids = ft.lambda_ids.read();
                                if ids.len() > 0 {
                                    let mut wids = ftype.lambda_ids.write();
                                    for id in ids.iter().copied() {
                                        hof_idmap.insert(id, i);
                                        wids.insert(id);
                                    }
                                }
                            }
                        }
                        Ok(None | Some(_)) | Err(_) => (),
                    }
                }
            }
        }
        // Typecheck vargs
        if let Some(typ) = &ftype.vargs {
            loop {
                let key = ArgKey::Positional(pos_idx);
                pos_idx += 1;
                match self.args.get_mut(&key) {
                    Some(arg) => {
                        if let Some(ref mut n) = arg.node {
                            typ.contains(&ctx.env, n.typ())?;
                            wrap!(n, n.typecheck(ctx))?;
                            wrap!(n, typ.check_contains(&ctx.env, n.typ()))?;
                        }
                    }
                    None => break,
                }
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
                Err(_) if t == Type::Bottom => (), // it doesn't throw any errors
                Err(_) => {
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
            }
        }
        wrap!(self.fnode, self.rtype.check_contains(&ctx.env, &ftype.rtype))?;
        if !ftype.lambda_ids.read().is_empty() {
            let ftype = ftype.clone();
            let spec = self.spec.clone();
            ctx.deferred_checks.push(Box::new(move |ctx| {
                let resolved = ftype.resolve_tvars();
                let mut ids: LPooled<Vec<_>> =
                    ftype.lambda_ids.read().iter().copied().collect();
                for id in ids.drain(..) {
                    let resolved = match hof_idmap.get(&id) {
                        None => &resolved,
                        Some(i) => match &resolved.args[*i].typ {
                            Type::Fn(ft) => ft,
                            t => bail!("unexpected resolved arg type {t}"),
                        },
                    };
                    if let Some(val) = ctx.lambda_defs.get(&id).cloned() {
                        let ldef = val
                            .downcast_ref::<LambdaDef<R, E>>()
                            .expect("failed to unwrap lambda for deferred check");
                        if let Some(apply) = &mut *ldef.check.lock() {
                            apply
                                .typecheck(
                                    ctx,
                                    &mut [],
                                    TypecheckPhase::CallSite(resolved),
                                )
                                .with_context(|| ErrorContext((*spec).clone()))?;
                        }
                    }
                }
                Ok(())
            }));
        }
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        if let Some((_, fun)) = &self.function {
            fun.refs(refs)
        }
        self.fnode.refs(refs);
        for arg in self.args.values() {
            refs.bound.insert(arg.id);
            if let Some(ref n) = arg.node {
                n.refs(refs);
            }
        }
        for n in &self.arg_refs {
            n.refs(refs);
        }
    }
}
