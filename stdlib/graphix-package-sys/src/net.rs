use anyhow::{anyhow, bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    deref_typ, err, errf,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, PrintFlag, Rt, Scope,
    TypecheckPhase, UserEvent,
};
use graphix_package_core::{arity1, arity2, extract_cast_type, CachedVals};
use netidx::{
    path::Path,
    publisher::{Typ, Val},
    subscriber::{self, Dval, UpdatesFlags, Value},
};
use netidx_core::utils::Either;
use netidx_protocols::rpc::server::{self, ArgSpec};
use netidx_value::ValArray;
use smallvec::{smallvec, SmallVec};
use std::collections::VecDeque;
use triomphe::Arc as TArc;

fn is_null_type(t: &Type) -> bool {
    matches!(t, Type::Primitive(flags) if flags.iter().count() == 1 && flags.contains(Typ::Null))
}

fn as_path(v: Value) -> Option<Path> {
    match v.cast_to::<String>() {
        Err(_) => None,
        Ok(p) => {
            if Path::is_absolute(&p) {
                Some(Path::from(p))
            } else {
                None
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct Write {
    args: CachedVals,
    top_id: ExprId,
    dv: Either<(Path, Dval), Vec<Value>>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Write {
    const NAME: &str = "sys_net_write";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Write {
            args: CachedVals::new(from),
            dv: Either::Right(vec![]),
            top_id,
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Write {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        fn set(dv: &mut Either<(Path, Dval), Vec<Value>>, val: &Value) {
            match dv {
                Either::Right(q) => q.push(val.clone()),
                Either::Left((_, dv)) => {
                    dv.write(val.clone());
                }
            }
        }
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        let ((path, value), (path_up, value_up)) = arity2!(self.args.0, &up);
        match ((path, value), (path_up, value_up)) {
            ((_, _), (false, false)) => (),
            ((_, Some(val)), (false, true)) => set(&mut self.dv, val),
            ((_, None), (false, true)) => (),
            ((None, Some(val)), (true, true)) => set(&mut self.dv, val),
            ((Some(path), Some(val)), (true, true)) if self.same_path(path) => {
                set(&mut self.dv, val)
            }
            ((Some(path), _), (true, false)) if self.same_path(path) => (),
            ((None, _), (true, false)) => (),
            ((None, None), (_, _)) => (),
            ((Some(path), val), (true, _)) => match as_path(path.clone()) {
                None => {
                    if let Either::Left(_) = &self.dv {
                        self.dv = Either::Right(vec![]);
                    }
                    let e = errf!(literal!("WriteError"), "invalid path {path:?}");
                    return Some(Value::Error(TArc::new(e)));
                }
                Some(path) => {
                    let dv = ctx.rt.subscribe(
                        UpdatesFlags::empty(),
                        path.clone(),
                        self.top_id,
                    );
                    match &mut self.dv {
                        Either::Left(_) => (),
                        Either::Right(q) => {
                            for v in q.drain(..) {
                                dv.write(v);
                            }
                        }
                    }
                    self.dv = Either::Left((path, dv));
                    if let Some(val) = val {
                        set(&mut self.dv, val)
                    }
                }
            },
        }
        None
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Either::Left((path, dv)) = &self.dv {
            ctx.rt.unsubscribe(path.clone(), dv.clone(), self.top_id)
        }
        self.dv = Either::Right(vec![])
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.args.clear();
        match &mut self.dv {
            Either::Left((path, dv)) => {
                ctx.rt.unsubscribe(path.clone(), dv.clone(), self.top_id);
                self.dv = Either::Right(vec![])
            }
            Either::Right(_) => (),
        }
    }
}

impl Write {
    fn same_path(&self, new_path: &Value) -> bool {
        match (new_path, &self.dv) {
            (Value::String(p0), Either::Left((p1, _))) => &**p0 == &**p1,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Subscribe {
    args: CachedVals,
    cur: Option<(Path, Dval)>,
    top_id: ExprId,
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Subscribe {
    const NAME: &str = "sys_net_subscribe";
    const NEEDS_CALLSITE: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Subscribe {
            args: CachedVals::new(from),
            cur: None,
            top_id,
            cast_typ: extract_cast_type(resolved),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Subscribe {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        static ERR_TAG: ArcStr = literal!("SubscribeError");
        let mut up = [false; 1];
        self.args.update_diff(&mut up, ctx, from, event);
        let (path, path_up) = arity1!(self.args.0, &up);
        match (path, path_up) {
            (Some(_), false) | (None, false) => (),
            (None, true) => {
                if let Some((path, dv)) = self.cur.take() {
                    ctx.rt.unsubscribe(path, dv, self.top_id)
                }
                return None;
            }
            (Some(Value::String(path)), true)
                if self.cur.as_ref().map(|(p, _)| &**p) != Some(&*path) =>
            {
                if let Some((path, dv)) = self.cur.take() {
                    ctx.rt.unsubscribe(path, dv, self.top_id)
                }
                let path = Path::from(path);
                if !Path::is_absolute(&path) {
                    return Some(err!(ERR_TAG, "expected absolute path"));
                }
                let dval = ctx.rt.subscribe(
                    UpdatesFlags::BEGIN_WITH_LAST,
                    path.clone(),
                    self.top_id,
                );
                self.cur = Some((path, dval));
            }
            (Some(Value::String(_)), true) => (),
            (Some(v), true) => {
                return Some(errf!(ERR_TAG, "invalid path {v}, expected string"))
            }
        }
        self.cur.as_ref().and_then(|(_, dv)| {
            event.netidx.get(&dv.id()).map(|e| match e {
                subscriber::Event::Unsubscribed => Value::error(literal!("unsubscribed")),
                subscriber::Event::Update(v) => match &self.cast_typ {
                    Some(typ) => typ.cast_value(&ctx.env, v.clone()),
                    None => v.clone(),
                },
            })
        })
    }

    fn typecheck(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("sys::net::subscribe requires a concrete return type")
                }
                Ok(())
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((path, dv)) = self.cur.take() {
            ctx.rt.unsubscribe(path, dv, self.top_id)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.args.clear();
        if let Some((path, dv)) = self.cur.take() {
            ctx.rt.unsubscribe(path, dv, self.top_id);
        }
    }
}

#[derive(Debug)]
pub(crate) struct RpcCall {
    args: CachedVals,
    top_id: ExprId,
    id: BindId,
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for RpcCall {
    const NAME: &str = "sys_net_call";
    const NEEDS_CALLSITE: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(RpcCall {
            args: CachedVals::new(from),
            top_id,
            id,
            cast_typ: extract_cast_type(resolved),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for RpcCall {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        fn parse_args(
            path: &Value,
            args: &Value,
        ) -> Result<(Path, Vec<(ArcStr, Value)>)> {
            let path = as_path(path.clone()).ok_or_else(|| anyhow!("invalid path"))?;
            let args = match args {
                Value::Null => vec![],
                Value::Array(args) => args
                    .iter()
                    .map(|v| match v {
                        Value::Array(p) => match &**p {
                            [Value::String(name), value] => {
                                Ok((name.clone(), value.clone()))
                            }
                            _ => Err(anyhow!("rpc args expected [name, value] pair")),
                        },
                        _ => Err(anyhow!("rpc args expected [name, value] pair")),
                    })
                    .collect::<Result<Vec<_>>>()?,
                _ => bail!("rpc args expected to be a struct or null"),
            };
            Ok((path, args))
        }
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        let ((path, args), (path_up, args_up)) = arity2!(self.args.0, &up);
        match ((path, args), (path_up, args_up)) {
            ((Some(path), Some(args)), (_, true))
            | ((Some(path), Some(args)), (true, _)) => match parse_args(path, args) {
                Err(e) => return Some(errf!(literal!("RpcError"), "{e}")),
                Ok((path, args)) => ctx.rt.call_rpc(path, args, self.id),
            },
            ((None, _), (_, _)) | ((_, None), (_, _)) | ((_, _), (false, false)) => (),
        }
        event.variables.get(&self.id).map(|v| match &self.cast_typ {
            Some(typ) => typ.cast_value(&ctx.env, v.clone()),
            None => v.clone(),
        })
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("sys::net::call requires a concrete return type")
                }
                // validate args type: must be a struct or null
                if let Some(args_arg) = resolved.args.get(1) {
                    deref_typ!("struct, null, or Any", ctx, &args_arg.typ,
                        Some(Type::Struct(_)) => Ok(()),
                        Some(Type::Any) => Ok(()),
                        Some(t @ Type::Primitive(_)) => {
                            if is_null_type(t) { Ok(()) }
                            else { bail!("sys::net::call args must be a struct or null") }
                        }
                    )?;
                }
                Ok(())
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.args.clear()
    }
}

macro_rules! list {
    ($name:ident, $builtin:literal, $method:ident, $typ:literal) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            args: CachedVals,
            current: Option<Path>,
            id: BindId,
            top_id: ExprId,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const NAME: &str = $builtin;
            const NEEDS_CALLSITE: bool = false;

            fn init<'a, 'b, 'c, 'd>(
                ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R, E>],
                top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                Ok(Box::new($name {
                    args: CachedVals::new(from),
                    current: None,
                    top_id,
                    id,
                }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                let mut up = [false; 2];
                self.args.update_diff(&mut up, ctx, from, event);
                let ((_, path), (trigger_up, path_up)) = arity2!(self.args.0, &up);
                match (path, path_up, trigger_up) {
                    (Some(Value::String(path)), true, _)
                        if self
                            .current
                            .as_ref()
                            .map(|p| &**p != &**path)
                            .unwrap_or(true) =>
                    {
                        let path = Path::from(path);
                        self.current = Some(path.clone());
                        ctx.rt.$method(self.id, path);
                    }
                    (Some(Value::String(path)), _, true) => {
                        ctx.rt.$method(self.id, Path::from(path));
                    }
                    _ => (),
                }
                event.variables.get(&self.id).and_then(|v| match v {
                    Value::Null => None,
                    Value::Error(e) => Some(errf!(literal!("ListError"), "{e}")),
                    v => Some(v.clone()),
                })
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                ctx.rt.unref_var(self.id, self.top_id);
                ctx.rt.stop_list(self.id);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                ctx.rt.unref_var(self.id, self.top_id);
                ctx.rt.stop_list(self.id);
                self.id = BindId::new();
                ctx.rt.ref_var(self.id, self.top_id);
                self.current = None;
                self.args.clear();
            }
        }
    };
}

list!(
    List,
    "sys_net_list",
    list,
    "fn(?#update:Any, string) -> Result<Array<string>, `ListError(string)>"
);

list!(
    ListTable,
    "sys_net_list_table",
    list_table,
    "fn(?#update:Any, string) -> Result<Table, `ListError(string)>"
);

fn extract_publish_cast_type(resolved: Option<&FnType>) -> Option<Type> {
    let resolved = resolved?;
    resolved.args.first().and_then(|a| match &a.typ {
        Type::Fn(cb_ft) if !cb_ft.args.is_empty() => {
            let t = &cb_ft.args[0].typ;
            if format!("{t}").contains('\'') {
                None
            } else {
                Some(t.clone())
            }
        }
        _ => None,
    })
}

#[derive(Debug)]
pub(crate) struct Publish<R: Rt, E: UserEvent> {
    args: CachedVals,
    current: Option<(Path, Val)>,
    top_id: ExprId,
    x: BindId,
    pid: BindId,
    on_write: Node<R, E>,
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Publish<R, E> {
    const NAME: &str = "sys_net_publish";
    const NEEDS_CALLSITE: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
                let pid = BindId::new();
                let mftyp = match &typ.args[0].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let (x, xn) = genn::bind(
                    ctx,
                    &scope.lexical,
                    "x",
                    mftyp.args[0].typ.clone(),
                    top_id,
                );
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let on_write = genn::apply(fnode, scope, vec![xn], &mftyp, top_id);
                Ok(Box::new(Publish {
                    args: CachedVals::new(from),
                    current: None,
                    top_id,
                    pid,
                    x,
                    on_write,
                    cast_typ: extract_publish_cast_type(resolved),
                }))
            }
            _ => bail!("expected three arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Publish<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! publish {
            ($path:expr, $v:expr) => {{
                let path = Path::from($path.clone());
                match ctx.rt.publish(path.clone(), $v.clone(), self.top_id) {
                    Err(e) => {
                        let msg: ArcStr = format_compact!("{e:?}").as_str().into();
                        let e: Value = (literal!("PublishError"), msg).into();
                        return Some(Value::Error(TArc::new(e)));
                    }
                    Ok(id) => {
                        self.current = Some((path, id));
                    }
                }
            }};
        }
        let mut up = [false; 3];
        self.args.update_diff(&mut up, ctx, from, event);
        if up[0] {
            if let Some(v) = self.args.0[0].clone() {
                ctx.cached.insert(self.pid, v.clone());
                event.variables.insert(self.pid, v);
            }
        }
        match (&up[1..], &self.args.0[1..]) {
            ([true, _], [Some(Value::String(path)), Some(v)])
                if self.current.as_ref().map(|(p, _)| &**p != path).unwrap_or(true) =>
            {
                if let Some((_, id)) = self.current.take() {
                    ctx.rt.unpublish(id, self.top_id);
                }
                publish!(path, v)
            }
            ([_, true], [Some(Value::String(path)), Some(v)]) => match &self.current {
                Some((_, val)) => ctx.rt.update(val, v.clone()),
                None => publish!(path, v),
            },
            _ => (),
        }
        let mut reply = None;
        if let Some((_, val)) = &self.current {
            if let Some(req) = event.writes.remove(&val.id()) {
                let v = match &self.cast_typ {
                    Some(typ) => typ.cast_value(&ctx.env, req.value.clone()),
                    None => req.value.clone(),
                };
                ctx.cached.insert(self.x, v.clone());
                event.variables.insert(self.x, v);
                reply = req.send_result;
            }
        }
        if let Some(v) = self.on_write.update(ctx, event) {
            if let Some(reply) = reply {
                reply.send(v)
            }
        }
        None
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => {
                self.on_write.typecheck(ctx)?;
                Ok(())
            }
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_publish_cast_type(Some(resolved));
                Ok(())
            }
        }
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.on_write.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, val)) = self.current.take() {
            ctx.rt.unpublish(val, self.top_id);
        }
        ctx.cached.remove(&self.pid);
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.on_write.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, val)) = self.current.take() {
            ctx.rt.unpublish(val, self.top_id);
        }
        self.args.clear();
        self.on_write.sleep(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct PublishRpc<R: Rt, E: UserEvent> {
    args: CachedVals,
    id: BindId,
    top_id: ExprId,
    f: Node<R, E>,
    pid: BindId,
    x: BindId,
    queue: VecDeque<server::RpcCall>,
    argbuf: SmallVec<[(ArcStr, Value); 6]>,
    ready: bool,
    current: Option<Path>,
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> PublishRpc<R, E> {
    fn validate_spec(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        resolved: &FnType,
    ) -> Result<()> {
        // validate spec: must be a struct of RpcArg fields, or null (no args)
        let (spec_is_null, spec_fields) = if let Some(spec_arg) = resolved.args.get(2) {
            deref_typ!("struct or null", ctx, &spec_arg.typ,
                Some(Type::Struct(fields)) => Ok((false, fields.clone())),
                Some(t @ Type::Primitive(_)) => {
                    if is_null_type(t) { Ok((true, TArc::from_iter([]))) }
                    else { bail!("rpc #spec must be a struct or null") }
                }
            )?
        } else {
            bail!("rpc #spec type not available")
        };
        // validate each spec field is {default: T, doc: string}
        for (name, field_typ) in spec_fields.iter() {
            deref_typ!("RpcArg {{default: 'a, doc: string}}", ctx, field_typ,
                Some(Type::Struct(inner)) => {
                    if inner.len() == 2 {
                        let has_default = inner.iter().any(|(n, _)| n.as_str() == "default");
                        let has_doc = inner.iter().any(|(n, _)| n.as_str() == "doc");
                        if has_default && has_doc { Ok(()) }
                        else { bail!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}") }
                    } else {
                        bail!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}")
                    }
                }
            )?;
        }
        // validate callback arg
        let cb_fn = if let Some(f_arg) = resolved.args.get(3) {
            deref_typ!("fn", ctx, &f_arg.typ,
                Some(Type::Fn(ft)) => Ok(ft.clone())
            )?
        } else {
            bail!("rpc #f must be a function with an argument")
        };
        if cb_fn.args.is_empty() {
            bail!("rpc #f must be a function with an argument")
        }
        let cb_arg_typ = &cb_fn.args[0].typ;
        // if spec is null, callback arg must also be null
        if spec_is_null {
            deref_typ!("null", ctx, cb_arg_typ,
                Some(t @ Type::Primitive(_)) => {
                    if is_null_type(t) { Ok(()) }
                    else { bail!("rpc #f argument must be null when #spec is null") }
                }
            )?;
            self.cast_typ = Some(cb_arg_typ.clone());
            return Ok(());
        }
        let cb_fields = deref_typ!("struct", ctx, cb_arg_typ,
            Some(Type::Struct(fields)) => Ok(fields.clone())
        )?;
        // verify same fields and matching types.
        // the length check catches extra fields in either direction,
        // and the name check below catches mismatched names.
        if spec_fields.len() != cb_fields.len() {
            bail!(
                "rpc #spec has {} fields but #f argument has {}",
                spec_fields.len(),
                cb_fields.len()
            )
        }
        for (spec_name, spec_field_typ) in spec_fields.iter() {
            // extract the value type T from {default: T, doc: string}
            let value_typ = deref_typ!(
                "{{default: 'a, doc: string}}", ctx, spec_field_typ,
                Some(Type::Struct(inner)) => {
                    match inner.iter().find(|(n, _)| n.as_str() == "default") {
                        Some((_, t)) => Ok(t.clone()),
                        None => bail!("rpc #spec field '{spec_name}' missing 'default'"),
                    }
                }
            )?;
            let cb_field = cb_fields.iter().find(|(n, _)| n == spec_name);
            match cb_field {
                None => bail!("rpc #f argument missing field '{spec_name}'"),
                Some((_, cb_typ)) => {
                    let check = |t: &Type| -> Result<()> {
                        if !t.contains(&ctx.env, &value_typ)? {
                            bail!(
                                "rpc field '{spec_name}' type mismatch: \
                                 #f argument type {t} does not contain \
                                 #spec default type {value_typ}"
                            )
                        }
                        Ok(())
                    };
                    deref_typ!("type", ctx, cb_typ,
                        Some(Type::Any | Type::Bottom) => Ok(()),
                        Some(t @ (
                            Type::Primitive(_) | Type::Fn(_) | Type::Set(_)
                            | Type::Error(_) | Type::Array(_) | Type::ByRef(_)
                            | Type::Tuple(_) | Type::Struct(_) | Type::Variant(_, _)
                            | Type::Map { .. } | Type::Abstract { .. }
                        )) => check(t)
                    )?;
                }
            }
        }
        self.cast_typ = Some(cb_arg_typ.clone());
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for PublishRpc<R, E> {
    const NAME: &str = "sys_net_publish_rpc";
    const NEEDS_CALLSITE: bool = true;

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                let pid = BindId::new();
                let mftyp = match &typ.args[3].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                };
                let (x, xn) = genn::bind(
                    ctx,
                    &scope.lexical,
                    "x",
                    mftyp.args[0].typ.clone(),
                    top_id,
                );
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let f = genn::apply(fnode, scope, vec![xn], &mftyp, top_id);
                let mut t = PublishRpc {
                    queue: VecDeque::new(),
                    args: CachedVals::new(from),
                    x,
                    id,
                    top_id,
                    f,
                    pid,
                    argbuf: smallvec![],
                    ready: true,
                    current: None,
                    cast_typ: None,
                };
                if let Some(resolved) = resolved {
                    let _ = t.validate_spec(ctx, resolved);
                }
                Ok(Box::new(t))
            }
            _ => bail!("expected four arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for PublishRpc<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut changed = [false; 4];
        self.args.update_diff(&mut changed, ctx, from, event);
        if changed[3] {
            if let Some(v) = self.args.0[3].clone() {
                ctx.cached.insert(self.pid, v.clone());
                event.variables.insert(self.pid, v);
            }
        }
        if changed[0] || changed[1] || changed[2] {
            if let Some(path) = self.current.take() {
                ctx.rt.unpublish_rpc(path);
            }
            if let (Some(Value::String(path)), Some(doc)) =
                (&self.args.0[0], &self.args.0[1])
            {
                let path = Path::from(path);
                let spec = match &self.args.0[2] {
                    Some(Value::Null) => vec![],
                    Some(Value::Array(spec)) => spec
                        .iter()
                        .map(|field| match field {
                            Value::Array(pair) if pair.len() == 2 => {
                                let name = match &pair[0] {
                                    Value::String(n) => n.clone(),
                                    _ => unreachable!(),
                                };
                                // pair[1] is {default: val, doc: docstr} struct
                                // fields sorted: "default" < "doc"
                                match &pair[1] {
                                    Value::Array(rpc_arg) if rpc_arg.len() == 2 => {
                                        let default_value = match &rpc_arg[0] {
                                            Value::Array(p) => p[1].clone(),
                                            _ => unreachable!(),
                                        };
                                        let doc = match &rpc_arg[1] {
                                            Value::Array(p) => p[1].clone(),
                                            _ => unreachable!(),
                                        };
                                        ArgSpec { name, doc, default_value }
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        })
                        .collect::<Vec<_>>(),
                    _ => vec![],
                };
                if let Err(e) =
                    ctx.rt.publish_rpc(path.clone(), doc.clone(), spec, self.id)
                {
                    let e: ArcStr = format_compact!("{e:?}").as_str().into();
                    let e: Value = (literal!("PublishRpcError"), e).into();
                    return Some(Value::Error(TArc::new(e)));
                }
                self.current = Some(path);
            }
        }
        macro_rules! set {
            ($c:expr) => {{
                self.ready = false;
                self.argbuf.extend($c.args.iter().map(|(n, v)| (n.clone(), v.clone())));
                self.argbuf.sort_by_key(|(n, _)| n.clone());
                let args =
                    ValArray::from_iter_exact(self.argbuf.drain(..).map(|(n, v)| {
                        Value::Array(ValArray::from([Value::String(n), v]))
                    }));
                let args = match &self.cast_typ {
                    Some(typ) => typ.cast_value(&ctx.env, Value::Array(args)),
                    None => Value::Array(args),
                };
                ctx.cached.insert(self.x, args.clone());
                event.variables.insert(self.x, args);
            }};
        }
        if let Some(c) = event.rpc_calls.remove(&self.id) {
            self.queue.push_back(c);
        }
        if self.ready && self.queue.len() > 0 {
            if let Some(c) = self.queue.front() {
                set!(c)
            }
        }
        loop {
            match self.f.update(ctx, event) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    if let Some(mut call) = self.queue.pop_front() {
                        call.reply.send(v);
                    }
                    match self.queue.front() {
                        Some(c) => set!(c),
                        None => break None,
                    }
                }
            }
        }
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => {
                self.f.typecheck(ctx)?;
                Ok(())
            }
            TypecheckPhase::CallSite(resolved) => {
                self.validate_spec(ctx, resolved)?;
                Ok(())
            }
        }
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.f.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        if let Some(path) = self.current.take() {
            ctx.rt.unpublish_rpc(path);
        }
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.cached.remove(&self.pid);
        self.f.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        if let Some(path) = self.current.take() {
            ctx.rt.unpublish_rpc(path);
        }
        self.args.clear();
        self.queue.clear();
        self.argbuf.clear();
        self.ready = true;
        self.f.sleep(ctx);
    }
}
