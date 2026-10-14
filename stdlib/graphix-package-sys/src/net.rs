use crate::netstate::{NetRpcWake, NetState, NetWrite};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Rt, Scope, TagValue,
    UserEvent,
    effects::Effect,
    err, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{CastTarget, Handler, Inbox, Reply, castable, seam_arg};
use netidx::{
    path::Path,
    publisher::Val,
    subscriber::{Dval, UpdatesFlags, Value},
};
use netidx_core::{
    pack::{Pack, PackError},
    utils::Either,
};
use netidx_derive::FromValue;
use netidx_protocols::rpc::server::{self, ArgSpec};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::any::Any;

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

fn remote_err(tag: &ArcStr, e: &Value) -> Value {
    match e {
        Value::String(msg) => {
            Value::Error(Value::from((tag.clone(), msg.clone())).into())
        }
        e => errf!(tag, "{e}"),
    }
}

fn list_response(value: &Value) -> Option<Value> {
    match value {
        Value::Null => None,
        Value::Error(e) => Some(remote_err(&literal!("ListError"), e)),
        v => Some(v.clone()),
    }
}

/// The value written to a path. Values that fire before the path is
/// known queue and are written, in order, when it arrives.
#[derive(Debug)]
pub(crate) struct Write {
    dv: Either<(Path, Dval), Vec<Value>>,
    /// Set by `sleep`, taken by the next update: the target is found again
    /// from the present path.
    slept: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Write {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_write";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Write {
            dv: Either::Right(vec![]),
            slept: false,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let queued = Pack::decode(buf)?;
        Ok(Box::new(Write {
            dv: Either::Right(queued),
            slept: false,
            out: TagValue::phantom(),
        }))
    }
}

impl Write {
    fn same_path(&self, new_path: &Value) -> bool {
        match (new_path, &self.dv) {
            (Value::String(p0), Either::Left((p1, _))) => &**p0 == &**p1,
            _ => false,
        }
    }

    /// Drop the target; what fires next queues.
    fn release<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // CR claude for eric: [bug/medium] release() replaces self.dv with an empty
        // Right(vec![]) even when it already holds the queue. update calls it on every
        // path arrival before the drain, so the drain loop never writes a queued value.
        // Every value that fires while the path is bottom or invalid is pushed without
        // bound and then thrown away, and only the standing value reaches the new path.
        // Write's doc promises those values are written in order. Decide which is
        // meant. Either keep the queue across release (and bound it), or drop the queue
        // and make dv an Option<(Path, Dval)> that writes only the standing value,
        // which also ends the unbounded growth. Either way the doc and image_encode
        // follow. probe: design/review-2026-10-12/repro/sys-net-10.gx (sys-net-10)
        if let Either::Left((_, dv)) =
            std::mem::replace(&mut self.dv, Either::Right(vec![]))
        {
            NetState::get(ctx).release(dv)
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Write {
    /// `Left` is a live subscription.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        match &self.dv {
            Either::Left(_) => Err(PackError::Application(image::NOT_QUIESCENT)),
            Either::Right(queued) => queued.encode(buf),
        }
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept);
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (val, val_fired) = seam_arg(ctx, &mut from[1]);
        // the standing value goes to a new path when nothing queued is newer
        let mut standing = val_fired;
        if (woke || path_fired)
            && let Some(path) = &path
            && (woke || !self.same_path(path))
        {
            self.release(ctx);
            let Some(p) = as_path(path.clone()) else {
                let e = errf!(literal!("WriteError"), "invalid path {path}");
                return self.out.set(TagValue::fired(e));
            };
            let dv = match NetState::get(ctx).write_target(ctx, p.clone()) {
                Ok(dv) => dv,
                Err(e) => {
                    let e = errf!(literal!("WriteError"), "{e:#}");
                    return self.out.set(TagValue::fired(e));
                }
            };
            if let Either::Right(q) = &mut self.dv {
                standing |= q.is_empty() && path_fired;
                for v in q.drain(..) {
                    dv.write(v);
                }
            }
            self.dv = Either::Left((p, dv));
        }
        if standing && let Some(val) = val {
            match &mut self.dv {
                Either::Right(q) => q.push(val),
                Either::Left((_, dv)) => {
                    dv.write(val);
                }
            }
        }
        self.out.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.release(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        self.slept = true;
        self.release(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct Subscribe {
    /// `sleep()` tears the subscription down, so the first update after
    /// must re-establish it from the PRESENT path.
    slept: bool,
    cur: Option<(Path, Dval)>,
    id: BindId,
    top_id: ExprId,
    target: CastTarget,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Subscribe {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_subscribe";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let _ = from;
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Subscribe {
            slept: false,
            cur: None,
            id,
            top_id,
            target: CastTarget::of(resolved),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let slept = bool::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let target = CastTarget::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Subscribe {
            slept,
            cur: None,
            id,
            top_id,
            target,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Subscribe {
    /// `cur` is a live subscription.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.cur.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.target.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        static ERR_TAG: ArcStr = literal!("SubscribeError");
        let woke = std::mem::take(&mut self.slept);
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        match (path, path_fired || woke) {
            (_, false) => (),
            (Some(Value::String(path)), true)
                if self.cur.as_ref().map(|(p, _)| &**p) != Some(&*path) =>
            {
                if let Some((_, dv)) = self.cur.take() {
                    NetState::get(ctx).unsubscribe(dv, self.id);
                    self.renew(ctx);
                }
                let net = NetState::get(ctx);
                let path = Path::from(path);
                if !Path::is_absolute(&path) {
                    return self
                        .out
                        .set(TagValue::fired(err!(ERR_TAG, "expected absolute path")));
                }
                let dval = match net.subscribe(
                    ctx,
                    UpdatesFlags::BEGIN_WITH_LAST,
                    path.clone(),
                    self.id,
                ) {
                    Ok(dv) => dv,
                    Err(e) => {
                        return self.out.set(TagValue::fired(errf!(ERR_TAG, "{e:#}")));
                    }
                };
                self.cur = Some((path, dval));
            }
            (Some(Value::String(_)), true) => (),
            (Some(v), true) => {
                return self.out.set(TagValue::fired(errf!(
                    ERR_TAG,
                    "invalid path {v}, expected string"
                )));
            }
            (None, true) => (),
        }
        // updates arrive on our BindId via the NetState pump; the pump
        // already translated Unsubscribed to the error value
        let res = self.cur.as_ref().and_then(|_| {
            ctx.event.variables.get(&self.id).map(|v| match v.value_cloned() {
                // the pump's unsubscribe, or the publisher's own error
                Value::Error(e) => remote_err(&ERR_TAG, &e),
                v => self.target.cast(&ctx.env, v),
            })
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.target.refresh(resolved);
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        if let Some((_, dv)) = self.cur.take() {
            NetState::get(ctx).unsubscribe(dv, self.id)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        if let Some((_, dv)) = self.cur.take() {
            NetState::get(ctx).unsubscribe(dv, self.id);
        }
        self.renew(ctx);
    }
}

impl Subscribe {
    /// Answer under a fresh id: an update already sent for the old
    /// subscription arrives on the old one, which nothing reads.
    fn renew<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
pub(crate) struct RpcCall {
    top_id: ExprId,
    id: BindId,
    target: CastTarget,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for RpcCall {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_call";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        let _ = from;
        Ok(Box::new(RpcCall {
            top_id,
            id,
            target: CastTarget::of(resolved),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let top_id = ExprId::decode(buf)?;
        let id = BindId::decode(buf)?;
        let target = CastTarget::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(RpcCall { top_id, id, target, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for RpcCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.top_id.encode(buf)?;
        self.id.encode(buf)?;
        self.target.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
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
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (args, args_fired) = seam_arg(ctx, &mut from[1]);
        // CR claude for claude: [bug/medium] `RpcCall::sleep` clears the output and
        // remints the reply id but keeps no slept bit, and this update calls only when
        // path or args fired. After a wake whose arguments are bound outside the arm
        // (so they read stale and do not fire), the call is never issued again and its
        // value stays bottom for good. The `list!` builtins (`sys_net_list`,
        // `sys_net_list_table`) have the same gap: their `sleep` clears `current` and
        // the output, and their update lists only on a fired path or trigger.
        // design/async_sleep_outputs.md requires a restart over the arguments as they
        // stand, and `CachedArgsAsync` does that (`Invocation::Quiet => woke`), as do
        // Subscribe, Publish and PublishRpc. Fix: add a `slept` bit to RpcCall and the
        // list! struct, set it in `sleep`, and treat it as a fire in update. Pin each
        // with arguments bound outside a toggled arm. `Write` has a `woke` bit but
        // writes the standing value only when `val` (or a new path) fired. A value that
        // fired during the sleep is still delivered by catch-up, but an unchanged one
        // is not written again, unlike fs::write_all under `CachedArgsAsync`. Decide
        // which rule a write follows and pin it. probe:
        // design/review-2026-10-12/repro/sys-net-04.gx (sys-net-04)
        if (path_fired || args_fired)
            && let (Some(path), Some(args)) = (&path, &args)
        {
            match parse_args(path, args) {
                Err(e) => {
                    return self
                        .out
                        .set(TagValue::fired(errf!(literal!("RpcError"), "{e}")));
                }
                Ok((path, args)) => {
                    self.remint(ctx);
                    NetState::get(ctx).call_rpc(ctx, path, args, self.id)
                }
            }
        }
        let res = ctx.event.variables.get(&self.id).map(|v| match v.value_cloned() {
            Value::Error(e) => remote_err(&literal!("RpcError"), &e),
            v => self.target.cast(&ctx.env, v),
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.target.refresh(resolved);
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.remint(ctx);
        self.out = TagValue::phantom();
    }
}

impl RpcCall {
    /// A fresh delivery id: a reply to an earlier call lands nowhere.
    fn remint<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

macro_rules! list {
    ($name:ident, $builtin:literal, $table:expr, $typ:literal) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            current: Option<Path>,
            id: BindId,
            top_id: ExprId,
            out: TagValue,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const EFFECT: Effect = Effect::Async;
            const NAME: &str = $builtin;

            fn init<'a, 'b, 'c, 'd>(
                ctx: &'a mut CompileCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R, E>],
                top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                let _ = from;
                let id = BindId::new();
                ctx.record_ref(id, top_id);
                Ok(Box::new($name {
                    current: None,
                    top_id,
                    id,
                    out: TagValue::phantom(),
                }))
            }

            fn image_decode(
                ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let id = BindId::decode(buf)?;
                let top_id = ExprId::decode(buf)?;
                ctx.record_ref(id, top_id);
                Ok(Box::new($name {
                    current: None,
                    top_id,
                    id,
                    out: TagValue::phantom(),
                }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            /// `current` is a list the resolver is serving.
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                if self.current.is_some() {
                    return Err(PackError::Application(image::NOT_QUIESCENT));
                }
                self.id.encode(buf)?;
                self.top_id.encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &mut [Node<R, E>],
            ) -> &TagValue {
                let (_, trigger_fired) = seam_arg(ctx, &mut from[0]);
                let (path, path_fired) = seam_arg(ctx, &mut from[1]);
                // CR claude for claude: [bug/low] Both arms build the path with
                // Path::from and send it on to the resolver, so a relative path (a
                // typo, a missing leading slash) lists [] and reads as an empty
                // directory. Write, Subscribe and the rpc builtins refuse it. Validate
                // with as_path in both arms and fire `ListError("invalid path ..")` on
                // None, as Write fires its WriteError. probe:
                // design/review-2026-10-12/repro/sys-net-06.gx (sys-net-06)
                match (path, path_fired, trigger_fired) {
                    (Some(Value::String(path)), true, _)
                        if self
                            .current
                            .as_ref()
                            .map(|p| &**p != &*path)
                            .unwrap_or(true) =>
                    {
                        let path = Path::from(path);
                        self.current = Some(path.clone());
                        NetState::get(ctx).list(ctx, self.id, path, $table);
                    }
                    (Some(Value::String(path)), _, true) => {
                        NetState::get(ctx).list(ctx, self.id, Path::from(path), $table);
                    }
                    _ => (),
                }
                let res = ctx
                    .event
                    .variables
                    .get(&self.id)
                    .and_then(|v| v.with_value(list_response));
                match res {
                    Some(v) => self.out.set(TagValue::fired(v)),
                    None => self.out.ride(),
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                ctx.release_var(self.id, self.top_id);
                NetState::get(ctx).stop_list(self.id);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                ctx.release_var(self.id, self.top_id);
                NetState::get(ctx).stop_list(self.id);
                self.id = BindId::new();
                ctx.rt.ref_var(self.id, self.top_id);
                self.current = None;
                self.out = TagValue::phantom();
            }
        }
    };
}

list!(
    List,
    "sys_net_list",
    false,
    "fn(?#update:Any, string) -> Result<Array<string>, `ListError(string)>"
);

list!(
    ListTable,
    "sys_net_list_table",
    true,
    "fn(?#update:Any, string) -> Result<Table, `ListError(string)>"
);

fn extract_publish_cast_type(resolved: Option<&FnType>) -> Option<Type> {
    resolved?.args.first().and_then(|a| match &a.typ {
        Type::Fn(cb_ft) if !cb_ft.args.is_empty() => {
            let t = &cb_ft.args[0].typ;
            castable(t).then(|| t.clone())
        }
        _ => None,
    })
}

#[derive(Debug)]
pub(crate) struct Publish<R: Rt, E: UserEvent> {
    /// Wake catch-up: `sleep()` unpublishes, so the first update after
    /// must republish from the present path/value.
    slept: bool,
    current: Option<(Path, Val)>,
    top_id: ExprId,
    x: BindId,
    pid: BindId,
    /// write requests on the published value arrive here as custom
    /// events (routed by the NetState pump)
    wid: BindId,
    on_write: Node<R, E>,
    cast_typ: Option<Type>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Publish<R, E> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_publish";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope = scope.append_block("fn", LambdaId::new().inner());
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
                let on_write =
                    genn::apply(fnode, scope, smallvec::smallvec![xn], &mftyp, top_id);
                let wid = BindId::new();
                ctx.record_ref(wid, top_id);
                Ok(Box::new(Publish {
                    slept: false,
                    current: None,
                    top_id,
                    pid,
                    x,
                    wid,
                    on_write,
                    cast_typ: extract_publish_cast_type(resolved),
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("expected three arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let slept = bool::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let x = BindId::decode(buf)?;
        let pid = BindId::decode(buf)?;
        let wid = BindId::decode(buf)?;
        let on_write = image::decode_node(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(wid, top_id);
        Ok(Box::new(Publish {
            slept,
            current: None,
            top_id,
            x,
            pid,
            wid,
            on_write,
            cast_typ,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Publish<R, E> {
    /// `current` is a live publication.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.top_id.encode(buf)?;
        self.x.encode(buf)?;
        self.pid.encode(buf)?;
        self.wid.encode(buf)?;
        self.on_write.image_encode(buf)?;
        self.cast_typ.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        macro_rules! publish {
            ($path:expr, $v:expr) => {{
                let path = Path::from($path.clone());
                let net = NetState::get(ctx);
                match net.publish(ctx, path.clone(), $v.clone(), self.wid) {
                    Err(e) => {
                        let msg: ArcStr = format_compact!("{e:#}").as_str().into();
                        let e: Value = (literal!("PublishError"), msg).into();
                        return self.out.set(TagValue::fired(Value::Error(e.into())));
                    }
                    Ok(id) => {
                        self.current = Some((path, id));
                    }
                }
            }};
        }
        let woke = std::mem::take(&mut self.slept);
        let (fv, f_fired) = seam_arg(ctx, &mut from[0]);
        let (pathv, path_fired) = seam_arg(ctx, &mut from[1]);
        let (val, val_fired) = seam_arg(ctx, &mut from[2]);
        if f_fired && let Some(v) = fv {
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::fired(v));
        }
        // a path that moved drops the publication even while the value is
        // bottom, so the next value goes to the new path
        if (path_fired || woke)
            && let Some(Value::String(path)) = &pathv
            && self.current.as_ref().is_some_and(|(p, _)| &**p != path)
            && let Some((_, id)) = self.current.take()
        {
            NetState::get(ctx).unpublish(id);
        }
        if let (Some(Value::String(path)), Some(v)) = (&pathv, &val) {
            match &self.current {
                None if path_fired || val_fired || woke => publish!(path, v),
                Some((_, id)) if val_fired => {
                    NetState::get(ctx).update_val(id, v.clone())
                }
                None | Some(_) => (),
            }
        }
        let mut reply = None;
        if self.current.is_some() {
            if let Some(mut cbt) = ctx.event.take_custom(&self.wid) {
                if let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<NetWrite>() {
                    let req = &mut w.0;
                    let v = match &self.cast_typ {
                        Some(typ) => typ.cast_value(&ctx.env, req.value.clone()),
                        None => req.value.clone(),
                    };
                    // a value the handler cannot take is the writer's error
                    if let Value::Error(_) = &v
                        && !matches!(req.value, Value::Error(_))
                    {
                        if let Some(reply) = req.send_result.take() {
                            reply.send(v)
                        }
                    } else {
                        ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
                        ctx.event.variables.insert(self.x, TagValue::fired(v));
                        reply = req.send_result.take();
                    }
                }
            }
        }
        // CR claude for claude: [dead/low] net.gx types on_write as `fn(v: 'a) -> _`,
        // and the check refuses a callback that returns a value (`fn(v: i64) -> i64` is
        // not contained). So this branch never yields a value and `reply.send` never
        // runs. The `reply` taken from `req.send_result` is only dropped, which netidx
        // answers as a Null write result. Delete the `reply` variable and this send.
        // Keep `self.on_write.update(ctx)` for its effects and let send_result drop
        // with the NetWrite. (sys-net-07)
        if let Some(v) = graphix_package_core::seam_tick(self.on_write.update(ctx))
            .map(|tv| tv.clone())
        {
            if let Some(reply) = reply {
                reply.send(v.value())
            }
        }
        self.out.ride()
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.on_write.typecheck0(ctx)?;
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = extract_publish_cast_type(Some(resolved));
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.on_write.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some((_, val)) = self.current.take() {
            NetState::get(ctx).unpublish(val);
        }
        ctx.release_var(self.wid, self.top_id);
        ctx.rt.store_remove(&self.pid);
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.on_write.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        if let Some((_, val)) = self.current.take() {
            NetState::get(ctx).unpublish(val);
        }
        self.on_write.sleep(ctx);
    }
}

/// A call's reply.
#[derive(Debug)]
struct RpcReply(server::RpcCall);

impl Reply for RpcReply {
    fn reply(mut self, v: Value) {
        self.0.reply.send(v)
    }
}

/// Named arguments as a struct value, sorted by name; none is null.
fn args_struct(args: &mut LPooled<Vec<(ArcStr, Value)>>) -> Value {
    if args.is_empty() {
        return Value::Null;
    }
    args.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    Value::Array(ValArray::from_iter_exact(
        args.drain(..).map(|(n, v)| Value::Array(ValArray::from([Value::String(n), v]))),
    ))
}

/// An rpc's spec, from `#spec`: each field's `{default, doc}`.
fn parse_spec(spec: &Value) -> Result<Vec<ArgSpec>> {
    #[derive(FromValue)]
    struct RpcArg {
        default: Value,
        doc: Value,
    }
    let fields = match spec {
        Value::Null => return Ok(vec![]),
        Value::Array(fields) => fields,
        v => bail!("rpc #spec must be a struct or null, not {v}"),
    };
    fields
        .iter()
        .map(|field| match field {
            Value::Array(p) if let [Value::String(name), arg] = &p[..] => {
                let RpcArg { default, doc } = arg.clone().cast_to::<RpcArg>().map_err(|e| {
                    anyhow!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}: {e}")
                })?;
                Ok(ArgSpec { name: name.clone(), doc, default_value: default })
            }
            v => bail!("rpc #spec must be a struct, not {v}"),
        })
        .collect()
}

#[derive(Debug)]
pub(crate) struct PublishRpc<R: Rt, E: UserEvent> {
    /// Wake catch-up: `sleep()` drops the published proc, so the first
    /// update after must republish from the present path/doc/spec.
    slept: bool,
    id: BindId,
    top_id: ExprId,
    handler: Handler<R, E, RpcReply>,
    /// Calls posted under `id`, taken at each wake.
    inbox: std::sync::Arc<Inbox<server::RpcCall>>,
    current: Option<(Path, server::Proc)>,
    /// The handler's argument type, which a call's arguments are cast to.
    cast_typ: Option<Type>,
    /// The published spec's defaults, which a call's arguments override.
    defaults: Vec<(ArcStr, Value)>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> PublishRpc<R, E> {
    fn handler_arg(resolved: &FnType) -> Option<Type> {
        match &resolved.args.get(3)?.typ {
            Type::Fn(ft) => Some(ft.args.first()?.typ.clone()),
            _ => None,
        }
    }

    /// The call's arguments as the handler takes them: the spec's
    /// defaults under the arguments given, sorted into a struct and cast to
    /// its argument type.
    fn arg(&self, ctx: &ExecCtx<'_, R, E>, call: &server::RpcCall) -> Value {
        let mut args: LPooled<Vec<(ArcStr, Value)>> = self
            .defaults
            .iter()
            .filter(|(n, _)| !call.args.contains_key(n))
            .cloned()
            .chain(call.args.iter().map(|(n, v)| (n.clone(), v.clone())))
            .collect();
        let args = args_struct(&mut args);
        match &self.cast_typ {
            Some(typ) => typ.cast_value(&ctx.env, args),
            None => args,
        }
    }

    fn publish(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: &Value,
        doc: &Value,
        spec: &Value,
    ) -> Result<()> {
        let path = as_path(path.clone()).ok_or_else(|| anyhow!("invalid path {path}"))?;
        let spec = parse_spec(spec)?;
        self.defaults =
            spec.iter().map(|a| (a.name.clone(), a.default_value.clone())).collect();
        if let Some(typ) = &self.cast_typ {
            let mut defaults: LPooled<Vec<(ArcStr, Value)>> =
                self.defaults.iter().cloned().collect();
            if let Value::Error(e) = typ.cast_value(&ctx.env, args_struct(&mut defaults))
            {
                bail!("rpc #spec does not fit #f's argument {typ}: {e}")
            }
        }
        let proc = NetState::get(ctx).publish_rpc(
            ctx,
            path.clone(),
            doc.clone(),
            spec,
            self.id,
            self.inbox.clone(),
        )?;
        self.current = Some((path, proc));
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for PublishRpc<R, E> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_publish_rpc";

    fn init<'a, 'b, 'c>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let [_, _, _, _] = from else { bail!("expected four arguments") };
        let typ = resolved.unwrap_or(typ);
        let handler = Handler::new(ctx, &typ.args[3].typ, scope, top_id)?;
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(PublishRpc {
            slept: false,
            id,
            top_id,
            handler,
            inbox: Default::default(),
            current: None,
            cast_typ: resolved.and_then(Self::handler_arg),
            defaults: Vec::new(),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let slept = bool::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let handler = Handler::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(PublishRpc {
            slept,
            id,
            top_id,
            handler,
            inbox: Default::default(),
            current: None,
            cast_typ,
            defaults: Vec::new(),
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for PublishRpc<R, E> {
    /// `current` is a live procedure.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.handler.image_encode(buf)?;
        self.cast_typ.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept);
        let (pathv, path_fired) = seam_arg(ctx, &mut from[0]);
        let (docv, doc_fired) = seam_arg(ctx, &mut from[1]);
        let (specv, spec_fired) = seam_arg(ctx, &mut from[2]);
        let (fv, f_fired) = seam_arg(ctx, &mut from[3]);
        self.handler.set_fn(ctx, fv, f_fired);
        let mut failed = None;
        if path_fired || doc_fired || spec_fired || woke {
            if crate::netstate::rpc_dbg() {
                eprintln!(
                    "RPCDBG publish_rpc {:?}: (re)publish changed={:?} had={:?}",
                    self.id,
                    [path_fired, doc_fired, spec_fired, f_fired],
                    self.current.as_ref().map(|(p, _)| p)
                );
            }
            // dropping the proc unpublishes it
            // CR claude for claude: [bug/medium] Dropping the Proc does not unpublish
            // it in time. netidx_protocols::rpc::server::Proc holds only a stop sender;
            // the published Vals live in its ProcInner task and drop only after that
            // task sees the stop. Re-publishing at the same path in the same cycle
            // therefore often fails with PublishRpcError "already published". `current`
            // then stays None, so the rpc is offline (calls go unanswered or are
            // cancelled) until #path, #doc or #spec fires again. A reactive #doc fails
            // on 2-4 of 10 changes. Fix the root cause in netidx-protocols: make
            // server::Proc own its Vals, or give it a synchronous unpublish, so a drop
            // frees the path at once the way publisher::Val's drop does. Then add a pin
            // that changes #doc and calls the rpc after each change. probe:
            // design/review-2026-10-12/repro/sys-net-03.gx (sys-net-03)
            self.current = None;
            if let (Some(path), Some(doc), Some(spec)) = (&pathv, &docv, &specv)
                && let Err(e) = self.publish(ctx, path, doc, spec)
            {
                failed = Some(errf!(literal!("PublishRpcError"), "{e:#}"));
            }
        }
        if let Some(cbt) = ctx.event.take_custom(&self.id)
            && (&*cbt as &dyn Any).is::<NetRpcWake>()
        {
            let mut calls: LPooled<Vec<server::RpcCall>> = LPooled::take();
            self.inbox.take(&mut *calls);
            for c in calls.drain(..) {
                match self.arg(ctx, &c) {
                    e @ Value::Error(_) => RpcReply(c).reply(e),
                    arg => self.handler.push(arg, RpcReply(c)),
                }
            }
        }
        self.handler.update(ctx);
        match failed {
            Some(e) => self.out.set(TagValue::fired(e)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.handler.typecheck0(ctx)
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = Self::handler_arg(resolved);
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.handler.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.current = None;
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.inbox = Default::default();
        self.current = None;
        self.handler.sleep(ctx);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn list_error_preserves_remote_message() {
        let message = literal!("context: \"detail\"");
        let error = Value::error(message.clone());
        let expected = Value::Error(Value::from((literal!("ListError"), message)).into());
        assert_eq!(list_response(&error), Some(expected));
        assert_eq!(list_response(&Value::Null), None);
    }
}
