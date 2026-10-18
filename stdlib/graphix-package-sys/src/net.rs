use crate::netstate::{NetRpcWake, NetState, NetWrite};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Rt, Scope, TagValue,
    UserEvent,
    effects::Effect,
    err, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::{coretraits, genn},
    rep,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CastTarget, Handler, Inbox, Reply, RtVar, castable, seam_arg, write_data,
};
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
use poolshark::local::LPooled;
use std::any::Any;

fn as_path(v: Value) -> Option<Path> {
    match v.cast_to::<ArcStr>() {
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
        Value::String(msg) => Value::Error(
            rep::make_variant(tag.clone(), [Value::String(msg.clone())]).into(),
        ),
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
        let mut standing = woke || val_fired;
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
            let val = match write_data(ctx, &val) {
                Ok(val) => val,
                Err(e) => {
                    let e = errf!(literal!("WriteError"), "{e:#}");
                    return self.out.set(TagValue::fired(e));
                }
            };
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
    var: RtVar,
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
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(Subscribe {
            slept: false,
            cur: None,
            var,
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
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Box::new(Subscribe {
            slept,
            cur: None,
            var,
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
        self.var.image_encode(buf)?;
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
                    NetState::get(ctx).unsubscribe(dv, self.var.id);
                    self.var.remint(ctx);
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
                    self.var.id,
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
        let got = self.cur.as_ref().and_then(|_| {
            ctx.event.variables.get(&self.var.id).map(|v| v.value_cloned())
        });
        let res = got.map(|v| match v {
            // the pump's unsubscribe, or the publisher's own error
            Value::Error(e) => remote_err(&ERR_TAG, &e),
            v => coretraits::with_display_hooks(ctx, |env| self.target.cast(env, v)),
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
        self.var.release(ctx);
        if let Some((_, dv)) = self.cur.take() {
            NetState::get(ctx).unsubscribe(dv, self.var.id)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        if let Some((_, dv)) = self.cur.take() {
            NetState::get(ctx).unsubscribe(dv, self.var.id);
        }
        self.var.remint(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct RpcCall {
    slept: bool,
    var: RtVar,
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
        let var = RtVar::new(ctx, top_id);
        let _ = from;
        Ok(Box::new(RpcCall {
            slept: false,
            var,
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
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Box::new(RpcCall { slept, var, target, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for RpcCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.slept.encode(buf)?;
        self.var.image_encode(buf)?;
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
                args => match rep::read_struct(args).map(rep::Parts::elems) {
                    Some(fields) => fields
                        .iter()
                        .map(|f| match rep::field(f) {
                            Some((name, value)) => Ok((name.clone(), value.clone())),
                            None => Err(anyhow!("rpc args expected [name, value] pair")),
                        })
                        .collect::<Result<Vec<_>>>()?,
                    None => bail!("rpc args expected to be a struct or null"),
                },
            };
            Ok((path, args))
        }
        let woke = std::mem::take(&mut self.slept);
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (args, args_fired) = seam_arg(ctx, &mut from[1]);
        if (woke || path_fired || args_fired)
            && let (Some(path), Some(args)) = (&path, &args)
        {
            self.remint(ctx);
            let args = match write_data(ctx, args) {
                Ok(args) => args,
                Err(e) => {
                    return self
                        .out
                        .set(TagValue::fired(errf!(literal!("RpcError"), "{e:#}")));
                }
            };
            match parse_args(path, &args) {
                Err(e) => {
                    return self
                        .out
                        .set(TagValue::fired(errf!(literal!("RpcError"), "{e}")));
                }
                Ok((path, args)) => {
                    NetState::get(ctx).call_rpc(ctx, path, args, self.var.id)
                }
            }
        }
        let got = ctx.event.variables.get(&self.var.id).map(|v| v.value_cloned());
        let res = got.map(|v| match v {
            Value::Error(e) => remote_err(&literal!("RpcError"), &e),
            v => coretraits::with_display_hooks(ctx, |env| self.target.cast(env, v)),
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
        self.release(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.remint(ctx);
        self.out = TagValue::phantom();
    }
}

impl RpcCall {
    fn release<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(net) = ctx.libstate.get::<NetState>() {
            net.stop_request(self.var.id);
        }
        self.var.release(ctx);
    }

    /// A fresh delivery id: a reply to an earlier call lands nowhere.
    fn remint<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(net) = ctx.libstate.get::<NetState>() {
            net.stop_request(self.var.id);
        }
        self.var.remint(ctx);
    }
}

macro_rules! list {
    ($name:ident, $builtin:literal, $table:expr, $typ:literal) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            slept: bool,
            current: Option<Path>,
            var: RtVar,
            out: TagValue,
        }

        impl $name {
            fn remint<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                NetState::get(ctx).stop_list(self.var.id);
                self.var.remint(ctx);
                self.current = None;
            }
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
                let var = RtVar::new(ctx, top_id);
                Ok(Box::new($name {
                    slept: false,
                    current: None,
                    var,
                    out: TagValue::phantom(),
                }))
            }

            fn image_decode(
                ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let slept = bool::decode(buf)?;
                let var = RtVar::image_decode(ctx, buf)?;
                Ok(Box::new($name {
                    slept,
                    current: None,
                    var,
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
                self.slept.encode(buf)?;
                self.var.image_encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &mut [Node<R, E>],
            ) -> &TagValue {
                let woke = std::mem::take(&mut self.slept);
                let (_, trigger_fired) = seam_arg(ctx, &mut from[0]);
                let (path, path_fired) = seam_arg(ctx, &mut from[1]);
                if (woke || path_fired || trigger_fired)
                    && let Some(requested) = path
                {
                    let Some(path) = as_path(requested.clone()) else {
                        self.remint(ctx);
                        return self.out.set(TagValue::fired(errf!(
                            literal!("ListError"),
                            "invalid path {requested}"
                        )));
                    };
                    if trigger_fired || self.current.as_ref() != Some(&path) {
                        if self.current.as_ref() != Some(&path) {
                            self.remint(ctx);
                        }
                        self.current = Some(path.clone());
                        NetState::get(ctx).list(ctx, self.var.id, path, $table);
                    }
                }
                let res = ctx
                    .event
                    .variables
                    .get(&self.var.id)
                    .and_then(|v| v.with_value(list_response));
                match res {
                    Some(v) => self.out.set(TagValue::fired(v)),
                    None => self.out.ride(),
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.var.release(ctx);
                NetState::get(ctx).stop_list(self.var.id);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.slept = true;
                self.remint(ctx);
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
    /// write requests on the published value arrive here as custom
    /// events (routed by the NetState pump)
    wid: BindId,
    on_write: genn::Callback<R, E>,
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
                let mftyp = match &typ.args[0].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let on_write = genn::Callback::new(ctx, &scope, &mftyp, &["x"], top_id)?;
                let wid = BindId::new();
                ctx.record_ref(wid, top_id);
                Ok(Box::new(Publish {
                    slept: false,
                    current: None,
                    top_id,
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
        let wid = BindId::decode(buf)?;
        let on_write = genn::Callback::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(wid, top_id);
        Ok(Box::new(Publish {
            slept,
            current: None,
            top_id,

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
                        let e = errf!("PublishError", "{e:#}");
                        return self.out.set(TagValue::fired(e));
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
            self.on_write.feed_fn(ctx, TagValue::fired(v));
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
        let published = val.filter(|_| path_fired || val_fired || woke);
        let val = match published.map(|v| write_data(ctx, &v)).transpose() {
            Ok(val) => val,
            Err(e) => {
                let e = errf!(literal!("PublishError"), "{e:#}");
                return self.out.set(TagValue::fired(e));
            }
        };
        if let (Some(Value::String(path)), Some(v)) = (&pathv, &val) {
            match &self.current {
                None if path_fired || val_fired || woke => publish!(path, v),
                Some((_, id)) if val_fired => {
                    NetState::get(ctx).update_val(id, v.clone())
                }
                None | Some(_) => (),
            }
        }
        if self.current.is_some() {
            if let Some(mut cbt) = ctx.event.take_custom(&self.wid) {
                if let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<NetWrite>() {
                    let req = &mut w.0;
                    let v = match &self.cast_typ {
                        Some(typ) => coretraits::with_display_hooks(ctx, |env| {
                            typ.cast_value(env, req.value.clone())
                        }),
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
                        self.on_write.feed_arg(ctx, 0, TagValue::fired(v));
                    }
                }
            }
        }
        self.on_write.update(ctx);
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

    fn data(&self) -> Option<ArcStr> {
        Some(literal!("RpcError"))
    }
}

/// Named arguments as a struct value, sorted by name; none is null.
fn args_struct(args: &mut LPooled<Vec<(ArcStr, Value)>>) -> Value {
    if args.is_empty() {
        return Value::Null;
    }
    args.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    rep::structure(args.drain(..))
}

/// An rpc's spec, from `#spec`: each field's `{default, doc}`.
fn parse_spec(spec: &Value) -> Result<Vec<ArgSpec>> {
    #[derive(FromValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
    struct RpcArg {
        default: Value,
        doc: Value,
    }
    let fields = match spec {
        Value::Null => return Ok(vec![]),
        v => match rep::read_struct(v).map(rep::Parts::elems) {
            Some(fields) => fields,
            None => bail!(
                "rpc #spec must be a struct or null, not {}",
                graphix_compiler::typ::Naked(v)
            ),
        },
    };
    fields
        .iter()
        .map(|field| match rep::field(field) {
            Some((name, arg)) => {
                let RpcArg { default, doc } = arg.clone().cast_to::<RpcArg>().map_err(|e| {
                    anyhow!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}: {e}")
                })?;
                Ok(ArgSpec { name: name.clone(), doc, default_value: default })
            }
            None => bail!("rpc #spec must be a struct, not {field}"),
        })
        .collect()
}

#[derive(Debug)]
pub(crate) struct PublishRpc<R: Rt, E: UserEvent> {
    /// Wake catch-up: `sleep()` drops the published proc, so the first
    /// update after must republish from the present path/doc/spec.
    slept: bool,
    var: RtVar,
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
    fn arg(&self, ctx: &mut ExecCtx<'_, R, E>, call: &server::RpcCall) -> Value {
        let mut args: LPooled<Vec<(ArcStr, Value)>> = self
            .defaults
            .iter()
            .filter(|(n, _)| !call.args.contains_key(n))
            .cloned()
            .chain(call.args.iter().map(|(n, v)| (n.clone(), v.clone())))
            .collect();
        let args = args_struct(&mut args);
        match &self.cast_typ {
            Some(typ) => {
                coretraits::with_display_hooks(ctx, |env| typ.cast_value(env, args))
            }
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
        let spec = parse_spec(&write_data(ctx, spec)?)?;
        self.defaults =
            spec.iter().map(|a| (a.name.clone(), a.default_value.clone())).collect();
        if let Some(typ) = &self.cast_typ {
            let mut defaults: LPooled<Vec<(ArcStr, Value)>> =
                self.defaults.iter().cloned().collect();
            let defaults = args_struct(&mut defaults);
            if let Value::Error(e) =
                coretraits::with_display_hooks(ctx, |env| typ.cast_value(env, defaults))
            {
                bail!("rpc #spec does not fit #f's argument {typ}: {e}")
            }
        }
        let proc = NetState::get(ctx).publish_rpc(
            ctx,
            path.clone(),
            doc.clone(),
            spec,
            self.var.id,
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
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(PublishRpc {
            slept: false,
            var,
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
        let var = RtVar::image_decode(ctx, buf)?;
        let handler = Handler::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        Ok(Box::new(PublishRpc {
            slept,
            var,
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
        self.var.image_encode(buf)?;
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
                    self.var.id,
                    [path_fired, doc_fired, spec_fired, f_fired],
                    self.current.as_ref().map(|(p, _)| p)
                );
            }
            // dropping the proc unpublishes it
            self.current = None;
            if let (Some(path), Some(doc), Some(spec)) = (&pathv, &docv, &specv)
                && let Err(e) = self.publish(ctx, path, doc, spec)
            {
                failed = Some(errf!(literal!("PublishRpcError"), "{e:#}"));
            }
        }
        if let Some(cbt) = ctx.event.take_custom(&self.var.id)
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
        self.var.release(ctx);
        self.current = None;
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        self.var.remint(ctx);
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
        let expected = Value::Error(
            rep::make_variant(literal!("ListError"), [Value::String(message)]).into(),
        );
        assert_eq!(list_response(&error), Some(expected));
        assert_eq!(list_response(&Value::Null), None);
    }
}
