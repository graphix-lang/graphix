use crate::netstate::{NetRpcCall, NetState, NetWrite};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Rt, Scope, TagValue,
    UserEvent, deref_typ,
    effects::Effect,
    err, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{extract_cast_type, seam_arg};
use netidx::{
    path::Path,
    publisher::{Typ, Val},
    subscriber::{Dval, UpdatesFlags, Value},
};
use netidx_core::{
    pack::{Pack, PackError},
    utils::Either,
};
use netidx_protocols::rpc::server::{self, ArgSpec};
use netidx_value::ValArray;
use smallvec::{SmallVec, smallvec};
use std::any::Any;
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
    id: BindId,
    dv: Either<(Path, Dval), Vec<Value>>,
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
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let _ = (top_id, from);
        Ok(Box::new(Write {
            dv: Either::Right(vec![]),
            id: BindId::new(),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let queued = Pack::decode(buf)?;
        Ok(Box::new(Write { id, dv: Either::Right(queued), out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Write {
    /// `Left` is a live subscription.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let queued = match &self.dv {
            Either::Left(_) => return Err(PackError::Application(image::NOT_QUIESCENT)),
            Either::Right(queued) => queued,
        };
        self.id.encode(buf)?;
        queued.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        fn set(dv: &mut Either<(Path, Dval), Vec<Value>>, val: &Value) {
            match dv {
                Either::Right(q) => q.push(val.clone()),
                Either::Left((_, dv)) => {
                    dv.write(val.clone());
                }
            }
        }
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (val, val_fired) = seam_arg(ctx, &mut from[1]);
        let mut wrote = false;
        if path_fired
            && let Some(path) = &path
            && !self.same_path(path)
        {
            match as_path(path.clone()) {
                None => {
                    // Release the old target's registration before dropping it;
                    // the graveyard only sees UNSUBSCRIBED Dvals.
                    if let Either::Left((_, old)) = &self.dv {
                        let old = old.clone();
                        NetState::get(ctx).unsubscribe(old, self.id);
                        self.dv = Either::Right(vec![]);
                    }
                    let e = errf!(literal!("WriteError"), "invalid path {path:?}");
                    return self.out.set(TagValue::fired(Value::Error(e.into())));
                }
                Some(path) => {
                    let net = NetState::get(ctx);
                    // CR claude for claude: [perf] Write gets its Dval through
                    // NetState::subscribe with self.id. That registers the shared
                    // update channel and a pump route for the id (netstate.rs:408-410),
                    // yet Write never refs the id and never reads it. Every update of
                    // the target path is routed there anyway: the runtime stores the
                    // value under the id, where it stays after the Write is gone, and
                    // runs a cycle with nothing scheduled. A process whose only
                    // statement was a write to a path updated about 500 times a second
                    // used 28 clock ticks of CPU in 6 s, against 0-1 for a write to an
                    // unpublished path. A write needs only the Dval: take it from
                    // subscriber.subscribe(path), with no update channel and no route.
                    // (sys-net-12)
                    let dv = match net.subscribe(
                        ctx,
                        UpdatesFlags::empty(),
                        path.clone(),
                        self.id,
                    ) {
                        Ok(dv) => dv,
                        Err(e) => {
                            let e = errf!(literal!("WriteError"), "{e:?}");
                            return self.out.set(TagValue::fired(Value::Error(e.into())));
                        }
                    };
                    match &mut self.dv {
                        Either::Left((_, old)) => {
                            // Same release-before-overwrite as the
                            // invalid-path arm above.
                            let old = old.clone();
                            NetState::get(ctx).unsubscribe(old, self.id);
                        }
                        // CR claude for claude: [bug] When the path first arrives, this
                        // drains the queue into the new Dval, and then line 158 writes
                        // the standing `val` again. Unless `val` fired this cycle, the
                        // queue's last entry already is that value. A value that fired
                        // before its path was known (a constant, with the path read
                        // from a subscription) therefore reaches the publisher twice,
                        // and its on_write runs twice; queued values 1, 2 arrive as 1,
                        // 2, 2. The standing write belongs only to a value that fired
                        // this cycle, or to an empty queue (a path switch). probe:
                        // design/review-2026-10-05/repro/sys-net-05.gx (sys-net-05)
                        Either::Right(q) => {
                            for v in q.drain(..) {
                                dv.write(v);
                            }
                        }
                    }
                    self.dv = Either::Left((path, dv));
                    if let Some(val) = &val {
                        set(&mut self.dv, val);
                        wrote = true;
                    }
                }
            }
        }
        if val_fired && !wrote {
            if let Some(val) = &val {
                set(&mut self.dv, val)
            }
        }
        self.out.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Either::Left((_, dv)) = &self.dv {
            NetState::get(ctx).unsubscribe(dv.clone(), self.id)
        }
        self.dv = Either::Right(vec![])
    }

    // CR claude for claude: [bug] This sleep unsubscribes and empties `dv`, but Write has
    // no `slept` bit, and update re-subscribes only when the path fires (line 114). If
    // the path is bound outside the arm, a woken arm reads it stale, so every later
    // write is pushed onto the queue (line 105) and never sent. The queue grows without
    // bound and is replayed whole to the next path that fires, which is a different
    // publisher if the path moved. Subscribe, Publish and PublishRpc re-establish from
    // their present arguments on the first update after sleep; Write needs the same.
    // probe: design/review-2026-10-05/repro/sys-net-04.gx (the writes at x = 4, 5, 7, 8
    // never reach /a and land on /b at x = 10). (sys-net-04)
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        match &mut self.dv {
            Either::Left((_, dv)) => {
                let dv = dv.clone();
                NetState::get(ctx).unsubscribe(dv, self.id);
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
    /// `sleep()` tears the subscription down, so the first update after
    /// must re-establish it from the PRESENT path.
    slept: bool,
    cur: Option<(Path, Dval)>,
    id: BindId,
    top_id: ExprId,
    cast_typ: Option<Type>,
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
            cast_typ: extract_cast_type(resolved),
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
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Subscribe {
            slept,
            cur: None,
            id,
            top_id,
            cast_typ,
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
        self.cast_typ.encode(buf)
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
                let net = NetState::get(ctx);
                if let Some((_, dv)) = self.cur.take() {
                    net.unsubscribe(dv, self.id)
                }
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
                        return self.out.set(TagValue::fired(errf!(ERR_TAG, "{e:?}")));
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
            // CR claude for claude: [bug] Every delivered value is cast to the success
            // type here, errors included, and RpcCall::update does the same at line
            // 461. So the pump's unsubscribed error (netstate.rs:47) and every call
            // failure (a handler's error reply, an unknown argument name, the 10 s
            // subscribe timeout) reach Type::cast_inner, whose Primitive arm passes the
            // Error to netidx's Value::cast, which treats it as false. A publisher
            // going away reads as 0 under `let x: i64 = sys::net::subscribe(p)?` (the
            // book's intro example), a `null`-typed rpc command reports success on
            // every failure, and the declared SubscribeError/RpcError is never
            // produced. Turn an Error delivery into SubscribeError/RpcError before the
            // cast, as List does at line 598; the pump's translation leaves no way to
            // tell Unsubscribed from a published error value. The cast core has the
            // same hole on its own: cast<i64>(error(`E)) is ok 0. probe:
            // design/review-2026-10-05/repro/sys-net-01.gx (sys-net-01)
            ctx.event.variables.get(&self.id).map(|v| match &self.cast_typ {
                Some(typ) => typ.cast_value(&ctx.env, v.value_cloned()),
                None => v.value_cloned(),
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
        self.cast_typ = extract_cast_type(Some(resolved));
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
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
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
pub(crate) struct RpcCall {
    top_id: ExprId,
    id: BindId,
    cast_typ: Option<Type>,
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
            cast_typ: extract_cast_type(resolved),
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
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(RpcCall { top_id, id, cast_typ, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for RpcCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.top_id.encode(buf)?;
        self.id.encode(buf)?;
        self.cast_typ.encode(buf)
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
        if (path_fired || args_fired)
            && let (Some(path), Some(args)) = (&path, &args)
        {
            match parse_args(path, args) {
                Err(e) => {
                    return self
                        .out
                        .set(TagValue::fired(errf!(literal!("RpcError"), "{e}")));
                }
                // CR claude for eric: [bug] Every fire of `path` or `args` spawns
                // another call onto the same `self.id` while earlier calls are still in
                // flight. The runtime delivers the replies in completion order, so a
                // slow reply to an abandoned request that lands after the current
                // request's reply becomes the settled value. Moving `p` from a slow
                // proc to a fast one gives 2, then the slow proc's 1 while `p` names
                // the fast proc. An abandoned call that fails late (an unpublished
                // path's 10 s subscribe timeout) likewise replaces a good reply with
                // its error. Either keep one call in flight and queue the rest, as
                // CachedArgsAsync does, or re-mint the delivery id per request, as
                // `sleep` already does, so a stale reply lands nowhere. probe:
                // design/review-2026-10-05/repro/sys-net-09.gx (sys-net-09)
                Ok((path, args)) => NetState::get(ctx).call_rpc(ctx, path, args, self.id),
            }
        }
        let res = ctx.event.variables.get(&self.id).map(|v| match &self.cast_typ {
            Some(typ) => typ.cast_value(&ctx.env, v.value_cloned()),
            None => v.value_cloned(),
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
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = extract_cast_type(Some(resolved));
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.out = TagValue::phantom();
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
                let res = ctx.event.variables.get(&self.id).and_then(|v| {
                    v.with_value(|v| match v {
                        Value::Null => None,
                        Value::Error(e) => Some(errf!(literal!("ListError"), "{e}")),
                        v => Some(v.clone()),
                    })
                });
                match res {
                    Some(v) => self.out.set(TagValue::fired(v)),
                    None => self.out.ride(),
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                ctx.unref_var(self.id, self.top_id);
                NetState::get(ctx).stop_list(self.id);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                ctx.unref_var(self.id, self.top_id);
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
    let resolved = resolved?;
    resolved.args.first().and_then(|a| match &a.typ {
        Type::Fn(cb_ft) if !cb_ft.args.is_empty() => {
            let t = &cb_ft.args[0].typ;
            // CR claude for claude: [structure] This decides whether t can be a cast
            // target by printing it and looking for a quote. That allocates a String
            // per typecheck and depends on printer details: without DerefTVars a TVar
            // prints as its name whether bound or not, and ⊥ prints as _ and passes.
            // extract_cast_type (stdlib/graphix-package-core/src/lib.rs:57-90) answers
            // the same question structurally, with has_unbound plus a ⊥ check, so
            // publish and subscribe/call follow two different rules. Move that
            // predicate into one package-core helper and call it from both.
            // (sys-net-17)
            if format!("{t}").contains('\'') { None } else { Some(t.clone()) }
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
                        let msg: ArcStr = format_compact!("{e:?}").as_str().into();
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
        match ((path_fired || woke, val_fired), (&pathv, &val)) {
            ((true, _), (Some(Value::String(path)), Some(v)))
                if self.current.as_ref().map(|(p, _)| &**p != path).unwrap_or(true) =>
            {
                if let Some((_, id)) = self.current.take() {
                    NetState::get(ctx).unpublish(id);
                }
                publish!(path, v)
            }
            // CR claude for claude: [bug] A path fire while `v` is bottom matches neither
            // arm: the arm above needs a value and this one needs a fire of `v`. So
            // `current` keeps the old path's Val. When `v` fires again, this arm calls
            // update_val on that Val, so the value is published at the old path, and
            // the new path is not published until `p` fires again. Either compare the
            // present path with `current`'s here and republish when they differ, or
            // drop the publication on every path fire as PublishRpc does (net.rs:1122).
            // probe: design/review-2026-10-05/repro/sys-net-08.gx (sys-net-08)
            ((_, true), (Some(Value::String(path)), Some(v))) => match &self.current {
                Some((_, val)) => NetState::get(ctx).update_val(val, v.clone()),
                None => publish!(path, v),
            },
            _ => (),
        }
        let mut reply = None;
        if self.current.is_some() {
            if let Some(mut cbt) = ctx.event.take_custom(&self.wid) {
                if let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<NetWrite>() {
                    let req = &mut w.0;
                    // CR claude for claude: [bug] When the cast to the on_write parameter
                    // type fails, cast_value returns an InvalidCast error value, and it
                    // is stored in `x` and delivered to the callback anyway, so a
                    // parameter typed i64 (annotated or inferred) holds an error. Any
                    // netidx client that writes a wrong-typed value (another process on
                    // the machine-local resolver included) then kills the runtime under
                    // the JIT (the staging panic at fusion/kernel.rs:243, "graphix
                    // runtime is dead"), while the node-walk computes with the mistyped
                    // value and bottoms. A failed cast should reach neither `x` nor the
                    // callback; answer the writer with the cast error through
                    // req.send_result instead. PublishRpc's set! casts call arguments
                    // the same way. probe: design/review-2026-10-05/repro/sys-net-02.gx
                    // (sys-net-02)
                    let v = match &self.cast_typ {
                        Some(typ) => typ.cast_value(&ctx.env, req.value.clone()),
                        None => req.value.clone(),
                    };
                    ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
                    ctx.event.variables.insert(self.x, TagValue::fired(v));
                    reply = req.send_result.take();
                }
            }
        }
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
        ctx.unref_var(self.wid, self.top_id);
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

#[derive(Debug)]
pub(crate) struct PublishRpc<R: Rt, E: UserEvent> {
    /// Wake catch-up: `sleep()` drops the published proc, so the first
    /// update after must republish from the present path/doc/spec.
    slept: bool,
    id: BindId,
    top_id: ExprId,
    f: Node<R, E>,
    pid: BindId,
    x: BindId,
    queue: VecDeque<server::RpcCall>,
    // CR claude for claude: [dead] argbuf is scratch: set! extends, sorts and drains it
    // within one expansion (lines 1177-1182), so it is empty between updates. The
    // !self.argbuf.is_empty() guard in image_encode (line 1085) never fires and the
    // clear in sleep (line 1286) does nothing, and both suggest state that does not
    // exist. Make it a local SmallVec in set! and drop the field, the guard and the
    // clear. sort_by_key(|(n, _)| n.clone()) clones an ArcStr each time it computes a
    // key, while sort_unstable_by(|a, b| a.0.cmp(&b.0)) does not, and argument names
    // are unique. (sys-net-18)
    argbuf: SmallVec<[(ArcStr, Value); 6]>,
    ready: bool,
    current: Option<(Path, server::Proc)>,
    cast_typ: Option<Type>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> PublishRpc<R, E> {
    fn validate_spec(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        resolved: &FnType,
    ) -> Result<()> {
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
        for (name, field_typ, _) in spec_fields.iter() {
            deref_typ!("RpcArg {{default: 'a, doc: string}}", ctx, field_typ,
                Some(Type::Struct(inner)) => {
                    if inner.len() == 2 {
                        let has_default = inner.iter().any(|(n, _, _)| n.as_str() == "default");
                        let has_doc = inner.iter().any(|(n, _, _)| n.as_str() == "doc");
                        if has_default && has_doc { Ok(()) }
                        else { bail!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}") }
                    } else {
                        bail!("rpc #spec field '{name}' must be {{default: 'a, doc: string}}")
                    }
                }
            )?;
        }
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

        if spec_fields.len() != cb_fields.len() {
            bail!(
                "rpc #spec has {} fields but #f argument has {}",
                spec_fields.len(),
                cb_fields.len()
            )
        }
        for (spec_name, spec_field_typ, _) in spec_fields.iter() {
            // extract the value type T from {default: T, doc: string}
            let value_typ = deref_typ!(
                "{{default: 'a, doc: string}}", ctx, spec_field_typ,
                Some(Type::Struct(inner)) => {
                    match inner.iter().find(|(n, _, _)| n.as_str() == "default") {
                        Some((_, t, _)) => Ok(t.clone()),
                        None => bail!("rpc #spec field '{spec_name}' missing 'default'"),
                    }
                }
            )?;
            let cb_field = cb_fields.iter().find(|(n, _, _)| n == spec_name);
            match cb_field {
                None => bail!("rpc #f argument missing field '{spec_name}'"),
                Some((_, cb_typ, _)) => {
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
                            | Type::Error(_) | Type::Array(_) | Type::ByRef(..)
                            | Type::Tuple(_) | Type::Struct(_) | Type::Variant(_, _, _)
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
        match from {
            [_, _, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope = scope.append_block("fn", LambdaId::new().inner());
                let id = BindId::new();
                ctx.record_ref(id, top_id);
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
                let f =
                    genn::apply(fnode, scope, smallvec::smallvec![xn], &mftyp, top_id);
                let mut t = PublishRpc {
                    slept: false,
                    queue: VecDeque::new(),
                    x,
                    id,
                    top_id,
                    f,
                    pid,
                    argbuf: smallvec![],
                    ready: true,
                    current: None,
                    cast_typ: None,
                    out: TagValue::phantom(),
                };
                if let Some(resolved) = resolved {
                    let _ = t.validate_spec(ctx, resolved);
                }
                Ok(Box::new(t))
            }
            _ => bail!("expected four arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let slept = bool::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let f = image::decode_node(ctx, buf)?;
        let pid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        let ready = bool::decode(buf)?;
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(PublishRpc {
            slept,
            id,
            top_id,
            f,
            pid,
            x,
            queue: VecDeque::new(),
            argbuf: smallvec![],
            ready,
            current: None,
            cast_typ,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for PublishRpc<R, E> {
    /// `current` is a live procedure; `queue` holds calls awaiting a
    /// reply.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() || !self.queue.is_empty() || !self.argbuf.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.f.image_encode(buf)?;
        self.pid.encode(buf)?;
        self.x.encode(buf)?;
        self.ready.encode(buf)?;
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
        if f_fired && let Some(v) = fv {
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::fired(v));
        }
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
            self.current = None;
            if let (Some(Value::String(path)), Some(doc)) = (&pathv, &docv) {
                let path = Path::from(path);
                let spec = match &specv {
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
                let proc = match NetState::get(ctx).publish_rpc(
                    ctx,
                    path.clone(),
                    doc.clone(),
                    spec,
                    self.id,
                ) {
                    Ok(proc) => proc,
                    Err(e) => {
                        let e: ArcStr = format_compact!("{e:?}").as_str().into();
                        let e: Value = (literal!("PublishRpcError"), e).into();
                        return self.out.set(TagValue::fired(Value::Error(e.into())));
                    }
                };
                self.current = Some((path, proc));
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
                ctx.rt.store_insert(self.x, TagValue::fired(args.clone()));
                ctx.event.variables.insert(self.x, TagValue::fired(args));
            }};
        }
        if let Some(mut cbt) = ctx.event.take_custom(&self.id) {
            if let Some(c) = (&mut *cbt as &mut dyn Any).downcast_mut::<NetRpcCall>() {
                if let Some(c) = c.0.take() {
                    if crate::netstate::rpc_dbg() {
                        eprintln!(
                            "RPCDBG publish_rpc {:?}: call queued (ready={} qlen={})",
                            self.id,
                            self.ready,
                            self.queue.len()
                        );
                    }
                    self.queue.push_back(c);
                }
            }
        }
        // CR claude for eric: [bug] Calls are answered strictly in order, and only a
        // fire of `f` sets `ready` back (line 1219). So if `f` never answers one call,
        // every later call is blocked forever: they pile up in `queue` without bound,
        // and their callers hang because netidx's client call has no timeout. Any
        // client can cause this. An argument that fails the cast in `set!` puts an
        // InvalidCast error in `x`, and the handler's field read bottoms; a call that
        // omits an argument fails the same cast, because the spec's defaults are never
        // filled in. A handler that throws (rpc's type allows `throws 'e`) or bottoms
        // on one input (integer div0) wedges the server too. A call whose cast fails
        // should get the error as its reply instead of being dispatched, and one
        // unanswered call should not hold up the queue. probe:
        // design/review-2026-10-05/repro/sys-net-03.gx (sys-net-03)
        if self.ready && self.queue.len() > 0 {
            if let Some(c) = self.queue.front() {
                if crate::netstate::rpc_dbg() {
                    eprintln!("RPCDBG publish_rpc {:?}: dispatch", self.id);
                }
                set!(c)
            }
        }
        loop {
            match graphix_package_core::seam_tick(self.f.update(ctx)).map(|tv| tv.clone())
            {
                None => break self.out.ride(),
                Some(v) => {
                    self.ready = true;
                    if let Some(mut call) = self.queue.pop_front() {
                        if crate::netstate::rpc_dbg() {
                            eprintln!("RPCDBG publish_rpc {:?}: reply {v:?}", self.id);
                        }
                        call.reply.send(v.value());
                    }
                    match self.queue.front() {
                        Some(c) => {
                            if crate::netstate::rpc_dbg() {
                                eprintln!(
                                    "RPCDBG publish_rpc {:?}: dispatch next",
                                    self.id
                                );
                            }
                            set!(c)
                        }
                        None => break self.out.ride(),
                    }
                }
            }
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.f.typecheck0(ctx)?;
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.validate_spec(ctx, resolved)?;
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.f.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
        self.current = None;
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.rt.store_remove(&self.pid);
        self.f.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = true;
        self.out = TagValue::phantom();
        if crate::netstate::rpc_dbg() {
            eprintln!("RPCDBG publish_rpc {:?}: sleep (id re-minted)", self.id);
        }
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.current = None;
        self.queue.clear();
        self.argbuf.clear();
        self.ready = true;
        self.f.sleep(ctx);
    }
}
