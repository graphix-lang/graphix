use crate::netstate::{NetRpcWake, NetState, NetWrite};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, FAILURE_TARGET, LambdaId, Node, Rt,
    Scope, TagValue, UserEvent,
    effects::Effect,
    err, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::{WakeBit, coretraits, genn},
    rep,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CastTarget, ErrorText, Handler, Inbox, Reply, RtVar, castable, seam_arg, write_data,
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
    slept: WakeBit,
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
            slept: WakeBit::default(),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = buf;
        Ok(Box::new(Write {
            dv: Either::Right(vec![]),
            slept: WakeBit::default(),
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
    /// A live subscription and queued values exist only once a cycle ran.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let _ = buf;
        match &self.dv {
            Either::Right(queued) if queued.is_empty() => Ok(()),
            _ => Err(PackError::Application(image::NOT_QUIESCENT)),
        }
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = self.slept.take();
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
        self.slept.set();
        self.release(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct Subscribe {
    /// `sleep()` tears the subscription down, so the first update after
    /// must re-establish it from the PRESENT path.
    slept: WakeBit,
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
            slept: WakeBit::default(),
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
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Box::new(Subscribe {
            slept: WakeBit::default(),
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
        self.var.image_encode(buf)?;
        self.target.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        static ERR_TAG: ArcStr = literal!("SubscribeError");
        let woke = self.slept.take();
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
        self.out = TagValue::phantom();
        self.slept.set();
        if let Some((_, dv)) = self.cur.take() {
            NetState::get(ctx).unsubscribe(dv, self.var.id);
        }
        self.var.remint(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct RpcCall {
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
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Box::new(RpcCall { var, target, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for RpcCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
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
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (args, args_fired) = seam_arg(ctx, &mut from[1]);
        if (path_fired || args_fired)
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
        self.remint(ctx);
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

/// Where a listing stands: answered, in flight, or in flight with a tick
/// that came meanwhile owed another.
#[derive(Debug, Clone, Copy)]
enum Listing {
    Done,
    InFlight,
    Owed,
}

macro_rules! list {
    ($name:ident, $builtin:literal, $table:expr) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            slept: WakeBit,
            /// The path listed, and where its listing stands.
            current: Option<(Path, Listing)>,
            var: RtVar,
            out: TagValue,
        }

        impl $name {
            fn remint<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.stop(ctx);
                self.var.remint(ctx);
            }

            /// Stop the listing in progress, if one is.
            fn stop<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                if self.current.take().is_some() {
                    NetState::get(ctx).stop_list(self.var.id);
                }
            }

            fn list<R: Rt, E: UserEvent>(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                path: Path,
            ) {
                self.current = Some((path.clone(), Listing::InFlight));
                NetState::get(ctx).list(ctx, self.var.id, path, $table);
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
                    slept: WakeBit::default(),
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
                let var = RtVar::image_decode(ctx, buf)?;
                Ok(Box::new($name {
                    slept: WakeBit::default(),
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
                self.var.image_encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &mut [Node<R, E>],
            ) -> &TagValue {
                let woke = self.slept.take();
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
                    // XCR claude for claude: [bug/medium] Each poll tick with an
                    // unchanged path reissues the listing under the same id, and
                    // NetState::spawn_request's replacement aborts the one in flight. A
                    // listing slower than its #update period (default 1 s) therefore
                    // never answers. With the resolver 0.3 s away each way, the
                    // default-polled list answered 0 times in 11 s while the same list
                    // on a 30 s trigger answered. Abort only on a path change, sleep or
                    // delete. On a tick while a listing is in flight, let it finish and
                    // owe at most one more listing (or drop the tick). probe:
                    // design/review-2026-10-19/repro/sys-net-list-poll-starves.sh
                    // (sys-net-02)
                    // a tick while a listing is in flight owes one more: a
                    // listing slower than the ticks still answers
                    // 2026-10-19 claude: A listing in flight is no longer replaced by a
                    // tick on the same path: each site keeps a Listing (Done, InFlight,
                    // Owed) beside its path; a tick while one is in flight owes one
                    // more, issued when the answer arrives; a path change, sleep or
                    // delete still aborts. No pin: needs a resolver slower than the
                    // tick
                    // (design/review-2026-10-19/repro/sys-net-list-poll-starves.sh);
                    // net's list tests pass.
                    // 2026-10-20 claude: XCR review: The new Listing state goes back to
                    // Done only when list_response returns Some, but NetState::list
                    // answers Value::Null when the resolver's ChangeTracker reports no
                    // change (netstate.rs, check_changed Ok(false)), and list_response
                    // maps Null to None. So the first poll that finds nothing changed
                    // leaves the site InFlight forever: every later tick only sets
                    // Owed, nothing ever issues the owed listing, and the list stops
                    // polling. Ran with the current quick binary: publish /local/a,
                    // then /local/b at 700 ms, list(#update: 100 ms timer, '/local')
                    // prints 1 and never 2. A single trigger at 1 s does print 2.
                    // Before this change every tick reissued the listing, so a Null
                    // answer was harmless. Fix: treat any delivery on the var, Null
                    // included, as the end of the listing in flight (Done, or issue the
                    // owed one), and pin a polled list that picks up a path published
                    // after an unchanged poll.
                    // 2026-10-20 claude: Any delivery on the listing's var now ends the
                    // listing in flight, an unchanged poll's Null included: the Null
                    // sets Done (or issues the owed listing) and answers nothing, so
                    // the next tick lists again. Pin:
                    // lib_tests::net::net_list_polled_sees_a_later_path (a path
                    // published at 700 ms under a list polled every 100 ms; it answers
                    // 1 instead of 2 with the fix undone, all four modes).
                    match &mut self.current {
                        Some((p, listing)) if *p == path => {
                            match (trigger_fired, *listing) {
                                (false, _) => (),
                                (true, Listing::Done) => self.list(ctx, path),
                                (true, Listing::InFlight | Listing::Owed) => {
                                    *listing = Listing::Owed
                                }
                            }
                        }
                        Some(_) | None => {
                            self.remint(ctx);
                            self.list(ctx, path)
                        }
                    }
                }
                let answer = ctx.event.variables.get(&self.var.id);
                let res = answer.and_then(|v| v.with_value(list_response));
                if answer.is_some() {
                    match &mut self.current {
                        Some((path, Listing::Owed)) => {
                            let path = path.clone();
                            self.list(ctx, path)
                        }
                        Some((_, listing)) => *listing = Listing::Done,
                        None => (),
                    }
                }
                match res {
                    Some(v) => self.out.set(TagValue::fired(v)),
                    None => self.out.ride(),
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.var.release(ctx);
                self.stop(ctx);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.out = TagValue::phantom();
                self.slept.set();
                self.remint(ctx);
            }
        }
    };
}

list!(List, "sys_net_list", false);
list!(ListTable, "sys_net_list_table", true);

/// What a write is cast to for the handler: its argument type, `Any` where
/// nothing constrains it (open, or settled to ⊥: the handler takes data
/// as it comes), none where only a part is open (no cast makes that form).
fn extract_publish_cast_type(resolved: Option<&FnType>) -> Option<Type> {
    resolved?.args.first().and_then(|a| match &a.typ {
        Type::Fn(cb_ft) if !cb_ft.args.is_empty() => {
            let t = &cb_ft.args[0].typ;
            match t.with_deref(|t| matches!(t, None | Some(Type::Bottom))) {
                true => Some(Type::Any),
                false => castable(t).then(|| t.clone()),
            }
        }
        _ => None,
    })
}

#[derive(Debug)]
pub(crate) struct Publish<R: Rt, E: UserEvent> {
    /// Wake catch-up: `sleep()` unpublishes, so the first update after
    /// must republish from the present path/value.
    slept: WakeBit,
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
                    slept: WakeBit::default(),
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
        let top_id = ExprId::decode(buf)?;
        let wid = BindId::decode(buf)?;
        let on_write = genn::Callback::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        ctx.record_ref(wid, top_id);
        Ok(Box::new(Publish {
            slept: WakeBit::default(),
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
        let woke = self.slept.take();
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
        if let Some((path, _)) = &self.current {
            if let Some(mut cbt) = ctx.event.take_custom(&self.wid) {
                if let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<NetWrite>() {
                    let req = &mut w.0;
                    // the check holds the handler's argument concrete: a
                    // type it could not settle takes no write
                    let v = match &self.cast_typ {
                        Some(typ) => coretraits::with_display_hooks(ctx, |env| {
                            typ.cast_value(env, req.value.clone())
                        }),
                        // CR claude for eric: [bug/medium] A remote write reaches the
                        // handler raw whenever its argument type is not concrete,
                        // because #on_write's 'a has no bound (subscribe's target has
                        // `'a: Concrete`). A handler taking a function, a reference or
                        // an open type therefore takes whatever any netidx client
                        // writes. A non-function written to `|f: fn(x: i64) -> i64|
                        // f(1)` hits CallSite's `value .. is not a function` panic and
                        // kills the runtime ('graphix runtime is dead'). Other shapes
                        // bottom silently. Decide the rule: bound on_write's argument
                        // `'a: Concrete` so every write is cast, or refuse a write here
                        // when cast_typ is None and the type is not Any. probe:
                        // design/review-2026-10-19/repro/x-panics-on-write-uncast.gx
                        // (x-panics-02)
                        // 2026-10-19 claude: The bound is in: net.gx/net.gxi declare
                        // publish's on_write argument 'a: Concrete + Data and rpc's
                        // 'args: Concrete + Data (the for-claude CRs
                        // x-builtin-effects-02, gx-stdlib-01, sys-net-03 and
                        // tests-lib-b2-01 asked for it), so the check refuses a
                        // function, reference or open handler type, and Publish::update
                        // now answers the writer with InvalidCast where cast_typ is
                        // None instead of feeding the raw value. If that matches the
                        // rule you want, delete this CR.
                        // 2026-10-19 claude: Correction to the note above: publish's
                        // on_write 'a is bounded Data only (Concrete refused the
                        // omitted default, which leaves 'a open); an open handler type
                        // takes no write, the writer is answered InvalidCast.
                        None => {
                            log::error!(
                                target: FAILURE_TARGET,
                                "a write of {} to {path} was refused: the write \
                                 handler's argument type is not known; annotate it",
                                req.value
                            );
                            errf!(
                                literal!("InvalidCast"),
                                "the write handler's argument type is not known"
                            )
                        }
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
        self.out = TagValue::phantom();
        self.slept.set();
        if let Some((_, val)) = self.current.take() {
            NetState::get(ctx).unpublish(val);
        }
        self.on_write.sleep(ctx);
    }
}

/// A call's reply.
#[derive(Debug)]
struct RpcReply(server::RpcCall);

// CR claude for eric: [risk/medium] A published rpc keeps one handler instance for
// every call f has not answered, with no limit and no way to drop one. RpcReply does
// not implement Reply::abandoned, and Inbox and Handler's pending/running lists are
// unbounded Vecs. The Inbox wakes the runtime outside the 100-deep rpcs channel, so
// callers get no backpressure. Any netidx client allowed to write the rpc path can grow
// the server's memory at will, even with writes that carry no reply channel and that
// nobody waits for. In the probe, 1000 writes/s to an rpc whose f answers later grow
// RSS ~3.5 MB/s (87 to 180 MB in 25 s); the same program answering at once stays at 72
// MB. http::serve bounds the same exposure with max_connections. Decide the policy: a
// #max_inflight that answers an RpcError past it, and dropping a call whose RpcCall has
// no reply channel or whose client is gone (needs an accessor on netidx_protocols'
// RpcReply). probe: design/review-2026-10-19/repro/x-security-rpc-unbounded.sh
// (x-security-02)
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
    slept: WakeBit,
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
                bail!("rpc #spec does not fit #f's argument {typ}: {}", ErrorText(&e))
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
            slept: WakeBit::default(),
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
        let var = RtVar::image_decode(ctx, buf)?;
        let handler = Handler::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        Ok(Box::new(PublishRpc {
            slept: WakeBit::default(),
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
        self.var.image_encode(buf)?;
        self.handler.image_encode(buf)?;
        self.cast_typ.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = self.slept.take();
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
        self.out = TagValue::phantom();
        self.slept.set();
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
