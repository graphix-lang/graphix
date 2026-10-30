use crate::netstate::{NetRpcWake, NetState, NetWrite, Reader, RpcAnswer, RpcId};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Rt, Scope, TagValue,
    effects::Effect,
    env::Env,
    err, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::{WakeBit, coretraits, genn},
    rep,
    typ::{FnType, Naked, Type},
};
use graphix_package_core::{
    CastTarget, ErrorText, Handler, Inbox, Reply, RtVar, castable, seam_arg, seam_value,
    write_data,
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
use std::{any::Any, collections::VecDeque, fmt};
use triomphe::Arc as TArc;

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

/// An error a caller is answered with, as netidx carries one: its message
/// alone, which the caller reads as `RpcError(msg)` (`remote_err`).
fn remote_failure(msg: impl fmt::Display) -> Value {
    Value::Error(Value::String(format_compact!("{msg}").as_str().into()).into())
}

/// A refusal ([`Type::try_cast_value`]'s error) as the caller is answered.
fn refusal(e: &Value) -> Value {
    match e {
        Value::Error(payload) => remote_failure(ErrorText(payload)),
        e => remote_failure(Naked(e)),
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

/// Values that fire while no path is known, the most recent kept.
const WRITE_QUEUE: usize = 1024;

/// The value written to a path. Values that fire before the path is
/// known queue, the last [`WRITE_QUEUE`] of them, and are written in
/// order when it arrives.
#[derive(Debug)]
pub(crate) struct Write {
    dv: Either<(Path, Dval), VecDeque<Value>>,
    /// Set by `sleep`, taken by the next update: the target is found again
    /// from the present path.
    slept: WakeBit,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Write {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_write";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self::new()))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let _ = buf;
        Ok(Box::new(Self::new()))
    }
}

impl Write {
    fn new() -> Self {
        Self {
            dv: Either::Right(VecDeque::new()),
            slept: WakeBit::default(),
            out: TagValue::phantom(),
        }
    }

    fn same_path(&self, new_path: Option<&Path>) -> bool {
        match (new_path, &self.dv) {
            (Some(p0), Either::Left((p1, _))) => p0 == p1,
            _ => false,
        }
    }

    /// Drop the target, if there is one; what fires next queues.
    fn release<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if self.dv.is_left()
            && let Either::Left((_, dv)) =
                std::mem::replace(&mut self.dv, Either::Right(VecDeque::new()))
        {
            NetState::get(ctx).release(dv)
        }
    }
}

impl<R: Rt> Apply<R> for Write {
    /// A live subscription and queued values exist only once a cycle ran.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let _ = buf;
        match &self.dv {
            Either::Right(queued) if queued.is_empty() => Ok(()),
            _ => Err(PackError::Application(image::NOT_QUIESCENT)),
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let woke = self.slept.take();
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (val, val_fired) = seam_arg(ctx, &mut from[1]);
        // the standing value goes to a new path when nothing queued is newer
        let mut standing = val_fired;
        if (woke || path_fired)
            && let Some(path) = &path
            && let p = as_path(path.clone())
            && (woke || !self.same_path(p.as_ref()))
        {
            self.release(ctx);
            let Some(p) = p else {
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
                Either::Right(q) => {
                    if q.len() == WRITE_QUEUE {
                        q.pop_front();
                    }
                    q.push_back(val)
                }
                Either::Left((_, dv)) => {
                    dv.write(val);
                }
            }
        }
        self.out.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.release(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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
    cur: Option<(Path, Reader)>,
    var: RtVar,
    target: CastTarget,
    out: TagValue,
}

impl Subscribe {
    fn with(var: RtVar, target: CastTarget) -> Self {
        Self {
            slept: WakeBit::default(),
            cur: None,
            var,
            target,
            out: TagValue::phantom(),
        }
    }
}

impl<R: Rt> BuiltIn<R> for Subscribe {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_subscribe";
    const RESUMES: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let _ = from;
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(Self::with(var, CastTarget::of(&ctx.env, resolved))))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Box::new(Self::with(var, target)))
    }
}

impl<R: Rt> Apply<R> for Subscribe {
    /// `cur` is a live subscription.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.cur.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)?;
        self.target.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        static ERR_TAG: ArcStr = literal!("SubscribeError");
        let woke = self.slept.take();
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        match (path, path_fired || woke) {
            (_, false) | (None, true) => (),
            (Some(Value::String(path)), true) => {
                let path = Path::from(path);
                if self.cur.as_ref().is_none_or(|(p, _)| *p != path) {
                    if self.cur.take().is_some() {
                        self.var.remint(ctx);
                    }
                    if !Path::is_absolute(&path) {
                        return self.out.set(TagValue::fired(err!(
                            ERR_TAG,
                            "expected absolute path"
                        )));
                    }
                    let reader = match NetState::get(ctx).subscribe(
                        ctx,
                        UpdatesFlags::BEGIN_WITH_LAST,
                        path.clone(),
                        self.var.id,
                    ) {
                        Ok(reader) => reader,
                        Err(e) => {
                            return self
                                .out
                                .set(TagValue::fired(errf!(ERR_TAG, "{e:#}")));
                        }
                    };
                    self.cur = Some((path, reader));
                }
            }
            (Some(v), true) => {
                return self.out.set(TagValue::fired(errf!(
                    ERR_TAG,
                    "invalid path {v}, expected string"
                )));
            }
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
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.target.refresh(&ctx.env, resolved);
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx);
        self.cur = None;
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.out = TagValue::phantom();
        self.slept.set();
        self.cur = None;
        self.var.remint(ctx);
    }
}

graphix_package_core::abstract_wrapper!(
    RpcId,
    static RPC_ID_WRAPPER = "sys::net::RpcId"
);

fn rpc_id(v: &Value) -> Option<RpcId> {
    match v {
        Value::Abstract(a) => a.downcast_ref::<RpcId>().copied(),
        _ => None,
    }
}

/// `call_id(e)`: a fresh id at each fire of `e`.
#[derive(Debug)]
pub(crate) struct CallId {
    out: TagValue,
}

impl CallId {
    fn new() -> Self {
        Self { out: TagValue::phantom() }
    }
}

impl<R: Rt> BuiltIn<R> for CallId {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "sys_net_call_id";
    /// Ids order as they are minted.
    const ORDERED: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self::new()))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new()))
    }
}

impl<R: Rt> Apply<R> for CallId {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let tag = from[0].update(ctx).tag();
        if tag.is_bottom() {
            self.out.set_bottom_as(tag)
        } else if tag.is_fired() {
            self.out.set(TagValue::fired(RPC_ID_WRAPPER.wrap(RpcId::new())))
        } else {
            self.out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

/// `abort_call(id)`: the call in flight under `id`, if any, answers
/// `` `Aborted ``.
#[derive(Debug)]
pub(crate) struct AbortCall {
    out: TagValue,
}

impl AbortCall {
    fn new() -> Self {
        Self { out: TagValue::phantom() }
    }
}

impl<R: Rt> BuiltIn<R> for AbortCall {
    const EFFECT: Effect = Effect::Io(None);
    const NAME: &str = "sys_net_abort_call";
    /// Calls and aborts under one id decide each other's answers.
    const ORDERED: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self::new()))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new()))
    }
}

impl<R: Rt> Apply<R> for AbortCall {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let id = from[0].update(ctx);
        let tag = id.tag();
        let Some(id) = seam_value(id).map(|tv| tv.value_cloned()) else {
            return self.out.set_bottom_as(tag);
        };
        if tag.is_fired()
            && let Some(id) = rpc_id(&id)
            && let Some(net) = ctx.libstate.get::<NetState>()
        {
            net.abort_call(id)
        }
        self.out.set(TagValue::tagged(Value::Null, tag))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

/// The reply's target: the success type of the second element of
/// `call`'s `(RpcId, Result<'b, ..>)`.
fn reply_target(env: &Env, resolved: Option<&FnType>) -> CastTarget {
    let reply = resolved.and_then(|ft| {
        ft.rtype.with_deref(|t| match t? {
            Type::Tuple(elems) if elems.len() == 2 => Some(elems[1].clone()),
            _ => None,
        })
    });
    CastTarget::of_result(env, reply.as_ref())
}

/// The procedure and arguments of a call to `path` with `args`, a struct
/// or null.
fn rpc_request(path: &Value, args: &Value) -> Result<(Path, Vec<(ArcStr, Value)>)> {
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

/// What `call` and `callq` share: where a site's calls answer, and the
/// type their replies cast to.
#[derive(Debug)]
struct RpcSite {
    /// Reminted at sleep, so the answers of the calls the site abandons
    /// land nowhere.
    var: RtVar,
    target: CastTarget,
}

impl RpcSite {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.var.image_encode(buf)?;
        self.target.encode(buf)
    }

    fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let var = RtVar::image_decode(ctx, buf)?;
        let target = CastTarget::decode(buf)?;
        Ok(Self { var, target })
    }

    /// Call the procedure at `path` with `args` as `id`, answering here.
    fn issue<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        id: RpcId,
        path: &Value,
        args: &Value,
    ) {
        let request = write_data(ctx, args).and_then(|args| rpc_request(path, &args));
        NetState::get(ctx).call_rpc(ctx, self.var.id, id, request);
    }

    /// The call this cycle's answer settles and its result: the reply cast
    /// to the target or an `` `RpcError ``, `None` when the call was
    /// aborted or replaced first.
    fn answer<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) -> Option<(RpcId, Option<Value>)> {
        let answer: Box<dyn Any> = ctx.event.take_custom(&self.var.id)?;
        let answer = *answer.downcast::<RpcAnswer>().ok()?;
        let id = answer.id;
        let result = NetState::get(ctx).settle_call(answer).map(|reply| match reply {
            Value::Error(e) => remote_err(&literal!("RpcError"), &e),
            v => coretraits::with_display_hooks(ctx, |env| self.target.cast(env, v)),
        });
        Some((id, result))
    }

    /// Abandon the calls in flight here: none of them answers.
    fn sleep<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.abandon(ctx);
        self.var.remint(ctx);
    }

    fn delete<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.abandon(ctx);
        self.var.release(ctx);
    }

    fn abandon<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(net) = ctx.libstate.get::<NetState>() {
            net.abandon_calls(self.var.id);
        }
    }
}

/// `call(#id, path, args)`: each fire of an argument starts a call, as
/// `#id` or under a fresh id, and each call answers `(id, result)` once,
/// in the order the answers come, one a cycle.
#[derive(Debug)]
pub(crate) struct RpcCall {
    site: RpcSite,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for RpcCall {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_call";
    /// Calls and aborts under one id decide each other's answers.
    const ORDERED: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let var = RtVar::new(ctx, top_id);
        let target = reply_target(&ctx.env, resolved);
        Ok(Box::new(Self { site: RpcSite { var, target }, out: TagValue::phantom() }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let site = RpcSite::image_decode(ctx, buf)?;
        Ok(Box::new(Self { site, out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for RpcCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.site.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let (id, id_fired) = seam_arg(ctx, &mut from[0]);
        let (path, path_fired) = seam_arg(ctx, &mut from[1]);
        let (args, args_fired) = seam_arg(ctx, &mut from[2]);
        if (id_fired || path_fired || args_fired)
            && let (Some(id), Some(path), Some(args)) = (&id, &path, &args)
        {
            self.site.issue(ctx, rpc_id(id).unwrap_or_else(RpcId::new), path, args);
        }
        match self.site.answer(ctx) {
            Some((id, result)) => {
                let result = result.unwrap_or_else(|| {
                    Value::Error(
                        rep::make_variant(literal!("Aborted"), std::iter::empty()).into(),
                    )
                });
                let v = rep::tuple([RPC_ID_WRAPPER.wrap(id), result]);
                self.out.set(TagValue::fired(v))
            }
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.site.target = reply_target(&ctx.env, Some(resolved));
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.site.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.site.sleep(ctx);
    }

    /// Born again, the site holds no answer: its last one was the last
    /// run's, and the sleep before it abandoned the calls in flight.
    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.out = TagValue::phantom();
    }
}

/// `callq(path, args)`: each fire of an argument starts a call under an
/// id of its own, and each call answers once, in the order the calls were
/// made, one a cycle.
#[derive(Debug)]
pub(crate) struct RpcCallQ {
    site: RpcSite,
    /// The calls made here and not answered yet, in call order, each with
    /// its result once it came. A site mints its ids in call order, so
    /// this is sorted by id.
    calls: VecDeque<(RpcId, Option<Value>)>,
    out: TagValue,
}

impl RpcCallQ {
    fn new(site: RpcSite) -> Self {
        Self { site, calls: VecDeque::new(), out: TagValue::phantom() }
    }
}

impl<R: Rt> BuiltIn<R> for RpcCallQ {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_callq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let var = RtVar::new(ctx, top_id);
        let target = CastTarget::of(&ctx.env, resolved);
        Ok(Box::new(Self::new(RpcSite { var, target })))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new(RpcSite::image_decode(ctx, buf)?)))
    }
}

impl<R: Rt> Apply<R> for RpcCallQ {
    /// A call in flight exists only once a cycle ran.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.calls.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.site.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let (path, path_fired) = seam_arg(ctx, &mut from[0]);
        let (args, args_fired) = seam_arg(ctx, &mut from[1]);
        if (path_fired || args_fired)
            && let (Some(path), Some(args)) = (&path, &args)
        {
            let id = RpcId::new();
            self.site.issue(ctx, id, path, args);
            self.calls.push_back((id, None));
        }
        if let Some((id, result)) = self.site.answer(ctx)
            && let Ok(i) = self.calls.binary_search_by_key(&id, |(id, _)| *id)
        {
            match result {
                Some(v) => self.calls[i].1 = Some(v),
                None => drop(self.calls.remove(i)),
            }
        }
        // the wake a held answer asked for
        ctx.event.variables.remove(&self.site.var.id);
        let Some((_, Some(v))) = self.calls.pop_front_if(|(_, result)| result.is_some())
        else {
            return self.out.ride();
        };
        if let Some((_, Some(_))) = self.calls.front() {
            ctx.rt.set_var(self.site.var.id, Value::Null);
        }
        self.out.set(TagValue::fired(v))
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.site.target.refresh(&ctx.env, resolved);
        Ok(())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.site.delete(ctx);
    }

    /// The answers held behind an abandoned call go with it: a later one
    /// alone would break call order.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.site.sleep(ctx);
        self.calls.clear();
    }

    /// Born again, the site holds no answer and no call: both were the
    /// last run's.
    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.calls.clear();
        self.out = TagValue::phantom();
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
            fn with(var: RtVar) -> Self {
                Self {
                    slept: WakeBit::default(),
                    current: None,
                    var,
                    out: TagValue::phantom(),
                }
            }

            fn remint<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
                self.stop(ctx);
                self.var.remint(ctx);
            }

            /// Stop the listing in progress, if one is.
            fn stop<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
                if self.current.take().is_some() {
                    NetState::get(ctx).stop_list(self.var.id);
                }
            }

            fn list<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>, path: Path) {
                self.current = Some((path.clone(), Listing::InFlight));
                NetState::get(ctx).list(ctx, self.var.id, path, $table);
            }
        }

        impl<R: Rt> BuiltIn<R> for $name {
            const EFFECT: Effect = Effect::Async;
            const NAME: &str = $builtin;
            const RESUMES: bool = true;

            fn init<'a, 'b, 'c, 'd>(
                ctx: &'a mut CompileCtx<R>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R>],
                top_id: ExprId,
            ) -> Result<Box<dyn Apply<R>>> {
                let _ = from;
                let var = RtVar::new(ctx, top_id);
                Ok(Box::new(Self::with(var)))
            }

            fn image_decode(
                ctx: &mut ExecCtx<'_, R>,
                _from: &[Node<R>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R>>, PackError> {
                let var = RtVar::image_decode(ctx, buf)?;
                Ok(Box::new(Self::with(var)))
            }
        }

        impl<R: Rt> Apply<R> for $name {
            /// `current` is a list the resolver is serving.
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                if self.current.is_some() {
                    return Err(PackError::Application(image::NOT_QUIESCENT));
                }
                self.var.image_encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R>,
                from: &mut [Node<R>],
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
                    match &mut self.current {
                        Some((p, listing)) if *p == path => {
                            match (trigger_fired, *listing) {
                                (false, _) => (),
                                (true, Listing::Done) => self.list(ctx, path),
                                // a tick while a listing is in flight owes one
                                // more: a listing slower than the ticks still answers
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

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
                self.var.release(ctx);
                self.stop(ctx);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
                self.out = TagValue::phantom();
                self.slept.set();
                self.remint(ctx);
            }
        }
    };
}

list!(List, "sys_net_list", false);
list!(ListTable, "sys_net_list_table", true);

/// The handler's type: the function member of `#on_write`'s type.
fn on_write_ftype(typ: &FnType) -> Option<TArc<FnType>> {
    let member = |t: &Type| {
        t.with_deref(|t| match t {
            Some(Type::Fn(ft)) => Some(ft.clone()),
            _ => None,
        })
    };
    typ.args.first()?.typ.with_deref(|t| match t {
        Some(Type::Set(ts)) => ts.iter().find_map(member),
        Some(t) => member(t),
        None => None,
    })
}

/// What a write is cast to for the handler: its argument type, none where
/// the check settled it to ⊥ (no argument of the call is a handler).
fn write_type(env: &Env, resolved: Option<&FnType>) -> Option<Type> {
    let ft = on_write_ftype(resolved?)?;
    let t = &ft.args.first()?.typ;
    castable(env, t).then(|| t.clone())
}

#[derive(Debug)]
pub(crate) struct Publish<R: Rt> {
    /// Wake catch-up: `sleep()` unpublishes, so the first update after
    /// must republish from the present path/value.
    slept: WakeBit,
    current: Option<(Path, Val)>,
    top_id: ExprId,
    /// write requests on the published value arrive here as custom
    /// events (routed by the NetState pump)
    wid: BindId,
    on_write: genn::Callback<R>,
    /// None when no handler can arrive: the publication takes no writes.
    write_typ: Option<Type>,
    /// The handler is a function now, not null.
    handler: bool,
    out: TagValue,
}

impl<R: Rt> Publish<R> {
    fn with(
        top_id: ExprId,
        wid: BindId,
        on_write: genn::Callback<R>,
        write_typ: Option<Type>,
    ) -> Self {
        Self {
            slept: WakeBit::default(),
            current: None,
            top_id,
            wid,
            on_write,
            write_typ,
            handler: false,
            out: TagValue::phantom(),
        }
    }
}

impl<R: Rt> BuiltIn<R> for Publish<R> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_publish";
    const RESUMES: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        match from {
            [_, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope = scope.append_block("fn", LambdaId::new().inner());
                let Some(mftyp) = on_write_ftype(typ) else {
                    bail!("expected a function or null not {}", typ.args[0].typ)
                };
                let on_write = genn::Callback::new(ctx, &scope, &mftyp, &["x"], top_id)?;
                let wid = BindId::new();
                ctx.record_ref(wid, top_id);
                let write_typ = write_type(&ctx.env, resolved);
                Ok(Box::new(Self::with(top_id, wid, on_write, write_typ)))
            }
            _ => bail!("expected three arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let top_id = ExprId::decode(buf)?;
        let wid = BindId::decode(buf)?;
        let on_write = genn::Callback::image_decode(ctx, buf)?;
        let write_typ = Pack::decode(buf)?;
        ctx.record_ref(wid, top_id);
        Ok(Box::new(Self::with(top_id, wid, on_write, write_typ)))
    }
}

impl<R: Rt> Apply<R> for Publish<R> {
    /// `current` is a live publication.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.top_id.encode(buf)?;

        self.wid.encode(buf)?;
        self.on_write.image_encode(buf)?;
        self.write_typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        macro_rules! publish {
            ($path:expr, $v:expr) => {{
                let path = Path::from($path.clone());
                let net = NetState::get(ctx);
                let writes = self.write_typ.as_ref().map(|_| self.wid);
                match net.publish(ctx, path.clone(), $v.clone(), writes) {
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
            self.handler = !matches!(v, Value::Null);
            if self.handler {
                self.on_write.feed_fn(ctx, TagValue::fired(v));
            }
        }
        // a path that moved drops the publication even while the value is
        // bottom, so the next value goes to the new path
        if (path_fired || woke)
            && let Some(Value::String(path)) = &pathv
            && self.current.as_ref().is_some_and(|(p, _)| *p != Path::from(path.clone()))
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

                    // a write the handler cannot take is the writer's error
                    let taken = match &self.write_typ {
                        Some(typ) if self.handler => {
                            coretraits::with_display_hooks(ctx, |env| {
                                typ.try_cast_value(env, req.value.clone())
                            })
                            .map_err(|e| refusal(&e))
                        }
                        Some(_) | None => Err(remote_failure(format_args!(
                            "{path} has no write handler"
                        ))),
                    };
                    match taken {
                        Ok(v) => self.on_write.feed_arg(ctx, 0, TagValue::fired(v)),
                        Err(e) => {
                            if let Some(reply) = req.send_result.take() {
                                reply.send(e)
                            }
                        }
                    }
                }
            }
        }
        self.on_write.update(ctx);
        self.out.ride()
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        self.on_write.typecheck0(ctx)?;
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.write_typ = write_type(&ctx.env, Some(resolved));
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.on_write.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some((_, val)) = self.current.take() {
            NetState::get(ctx).unpublish(val);
        }
        ctx.release_var(self.wid, self.top_id);
        self.on_write.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

impl Reply for RpcReply {
    fn reply(mut self, v: Value) {
        self.0.reply.send(v)
    }

    /// The error is logged here and the caller told only that the call
    /// failed: an error's chain names the server's source.
    fn raised(self, e: Value) {
        log::error!(
            target: graphix_compiler::FAILURE_TARGET,
            "an rpc handler raised: {}",
            Naked(&e)
        );
        self.reply(remote_failure("the rpc failed"))
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

/// A published procedure and the path, doc and spec it was published from.
#[derive(Debug)]
struct Published {
    path: Path,
    doc: Value,
    spec: Value,
    /// Dropping it unpublishes the procedure.
    _proc: server::Proc,
}

impl Published {
    fn is(&self, path: &Value, doc: &Value, spec: &Value) -> bool {
        self.doc == *doc
            && self.spec == *spec
            && as_path(path.clone()).is_some_and(|p| p == self.path)
    }
}

#[derive(Debug)]
pub(crate) struct PublishRpc<R: Rt> {
    /// Wake catch-up: `sleep()` drops the published proc, so the first
    /// update after must republish from the present path/doc/spec.
    slept: WakeBit,
    var: RtVar,
    handler: Handler<R, RpcReply>,
    /// Calls posted under `id`, taken at each wake.
    inbox: std::sync::Arc<Inbox<server::RpcCall>>,
    current: Option<Published>,
    /// The handler's argument type, which a call's arguments are cast to.
    cast_typ: Option<Type>,
    /// The published spec's defaults, which a call's arguments override.
    defaults: Vec<(ArcStr, Value)>,
    /// `#max_inflight`: calls past it are answered an error; none is
    /// refused before the argument has arrived.
    max_inflight: usize,
    out: TagValue,
}

impl<R: Rt> PublishRpc<R> {
    fn with(var: RtVar, handler: Handler<R, RpcReply>, cast_typ: Option<Type>) -> Self {
        Self {
            slept: WakeBit::default(),
            var,
            handler,
            inbox: Default::default(),
            current: None,
            cast_typ,
            defaults: Vec::new(),
            max_inflight: usize::MAX,
            out: TagValue::phantom(),
        }
    }

    fn handler_arg(resolved: &FnType) -> Option<Type> {
        match &resolved.args.get(4)?.typ {
            Type::Fn(ft) => Some(ft.args.first()?.typ.clone()),
            _ => None,
        }
    }

    /// The call's arguments as the handler takes them: the spec's
    /// defaults under the arguments given, sorted into a struct and cast to
    /// its argument type; `Err` is the cast's refusal.
    fn arg(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        call: &server::RpcCall,
    ) -> Result<Value, Value> {
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
                coretraits::with_display_hooks(ctx, |env| typ.try_cast_value(env, args))
            }
            None => Ok(args),
        }
    }

    fn publish(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        path: &Value,
        doc: &Value,
        spec: &Value,
    ) -> Result<()> {
        let path = as_path(path.clone()).ok_or_else(|| anyhow!("invalid path {path}"))?;
        let parsed = parse_spec(&write_data(ctx, spec)?)?;
        self.defaults =
            parsed.iter().map(|a| (a.name.clone(), a.default_value.clone())).collect();
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
            parsed,
            self.var.id,
            self.inbox.clone(),
        )?;
        self.current =
            Some(Published { path, doc: doc.clone(), spec: spec.clone(), _proc: proc });
        Ok(())
    }
}

impl<R: Rt> BuiltIn<R> for PublishRpc<R> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_net_publish_rpc";
    const RESUMES: bool = true;

    fn init<'a, 'b, 'c>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let [_, _, _, _, _] = from else { bail!("expected five arguments") };
        let typ = resolved.unwrap_or(typ);
        let handler = Handler::new(ctx, &typ.args[4].typ, scope, top_id)?;
        let var = RtVar::new(ctx, top_id);
        let cast_typ = resolved.and_then(Self::handler_arg);
        Ok(Box::new(Self::with(var, handler, cast_typ)))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let var = RtVar::image_decode(ctx, buf)?;
        let handler = Handler::image_decode(ctx, buf)?;
        let cast_typ = Pack::decode(buf)?;
        Ok(Box::new(Self::with(var, handler, cast_typ)))
    }
}

impl<R: Rt> Apply<R> for PublishRpc<R> {
    /// `current` is a live procedure.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.current.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)?;
        self.handler.image_encode(buf)?;
        self.cast_typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let woke = self.slept.take();
        let (pathv, path_fired) = seam_arg(ctx, &mut from[0]);
        let (docv, doc_fired) = seam_arg(ctx, &mut from[1]);
        let (specv, spec_fired) = seam_arg(ctx, &mut from[2]);
        let (maxv, max_fired) = seam_arg(ctx, &mut from[3]);
        let (fv, f_fired) = seam_arg(ctx, &mut from[4]);
        self.handler.set_fn(ctx, fv, f_fired);
        let mut failed = None;
        if max_fired && let Some(Value::I64(n)) = maxv {
            match usize::try_from(n) {
                Ok(n) => self.max_inflight = n,
                Err(_) => {
                    failed = Some(errf!(
                        literal!("PublishRpcError"),
                        "max_inflight must be at least 0, got {n}"
                    ))
                }
            }
        }
        if path_fired || doc_fired || spec_fired || woke {
            match (&pathv, &docv, &specv) {
                (Some(path), Some(doc), Some(spec))
                    if self.current.as_ref().is_some_and(|p| p.is(path, doc, spec)) => {}
                (Some(path), Some(doc), Some(spec)) => {
                    if crate::netstate::rpc_dbg() {
                        eprintln!(
                            "RPCDBG publish_rpc {:?}: (re)publish changed={:?} had={:?}",
                            self.var.id,
                            [path_fired, doc_fired, spec_fired, f_fired],
                            self.current.as_ref().map(|p| &p.path)
                        );
                    }
                    self.current = None;
                    if let Err(e) = self.publish(ctx, path, doc, spec) {
                        failed = Some(errf!(literal!("PublishRpcError"), "{e:#}"));
                    }
                }
                _ => self.current = None,
            }
        }
        if let Some(cbt) = ctx.event.take_custom(&self.var.id)
            && (&*cbt as &dyn Any).is::<NetRpcWake>()
        {
            let mut calls: LPooled<Vec<server::RpcCall>> = LPooled::take();
            self.inbox.take(&mut *calls);
            for c in calls.drain(..) {
                if self.handler.in_flight() >= self.max_inflight {
                    let n = self.max_inflight;
                    let msg = format_args!("{n} calls are already in flight");
                    RpcReply(c).reply(remote_failure(msg));
                    continue;
                }
                match self.arg(ctx, &c) {
                    Err(e) => RpcReply(c).reply(refusal(&e)),
                    Ok(arg) => self.handler.push(arg, RpcReply(c)),
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
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        self.handler.typecheck0(ctx)
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = Self::handler_arg(resolved);
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.handler.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx);
        self.current = None;
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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
