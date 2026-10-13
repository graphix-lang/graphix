use anyhow::{Result, bail};
use graphix_compiler::{
    BindId, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue, TagView,
    UserEvent, branch,
    expr::ExprId,
    image::{self, ImageBuf},
    node::{ErrorRelay, genn},
    typ::{FnType, Type},
};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use triomphe::Arc;

/// Where a request's answer goes.
pub trait Reply: Send + 'static {
    fn reply(self, v: Value);

    /// The asker stopped waiting.
    fn abandoned(&self) -> bool {
        false
    }
}

impl Reply for tokio::sync::oneshot::Sender<Value> {
    fn reply(self, v: Value) {
        let _ = self.send(v);
    }

    fn abandoned(&self) -> bool {
        self.is_closed()
    }
}

/// Requests for a builtin posted from outside the runtime, all taken in
/// one cycle: the runtime delivers one event per id a cycle, so a request
/// sent as an event of its own would start a cycle after the one before.
/// A poster wakes the builtin only when no wake is on its way.
#[derive(Debug)]
pub struct Inbox<T> {
    queue: parking_lot::Mutex<Vec<T>>,
    woken: std::sync::atomic::AtomicBool,
}

impl<T> Default for Inbox<T> {
    fn default() -> Self {
        Self { queue: parking_lot::Mutex::new(Vec::new()), woken: Default::default() }
    }
}

impl<T> Inbox<T> {
    /// Post `t`: true when the builtin must be woken.
    pub fn post(&self, t: T) -> bool {
        use std::sync::atomic::Ordering;
        self.queue.lock().push(t);
        !self.woken.swap(true, Ordering::AcqRel)
    }

    /// Every request posted; the next post wakes the builtin again.
    pub fn take(&self, out: &mut impl Extend<T>) {
        use std::sync::atomic::Ordering;
        self.woken.store(false, Ordering::Release);
        out.extend(self.queue.lock().drain(..));
    }
}

/// A Graphix function answering requests for a builtin (an http server,
/// a published rpc): each request gets an instance of the function of
/// its own, answered by that instance's first fire, or by the error it
/// raised, which also goes on to the handler covering the builtin. A
/// request whose asker stopped waiting is dropped with its instance.
#[derive(Debug)]
pub struct Handler<R: Rt, E: UserEvent, Q> {
    /// The call the check types; requests run in instances like it.
    proto: Request<R, E>,
    /// The function's value.
    pid: BindId,
    ftyp: Arc<FnType>,
    scope: Scope,
    top_id: ExprId,
    /// Requests not yet given an instance.
    pending: Vec<(Value, Q)>,
    running: Vec<(Request<R, E>, Q)>,
}

/// One call of the function over its own request binding.
#[derive(Debug)]
// CR claude for claude: [structure/medium] Six builtins build "call a Graphix function
// from Rust" by hand: this Request, core Filter, array Group, opt OptHof/OrElse,
// queuefn and sys::net Publish. Each one binds args, references and applies the
// function, feeds fid and args every cycle, removes and unbinds at delete, and forwards
// refs/sleep/image. The copies have drifted. Filter, Group and opt's feed put the
// production's tag in the event, while this file and net put FIRED. OptHof refuses a
// non-quiescent image, while Filter encodes `pending` and Group encodes its queue.
// Input-bottom handling differs too (x-dup-03). Make this a public `Callback { call,
// fid, args }` with new, feed_fn, feed_arg (over the shared deliver of x-dup-06),
// update, delete, refs, sleep and an image codec. Each builtin then holds one and keeps
// only its own policy. (x-dup-04)
// 2026-10-13 claude: OptHof is gone (opt's unary HOFs are selects in opt.gx); the
// other five copies remain. Filter's CR (x-node-contract-06) asks for the same
// struct.
struct Request<R: Rt, E: UserEvent> {
    f: Node<R, E>,
    relay: ErrorRelay,
    x: BindId,
}

impl<R: Rt, E: UserEvent> Request<R, E> {
    /// A call of the function, the binding `pid`'s value, or the value `f`
    /// it holds now.
    fn new(
        ctx: &mut CompileCtx<R, E>,
        function: Result<BindId, Value>,
        ftyp: &Arc<FnType>,
        scope: &Scope,
        top_id: ExprId,
    ) -> Self {
        let (relay, scope) = ErrorRelay::new(ctx, scope, top_id);
        let (x, xn) =
            genn::bind(ctx, &scope.lexical, "x", ftyp.args[0].typ.clone(), top_id);
        let args = smallvec::smallvec![xn];
        let f = match function {
            Ok(pid) => {
                let fnode = genn::reference(ctx, pid, Type::Fn(ftyp.clone()), top_id);
                genn::apply(fnode, scope, args, ftyp, top_id)
            }
            Err(f) => genn::apply_value(f, scope, args, ftyp, top_id),
        };
        Self { f, relay, x }
    }

    /// The answer this cycle: the call's first fire, or what it raised.
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Option<Value> {
        let out = match self.f.update(ctx).view() {
            TagView::Fired(tv) => Some(tv.value_cloned()),
            _ => None,
        };
        let raised = self.relay.update(ctx);
        out.or(raised)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.f.delete(ctx);
        self.relay.delete(ctx);
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
    }
}

fn call_of<R: Rt, E: UserEvent, Q>(running: &mut (Request<R, E>, Q)) -> &mut Node<R, E> {
    &mut running.0.f
}

impl<R: Rt, E: UserEvent, Q: Reply> Handler<R, E, Q> {
    /// A handler of type `ftyp` (a one-argument function type).
    pub fn new(
        ctx: &mut CompileCtx<R, E>,
        ftyp: &Type,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Self> {
        let ftyp = match ftyp {
            Type::Fn(ft) if ft.args.len() == 1 => ft.clone(),
            t => bail!("expected a function of one argument not {t}"),
        };
        let scope = scope.append_block("fn", LambdaId::new().inner());
        let pid = BindId::new();
        let proto = Request::new(ctx, Ok(pid), &ftyp, &scope, top_id);
        Ok(Self {
            proto,
            pid,
            ftyp,
            scope,
            top_id,
            pending: Vec::new(),
            running: Vec::new(),
        })
    }

    /// Take the function's value when its argument fires.
    pub fn set_fn(&mut self, ctx: &mut ExecCtx<'_, R, E>, f: Option<Value>, fired: bool) {
        if fired && let Some(v) = f {
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::fired(v));
        }
    }

    pub fn push(&mut self, request: Value, reply: Q) {
        self.pending.push((request, reply));
    }

    /// Start an instance of the function for each new request, then
    /// answer each request whose instance fired or raised. The instances
    /// run on branches of their own where they are independent.
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if !self.pending.is_empty()
            && let Some(function) = ctx.rt.store_value(&self.pid)
        {
            for (req, q) in self.pending.drain(..) {
                let r = Request::new(
                    ctx,
                    Err(function.clone()),
                    &self.ftyp,
                    &self.scope,
                    self.top_id,
                );
                ctx.rt.store_insert(r.x, TagValue::fired(req.clone()));
                ctx.event.variables.insert(r.x, TagValue::fired(req));
                self.running.push((r, q));
            }
        }
        // None: still running; Some(None): its asker stopped waiting
        let answers = branch::fork_instances(
            ctx,
            &mut self.running,
            call_of,
            |ctx, (r, q)| match q.abandoned() {
                true => Some(None),
                false => r.update(ctx).map(Some),
            },
        );
        for (i, answer) in answers.into_iter().enumerate().rev() {
            let Some(answer) = answer else { continue };
            let (mut r, q) = self.running.swap_remove(i);
            r.delete(ctx);
            if let Some(v) = answer {
                q.reply(v)
            }
        }
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.proto.f.typecheck0(ctx)
    }

    pub fn refs(&self, refs: &mut Refs) {
        self.proto.f.refs(refs)
    }

    /// Requests pending are dropped unanswered.
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.pending.clear();
        for (mut r, _) in self.running.drain(..) {
            r.delete(ctx)
        }
    }

    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.sleep(ctx);
        self.proto.delete(ctx);
        ctx.rt.store_remove(&self.pid);
    }

    /// Requests exist only once a cycle has run.
    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.pending.is_empty() || !self.running.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.proto.f.image_encode(buf)?;
        self.proto.relay.image_encode(buf)?;
        self.proto.x.encode(buf)?;
        self.pid.encode(buf)?;
        self.ftyp.encode(buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.top_id.encode(buf)
    }

    pub fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let f = image::decode_node(ctx, buf)?;
        let relay = ErrorRelay::image_decode(ctx, buf)?;
        let x = BindId::decode(buf)?;
        let pid = BindId::decode(buf)?;
        let ftyp = Arc::new(FnType::decode(buf)?);
        let scope = image::scope_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        Ok(Self {
            proto: Request { f, relay, x },
            pid,
            ftyp,
            scope,
            top_id,
            pending: Vec::new(),
            running: Vec::new(),
        })
    }
}
