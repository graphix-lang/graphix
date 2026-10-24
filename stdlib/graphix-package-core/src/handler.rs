use anyhow::{Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue, TagView,
    UserEvent, branch,
    cost::Siblings,
    errf,
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

    /// Answer with the error the request's instance raised; the default
    /// answers with the error itself.
    fn raised(self, e: Value)
    where
        Self: Sized,
    {
        self.reply(e)
    }

    /// The asker stopped waiting.
    fn abandoned(&self) -> bool {
        false
    }

    /// Whether the answer leaves as data ([`crate::write_data`]), and the
    /// tag of the error answered when it is none.
    fn data(&self) -> Option<ArcStr> {
        None
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
    /// Whether the running instances may run apart, once decided; a new
    /// function decides again.
    siblings: Siblings,
}

/// How a request's instance answered.
enum Answer {
    Fired(Value),
    Raised(Value),
}

/// One call of the function over its own request binding.
#[derive(Debug)]
struct Request<R: Rt, E: UserEvent> {
    call: genn::Callback<R, E>,
    relay: ErrorRelay,
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
        let call = match function {
            Ok(pid) => genn::Callback::through(ctx, &scope, ftyp, &["x"], pid, top_id),
            Err(f) => genn::Callback::of_value(ctx, &scope, ftyp, &["x"], f, top_id),
        }
        .expect("Handler::new held the function to one argument");
        Self { call, relay }
    }

    /// The request's binding.
    fn x(&self) -> BindId {
        self.call.args()[0]
    }

    /// The answer this cycle: the call's first fire, or what it raised.
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Option<Answer> {
        let out = match self.call.update(ctx).view() {
            TagView::Fired(tv) => Some(Answer::Fired(tv.value_cloned())),
            _ => None,
        };
        let raised = self.relay.update(ctx).map(Answer::Raised);
        out.or(raised)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.delete(ctx);
        self.relay.delete(ctx);
    }
}

fn call_of<R: Rt, E: UserEvent, Q>(running: &mut (Request<R, E>, Q)) -> &mut Node<R, E> {
    running.0.call.call_mut()
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
            siblings: Siblings::default(),
        })
    }

    /// Take the function's value when its argument fires.
    pub fn set_fn(&mut self, ctx: &mut ExecCtx<'_, R, E>, f: Option<Value>, fired: bool) {
        if fired && let Some(v) = f {
            ctx.deliver(self.pid, TagValue::fired(v));
            self.siblings = Siblings::default();
        }
    }

    pub fn push(&mut self, request: Value, reply: Q) {
        self.pending.push((request, reply));
    }

    /// The requests taken and not yet answered.
    pub fn in_flight(&self) -> usize {
        self.pending.len() + self.running.len()
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
                ctx.deliver(r.x(), TagValue::fired(req));
                self.running.push((r, q));
            }
        }
        // None: still running; Some(None): its asker stopped waiting
        let answers = branch::fork_instances(
            ctx,
            &mut self.siblings,
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
            match answer {
                None => (),
                Some(Answer::Raised(e)) => q.raised(e),
                Some(Answer::Fired(v)) => {
                    let v = match q.data() {
                        None => v,
                        Some(tag) => crate::write_data(ctx, &v)
                            .unwrap_or_else(|e| errf!(tag, "{e:#}")),
                    };
                    q.reply(v)
                }
            }
        }
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.proto.call.typecheck0(ctx)
    }

    pub fn refs(&self, refs: &mut Refs) {
        self.proto.call.refs(refs)
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
        self.proto.call.image_encode(buf)?;
        self.proto.relay.image_encode(buf)?;
        self.pid.encode(buf)?;
        self.ftyp.encode(buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.top_id.encode(buf)
    }

    pub fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let call = genn::Callback::image_decode(ctx, buf)?;
        let relay = ErrorRelay::image_decode(ctx, buf)?;
        let pid = BindId::decode(buf)?;
        let ftyp = Arc::new(FnType::decode(buf)?);
        let scope = image::scope_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        Ok(Self {
            proto: Request { call, relay },
            pid,
            ftyp,
            scope,
            top_id,
            pending: Vec::new(),
            running: Vec::new(),
            siblings: Siblings::default(),
        })
    }
}
