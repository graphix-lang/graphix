use anyhow::{Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue, TagView,
    UserEvent, errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::Type,
};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use std::collections::VecDeque;

/// Where a request's answer goes.
pub trait Reply: Send + 'static {
    fn reply(self, v: Value);
}

impl Reply for tokio::sync::oneshot::Sender<Value> {
    fn reply(self, v: Value) {
        let _ = self.send(v);
    }
}

/// A Graphix function answering requests for a builtin (an http server,
/// a published rpc): one instance of the function, the requests queued
/// and handed to it one at a time, each answered by its next fire. A
/// request it answers with a fresh bottom (it raised, or has no value for
/// that request) is answered with an error, so it never holds up the rest.
#[derive(Debug)]
pub struct Handler<R: Rt, E: UserEvent, Q> {
    f: Node<R, E>,
    /// The function's value.
    pid: BindId,
    /// The request the function is answering.
    x: BindId,
    /// The request at the front is the function's while `busy`.
    queue: VecDeque<(Value, Q)>,
    busy: bool,
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
        let (x, xn) =
            genn::bind(ctx, &scope.lexical, "x", ftyp.args[0].typ.clone(), top_id);
        let pid = BindId::new();
        let fnode = genn::reference(ctx, pid, Type::Fn(ftyp.clone()), top_id);
        let f = genn::apply(fnode, scope, smallvec::smallvec![xn], &ftyp, top_id);
        Ok(Self { f, pid, x, queue: VecDeque::new(), busy: false })
    }

    /// Take the function's value when its argument fires.
    pub fn set_fn(&mut self, ctx: &mut ExecCtx<'_, R, E>, f: Option<Value>, fired: bool) {
        if fired && let Some(v) = f {
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::fired(v));
        }
    }

    pub fn push(&mut self, request: Value, reply: Q) {
        self.queue.push_back((request, reply));
    }

    fn dispatch(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some((req, _)) = self.queue.front()
            && !self.busy
        {
            self.busy = true;
            ctx.rt.store_insert(self.x, TagValue::fired(req.clone()));
            ctx.event.variables.insert(self.x, TagValue::fired(req.clone()));
        }
    }

    /// Hand the next request to the function and answer what it answers.
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.dispatch(ctx);
        // CR claude for eric: [bug] HttpServe answers the oldest queued request with
        // whatever its one shared handler instance fires next, so replies cross between
        // clients. After a reply this loop writes the next request into x and updates
        // the handler again in the same cycle. A `let` in the handler body still reads
        // as fired in ctx.event.variables there, so every concurrently queued client
        // gets the first client's response, and the real answers later reach an empty
        // queue and are dropped (a seqq handler and a sys::fs::read_all file server
        // fail the same way). With strictly sequential requests, any fire the new
        // request did not cause (a second async value, a `hits <- req ~ hits + 1`
        // write, a timer) is sent as the reply with the previous request's data, and
        // `ready` serializes all requests server-wide. PublishRpc in
        // stdlib/graphix-package-sys/src/net.rs:1214 has the same loop and leaks the
        // same way. probe: design/review-2026-10-05/repro/http-sqlite-db1-01.gx
        // (http-sqlite-db1-01)
        // 2026-10-07 claude: moved with the loop from HttpServe::update, which
        // PublishRpc now shares through this Handler; the pairing is unchanged.
        loop {
            let answer = match self.f.update(ctx).view() {
                TagView::Fired(tv) => tv.value_cloned(),
                // XCR claude for claude: [bug] `ready` is cleared when a request goes to the
                // handler and set again only when the handler's output fires (871). Some
                // handlers never fire for a request: one that raises with `?` (the error goes
                // to the serve site's catch through `throws 'e`), or one that is bottom for it
                // (`req.body$` on a GET). That client then gets no reply, no 500 and no
                // timeout. Every later request from any client waits behind it forever, and
                // `queue` grows by one per request. One malformed body sent to a
                // `json::read(req.body$)?` handler stops the server for good. A request the
                // handler raised on needs an answer (a 500), and one unanswered request should
                // not hold up the others. probe:
                // design/review-2026-10-05/repro/http-sqlite-db1-02.gx (http-sqlite-db1-02)
                // 2026-10-07 claude: a fresh bottom from the handler while a request is its
                // answers that request with a HandlerError (http: 500), which is how a raise or a
                // `$` on a missing value shows here; a fresh bottom some other input causes while a
                // request waits answers it too (the pairing of db1-01). Pin:
                // lib_tests http_bottom_handler_then_good; the repro answers 500 then "200 hi bob".
                TagView::FreshBottom if self.busy => {
                    errf!("HandlerError", "the handler has no value for this request")
                }
                _ => break,
            };
            self.busy = false;
            match self.queue.pop_front() {
                Some((_, reply)) => reply.reply(answer),
                None => break,
            }
            if self.queue.is_empty() {
                break;
            }
            self.dispatch(ctx);
        }
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.f.typecheck0(ctx)
    }

    pub fn refs(&self, refs: &mut Refs) {
        self.f.refs(refs)
    }

    /// Requests pending are dropped unanswered.
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.queue.clear();
        self.busy = false;
        self.f.sleep(ctx);
    }

    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.rt.store_remove(&self.pid);
        self.f.delete(ctx);
    }

    /// Pending requests exist only once a cycle has run.
    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queue.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.f.image_encode(buf)?;
        self.pid.encode(buf)?;
        self.x.encode(buf)
    }

    pub fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let f = image::decode_node(ctx, buf)?;
        let pid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        Ok(Self { f, pid, x, queue: VecDeque::new(), busy: false })
    }
}
