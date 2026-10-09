use anyhow::{Result, bail};
use graphix_compiler::{
    BindId, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue, TagView,
    UserEvent,
    expr::ExprId,
    image::{self, ImageBuf},
    node::{ErrorRelay, genn},
    typ::Type,
};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use std::collections::VecDeque;

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

/// A Graphix function answering requests for a builtin (an http server,
/// a published rpc): one instance of the function, the requests queued
/// and handed to it one at a time, each answered by its next fire. A
/// request it raised on is answered with the error, which also goes on to
/// the handler covering the builtin; one whose asker stopped waiting is
/// dropped.
#[derive(Debug)]
pub struct Handler<R: Rt, E: UserEvent, Q> {
    f: Node<R, E>,
    relay: ErrorRelay,
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
        let (relay, scope) = ErrorRelay::new(ctx, &scope, top_id);
        let (x, xn) =
            genn::bind(ctx, &scope.lexical, "x", ftyp.args[0].typ.clone(), top_id);
        let pid = BindId::new();
        let fnode = genn::reference(ctx, pid, Type::Fn(ftyp.clone()), top_id);
        let f = genn::apply(fnode, scope, smallvec::smallvec![xn], &ftyp, top_id);
        Ok(Self { f, relay, pid, x, queue: VecDeque::new(), busy: false })
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
        if self.busy && self.queue.front().is_some_and(|(_, q)| q.abandoned()) {
            self.queue.pop_front();
            self.busy = false;
        }
        self.queue.retain(|(_, q)| !q.abandoned());
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
            let out = match self.f.update(ctx).view() {
                TagView::Fired(tv) => Some(tv.value_cloned()),
                _ => None,
            };
            let answer = match (out, self.relay.update(ctx)) {
                (Some(v), _) => v,
                (None, Some(e)) if self.busy => e,
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
                // 2026-10-09 reviewer: the fix answers 500 to every handler whose reply mixes
                // the request with an async value. At dispatch `req.method` fires while the
                // async value is still bottom, so the reply struct is FreshBottom on the
                // dispatch cycle and the request gets the HandlerError before the value lands.
                // Probe (quick build, --no-cache, fusion on and off):
                // `|req| { let page = sys::time::after_idle(duration:100.ms, "p [req.path]");
                // { body: "[req.method] [page]", .. } }` answers
                // `500 ["HandlerError", "the handler has no value for this request"]`; inlining
                // the after_idle in the body string does the same; `body: page` alone answers
                // 200. A fresh bottom is not "no value for this request" while an async part
                // is pending; the wedge needs another signal (the raise itself, or a timeout).
                // Pin to add: a handler of that shape answering 200.
                // 2026-10-09 claude: a raise inside the handler now reaches an ErrorRelay
                // (node/error.rs) that Handler installs over it: the request is answered
                // with the error (http: 500) and the error goes on to the catch covering
                // serve, or is logged. A fresh bottom no longer answers anything, so a
                // handler waiting on an async value answers when it arrives. A request
                // that is bottom for good (`req.body$` on a GET) stays unanswered until
                // its client gives up; a closed oneshot (Reply::abandoned) then drops it
                // from the queue. rpc replies cannot tell, so an rpc call stays queued
                // (sys-net-03). Pins: lib_tests http_raising_handler_then_good,
                // http_async_handler_answers, http_abandoned_request_then_good (fails
                // with abandoned() false).
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
        self.relay.give_up_in_flight();
        self.f.sleep(ctx);
    }

    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.rt.store_remove(&self.pid);
        self.relay.delete(ctx);
        self.f.delete(ctx);
    }

    /// Pending requests exist only once a cycle has run.
    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queue.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.f.image_encode(buf)?;
        self.relay.image_encode(buf)?;
        self.pid.encode(buf)?;
        self.x.encode(buf)
    }

    pub fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let f = image::decode_node(ctx, buf)?;
        let relay = ErrorRelay::image_decode(ctx, buf)?;
        let pid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        Ok(Self { f, relay, pid, x, queue: VecDeque::new(), busy: false })
    }
}
