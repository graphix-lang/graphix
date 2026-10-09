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

    // XCR claude for claude: [bug] HttpServe answers the oldest queued request with
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
    // 2026-10-09 claude: Eric ruled 10-09: an instance per request. Handler builds a call
    // of the function per request (its own request binding and ErrorRelay, like a
    // collection slot), answers it with that instance's first fire or raise, and deletes
    // it; the check types a prototype call. Requests run concurrently, and a fire the
    // request did not cause answers nothing. Pins: lib_tests
    // http_concurrent_requests_pair and http_sequential_requests_pair (both fail on the
    // shared instance); the repro prints each path's own page in both triples.
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
    // 2026-10-09 reviewer: the three pins hold (raising_handler_then_good fails
    // with the relay's answer dropped, abandoned_request_then_good with
    // abandoned() false), but ErrorRelay::update lacks Catch::update's
    // `last_cycle` guard, and this loop calls it again in the cycle after it
    // answered. When the next queued request raises at once on dispatch, the
    // generation moves past `received` while the first error is still the
    // delivered value of the relay's bind (deliver_error's try_insert finds it
    // taken and sends the new one next cycle), so the relay counts the old
    // error again: the second request is answered with the first one's error,
    // the first error is passed on twice and the second never. Probe (scratch
    // run! in lib_tests/http.rs, all four modes): handler `let p = select
    // req.path { "/a" => sys::time::after_idle(duration:300.ms, "/a"), p => p };
    // let r: Result<string, `Bad(string)> = error(`Bad(p)); { body: r?, .. }`,
    // GET /a, then GET /b 100ms later: both bodies carry `Bad("/a")`, and
    // "unhandled error ..Bad /a.." is logged twice per run. Adding a
    // `last_cycle: Option<u64>` to ErrorRelay, checked and set as Catch does,
    // makes both answers right and keeps the 48 http tests green. Pin to add:
    // that probe, asserting /b's body names /b.
    // 2026-10-09 claude: ErrorRelay takes one delivery a cycle (last_cycle,
    // as Catch does): a request that raises as it is dispatched, in the cycle
    // the one before it was answered, gets its own error the next cycle, and
    // each error goes on once. Pin: lib_tests
    // http_raises_answer_their_own_requests (both bodies carry /a's error
    // without the check).
    // 2026-10-09 claude: the handler now runs an instance per request
    // (http-sqlite-db1-01), each with an ErrorRelay of its own, so a raise answers the
    // request whose instance raised; the pins above still hold.
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
