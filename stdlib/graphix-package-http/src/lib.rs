#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, ExecCtx, Node,
    Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, Handler,
    ImageState, Inbox, RtVar, seam_arg,
};
use graphix_rt::GXRt;
use http_body_util::{BodyExt, Full, LengthLimitError, Limited};
use hyper::StatusCode;
use netidx_core::pack::PackError;
use netidx_derive::{FromValue, IntoValue};
use netidx_value::{FromValue, ValArray, Value};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{
    fmt,
    marker::PhantomData,
    pin::Pin,
    sync::{Arc, LazyLock},
    task::{Context, Poll},
    time::Duration,
};
use tokio::{
    io::{AsyncRead, AsyncWrite, ReadBuf},
    sync::{Semaphore, oneshot},
    task::JoinSet,
};

#[derive(Debug, Clone)]
struct ClientValue {
    inner: Arc<reqwest::Client>,
}

graphix_package_core::impl_abstract_arc!(
    ClientValue,
    static CLIENT_WRAPPER = "http::Client"
);

#[derive(Debug)]
struct ServerHandle {
    abort: tokio::task::AbortHandle,
    addr: std::net::SocketAddr,
}

impl Drop for ServerHandle {
    fn drop(&mut self) {
        self.abort.abort();
    }
}

#[derive(Debug, Clone)]
struct ServerValue {
    inner: Arc<ServerHandle>,
}

graphix_package_core::impl_abstract_arc!(
    ServerValue,
    static SERVER_WRAPPER = "http::Server"
);

fn http_err(e: impl fmt::Display) -> Value {
    errf!("HTTPError", "{e}")
}

/// Headers from `[(name, value)]`; one that cannot be sent is an error.
fn value_to_header_map(
    v: &Value,
) -> std::result::Result<reqwest::header::HeaderMap, Value> {
    use reqwest::header::{HeaderName, HeaderValue};
    let mut map = reqwest::header::HeaderMap::new();
    let Value::Array(arr) = v else { return Ok(map) };
    for pair in arr.iter() {
        if let Value::Array(p) = pair
            && let [Value::String(k), Value::String(v)] = &p[..]
        {
            let name = HeaderName::from_bytes(k.as_bytes())
                .map_err(|e| http_err(format_args!("invalid header name {k:?}: {e}")))?;
            let val = HeaderValue::from_str(v).map_err(|e| {
                http_err(format_args!("invalid value for header {k}: {e}"))
            })?;
            map.append(name, val);
        }
    }
    Ok(map)
}

/// Headers as `[(name, value)]`, a value that is not UTF-8 decoded lossily.
fn headers_to_value<'a>(
    iter: impl Iterator<
        Item = (&'a hyper::header::HeaderName, &'a hyper::header::HeaderValue),
    >,
) -> Value {
    Value::Array(ValArray::from_iter(iter.map(|(k, v)| {
        let v = String::from_utf8_lossy(v.as_bytes());
        Value::Array(ValArray::from([
            Value::String(ArcStr::from(k.as_str())),
            Value::String(ArcStr::from(&*v)),
        ]))
    })))
}

#[derive(IntoValue)]
struct Response<B> {
    body: B,
    headers: Value,
    status: u16,
    url: ArcStr,
}

fn parse_method(s: &str) -> std::result::Result<reqwest::Method, Value> {
    match s {
        "GET" => Ok(reqwest::Method::GET),
        "POST" => Ok(reqwest::Method::POST),
        "PUT" => Ok(reqwest::Method::PUT),
        "DELETE" => Ok(reqwest::Method::DELETE),
        "PATCH" => Ok(reqwest::Method::PATCH),
        "HEAD" => Ok(reqwest::Method::HEAD),
        "OPTIONS" => Ok(reqwest::Method::OPTIONS),
        other => Err(http_err(format_args!("unknown HTTP method: {other}"))),
    }
}

// CR claude for eric: [bug/medium] A request that never answers wedges its call site
// for good. `HttpRequestEv` takes the default `Calls::Fifo`, so a site runs one call at
// a time and queues the rest without bound. This client, and `http::client` without
// `#timeout`, has no request timeout and no connect timeout. So when a server accepts
// the connection but never replies (or trickles the reply), every later call at that
// site waits behind it forever, even calls to a healthy host. A poller whose url moves
// on gets no further replies. Probe:
// design/review-2026-10-12/repro/http-sqlite-db1-05.gx (request 1 goes to a server that
// never replies, requests 2..29 go to a second server that answers at once, and nothing
// is printed). Two fixes to decide on: (1) give this client and the `http::client`
// builder a default total timeout plus a connect timeout; (2) pick a different `CALLS`
// for requests: `Concurrent` (independent calls, replies in completion order) or
// `Latest` (fits a GET poller, but drops the replies of queued writes). Alternatively,
// bound the queue. Also reported as (x-security-05). (http-sqlite-db1-05)
static DEFAULT_CLIENT: LazyLock<Arc<reqwest::Client>> = LazyLock::new(|| {
    Arc::new(
        reqwest::Client::builder().build().expect("failed to create default HTTP client"),
    )
});

#[derive(Debug, Default)]
pub(crate) struct HttpClientEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpClientEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "http_client";

    fn eval(
        &mut self,
        _ctx: &mut ExecCtx<'_, R, E>,
        cached: &CachedVals,
    ) -> Option<Value> {
        let timeout = cached.get::<Option<Duration>>(0)?;
        let default_headers = cached.value(1)?;
        let redirect_limit = cached.get::<u32>(2)?;
        let ca_cert = cached.get::<Option<Bytes>>(3)?;
        cached.value(4)?;
        let build = || {
            let mut builder = reqwest::Client::builder()
                .redirect(reqwest::redirect::Policy::limited(redirect_limit as usize));
            if let Some(timeout) = timeout {
                builder = builder.timeout(timeout);
            }
            let headers = value_to_header_map(default_headers)?;
            if !headers.is_empty() {
                builder = builder.default_headers(headers);
            }
            if let Some(ca_cert) = &ca_cert {
                let cert = reqwest::Certificate::from_pem(ca_cert)
                    .map_err(|e| http_err(format_args!("invalid ca_cert PEM: {e}")))?;
                // CR claude for eric: [risk/medium] `#ca_cert` here adds the CA to the
                // OS trust store: reqwest 0.13's deprecated `add_root_certificate`
                // merges with the platform verifier's roots. `sys::tls::connect`'s
                // `#ca_cert` instead replaces the roots with only that CA. So an http
                // client given a private CA still accepts any publicly trusted
                // certificate for the host, where a sys::tls connection with the same
                // argument refuses it. The probe shows a fresh private CA: sys::tls
                // refuses example.com with UnknownIssuer, and http::get through
                // `http::client(#ca_cert: ca, ..)` gets 200 (probe:
                // design/review-2026-10-12/repro/x-security-08.sh). Pick one meaning
                // for both. To pin, use `tls_certs_only`. To trust it in addition, use
                // `tls_certs_merge` here and make sys::tls add the CA to webpki_roots.
                // Then say which in both gxi docs, or give the two meanings two names.
                // (x-security-08)
                builder = builder.add_root_certificate(cert);
            }
            let client = builder
                .build()
                .map_err(|e| http_err(format_args!("failed to build client: {e}")))?;
            Ok(CLIENT_WRAPPER.wrap(ClientValue { inner: Arc::new(client) }))
        };
        Some(build().unwrap_or_else(|e: Value| e))
    }
}

pub(crate) type HttpClient = CachedArgs<HttpClientEv>;

#[derive(Debug, Default)]
pub(crate) struct HttpDefaultClientEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpDefaultClientEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "http_default_client";

    fn eval(
        &mut self,
        _ctx: &mut ExecCtx<'_, R, E>,
        cached: &CachedVals,
    ) -> Option<Value> {
        cached.value(0)?;
        Some(CLIENT_WRAPPER.wrap(ClientValue { inner: DEFAULT_CLIENT.clone() }))
    }
}

pub(crate) type HttpDefaultClient = CachedArgs<HttpDefaultClientEv>;

#[derive(Debug, Default)]
pub(crate) struct HttpServerAddrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpServerAddrEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "http_server_addr";

    fn eval(
        &mut self,
        _ctx: &mut ExecCtx<'_, R, E>,
        cached: &CachedVals,
    ) -> Option<Value> {
        let sv = cached.abstract_arg::<ServerValue>(0)?;
        Some(Value::String(ArcStr::from(sv.inner.addr.to_string().as_str())))
    }
}

pub(crate) type HttpServerAddr = CachedArgs<HttpServerAddrEv>;

graphix_package_core::unit_image_state!(
    HttpClientEv,
    HttpDefaultClientEv,
    HttpServerAddrEv
);

/// What a request sends and reads back: text or bytes.
pub(crate) trait BodyKind: FromValue + fmt::Debug + Send + Sync + 'static {
    const NAME: &str;
    type Read: Into<Value> + Send;

    fn body(self) -> reqwest::Body;

    fn read(
        resp: reqwest::Response,
    ) -> impl Future<Output = reqwest::Result<Self::Read>> + Send;
}

impl BodyKind for ArcStr {
    const NAME: &str = "http_request";
    type Read = ArcStr;

    fn body(self) -> reqwest::Body {
        Bytes::from_owner(self).into()
    }

    // CR claude for eric: [risk/medium] The client reads a response body whole, with no
    // bound. Both impls of BodyKind::read (`resp.text()` here, which is then copied
    // again into an ArcStr, and `resp.bytes()` in the Bytes impl) buffer whatever the
    // server sends. `#timeout` defaults to null, so a server that streams without end,
    // or a redirect to a huge file, grows the process until the OS kills it, and the
    // program never gets a catchable `HTTPError`. Probe:
    // design/review-2026-10-12/repro/http-sqlite-db1-10.sh hit a 6 GB cap in 3 s.
    // http::serve already bounds request bodies with `#max_body` through `Limited`.
    // Proposed fix: give request/request_bin (and get/get_bin) a `?#max_body: i64`,
    // read through `Limited::new(resp, max)` or a `resp.chunk()` loop, fail with
    // `HTTPError("body over N bytes")`, and build the ArcStr once from the bytes. Eric
    // needs to choose the default bound, because legitimate downloads can be large.
    // (http-sqlite-db1-10)
    async fn read(resp: reqwest::Response) -> reqwest::Result<ArcStr> {
        // CR claude for eric: [bug/medium] BodyKind::read buffers the whole response
        // body with no size limit, here and in the Bytes impl's resp.bytes(), and the
        // text path copies it a second time into the ArcStr. The default client and
        // http::get set no timeout. A server that streams an endless or very large body
        // therefore grows the process until the kernel kills it, and the program never
        // gets an HTTPError. The server side already bounds request bodies with
        // #max_body and Limited; the client side reads untrusted network data with no
        // bound. Fix: add a #max_body to http::client and to request/request_bin,
        // defaulting like serve's. Reject a Content-Length over it up front, read with
        // resp.chunk() in a loop (or Limited over the body stream) and return HTTPError
        // past the limit. Build the ArcStr once from the collected bytes. probe:
        // design/review-2026-10-12/repro/x-panics-03.py (OOM-killed at the 6 GB cap
        // after about 3 s). (x-panics-03)
        // 2026-10-13 claude: re-addressed: the bound's default is the choice
        // http-sqlite-db1-10 above asks of Eric; this waits on that ruling.
        Ok(ArcStr::from(resp.text().await?.as_str()))
    }
}

impl BodyKind for Bytes {
    const NAME: &str = "http_request_bin";
    type Read = Bytes;

    fn body(self) -> reqwest::Body {
        self.into()
    }

    async fn read(resp: reqwest::Response) -> reqwest::Result<Bytes> {
        resp.bytes().await
    }
}

#[derive(Debug)]
pub(crate) struct RequestArgs<B> {
    method: ArcStr,
    headers: Value,
    body: Option<B>,
    timeout: Option<Duration>,
    client: Arc<reqwest::Client>,
    url: ArcStr,
}

#[derive(Debug)]
pub(crate) struct HttpRequestEv<B>(PhantomData<B>);

impl<B> Default for HttpRequestEv<B> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<B: BodyKind> ImageState for HttpRequestEv<B> {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        _buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self(PhantomData))
    }
}

impl<B: BodyKind> EvalCachedAsync for HttpRequestEv<B> {
    type Args = RequestArgs<B>;

    const NAME: &str = B::NAME;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some(RequestArgs {
            method: cached.get::<ArcStr>(0)?,
            headers: cached.value(1)?.clone(),
            body: cached.get::<Option<B>>(2)?,
            timeout: cached.get::<Option<Duration>>(3)?,
            client: cached.abstract_arg::<ClientValue>(4)?.inner.clone(),
            url: cached.get::<ArcStr>(5)?,
        })
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let send = async {
                let mut req =
                    args.client.request(parse_method(&args.method)?, &*args.url);
                let headers = value_to_header_map(&args.headers)?;
                if !headers.is_empty() {
                    req = req.headers(headers);
                }
                if let Some(body) = args.body {
                    req = req.body(body.body());
                }
                if let Some(timeout) = args.timeout {
                    req = req.timeout(timeout);
                }
                let resp = req
                    .send()
                    .await
                    .map_err(|e| http_err(format_args!("request failed: {e}")))?;
                let status = resp.status().as_u16();
                let url = ArcStr::from(resp.url().as_str());
                let headers = headers_to_value(resp.headers().iter());
                let body = B::read(resp)
                    .await
                    .map_err(|e| http_err(format_args!("failed to read body: {e}")))?;
                Ok(Response { body: body.into(), headers, status, url }.into())
            };
            send.await.unwrap_or_else(|e: Value| e)
        }
    }
}

pub(crate) type HttpRequest = CachedArgsAsync<HttpRequestEv<ArcStr>>;
pub(crate) type HttpRequestBin = CachedArgsAsync<HttpRequestEv<Bytes>>;

/// A request and where its answer goes.
type Asked = (Value, oneshot::Sender<Value>);

/// Requests are waiting in the server's inbox.
#[derive(Debug)]
struct HttpWake;

impl CustomBuiltinType for HttpWake {}

type Batches = mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>;

/// Where a connection posts its requests: the server's inbox, and the
/// event that wakes it under its id.
#[derive(Clone)]
struct Post {
    tx: Batches,
    id: BindId,
    inbox: Arc<Inbox<Asked>>,
}

fn text_response(
    status: StatusCode,
    msg: impl fmt::Display,
) -> hyper::Response<Full<Bytes>> {
    let mut response = hyper::Response::new(Full::new(Bytes::from(msg.to_string())));
    *response.status_mut() = status;
    response
}

fn build_hyper_response(v: &Value) -> hyper::Response<Full<Bytes>> {
    // CR claude for eric: [bug/high] db1-02 regression: a raising http handler's 500
    // body leaks ErrChain internals to the client. Since a959e8cc,
    // core::handler::Request::update answers a request with the error its handler
    // raised (`out.or(raised)`), and build_hyper_response sends any Value::Error to the
    // HTTP client verbatim: `if let Value::Error(e) = v { return
    // text_response(StatusCode::INTERNAL_SERVER_ERROR, e) }`. The raised value is the
    // full ErrChain from node/error.rs wrap_error: cause, error, `ori`
    // (Origin::to_value: source with its absolute file path, the parent origins) and
    // `pos`. For Source::Program and Source::Unspecified, Origin::to_value
    // (graphix-types/src/expr/mod.rs) also copies the source text into `text`.
    // sys::net::publish_rpc uses the same Handler, so an rpc caller receives the same
    // chain. Failure: Any client that can make a handler's `?` fire (here, a malformed
    // JSON body) gets back: 500 [["cause", null], ["error", ["JsonErr", ...]], ["ori",
    // [["parent", null], ["source", ["File", "/home/eric/.../11_leak.gx"]], ["text",
    // ""]]], ["pos", [["column", i32:29], ["line", i32:5]]]]. That is the server's
    // absolute source path and the line/column of the failing expression. Under
    // graphix-fuzz (Source::Program root) the body also carries the parent Program
    // origin's text. The same holds for a standalone binary whose embedded main program
    // runs under Source::Program (graphix-shell/src/lib.rs); I inferred this from that
    // code and did not run such a binary. Fix: Answer a raised or bottom handler with a
    // fixed body ("Internal Server Error") and log the chain server-side; the catch at
    // the serve site already receives the error. Do the same for publish_rpc's reply.
    // Also stop copying a Source::Program's whole text into every ErrChain link.
    // Unverified: the reviewer's report, not checked by a verifier. (x-rerun-4-01)
    // 2026-10-13 claude: re-addressed: answering a generic body is the policy question
    // http-sqlite-db1-03 below puts to Eric; this waits on that ruling.
    // CR claude for eric: [risk/medium] When a handler raises, the remote client gets a
    // 500 whose body is the Display of the whole ErrChain. That includes the error
    // payload (db error text, paths, anything the handler put in the message) and the
    // `ori`/`pos` fields, which name the server's source file and line. Any client that
    // can make the handler fail can read internal details this way. The interface doc
    // for serve only promises "answered 500". Proposed fix: log the error server-side
    // (FAILURE_TARGET, or the covering handler, which already receives it) and answer a
    // generic "Internal Server Error" body. A handler that wants to expose details
    // returns its own 500 Response. The test http_raises_answer_their_own_requests
    // tells requests apart by this body, so it would need another way to tell which
    // request got which answer. probe:
    // design/review-2026-10-12/repro/http-sqlite-db1-03.gx Also reported as
    // (x-security-04). (http-sqlite-db1-03)
    if let Value::Error(e) = v {
        return text_response(StatusCode::INTERNAL_SERVER_ERROR, e);
    }
    #[derive(FromValue)]
    struct Fields {
        body: ArcStr,
        headers: ValArray,
        status: u16,
    }
    let Fields { body, headers, status } = match v.clone().cast_to::<Fields>() {
        Ok(f) => f,
        Err(e) => {
            let msg = format_args!("invalid response: {e}");
            return text_response(StatusCode::INTERNAL_SERVER_ERROR, msg);
        }
    };
    let mut response = hyper::Response::builder().status(status);
    for h in headers.iter() {
        if let Value::Array(pair) = h
            && let [Value::String(k), Value::String(v)] = &pair[..]
        {
            response = response.header(&**k, &**v);
        }
    }
    match response.body(Full::new(Bytes::from_owner(body))) {
        Ok(response) => response,
        Err(e) => {
            let msg = format_args!("invalid response: {e}");
            text_response(StatusCode::INTERNAL_SERVER_ERROR, msg)
        }
    }
}

/// Read a request whole, at most `max_body` bytes of body, and hand it to
/// the server's node; a body over the limit is 413 and one that did not
/// arrive whole is 400, neither dispatched.
async fn handle_http_request(
    req: hyper::Request<hyper::body::Incoming>,
    mut post: Post,
    max_body: usize,
) -> hyper::Response<Full<Bytes>> {
    let (parts, body) = req.into_parts();
    let declared = parts
        .headers
        .get(hyper::header::CONTENT_LENGTH)
        .and_then(|v| v.to_str().ok()?.parse::<u64>().ok());
    if declared.is_some_and(|n| n > max_body as u64) {
        return text_response(StatusCode::PAYLOAD_TOO_LARGE, "Payload Too Large");
    }
    let body =
        tokio::time::timeout(BODY_READ_TIMEOUT, Limited::new(body, max_body).collect());
    let raw = match body.await {
        Err(_) => return text_response(StatusCode::REQUEST_TIMEOUT, "Request Timeout"),
        Ok(Ok(b)) => b.to_bytes(),
        Ok(Err(e)) if e.is::<LengthLimitError>() => {
            return text_response(StatusCode::PAYLOAD_TOO_LARGE, "Payload Too Large");
        }
        Ok(Err(e)) => return text_response(StatusCode::BAD_REQUEST, e),
    };
    // CR claude for eric: [risk/medium] Every connection collects its whole body (up to
    // `max_body`) before dispatch, and a UTF-8 body is then copied a second time into
    // `body` with `ArcStr::from`, so a request holds two copies. With the defaults (768
    // connections, 16 MiB) remote clients can pin about 24 GiB before the program sees
    // a single request. Dropping the copy means changing `Request`: drop `body` and let
    // the handler decode `raw`. The other part is a cap on total in-flight body bytes,
    // such as a byte-counting semaphore, or smaller defaults. Both are API and policy
    // choices. The defaults are also written twice, in `serve`'s labels in mod.gx and
    // in `Settings` here (`DEFAULT_MAX_BODY`, the 768 literal). (x-security-06)
    let body = match std::str::from_utf8(&raw) {
        Ok(s) if !s.is_empty() => Some(ArcStr::from(s)),
        Ok(_) | Err(_) => None,
    };
    #[derive(IntoValue)]
    struct Fields {
        body: Option<ArcStr>,
        headers: Value,
        method: ArcStr,
        path: ArcStr,
        query: Option<ArcStr>,
        raw: Bytes,
    }
    let request = Fields {
        body,
        headers: headers_to_value(parts.headers.iter()),
        method: ArcStr::from(parts.method.as_str()),
        path: ArcStr::from(parts.uri.path()),
        query: parts.uri.query().map(ArcStr::from),
        raw,
    }
    .into();
    let (reply_tx, reply_rx) = oneshot::channel();
    if post.inbox.post((request, reply_tx)) {
        let mut batch = CBATCH_POOL.take();
        batch.push((post.id, Box::new(HttpWake) as Box<dyn CustomBuiltinType>));
        if post.tx.send(batch).await.is_err() {
            return text_response(StatusCode::SERVICE_UNAVAILABLE, "Service Unavailable");
        }
    }
    match reply_rx.await {
        Ok(resp_value) => build_hyper_response(&resp_value),
        Err(_) => {
            text_response(StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error")
        }
    }
}

fn build_tls_acceptor(
    cert_pem: &[u8],
    key_pem: &[u8],
) -> std::result::Result<tokio_rustls::TlsAcceptor, Value> {
    let certs: Vec<_> = rustls_pemfile::certs(&mut &*cert_pem)
        .collect::<std::result::Result<_, _>>()
        .map_err(|e| http_err(format_args!("invalid cert PEM: {e}")))?;
    let key = rustls_pemfile::private_key(&mut &*key_pem)
        .map_err(|e| http_err(format_args!("invalid key PEM: {e}")))?
        .ok_or_else(|| http_err("no private key found in key PEM"))?;
    let config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| http_err(format_args!("TLS config error: {e}")))?;
    Ok(tokio_rustls::TlsAcceptor::from(Arc::new(config)))
}

enum MaybeTls {
    Plain(tokio::net::TcpStream),
    Tls(tokio_rustls::server::TlsStream<tokio::net::TcpStream>),
}

impl AsyncRead for MaybeTls {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            MaybeTls::Plain(s) => Pin::new(s).poll_read(cx, buf),
            MaybeTls::Tls(s) => Pin::new(s).poll_read(cx, buf),
        }
    }
}

impl AsyncWrite for MaybeTls {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        match self.get_mut() {
            MaybeTls::Plain(s) => Pin::new(s).poll_write(cx, buf),
            MaybeTls::Tls(s) => Pin::new(s).poll_write(cx, buf),
        }
    }

    fn poll_flush(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            MaybeTls::Plain(s) => Pin::new(s).poll_flush(cx),
            MaybeTls::Tls(s) => Pin::new(s).poll_flush(cx),
        }
    }

    fn poll_shutdown(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            MaybeTls::Plain(s) => Pin::new(s).poll_shutdown(cx),
            MaybeTls::Tls(s) => Pin::new(s).poll_shutdown(cx),
        }
    }
}

/// One connection: its TLS handshake, if any, then its requests.
async fn serve_connection(
    stream: tokio::net::TcpStream,
    tls: Option<tokio_rustls::TlsAcceptor>,
    post: Post,
    max_body: usize,
) {
    let io = match tls {
        None => MaybeTls::Plain(stream),
        Some(acceptor) => {
            match tokio::time::timeout(
                graphix_package_core::TLS_HANDSHAKE_TIMEOUT,
                acceptor.accept(stream),
            )
            .await
            {
                Ok(Ok(s)) => MaybeTls::Tls(s),
                Ok(Err(e)) => return log::error!("TLS handshake error: {e}"),
                Err(_) => return log::error!("TLS handshake timed out"),
            }
        }
    };
    let service = hyper::service::service_fn(|req| {
        let resp = handle_http_request(req, post.clone(), max_body);
        async move { Ok::<_, std::convert::Infallible>(resp.await) }
    });
    // CR claude for eric: [bug/high] This connection builder has no timer, so hyper
    // drops its default 30 s header-read timeout ("has default, but no timer set").
    // Nothing else limits how long a connection may stay idle. Each of these holds its
    // `max_connections` permit from `serve_loop` for as long as the client keeps the
    // socket open: - a connection that never sends a byte, - a keep-alive connection
    // idle between requests, - a request whose body trickles in (the `Limited::collect`
    // in `handle_http_request` has no deadline). So `max_connections` silent sockets
    // (768 by default, opened in about 2 s) lock every other client out indefinitely.
    // Fix: - give the builder `.timer(hyper_util::rt::TokioTimer::new())` and an
    // explicit `.header_read_timeout(..)`; - bound the body collect with
    // `tokio::time::timeout`; - bound keep-alive idle time between requests. Whether
    // these are `serve` arguments with defaults is a policy call. Probe:
    // design/review-2026-10-12/repro/x-security-01.py (x-security-01)
    // 2026-10-13 claude: the builder now has a timer and a 30 s header read timeout
    // (HEADER_READ_TIMEOUT, which also closes a keep-alive connection idle that long),
    // and a body has 60 s to arrive (BODY_READ_TIMEOUT, else 408). Fixed values: whether
    // they are serve arguments is the policy call above. Pin:
    // lib_tests::http::http_silent_socket_releases_its_permit (slow-tests).
    // a connection waiting on a request's headers, a silent one or one idle
    // between requests, is closed past the header timeout: it holds a permit
    if let Err(e) = hyper::server::conn::http1::Builder::new()
        .timer(hyper_util::rt::TokioTimer::new())
        .header_read_timeout(HEADER_READ_TIMEOUT)
        .serve_connection(hyper_util::rt::TokioIo::new(io), service)
        .await
    {
        log::error!("HTTP connection error: {e}");
    }
}

/// The connections a socket has accepted: they outlive a restart on the
/// socket and end with it.
#[derive(Debug, Clone, Default)]
struct Conns(Arc<Mutex<JoinSet<()>>>);

/// Accept connections until aborted.
async fn serve_loop(
    listener: tokio::net::TcpListener,
    conns: Conns,
    tls: Option<tokio_rustls::TlsAcceptor>,
    post: Post,
    max_connections: Arc<Semaphore>,
    max_body: usize,
) {
    loop {
        while conns.0.lock().try_join_next().is_some() {}
        let Ok(permit) = max_connections.clone().acquire_owned().await else { return };
        let stream = match listener.accept().await {
            Ok((stream, _)) => stream,
            Err(e) => {
                log::error!("HTTP accept error: {e}");
                continue;
            }
        };
        let (tls, post) = (tls.clone(), post.clone());
        conns.0.lock().spawn(async move {
            let _permit = permit;
            serve_connection(stream, tls, post, max_body).await
        });
    }
}

/// The server's settings, as its arguments give them.
struct Settings<'a> {
    addr: &'a ArcStr,
    tls: Option<tokio_rustls::TlsAcceptor>,
    max_connections: usize,
    max_body: usize,
}

impl<'a> Settings<'a> {
    fn of(
        addr: &'a ArcStr,
        cert: &Value,
        key: &Value,
        max_connections: &Value,
        max_body: &Value,
    ) -> std::result::Result<Self, Value> {
        // plain HTTP only when both are null, never because one is missing
        let tls = match (cert, key) {
            (Value::Bytes(cert), Value::Bytes(key)) => {
                Some(build_tls_acceptor(cert, key)?)
            }
            (Value::Null, Value::Null) => None,
            _ => return Err(http_err("both cert and key must be provided for TLS")),
        };
        let max_connections = match max_connections {
            Value::I64(n) => usize::try_from(*n)
                .ok()
                .filter(|n| (1..=Semaphore::MAX_PERMITS).contains(n))
                .ok_or_else(|| {
                    let max = Semaphore::MAX_PERMITS;
                    http_err(format_args!(
                        "max_connections must be in 1..={max}, got {n}"
                    ))
                })?,
            _ => 768,
        };
        let max_body = match max_body {
            Value::I64(n) => usize::try_from(*n).map_err(|_| {
                http_err(format_args!("max_body must be at least 0, got {n}"))
            })?,
            _ => DEFAULT_MAX_BODY,
        };
        Ok(Self { addr, tls, max_connections, max_body })
    }
}

const DEFAULT_MAX_BODY: usize = 16 * 1024 * 1024;

/// How long a connection may take to send a request's headers, counted
/// from when it may send them: a new connection, or the last response.
const HEADER_READ_TIMEOUT: Duration = Duration::from_secs(30);

/// How long a request may take to send its body.
const BODY_READ_TIMEOUT: Duration = Duration::from_secs(60);

/// The bound socket, kept across restarts so one on the same address
/// reuses it rather than racing the old loop's drop, and the connections
/// it accepted, which end with it.
#[derive(Debug)]
struct Listening {
    addr: ArcStr,
    socket: std::net::TcpListener,
    conns: Conns,
}

impl Drop for Listening {
    fn drop(&mut self) {
        self.conns.0.lock().abort_all()
    }
}

#[derive(Debug)]
pub(crate) struct HttpServe<R: Rt, E: UserEvent> {
    var: RtVar,
    handler: Handler<R, E, oneshot::Sender<Value>>,
    /// Requests posted under `id`, taken at each wake.
    inbox: Arc<Inbox<Asked>>,
    listening: Option<Listening>,
    abort: Option<tokio::task::AbortHandle>,
    /// Set by `sleep`, taken by the next update as a restart.
    slept: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for HttpServe<R, E> {
    const NAME: &str = "http_serve";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let [_, _, _, _, _, _] = from else { bail!("expected six arguments") };
        let typ = resolved.unwrap_or(typ);
        let handler = Handler::new(ctx, &typ.args[5].typ, scope, top_id)?;
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(HttpServe {
            var,
            handler,
            inbox: Arc::default(),
            listening: None,
            abort: None,
            slept: false,
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
        Ok(Box::new(HttpServe {
            var,
            handler,
            inbox: Arc::default(),
            listening: None,
            abort: None,
            slept: false,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> HttpServe<R, E> {
    fn stop(&mut self) {
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
    }

    fn start(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        s: Settings,
    ) -> std::result::Result<Value, Value> {
        self.stop();
        let l = match &mut self.listening {
            Some(l) if l.addr == *s.addr => l,
            slot => {
                *slot = None;
                let socket = std::net::TcpListener::bind(&**s.addr)
                    .and_then(|l| l.set_nonblocking(true).map(|()| l))
                    .map_err(|e| {
                        http_err(format_args!("bind to {} failed: {e}", s.addr))
                    })?;
                let conns = Conns::default();
                slot.insert(Listening { addr: s.addr.clone(), socket, conns })
            }
        };
        let conns = l.conns.clone();
        // a clone does not share the original's non-blocking mode on Windows
        let std_listener = l
            .socket
            .try_clone()
            .and_then(|c| c.set_nonblocking(true).map(|()| c))
            .map_err(|e| http_err(format_args!("bind to {} failed: {e}", s.addr)))?;
        let addr = std_listener
            .local_addr()
            .map_err(|e| http_err(format_args!("local_addr failed: {e}")))?;
        let listener = tokio::net::TcpListener::from_std(std_listener)
            .map_err(|e| http_err(format_args!("tokio listener failed: {e}")))?;
        let (tx, rx) = mpsc::channel(100);
        ctx.rt.watch(rx);
        let semaphore = Arc::new(Semaphore::new(s.max_connections));
        let post = Post { tx, id: self.var.id, inbox: self.inbox.clone() };
        let serve = serve_loop(listener, conns, s.tls, post, semaphore, s.max_body);
        let abort = tokio::spawn(serve).abort_handle();
        self.abort = Some(abort.clone());
        Ok(SERVER_WRAPPER
            .wrap(ServerValue { inner: Arc::new(ServerHandle { abort, addr }) }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for HttpServe<R, E> {
    /// `abort` is a running server.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.abort.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)?;
        self.handler.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept);
        let (addr, addr_fired) = seam_arg(ctx, &mut from[0]);
        let (cert, cert_fired) = seam_arg(ctx, &mut from[1]);
        let (key, key_fired) = seam_arg(ctx, &mut from[2]);
        let (max_conns, max_conns_fired) = seam_arg(ctx, &mut from[3]);
        let (max_body, max_body_fired) = seam_arg(ctx, &mut from[4]);
        let (f, f_fired) = seam_arg(ctx, &mut from[5]);
        self.handler.set_fn(ctx, f, f_fired);
        let mut started = None;
        if woke
            || addr_fired
            || cert_fired
            || key_fired
            || max_conns_fired
            || max_body_fired
        {
            self.stop();
            // a bottom argument bottoms the call, and no server runs
            started = Some(match (&addr, &cert, &key, &max_conns, &max_body) {
                (
                    Some(Value::String(addr)),
                    Some(cert),
                    Some(key),
                    Some(mc),
                    Some(mb),
                ) => {
                    let s = Settings::of(addr, cert, key, mc, mb);
                    Some(s.and_then(|s| self.start(ctx, s)).unwrap_or_else(|e| e))
                }
                _ => None,
            });
        }
        // XCR claude for claude: [perf/medium] Each connection sends its own
        // HttpReqEvent under the server's one BindId, and the runtime delivers one
        // custom event per id per cycle, queuing the rest. N simultaneous requests
        // therefore start over N cycles. A sync handler answers in the cycle it starts,
        // so Handler::update's fork_instances never holds two of them, and the
        // advertised forking of a server's requests only happens for async handlers
        // that happen to finish together. Each request also pays for a whole cycle.
        // Fix: connections push (request, reply) into a shared queue and send a wake
        // event, or the event carries every waiting request, so Handler::push gets the
        // whole batch in one cycle. sys::net's rpc server (its take_custom then
        // handler.push) needs the same change. (http-sqlite-db1-09)
        // 2026-10-13 claude: connections post into the server's core::Inbox and send
        // a wake only when none is on its way; a wake takes every request posted, and
        // publish_rpc does the same. Delivery is pinned by http_concurrent_requests_pair
        // and the net rpc tests; the batching itself depends on arrival timing and has
        // no deterministic pin.
        if ctx.event.take_custom(&self.var.id).is_some() {
            let mut asked: LPooled<Vec<Asked>> = LPooled::take();
            self.inbox.take(&mut *asked);
            for (request, reply) in asked.drain(..) {
                self.handler.push(request, reply);
            }
        }
        self.handler.update(ctx);
        match started {
            Some(Some(v)) => self.out.set(TagValue::fired(v)),
            Some(None) => self.out.set_bottom(true),
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

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.handler.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.var.release(ctx);
        self.stop();
        self.listening = None;
        self.handler.delete(ctx);
    }

    /// The server stops, its connections and pending requests dropped, and
    /// starts again at the wake on the socket it keeps.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.var.remint(ctx);
        self.stop();
        if let Some(l) = &self.listening {
            l.conns.0.lock().abort_all();
        }
        self.slept = true;
        self.out = TagValue::phantom();
        self.inbox = Arc::default();
        self.handler.sleep(ctx);
    }
}

graphix_derive::defpackage! {
    builtins => [
        HttpClient,
        HttpDefaultClient,
        HttpServerAddr,
        HttpRequest,
        HttpRequestBin,
        HttpServe as HttpServe<GXRt<X>, X::UserEvent>,
    ],
}
