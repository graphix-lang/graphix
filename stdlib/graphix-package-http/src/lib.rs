#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use compact_str::format_compact;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, ExecCtx, Node,
    Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::WakeBit,
    rep,
    typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, Calls, EvalCached, EvalCachedAsync, Handler,
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
    sync::{Semaphore, oneshot, watch},
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

/// `e` with its whole source chain, which reqwest's and hyper's Display
/// leave out: a refused connection and an unknown host read alike
/// without it.
fn chain(e: impl std::error::Error + Send + Sync + 'static) -> impl fmt::Display {
    format_compact!("{:#}", anyhow::Error::from(e))
}

/// Headers from `[(name, value)]`; one that cannot be sent is an error.
fn value_to_header_map(
    v: &Value,
) -> std::result::Result<reqwest::header::HeaderMap, Value> {
    use reqwest::header::{HeaderName, HeaderValue};
    let mut map = reqwest::header::HeaderMap::new();
    let Value::Array(arr) = v else { return Ok(map) };
    for pair in arr.iter() {
        if let Some([Value::String(k), Value::String(v)]) = rep::tuple_elems(pair) {
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
        rep::tuple([
            Value::String(ArcStr::from(k.as_str())),
            Value::String(ArcStr::from(&*v)),
        ])
    })))
}

#[derive(IntoValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
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

/// How long a client waits for a connection, unless it says otherwise.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(30);

static DEFAULT_CLIENT: LazyLock<Arc<reqwest::Client>> = LazyLock::new(|| {
    Arc::new(
        reqwest::Client::builder()
            .connect_timeout(CONNECT_TIMEOUT)
            .build()
            .expect("failed to create default HTTP client"),
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
        let connect_timeout = cached.get::<Option<Duration>>(4)?;
        cached.value(5)?;
        let build = || {
            let mut builder = reqwest::Client::builder()
                .redirect(reqwest::redirect::Policy::limited(redirect_limit as usize));
            if let Some(timeout) = timeout {
                builder = builder.timeout(timeout);
            }
            if let Some(timeout) = connect_timeout {
                builder = builder.connect_timeout(timeout);
            }
            let headers = value_to_header_map(default_headers)?;
            if !headers.is_empty() {
                builder = builder.default_headers(headers);
            }
            if let Some(ca_cert) = &ca_cert {
                let certs = reqwest::Certificate::from_pem_bundle(ca_cert)
                    .map_err(|e| http_err(format_args!("invalid ca_cert PEM: {e}")))?;
                if certs.is_empty() {
                    return Err(http_err("invalid ca_cert PEM: no certificate in it"));
                }
                builder = builder.tls_certs_only(certs);
            }
            let client = builder.build().map_err(|e| {
                http_err(format_args!("failed to build client: {}", chain(e)))
            })?;
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

    /// The body read, as this kind reads it.
    fn of(body: Bytes) -> Self::Read;
}

impl BodyKind for ArcStr {
    const NAME: &str = "http_request";
    type Read = ArcStr;

    fn body(self) -> reqwest::Body {
        Bytes::from_owner(self).into()
    }

    fn of(body: Bytes) -> ArcStr {
        ArcStr::from(&*String::from_utf8_lossy(&body))
    }
}

impl BodyKind for Bytes {
    const NAME: &str = "http_request_bin";
    type Read = Bytes;

    fn body(self) -> reqwest::Body {
        self.into()
    }

    fn of(body: Bytes) -> Bytes {
        body
    }
}

/// A response's body, refused past `max` bytes: by its Content-Length
/// before reading, else as it arrives.
async fn read_body(
    mut resp: reqwest::Response,
    max: u64,
) -> std::result::Result<Bytes, Value> {
    let over = || http_err(format_args!("body over {max} bytes"));
    if resp.content_length().is_some_and(|n| n > max) {
        return Err(over());
    }
    let mut body = bytes::BytesMut::new();
    while let Some(chunk) = resp
        .chunk()
        .await
        .map_err(|e| http_err(format_args!("failed to read body: {}", chain(e))))?
    {
        if (body.len() + chunk.len()) as u64 > max {
            return Err(over());
        }
        body.extend_from_slice(&chunk);
    }
    Ok(body.freeze())
}

#[derive(Debug)]
pub(crate) struct RequestArgs<B> {
    method: ArcStr,
    headers: Value,
    body: Option<B>,
    timeout: Option<Duration>,
    max_body: i64,
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
    const CALLS: Calls = Calls::Concurrent;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some(RequestArgs {
            method: match rep::variant(cached.value(0)?)? {
                (method, []) => method.clone(),
                _ => return None,
            },
            headers: cached.value(1)?.clone(),
            body: cached.get::<Option<B>>(2)?,
            timeout: cached.get::<Option<Duration>>(3)?,
            max_body: cached.get::<i64>(4)?,
            client: cached.abstract_arg::<ClientValue>(5)?.inner.clone(),
            url: cached.get::<ArcStr>(6)?,
        })
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let send = async {
                let max_body = u64::try_from(args.max_body).map_err(|_| {
                    http_err(format_args!(
                        "max_body must be at least 0, got {}",
                        args.max_body
                    ))
                })?;
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
                let resp = req.send().await.map_err(|e| {
                    http_err(format_args!("request failed: {}", chain(e)))
                })?;
                let status = resp.status().as_u16();
                let url = ArcStr::from(resp.url().as_str());
                let headers = headers_to_value(resp.headers().iter());
                let body = B::of(read_body(resp, max_body).await?);
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
    if let Value::Error(_) = v {
        log::error!(
            target: graphix_compiler::FAILURE_TARGET,
            "an http handler raised: {}",
            graphix_compiler::typ::Naked(v)
        );
        return text_response(StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error");
    }
    #[derive(FromValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        if let Some([Value::String(k), Value::String(v)]) =
            rep::read_tuple(h).map(rep::Parts::elems)
            && !k.eq_ignore_ascii_case(hyper::header::CONTENT_LENGTH.as_str())
        {
            // XCR claude for claude: [bug/medium] A content-length the handler returns
            // is passed to hyper next to a Full body of another length. hyper checks
            // that they match only in debug builds, where the connection task panics.
            // In release it writes the handler's value and then the real body, so the
            // extra bytes are read as the next response on a keep-alive connection
            // (response splitting). A handler that forwards an upstream response's
            // headers hits this whenever lossy UTF-8 decoding changes the body's
            // length. Fix: drop content-length here (hyper sets it from the body), or
            // answer 500 on a mismatch. probe:
            // design/review-2026-10-26/repro/x-security-http-content-length.sh
            // (x-security-04)
            // 2026-10-26 claude: build_hyper_response drops a content-length the
            // handler gives (any case), so hyper writes the body's own;
            // http/mod.gxi's Reply says so. Pin:
            // lib_tests::http::http_handler_content_length_is_the_bodys (debug hyper
            // panics the connection with the header kept).
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
    body_timeout: Duration,
) -> hyper::Response<Full<Bytes>> {
    let (parts, body) = req.into_parts();
    let declared = parts
        .headers
        .get(hyper::header::CONTENT_LENGTH)
        .and_then(|v| v.to_str().ok()?.parse::<u64>().ok());
    if declared.is_some_and(|n| n > max_body as u64) {
        return text_response(StatusCode::PAYLOAD_TOO_LARGE, "Payload Too Large");
    }
    let body = tokio::time::timeout(body_timeout, Limited::new(body, max_body).collect());
    let raw = match body.await {
        Err(_) => return text_response(StatusCode::REQUEST_TIMEOUT, "Request Timeout"),
        Ok(Ok(b)) => b.to_bytes(),
        Ok(Err(e)) if e.is::<LengthLimitError>() => {
            return text_response(StatusCode::PAYLOAD_TOO_LARGE, "Payload Too Large");
        }
        Ok(Err(e)) => return text_response(StatusCode::BAD_REQUEST, e),
    };
    #[derive(IntoValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
    struct Fields {
        headers: Value,
        method: ArcStr,
        path: ArcStr,
        query: Option<ArcStr>,
        raw: Bytes,
    }
    let request = Fields {
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

/// One connection: its TLS handshake, if any, then its requests, until
/// `closing`, when it answers the requests it holds and closes.
async fn serve_connection(
    stream: tokio::net::TcpStream,
    tls: Option<tokio_rustls::TlsAcceptor>,
    post: Post,
    limits: Limits,
    mut closing: watch::Receiver<bool>,
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
        let resp =
            handle_http_request(req, post.clone(), limits.max_body, limits.body_timeout);
        async move { Ok::<_, std::convert::Infallible>(resp.await) }
    });
    let conn = hyper::server::conn::http1::Builder::new()
        .timer(hyper_util::rt::TokioTimer::new())
        .header_read_timeout(limits.header_timeout)
        .serve_connection(hyper_util::rt::TokioIo::new(io), service);
    let mut conn = std::pin::pin!(conn);
    let res = tokio::select! {
        res = conn.as_mut() => res,
        () = async { drop(closing.wait_for(|closing| *closing).await) } => {
            conn.as_mut().graceful_shutdown();
            conn.await
        }
    };
    if let Err(e) = res {
        log::error!("HTTP connection error: {}", chain(e));
    }
}

/// How long a closing connection has to write what it holds.
const CLOSE_GRACE: Duration = Duration::from_secs(5);

/// The connections accepted since the last close, and their signal to
/// close.
#[derive(Debug)]
struct Generation {
    set: JoinSet<()>,
    closing: watch::Sender<bool>,
}

impl Default for Generation {
    fn default() -> Self {
        Self { set: JoinSet::new(), closing: watch::Sender::new(false) }
    }
}

/// The connections a socket has accepted: they outlive a restart on the
/// socket and close with the server's sleep or the socket.
#[derive(Debug, Clone, Default)]
struct Conns(Arc<Mutex<Generation>>);

impl Conns {
    fn spawn<F: Future<Output = ()> + Send + 'static>(
        &self,
        f: impl FnOnce(watch::Receiver<bool>) -> F,
    ) {
        let mut g = self.0.lock();
        let closing = g.closing.subscribe();
        g.set.spawn(f(closing));
    }

    fn reap(&self) {
        while self.0.lock().set.try_join_next().is_some() {}
    }

    /// Every connection answers the requests it holds and closes; one
    /// still open after `CLOSE_GRACE` is aborted.
    fn close(&self) {
        let Generation { mut set, closing } = std::mem::take(&mut *self.0.lock());
        closing.send_replace(true);
        if let Ok(rt) = tokio::runtime::Handle::try_current() {
            rt.spawn(async move {
                let drained = async { while set.join_next().await.is_some() {} };
                let _ = tokio::time::timeout(CLOSE_GRACE, drained).await;
            });
        }
    }
}

/// How long the server waits after a failed accept before the next.
const ACCEPT_RETRY: Duration = Duration::from_millis(100);

/// Accept connections until aborted.
async fn serve_loop(
    listener: tokio::net::TcpListener,
    conns: Conns,
    tls: Option<tokio_rustls::TlsAcceptor>,
    post: Post,
    max_connections: Arc<Semaphore>,
    limits: Limits,
) {
    loop {
        conns.reap();
        let Ok(permit) = max_connections.clone().acquire_owned().await else { return };
        let stream = match listener.accept().await {
            Ok((stream, _)) => stream,
            Err(e) => {
                log::error!("HTTP accept error: {e}");
                drop(permit);
                tokio::time::sleep(ACCEPT_RETRY).await;
                continue;
            }
        };
        let (tls, post) = (tls.clone(), post.clone());
        conns.spawn(|closing| async move {
            let _permit = permit;
            serve_connection(stream, tls, post, limits, closing).await
        });
    }
}

/// What a connection may take: a request's body, at most `max_body`
/// bytes in `body_timeout`, and its headers in `header_timeout`, counted
/// from when it may send them (a new connection, or the last response),
/// so a connection idle that long between requests is closed.
#[derive(Debug, Clone, Copy)]
struct Limits {
    max_body: usize,
    header_timeout: Duration,
    body_timeout: Duration,
}

/// The server's settings, as its arguments give them.
struct Settings<'a> {
    addr: &'a ArcStr,
    tls: Option<tokio_rustls::TlsAcceptor>,
    max_connections: usize,
    limits: Limits,
}

impl<'a> Settings<'a> {
    fn of(addr: &'a ArcStr, args: [&Value; 6]) -> std::result::Result<Self, Value> {
        let [cert, key, max_connections, max_body, header_timeout, body_timeout] = args;
        let tls = match (cert, key) {
            (Value::Bytes(cert), Value::Bytes(key)) => {
                Some(build_tls_acceptor(cert, key)?)
            }
            (Value::Null, Value::Null) => None,
            _ => return Err(http_err("both cert and key must be provided for TLS")),
        };
        let int = |v: &Value| match v {
            Value::I64(n) => Ok(*n),
            v => Err(http_err(format_args!("expected an integer, got {v}"))),
        };
        let duration = |v: &Value| match v {
            Value::Duration(d) => Ok(**d),
            v => Err(http_err(format_args!("expected a duration, got {v}"))),
        };
        let n = int(max_connections)?;
        let max_connections = usize::try_from(n)
            .ok()
            .filter(|n| (1..=Semaphore::MAX_PERMITS).contains(n))
            .ok_or_else(|| {
                let max = Semaphore::MAX_PERMITS;
                http_err(format_args!("max_connections must be in 1..={max}, got {n}"))
            })?;
        let n = int(max_body)?;
        let max_body = usize::try_from(n).map_err(|_| {
            http_err(format_args!("max_body must be at least 0, got {n}"))
        })?;
        let limits = Limits {
            max_body,
            header_timeout: duration(header_timeout)?,
            body_timeout: duration(body_timeout)?,
        };
        Ok(Self { addr, tls, max_connections, limits })
    }
}

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
        self.conns.close()
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
    slept: WakeBit,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for HttpServe<R, E> {
    const NAME: &str = "http_serve";
    const RESUMES: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let [_, _, _, _, _, _, _, _] = from else { bail!("expected eight arguments") };
        let typ = resolved.unwrap_or(typ);
        let handler = Handler::new(ctx, &typ.args[7].typ, scope, top_id)?;
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(HttpServe {
            var,
            handler,
            inbox: Arc::default(),
            listening: None,
            abort: None,
            slept: WakeBit::default(),
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
            slept: WakeBit::default(),
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
        let serve = serve_loop(listener, conns, s.tls, post, semaphore, s.limits);
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
        let woke = self.slept.take();
        let (addr, mut fired) = seam_arg(ctx, &mut from[0]);
        fired |= woke;
        let mut args: [Option<Value>; 6] = Default::default();
        for (i, arg) in args.iter_mut().enumerate() {
            let (v, f) = seam_arg(ctx, &mut from[i + 1]);
            *arg = v;
            fired |= f;
        }
        let (f, f_fired) = seam_arg(ctx, &mut from[7]);
        self.handler.set_fn(ctx, f, f_fired);
        let mut started = None;
        if fired {
            self.stop();
            // a bottom argument bottoms the call, and no server runs
            let [a, b, c, d, e, g] = &args;
            started = Some(match (&addr, a, b, c, d, e, g) {
                (
                    Some(Value::String(addr)),
                    Some(a),
                    Some(b),
                    Some(c),
                    Some(d),
                    Some(e),
                    Some(g),
                ) => {
                    let s = Settings::of(addr, [a, b, c, d, e, g]);
                    Some(s.and_then(|s| self.start(ctx, s)).unwrap_or_else(|e| e))
                }
                _ => None,
            });
        }
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
        self.inbox.close();
        self.handler.delete(ctx);
        self.listening = None;
    }

    /// The server stops, its pending requests answered 500 and its
    /// connections closed, and starts again at the wake on the socket it
    /// keeps.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.var.remint(ctx);
        self.stop();
        // XCR claude for claude: [bug/medium] mod.gxi promises that a request the server
        // drops because it sleeps or is deleted is answered 500. But abort_all here,
        // and in Listening::drop on delete, kills each connection task before its
        // handle_http_request sees the dropped reply sender, so the client gets a
        // closed connection with no status line. Drop the reply senders first, then ask
        // each connection to close gracefully (a per-Listening shutdown signal that
        // serve_connection selects on to call hyper's graceful_shutdown, awaited with a
        // short bound), and keep abort_all only as the fallback. Otherwise change the
        // gxi to say what happens. probe:
        // design/review-2026-10-26/repro/http-sqlite-db1-serve-sleep-no-500.sh
        // (http-sqlite-db1-02)
        // 2026-10-26 claude: Inbox::close drops queued requests and refuses later posts
        // (each dropped reply sender answers 500); sleep and delete close the inbox and
        // drop the handler's requests, then Conns::close signals the connection
        // generation (a watch flag serve_connection selects on to call hyper's
        // graceful_shutdown) and aborts what is still open after CLOSE_GRACE (5 s) in a
        // task; a restart on another address closes the old connections the same way.
        // Pin: lib_tests::http::http_request_held_at_the_sleep_is_500 (gets a closed
        // connection with the abort_all restored).
        self.inbox.close();
        self.inbox = Arc::default();
        self.handler.sleep(ctx);
        if let Some(l) = &self.listening {
            l.conns.close();
        }
        self.slept.set();
        self.out = TagValue::phantom();
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
