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
    ImageState, seam_arg,
};
use graphix_rt::GXRt;
use http_body_util::{BodyExt, Full, LengthLimitError, Limited};
use hyper::StatusCode;
use netidx_core::pack::{Pack, PackError};
use netidx_derive::{FromValue, IntoValue};
use netidx_value::{FromValue, ValArray, Value};
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{
    any::Any,
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

fn abstract_arg<T: Any + Send + Sync>(cached: &CachedVals, idx: usize) -> Option<&T> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => a.downcast_ref::<T>(),
        _ => None,
    }
}

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
        let default_headers = cached.0.get(1)?.as_ref()?;
        let redirect_limit = cached.get::<u32>(2)?;
        let ca_cert = cached.get::<Option<Bytes>>(3)?;
        cached.0.get(4)?.as_ref()?;
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
        cached.0.first()?.as_ref()?;
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
        let sv = abstract_arg::<ServerValue>(cached, 0)?;
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

    async fn read(resp: reqwest::Response) -> reqwest::Result<ArcStr> {
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
            headers: cached.0.get(1)?.as_ref()?.clone(),
            body: cached.get::<Option<B>>(2)?,
            timeout: cached.get::<Option<Duration>>(3)?,
            client: abstract_arg::<ClientValue>(cached, 4)?.inner.clone(),
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

struct HttpReqEvent {
    request: Value,
    reply: Option<oneshot::Sender<Value>>,
}

impl fmt::Debug for HttpReqEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HttpReqEvent")
            .field("request", &self.request)
            .field("reply", &self.reply.is_some())
            .finish()
    }
}

impl CustomBuiltinType for HttpReqEvent {}

type Batches = mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>;

fn text_response(
    status: StatusCode,
    msg: impl fmt::Display,
) -> hyper::Response<Full<Bytes>> {
    let mut response = hyper::Response::new(Full::new(Bytes::from(msg.to_string())));
    *response.status_mut() = status;
    response
}

fn build_hyper_response(v: &Value) -> hyper::Response<Full<Bytes>> {
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
    mut tx: Batches,
    id: BindId,
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
    let raw = match Limited::new(body, max_body).collect().await {
        Ok(b) => b.to_bytes(),
        Err(e) if e.is::<LengthLimitError>() => {
            return text_response(StatusCode::PAYLOAD_TOO_LARGE, "Payload Too Large");
        }
        Err(e) => return text_response(StatusCode::BAD_REQUEST, e),
    };
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
    let mut batch = CBATCH_POOL.take();
    batch.push((
        id,
        Box::new(HttpReqEvent { request, reply: Some(reply_tx) })
            as Box<dyn CustomBuiltinType>,
    ));
    if tx.send(batch).await.is_err() {
        return text_response(StatusCode::SERVICE_UNAVAILABLE, "Service Unavailable");
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

const TLS_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(10);

/// One connection: its TLS handshake, if any, then its requests.
async fn serve_connection(
    stream: tokio::net::TcpStream,
    tls: Option<tokio_rustls::TlsAcceptor>,
    tx: Batches,
    id: BindId,
    max_body: usize,
) {
    let io = match tls {
        None => MaybeTls::Plain(stream),
        Some(acceptor) => {
            match tokio::time::timeout(TLS_HANDSHAKE_TIMEOUT, acceptor.accept(stream))
                .await
            {
                Ok(Ok(s)) => MaybeTls::Tls(s),
                Ok(Err(e)) => return log::error!("TLS handshake error: {e}"),
                Err(_) => return log::error!("TLS handshake timed out"),
            }
        }
    };
    let service = hyper::service::service_fn(|req| {
        let resp = handle_http_request(req, tx.clone(), id, max_body);
        async move { Ok::<_, std::convert::Infallible>(resp.await) }
    });
    if let Err(e) = hyper::server::conn::http1::Builder::new()
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
    tx: Batches,
    id: BindId,
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
        let (tls, tx) = (tls.clone(), tx.clone());
        conns.0.lock().spawn(async move {
            let _permit = permit;
            serve_connection(stream, tls, tx, id, max_body).await
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
        cert: &Option<Value>,
        key: &Option<Value>,
        max_connections: &Option<Value>,
        max_body: &Option<Value>,
    ) -> std::result::Result<Self, Value> {
        let tls = match (cert, key) {
            (Some(Value::Bytes(cert)), Some(Value::Bytes(key))) => {
                Some(build_tls_acceptor(cert, key)?)
            }
            (Some(Value::Null) | None, Some(Value::Null) | None) => None,
            _ => return Err(http_err("both cert and key must be provided for TLS")),
        };
        let max_connections = match max_connections {
            Some(Value::I64(n)) => usize::try_from(*n)
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
            Some(Value::I64(n)) => usize::try_from(*n).map_err(|_| {
                http_err(format_args!("max_body must be at least 0, got {n}"))
            })?,
            _ => DEFAULT_MAX_BODY,
        };
        Ok(Self { addr, tls, max_connections, max_body })
    }
}

const DEFAULT_MAX_BODY: usize = 16 * 1024 * 1024;

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
    id: BindId,
    top_id: ExprId,
    handler: Handler<R, E, oneshot::Sender<Value>>,
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
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(HttpServe {
            id,
            top_id,
            handler,
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
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let handler = Handler::image_decode(ctx, buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(HttpServe {
            id,
            top_id,
            handler,
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
        let std_listener = l
            .socket
            .try_clone()
            .map_err(|e| http_err(format_args!("bind to {} failed: {e}", s.addr)))?;
        let addr = std_listener
            .local_addr()
            .map_err(|e| http_err(format_args!("local_addr failed: {e}")))?;
        let listener = tokio::net::TcpListener::from_std(std_listener)
            .map_err(|e| http_err(format_args!("tokio listener failed: {e}")))?;
        let (tx, rx) = mpsc::channel(100);
        ctx.rt.watch(rx);
        let semaphore = Arc::new(Semaphore::new(s.max_connections));
        let serve =
            serve_loop(listener, conns, s.tls, tx, self.id, semaphore, s.max_body);
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
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
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
            if let Some(Value::String(addr)) = &addr {
                let s = Settings::of(addr, &cert, &key, &max_conns, &max_body);
                started = Some(s.and_then(|s| self.start(ctx, s)).unwrap_or_else(|e| e));
            }
        }
        if let Some(mut cbt) = ctx.event.take_custom(&self.id)
            && let Some(req) = (&mut *cbt as &mut dyn Any).downcast_mut::<HttpReqEvent>()
            && let Some(reply) = req.reply.take()
        {
            self.handler.push(req.request.clone(), reply);
        }
        self.handler.update(ctx);
        match started {
            Some(v) => self.out.set(TagValue::fired(v)),
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
        ctx.release_var(self.id, self.top_id);
        self.stop();
        self.listening = None;
        self.handler.delete(ctx);
    }

    /// The server stops, its connections and pending requests dropped, and
    /// starts again at the wake on the socket it keeps.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.stop();
        if let Some(l) = &self.listening {
            l.conns.0.lock().abort_all();
        }
        self.slept = true;
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
