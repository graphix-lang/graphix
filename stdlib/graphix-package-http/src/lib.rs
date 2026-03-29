#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use bytes::Bytes;
use compact_str::format_compact;
use futures::{channel::mpsc, SinkExt};
use graphix_compiler::{
    errf,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, CustomBuiltinType, Event, ExecCtx, LambdaId, Node, Rt, Scope,
    UserEvent, CBATCH_POOL,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use graphix_rt::GXRt;
use netidx_value::{
    abstract_type::AbstractWrapper, Abstract, FromValue, PBytes, ValArray, Value,
};
use std::{
    any::Any,
    cmp::Ordering,
    collections::VecDeque,
    fmt,
    hash::{Hash, Hasher},
    pin::Pin,
    sync::{Arc, LazyLock},
    task::{Context, Poll},
    time::Duration,
};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};

// ── Abstract ClientValue ─────────────────────────────────────────

#[derive(Debug, Clone)]
struct ClientValue {
    client: Arc<reqwest::Client>,
}

impl PartialEq for ClientValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.client, &other.client)
    }
}

impl Eq for ClientValue {}

impl PartialOrd for ClientValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ClientValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.client).cmp(&Arc::as_ptr(&other.client))
    }
}

impl Hash for ClientValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.client).hash(state)
    }
}

graphix_package_core::impl_no_pack!(ClientValue);

static CLIENT_WRAPPER: LazyLock<AbstractWrapper<ClientValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xc7, 0xd8, 0xe9, 0xfa, 0x0b, 0x1c, 0x4d, 0x2e, 0x3f, 0x40, 0x51, 0x62, 0x73,
        0x84, 0x95, 0xa6,
    ]);
    Abstract::register::<ClientValue>(id).expect("failed to register ClientValue")
});

fn get_client(cached: &CachedVals, idx: usize) -> Option<Arc<reqwest::Client>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let cv = a.downcast_ref::<ClientValue>()?;
            Some(cv.client.clone())
        }
        _ => None,
    }
}

// ── Abstract ServerValue ─────────────────────────────────────────

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
    handle: Arc<ServerHandle>,
}

impl PartialEq for ServerValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.handle, &other.handle)
    }
}

impl Eq for ServerValue {}

impl PartialOrd for ServerValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ServerValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.handle).cmp(&Arc::as_ptr(&other.handle))
    }
}

impl Hash for ServerValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.handle).hash(state)
    }
}

graphix_package_core::impl_no_pack!(ServerValue);

static SERVER_WRAPPER: LazyLock<AbstractWrapper<ServerValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd7, 0xe8, 0xf9, 0x0a, 0x1b, 0x2c, 0x4d, 0x3e, 0x4f, 0x50, 0x61, 0x72, 0x83,
        0x94, 0xa5, 0xb6,
    ]);
    Abstract::register::<ServerValue>(id).expect("failed to register ServerValue")
});

// ── Shared helpers ───────────────────────────────────────────────

fn value_to_header_map(v: &Value) -> reqwest::header::HeaderMap {
    let mut map = reqwest::header::HeaderMap::new();
    if let Value::Array(arr) = v {
        for pair in arr.iter() {
            if let Value::Array(p) = pair {
                if p.len() == 2 {
                    if let (Value::String(k), Value::String(v)) = (&p[0], &p[1]) {
                        if let (Ok(name), Ok(val)) = (
                            reqwest::header::HeaderName::from_bytes(k.as_bytes()),
                            reqwest::header::HeaderValue::from_str(v),
                        ) {
                            map.append(name, val);
                        }
                    }
                }
            }
        }
    }
    map
}

fn headers_to_value<'a>(
    iter: impl Iterator<
        Item = (&'a hyper::header::HeaderName, &'a hyper::header::HeaderValue),
    >,
) -> Value {
    let v: Vec<Value> = iter
        .map(|(k, v)| {
            Value::Array(ValArray::from([
                Value::String(ArcStr::from(k.as_str())),
                Value::String(ArcStr::from(v.to_str().unwrap_or(""))),
            ]))
        })
        .collect();
    Value::Array(ValArray::from(v))
}

fn build_response(body: ArcStr, headers: Value, status: u16, url: ArcStr) -> Value {
    let r: [(ArcStr, Value); 4] = [
        (literal!("body"), Value::String(body)),
        (literal!("headers"), headers),
        (literal!("status"), Value::U16(status)),
        (literal!("url"), Value::String(url)),
    ];
    r.into()
}

fn build_bin_response(body: Bytes, headers: Value, status: u16, url: ArcStr) -> Value {
    let r: [(ArcStr, Value); 4] = [
        (literal!("body"), Value::Bytes(PBytes::new(body))),
        (literal!("headers"), headers),
        (literal!("status"), Value::U16(status)),
        (literal!("url"), Value::String(url)),
    ];
    r.into()
}

fn parse_method(s: &str) -> std::result::Result<reqwest::Method, String> {
    match s {
        "GET" => Ok(reqwest::Method::GET),
        "POST" => Ok(reqwest::Method::POST),
        "PUT" => Ok(reqwest::Method::PUT),
        "DELETE" => Ok(reqwest::Method::DELETE),
        "PATCH" => Ok(reqwest::Method::PATCH),
        "HEAD" => Ok(reqwest::Method::HEAD),
        "OPTIONS" => Ok(reqwest::Method::OPTIONS),
        other => Err(format!("unknown HTTP method: {other}")),
    }
}

static DEFAULT_CLIENT: LazyLock<Arc<reqwest::Client>> = LazyLock::new(|| {
    Arc::new(
        reqwest::Client::builder().build().expect("failed to create default HTTP client"),
    )
});

// ── HttpClient ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct HttpClientEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpClientEv {
    const NAME: &str = "http_client";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let timeout = cached.get::<Option<Duration>>(0)?;
        let default_headers = cached.0.get(1)?.as_ref()?.clone();
        let redirect_limit = cached.get::<u32>(2)?;
        let ca_cert = cached.get::<Option<Bytes>>(3)?;
        let _ = cached.0.get(4)?.as_ref()?;
        let mut builder = reqwest::Client::builder();
        if let Some(timeout) = timeout {
            builder = builder.timeout(timeout);
        }
        builder =
            builder.redirect(reqwest::redirect::Policy::limited(redirect_limit as usize));
        let headers = value_to_header_map(&default_headers);
        if !headers.is_empty() {
            builder = builder.default_headers(headers);
        }
        if let Some(ca_cert) = &ca_cert {
            let cert = match reqwest::Certificate::from_pem(ca_cert) {
                Ok(c) => c,
                Err(e) => return Some(errf!("HTTPError", "invalid ca_cert PEM: {e}")),
            };
            builder = builder.add_root_certificate(cert);
        }
        Some(match builder.build() {
            Ok(client) => CLIENT_WRAPPER.wrap(ClientValue { client: Arc::new(client) }),
            Err(e) => errf!("HTTPError", "failed to build client: {e}"),
        })
    }
}

pub(crate) type HttpClient = CachedArgs<HttpClientEv>;

// ── HttpDefaultClient ────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct HttpDefaultClientEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpDefaultClientEv {
    const NAME: &str = "http_default_client";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        cached.0.get(0)?.as_ref()?;
        Some(CLIENT_WRAPPER.wrap(ClientValue { client: DEFAULT_CLIENT.clone() }))
    }
}

pub(crate) type HttpDefaultClient = CachedArgs<HttpDefaultClientEv>;

// ── HttpServerAddr ──────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct HttpServerAddrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HttpServerAddrEv {
    const NAME: &str = "http_server_addr";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let v = cached.0.get(0)?.as_ref()?;
        match v {
            Value::Abstract(a) => {
                let sv = a.downcast_ref::<ServerValue>()?;
                Some(Value::String(ArcStr::from(sv.handle.addr.to_string().as_str())))
            }
            _ => None,
        }
    }
}

pub(crate) type HttpServerAddr = CachedArgs<HttpServerAddrEv>;

// ── HttpRequest / HttpRequestBin ─────────────────────────────────

#[derive(Debug)]
pub(crate) struct RequestArgs<B> {
    method: ArcStr,
    headers: Value,
    body: Option<B>,
    timeout: Option<Duration>,
    client: Arc<reqwest::Client>,
    url: ArcStr,
}

fn prepare_request_args<B: FromValue>(cached: &CachedVals) -> Option<RequestArgs<B>> {
    let method = cached.get::<ArcStr>(0)?;
    let headers = cached.0.get(1)?.as_ref()?.clone();
    let body = cached.get::<Option<B>>(2)?;
    let timeout = cached.get::<Option<Duration>>(3)?;
    let client = get_client(cached, 4)?;
    let url = cached.get::<ArcStr>(5)?;
    Some(RequestArgs { method, headers, body, timeout, client, url })
}

async fn send_request(
    method: &str,
    client: &reqwest::Client,
    url: &str,
    headers: &Value,
    body: Option<reqwest::Body>,
    timeout: Option<Duration>,
) -> std::result::Result<reqwest::Response, Value> {
    let method = parse_method(method).map_err(|e| errf!("HTTPError", "{e}"))?;
    let mut req = client.request(method, url);
    let hdrs = value_to_header_map(headers);
    if !hdrs.is_empty() {
        req = req.headers(hdrs);
    }
    if let Some(body) = body {
        req = req.body(body);
    }
    if let Some(timeout) = timeout {
        req = req.timeout(timeout);
    }
    req.send().await.map_err(|e| errf!("HTTPError", "request failed: {e}"))
}

#[derive(Debug, Default)]
pub(crate) struct HttpRequestEv;

impl EvalCachedAsync for HttpRequestEv {
    const NAME: &str = "http_request";
    const NEEDS_CALLSITE: bool = false;
    type Args = RequestArgs<ArcStr>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        prepare_request_args(cached)
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let resp = match send_request(
                &args.method,
                &args.client,
                &args.url,
                &args.headers,
                args.body.map(|s| reqwest::Body::from(s.to_string())),
                args.timeout,
            )
            .await
            {
                Ok(r) => r,
                Err(e) => return e,
            };
            let status = resp.status().as_u16();
            let url = ArcStr::from(resp.url().as_str());
            let hdrs = headers_to_value(resp.headers().iter());
            match resp.text().await {
                Ok(body) => {
                    build_response(ArcStr::from(body.as_str()), hdrs, status, url)
                }
                Err(e) => errf!("HTTPError", "failed to read body: {e}"),
            }
        }
    }
}

pub(crate) type HttpRequest = CachedArgsAsync<HttpRequestEv>;

#[derive(Debug, Default)]
pub(crate) struct HttpRequestBinEv;

impl EvalCachedAsync for HttpRequestBinEv {
    const NAME: &str = "http_request_bin";
    const NEEDS_CALLSITE: bool = false;
    type Args = RequestArgs<Bytes>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        prepare_request_args(cached)
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let resp = match send_request(
                &args.method,
                &args.client,
                &args.url,
                &args.headers,
                args.body.map(reqwest::Body::from),
                args.timeout,
            )
            .await
            {
                Ok(r) => r,
                Err(e) => return e,
            };
            let status = resp.status().as_u16();
            let url = ArcStr::from(resp.url().as_str());
            let hdrs = headers_to_value(resp.headers().iter());
            match resp.bytes().await {
                Ok(body) => build_bin_response(body, hdrs, status, url),
                Err(e) => errf!("HTTPError", "failed to read body: {e}"),
            }
        }
    }
}

pub(crate) type HttpRequestBin = CachedArgsAsync<HttpRequestBinEv>;

// ── HttpServe (server) ───────────────────────────────────────────

struct HttpReqEvent {
    request: Value,
    reply: Option<tokio::sync::oneshot::Sender<Value>>,
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

fn build_request_value(
    body: Option<ArcStr>,
    headers: Value,
    method: ArcStr,
    path: ArcStr,
    query: Option<ArcStr>,
) -> Value {
    let r: [(ArcStr, Value); 5] = [
        (literal!("body"), body.map(Value::String).unwrap_or(Value::Null)),
        (literal!("headers"), headers),
        (literal!("method"), Value::String(method)),
        (literal!("path"), Value::String(path)),
        (literal!("query"), query.map(Value::String).unwrap_or(Value::Null)),
    ];
    r.into()
}

fn struct_field(v: &Value, idx: usize) -> Option<&Value> {
    match v {
        Value::Array(arr) => match arr.get(idx)? {
            Value::Array(pair) if pair.len() == 2 => Some(&pair[1]),
            _ => None,
        },
        _ => None,
    }
}

fn build_hyper_response(
    v: &Value,
) -> std::result::Result<
    hyper::Response<http_body_util::Full<Bytes>>,
    std::convert::Infallible,
> {
    if let Value::Error(e) = v {
        return Ok(hyper::Response::builder()
            .status(500)
            .body(http_body_util::Full::new(Bytes::from(format!("{e}"))))
            .unwrap());
    }
    // Response fields (alphabetical): body(0), headers(1), status(2), url(3)
    let body = match struct_field(v, 0) {
        Some(Value::String(s)) => Bytes::from(s.to_string()),
        _ => Bytes::new(),
    };
    let status = match struct_field(v, 2) {
        Some(Value::U16(s)) => *s,
        _ => 200,
    };
    let mut response = hyper::Response::builder().status(status);
    if let Some(Value::Array(hdrs)) = struct_field(v, 1) {
        for h in hdrs.iter() {
            if let Value::Array(pair) = h {
                if pair.len() == 2 {
                    if let (Value::String(k), Value::String(v)) = (&pair[0], &pair[1]) {
                        response = response.header(&**k, &**v);
                    }
                }
            }
        }
    }
    Ok(response.body(http_body_util::Full::new(body)).unwrap())
}

async fn handle_http_request(
    req: hyper::Request<hyper::body::Incoming>,
    mut tx: mpsc::Sender<
        poolshark::global::GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
    >,
    id: BindId,
) -> std::result::Result<
    hyper::Response<http_body_util::Full<Bytes>>,
    std::convert::Infallible,
> {
    use http_body_util::BodyExt;
    let (parts, body) = req.into_parts();
    let body_bytes = match body.collect().await {
        Ok(b) => b.to_bytes(),
        Err(_) => Bytes::new(),
    };
    let method = ArcStr::from(parts.method.as_str());
    let path = ArcStr::from(parts.uri.path());
    let query = parts.uri.query().map(ArcStr::from);
    let hdrs = headers_to_value(parts.headers.iter());
    let body_str = if body_bytes.is_empty() {
        None
    } else {
        match std::str::from_utf8(&body_bytes) {
            Ok(s) => Some(ArcStr::from(s)),
            Err(_) => None,
        }
    };
    let request_value = build_request_value(body_str, hdrs, method, path, query);
    let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
    let mut batch = CBATCH_POOL.take();
    batch.push((
        id,
        Box::new(HttpReqEvent { request: request_value, reply: Some(reply_tx) })
            as Box<dyn CustomBuiltinType>,
    ));
    if tx.send(batch).await.is_err() {
        return Ok(hyper::Response::builder()
            .status(503)
            .body(http_body_util::Full::new(Bytes::from("Service Unavailable")))
            .unwrap());
    }
    match reply_rx.await {
        Ok(resp_value) => build_hyper_response(&resp_value),
        Err(_) => Ok(hyper::Response::builder()
            .status(500)
            .body(http_body_util::Full::new(Bytes::from("Internal Server Error")))
            .unwrap()),
    }
}

fn build_tls_acceptor(
    cert_pem: &[u8],
    key_pem: &[u8],
) -> std::result::Result<tokio_rustls::TlsAcceptor, Value> {
    let certs: Vec<_> = rustls_pemfile::certs(&mut &*cert_pem)
        .collect::<std::result::Result<_, _>>()
        .map_err(|e| errf!("HTTPError", "invalid cert PEM: {e}"))?;
    let key = rustls_pemfile::private_key(&mut &*key_pem)
        .map_err(|e| errf!("HTTPError", "invalid key PEM: {e}"))?
        .ok_or_else(|| errf!("HTTPError", "no private key found in key PEM"))?;
    let config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| errf!("HTTPError", "TLS config error: {e}"))?;
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

async fn serve_loop(
    listener: tokio::net::TcpListener,
    tls: Option<tokio_rustls::TlsAcceptor>,
    tx: mpsc::Sender<
        poolshark::global::GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
    >,
    id: BindId,
    max_connections: Arc<tokio::sync::Semaphore>,
) {
    loop {
        let permit = match max_connections.clone().acquire_owned().await {
            Ok(p) => p,
            Err(_) => return, // semaphore closed
        };
        let (stream, _) = match listener.accept().await {
            Ok(conn) => conn,
            Err(e) => {
                log::error!("HTTP accept error: {e}");
                continue;
            }
        };
        let io = match &tls {
            None => MaybeTls::Plain(stream),
            Some(acceptor) => match acceptor.accept(stream).await {
                Ok(tls_stream) => MaybeTls::Tls(tls_stream),
                Err(e) => {
                    log::error!("TLS handshake error: {e}");
                    continue;
                }
            },
        };
        let io = hyper_util::rt::TokioIo::new(io);
        let tx = tx.clone();
        tokio::spawn(async move {
            let _permit = permit;
            let service = hyper::service::service_fn(|req| {
                handle_http_request(req, tx.clone(), id)
            });
            if let Err(e) = hyper::server::conn::http1::Builder::new()
                .serve_connection(io, service)
                .await
            {
                log::error!("HTTP connection error: {e}");
            }
        });
    }
}

#[derive(Debug)]
pub(crate) struct HttpServe<R: Rt, E: UserEvent> {
    args: CachedVals,
    id: BindId,
    top_id: ExprId,
    handler: Node<R, E>,
    pid: BindId,
    x: BindId,
    queue: VecDeque<(Value, Option<tokio::sync::oneshot::Sender<Value>>)>,
    ready: bool,
    abort: Option<tokio::task::AbortHandle>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for HttpServe<R, E> {
    const NAME: &str = "http_serve";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                let pid = BindId::new();
                let mftyp = match &typ.args[4].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                };
                let (x, xn) = genn::bind(
                    ctx,
                    &scope.lexical,
                    "x",
                    mftyp.args[0].typ.clone(),
                    top_id,
                );
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let handler = genn::apply(fnode, scope, vec![xn], &mftyp, top_id);
                Ok(Box::new(HttpServe {
                    args: CachedVals::new(from),
                    id,
                    top_id,
                    handler,
                    pid,
                    x,
                    queue: VecDeque::new(),
                    ready: true,
                    abort: None,
                }))
            }
            _ => bail!("expected five arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for HttpServe<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut changed = [false; 5];
        self.args.update_diff(&mut changed, ctx, from, event);
        // update handler function reference
        if changed[4] {
            if let Some(v) = self.args.0[4].clone() {
                ctx.cached.insert(self.pid, v.clone());
                event.variables.insert(self.pid, v);
            }
        }
        // start/restart server when addr/cert/key/max_connections changes
        let mut server_result = None;
        if changed[0] || changed[1] || changed[2] || changed[3] {
            if let Some(abort) = self.abort.take() {
                abort.abort();
            }
            if let Some(Value::String(addr)) = &self.args.0[0] {
                // build TLS acceptor if cert and key are provided
                let tls = match (&self.args.0[1], &self.args.0[2]) {
                    (Some(Value::Bytes(cert)), Some(Value::Bytes(key))) => {
                        match build_tls_acceptor(cert, key) {
                            Ok(a) => Some(a),
                            Err(e) => return Some(e),
                        }
                    }
                    (Some(Value::Null), Some(Value::Null))
                    | (None, None)
                    | (Some(Value::Null), None)
                    | (None, Some(Value::Null)) => None,
                    _ => {
                        return Some(errf!(
                            "HTTPError",
                            "both cert and key must be provided for TLS"
                        ))
                    }
                };
                let max_conn = match &self.args.0[3] {
                    Some(Value::I64(n)) if *n > 0 => *n as usize,
                    Some(Value::I64(n)) => {
                        return Some(errf!(
                            "HTTPError",
                            "max_connections must be > 0, got {n}"
                        ))
                    }
                    _ => 768,
                };
                let std_listener = match std::net::TcpListener::bind(&**addr) {
                    Ok(l) => l,
                    Err(e) => {
                        return Some(errf!("HTTPError", "bind to {addr} failed: {e}"))
                    }
                };
                let bound_addr = match std_listener.local_addr() {
                    Ok(a) => a,
                    Err(e) => return Some(errf!("HTTPError", "local_addr failed: {e}")),
                };
                if let Err(e) = std_listener.set_nonblocking(true) {
                    return Some(errf!("HTTPError", "set_nonblocking failed: {e}"));
                }
                let listener = match tokio::net::TcpListener::from_std(std_listener) {
                    Ok(l) => l,
                    Err(e) => {
                        return Some(errf!("HTTPError", "tokio listener failed: {e}"))
                    }
                };
                let (tx, rx) = mpsc::channel(100);
                ctx.rt.watch(rx);
                let id = self.id;
                let semaphore = Arc::new(tokio::sync::Semaphore::new(max_conn));
                let handle = tokio::spawn(serve_loop(listener, tls, tx, id, semaphore));
                let abort = handle.abort_handle();
                self.abort = Some(abort.clone());
                server_result = Some(SERVER_WRAPPER.wrap(ServerValue {
                    handle: Arc::new(ServerHandle { abort, addr: bound_addr }),
                }));
            }
        }
        // receive incoming requests from the server
        if let Some(mut cbt) = event.custom.remove(&self.id) {
            if let Some(req) = (&mut *cbt as &mut dyn Any).downcast_mut::<HttpReqEvent>()
            {
                let request = req.request.clone();
                let reply = req.reply.take();
                self.queue.push_back((request, reply));
            }
        }
        // set up first queued request for handler processing
        if self.ready && !self.queue.is_empty() {
            if let Some((req, _)) = self.queue.front() {
                self.ready = false;
                ctx.cached.insert(self.x, req.clone());
                event.variables.insert(self.x, req.clone());
            }
        }
        // process handler responses
        loop {
            match self.handler.update(ctx, event) {
                None => break,
                Some(v) => {
                    self.ready = true;
                    if let Some((_, reply)) = self.queue.pop_front() {
                        if let Some(reply) = reply {
                            let _ = reply.send(v);
                        }
                    }
                    match self.queue.front() {
                        Some((req, _)) => {
                            self.ready = false;
                            ctx.cached.insert(self.x, req.clone());
                            event.variables.insert(self.x, req.clone());
                        }
                        None => break,
                    }
                }
            }
        }
        server_result
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: graphix_compiler::TypecheckPhase<'_>,
    ) -> Result<()> {
        self.handler.typecheck(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.handler.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.cached.remove(&self.pid);
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
        self.args.clear();
        self.queue.clear();
        self.ready = true;
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
