#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, ExecCtx,
    LambdaId, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, seam_arg,
};
use graphix_rt::GXRt;
use http_body_util::Full;
use hyper::StatusCode;
use netidx_core::pack::{Pack, PackError};
use netidx_derive::{FromValue, IntoValue};
use netidx_value::{FromValue, ValArray, Value};
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

// CR claude for claude: [structure] ClientValue (41-77) and ServerValue (101-137)
// hand-write the PartialEq/Eq/PartialOrd/Ord/Hash-by-Arc-identity impls plus
// impl_no_pack!/abstract_wrapper! that impl_abstract_arc!(T, static W = "path")
// (graphix-package-core/src/lib.rs:175) generates, as the db and sqlite handles already
// use. Rename the fields to inner (or let the macro name the field) and replace each
// block with one macro call. graphix-package-sys hand-writes the same impls for
// TcpListenerValue (tcp.rs:22-46) and WatcherValue (watch.rs:161-185).
// (http-sqlite-db1-16)
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

graphix_package_core::abstract_wrapper!(
    ClientValue,
    static CLIENT_WRAPPER = "http::Client"
);

fn get_client(cached: &CachedVals, idx: usize) -> Option<Arc<reqwest::Client>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let cv = a.downcast_ref::<ClientValue>()?;
            Some(cv.client.clone())
        }
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

graphix_package_core::abstract_wrapper!(
    ServerValue,
    static SERVER_WRAPPER = "http::Server"
);

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

#[derive(IntoValue)]
struct Response<B> {
    body: B,
    headers: Value,
    status: u16,
    url: ArcStr,
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
        cached.0.get(0)?.as_ref()?;
        Some(CLIENT_WRAPPER.wrap(ClientValue { client: DEFAULT_CLIENT.clone() }))
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
    type Args = RequestArgs<ArcStr>;

    const NAME: &str = "http_request";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        prepare_request_args(cached)
    }

    // CR claude for claude: [structure] HttpRequestEv::eval and HttpRequestBinEv::eval
    // (388-411) differ only in the body conversion and text() vs bytes(): the
    // send_request call, status, url, headers and both error mappings are written
    // twice. Fold them into one helper that takes the body conversion and the body
    // read. The text body is also copied for nothing:
    // reqwest::Body::from(s.to_string()) (352) allocates and copies the ArcStr, where
    // Bytes::from_owner(s) (as build_hyper_response does at 478) hands it to reqwest
    // without a copy. (http-sqlite-db1-17)
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
            let headers = headers_to_value(resp.headers().iter());
            match resp.text().await {
                Ok(body) => {
                    let body = ArcStr::from(body.as_str());
                    Response { body, headers, status, url }.into()
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
    type Args = RequestArgs<Bytes>;

    const NAME: &str = "http_request_bin";

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
            let headers = headers_to_value(resp.headers().iter());
            match resp.bytes().await {
                Ok(body) => Response { body, headers, status, url }.into(),
                Err(e) => errf!("HTTPError", "failed to read body: {e}"),
            }
        }
    }
}

pub(crate) type HttpRequestBin = CachedArgsAsync<HttpRequestBinEv>;

graphix_package_core::unit_image_state!(
    HttpClientEv,
    HttpDefaultClientEv,
    HttpServerAddrEv,
    HttpRequestEv,
    HttpRequestBinEv
);

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

fn text_response(
    status: StatusCode,
    msg: impl fmt::Display,
) -> hyper::Response<Full<Bytes>> {
    let mut response = hyper::Response::new(Full::new(Bytes::from(msg.to_string())));
    *response.status_mut() = status;
    response
}

fn build_hyper_response(
    v: &Value,
) -> std::result::Result<hyper::Response<Full<Bytes>>, std::convert::Infallible> {
    if let Value::Error(e) = v {
        return Ok(text_response(StatusCode::INTERNAL_SERVER_ERROR, e));
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
            return Ok(text_response(StatusCode::INTERNAL_SERVER_ERROR, msg));
        }
    };
    let mut response = hyper::Response::builder().status(status);
    for h in headers.iter() {
        if let Value::Array(pair) = h {
            if pair.len() == 2 {
                if let (Value::String(k), Value::String(v)) = (&pair[0], &pair[1]) {
                    response = response.header(&**k, &**v);
                }
            }
        }
    }
    Ok(match response.body(Full::new(Bytes::from_owner(body))) {
        Ok(response) => response,
        Err(e) => {
            let msg = format_args!("invalid response: {e}");
            text_response(StatusCode::INTERNAL_SERVER_ERROR, msg)
        }
    })
}

async fn handle_http_request(
    req: hyper::Request<hyper::body::Incoming>,
    mut tx: mpsc::Sender<
        poolshark::global::GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
    >,
    id: BindId,
) -> std::result::Result<hyper::Response<Full<Bytes>>, std::convert::Infallible> {
    use http_body_util::BodyExt;
    let (parts, body) = req.into_parts();
    // CR claude for claude: [bug] A body read error becomes an empty body (498) and a
    // non-UTF-8 body becomes null (504-511), and the request is dispatched either way:
    // a POST that declares 100 bytes, sends 5 and closes runs the handler with body
    // null and is answered 200, and a binary upload cannot be received at all.
    // headers_to_value (169) turns every header value that is not visible ASCII into ""
    // (request headers here, response headers in http::request), and
    // value_to_header_map (146-151) drops an outgoing header it cannot encode, so an
    // Authorization value with a trailing newline is silently not sent. Answer 400 on a
    // body read error without dispatching, carry the body as bytes (or [string, bytes,
    // null]), decode header values lossily, and return HTTPError for an invalid
    // outgoing header. probe: design/review-2026-10-05/repro/http-sqlite-db1-14.gx
    // (http-sqlite-db1-14)
    // CR claude for claude: [risk] Every request body is read whole, with no size limit,
    // before the handler runs. There is no Content-Length check and no
    // http_body_util::Limited, and serve has no option for a limit, so a Graphix
    // handler cannot refuse an upload. The bytes are then copied into an ArcStr while
    // body_bytes lives until the reply, so a request costs about twice its body, and
    // each of the 768 default connections can hold one: a single large POST from any
    // client OOM-kills the process. The Err(_) => Bytes::new() arm also gives the
    // handler a body that never arrived (the client left mid-upload) as body: null.
    // probe: design/review-2026-10-05/repro/x-panics-14.py (a declared 100 GiB body
    // gets no 413; a 32 MiB POST raises peak RSS by 63 MiB; under an 80M cap one 36 MiB
    // POST OOM-kills the server). (x-panics-14)
    let body_bytes = match body.collect().await {
        Ok(b) => b.to_bytes(),
        Err(_) => Bytes::new(),
    };
    let method = ArcStr::from(parts.method.as_str());
    let path = ArcStr::from(parts.uri.path());
    let query = parts.uri.query().map(ArcStr::from);
    let headers = headers_to_value(parts.headers.iter());
    let body = if body_bytes.is_empty() {
        None
    } else {
        match std::str::from_utf8(&body_bytes) {
            Ok(s) => Some(ArcStr::from(s)),
            Err(_) => None,
        }
    };
    #[derive(IntoValue)]
    struct Fields {
        body: Option<ArcStr>,
        headers: Value,
        method: ArcStr,
        path: ArcStr,
        query: Option<ArcStr>,
    }
    let request_value = Fields { body, headers, method, path, query }.into();
    let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
    let mut batch = CBATCH_POOL.take();
    batch.push((
        id,
        Box::new(HttpReqEvent { request: request_value, reply: Some(reply_tx) })
            as Box<dyn CustomBuiltinType>,
    ));
    if tx.send(batch).await.is_err() {
        return Ok(text_response(StatusCode::SERVICE_UNAVAILABLE, "Service Unavailable"));
    }
    match reply_rx.await {
        Ok(resp_value) => build_hyper_response(&resp_value),
        Err(_) => {
            Ok(text_response(StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error"))
        }
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
            // CR claude for claude: [bug] The TLS handshake is awaited here, inside the
            // accept loop and with no timeout, so no other connection is accepted until
            // it finishes. One peer that opens a TCP connection to an HTTPS server and
            // sends nothing stalls every other client until it disconnects; plain HTTP
            // spawns at once and is unaffected. Each honest handshake also holds the
            // loop for its client's round trip. Run the handshake in the spawned
            // connection task under a timeout, so this loop only accepts TCP. probe:
            // design/review-2026-10-05/repro/x-panics-09.py (x-panics-09)
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
        // CR claude for claude: [bug] Each connection is a detached task that holds this
        // server's id and sender. The abort() on restart (773), delete (911), sleep
        // (924) and in ServerHandle::drop (97) stops only the accept loop, so open
        // keep-alive connections outlive the server. After a sleep or a delete, their
        // requests carry an id that no node takes. The cycle drops the reply channel,
        // and the client gets 500 on every request until it reconnects, even after the
        // arm has woken. After an address change, the handler keeps serving those
        // connections on the port the server left, and their channel stays watched for
        // as long as the clients keep their sockets open. probe:
        // design/review-2026-10-05/repro/http-sqlite-db1-08.gx (http-sqlite-db1-08)
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

// CR claude for claude: [structure] HttpServe is PublishRpc
// (graphix-package-sys/src/net.rs:872-1290) under another name: the same handler built
// by genn::bind/reference/apply in init, the same pid write, the same take_custom ->
// queue -> ready dispatch -> seam_tick reply loop, the same delete, and a sleep that
// differs only by PublishRpc's wake flag. The copies have already drifted (only
// PublishRpc republishes after a wake) and share the reply-pairing and wedge bugs, so
// every fix to the request/reply machine must be made twice. Move the queue, the
// handler instance, dispatch, reply and sleep/delete into one type in
// graphix-package-core that both builtins hold, so each keeps only its transport setup.
// (http-sqlite-db1-12)
#[derive(Debug)]
pub(crate) struct HttpServe<R: Rt, E: UserEvent> {
    id: BindId,
    top_id: ExprId,
    handler: Node<R, E>,
    pid: BindId,
    x: BindId,
    queue: VecDeque<(Value, Option<tokio::sync::oneshot::Sender<Value>>)>,
    ready: bool,
    abort: Option<tokio::task::AbortHandle>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for HttpServe<R, E> {
    const NAME: &str = "http_serve";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _, _, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope = scope.append_block("fn", LambdaId::new().inner());
                let id = BindId::new();
                ctx.record_ref(id, top_id);
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
                let handler =
                    genn::apply(fnode, scope, smallvec::smallvec![xn], &mftyp, top_id);
                Ok(Box::new(HttpServe {
                    id,
                    top_id,
                    handler,
                    pid,
                    x,
                    queue: VecDeque::new(),
                    ready: true,
                    abort: None,
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("expected five arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let handler = image::decode_node(ctx, buf)?;
        let pid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        let ready = bool::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(HttpServe {
            id,
            top_id,
            handler,
            pid,
            x,
            queue: VecDeque::new(),
            ready,
            abort: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for HttpServe<R, E> {
    /// `abort` is a running server; `queue` holds requests awaiting a
    /// reply.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.abort.is_some() || !self.queue.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.handler.image_encode(buf)?;
        self.pid.encode(buf)?;
        self.x.encode(buf)?;
        self.ready.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let (addrv, addr_fired) = seam_arg(ctx, &mut from[0]);
        let (certv, cert_fired) = seam_arg(ctx, &mut from[1]);
        let (keyv, key_fired) = seam_arg(ctx, &mut from[2]);
        let (maxv, max_fired) = seam_arg(ctx, &mut from[3]);
        let (fv, f_fired) = seam_arg(ctx, &mut from[4]);
        if f_fired && let Some(v) = fv {
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::fired(v));
        }
        let mut server_result = None;
        // CR claude for claude: [bug] Each start error in this block (781-832) returns
        // before the request intake (850) and the handler update (866), so that cycle's
        // work is lost. A request delivered in that cycle is cleared with the event,
        // and its client gets 500. An async reply the handler is waiting for (a db or
        // fs call) is cleared too: its CachedArgsAsync stays running, `ready` never
        // comes back, and the server answers nothing again, even after a later restart
        // succeeds. A restart on a fixed port takes this path today (EADDRINUSE: the
        // old listener is still open). Keep the error in a local, fall through, and set
        // `out` once at the end. probe: GRAPHIX_PAR=off graphix --no-cache
        // design/review-2026-10-05/repro/http-sqlite-db1-07.gx (http-sqlite-db1-07)
        // CR claude for claude: [bug] A wake never restarts the server that sleep()
        // aborted (line 920). This condition only looks at argument fires, and at a
        // wake an argument bound outside the arm is delivered stale. So an http::serve
        // in a select arm whose addr/cert/key/max_connections are all variables stays
        // down for good once the arm sleeps and is reselected. The same call with an
        // inline literal, or with the defaults left out, comes back, because the arm's
        // constants re-fire. Give HttpServe a `slept` bit set in sleep() and taken here
        // as a restart, as Subscribe/Publish/PublishRpc do
        // (graphix-package-sys/src/net.rs:1103, 1112, 1276). probe:
        // design/review-2026-10-05/repro/http-sqlite-db1-06.gx (http-sqlite-db1-06)
        if addr_fired || cert_fired || key_fired || max_fired {
            if let Some(abort) = self.abort.take() {
                abort.abort();
            }
            if let Some(Value::String(addr)) = &addrv {
                let tls = match (&certv, &keyv) {
                    (Some(Value::Bytes(cert)), Some(Value::Bytes(key))) => {
                        match build_tls_acceptor(cert, key) {
                            Ok(a) => Some(a),
                            Err(e) => return self.out.set(TagValue::fired(e)),
                        }
                    }
                    (Some(Value::Null), Some(Value::Null))
                    | (None, None)
                    | (Some(Value::Null), None)
                    | (None, Some(Value::Null)) => None,
                    _ => {
                        return self.out.set(TagValue::fired(errf!(
                            "HTTPError",
                            "both cert and key must be provided for TLS"
                        )));
                    }
                };
                let max_conn = match &maxv {
                    // CR claude for claude: [bug] Any positive `n` is accepted here, but
                    // `tokio::sync::Semaphore::new` (line 841) asserts `n <=
                    // Semaphore::MAX_PERMITS` (2^61 - 1 on 64-bit). So
                    // `#max_connections: 2305843009213693952` panics the runtime
                    // ('runtime did not respond') instead of returning an `HTTPError`.
                    // Reject values above `Semaphore::MAX_PERMITS` here as `<= 0` is
                    // rejected, or clamp to it. `usize::try_from` in place of `as
                    // usize` also covers the truncation on 32-bit targets. probe:
                    // design/review-2026-10-05/repro/x-panics-16.gx (x-panics-16)
                    Some(Value::I64(n)) if *n > 0 => *n as usize,
                    Some(Value::I64(n)) => {
                        return self.out.set(TagValue::fired(errf!(
                            "HTTPError",
                            "max_connections must be > 0, got {n}"
                        )));
                    }
                    _ => 768,
                };
                // CR claude for claude: [bug] Restarting on the same address fails. The
                // abort() above (773-775) only schedules the old serve_loop's
                // cancellation, and that future owns the old TcpListener until a worker
                // drops it later, so this bind meets a listening socket and returns
                // EADDRINUSE. The node then outputs the error and the server stays down
                // until an argument fires again, because the old task is gone a moment
                // later. Changing #max_connections, rotating #cert/#key, or any re-fire
                // of an unchanged #addr hits it (10 of 10 runs for #max_connections).
                // Keep the bound listener in the node and give the new loop a
                // try_clone() when the address is unchanged, or bind before aborting
                // the old loop. probe:
                // design/review-2026-10-05/repro/http-sqlite-db1-05.gx
                // (http-sqlite-db1-05)
                let std_listener = match std::net::TcpListener::bind(&**addr) {
                    Ok(l) => l,
                    Err(e) => {
                        return self.out.set(TagValue::fired(errf!(
                            "HTTPError",
                            "bind to {addr} failed: {e}"
                        )));
                    }
                };
                let bound_addr = match std_listener.local_addr() {
                    Ok(a) => a,
                    Err(e) => {
                        return self.out.set(TagValue::fired(errf!(
                            "HTTPError",
                            "local_addr failed: {e}"
                        )));
                    }
                };
                if let Err(e) = std_listener.set_nonblocking(true) {
                    return self.out.set(TagValue::fired(errf!(
                        "HTTPError",
                        "set_nonblocking failed: {e}"
                    )));
                }
                let listener = match tokio::net::TcpListener::from_std(std_listener) {
                    Ok(l) => l,
                    Err(e) => {
                        return self.out.set(TagValue::fired(errf!(
                            "HTTPError",
                            "tokio listener failed: {e}"
                        )));
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
        if let Some(mut cbt) = ctx.event.take_custom(&self.id) {
            if let Some(req) = (&mut *cbt as &mut dyn Any).downcast_mut::<HttpReqEvent>()
            {
                let request = req.request.clone();
                let reply = req.reply.take();
                self.queue.push_back((request, reply));
            }
        }
        // CR claude for claude: [bug] `ready` is cleared when a request goes to the
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
        if self.ready && !self.queue.is_empty() {
            if let Some((req, _)) = self.queue.front() {
                self.ready = false;
                ctx.rt.store_insert(self.x, TagValue::fired(req.clone()));
                ctx.event.variables.insert(self.x, TagValue::fired(req.clone()));
            }
        }
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
        loop {
            match graphix_package_core::seam_tick(self.handler.update(ctx))
                .map(|tv| tv.clone())
            {
                None => break,
                Some(v) => {
                    self.ready = true;
                    if let Some((_, reply)) = self.queue.pop_front() {
                        if let Some(reply) = reply {
                            let _ = reply.send(v.value());
                        }
                    }
                    match self.queue.front() {
                        Some((req, _)) => {
                            self.ready = false;
                            ctx.rt.store_insert(self.x, TagValue::fired(req.clone()));
                            ctx.event
                                .variables
                                .insert(self.x, TagValue::fired(req.clone()));
                        }
                        None => break,
                    }
                }
            }
        }
        match server_result {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.handler.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut graphix_compiler::Refs) {
        self.handler.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        ctx.rt.store_remove(&self.pid);
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
        self.queue.clear();
        self.ready = true;
        self.handler.sleep(ctx);
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
