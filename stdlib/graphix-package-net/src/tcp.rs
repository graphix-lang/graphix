use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, PBytes, Value};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::{Arc, LazyLock},
};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    sync::Mutex,
};

// ── Abstract TcpStreamValue ────────────────────────────────────

#[derive(Debug, Clone)]
struct TcpStreamValue {
    stream: Arc<Mutex<TcpStream>>,
}

impl PartialEq for TcpStreamValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.stream) == Arc::as_ptr(&other.stream)
    }
}

impl Eq for TcpStreamValue {}

impl PartialOrd for TcpStreamValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TcpStreamValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.stream).cmp(&Arc::as_ptr(&other.stream))
    }
}

impl Hash for TcpStreamValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.stream).hash(state)
    }
}

graphix_package_core::impl_no_pack!(TcpStreamValue);

static STREAM_WRAPPER: LazyLock<AbstractWrapper<TcpStreamValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xb7, 0xc8, 0xd9, 0xea, 0xfb, 0x0c, 0x4d, 0x1e, 0x2f, 0x30, 0x41, 0x52, 0x63,
        0x74, 0x85, 0x96,
    ]);
    Abstract::register::<TcpStreamValue>(id).expect("failed to register TcpStreamValue")
});

fn get_stream(cached: &CachedVals, idx: usize) -> Option<Arc<Mutex<TcpStream>>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let sv = a.downcast_ref::<TcpStreamValue>()?;
            Some(sv.stream.clone())
        }
        _ => None,
    }
}

// ── Abstract TcpListenerValue ──────────────────────────────────

#[derive(Debug, Clone)]
struct TcpListenerValue {
    listener: Arc<TcpListener>,
}

impl PartialEq for TcpListenerValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.listener) == Arc::as_ptr(&other.listener)
    }
}

impl Eq for TcpListenerValue {}

impl PartialOrd for TcpListenerValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TcpListenerValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.listener).cmp(&Arc::as_ptr(&other.listener))
    }
}

impl Hash for TcpListenerValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.listener).hash(state)
    }
}

graphix_package_core::impl_no_pack!(TcpListenerValue);

static LISTENER_WRAPPER: LazyLock<AbstractWrapper<TcpListenerValue>> =
    LazyLock::new(|| {
        let id = uuid::Uuid::from_bytes([
            0xa6, 0xb7, 0xc8, 0xd9, 0xea, 0xfb, 0x4c, 0x0d, 0x1e, 0x2f, 0x30, 0x41,
            0x52, 0x63, 0x74, 0x85,
        ]);
        Abstract::register::<TcpListenerValue>(id)
            .expect("failed to register TcpListenerValue")
    });

fn get_listener(cached: &CachedVals, idx: usize) -> Option<Arc<TcpListener>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let lv = a.downcast_ref::<TcpListenerValue>()?;
            Some(lv.listener.clone())
        }
        _ => None,
    }
}

// ── TcpConnect ─────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpConnectEv;

impl EvalCachedAsync for TcpConnectEv {
    const NAME: &str = "net_tcp_connect";
    deftype!("fn(string) -> Result<string, `TCPError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(addr: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match TcpStream::connect(&*addr).await {
                Ok(stream) => STREAM_WRAPPER.wrap(TcpStreamValue {
                    stream: Arc::new(Mutex::new(stream)),
                }),
                Err(e) => errf!("TCPError", "connect to {addr} failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpConnect = CachedArgsAsync<TcpConnectEv>;

// ── TcpListen ──────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpListenEv;

impl EvalCachedAsync for TcpListenEv {
    const NAME: &str = "net_tcp_listen";
    deftype!("fn(string) -> Result<string, `TCPError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(addr: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match TcpListener::bind(&*addr).await {
                Ok(listener) => LISTENER_WRAPPER.wrap(TcpListenerValue {
                    listener: Arc::new(listener),
                }),
                Err(e) => errf!("TCPError", "bind to {addr} failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpListen = CachedArgsAsync<TcpListenEv>;

// ── TcpAccept ──────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpAcceptEv;

impl EvalCachedAsync for TcpAcceptEv {
    const NAME: &str = "net_tcp_accept";
    deftype!("fn(string, Any) -> Result<string, `TCPError(string)>");
    type Args = Arc<TcpListener>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let _ = cached.0.get(1)?.as_ref()?;
        get_listener(cached, 0)
    }

    fn eval(listener: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match listener.accept().await {
                Ok((stream, _addr)) => STREAM_WRAPPER.wrap(TcpStreamValue {
                    stream: Arc::new(Mutex::new(stream)),
                }),
                Err(e) => errf!("TCPError", "accept failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpAccept = CachedArgsAsync<TcpAcceptEv>;

// ── TcpRead ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpReadEv;

impl EvalCachedAsync for TcpReadEv {
    const NAME: &str = "net_tcp_read";
    deftype!("fn(string, u64) -> Result<bytes, `TCPError(string)>");
    type Args = (Arc<Mutex<TcpStream>>, u64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut s = stream.lock().await;
            let mut buf = vec![0u8; n as usize];
            match s.read(&mut buf).await {
                Ok(n) => {
                    buf.truncate(n);
                    Value::Bytes(PBytes::new(Bytes::from(buf)))
                }
                Err(e) => errf!("TCPError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpRead = CachedArgsAsync<TcpReadEv>;

// ── TcpReadExact ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpReadExactEv;

impl EvalCachedAsync for TcpReadExactEv {
    const NAME: &str = "net_tcp_read_exact";
    deftype!("fn(string, u64) -> Result<bytes, `TCPError(string)>");
    type Args = (Arc<Mutex<TcpStream>>, u64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut s = stream.lock().await;
            let mut buf = vec![0u8; n as usize];
            let mut pos = 0;
            while pos < buf.len() {
                match s.read(&mut buf[pos..]).await {
                    Ok(0) => break,
                    Ok(n) => pos += n,
                    Err(e) => return errf!("TCPError", "read_exact failed: {e}"),
                }
            }
            buf.truncate(pos);
            Value::Bytes(PBytes::new(Bytes::from(buf)))
        }
    }
}

pub(crate) type TcpReadExact = CachedArgsAsync<TcpReadExactEv>;

// ── TcpWrite ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpWriteEv;

impl EvalCachedAsync for TcpWriteEv {
    const NAME: &str = "net_tcp_write";
    deftype!("fn(string, bytes) -> Result<u64, `TCPError(string)>");
    type Args = (Arc<Mutex<TcpStream>>, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut s = stream.lock().await;
            match s.write(&data).await {
                Ok(n) => Value::U64(n as u64),
                Err(e) => errf!("TCPError", "write failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpWrite = CachedArgsAsync<TcpWriteEv>;

// ── TcpWriteExact ──────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpWriteExactEv;

impl EvalCachedAsync for TcpWriteExactEv {
    const NAME: &str = "net_tcp_write_exact";
    deftype!("fn(string, bytes) -> Result<null, `TCPError(string)>");
    type Args = (Arc<Mutex<TcpStream>>, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut s = stream.lock().await;
            match s.write_all(&data).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("TCPError", "write_exact failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpWriteExact = CachedArgsAsync<TcpWriteExactEv>;

// ── TcpShutdown ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpShutdownEv;

impl EvalCachedAsync for TcpShutdownEv {
    const NAME: &str = "net_tcp_shutdown";
    deftype!("fn(string) -> Result<null, `TCPError(string)>");
    type Args = Arc<Mutex<TcpStream>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut s = stream.lock().await;
            match s.shutdown().await {
                Ok(()) => Value::Null,
                Err(e) => errf!("TCPError", "shutdown failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpShutdown = CachedArgsAsync<TcpShutdownEv>;

// ── TcpPeerAddr ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpPeerAddrEv;

impl EvalCachedAsync for TcpPeerAddrEv {
    const NAME: &str = "net_tcp_peer_addr";
    deftype!("fn(string) -> Result<string, `TCPError(string)>");
    type Args = Arc<Mutex<TcpStream>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let s = stream.lock().await;
            match s.peer_addr() {
                Ok(addr) => Value::String(ArcStr::from(addr.to_string().as_str())),
                Err(e) => errf!("TCPError", "peer_addr failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpPeerAddr = CachedArgsAsync<TcpPeerAddrEv>;

// ── TcpLocalAddr ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpLocalAddrEv;

impl EvalCachedAsync for TcpLocalAddrEv {
    const NAME: &str = "net_tcp_local_addr";
    deftype!("fn(string) -> Result<string, `TCPError(string)>");
    type Args = Arc<Mutex<TcpStream>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let s = stream.lock().await;
            match s.local_addr() {
                Ok(addr) => Value::String(ArcStr::from(addr.to_string().as_str())),
                Err(e) => errf!("TCPError", "local_addr failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpLocalAddr = CachedArgsAsync<TcpLocalAddrEv>;
