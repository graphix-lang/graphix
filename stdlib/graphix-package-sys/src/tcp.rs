use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, Value};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::{Arc, LazyLock},
};
use tokio::{
    net::{TcpListener, TcpStream},
    sync::Mutex,
};

use crate::{get_stream, wrap_tcp, StreamKind};

// ── Abstract TcpListenerValue ──────────────────────────────────

#[derive(Debug, Clone)]
pub(crate) struct TcpListenerValue {
    listener: Arc<TcpListener>,
}

impl PartialEq for TcpListenerValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.listener, &other.listener)
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
            0xa6, 0xb7, 0xc8, 0xd9, 0xea, 0xfb, 0x4c, 0x0d, 0x1e, 0x2f, 0x30, 0x41, 0x52,
            0x63, 0x74, 0x85,
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
    const NAME: &str = "sys_tcp_connect";
    const NEEDS_CALLSITE: bool = false;
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(addr: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match TcpStream::connect(&*addr).await {
                Ok(stream) => wrap_tcp(stream),
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
    const NAME: &str = "sys_tcp_listen";
    const NEEDS_CALLSITE: bool = false;
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(addr: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match TcpListener::bind(&*addr).await {
                Ok(listener) => LISTENER_WRAPPER
                    .wrap(TcpListenerValue { listener: Arc::new(listener) }),
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
    const NAME: &str = "sys_tcp_accept";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TcpListener>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let _ = cached.0.get(1)?.as_ref()?;
        get_listener(cached, 0)
    }

    fn eval(listener: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match listener.accept().await {
                Ok((stream, _addr)) => wrap_tcp(stream),
                Err(e) => errf!("TCPError", "accept failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpAccept = CachedArgsAsync<TcpAcceptEv>;

// ── TcpShutdown ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpShutdownEv;

impl EvalCachedAsync for TcpShutdownEv {
    const NAME: &str = "sys_tcp_shutdown";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<Mutex<Option<StreamKind>>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            use tokio::io::AsyncWriteExt;
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("TCPError", "stream unavailable"),
            };
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
    const NAME: &str = "sys_tcp_peer_addr";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<Mutex<Option<StreamKind>>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let guard = stream.lock().await;
            let s = match guard.as_ref() {
                Some(s) => s,
                None => return errf!("TCPError", "stream unavailable"),
            };
            match s.tcp_ref() {
                Some(tcp) => match tcp.peer_addr() {
                    Ok(addr) => Value::String(ArcStr::from(addr.to_string().as_str())),
                    Err(e) => errf!("TCPError", "peer_addr failed: {e}"),
                },
                None => errf!("TCPError", "peer_addr not supported on file streams"),
            }
        }
    }
}

pub(crate) type TcpPeerAddr = CachedArgsAsync<TcpPeerAddrEv>;

// ── TcpLocalAddr ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpLocalAddrEv;

impl EvalCachedAsync for TcpLocalAddrEv {
    const NAME: &str = "sys_tcp_local_addr";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<Mutex<Option<StreamKind>>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let guard = stream.lock().await;
            let s = match guard.as_ref() {
                Some(s) => s,
                None => return errf!("TCPError", "stream unavailable"),
            };
            match s.tcp_ref() {
                Some(tcp) => match tcp.local_addr() {
                    Ok(addr) => Value::String(ArcStr::from(addr.to_string().as_str())),
                    Err(e) => errf!("TCPError", "local_addr failed: {e}"),
                },
                None => errf!("TCPError", "local_addr not supported on file streams"),
            }
        }
    }
}

pub(crate) type TcpLocalAddr = CachedArgsAsync<TcpLocalAddrEv>;

// ── TcpListenerAddr ────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TcpListenerAddrEv;

impl EvalCachedAsync for TcpListenerAddrEv {
    const NAME: &str = "sys_tcp_listener_addr";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TcpListener>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_listener(cached, 0)
    }

    fn eval(listener: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match listener.local_addr() {
                Ok(addr) => Value::String(ArcStr::from(addr.to_string().as_str())),
                Err(e) => errf!("TCPError", "listener_addr failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpListenerAddr = CachedArgsAsync<TcpListenerAddrEv>;
