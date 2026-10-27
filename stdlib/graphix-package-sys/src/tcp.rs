use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, Calls, EvalCachedAsync};
use netidx_value::Value;
use std::sync::Arc;
use tokio::net::{TcpListener, TcpStream};

use crate::{Halves, StreamKind, get_stream, stream_of, wrap_tcp};

#[derive(Debug, Clone)]
pub(crate) struct TcpListenerValue {
    listener: Arc<TcpListener>,
}

graphix_package_core::impl_abstract_arc!(@identity TcpListenerValue.listener);

graphix_package_core::abstract_wrapper!(
    TcpListenerValue,
    static LISTENER_WRAPPER = "sys::tcp::TcpListener"
);

fn get_listener(cached: &CachedVals, idx: usize) -> Option<Arc<TcpListener>> {
    match cached.value(idx)? {
        Value::Abstract(a) => {
            let lv = a.downcast_ref::<TcpListenerValue>()?;
            Some(lv.listener.clone())
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
pub(crate) struct TcpConnectEv;

impl EvalCachedAsync for TcpConnectEv {
    type Args = ArcStr;

    const NAME: &str = "sys_tcp_connect";
    const CANCEL_SAFE: bool = false;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(addr: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match TcpStream::connect(&*addr).await {
                Ok(stream) => wrap_tcp(StreamKind::Tcp(stream)),
                Err(e) => errf!("TCPError", "connect to {addr} failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpConnect = CachedArgsAsync<TcpConnectEv>;

#[derive(Debug, Default)]
pub(crate) struct TcpListenEv;

impl EvalCachedAsync for TcpListenEv {
    type Args = ArcStr;

    const NAME: &str = "sys_tcp_listen";
    const CANCEL_SAFE: bool = false;

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

#[derive(Debug, Default)]
pub(crate) struct TcpAcceptEv;

impl EvalCachedAsync for TcpAcceptEv {
    type Args = Arc<TcpListener>;

    const NAME: &str = "sys_tcp_accept";
    const CALLS: Calls = Calls::Latest;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let _ = cached.value(1)?;
        get_listener(cached, 0)
    }

    fn eval(listener: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match listener.accept().await {
                Ok((stream, _addr)) => wrap_tcp(StreamKind::Tcp(stream)),
                Err(e) => errf!("TCPError", "accept failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpAccept = CachedArgsAsync<TcpAcceptEv>;

#[derive(Debug, Default)]
pub(crate) struct TcpShutdownEv;

impl EvalCachedAsync for TcpShutdownEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_tcp_shutdown";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            use tokio::io::AsyncWriteExt;
            stream
                .on_writer("TCPError", "shutdown", async |s| s.shutdown().await)
                .await
                .map_or_else(|e| e, |()| Value::Null)
        }
    }
}

pub(crate) type TcpShutdown = CachedArgsAsync<TcpShutdownEv>;

fn fc_peer_addr(args: &[Value]) -> Option<Value> {
    Some(match stream_of(&args[0])?.addrs() {
        Some((peer, _)) => Value::String(format_compact!("{peer}").as_str().into()),
        None => errf!("TCPError", "peer_addr: not a socket"),
    })
}

graphix_package_core::fast_builtin!(pub(crate) TcpPeerAddr, TcpPeerAddrEv, "sys_tcp_peer_addr", fc_peer_addr);

fn fc_local_addr(args: &[Value]) -> Option<Value> {
    Some(match stream_of(&args[0])?.addrs() {
        Some((_, local)) => Value::String(format_compact!("{local}").as_str().into()),
        None => errf!("TCPError", "local_addr: not a socket"),
    })
}

graphix_package_core::fast_builtin!(pub(crate) TcpLocalAddr, TcpLocalAddrEv, "sys_tcp_local_addr", fc_local_addr);

fn fc_listener_addr(args: &[Value]) -> Option<Value> {
    let Value::Abstract(a) = &args[0] else { return None };
    let listener = &a.downcast_ref::<TcpListenerValue>()?.listener;
    Some(match listener.local_addr() {
        Ok(addr) => Value::String(format_compact!("{addr}").as_str().into()),
        Err(e) => errf!("TCPError", "listener_addr failed: {e}"),
    })
}

graphix_package_core::fast_builtin!(pub(crate) TcpListenerAddr, TcpListenerAddrEv, "sys_tcp_listener_addr", fc_listener_addr);

graphix_package_core::unit_image_state!(
    TcpConnectEv,
    TcpListenEv,
    TcpAcceptEv,
    TcpShutdownEv
);
