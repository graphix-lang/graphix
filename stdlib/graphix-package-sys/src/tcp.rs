use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, Calls, EvalCachedAsync};
use netidx_value::Value;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::Arc,
};
use tokio::net::{TcpListener, TcpStream};

use crate::{Halves, StreamKind, get_stream, wrap_tcp};

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

graphix_package_core::abstract_wrapper!(
    TcpListenerValue,
    static LISTENER_WRAPPER = "sys::tcp::TcpListener"
);

fn get_listener(cached: &CachedVals, idx: usize) -> Option<Arc<TcpListener>> {
    match cached.0.get(idx)?.as_ref()? {
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
    // CR claude for eric: [bug/medium] With `Calls::Latest`, a trigger that arrives
    // while an accept's reply is completed but not yet delivered makes
    // `CachedArgsAsync::renew` release the reply id. The reply is then dropped as
    // stale, and the `TcpStream` it carries is closed: the client was accepted and then
    // hung up on. `Calls` reserves Latest for an eval that loses nothing when dropped,
    // and a completed accept loses a connection (an accept aborted while still pending
    // is harmless, since the client stays in the backlog). With a 200us accept trigger,
    // 1001 clients connect and the server holds only about 983 streams. Fix: either
    // `Calls::Fifo`, so each trigger accepts one connection in order, which is what the
    // tcp.gxi doc promises, or have a renewed site still deliver or bank a completed
    // reply whose value owns a resource. CLAUDE.md lists `accept` under Latest, so the
    // choice is a semantics decision. probe:
    // design/review-2026-10-12/repro/x-builtin-effects-11.gx (x-builtin-effects-11)
    // CR claude for claude: [bug/medium] Calls::Latest is wrong for accept because a
    // completed accept is not "an eval that loses nothing when dropped". Suppose the
    // trigger fires while an accept has already returned a stream but the runtime has
    // not joined its reply yet. Then CachedArgsAsync::renew releases the reply id,
    // GX::deliver drops the Via::Reply for the unreferenced id, and the TcpStream is
    // closed without the program ever seeing it. With 200 clients and a 1 ms timer
    // trigger, 3-4 connections are lost per run; with a busy trigger, 84-136 are lost.
    // This also breaks the gxi's promise that each update performs an accept. Fix: use
    // Calls::Fifo, one accept per fire, queued. If coalescing is wanted, a renew must
    // not discard an accept that has already completed. probe:
    // design/review-2026-10-12/repro/sys-io-08.gx (sys-io-08)
    const CALLS: Calls = Calls::Latest;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let _ = cached.0.get(1)?.as_ref()?;
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
            let mut guard = stream.writer().await;
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

#[derive(Debug, Default)]
// CR claude for claude: [structure/low] `TcpPeerAddrEv`, `TcpLocalAddrEv`,
// `TcpListenerAddrEv` and `ProcessPidEv` read their answer at once from the handle (the
// cached `Halves::addrs`, `local_addr()`, the pid), yet each is an `EvalCachedAsync`.
// Every call spawns a task and answers a cycle later, and the classification is Async.
// So a caller is async (`#[sync]` refuses `|l| tcp::listener_addr(l)`), the call never
// fuses, and a seq step waits an extra cycle. `HttpServerAddrEv` is the same kind of
// accessor and is a synchronous `EvalCached`. Fix: make these `EvalCached` with a
// `FastCall::Plain` fn that downcasts the abstract handle. Make the
// `IoStdinEv`/`IoStdoutEv`/`IoStderrEv` constructors `Effect::Sync`, since each call
// makes a new handle. Keep `EvalCachedAsync` for builtins that wait on I/O.
// (x-builtin-effects-07)
pub(crate) struct TcpPeerAddrEv;

impl EvalCachedAsync for TcpPeerAddrEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_tcp_peer_addr";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match stream.addrs() {
                Some((peer, _local)) => Value::String(
                    compact_str::format_compact!("{}", peer).as_str().into(),
                ),
                None => errf!("TCPError", "peer_addr: not a socket"),
            }
        }
    }
}

pub(crate) type TcpPeerAddr = CachedArgsAsync<TcpPeerAddrEv>;

#[derive(Debug, Default)]
pub(crate) struct TcpLocalAddrEv;

impl EvalCachedAsync for TcpLocalAddrEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_tcp_local_addr";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match stream.addrs() {
                Some((_peer, local)) => Value::String(
                    compact_str::format_compact!("{}", local).as_str().into(),
                ),
                None => errf!("TCPError", "local_addr: not a socket"),
            }
        }
    }
}

pub(crate) type TcpLocalAddr = CachedArgsAsync<TcpLocalAddrEv>;

#[derive(Debug, Default)]
pub(crate) struct TcpListenerAddrEv;

impl EvalCachedAsync for TcpListenerAddrEv {
    type Args = Arc<TcpListener>;

    const NAME: &str = "sys_tcp_listener_addr";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_listener(cached, 0)
    }

    fn eval(listener: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match listener.local_addr() {
                Ok(addr) => {
                    Value::String(compact_str::format_compact!("{addr}").as_str().into())
                }
                Err(e) => errf!("TCPError", "listener_addr failed: {e}"),
            }
        }
    }
}

pub(crate) type TcpListenerAddr = CachedArgsAsync<TcpListenerAddrEv>;

graphix_package_core::unit_image_state!(
    TcpConnectEv,
    TcpListenEv,
    TcpAcceptEv,
    TcpShutdownEv,
    TcpPeerAddrEv,
    TcpLocalAddrEv,
    TcpListenerAddrEv
);
