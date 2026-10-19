#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::CompactString;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::ImageBuf,
    rep,
    typ::{FnType, abstract_uuid},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, FireOnce, ProgramArgs, seam_tick,
};
use graphix_rt::GXRt;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, ValArray, Value, abstract_type::AbstractWrapper};
use poolshark::{global::GPooled, local::LPooled};
use std::{
    cell::RefCell,
    cmp::Ordering,
    hash::{Hash, Hasher},
    io::{ErrorKind, Result as IoResult},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    pin::Pin,
    sync::{
        Arc, LazyLock, OnceLock,
        atomic::{AtomicBool, Ordering as AtomicOrdering},
    },
    task::{Context, Poll},
};
use tempfile::TempDir;
use tokio::{
    io::{AsyncRead, AsyncWrite, AsyncWriteExt, ReadBuf, ReadHalf, WriteHalf},
    sync::{Mutex, MutexGuard, Notify, watch as tokio_watch},
};

pub(crate) mod dir;
pub(crate) mod dirs_mod;
pub(crate) mod fs;
pub(crate) mod io;
pub mod loader;
pub(crate) mod metadata;
pub(crate) mod net;
pub mod netstate;
pub(crate) mod process;
pub(crate) mod tcp;
pub(crate) mod time;
pub(crate) mod tls;
pub(crate) mod watch;

/// A stream with bytes retained by an interrupted line reader.
#[derive(Debug)]
pub struct BufferedRead<S> {
    stream: S,
    pending: GPooled<Vec<u8>>,
    consumed: usize,
}

impl<S> BufferedRead<S> {
    fn new(stream: S, pending: GPooled<Vec<u8>>) -> Self {
        Self { stream, pending, consumed: 0 }
    }

    fn compact(&mut self) {
        self.pending.drain(..self.consumed);
        self.consumed = 0;
    }

    fn into_parts(mut self) -> (S, GPooled<Vec<u8>>) {
        self.compact();
        (self.stream, self.pending)
    }
}

impl<S: AsyncRead + Unpin> AsyncRead for BufferedRead<S> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<IoResult<()>> {
        let this = self.get_mut();
        if this.consumed < this.pending.len() {
            let n = buf.remaining().min(this.pending.len() - this.consumed);
            buf.put_slice(&this.pending[this.consumed..this.consumed + n]);
            this.consumed += n;
            if this.consumed == this.pending.len() {
                this.pending.clear();
                this.consumed = 0;
            }
            Poll::Ready(Ok(()))
        } else {
            Pin::new(&mut this.stream).poll_read(cx, buf)
        }
    }
}

impl<S: AsyncWrite + Unpin> AsyncWrite for BufferedRead<S> {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<IoResult<usize>> {
        Pin::new(&mut self.get_mut().stream).poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<IoResult<()>> {
        Pin::new(&mut self.get_mut().stream).poll_flush(cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<IoResult<()>> {
        Pin::new(&mut self.get_mut().stream).poll_shutdown(cx)
    }

    fn poll_write_vectored(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<IoResult<usize>> {
        Pin::new(&mut self.get_mut().stream).poll_write_vectored(cx, bufs)
    }

    fn is_write_vectored(&self) -> bool {
        self.stream.is_write_vectored()
    }
}

pub enum StreamKind {
    File(tokio::fs::File),
    Tcp(tokio::net::TcpStream),
    Tls(tokio_rustls::TlsStream<BufferedRead<tokio::net::TcpStream>>),
    Stdin(tokio::io::Stdin),
    Stdout(tokio::io::Stdout),
    Stderr(tokio::io::Stderr),
    ChildStdin(tokio::process::ChildStdin),
    ChildStdout(tokio::process::ChildStdout),
    ChildStderr(tokio::process::ChildStderr),
}

impl std::fmt::Debug for StreamKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StreamKind::File(_) => f.debug_tuple("File").finish(),
            StreamKind::Tcp(s) => f.debug_tuple("Tcp").field(s).finish(),
            StreamKind::Tls(_) => f.debug_tuple("Tls").finish(),
            StreamKind::Stdin(_) => f.debug_tuple("Stdin").finish(),
            StreamKind::Stdout(_) => f.debug_tuple("Stdout").finish(),
            StreamKind::Stderr(_) => f.debug_tuple("Stderr").finish(),
            StreamKind::ChildStdin(_) => f.debug_tuple("ChildStdin").finish(),
            StreamKind::ChildStdout(_) => f.debug_tuple("ChildStdout").finish(),
            StreamKind::ChildStderr(_) => f.debug_tuple("ChildStderr").finish(),
        }
    }
}

impl StreamKind {
    pub(crate) fn tcp_ref(&self) -> Option<&tokio::net::TcpStream> {
        match self {
            StreamKind::Tcp(s) => Some(s),
            StreamKind::Tls(s) => {
                let (tcp, _) = s.get_ref();
                Some(&tcp.stream)
            }
            _ => None,
        }
    }
}

impl AsyncRead for StreamKind {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamKind::File(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::Tcp(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::Tls(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::Stdin(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::ChildStdout(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::ChildStderr(s) => Pin::new(s).poll_read(cx, buf),
            StreamKind::Stdout(_) | StreamKind::Stderr(_) | StreamKind::ChildStdin(_) => {
                Poll::Ready(Err(std::io::Error::new(
                    std::io::ErrorKind::Unsupported,
                    "cannot read from write-only stream",
                )))
            }
        }
    }
}

impl AsyncWrite for StreamKind {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        match self.get_mut() {
            StreamKind::File(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::Tcp(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::Tls(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::Stdout(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::Stderr(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::ChildStdin(s) => Pin::new(s).poll_write(cx, buf),
            StreamKind::Stdin(_)
            | StreamKind::ChildStdout(_)
            | StreamKind::ChildStderr(_) => Poll::Ready(Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "cannot write to read-only stream",
            ))),
        }
    }

    fn poll_flush(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamKind::File(s) => Pin::new(s).poll_flush(cx),
            StreamKind::Tcp(s) => Pin::new(s).poll_flush(cx),
            StreamKind::Tls(s) => Pin::new(s).poll_flush(cx),
            StreamKind::Stdout(s) => Pin::new(s).poll_flush(cx),
            StreamKind::Stderr(s) => Pin::new(s).poll_flush(cx),
            StreamKind::ChildStdin(s) => Pin::new(s).poll_flush(cx),
            StreamKind::Stdin(_)
            | StreamKind::ChildStdout(_)
            | StreamKind::ChildStderr(_) => Poll::Ready(Ok(())),
        }
    }

    fn poll_shutdown(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            StreamKind::File(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::Tcp(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::Tls(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::Stdout(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::Stderr(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::ChildStdin(s) => Pin::new(s).poll_shutdown(cx),
            StreamKind::Stdin(_)
            | StreamKind::ChildStdout(_)
            | StreamKind::ChildStderr(_) => Poll::Ready(Ok(())),
        }
    }
}

/// Which kind of stream a handle is.
///
/// One representation, five nominal Graphix types (`sys::fs::File`,
/// `sys::tcp::TcpStream`, `sys::tls::TlsStream`, `sys::process::Pipe`,
/// `sys::io::Stdio`): the io code is shared, each type carries the trait
/// impls that say what it can do, and the marker is what the abstract
/// registry keys a UUID on.
pub trait StreamMark: 'static + Send + Sync {
    /// The type's canonical Graphix path. Its UUID is derived from it
    /// ([`abstract_uuid`]), so a type test (`File as f`) recognizes
    /// the value by the path alone.
    const PATH: &'static str;
}

/// A stream's read and write halves, locked apart so a read waiting on
/// its peer never holds up a write. An operation on the whole stream (a
/// seek, a TLS upgrade) takes both halves, the read half first. A half is
/// `None` once closed.
pub struct Halves {
    closing: AtomicBool,
    closed: Notify,
    r: Mutex<Option<BufferedRead<ReadHalf<StreamKind>>>>,
    stop_lines: OnceLock<tokio_watch::Sender<()>>,
    w: Mutex<Option<WriteHalf<StreamKind>>>,
    /// A socket's (peer, local) addresses, read when it was made.
    addrs: Option<(std::net::SocketAddr, std::net::SocketAddr)>,
}

impl std::fmt::Debug for Halves {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Halves").field("addrs", &self.addrs).finish()
    }
}

/// Holds both halves while an operation borrows the whole stream.
pub(crate) struct Whole<'a> {
    r: MutexGuard<'a, Option<BufferedRead<ReadHalf<StreamKind>>>>,
    w: MutexGuard<'a, Option<WriteHalf<StreamKind>>>,
    kind: Option<(StreamKind, GPooled<Vec<u8>>)>,
}

impl Whole<'_> {
    pub(crate) fn buffered_len(&self) -> usize {
        self.kind.as_ref().unwrap().1.len()
    }

    pub(crate) fn into_tcp(mut self) -> Option<BufferedRead<tokio::net::TcpStream>> {
        match self.kind.take()? {
            (StreamKind::Tcp(tcp), pending) => Some(BufferedRead::new(tcp, pending)),
            other => {
                self.kind = Some(other);
                None
            }
        }
    }
}

impl Deref for Whole<'_> {
    type Target = StreamKind;

    fn deref(&self) -> &Self::Target {
        &self.kind.as_ref().unwrap().0
    }
}

impl DerefMut for Whole<'_> {
    /// Mutable whole-stream access invalidates bytes read ahead.
    fn deref_mut(&mut self) -> &mut Self::Target {
        let (kind, pending) = self.kind.as_mut().unwrap();
        pending.clear();
        kind
    }
}

impl Drop for Whole<'_> {
    /// Put back what [Halves::take] took, unless the stream was consumed.
    fn drop(&mut self) {
        if let Some((kind, pending)) = self.kind.take() {
            let (r, w) = tokio::io::split(kind);
            *self.r = Some(BufferedRead::new(r, pending));
            *self.w = Some(w);
        }
    }
}

impl Halves {
    fn new(kind: StreamKind) -> Self {
        let addrs = kind
            .tcp_ref()
            .and_then(|t| Some((t.peer_addr().ok()?, t.local_addr().ok()?)));
        let (r, w) = tokio::io::split(kind);
        Self {
            closing: AtomicBool::new(false),
            closed: Notify::new(),
            r: Mutex::new(Some(BufferedRead::new(r, GPooled::take()))),
            stop_lines: OnceLock::new(),
            w: Mutex::new(Some(w)),
            addrs,
        }
    }

    pub(crate) async fn until_closed<T>(
        &self,
        read: impl Future<Output = IoResult<T>>,
    ) -> IoResult<T> {
        let closed = self.closed.notified();
        tokio::pin!(closed);
        closed.as_mut().enable();
        if self.closing.load(AtomicOrdering::Acquire) {
            return Err(ErrorKind::BrokenPipe.into());
        }
        tokio::select! {
            biased;
            _ = closed => Err(ErrorKind::BrokenPipe.into()),
            result = read => result,
        }
    }

    pub(crate) async fn reader(
        &self,
    ) -> MutexGuard<'_, Option<BufferedRead<ReadHalf<StreamKind>>>> {
        self.r.lock().await
    }

    pub(crate) async fn writer(&self) -> MutexGuard<'_, Option<WriteHalf<StreamKind>>> {
        self.w.lock().await
    }

    pub(crate) fn addrs(&self) -> Option<(std::net::SocketAddr, std::net::SocketAddr)> {
        self.addrs
    }

    pub(crate) fn lines_stopped(&self) -> tokio_watch::Receiver<()> {
        self.stop_lines.get_or_init(|| tokio_watch::channel(()).0).subscribe()
    }

    /// Borrow the whole stream under both locks; `None` if it is closed.
    // CR claude for eric: [bug/medium] Every whole-stream borrow ends the stream's line
    // readers permanently, including those by fs::fstat and fs::truncate, which change
    // nothing a reader depends on. take sends stop_lines and line_reader returns.
    // IoLines keeps its finished Reading and ignores the same handle, so that site
    // never reads again and the program sees no error. In the probe, lines(f) over a
    // 30000-line file stops at line 12773 after one fstat. Fix: make the stop a pause
    // for any take that puts the stream back. The reader drops its guard, waits for
    // Whole::drop to restore the halves (a Notify there), and reads on. It ends only
    // when the halves are gone (close, a TLS upgrade). This also makes lines read on
    // after a seek, from the new position, where design/async_sleep_outputs.md now
    // lists seek as a stop: decide which a seek should do. probe:
    // design/review-2026-10-19/repro/sys-io-fstat-stops-lines.gx (sys-io-02)
    pub(crate) async fn take(&self) -> Option<Whole<'_>> {
        // CR claude for claude: [bug/high] take() stops the stream's line reader for
        // every whole-stream operation, not only the TLS upgrade it was added for.
        // FileFstatEv, FileSeekEv and FileTruncateEv borrow through take, so an fstat
        // beside Lines::lines ends the reader with no value and no error. line_reader's
        // select! just returns, while the Lines doc promises a stop only at end of
        // stream or on a read error. This brings back x-builtin-effects-01's symptom by
        // another path: the probe prints "lines read 0 of 200000". Fix: signal
        // stop_lines only on the path that consumes the stream (the TLS handoff in
        // tls.rs). fstat and truncate on a file can wait for the reader's per-chunk
        // lock. If seek should end lines, it must say so with an error. probe:
        // design/review-2026-10-19/repro/x-rerun-5-fstat-stops-lines.gx (x-rerun-5-01)
        if let Some(stop) = self.stop_lines.get() {
            stop.send_replace(());
        }
        let mut r = self.r.lock().await;
        let mut w = self.w.lock().await;
        match (r.take(), w.take()) {
            (Some(rh), Some(wh)) => {
                let (rh, pending) = rh.into_parts();
                Some(Whole { r, w, kind: Some((rh.unsplit(wh), pending)) })
            }
            (rh, wh) => {
                *r = rh;
                *w = wh;
                None
            }
        }
    }

    /// Cancel pending reads, shut down the writer, and release both halves.
    pub(crate) async fn close(self: &Arc<Self>) -> IoResult<()> {
        self.closing.store(true, AtomicOrdering::Release);
        self.closed.notify_waiters();
        let mut r = self.r.lock().await;
        let mut w = self.w.lock().await;
        let res = match w.take() {
            Some(mut w) => w.shutdown().await,
            None => Ok(()),
        };
        r.take();
        res
    }
}

pub struct Stream<K: StreamMark> {
    pub inner: Arc<Halves>,
    mark: PhantomData<K>,
}

impl<K: StreamMark> Stream<K> {
    fn new(kind: StreamKind) -> Self {
        Stream { inner: Arc::new(Halves::new(kind)), mark: PhantomData }
    }
}

impl<K: StreamMark> std::fmt::Debug for Stream<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", K::PATH)
    }
}

impl<K: StreamMark> PartialEq for Stream<K> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<K: StreamMark> Eq for Stream<K> {}

impl<K: StreamMark> PartialOrd for Stream<K> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// CR claude for eric: [risk/low] Stream orders and hashes by Arc address, as do
// ProcValue, TcpListenerValue, WatcherValue and every type built by core's
// impl_abstract_arc!. `a < b`, sort, min/max and map key order over two handles
// therefore change from run to run. A program over handles is nondeterministic, and a
// fuzzer run comparing two processes can diverge on it. Either refuse `Ordered` for
// abstract handle types in the checker, as for references and functions, or give each
// handle a creation ordinal (an atomic_id!) to order and hash by. Either way, the four
// hand-written impls in sys should become impl_abstract_arc!. probe:
// design/review-2026-10-12/repro/sys-io-09-handle-order-nondeterministic.gx (sys-io-09)
// CR claude for eric: [bug/medium] Stream handles order and hash by `Arc::as_ptr`.
// Because `Ordered` admits them as map keys and sort elements, `map::fold` over a Map
// keyed by handles and `array::sort` of handles give a different order on each run.
// With the same program, stdout/stderr/stdin keys folded to [3, 2, 1], [2, 3, 1] and
// [3, 1, 2] on different runs (probe: design/review-2026-10-12/repro/sys-net-05.gx).
// Other code repeats the same address identity: `TcpListenerValue` and `WatcherValue`
// in this crate, core's `impl_abstract_arc!(@identity)` (used by the db, sqlite and
// http handles), and netidx-admin's `Staged`. Two fixes are possible. The first: refuse
// Rust-backed handle types under `Ordered`, keep `==` by identity, and treat them like
// references and functions. The second: order and hash them by a creation id minted in
// `impl_abstract_arc` and used by every hand-rolled copy. Under parallel eval such an
// id still depends on scheduling (see the "Ids" table in design/parallel_eval.md).
// Choosing between them is a language rule. (sys-net-05)
impl<K: StreamMark> Ord for Stream<K> {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl<K: StreamMark> Hash for Stream<K> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
    }
}

impl<K: StreamMark> Pack for Stream<K> {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(&self, _buf: &mut impl BufMut) -> Result<(), PackError> {
        Err(PackError::Application(0))
    }

    fn decode(_buf: &mut impl Buf) -> Result<Self, PackError> {
        Err(PackError::Application(0))
    }
}

macro_rules! stream_kinds {
    ($($mark:ident => $path:literal, $wrapper:ident, $wrap:ident;)*) => {
        $(
            #[derive(Debug)]
            pub struct $mark;

            impl StreamMark for $mark {
                const PATH: &'static str = $path;
            }

            pub(crate) static $wrapper: LazyLock<AbstractWrapper<Stream<$mark>>> =
                LazyLock::new(|| {
                    Abstract::register::<Stream<$mark>>(abstract_uuid($path))
                        .expect(concat!("failed to register ", $path))
                });

            pub(crate) fn $wrap(kind: StreamKind) -> Value {
                $wrapper.wrap(Stream::<$mark>::new(kind))
            }
        )*

        /// The stream behind `v`, whatever kind of handle it is; the Graphix
        /// type and its trait impls decide which operations are legal.
        pub fn stream_of(v: &Value) -> Option<Arc<Halves>> {
            let Value::Abstract(a) = v else { return None };
            $(
                if let Some(s) = a.downcast_ref::<Stream<$mark>>() {
                    return Some(s.inner.clone());
                }
            )*
            None
        }
    };
}

// CR claude for eric: [structure/low] Stdio and Pipe each name both directions of
// stream and implement Read, Lines and Write. So `Read::read(child.stdin$, n)` and
// `Write::write(sys::io::stdin(null), d)` pass the check and fail at run time, through
// StreamKind's Unsupported arms. Splitting each by direction makes the misuse a type
// error, and lets those arms go: e.g. marks for sys::io::Stdin/Stdout and
// sys::process::PipeIn/PipeOut, each with only the matching Read+Lines or Write impls
// in io.gx/process.gx. This changes the stdlib API. probe:
// design/review-2026-10-19/repro/sys-io-direction-less-stdio-pipe.gx (sys-io-03)
stream_kinds! {
    FileMark => "sys::fs::File", FILE_WRAPPER, wrap_file;
    TcpMark => "sys::tcp::TcpStream", TCP_WRAPPER, wrap_tcp;
    TlsMark => "sys::tls::TlsStream", TLS_WRAPPER, wrap_tls;
    PipeMark => "sys::process::Pipe", PIPE_WRAPPER, wrap_pipe;
    StdioMark => "sys::io::Stdio", STDIO_WRAPPER, wrap_stdio;
}

pub fn get_stream(cached: &CachedVals, idx: usize) -> Option<Arc<Halves>> {
    stream_of(cached.value(idx)?)
}

#[derive(Debug)]
struct TempDirValue {
    path: ArcStr,
    _dir: TempDir,
}

impl PartialEq for TempDirValue {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Eq for TempDirValue {}

impl PartialOrd for TempDirValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TempDirValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.path.cmp(&other.path)
    }
}

impl Hash for TempDirValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state)
    }
}

graphix_package_core::impl_no_pack!(TempDirValue);

graphix_package_core::abstract_wrapper!(
    TempDirValue,
    static TEMPDIR_WRAPPER = "sys::fs::tempdir::T"
);

#[derive(Debug)]
enum Name {
    Prefix(ArcStr),
    Suffix(ArcStr),
}

#[derive(Debug)]
pub(crate) struct TempDirArgs {
    dir: Option<ArcStr>,
    name: Option<Name>,
}

#[derive(Debug, Default)]
pub(crate) struct GxTempDirEv;

impl EvalCachedAsync for GxTempDirEv {
    type Args = TempDirArgs;

    const NAME: &str = "sys_tempdir";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        if cached.values().any(|v| v.is_none()) {
            None
        } else {
            let dir = cached.get::<Option<ArcStr>>(0).flatten();
            let name = match cached.value(1).and_then(rep::variant) {
                Some((tag, [Value::String(v)])) => match tag.as_str() {
                    "Prefix" => Some(Name::Prefix(v.clone())),
                    "Suffix" => Some(Name::Suffix(v.clone())),
                    _ => None,
                },
                _ => None,
            };
            let _ = cached.get::<Value>(2)?;
            Some(TempDirArgs { dir, name })
        }
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let td = tokio::task::spawn_blocking(|| match (args.dir, args.name) {
                (None, None) => TempDir::new(),
                (None, Some(Name::Prefix(pfx))) => TempDir::with_prefix(&*pfx),
                (None, Some(Name::Suffix(sfx))) => TempDir::with_suffix(&*sfx),
                (Some(dir), None) => TempDir::new_in(&*dir),
                (Some(dir), Some(Name::Prefix(pfx))) => {
                    TempDir::with_prefix_in(&*pfx, &*dir)
                }
                (Some(dir), Some(Name::Suffix(sfx))) => {
                    TempDir::with_suffix_in(&*sfx, &*dir)
                }
            })
            .await;
            match td {
                Err(e) => errf!("IOError", "failed to spawn create temp dir {e:?}"),
                Ok(Err(e)) => errf!("IOError", "failed to create temp dir {e:?}"),
                Ok(Ok(td)) => {
                    use std::fmt::Write;
                    let mut buf = CompactString::new("");
                    write!(buf, "{}", td.path().display()).unwrap();
                    let path = ArcStr::from(buf.as_str());
                    TEMPDIR_WRAPPER.wrap(TempDirValue { path, _dir: td })
                }
            }
        }
    }
}

pub(crate) type GxTempDir = CachedArgsAsync<GxTempDirEv>;

fn fc_tempdir_path(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Abstract(a) => {
            let td = a.downcast_ref::<TempDirValue>()?;
            Some(Value::String(td.path.clone()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(pub(crate) TempDirPath, TempDirPathEv, "sys_tempdir_path", fc_tempdir_path);

pub(crate) fn convert_path(path: &Path) -> ArcStr {
    ArcStr::from(&*path.to_string_lossy())
}

fn fc_join_path(args: &[Value]) -> Option<Value> {
    let mut parts: LPooled<Vec<ArcStr>> = LPooled::take();
    for part in args {
        match part {
            Value::String(s) => parts.push(s.clone()),
            Value::Array(a) => {
                for part in a.iter() {
                    match part {
                        Value::String(s) => parts.push(s.clone()),
                        _ => return None,
                    }
                }
            }
            _ => return None,
        }
    }
    thread_local! {
        static BUF: RefCell<PathBuf> = RefCell::new(PathBuf::new());
    }
    BUF.with_borrow_mut(|path| {
        path.clear();
        for part in parts.drain(..) {
            path.push(&*part)
        }
        Some(Value::String(convert_path(&path)))
    })
}

graphix_package_core::fast_builtin!(pub(crate) JoinPath, JoinPathEv, "sys_join_path", fc_join_path);

graphix_package_core::unit_image_state!(GxTempDirEv);

#[derive(Debug)]
// CR claude for claude: [structure/low] [structure] Args is the builtin dirs_mod.rs's
// dirs_builtin! generates, written out by hand: a FireOnce latch, fired when born() and
// the latch is free, reset at reborn. Give the macro a value closure over the ExecCtx
// and define Args through it. Found by reading; not run. (x-dup-05)
pub(crate) struct Args {
    once: FireOnce,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Args {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_args";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { once: FireOnce::default(), out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let once = FireOnce::decode(buf)?;
        Ok(Box::new(Self { once, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Args {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.once.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &mut [Node<R, E>],
    ) -> &TagValue {
        if ctx.event.born() && self.once.take() {
            let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
            let arr: ValArray =
                pargs.0.iter().map(|s| Value::String(s.clone())).collect();
            self.out.set(TagValue::fired(Value::Array(arr)))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.once.reset();
    }
}

#[derive(Debug)]
pub(crate) struct Exit;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Exit {
    const EFFECT: Effect = Effect::Io(None);
    const NAME: &str = "sys_exit";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Exit {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        // CR claude for eric: [bug/low] `code as i32` wraps, and Unix keeps only the
        // low 8 bits: sys::exit(256) exits with status 0, a failure reported as
        // success. Code 2^32 does the same through the cast. Decide the rule: map a
        // code outside 0..=255 to 1 (or refuse it with a logged failure and exit 1),
        // use i32::try_from in place of `as`, and state the range on `val exit` in
        // mod.gxi. Ran: sys::exit(after_idle(50.ms, 256)) exited 0. (sys-net-05)
        if let Some(Value::I64(code)) = from
            .get_mut(0)
            .and_then(|n| seam_tick(n.update(ctx)))
            .map(|tv| tv.value_cloned())
        {
            use std::io::Write;
            let _ = std::io::stdout().flush();
            let _ = std::io::stderr().flush();
            let asked = ctx
                .libstate
                .get::<graphix_package_core::ExitRequest>()
                .is_some_and(|r| r.request(code as i32));
            if !asked {
                // CR claude for claude: [bug/low] `code as i32`, and then the OS's low
                // 8 bits, turn a code the program computed into one it did not:
                // sys::exit(256) exits 0, so a failure reports success. The ExitRequest
                // path above takes the same cast. Map a code outside 0..=255 to 1 for
                // both uses (or type the gxi argument u8). probe:
                // design/review-2026-10-19/repro/x-panics-exit-code-truncated.gx
                // (x-panics-07)
                std::process::exit(code as i32);
            }
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

graphix_derive::defpackage! {
    builtins => [
        Args,
        Exit,
        GxTempDir,
        TempDirPath,
        JoinPath,
        metadata::IsFile,
        metadata::IsDir,
        metadata::Metadata,
        watch::CreateWatcher,
        watch::WatchApply,
        watch::WatchPath,
        watch::WatchEvents,
        fs::ReadAll,
        fs::ReadAllBin,
        fs::WriteAll,
        fs::WriteAllBin,
        fs::RemoveFile,
        fs::FileOpen,
        fs::FileSeek,
        fs::FileFstat,
        fs::FileTruncate,
        dir::ReadDir,
        dir::CreateDir,
        dir::RemoveDir,
        io::IoRead,
        io::IoReadExact,
        io::IoReadAll,
        io::IoLines<false>,
        io::IoLines<true>,
        io::IoWrite,
        io::IoWriteExact,
        io::IoFlush,
        io::IoClose,
        io::IoStdin,
        io::IoStdout,
        io::IoStderr,
        tcp::TcpConnect,
        tcp::TcpListen,
        tcp::TcpAccept,
        tcp::TcpShutdown,
        tcp::TcpPeerAddr,
        tcp::TcpLocalAddr,
        tcp::TcpListenerAddr,
        process::ProcessSpawn,
        process::ProcessWait,
        process::ProcessKill,
        process::ProcessPid,
        tls::TlsConnect,
        tls::TlsAccept,
        net::Write,
        net::Subscribe,
        net::RpcCall,
        net::List,
        net::ListTable,
        net::Publish as net::Publish<GXRt<X>, X::UserEvent>,
        net::PublishRpc as net::PublishRpc<GXRt<X>, X::UserEvent>,
        time::AfterIdle,
        time::Timer,
        time::Now,
        time::TimeAdd,
        time::TimeSub,
        time::TimeAddDur,
        time::TimeSubDur,
        time::TimeDiff,
        time::TimeScale,
        dirs_mod::HomeDir,
        dirs_mod::CacheDir,
        dirs_mod::ConfigDir,
        dirs_mod::ConfigLocalDir,
        dirs_mod::DataDir,
        dirs_mod::DataLocalDir,
        dirs_mod::ExecutableDir,
        dirs_mod::PreferenceDir,
        dirs_mod::RuntimeDir,
        dirs_mod::StateDir,
        dirs_mod::AudioDir,
        dirs_mod::DesktopDir,
        dirs_mod::DocumentDir,
        dirs_mod::DownloadDir,
        dirs_mod::FontDir,
        dirs_mod::PictureDir,
        dirs_mod::PublicDir,
        dirs_mod::TemplateDir,
        dirs_mod::VideoDir,
    ],
}

#[cfg(test)]
mod test {
    use super::*;
    use futures::poll;
    use std::pin::pin;

    #[tokio::test]
    async fn close_waits_for_whole_stream_operation() {
        let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
        let stream = Arc::new(Halves::new(StreamKind::File(file)));
        let whole = stream.take().await.unwrap();
        let mut close = pin!(stream.close());
        assert!(poll!(close.as_mut()).is_pending());
        drop(whole);
        close.await.unwrap();
        assert!(stream.reader().await.is_none());
        assert!(stream.writer().await.is_none());
        assert!(stream.take().await.is_none());
    }

    #[tokio::test]
    async fn rejecting_non_tcp_restores_stream() {
        let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
        let stream = Halves::new(StreamKind::File(file));
        assert!(stream.take().await.unwrap().into_tcp().is_none());
        assert!(stream.reader().await.is_some());
        assert!(stream.writer().await.is_some());
    }
    #[tokio::test]
    async fn closed_stream_refuses_late_reads() {
        let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
        let stream = Arc::new(Halves::new(StreamKind::File(file)));
        stream.close().await.unwrap();
        let error = stream.until_closed(std::future::ready(Ok(()))).await.unwrap_err();
        assert_eq!(error.kind(), ErrorKind::BrokenPipe);
    }

    #[tokio::test]
    async fn buffered_stream_reads_prefix_and_forwards_writes() {
        use tokio::io::AsyncReadExt;

        let (inner, mut peer) = tokio::io::duplex(16);
        let mut pending: GPooled<Vec<u8>> = GPooled::take();
        pending.extend_from_slice(b"head");
        let mut stream = BufferedRead::new(inner, pending);
        peer.write_all(b"tail").await.unwrap();
        let mut read = [0; 8];
        stream.read_exact(&mut read).await.unwrap();
        assert_eq!(&read, b"headtail");
        stream.write_all(b"reply").await.unwrap();
        let mut written = [0; 5];
        peer.read_exact(&mut written).await.unwrap();
        assert_eq!(&written, b"reply");
    }
}
