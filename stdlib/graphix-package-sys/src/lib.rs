#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::CompactString;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue,
    effects::Effect,
    errf,
    expr::ExprId,
    image::ImageBuf,
    rep,
    typ::{FnType, abstract_uuid},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, ProgramArgs, seam_tick,
};
use graphix_rt::GXRt;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, Value, abstract_type::AbstractWrapper};
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
    time::Duration,
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
/// One representation, nine nominal Graphix types (`sys::fs::File`,
/// `sys::tcp::TcpStream`, `sys::tls::TlsStream`, a type per direction of
/// `sys::process::Child*` and `sys::io::Std*`): the io code is shared,
/// each type carries the trait impls that say what it can do, so a read
/// of a write-only stream is a type error, and the marker is what the
/// abstract registry keys a UUID on.
pub trait StreamMark: 'static + Send + Sync {
    /// The type's canonical Graphix path. Its UUID is derived from it
    /// ([`abstract_uuid`]), so a type test (`File as f`) recognizes
    /// the value by the path alone.
    const PATH: &'static str;
}

/// How long a close waits for its stream's shutdown.
const CLOSE_SHUTDOWN: Duration = Duration::from_secs(5);

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

    /// `op` on the read half under [`Self::until_closed`]: the error tagged
    /// `tag` is `stream unavailable` once the half is closed, else `{what}
    /// failed: ..`.
    pub(crate) async fn on_reader<T>(
        &self,
        tag: &'static str,
        what: &str,
        op: impl AsyncFnOnce(&mut BufferedRead<ReadHalf<StreamKind>>) -> IoResult<T>,
    ) -> Result<T, Value> {
        self.on_half(&self.r, tag, what, op).await
    }

    /// [`Self::on_reader`] over the write half.
    pub(crate) async fn on_writer<T>(
        &self,
        tag: &'static str,
        what: &str,
        op: impl AsyncFnOnce(&mut WriteHalf<StreamKind>) -> IoResult<T>,
    ) -> Result<T, Value> {
        self.on_half(&self.w, tag, what, op).await
    }

    async fn on_half<H, T>(
        &self,
        half: &Mutex<Option<H>>,
        tag: &'static str,
        what: &str,
        op: impl AsyncFnOnce(&mut H) -> IoResult<T>,
    ) -> Result<T, Value> {
        let mut guard = half.lock().await;
        let Some(h) = guard.as_mut() else {
            return Err(errf!(tag, "stream unavailable"));
        };
        self.until_closed(op(h)).await.map_err(|e| errf!(tag, "{what} failed: {e}"))
    }

    pub(crate) async fn reader(
        &self,
    ) -> MutexGuard<'_, Option<BufferedRead<ReadHalf<StreamKind>>>> {
        self.r.lock().await
    }

    #[cfg(test)]
    pub(crate) async fn writer(&self) -> MutexGuard<'_, Option<WriteHalf<StreamKind>>> {
        self.w.lock().await
    }

    pub(crate) fn addrs(&self) -> Option<(std::net::SocketAddr, std::net::SocketAddr)> {
        self.addrs
    }

    pub(crate) fn lines_stopped(&self) -> tokio_watch::Receiver<()> {
        self.stop_lines.get_or_init(|| tokio_watch::channel(()).0).subscribe()
    }

    /// End the stream's line readers: what follows consumes the stream or
    /// moves its position.
    pub(crate) fn stop_lines(&self) {
        if let Some(stop) = self.stop_lines.get() {
            stop.send_replace(());
        }
    }

    /// Borrow the whole stream under both locks; `None` if it is closed.
    pub(crate) async fn take(&self) -> Option<Whole<'_>> {
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

    /// Cancel pending reads and writes, shut down the writer, at most
    /// `CLOSE_SHUTDOWN` (a peer that does not read can hold up a flush or
    /// a TLS close_notify), and release both halves.
    pub(crate) async fn close(self: &Arc<Self>) -> IoResult<()> {
        self.closing.store(true, AtomicOrdering::Release);
        self.closed.notify_waiters();
        let mut r = self.r.lock().await;
        let mut w = self.w.lock().await;
        let res = match w.take() {
            Some(mut w) => tokio::time::timeout(CLOSE_SHUTDOWN, w.shutdown())
                .await
                .unwrap_or_else(|_| Err(ErrorKind::TimedOut.into())),
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

stream_kinds! {
    FileMark => "sys::fs::File", FILE_WRAPPER, wrap_file;
    TcpMark => "sys::tcp::TcpStream", TCP_WRAPPER, wrap_tcp;
    TlsMark => "sys::tls::TlsStream", TLS_WRAPPER, wrap_tls;
    ChildStdinMark => "sys::process::ChildStdin", CHILD_STDIN_WRAPPER, wrap_child_stdin;
    ChildStdoutMark => "sys::process::ChildStdout", CHILD_STDOUT_WRAPPER, wrap_child_stdout;
    ChildStderrMark => "sys::process::ChildStderr", CHILD_STDERR_WRAPPER, wrap_child_stderr;
    StdinMark => "sys::io::Stdin", STDIN_WRAPPER, wrap_stdin;
    StdoutMark => "sys::io::Stdout", STDOUT_WRAPPER, wrap_stdout;
    StderrMark => "sys::io::Stderr", STDERR_WRAPPER, wrap_stderr;
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

dirs_mod::born_builtin!(Args, "sys_args", |ctx| {
    let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
    Value::Array(pargs.0.iter().map(|s| Value::String(s.clone())).collect())
});

#[derive(Debug)]
pub(crate) struct Exit;

impl<R: Rt> BuiltIn<R> for Exit {
    const EFFECT: Effect = Effect::Io(None);
    const NAME: &str = "sys_exit";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self))
    }
}

impl<R: Rt> Apply<R> for Exit {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        if let Some(Value::I64(code)) = from
            .get_mut(0)
            .and_then(|n| seam_tick(n.update(ctx)))
            .map(|tv| tv.value_cloned())
        {
            use std::io::Write;
            let _ = std::io::stdout().flush();
            let _ = std::io::stderr().flush();
            let code = u8::try_from(code).map_or(1, i32::from);
            match ctx.libstate.get::<graphix_package_core::ExitRequest>() {
                Some(r) => r.request(code),
                None => std::process::exit(code),
            }
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
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
        net::Publish as net::Publish<GXRt<X>>,
        net::PublishRpc as net::PublishRpc<GXRt<X>>,
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
