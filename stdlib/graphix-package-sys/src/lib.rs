#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::CompactString;
use graphix_compiler::{
    Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::EffectKind,
    errf,
    expr::ExprId,
    typ::{FnType, abstract_uuid},
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, ProgramArgs,
    seam_tick,
};
use graphix_rt::GXRt;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, ValArray, Value, abstract_type::AbstractWrapper};
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    cmp::Ordering,
    hash::{Hash, Hasher},
    marker::PhantomData,
    path::{Path, PathBuf},
    pin::Pin,
    sync::{Arc, LazyLock},
    task::{Context, Poll},
};
use tempfile::TempDir;
use tokio::{
    io::{AsyncRead, AsyncWrite, ReadBuf},
    sync::Mutex,
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

// ── StreamKind ─────────────────────────────────────────────────

pub enum StreamKind {
    File(tokio::fs::File),
    Tcp(tokio::net::TcpStream),
    Tls(tokio_rustls::TlsStream<tokio::net::TcpStream>),
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
                Some(tcp)
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

// ── Streams ────────────────────────────────────────────────────

/// Which kind of stream a handle is.
///
/// ONE representation, FIVE nominal types: read/write/close are the
/// same code whatever is behind the descriptor, so [`StreamKind`]
/// stays one enum — but `sys::fs::File`, `sys::tcp::TcpStream`,
/// `sys::tls::TlsStream`, `sys::process::Pipe` and `sys::io::Stdio`
/// are five distinct types in Graphix, each carrying the trait
/// implementations that say what it can do (`design/traits.md` §6).
/// The marker is what makes each a distinct RUST type, which is what
/// the abstract registry keys a UUID on.
pub trait StreamMark: 'static + Send + Sync {
    /// The type's canonical Graphix path. Its UUID is derived from it
    /// ([`abstract_uuid`]), so a type test (`File as f`) recognizes
    /// the value by the path alone.
    const PATH: &'static str;
}

pub struct Stream<K: StreamMark> {
    pub inner: Arc<Mutex<Option<StreamKind>>>,
    mark: PhantomData<K>,
}

impl<K: StreamMark> Stream<K> {
    fn new(kind: StreamKind) -> Self {
        Self::from_inner(Arc::new(Mutex::new(Some(kind))))
    }

    /// A handle of this kind onto an EXISTING stream. `tls::connect`
    /// mints one: the TLS session and the TCP handle it was built
    /// from are the same socket, and both handles see it.
    pub(crate) fn from_inner(inner: Arc<Mutex<Option<StreamKind>>>) -> Self {
        Stream { inner, mark: PhantomData }
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

        /// The stream behind `v`, whatever kind of handle it is. The
        /// io builtins are shared by every kind — the TYPE says which
        /// operations are legal, and the trait implementations in the
        /// `.gx` files are what enforce it.
        pub fn stream_of(v: &Value) -> Option<Arc<Mutex<Option<StreamKind>>>> {
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
    PipeMark => "sys::process::Pipe", PIPE_WRAPPER, wrap_pipe;
    StdioMark => "sys::io::Stdio", STDIO_WRAPPER, wrap_stdio;
}

pub fn get_stream(
    cached: &CachedVals,
    idx: usize,
) -> Option<Arc<Mutex<Option<StreamKind>>>> {
    stream_of(cached.0.get(idx)?.as_ref()?)
}

// ── TempDir ────────────────────────────────────────────────────

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
        if cached.0.iter().any(|v| v.is_none()) {
            None
        } else {
            let dir = cached.get::<Option<ArcStr>>(0).flatten();
            let name = cached
                .get::<Option<(ArcStr, ArcStr)>>(1)
                .and_then(|v| v)
                .and_then(|(tag, v)| match &*tag {
                    "Prefix" => Some(Name::Prefix(v)),
                    "Suffix" => Some(Name::Suffix(v)),
                    _ => None,
                });
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

#[derive(Debug, Default)]
pub(crate) struct TempDirPathEv;

// sys::tempdir_path returns a path string from a TempDir handle. Pure
// transform, sync.
impl<R: Rt, E: UserEvent> EvalCached<R, E> for TempDirPathEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "sys_tempdir_path";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let v = from.0.first()?.as_ref()?;
        match v {
            Value::Abstract(a) => {
                let td = a.downcast_ref::<TempDirValue>()?;
                Some(Value::String(td.path.clone()))
            }
            _ => None,
        }
    }
}

pub(crate) type TempDirPath = CachedArgs<TempDirPathEv>;

pub(crate) fn convert_path(path: &Path) -> ArcStr {
    thread_local! {
        static BUF: RefCell<String> = RefCell::new(String::new());
    }
    BUF.with_borrow_mut(|buf| {
        buf.clear();
        use std::fmt::Write;
        write!(buf, "{}", path.display()).unwrap();
        ArcStr::from(buf.as_str())
    })
}

#[derive(Debug, Default)]
pub(crate) struct JoinPathEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for JoinPathEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "sys_join_path";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut parts: LPooled<Vec<ArcStr>> = LPooled::take();
        for part in from.0.iter() {
            match part {
                None => return None,
                Some(Value::String(s)) => parts.push(s.clone()),
                Some(Value::Array(a)) => {
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
}

pub(crate) type JoinPath = CachedArgs<JoinPathEv>;

// ── Args ──────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct Args {
    fired: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Args {
    // Fires once on init with the cmd-line args — same-cycle output,
    // but NOT replayable (the `fired` latch), so it must not be Sync:
    // a fused HOF loop's shared DynCall slot instance would pend after
    // the first element (the sys::dirs class, soak jul07b). Async
    // de-fuses it.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "sys_args";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { fired: false, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Args {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if event.init && !self.fired {
            self.fired = true;
            let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
            let arr: ValArray =
                pargs.0.iter().map(|s| Value::String(s.clone())).collect();
            self.out.set(TagValue::fired(Value::Array(arr)))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.fired = false;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

// ── Exit ──────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct Exit;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Exit {
    // exit consumes its arg and terminates the process; no future-cycle
    // output. Sync.
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "sys_exit";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Exit {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(Value::I64(code)) = from
            .get_mut(0)
            .and_then(|n| seam_tick(n.update(ctx, event)))
            .map(|tv| tv.value_cloned())
        {
            use std::io::Write;
            let _ = std::io::stdout().flush();
            let _ = std::io::stderr().flush();
            std::process::exit(code as i32);
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
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
