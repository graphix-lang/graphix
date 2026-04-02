#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::{
    errf, expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope,
    UserEvent,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, ProgramArgs,
};
use graphix_rt::GXRt;
use netidx_value::{abstract_type::AbstractWrapper, Abstract, ValArray, Value};
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    cmp::Ordering,
    hash::{Hash, Hasher},
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
pub(crate) mod metadata;
pub(crate) mod net;
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
            StreamKind::Stdout(_) | StreamKind::Stderr(_) => Poll::Ready(Err(
                std::io::Error::new(std::io::ErrorKind::Unsupported, "cannot read from stdout/stderr"),
            )),
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
            StreamKind::Stdin(_) => Poll::Ready(Err(
                std::io::Error::new(std::io::ErrorKind::Unsupported, "cannot write to stdin"),
            )),
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
            StreamKind::Stdin(_) => Poll::Ready(Ok(())),
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
            StreamKind::Stdin(_) => Poll::Ready(Ok(())),
        }
    }
}

// ── StreamValue ────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct StreamValue {
    pub inner: Arc<Mutex<Option<StreamKind>>>,
}

impl PartialEq for StreamValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for StreamValue {}

impl PartialOrd for StreamValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StreamValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl Hash for StreamValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
    }
}

graphix_package_core::impl_no_pack!(StreamValue);

pub static STREAM_WRAPPER: LazyLock<AbstractWrapper<StreamValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xb7, 0xc8, 0xd9, 0xea, 0xfb, 0x0c, 0x4d, 0x1e, 0x2f, 0x30, 0x41, 0x52, 0x63,
        0x74, 0x85, 0x96,
    ]);
    Abstract::register::<StreamValue>(id).expect("failed to register StreamValue")
});

pub(crate) fn wrap_file(file: tokio::fs::File) -> Value {
    STREAM_WRAPPER
        .wrap(StreamValue { inner: Arc::new(Mutex::new(Some(StreamKind::File(file)))) })
}

pub(crate) fn wrap_tcp(stream: tokio::net::TcpStream) -> Value {
    STREAM_WRAPPER
        .wrap(StreamValue { inner: Arc::new(Mutex::new(Some(StreamKind::Tcp(stream)))) })
}

pub fn get_stream(
    cached: &CachedVals,
    idx: usize,
) -> Option<Arc<Mutex<Option<StreamKind>>>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let sv = a.downcast_ref::<StreamValue>()?;
            Some(sv.inner.clone())
        }
        _ => None,
    }
}

pub fn get_stream_value(cached: &CachedVals, idx: usize) -> Option<StreamValue> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => a.downcast_ref::<StreamValue>().cloned(),
        _ => None,
    }
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

static TEMPDIR_WRAPPER: LazyLock<AbstractWrapper<TempDirValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xa1, 0xb2, 0xc3, 0xd4, 0xe5, 0xf6, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x78,
    ]);
    Abstract::register::<TempDirValue>(id).expect("failed to register TempDirValue")
});

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
    const NAME: &str = "sys_tempdir";
    const NEEDS_CALLSITE: bool = false;
    type Args = TempDirArgs;

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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TempDirPathEv {
    const NAME: &str = "sys_tempdir_path";
    const NEEDS_CALLSITE: bool = false;

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
    const NAME: &str = "sys_join_path";
    const NEEDS_CALLSITE: bool = false;

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
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Args {
    const NAME: &str = "sys_args";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { fired: false }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Args {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if event.init && !self.fired {
            self.fired = true;
            let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
            let arr: ValArray =
                pargs.0.iter().map(|s| Value::String(s.clone())).collect();
            Some(Value::Array(arr))
        } else {
            None
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.fired = false;
    }
}

graphix_derive::defpackage! {
    builtins => [
        Args,
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
        io::IoWrite,
        io::IoWriteExact,
        io::IoFlush,
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
