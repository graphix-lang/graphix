use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{PBytes, Value};
use std::sync::Arc;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
};

use crate::{get_stream, StreamKind, StreamValue, STREAM_WRAPPER};

// ── IoRead ─────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoReadEv;

impl EvalCachedAsync for IoReadEv {
    const NAME: &str = "sys_io_read";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<StreamKind>>>, u64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            let mut buf = vec![0u8; n as usize];
            match s.read(&mut buf).await {
                Ok(n) => {
                    buf.truncate(n);
                    Value::Bytes(PBytes::new(Bytes::from(buf)))
                }
                Err(e) => errf!("IOError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type IoRead = CachedArgsAsync<IoReadEv>;

// ── IoReadExact ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoReadExactEv;

impl EvalCachedAsync for IoReadExactEv {
    const NAME: &str = "sys_io_read_exact";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<StreamKind>>>, u64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            let mut buf = vec![0u8; n as usize];
            let mut pos = 0;
            while pos < buf.len() {
                match s.read(&mut buf[pos..]).await {
                    Ok(0) => break,
                    Ok(n) => pos += n,
                    Err(e) => return errf!("IOError", "read_exact failed: {e}"),
                }
            }
            buf.truncate(pos);
            Value::Bytes(PBytes::new(Bytes::from(buf)))
        }
    }
}

pub(crate) type IoReadExact = CachedArgsAsync<IoReadExactEv>;

// ── IoWrite ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoWriteEv;

impl EvalCachedAsync for IoWriteEv {
    const NAME: &str = "sys_io_write";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<StreamKind>>>, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            match s.write(&data).await {
                Ok(n) => Value::U64(n as u64),
                Err(e) => errf!("IOError", "write failed: {e}"),
            }
        }
    }
}

pub(crate) type IoWrite = CachedArgsAsync<IoWriteEv>;

// ── IoWriteExact ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoWriteExactEv;

impl EvalCachedAsync for IoWriteExactEv {
    const NAME: &str = "sys_io_write_exact";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<StreamKind>>>, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            match s.write_all(&data).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "write_exact failed: {e}"),
            }
        }
    }
}

pub(crate) type IoWriteExact = CachedArgsAsync<IoWriteExactEv>;

// ── IoFlush ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoFlushEv;

impl EvalCachedAsync for IoFlushEv {
    const NAME: &str = "sys_io_flush";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<Mutex<Option<StreamKind>>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            match s.flush().await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "flush failed: {e}"),
            }
        }
    }
}

pub(crate) type IoFlush = CachedArgsAsync<IoFlushEv>;

// ── Stdio constructors ────────────────────────────────────────

fn wrap_stream(kind: StreamKind) -> Value {
    STREAM_WRAPPER.wrap(StreamValue { inner: Arc::new(Mutex::new(Some(kind))) })
}

#[derive(Debug, Default)]
pub(crate) struct IoStdinEv;

impl EvalCachedAsync for IoStdinEv {
    const NAME: &str = "sys_io_stdin";
    const NEEDS_CALLSITE: bool = false;
    type Args = ();

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stream(StreamKind::Stdin(tokio::io::stdin())) }
    }
}

pub(crate) type IoStdin = CachedArgsAsync<IoStdinEv>;

#[derive(Debug, Default)]
pub(crate) struct IoStdoutEv;

impl EvalCachedAsync for IoStdoutEv {
    const NAME: &str = "sys_io_stdout";
    const NEEDS_CALLSITE: bool = false;
    type Args = ();

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stream(StreamKind::Stdout(tokio::io::stdout())) }
    }
}

pub(crate) type IoStdout = CachedArgsAsync<IoStdoutEv>;

#[derive(Debug, Default)]
pub(crate) struct IoStderrEv;

impl EvalCachedAsync for IoStderrEv {
    const NAME: &str = "sys_io_stderr";
    const NEEDS_CALLSITE: bool = false;
    type Args = ();

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stream(StreamKind::Stderr(tokio::io::stderr())) }
    }
}

pub(crate) type IoStderr = CachedArgsAsync<IoStderrEv>;
