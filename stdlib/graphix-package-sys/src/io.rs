use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{PBytes, Value};
use std::sync::Arc;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
};

use crate::{StreamKind, get_stream};

// ── IoRead ─────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoReadEv;

impl EvalCachedAsync for IoReadEv {
    const NAME: &str = "sys_io_read";
    deftype!("fn(string, u64) -> Result<bytes, `IOError(string)>");
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
    deftype!("fn(string, u64) -> Result<bytes, `IOError(string)>");
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
    deftype!("fn(string, bytes) -> Result<u64, `IOError(string)>");
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
    deftype!("fn(string, bytes) -> Result<null, `IOError(string)>");
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
    deftype!("fn(string) -> Result<null, `IOError(string)>");
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
