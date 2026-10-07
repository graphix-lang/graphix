use anyhow::Result;
use arcstr::ArcStr;
use bytes::Bytes;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync, seam_value};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::sync::{Arc, LazyLock};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::{Halves, StreamKind, get_stream, stream_of, wrap_stdio};

#[derive(Debug, Default)]
pub(crate) struct IoReadEv;

impl EvalCachedAsync for IoReadEv {
    type Args = (Arc<Halves>, u64);

    const NAME: &str = "sys_io_read";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.reader().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            let mut buf: LPooled<Vec<u8>> = LPooled::take();
            buf.resize(n.min(MAX_READ) as usize, 0);
            match s.read(&mut buf).await {
                Ok(n) => Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf[..n]))),
                Err(e) => errf!("IOError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type IoRead = CachedArgsAsync<IoReadEv>;

/// The most one `read` returns, whatever it asks for: a read may return
/// fewer bytes, and the request must not size the allocation.
const MAX_READ: u64 = 1 << 20;

/// The longest line a line reader holds; a longer one is an IOError, so a
/// peer that never sends a newline cannot grow it without bound.
const MAX_LINE: usize = 16 << 20;

static LBATCH: LazyLock<Pool<Vec<(BindId, Value)>>> =
    LazyLock::new(|| Pool::new(32, 16384));

/// Read `stream` to its end, framing it into lines and delivering them
/// into the graph.
///
/// Framing is at the BYTE level so a multi-byte character split across a
/// read boundary survives; only complete lines are decoded, and lossily.
///
/// `batched` sends ONE array per read; unbatched sends one event per
/// line, which the runtime spreads across cycles.
async fn line_reader(
    stream: Arc<Halves>,
    id: BindId,
    batched: bool,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
) {
    let mut held: LPooled<Vec<u8>> = LPooled::take();
    let mut chunk: LPooled<Vec<u8>> = LPooled::take();
    chunk.resize(65536, 0);
    loop {
        let n = {
            let mut guard = stream.reader().await;
            let Some(s) = guard.as_mut() else { break };
            match s.read(&mut chunk).await {
                Ok(0) => {
                    // the end: an unterminated last line is still a line
                    if !held.is_empty() {
                        let line =
                            Value::String(String::from_utf8_lossy(&held).as_ref().into());
                        let mut b = LBATCH.take();
                        let line = match batched {
                            true => Value::Array(ValArray::from([line])),
                            false => line,
                        };
                        b.push((id, line));
                        let _ = tx.send(b).await;
                    }
                    break;
                }
                Ok(n) => n,
                Err(e) => {
                    let mut b = LBATCH.take();
                    b.push((id, errf!("IOError", "read failed: {e}")));
                    let _ = tx.send(b).await;
                    break;
                }
            }
        };
        let scanned = held.len();
        held.extend_from_slice(&chunk[..n]);
        let mut out = LBATCH.take();
        let mut lines: LPooled<Vec<Value>> = LPooled::take();
        let mut start = 0;
        let mut from = scanned;
        while let Some(off) = held[from..].iter().position(|b| *b == b'\n') {
            let end = from + off;
            // Tolerate CRLF so a line framed on one platform reads the
            // same on the other.
            let line = match held[start..end].last() {
                Some(b'\r') => &held[start..end - 1],
                _ => &held[start..end],
            };
            let line = Value::String(String::from_utf8_lossy(line).as_ref().into());
            if batched {
                lines.push(line);
            } else {
                out.push((id, line));
            }
            start = end + 1;
            from = start;
        }
        held.drain(..start);
        if held.len() > MAX_LINE {
            out.push((id, errf!("IOError", "a line is longer than {MAX_LINE} bytes")));
            let _ = tx.send(out).await;
            break;
        }
        if batched && !lines.is_empty() {
            out.push((id, Value::Array(ValArray::from_iter_exact(lines.drain(..)))));
        }
        if !out.is_empty() && tx.send(out).await.is_err() {
            break;
        }
    }
}

/// `Lines::lines` (BATCHED = false) and `Lines::lines_batched`
/// (BATCHED = true). Shared by every stream kind.
#[derive(Debug)]
pub(crate) struct IoLines<const BATCHED: bool> {
    id: BindId,
    top_id: ExprId,
    /// The stream being read and its reader.
    reading: Option<(Arc<Halves>, tokio::task::AbortHandle)>,
    out: TagValue,
}

impl<const BATCHED: bool> IoLines<BATCHED> {
    fn stop(&mut self) {
        if let Some((_, reader)) = self.reading.take() {
            reader.abort();
        }
    }
}

impl<R: Rt, E: UserEvent, const BATCHED: bool> BuiltIn<R, E> for IoLines<BATCHED> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = if BATCHED { "sys_io_lines_batched" } else { "sys_io_lines" };

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, reading: None, out: TagValue::phantom() }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, reading: None, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent, const BATCHED: bool> Apply<R, E> for IoLines<BATCHED> {
    /// A started instance has a reader task holding the stream.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.reading.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        // One reader per stream: a re-delivery of the same handle is
        // ignored (the reader consumes it); another stream replaces the
        // reader, under a fresh id so the old one's lines land nowhere.
        if let Some(tv) = seam_value(from[0].update(ctx))
            && tv.is_fired()
            && let Some(stream) = stream_of(&tv.value_cloned())
            && !self.reading.as_ref().is_some_and(|(s, _)| Arc::ptr_eq(s, &stream))
        {
            if self.reading.is_some() {
                self.stop();
                ctx.unref_var(self.id, self.top_id);
                self.id = BindId::new();
                ctx.rt.ref_var(self.id, self.top_id);
            }
            let (tx, rx) = mpsc::channel(3);
            ctx.rt.watch_var(rx);
            let reader = tokio::spawn(line_reader(stream.clone(), self.id, BATCHED, tx));
            self.reading = Some((stream, reader.abort_handle()));
        }
        match ctx.event.variables.get(&self.id) {
            Some(tv) => self.out.set(TagValue::fired(tv.value_cloned())),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.stop();
        ctx.unref_var(self.id, self.top_id);
        ctx.rt.store_remove(&self.id);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
    }
}

#[derive(Debug, Default)]
pub(crate) struct IoReadExactEv;

impl EvalCachedAsync for IoReadExactEv {
    type Args = (Arc<Halves>, u64);

    const NAME: &str = "sys_io_read_exact";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, n): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.reader().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOError", "stream unavailable"),
            };
            // the buffer grows as bytes arrive, not to n
            let mut buf = Vec::new();
            match (&mut *s).take(n).read_to_end(&mut buf).await {
                Ok(_) => Value::Bytes(PBytes::new(Bytes::from(buf))),
                Err(e) => errf!("IOError", "read_exact failed: {e}"),
            }
        }
    }
}

pub(crate) type IoReadExact = CachedArgsAsync<IoReadExactEv>;

#[derive(Debug, Default)]
pub(crate) struct IoReadAllEv;

impl EvalCachedAsync for IoReadAllEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_io_read_all";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.reader().await;
            let Some(s) = guard.as_mut() else {
                return errf!("IOError", "stream unavailable");
            };
            let mut buf = Vec::new();
            match s.read_to_end(&mut buf).await {
                Ok(_) => Value::Bytes(PBytes::new(Bytes::from(buf))),
                Err(e) => errf!("IOError", "read_all failed: {e}"),
            }
        }
    }
}

pub(crate) type IoReadAll = CachedArgsAsync<IoReadAllEv>;

#[derive(Debug, Default)]
pub(crate) struct IoWriteEv;

impl EvalCachedAsync for IoWriteEv {
    type Args = (Arc<Halves>, Bytes);

    const NAME: &str = "sys_io_write";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.writer().await;
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

#[derive(Debug, Default)]
pub(crate) struct IoWriteExactEv;

impl EvalCachedAsync for IoWriteExactEv {
    type Args = (Arc<Halves>, Bytes);

    const NAME: &str = "sys_io_write_exact";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.writer().await;
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

#[derive(Debug, Default)]
pub(crate) struct IoFlushEv;

impl EvalCachedAsync for IoFlushEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_io_flush";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.writer().await;
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

#[derive(Debug, Default)]
pub(crate) struct IoCloseEv;

impl EvalCachedAsync for IoCloseEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_io_close";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match stream.close().await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "close failed: {e}"),
            }
        }
    }
}

pub(crate) type IoClose = CachedArgsAsync<IoCloseEv>;

#[derive(Debug, Default)]
pub(crate) struct IoStdinEv;

impl EvalCachedAsync for IoStdinEv {
    type Args = ();

    const NAME: &str = "sys_io_stdin";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stdio(StreamKind::Stdin(tokio::io::stdin())) }
    }
}

pub(crate) type IoStdin = CachedArgsAsync<IoStdinEv>;

#[derive(Debug, Default)]
pub(crate) struct IoStdoutEv;

impl EvalCachedAsync for IoStdoutEv {
    type Args = ();

    const NAME: &str = "sys_io_stdout";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stdio(StreamKind::Stdout(tokio::io::stdout())) }
    }
}

pub(crate) type IoStdout = CachedArgsAsync<IoStdoutEv>;

#[derive(Debug, Default)]
pub(crate) struct IoStderrEv;

impl EvalCachedAsync for IoStderrEv {
    type Args = ();

    const NAME: &str = "sys_io_stderr";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(0)?.as_ref()?;
        Some(())
    }

    fn eval((): Self::Args) -> impl Future<Output = Value> + Send {
        async { wrap_stdio(StreamKind::Stderr(tokio::io::stderr())) }
    }
}

pub(crate) type IoStderr = CachedArgsAsync<IoStderrEv>;

graphix_package_core::unit_image_state!(
    IoReadEv,
    IoReadExactEv,
    IoReadAllEv,
    IoWriteEv,
    IoWriteExactEv,
    IoFlushEv,
    IoCloseEv,
    IoStdinEv,
    IoStdoutEv,
    IoStderrEv
);
