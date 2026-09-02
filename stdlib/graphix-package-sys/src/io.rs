use anyhow::Result;
use arcstr::ArcStr;
use bytes::Bytes;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect, errf, expr::ExprId, typ::FnType,
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync, seam_value};
use netidx_value::{PBytes, ValArray, Value};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::sync::{Arc, LazyLock};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::sync::Mutex;

use crate::{StreamKind, get_stream, stream_of, wrap_stdio};

// ── IoRead ─────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoReadEv;

impl EvalCachedAsync for IoReadEv {
    type Args = (Arc<Mutex<Option<StreamKind>>>, u64);

    const NAME: &str = "sys_io_read";

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
            let mut buf: LPooled<Vec<u8>> = LPooled::take();
            buf.resize(n as usize, 0);
            match s.read(&mut buf).await {
                Ok(n) => Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf[..n]))),
                Err(e) => errf!("IOError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type IoRead = CachedArgsAsync<IoReadEv>;

// ── IoLines / IoLinesBatched ───────────────────────────────────

static LBATCH: LazyLock<Pool<Vec<(BindId, Value)>>> =
    LazyLock::new(|| Pool::new(32, 16384));

/// Read `stream` to its end, framing it into lines and delivering them
/// into the graph.
///
/// Framing is at the BYTE level, which is the whole reason this is not
/// a `read` loop written in Graphix: a multi-byte character split across
/// a read boundary is destroyed by decoding each chunk on its own, and
/// nothing the caller does controls where those boundaries fall. Only
/// complete lines are decoded, and lossily — one line of invalid UTF-8
/// must not take down the stream.
///
/// `batched` picks the delivery shape. Batched sends ONE array per read,
/// which is the cheap form. Unbatched sends one entry per line and lets
/// the runtime spread repeats of a BindId across cycles (the
/// `push_var_event!` requeue), which is `array::iter`'s cadence for
/// free — no queue of our own.
async fn line_reader(
    stream: Arc<Mutex<Option<StreamKind>>>,
    id: BindId,
    batched: bool,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
) {
    let mut held: LPooled<Vec<u8>> = LPooled::take();
    let mut chunk: LPooled<Vec<u8>> = LPooled::take();
    chunk.resize(65536, 0);
    loop {
        let n = {
            let mut guard = stream.lock().await;
            let Some(s) = guard.as_mut() else { break };
            match s.read(&mut chunk).await {
                // EOF. A trailing fragment with no newline is NOT a
                // line and is dropped, exactly as `tail` would.
                Ok(0) => break,
                Ok(n) => n,
                Err(e) => {
                    let mut b = LBATCH.take();
                    b.push((id, errf!("IOError", "read failed: {e}")));
                    let _ = tx.send(b).await;
                    break;
                }
            }
        };
        held.extend_from_slice(&chunk[..n]);
        let mut out = LBATCH.take();
        let mut lines: LPooled<Vec<Value>> = LPooled::take();
        let mut start = 0;
        while let Some(off) = held[start..].iter().position(|b| *b == b'\n') {
            let end = start + off;
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
        }
        held.drain(..start);
        if batched && !lines.is_empty() {
            out.push((id, Value::Array(ValArray::from_iter_exact(lines.drain(..)))));
        }
        if !out.is_empty() && tx.send(out).await.is_err() {
            break;
        }
    }
}

/// `Lines::lines` (BATCHED = false) and `Lines::lines_batched`
/// (BATCHED = true): one event per line, or one array of every line the
/// read made available. Shared by every stream kind — the trait
/// implementations in the `.gx` files decide who gets them.
#[derive(Debug)]
pub(crate) struct IoLines<const BATCHED: bool> {
    id: BindId,
    top_id: ExprId,
    started: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent, const BATCHED: bool> BuiltIn<R, E> for IoLines<BATCHED> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = if BATCHED { "sys_io_lines_batched" } else { "sys_io_lines" };

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(Self { id, top_id, started: false, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent, const BATCHED: bool> Apply<R, E> for IoLines<BATCHED> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        // One reader per instance, started by the first stream that
        // arrives. A stream is consumed as it is read, so re-arming on a
        // later delivery of the same handle would race the reader for
        // its bytes.
        if let Some(tv) = seam_value(from[0].update(ctx, event))
            && tv.is_fired()
            && !self.started
            && let Some(stream) = stream_of(&tv.value_cloned())
        {
            self.started = true;
            let (tx, rx) = mpsc::channel(3);
            ctx.rt.watch_var(rx);
            let id = self.id;
            tokio::spawn(line_reader(stream, id, BATCHED, tx));
        }
        match event.variables.get(&self.id) {
            Some(tv) => self.out.set(TagValue::fired(tv.value_cloned())),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The reader owns the stream's position; a slept-then-woken
        // instance must not start a second one over the same bytes, so
        // `started` stays set. Re-key the wake registration exactly as
        // `array::iter` does.
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Delivery rides watch_var (async); the only state is the wake
        // registration and the reader, neither of which is replay memory.
    }
}

// ── IoReadExact ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoReadExactEv;

impl EvalCachedAsync for IoReadExactEv {
    type Args = (Arc<Mutex<Option<StreamKind>>>, u64);

    const NAME: &str = "sys_io_read_exact";

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
            let mut buf: LPooled<Vec<u8>> = LPooled::take();
            buf.resize(n as usize, 0);
            let mut pos = 0;
            while pos < buf.len() {
                match s.read(&mut buf[pos..]).await {
                    Ok(0) => break,
                    Ok(n) => pos += n,
                    Err(e) => return errf!("IOError", "read_exact failed: {e}"),
                }
            }
            Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf[..pos])))
        }
    }
}

pub(crate) type IoReadExact = CachedArgsAsync<IoReadExactEv>;

// ── IoWrite ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoWriteEv;

impl EvalCachedAsync for IoWriteEv {
    type Args = (Arc<Mutex<Option<StreamKind>>>, Bytes);

    const NAME: &str = "sys_io_write";

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
    type Args = (Arc<Mutex<Option<StreamKind>>>, Bytes);

    const NAME: &str = "sys_io_write_exact";

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
    type Args = Arc<Mutex<Option<StreamKind>>>;

    const NAME: &str = "sys_io_flush";

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

// ── IoClose ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IoCloseEv;

impl EvalCachedAsync for IoCloseEv {
    type Args = Arc<Mutex<Option<StreamKind>>>;

    const NAME: &str = "sys_io_close";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            // Take the kind out first: concurrent ops see the stream
            // closed immediately, and a second close is a no-op.
            let kind = stream.lock().await.take();
            let Some(mut kind) = kind else {
                return Value::Null;
            };
            match kind.shutdown().await {
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
