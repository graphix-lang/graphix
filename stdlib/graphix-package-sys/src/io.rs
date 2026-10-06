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
use tokio::sync::Mutex;

use crate::{StreamKind, get_stream, stream_of, wrap_stdio};

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
            // CR claude for eric: [bug] This allocates and zeroes n bytes before
            // reading, and read_exact does the same at line 227, so the caller's n sets
            // the allocation whatever the stream holds. n = u64:4611686018427387904
            // aborts the process (SIGABRT, "memory allocation of ... bytes failed"). n
            // >= 2^63 panics "capacity overflow" in the task, and that call site never
            // answers again. n = 1 GiB on a 12-byte file peaks at 1 GB RSS, and a
            // length prefix read from a peer and passed to read_exact lets the peer
            // pick n. read may return fewer than n bytes, so its buffer can be capped;
            // read_exact can grow as bytes arrive. probe:
            // design/review-2026-10-05/repro/x-panics-12.gx (x-panics-12)
            buf.resize(n as usize, 0);
            match s.read(&mut buf).await {
                Ok(n) => Value::Bytes(PBytes::new(Bytes::copy_from_slice(&buf[..n]))),
                Err(e) => errf!("IOError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type IoRead = CachedArgsAsync<IoReadEv>;

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
                // CR claude for eric: [risk] The rationale above is false: `printf
                // 'a\nb' | tail -n 1` prints b. The reader stops at EOF and does not
                // follow the stream, so the held fragment is the stream's real last
                // line, and it is lost. A file without a trailing newline, or a child
                // running `printf 'first\nlast'`, yields only "first". BufRead::lines
                // and tokio_util's LinesCodec::decode_eof emit that line. Emit `held`
                // as a final line at EOF and update io.gxi:72-74, or keep the drop and
                // delete the false comment. probe:
                // design/review-2026-10-05/repro/sys-io-17.gx (sys-io-17)
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
        // CR claude for eric: [perf] Each read starts the newline search over at offset
        // 0 of `held`, rescanning the partial line already known to hold no '\n'. One
        // S-byte line therefore costs about S²/128K compares: a single 32 MiB line took
        // 2.4 s through lines_batched (16 MiB 0.9 s, 8 MiB 0.17 s), against 0.05 s for
        // 32 MiB of 1 KiB lines. Start the first search at the length `held` had before
        // this read's extend. `held` also has no cap, so a peer that never sends '\n'
        // grows it without limit. A maximum line length, past which the reader returns
        // an IOError, would bound a socket reader. (sys-io-10)
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
/// (BATCHED = true). Shared by every stream kind.
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
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, started: false, out: TagValue::phantom() }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, started: false, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent, const BATCHED: bool> Apply<R, E> for IoLines<BATCHED> {
    /// A started instance has a reader task holding the stream.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.started {
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
        // One reader per instance, started by the first stream that
        // arrives. A stream is consumed as it is read, so re-arming on a
        // later delivery of the same handle would race the reader.
        // CR claude for eric: [bug] The `started` latch runs one detached reader for
        // the first stream and ignores every later delivery. When the argument becomes
        // a different stream (a reconnect, a restarted child, a rotated file), this
        // call keeps delivering the old stream's lines and never reads the new one. No
        // handle to the task is kept, so neither a new stream nor `delete` stops the
        // old reader: it reads into a dead id until EOF and races any later reader of
        // the same stream for its bytes. Only a re-delivery of the same handle
        // (`Arc::ptr_eq`) should be ignored. A different stream should abort the old
        // reader and start a new one under a fresh id, and `delete` should abort it.
        // probe: design/review-2026-10-05/repro/sys-io-08.gx (after `s` becomes b's
        // stdout it prints a5..a8 and never a b line); the delete half:
        // design/review-2026-10-05/repro/x-node-contract-03.gx. (sys-io-08)
        if let Some(tv) = seam_value(from[0].update(ctx))
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
        match ctx.event.variables.get(&self.id) {
            Some(tv) => self.out.set(TagValue::fired(tv.value_cloned())),
            None => self.out.ride(),
        }
    }

    // CR claude for eric: [bug] delete only unrefs the id: the line_reader task that
    // update spawned keeps the stream and reads it into the dead id until EOF, holding
    // the stream's lock, its watch channel and a store entry. A fresh instance on the
    // same stream (a regrown collection slot, a replaced dynamic callee, a re-reached
    // recursion depth) then takes turns with it on the lock and gets every other line.
    // Keep the spawn's AbortHandle and abort it here, as DbSubscribe and HttpServe do,
    // and store_remove the id. Related: a started instance ignores a different stream
    // that arrives later, so after the first of two streams is removed from an
    // array::map, the remaining slot keeps reading the removed stream while the deleted
    // slot's reader eats the other one. probe:
    // design/review-2026-10-05/repro/x-node-contract-03.gx (x-node-contract-03)
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
    }
}

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

graphix_package_core::unit_image_state!(
    IoReadEv,
    IoReadExactEv,
    IoWriteEv,
    IoWriteExactEv,
    IoFlushEv,
    IoCloseEv,
    IoStdinEv,
    IoStdoutEv,
    IoStderrEv
);
