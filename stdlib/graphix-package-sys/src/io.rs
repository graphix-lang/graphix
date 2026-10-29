use anyhow::Result;
use bytes::{BufMut, Bytes};
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, Reclaim, RtVar,
    seam_value,
};
use netidx_core::pack::PackError;
use netidx_value::{PBytes, ValArray, Value};
use parking_lot::Mutex;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::sync::{Arc, LazyLock};
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWriteExt},
    sync::{Notify, watch},
};

use crate::{
    Halves, ReadTicket, StreamKind, get_stream, stream_of, wrap_stderr, wrap_stdin,
    wrap_stdout,
};

#[derive(Debug, Default)]
pub(crate) struct IoReadEv;

impl EvalCachedAsync for IoReadEv {
    type Args = (Arc<Halves>, u64, Arc<ReadTicket>);

    const NAME: &str = "sys_io_read";
    const CANCEL_SAFE: bool = false;

    fn reclaim((_, _, ticket): &Self::Args) -> Option<Reclaim> {
        settle(ticket)
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let ticket = stream.ticket();
        Some((stream, cached.get::<u64>(1)?, ticket))
    }

    fn eval((stream, n, ticket): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            stream
                .on_reader("IOError", "read", async |s| {
                    ticket.enter();
                    read_chunk(s, n).await
                })
                .await
                .map_or_else(|e| e, |bytes| Value::Bytes(PBytes::new(bytes)))
        }
    }
}

/// Settle a read's reply through its ticket: answered, or its bytes given
/// back to the stream.
fn settle(ticket: &Arc<ReadTicket>) -> Option<Reclaim> {
    let ticket = ticket.clone();
    Some(Box::new(move |reply| ticket.settle(reply)))
}

async fn read_chunk(
    reader: &mut (impl AsyncRead + Unpin),
    n: u64,
) -> std::io::Result<Bytes> {
    let mut buf: LPooled<Vec<u8>> = LPooled::take();
    let n = n.min(MAX_READ) as usize;
    buf.reserve(n);
    reader.read_buf(&mut (&mut *buf).limit(n)).await?;
    Ok(Bytes::copy_from_slice(&buf))
}

pub(crate) type IoRead = CachedArgsAsync<IoReadEv>;

/// The most one `read` returns, whatever it asks for: a read may return
/// fewer bytes, and the request must not size the allocation.
const MAX_READ: u64 = 1 << 20;

/// The longest line a line reader holds; a longer one is an IOError, so a
/// peer that never sends a newline cannot grow it without bound.
const MAX_LINE: usize = 16 << 20;

static LBATCH: LazyLock<Pool<Vec<(BindId, Value)>>> =
    LazyLock::new(|| Pool::new(32, 65536));

#[derive(Debug)]
struct LineQueue {
    pending: Mutex<GPooled<Vec<(BindId, Value)>>>,
    drained: Notify,
}

impl LineQueue {
    fn new() -> Self {
        Self { pending: Mutex::new(LBATCH.take()), drained: Notify::new() }
    }

    async fn wait_empty(&self) {
        while !self.pending.lock().is_empty() {
            self.drained.notified().await;
        }
    }

    fn pop(&self) -> Option<(Value, bool)> {
        let (value, more) = {
            let mut pending = self.pending.lock();
            let (_, value) = pending.pop()?;
            (value, !pending.is_empty())
        };
        if !more {
            self.drained.notify_one();
        }
        Some((value, more))
    }
}

/// Queue `out` and wake the graph to take it; unless it is the `last`
/// the reader sends, wait until the graph has taken it all.
async fn send_lines(
    tx: &mut mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    id: BindId,
    mut out: GPooled<Vec<(BindId, Value)>>,
    queue: &LineQueue,
    last: bool,
) -> bool {
    if out.is_empty() {
        return true;
    }
    out.reverse();
    {
        let mut pending = queue.pending.lock();
        assert!(pending.is_empty());
        std::mem::swap(&mut *pending, &mut out);
    }
    out.push((id, Value::Null));
    if tx.send(out).await.is_err() {
        return false;
    }
    if !last {
        queue.wait_empty().await;
    }
    true
}

/// Read `stream` to its end, framing it into lines and delivering them
/// into the graph.
///
/// Framing is at the BYTE level so a multi-byte character split across a
/// read boundary survives; only complete lines are decoded, and lossily.
///
/// Batched, queue one array per read, else each line; either way wait
/// for the graph to take everything queued before reading another chunk.
fn line_reader(
    stream: Arc<Halves>,
    id: BindId,
    batched: bool,
    queue: Arc<LineQueue>,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    mut stopped: watch::Receiver<()>,
) -> impl Future<Output = ()> + Send {
    async move {
        let read = async {
            queue.wait_empty().await;
            loop {
                let mut guard = stream.reader().await;
                let Some(s) = guard.as_mut() else { break };
                s.compact();
                let scanned = s.pending.len();
                s.pending.reserve(65536);
                let read = stream
                    .until_closed(s.stream.read_buf(&mut (&mut *s.pending).limit(65536)))
                    .await;
                let mut out = LBATCH.take();
                let done = match read {
                    Err(e) => {
                        out.push((id, errf!("IOError", "read failed: {e}")));
                        true
                    }
                    Ok(n) => {
                        let held = &mut s.pending;
                        let mut lines: LPooled<Vec<Value>> = LPooled::take();
                        let mut start = 0;
                        let mut from = scanned;
                        while let Some(off) =
                            held[from..].iter().position(|b| *b == b'\n')
                        {
                            let end = from + off;
                            let line = match held[start..end].last() {
                                Some(b'\r') => &held[start..end - 1],
                                _ => &held[start..end],
                            };
                            let line = Value::String(
                                String::from_utf8_lossy(line).as_ref().into(),
                            );
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
                            out.push((
                                id,
                                errf!(
                                    "IOError",
                                    "a line is longer than {MAX_LINE} bytes"
                                ),
                            ));
                            true
                        } else {
                            if n == 0 && !held.is_empty() {
                                let line = Value::String(
                                    String::from_utf8_lossy(held).as_ref().into(),
                                );
                                if batched {
                                    lines.push(line);
                                } else {
                                    out.push((id, line));
                                }
                                held.clear();
                            }
                            if batched && !lines.is_empty() {
                                out.push((
                                    id,
                                    Value::Array(ValArray::from_iter_exact(
                                        lines.drain(..),
                                    )),
                                ));
                            }
                            n == 0
                        }
                    }
                };
                drop(guard);
                if !send_lines(&mut tx, id, out, &queue, done).await || done {
                    break;
                }
            }
        };
        tokio::select! {
            biased;
            _ = stopped.changed() => (),
            () = read => (),
        }
    }
}

#[derive(Debug)]
struct Reading {
    stream: Arc<Halves>,
    task: tokio::task::AbortHandle,
    queue: Arc<LineQueue>,
    stopped: watch::Receiver<()>,
}

impl Reading {
    fn current_for(&self, stream: &Arc<Halves>) -> bool {
        Arc::ptr_eq(&self.stream, stream) && !self.stopped.has_changed().unwrap_or(true)
    }

    fn active_for(&self, stream: &Arc<Halves>) -> bool {
        self.current_for(stream) && !self.task.is_finished()
    }
}

impl Drop for Reading {
    fn drop(&mut self) {
        self.task.abort()
    }
}

/// `Lines::lines` (BATCHED = false) and `Lines::lines_batched`
/// (BATCHED = true). Shared by every stream kind.
#[derive(Debug)]
pub(crate) struct IoLines<const BATCHED: bool> {
    var: RtVar,
    /// The stream being read and its reader.
    reading: Option<Reading>,
    out: TagValue,
}

impl<const BATCHED: bool> IoLines<BATCHED> {
    fn with(var: RtVar) -> Self {
        Self { var, reading: None, out: TagValue::phantom() }
    }
}

impl<R: Rt, const BATCHED: bool> BuiltIn<R> for IoLines<BATCHED> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = if BATCHED { "sys_io_lines_batched" } else { "sys_io_lines" };
    const RESUMES: bool = true;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let var = RtVar::new(ctx, top_id);
        Ok(Box::new(Self::with(var)))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let var = RtVar::image_decode(ctx, buf)?;
        Ok(Box::new(Self::with(var)))
    }
}

impl<R: Rt, const BATCHED: bool> Apply<R> for IoLines<BATCHED> {
    /// A started instance has a reader task holding the stream.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.reading.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        if let Some(tv) = seam_value(from[0].update(ctx))
            && tv.is_fired()
            && let Some(stream) = stream_of(&tv.value_cloned())
            && !self.reading.as_ref().is_some_and(|r| r.active_for(&stream))
        {
            let previous = self.reading.take();
            let queue = previous
                .as_ref()
                .filter(|r| r.current_for(&stream))
                .map(|r| r.queue.clone())
                .unwrap_or_else(|| Arc::new(LineQueue::new()));
            if previous.is_some() {
                self.var.remint(ctx);
            }
            drop(previous);
            let (tx, rx) = mpsc::channel(3);
            ctx.rt.watch_var(rx);
            let stopped = stream.lines_stopped();
            let reader = line_reader(
                stream.clone(),
                self.var.id,
                BATCHED,
                queue.clone(),
                tx,
                stopped.clone(),
            );
            let task = tokio::spawn(reader).abort_handle();
            self.reading = Some(Reading { stream, task, queue, stopped });
        }
        match self.reading.as_ref().and_then(|r| r.queue.pop()) {
            Some((value, more)) => {
                if more {
                    ctx.rt.set_var(self.var.id, Value::Null);
                }
                self.out.set(TagValue::fired(value))
            }
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.reading = None;
        self.var.release(ctx);
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.out = TagValue::phantom();
    }
}

#[derive(Debug, Default)]
pub(crate) struct IoReadExactEv;

impl EvalCachedAsync for IoReadExactEv {
    type Args = (Arc<Halves>, u64, Arc<ReadTicket>);

    const NAME: &str = "sys_io_read_exact";
    const CANCEL_SAFE: bool = false;

    fn reclaim((_, _, ticket): &Self::Args) -> Option<Reclaim> {
        settle(ticket)
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let ticket = stream.ticket();
        Some((stream, cached.get::<u64>(1)?, ticket))
    }

    fn eval((stream, n, ticket): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            // the buffer grows as bytes arrive, not to n
            let mut buf = Vec::new();
            stream
                .on_reader("IOError", "read_exact", async |s| {
                    ticket.enter();
                    s.take(n).read_to_end(&mut buf).await
                })
                .await
                .map_or_else(|e| e, |_| Value::Bytes(PBytes::new(Bytes::from(buf))))
        }
    }
}

pub(crate) type IoReadExact = CachedArgsAsync<IoReadExactEv>;

#[derive(Debug, Default)]
pub(crate) struct IoReadAllEv;

impl EvalCachedAsync for IoReadAllEv {
    type Args = (Arc<Halves>, Arc<ReadTicket>);

    const NAME: &str = "sys_io_read_all";
    const CANCEL_SAFE: bool = false;

    fn reclaim((_, ticket): &Self::Args) -> Option<Reclaim> {
        settle(ticket)
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let ticket = stream.ticket();
        Some((stream, ticket))
    }

    fn eval((stream, ticket): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut buf = Vec::new();
            stream
                .on_reader("IOError", "read_all", async |s| {
                    ticket.enter();
                    s.read_to_end(&mut buf).await
                })
                .await
                .map_or_else(|e| e, |_| Value::Bytes(PBytes::new(Bytes::from(buf))))
        }
    }
}

pub(crate) type IoReadAll = CachedArgsAsync<IoReadAllEv>;

#[derive(Debug, Default)]
pub(crate) struct IoWriteEv;

impl EvalCachedAsync for IoWriteEv {
    type Args = (Arc<Halves>, Bytes);

    const NAME: &str = "sys_io_write";
    const CANCEL_SAFE: bool = false;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            stream
                .on_writer("IOError", "write", async |s| s.write(&data).await)
                .await
                .map_or_else(|e| e, |n| Value::U64(n as u64))
        }
    }
}

pub(crate) type IoWrite = CachedArgsAsync<IoWriteEv>;

#[derive(Debug, Default)]
pub(crate) struct IoWriteExactEv;

impl EvalCachedAsync for IoWriteExactEv {
    type Args = (Arc<Halves>, Bytes);

    const NAME: &str = "sys_io_write_exact";
    const CANCEL_SAFE: bool = false;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((stream, data): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            stream
                .on_writer("IOError", "write_exact", async |s| s.write_all(&data).await)
                .await
                .map_or_else(|e| e, |()| Value::Null)
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
            stream
                .on_writer("IOError", "flush", async |s| s.flush().await)
                .await
                .map_or_else(|e| e, |()| Value::Null)
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

macro_rules! stdio {
    ($name:ident, $ev:ident, $builtin:literal, $kind:ident, $constructor:ident, $wrap:ident) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl<R: Rt> EvalCached<R> for $ev {
            const NAME: &str = $builtin;
            const EFFECT: Effect = Effect::Sync;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<'_, R>,
                cached: &CachedVals,
            ) -> Option<Value> {
                cached.value(0)?;
                Some($wrap(StreamKind::$kind(tokio::io::$constructor())))
            }
        }

        pub(crate) type $name = CachedArgs<$ev>;
    };
}

stdio!(IoStdin, IoStdinEv, "sys_io_stdin", Stdin, stdin, wrap_stdin);
stdio!(IoStdout, IoStdoutEv, "sys_io_stdout", Stdout, stdout, wrap_stdout);
stdio!(IoStderr, IoStderrEv, "sys_io_stderr", Stderr, stderr, wrap_stderr);

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

#[cfg(test)]
mod test {
    use super::*;
    use std::{
        pin::Pin,
        task::{Context, Poll},
    };
    use tokio::io::ReadBuf;

    struct SpareReader {
        capacity: usize,
    }

    impl AsyncRead for SpareReader {
        fn poll_read(
            self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<std::io::Result<()>> {
            assert!(buf.initialized().is_empty(), "read buffer was zero-filled");
            assert_eq!(buf.capacity(), self.capacity);
            let n = buf.remaining().min(3);
            buf.put_slice(&b"abc"[..n]);
            Poll::Ready(Ok(()))
        }
    }

    #[tokio::test]
    async fn reads_use_bounded_spare_capacity() {
        for n in [0, 1, 3, 65536, MAX_READ + 1] {
            let mut reader = SpareReader { capacity: n.min(MAX_READ) as usize };
            let bytes = read_chunk(&mut reader, n).await.unwrap();
            assert_eq!(bytes.as_ref(), &b"abc"[..n.min(3) as usize]);
        }
    }
    #[tokio::test]
    async fn line_batches_wait_until_consumed() {
        use futures::StreamExt;
        use std::time::Duration;
        use tokio::time::timeout;

        let queue = Arc::new(LineQueue::new());
        queue.drained.notify_one();
        let sender_queue = queue.clone();
        let id = BindId::new();
        let (mut tx, mut rx) = mpsc::channel(3);
        let sender = tokio::spawn(async move {
            for chunk in 0..2 {
                let mut out = LBATCH.take();
                out.extend([
                    (id, Value::I64(chunk * 2)),
                    (id, Value::I64(chunk * 2 + 1)),
                ]);
                assert!(send_lines(&mut tx, id, out, &sender_queue, false).await);
            }
        });
        for chunk in 0..2 {
            let wake = timeout(Duration::from_secs(1), rx.next()).await.unwrap().unwrap();
            assert_eq!(&**wake, &[(id, Value::Null)]);
            assert!(timeout(Duration::from_millis(20), rx.next()).await.is_err());
            assert_eq!(queue.pop(), Some((Value::I64(chunk * 2), true)));
            assert!(timeout(Duration::from_millis(20), rx.next()).await.is_err());
            assert_eq!(queue.pop(), Some((Value::I64(chunk * 2 + 1), false)));
        }
        timeout(Duration::from_secs(1), sender).await.unwrap().unwrap();
        assert!(queue.pop().is_none());
    }
    #[tokio::test]
    async fn a_restarted_reader_drains_its_retained_lines_first() {
        use arcstr::literal;
        use futures::StreamExt;
        use std::time::Duration;
        use tokio::time::timeout;

        let file = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(file.path(), b"new\n").unwrap();
        let file = tokio::fs::File::open(file.path()).await.unwrap();
        let stream = Arc::new(Halves::new(StreamKind::File(file)));
        let queue = Arc::new(LineQueue::new());
        queue.pending.lock().push((BindId::new(), Value::String(literal!("old"))));
        let (tx, mut rx) = mpsc::channel(3);
        let reader = tokio::spawn(line_reader(
            stream.clone(),
            BindId::new(),
            false,
            queue.clone(),
            tx,
            stream.lines_stopped(),
        ));
        assert!(timeout(Duration::from_millis(20), rx.next()).await.is_err());
        assert_eq!(queue.pop(), Some((Value::String(literal!("old")), false)));
        timeout(Duration::from_secs(1), rx.next()).await.unwrap().unwrap();
        assert_eq!(queue.pop(), Some((Value::String(literal!("new")), false)));
        timeout(Duration::from_secs(1), reader).await.unwrap().unwrap();
    }

    #[tokio::test]
    async fn a_stopped_reader_is_inactive_before_its_task_finishes() {
        let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
        let stream = Arc::new(Halves::new(StreamKind::File(file)));
        let task = tokio::spawn(std::future::pending::<()>());
        let reading = Reading {
            stopped: stream.lines_stopped(),
            stream: stream.clone(),
            task: task.abort_handle(),
            queue: Arc::new(LineQueue::new()),
        };
        assert!(reading.active_for(&stream));
        stream.stop_lines();
        assert!(!task.is_finished());
        assert!(!reading.active_for(&stream));
    }

    #[cfg(unix)]
    async fn close_pending_read<F: Future>(read: impl FnOnce(Arc<Halves>) -> F) {
        use futures::poll;
        use std::{pin::pin, process::Stdio};
        use tokio::process::Command;

        let mut child = Command::new("/bin/sleep")
            .arg("30")
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let stream =
            Arc::new(Halves::new(StreamKind::ChildStdout(child.stdout.take().unwrap())));
        let mut read = pin!(read(stream.clone()));
        assert!(poll!(read.as_mut()).is_pending());
        let mut close = pin!(stream.close());
        let closed = poll!(close.as_mut());
        assert!(poll!(read.as_mut()).is_ready(), "close left the read pending");
        match closed {
            Poll::Ready(result) => result.unwrap(),
            Poll::Pending => close.await.unwrap(),
        }
        assert!(stream.r.try_lock().unwrap().is_none());
        assert!(stream.w.try_lock().unwrap().is_none());
        child.kill().await.unwrap();
    }

    /// A read dropped while it holds the read half (its site deleted)
    /// does not hold up the reads after it.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_dropped_read_lets_the_next_read() {
        use futures::poll;
        use std::time::Duration;
        use std::{pin::pin, process::Stdio};
        use tokio::{process::Command, time::timeout};

        let mut child = Command::new("/bin/sh")
            .args(["-c", "sleep 0.2; printf ab; sleep 30"])
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let stream =
            Arc::new(Halves::new(StreamKind::ChildStdout(child.stdout.take().unwrap())));
        {
            let mut read = pin!(IoReadEv::eval((stream.clone(), 10, stream.ticket())));
            assert!(poll!(read.as_mut()).is_pending());
        }
        let value = timeout(
            Duration::from_secs(5),
            IoReadEv::eval((stream.clone(), 10, stream.ticket())),
        )
        .await
        .expect("the dropped read held up the next one");
        assert!(matches!(value, Value::Bytes(ref b) if &b[..] == b"ab"));
        child.kill().await.unwrap();
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_read() {
        close_pending_read(|stream| {
            let ticket = stream.ticket();
            IoReadEv::eval((stream, 10, ticket))
        })
        .await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_read_exact() {
        close_pending_read(|stream| {
            let ticket = stream.ticket();
            IoReadExactEv::eval((stream, 10, ticket))
        })
        .await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_read_all() {
        close_pending_read(|stream| {
            let ticket = stream.ticket();
            IoReadAllEv::eval((stream, ticket))
        })
        .await;
    }

    /// A write blocked on a child that never reads its stdin fails at the
    /// close, which answers.
    #[cfg(unix)]
    async fn close_pending_write<F: Future<Output = Value>>(
        write: impl FnOnce(Arc<Halves>) -> F,
    ) {
        use futures::poll;
        use std::{pin::pin, process::Stdio, time::Duration};
        use tokio::{io::AsyncWriteExt, process::Command, time::timeout};

        let mut child = Command::new("/bin/sleep")
            .arg("30")
            .stdin(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let stream =
            Arc::new(Halves::new(StreamKind::ChildStdin(child.stdin.take().unwrap())));
        let fill = [b'x'; 1 << 20];
        {
            let mut w = stream.writer().await;
            let w = w.as_mut().unwrap();
            while let Ok(written) =
                timeout(Duration::from_millis(100), w.write(&fill)).await
            {
                written.unwrap();
            }
        }
        let mut write = pin!(write(stream.clone()));
        assert!(poll!(write.as_mut()).is_pending());
        let close = stream.close();
        let (written, closed) =
            timeout(Duration::from_secs(5), futures::future::join(write, close))
                .await
                .expect("close waited on the blocked write");
        assert!(matches!(written, Value::Error(_)), "{written:?}");
        closed.unwrap();
        assert!(stream.w.try_lock().unwrap().is_none());
        child.kill().await.unwrap();
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_write() {
        let data = Bytes::from_static(b"more");
        close_pending_write(|stream| IoWriteEv::eval((stream, data))).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_write_exact() {
        let data = Bytes::from_static(b"more");
        close_pending_write(|stream| IoWriteExactEv::eval((stream, data))).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn close_cancels_lines() {
        let (tx, _rx) = mpsc::channel(3);
        let queue = Arc::new(LineQueue::new());
        close_pending_read(|stream| {
            line_reader(
                stream.clone(),
                BindId::new(),
                true,
                queue,
                tx,
                stream.lines_stopped(),
            )
        })
        .await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn stopped_lines_preserve_partial_bytes() {
        use futures::StreamExt;
        use std::{process::Stdio, time::Duration};
        use tokio::{process::Command, time::timeout};

        let mut child = Command::new("/bin/sh")
            .args(["-c", "printf 'ready\\npartial'; exec sleep 30"])
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let stream =
            Arc::new(Halves::new(StreamKind::ChildStdout(child.stdout.take().unwrap())));
        let (tx, mut rx) = mpsc::channel(3);
        let queue = Arc::new(LineQueue::new());
        let reader = line_reader(
            stream.clone(),
            BindId::new(),
            true,
            queue.clone(),
            tx,
            stream.lines_stopped(),
        );
        let reader = tokio::spawn(reader);
        timeout(Duration::from_secs(1), rx.next()).await.unwrap().unwrap();
        let (batch, more) = queue.pop().unwrap();
        assert!(!more);
        assert!(matches!(&batch, Value::Array(lines)
            if matches!(&lines[..], [Value::String(s)] if s == "ready")));
        stream.stop_lines();
        let whole = timeout(Duration::from_secs(1), stream.take())
            .await
            .expect("take stayed behind the line reader")
            .unwrap();
        drop(whole);
        let value = timeout(
            Duration::from_secs(1),
            IoReadEv::eval((stream.clone(), 2, stream.ticket())),
        )
        .await
        .expect("the line reader lost its partial bytes");
        assert!(matches!(value, Value::Bytes(ref b) if &b[..] == b"pa"));
        drop(stream.take().await.unwrap());
        let value = timeout(
            Duration::from_secs(1),
            IoReadEv::eval((stream.clone(), 5, stream.ticket())),
        )
        .await
        .unwrap();
        assert!(matches!(value, Value::Bytes(ref b) if &b[..] == b"rtial"));
        timeout(Duration::from_secs(1), reader).await.unwrap().unwrap();
        child.kill().await.unwrap();
    }
}
