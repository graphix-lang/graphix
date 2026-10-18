use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{errf, rep};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::{io::SeekFrom, sync::Arc};
use tokio::io::AsyncSeekExt;

use crate::{Halves, StreamKind, get_stream, metadata::convert_metadata, wrap_file};

#[derive(Debug, Default)]
pub(crate) struct FileOpenEv;

impl EvalCachedAsync for FileOpenEv {
    type Args = (ArcStr, ArcStr);

    const NAME: &str = "sys_fs_open";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let (mode, []) = rep::variant(cached.value(0)?)? else { return None };
        Some((mode.clone(), cached.get::<ArcStr>(1)?))
    }

    fn eval((mode, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut opts = tokio::fs::OpenOptions::new();
            match &*mode {
                "Read" => {
                    opts.read(true);
                }
                "Write" => {
                    opts.write(true).create(true).truncate(true);
                }
                "Append" => {
                    opts.append(true).create(true);
                }
                "ReadWrite" => {
                    opts.read(true).write(true);
                }
                "Create" => {
                    opts.read(true).write(true).create(true).truncate(true);
                }
                "CreateNew" => {
                    opts.read(true).write(true).create_new(true);
                }
                other => return errf!("IOError", "unknown mode: {other}"),
            };
            match opts.open(&*path).await {
                Ok(file) => wrap_file(StreamKind::File(file)),
                Err(e) => errf!("IOError", "could not open {path}: {e}"),
            }
        }
    }
}

pub(crate) type FileOpen = CachedArgsAsync<FileOpenEv>;

#[derive(Debug, Default)]
pub(crate) struct FileSeekEv;

impl EvalCachedAsync for FileSeekEv {
    type Args = (Arc<Halves>, SeekFrom);

    const NAME: &str = "sys_fs_seek";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let v = cached.value(1)?;
        let seek_from = parse_seek_from(v)?;
        Some((stream, seek_from))
    }

    fn eval((stream, pos): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let Some(mut kind) = stream.take().await else {
                return errf!("IOError", "stream unavailable");
            };
            let pos = match pos {
                SeekFrom::Current(n) => match n.checked_sub(kind.buffered_len() as i64) {
                    Some(n) => SeekFrom::Current(n),
                    None => return errf!("IOError", "seek offset is out of range"),
                },
                pos => pos,
            };
            match &mut *kind {
                StreamKind::File(f) => match f.seek(pos).await {
                    Ok(n) => Value::U64(n),
                    Err(e) => errf!("IOError", "seek failed: {e}"),
                },
                _ => errf!("IOError", "seek is only supported on file streams"),
            }
        }
    }
}

fn parse_seek_from(v: &Value) -> Option<SeekFrom> {
    let (tag, [payload]) = rep::variant(v)? else { return None };
    let payload = payload.clone();
    match tag.as_str() {
        "Start" => Some(SeekFrom::Start(payload.cast_to::<u64>().ok()?)),
        "End" => Some(SeekFrom::End(payload.cast_to::<i64>().ok()?)),
        "Current" => Some(SeekFrom::Current(payload.cast_to::<i64>().ok()?)),
        _ => None,
    }
}

pub(crate) type FileSeek = CachedArgsAsync<FileSeekEv>;

#[derive(Debug, Default)]
pub(crate) struct FileFstatEv;

impl EvalCachedAsync for FileFstatEv {
    type Args = Arc<Halves>;

    const NAME: &str = "sys_fs_fstat";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let Some(kind) = stream.take().await else {
                return errf!("IOError", "stream unavailable");
            };
            match &*kind {
                StreamKind::File(f) => match f.metadata().await {
                    Ok(m) => convert_metadata(m),
                    Err(e) => errf!("IOError", "fstat failed: {e}"),
                },
                _ => errf!("IOError", "fstat is only supported on file streams"),
            }
        }
    }
}

pub(crate) type FileFstat = CachedArgsAsync<FileFstatEv>;

#[derive(Debug, Default)]
pub(crate) struct FileTruncateEv;

impl EvalCachedAsync for FileTruncateEv {
    type Args = (Arc<Halves>, u64);

    const NAME: &str = "sys_fs_truncate";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, len): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let Some(kind) = stream.take().await else {
                return errf!("IOError", "stream unavailable");
            };
            match &*kind {
                StreamKind::File(f) => match f.set_len(len).await {
                    Ok(()) => Value::Null,
                    Err(e) => errf!("IOError", "truncate failed: {e}"),
                },
                _ => errf!("IOError", "truncate is only supported on file streams"),
            }
        }
    }
}

pub(crate) type FileTruncate = CachedArgsAsync<FileTruncateEv>;

#[derive(Debug, Default)]
pub(crate) struct ReadAllOp;

impl EvalCachedAsync for ReadAllOp {
    type Args = ArcStr;

    const NAME: &str = "sys_fs_read_all";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read_to_string(&*path).await {
                Ok(s) => Value::from(s),
                Err(e) => errf!("IOError", "could not read {path}: {e}"),
            }
        }
    }
}

pub(crate) type ReadAll = CachedArgsAsync<ReadAllOp>;

#[derive(Debug, Default)]
pub(crate) struct ReadAllBinOp;

impl EvalCachedAsync for ReadAllBinOp {
    type Args = ArcStr;

    const NAME: &str = "sys_fs_read_all_bin";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read(&*path).await {
                Ok(s) => Value::from(Bytes::from(s)),
                Err(e) => errf!("IOError", "could not read {path}: {e}"),
            }
        }
    }
}

pub(crate) type ReadAllBin = CachedArgsAsync<ReadAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllOp;

impl EvalCachedAsync for WriteAllOp {
    type Args = (ArcStr, ArcStr);

    const NAME: &str = "sys_fs_write_all";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}: {e}"),
            }
        }
    }
}

pub(crate) type WriteAll = CachedArgsAsync<WriteAllOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllBinOp;

impl EvalCachedAsync for WriteAllBinOp {
    type Args = (ArcStr, Bytes);

    const NAME: &str = "sys_fs_write_all_bin";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}: {e}"),
            }
        }
    }
}

pub(crate) type WriteAllBin = CachedArgsAsync<WriteAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct RemoveFileOp;

impl EvalCachedAsync for RemoveFileOp {
    type Args = ArcStr;

    const NAME: &str = "sys_fs_remove_file";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::remove_file(&*path).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not remove file {path}: {e}"),
            }
        }
    }
}

pub(crate) type RemoveFile = CachedArgsAsync<RemoveFileOp>;

graphix_package_core::unit_image_state!(
    FileOpenEv,
    FileSeekEv,
    FileFstatEv,
    FileTruncateEv,
    ReadAllOp,
    ReadAllBinOp,
    WriteAllOp,
    WriteAllBinOp,
    RemoveFileOp
);

#[cfg(test)]
mod test {
    use super::*;
    use futures::poll;
    use std::{
        io::{Seek, Write},
        pin::pin,
    };
    use tokio::{io::AsyncReadExt, runtime::Builder, sync::oneshot};

    fn abort_file_op<F: Future<Output = Value>>(op: impl FnOnce(Arc<Halves>) -> F) {
        Builder::new_current_thread()
            .enable_all()
            .max_blocking_threads(1)
            .build()
            .unwrap()
            .block_on(async {
                let mut file = tempfile::tempfile().unwrap();
                file.write_all(b"hello").unwrap();
                file.rewind().unwrap();
                let stream = Arc::new(Halves::new(StreamKind::File(
                    tokio::fs::File::from_std(file),
                )));
                let (release, blocked) = std::sync::mpsc::channel();
                let (ready, started) = oneshot::channel();
                let worker = tokio::task::spawn_blocking(move || {
                    ready.send(()).unwrap();
                    let _ = blocked.recv();
                });
                started.await.unwrap();
                {
                    let mut eval = pin!(op(stream.clone()));
                    assert!(poll!(eval.as_mut()).is_pending());
                    assert!(
                        stream.r.try_lock().is_err(),
                        "read half exposed during operation"
                    );
                    assert!(
                        stream.w.try_lock().is_err(),
                        "write half exposed during operation"
                    );
                }
                release.send(()).unwrap();
                worker.await.unwrap();
                let mut guard = stream.reader().await;
                let reader = guard.as_mut().expect("aborted operation lost the file");
                let mut data = [0; 5];
                reader.read_exact(&mut data).await.unwrap();
                assert_eq!(&data, b"hello");
            });
    }

    #[test]
    fn aborted_fstat_preserves_file() {
        abort_file_op(FileFstatEv::eval);
    }

    #[test]
    fn aborted_seek_preserves_file() {
        abort_file_op(|stream| FileSeekEv::eval((stream, SeekFrom::Start(0))));
    }

    #[test]
    fn aborted_truncate_preserves_file() {
        abort_file_op(|stream| FileTruncateEv::eval((stream, 5)));
    }

    #[tokio::test]
    async fn relative_seek_accounts_for_buffered_bytes() {
        use std::io::Write;
        let mut file = tempfile::tempfile().unwrap();
        file.write_all(b"hello").unwrap();
        let stream =
            Arc::new(Halves::new(StreamKind::File(tokio::fs::File::from_std(file))));
        stream.reader().await.as_mut().unwrap().pending.extend_from_slice(b"hello");
        assert_eq!(
            FileSeekEv::eval((stream.clone(), SeekFrom::Current(0))).await,
            Value::U64(0)
        );
        assert!(matches!(crate::io::IoReadEv::eval((stream, 5)).await,
            Value::Bytes(ref bytes) if &bytes[..] == b"hello"));
    }
}
