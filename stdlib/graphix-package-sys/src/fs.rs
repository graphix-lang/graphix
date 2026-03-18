use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::{
    io::SeekFrom,
    sync::Arc,
};
use tokio::{
    io::AsyncSeekExt,
    sync::Mutex,
};

use crate::{StreamKind, get_stream, metadata::convert_metadata, wrap_file};

// ── FileOpen ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileOpenEv;

impl EvalCachedAsync for FileOpenEv {
    const NAME: &str = "sys_fs_open";
    deftype!(
        r#"fn([`Read, `Write, `Append, `ReadWrite, `Create, `CreateNew], string)
           -> Result<string, `IOError(string)>"#
    );
    type Args = (ArcStr, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<ArcStr>(1)?))
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
                Ok(file) => wrap_file(file),
                Err(e) => errf!("IOError", "could not open {path}: {e}"),
            }
        }
    }
}

pub(crate) type FileOpen = CachedArgsAsync<FileOpenEv>;

// ── FileSeek ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileSeekEv;

impl EvalCachedAsync for FileSeekEv {
    const NAME: &str = "sys_fs_seek";
    deftype!(
        r#"fn(string, [`Start(u64), `End(i64), `Current(i64)])
           -> Result<u64, `IOError(string)>"#
    );
    type Args = (Arc<Mutex<Option<StreamKind>>>, SeekFrom);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let seek_from = parse_seek_from(v)?;
        Some((stream, seek_from))
    }

    fn eval((stream, pos): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            match guard.as_mut() {
                Some(StreamKind::File(f)) => match f.seek(pos).await {
                    Ok(n) => Value::U64(n),
                    Err(e) => errf!("IOError", "seek failed: {e}"),
                },
                Some(_) => errf!("IOError", "seek is only supported on file streams"),
                None => errf!("IOError", "stream unavailable"),
            }
        }
    }
}

fn parse_seek_from(v: &Value) -> Option<SeekFrom> {
    let (tag, payload): (ArcStr, Value) = v.clone().cast_to().ok()?;
    match &*tag {
        "Start" => Some(SeekFrom::Start(payload.cast_to::<u64>().ok()?)),
        "End" => Some(SeekFrom::End(payload.cast_to::<i64>().ok()?)),
        "Current" => Some(SeekFrom::Current(payload.cast_to::<i64>().ok()?)),
        _ => None,
    }
}

pub(crate) type FileSeek = CachedArgsAsync<FileSeekEv>;

// ── FileFstat ──────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileFstatEv;

impl EvalCachedAsync for FileFstatEv {
    const NAME: &str = "sys_fs_fstat";
    deftype!("fn(string) -> Result<Metadata, `IOError(string)>");
    type Args = Arc<Mutex<Option<StreamKind>>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_stream(cached, 0)
    }

    fn eval(stream: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let guard = stream.lock().await;
            match guard.as_ref() {
                Some(StreamKind::File(f)) => match f.metadata().await {
                    Ok(m) => convert_metadata(m),
                    Err(e) => errf!("IOError", "fstat failed: {e}"),
                },
                Some(_) => errf!("IOError", "fstat is only supported on file streams"),
                None => errf!("IOError", "stream unavailable"),
            }
        }
    }
}

pub(crate) type FileFstat = CachedArgsAsync<FileFstatEv>;

// ── FileTruncate ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileTruncateEv;

impl EvalCachedAsync for FileTruncateEv {
    const NAME: &str = "sys_fs_truncate";
    deftype!("fn(string, u64) -> Result<null, `IOError(string)>");
    type Args = (Arc<Mutex<Option<StreamKind>>>, u64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_stream(cached, 0)?, cached.get::<u64>(1)?))
    }

    fn eval((stream, len): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let guard = stream.lock().await;
            match guard.as_ref() {
                Some(StreamKind::File(f)) => match f.set_len(len).await {
                    Ok(()) => Value::Null,
                    Err(e) => errf!("IOError", "truncate failed: {e}"),
                },
                Some(_) => {
                    errf!("IOError", "truncate is only supported on file streams")
                }
                None => errf!("IOError", "stream unavailable"),
            }
        }
    }
}

pub(crate) type FileTruncate = CachedArgsAsync<FileTruncateEv>;

// ── ReadAll ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ReadAllOp;

impl EvalCachedAsync for ReadAllOp {
    const NAME: &str = "sys_fs_read_all";
    deftype!("fn(string) -> Result<string, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read_to_string(&*path).await {
                Ok(s) => Value::from(s),
                Err(e) => errf!("IOError", "could not read {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type ReadAll = CachedArgsAsync<ReadAllOp>;

#[derive(Debug, Default)]
pub(crate) struct ReadAllBinOp;

impl EvalCachedAsync for ReadAllBinOp {
    const NAME: &str = "sys_fs_read_all_bin";
    deftype!("fn(string) -> Result<bytes, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read(&*path).await {
                Ok(s) => Value::from(Bytes::from(s)),
                Err(e) => errf!("IOError", "could not read {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type ReadAllBin = CachedArgsAsync<ReadAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllOp;

impl EvalCachedAsync for WriteAllOp {
    const NAME: &str = "sys_fs_write_all";
    deftype!("fn(#path:string, string) -> Result<null, `IOError(string)>");
    type Args = (ArcStr, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type WriteAll = CachedArgsAsync<WriteAllOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllBinOp;

impl EvalCachedAsync for WriteAllBinOp {
    const NAME: &str = "sys_fs_write_all_bin";
    deftype!("fn(#path:string, bytes) -> Result<null, `IOError(string)>");
    type Args = (ArcStr, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type WriteAllBin = CachedArgsAsync<WriteAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct RemoveFileOp;

impl EvalCachedAsync for RemoveFileOp {
    const NAME: &str = "sys_fs_remove_file";
    deftype!("fn(string) -> Result<null, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::remove_file(&*path).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not remove file {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type RemoveFile = CachedArgsAsync<RemoveFileOp>;
