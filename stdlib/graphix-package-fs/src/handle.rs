use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, PBytes, Value};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    io::SeekFrom,
    sync::{Arc, LazyLock},
};
use tokio::{
    fs::File,
    io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt},
    sync::Mutex,
};

use crate::metadata::convert_metadata;

// ── Abstract FileValue ──────────────────────────────────────────

#[derive(Debug, Clone)]
struct FileValue {
    file: Arc<Mutex<File>>,
}

impl PartialEq for FileValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.file) == Arc::as_ptr(&other.file)
    }
}

impl Eq for FileValue {}

impl PartialOrd for FileValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.file).cmp(&Arc::as_ptr(&other.file))
    }
}

impl Hash for FileValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.file).hash(state)
    }
}

graphix_package_core::impl_no_pack!(FileValue);

static FILE_WRAPPER: LazyLock<AbstractWrapper<FileValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd4, 0xe5, 0xf6, 0xa1, 0xb2, 0xc3, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x7b,
    ]);
    Abstract::register::<FileValue>(id).expect("failed to register FileValue")
});

fn get_file(cached: &CachedVals, idx: usize) -> Option<Arc<Mutex<File>>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let fv = a.downcast_ref::<FileValue>()?;
            Some(fv.file.clone())
        }
        _ => None,
    }
}

// ── FileOpen ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileOpenEv;

impl EvalCachedAsync for FileOpenEv {
    const NAME: &str = "fs_file_open";
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
                Ok(file) => {
                    FILE_WRAPPER.wrap(FileValue { file: Arc::new(Mutex::new(file)) })
                }
                Err(e) => errf!("IOError", "could not open {path}: {e}"),
            }
        }
    }
}

pub(crate) type FileOpen = CachedArgsAsync<FileOpenEv>;

// ── FileRead ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileReadEv;

impl EvalCachedAsync for FileReadEv {
    const NAME: &str = "fs_file_read";
    deftype!("fn(u64, string) -> Result<bytes, `IOError(string)>");
    type Args = (u64, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<u64>(0)?, get_file(cached, 1)?))
    }

    fn eval((n, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            let mut buf = vec![0u8; n as usize];
            match f.read(&mut buf).await {
                Ok(n) => {
                    buf.truncate(n);
                    Value::Bytes(PBytes::new(Bytes::from(buf)))
                }
                Err(e) => errf!("IOError", "read failed: {e}"),
            }
        }
    }
}

pub(crate) type FileRead = CachedArgsAsync<FileReadEv>;

// ── FileReadExact ───────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileReadExactEv;

impl EvalCachedAsync for FileReadExactEv {
    const NAME: &str = "fs_file_read_exact";
    deftype!("fn(u64, string) -> Result<bytes, `IOError(string)>");
    type Args = (u64, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<u64>(0)?, get_file(cached, 1)?))
    }

    fn eval((n, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            let mut buf = vec![0u8; n as usize];
            let mut pos = 0;
            while pos < buf.len() {
                match f.read(&mut buf[pos..]).await {
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

pub(crate) type FileReadExact = CachedArgsAsync<FileReadExactEv>;

// ── FileWrite ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileWriteEv;

impl EvalCachedAsync for FileWriteEv {
    const NAME: &str = "fs_file_write";
    deftype!("fn(bytes, string) -> Result<u64, `IOError(string)>");
    type Args = (Bytes, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<Bytes>(0)?, get_file(cached, 1)?))
    }

    fn eval((data, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            match f.write(&data).await {
                Ok(n) => Value::U64(n as u64),
                Err(e) => errf!("IOError", "write failed: {e}"),
            }
        }
    }
}

pub(crate) type FileWrite = CachedArgsAsync<FileWriteEv>;

// ── FileWriteExact ──────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileWriteExactEv;

impl EvalCachedAsync for FileWriteExactEv {
    const NAME: &str = "fs_file_write_exact";
    deftype!("fn(bytes, string) -> Result<null, `IOError(string)>");
    type Args = (Bytes, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<Bytes>(0)?, get_file(cached, 1)?))
    }

    fn eval((data, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            match f.write_all(&data).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "write_exact failed: {e}"),
            }
        }
    }
}

pub(crate) type FileWriteExact = CachedArgsAsync<FileWriteExactEv>;

// ── FileSeek ────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileSeekEv;

impl EvalCachedAsync for FileSeekEv {
    const NAME: &str = "fs_file_seek";
    deftype!(
        r#"fn([`Start(u64), `End(i64), `Current(i64)], string)
           -> Result<u64, `IOError(string)>"#
    );
    type Args = (SeekFrom, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        let seek_from = parse_seek_from(v)?;
        let file = get_file(cached, 1)?;
        Some((seek_from, file))
    }

    fn eval((pos, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            match f.seek(pos).await {
                Ok(n) => Value::U64(n),
                Err(e) => errf!("IOError", "seek failed: {e}"),
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

// ── FileFstat ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileFstatEv;

impl EvalCachedAsync for FileFstatEv {
    const NAME: &str = "fs_file_fstat";
    deftype!("fn(string) -> Result<Metadata, `IOError(string)>");
    type Args = Arc<Mutex<File>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_file(cached, 0)
    }

    fn eval(file: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let f = file.lock().await;
            match f.metadata().await {
                Ok(m) => convert_metadata(m),
                Err(e) => errf!("IOError", "fstat failed: {e}"),
            }
        }
    }
}

pub(crate) type FileFstat = CachedArgsAsync<FileFstatEv>;

// ── FileFlush ───────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileFlushEv;

impl EvalCachedAsync for FileFlushEv {
    const NAME: &str = "fs_file_flush";
    deftype!("fn(string) -> Result<null, `IOError(string)>");
    type Args = Arc<Mutex<File>>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_file(cached, 0)
    }

    fn eval(file: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut f = file.lock().await;
            match f.flush().await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "flush failed: {e}"),
            }
        }
    }
}

pub(crate) type FileFlush = CachedArgsAsync<FileFlushEv>;

// ── FileTruncate ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct FileTruncateEv;

impl EvalCachedAsync for FileTruncateEv {
    const NAME: &str = "fs_file_truncate";
    deftype!("fn(u64, string) -> Result<null, `IOError(string)>");
    type Args = (u64, Arc<Mutex<File>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<u64>(0)?, get_file(cached, 1)?))
    }

    fn eval((len, file): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let f = file.lock().await;
            match f.set_len(len).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "truncate failed: {e}"),
            }
        }
    }
}

pub(crate) type FileTruncate = CachedArgsAsync<FileTruncateEv>;
