use std::fs::FileType;

use crate::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use arcstr::{literal, ArcStr};
use chrono::{DateTime, Utc};
use graphix_compiler::errf;
use netidx_value::Value;

#[derive(Debug, Default)]
pub(super) struct IsFileEv;

impl EvalCachedAsync for IsFileEv {
    const NAME: &str = "fs_is_file";
    deftype!("fn(string) -> Result<string, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::metadata(&*path).await {
                Err(e) => errf!("IOError", "can't stat {e:?}"),
                Ok(m) if m.is_file() => Value::String(path),
                Ok(_) => errf!("IOError", "not a file"),
            }
        }
    }
}

pub(super) type IsFile = CachedArgsAsync<IsFileEv>;

#[derive(Debug, Default)]
pub(super) struct IsDirEv;

impl EvalCachedAsync for IsDirEv {
    const NAME: &str = "fs_is_dir";
    deftype!("fn(string) -> Result<string, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::metadata(&*path).await {
                Err(e) => errf!("IOError", "can't stat {e:?}"),
                Ok(m) if m.is_dir() => Value::String(path),
                Ok(_) => errf!("IOError", "not a directory"),
            }
        }
    }
}

pub(super) type IsDir = CachedArgsAsync<IsDirEv>;

pub(super) fn convert_filetype(typ: FileType) -> Value {
    #[cfg(unix)]
    {
        use std::os::unix::fs::FileTypeExt;
        if typ.is_dir() {
            Value::String(literal!("Dir"))
        } else if typ.is_file() {
            Value::String(literal!("File"))
        } else if typ.is_symlink() {
            Value::String(literal!("Symlink"))
        } else if typ.is_block_device() {
            Value::String(literal!("BlockDev"))
        } else if typ.is_char_device() {
            Value::String(literal!("CharDev"))
        } else if typ.is_fifo() {
            Value::String(literal!("Fifo"))
        } else if typ.is_socket() {
            Value::String(literal!("Socket"))
        } else {
            Value::Null
        }
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::FileTypeExt;
        if typ.is_dir() {
            Value::String(literal!("Dir"))
        } else if typ.is_file() {
            Value::String(literal!("File"))
        } else if typ.is_symlink_file() {
            Value::String(literal!("Symlink"))
        } else if typ.is_symlink_dir() {
            Value::String(literal!("SymlinkDir"))
        } else {
            Value::Null
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct MetadataEv;

impl EvalCachedAsync for MetadataEv {
    const NAME: &str = "fs_metadata";
    deftype!(
        "fn(?#follow_symlinks:bool, string) -> Result<Metadata, `IOError(string)>"
    );
    type Args = (bool, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<bool>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((follow_symlinks, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let md = if follow_symlinks {
                tokio::fs::metadata(&*path).await
            } else {
                tokio::fs::symlink_metadata(&*path).await
            };
            match md {
                Err(e) => errf!("IOError", "could not stat {e:?}"),
                Ok(m) => {
                    let accessed: Option<DateTime<Utc>> =
                        m.accessed().ok().map(|ts| ts.into());
                    let created: Option<DateTime<Utc>> =
                        m.created().ok().map(|ts| ts.into());
                    let modified: Option<DateTime<Utc>> =
                        m.modified().ok().map(|ts| ts.into());
                    let kind = convert_filetype(m.file_type());
                    let len = m.len();
                    let permissions = {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::MetadataExt;
                            Value::U32(m.mode())
                        }
                        #[cfg(windows)]
                        {
                            Value::from((
                                literal!("ReadOnly"),
                                m.permissions().readonly(),
                            ))
                        }
                    };
                    let r: [(ArcStr, Value); 6] = [
                        (literal!("accessed"), accessed.into()),
                        (literal!("created"), created.into()),
                        (literal!("kind"), kind),
                        (literal!("len"), len.into()),
                        (literal!("modified"), modified.into()),
                        (literal!("permissions"), permissions),
                    ];
                    r.into()
                }
            }
        }
    }
}

pub(super) type Metadata = CachedArgsAsync<MetadataEv>;
