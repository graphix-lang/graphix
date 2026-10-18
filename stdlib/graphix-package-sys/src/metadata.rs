use std::fs::FileType;

use arcstr::{ArcStr, literal};
use chrono::{DateTime, Utc};
use graphix_compiler::{errf, rep};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_derive::IntoValue;
use netidx_value::Value;

#[derive(Debug, Default)]
pub(crate) struct IsFileEv;

impl EvalCachedAsync for IsFileEv {
    type Args = ArcStr;

    const NAME: &str = "sys_fs_is_file";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::metadata(&*path).await {
                Err(e) => errf!("IOError", "can't stat {path}: {e}"),
                Ok(m) if m.is_file() => Value::String(path),
                Ok(_) => errf!("IOError", "{path}: not a file"),
            }
        }
    }
}

pub(crate) type IsFile = CachedArgsAsync<IsFileEv>;

#[derive(Debug, Default)]
pub(crate) struct IsDirEv;

impl EvalCachedAsync for IsDirEv {
    type Args = ArcStr;

    const NAME: &str = "sys_fs_is_dir";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::metadata(&*path).await {
                Err(e) => errf!("IOError", "can't stat {path}: {e}"),
                Ok(m) if m.is_dir() => Value::String(path),
                Ok(_) => errf!("IOError", "{path}: not a directory"),
            }
        }
    }
}

pub(crate) type IsDir = CachedArgsAsync<IsDirEv>;

pub(crate) fn convert_filetype(typ: FileType) -> Value {
    #[cfg(unix)]
    let tag = {
        use std::os::unix::fs::FileTypeExt;
        if typ.is_dir() {
            literal!("Dir")
        } else if typ.is_file() {
            literal!("File")
        } else if typ.is_symlink() {
            literal!("Symlink")
        } else if typ.is_block_device() {
            literal!("BlockDev")
        } else if typ.is_char_device() {
            literal!("CharDev")
        } else if typ.is_fifo() {
            literal!("Fifo")
        } else if typ.is_socket() {
            literal!("Socket")
        } else {
            return Value::Null;
        }
    };
    #[cfg(windows)]
    let tag = {
        use std::os::windows::fs::FileTypeExt;
        if typ.is_dir() {
            literal!("Dir")
        } else if typ.is_file() {
            literal!("File")
        } else if typ.is_symlink_file() {
            literal!("Symlink")
        } else if typ.is_symlink_dir() {
            literal!("SymlinkDir")
        } else {
            return Value::Null;
        }
    };
    rep::make_variant(tag, [])
}

pub(crate) fn convert_metadata(m: std::fs::Metadata) -> Value {
    let accessed: Option<DateTime<Utc>> = m.accessed().ok().map(|ts| ts.into());
    let created: Option<DateTime<Utc>> = m.created().ok().map(|ts| ts.into());
    let modified: Option<DateTime<Utc>> = m.modified().ok().map(|ts| ts.into());
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
            rep::make_variant(
                literal!("ReadOnly"),
                [Value::Bool(m.permissions().readonly())],
            )
        }
    };
    #[derive(IntoValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
    struct Fields {
        accessed: Option<DateTime<Utc>>,
        created: Option<DateTime<Utc>>,
        kind: Value,
        len: u64,
        modified: Option<DateTime<Utc>>,
        permissions: Value,
    }
    Fields { accessed, created, kind, len, modified, permissions }.into()
}

#[derive(Debug, Default)]
pub(crate) struct MetadataEv;

impl EvalCachedAsync for MetadataEv {
    type Args = (bool, ArcStr);

    const NAME: &str = "sys_fs_metadata";

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
                Err(e) => errf!("IOError", "could not stat {path}: {e}"),
                Ok(m) => convert_metadata(m),
            }
        }
    }
}

pub(crate) type Metadata = CachedArgsAsync<MetadataEv>;

graphix_package_core::unit_image_state!(IsFileEv, IsDirEv, MetadataEv);

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use crate::{convert_path, dir, fs};
    use bytes::Bytes;

    pub(crate) fn assert_path_error(value: &Value, path: &str) {
        let Value::Error(error) = value else {
            panic!("expected IOError, got {value:?}")
        };
        let Value::Array(fields) = &**error else { panic!("expected tagged error") };
        assert_eq!(fields[0], Value::String(literal!("IOError")));
        let Value::String(message) = &fields[1] else { panic!("expected error text") };
        assert!(message.contains(path), "path missing: {message}");
        assert!(!message.contains("Os {"), "debug error: {message}");
        assert!(!message.contains("Caused by:"), "debug context: {message}");
    }

    #[tokio::test]
    async fn filesystem_errors_include_paths_and_display_text() {
        let dir = tempfile::tempdir().unwrap();
        let path = convert_path(&dir.path().join("missing/leaf"));
        let errors = [
            IsFileEv::eval(path.clone()).await,
            IsDirEv::eval(path.clone()).await,
            MetadataEv::eval((true, path.clone())).await,
            MetadataEv::eval((false, path.clone())).await,
            fs::ReadAllOp::eval(path.clone()).await,
            fs::ReadAllBinOp::eval(path.clone()).await,
            fs::WriteAllOp::eval((path.clone(), literal!("data"))).await,
            fs::WriteAllBinOp::eval((path.clone(), Bytes::from_static(b"data"))).await,
            fs::RemoveFileOp::eval(path.clone()).await,
            dir::CreateDirOp::eval((false, path.clone())).await,
            dir::RemoveDirOp::eval((false, path.clone())).await,
        ];
        for error in errors {
            assert_path_error(&error, &path);
        }
        let dir_path = convert_path(dir.path());
        assert_path_error(&IsFileEv::eval(dir_path.clone()).await, &dir_path);
        let file_path = dir.path().join("file");
        std::fs::write(&file_path, b"").unwrap();
        let file_path = convert_path(&file_path);
        assert_path_error(&IsDirEv::eval(file_path.clone()).await, &file_path);
    }
}
