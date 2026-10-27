use std::{
    fs::FileType,
    time::{SystemTime, UNIX_EPOCH},
};

use arcstr::{ArcStr, literal};
use chrono::{DateTime, TimeDelta, Utc};
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

fn utc(t: SystemTime) -> Option<DateTime<Utc>> {
    let (since, after) = match t.duration_since(UNIX_EPOCH) {
        Ok(d) => (d, true),
        Err(e) => (e.duration(), false),
    };
    let since = TimeDelta::from_std(since).ok()?;
    if after {
        DateTime::UNIX_EPOCH.checked_add_signed(since)
    } else {
        DateTime::UNIX_EPOCH.checked_sub_signed(since)
    }
}

pub(crate) fn convert_metadata(m: std::fs::Metadata) -> Value {
    // XCR claude for claude: [bug/medium] `ts.into()` is chrono's From<SystemTime>,
    // which unwraps `timestamp_opt` and panics for a time outside chrono's range. btrfs
    // and tmpfs let any user set such an mtime (utimensat with 1e14 s).
    // sys::fs::metadata and fstat then return `sys_fs_metadata panicked` instead of the
    // metadata, and the panic message goes to stderr (under the TUI the panic hook
    // tears the display down). Convert with `duration_since(UNIX_EPOCH)` plus
    // `DateTime::from_timestamp(secs, nanos)` and give null for a time it cannot hold.
    // probe: design/review-2026-10-26/repro/x-panics-metadata-far-timestamp.sh
    // (x-panics-01)
    // 2026-10-26 claude: metadata::utc converts through TimeDelta::from_std and checked
    // add/sub from the epoch, so a time chrono cannot hold is null and a time before
    // the epoch keeps its sign. Pin: graphix-package-sys
    // metadata::test::a_time_chrono_cannot_hold_is_null (sets a far mtime, panics with
    // the old ts.into()).
    let accessed = m.accessed().ok().and_then(utc);
    let created = m.created().ok().and_then(utc);
    let modified = m.modified().ok().and_then(utc);
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
        let Some((tag, [Value::String(message)])) = rep::variant(error) else {
            panic!("expected tagged error text")
        };
        assert_eq!(tag, "IOError");
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

    #[tokio::test]
    async fn a_time_chrono_cannot_hold_is_null() {
        use std::time::Duration;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("far");
        let file = std::fs::File::create(&path).unwrap();
        let far = UNIX_EPOCH + Duration::from_secs(99_999_999_999_999);
        file.set_times(std::fs::FileTimes::new().set_accessed(far).set_modified(far))
            .unwrap();
        drop(file);
        let md = MetadataEv::eval((true, convert_path(&path))).await;
        let fields = rep::struct_fields(&md).unwrap_or_else(|| panic!("{md:?}"));
        for name in ["accessed", "modified"] {
            assert_eq!(rep::field_named(fields, name).unwrap().1, &Value::Null);
        }
        let before = UNIX_EPOCH - Duration::from_millis(1500);
        assert_eq!(utc(before).unwrap().timestamp_millis(), -1500);
        assert_eq!(utc(SystemTime::UNIX_EPOCH).unwrap().timestamp(), 0);
    }
}
