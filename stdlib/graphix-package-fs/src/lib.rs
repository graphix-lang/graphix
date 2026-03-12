#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::errf;
use graphix_package_core::{
    deftype, CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, Value};
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    cmp::Ordering,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::LazyLock,
};
use tempfile::TempDir;

pub(crate) mod dir;
pub(crate) mod file;
pub(crate) mod metadata;
pub(crate) mod watch;

#[derive(Debug)]
struct TempDirValue {
    path: ArcStr,
    _dir: TempDir,
}

impl PartialEq for TempDirValue {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Eq for TempDirValue {}

impl PartialOrd for TempDirValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TempDirValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.path.cmp(&other.path)
    }
}

impl Hash for TempDirValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state)
    }
}

impl netidx_core::pack::Pack for TempDirValue {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(
        &self,
        _buf: &mut impl bytes::BufMut,
    ) -> Result<(), netidx_core::pack::PackError> {
        Err(netidx_core::pack::PackError::Application(0))
    }

    fn decode(
        _buf: &mut impl bytes::Buf,
    ) -> Result<Self, netidx_core::pack::PackError> {
        Err(netidx_core::pack::PackError::Application(0))
    }
}

static TEMPDIR_WRAPPER: LazyLock<AbstractWrapper<TempDirValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xa1, 0xb2, 0xc3, 0xd4, 0xe5, 0xf6, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x78,
    ]);
    Abstract::register::<TempDirValue>(id).expect("failed to register TempDirValue")
});

#[derive(Debug)]
enum Name {
    Prefix(ArcStr),
    Suffix(ArcStr),
}

#[derive(Debug)]
pub(crate) struct TempDirArgs {
    dir: Option<ArcStr>,
    name: Option<Name>,
}

#[derive(Debug, Default)]
pub(crate) struct GxTempDirEv;

impl EvalCachedAsync for GxTempDirEv {
    const NAME: &str = "fs_tempdir";
    deftype!(
        r#"fn(?#in:[null, string],
              ?#name:[null, `Prefix(string), `Suffix(string)],
              Any)
           -> Result<string, `IOError(string)>"#
    );
    type Args = TempDirArgs;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        if cached.0.iter().any(|v| v.is_none()) {
            None
        } else {
            let dir = cached.get::<Option<ArcStr>>(0).flatten();
            let name = cached
                .get::<Option<(ArcStr, ArcStr)>>(1)
                .and_then(|v| v)
                .and_then(|(tag, v)| match &*tag {
                    "Prefix" => Some(Name::Prefix(v)),
                    "Suffix" => Some(Name::Suffix(v)),
                    _ => None,
                });
            let _ = cached.get::<Value>(2)?;
            Some(TempDirArgs { dir, name })
        }
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let td = tokio::task::spawn_blocking(|| match (args.dir, args.name) {
                (None, None) => TempDir::new(),
                (None, Some(Name::Prefix(pfx))) => TempDir::with_prefix(&*pfx),
                (None, Some(Name::Suffix(sfx))) => TempDir::with_suffix(&*sfx),
                (Some(dir), None) => TempDir::new_in(&*dir),
                (Some(dir), Some(Name::Prefix(pfx))) => {
                    TempDir::with_prefix_in(&*pfx, &*dir)
                }
                (Some(dir), Some(Name::Suffix(sfx))) => {
                    TempDir::with_suffix_in(&*sfx, &*dir)
                }
            })
            .await;
            match td {
                Err(e) => errf!("IOError", "failed to spawn create temp dir {e:?}"),
                Ok(Err(e)) => errf!("IOError", "failed to create temp dir {e:?}"),
                Ok(Ok(td)) => {
                    use std::fmt::Write;
                    let mut buf = CompactString::new("");
                    write!(buf, "{}", td.path().display()).unwrap();
                    let path = ArcStr::from(buf.as_str());
                    TEMPDIR_WRAPPER.wrap(TempDirValue { path, _dir: td })
                }
            }
        }
    }
}

pub(crate) type GxTempDir = CachedArgsAsync<GxTempDirEv>;

#[derive(Debug, Default)]
pub(crate) struct TempDirPathEv;

impl EvalCached for TempDirPathEv {
    const NAME: &str = "fs_tempdir_path";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let v = from.0.first()?.as_ref()?;
        match v {
            Value::Abstract(a) => {
                let td = a.downcast_ref::<TempDirValue>()?;
                Some(Value::String(td.path.clone()))
            }
            _ => None,
        }
    }
}

pub(crate) type TempDirPath = CachedArgs<TempDirPathEv>;

pub(crate) fn convert_path(path: &Path) -> ArcStr {
    thread_local! {
        static BUF: RefCell<String> = RefCell::new(String::new());
    }
    BUF.with_borrow_mut(|buf| {
        buf.clear();
        use std::fmt::Write;
        write!(buf, "{}", path.display()).unwrap();
        ArcStr::from(buf.as_str())
    })
}

#[derive(Debug, Default)]
pub(crate) struct JoinPathEv;

impl EvalCached for JoinPathEv {
    const NAME: &str = "fs_join_path";
    deftype!("fn(string, @args: [string, Array<string>]) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut parts: LPooled<Vec<ArcStr>> = LPooled::take();
        for part in from.0.iter() {
            match part {
                None => return None,
                Some(Value::String(s)) => parts.push(s.clone()),
                Some(Value::Array(a)) => {
                    for part in a.iter() {
                        match part {
                            Value::String(s) => parts.push(s.clone()),
                            _ => return None,
                        }
                    }
                }
                _ => return None,
            }
        }
        thread_local! {
            static BUF: RefCell<PathBuf> = RefCell::new(PathBuf::new());
        }
        BUF.with_borrow_mut(|path| {
            path.clear();
            for part in parts.drain(..) {
                path.push(&*part)
            }
            Some(Value::String(convert_path(&path)))
        })
    }
}

pub(crate) type JoinPath = CachedArgs<JoinPathEv>;

#[cfg(test)]
mod test;

graphix_derive::defpackage! {
    builtins => [
        GxTempDir,
        TempDirPath,
        JoinPath,
        metadata::IsFile,
        metadata::IsDir,
        metadata::Metadata,
        watch::SetGlobals,
        watch::WatchBuiltIn,
        watch::WatchFullBuiltIn,
        file::ReadAll,
        file::ReadAllBin,
        file::WriteAll,
        file::WriteAllBin,
        file::RemoveFile,
        dir::ReadDir,
        dir::CreateDir,
        dir::RemoveDir,
    ],
}
