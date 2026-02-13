use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::errf;
use graphix_package_core::{
    deftype, CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    fmt,
    path::{Path, PathBuf},
    sync::Arc,
};
use tempfile::TempDir;

pub(crate) mod dir;
pub(crate) mod file;
pub(crate) mod metadata;
pub(crate) mod watch;

#[derive(Debug)]
enum Name {
    Prefix(ArcStr),
    Suffix(ArcStr),
}

pub(crate) struct TempDirArgs {
    dir: Option<ArcStr>,
    name: Option<Name>,
    t: Arc<Mutex<Option<TempDir>>>,
}

impl fmt::Debug for TempDirArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ dir: {:?}, name: {:?} }}", self.dir, self.name)
    }
}

#[derive(Debug, Default)]
pub(crate) struct GxTempDirEv {
    current: Arc<Mutex<Option<TempDir>>>,
}

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
            Some(TempDirArgs { dir, name, t: self.current.clone() })
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
                    *args.t.lock() = Some(td);
                    Value::String(ArcStr::from(buf.as_str()))
                }
            }
        }
    }
}

pub(crate) type GxTempDir = CachedArgsAsync<GxTempDirEv>;

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
