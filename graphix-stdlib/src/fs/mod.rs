use crate::{
    deftype, CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use anyhow::Result;
use arcstr::{literal, ArcStr};
use compact_str::CompactString;
use graphix_compiler::{errf, ExecCtx, Rt, UserEvent};
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

mod dir;
mod file;
mod metadata;
mod watch;

#[derive(Debug)]
enum Name {
    Prefix(ArcStr),
    Suffix(ArcStr),
}

struct TempDirArgs {
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
struct GxTempDirEv {
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

type GxTempDir = CachedArgsAsync<GxTempDirEv>;

fn convert_path(path: &Path) -> ArcStr {
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
struct JoinPathEv;

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

type JoinPath = CachedArgs<JoinPathEv>;

pub(super) fn register<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<(ArcStr, ArcStr)> {
    ctx.register_builtin::<GxTempDir>()?;
    ctx.register_builtin::<JoinPath>()?;
    ctx.register_builtin::<metadata::IsFile>()?;
    ctx.register_builtin::<metadata::IsDir>()?;
    ctx.register_builtin::<metadata::Metadata>()?;
    ctx.register_builtin::<watch::SetGlobals>()?;
    ctx.register_builtin::<watch::WatchBuiltIn>()?;
    ctx.register_builtin::<watch::WatchFullBuiltIn>()?;
    ctx.register_builtin::<file::ReadAll>()?;
    ctx.register_builtin::<file::ReadAllBin>()?;
    ctx.register_builtin::<file::WriteAll>()?;
    ctx.register_builtin::<file::WriteAllBin>()?;
    ctx.register_builtin::<file::RemoveFile>()?;
    ctx.register_builtin::<dir::ReadDir>()?;
    ctx.register_builtin::<dir::CreateDir>()?;
    ctx.register_builtin::<dir::RemoveDir>()?;
    Ok((literal!(include_str!("fs.gx")), literal!(include_str!("fs.gxi"))))
}
