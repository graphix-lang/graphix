//! The netidx module loader: graphix modules served as netidx
//! published values, threaded in as an ordinary [`ModuleResolver`].
use crate::netstate::NetHandles;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::{
    LibState,
    expr::{
        ModPath, ModuleResolver, Origin, Resolution, ResolverFactory, ResolverRef, Source,
    },
};
use graphix_package_core::NetConfig;
use netidx::{
    path::Path,
    subscriber::{Event, Subscriber},
};
use netidx_value::Value;
use std::{future::Future, pin::Pin, time::Duration};
use tokio::join;
use triomphe::Arc;

#[derive(Debug, Clone)]
enum SubSource {
    Ready(Subscriber),
    Lazy { handles: NetHandles, cfg: NetConfig },
}

#[derive(Debug, Clone)]
pub struct NetidxResolver {
    source: SubSource,
    base: Path,
    timeout: Option<Duration>,
}

impl NetidxResolver {
    pub fn new(
        subscriber: Subscriber,
        base: Path,
        timeout: Option<Duration>,
    ) -> ResolverRef {
        std::sync::Arc::new(NetidxResolver {
            source: SubSource::Ready(subscriber),
            base,
            timeout,
        })
    }

    /// A GRAPHIX_MODPATH factory for `netidx:<base>` entries. The netidx
    /// handles come from the context's libstate at first use — the same
    /// universe sys::net's builtins use.
    pub fn factory(timeout: Option<Duration>) -> ResolverFactory {
        std::sync::Arc::new(move |libstate: &mut LibState, rest: &str| {
            let handles = libstate.get_or_default::<NetHandles>().clone();
            let cfg = libstate.get::<NetConfig>().unwrap_or(NetConfig::Internal);
            Ok(std::sync::Arc::new(NetidxResolver {
                source: SubSource::Lazy { handles, cfg },
                base: Path::from_str(rest),
                timeout,
            }))
        })
    }

    fn subscriber(&self) -> Result<Subscriber> {
        match &self.source {
            SubSource::Ready(s) => Ok(s.clone()),
            SubSource::Lazy { handles, cfg } => handles.subscriber(cfg.clone()),
        }
    }

    async fn fetch_one(&self, path: Path) -> Result<ArcStr> {
        let v = self
            .subscriber()?
            .subscribe_nondurable_one(path.clone(), self.timeout)
            .await?;
        match v.last() {
            Event::Update(Value::String(text)) => Ok(text),
            Event::Unsubscribed | Event::Update(_) => {
                Err(anyhow!("{path}: expected a string"))
            }
        }
    }
}

impl ModuleResolver for NetidxResolver {
    fn resolve<'a>(
        &'a self,
        _scope: &'a ModPath,
        parent: &'a Arc<Origin>,
        name: &'a Path,
        errors: &'a mut Vec<anyhow::Error>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>> {
        Box::pin(async move {
            let ori = |text: ArcStr, p: &Path| Origin {
                parent: Some(parent.clone()),
                source: Source::Netidx(p.clone()),
                text,
            };
            // CR claude for eric: [bug] The netidx loader lays modules out differently
            // from files. It has three gaps: - This line tries only `{base}/{name}.gx`,
            // never `{base}/{name}/mod.gx`. - `for_source` below puts a module's
            // submodules under the module's own path (`/s/m.gx` looks for
            // `/s/m.gx/n.gx`), where a file module looks in `<dir>/m/`. -
            // graphix-shell/src/main.rs:412 looks for a `netidx:` script's modules
            // under the script's own path, where a file script uses its parent
            // directory. The book's netidx hierarchy
            // (book/src/modules/implementation.md:153, `m/mod.gx` + `m/n.gx`) therefore
            // fails with "module m could not be found". The layout `m.gx` + `m/n.gx`
            // fails on `n`. An `m.gx` beside a `netidx:` script is not found without
            // GRAPHIX_MODPATH. The same trees on disk all load. The layout rule (which
            // files a name maps to, and where an implementation's submodules live) is
            // written four times: resolve_from_vfs, resolve_from_files, the File branch
            // of resolve_modules_int, and here. One shared helper would keep the four
            // in step. Separately, design/netidx_extraction.md:28 still names
            // graphix-compiler/src/expr/resolver.rs, which is now
            // graphix-types/src/expr/resolver.rs. probe:
            // design/review-2026-10-05/repro/x-dup-02.sh (x-dup-02)
            let impl_path = self.base.append(&format_compact!("{name}.gx"));
            let intf_path = self.base.append(&format_compact!("{name}.gxi"));
            let (impl_sub, intf_sub) = join!(
                self.fetch_one(impl_path.clone()),
                self.fetch_one(intf_path.clone())
            );
            let implementation = match impl_sub {
                Ok(text) => ori(text, &impl_path),
                Err(e) => {
                    errors.push(e);
                    return Resolution::TryNextMethod;
                }
            };
            // CR claude for eric: [bug] Every failure to fetch the .gxi becomes "no
            // interface": a non-string value, a publisher that exited but is still
            // listed, a --resolve-timeout expiry, a denial. The module then compiles
            // without its interface, so items it hides are visible and its signatures
            // and abstract types are not enforced. The implementation's Err arm above
            // does the same with TryNextMethod, so a later resolver's module of the
            // same name stands in. FilesResolver returns Resolution::Broken in both
            // cases; here only an error whose SubscribeErrors holds NotFound should
            // mean absent. probe: design/review-2026-10-05/repro/sys-net-15.sh (bar.gxi
            // published as 42, or by a publisher that has exited: `bar::hidden` gives
            // 2, not "not defined"; bar.gx published as 42 loads a file fallback
            // instead). (sys-net-15)
            let interface = intf_sub.ok().map(|text| ori(text, &intf_path));
            Resolution::parsed(interface, implementation)
        })
    }

    fn for_source(&self, source: &Source) -> Option<ResolverRef> {
        match source {
            Source::Netidx(p) => Some(std::sync::Arc::new(NetidxResolver {
                source: self.source.clone(),
                base: p.clone(),
                timeout: self.timeout,
            })),
            _ => None,
        }
    }

    fn fetch_source<'a>(
        &'a self,
        source: &'a Source,
    ) -> Option<Pin<Box<dyn Future<Output = Result<ArcStr>> + Send + Sync + 'a>>> {
        match source {
            Source::Netidx(p) => {
                let p = p.clone();
                Some(Box::pin(async move { self.fetch_one(p).await }))
            }
            _ => None,
        }
    }
}
