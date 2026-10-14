//! The netidx module loader: graphix modules served as netidx
//! published values, threaded in as an ordinary [`ModuleResolver`].
use crate::netstate::NetHandles;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::{
    LibState,
    expr::{
        MODULE_LAYOUTS, ModPath, ModuleResolver, Origin, Probe, Probes, Resolution,
        ResolverFactory, ResolverRef, Source, submodule_base,
    },
};
use graphix_package_core::NetConfig;
use netidx::{
    path::Path,
    subscriber::{Event, SubscribeError, SubscribeErrors, Subscriber},
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
        probes: Option<&'a Probes>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>> {
        Box::pin(async move {
            let ori = |text: ArcStr, p: &Path| Origin {
                parent: Some(parent.clone()),
                source: Source::Netidx(p.clone()),
                text,
            };
            // a path nobody publishes is absent; any other failure is the
            // module's, never a reason to look elsewhere or to drop its
            // interface
            let absent = |e: &anyhow::Error| {
                e.downcast_ref::<SubscribeErrors>().is_some_and(|s| {
                    s.contains(SubscribeError::NotFound)
                        || s.contains(SubscribeError::NoSuchValue)
                })
            };
            // CR claude for claude: [bug/low] This loop takes the first layout whose
            // implementation answers and never looks at the other. Two things follow.
            // `m.gx` beside `m/mod.gx` loads here but is refused from disk and the vfs.
            // A stray interface beside the unused layout (`m.gx` with `m/mod.gxi`) is
            // silently dropped, so every item is public, which `stray_interface`
            // prevents in the other two resolvers. `resolve_from_files` and
            // `resolve_from_vfs` also each carry their own copy of the same decision.
            // Fix: every resolver fetches both layouts' implementation and interface,
            // and one shared function in `expr::resolver` (keyed by `MODULE_LAYOUTS`)
            // decides Resolved, Broken or TryNextMethod. (x-security-14)
            // 2026-10-13 codex: Needs the shared four-probe layout decision in
            // graphix-types/src/expr/resolver.rs and callers in all three resolvers.
            // That file is outside this scope. The netidx probe (stray and both
            // modes) is unverified because this sandbox refuses sockets; the early
            // return here still bypasses the second layout.
            // CR claude for claude: [bug/medium] This loop takes the first layout whose
            // implementation exists and pairs it only with the interface beside it. It
            // never looks at the other layout. resolve_from_vfs and resolve_from_files
            // refuse two cases this accepts. First, `m/mod.gx` with a stray `m.gxi`:
            // here the interface is silently dropped, so every private item of m is
            // public and its abstract types are transparent. Second, `m.gx` and
            // `m/mod.gx` both published: here `m.gx` silently wins. The same tree on
            // disk is refused with stray_interface or "both define module". Fix: put
            // the five-arm layout decision in resolver.rs as one function over the four
            // probed results. Have all three resolvers call it, with netidx fetching
            // all four paths and treating an absent path as None. This also removes the
            // copied match from resolve_from_vfs and resolve_from_files. Probe:
            // design/review-2026-10-12/repro/t-format-resolver-01.sh (stray, both).
            // (t-format-resolver-01)
            // 2026-10-13 codex: Needs the shared four-probe layout decision in
            // graphix-types/src/expr/resolver.rs and callers in all three resolvers.
            // That file is outside this scope. The netidx probe (stray and both
            // modes) is unverified because this sandbox refuses sockets; the early
            // return here still bypasses the second layout.
            for (imp, intf) in MODULE_LAYOUTS {
                let impl_path = self.base.append(&format_compact!("{name}{imp}"));
                let intf_path = self.base.append(&format_compact!("{name}{intf}"));
                let (impl_sub, intf_sub) = join!(
                    self.fetch_one(impl_path.clone()),
                    self.fetch_one(intf_path.clone())
                );
                let implementation = match impl_sub {
                    Ok(text) => {
                        // no file vouches for a module read from netidx
                        if let Some(probes) = probes {
                            probes.push(Probe::Netidx(impl_path.clone()))
                        }
                        ori(text, &impl_path)
                    }
                    Err(e) if absent(&e) => {
                        errors.push(e);
                        continue;
                    }
                    Err(e) => return Resolution::Broken(e),
                };
                let interface = match intf_sub {
                    Ok(text) => Some(ori(text, &intf_path)),
                    Err(e) if absent(&e) => None,
                    Err(e) => return Resolution::Broken(e),
                };
                return Resolution::parsed(interface, implementation);
            }
            Resolution::TryNextMethod
        })
    }

    fn for_source(&self, source: &Source) -> Option<ResolverRef> {
        match source {
            Source::Netidx(p) => Some(std::sync::Arc::new(NetidxResolver {
                source: self.source.clone(),
                base: Path::from(ArcStr::from(submodule_base(p))),
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
