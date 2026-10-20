//! The netidx module loader: graphix modules served as netidx
//! published values, threaded in as an ordinary [`ModuleResolver`].
use crate::netstate::NetHandles;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::{
    LibState,
    expr::{
        LayoutProbe, MODULE_LAYOUTS, ModPath, ModuleResolver, Origin, Probe, Probes,
        Resolution, ResolverFactory, ResolverRef, Source, choose_layout, submodule_base,
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
            let fetch = |file: &str| {
                let path = self.base.append(&format_compact!("{name}{file}"));
                async move {
                    let text = self.fetch_one(path.clone()).await;
                    (path, text)
                }
            };
            let [(imp0, intf0), (imp1, intf1)] = MODULE_LAYOUTS;
            let (i0, f0, i1, f1) =
                join!(fetch(imp0), fetch(intf0), fetch(imp1), fetch(intf1));
            if let Some(probes) = probes {
                probes.push(Probe::Netidx(i0.0.clone()))
            }
            let mut found = |(path, text): (Path, Result<ArcStr>), imp: bool| match text {
                Ok(text) => Ok((path, Some(text))),
                Err(e) if absent(&e) => {
                    if imp {
                        errors.push(e);
                    }
                    Ok((path, None))
                }
                Err(e) => Err(e),
            };
            let layouts = (|| {
                Ok::<_, anyhow::Error>([
                    LayoutProbe {
                        implementation: found(i0, true)?,
                        interface: found(f0, false)?,
                    },
                    LayoutProbe {
                        implementation: found(i1, true)?,
                        interface: found(f1, false)?,
                    },
                ])
            })();
            match layouts.and_then(|layouts| choose_layout(name, layouts)) {
                Err(e) => Resolution::Broken(e),
                Ok(None) => Resolution::TryNextMethod,
                Ok(Some(((impl_path, text), intf))) => Resolution::parsed(
                    intf.map(|(path, text)| ori(text, &path)),
                    ori(text, &impl_path),
                ),
            }
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

#[cfg(test)]
mod test {
    use super::*;

    /// A module netidx does not serve still records that netidx was
    /// asked: a later file resolver's answer is no file's alone to vouch
    /// for, since netidx may serve the module later (sys-net-01, shell-02).
    #[tokio::test(flavor = "multi_thread")]
    async fn a_miss_records_its_probe() {
        let env = netidx::InternalOnly::new().await.unwrap();
        let resolver = NetidxResolver::new(
            env.subscriber().clone(),
            Path::from("/mods"),
            Some(Duration::from_secs(1)),
        );
        let parent = Arc::new(Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(""),
        });
        let probes = Probes::default();
        let mut errors = vec![];
        let name = Path::from("foo");
        let r = resolver
            .resolve(&ModPath::root(), &parent, &name, &mut errors, Some(&probes))
            .await;
        assert!(matches!(r, Resolution::TryNextMethod));
        assert!(matches!(&probes.take()[..], [Probe::Netidx(_)]));
    }
}
