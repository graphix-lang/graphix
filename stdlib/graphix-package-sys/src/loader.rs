//! The netidx module loader — graphix modules served as netidx
//! published values. Extracted from the compiler's resolver (the core
//! has no networking since the 2026-07 netidx extraction,
//! design/netidx_extraction.md); Atlas and the shell's `netidx:`
//! script prefix thread it in as an ordinary [`ModuleResolver`].
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

    /// A GRAPHIX_MODPATH factory for `netidx:<base>` entries. The
    /// netidx handles come from the context's libstate at first use:
    /// the same universe sys::net's builtins use, materialized from
    /// the seeded [`NetConfig`] (Internal when unseeded).
    pub fn factory(timeout: Option<Duration>) -> ResolverFactory {
        std::sync::Arc::new(move |libstate: &mut LibState, rest: &str| {
            let handles = libstate.get_or_default::<NetHandles>().clone();
            let cfg = libstate.get::<NetConfig>().cloned().unwrap_or(NetConfig::Internal);
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
