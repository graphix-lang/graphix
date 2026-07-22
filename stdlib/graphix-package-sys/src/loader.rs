//! The netidx module loader — graphix modules served as netidx
//! published values. Extracted from the compiler's resolver (the core
//! has no networking since the 2026-07 netidx extraction,
//! design/netidx_extraction.md); Atlas and the shell's `netidx:`
//! script prefix thread it in as an ordinary [`ModuleResolver`].
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::expr::{
    ModPath, ModuleResolver, Origin, Resolution, ResolverFactory, ResolverRef, Source,
};
use netidx::{
    path::Path,
    subscriber::{Event, Subscriber},
};
use netidx_value::Value;
use std::{future::Future, pin::Pin, time::Duration};
use tokio::join;
use triomphe::Arc;

#[derive(Debug, Clone)]
pub struct NetidxResolver {
    pub subscriber: Subscriber,
    pub base: Path,
    pub timeout: Option<Duration>,
}

impl NetidxResolver {
    pub fn new(
        subscriber: Subscriber,
        base: Path,
        timeout: Option<Duration>,
    ) -> ResolverRef {
        std::sync::Arc::new(NetidxResolver { subscriber, base, timeout })
    }

    /// A GRAPHIX_MODPATH factory for `netidx:<base>` entries.
    pub fn factory(subscriber: Subscriber, timeout: Option<Duration>) -> ResolverFactory {
        std::sync::Arc::new(move |rest: &str| {
            Ok(NetidxResolver::new(subscriber.clone(), Path::from_str(rest), timeout))
        })
    }

    async fn fetch_one(&self, path: Path) -> Result<ArcStr> {
        let v =
            self.subscriber.subscribe_nondurable_one(path.clone(), self.timeout).await?;
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
            Source::Netidx(p) => Some(NetidxResolver::new(
                self.subscriber.clone(),
                p.clone(),
                self.timeout,
            )),
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
