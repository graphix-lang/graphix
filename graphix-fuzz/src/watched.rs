//! The watched run of [`crate::Pair::Watched`]: the program with a
//! debugger session attached that watches every binding its author wrote
//! (`design/debugger.md`). A debugger that changes what a program
//! computes is a bug, so the run's trace is held to the plain run's.

use anyhow::{Result, bail};
use compact_str::format_compact;
use graphix_rt::{
    GXHandle, NoExt,
    debug::{
        Client, Debugger,
        proto::{Command, Outcome, Reply},
    },
};
use poolshark::local::LPooled;
use std::collections::HashSet;
use tokio::task::JoinHandle;

/// A session watching a program, ended when this is dropped.
pub(crate) struct Watcher {
    served: JoinHandle<Result<()>>,
    heard: JoinHandle<Result<()>>,
    /// The session found a binding of the program's to watch.
    watching: bool,
}

impl Drop for Watcher {
    fn drop(&mut self) {
        self.served.abort();
        self.heard.abort()
    }
}

impl Watcher {
    /// What went wrong in the session while the program ran, where
    /// something did: the service or its client ended by an error or a
    /// panic, or the program `ran` and the session had found nothing of
    /// it to watch (a subject that compiles binds its result, fused or
    /// not). A session still open, or one the program's end closed, is
    /// well.
    pub(crate) async fn failed(mut self, ran: bool) -> Option<String> {
        if ran && !self.watching {
            return Some("the session listed none of the program's bindings".into());
        }
        for (who, task) in [("service", &mut self.served), ("client", &mut self.heard)] {
            if task.is_finished() {
                match task.await {
                    Ok(Ok(())) => (),
                    Ok(Err(e)) => return Some(format!("the session's {who}: {e:#}")),
                    Err(e) => return Some(format!("the session's {who} died: {e}")),
                }
            }
        }
        None
    }
}

/// Attach a session to `rt`, whose program is held before its first
/// cycle, watch every binding the program wrote, and let it run: each
/// cycle then looks at every watch, tells the session what fired, and
/// keeps its log, and the session prints each event for its client.
pub(crate) async fn watch_all(rt: &GXHandle<NoExt>) -> Result<Watcher> {
    let (ours, theirs) = tokio::io::duplex(1 << 16);
    let debugger = Debugger::new(rt.clone());
    let served = tokio::spawn(async move { debugger.serve(theirs).await });
    let mut client = Client::connect(ours).await?;
    let Reply::Bindings { bindings, .. } =
        client.request(Command::List { scope: None }).await?
    else {
        bail!("the debugger answered a list with something else")
    };
    let mut names: LPooled<HashSet<&str>> = LPooled::take();
    for b in bindings.iter() {
        // a name covers every instance of it, where a watch takes that
        // many: else the one listed
        let named = match names.insert(b.name.as_str()) {
            true => client.ask(Command::Watch { target: b.name.clone() }).await?,
            false => continue,
        };
        if let Outcome::Err(_) = named {
            let target = format_compact!("#{}", b.id);
            client.request(Command::Watch { target }).await?;
        }
    }
    client.request(Command::Continue).await?;
    let heard = tokio::spawn(async move {
        while client.event().await?.is_some() {}
        Ok(())
    });
    Ok(Watcher { served, heard, watching: !bindings.is_empty() })
}
