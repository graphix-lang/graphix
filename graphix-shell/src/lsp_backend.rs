//! LSP backend that owns a graphix runtime with the stdlib loaded
//! and exposes a synchronous interface for the LSP server.

use ahash::AHashMap;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, ExecCtx,
    env::Env,
    expr::{BufferOverrides, FilesResolver, ResolverRef, Source, VfsResolver},
};
use graphix_lsp::{Checked, Connection, LspBackend};
use graphix_rt::{CheckResult, GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{
    path::{Path, PathBuf},
    sync::Arc as StdArc,
};
use tokio::{
    runtime::{Handle, Runtime},
    sync::mpsc,
    task,
};
use triomphe::Arc;

/// Build a tokio runtime, stand up a graphix runtime with the
/// in-process netidx and the full stdlib loaded, and run the LSP
/// server until it shuts down.
pub fn run() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    serve(connection)?;
    io_threads.join()?;
    Ok(())
}

/// [`run`] over any connection; tests drive `Connection::memory()`.
pub fn serve(connection: Connection) -> Result<()> {
    let rt = Runtime::new().context("building tokio runtime")?;
    graphix_lsp::serve(connection, |init| {
        rt.block_on(build_backend(graphix_lsp::workspace_roots(init)))
    })
}

async fn build_backend(roots: Vec<PathBuf>) -> Result<StdArc<dyn LspBackend>> {
    let mut ctx =
        ExecCtx::new(GXRt::<NoExt>::new()).context("creating graphix context")?;
    let mut vfs = AHashMap::default();
    let mut root_mods = graphix_package::IndexSet::new();
    for pkg in crate::stdlib_packages::<NoExt>() {
        pkg.register(&mut ctx, &mut vfs, &mut root_mods)
            .context("registering stdlib modules")?;
    }
    let root = graphix_package::root_module_source(&root_mods);
    let mut resolvers: Vec<ResolverRef> = vec![VfsResolver::new(vfs)];
    // The stdlib layer, shared by every per-project check.
    let base_resolvers = resolvers.clone();
    for root in roots {
        resolvers.push(FilesResolver::new(root, None));
    }
    // lsp_mode forces fusion off in compile().
    let flags = CFlag::WarnUnhandled | CFlag::WarnUnused;
    // Drain runtime events so the channel never stalls the runtime.
    let (tx, rx) = mpsc::channel(100);
    task::spawn(drain(rx));
    let gx = GXConfig::builder(ctx, tx)
        .flags(BitFlags::from(flags))
        .root(root)
        .resolvers(resolvers)
        .lsp_mode(true)
        .build()
        .context("building runtime config")?
        .start()
        .await
        .context("loading stdlib")?;
    let buffer_overrides: BufferOverrides = Arc::new(Mutex::new(AHashMap::default()));
    Ok(StdArc::new(ShellLspBackend {
        gx,
        rt_handle: Handle::current(),
        base_resolvers,
        buffer_overrides,
    }))
}

async fn drain(mut rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>) {
    while rx.recv().await.is_some() {}
}

struct ShellLspBackend {
    gx: GXHandle<NoExt>,
    rt_handle: Handle,
    /// Stdlib + GRAPHIX_MODPATH resolvers, in scope for every project.
    base_resolvers: Vec<ResolverRef>,
    /// Open-buffer overrides, layered into every resolver chain so
    /// unsaved edits are visible to all checks.
    buffer_overrides: BufferOverrides,
}

impl ShellLspBackend {
    /// Resolver chain for checking `file`: the base resolvers plus a
    /// `BufferOverride` rooted at the file's parent dir.
    fn resolvers_for(&self, file: &Path) -> Vec<ResolverRef> {
        let mut resolvers = self.base_resolvers.clone();
        if let Some(parent) = file.parent() {
            resolvers.push(FilesResolver::new(
                parent.to_path_buf(),
                Some(self.buffer_overrides.clone()),
            ));
        }
        resolvers
    }
}

impl LspBackend for ShellLspBackend {
    fn env(&self) -> Env {
        self.rt_handle.block_on(self.gx.get_env()).unwrap_or_default()
    }

    fn buffer_overrides(&self) -> BufferOverrides {
        self.buffer_overrides.clone()
    }

    fn typecheck_project(
        &self,
        root: &Path,
        initial_scope: Option<ArcStr>,
    ) -> Result<Checked> {
        let CheckResult { env, ide } =
            self.rt_handle.block_on(self.gx.check_with_resolvers(
                Source::File(root.to_path_buf()),
                self.resolvers_for(root),
                initial_scope,
            ))?;
        Ok(Checked { env, ide })
    }
}
