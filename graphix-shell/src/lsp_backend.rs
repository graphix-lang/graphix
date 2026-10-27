//! LSP backend that owns a graphix runtime with the stdlib loaded
//! and exposes a synchronous interface for the LSP server.

use ahash::AHashMap;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag,
    env::Env,
    expr::{BufferOverrides, FilesResolver, Load, ResolverRef},
};
use graphix_lsp::{Checked, Connection, LspBackend};
use graphix_rt::{CheckResult, GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{path::Path, sync::Arc as StdArc};
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
    let (connection, io_threads) = graphix_lsp::stdio::stdio()?;
    serve(connection)?;
    io_threads.join()?;
    Ok(())
}

/// [`run`] over any connection; tests drive `Connection::memory()`.
pub fn serve(connection: Connection) -> Result<()> {
    let rt = Runtime::new().context("building tokio runtime")?;
    graphix_lsp::serve(connection, |_| rt.block_on(build_backend()))
}

async fn build_backend() -> Result<StdArc<dyn LspBackend>> {
    let mut ctx = GXRt::<NoExt>::new_state().context("creating graphix context")?;
    let packages = crate::stdlib_packages::<NoExt>();
    let (vfs, root) =
        graphix_package::register_packages(&mut ctx, packages.iter().map(|p| &**p))
            .context("registering stdlib modules")?;
    // The stdlib layer, shared by every per-project check.
    let base_resolvers: Vec<ResolverRef> = vec![vfs];
    // XCR claude for claude: [bug/low] These search-path resolvers (GRAPHIX_MODPATH
    // entries and the data dir) are FilesResolvers with no buffer overrides, and
    // `resolvers_for` appends them as they are. A module a project reaches through them
    // is checked from disk, ignoring the open buffer. Its own submodules do see
    // buffers, because `resolve_modules_level` takes the overrides of any resolver in
    // the chain. Build these entries with `Some(buffer_overrides)` (an overrides
    // argument to `search_path`/`parse_modpath`). Found by reading; not run.
    // (t-format-resolver-05)
    // 2026-10-27 claude: search_path and parse_modpath take the overrides their
    // FilesResolvers read through; the LSP backend passes its buffer overrides, the
    // runtime None. Pin: graphix-shell/tests/lsp_modpath.rs
    // a_modpath_module_is_read_from_its_buffer (fails with None passed: m::b not
    // defined).
    let buffer_overrides: BufferOverrides = Arc::new(Mutex::new(AHashMap::default()));
    let search = graphix_rt::search_path(
        &Default::default(),
        &mut ctx.libstate,
        Some(&buffer_overrides),
    )?;
    // A check runs CFlag::CheckOnly, which never fuses.
    let flags = BitFlags::from(CFlag::WarnUnhandled);
    // Drain runtime events so the channel never stalls the runtime.
    let (tx, rx) = mpsc::channel(100);
    task::spawn(drain(rx));
    let gx = GXConfig::builder(ctx, tx)
        .flags(BitFlags::from(flags))
        .root(root)
        .resolvers(base_resolvers.clone())
        .lsp_mode(true)
        .build()
        .context("building runtime config")?
        .start()
        .await
        .context("loading stdlib")?;
    Ok(StdArc::new(ShellLspBackend {
        gx,
        rt_handle: Handle::current(),
        base_resolvers,
        search,
        buffer_overrides,
    }))
}

async fn drain(mut rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>) {
    while rx.recv().await.is_some() {}
}

struct ShellLspBackend {
    gx: GXHandle<NoExt>,
    rt_handle: Handle,
    /// The stdlib, in scope for every project, ahead of a file's
    /// directory.
    base_resolvers: Vec<ResolverRef>,
    /// GRAPHIX_MODPATH and the data directory, behind a file's
    /// directory, as the CLI searches.
    search: Vec<ResolverRef>,
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
        resolvers.extend(self.search.iter().cloned());
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
                Load::File(root.to_path_buf()),
                self.resolvers_for(root),
                initial_scope,
            ))?;
        Ok(Checked { env, ide })
    }
}
