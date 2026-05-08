//! LSP backend that owns a graphix runtime with the stdlib loaded
//! and exposes a synchronous interface for the LSP server.

use crate::deps;
use ahash::AHashMap;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{
    env::Env,
    expr::{BufferOverrides, ModuleResolver, Source},
    CFlag, ExecCtx,
};
use graphix_lsp::{LspBackend, TypecheckResult};
use graphix_rt::{CheckResult, GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use lsp_types::{InitializeParams, Uri};
use netidx::InternalOnly;
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
    let rt = Runtime::new().context("building tokio runtime")?;
    let result = graphix_lsp::serve(|init| {
        let roots = project_roots(init);
        rt.block_on(build_backend(roots))
    });
    drop(rt);
    result
}

async fn build_backend(roots: Vec<PathBuf>) -> Result<StdArc<dyn LspBackend>> {
    let netidx = InternalOnly::new().await.context("starting internal netidx")?;
    let publisher = netidx.publisher().clone();
    let subscriber = netidx.subscriber().clone();
    let mut ctx = ExecCtx::new(GXRt::<NoExt>::new(publisher, subscriber))
        .context("creating graphix context")?;
    let mut vfs = AHashMap::default();
    let res = deps::register::<NoExt>(&mut ctx, &mut vfs)
        .context("registering stdlib modules")?;
    let mut resolvers: Vec<ModuleResolver> = vec![ModuleResolver::VFS(vfs)];
    // Cache the stdlib (+ later, GRAPHIX_MODPATH) layer so per-project
    // checks can prepend it under their own BufferOverride resolver.
    let base_resolvers = resolvers.clone();
    for root in roots {
        resolvers.push(ModuleResolver::Files { base: root, overrides: None });
    }
    let flags = CFlag::WarnUnhandled | CFlag::WarnUnused;
    // We don't consume runtime events in the LSP — drain them on a task
    // so the channel doesn't fill and stall the runtime.
    let (tx, rx) = mpsc::channel(100);
    task::spawn(drain(rx));
    let gx = GXConfig::builder(ctx, tx)
        .flags(BitFlags::from(flags))
        .root(res.root)
        .resolvers(resolvers)
        .lsp_mode(true)
        .build()
        .context("building runtime config")?
        .start()
        .await
        .context("loading stdlib")?;
    let _keep_netidx = StdArc::new(netidx);
    let buffer_overrides: BufferOverrides = Arc::new(Mutex::new(AHashMap::default()));
    Ok(StdArc::new(ShellLspBackend {
        gx,
        rt_handle: Handle::current(),
        _keep_netidx,
        base_resolvers,
        buffer_overrides,
    }))
}

async fn drain(mut rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>) {
    while rx.recv().await.is_some() {}
}

/// Collect filesystem roots the editor told us about. Prefers
/// `workspaceFolders` (multi-root capable) and falls back to the
/// deprecated `rootUri` / `rootPath`.
fn project_roots(init: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(folders) = &init.workspace_folders {
        for folder in folders {
            if let Some(p) = file_uri_to_path(&folder.uri) {
                roots.push(p);
            }
        }
    }
    if roots.is_empty() {
        #[allow(deprecated)]
        if let Some(uri) = &init.root_uri {
            if let Some(p) = file_uri_to_path(uri) {
                roots.push(p);
            }
        }
    }
    if roots.is_empty() {
        #[allow(deprecated)]
        if let Some(p) = init.root_path.as_ref() {
            roots.push(PathBuf::from(p));
        }
    }
    roots
}

fn file_uri_to_path(uri: &Uri) -> Option<PathBuf> {
    graphix_lsp::uri::uri_to_path(uri)
}

struct ShellLspBackend {
    gx: GXHandle<NoExt>,
    rt_handle: Handle,
    _keep_netidx: StdArc<InternalOnly>,
    /// Stdlib + GRAPHIX_MODPATH-derived resolvers. Anything that should
    /// be in scope regardless of which project we're checking. The
    /// per-call `BufferOverride` (rooted at the file's parent dir) is
    /// appended in `typecheck_project`.
    base_resolvers: Vec<ModuleResolver>,
    /// Shared open-buffer override map. Mutated by the LSP on every
    /// `did_open` / `did_change` / `did_close`. Layered into every
    /// resolver chain, so unsaved edits in any open buffer are visible
    /// to all check calls.
    buffer_overrides: BufferOverrides,
}

impl ShellLspBackend {
    /// Build the resolver chain for checking a project rooted at
    /// `file`. Appends a `BufferOverride` whose base is the file's
    /// parent dir — the override map shadows on-disk text per path,
    /// and falls through to the disk for anything not in the map.
    /// Sibling-module imports work the same way they do when running
    /// the file directly, plus the editor-buffer view is honored.
    fn resolvers_for(&self, file: &Path) -> Vec<ModuleResolver> {
        let mut resolvers = self.base_resolvers.clone();
        if let Some(parent) = file.parent() {
            resolvers.push(ModuleResolver::Files {
                base: parent.to_path_buf(),
                overrides: Some(self.buffer_overrides.clone()),
            });
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
    ) -> Result<TypecheckResult> {
        let CheckResult { env, references, module_references, scope_map, lsp } =
            self.rt_handle.block_on(self.gx.check_with_resolvers(
                Source::File(root.to_path_buf()),
                self.resolvers_for(root),
                initial_scope,
            ))?;
        Ok(TypecheckResult { env, references, module_references, scope_map, lsp })
    }
}
