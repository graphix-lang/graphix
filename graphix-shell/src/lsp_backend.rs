//! LSP backend that owns a graphix runtime with the stdlib loaded
//! and exposes a synchronous interface for the LSP server.

use crate::deps;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use graphix_compiler::{
    env::Env,
    expr::{ModuleResolver, Source},
    CFlag, ExecCtx,
};
use graphix_lsp::{LspBackend, TypecheckResult};
use graphix_rt::{CheckResult, GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use lsp_types::{InitializeParams, Uri};
use netidx::InternalOnly;
use poolshark::global::GPooled;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::{
    runtime::{Handle, Runtime},
    sync::mpsc,
    task,
};

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

async fn build_backend(roots: Vec<PathBuf>) -> Result<Arc<dyn LspBackend>> {
    let netidx = InternalOnly::new().await.context("starting internal netidx")?;
    let publisher = netidx.publisher().clone();
    let subscriber = netidx.subscriber().clone();
    let mut ctx = ExecCtx::new(GXRt::<NoExt>::new(publisher, subscriber))
        .context("creating graphix context")?;
    let mut vfs = FxHashMap::default();
    let res = deps::register::<NoExt>(&mut ctx, &mut vfs)
        .context("registering stdlib modules")?;
    let mut resolvers: Vec<ModuleResolver> = vec![ModuleResolver::VFS(vfs)];
    // Cache the stdlib (+ later, GRAPHIX_MODPATH) layer so per-project
    // checks can prepend it under their own Files resolver.
    let base_resolvers = resolvers.clone();
    for root in roots {
        resolvers.push(ModuleResolver::Files(root));
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
    let _keep_netidx = Arc::new(netidx);
    Ok(Arc::new(ShellLspBackend {
        gx,
        rt_handle: Handle::current(),
        _keep_netidx,
        base_resolvers,
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
    let s = uri.as_str();
    s.strip_prefix("file://").map(PathBuf::from)
}

struct ShellLspBackend {
    gx: GXHandle<NoExt>,
    rt_handle: Handle,
    _keep_netidx: Arc<InternalOnly>,
    /// Stdlib + GRAPHIX_MODPATH-derived resolvers (anything that
    /// should be in scope regardless of which project we're
    /// checking). The file's parent directory is appended on each
    /// check so sibling modules resolve like `graphix --check file.gx`.
    base_resolvers: Vec<ModuleResolver>,
}

impl ShellLspBackend {
    /// Build the resolver chain for checking `file`. Appends a
    /// `Files(file.parent())` resolver to `base_resolvers` so
    /// sibling-module imports work the same way they do when running
    /// the file directly. With `None`, returns only the base chain.
    fn resolvers_for(&self, file: Option<&Path>) -> Vec<ModuleResolver> {
        let mut resolvers = self.base_resolvers.clone();
        if let Some(parent) = file.and_then(|p| p.parent()) {
            resolvers.push(ModuleResolver::Files(parent.to_path_buf()));
        }
        resolvers
    }
}

impl LspBackend for ShellLspBackend {
    fn env(&self) -> Env {
        self.rt_handle.block_on(self.gx.get_env()).unwrap_or_default()
    }

    fn typecheck(&self, source: Source, text: ArcStr) -> Result<TypecheckResult> {
        let file = match &source {
            Source::File(p) => Some(p.as_path()),
            _ => None,
        };
        let CheckResult { env, references, module_references, type_references, scope_map } =
            self.rt_handle.block_on(
                self.gx
                    .check_with_resolvers(Source::Internal(text), self.resolvers_for(file)),
            )?;
        Ok(TypecheckResult {
            env,
            references,
            module_references,
            type_references,
            scope_map,
        })
    }

    fn typecheck_project(&self, root: &Path) -> Result<TypecheckResult> {
        let CheckResult { env, references, module_references, type_references, scope_map } =
            self.rt_handle.block_on(self.gx.check_with_resolvers(
                Source::File(root.to_path_buf()),
                self.resolvers_for(Some(root)),
            ))?;
        Ok(TypecheckResult {
            env,
            references,
            module_references,
            type_references,
            scope_map,
        })
    }
}
