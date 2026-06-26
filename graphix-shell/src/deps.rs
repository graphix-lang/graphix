use ahash::AHashMap;
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::VfsEntry, ExecCtx};
use graphix_package::{CustomDisplay, IndexSet, MainThreadHandle, Package};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use netidx_core::path::Path;
use tokio::sync::oneshot;

pub struct RegisterResult {
    pub root: ArcStr,
}

/// Register the stdlib packages (each guarded by its Cargo feature, so a build
/// that drops a feature drops the corresponding registration), then call
/// `register_packages` to register any additional (external / embedder)
/// packages. The closure is the `ShellBuilder::register_packages` hook — the
/// package manager generates one for installed external packages, and embedders
/// supply their own.
pub fn register<X: GXExt>(
    ctx: &mut ExecCtx<GXRt<X>, X::UserEvent>,
    modules: &mut AHashMap<Path, VfsEntry>,
    register_packages: impl FnOnce(
        &mut ExecCtx<GXRt<X>, X::UserEvent>,
        &mut AHashMap<Path, VfsEntry>,
        &mut IndexSet<ArcStr>,
    ) -> Result<()>,
) -> Result<RegisterResult> {
    let mut root_mods = IndexSet::new();
    graphix_package_core::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "array")]
    graphix_package_array::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "list")]
    graphix_package_list::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "str")]
    graphix_package_str::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "map")]
    graphix_package_map::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "sys")]
    graphix_package_sys::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "args")]
    graphix_package_args::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "http")]
    graphix_package_http::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "json")]
    graphix_package_json::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "toml")]
    graphix_package_toml::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "pack")]
    graphix_package_pack::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "xls")]
    graphix_package_xls::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "sqlite")]
    graphix_package_sqlite::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "db")]
    graphix_package_db::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "hbs")]
    graphix_package_hbs::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "re")]
    graphix_package_re::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "rand")]
    graphix_package_rand::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "bench")]
    graphix_package_bench::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "tui")]
    graphix_package_tui::P::register(ctx, modules, &mut root_mods)?;
    #[cfg(feature = "gui")]
    graphix_package_gui::P::register(ctx, modules, &mut root_mods)?;
    register_packages(ctx, modules, &mut root_mods)?;
    let mut parts = Vec::new();
    for name in &root_mods {
        if name == "core" {
            parts.push(format!("mod core;\nuse core"));
        } else {
            parts.push(format!("mod {name}"));
        }
    }
    Ok(RegisterResult { root: ArcStr::from(parts.join(";\n")) })
}

pub struct Cdc<X: GXExt> {
    pub stop: oneshot::Receiver<()>,
    pub custom: Box<dyn CustomDisplay<X>>,
}

pub enum CustomResult<X: GXExt> {
    Custom(Cdc<X>),
    NotCustom(CompExp<X>),
}

pub async fn maybe_init_custom<X: GXExt>(
    gx: &GXHandle<X>,
    env: &Env,
    e: CompExp<X>,
    run_on_main: &MainThreadHandle,
) -> Result<CustomResult<X>> {
    macro_rules! try_pkg {
        ($pkg:path) => {
            if <$pkg>::is_custom(gx, env, &e) {
                let (tx, rx) = oneshot::channel();
                return <$pkg>::init_custom(gx, env, tx, e, run_on_main.clone())
                    .await
                    .map(|custom| CustomResult::Custom(Cdc { stop: rx, custom }));
            }
        };
    }
    try_pkg!(graphix_package_core::P);
    #[cfg(feature = "array")]
    try_pkg!(graphix_package_array::P);
    #[cfg(feature = "list")]
    try_pkg!(graphix_package_list::P);
    #[cfg(feature = "str")]
    try_pkg!(graphix_package_str::P);
    #[cfg(feature = "map")]
    try_pkg!(graphix_package_map::P);
    #[cfg(feature = "sys")]
    try_pkg!(graphix_package_sys::P);
    #[cfg(feature = "args")]
    try_pkg!(graphix_package_args::P);
    #[cfg(feature = "http")]
    try_pkg!(graphix_package_http::P);
    #[cfg(feature = "json")]
    try_pkg!(graphix_package_json::P);
    #[cfg(feature = "toml")]
    try_pkg!(graphix_package_toml::P);
    #[cfg(feature = "pack")]
    try_pkg!(graphix_package_pack::P);
    #[cfg(feature = "xls")]
    try_pkg!(graphix_package_xls::P);
    #[cfg(feature = "sqlite")]
    try_pkg!(graphix_package_sqlite::P);
    #[cfg(feature = "db")]
    try_pkg!(graphix_package_db::P);
    #[cfg(feature = "hbs")]
    try_pkg!(graphix_package_hbs::P);
    #[cfg(feature = "re")]
    try_pkg!(graphix_package_re::P);
    #[cfg(feature = "rand")]
    try_pkg!(graphix_package_rand::P);
    #[cfg(feature = "bench")]
    try_pkg!(graphix_package_bench::P);
    #[cfg(feature = "tui")]
    try_pkg!(graphix_package_tui::P);
    #[cfg(feature = "gui")]
    try_pkg!(graphix_package_gui::P);
    Ok(CustomResult::NotCustom(e))
}
