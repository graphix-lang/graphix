use anyhow::{bail, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use fxhash::FxHashMap;
use graphix_compiler::{env::Env, expr::ExprId, ExecCtx};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use handlebars::Handlebars;
use netidx_value::Value;
use serde_json::json;
use std::{any::Any, path::Path};
use tokio::{fs, sync::oneshot};

/// Trait implemented by custom Graphix displays, e.g. TUIs, GUIs, etc.
#[async_trait]
pub trait CustomDisplay<X: GXExt>: Any {
    /// Clear the custom display, freeing any used resources.
    ///
    /// This is called when the shell user has indicated that they
    /// want to return to the normal display mode or when the stop
    /// channel has been triggered by this custom display.
    async fn clear(&mut self);

    /// Process an update from the Graphix rt in the context of the
    /// custom display.
    ///
    /// This will be called by every update, even if it isn't related
    /// to the custom display. If the future returned by this method
    /// is never determined then the shell will hang.
    async fn process_update(&mut self, env: &Env, id: ExprId, v: Value);
}

/// Trait implemented by Graphix packages
pub trait Package<X: GXExt> {
    /// register builtins and return a resolver containing Graphix
    /// code contained in the package.
    ///
    /// The returned resolver must be a VFS resolver, and it's root
    /// path must be the name of the package. If this isn't the case
    /// then the package will be rejected by the shell, and a warning
    /// will be printed to the user.
    fn register(
        ctx: ExecCtx<GXRt<X>, X::UserEvent>,
        modules: &mut FxHashMap<netidx_core::path::Path, ArcStr>,
    ) -> Result<()>;

    /// Return true if the `CompExp` matches the custom display type
    /// of this package.
    fn is_custom(gx: &GXHandle<X>, env: &Env, e: &CompExp<X>) -> bool;

    /// Build and return a `CustomDisplay` instance which will be used
    /// to display the `CompExp` `e`.
    ///
    /// If the custom display mode wishes to stop (for example the
    /// user closed the last gui window), then the stop channel should
    /// be triggered, and the shell will call `CustomDisplay::clear`
    /// before dropping the `CustomDisplay`.
    fn init_custom(
        gx: &GXHandle<X>,
        env: &Env,
        stop: oneshot::Sender<()>,
        e: &CompExp<X>,
    ) -> Result<Box<dyn CustomDisplay<X>>>;
}

/// Create a new graphix package
///
/// The package will be created in a new directory named
/// `graphix-package-{name}` inside the directory `base`. If base is not a
/// directory the function will fail.
pub async fn create_package(base: &Path, name: &str) -> Result<()> {
    static CARGO_TOML: &str = include_str!("../skel/Cargo.toml");
    static LIB_RS: &str = include_str!("../skel/lib.rs");
    static MOD_GX: &str = include_str!("../skel/mod.gx");
    static MOD_GXI: &str = include_str!("../skel/mod.gxi");
    static README_MD: &str = include_str!("../skel/README.md");
    if !fs::metadata(base).await?.is_dir() {
        bail!("base path {base:?} does not exist, or is not a directory")
    }
    if name.contains(|c: char| c != '-' && !c.is_ascii_alphanumeric())
        || !name.starts_with("graphix-package-")
    {
        bail!("invalid package name, name must match graphix-package-[-a-z]+")
    }
    let full_path = base.join(name);
    if fs::metadata(&full_path).await.is_ok() {
        bail!("package {name} already exists")
    }
    fs::create_dir_all(&full_path.join("src").join("graphix")).await?;
    let mut hb = Handlebars::new();
    hb.register_template_string("Cargo.toml", CARGO_TOML)?;
    hb.register_template_string("lib.rs", LIB_RS)?;
    hb.register_template_string("mod.gx", MOD_GX)?;
    hb.register_template_string("mod.gxi", MOD_GXI)?;
    hb.register_template_string("README.md", README_MD)?;
    let params = json!({
        "version": env!("CARGO_PKG_VERSION"),

    });
    todo!()
}
