use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crates_io_api::AsyncClient;
use fxhash::FxHashMap;
use graphix_compiler::{env::Env, expr::ExprId, ExecCtx};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use handlebars::Handlebars;
use netidx_value::Value;
use serde_json::json;
use std::{
    any::Any,
    path::{Path, PathBuf},
    process::Stdio,
};
use tokio::{
    fs,
    io::{AsyncBufReadExt, BufReader},
    process::Command,
    sync::oneshot,
};

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

const VERSION: &str = env!("CARGO_PKG_VERSION");

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
    let name = name.strip_prefix("graphix-package-").unwrap();
    let params = json!({
        "version": VERSION,
        "name": name,
        "deps": []
    });
    fs::write(full_path.join("Cargo.toml"), hb.render("Cargo.toml", &params)?).await?;
    fs::write(full_path.join("README.md"), hb.render("README.md", &params)?).await?;
    let src = full_path.join("src");
    fs::write(src.join("lib.rs"), hb.render("lib.rs", &params)?).await?;
    let graphix_src = src.join("graphix");
    fs::write(&graphix_src.join("mod.gx"), hb.render("mod.gx", &params)?).await?;
    fs::write(&graphix_src.join("mod.gxi"), hb.render("mod.gxi", &params)?).await?;
    Ok(())
}

async fn extract_local_source(
    cargo: &Path,
    version: String,
    graphix_build_dir: PathBuf,
    graphix_dir: PathBuf,
) -> Result<PathBuf> {
    todo!()
}

async fn maybe_extract_local_source(cargo: &Path) -> Result<PathBuf> {
    let graphix = which::which("graphix").context("can't find the graphix command")?;
    let version = {
        let mut c =
            Command::new(&graphix).arg("--version").stdout(Stdio::piped()).spawn()?;
        BufReader::new(c.stdout.take().unwrap())
            .lines()
            .next_line()
            .await?
            .ok_or_else(|| anyhow!("graphix did not return a version"))?
    };
    let graphix_build_dir = dirs::data_local_dir()
        .ok_or_else(|| anyhow!("can't find your data dir"))?
        .join("graphix")
        .join("build");
    let graphix_dir = graphix_build_dir.join(format!("graphix-shell-{version}"));
    match fs::metadata(&graphix_build_dir).await {
        Err(_) => fs::create_dir_all(&graphix_build_dir).await?,
        Ok(md) if !md.is_dir() => bail!("{graphix_build_dir:?} isn't a directory"),
        Ok(_) => (),
    }
    match fs::metadata(&graphix_dir).await {
        Err(_) => {
            extract_local_source(cargo, version, graphix_build_dir, graphix_dir).await
        }
        Ok(md) if !md.is_dir() => bail!("{graphix_dir:?} isn't a directory"),
        Ok(_) => Ok(graphix_dir),
    }
}

/// The Graphix package manager
pub struct GraphixPM {
    cratesio: AsyncClient,
    cargo: PathBuf,
    local_src: PathBuf,
}

impl GraphixPM {
    async fn new() -> Result<Self> {
        let cargo = which::which("cargo").context("can't find the cargo command")?;

        todo!()
    }
}
