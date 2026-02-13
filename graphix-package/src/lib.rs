use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use compact_str::{format_compact, CompactString};
use crates_io_api::AsyncClient;
use flate2::bufread::MultiGzDecoder;
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
    time::Duration,
};
use tokio::{
    fs,
    io::{AsyncBufReadExt, BufReader},
    process::Command,
    sync::oneshot,
    task,
};
use walkdir::WalkDir;

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
        ctx: &mut ExecCtx<GXRt<X>, X::UserEvent>,
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
        e: CompExp<X>,
    ) -> Result<Box<dyn CustomDisplay<X>>>;
}

// package skeleton, our version, and deps template
struct Skel {
    version: &'static str,
    cargo_toml: &'static str,
    deps_rs: &'static str,
    lib_rs: &'static str,
    mod_gx: &'static str,
    mod_gxi: &'static str,
    readme_md: &'static str,
}

static SKEL: Skel = Skel {
    version: env!("CARGO_PKG_VERSION"),
    cargo_toml: include_str!("../skel/Cargo.toml"),
    deps_rs: include_str!("../skel/deps.rs"),
    lib_rs: include_str!("../skel/lib.rs"),
    mod_gx: include_str!("../skel/mod.gx"),
    mod_gxi: include_str!("../skel/mod.gxi"),
    readme_md: include_str!("../skel/README.md"),
};

/// Create a new graphix package
///
/// The package will be created in a new directory named
/// `graphix-package-{name}` inside the directory `base`. If base is not a
/// directory the function will fail.
pub async fn create_package(base: &Path, name: &str) -> Result<()> {
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
    hb.register_template_string("Cargo.toml", SKEL.cargo_toml)?;
    hb.register_template_string("lib.rs", SKEL.lib_rs)?;
    hb.register_template_string("mod.gx", SKEL.mod_gx)?;
    hb.register_template_string("mod.gxi", SKEL.mod_gxi)?;
    hb.register_template_string("README.md", SKEL.readme_md)?;
    let name = name.strip_prefix("graphix-package-").unwrap();
    let params = json!({
        "version": SKEL.version,
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

// fetch our source from the local cargo cache (preferred method)
async fn extract_local_source(cargo: &Path) -> Result<PathBuf> {
    let graphix = which::which("graphix").context("can't find the graphix command")?;
    let version = {
        let c = Command::new(&graphix).arg("--version").stdout(Stdio::piped()).spawn()?;
        BufReader::new(c.stdout.unwrap())
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
        Ok(md) if !md.is_dir() => bail!("{graphix_dir:?} isn't a directory"),
        Ok(_) => Ok(graphix_dir),
        Err(_) => {
            let package = format!("graphix-shell-{version}");
            let cargo_root =
                cargo.parent().ok_or_else(|| anyhow!("can't find cargo root"))?;
            let cargo_src = cargo_root.join("registry").join("src");
            match fs::metadata(&cargo_src).await {
                Ok(md) if md.is_dir() => (),
                Err(_) | Ok(_) => bail!("can't find cargo cache"),
            };
            let r = task::spawn_blocking({
                let graphix_dir = graphix_dir.clone();
                move || -> Result<()> {
                    let src_path = WalkDir::new(&cargo_src)
                        .max_depth(2)
                        .into_iter()
                        .find_map(|e| {
                            let e = e.ok()?;
                            if e.file_type().is_dir() && e.path().ends_with(&package) {
                                return Some(e.into_path());
                            }
                            None
                        })
                        .ok_or_else(|| {
                            anyhow!("graphix-shell-{version} in {cargo_src:?}")
                        })?;
                    cp_r::CopyOptions::new().copy_tree(&src_path, graphix_dir)?;
                    Ok(())
                }
            })
            .await?;
            match r {
                Ok(()) => Ok(graphix_dir),
                Err(e) => {
                    // clean up partial copy, if necessary
                    fs::remove_dir_all(&graphix_dir).await?;
                    Err(e)
                }
            }
        }
    }
}

// download our src from crates.io (backup method)
async fn download_source(crates_io: &AsyncClient) -> Result<PathBuf> {
    let graphix = which::which("graphix").context("can't find the graphix command")?;
    let version = {
        let c = Command::new(&graphix).arg("--version").stdout(Stdio::piped()).spawn()?;
        BufReader::new(c.stdout.unwrap())
            .lines()
            .next_line()
            .await?
            .ok_or_else(|| anyhow!("graphix did not return a version"))?
    };
    let package = format!("graphix-shell-{version}");
    let graphix_build_dir = dirs::data_local_dir()
        .ok_or_else(|| anyhow!("can't find your data dir"))?
        .join("graphix")
        .join("build");
    let graphix_dir = graphix_build_dir.join(&package);
    match fs::metadata(&graphix_build_dir).await {
        Err(_) => fs::create_dir_all(&graphix_build_dir).await?,
        Ok(md) if !md.is_dir() => bail!("{graphix_build_dir:?} isn't a directory"),
        Ok(_) => (),
    }
    match fs::metadata(&graphix_dir).await {
        Ok(md) if !md.is_dir() => bail!("{graphix_dir:?} isn't a directory"),
        Ok(_) => Ok(graphix_dir),
        Err(_) => {
            let cr = crates_io.get_crate("graphix-shell").await?;
            let cr_version =
                cr.versions.into_iter().find(|v| v.num == version).ok_or_else(|| {
                    anyhow!("can't find version {version} on crates.io")
                })?;
            let crate_data_tar_gz =
                reqwest::get(&cr_version.dl_path).await?.bytes().await?;
            let r = task::spawn_blocking({
                let graphix_dir = graphix_dir.clone();
                move || -> Result<()> {
                    use std::io::Read;
                    let mut crate_data_tar = vec![];
                    MultiGzDecoder::new(&crate_data_tar_gz[..])
                        .read_to_end(&mut crate_data_tar)?;
                    std::fs::create_dir_all(&graphix_dir)?;
                    tar::Archive::new(&mut &crate_data_tar[..]).unpack(&graphix_dir)?;
                    Ok(())
                }
            })
            .await?;
            match r {
                Ok(()) => Ok(graphix_dir),
                Err(e) => {
                    // clean up the failed unpack, if necessary
                    fs::remove_dir_all(&graphix_dir).await?;
                    Err(e)
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackageId {
    name: CompactString,
    version: Option<CompactString>,
}

impl PackageId {
    pub fn new(name: &str, version: Option<&str>) -> Self {
        let name = if name.starts_with("graphix-package-") {
            CompactString::from(name)
        } else {
            format_compact!("graphix-package-{name}")
        };
        let version = version.map(CompactString::from);
        Self { name, version }
    }

    pub fn name(&self) -> &str {
        self.name.strip_prefix("graphix-package-").unwrap()
    }

    pub fn version(&self) -> Option<&str> {
        self.version.as_ref().map(|s| s.as_str())
    }
}

/// The Graphix package manager
pub struct GraphixPM {
    cratesio: AsyncClient,
    cargo: PathBuf,
}

impl GraphixPM {
    /// Create a new package manager
    pub async fn new() -> Result<Self> {
        let cargo = which::which("cargo").context("can't find the cargo command")?;
        let cratesio = AsyncClient::new(
            "Graphix Package Manager <eestokes@pm.me>",
            Duration::from_secs(1),
        )?;
        Ok(Self { cratesio, cargo })
    }

    /// Unpack a fresh copy of the graphix-shell source. Tries the
    /// local cargo registry cache first, falls back to downloading
    /// from crates.io.
    async fn unpack_source(&self) -> Result<PathBuf> {
        match extract_local_source(&self.cargo).await {
            Ok(p) => Ok(p),
            Err(local) => match download_source(&self.cratesio).await {
                Ok(p) => Ok(p),
                Err(dl) => bail!("could not find our source local: {local}, dl: {dl}"),
            },
        }
    }

    /// Build a new graphix runtime with additional packages included
    ///
    /// Packages already built in will be ignored, packages with different
    /// versions be updated to the newly specified version.
    ///
    /// If there are any changes the graphix runtime will be rebuilt and
    /// reinstalled. You must restart graphix shell instances in order to use
    /// the new packages. The previous runtime will be saved as
    /// graphix-previous-{date}. Runtimes older than a week will be cleaned up
    /// by the shell on launch.
    pub async fn add_packages(&self, packages: &[PackageId]) -> Result<()> {
        todo!()
    }

    /// Remove packages from the graphix runtime
    ///
    /// The core package cannot be removed.
    pub async fn remove_packages(&self, packages: &[PackageId]) -> Result<()> {
        todo!()
    }

    /// Search crates.io for graphix packages
    pub async fn search(&self, query: &str) -> Result<()> {
        todo!()
    }

    /// List installed packages
    pub async fn list(&self) -> Result<()> {
        todo!()
    }
}
