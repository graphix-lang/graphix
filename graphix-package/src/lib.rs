use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use chrono::Local;
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
    collections::BTreeMap,
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

fn graphix_data_dir() -> Result<PathBuf> {
    Ok(dirs::data_local_dir()
        .ok_or_else(|| anyhow!("can't find your data dir"))?
        .join("graphix"))
}

fn packages_toml_path() -> Result<PathBuf> {
    Ok(graphix_data_dir()?.join("packages.toml"))
}

/// The default set of packages shipped with graphix
const DEFAULT_PACKAGES: &[(&str, &str)] = &[
    ("core", SKEL.version),
    ("array", SKEL.version),
    ("str", SKEL.version),
    ("map", SKEL.version),
    ("fs", SKEL.version),
    ("net", SKEL.version),
    ("time", SKEL.version),
    ("re", SKEL.version),
    ("rand", SKEL.version),
    ("tui", SKEL.version),
];

/// A package entry in packages.toml — either a version string or a path.
#[derive(Debug, Clone)]
pub enum PackageEntry {
    Version(String),
    Path(PathBuf),
}

impl std::fmt::Display for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Version(v) => write!(f, "{v}"),
            Self::Path(p) => write!(f, "path:{}", p.display()),
        }
    }
}

/// Read the packages.toml file, creating it with defaults if it doesn't exist.
async fn read_packages() -> Result<BTreeMap<String, PackageEntry>> {
    let path = packages_toml_path()?;
    match fs::read_to_string(&path).await {
        Ok(contents) => {
            let doc: toml::Value =
                toml::from_str(&contents).context("parsing packages.toml")?;
            let tbl = doc
                .get("packages")
                .and_then(|v| v.as_table())
                .ok_or_else(|| anyhow!("packages.toml missing [packages] table"))?;
            let mut packages = BTreeMap::new();
            for (k, v) in tbl {
                let entry = match v {
                    toml::Value::String(s) => PackageEntry::Version(s.clone()),
                    toml::Value::Table(t) => {
                        if let Some(p) = t.get("path").and_then(|v| v.as_str()) {
                            PackageEntry::Path(PathBuf::from(p))
                        } else {
                            bail!("package {k}: table entry must have a 'path' key")
                        }
                    }
                    _ => bail!("package {k}: expected a version string or table"),
                };
                packages.insert(k.clone(), entry);
            }
            Ok(packages)
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let packages: BTreeMap<String, PackageEntry> = DEFAULT_PACKAGES
                .iter()
                .map(|(k, v)| (k.to_string(), PackageEntry::Version(v.to_string())))
                .collect();
            write_packages(&packages).await?;
            Ok(packages)
        }
        Err(e) => Err(e.into()),
    }
}

/// Write the packages.toml file
async fn write_packages(packages: &BTreeMap<String, PackageEntry>) -> Result<()> {
    let path = packages_toml_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).await?;
    }
    let mut doc = toml::value::Table::new();
    let mut tbl = toml::value::Table::new();
    for (k, entry) in packages {
        match entry {
            PackageEntry::Version(v) => {
                tbl.insert(k.clone(), toml::Value::String(v.clone()));
            }
            PackageEntry::Path(p) => {
                let mut t = toml::value::Table::new();
                t.insert(
                    "path".to_string(),
                    toml::Value::String(p.to_string_lossy().into_owned()),
                );
                tbl.insert(k.clone(), toml::Value::Table(t));
            }
        }
    }
    doc.insert("packages".to_string(), toml::Value::Table(tbl));
    fs::write(&path, toml::to_string_pretty(&doc)?).await?;
    Ok(())
}

/// Get the graphix version string from the running binary
async fn graphix_version() -> Result<String> {
    let graphix = which::which("graphix").context("can't find the graphix command")?;
    let c = Command::new(&graphix).arg("--version").stdout(Stdio::piped()).spawn()?;
    let line = BufReader::new(c.stdout.unwrap())
        .lines()
        .next_line()
        .await?
        .ok_or_else(|| anyhow!("graphix did not return a version"))?;
    // version output may be "graphix 0.3.2" or just "0.3.2"
    Ok(line.split_whitespace().last().unwrap_or(&line).to_string())
}

// fetch our source from the local cargo cache (preferred method)
async fn extract_local_source(cargo: &Path) -> Result<PathBuf> {
    let version = graphix_version().await?;
    let graphix_build_dir = graphix_data_dir()?.join("build");
    let graphix_dir = graphix_build_dir.join(format!("graphix-shell-{version}"));
    match fs::metadata(&graphix_build_dir).await {
        Err(_) => fs::create_dir_all(&graphix_build_dir).await?,
        Ok(md) if !md.is_dir() => bail!("{graphix_build_dir:?} isn't a directory"),
        Ok(_) => (),
    }
    match fs::metadata(&graphix_dir).await {
        Ok(md) if !md.is_dir() => bail!("{graphix_dir:?} isn't a directory"),
        Ok(_) => return Ok(graphix_dir),
        Err(_) => (),
    }
    let package = format!("graphix-shell-{version}");
    let cargo_root = cargo.parent().ok_or_else(|| anyhow!("can't find cargo root"))?;
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
                .ok_or_else(|| anyhow!("can't find {package} in {cargo_src:?}"))?;
            cp_r::CopyOptions::new().copy_tree(&src_path, graphix_dir)?;
            Ok(())
        }
    })
    .await?;
    match r {
        Ok(()) => Ok(graphix_dir),
        Err(e) => {
            let _ = fs::remove_dir_all(&graphix_dir).await;
            Err(e)
        }
    }
}

// download our src from crates.io (backup method)
async fn download_source(crates_io: &AsyncClient) -> Result<PathBuf> {
    let version = graphix_version().await?;
    let package = format!("graphix-shell-{version}");
    let graphix_build_dir = graphix_data_dir()?.join("build");
    let graphix_dir = graphix_build_dir.join(&package);
    match fs::metadata(&graphix_build_dir).await {
        Err(_) => fs::create_dir_all(&graphix_build_dir).await?,
        Ok(md) if !md.is_dir() => bail!("{graphix_build_dir:?} isn't a directory"),
        Ok(_) => (),
    }
    match fs::metadata(&graphix_dir).await {
        Ok(md) if !md.is_dir() => bail!("{graphix_dir:?} isn't a directory"),
        Ok(_) => return Ok(graphix_dir),
        Err(_) => (),
    }
    let cr = crates_io.get_crate("graphix-shell").await?;
    let cr_version = cr
        .versions
        .into_iter()
        .find(|v| v.num == version)
        .ok_or_else(|| anyhow!("can't find version {version} on crates.io"))?;
    let crate_data_tar_gz = reqwest::get(&cr_version.dl_path).await?.bytes().await?;
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
            let _ = fs::remove_dir_all(&graphix_dir).await;
            Err(e)
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackageId {
    name: CompactString,
    version: Option<CompactString>,
    path: Option<PathBuf>,
}

impl PackageId {
    pub fn new(name: &str, version: Option<&str>) -> Self {
        let name = if name.starts_with("graphix-package-") {
            CompactString::from(name.strip_prefix("graphix-package-").unwrap())
        } else {
            CompactString::from(name)
        };
        let version = version.map(CompactString::from);
        Self { name, version, path: None }
    }

    pub fn with_path(name: &str, path: PathBuf) -> Self {
        let name = if name.starts_with("graphix-package-") {
            CompactString::from(name.strip_prefix("graphix-package-").unwrap())
        } else {
            CompactString::from(name)
        };
        Self { name, version: None, path: Some(path) }
    }

    /// Short name without graphix-package- prefix
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The full crate name
    pub fn crate_name(&self) -> CompactString {
        format_compact!("graphix-package-{}", self.name)
    }

    pub fn version(&self) -> Option<&str> {
        self.version.as_ref().map(|s| s.as_str())
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
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

    /// Open the lock file for the graphix data directory.
    /// Call `.write()` on the returned lock to acquire exclusive access.
    fn lock_file() -> Result<fd_lock::RwLock<std::fs::File>> {
        let lock_path = graphix_data_dir()?.join("graphix.lock");
        if let Some(parent) = lock_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = std::fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(&lock_path)
            .context("opening lock file")?;
        Ok(fd_lock::RwLock::new(file))
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

    /// Generate deps.rs from the package list
    fn generate_deps_rs(
        &self,
        packages: &BTreeMap<String, PackageEntry>,
    ) -> Result<String> {
        let mut hb = Handlebars::new();
        hb.register_template_string("deps.rs", SKEL.deps_rs)?;
        let deps: Vec<serde_json::Value> = packages
            .keys()
            .map(|name| {
                json!({
                    "crate_name": format!("graphix_package_{}", name.replace('-', "_")),
                })
            })
            .collect();
        let mut root_parts = vec![];
        for name in packages.keys() {
            if name == "core" {
                root_parts.push("mod core;\\nuse core".to_string());
            } else {
                root_parts.push(format!("mod {name}"));
            }
        }
        let root_expr = root_parts.join(";\\n");
        let params = json!({
            "deps": deps,
            "root_expr": root_expr,
        });
        Ok(hb.render("deps.rs", &params)?)
    }

    /// Update Cargo.toml to include package dependencies
    fn update_cargo_toml(
        &self,
        cargo_toml_content: &str,
        packages: &BTreeMap<String, PackageEntry>,
    ) -> Result<String> {
        use toml_edit::DocumentMut;
        let mut doc: DocumentMut =
            cargo_toml_content.parse().context("parsing Cargo.toml")?;
        let deps = doc["dependencies"]
            .as_table_mut()
            .ok_or_else(|| anyhow!("Cargo.toml missing [dependencies]"))?;
        let to_remove: Vec<String> = deps
            .iter()
            .filter_map(|(k, _)| {
                if k.starts_with("graphix-package-") {
                    Some(k.to_string())
                } else {
                    None
                }
            })
            .collect();
        for k in to_remove {
            deps.remove(&k);
        }
        for (name, entry) in packages {
            let crate_name = format!("graphix-package-{name}");
            match entry {
                PackageEntry::Version(version) => {
                    deps[&crate_name] = toml_edit::value(version);
                }
                PackageEntry::Path(path) => {
                    let mut tbl = toml_edit::InlineTable::new();
                    tbl.insert(
                        "path",
                        toml_edit::Value::from(path.to_string_lossy().as_ref()),
                    );
                    deps[&crate_name] = toml_edit::Item::Value(tbl.into());
                }
            }
        }
        Ok(doc.to_string())
    }

    /// Rebuild the graphix binary with the current package set
    async fn rebuild(&self) -> Result<()> {
        let packages = read_packages().await?;
        println!("Unpacking graphix-shell source...");
        // Delete existing build dir to get a fresh source
        let build_dir = graphix_data_dir()?.join("build");
        if fs::metadata(&build_dir).await.is_ok() {
            fs::remove_dir_all(&build_dir).await?;
        }
        let source_dir = self.unpack_source().await?;
        // Generate deps.rs
        println!("Generating deps.rs...");
        let deps_rs = self.generate_deps_rs(&packages)?;
        fs::write(source_dir.join("src").join("deps.rs"), &deps_rs).await?;
        // Update Cargo.toml with package dependencies
        println!("Updating Cargo.toml...");
        let cargo_toml_path = source_dir.join("Cargo.toml");
        let cargo_toml_content = fs::read_to_string(&cargo_toml_path).await?;
        let updated_cargo_toml =
            self.update_cargo_toml(&cargo_toml_content, &packages)?;
        fs::write(&cargo_toml_path, &updated_cargo_toml).await?;
        // Save previous binary
        if let Ok(graphix_path) = which::which("graphix") {
            let date = Local::now().format("%Y%m%d-%H%M%S");
            let backup_name = format!(
                "graphix-previous-{date}{}",
                graphix_path
                    .extension()
                    .map(|e| format!(".{}", e.to_string_lossy()))
                    .unwrap_or_default()
            );
            let backup_path = graphix_path.with_file_name(&backup_name);
            let _ = fs::copy(&graphix_path, &backup_path).await;
        }
        // Build and install
        println!("Building graphix with updated packages (this may take a while)...");
        let status = Command::new(&self.cargo)
            .arg("install")
            .arg("--path")
            .arg(&source_dir)
            .arg("--force")
            .status()
            .await
            .context("running cargo install")?;
        if !status.success() {
            bail!("cargo install failed with status {status}")
        }
        // Clean up old previous binaries (>1 week)
        self.cleanup_old_binaries().await;
        println!("Done! Restart graphix to use the updated packages.");
        Ok(())
    }

    /// Clean up graphix-previous-* binaries older than 1 week
    async fn cleanup_old_binaries(&self) {
        let Ok(graphix_path) = which::which("graphix") else { return };
        let Some(bin_dir) = graphix_path.parent() else { return };
        let Ok(mut entries) = fs::read_dir(bin_dir).await else { return };
        let week_ago =
            std::time::SystemTime::now() - std::time::Duration::from_secs(7 * 24 * 3600);
        while let Ok(Some(entry)) = entries.next_entry().await {
            let name = entry.file_name();
            let Some(name) = name.to_str() else { continue };
            if !name.starts_with("graphix-previous-") {
                continue;
            }
            if let Ok(md) = entry.metadata().await {
                if let Ok(modified) = md.modified() {
                    if modified < week_ago {
                        let _ = fs::remove_file(entry.path()).await;
                    }
                }
            }
        }
    }

    /// Read the version from a package crate's Cargo.toml at the given path
    async fn read_package_version(path: &Path) -> Result<String> {
        let cargo_toml_path = path.join("Cargo.toml");
        let contents = fs::read_to_string(&cargo_toml_path)
            .await
            .with_context(|| format!("reading {}", cargo_toml_path.display()))?;
        let doc: toml::Value =
            toml::from_str(&contents).context("parsing package Cargo.toml")?;
        doc.get("package")
            .and_then(|p| p.get("version"))
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .ok_or_else(|| anyhow!("no version found in {}", cargo_toml_path.display()))
    }

    /// Add packages and rebuild
    pub async fn add_packages(
        &self,
        packages: &[PackageId],
        skip_crates_io_check: bool,
    ) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = lock.write().context("waiting for package lock")?;
        let mut installed = read_packages().await?;
        let mut changed = false;
        for pkg in packages {
            let entry = if let Some(path) = pkg.path() {
                let path = path
                    .canonicalize()
                    .with_context(|| format!("resolving path {}", path.display()))?;
                let version = Self::read_package_version(&path).await?;
                println!(
                    "Adding {} @ path {} (version {version})",
                    pkg.name(),
                    path.display()
                );
                PackageEntry::Path(path)
            } else if skip_crates_io_check {
                match pkg.version() {
                    Some(v) => {
                        println!("Adding {}@{v}", pkg.name());
                        PackageEntry::Version(v.to_string())
                    }
                    None => bail!(
                        "version is required for {} when using --skip-crates-io-check",
                        pkg.name()
                    ),
                }
            } else {
                let crate_name = pkg.crate_name();
                let cr =
                    self.cratesio.get_crate(&crate_name).await.with_context(|| {
                        format!("package {crate_name} not found on crates.io")
                    })?;
                let version = match pkg.version() {
                    Some(v) => v.to_string(),
                    None => cr.crate_data.max_version.clone(),
                };
                println!("Adding {}@{version}", pkg.name());
                PackageEntry::Version(version)
            };
            installed.insert(pkg.name().to_string(), entry);
            changed = true;
        }
        if changed {
            write_packages(&installed).await?;
            self.rebuild().await?;
        } else {
            println!("No changes needed.");
        }
        Ok(())
    }

    /// Remove packages and rebuild
    pub async fn remove_packages(&self, packages: &[PackageId]) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = lock.write().context("waiting for package lock")?;
        let mut installed = read_packages().await?;
        let mut changed = false;
        for pkg in packages {
            if pkg.name() == "core" {
                eprintln!("Cannot remove the core package");
                continue;
            }
            if installed.remove(pkg.name()).is_some() {
                println!("Removing {}", pkg.name());
                changed = true;
            } else {
                println!("{} is not installed", pkg.name());
            }
        }
        if changed {
            write_packages(&installed).await?;
            self.rebuild().await?;
        } else {
            println!("No changes needed.");
        }
        Ok(())
    }

    /// Search crates.io for graphix packages
    pub async fn search(&self, query: &str) -> Result<()> {
        let search_query = format!("graphix-package-{query}");
        let results = self
            .cratesio
            .crates(crates_io_api::CratesQuery::builder().search(&search_query).build())
            .await?;
        if results.crates.is_empty() {
            println!("No packages found matching '{query}'");
        } else {
            for cr in &results.crates {
                let name = cr.name.strip_prefix("graphix-package-").unwrap_or(&cr.name);
                let desc = cr.description.as_deref().unwrap_or("");
                println!("{name} ({}) - {desc}", cr.max_version);
            }
        }
        Ok(())
    }

    /// Rebuild the graphix binary from the current packages.toml
    pub async fn do_rebuild(&self) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = lock.write().context("waiting for package lock")?;
        self.rebuild().await
    }

    /// List installed packages
    pub async fn list(&self) -> Result<()> {
        let packages = read_packages().await?;
        if packages.is_empty() {
            println!("No packages installed");
        } else {
            for (name, version) in &packages {
                println!("{name}: {version}");
            }
        }
        Ok(())
    }
}
