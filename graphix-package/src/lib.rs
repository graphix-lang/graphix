#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use chrono::Local;
use compact_str::{CompactString, format_compact};
use crates_io_api::AsyncClient;
use flate2::bufread::MultiGzDecoder;
use graphix_compiler::{
    ExecCtx,
    env::Env,
    expr::{ExprId, VfsEntry},
};
/// `packages!()` builds `Vec<Box<dyn Package<X>>>` (owned, feature-gated) and
/// `package_refs!()` builds `&'static [&'static dyn Package<NoExt>]` (const),
/// both by scraping the calling crate's `Cargo.toml` for `graphix-package-*`
/// deps. Re-exported here so callers use `graphix_package::packages!()`.
pub use graphix_derive::{package_refs, packages};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use handlebars::Handlebars;
pub use indexmap::IndexSet;
use netidx_value::Value;
use reqwest::Url;
use serde_json::json;
use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet},
    future::Future,
    io::IsTerminal,
    path::{Path, PathBuf},
    pin::Pin,
    process::Stdio,
    sync::mpsc as smpsc,
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

#[cfg(test)]
mod test;

/// Handle to run a closure on the main thread
#[derive(Clone)]
pub struct MainThreadHandle(smpsc::Sender<Box<dyn FnOnce() + Send + 'static>>);

impl MainThreadHandle {
    pub fn new() -> (Self, smpsc::Receiver<Box<dyn FnOnce() + Send + 'static>>) {
        let (tx, rx) = smpsc::channel();
        (Self(tx), rx)
    }

    pub fn run(&self, f: Box<dyn FnOnce() + Send + 'static>) -> Result<()> {
        self.0.send(f).map_err(|_| anyhow!("main thread receiver dropped"))
    }
}

/// Trait implemented by custom Graphix displays, e.g. TUIs, GUIs, etc.
#[async_trait]
pub trait CustomDisplay<X: GXExt>: Any {
    /// Clear the custom display, freeing any used resources.
    ///
    /// This is called when the shell user has indicated that they
    /// want to return to the normal display mode or when the stop
    /// channel has been triggered by this custom display.
    ///
    /// If the custom display has started a closure on the main thread, it must
    /// now stop it.
    async fn clear(&mut self);

    /// Process an update from the Graphix rt in the context of the
    /// custom display.
    ///
    /// This will be called by every update, even if it isn't related
    /// to the custom display. If the future returned by this method
    /// is never determined then the shell will hang.
    async fn process_update(&mut self, env: &Env, id: ExprId, v: Value);
}

/// A live custom display plus the channel the shell watches for it to stop.
pub struct Cdc<X: GXExt> {
    pub stop: oneshot::Receiver<()>,
    pub custom: Box<dyn CustomDisplay<X>>,
}

/// The result of `Package::maybe_init_custom`: the package either claimed the
/// value and built a custom display (`Custom`), or passed it through
/// (`NotCustom`, returning the value for the next package / normal display).
pub enum CustomResult<X: GXExt> {
    Custom(Cdc<X>),
    NotCustom(CompExp<X>),
}

/// Trait implemented by Graphix packages. Object-safe, so packages can be
/// collected as `Vec<Box<dyn Package<X>>>` (or `&[&dyn Package<X>]`) and driven
/// uniformly by the shell and the test harness. `Send + Sync` because the
/// generated package type `P` is a ZST (the bound is always satisfied) and some
/// callers share/move the package list across threads. Normally implemented by
/// the `defpackage!` macro.
pub trait Package<X: GXExt>: Send + Sync {
    /// Register this package's builtins and Graphix modules: modules by path in
    /// `modules`, the package itself by name in `root_mods`. Registration is
    /// idempotent (guarded by `root_mods`) and transitively registers the
    /// package's dependency packages.
    fn register(
        &self,
        ctx: &mut ExecCtx<GXRt<X>, X::UserEvent>,
        modules: &mut AHashMap<netidx_core::path::Path, VfsEntry>,
        root_mods: &mut IndexSet<ArcStr>,
    ) -> Result<()>;

    /// If `e` matches this package's custom display type, build and return the
    /// display (`Custom`); otherwise return `NotCustom(e)` to pass it on. When a
    /// display wants to stop, it triggers its stop channel and the shell calls
    /// `CustomDisplay::clear` before dropping it. Most packages have no custom
    /// display and always return `NotCustom`.
    fn maybe_init_custom<'a>(
        &'a self,
        gx: &'a GXHandle<X>,
        env: &'a Env,
        e: CompExp<X>,
        run_on_main: &'a MainThreadHandle,
    ) -> Pin<Box<dyn Future<Output = Result<CustomResult<X>>> + 'a>>;

    /// The package's main program source, if it has one and the `standalone`
    /// feature is enabled.
    fn main_program(&self) -> Option<&'static str>;
}

/// Build the root-module prelude from the registered package names: `mod
/// core;\nuse core` (core is brought into scope) plus `mod <name>` for each
/// other package, joined by `;\n`. Shared by the shell, the LSP, and the test
/// harness.
pub fn root_module_source(root_mods: &IndexSet<ArcStr>) -> ArcStr {
    let mut parts = Vec::new();
    for name in root_mods {
        if name == "core" {
            parts.push(format!("mod core;\nuse core"));
        } else {
            parts.push(format!("mod {name}"));
        }
    }
    ArcStr::from(parts.join(";\n"))
}

// new-package skeleton templates
struct Skel {
    cargo_toml: &'static str,
    lib_rs: &'static str,
    build_rs: &'static str,
    mod_gx: &'static str,
    mod_gxi: &'static str,
    readme_md: &'static str,
}

static SKEL: Skel = Skel {
    cargo_toml: include_str!("skel/Cargo.toml.hbs"),
    lib_rs: include_str!("skel/lib.rs"),
    build_rs: include_str!("skel/build.rs"),
    mod_gx: include_str!("skel/mod.gx"),
    mod_gxi: include_str!("skel/mod.gxi"),
    readme_md: include_str!("skel/README.md"),
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
    let params = json!({"name": name, "deps": []});
    fs::write(full_path.join("Cargo.toml"), hb.render("Cargo.toml", &params)?).await?;
    fs::write(full_path.join("README.md"), hb.render("README.md", &params)?).await?;
    fs::write(full_path.join("build.rs"), SKEL.build_rs).await?;
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

/// The default set of user-facing stdlib packages shipped with graphix.
/// Stdlib packages track the shell version and carry no per-package version.
/// This list seeds a fresh packages.toml and serves as the "known stdlib" set
/// when migrating an old-format file. The authoritative set at any given shell
/// version is enumerated from that version's source Cargo.toml
/// (`stdlib_packages_in_source`); this constant is only a bootstrap heuristic
/// and may legitimately drift behind the source.
const DEFAULT_PACKAGES: &[&str] = &[
    "core", "array", "str", "map", "sys", "http", "json", "toml", "pack", "xls",
    "sqlite", "db", "list", "args", "hbs", "re", "rand", "tui", "gui",
];

/// Stdlib crates the shell depends on but that are not user-facing: never
/// seeded into a fresh install, never auto-surfaced by `update`. Still
/// installable on purpose via `graphix package add <name>`.
const INTERNAL_PACKAGES: &[&str] = &["bench"];

/// Old top-level stdlib packages that were merged into another package. On
/// migration the dead name is dropped (the crate has no version compatible with
/// the current shell, so leaving it would break the build) and its replacement
/// is installed instead, so the user keeps the functionality — `fs`/`net`/`time`
/// are now submodules of `sys` (`sys::fs`, etc.).
const LEGACY_REMAP: &[(&str, &str)] = &[("fs", "sys"), ("net", "sys"), ("time", "sys")];

/// True if `name` is a stdlib package (user-facing or internal). Stdlib
/// packages track the shell version and are recorded by name only.
fn is_stdlib_package(name: &str) -> bool {
    DEFAULT_PACKAGES.contains(&name) || INTERNAL_PACKAGES.contains(&name)
}

/// A package entry in packages.toml — either a version string or a path.
#[derive(Debug, Clone, PartialEq, Eq)]
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

/// The in-memory model of packages.toml (format v2).
///
/// Stdlib packages track the shell version and are recorded by name only,
/// split into the set the user wants (`stdlib_installed`) and the set they
/// explicitly declined or removed (`stdlib_removed`). Third-party packages
/// live in `external`, keyed by short-name, still carrying a version or path.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Packages {
    stdlib_installed: BTreeSet<String>,
    stdlib_removed: BTreeSet<String>,
    external: BTreeMap<String, PackageEntry>,
}

impl Packages {
    /// `core` is mandatory and can never be removed; a name can't be both
    /// installed and removed (installed wins). Call after every load/mutation.
    fn enforce_invariants(&mut self) {
        self.stdlib_installed.insert("core".to_string());
        let Self { stdlib_installed, stdlib_removed, external: _ } = self;
        stdlib_removed.retain(|n| !stdlib_installed.contains(n));
    }

    /// Derive the inputs to a shell build: the stdlib Cargo feature list
    /// (installed stdlib packages minus `core`, which is always compiled) and
    /// the external packages (compiled as regular deps and registered via the
    /// generated `packages.rs`). Stdlib packages are selected by feature, so a
    /// removed stdlib package is simply absent from the feature list — no files
    /// are edited for stdlib changes.
    fn build_plan(&self) -> BuildPlan {
        let features = self
            .stdlib_installed
            .iter()
            .filter(|n| n.as_str() != "core")
            .cloned()
            .collect();
        BuildPlan { features, external: self.external.clone() }
    }
}

/// The inputs to a shell build derived from a package set (see
/// [`Packages::build_plan`]). `features` is sorted (it comes from a `BTreeSet`).
#[derive(Debug, Clone, PartialEq, Eq)]
struct BuildPlan {
    features: Vec<String>,
    external: BTreeMap<String, PackageEntry>,
}

/// Parse a single external entry value (`"1.2.3"` or `{ path = "..." }`).
fn parse_entry(k: &str, v: &toml::Value) -> Result<PackageEntry> {
    match v {
        toml::Value::String(s) => Ok(PackageEntry::Version(s.clone())),
        toml::Value::Table(t) => {
            if let Some(p) = t.get("path").and_then(|v| v.as_str()) {
                Ok(PackageEntry::Path(PathBuf::from(p)))
            } else {
                bail!("package {k}: table entry must have a 'path' key")
            }
        }
        _ => bail!("package {k}: expected a version string or table"),
    }
}

fn parse_external_table(
    tbl: Option<&toml::Value>,
) -> Result<BTreeMap<String, PackageEntry>> {
    let mut external = BTreeMap::new();
    let Some(tbl) = tbl else { return Ok(external) };
    let tbl = tbl.as_table().ok_or_else(|| anyhow!("[packages] must be a table"))?;
    for (k, v) in tbl {
        external.insert(k.clone(), parse_entry(k, v)?);
    }
    Ok(external)
}

/// Parse packages.toml. Returns the model and whether it was migrated from the
/// old flat format (`true`) or was already v2 (`false`).
fn parse_packages(contents: &str) -> Result<(Packages, bool)> {
    let doc: toml::Value = toml::from_str(contents).context("parsing packages.toml")?;
    if doc.get("stdlib").and_then(|v| v.as_table()).is_some() {
        Ok((parse_v2(&doc)?, false))
    } else {
        Ok((migrate_old(&doc)?, true))
    }
}

fn parse_v2(doc: &toml::Value) -> Result<Packages> {
    let stdlib = doc
        .get("stdlib")
        .and_then(|v| v.as_table())
        .ok_or_else(|| anyhow!("packages.toml missing [stdlib] table"))?;
    let read_names = |key: &str| -> Result<BTreeSet<String>> {
        match stdlib.get(key) {
            None => Ok(BTreeSet::new()),
            Some(toml::Value::Array(a)) => a
                .iter()
                .map(|v| {
                    v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                        anyhow!("[stdlib].{key} must be an array of strings")
                    })
                })
                .collect(),
            Some(_) => bail!("[stdlib].{key} must be an array of strings"),
        }
    };
    let mut p = Packages {
        stdlib_installed: read_names("installed")?,
        stdlib_removed: read_names("removed")?,
        external: parse_external_table(doc.get("packages"))?,
    };
    p.enforce_invariants();
    Ok(p)
}

/// Migrate an old-format file (flat `[packages]` of stdlib + external) to v2.
fn migrate_old(doc: &toml::Value) -> Result<Packages> {
    let tbl = doc
        .get("packages")
        .and_then(|v| v.as_table())
        .ok_or_else(|| anyhow!("packages.toml missing [packages] table"))?;
    let mut stdlib_installed = BTreeSet::new();
    let mut external = BTreeMap::new();
    for (k, v) in tbl {
        if let Some((_, replacement)) = LEGACY_REMAP.iter().find(|(old, _)| old == k) {
            eprintln!(
                "warning: package '{k}' is now part of '{replacement}'; installing \
                 '{replacement}' (use {replacement}::{k} instead of {k})"
            );
            stdlib_installed.insert(replacement.to_string());
            continue;
        }
        let entry = parse_entry(k, v)?;
        if is_stdlib_package(k) {
            if let PackageEntry::Path(p) = &entry {
                eprintln!(
                    "warning: stdlib package '{k}' had a path override ({}); \
                     stdlib now tracks the shell version, dropping the override",
                    p.display()
                );
            }
            stdlib_installed.insert(k.clone());
        } else {
            external.insert(k.clone(), entry);
        }
    }
    // Stdlib names absent from the old file were removed by the user.
    let stdlib_removed = DEFAULT_PACKAGES
        .iter()
        .filter(|n| !stdlib_installed.contains(**n))
        .map(|n| n.to_string())
        .collect();
    let mut p = Packages { stdlib_installed, stdlib_removed, external };
    p.enforce_invariants();
    Ok(p)
}

/// Serialize a `Packages` to the v2 on-disk format.
fn to_toml_string(p: &Packages) -> Result<String> {
    let to_arr = |s: &BTreeSet<String>| -> toml::Value {
        toml::Value::Array(s.iter().map(|n| toml::Value::String(n.clone())).collect())
    };
    let mut stdlib = toml::value::Table::new();
    stdlib.insert("installed".to_string(), to_arr(&p.stdlib_installed));
    stdlib.insert("removed".to_string(), to_arr(&p.stdlib_removed));
    let mut pkgs = toml::value::Table::new();
    for (k, entry) in &p.external {
        match entry {
            PackageEntry::Version(v) => {
                pkgs.insert(k.clone(), toml::Value::String(v.clone()));
            }
            PackageEntry::Path(path) => {
                let mut t = toml::value::Table::new();
                t.insert(
                    "path".to_string(),
                    toml::Value::String(path.to_string_lossy().into_owned()),
                );
                pkgs.insert(k.clone(), toml::Value::Table(t));
            }
        }
    }
    let mut doc = toml::value::Table::new();
    doc.insert("stdlib".to_string(), toml::Value::Table(stdlib));
    doc.insert("packages".to_string(), toml::Value::Table(pkgs));
    Ok(toml::to_string_pretty(&toml::Value::Table(doc))?)
}

/// Read packages.toml, creating it with defaults (and migrating if needed).
/// An old-format file is upgraded to v2 in place on first read (best-effort —
/// a write failure doesn't fail the read), so migration runs exactly once.
async fn read_packages() -> Result<Packages> {
    let path = packages_toml_path()?;
    match fs::read_to_string(&path).await {
        Ok(contents) => {
            let (p, migrated) = parse_packages(&contents)?;
            if migrated {
                let _ = write_packages(&p).await;
            }
            Ok(p)
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let mut p = Packages::default();
            p.stdlib_installed = DEFAULT_PACKAGES.iter().map(|n| n.to_string()).collect();
            p.enforce_invariants();
            write_packages(&p).await?;
            Ok(p)
        }
        Err(e) => Err(e.into()),
    }
}

/// Write the packages.toml file
async fn write_packages(p: &Packages) -> Result<()> {
    let path = packages_toml_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).await?;
    }
    fs::write(&path, to_toml_string(p)?).await?;
    Ok(())
}

/// Enumerate the stdlib package short-names from a Cargo.toml's
/// `[dependencies]` — every `graphix-package-<name>` key (the trailing dash
/// excludes the `graphix-package` crate itself; the value shape is ignored, so
/// the `optional` gui entry is included).
fn stdlib_packages_in_cargo_toml(content: &str) -> Result<BTreeSet<String>> {
    use toml_edit::DocumentMut;
    let doc: DocumentMut = content.parse().context("parsing shell Cargo.toml")?;
    let deps = doc
        .get("dependencies")
        .and_then(|d| d.as_table())
        .ok_or_else(|| anyhow!("Cargo.toml missing [dependencies]"))?;
    Ok(deps
        .iter()
        .filter_map(|(k, _)| k.strip_prefix("graphix-package-").map(|s| s.to_string()))
        .collect())
}

/// The stdlib set shipped by an unpacked graphix-shell source tree.
async fn stdlib_packages_in_source(source_dir: &Path) -> Result<BTreeSet<String>> {
    let cargo_toml = source_dir.join("Cargo.toml");
    let content = fs::read_to_string(&cargo_toml)
        .await
        .with_context(|| format!("reading {}", cargo_toml.display()))?;
    stdlib_packages_in_cargo_toml(&content)
}

/// Parse the shell `[features]` table into forward edges: each feature mapped to
/// the package features it directly enables. Only bare feature references are
/// edges; `dep:` activations and `crate/feat` / `crate?/feat` entries are
/// ignored. The dependency closure of a package is the transitive reachability
/// over these edges — this mirrors the closure the shell's feature graph
/// compiles, so it is the single source of truth for "what depends on what".
fn feature_edges(content: &str) -> Result<BTreeMap<String, BTreeSet<String>>> {
    use toml_edit::DocumentMut;
    let doc: DocumentMut = content.parse().context("parsing shell Cargo.toml")?;
    let mut edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let Some(features) = doc.get("features").and_then(|f| f.as_table()) else {
        return Ok(edges);
    };
    for (feat, val) in features.iter() {
        let Some(arr) = val.as_array() else { continue };
        let deps = arr
            .iter()
            .filter_map(|v| v.as_str())
            .filter(|s| !s.contains(':') && !s.contains('/'))
            .map(|s| s.to_string())
            .collect();
        edges.insert(feat.to_string(), deps);
    }
    Ok(edges)
}

/// True if enabling `pkg`'s feature transitively enables `target`'s (i.e. `pkg`
/// depends on `target`).
fn feature_depends_on(
    pkg: &str,
    target: &str,
    edges: &BTreeMap<String, BTreeSet<String>>,
) -> bool {
    let mut stack = vec![pkg.to_string()];
    let mut seen = BTreeSet::new();
    while let Some(cur) = stack.pop() {
        if !seen.insert(cur.clone()) {
            continue;
        }
        if let Some(deps) = edges.get(&cur) {
            if deps.contains(target) {
                return true;
            }
            stack.extend(deps.iter().cloned());
        }
    }
    false
}

/// The installed stdlib packages that transitively depend on `target` (so
/// removing `target` would leave them broken, or — because the feature graph
/// still pulls `target` in — silently un-removed).
fn installed_dependents(
    target: &str,
    installed: &BTreeSet<String>,
    edges: &BTreeMap<String, BTreeSet<String>>,
) -> Vec<String> {
    installed
        .iter()
        .filter(|p| p.as_str() != target && feature_depends_on(p, target, edges))
        .cloned()
        .collect()
}

/// True if version `a` is strictly newer than `b` by semver. Falls back to
/// string inequality if either fails to parse (a degraded "they differ" test).
fn version_gt(a: &str, b: &str) -> bool {
    match (semver::Version::parse(a), semver::Version::parse(b)) {
        (Ok(a), Ok(b)) => a > b,
        _ => a != b,
    }
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
async fn extract_local_source(cargo: &Path, version: &str) -> Result<PathBuf> {
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
    let cargo_root = cargo
        .parent()
        .ok_or_else(|| anyhow!("can't find cargo root"))?
        .parent()
        .ok_or_else(|| anyhow!("can't find cargo root"))?;
    let cargo_src = cargo_root.join("registry").join("src");
    match fs::metadata(&cargo_src).await {
        Ok(md) if md.is_dir() => (),
        Err(_) | Ok(_) => bail!("can't find cargo cache {cargo_src:?}"),
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
async fn download_source(
    crates_io: &AsyncClient,
    graphix_data_dir: &Path,
    version: &str,
) -> Result<PathBuf> {
    let package = format!("graphix-shell-{version}");
    let graphix_build_dir = graphix_data_dir.join("build");
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
    let crate_data_tar_gz =
        reqwest::get(Url::parse("https://crates.io")?.join(&cr_version.dl_path)?)
            .await?
            .bytes()
            .await?;
    let r = task::spawn_blocking({
        let graphix_build_dir = graphix_build_dir.clone();
        let cargo_toml = graphix_dir.join("Cargo.toml");
        move || -> Result<()> {
            use std::io::Read;
            let mut crate_data_tar = vec![];
            MultiGzDecoder::new(&crate_data_tar_gz[..])
                .read_to_end(&mut crate_data_tar)?;
            tar::Archive::new(&mut &crate_data_tar[..]).unpack(&graphix_build_dir)?;
            if !std::fs::exists(&cargo_toml)? {
                bail!("package missing Cargo.toml")
            }
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

/// The set of changes `update` discovered as available.
#[derive(Debug, Clone, PartialEq, Eq)]
struct UpdatePlan {
    /// `Some((current, latest))` iff a newer shell version is available.
    shell: Option<(String, String)>,
    /// Newly-shipped stdlib packages not yet seen (installed or removed).
    new_stdlib: Vec<String>,
    /// `(name, old, new)` for each external package with a newer version.
    external_updates: Vec<(String, String, String)>,
}

impl UpdatePlan {
    fn is_empty(&self) -> bool {
        self.shell.is_none()
            && self.new_stdlib.is_empty()
            && self.external_updates.is_empty()
    }
}

/// Which items of an `UpdatePlan` the user chose to apply.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Selection {
    shell: bool,
    new_stdlib: BTreeSet<String>,
    external: BTreeSet<String>,
}

impl Selection {
    /// The "apply everything" selection.
    fn all(plan: &UpdatePlan) -> Self {
        Self {
            shell: plan.shell.is_some(),
            new_stdlib: plan.new_stdlib.iter().cloned().collect(),
            external: plan.external_updates.iter().map(|(n, _, _)| n.clone()).collect(),
        }
    }
}

/// Compute the available changes from the current state. Pure.
fn compute_update_plan(
    current_shell: &str,
    latest_shell: &str,
    stdlib_set_from_source: &BTreeSet<String>,
    packages: &Packages,
    external_latest: &BTreeMap<String, String>,
) -> UpdatePlan {
    let shell = if version_gt(latest_shell, current_shell) {
        Some((current_shell.to_string(), latest_shell.to_string()))
    } else {
        None
    };
    // BTreeSet iterates sorted, so new_stdlib comes out sorted.
    let new_stdlib = stdlib_set_from_source
        .iter()
        .filter(|n| {
            !packages.stdlib_installed.contains(*n)
                && !packages.stdlib_removed.contains(*n)
                && !INTERNAL_PACKAGES.contains(&n.as_str())
        })
        .cloned()
        .collect();
    let mut external_updates = Vec::new();
    for (name, entry) in &packages.external {
        if let PackageEntry::Version(cur) = entry {
            if let Some(latest) = external_latest.get(name) {
                if version_gt(latest, cur) {
                    external_updates.push((name.clone(), cur.clone(), latest.clone()));
                }
            }
        }
    }
    UpdatePlan { shell, new_stdlib, external_updates }
}

/// Apply a selection to `packages`, returning the shell version to build
/// against. Pure. New stdlib packages can only be built against the latest
/// shell source, so they are only acted on when the build version is the
/// latest; a deselected new package is then explicitly recorded in `removed`.
/// When the shell bump is declined the new packages are left untracked so they
/// resurface once the user takes the bump.
fn apply_selection(
    current_shell: &str,
    latest_shell: &str,
    plan: &UpdatePlan,
    sel: &Selection,
    packages: &mut Packages,
) -> String {
    let build_version = if plan.shell.is_some() && sel.shell {
        latest_shell.to_string()
    } else {
        current_shell.to_string()
    };
    if build_version == latest_shell {
        for name in &plan.new_stdlib {
            if sel.new_stdlib.contains(name) {
                packages.stdlib_removed.remove(name);
                packages.stdlib_installed.insert(name.clone());
            } else {
                packages.stdlib_installed.remove(name);
                packages.stdlib_removed.insert(name.clone());
            }
        }
    }
    for (name, _old, new) in &plan.external_updates {
        if sel.external.contains(name) {
            packages.external.insert(name.clone(), PackageEntry::Version(new.clone()));
        }
    }
    packages.enforce_invariants();
    build_version
}

/// A single presentable/selectable line of an `UpdatePlan`.
enum Item {
    Shell { current: String, latest: String },
    NewStdlib(String),
    External { name: String, old: String, new: String },
}

impl Item {
    fn section(&self) -> &'static str {
        match self {
            Item::Shell { .. } => "Shell",
            Item::NewStdlib(_) => "New stdlib packages (require the shell update)",
            Item::External { .. } => "External package updates",
        }
    }

    fn render(&self) -> String {
        match self {
            Item::Shell { current, latest } => {
                format!("graphix-shell   {current} -> {latest}")
            }
            Item::NewStdlib(n) => n.clone(),
            Item::External { name, old, new } => format!("{name}   {old} -> {new}"),
        }
    }
}

/// Flatten a plan into ordered items (shell, then new stdlib, then external).
/// The 1-based position is the stable index used by the edit prompt.
fn plan_items(plan: &UpdatePlan) -> Vec<Item> {
    let mut items = Vec::new();
    if let Some((current, latest)) = &plan.shell {
        items.push(Item::Shell { current: current.clone(), latest: latest.clone() });
    }
    for n in &plan.new_stdlib {
        items.push(Item::NewStdlib(n.clone()));
    }
    for (name, old, new) in &plan.external_updates {
        items.push(Item::External {
            name: name.clone(),
            old: old.clone(),
            new: new.clone(),
        });
    }
    items
}

/// Render items grouped by section. With `selected`, draw checkboxes for edit
/// mode; without, draw plain `[n]` indices for the summary.
fn render_items(items: &[Item], selected: Option<&BTreeSet<usize>>) {
    let mut last_section = "";
    for (i, item) in items.iter().enumerate() {
        let sec = item.section();
        if sec != last_section {
            println!("  {sec}");
            last_section = sec;
        }
        let num = i + 1;
        match selected {
            None => println!("    [{num}]  {}", item.render()),
            Some(sel) => {
                let mark = if sel.contains(&i) { "x" } else { " " };
                println!("    [{mark}] {num}  {}", item.render());
            }
        }
    }
}

fn present(items: &[Item]) {
    println!("\ngraphix update — available changes:\n");
    render_items(items, None);
    println!();
}

fn flush_stdout() {
    use std::io::Write;
    let _ = std::io::stdout().flush();
}

/// Read one line from stdin on the blocking pool. `Ok(None)` is EOF.
async fn prompt_line() -> Result<Option<String>> {
    task::spawn_blocking(|| -> Result<Option<String>> {
        let mut s = String::new();
        let n = std::io::stdin().read_line(&mut s)?;
        Ok(if n == 0 { None } else { Some(s) })
    })
    .await?
}

/// Prompt for a yes/no confirmation, defaulting to no. Returns `false` without
/// prompting when stdin is not a terminal, so a scripted/CI removal never
/// cascades silently.
async fn confirm_yn(prompt: &str) -> Result<bool> {
    if !std::io::stdin().is_terminal() {
        return Ok(false);
    }
    print!("{prompt} [y/N] ");
    flush_stdout();
    match prompt_line().await? {
        Some(line) => {
            Ok(matches!(line.trim().to_ascii_lowercase().as_str(), "y" | "yes"))
        }
        None => Ok(false),
    }
}

/// Parse space/comma-separated 1-based item numbers into 0-based indices,
/// announcing and skipping anything invalid or out of range.
fn parse_toggles(line: &str, n_items: usize) -> Vec<usize> {
    let mut out = Vec::new();
    for tok in
        line.split(|c: char| c.is_whitespace() || c == ',').filter(|t| !t.is_empty())
    {
        match tok.parse::<usize>() {
            Ok(num) if num >= 1 && num <= n_items => out.push(num - 1),
            _ => println!("ignoring invalid entry '{tok}'"),
        }
    }
    out
}

/// The result of asking the user what to do with a plan.
enum Outcome {
    Cancel,
    Apply(Selection),
}

/// Build a `Selection` from the set of selected item indices.
fn selection_from_indices(items: &[Item], selected: &BTreeSet<usize>) -> Selection {
    let mut sel = Selection::default();
    for (i, item) in items.iter().enumerate() {
        let on = selected.contains(&i);
        match item {
            Item::Shell { .. } => sel.shell = on,
            Item::NewStdlib(n) => {
                if on {
                    sel.new_stdlib.insert(n.clone());
                }
            }
            Item::External { name, .. } => {
                if on {
                    sel.external.insert(name.clone());
                }
            }
        }
    }
    sel
}

/// The interactive `[Y/e/n]` confirmation (the plan is already presented).
async fn prompt_y_e_n(items: &[Item]) -> Result<Outcome> {
    loop {
        print!(
            "Apply all changes? [Y/e/n]  (Y = apply all, e = edit selection, n = cancel) "
        );
        flush_stdout();
        match prompt_line().await? {
            None => return Ok(Outcome::Cancel),
            Some(line) => match line.trim().to_ascii_lowercase().as_str() {
                "" | "y" | "yes" => {
                    let all: BTreeSet<usize> = (0..items.len()).collect();
                    return Ok(Outcome::Apply(selection_from_indices(items, &all)));
                }
                "n" | "no" => return Ok(Outcome::Cancel),
                "e" | "edit" => return edit_selection(items).await,
                other => println!("please answer y, e, or n (got '{other}')"),
            },
        }
    }
}

/// If the shell item is deselected, force every new-stdlib item off (they can't
/// build against the current shell). Returns the names that were forced off, so
/// the caller can note them. Pure.
fn normalize_selection(items: &[Item], selected: &mut BTreeSet<usize>) -> Vec<String> {
    let Some(si) = items.iter().position(|it| matches!(it, Item::Shell { .. })) else {
        return Vec::new();
    };
    if selected.contains(&si) {
        return Vec::new();
    }
    let mut off = Vec::new();
    for (i, item) in items.iter().enumerate() {
        if let Item::NewStdlib(n) = item {
            if selected.remove(&i) {
                off.push(n.clone());
            }
        }
    }
    off
}

/// The numbered toggle list editor. Everything starts selected; numbers toggle;
/// a blank line confirms. Deselecting the shell forces every new-stdlib item off
/// (they can't be built against the current shell).
async fn edit_selection(items: &[Item]) -> Result<Outcome> {
    let mut selected: BTreeSet<usize> = (0..items.len()).collect();
    loop {
        println!(
            "\nEdit selection — type item numbers to toggle (space/comma separated),"
        );
        println!("blank line to confirm.\n");
        render_items(items, Some(&selected));
        print!("toggle> ");
        flush_stdout();
        match prompt_line().await? {
            None => {
                println!("input closed; aborting, no changes made.");
                return Ok(Outcome::Cancel);
            }
            Some(line) => {
                let line = line.trim();
                if line.is_empty() {
                    let sel = selection_from_indices(items, &selected);
                    if !sel.shell && sel.new_stdlib.is_empty() && sel.external.is_empty()
                    {
                        println!("no changes selected.");
                        return Ok(Outcome::Cancel);
                    }
                    return Ok(Outcome::Apply(sel));
                }
                for idx in parse_toggles(line, items.len()) {
                    if !selected.remove(&idx) {
                        selected.insert(idx);
                    }
                }
                let off = normalize_selection(items, &mut selected);
                if !off.is_empty() {
                    println!(
                        "note: declining the shell update — new stdlib packages \
                         ({}) were deselected",
                        off.join(", ")
                    );
                }
            }
        }
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
    async fn unpack_source(&self, version: &str) -> Result<PathBuf> {
        let graphix_data_dir = graphix_data_dir()?;
        match extract_local_source(&self.cargo, version).await {
            Ok(p) => Ok(p),
            Err(local) => {
                match download_source(&self.cratesio, &graphix_data_dir, version).await {
                    Ok(p) => Ok(p),
                    Err(dl) => {
                        bail!("could not find our source local: {local}, dl: {dl}")
                    }
                }
            }
        }
    }

    /// Update Cargo.toml to include package dependencies
    /// Add the external packages to the shell's `[dependencies]`. Only external
    /// (third-party) packages are managed here — the stdlib packages are
    /// permanent optional dependencies of the shell, selected at build time by
    /// Cargo feature, and are never touched, nor is the `[features]` table. The
    /// source tree is freshly unpacked before each build (so it contains only
    /// the stdlib deps), and this adds the externals on top.
    fn update_cargo_toml(
        &self,
        cargo_toml_content: &str,
        external: &BTreeMap<String, PackageEntry>,
    ) -> Result<String> {
        use toml_edit::DocumentMut;
        let mut doc: DocumentMut =
            cargo_toml_content.parse().context("parsing Cargo.toml")?;
        let deps = doc["dependencies"]
            .as_table_mut()
            .ok_or_else(|| anyhow!("Cargo.toml missing [dependencies]"))?;
        for (name, entry) in external {
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

    /// Delete the scratch build dir and unpack a fresh graphix-shell source
    /// tree for `version`.
    async fn prepare_source(&self, version: &str) -> Result<PathBuf> {
        println!("Unpacking graphix-shell source...");
        let build_dir = graphix_data_dir()?.join("build");
        if fs::metadata(&build_dir).await.is_ok() {
            fs::remove_dir_all(&build_dir).await?;
        }
        self.unpack_source(version).await
    }

    /// Write deps.rs + Cargo.toml for the given package set into an already
    /// unpacked source tree, back up the current binary, then build & install.
    async fn install_from_source(
        &self,
        source_dir: &Path,
        plan: &BuildPlan,
    ) -> Result<()> {
        // Add external package deps to Cargo.toml (the permanent stdlib deps and
        // the [features] table are untouched). The shell's `packages!()` macro
        // reads this Cargo.toml at compile time, so registration needs no
        // generated source — adding the dep is enough.
        println!("Updating Cargo.toml...");
        let cargo_toml_path = source_dir.join("Cargo.toml");
        let cargo_toml_content = fs::read_to_string(&cargo_toml_path).await?;
        let updated_cargo_toml =
            self.update_cargo_toml(&cargo_toml_content, &plan.external)?;
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
        // Build and install. Stdlib packages are selected by Cargo feature, so a
        // removed package is simply absent from the feature list — no source
        // edits. `core` is non-optional and always compiled.
        println!("Building graphix with updated packages (this may take a while)...");
        let mut cmd = Command::new(&self.cargo);
        cmd.arg("install")
            .arg("--path")
            .arg(source_dir)
            .arg("--force")
            .arg("--no-default-features");
        if !plan.features.is_empty() {
            cmd.arg("--features").arg(plan.features.join(" "));
        }
        let status = cmd.status().await.context("running cargo install")?;
        if !status.success() {
            bail!("cargo install failed with status {status}")
        }
        // Clean up old previous binaries (>1 week)
        self.cleanup_old_binaries().await;
        println!("Done! Restart graphix to use the updated packages.");
        Ok(())
    }

    /// Rebuild the graphix binary with the given package set
    async fn rebuild(&self, plan: &BuildPlan, version: &str) -> Result<()> {
        let source_dir = self.prepare_source(version).await?;
        self.install_from_source(&source_dir, plan).await
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
            let name = pkg.name();
            if is_stdlib_package(name) {
                if pkg.path().is_some() || pkg.version().is_some() {
                    bail!(
                        "stdlib package '{name}' tracks the shell version and cannot \
                         be pinned to a version or path; for local development build \
                         from the workspace"
                    )
                }
                println!("Adding stdlib package {name}");
                installed.stdlib_removed.remove(name);
                installed.stdlib_installed.insert(name.to_string());
                changed = true;
                continue;
            }
            let entry = if let Some(path) = pkg.path() {
                let path = path
                    .canonicalize()
                    .with_context(|| format!("resolving path {}", path.display()))?;
                let version = Self::read_package_version(&path).await?;
                println!("Adding {name} @ path {} (version {version})", path.display());
                PackageEntry::Path(path)
            } else if skip_crates_io_check {
                match pkg.version() {
                    Some(v) => {
                        println!("Adding {name}@{v}");
                        PackageEntry::Version(v.to_string())
                    }
                    None => bail!(
                        "version is required for {name} when using --skip-crates-io-check"
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
                println!("Adding {name}@{version}");
                PackageEntry::Version(version)
            };
            installed.external.insert(name.to_string(), entry);
            changed = true;
        }
        if changed {
            let version = graphix_version().await?;
            self.rebuild(&installed.build_plan(), &version).await?;
            write_packages(&installed).await?;
        } else {
            println!("No changes needed.");
        }
        Ok(())
    }

    /// Remove packages and rebuild. Removing a stdlib package that other
    /// installed packages depend on cascades (with confirmation) to those
    /// dependents — otherwise the feature graph would silently keep the
    /// "removed" package compiled in, contradicting the recorded state.
    pub async fn remove_packages(&self, packages: &[PackageId]) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = lock.write().context("waiting for package lock")?;
        let mut installed = read_packages().await?;
        let version = graphix_version().await?;
        // Removing a stdlib package needs the shell's feature graph to find its
        // dependents; unpack the source once and reuse it for the build.
        // External-only removals don't need it (`rebuild` unpacks at the end).
        let needs_edges =
            packages.iter().any(|p| p.name() != "core" && is_stdlib_package(p.name()));
        let prepared = if needs_edges {
            let source = self.prepare_source(&version).await?;
            let cargo = fs::read_to_string(source.join("Cargo.toml")).await?;
            let edges = feature_edges(&cargo)?;
            Some((source, edges))
        } else {
            None
        };
        let mut changed = false;
        for pkg in packages {
            let name = pkg.name();
            if name == "core" {
                eprintln!("Cannot remove the core package");
                continue;
            }
            if is_stdlib_package(name) {
                if !installed.stdlib_installed.contains(name) {
                    println!("{name} is already removed");
                    continue;
                }
                let edges = prepared
                    .as_ref()
                    .map(|(_, e)| e)
                    .expect("edges are unpacked when a stdlib package is removed");
                let dependents =
                    installed_dependents(name, &installed.stdlib_installed, edges);
                if !dependents.is_empty() {
                    println!(
                        "Removing {name} also removes packages that depend on it: {}",
                        dependents.join(", ")
                    );
                    if !confirm_yn("Remove them too?").await? {
                        println!("Skipping {name} (its dependents are still installed)");
                        continue;
                    }
                    for d in &dependents {
                        installed.stdlib_installed.remove(d);
                        installed.stdlib_removed.insert(d.clone());
                        println!("Removing stdlib package {d}");
                    }
                }
                installed.stdlib_installed.remove(name);
                installed.stdlib_removed.insert(name.to_string());
                println!("Removing stdlib package {name}");
                changed = true;
            } else if installed.external.remove(name).is_some() {
                println!("Removing {name}");
                changed = true;
            } else {
                println!("{name} is not installed");
            }
        }
        if changed {
            installed.enforce_invariants();
            let plan = installed.build_plan();
            match &prepared {
                Some((source, _)) => self.install_from_source(source, &plan).await?,
                None => self.rebuild(&plan, &version).await?,
            }
            write_packages(&installed).await?;
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
        let packages = read_packages().await?;
        let version = graphix_version().await?;
        self.rebuild(&packages.build_plan(), &version).await
    }

    /// List installed packages
    pub async fn list(&self) -> Result<()> {
        let packages = read_packages().await?;
        if packages.stdlib_installed.is_empty() && packages.external.is_empty() {
            println!("No packages installed");
            return Ok(());
        }
        if !packages.stdlib_installed.is_empty() {
            println!("stdlib packages:");
            for name in &packages.stdlib_installed {
                println!("  {name}");
            }
        }
        if !packages.stdlib_removed.is_empty() {
            println!("removed stdlib packages:");
            for name in &packages.stdlib_removed {
                println!("  {name}");
            }
        }
        if !packages.external.is_empty() {
            println!("external packages:");
            for (name, entry) in &packages.external {
                println!("  {name}: {entry}");
            }
        }
        Ok(())
    }

    /// Build a standalone graphix binary from a local package directory.
    ///
    /// The binary is placed in `package_dir/graphix`. Only the local
    /// package is included directly — cargo resolves its transitive
    /// dependencies (including stdlib packages) normally.
    pub async fn build_standalone(
        &self,
        package_dir: &Path,
        source_override: Option<&Path>,
    ) -> Result<()> {
        let package_dir = package_dir
            .canonicalize()
            .with_context(|| format!("resolving {}", package_dir.display()))?;
        // Read the package name from Cargo.toml
        let cargo_toml_path = package_dir.join("Cargo.toml");
        let contents = fs::read_to_string(&cargo_toml_path)
            .await
            .with_context(|| format!("reading {}", cargo_toml_path.display()))?;
        let doc: toml::Value =
            toml::from_str(&contents).context("parsing package Cargo.toml")?;
        let crate_name = doc
            .get("package")
            .and_then(|p| p.get("name"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow!("no package name in {}", cargo_toml_path.display()))?;
        let short_name =
            crate_name.strip_prefix("graphix-package-").ok_or_else(|| {
                anyhow!("package name must start with graphix-package-, got {crate_name}")
            })?;
        // The embedded package is registered as an external (path) package, and
        // the binary auto-runs its standalone main program. Enable the stdlib
        // features the embedded package directly depends on — the shell feature
        // graph pulls their transitive closure; `core` is always compiled.
        let mut external = BTreeMap::new();
        external.insert(short_name.to_string(), PackageEntry::Path(package_dir.clone()));
        let features: Vec<String> = stdlib_packages_in_cargo_toml(&contents)?
            .into_iter()
            .filter(|n| n != "core")
            .collect();
        let mut lock_storage =
            if source_override.is_none() { Some(Self::lock_file()?) } else { None };
        let _guard = lock_storage
            .as_mut()
            .map(|l| l.write().context("waiting for package lock"))
            .transpose()?;
        let source_dir = if let Some(dir) = source_override {
            dir.to_path_buf()
        } else {
            println!("Unpacking graphix-shell source...");
            let build_dir = graphix_data_dir()?.join("build");
            if fs::metadata(&build_dir).await.is_ok() {
                fs::remove_dir_all(&build_dir).await?;
            }
            self.unpack_source(&graphix_version().await?).await?
        };
        // Add the embedded package as a dep; the shell's `packages!()` macro
        // discovers it (and its `main_program`) from this Cargo.toml.
        println!("Updating Cargo.toml...");
        let shell_cargo_toml_path = source_dir.join("Cargo.toml");
        let shell_cargo_toml = fs::read_to_string(&shell_cargo_toml_path).await?;
        let updated = self.update_cargo_toml(&shell_cargo_toml, &external)?;
        fs::write(&shell_cargo_toml_path, &updated).await?;
        println!("Building standalone binary (this may take a while)...");
        // Pin the target dir under the source tree so we know exactly where
        // the binary lands. A user's global build.target-dir (in
        // ~/.cargo/config.toml) or CARGO_TARGET_DIR would otherwise redirect
        // the output out from under the copy below, leaving us unable to find
        // the binary we just built.
        let target_dir = source_dir.join("target");
        let status = Command::new(&self.cargo)
            .arg("build")
            .arg("--release")
            .arg("--target-dir")
            .arg(&target_dir)
            .arg("--no-default-features")
            .arg("--features")
            .arg({
                let mut f = features.clone();
                f.push(format!("{crate_name}/standalone"));
                f.join(" ")
            })
            .current_dir(&source_dir)
            .status()
            .await
            .context("running cargo build")?;
        if !status.success() {
            bail!("cargo build --release failed with status {status}")
        }
        let bin_name = format!("{short_name}{}", std::env::consts::EXE_SUFFIX);
        let built = target_dir
            .join("release")
            .join(format!("graphix{}", std::env::consts::EXE_SUFFIX));
        let dest = package_dir.join(&bin_name);
        fs::copy(&built, &dest).await.with_context(|| {
            format!("copying {} to {}", built.display(), dest.display())
        })?;
        println!("Done! Binary written to {}", dest.display());
        Ok(())
    }

    /// Query crates.io for the latest version of a crate
    async fn latest_version(&self, crate_name: &str) -> Result<String> {
        let cr = self
            .cratesio
            .get_crate(crate_name)
            .await
            .with_context(|| format!("querying crates.io for {crate_name}"))?;
        Ok(cr.crate_data.max_version)
    }

    /// Update graphix and its packages: discover the available changes (shell
    /// bump, newly-shipped stdlib packages, external package updates), confirm
    /// with the user (unless `assume_yes`), then rebuild against the chosen
    /// shell version with the chosen package set.
    pub async fn update(&self, assume_yes: bool) -> Result<()> {
        let mut lock = Self::lock_file()?;
        // Probe the lock so we can tell the user we're waiting; the probe guard
        // is dropped immediately, then we (re)acquire and hold for the op.
        if lock.try_write().is_err() {
            println!("waiting for another graphix package operation to finish...");
        }
        let _guard = lock.write().context("waiting for package lock")?;
        let current = graphix_version().await?;
        let latest = self
            .latest_version("graphix-shell")
            .await
            .context("checking crates.io for the latest graphix-shell")?;
        let mut packages = read_packages().await?;
        // New stdlib packages only ship with a shell bump, so only unpack the
        // latest source to enumerate them when a bump is available.
        let shell_bump = version_gt(&latest, &current);
        let (latest_src, stdlib_latest) = if shell_bump {
            let s = self.prepare_source(&latest).await?;
            let set = stdlib_packages_in_source(&s).await?;
            (Some(s), set)
        } else {
            (None, BTreeSet::new())
        };
        // Check each external (crates.io versioned) package for an update. A
        // single failing crate must not abort the whole update.
        let mut external_latest = BTreeMap::new();
        for (name, entry) in &packages.external {
            if let PackageEntry::Version(_) = entry {
                let crate_name = format!("graphix-package-{name}");
                match self.latest_version(&crate_name).await {
                    Ok(v) => {
                        external_latest.insert(name.clone(), v);
                    }
                    Err(e) => {
                        eprintln!("warning: couldn't check updates for {name}: {e}")
                    }
                }
            }
        }
        let plan = compute_update_plan(
            &current,
            &latest,
            &stdlib_latest,
            &packages,
            &external_latest,
        );
        if plan.is_empty() {
            println!("graphix is already up to date (version {current})");
            return Ok(());
        }
        let items = plan_items(&plan);
        present(&items);
        let outcome = if assume_yes {
            println!("applying all changes (--yes)");
            Outcome::Apply(Selection::all(&plan))
        } else if !std::io::stdin().is_terminal() {
            bail!(
                "stdin is not a terminal; re-run with --yes to apply all available changes"
            )
        } else {
            prompt_y_e_n(&items).await?
        };
        let sel = match outcome {
            Outcome::Cancel => {
                println!("cancelled.");
                return Ok(());
            }
            Outcome::Apply(sel) => sel,
        };
        let build_version =
            apply_selection(&current, &latest, &plan, &sel, &mut packages);
        // Reuse the latest source if we're building against it; otherwise the
        // shell bump was declined and we must build against the current source.
        let (build_src, stdlib_build) = match latest_src {
            Some(s) if build_version == latest => (s, stdlib_latest),
            _ => {
                let s = self.prepare_source(&build_version).await?;
                let set = stdlib_packages_in_source(&s).await?;
                (s, set)
            }
        };
        // Don't try to build an installed stdlib package that doesn't exist at
        // the build version (e.g. removed upstream); keep the recorded intent.
        let mut effective = packages.clone();
        effective.stdlib_installed.retain(|n| n == "core" || stdlib_build.contains(n));
        // Build before writing: a failed cargo install leaves packages.toml
        // untouched and the user re-runs to the same choices.
        self.install_from_source(&build_src, &effective.build_plan()).await?;
        write_packages(&packages).await?;
        Ok(())
    }
}
