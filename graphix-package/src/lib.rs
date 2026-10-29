#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use chrono::Local;
use compact_str::{CompactString, ToCompactString, format_compact};
use crates_io_api::AsyncClient;
use flate2::bufread::MultiGzDecoder;
use graphix_compiler::{
    ExecState,
    env::Env,
    expr::{ExprId, ResolverRef, VfsEntry, VfsResolver},
};
/// `packages!()` builds `Vec<Box<dyn Package<X>>>` (owned, feature-gated) and
/// `package_refs!()` builds `&'static [&'static dyn Package<NoExt>]` (const),
/// both by scraping the calling crate's `Cargo.toml` for `graphix-package-*`
/// deps. Re-exported here so callers use `graphix_package::packages!()`.
pub use graphix_derive::{package_refs, packages};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use handlebars::Handlebars;
pub use indexmap::IndexSet;

/// What `defpackage!` expands to names, so a package depends on these
/// only through this crate.
#[doc(hidden)]
pub mod __private {
    pub use {ahash, anyhow, arcstr, graphix_compiler, graphix_rt, netidx_core, tokio};
}
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
use toml_edit::DocumentMut;
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

/// Whether `e` settled to the display type the named typedef at `path`
/// describes, wrapped by `wrap` (`|t| t` for the type itself). The type
/// is built for each question: its reference resolves in `env`, the
/// runtime asking. A probe decides nothing about `e`'s type: a type
/// still holding unbound variables is not a display.
pub fn shows_as<X: GXExt>(
    env: &Env,
    e: &CompExp<X>,
    path: &[&str],
    wrap: impl FnOnce(graphix_compiler::typ::Type) -> graphix_compiler::typ::Type,
) -> bool {
    use graphix_compiler::{
        expr::ModPath,
        typ::{Type, TypeRef},
    };
    let Some(typ) = e.typ.with_deref(|t| t.cloned()) else { return false };
    // ⊥ fits any position, so a type holding one anywhere (`Array<⊥>`
    // from an empty array) would be contained by every display
    if typ.has_bottom() || typ == Type::Any || typ.has_unbound() {
        return false;
    }
    let display = wrap(Type::Ref(triomphe::Arc::new(TypeRef::synthetic(
        ModPath::root(),
        ModPath::from_iter(path.iter().copied()),
        triomphe::Arc::from_iter([]),
    ))));
    display.contains_with_flags(enumflags2::BitFlags::empty(), env, &typ).unwrap_or(false)
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

    /// The updates of one runtime cycle have all been processed: a
    /// display that redraws shows the cycle whole here.
    async fn batch_done(&mut self);

    /// The runtime went idle after the display asked
    /// (`GXHandle::notify_idle`), every update before it processed.
    async fn idle(&mut self);
}

/// Why a custom display ended without failing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ended {
    /// The user interrupted it (Ctrl-C): a script exits as interrupted.
    Interrupted,
    /// The program asked to end (`tui::exit`), or its last window closed:
    /// a script exits successfully.
    Requested,
}

/// How a custom display tells the shell it is done: why, or the error
/// when the display cannot go on. A display that drops it unsent has
/// died, which the shell reports too.
pub type Stop = oneshot::Sender<Result<Ended>>;

/// A live custom display plus the channel the shell watches for it to stop.
pub struct Cdc<X: GXExt> {
    pub stop: oneshot::Receiver<Result<Ended>>,
    pub custom: Box<dyn CustomDisplay<X>>,
}

/// The result of `Package::maybe_init_custom`: the package either claimed the
/// value and built a custom display (`Custom`), or passed it through
/// (`NotCustom`, returning the value for the next package / normal display).
pub enum CustomResult<X: GXExt> {
    Custom(Cdc<X>),
    NotCustom(CompExp<X>),
}

/// Trait implemented by Graphix packages, normally via `defpackage!`.
/// Object-safe so packages can be collected as `Vec<Box<dyn Package<X>>>`
/// or `&[&dyn Package<X>]` and driven uniformly.
pub trait Package<X: GXExt>: Send + Sync {
    /// Register this package's builtins and Graphix modules: modules by path in
    /// `modules`, the package itself by name in `root_mods`. Registration is
    /// idempotent (guarded by `root_mods`) and transitively registers the
    /// package's dependency packages.
    fn register(
        &self,
        ctx: &mut ExecState<GXRt<X>>,
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

    /// Run as the process ends, to hand off what only a drop hands off
    /// (statics are never dropped).
    fn shutdown(&self) {}

    /// Told, once the shell has chosen what shows a program's output (a
    /// package's display, text, or nothing), whether this package's
    /// display does.
    fn output_chosen<'a>(
        &'a self,
        _gx: &'a GXHandle<X>,
        _mine: bool,
    ) -> Pin<Box<dyn Future<Output = ()> + 'a>> {
        Box::pin(async {})
    }
}

/// Register `packages` in `ctx`: the resolver of their Graphix modules,
/// and the root module source naming each (`mod <name>`; core needs no
/// `use`, the compiler's core prelude makes its root items visible
/// everywhere).
pub fn register_packages<'a, X: GXExt + 'a>(
    ctx: &mut ExecState<GXRt<X>>,
    packages: impl IntoIterator<Item = &'a dyn Package<X>>,
) -> Result<(ResolverRef, ArcStr)> {
    let mut modules = AHashMap::default();
    let mut root_mods = IndexSet::new();
    for p in packages {
        p.register(ctx, &mut modules, &mut root_mods)?;
    }
    let mut root = String::new();
    for name in &root_mods {
        if !root.is_empty() {
            root.push_str(";\n");
        }
        root.push_str("mod ");
        root.push_str(name);
    }
    Ok((VfsResolver::with_packages(modules, root_mods), ArcStr::from(root)))
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
    if !fs::metadata(base).await.is_ok_and(|m| m.is_dir()) {
        bail!("base path {base:?} does not exist, or is not a directory")
    }
    // the short name becomes the Graphix module name and, `-` read as `_`,
    // the prefix of every builtin's name
    let short = name.strip_prefix("graphix-package-").unwrap_or("");
    let valid = short.starts_with(|c: char| c.is_ascii_lowercase())
        && short
            .chars()
            .all(|c| c == '-' || c.is_ascii_lowercase() || c.is_ascii_digit());
    if !valid {
        bail!("invalid package name, name must match graphix-package-[a-z][-a-z0-9]*")
    }
    if graphix_compiler::expr::parser::is_reserved_binding(&short.replace('-', "_")) {
        bail!("invalid package name, {short} is a reserved word")
    }
    if is_stdlib_package(short) {
        bail!("invalid package name, {short} is a stdlib package")
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
    let params = json!({"name": short, "ident": short.replace('-', "_"), "deps": []});
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
    graphix_rt::data_dir().ok_or_else(|| anyhow!("can't find your data dir"))
}

/// The target directory the builds of the source the package manager
/// unpacks share, outside the build directory each unpack wipes, so a
/// build compiles only what changed.
fn shared_target_dir() -> Result<PathBuf> {
    Ok(graphix_data_dir()?.join("target"))
}

fn packages_toml_path() -> Result<PathBuf> {
    Ok(graphix_data_dir()?.join("packages.toml"))
}

/// The user-facing stdlib packages that seed a fresh packages.toml and
/// the "known stdlib" set when migrating an old-format file. The
/// authoritative set at a shell version comes from that version's
/// Cargo.toml (`stdlib_packages_in_source`); this is only a bootstrap.
const DEFAULT_PACKAGES: &[&str] = &[
    "core", "array", "str", "map", "sys", "http", "json", "toml", "pack", "xls",
    "sqlite", "db", "list", "args", "hbs", "re", "rand", "tui", "gui",
];

/// Old top-level stdlib packages merged into another package. On
/// migration the dead name is dropped and its replacement installed
/// (`fs`/`net`/`time` are submodules of `sys`).
const LEGACY_REMAP: &[(&str, &str)] = &[("fs", "sys"), ("net", "sys"), ("time", "sys")];

/// True if `name` is a stdlib package. Stdlib packages track the shell version and are recorded by name only.
fn is_stdlib_package(name: &str) -> bool {
    DEFAULT_PACKAGES.contains(&name)
}

/// A package entry in packages.toml — either a version string or a path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageEntry {
    Version(CompactString),
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
    stdlib_installed: BTreeSet<CompactString>,
    stdlib_removed: BTreeSet<CompactString>,
    external: BTreeMap<CompactString, PackageEntry>,
}

impl Packages {
    /// `core` is mandatory and can never be removed; a name can't be both
    /// installed and removed (installed wins). Call after every load/mutation.
    fn enforce_invariants(&mut self) {
        self.stdlib_installed.insert("core".to_compact_string());
        let Self { stdlib_installed, stdlib_removed, external: _ } = self;
        stdlib_removed.retain(|n| !stdlib_installed.contains(n));
    }

    /// Derive the inputs to a shell build: the stdlib Cargo feature list
    /// (installed stdlib packages minus `core`, always compiled) and the
    /// external packages (compiled as regular deps).
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

/// The inputs to a shell build: the Cargo features it turns on and the
/// external packages it adds, from a package set ([`Packages::build_plan`])
/// or a standalone package.
#[derive(Debug, Clone, PartialEq, Eq)]
struct BuildPlan {
    features: Vec<CompactString>,
    external: BTreeMap<CompactString, PackageEntry>,
}

/// Parse a single external entry value (`"1.2.3"` or `{ path = "..." }`).
fn parse_entry(k: &str, v: &toml::Value) -> Result<PackageEntry> {
    match v {
        toml::Value::String(s) => Ok(PackageEntry::Version(s.as_str().into())),
        toml::Value::Table(t) => {
            if let Some(p) = t.get("path").and_then(|v| v.as_str()) {
                let path = PathBuf::from(p);
                if !path.is_absolute() {
                    bail!(
                        "package {k}: path {p} is relative; packages.toml takes an \
                         absolute path"
                    )
                }
                Ok(PackageEntry::Path(path))
            } else {
                bail!("package {k}: table entry must have a 'path' key")
            }
        }
        _ => bail!("package {k}: expected a version string or table"),
    }
}

fn parse_external_table(
    tbl: Option<&toml::Value>,
) -> Result<BTreeMap<CompactString, PackageEntry>> {
    let mut external = BTreeMap::new();
    let Some(tbl) = tbl else { return Ok(external) };
    let tbl = tbl.as_table().ok_or_else(|| anyhow!("[packages] must be a table"))?;
    for (k, v) in tbl {
        external.insert(k.as_str().into(), parse_entry(k, v)?);
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
    let read_names = |key: &str| -> Result<BTreeSet<CompactString>> {
        match stdlib.get(key) {
            None => Ok(BTreeSet::new()),
            Some(toml::Value::Array(a)) => a
                .iter()
                .map(|v| {
                    v.as_str().map(|s| s.to_compact_string()).ok_or_else(|| {
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
            stdlib_installed.insert(replacement.to_compact_string());
            continue;
        }
        if is_stdlib_package(k) {
            if let Some(p) = v.get("path").and_then(|p| p.as_str()) {
                eprintln!(
                    "warning: stdlib package '{k}' had a path override ({p}); \
                     stdlib now tracks the shell version, dropping the override"
                );
            }
            stdlib_installed.insert(k.as_str().into());
        } else {
            external.insert(k.as_str().into(), parse_entry(k, v)?);
        }
    }
    // the old file was written from its era's defaults, so a name it lacks
    // is untracked, never removed: reconcile announces it, update offers it
    let mut p = Packages { stdlib_installed, stdlib_removed: BTreeSet::new(), external };
    p.enforce_invariants();
    Ok(p)
}

/// Serialize a `Packages` to the v2 on-disk format.
fn to_toml_string(p: &Packages) -> Result<String> {
    let to_arr = |s: &BTreeSet<CompactString>| -> toml::Value {
        toml::Value::Array(s.iter().map(|n| toml::Value::String(n.to_string())).collect())
    };
    let mut stdlib = toml::value::Table::new();
    stdlib.insert("installed".to_string(), to_arr(&p.stdlib_installed));
    stdlib.insert("removed".to_string(), to_arr(&p.stdlib_removed));
    let mut pkgs = toml::value::Table::new();
    for (k, entry) in &p.external {
        match entry {
            PackageEntry::Version(v) => {
                pkgs.insert(k.to_string(), toml::Value::String(v.to_string()));
            }
            PackageEntry::Path(path) => {
                let mut t = toml::value::Table::new();
                t.insert(
                    "path".to_string(),
                    toml::Value::String(path.to_string_lossy().into_owned()),
                );
                pkgs.insert(k.to_string(), toml::Value::Table(t));
            }
        }
    }
    let mut doc = toml::value::Table::new();
    doc.insert("stdlib".to_string(), toml::Value::Table(stdlib));
    doc.insert("packages".to_string(), toml::Value::Table(pkgs));
    Ok(toml::to_string_pretty(&toml::Value::Table(doc))?)
}

/// The record, migrated from an old format or the defaults when there is
/// none. A migrated record is written back when the package lock is free;
/// a command holding it writes the record itself.
async fn read_packages() -> Result<Packages> {
    let path = packages_toml_path()?;
    match fs::read_to_string(&path).await {
        Ok(contents) => {
            let (p, migrated) = parse_packages(&contents)?;
            if migrated
                && let Ok(mut lock) = GraphixPM::lock_file()
                && let Ok(_guard) = lock.try_write()
            {
                write_packages(&p).await?;
            }
            Ok(p)
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let mut p = Packages::default();
            p.stdlib_installed =
                DEFAULT_PACKAGES.iter().map(|n| n.to_compact_string()).collect();
            p.enforce_invariants();
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
    // a whole file or the old one: written beside it, then renamed over it
    let tmp = path.with_extension("toml.tmp");
    fs::write(&tmp, to_toml_string(p)?).await?;
    fs::rename(&tmp, &path).await?;
    Ok(())
}

/// The shell features a standalone build of the package with manifest
/// `package` turns on: the stdlib packages it depends on that the shell
/// (manifest `shell`) ships, but core, which is always compiled.
fn standalone_features(package: &str, shell: &str) -> Result<Vec<CompactString>> {
    let shipped = stdlib_packages_in_cargo_toml(shell)?;
    Ok(stdlib_packages_in_cargo_toml(package)?
        .into_iter()
        .filter(|n| n != "core" && shipped.contains(n))
        .collect())
}

/// Enumerate the stdlib package short-names from a Cargo.toml's
/// `[dependencies]` — every `graphix-package-<name>` key (the trailing dash
/// excludes the `graphix-package` crate itself; the value shape is ignored, so
/// the `optional` gui entry is included).
fn stdlib_packages_in_cargo_toml(content: &str) -> Result<BTreeSet<CompactString>> {
    let doc: DocumentMut = content.parse().context("parsing shell Cargo.toml")?;
    let deps = doc
        .get("dependencies")
        .and_then(|d| d.as_table())
        .ok_or_else(|| anyhow!("Cargo.toml missing [dependencies]"))?;
    Ok(deps
        .iter()
        .filter_map(|(k, _)| {
            k.strip_prefix("graphix-package-").map(|s| s.to_compact_string())
        })
        .collect())
}

/// The manifest of an unpacked graphix-shell source tree.
async fn source_manifest(source_dir: &Path) -> Result<String> {
    let cargo_toml = source_dir.join("Cargo.toml");
    fs::read_to_string(&cargo_toml)
        .await
        .with_context(|| format!("reading {}", cargo_toml.display()))
}

/// The stdlib set shipped by an unpacked graphix-shell source tree.
async fn stdlib_packages_in_source(source_dir: &Path) -> Result<BTreeSet<CompactString>> {
    stdlib_packages_in_cargo_toml(&source_manifest(source_dir).await?)
}

/// Parse the shell `[features]` table into forward edges: each feature
/// mapped to the package features it directly enables. Only bare feature
/// references are edges; `dep:`, `crate/feat` and `crate?/feat` are ignored.
fn feature_edges(
    content: &str,
) -> Result<BTreeMap<CompactString, BTreeSet<CompactString>>> {
    let doc: DocumentMut = content.parse().context("parsing shell Cargo.toml")?;
    let mut edges: BTreeMap<CompactString, BTreeSet<CompactString>> = BTreeMap::new();
    let Some(features) = doc.get("features").and_then(|f| f.as_table()) else {
        return Ok(edges);
    };
    for (feat, val) in features.iter() {
        let Some(arr) = val.as_array() else { continue };
        let deps = arr
            .iter()
            .filter_map(|v| v.as_str())
            .filter(|s| !s.contains(':') && !s.contains('/'))
            .map(|s| s.to_compact_string())
            .collect();
        edges.insert(feat.to_compact_string(), deps);
    }
    Ok(edges)
}

/// True if enabling `pkg`'s feature transitively enables `target`'s (i.e. `pkg`
/// depends on `target`).
fn feature_depends_on(
    pkg: &str,
    target: &str,
    edges: &BTreeMap<CompactString, BTreeSet<CompactString>>,
) -> bool {
    let mut stack = vec![pkg.to_compact_string()];
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
    installed: &BTreeSet<CompactString>,
    edges: &BTreeMap<CompactString, BTreeSet<CompactString>>,
) -> Vec<CompactString> {
    installed
        .iter()
        .filter(|p| p.as_str() != target && feature_depends_on(p, target, edges))
        .cloned()
        .collect()
}

/// The installed stdlib packages removed with `stdlib` because they depend
/// on one of it, by the feature graph `edges`. Refused when the external
/// packages register (`registered`, [`registered_by_external`]) one of
/// `stdlib`: the build would compile it again, without its dependents.
fn removal_cascade(
    stdlib: &BTreeSet<CompactString>,
    installed: &BTreeSet<CompactString>,
    edges: &BTreeMap<CompactString, BTreeSet<CompactString>>,
    registered: &BTreeSet<CompactString>,
) -> Result<BTreeSet<CompactString>> {
    let needed: Vec<&str> =
        stdlib.iter().filter(|n| registered.contains(*n)).map(|n| n.as_str()).collect();
    if !needed.is_empty() {
        bail!(
            "nothing removed: an installed external package depends on {}",
            needed.join(", ")
        )
    }
    let mut cascade = BTreeSet::new();
    for name in stdlib {
        let dependents = installed_dependents(name, installed, edges);
        cascade.extend(dependents.into_iter().filter(|d| !stdlib.contains(d)));
    }
    Ok(cascade)
}

/// The shell manifest `content` with the external packages added to its
/// `[dependencies]`. Stdlib packages are permanent optional deps selected
/// by Cargo feature; neither they nor `[features]` are touched.
fn with_external(
    content: &str,
    external: &BTreeMap<CompactString, PackageEntry>,
) -> Result<String> {
    let mut doc: DocumentMut = content.parse().context("parsing Cargo.toml")?;
    let deps = doc["dependencies"]
        .as_table_mut()
        .ok_or_else(|| anyhow!("Cargo.toml missing [dependencies]"))?;
    for (name, entry) in external {
        let dep = crate_name(name);
        if deps.contains_key(&dep) {
            bail!("the package {name} is already a dependency of the shell")
        }
        match entry {
            PackageEntry::Version(version) => {
                deps[&dep] = toml_edit::value(version.as_str());
            }
            PackageEntry::Path(path) => {
                let mut tbl = toml_edit::InlineTable::new();
                tbl.insert(
                    "path",
                    toml_edit::Value::from(path.to_string_lossy().as_ref()),
                );
                deps[&dep] = toml_edit::Item::Value(tbl.into());
            }
        }
    }
    Ok(doc.to_string())
}

/// Add the external packages to the shell source's manifest. The shell's
/// `packages!()` macro reads it at compile time, so adding the dep is the
/// whole registration.
async fn rewrite_manifest(
    source_dir: &Path,
    external: &BTreeMap<CompactString, PackageEntry>,
) -> Result<()> {
    let path = source_dir.join("Cargo.toml");
    let content = fs::read_to_string(&path).await?;
    fs::write(&path, with_external(&content, external)?).await?;
    Ok(())
}

/// Bring the record in line with the source about to be built: a recorded
/// stdlib package the source no longer ships is dropped, as is an external
/// package named like one it ships, one it ships that the record never
/// named is announced, and a removed one an installed one depends on is
/// installed again.
async fn reconcile(source_dir: &Path, packages: &mut Packages) -> Result<()> {
    let content = source_manifest(source_dir).await?;
    let shipped = stdlib_packages_in_cargo_toml(&content)?;
    let gone: Vec<CompactString> = packages
        .stdlib_installed
        .iter()
        .filter(|n| *n != "core" && !shipped.contains(*n))
        .cloned()
        .collect();
    for name in gone {
        println!("warning: {name} is no longer part of graphix; dropping it");
        packages.stdlib_installed.remove(&name);
    }
    packages.stdlib_removed.retain(|n| shipped.contains(n));
    let shadowed: Vec<CompactString> =
        packages.external.keys().filter(|n| shipped.contains(*n)).cloned().collect();
    for name in shadowed {
        println!(
            "warning: {name} is now part of graphix; dropping the external package of that name"
        );
        packages.external.remove(&name);
    }
    for name in shipped.iter().filter(|n| {
        DEFAULT_PACKAGES.contains(&n.as_str())
            && !packages.stdlib_installed.contains(*n)
            && !packages.stdlib_removed.contains(*n)
    }) {
        println!(
            "note: stdlib package {name} is not installed; `graphix package add {name}` adds it"
        );
    }
    for name in reinstall_dependencies(packages, &feature_edges(&content)?) {
        println!("note: {name} is installed again: an installed package depends on it");
    }
    Ok(())
}

/// Move every removed stdlib package an installed one depends on back to
/// the installed set, which is what the build compiles: the add-side twin
/// of the removal cascade. The names moved.
fn reinstall_dependencies(
    packages: &mut Packages,
    edges: &BTreeMap<CompactString, BTreeSet<CompactString>>,
) -> Vec<CompactString> {
    reinstall(packages, |installed, r| {
        installed.iter().any(|p| feature_depends_on(p, r, edges))
    })
}

/// Move every removed stdlib package in `registered` (what the external
/// packages register, [`registered_by_external`]) back to the installed
/// set. The names moved.
fn reinstall_registered(
    packages: &mut Packages,
    registered: &BTreeSet<CompactString>,
) -> Vec<CompactString> {
    reinstall(packages, |_, r| registered.contains(r))
}

/// Move every removed stdlib package `needed` says the installed set needs
/// back to it. The names moved.
fn reinstall(
    packages: &mut Packages,
    needed: impl Fn(&BTreeSet<CompactString>, &str) -> bool,
) -> Vec<CompactString> {
    let Packages { stdlib_installed, stdlib_removed, external: _ } = packages;
    let needed: Vec<CompactString> =
        stdlib_removed.iter().filter(|r| needed(stdlib_installed, r)).cloned().collect();
    for name in &needed {
        stdlib_removed.remove(name);
        stdlib_installed.insert(name.clone());
    }
    needed
}

/// The resolve graph of `cargo metadata`'s output, by package id.
struct Resolve<'a> {
    /// Each package's name and version.
    packages: BTreeMap<&'a str, (&'a str, &'a str)>,
    /// Each package's dependencies, of every kind.
    deps: BTreeMap<&'a str, &'a Vec<serde_json::Value>>,
    /// The package cargo resolved for, the shell.
    root: Option<&'a str>,
}

impl<'a> Resolve<'a> {
    fn new(metadata: &'a serde_json::Value) -> Result<Self> {
        let malformed = || anyhow!("malformed cargo metadata");
        let packages = metadata["packages"]
            .as_array()
            .ok_or_else(malformed)?
            .iter()
            .filter_map(|p| {
                Some((
                    p["id"].as_str()?,
                    (p["name"].as_str()?, p["version"].as_str().unwrap_or("")),
                ))
            })
            .collect();
        let resolve = &metadata["resolve"];
        let deps = resolve["nodes"]
            .as_array()
            .ok_or_else(malformed)?
            .iter()
            .filter_map(|n| Some((n["id"].as_str()?, n["deps"].as_array()?)))
            .collect();
        Ok(Self { packages, deps, root: resolve["root"].as_str() })
    }

    fn name(&self, id: &str) -> Option<&'a str> {
        self.packages.get(id).map(|(name, _)| *name)
    }

    /// The ids of the packages named `name`.
    fn named(&self, name: &str) -> impl Iterator<Item = &'a str> {
        self.packages.iter().filter(move |(_, (n, _))| *n == name).map(|(id, _)| *id)
    }

    /// The packages `id` depends on to build, never dev- or build-only.
    fn normal_deps(&self, id: &str) -> impl Iterator<Item = &'a str> {
        self.deps.get(id).into_iter().flat_map(|deps| deps.iter()).filter_map(|dep| {
            let normal = dep["dep_kinds"]
                .as_array()
                .is_some_and(|kinds| kinds.iter().any(|k| k["kind"].is_null()));
            if normal { dep["pkg"].as_str() } else { None }
        })
    }

    /// The packages `from` reaches through normal dependencies, walking
    /// only through those whose name passes `through`.
    fn reach(
        &self,
        from: impl IntoIterator<Item = &'a str>,
        through: impl Fn(&str) -> bool,
    ) -> BTreeSet<&'a str> {
        let mut stack: Vec<&str> = from.into_iter().collect();
        let mut reached = BTreeSet::new();
        while let Some(id) = stack.pop() {
            if self.name(id).is_some_and(|n| through(n)) && reached.insert(id) {
                stack.extend(self.normal_deps(id));
            }
        }
        reached
    }
}

/// The packages the external packages register, by short name: each one's
/// `graphix-package-*` dependencies (never a dev- or build-only one),
/// transitively, in the resolve graph of `metadata` (`cargo metadata`'s
/// output).
fn registered_by_external(
    metadata: &serde_json::Value,
    external: &BTreeMap<CompactString, PackageEntry>,
) -> Result<BTreeSet<CompactString>> {
    let resolve = Resolve::new(metadata)?;
    let from = resolve.packages.iter().filter_map(|(id, (name, _))| {
        let short = name.strip_prefix("graphix-package-")?;
        external.contains_key(short).then_some(*id)
    });
    Ok(resolve
        .reach(from, |n| n.starts_with("graphix-package-"))
        .into_iter()
        .filter_map(|id| Some(short_name(resolve.name(id)?)))
        .collect())
}

/// Refuse an external package whose dependencies reach a graphix-package
/// other than the shell's: its `Package` impl is of another trait, so the
/// shell could not register it.
fn refuse_foreign_graphix_package(
    metadata: &serde_json::Value,
    external: &BTreeMap<CompactString, PackageEntry>,
) -> Result<()> {
    let resolve = Resolve::new(metadata)?;
    if resolve.named("graphix-package").nth(1).is_none() {
        return Ok(());
    }
    let ours = resolve
        .root
        .into_iter()
        .flat_map(|root| resolve.normal_deps(root))
        .find(|id| resolve.name(id) == Some("graphix-package"))
        .ok_or_else(|| anyhow!("the shell does not depend on graphix-package"))?;
    let version = |id: &str| resolve.packages.get(id).map_or("", |(_, v)| *v);
    let mut refused = Vec::new();
    for name in external.keys() {
        let krate = crate_name(name);
        let reached = resolve.reach(resolve.named(&krate), |_| true);
        for id in reached {
            if id != ours && resolve.name(id) == Some("graphix-package") {
                refused.push(format_compact!("{name} (graphix-package {})", version(id)));
            }
        }
    }
    if !refused.is_empty() {
        bail!(
            "this shell is built with graphix-package {}, and these packages are \
             built with another: {}; update or remove them",
            version(ours),
            refused.join(", ")
        )
    }
    Ok(())
}

/// `cargo metadata` for the shell source in `source_dir` built with
/// `features`, under the cargo configuration its build reads.
async fn shell_metadata(
    cargo: &Path,
    source_dir: &Path,
    features: &[CompactString],
) -> Result<serde_json::Value> {
    let mut cmd = Command::new(cargo);
    cmd.arg("metadata")
        .arg("--format-version")
        .arg("1")
        .arg("--no-default-features")
        .current_dir(source_dir)
        .kill_on_drop(true);
    if !features.is_empty() {
        cmd.arg("--features").arg(features.join(" "));
    }
    let out = cmd.output().await.context("running cargo metadata")?;
    if !out.status.success() {
        bail!("cargo metadata failed: {}", String::from_utf8_lossy(&out.stderr))
    }
    serde_json::from_slice(&out.stdout).context("parsing cargo metadata")
}

/// A source tree's manifest and lockfile as they were, to put back.
struct Saved {
    files: Vec<(PathBuf, Option<Vec<u8>>)>,
}

impl Saved {
    /// `source_dir`'s manifest, and the lockfile cargo will write: its own,
    /// else its workspace's.
    async fn take(source_dir: &Path) -> Result<Self> {
        let lock = source_dir
            .ancestors()
            .map(|d| d.join("Cargo.lock"))
            .find(|p| p.exists())
            .unwrap_or_else(|| source_dir.join("Cargo.lock"));
        let mut files = Vec::new();
        for path in [source_dir.join("Cargo.toml"), lock] {
            let content = match fs::read(&path).await {
                Ok(c) => Some(c),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                Err(e) => {
                    return Err(e).with_context(|| format!("saving {}", path.display()));
                }
            };
            files.push((path, content));
        }
        Ok(Saved { files })
    }

    /// Every file put back as it was; the first failure is reported once
    /// all were tried.
    async fn put_back(self) -> Result<()> {
        let mut res = Ok(());
        for (path, content) in self.files {
            let r = match content {
                Some(c) => fs::write(&path, c).await,
                None => match fs::remove_file(&path).await {
                    Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
                    r => r,
                },
            };
            if let Err(e) = r
                && res.is_ok()
            {
                res = Err(e).with_context(|| format!("restoring {}", path.display()));
            }
        }
        res
    }
}

/// The executable of the shell's `graphix` binary, when `message` (a line
/// of `cargo build --message-format=json`) reports building it.
fn graphix_executable(message: &str) -> Option<PathBuf> {
    let m: serde_json::Value = serde_json::from_str(message).ok()?;
    let target = &m["target"];
    let is_graphix = m["reason"] == "compiler-artifact"
        && target["name"] == "graphix"
        && target["kind"].as_array()?.iter().any(|k| k == "bin");
    is_graphix.then(|| m["executable"].as_str().map(PathBuf::from))?
}

/// The newest stable version a crate has, else its newest.
fn newest(c: &crates_io_api::Crate) -> CompactString {
    c.max_stable_version.as_deref().unwrap_or(&c.max_version).into()
}

/// True if version `a` is strictly newer than `b` by semver. Falls back to
/// string inequality if either fails to parse (a degraded "they differ" test).
fn version_gt(a: &str, b: &str) -> bool {
    match (semver::Version::parse(a), semver::Version::parse(b)) {
        (Ok(a), Ok(b)) => a > b,
        _ => a != b,
    }
}

/// The build directory under `graphix_data_dir`, created, and where the
/// source of graphix-shell `version` unpacks in it.
async fn build_dirs(
    graphix_data_dir: &Path,
    version: &str,
) -> Result<(PathBuf, PathBuf)> {
    let build_dir = graphix_data_dir.join("build");
    fs::create_dir_all(&build_dir).await?;
    let graphix_dir = build_dir.join(format!("graphix-shell-{version}"));
    Ok((build_dir, graphix_dir))
}

/// Where cargo keeps its registry: `$CARGO_HOME`, else `~/.cargo`, wherever
/// the cargo binary lives.
fn cargo_home() -> Result<PathBuf> {
    match std::env::var_os("CARGO_HOME") {
        Some(home) if !home.is_empty() => Ok(PathBuf::from(home)),
        _ => dirs::home_dir()
            .map(|home| home.join(".cargo"))
            .ok_or_else(|| anyhow!("can't find your home directory")),
    }
}

// fetch our source from the local cargo cache (preferred method)
async fn extract_local_source(
    cargo_home: &Path,
    graphix_data_dir: &Path,
    version: &str,
) -> Result<PathBuf> {
    let (_, graphix_dir) = build_dirs(graphix_data_dir, version).await?;
    let package = format!("graphix-shell-{version}");
    let cargo_src = cargo_home.join("registry").join("src");
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
    let (graphix_build_dir, graphix_dir) = build_dirs(graphix_data_dir, version).await?;
    let cr = crates_io.get_crate("graphix-shell").await?;
    let cr_version = cr
        .versions
        .into_iter()
        .find(|v| v.num == version)
        .ok_or_else(|| anyhow!("can't find version {version} on crates.io"))?;
    let crate_data_tar_gz =
        reqwest::get(Url::parse("https://crates.io")?.join(&cr_version.dl_path)?)
            .await?
            .error_for_status()?
            .bytes()
            .await?;
    let hex = format_compact!(
        "{:x}",
        <sha2::Sha256 as sha2::Digest>::digest(&crate_data_tar_gz)
    );
    if hex != cr_version.checksum {
        bail!(
            "graphix-shell {version} from crates.io has sha256 {hex}, the index says {}",
            cr_version.checksum
        )
    }
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

/// A package's short name, its crate name less `graphix-package-`.
fn short_name(name: &str) -> CompactString {
    name.strip_prefix("graphix-package-").unwrap_or(name).into()
}

/// The crate of the package with short name `name`.
fn crate_name(name: &str) -> CompactString {
    format_compact!("graphix-package-{name}")
}

impl PackageId {
    pub fn new(name: &str, version: Option<&str>) -> Self {
        let version = version.map(CompactString::from);
        Self { name: short_name(name), version, path: None }
    }

    pub fn with_path(name: &str, path: PathBuf) -> Self {
        Self { name: short_name(name), version: None, path: Some(path) }
    }

    /// Short name without graphix-package- prefix
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The full crate name
    pub fn crate_name(&self) -> CompactString {
        crate_name(&self.name)
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
    /// A newer shell version, when one is available.
    shell: Option<ShellBump>,
    /// `(name, old, new)` for each external package with a newer version.
    external_updates: Vec<(CompactString, CompactString, CompactString)>,
}

/// A newer shell, and the stdlib packages it ships that the record has
/// not seen (installed or removed): only that shell builds them.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ShellBump {
    current: CompactString,
    latest: CompactString,
    new_stdlib: Vec<CompactString>,
}

impl UpdatePlan {
    fn is_empty(&self) -> bool {
        self.shell.is_none() && self.external_updates.is_empty()
    }
}

/// Which items of an `UpdatePlan` the user chose to apply.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Selection {
    shell: bool,
    new_stdlib: BTreeSet<CompactString>,
    external: BTreeSet<CompactString>,
}

impl Selection {
    /// The "apply everything" selection.
    fn all(plan: &UpdatePlan) -> Self {
        Self {
            shell: plan.shell.is_some(),
            new_stdlib: plan
                .shell
                .iter()
                .flat_map(|b| b.new_stdlib.iter().cloned())
                .collect(),
            external: plan.external_updates.iter().map(|(n, _, _)| n.clone()).collect(),
        }
    }
}

/// Compute the available changes from the current state. Pure.
fn compute_update_plan(
    current_shell: &str,
    latest_shell: &str,
    stdlib_set_from_source: &BTreeSet<CompactString>,
    packages: &Packages,
    external_latest: &BTreeMap<CompactString, CompactString>,
) -> UpdatePlan {
    // BTreeSet iterates sorted, so new_stdlib comes out sorted.
    let shell = version_gt(latest_shell, current_shell).then(|| ShellBump {
        current: current_shell.to_compact_string(),
        latest: latest_shell.to_compact_string(),
        new_stdlib: stdlib_set_from_source
            .iter()
            .filter(|n| {
                !packages.stdlib_installed.contains(*n)
                    && !packages.stdlib_removed.contains(*n)
            })
            .cloned()
            .collect(),
    });
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
    UpdatePlan { shell, external_updates }
}

/// Apply a selection to `packages`, returning the shell version to build
/// against. Pure. New stdlib packages build only against the latest shell,
/// so with the bump declined they are left untracked to resurface later;
/// with it taken, a deselected new package is recorded in `removed`.
fn apply_selection(
    current_shell: &str,
    plan: &UpdatePlan,
    sel: &Selection,
    packages: &mut Packages,
) -> CompactString {
    let build_version = match &plan.shell {
        Some(bump) if sel.shell => {
            for name in &bump.new_stdlib {
                if sel.new_stdlib.contains(name) {
                    packages.stdlib_removed.remove(name);
                    packages.stdlib_installed.insert(name.clone());
                } else {
                    packages.stdlib_installed.remove(name);
                    packages.stdlib_removed.insert(name.clone());
                }
            }
            bump.latest.clone()
        }
        _ => current_shell.to_compact_string(),
    };
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
    Shell { current: CompactString, latest: CompactString },
    NewStdlib(CompactString),
    External { name: CompactString, old: CompactString, new: CompactString },
}

impl Item {
    fn section(&self) -> &'static str {
        match self {
            Item::Shell { .. } => "Shell",
            Item::NewStdlib(_) => "New stdlib packages (require the shell update)",
            Item::External { .. } => "External package updates",
        }
    }

    fn render(&self) -> CompactString {
        match self {
            Item::Shell { current, latest } => {
                format_compact!("graphix-shell   {current} -> {latest}")
            }
            Item::NewStdlib(n) => n.clone(),
            Item::External { name, old, new } => {
                format_compact!("{name}   {old} -> {new}")
            }
        }
    }
}

/// Flatten a plan into ordered items (shell, then new stdlib, then external).
/// The 1-based position is the stable index used by the edit prompt.
fn plan_items(plan: &UpdatePlan) -> Vec<Item> {
    let mut items = Vec::new();
    if let Some(bump) = &plan.shell {
        items.push(Item::Shell {
            current: bump.current.clone(),
            latest: bump.latest.clone(),
        });
        for n in &bump.new_stdlib {
            items.push(Item::NewStdlib(n.clone()));
        }
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

/// Prompt for a yes/no confirmation, defaulting to no.
async fn confirm_yn(prompt: &str) -> Result<bool> {
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
fn normalize_selection(
    items: &[Item],
    selected: &mut BTreeSet<usize>,
) -> Vec<CompactString> {
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

/// A backup of a replaced shell is named this prefix, then the local time
/// it was made in `BACKUP_STAMP`'s format.
const BACKUP_PREFIX: &str = "graphix-previous-";
const BACKUP_STAMP: &str = "%Y%m%d-%H%M%S";

/// When the backup named `name` was made, read from its name: a copy keeps
/// its source's mtime on some platforms.
fn backup_stamp(name: &str) -> Option<chrono::NaiveDateTime> {
    let stamp = name.strip_prefix(BACKUP_PREFIX)?.get(..15)?;
    chrono::NaiveDateTime::parse_from_str(stamp, BACKUP_STAMP).ok()
}

/// The Graphix package manager
pub struct GraphixPM {
    cratesio: AsyncClient,
    cargo: PathBuf,
    /// The running shell's version, the source a rebuild starts from.
    version: CompactString,
    /// The running shell's executable, which a rebuild replaces.
    exe: PathBuf,
    /// Where the unpacked source builds ([`shared_target_dir`]).
    target_dir: PathBuf,
}

impl GraphixPM {
    /// A package manager for the running shell, of version `version`.
    pub async fn new(version: &str) -> Result<Self> {
        let cargo = which::which("cargo").context("can't find the cargo command")?;
        let cratesio = AsyncClient::new(
            "Graphix Package Manager <eestokes@pm.me>",
            Duration::from_secs(1),
        )?;
        let exe = std::env::current_exe().context("locating the running graphix")?;
        let target_dir = shared_target_dir()?;
        Ok(Self { cratesio, cargo, version: version.into(), exe, target_dir })
    }

    fn check_rebuild_exe(&self) -> Result<()> {
        let exe = self.exe.canonicalize().context("resolving the running executable")?;
        for dir in exe.ancestors().skip(1) {
            for marker in [".rustc_info.json", ".cargo-lock"] {
                if dir.join(marker).try_exists().with_context(|| {
                    format_compact!("checking Cargo build markers in {}", dir.display())
                })? {
                    bail!(
                        "cannot replace {} inside a Cargo target directory; copy the executable outside the build directory before rebuilding packages",
                        exe.display()
                    );
                }
            }
        }
        Ok(())
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

    /// Hold the package lock for the life of the guard, saying so when
    /// another operation has it.
    fn hold(
        lock: &mut fd_lock::RwLock<std::fs::File>,
    ) -> Result<fd_lock::RwLockWriteGuard<'_, std::fs::File>> {
        let busy = lock.try_write().is_err();
        if busy {
            println!("waiting for another graphix package operation to finish...");
        }
        lock.write().context("waiting for package lock")
    }

    /// Unpack a fresh copy of the graphix-shell source. Tries the
    /// local cargo registry cache first, falls back to downloading
    /// from crates.io.
    async fn unpack_source(&self, version: &str) -> Result<PathBuf> {
        let graphix_data_dir = graphix_data_dir()?;
        let local = async {
            extract_local_source(&cargo_home()?, &graphix_data_dir, version).await
        };
        match local.await {
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

    /// Delete the scratch build dir and unpack a fresh graphix-shell source
    /// tree for `version`.
    async fn prepare_source(&self, version: &str) -> Result<PathBuf> {
        self.check_rebuild_exe()?;
        println!("Unpacking graphix-shell source...");
        let build_dir = graphix_data_dir()?.join("build");
        if fs::metadata(&build_dir).await.is_ok() {
            fs::remove_dir_all(&build_dir).await?;
        }
        self.unpack_source(version).await
    }

    /// Build and install `packages` from an unpacked source tree, the record
    /// reconciled with what the source ships first, and back up the current
    /// binary.
    async fn install_from_source(
        &self,
        source_dir: &Path,
        packages: &mut Packages,
    ) -> Result<()> {
        reconcile(source_dir, packages).await?;
        packages.enforce_invariants();
        println!("Updating Cargo.toml...");
        let registered = self.add_external(source_dir, &packages.build_plan()).await?;
        for name in reinstall_registered(packages, &registered) {
            println!(
                "note: {name} is installed again: an external package depends on it"
            );
        }
        let plan = packages.build_plan();
        // Stdlib packages are selected by Cargo feature; `core` is always compiled.
        println!("Building graphix with updated packages (this may take a while)...");
        let built =
            self.cargo_build(source_dir, &plan.features, &self.target_dir).await?;
        self.replace_exe(&built).await?;
        // Clean up old previous binaries (>1 week)
        self.cleanup_old_binaries().await;
        println!("Done! Restart graphix to use the updated packages.");
        Ok(())
    }

    /// Add `plan`'s external packages to the shell source in `source_dir`:
    /// what they register ([`registered_by_external`]), refused when one is
    /// built with another graphix-package.
    async fn add_external(
        &self,
        source_dir: &Path,
        plan: &BuildPlan,
    ) -> Result<BTreeSet<CompactString>> {
        rewrite_manifest(source_dir, &plan.external).await?;
        if plan.external.is_empty() {
            return Ok(BTreeSet::new());
        }
        let metadata = shell_metadata(&self.cargo, source_dir, &plan.features).await?;
        refuse_foreign_graphix_package(&metadata, &plan.external)?;
        registered_by_external(&metadata, &plan.external)
    }

    /// Put the binary at `built` in place of the running one, which is kept
    /// beside it as a dated backup: moved aside, since a running executable
    /// cannot be written over on every host. The new binary is copied
    /// beside the old first, so the exe path only ever changes by rename.
    async fn replace_exe(&self, built: &Path) -> Result<()> {
        self.check_rebuild_exe()?;
        let date = Local::now().format(BACKUP_STAMP);
        let ext =
            self.exe.extension().map(|e| format_compact!(".{}", e.to_string_lossy()));
        let backup = self.exe.with_file_name(
            format_compact!("{BACKUP_PREFIX}{date}{}", ext.unwrap_or_default()).as_str(),
        );
        let name = self.exe.file_name().ok_or_else(|| anyhow!("the exe has no name"))?;
        let staged = self
            .exe
            .with_file_name(format_compact!(".{}.new", name.to_string_lossy()).as_str());
        if let Err(e) = fs::copy(built, &staged).await {
            let _ = fs::remove_file(&staged).await;
            return Err(e).with_context(|| {
                format_compact!("copying {} to {}", built.display(), staged.display())
            });
        }
        if let Err(e) = fs::rename(&self.exe, &backup).await {
            let _ = fs::remove_file(&staged).await;
            return Err(e)
                .with_context(|| format_compact!("moving {} aside", self.exe.display()));
        }
        if let Err(e) = fs::rename(&staged, &self.exe).await {
            let _ = fs::rename(&backup, &self.exe).await;
            let _ = fs::remove_file(&staged).await;
            return Err(e).with_context(|| {
                format_compact!("moving {} to {}", staged.display(), self.exe.display())
            });
        }
        Ok(())
    }

    /// `cargo build --release` of the shell in `source_dir` with `features`
    /// on and no others, keeping the source's lockfile, into `target_dir`:
    /// the binary's path, as cargo reports it.
    async fn cargo_build(
        &self,
        source_dir: &Path,
        features: &[CompactString],
        target_dir: &Path,
    ) -> Result<PathBuf> {
        let mut cmd = Command::new(&self.cargo);
        cmd.arg("build")
            .arg("--release")
            .arg("--target-dir")
            .arg(target_dir)
            .arg("--no-default-features")
            .arg("--message-format=json-render-diagnostics")
            .current_dir(source_dir)
            .stdout(Stdio::piped())
            .kill_on_drop(true);
        if !features.is_empty() {
            cmd.arg("--features").arg(features.join(" "));
        }
        let mut child = cmd.spawn().context("running cargo build")?;
        let stdout = child.stdout.take().ok_or_else(|| anyhow!("cargo has no stdout"))?;
        let mut stdout = BufReader::new(stdout);
        let mut line = String::new();
        let mut built = None;
        while stdout.read_line(&mut line).await.context("reading cargo build")? > 0 {
            if let Some(exe) = graphix_executable(&line) {
                built = Some(exe);
            }
            line.clear();
        }
        let status = child.wait().await.context("running cargo build")?;
        if !status.success() {
            bail!("cargo build --release failed with status {status}")
        }
        built.ok_or_else(|| anyhow!("cargo build --release reported no graphix binary"))
    }

    /// Rebuild the graphix binary with the given package set
    async fn rebuild(&self, packages: &mut Packages, version: &str) -> Result<()> {
        let source_dir = self.prepare_source(version).await?;
        self.install_from_source(&source_dir, packages).await
    }

    /// Clean up graphix-previous-* binaries older than 1 week
    async fn cleanup_old_binaries(&self) {
        let Some(bin_dir) = self.exe.parent() else { return };
        let Ok(mut entries) = fs::read_dir(bin_dir).await else { return };
        let week_ago = Local::now().naive_local() - chrono::Duration::days(7);
        while let Ok(Some(entry)) = entries.next_entry().await {
            let name = entry.file_name();
            if let Some(made) = name.to_str().and_then(backup_stamp)
                && made < week_ago
            {
                let _ = fs::remove_file(entry.path()).await;
            }
        }
    }

    /// The version written in the Cargo.toml at `path`, which must be crate
    /// `crate_name`'s; none when it is inherited from a workspace, which
    /// cargo resolves for a path dependency.
    async fn read_package_version(
        path: &Path,
        crate_name: &str,
    ) -> Result<Option<CompactString>> {
        let cargo_toml_path = path.join("Cargo.toml");
        let contents = fs::read_to_string(&cargo_toml_path)
            .await
            .with_context(|| format!("reading {}", cargo_toml_path.display()))?;
        let doc: toml::Value =
            toml::from_str(&contents).context("parsing package Cargo.toml")?;
        let field =
            |f| doc.get("package").and_then(|p| p.get(f)).and_then(|v| v.as_str());
        match field("name") {
            Some(name) if name == crate_name => (),
            name => bail!(
                "{} names the crate {}, not {crate_name}",
                cargo_toml_path.display(),
                name.unwrap_or("nothing")
            ),
        }
        Ok(field("version").map(|s| s.to_compact_string()))
    }

    /// Add packages and rebuild
    pub async fn add_packages(
        &self,
        packages: &[PackageId],
        skip_crates_io_check: bool,
    ) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = Self::hold(&mut lock)?;
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
                installed.stdlib_removed.remove(name);
                if installed.stdlib_installed.insert(name.to_compact_string()) {
                    println!("Adding stdlib package {name}");
                    changed = true;
                } else {
                    println!("{name} is already installed");
                }
                continue;
            }
            let entry = if let Some(path) = pkg.path() {
                let path = path
                    .canonicalize()
                    .with_context(|| format!("resolving path {}", path.display()))?;
                match Self::read_package_version(&path, &pkg.crate_name()).await? {
                    Some(v) => {
                        println!("Adding {name} @ path {} (version {v})", path.display())
                    }
                    None => println!("Adding {name} @ path {}", path.display()),
                }
                PackageEntry::Path(path)
            } else if skip_crates_io_check {
                match pkg.version() {
                    Some(v) => {
                        println!("Adding {name}@{v}");
                        PackageEntry::Version(v.to_compact_string())
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
                    Some(v) => v.to_compact_string(),
                    None => newest(&cr.crate_data),
                };
                println!("Adding {name}@{version}");
                PackageEntry::Version(version)
            };
            if installed.external.get(name) == Some(&entry) {
                println!("{name} is already installed as {entry}");
                continue;
            }
            installed.external.insert(name.to_compact_string(), entry);
            changed = true;
        }
        if changed {
            self.rebuild(&mut installed, &self.version).await?;
            write_packages(&installed).await?;
        } else {
            println!("No changes needed.");
        }
        Ok(())
    }

    /// Remove packages and rebuild. Removing a stdlib package removes the
    /// installed packages that depend on it too (the feature graph would
    /// keep it compiled in): those not asked for are confirmed, or taken
    /// with `assume_yes`, and a refusal removes nothing.
    pub async fn remove_packages(
        &self,
        packages: &[PackageId],
        assume_yes: bool,
    ) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = Self::hold(&mut lock)?;
        let mut installed = read_packages().await?;
        let version = &self.version;
        // the whole removal, decided against the set installed now
        let mut stdlib: BTreeSet<CompactString> = BTreeSet::new();
        let mut external: Vec<&str> = Vec::new();
        for pkg in packages {
            let name = pkg.name();
            if name == "core" {
                eprintln!("Cannot remove the core package");
            } else if is_stdlib_package(name) || installed.stdlib_installed.contains(name)
            {
                if installed.stdlib_installed.contains(name) {
                    stdlib.insert(name.to_compact_string());
                } else {
                    println!("{name} is already removed");
                }
            } else if installed.external.contains_key(name) {
                external.push(name);
            } else {
                println!("{name} is not installed");
            }
        }
        // a stdlib removal needs the shell's feature graph and what the
        // external packages kept register: the source, unpacked once and
        // built from
        let (prepared, cascade) = match stdlib.is_empty() {
            true => (None, BTreeSet::new()),
            false => {
                let source = self.prepare_source(&version).await?;
                let cargo = fs::read_to_string(source.join("Cargo.toml")).await?;
                let edges = feature_edges(&cargo)?;
                let mut plan = installed.build_plan();
                plan.external.retain(|name, _| !external.contains(&name.as_str()));
                // the build adds the external packages to the source again
                let saved = Saved::take(&source).await?;
                let registered = self.add_external(&source, &plan).await;
                saved.put_back().await?;
                let cascade = removal_cascade(
                    &stdlib,
                    &installed.stdlib_installed,
                    &edges,
                    &registered?,
                )?;
                (Some(source), cascade)
            }
        };
        if !cascade.is_empty() {
            let list: Vec<&str> = cascade.iter().map(|s| s.as_str()).collect();
            println!(
                "Removing these also removes packages that depend on them: {}",
                list.join(", ")
            );
            if !assume_yes {
                if !std::io::stdin().is_terminal() {
                    bail!(
                        "stdin is not a terminal; re-run with --yes to remove the dependents too"
                    );
                }
                if !confirm_yn("Remove them too?").await? {
                    bail!("nothing removed");
                }
            }
        }
        if stdlib.is_empty() && external.is_empty() {
            println!("No changes needed.");
            return Ok(());
        }
        for name in stdlib.iter().chain(cascade.iter()) {
            installed.stdlib_installed.remove(name);
            installed.stdlib_removed.insert(name.clone());
            println!("Removing stdlib package {name}");
        }
        for name in external {
            installed.external.remove(name);
            println!("Removing {name}");
        }
        match &prepared {
            Some(source) => self.install_from_source(source, &mut installed).await?,
            None => self.rebuild(&mut installed, &version).await?,
        }
        write_packages(&installed).await
    }

    /// Search crates.io for graphix packages
    pub async fn search(&self, query: &str) -> Result<()> {
        let search_query = crate_name(query);
        let results = self
            .cratesio
            .crates(
                crates_io_api::CratesQuery::builder()
                    .search(search_query.as_str())
                    .build(),
            )
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
        let _guard = Self::hold(&mut lock)?;
        let mut packages = read_packages().await?;
        self.rebuild(&mut packages, &self.version).await?;
        write_packages(&packages).await
    }

    /// List installed packages
    pub async fn list(&self) -> Result<()> {
        let packages = read_packages().await?;
        // core is always installed
        println!("stdlib packages:");
        for name in &packages.stdlib_installed {
            println!("  {name}");
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
    /// The binary is placed in `package_dir/<short name>`. Only the local
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
        if is_stdlib_package(short_name) {
            bail!(
                "{crate_name} is named like the stdlib package {short_name}: a standalone \
                 shell cannot carry both"
            )
        }
        let mut lock = Self::lock_file()?;
        let _guard = Self::hold(&mut lock)?;
        let source_dir = match source_override {
            Some(dir) => dir
                .canonicalize()
                .with_context(|| format!("resolving {}", dir.display()))?,
            None => self.prepare_source(&self.version).await?,
        };
        // The embedded package is registered as an external path package, and
        // the stdlib packages it depends on, as the shell ships them, are
        // features: the feature graph pulls their closure and `core` is always
        // compiled.
        let shell_manifest = fs::read_to_string(source_dir.join("Cargo.toml")).await?;
        let mut features = standalone_features(&contents, &shell_manifest)?;
        features.push(format_compact!("{crate_name}/standalone"));
        let plan = BuildPlan {
            features,
            external: BTreeMap::from([(
                short_name.to_compact_string(),
                PackageEntry::Path(package_dir.clone()),
            )]),
        };
        // an override is the caller's own tree: its manifest and lockfile are
        // put back however the build ends, an interrupt included
        let saved = match source_override {
            Some(_) => Some(Saved::take(&source_dir).await?),
            None => None,
        };
        // a caller's own tree builds in its own target directory
        let target_dir = match source_override {
            Some(_) => source_dir.join("target"),
            None => self.target_dir.clone(),
        };
        let built = tokio::select! {
            built = self.build_embedding(&source_dir, &plan, &target_dir) => built,
            _ = tokio::signal::ctrl_c() => Err(anyhow!("interrupted")),
        };
        if let Some(saved) = saved {
            saved.put_back().await?;
        }
        let built = built?;
        let bin_name = format!("{short_name}{}", std::env::consts::EXE_SUFFIX);
        let dest = package_dir.join(&bin_name);
        fs::copy(&built, &dest).await.with_context(|| {
            format!("copying {} to {}", built.display(), dest.display())
        })?;
        println!("Done! Binary written to {}", dest.display());
        Ok(())
    }

    /// Build the shell in `source_dir` as `plan` says, into `target_dir`:
    /// the binary's path.
    async fn build_embedding(
        &self,
        source_dir: &Path,
        plan: &BuildPlan,
        target_dir: &Path,
    ) -> Result<PathBuf> {
        println!("Updating Cargo.toml...");
        self.add_external(source_dir, plan).await?;
        println!("Building standalone binary (this may take a while)...");
        self.cargo_build(source_dir, &plan.features, target_dir).await
    }

    /// Query crates.io for the latest version of a crate
    async fn latest_version(&self, crate_name: &str) -> Result<CompactString> {
        let cr = self
            .cratesio
            .get_crate(crate_name)
            .await
            .with_context(|| format!("querying crates.io for {crate_name}"))?;
        Ok(newest(&cr.crate_data))
    }

    /// Update graphix and its packages: discover the available changes (shell
    /// bump, newly-shipped stdlib packages, external package updates), confirm
    /// with the user (unless `assume_yes`), then rebuild against the chosen
    /// shell version with the chosen package set.
    pub async fn update(&self, assume_yes: bool) -> Result<()> {
        let mut lock = Self::lock_file()?;
        let _guard = Self::hold(&mut lock)?;
        let current = &self.version;
        let latest = self
            .latest_version("graphix-shell")
            .await
            .context("checking crates.io for the latest graphix-shell")?;
        let mut packages = read_packages().await?;
        // New stdlib packages only ship with a shell bump, so only unpack the
        // latest source to enumerate them when a bump is available.
        let shell_bump = version_gt(&latest, &current);
        let answerable = assume_yes || std::io::stdin().is_terminal();
        let unanswerable = || {
            anyhow!(
                "stdin is not a terminal; re-run with --yes to apply all available changes"
            )
        };
        if shell_bump && !answerable {
            return Err(unanswerable());
        }
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
                match self.latest_version(&crate_name(name)).await {
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
        } else if !answerable {
            return Err(unanswerable());
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
        let build_version = apply_selection(&current, &plan, &sel, &mut packages);
        // Reuse the latest source if we're building against it; otherwise the
        // shell bump was declined and we must build against the current source.
        let build_src = match latest_src {
            Some(s) if build_version == latest => s,
            _ => self.prepare_source(&build_version).await?,
        };
        // Build before writing: a failed build leaves packages.toml
        // untouched and the user re-runs to the same choices.
        self.install_from_source(&build_src, &mut packages).await?;
        write_packages(&packages).await?;
        Ok(())
    }
}
