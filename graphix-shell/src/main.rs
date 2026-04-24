use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use clap::{Parser, Subcommand};
use enumflags2::BitFlags;
use flexi_logger::{FileSpec, Logger};
use graphix_compiler::{
    expr::{ModuleResolver, Source},
    CFlag,
};
use graphix_package::{GraphixPM, MainThreadHandle, PackageId};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use log::info;
use netidx::{
    config::Config,
    path::Path,
    publisher::{BindCfg, DesiredAuth, Publisher, PublisherBuilder},
    subscriber::{Subscriber, SubscriberBuilder},
    InternalOnly,
};
use std::{path::PathBuf, str::FromStr, sync::OnceLock, time::Duration};

#[derive(Debug, Clone, Copy)]
enum RawFlag {
    Unhandled,
    NoUnhandled,
    Unused,
    NoUnused,
    Error,
    NoError,
}

impl FromStr for RawFlag {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "unhandled" => Ok(Self::Unhandled),
            "no-unhandled" => Ok(Self::NoUnhandled),
            "unused" => Ok(Self::Unused),
            "no-unused" => Ok(Self::NoUnused),
            "error" => Ok(Self::Error),
            "no-error" => Ok(Self::NoError),
            s => bail!("invalid flag {s}"),
        }
    }
}

impl RawFlag {
    fn as_flags(flags: &[RawFlag]) -> (BitFlags<CFlag>, BitFlags<CFlag>) {
        let mut enable = BitFlags::empty();
        let mut disable = BitFlags::empty();
        for fl in flags {
            match fl {
                Self::Unhandled => enable.insert(CFlag::WarnUnhandled),
                Self::NoUnhandled => disable.insert(CFlag::WarnUnhandled),
                Self::Unused => enable.insert(CFlag::WarnUnused),
                Self::NoUnused => disable.insert(CFlag::WarnUnused),
                Self::Error => enable.insert(CFlag::WarningsAreErrors),
                Self::NoError => disable.insert(CFlag::WarningsAreErrors),
            }
        }
        (enable, disable)
    }
}

#[derive(Subcommand)]
enum PackageAction {
    /// Add packages to the graphix runtime
    Add {
        /// Package names to add (with optional @version suffix)
        packages: Vec<String>,
        /// Skip crates.io validation (for packages from alternative registries or local sources)
        #[arg(long)]
        skip_crates_io_check: bool,
        /// Use a local path dependency instead of a crates.io version
        #[arg(long)]
        path: Option<PathBuf>,
    },
    /// Remove packages from the graphix runtime
    Remove {
        /// Package names to remove
        packages: Vec<String>,
    },
    /// Search crates.io for graphix packages
    Search {
        /// Search query
        query: String,
    },
    /// List installed packages
    List,
    /// Rebuild graphix from the current packages.toml
    Rebuild,
    /// Create a new graphix package
    Create {
        /// Package name (will be prefixed with graphix-package- if needed)
        name: String,
        /// Directory to create the package in
        #[arg(long, default_value = ".")]
        dir: PathBuf,
    },
    /// Update graphix to the latest version
    Update,
    /// Build a standalone graphix binary from the package in the
    /// current directory. Pass `--source-override` to reuse a
    /// pre-extracted graphix-shell source tree (e.g. the workspace)
    /// instead of downloading it from crates.io — useful in dev.
    BuildStandalone {
        #[arg(long = "source-override")]
        source_override: Option<PathBuf>,
    },
}

#[derive(Subcommand)]
enum Command {
    /// Manage graphix packages
    Package {
        #[command(subcommand)]
        action: PackageAction,
    },
    /// Run the Language Server Protocol server (communicates over stdio).
    /// Editors typically launch this automatically; you don't usually run it directly.
    Lsp,
    /// Compile a graphix program, emitting a cargo package with native
    /// fused kernels alongside the (possibly rewritten) main program.
    /// The emitted package is ready to be built via `cargo build`.
    Compile {
        /// Input .gx file to analyse
        input: PathBuf,
        /// Directory where the generated package should be written.
        /// Must be empty or nonexistent.
        #[arg(short = 'o', long = "out")]
        out_dir: PathBuf,
        /// Short package name for the generated crate (no
        /// `graphix-package-` prefix). Defaults to the input file's
        /// stem, sanitized.
        #[arg(long)]
        name: Option<String>,
        /// Path to the graphix workspace root. Used to generate path
        /// dependencies in the emitted Cargo.toml. Defaults to the
        /// ancestors of the running binary.
        #[arg(long)]
        workspace_root: Option<PathBuf>,
        /// After emitting the package, invoke the graphix standalone
        /// build pipeline on it (equivalent to running
        /// `graphix package build-standalone --source-override <root>`
        /// from the output directory). Produces a native binary.
        #[arg(long)]
        build: bool,
        /// When `--build` is on, use the dev profile (fast build, no
        /// LTO, unoptimized) instead of release. Useful for
        /// differential testing / smoke-testing the fused kernels —
        /// cuts build time from ~15 min to ~2 min, at the cost of
        /// runtime speed (binary will still be correct, just not
        /// optimized).
        #[arg(long)]
        dev: bool,
    },
}

#[derive(Parser)]
#[command(version, about, trailing_var_arg = true)]
struct Params {
    #[command(subcommand)]
    command: Option<Command>,
    /// enable logging and put the log in the specified directory. You
    /// should also set the RUST_LOG enviornment variable. e.g. RUST_LOG=debug
    #[arg(long)]
    log_dir: Option<PathBuf>,
    /// path to the netidx config to load, otherwise the default will
    /// be loaded (unless --no-netidx is specified)
    #[arg(long)]
    config: Option<PathBuf>,
    /// the desired netidx auth mechanism to use, otherwise use the config default
    #[arg(long)]
    auth: Option<DesiredAuth>,
    /// the kerberos user principal name to use for netidx, otherwise
    /// the default from the current user's cached tickets, only valid
    /// if using kerberos auth
    #[arg(long)]
    upn: Option<String>,
    /// the netidx nerberos service princial name, otherwise the
    /// default from the current user's cached ticket, only valid if
    /// using kerberos auth
    #[arg(long)]
    spn: Option<String>,
    /// the netidx tls identity to use, otherwise use the configured
    /// default, only valid if using tls auth.
    #[arg(long)]
    identity: Option<String>,
    /// specify the netidx publisher bind address.
    #[arg(long)]
    bind: Option<BindCfg>,
    /// drop subscribers if they don't consume published values with
    /// the specifed timeout (in seconds).
    #[arg(long)]
    publish_timeout: Option<u64>,
    /// module resolution from netidx should fail if we can't
    /// subscribe to the module before the timeout expires. Default,
    /// wait forever.
    #[arg(long)]
    resolve_timeout: Option<u64>,
    /// disable netidx, net functions will only work internally
    #[arg(short, long)]
    no_netidx: bool,
    /// do not attempt to run the init module
    #[arg(short = 'i', long)]
    no_init: bool,
    /// do not execute the program, just veryify that it compiles and
    /// type checks.
    #[arg(long = "check")]
    check: bool,
    /// run the program in the specified file instead of starting the REPL
    file: Option<ArcStr>,
    /// enable or disable compiler flags. Currently supported flags are,
    /// - unhandled, no-unhandled: warn about unhandled ? operators (default)
    /// - unused, no-unused: warn about unused variables (default)
    /// - error, no-error makes warnings errors
    ///
    /// the no- variant turns the flag off. If both are specifed the no- variant
    /// always wins
    #[arg(short = 'W')]
    warn: Vec<RawFlag>,
    /// arguments passed to the graphix program (everything after the filename)
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    program_args: Vec<String>,
}

impl Params {
    async fn get_pub_sub(&self, cfg: Result<Config>) -> Result<(Publisher, Subscriber)> {
        let res = async {
            let cfg = cfg?;
            let auth = match &self.auth {
                None => cfg.default_auth(),
                Some(a) => a.clone(),
            };
            let publisher = PublisherBuilder::new(cfg.clone())
                .bind_cfg(self.bind)
                .build()
                .await
                .context("creating publisher")?;
            let subscriber = SubscriberBuilder::new(cfg)
                .desired_auth(auth)
                .build()
                .context("creating subscriber")?;
            Ok::<_, anyhow::Error>((publisher, subscriber))
        };
        match res.await {
            Ok(ps) => Ok(ps),
            Err(e) => {
                eprintln!("netidx initialization failed {e:?}");
                eprintln!("netidx will be process internal only");
                eprintln!("to fix this see https://netidx.github.io/netidx-book");
                static NETIDX: OnceLock<InternalOnly> = OnceLock::new();
                if let Err(_) = NETIDX.set(InternalOnly::new().await?) {
                    panic!("BUG: NETIDX static set multiple times")
                }
                let env = NETIDX.get().unwrap();
                Ok((env.publisher().clone(), env.subscriber().clone()))
            }
        }
    }
}

fn parse_package_arg(s: &str) -> PackageId {
    match s.split_once('@') {
        Some((name, version)) => PackageId::new(name, Some(version)),
        None => PackageId::new(s, None),
    }
}

#[tokio::main]
async fn handle_package(action: PackageAction) -> Result<()> {
    match action {
        PackageAction::Add { packages, skip_crates_io_check, path } => {
            let pm = GraphixPM::new().await?;
            let ids: Vec<_> = if let Some(ref path) = path {
                packages
                    .iter()
                    .map(|s| {
                        let name = s.split('@').next().unwrap();
                        PackageId::with_path(name, path.clone())
                    })
                    .collect()
            } else {
                packages.iter().map(|s| parse_package_arg(s)).collect()
            };
            pm.add_packages(&ids, skip_crates_io_check).await
        }
        PackageAction::Remove { packages } => {
            let pm = GraphixPM::new().await?;
            let ids: Vec<_> = packages.iter().map(|s| PackageId::new(s, None)).collect();
            pm.remove_packages(&ids).await
        }
        PackageAction::Search { query } => {
            let pm = GraphixPM::new().await?;
            pm.search(&query).await
        }
        PackageAction::List => {
            let pm = GraphixPM::new().await?;
            pm.list().await
        }
        PackageAction::Rebuild => {
            let pm = GraphixPM::new().await?;
            pm.do_rebuild().await
        }
        PackageAction::Create { name, dir } => {
            let full_name = if name.starts_with("graphix-package-") {
                name
            } else {
                format!("graphix-package-{name}")
            };
            graphix_package::create_package(&dir, &full_name).await
        }
        PackageAction::Update => {
            let pm = GraphixPM::new().await?;
            pm.update().await
        }
        PackageAction::BuildStandalone { source_override } => {
            let pm = GraphixPM::new().await?;
            let cwd = std::env::current_dir().context("getting current directory")?;
            pm.build_standalone(&cwd, source_override.as_deref()).await
        }
    }
}

fn tokio_main(
    p: Params,
    cfg: Result<Config>,
    run_on_main: MainThreadHandle,
) -> Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("building tokio runtime")?;
    rt.block_on(async move {
        if let Some(dir) = &p.log_dir {
            let _ = Logger::try_with_env()
                .context("initializing log")?
                .log_to_file(
                    FileSpec::default()
                        .directory(dir)
                        .basename("graphix")
                        .use_timestamp(false),
                )
                .start()
                .context("starting log")?;
        }
        info!("graphix shell starting");
        let mut _internal = None;
        let (publisher, subscriber) = if p.no_netidx {
            let i = InternalOnly::new().await?;
            let (p, s) = (i.publisher().clone(), i.subscriber().clone());
            _internal = Some(i);
            (p, s)
        } else {
            p.get_pub_sub(cfg).await?
        };
        let mut shell = ShellBuilder::<NoExt>::default();
        let program_args: Vec<ArcStr> =
            p.program_args.iter().map(|s| ArcStr::from(s.as_str())).collect();
        shell = shell.program_args(program_args);
        shell = shell.no_init(p.no_init);
        if let Some(t) = p.publish_timeout {
            shell = shell.publish_timeout(Duration::from_secs(t));
        }
        if let Some(t) = p.resolve_timeout {
            shell = shell.resolve_timeout(Duration::from_secs(t));
        }
        if p.file.is_none() && p.check {
            bail!("check mode requires a file to check")
        }
        if let Some(f) = &p.file {
            let source = match f.strip_prefix("netidx:") {
                Some(path) => {
                    shell = shell.module_resolvers(vec![ModuleResolver::Netidx {
                        subscriber: subscriber.clone(),
                        base: netidx::path::Path::from(ArcStr::from(path)),
                        timeout: None,
                    }]);
                    Source::Netidx(Path::from(ArcStr::from(path)))
                }
                None => {
                    let path = PathBuf::from(&**f).canonicalize()?;
                    let path = if path.is_dir() { path.join("main.gx") } else { path };
                    match path.parent() {
                        Some(p) if p.as_os_str().is_empty() => (),
                        None => (),
                        Some(p) => {
                            let p = PathBuf::from(p);
                            shell =
                                shell.module_resolvers(vec![ModuleResolver::Files(p)]);
                        }
                    };
                    Source::File(path)
                }
            };
            let mode = if p.check { Mode::Check(source) } else { Mode::Script(source) };
            shell = shell.mode(mode);
        }
        let (enable, disable) = RawFlag::as_flags(&p.warn);
        shell
            .publisher(publisher)
            .subscriber(subscriber)
            .enable_flags(enable)
            .disable_flags(disable)
            .build()?
            .run(run_on_main)
            .await
    })
}

fn handle_compile(
    input: PathBuf,
    out_dir: PathBuf,
    name: Option<String>,
    workspace_root: Option<PathBuf>,
    build: bool,
    dev: bool,
) -> Result<()> {
    use graphix_compiler::expr::parser;
    use graphix_compiler::expr::{Expr, ExprKind, Origin, Source as ExprSource};
    use graphix_compiler::fusion;
    use std::fs;
    use std::fmt::Write as _;

    // Verification step: typecheck the program before any fusion.
    // `check_with_types` runs the real graphix typechecker (same
    // one the interpreter uses at runtime) and returns both the
    // parsed `Expr` tree and a FxHashMap<ExprId, FnType>. Fusion
    // rewrites the returned tree — since ExprIds match the types
    // map, lookups are exact. This replaces the ad-hoc
    // PrimType::from_type / infer_body_rtype / known_hof_callback_args
    // heuristics the fusion emitter used before.
    let (exprs, fn_types) = run_pre_fusion_typecheck(input.clone())
        .context("pre-fusion typecheck failed; fix type errors before compiling")?;
    // `exprs` is what check returned — already resolved + typechecked.
    // The top-level parser is no longer invoked in this function, so
    // suppress its unused-import warning with a throwaway reference.
    let _ = (&parser::parse, &Origin::default, &ExprSource::default);

    // Seed the rewrite state with the typechecker's fn_types so the
    // fusion pass reads real FnTypes for Lambda and Apply sites
    // instead of its ad-hoc heuristics.
    let mut state = fusion::RewriteState::with_fn_types(fn_types);
    let short_name = match name {
        Some(n) => n,
        None => {
            let stem = input
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("compiled");
            stem.chars()
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                .collect::<String>()
                .to_ascii_lowercase()
        }
    };
    let prefix = short_name.clone();
    // Apply the rewrite to each top-level expression. `state` carries
    // the fn_types map from the typecheck pass plus the cross-expr
    // kernel / constant registries.
    let mut rewritten: Vec<Expr> = exprs.iter().cloned().collect();
    for e in &mut rewritten {
        fusion::rewrite_program_with_state(e, &prefix, &mut state);
    }
    let kernels = state.kernels;
    println!("Fused {} lambda(s).", kernels.len());
    for k in &kernels {
        println!("  - {} → {}", k.fn_name, k.builtin_name);
    }
    let mut main_gx = String::new();
    for (i, e) in rewritten.iter().enumerate() {
        if i > 0 {
            main_gx.push_str(";\n");
        }
        write!(main_gx, "{}", e.kind).ok();
    }
    main_gx.push('\n');
    // Silence "unused" warnings for ExprKind since the variant is
    // exhaustively handled by the Display impl.
    let _ = ExprKind::NoOp;
    let pkg = fusion::emit_package(&short_name, kernels, main_gx);
    let workspace_root = match workspace_root {
        Some(p) => p.canonicalize().with_context(|| {
            format!("canonicalizing workspace root {}", p.display())
        })?,
        None => find_workspace_root().context(
            "could not auto-detect graphix workspace root; \
             please pass --workspace-root",
        )?,
    };
    let workspace_str = workspace_root
        .to_str()
        .context("workspace root path is not valid UTF-8")?;
    fs::create_dir_all(&out_dir)
        .with_context(|| format!("creating {}", out_dir.display()))?;
    fusion::write_package(&pkg, &out_dir, workspace_str)
        .with_context(|| format!("writing package to {}", out_dir.display()))?;
    println!(
        "Wrote package 'graphix-package-{}' to {}",
        pkg.short_name,
        out_dir.display()
    );
    if build {
        println!("Preparing isolated build source tree...");
        let build_src = prepare_shell_source_copy(&workspace_root)?;
        println!(
            "Building standalone binary in {} (profile: {}, this may take a while)...",
            build_src.display(),
            if dev { "dev" } else { "release (LTO)" }
        );
        build_standalone_direct(
            &build_src,
            &out_dir,
            &pkg.short_name,
            &workspace_root,
            dev,
        )?;
        println!(
            "Built; cleaning up temporary source copy at {}.",
            build_src.display()
        );
        let _ = std::fs::remove_dir_all(&build_src);
    } else {
        println!(
            "\nNext steps:\n  cd {dir}\n  graphix package build-standalone \
             --source-override {ws}",
            dir = out_dir.display(),
            ws = workspace_root.display(),
        );
        println!(
            "Or re-run with --build to invoke the standalone build \
             pipeline inline (safe: copies graphix-shell source to a \
             temp dir first, so the workspace stays untouched)."
        );
    }
    Ok(())
}

/// Build a throwaway copy of graphix-shell's source tree that
/// `build_standalone` can mutate freely. The copy lives under
/// `$TMPDIR/graphix-build-<suffix>/` and contains:
///   - `Cargo.toml` with `path = "../foo"` rewritten to absolute
///     paths AND `{ workspace = true }` inheritance replaced by the
///     concrete version specs read from the real workspace root,
///   - `src/` copied verbatim.
/// Returns the path to the copied directory — caller is responsible
/// for `remove_dir_all` after the build completes.
fn prepare_shell_source_copy(workspace_root: &std::path::Path) -> Result<PathBuf> {
    use std::fs;
    let shell_dir = workspace_root.join("graphix-shell");
    if !shell_dir.is_dir() {
        bail!("expected graphix-shell directory at {}", shell_dir.display());
    }
    let suffix: u128 = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let target = std::env::temp_dir().join(format!("graphix-build-{suffix}"));
    if target.exists() {
        fs::remove_dir_all(&target)?;
    }
    fs::create_dir_all(&target)?;

    // Copy src/ wholesale.
    let src_out = target.join("src");
    fs::create_dir_all(&src_out)?;
    copy_dir_recursive(&shell_dir.join("src"), &src_out)?;

    // Read the workspace root's `[workspace.dependencies]` so we can
    // materialize `{ workspace = true }` inheritance in the shell's
    // Cargo.toml (temp dir isn't part of a workspace, so inheritance
    // wouldn't resolve).
    let workspace_deps = load_workspace_deps(workspace_root)
        .context("reading workspace.dependencies from root Cargo.toml")?;

    // Copy Cargo.toml with path-dep + workspace-inheritance rewrites.
    let toml_in = shell_dir.join("Cargo.toml");
    let toml_text = fs::read_to_string(&toml_in)
        .with_context(|| format!("reading {}", toml_in.display()))?;
    let patched = rewrite_shell_cargo_toml(&toml_text, workspace_root, &workspace_deps)
        .context("patching shell Cargo.toml for temp build")?;
    fs::write(target.join("Cargo.toml"), patched)
        .with_context(|| format!("writing {}/Cargo.toml", target.display()))?;
    Ok(target)
}

/// Read `[workspace.dependencies]` from `<workspace>/Cargo.toml`.
/// Returns a map from dependency name to its resolved spec string
/// suitable for use on the RHS of `name = ...`. Relative `path = ".."`
/// entries are canonicalized to absolute paths so they still resolve
/// from the temp build dir.
fn load_workspace_deps(
    workspace_root: &std::path::Path,
) -> Result<std::collections::BTreeMap<String, String>> {
    use toml_edit::{DocumentMut, InlineTable, Item, Value};
    let path = workspace_root.join("Cargo.toml");
    let text = std::fs::read_to_string(&path)
        .with_context(|| format!("reading {}", path.display()))?;
    let doc: DocumentMut = text.parse().context("parsing workspace Cargo.toml")?;
    let deps = doc
        .get("workspace")
        .and_then(|w| w.get("dependencies"))
        .and_then(|d| d.as_table())
        .ok_or_else(|| anyhow::anyhow!("no [workspace.dependencies] in root toml"))?;

    fn absolutize_path_in_table(t: &mut InlineTable, workspace_root: &std::path::Path) {
        let Some(val) = t.get_mut("path") else { return };
        let Some(s) = val.as_str() else { return };
        if s.starts_with('/') {
            return;
        }
        let resolved = workspace_root.join(s);
        if let Some(rs) = resolved.to_str() {
            *val = Value::from(rs);
        }
    }

    let mut out = std::collections::BTreeMap::new();
    for (k, v) in deps.iter() {
        let rhs = match v {
            Item::Value(Value::String(_)) => v.as_value().unwrap().to_string(),
            Item::Value(Value::InlineTable(t)) => {
                let mut t = t.clone();
                absolutize_path_in_table(&mut t, workspace_root);
                Value::InlineTable(t).to_string()
            }
            Item::Table(t) => {
                let mut inline = InlineTable::new();
                for (kk, vv) in t.iter() {
                    if let Item::Value(val) = vv {
                        inline.insert(kk, val.clone());
                    }
                }
                absolutize_path_in_table(&mut inline, workspace_root);
                Value::InlineTable(inline).to_string()
            }
            _ => continue,
        };
        out.insert(k.to_string(), rhs.trim().to_string());
    }
    Ok(out)
}

/// Rewrite shell's Cargo.toml for the temp-dir build:
///   1. `path = "../foo"` → `path = "<workspace>/foo"` so path deps
///      still resolve from a dir outside the original workspace.
///   2. `name = { workspace = true[, ...] }` → `name = <resolved>`,
///      using the concrete spec from the workspace's
///      `[workspace.dependencies]` table.
fn rewrite_shell_cargo_toml(
    text: &str,
    workspace_root: &std::path::Path,
    workspace_deps: &std::collections::BTreeMap<String, String>,
) -> Result<String> {
    use toml_edit::{DocumentMut, Item};
    let ws = workspace_root.to_string_lossy();
    // Step 1: naive path-dep rewrite via text substitution (runs
    // before parse so it doesn't confuse the toml structure).
    let text = text.replace("path = \"../", &format!("path = \"{ws}/"));
    // Step 2: parse and rewrite `{ workspace = true }` inheritance.
    let mut doc: DocumentMut = text.parse().context("parsing shell Cargo.toml")?;
    rewrite_workspace_inherit(
        doc.get_mut("dependencies").and_then(|i| i.as_table_mut()),
        workspace_deps,
    )?;
    rewrite_workspace_inherit(
        doc.get_mut("dev-dependencies").and_then(|i| i.as_table_mut()),
        workspace_deps,
    )?;
    rewrite_workspace_inherit(
        doc.get_mut("build-dependencies").and_then(|i| i.as_table_mut()),
        workspace_deps,
    )?;
    // Step 3: if there's an `[lints] workspace = true`, drop it —
    // the temp crate isn't part of a workspace.
    if let Some(Item::Table(lints)) = doc.get_mut("lints") {
        lints.remove("workspace");
        if lints.is_empty() {
            doc.remove("lints");
        }
    }
    Ok(doc.to_string())
}

fn rewrite_workspace_inherit(
    deps: Option<&mut toml_edit::Table>,
    workspace_deps: &std::collections::BTreeMap<String, String>,
) -> Result<()> {
    use toml_edit::Item;
    let Some(deps) = deps else { return Ok(()) };
    // Collect the names to rewrite first; can't mutate while iterating.
    let names: Vec<String> = deps.iter().map(|(k, _)| k.to_string()).collect();
    for name in names {
        let inherits = match deps.get(&name) {
            Some(Item::Value(toml_edit::Value::InlineTable(t))) => t
                .get("workspace")
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            _ => false,
        };
        if !inherits {
            continue;
        }
        let spec = workspace_deps.get(&name).ok_or_else(|| {
            anyhow::anyhow!("dep `{name}` inherits from workspace but is missing")
        })?;
        // Build a concrete replacement by parsing the serialized
        // spec — it'll be either a bare version string or an inline
        // table like `{ version = "1", features = [...] }`.
        let parsed: toml_edit::Value = format!("x = {spec}\n")
            .parse::<toml_edit::DocumentMut>()
            .context("reparsing resolved spec")?
            .get("x")
            .cloned()
            .and_then(|i| match i {
                Item::Value(v) => Some(v),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("failed to reparse resolved spec"))?;
        // Preserve original `features`/`optional` that the inheriting
        // site added on top of the workspace spec, by merging them
        // into the resolved spec's inline table.
        let mut resolved_table = match parsed {
            toml_edit::Value::String(s) => {
                let mut t = toml_edit::InlineTable::new();
                t.insert("version", toml_edit::Value::String(s));
                t
            }
            toml_edit::Value::InlineTable(t) => t,
            _ => continue,
        };
        if let Some(Item::Value(toml_edit::Value::InlineTable(orig))) =
            deps.get(&name)
        {
            for (k, v) in orig.iter() {
                if k == "workspace" {
                    continue;
                }
                resolved_table.insert(k, v.clone());
            }
        }
        deps[&name] = Item::Value(toml_edit::Value::InlineTable(resolved_table));
    }
    Ok(())
}

fn copy_dir_recursive(src: &std::path::Path, dst: &std::path::Path) -> Result<()> {
    use std::fs;
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let to = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_recursive(&entry.path(), &to)?;
        } else if ty.is_file() {
            fs::copy(entry.path(), &to)?;
        }
    }
    Ok(())
}

// Note: `rewrite_shell_cargo_toml` does the full parse-and-rewrite
// path-dep + workspace-inheritance patching; the naive text-only
// substitution is kept inline there and no separate helper is needed.

/// Custom build path for `graphix compile --build`. Skips the
/// `GraphixPM::build_standalone` pipeline because that one strips
/// every `graphix-package-*` dep from graphix-shell's Cargo.toml and
/// re-adds only the user package plus `core` — leaving us without the
/// stdlib packages a typical program needs (array, sys, etc.). Here we
/// keep the existing stdlib deps, append our emitted package, patch
/// deps.rs to register it, then run `cargo build --release --features
/// graphix-package-<short>/standalone`.
fn build_standalone_direct(
    build_src: &std::path::Path,
    package_dir: &std::path::Path,
    short_name: &str,
    _workspace_root: &std::path::Path,
    dev: bool,
) -> Result<()> {
    use std::fs;
    use std::process::Command;
    use toml_edit::{DocumentMut, InlineTable, Item, Value};

    let package_dir = package_dir
        .canonicalize()
        .with_context(|| format!("canonicalizing {}", package_dir.display()))?;
    let crate_name = format!("graphix-package-{short_name}");

    // 1. Append our package as a dep to build_src/Cargo.toml (without
    //    removing the existing graphix-package-* deps). Also override
    //    the dev profile to strip debuginfo — default dev profile
    //    produces ~1.5GB binaries (debuginfo=2 propagated to all
    //    deps), which fills a 28GB tmpfs fast across multiple
    //    differential-test runs.
    let cargo_toml_path = build_src.join("Cargo.toml");
    let text = fs::read_to_string(&cargo_toml_path)
        .with_context(|| format!("reading {}", cargo_toml_path.display()))?;
    let mut doc: DocumentMut = text.parse().context("parsing shell Cargo.toml")?;
    {
        let deps = doc
            .get_mut("dependencies")
            .and_then(|i| i.as_table_mut())
            .ok_or_else(|| anyhow::anyhow!("shell Cargo.toml has no [dependencies]"))?;
        let mut entry = InlineTable::new();
        entry.insert("path", Value::from(package_dir.to_str().unwrap()));
        deps[&crate_name] = Item::Value(Value::InlineTable(entry));
    }
    if dev {
        // Inject a dev-profile override that matches the workspace's
        // convention: `opt-level = "s"` + `lto = "thin"` + `debug = 0`.
        // - debug = 0 keeps the binary small (dev default = 1.5GB;
        //   this gets it under ~250MB).
        // - opt-level = "s" is NOT for speed: the graphix parser
        //   (combine-based) overflows stack without some opt. See
        //   the note in CLAUDE.md. Without this, any program bigger
        //   than a trivial expression crashes at startup with "stack
        //   overflow, aborting" when the embedded main.gx gets
        //   reparsed.
        // - lto = "thin" shaves another few seconds off build time.
        let profile_toml = "\n[profile.dev]\nopt-level = \"s\"\nlto = \"thin\"\ndebug = 0\n";
        let existing = doc.to_string();
        if !existing.contains("[profile.dev]") {
            let with_profile = format!("{existing}{profile_toml}");
            fs::write(&cargo_toml_path, with_profile)
                .with_context(|| format!("writing {}", cargo_toml_path.display()))?;
        } else {
            fs::write(&cargo_toml_path, doc.to_string())
                .with_context(|| format!("writing {}", cargo_toml_path.display()))?;
        }
    } else {
        fs::write(&cargo_toml_path, doc.to_string())
            .with_context(|| format!("writing {}", cargo_toml_path.display()))?;
    }

    // 2. Patch deps.rs so the generated shell registers our package
    //    alongside the existing stdlib. The existing deps.rs already
    //    has entries for all stdlib crates (it's the real shell's
    //    deps.rs, copied verbatim), so we just splice one more line
    //    in the right place. The "right place" is: after the last
    //    existing `register` call and before the `main_program`
    //    collection; we insert next to the other registrations.
    let deps_rs_path = build_src.join("src").join("deps.rs");
    let deps_text = fs::read_to_string(&deps_rs_path)
        .with_context(|| format!("reading {}", deps_rs_path.display()))?;
    let crate_ident = crate_name.replace('-', "_");
    let register_line = format!(
        "    {crate_ident}::P::register(ctx, modules, &mut root_mods)?;\n"
    );
    let main_program_block = format!(
        "    let mut main_program: Option<&'static str> = None;\n\
         \x20   if let Some(m) = <{crate_ident}::P as Package<X>>::main_program() {{\n\
         \x20       main_program = Some(m);\n\
         \x20   }}\n"
    );
    let maybe_custom_line = format!(
        "    try_pkg!({crate_ident}::P);\n"
    );
    // Insert before the `let mut parts = ...` block (which runs after
    // all registers).
    let deps_text =
        if let Some(insert_pos) = deps_text.find("    let mut parts") {
            let (before, after) = deps_text.split_at(insert_pos);
            format!("{before}{register_line}{after}")
        } else {
            bail!("couldn't find register insertion point in deps.rs")
        };
    // The shell's hand-written deps.rs passes `main_program: None`
    // hardcoded; to make the standalone feature effective we replace
    // that with a fresh `let mut main_program = None` + per-package
    // check + the final `main_program` pickup.
    let deps_text = deps_text.replace(
        "Ok(RegisterResult { root: ArcStr::from(parts.join(\";\\n\")), main_program: None })",
        &format!(
            "{main_program_block}\
             \x20   Ok(RegisterResult {{ root: ArcStr::from(parts.join(\";\\n\")), main_program }})",
        ),
    );
    // Sanity check — if the exact substring changes in the future,
    // bail with a clear error rather than silently producing a
    // REPL-only binary.
    if !deps_text.contains("main_program: main_program")
        && !deps_text.contains("main_program }")
    {
        bail!(
            "couldn't splice main_program into deps.rs — the exact \
             hardcoded `main_program: None` line may have changed"
        );
    }
    // try_pkg! is gated by a macro that's defined inside
    // `maybe_init_custom`. Insert right before `Ok(CustomResult::...`.
    let deps_text =
        if let Some(insert_pos) = deps_text.find("    Ok(CustomResult::NotCustom")
        {
            let (before, after) = deps_text.split_at(insert_pos);
            format!("{before}{maybe_custom_line}{after}")
        } else {
            deps_text
        };
    fs::write(&deps_rs_path, &deps_text)
        .with_context(|| format!("writing {}", deps_rs_path.display()))?;

    // 3. Invoke cargo build with the standalone feature. Target dir
    //    logic:
    //    - If the caller set `GRAPHIX_BUILD_TARGET`, use that (for
    //      batched differential testing — keeping a shared target
    //      dir across many `--build` runs means dep crates only
    //      compile once, cutting per-program rebuild to ~10-20s).
    //    - Otherwise use a per-invocation dir inside build_src so
    //      each build is self-contained and doesn't conflict with
    //      another concurrent --build. Importantly, NEVER use the
    //      workspace's own target dir — that would clobber the
    //      dev-mode `target/debug/graphix` binary the user is
    //      invoking this command with.
    let target_dir = match std::env::var_os("GRAPHIX_BUILD_TARGET") {
        Some(p) => PathBuf::from(p),
        None => build_src.join("target"),
    };
    std::fs::create_dir_all(&target_dir).ok();
    let profile_subdir = if dev { "debug" } else { "release" };
    let mut cmd = Command::new("cargo");
    cmd.arg("build");
    if !dev {
        cmd.arg("--release");
    }
    cmd.arg("--features")
        .arg(format!("{crate_name}/standalone"))
        .current_dir(build_src)
        .env("CARGO_TARGET_DIR", &target_dir);
    let status = cmd.status().context("running cargo build")?;
    if !status.success() {
        bail!(
            "cargo build {} failed: {status}",
            if dev { "(dev)" } else { "--release" }
        );
    }
    // 4. Copy binary out.
    let built = target_dir.join(profile_subdir).join(format!(
        "graphix{}",
        std::env::consts::EXE_SUFFIX
    ));
    let dest = package_dir.join(format!("{short_name}{}", std::env::consts::EXE_SUFFIX));
    fs::copy(&built, &dest).with_context(|| {
        format!("copying {} to {}", built.display(), dest.display())
    })?;
    println!("Binary written to {}", dest.display());
    Ok(())
}

/// Spin up an in-process graphix runtime (with internal-only netidx
/// stubs + all stdlib packages registered), run `check_with_types`
/// on the input file, and return the parsed + typechecked `Expr`
/// tree together with the typechecker's `fn_types` map. If the
/// typecheck fails the error is propagated. The runtime drops at
/// function exit.
fn run_pre_fusion_typecheck(
    input: PathBuf,
) -> Result<(
    triomphe::Arc<[graphix_compiler::expr::Expr]>,
    fxhash::FxHashMap<
        graphix_compiler::expr::ExprId,
        graphix_compiler::typ::FnType,
    >,
)> {
    use graphix_compiler::{
        expr::{ModuleResolver, Source},
        CFlag,
    };
    use graphix_rt::{GXConfig, GXRt, NoExt};
    use netidx::InternalOnly;

    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("creating tokio runtime for typecheck")?;
    rt.block_on(async move {
        let internal = InternalOnly::new().await?;
        let publisher = internal.publisher().clone();
        let subscriber = internal.subscriber().clone();
        let mut ctx = graphix_compiler::ExecCtx::new(GXRt::<NoExt>::new(
            publisher,
            subscriber.clone(),
        ))
        .context("creating graphix context")?;
        let mut vfs_modules = fxhash::FxHashMap::default();
        let reg = graphix_shell_deps_register::<NoExt>(&mut ctx, &mut vfs_modules)
            .context("registering stdlib packages")?;
        let resolvers: Vec<ModuleResolver> = vec![ModuleResolver::VFS(vfs_modules)];
        let (tx, _rx) = tokio::sync::mpsc::channel(100);
        let flags = CFlag::WarnUnhandled | CFlag::WarnUnused;
        let handle = GXConfig::builder(ctx, tx)
            .flags(flags)
            .root(reg.root)
            .resolvers(resolvers)
            .build()
            .context("building GXConfig for typecheck")?
            .start()
            .await
            .context("starting graphix runtime for typecheck")?;
        let canonical = std::fs::canonicalize(&input)
            .with_context(|| format!("canonicalizing {}", input.display()))?;
        // Need the file's directory on the module search path so `mod foo;`
        // and `use foo;` statements in the source can resolve sibling files.
        // (Replicates what graphix-shell does with ModuleResolver::Files.)
        let _ = canonical.parent(); // not wiring yet; stdlib VFS covers typical cases
        let source = Source::File(canonical);
        let (exprs, types) = handle
            .check_with_types(source)
            .await
            .context("typechecking program")?;
        Ok((exprs, types))
    })
}

/// Wrapper around `deps::register` so `run_pre_fusion_typecheck` can
/// call into graphix-shell's package-registration logic.
fn graphix_shell_deps_register<X: graphix_rt::GXExt>(
    ctx: &mut graphix_compiler::ExecCtx<
        graphix_rt::GXRt<X>,
        <X as graphix_rt::GXExt>::UserEvent,
    >,
    modules: &mut fxhash::FxHashMap<netidx::path::Path, arcstr::ArcStr>,
) -> Result<graphix_shell::deps::RegisterResult> {
    graphix_shell::deps::register(ctx, modules)
}

/// Walk up from the running binary looking for a `Cargo.toml` that
/// describes the graphix workspace. Returns the directory containing
/// it. This is best-effort; failure mode is "pass --workspace-root".
fn find_workspace_root() -> Result<PathBuf> {
    let exe = std::env::current_exe().context("locating current exe")?;
    let mut dir = exe
        .parent()
        .context("exe has no parent directory")?
        .to_path_buf();
    for _ in 0..6 {
        let cargo_toml = dir.join("Cargo.toml");
        if cargo_toml.exists() {
            let contents = std::fs::read_to_string(&cargo_toml).unwrap_or_default();
            if contents.contains("graphix-compiler") && contents.contains("[workspace]") {
                return Ok(dir);
            }
        }
        if let Some(parent) = dir.parent() {
            dir = parent.to_path_buf();
        } else {
            break;
        }
    }
    // Fallback: CWD
    std::env::current_dir().context("getting CWD as fallback workspace root")
}

fn main() -> Result<()> {
    Config::maybe_run_machine_local_resolver()?;
    let p = Params::parse();
    match p.command {
        Some(Command::Package { action }) => return handle_package(action),
        Some(Command::Lsp) => return graphix_shell::lsp_backend::run(),
        Some(Command::Compile {
            input,
            out_dir,
            name,
            workspace_root,
            build,
            dev,
        }) => return handle_compile(input, out_dir, name, workspace_root, build, dev),
        None => (),
    }
    let cfg = match &p.config {
        None => Config::load_default_or_local_only(),
        Some(p) => Config::load(p),
    };
    let (handle, main_rx) = MainThreadHandle::new();
    let tokio_handle = std::thread::Builder::new()
        .name("graphix-tokio".into())
        .spawn(move || tokio_main(p, cfg, handle))
        .expect("failed to spawn tokio thread");
    while let Ok(f) = main_rx.recv() {
        f();
    }
    tokio_handle.join().map_err(|_| anyhow::anyhow!("tokio thread panicked"))?
}
