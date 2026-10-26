use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use clap::{Args, Parser, Subcommand};
use enumflags2::BitFlags;
use flexi_logger::{Duplicate, FileSpec, Logger};
use graphix_compiler::{
    CFlag, FAILURE_TARGET,
    expr::{FilesResolver, Load},
};
use graphix_package::{GraphixPM, MainThreadHandle, PackageId};
use graphix_package_core::{NetConfig, NetTimeouts};
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use log::info;
use netidx::{
    config::Config,
    path::Path,
    publisher::{BindCfg, DesiredAuth, Publisher, PublisherBuilder},
    subscriber::{Subscriber, SubscriberBuilder},
};
use std::{mem, path::PathBuf, str::FromStr, time::Duration};

#[derive(Debug, Clone, Copy)]
enum RawFlag {
    Unhandled,
    NoUnhandled,
    Error,
    NoError,
}

impl FromStr for RawFlag {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "unhandled" => Ok(Self::Unhandled),
            "no-unhandled" => Ok(Self::NoUnhandled),
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
        /// Remove the packages that depend on them without prompting
        #[arg(short = 'y', long = "yes")]
        yes: bool,
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
    /// Update graphix and its packages to their latest versions
    Update {
        /// Apply all available changes without prompting (for scripts/CI)
        #[arg(short = 'y', long = "yes")]
        yes: bool,
    },
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
    /// Format graphix source: each named file in place, or stdin to
    /// stdout when no file is named. Width and indent come from the
    /// nearest graphixfmt.json at or above the file, else the one in the
    /// user's configuration directory under graphix/, else 90 and 4
    Fmt {
        /// the .gx and .gxi files to format
        files: Vec<PathBuf>,
        /// write nothing; list the files that would change and fail if any would
        #[arg(long, conflicts_with = "stdout")]
        check: bool,
        /// print the formatted text instead of rewriting the files
        #[arg(long)]
        stdout: bool,
        /// stdin is an interface (.gxi)
        #[arg(long)]
        interface: bool,
        /// the line width to fit, over graphixfmt.json's
        #[arg(long)]
        width: Option<usize>,
        /// the spaces one level of nesting indents by, over graphixfmt.json's
        #[arg(long)]
        indent: Option<usize>,
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
    #[command(flatten)]
    netidx: NetArgs,
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
    /// neither read nor write the image cache
    #[arg(long = "no-cache")]
    no_cache: bool,
    /// write the image cache and exit
    #[arg(long, conflicts_with_all = ["no_cache", "check", "expand"])]
    warm: bool,
    /// disable JIT fusion and run the node-walk interpreter only
    #[arg(long = "no-fusion")]
    no_fusion: bool,
    /// after compiling the script, print its fusion profile (regions
    /// attempted/fused and per-region blocker reasons) to stderr
    #[arg(long = "fusion-stats", conflicts_with = "check")]
    fusion_stats: bool,
    /// do not execute the program, just veryify that it compiles and
    /// type checks.
    #[arg(long = "check")]
    check: bool,
    /// check the program and print each seq's lowered machine
    #[arg(long = "expand")]
    expand: bool,
    /// enable or disable compiler flags. Currently supported flags are,
    /// - unhandled, no-unhandled: warn about unhandled ? operators (default)
    /// - error, no-error makes warnings errors
    ///
    /// the no- variant turns the flag off. If both are specifed the no- variant
    /// always wins
    #[arg(short = 'W')]
    warn: Vec<RawFlag>,
    /// the program to run instead of starting the REPL (a file, or
    /// netidx:/path), then the arguments passed to it: everything after
    /// the program is the program's
    // XCR claude for claude: [bug/medium] trailing_var_arg only applies once
    // program_args has a value, so a first program argument that names a graphix option
    // or subcommand is taken by graphix. `graphix cli.gx --help` shows graphix's help,
    // `graphix cli.gx -n 3 foo` turns on --no-netidx and the script sees ["3", "foo"],
    // and `graphix cli.gx fmt x` runs the formatter on x in place. Fold `file` into
    // this positional (a Vec whose first element is the script) so everything after the
    // filename is the program's, as the help text promises. probe:
    // design/review-2026-10-26/repro/examples-rest-program-args-intercepted.sh
    // (examples-rest-02)
    // 2026-10-26 claude: The script and its arguments are one trailing positional
    // (Params::program, the script first), so everything after the script is the
    // script's: a graphix option or subcommand name included; main splits it into the
    // file and program_args, and a standalone binary keeps every argument as the
    // program's. Pin: graphix-shell tests/program_args.rs
    // arguments_after_the_script_are_the_scripts (--help, -n 3, fmt x, --check after
    // the script).
    #[arg(
        trailing_var_arg = true,
        allow_hyphen_values = true,
        value_name = "FILE [ARGS]"
    )]
    program: Vec<String>,
}

/// The netidx a run uses, loaded when the program first touches it.
#[derive(Args, Clone)]
struct NetArgs {
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
    /// the netidx kerberos service principal name, otherwise the
    /// default from the current user's cached ticket, only valid if
    /// using kerberos auth
    #[arg(long)]
    spn: Option<String>,
    /// the netidx tls identity to use, otherwise use the configured
    /// default, only valid if using tls auth.
    #[arg(long)]
    identity: Option<String>,
}

impl NetArgs {
    fn load(&self) -> Result<(Config, DesiredAuth)> {
        let cfg = match &self.config {
            None => Config::load_default_or_local_only(),
            Some(p) => Config::load(p),
        }
        .context("loading the netidx config")?;
        let auth = match self.auth.clone().unwrap_or_else(|| cfg.default_auth()) {
            DesiredAuth::Krb5 { .. } => {
                DesiredAuth::Krb5 { upn: self.upn.clone(), spn: self.spn.clone() }
            }
            DesiredAuth::Tls { .. } => {
                DesiredAuth::Tls { identity: self.identity.clone() }
            }
            auth @ (DesiredAuth::Anonymous | DesiredAuth::Local) => auth,
        };
        if !matches!(auth, DesiredAuth::Krb5 { .. })
            && (self.upn.is_some() || self.spn.is_some())
        {
            bail!("--upn and --spn are only valid with kerberos auth")
        }
        if !matches!(auth, DesiredAuth::Tls { .. }) && self.identity.is_some() {
            bail!("--identity is only valid with tls auth")
        }
        Ok((cfg, auth))
    }

    async fn pub_sub(&self, bind: Option<BindCfg>) -> Result<(Publisher, Subscriber)> {
        // XCR claude for claude: [bug/high] With no netidx config file, `graphix
        // netidx:/path` panics ("Cannot start a runtime from within a runtime"). This
        // load runs on the async main runtime, and Config::load_default_or_local_only
        // falls back to Config::local_only, which builds a runtime and block_on's it.
        // That is the zero-config local setup install.md advertises. Other netidx users
        // are unaffected because NetState loads on a thread of its own. Run this load
        // off the runtime the same way (a std::thread or spawn_blocking), or share
        // NetState's loader. probe:
        // design/review-2026-10-26/repro/book-core-netidx-script-panic.sh
        // (book-core-01)
        // a fallback to the machine-local resolver blocks on a runtime of its own
        // 2026-10-26 claude: NetArgs::pub_sub loads the config on a blocking-pool
        // thread (spawn_blocking), so the local-only fallback's block_on runs outside
        // the async runtime. Pin: graphix-shell tests/netidx_zero_config.rs
        // netidx_script_loads_without_a_config_file (no config, a held port standing in
        // for the local resolver; panicked before).
        let net = self.clone();
        let (cfg, auth) = tokio::task::spawn_blocking(move || net.load())
            .await
            .context("loading the netidx config")??;
        let publisher = PublisherBuilder::new(cfg.clone())
            .desired_auth(auth.clone())
            .bind_cfg(bind)
            .build()
            .await
            .context("creating publisher")?;
        let subscriber = SubscriberBuilder::new(cfg)
            .desired_auth(auth)
            .build()
            .context("creating subscriber")?;
        Ok((publisher, subscriber))
    }
}

fn parse_package_arg(s: &str) -> PackageId {
    match s.split_once('@') {
        Some((name, version)) => PackageId::new(name, Some(version)),
        None => PackageId::new(s, None),
    }
}

/// The package manager for this shell.
async fn pm() -> Result<GraphixPM> {
    GraphixPM::new(env!("CARGO_PKG_VERSION")).await
}

#[tokio::main]
async fn handle_package(action: PackageAction) -> Result<()> {
    match action {
        PackageAction::Add { packages, skip_crates_io_check, path } => {
            let pm = pm().await?;
            let ids: Vec<_> = if let Some(path) = path {
                match &packages[..] {
                    [name] if !name.contains('@') => {
                        vec![PackageId::with_path(name, path)]
                    }
                    _ => bail!("--path takes one package name, without a version"),
                }
            } else {
                packages.iter().map(|s| parse_package_arg(s)).collect()
            };
            pm.add_packages(&ids, skip_crates_io_check).await
        }
        PackageAction::Remove { packages, yes } => {
            let pm = pm().await?;
            let ids: Vec<_> = packages.iter().map(|s| PackageId::new(s, None)).collect();
            pm.remove_packages(&ids, yes).await
        }
        PackageAction::Search { query } => {
            let pm = pm().await?;
            pm.search(&query).await
        }
        PackageAction::List => {
            let pm = pm().await?;
            pm.list().await
        }
        PackageAction::Rebuild => {
            let pm = pm().await?;
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
        PackageAction::Update { yes } => {
            let pm = pm().await?;
            pm.update(yes).await
        }
        PackageAction::BuildStandalone { source_override } => {
            let pm = pm().await?;
            let cwd = std::env::current_dir().context("getting current directory")?;
            pm.build_standalone(&cwd, source_override.as_deref()).await
        }
    }
}

fn tokio_main(
    p: Params,
    file: Option<ArcStr>,
    program_args: Vec<ArcStr>,
    run_on_main: MainThreadHandle,
) -> Result<Option<i32>> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("building tokio runtime")?;
    let res = rt.block_on(async move {
        // a program's failures reach stderr with or without a log
        let _logger = match &p.log_dir {
            // RUST_LOG's spec, with the failure target always on
            Some(dir) => Logger::try_with_str(format!(
                "{}, {FAILURE_TARGET}=error",
                std::env::var("RUST_LOG").unwrap_or_else(|_| "off".into())
            ))
            .context("initializing log")?
                .log_to_file(
                    FileSpec::default()
                        .directory(dir)
                        .basename("graphix")
                        .use_timestamp(false),
                )
                .duplicate_to_stderr(Duplicate::Error)
                .start()
                .context("starting log")?,
            None => Logger::try_with_str(format!("off, {FAILURE_TARGET}=error"))
                .context("initializing log")?
                .log_to_stderr()
                .format(|w, _, record| write!(w, "{}", record.args()))
                .start()
                .context("starting log")?,
        };
        info!("graphix shell starting");
        // a netidx: script needs its subscriber to load; anything else
        // loads netidx when the program first touches it
        let net_config = if p.no_netidx {
            NetConfig::Internal
        } else if file.as_ref().is_some_and(|f| f.starts_with("netidx:")) {
            let (publisher, subscriber) = p.netidx.pub_sub(p.bind).await?;
            NetConfig::Ready { publisher, subscriber }
        } else {
            let net = p.netidx.clone();
            NetConfig::Load { load: std::sync::Arc::new(move || net.load()), bind: p.bind }
        };
        let mut shell = ShellBuilder::<NoExt>::default();
        shell = shell.program_args(program_args);
        shell = shell.no_init(p.no_init);
        shell = shell.cache(match (p.no_cache, p.warm) {
            (true, _) => CacheMode::Off,
            (false, true) => CacheMode::Warm,
            (false, false) => CacheMode::On,
        });
        shell = shell.fusion_stats(p.fusion_stats);
        {
            let net_config = net_config.clone();
            let publish = p.publish_timeout.map(Duration::from_secs);
            shell = shell.setup_context(Box::new(move |ctx| {
                ctx.libstate.set(net_config);
                ctx.libstate.set(NetTimeouts { publish });
            }));
        }
        #[cfg(feature = "sys")]
        {
            let mut factories = ahash::AHashMap::default();
            factories.insert(
                arcstr::literal!("netidx"),
                graphix_package_sys::loader::NetidxResolver::factory(
                    p.resolve_timeout.map(Duration::from_secs),
                ),
            );
            shell = shell.resolver_factories(factories);
        }
        let check = p.check || p.expand;
        if file.is_none() && check {
            bail!("check mode requires a file to check")
        }
        if let Some(f) = &file {
            let source = match f.strip_prefix("netidx:") {
                #[cfg(feature = "sys")]
                Some(path) => {
                    let subscriber = match &net_config {
                        NetConfig::Ready { subscriber, .. } => subscriber.clone(),
                        _ => {
                            bail!("loading a netidx: script requires netidx (remove --no-netidx)")
                        }
                    };
                    shell = shell.module_resolvers(vec![
                        // a script's modules sit beside it, as on disk
                        graphix_package_sys::loader::NetidxResolver::new(
                            subscriber,
                            netidx_core::path::Path::from(ArcStr::from(
                                netidx_core::path::Path::dirname(path).unwrap_or("/"),
                            )),
                            p.resolve_timeout.map(Duration::from_secs),
                        ),
                    ]);
                    Load::Netidx(Path::from(ArcStr::from(path)))
                }
                #[cfg(not(feature = "sys"))]
                Some(_) => bail!("netidx: sources require the sys feature"),
                None => {
                    let path = PathBuf::from(&**f)
                        .canonicalize()
                        .with_context(|| format!("{f}"))?;
                    let path = if path.is_dir() { path.join("main.gx") } else { path };
                    match path.parent() {
                        Some(p) if p.as_os_str().is_empty() => (),
                        None => (),
                        Some(p) => {
                            let p = PathBuf::from(p);
                            shell =
                                shell.module_resolvers(vec![FilesResolver::new(p, None)]);
                        }
                    };
                    Load::File(path)
                }
            };
            let mode = if check { Mode::Check(source) } else { Mode::Script(source) };
            shell = shell.mode(mode);
        }
        let (mut enable, disable) = RawFlag::as_flags(&p.warn);
        if p.no_fusion {
            enable.insert(CFlag::FusionDisabled);
        }
        if p.expand {
            enable.insert(CFlag::ExpandSeq);
        }
        shell
            .enable_flags(enable)
            .disable_flags(disable)
            .build()?
            .run(run_on_main)
            .await
    });
    // a blocking task (a stdin read) must not hold the exit
    // CR claude for eric: [bug/high] The process exits without waiting for the
    // runtime's teardown. Teardown only aborts the runtime task, and
    // shutdown_background abandons both GX's drop and the blocking thread where Reaped
    // flushes the database. So a db insert followed by sys::exit is lost in most runs:
    // sled had not flushed it, and db's interface does not say an insert needs
    // db::flush to be durable. Await the runtime task's end after the abort (GXHandle
    // holds its JoinHandle), then let blocking reaps finish within a bounded
    // shutdown_timeout, or have Reaped count outstanding reaps for the exit to wait on,
    // so a stdin read still cannot hold the exit. probe:
    // design/review-2026-10-26/repro/x-concurrency-db-write-lost-at-exit.sh
    // (x-concurrency-01)
    rt.shutdown_background();
    res
}

fn main() -> Result<()> {
    Config::maybe_run_machine_local_resolver()?;
    let standalone = graphix_shell::stdlib_packages::<NoExt>()
        .iter()
        .any(|p| p.main_program().is_some());
    let (p, file, program_args) = if standalone {
        // a standalone binary is its program: every argument is the program's
        let p = Params::parse_from(std::env::args_os().take(1));
        let args = std::env::args_os().skip(1).map(|a| ArcStr::from(a.to_string_lossy()));
        (p, None, args.collect())
    } else {
        let mut p = Params::parse();
        let mut program = mem::take(&mut p.program).into_iter().map(ArcStr::from);
        (p, program.next(), program.collect())
    };
    match p.command {
        Some(Command::Package { action }) => return handle_package(action),
        Some(Command::Lsp) => return graphix_shell::lsp_backend::run(),
        Some(Command::Fmt { files, check, stdout, interface, width, indent }) => {
            return graphix_shell::fmt::run(graphix_shell::fmt::Args {
                files,
                check,
                stdout,
                interface,
                width,
                indent,
            });
        }
        None => (),
    }
    let (handle, main_rx) = MainThreadHandle::new();
    let tokio_handle = std::thread::Builder::new()
        .name("graphix-tokio".into())
        .spawn(move || tokio_main(p, file, program_args, handle))
        .expect("failed to spawn tokio thread");
    while let Ok(f) = main_rx.recv() {
        f();
    }
    let code =
        tokio_handle.join().map_err(|_| anyhow::anyhow!("tokio thread panicked"))?;
    for p in graphix_shell::stdlib_packages::<NoExt>() {
        p.shutdown()
    }
    if let Some(code) = code? {
        std::process::exit(code)
    }
    Ok(())
}
