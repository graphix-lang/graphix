use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use clap::{Args, Parser, Subcommand};
use enumflags2::BitFlags;
use flexi_logger::{Duplicate, FileSpec, Logger};
use graphix_compiler::{CFlag, FAILURE_TARGET, expr::Load};
use graphix_package::{GraphixPM, MainThreadHandle, PackageId};
use graphix_package_core::{NetConfig, NetTimeouts};
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder, TEARDOWN_BOUND};
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
    // XCR claude for claude: [bug/high] The process exits without waiting for the
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
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q21, Exit waits
    // for teardown): yes, await (the recommendation: await with a bound (a few
    // seconds).)
    // 2026-10-28 qwen: Awaited the teardown, bounded by graphix_shell::TEARDOWN_BOUND
    // (5 s). GXHandle now holds its JoinHandle in a Mutex<Option<..>> and gains
    // GXHandle::ended(), which takes it and awaits the runtime task's end; Teardown's
    // abort still breaks a wedged cycle first. Shell::run drops Teardown (aborting the
    // runtime and the Ctrl-C task) and then awaits gx.ended() under that bound, so the
    // session gives up what it held while the shell's tokio runtime is still up to run
    // what a last drop starts. main.rs uses rt.shutdown_timeout(TEARDOWN_BOUND) where
    // it had shutdown_background, which is the bound on Reaped's blocking reap. A read
    // that never returns is now bounded by that instead of holding the exit, so the
    // rule above still holds; a normal exit pays nothing (the db pin runs 16 processes
    // in ~5.6 s, so neither wait is being spent when there is nothing to wait for).
    // Pins: graphix-shell/tests/exit_teardown.rs.
    // a_db_write_before_sys_exit_reaches_the_disk loops 8 runs with a fresh db each, so
    // a run can only read what its own write put there; before the fix it lost the
    // write on the first or second iteration in every invocation, after it passes.
    // a_stdin_read_that_never_ends_does_not_hold_the_exit holds stdin open on a pipe
    // that is never written and calls sys::exit from a timer; it passes before and
    // after, which is the point: the bound ends the process where the read cannot. The
    // probe reproduced only 2 of 12 here (the report said 5 of 6), which is why the pin
    // loops a fresh db per run rather than trusting it. cargo test -p graphix-shell
    // --offline: 181 passed, 0 failed, 1 ignored (44 suites).
    rt.shutdown_timeout(TEARDOWN_BOUND);
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
