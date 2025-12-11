use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use clap::Parser;
use enumflags2::BitFlags;
use flexi_logger::{FileSpec, Logger};
use graphix_compiler::{
    expr::{ModuleResolver, Source},
    CFlag,
};
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
    UnhandledArith,
    NoUnhandledArith,
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
            "unhandled-arith" => Ok(Self::UnhandledArith),
            "no-unhandled-arith" => Ok(Self::NoUnhandledArith),
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
                Self::UnhandledArith => enable.insert(CFlag::WarnUnhandledArith),
                Self::NoUnhandledArith => disable.insert(CFlag::WarnUnhandledArith),
                Self::Unused => enable.insert(CFlag::WarnUnused),
                Self::NoUnused => disable.insert(CFlag::WarnUnused),
                Self::Error => enable.insert(CFlag::WarningsAreErrors),
                Self::NoError => disable.insert(CFlag::WarningsAreErrors),
            }
        }
        (enable, disable)
    }
}

#[derive(Parser)]
#[command(version, about)]
struct Params {
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
    /// - unhandled-arith, no-unhandled-arith: warn about unhandled arith exceptions
    /// - unused, no-unused: warn about unused variables (default)
    /// - error, no-error makes warnings errors
    ///
    /// the no- variant turns the flag off. If both are specifed the no- variant
    /// always wins
    #[arg(short = 'W')]
    warn: Vec<RawFlag>,
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

#[tokio::main]
async fn tokio_main(p: Params, cfg: Result<Config>) -> Result<()> {
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
                        shell = shell.module_resolvers(vec![ModuleResolver::Files(p)]);
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
        .run()
        .await
}

fn main() -> Result<()> {
    Config::maybe_run_machine_local_resolver()?;
    let p = Params::parse();
    let cfg = match &p.config {
        None => Config::load_default_or_local_only(),
        Some(p) => Config::load(p),
    };
    tokio_main(p, cfg)
}
