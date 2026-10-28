#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, ExecState, FusionStats, PrintFlag,
    env::Env,
    expr::{
        CouldNotResolve, ExprId, FilesResolver, Load, ModPath, ResolverFactory,
        ResolverRef,
    },
    format_with_flags,
    typ::TVal,
};
use graphix_package::{Cdc, CustomResult, MainThreadHandle, Package, register_packages};
use graphix_package_core::ProgramArgs;
use graphix_rt::{
    CompExp, GXConfig, GXEvent, GXExt, GXHandle, GXRt, ProgramImage, RegistrationImage,
};
use input::{InputReader, Read};
use netidx::publisher::Value;
use poolshark::{global::GPooled, local::LPooled};
use reedline::Signal;
use std::{
    io::{self, Write},
    marker::PhantomData,
    ops::ControlFlow,
    time::Duration,
};
use tokio::{
    select,
    sync::{mpsc, oneshot},
};

mod cache;
use cache::Entry;
mod completion;
pub mod fmt;
mod input;
pub mod lsp_backend;

/// The shell's built-in package set: the stdlib packages this binary was
/// compiled with — and any external packages the package manager added to the
/// shell's `Cargo.toml` — auto-discovered by `graphix_package::packages!()`.
/// This is the default for `ShellBuilder::packages`; embedders append their own
/// with `ShellBuilder::add_packages`.
pub fn stdlib_packages<X: GXExt>() -> Vec<Box<dyn Package<X>>> {
    graphix_package::packages!()
}

/// Print the script's fusion profile: the stats delta between `base`
/// (after the stdlib root loaded) and `after` (after the script compiled).
fn print_fusion_stats(base: &FusionStats, after: &FusionStats) {
    let attempted = after.attempted - base.attempted;
    let fused = after.fused - base.fused;
    let failed = &after.failed[base.failed.len()..];
    eprintln!("fusion: {fused} of {attempted} attempted regions fused");
    if !failed.is_empty() {
        eprintln!("blockers ({}):", failed.len());
    }
    let mut by_reason: LPooled<AHashMap<&str, usize>> = LPooled::take();
    for failure in failed {
        let reason = failure.reason.lines().next().unwrap_or("").trim_end();
        *by_reason.entry(reason).or_insert(0) += 1;
    }
    let mut sorted: LPooled<Vec<(&str, usize)>> =
        by_reason.iter().map(|(r, n)| (*r, *n)).collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(b.0)));
    for (reason, n) in sorted.iter() {
        eprintln!("  {n}x {reason}");
    }
}

/// The argv[0] a program loaded from `source` names itself by, as it was
/// asked for on the command line.
fn argv0(source: &Load) -> Option<ArcStr> {
    match source {
        Load::File(p) => Some(arcstr::format!("{}", p.display())),
        Load::Netidx(p) => Some(arcstr::format!("netidx:{p}")),
        Load::Text(_) => None,
    }
}

enum Output<X: GXExt> {
    None,
    EmptyScript,
    Custom(Cdc<X>),
    Text(CompExp<X>),
}

impl<X: GXExt> Output<X> {
    async fn from_expr(
        gx: &GXHandle<X>,
        env: &Env,
        e: CompExp<X>,
        run_on_main: &MainThreadHandle,
        packages: &[Box<dyn Package<X>>],
    ) -> Result<Self> {
        let mut e = e;
        for (i, pkg) in packages.iter().enumerate() {
            let r = pkg.maybe_init_custom(gx, env, e, run_on_main).await;
            match r.context("initializing custom display")? {
                CustomResult::Custom(cdc) => {
                    Self::chosen(gx, packages, Some(i)).await;
                    return Ok(Self::Custom(cdc));
                }
                CustomResult::NotCustom(ret) => e = ret,
            }
        }
        Self::chosen(gx, packages, None).await;
        Ok(Self::Text(e))
    }

    /// Tell every package whose display, if any, shows the output.
    async fn chosen(
        gx: &GXHandle<X>,
        packages: &[Box<dyn Package<X>>],
        by: Option<usize>,
    ) {
        for (i, pkg) in packages.iter().enumerate() {
            pkg.output_chosen(gx, by == Some(i)).await
        }
    }

    async fn clear(&mut self) {
        if let Self::Custom(cdc) = self {
            cdc.custom.clear().await;
        }
        *self = Self::None;
    }

    /// Show an update; an error is the text output's, whose reader may
    /// have gone.
    async fn process_update(
        &mut self,
        env: &Env,
        id: ExprId,
        v: Value,
    ) -> io::Result<()> {
        match self {
            Self::None | Output::EmptyScript => Ok(()),
            Self::Custom(cdc) => Ok(cdc.custom.process_update(env, id, v).await),
            Self::Text(e) if e.id == id => {
                writeln!(
                    io::stdout().lock(),
                    "{}",
                    TVal { env: &env, typ: &e.typ, v: &v }
                )
            }
            Self::Text(_) => Ok(()),
        }
    }

    async fn batch_done(&mut self) {
        if let Self::Custom(cdc) = self {
            cdc.custom.batch_done().await
        }
    }

    /// Show a batch of the runtime's events, following its env: `Break`
    /// ends the run, `Ok(None)` when stdout's reader has gone.
    async fn process_batch(
        &mut self,
        env: &mut Env,
        newenv: &mut Option<Env>,
        batch: &mut GPooled<Vec<GXEvent>>,
    ) -> ControlFlow<Result<Option<i32>>> {
        for e in batch.drain(..) {
            match e {
                GXEvent::Updated(id, v) => match self.process_update(env, id, v).await {
                    Ok(()) => (),
                    // the reader is done with the output
                    Err(e) if e.kind() == io::ErrorKind::BrokenPipe => {
                        return ControlFlow::Break(Ok(None));
                    }
                    Err(e) => return ControlFlow::Break(Err(e.into())),
                },
                GXEvent::Env(e) => {
                    *env = e;
                    *newenv = Some(env.clone());
                }
                GXEvent::Idle => {
                    if let Self::Custom(cdc) = self {
                        cdc.custom.idle().await
                    }
                }
            }
        }
        self.batch_done().await;
        ControlFlow::Continue(())
    }
}

/// How long the end of the process waits for a runtime that is stopping:
/// for its task to end, and for the blocking work that ending starts (a
/// value that finishes its work as it drops, like a database's flush). Past
/// this the process ends regardless: a read that never returns must not
/// hold the exit.
pub const TEARDOWN_BOUND: Duration = Duration::from_secs(5);

/// Stops the runtime and the Ctrl-C task when a run ends, whichever way:
/// the Ctrl-C task holds a handle, so the runtime would outlive the run.
/// `abort()` breaks a cycle still spinning before stopping the runtime;
/// the tokio runtime's drop would block on it.
struct Teardown<X: GXExt> {
    gx: GXHandle<X>,
    sigint: tokio::task::JoinHandle<()>,
}

impl<X: GXExt> Drop for Teardown<X> {
    fn drop(&mut self) {
        self.gx.abort();
        self.sigint.abort();
    }
}

/// The signals that ask the process to end (SIGTERM, SIGHUP): the shell
/// stops as it would at Ctrl-D, so a display gives the terminal back.
/// The answer is the exit code a shell reports for the signal.
struct Terminated {
    #[cfg(unix)]
    term: tokio::signal::unix::Signal,
    #[cfg(unix)]
    hup: tokio::signal::unix::Signal,
}

impl Terminated {
    fn new() -> Result<Self> {
        #[cfg(unix)]
        {
            use tokio::signal::unix::{SignalKind, signal};
            Ok(Self {
                term: signal(SignalKind::terminate())?,
                hup: signal(SignalKind::hangup())?,
            })
        }
        #[cfg(not(unix))]
        Ok(Self {})
    }

    async fn recv(&mut self) -> Option<i32> {
        #[cfg(unix)]
        {
            tokio::select! {
                _ = self.term.recv() => Some(128 + 15),
                _ = self.hup.recv() => Some(128 + 1),
            }
        }
        #[cfg(not(unix))]
        futures::future::pending().await
    }
}

#[derive(Debug, Clone)]
pub enum Mode {
    /// Read input line by line from the user and compile/execute it.
    /// provide completion and print the value of the last expression
    /// as it executes. Ctrl-C cancel's execution of the last
    /// expression and Ctrl-D exits the shell.
    Repl,
    /// Load compile and execute a file. Print the value
    /// of the last expression in the file to stdout. Ctrl-C exits the
    /// shell.
    Script(Load),
    /// Check that the specified file compiles but do not run it
    Check(Load),
}

impl Mode {
    fn file_mode(&self) -> bool {
        match self {
            Self::Repl => false,
            Self::Script(_) | Self::Check(_) => true,
        }
    }
}

/// The embedder's context-setup hook — runs against the freshly
/// created [`ExecState`] before anything compiles. The place to seed
/// package libstate entries (the CLI seeds sys::net's `NetConfig`
/// and `NetTimeouts` here); embedders can do whatever they need.
pub type SetupContext<X> = Box<dyn FnOnce(&mut ExecState<GXRt<X>>) + Send + 'static>;

/// What a run does with the session image cache.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum CacheMode {
    /// neither read nor write it
    Off,
    /// start from it, and write the entries it lacks
    #[default]
    On,
    /// write it and exit, for an installer that wants the first real
    /// run to be warm: an entry that cannot be written fails the run
    Warm,
}

#[derive(Builder)]
#[builder(pattern = "owned")]
pub struct Shell<X: GXExt> {
    /// do not run the users init module
    #[builder(default = "false")]
    no_init: bool,
    /// What the run does with the image cache.
    #[builder(default)]
    cache: CacheMode,
    /// Module resolvers searched after a script file's own directory and
    /// before the search path. The image cache cannot vouch for what they
    /// serve, so a shell given any writes and restores no program entry.
    #[builder(default)]
    module_resolvers: Vec<ResolverRef>,
    /// GRAPHIX_MODPATH `scheme:` -> resolver factory registrations
    /// (the CLI registers the sys package's `netidx:` factory)
    #[builder(default)]
    resolver_factories: ahash::AHashMap<ArcStr, ResolverFactory>,
    /// run against the freshly created context before compilation —
    /// see [`SetupContext`]
    #[builder(setter(strip_option), default)]
    setup_context: Option<SetupContext<X>>,
    /// set the shell's mode
    #[builder(default = "Mode::Repl")]
    mode: Mode,
    /// Enable compiler flags, these will be ORed with the default set of flags
    /// for the mode.
    #[builder(default)]
    enable_flags: BitFlags<CFlag>,
    /// Disable compiler flags, these will be subtracted from the final set.
    /// (default_flags | enable_flags) - disable_flags
    #[builder(default)]
    disable_flags: BitFlags<CFlag>,
    /// after compiling a script, print its fusion profile (regions
    /// attempted/fused and the per-region blocker reasons) to stderr.
    /// The stdlib baseline is subtracted, so the profile covers the
    /// script alone.
    #[builder(default = "false")]
    fusion_stats: bool,
    /// program arguments to pass to the graphix script
    #[builder(default)]
    program_args: Vec<ArcStr>,
    /// The packages registered into the shell, in registration order. Each
    /// package is asked, at init, to register its builtins/modules, to supply a
    /// `main_program`, and (per displayed value) whether it has a custom
    /// display. Defaults to the built-in `stdlib_packages()`; replace with
    /// `packages` or extend with `add_packages`.
    #[builder(setter(custom), default = "stdlib_packages::<X>()")]
    packages: Vec<Box<dyn Package<X>>>,
    /// Where `sys::exit` asks the shell to end, installed at init.
    #[builder(setter(skip), default)]
    exit: Option<tokio::sync::oneshot::Receiver<i32>>,
    #[builder(setter(skip), default)]
    _phantom: PhantomData<X>,
}

impl<X: GXExt> ShellBuilder<X> {
    /// Replace the shell's package set entirely.
    pub fn packages(mut self, packages: Vec<Box<dyn Package<X>>>) -> Self {
        self.packages = Some(packages);
        self
    }

    /// Append packages onto the default stdlib set — e.g.
    /// `graphix_package::packages!()` for an embedder's own `graphix-package-*`
    /// dependencies.
    pub fn add_packages(mut self, packages: Vec<Box<dyn Package<X>>>) -> Self {
        self.packages.get_or_insert_with(stdlib_packages::<X>).extend(packages);
        self
    }
}

/// The cache a runtime's images go to, and the images it will send.
struct PendingImages {
    cache: cache::RegistrationCache,
    registration: Option<oneshot::Receiver<Result<Bytes>>>,
    program: Option<oneshot::Receiver<Result<ProgramImage>>>,
}

impl<X: GXExt> Shell<X> {
    async fn init(
        &mut self,
        sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    ) -> Result<GXHandle<X>> {
        let (config, pending) = self.configure(sub)?;
        let handle = config.start().await.context("loading initial modules")?;
        self.store(pending).await?;
        Ok(handle)
    }

    /// `--warm`: compile, write the cache's entries and run nothing.
    async fn warm(&mut self) -> Result<()> {
        let (sub, _) = mpsc::channel(1);
        let (config, pending) = self.configure(sub)?;
        let compiled = config.compile().await.context("loading initial modules");
        self.store(pending).await?;
        compiled
    }

    async fn store(&self, pending: Option<PendingImages>) -> Result<()> {
        let Some(PendingImages { cache, registration, program }) = pending else {
            return Ok(());
        };
        if let Some(rx) = registration {
            self.stored(Entry::Registration, rx.await, |i| cache.store_registration(&i))?;
        }
        if let Some(rx) = program {
            self.stored(Entry::Program, rx.await, |p| cache.store_program(&p))?;
        }
        Ok(())
    }

    fn configure(
        &mut self,
        sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    ) -> Result<(GXConfig<X>, Option<PendingImages>)> {
        if let Mode::Script(Load::File(p)) | Mode::Check(Load::File(p)) = &mut self.mode {
            *p = p.canonicalize().with_context(|| p.display().to_string())?;
        }
        let mut ctx = GXRt::<X>::new_state().context("creating graphix context")?;
        if let Some(setup) = self.setup_context.take() {
            setup(&mut ctx);
        }
        let (exit, exit_rx) = graphix_package_core::ExitRequest::new();
        ctx.libstate.set(exit);
        self.exit = Some(exit_rx);
        // argv[0] names the program: its file, or the binary a package's
        // embedded program is; an embedder's internal source gets exactly
        // the arguments it passes
        let mut args = vec![];
        match self.packages.iter().find_map(|p| p.main_program()) {
            Some(main) if matches!(self.mode, Mode::Repl) => {
                self.mode = Mode::Script(Load::Text(ArcStr::from(main)));
                if let Some(bin) = std::env::args_os().next() {
                    args.push(ArcStr::from(bin.to_string_lossy().as_ref()));
                }
            }
            _ => (),
        }
        if let Mode::Script(source) | Mode::Check(source) = &self.mode
            && let Some(name) = argv0(source)
        {
            args.push(name)
        }
        args.extend(self.program_args.drain(..));
        if !args.is_empty() {
            ctx.libstate.set(ProgramArgs(args));
        }
        let (vfs_modules, root) =
            register_packages(&mut ctx, self.packages.iter().map(|p| &**p))
                .context("register package modules")?;
        let program = match &self.mode {
            Mode::Script(source) => Some(source.clone()),
            Mode::Check(_) | Mode::Repl => None,
        };
        let uncached = match &program {
            Some(Load::Netidx(_)) => Some("nothing keys a netidx script's source"),
            Some(_) if !self.module_resolvers.is_empty() => {
                Some("the cache cannot vouch for an embedder's resolvers")
            }
            Some(Load::File(_) | Load::Text(_)) | None => None,
        };
        let cached_program = match uncached {
            Some(why) if self.cache == CacheMode::Warm => {
                bail!("--warm: the program cannot be cached: {why}")
            }
            Some(why) => {
                log::info!("no program entry: {why}");
                None
            }
            None => program.as_ref(),
        };
        let mut flags: BitFlags<CFlag> = match self.mode {
            Mode::Script(_) | Mode::Check(_) => CFlag::WarnUnhandled.into(),
            Mode::Repl => CFlag::ReplaceImports.into(),
        };
        flags.insert(self.enable_flags);
        flags.remove(self.disable_flags);
        // The compile flags shape the program's graph (fusion on or off).
        let cache = match self.cache {
            CacheMode::Off => None,
            CacheMode::On | CacheMode::Warm => match cache::RegistrationCache::new(
                &root,
                cached_program,
                flags.bits() as u64,
            ) {
                Ok(c) => Some(c),
                Err(e) if self.cache == CacheMode::Warm => {
                    return Err(e.context("image cache unavailable"));
                }
                Err(e) => {
                    log::warn!("image cache unavailable: {e}");
                    None
                }
            },
        };
        let mut pending_registration = None;
        let mut pending_program = None;
        let mut program_image = None;
        let registration = cache.as_ref().map(|c| {
            let mut image = RegistrationImage::default();
            // a fusion profile is of this process's compile, so it never
            // starts from the program's entry
            if !self.fusion_stats
                && let Some(entry) = c.load(Entry::Program)
            {
                image.restore.push(entry);
            }
            image.restore.extend(c.load(Entry::Registration));
            // What does not restore is compiled and written. The
            // registration entry serves a later run of a different
            // program under these packages; a program built into the
            // binary is the only one it runs, so it never writes one.
            if !matches!(self.mode, Mode::Script(Load::Text(_))) {
                let (tx, rx) = oneshot::channel();
                pending_registration = Some(rx);
                image.save = Some(tx);
            }
            if c.has_program() {
                let (tx, rx) = oneshot::channel();
                program_image = Some(tx);
                pending_program = Some(rx);
            }
            image
        });
        let mut mods = vec![vfs_modules];
        if let Mode::Script(Load::File(p)) | Mode::Check(Load::File(p)) = &self.mode
            && let Some(dir) = p.parent()
        {
            mods.push(FilesResolver::new(dir.to_path_buf(), None));
        }
        mods.extend(self.module_resolvers.drain(..));
        let mut gx = GXConfig::builder(ctx, sub);
        if let Some(r) = registration {
            gx = gx.registration(r);
        }
        if let Some(p) = program {
            gx = gx.program(p);
        }
        if let Some(tx) = program_image {
            gx = gx.program_image(tx);
        }
        if !self.resolver_factories.is_empty() {
            gx = gx.resolver_factories(std::mem::take(&mut self.resolver_factories));
        }
        gx = gx.flags(flags);
        let config =
            gx.root(root).resolvers(mods).build().context("building rt config")?;
        let pending = cache.map(|cache| PendingImages {
            cache,
            registration: pending_registration,
            program: pending_program,
        });
        Ok((config, pending))
    }

    /// Write the image the runtime sent, if it sent one: it sends none
    /// for what it restored, or for a program that failed to compile,
    /// which the run reports. A failure fails a warm run.
    fn stored<T>(
        &self,
        entry: Entry,
        image: Result<Result<T>, oneshot::error::RecvError>,
        store: impl FnOnce(T) -> Result<()>,
    ) -> Result<()> {
        let stored = match image {
            Err(_) => return Ok(()),
            Ok(Ok(image)) => {
                store(image).with_context(|| format!("{entry:?} image not written"))
            }
            Ok(Err(e)) => Err(e.context(format!("{entry:?} image not taken"))),
        };
        match stored {
            Ok(()) => Ok(()),
            Err(e) if self.cache == CacheMode::Warm => Err(e),
            Err(e) => {
                log::warn!("{e:#}");
                Ok(())
            }
        }
    }

    async fn load_env(
        &mut self,
        gx: &GXHandle<X>,
        newenv: &mut Option<Env>,
        output: &mut Output<X>,
        exprs: &mut Vec<CompExp<X>>,
        run_on_main: &MainThreadHandle,
    ) -> Result<Env> {
        let env;
        match &self.mode {
            Mode::Check(_) => bail!("run checks Mode::Check without loading"),
            Mode::Script(_) => {
                let r = gx
                    .program()
                    .await?
                    .ok_or_else(|| anyhow!("the runtime has no program"))?;
                if self.fusion_stats {
                    print_fusion_stats(
                        &FusionStats::default(),
                        &gx.fusion_stats().await?,
                    );
                }
                exprs.extend(r.exprs);
                env = gx.get_env().await?;
                match exprs.pop() {
                    Some(e) => {
                        *output =
                            Output::from_expr(&gx, &env, e, run_on_main, &self.packages)
                                .await?
                    }
                    None => Output::chosen(&gx, &self.packages, None).await,
                }
                *newenv = None
            }
            Mode::Repl if !self.no_init => match gx.compile("mod init".into()).await {
                Ok(res) => {
                    env = res.env;
                    exprs.extend(res.exprs);
                    *newenv = Some(env.clone())
                }
                // no init module is no error; anything wrong inside one is
                Err(e)
                    if e.chain().any(|c| {
                        c.downcast_ref::<CouldNotResolve>()
                            .is_some_and(|c| c.path == ModPath::from(["init"]))
                    }) =>
                {
                    env = gx.get_env().await?;
                    *newenv = Some(env.clone())
                }
                Err(e) => {
                    eprintln!("error in init module: {e:?}");
                    env = gx.get_env().await?;
                    *newenv = Some(env.clone())
                }
            },
            Mode::Repl => {
                env = gx.get_env().await?;
                *newenv = Some(env.clone());
            }
        }
        Ok(env)
    }

    async fn check_with(&self, gx: &GXHandle<X>) -> Result<()> {
        let Mode::Check(source) = &self.mode else { bail!("check requires Mode::Check") };
        let initial_scope = match source {
            Load::File(p) => graphix_lsp::workspace::detect_package_scope(p),
            _ => None,
        };
        let baseline =
            if self.fusion_stats { Some(gx.fusion_stats().await?) } else { None };
        gx.check(source.clone(), initial_scope).await?;
        if let Some(baseline) = baseline {
            print_fusion_stats(&baseline, &gx.fusion_stats().await?);
        }
        Ok(())
    }

    /// Compile and typecheck the `Mode::Check` source.
    pub async fn check(mut self) -> Result<()> {
        let (tx, _from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        self.check_with(&gx).await
    }

    /// Run the shell; `Some(code)` when the program asked to exit with it,
    /// after the display is cleared and the runtime stopped.
    /// `Mode::Check` checks, and is `Some(0)` when the check passes.
    pub async fn run(mut self, run_on_main: MainThreadHandle) -> Result<Option<i32>> {
        if let Mode::Check(_) = self.mode {
            return self.check().await.map(|()| Some(0));
        }
        if self.cache == CacheMode::Warm {
            return self.warm().await.map(|()| None);
        }
        let (tx, mut from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        // Armed before the first cycle: a program may wedge inside
        // `load_env`, before the input loop exists.
        let sigint = {
            let gx = gx.clone();
            tokio::spawn(async move {
                while tokio::signal::ctrl_c().await.is_ok() {
                    gx.interrupt();
                }
            })
        };
        let _teardown = Teardown { gx: gx.clone(), sigint };
        let mut terminated = Terminated::new()?;
        let script = self.mode.file_mode();
        let mut input = InputReader::new();
        let mut output = if script { Output::EmptyScript } else { Output::None };
        let mut newenv = None;
        let mut exprs = vec![];
        // a program whose first cycle does not end holds the env load, so a
        // termination signal must be heard there too
        let mut env = tokio::select! {
            env = self.load_env(&gx, &mut newenv, &mut output, &mut exprs, &run_on_main) => env?,
            code = terminated.recv() => {
                gx.interrupt();
                return Ok(code)
            }
        };
        if !script {
            println!("Welcome to the graphix shell");
            println!("Press ctrl-c to cancel, ctrl-d to exit, and tab for help")
        }
        let mut exit_rx = self.exit.take().expect("init installs the exit request");
        let mut exit_open = true;
        let exit = 'repl: loop {
            select! {
                code = &mut exit_rx, if exit_open => match code {
                    Ok(code) => {
                        // the runtime answers once the cycle that asked has
                        // sent its batch: show it, and every batch before it
                        let _ = gx.get_env().await;
                        while let Ok(mut batch) = from_gx.try_recv() {
                            let shown =
                                output.process_batch(&mut env, &mut newenv, &mut batch).await;
                            if let ControlFlow::Break(r) = shown {
                                break 'repl r
                            }
                        }
                        break 'repl Ok(Some(code))
                    }
                    // the runtime stopped: from_gx's end says why
                    Err(_) => exit_open = false,
                },
                batch = from_gx.recv() => match batch {
                    None => break 'repl Err(gx.stopped("graphix runtime is dead")),
                    Some(mut batch) => {
                        let shown =
                            output.process_batch(&mut env, &mut newenv, &mut batch).await;
                        if let ControlFlow::Break(r) = shown {
                            break 'repl r
                        }
                    }
                },
                code = terminated.recv() => break 'repl Ok(code),
                input = input.read_line(&mut output, &mut newenv, script) => {
                    match input {
                        // with no display up the error is the input's own
                        Err(e) if script || matches!(output, Output::None) => break Err(e),
                        Err(e) => {
                            eprintln!("error: {e:?}");
                            // A display that failed is still the output.
                            gx.interrupt();
                            output.clear().await;
                        }
                        // a program that ended its display ended successfully
                        Ok(Read::Ended) if script => break Ok(None),
                        Ok(Read::Ended) => output.clear().await,
                        // the status a shell reports for SIGINT
                        Ok(Read::Signal(Signal::CtrlC)) if script => break Ok(Some(128 + 2)),
                        Ok(Read::Signal(Signal::CtrlC)) => {
                            // Interrupt first: a wedged runtime cannot serve `output.clear()`.
                            gx.interrupt();
                            output.clear().await;
                        }
                        Ok(Read::Signal(Signal::CtrlD | Signal::ExternalBreak(_))) => {
                            break Ok(None)
                        }
                        Ok(Read::Signal(Signal::Success(line))) => {
                            match gx.compile(ArcStr::from(line)).await {
                                Err(e) => eprintln!("error: {e:?}"),
                                Ok(res) => {
                                    env = res.env;
                                    newenv = Some(env.clone());
                                    exprs.extend(res.exprs);
                                    if exprs.last().map(|e| e.output).unwrap_or(false) {
                                        let e = exprs.pop().unwrap();
                                        let typ = e.typ
                                            .with_deref(|t| t.cloned())
                                            .unwrap_or_else(|| e.typ.clone());
                                        format_with_flags(
                                            PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
                                            || println!("-: {}", typ)
                                        );
                                        output.clear().await;
                                        let o = Output::from_expr(
                                            &gx, &env, e, &run_on_main,
                                            &self.packages,
                                        ).await;
                                        output = o.unwrap_or_else(|e| {
                                            eprintln!("error: {e:?}");
                                            Output::None
                                        });
                                    } else {
                                        output.clear().await;
                                        Output::chosen(&gx, &self.packages, None).await;
                                    }
                                }
                            }
                        }
                        Ok(_) => ()
                    }
                },
            }
        };
        // A custom display holds the terminal (or a window) until it is
        // cleared. Interrupt first: a wedged runtime cannot serve
        // `output.clear()`.
        gx.interrupt();
        output.clear().await;
        // The session gives up what it held as the runtime's task ends, and
        // the work a last drop starts runs on this runtime, so it must stay
        // up while that task is awaited.
        drop(_teardown);
        let _ = tokio::time::timeout(TEARDOWN_BOUND, gx.ended()).await;
        exit
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn argv0_names_the_program_as_asked_for() {
        let file = Load::File(std::path::PathBuf::from("/apps/p.gx"));
        assert_eq!(argv0(&file).as_deref(), Some("/apps/p.gx"));
        let net = Load::Netidx(netidx_core::path::Path::from("/apps/p.gx"));
        assert_eq!(argv0(&net).as_deref(), Some("netidx:/apps/p.gx"));
        assert_eq!(argv0(&Load::Text(arcstr::literal!("1"))), None);
    }

    /// `--warm` of a program the cache keeps no entry for fails, before
    /// compiling anything.
    #[tokio::test(flavor = "multi_thread")]
    async fn warm_refuses_a_netidx_script() {
        let net = Load::Netidx(netidx_core::path::Path::from("/apps/p.gx"));
        let mut shell = ShellBuilder::<graphix_rt::NoExt>::default()
            .mode(Mode::Script(net))
            .cache(CacheMode::Warm)
            .build()
            .unwrap();
        let e = shell.warm().await.expect_err("warmed a netidx script");
        assert!(
            format!("{e:#}").contains("nothing keys a netidx script's source"),
            "{e:#}"
        );
    }

    /// The cache key and the program both name the file the program
    /// entry's record verifies, whatever directory asked for it.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_script_file_is_configured_by_its_canonical_path() {
        let relative = std::path::PathBuf::from("Cargo.toml");
        let mut shell = ShellBuilder::<graphix_rt::NoExt>::default()
            .mode(Mode::Script(Load::File(relative.clone())))
            .cache(CacheMode::Off)
            .build()
            .unwrap();
        let (tx, _rx) = mpsc::channel(1);
        shell.configure(tx).unwrap();
        let Mode::Script(Load::File(p)) = &shell.mode else { panic!("not a script") };
        assert_eq!(p, &relative.canonicalize().unwrap());
    }
}
