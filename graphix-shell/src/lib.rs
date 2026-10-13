#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, ExecState, FusionStats, PrintFlag,
    env::Env,
    expr::{CouldNotResolve, ExprId, ModPath, ResolverFactory, ResolverRef, Source},
    format_with_flags,
    typ::TVal,
};
use graphix_package::{Cdc, CustomResult, MainThreadHandle, Package, register_packages};
use graphix_package_core::ProgramArgs;
use graphix_rt::{CompExp, GXConfig, GXEvent, GXExt, GXHandle, GXRt, RegistrationImage};
use input::InputReader;
use netidx::publisher::Value;
use poolshark::{global::GPooled, local::LPooled};
use reedline::Signal;
use std::{
    io::{self, Write},
    marker::PhantomData,
    process::exit,
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
    eprintln!(
        "fusion: {fused} of {attempted} attempted regions fused, {} blocked",
        failed.len()
    );
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
        for pkg in packages {
            let r = pkg.maybe_init_custom(gx, env, e, run_on_main).await;
            match r.context("initializing custom display")? {
                CustomResult::Custom(cdc) => return Ok(Self::Custom(cdc)),
                CustomResult::NotCustom(ret) => e = ret,
            }
        }
        Ok(Self::Text(e))
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
    Script(Source),
    /// Check that the specified file compiles but do not run it
    Check(Source),
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
pub type SetupContext<X> =
    Box<dyn FnOnce(&mut ExecState<GXRt<X>, <X as GXExt>::UserEvent>) + Send + 'static>;

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
    /// define module resolvers to append to the default list
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
    /// `graphix_shell::packages!()` for an embedder's own `graphix-package-*`
    /// dependencies.
    pub fn add_packages(mut self, packages: Vec<Box<dyn Package<X>>>) -> Self {
        self.packages.get_or_insert_with(stdlib_packages::<X>).extend(packages);
        self
    }
}

impl<X: GXExt> Shell<X> {
    async fn init(
        &mut self,
        sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    ) -> Result<GXHandle<X>> {
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
                self.mode = Mode::Script(Source::Program(ArcStr::from(main)));
                if let Some(bin) = std::env::args_os().next() {
                    args.push(ArcStr::from(bin.to_string_lossy().as_ref()));
                }
            }
            _ => (),
        }
        if let Mode::Script(Source::File(p)) | Mode::Check(Source::File(p)) = &self.mode {
            args.push(ArcStr::from(p.display().to_string().as_str()));
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
                program.as_ref(),
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
            // CR claude for claude: [bug/low] A restored program entry compiles
            // nothing, so the warnings its compile emitted through `Env::warn`
            // (`CFlag::WarnUnhandled`'s unhandled `?`, unused imports) are shown on the
            // cold run only. The same program with the same flags prints different
            // diagnostics depending on cache state. Fix: store the compile's warnings
            // in the program entry and print them again on restore. They are fixed by
            // the key and the verified sources. The simpler alternative is to write no
            // program entry for a compile that warned. probe:
            // design/review-2026-10-12/repro/shell-04-warm-drops-warnings.sh (shell-04)
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
            if !matches!(self.mode, Mode::Script(Source::Program(_))) {
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
        for res in self.module_resolvers.drain(..) {
            mods.push(res);
        }
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
        let handle = gx
            .root(root)
            .resolvers(mods)
            .build()
            .context("building rt config")?
            .start()
            .await
            // CR claude for claude: [bug/medium] Shell loses a script's output on a
            // cold-cache run: the program image is stored before the output exists, and
            // select! may drop queued batches. In script mode, `init` starts the
            // runtime, which immediately runs cycles and queues GXEvent batches. The
            // shell then awaits storing the registration and program images (lib.rs)
            // before `load_env` builds the Output and the event loop starts. When the
            // loop starts, the program's exit request (exit_rx) may already be ready
            // beside the queued update batches. The loop's `select!` (lib.rs) is
            // unbiased tokio::select!, so it can take the exit and drop every queued
            // batch, including the program's value. With --no-cache or a warm image the
            // loop starts at once and the value prints. On a first run in a fresh cache
            // it is lost. The skill's own test idiom
            // (`sys::exit(sys::time::after_idle(duration:100.ms, 0))`) hits this for
            // larger programs. Failure: The 3-line program below prints `3` with
            // --no-cache and on warm runs, and prints nothing on the first run in a
            // fresh $XDG_CACHE_HOME (exit 0). With a 100 ms exit, f-jit-06's 20-element
            // program lost its value in about 1 of 6 cold runs; with a 20 ms exit it
            // lost it in 6 of 6 cold runs and 0 of 6 --no-cache runs. Fix: Store the
            // images off the event path: spawn the store, or do it after Output is
            // built and the loop runs. Make the loop `biased;` with from_gx ahead of
            // exit_rx, and drain from_gx with try_recv before honoring an exit, so a
            // value produced before the exit is always shown. Unverified: the
            // reviewer's report, not checked by a verifier. (x-rerun-2-05)
            .context("loading initial modules")?;
        if let Some(cache) = cache.as_ref() {
            if let Some(rx) = pending_registration {
                self.stored(Entry::Registration, rx.await, |i| {
                    cache.store_registration(&i)
                })?;
            }
            if let Some(rx) = pending_program {
                self.stored(Entry::Program, rx.await, |p| cache.store_program(&p))?;
            }
        }
        Ok(handle)
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
            Mode::Check(_) => {
                self.check_with(gx).await?;
                // CR claude for claude: [structure/low] The library ends the process
                // here, from inside the public `Shell::run`. That skips the teardown
                // its other exits do, and it kills an embedder that calls `run` in
                // `Mode::Check`. `Shell::check` exists only to avoid this. Fix: handle
                // `Mode::Check` at the top of `run` (call `check_with`, return
                // `Ok(Some(0))`) and let `main` exit, as it does for `sys::exit`. Then
                // `Shell::check` is the only other path and its doc no longer needs the
                // caveat. (shell-11)
                exit(0)
            }
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
                if let Some(e) = exprs.pop() {
                    *output =
                        Output::from_expr(&gx, &env, e, run_on_main, &self.packages)
                            .await?;
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
            Source::File(p) => graphix_lsp::workspace::detect_package_scope(p),
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

    /// Compile and typecheck the `Mode::Check` source, returning the
    /// result instead of exiting the process — the embeddable/test
    /// entry ([`Self::run`] exits after checking, as the CLI expects).
    pub async fn check(mut self) -> Result<()> {
        let (tx, _from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        self.check_with(&gx).await
    }

    /// Run the shell; `Some(code)` when the program asked to exit with it,
    /// after the display is cleared and the runtime stopped.
    pub async fn run(mut self, run_on_main: MainThreadHandle) -> Result<Option<i32>> {
        let (tx, mut from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        if self.cache == CacheMode::Warm {
            if let Mode::Script(_) = &self.mode {
                gx.program().await?;
            }
            return Ok(None);
        }
        // Armed before the first cycle: a program may wedge inside
        // `load_env`, before the input loop exists.
        // CR claude for claude: [risk/low] Two error exits skip the gx.abort() and
        // sigint.abort() at the end of run: the `?` on load_env, which is any script
        // that fails to compile, and the `return Err(gx.stopped(..))` arm. The sigint
        // task holds a GXHandle clone, so the runtime never sees its last handle drop.
        // In an embedder or a test calling Shell::run, the runtime task and the Ctrl-C
        // handler live for the rest of the process. Every cycle with work then logs
        // 'could not send batch' to a dropped receiver. Fix: once the sigint task is
        // armed, hold a guard whose Drop calls gx.abort() and sigint.abort(), so every
        // exit, `?` included, stops both. See x-concurrency-09 for the runtime's send
        // loop ignoring abort. (x-concurrency-08)
        let sigint = {
            let gx = gx.clone();
            tokio::spawn(async move {
                while tokio::signal::ctrl_c().await.is_ok() {
                    gx.interrupt();
                }
            })
        };
        let mut terminated = Terminated::new()?;
        let script = self.mode.file_mode();
        let mut input = InputReader::new();
        let mut output = if script { Output::EmptyScript } else { Output::None };
        let mut newenv = None;
        let mut exprs = vec![];
        let mut env = self
            .load_env(&gx, &mut newenv, &mut output, &mut exprs, &run_on_main)
            .await?;
        if !script {
            println!("Welcome to the graphix shell");
            println!("Press ctrl-c to cancel, ctrl-d to exit, and tab for help")
        }
        let mut exit_rx = self.exit.take().expect("init installs the exit request");
        let exit = 'repl: loop {
            // CR claude for claude: [bug/medium] When the exit request and an output
            // batch are both ready, this unbiased select! picks one at random. The exit
            // arm breaks without draining from_gx, so output the program has already
            // computed is lost. Example: a script whose first cycle runs about 1s and
            // ends with `sys::exit(sys::time::after_idle(duration:10.ms, 0))` prints
            // nothing in about half its runs and still exits 0. Adding `biased;` with
            // the batch arm first fixes only the cross-cycle case. ExitRequest::request
            // runs inside the cycle, before GX::do_cycle sends that cycle's batch, so
            // an exit in the same cycle as a value can still win. A real fix orders the
            // exit after the batch: either deliver the exit code through from_gx at the
            // end of the cycle (a GXEvent, say), or have the exit arm wait for the
            // current cycle's batch and print it before breaking. probe:
            // design/review-2026-10-12/repro/x-stack-03.gx (x-stack-03)
            // CR claude for claude: [bug/medium] This select! is unbiased. When
            // sys::exit completes exit_rx while an update batch from an earlier cycle
            // is still queued in from_gx, tokio picks between the two arms at random.
            // When it picks the exit arm, the loop breaks and the batch is dropped:
            // output the program produced before exiting is never printed, and the
            // process still exits 0. This hits the
            // `sys::exit(sys::time::after_idle(..))` idiom whenever cycle 0 outlasts
            // the delay. Fix: make the loop `biased;` with the from_gx arm ahead of
            // exit_rx. When the exit arm wins, also drain `from_gx.try_recv()` through
            // the same batch processing before breaking. Probe:
            // design/review-2026-10-12/repro/x-perf-compile-05.sh (prints 100 in 5/20
            // runs, expected 20/20). (x-perf-compile-05)
            // CR claude for claude: [bug/medium] This select is unbiased. In script
            // mode, when an Updated batch and the exit request are both ready, the
            // random rotation reaches exit_rx first 3 times in 4 (terminated and input
            // are pending and wrap round to it), and `break 'repl` drops the batch
            // still queued in from_gx. A script gated by
            // `sys::exit(sys::time::after_idle(..))` whose first cycle outlasts the
            // idle loses its result about 75% of the time. A script that exits in the
            // cycle that produces its value (`sys::exit(r ~ 0)`) loses it most of the
            // time too, because `Exit::update` signals before `do_cycle` sends that
            // cycle's batch. Fix: on exit, drain from_gx with `try_recv` through
            // `output.process_update` before breaking. That covers a batch already
            // queued. The same-cycle case also needs the runtime to send the cycle's
            // batch before the shell acts on the exit (e.g. deliver the exit code as an
            // event after the batch), or a documented rule that exit ends the program
            // before its own cycle's output. probe:
            // design/review-2026-10-12/repro/c-collection-04.gx (c-collection-04)
            select! {
                // CR claude for claude: [bug/medium] The exit request can win this
                // unbiased select! while from_gx still holds batches, and breaking here
                // drops them unprinted. ExitRequest::request sends during the cycle
                // that calls sys::exit, before that cycle's batch is sent, so even the
                // exit cycle's own value is usually lost: `let x =
                // sys::time::after_idle(duration:200.ms, 42); sys::exit(x ~ 0); x`
                // under --no-cache prints nothing in about 2 of 3 runs. On a cold start
                // Shell::init is still writing the images while the runtime runs, so a
                // script can lose all of its output: a counter that exits at 6 prints
                // nothing in about 1 of 9 cold runs and always prints 0..5 warm.
                // Draining from_gx with try_recv before breaking fixes the earlier
                // cycles but not the exit cycle's own value. A full fix delivers the
                // exit in order with the output: the runtime sends it on the event
                // channel after the batch of the cycle that requested it, or the shell
                // does a round trip to the runtime after the exit request and then
                // drains from_gx. probe: design/review-2026-10-12/repro/x-image-01.sh
                // (x-image-01)
                // CR claude for claude: [bug/medium] This arm breaks as soon as
                // `sys::exit` asks, so the script's last value is often never printed.
                // `Exit::update` sends on the oneshot in the middle of the cycle,
                // before `do_cycle` has sent that cycle's `GXEvent::Updated` batch.
                // When the timer exit lands after a slow cycle, the batch may already
                // be queued, and the unbiased select still picks the exit about half
                // the time. Either way `from_gx` is never drained: `let r = 42;
                // sys::exit(r ~ 0); r` exits 0 but prints 42 in only about 3 runs out
                // of 8. `biased;` alone does not fix the same-cycle case, because the
                // batch does not exist yet when the exit arrives. Instead, deliver the
                // exit in order with the output: the runtime sends it on the batch
                // channel after the cycle's batch (for example an exit `GXEvent`), or
                // on exit the shell waits for the runtime to finish the cycle and then
                // processes everything left in `from_gx` before breaking. Pin it with a
                // script test that checks the value is printed. probe:
                // design/review-2026-10-12/repro/x-engine-collections-04.gx
                // (x-engine-collections-04)
                // CR claude for claude: [bug/medium] When the runtime stops, its
                // ExitRequest sender is dropped and this branch resolves Err. The loop
                // neither breaks nor disables the branch, so the next select! polls the
                // completed oneshot again and tokio panics with "called after
                // complete". The user sees "Error: tokio thread panicked" instead of
                // the runtime's own error, such as the activation-budget message. It
                // happens in most runs, because select! picks branches in random order
                // and the clean path wins only when from_gx's None is taken first. Fix:
                // guard the branch with a flag (`code = &mut exit_rx, if !exit_closed
                // => match code { Ok(c) => break 'repl Ok(Some(c)), Err(_) =>
                // exit_closed = true }`), or treat Err as the runtime having stopped
                // and return gx.stopped(..). Probe:
                // design/review-2026-10-12/repro/c-image-03.gx with
                // GRAPHIX_ACTIVATION_BUDGET=20. (c-image-03)
                // CR claude for claude: [bug/high] When the runtime stops (a stack or
                // activation budget abort, a panic), the ExitRequest sender is dropped
                // and exit_rx resolves Err. This arm then falls through without
                // breaking, and the next select! polls the completed tokio oneshot
                // again, which panics with "called after complete". The process dies
                // with "Error: tokio thread panicked" and never shows the message from
                // GXHandle::stopped. Fix: once exit_rx has resolved Err, stop polling
                // it, for example with a select! precondition flag or by holding it as
                // an Option or a fused future. The from_gx None arm then returns
                // gx.stopped(..). Also add a graphix-shell test that runs a
                // budget-aborting script through Shell::run and checks for that
                // message. probe: design/review-2026-10-12/repro/x-concurrency-04.sh
                // Also reported as (f-kernel-04), (x-rerun-6-03). (x-concurrency-01)
                code = &mut exit_rx => if let Ok(code) = code {
                    break 'repl Ok(Some(code))
                },
                batch = from_gx.recv() => match batch {
                    None => return Err(gx.stopped("graphix runtime is dead")),
                    Some(mut batch) => {
                        for e in batch.drain(..) {
                            match e {
                                GXEvent::Updated(id, v) => {
                                    match output.process_update(&env, id, v).await {
                                        Ok(()) => (),
                                        // the reader is done with the output
                                        Err(e) if e.kind() == io::ErrorKind::BrokenPipe => {
                                            break 'repl Ok(None)
                                        }
                                        Err(e) => break 'repl Err(e.into()),
                                    }
                                },
                                GXEvent::Env(e) => {
                                    env = e;
                                    newenv = Some(env.clone());
                                }
                            }
                        }
                        output.batch_done().await
                    }
                },
                code = terminated.recv() => break 'repl Ok(code),
                input = input.read_line(&mut output, &mut newenv) => {
                    match input {
                        // with no display up the error is the input's own
                        Err(e) if script || matches!(output, Output::None) => break Err(e),
                        Err(e) => {
                            eprintln!("error: {e:?}");
                            // A display that failed is still the output.
                            gx.interrupt();
                            output.clear().await;
                        }
                        // CR claude for claude: [bug/low] An interrupted script ends
                        // with no exit code, so the process exits 0. `graphix long.gx
                        // && deploy` followed by Ctrl-C therefore runs `deploy`.
                        // `Terminated::recv` reports 128+n for SIGTERM and SIGHUP,
                        // which makes SIGINT the only end signal that reads as success.
                        // Probe:
                        // design/review-2026-10-12/repro/tests-shell-compiler-04.gx
                        // (SIGINT exits 0, SIGTERM 143). Fix: `break Ok(Some(128 +
                        // 2))`, and assert the exit status in interrupt_wedge.rs, which
                        // today checks only that the process exits.
                        // (tests-shell-compiler-04)
                        Ok(Signal::CtrlC) if script => break Ok(None),
                        Ok(Signal::CtrlC) => {
                            // Interrupt first: a wedged runtime cannot serve `output.clear()`.
                            gx.interrupt();
                            output.clear().await;
                        }
                        Ok(Signal::CtrlD) | Ok(Signal::ExternalBreak(_)) => break Ok(None),
                        Ok(Signal::Success(line)) => {
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
                                            PrintFlag::ReplacePrims,
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
        // `abort()` breaks a cycle still spinning before stopping the
        // runtime; the tokio runtime's drop would block on it.
        gx.abort();
        sigint.abort();
        exit
    }
}
