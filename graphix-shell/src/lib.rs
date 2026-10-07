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
    expr::{CouldNotResolve, ExprId, ResolverFactory, ResolverRef, Source, VfsResolver},
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
use std::{marker::PhantomData, process::exit};
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

    async fn process_update(&mut self, env: &Env, id: ExprId, v: Value) {
        match self {
            Self::None | Output::EmptyScript => (),
            Self::Custom(cdc) => cdc.custom.process_update(env, id, v).await,
            Self::Text(e) => {
                if e.id == id {
                    // CR claude for claude: [bug] println! panics on EPIPE, so piping a
                    // program whose value keeps updating into `head -2` ends in "thread
                    // 'graphix-tokio' panicked … failed printing to stdout: Broken
                    // pipe" and "Error: tokio thread panicked", exit 1. The print
                    // builtins' print! (stdlib/graphix-package-core/src/lib.rs:2489)
                    // panics the same way inside a node update and kills the runtime
                    // ("graphix runtime is dead"). Write with writeln! to a locked
                    // stdout and end the shell quietly on BrokenPipe; the builtins
                    // should drop or log a failed write. probe:
                    // design/review-2026-10-05/repro/x-errors-16.gx (x-errors-16)
                    println!("{}", TVal { env: &env, typ: &e.typ, v: &v })
                }
            }
        }
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

#[derive(Builder)]
#[builder(pattern = "owned")]
pub struct Shell<X: GXExt> {
    /// do not run the users init module
    #[builder(default = "false")]
    no_init: bool,
    /// Neither read nor write the registration image cache.
    #[builder(default = "false")]
    // CR claude for claude: [structure] `no_cache` and `warm` are independent bools, so
    // `--warm --no-cache` is accepted: init builds no cache and run returns Ok at line
    // 444, exiting 0 with nothing written (probe: `XDG_CACHE_HOME=$tmp graphix --warm
    // --no-cache warm.gx; find $tmp -type f` prints nothing, while `--warm` alone
    // writes two .img files). One cache mode (off, on, warm-and-exit) makes the pair
    // unrepresentable, and clap's `conflicts_with` refuses the flags together. Under
    // warm-and-exit an image that cannot be written should fail the run too; today that
    // is only a log warning and the exit is 0 (XDG_CACHE_HOME pointing at a file).
    // (x-invalid-states-10)
    no_cache: bool,
    /// Write the registration image and exit, for an installer that
    /// wants the first real run to be warm.
    #[builder(default = "false")]
    warm: bool,
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
        let mut args = vec![];
        if let Mode::Script(source) | Mode::Check(source) = &self.mode {
            if let Source::File(p) = source {
                args.push(ArcStr::from(p.display().to_string().as_str()));
            }
        }
        args.extend(self.program_args.drain(..));
        if !args.is_empty() {
            ctx.libstate.set(ProgramArgs(args));
        }
        let (vfs_modules, root) =
            register_packages(&mut ctx, self.packages.iter().map(|p| &**p))
                .context("register package modules")?;
        if let Some(main) = self.packages.iter().find_map(|p| p.main_program()) {
            if matches!(self.mode, Mode::Repl) {
                self.mode = Mode::Script(Source::Internal(ArcStr::from(main)));
            }
        }
        let program = match &self.mode {
            Mode::Script(source) => Some(source.clone()),
            Mode::Check(_) | Mode::Repl => None,
        };
        let program_text: Option<Vec<u8>> = program.as_ref().and_then(|s| match s {
            Source::File(p) => std::fs::read(p).ok(),
            Source::Internal(text) => Some(text.as_bytes().to_vec()),
            _ => None,
        });
        let mut flags = match self.mode {
            Mode::Script(_) | Mode::Check(_) => CFlag::WarnUnhandled | CFlag::WarnUnused,
            Mode::Repl => CFlag::ReplaceImports.into(),
        };
        flags.insert(self.enable_flags);
        flags.remove(self.disable_flags);
        // The compile flags shape the program's graph (fusion on or off).
        let cache = if self.no_cache {
            None
        } else {
            match cache::RegistrationCache::new(
                &root,
                program_text.as_deref(),
                flags.bits() as u64,
            ) {
                Ok(c) => Some(c),
                Err(e) => {
                    log::warn!("image cache unavailable: {e}");
                    None
                }
            }
        };
        let mut pending_registration = None;
        let mut pending_program = None;
        let mut program_image = None;
        let registration = match cache.as_ref() {
            None => None,
            Some(c) => {
                // CR claude for claude: [bug] An entry that fails to read is never
                // replaced. A loaded entry arms no save, and `program_loaded` keeps
                // `program_image` unarmed, so when the restore fails
                // (graphix-rt/src/gx.rs:296) the runtime compiles cold and never
                // rewrites the entry it was given. A bad program entry also skips the
                // intact registration entry. A zero-length file left by a crash after
                // the un-fsynced write (cache.rs:198), or an entry written by a host
                // with other CPU features (refused by the header's ISA check), makes
                // every later start of that program cold until the build id changes; a
                // bad registration entry does the same to every --check and REPL start.
                // `--warm` exits 0 without replacing it, and the only sign is a
                // --log-dir warning that says "registration image" for either entry and
                // names no path. probe: design/review-2026-10-05/repro/x-image-07.sh
                // (x-image-07)
                let loaded = c
                    .load(Entry::Program)
                    .map(|b| (b, true))
                    .or_else(|| c.load(Entry::Registration).map(|b| (b, false)));
                let program_loaded = matches!(loaded, Some((_, true)));
                if c.has_program() && !program_loaded {
                    let (tx, rx) = oneshot::channel();
                    program_image = Some(tx);
                    pending_program = Some(rx);
                }
                // The registration entry serves a later run of a
                // different program under these packages; a program
                // built into the binary is the only one it runs, so
                // its cold start skips the encode.
                let embedded = matches!(self.mode, Mode::Script(Source::Internal(_)));
                match loaded {
                    Some((bytes, _)) => Some(RegistrationImage::Load(bytes)),
                    None if embedded => None,
                    None => {
                        let (tx, rx) = oneshot::channel();
                        pending_registration = Some(rx);
                        Some(RegistrationImage::Save(tx))
                    }
                }
            }
        };
        let mut mods = vec![VfsResolver::new(vfs_modules)];
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
            .context("loading initial modules")?;
        if let Some(cache) = cache.as_ref() {
            for (entry, rx) in [
                (Entry::Registration, pending_registration),
                (Entry::Program, pending_program),
            ] {
                let Some(rx) = rx else { continue };
                match rx.await {
                    Ok(Ok(image)) => {
                        if let Err(e) = cache.store(entry, &image) {
                            log::warn!("{entry:?} image not written: {e:#}");
                        }
                    }
                    Ok(Err(e)) => log::warn!("{entry:?} image not taken: {e:#}"),
                    Err(_) => log::warn!("{entry:?} image not taken: runtime exited"),
                }
            }
        }
        Ok(handle)
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
                exit(0)
            }
            Mode::Script(_) => {
                let r = gx
                    .program()
                    .await?
                    .ok_or_else(|| anyhow!("the runtime has no program"))?;
                // CR claude for claude: [bug] --fusion-stats prints the fusion counters
                // of this process's own compile. A warm start restores the program
                // entry and fuses nothing, so the second run of a program prints
                // 'fusion: 0 of 0 attempted regions fused' where the cold run printed
                // '1 of 2'. --check never fuses either (CheckOnly,
                // graphix-rt/src/gx.rs:837-841), so `--check --fusion-stats` always
                // prints 0 of 0; only --expand builds. Skip the program entry when
                // fusion_stats is set (or carry the profile in the image), and refuse
                // the flag under --check without --expand. probe:
                // design/review-2026-10-05/repro/shell-09.gx (shell-09)
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
                Err(e) if e.is::<CouldNotResolve>() => {
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

    pub async fn run(mut self, run_on_main: MainThreadHandle) -> Result<()> {
        let (tx, mut from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        // CR claude for claude: [bug] --warm returns here without asking for the
        // program's result. GX::new keeps a program compile error in `program`, and
        // only load_env's gx.program() reports it. So `graphix --warm broken.gx` writes
        // only the registration entry, prints nothing and exits 0. With --log-dir the
        // log says only "Program image not taken: runtime exited". Check mode compiles
        // no program in init, so `--warm --check` and `--warm --expand` exit 0 without
        // checking, and `--warm --no-cache` does nothing; await gx.program() under
        // --warm and return its error, and refuse --warm together with --check,
        // --expand or --no-cache in main.rs. probe: GRAPHIX=<bin> bash
        // design/review-2026-10-05/repro/shell-07.sh (shell-07)
        if self.warm {
            return Ok(());
        }
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
        let exit = loop {
            select! {
                batch = from_gx.recv() => match batch {
                    None => bail!("graphix runtime is dead"),
                    Some(mut batch) => {
                        for e in batch.drain(..) {
                            match e {
                                GXEvent::Updated(id, v) => {
                                    output.process_update(&env, id, v).await
                                },
                                GXEvent::Env(e) => {
                                    env = e;
                                    newenv = Some(env.clone());
                                }
                            }
                        }
                    }
                },
                input = input.read_line(&mut output, &mut newenv) => {
                    match input {
                        Err(e) if script => break Err(e),
                        // CR claude for claude: [bug] In REPL mode this arm prints any
                        // error from read_line and goes round again. It was written for
                        // a failed display, but reedline's errors land here too. With
                        // no controlling terminal (ssh without -t, CI, cron, a systemd
                        // unit), reedline's read_line fails at once on every call:
                        // crossterm opens /dev/tty and gets ENXIO. So `graphix` with no
                        // file spins at about 140% CPU, printing `error: No such device
                        // or address (os error 6)` tens of thousands of times a second.
                        // It never exits, even with stdin at EOF, and piped input is
                        // never read. A dead reader task (`input stream ended`) loops
                        // the same way. An input error with no display up should end
                        // the REPL, or a non-tty stdin should be read line by line.
                        // probe: design/review-2026-10-05/repro/c-lib-05.sh (c-lib-05)
                        Err(e) => {
                            eprintln!("error: {e:?}");
                            // A display that failed is still the output.
                            gx.interrupt();
                            output.clear().await;
                        }
                        Ok(Signal::CtrlC) if script => break Ok(()),
                        Ok(Signal::CtrlC) => {
                            // Interrupt first: a wedged runtime cannot serve `output.clear()`.
                            gx.interrupt();
                            output.clear().await;
                        }
                        Ok(Signal::CtrlD) | Ok(Signal::ExternalBreak(_)) => break Ok(()),
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
