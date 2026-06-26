#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, ExecCtx, PrintFlag,
    env::Env,
    expr::{CouldNotResolve, ExprId, ModuleResolver, Source},
    format_with_flags,
    typ::TVal,
};
use graphix_package::{
    Cdc, CustomResult, IndexSet, MainThreadHandle, Package, root_module_source,
};
use graphix_package_core::ProgramArgs;
use graphix_rt::{CompExp, GXConfig, GXEvent, GXExt, GXHandle, GXRt};
use input::InputReader;
use netidx::{
    publisher::{Publisher, Value},
    subscriber::Subscriber,
};
use poolshark::global::GPooled;
use reedline::Signal;
use std::{marker::PhantomData, process::exit, time::Duration};
use tokio::{select, sync::mpsc};

mod completion;
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
    ) -> Self {
        // Offer the value to each package in turn; the first to claim it
        // (returning `Custom`) wins, otherwise it falls through to text.
        let mut e = e;
        for pkg in packages {
            match pkg.maybe_init_custom(gx, env, e, run_on_main).await {
                Err(err) => {
                    eprintln!("error initializing custom display: {err:?}");
                    return Self::None;
                }
                Ok(CustomResult::Custom(cdc)) => return Self::Custom(cdc),
                Ok(CustomResult::NotCustom(ret)) => e = ret,
            }
        }
        Self::Text(e)
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

#[derive(Builder)]
#[builder(pattern = "owned")]
pub struct Shell<X: GXExt> {
    /// do not run the users init module
    #[builder(default = "false")]
    no_init: bool,
    /// drop subscribers if they don't consume updates after this timeout
    #[builder(setter(strip_option), default)]
    publish_timeout: Option<Duration>,
    /// module resolution from netidx will fail if it can't subscribe
    /// before this time elapses
    #[builder(setter(strip_option), default)]
    resolve_timeout: Option<Duration>,
    /// define module resolvers to append to the default list
    #[builder(default)]
    module_resolvers: Vec<ModuleResolver>,
    /// set the shell's mode
    #[builder(default = "Mode::Repl")]
    mode: Mode,
    /// The netidx publisher to use. If you do not wish to use netidx
    /// you can use netidx::InternalOnly to create an internal netidx
    /// environment
    publisher: Publisher,
    /// The netidx subscriber to use. If you do not wish to use netidx
    /// you can use netidx::InternalOnly to create an internal netidx
    /// environment
    subscriber: Subscriber,
    /// Enable compiler flags, these will be ORed with the default set of flags
    /// for the mode.
    #[builder(default)]
    enable_flags: BitFlags<CFlag>,
    /// Disable compiler flags, these will be subtracted from the final set.
    /// (default_flags | enable_flags) - disable_flags
    #[builder(default)]
    disable_flags: BitFlags<CFlag>,
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
        let publisher = self.publisher.clone();
        let subscriber = self.subscriber.clone();
        let mut ctx = ExecCtx::new(GXRt::<X>::new(publisher, subscriber))
            .context("creating graphix context")?;
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
        let mut vfs_modules = AHashMap::default();
        let mut root_mods = IndexSet::new();
        for pkg in &self.packages {
            pkg.register(&mut ctx, &mut vfs_modules, &mut root_mods)
                .context("register package modules")?;
        }
        let root = root_module_source(&root_mods);
        if let Some(main) = self.packages.iter().find_map(|p| p.main_program()) {
            if matches!(self.mode, Mode::Repl) {
                self.mode = Mode::Script(Source::Internal(ArcStr::from(main)));
            }
        }
        let mut flags = match self.mode {
            Mode::Script(_) | Mode::Check(_) => CFlag::WarnUnhandled | CFlag::WarnUnused,
            Mode::Repl => BitFlags::empty(),
        };
        flags.insert(self.enable_flags);
        flags.remove(self.disable_flags);
        let mut mods = vec![ModuleResolver::VFS(vfs_modules)];
        for res in self.module_resolvers.drain(..) {
            mods.push(res);
        }
        let mut gx = GXConfig::builder(ctx, sub);
        gx = gx.flags(flags);
        if let Some(s) = self.publish_timeout {
            gx = gx.publish_timeout(s);
        }
        if let Some(s) = self.resolve_timeout {
            gx = gx.resolve_timeout(s);
        }
        let handle = gx
            .root(root)
            .resolvers(mods)
            .build()
            .context("building rt config")?
            .start()
            .await
            .context("loading initial modules")?;
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
            Mode::Check(source) => {
                let initial_scope = match source {
                    Source::File(p) => graphix_lsp::workspace::detect_package_scope(p),
                    _ => None,
                };
                gx.check(source.clone(), initial_scope).await?;
                exit(0)
            }
            Mode::Script(source) => {
                let r = gx.load(source.clone()).await?;
                exprs.extend(r.exprs);
                env = gx.get_env().await?;
                if let Some(e) = exprs.pop() {
                    *output =
                        Output::from_expr(&gx, &env, e, run_on_main, &self.packages)
                            .await;
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

    pub async fn run(mut self, run_on_main: MainThreadHandle) -> Result<()> {
        let (tx, mut from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
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
        loop {
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
                        Err(e) => eprintln!("error reading line {e:?}"),
                        Ok(Signal::CtrlC) if script => break Ok(()),
                        Ok(Signal::CtrlC) => {
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
                                        output = Output::from_expr(
                                            &gx, &env, e, &run_on_main,
                                            &self.packages,
                                        ).await;
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
        }
    }
}
