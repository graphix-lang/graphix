#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashMap;
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    env::Env,
    expr::{CouldNotResolve, ExprId, ModuleResolver, Source, VfsEntry},
    format_with_flags,
    typ::TVal,
    CFlag, ExecCtx, PrintFlag,
};
use graphix_package::{IndexSet, MainThreadHandle};
use graphix_package_core::ProgramArgs;
use graphix_rt::{CompExp, GXConfig, GXEvent, GXExt, GXHandle, GXRt};
use input::InputReader;
use netidx::{
    publisher::{Publisher, Value},
    subscriber::Subscriber,
};
use netidx_core::path::Path;
use poolshark::global::GPooled;
use reedline::Signal;
use std::{future::Future, marker::PhantomData, pin::Pin, process::exit, time::Duration};
use tokio::{select, sync::mpsc};

mod completion;
/// Package registration for graphix-shell. Made public so `graphix
/// compile`'s in-process typecheck step (in `main.rs`) can drive the
/// same stdlib-registration path the shell uses at startup.
pub mod deps;
mod input;
pub mod lsp_backend;

/// The `ShellBuilder::register_packages` hook: register additional (external or
/// embedder-supplied) packages into `ctx`/`modules`/`root_mods`, called once
/// after the stdlib packages have been registered. This is the same contract
/// the package manager generates for installed external packages.
pub type RegisterPackagesFn<X> = Box<
    dyn FnOnce(
        &mut ExecCtx<GXRt<X>, <X as GXExt>::UserEvent>,
        &mut AHashMap<Path, VfsEntry>,
        &mut IndexSet<ArcStr>,
    ) -> Result<()>,
>;

/// `register_packages` as a plain function pointer (no captured state). Used by
/// the LSP backend, where registration may run in a re-invokable context and a
/// one-shot `FnOnce` won't do. A generated/embedder `packages::register` (a
/// non-capturing `fn`) coerces to this.
pub type RegisterPackagesPtr<X> = fn(
    &mut ExecCtx<GXRt<X>, <X as GXExt>::UserEvent>,
    &mut AHashMap<Path, VfsEntry>,
    &mut IndexSet<ArcStr>,
) -> Result<()>;

/// The `ShellBuilder::custom_display` hook: dispatch a compiled expression to a
/// custom display for additional packages (the same contract as a package's
/// `is_custom`/`init_custom`). Return `NotCustom(e)` to pass the expression
/// through to normal text display.
pub type CustomDisplayFn<X> = Box<
    dyn for<'a> Fn(
        &'a GXHandle<X>,
        &'a Env,
        CompExp<X>,
        &'a MainThreadHandle,
    )
        -> Pin<Box<dyn Future<Output = Result<deps::CustomResult<X>>> + 'a>>,
>;

fn noop_register_packages<X: GXExt>() -> RegisterPackagesFn<X> {
    Box::new(|_, _, _| Ok(()))
}

fn passthrough_custom_display<X: GXExt>() -> CustomDisplayFn<X> {
    Box::new(|_gx, _env, e, _rom| {
        Box::pin(async move { Ok(deps::CustomResult::NotCustom(e)) })
    })
}

enum Output<X: GXExt> {
    None,
    EmptyScript,
    Custom(deps::Cdc<X>),
    Text(CompExp<X>),
}

impl<X: GXExt> Output<X> {
    async fn from_expr(
        gx: &GXHandle<X>,
        env: &Env,
        e: CompExp<X>,
        run_on_main: &MainThreadHandle,
        custom: &CustomDisplayFn<X>,
    ) -> Self {
        // Try the stdlib custom-display packages first, then fall through to the
        // additional-packages hook (external / embedder-supplied).
        let e = match deps::maybe_init_custom(gx, env, e, run_on_main).await {
            Err(e) => {
                eprintln!("error initializing custom display: {e:?}");
                return Self::None;
            }
            Ok(deps::CustomResult::Custom(cdc)) => return Self::Custom(cdc),
            Ok(deps::CustomResult::NotCustom(e)) => e,
        };
        match custom(gx, env, e, run_on_main).await {
            Err(e) => {
                eprintln!("error initializing custom display: {e:?}");
                Self::None
            }
            Ok(deps::CustomResult::Custom(cdc)) => Self::Custom(cdc),
            Ok(deps::CustomResult::NotCustom(e)) => Self::Text(e),
        }
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
    /// Register additional (external / embedder-supplied) packages after the
    /// stdlib packages. Set via the custom `register_packages` setter.
    #[builder(setter(custom), default = "noop_register_packages()")]
    register_packages: RegisterPackagesFn<X>,
    /// A default program to run (e.g. a standalone build's embedded program).
    /// If set and the mode is Repl, the shell runs this program instead of the
    /// REPL.
    #[builder(default)]
    main_program: Option<&'static str>,
    /// Custom-display dispatcher for additional packages. Set via the custom
    /// `custom_display` setter.
    #[builder(setter(custom), default = "passthrough_custom_display()")]
    custom_display: CustomDisplayFn<X>,
    #[builder(setter(skip), default)]
    _phantom: PhantomData<X>,
}

impl<X: GXExt> ShellBuilder<X> {
    /// Register additional packages (external or embedder-supplied) after the
    /// stdlib — the same hook the package manager generates for installed
    /// external packages. Called once during shell init.
    pub fn register_packages<F>(mut self, f: F) -> Self
    where
        F: FnOnce(
                &mut ExecCtx<GXRt<X>, X::UserEvent>,
                &mut AHashMap<Path, VfsEntry>,
                &mut IndexSet<ArcStr>,
            ) -> Result<()>
            + 'static,
    {
        self.register_packages = Some(Box::new(f));
        self
    }

    /// Set the custom-display dispatcher for additional packages.
    pub fn custom_display(mut self, f: CustomDisplayFn<X>) -> Self {
        self.custom_display = Some(f);
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
        let register_packages =
            std::mem::replace(&mut self.register_packages, noop_register_packages());
        let result = deps::register::<X>(&mut ctx, &mut vfs_modules, register_packages)
            .context("register package modules")?;
        if let Some(main) = self.main_program {
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
            .root(result.root)
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
                    *output = Output::from_expr(
                        &gx,
                        &env,
                        e,
                        run_on_main,
                        &self.custom_display,
                    )
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
                                            &self.custom_display,
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
