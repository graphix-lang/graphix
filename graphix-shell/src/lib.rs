use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use graphix_compiler::{
    env::Env,
    expr::{CouldNotResolve, ExprId, ModuleResolver, Source},
    format_with_flags,
    typ::TVal,
    CFlag, ExecCtx, PrintFlag,
};
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
mod deps;
mod input;

enum Output<X: GXExt> {
    None,
    EmptyScript,
    Custom(deps::Cdc<X>),
    Text(CompExp<X>),
}

impl<X: GXExt> Output<X> {
    fn from_expr(gx: &GXHandle<X>, env: &Env, e: CompExp<X>) -> Self {
        match deps::maybe_init_custom(gx, env, e) {
            Err(e) => {
                eprintln!("error initializing custom display: {e:?}");
                Self::None
            }
            Ok(deps::CustomResult::Custom(cdc)) => Self::Custom(cdc),
            Ok(deps::CustomResult::NotCustom(e)) => Self::Text(e),
        }
    }

    async fn clear(&mut self) {
        match self {
            Self::None | Self::Text(_) | Self::EmptyScript => (),
            Self::Custom(cdc) => cdc.custom.clear().await,
        }
        *self = Self::None
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
    #[builder(setter(skip), default)]
    _phantom: PhantomData<X>,
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
        let mut vfs_modules = FxHashMap::default();
        let root = deps::register::<X>(&mut ctx, &mut vfs_modules)
            .context("register package modules")?;
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
        Ok(gx
            .root(root)
            .resolvers(mods)
            .build()
            .context("building rt config")?
            .start()
            .await
            .context("loading initial modules")?)
    }

    async fn load_env(
        &mut self,
        gx: &GXHandle<X>,
        newenv: &mut Option<Env>,
        output: &mut Output<X>,
        exprs: &mut Vec<CompExp<X>>,
    ) -> Result<Env> {
        let env;
        match &self.mode {
            Mode::Check(source) => {
                gx.check(source.clone()).await?;
                exit(0)
            }
            Mode::Script(source) => {
                let r = gx.load(source.clone()).await?;
                exprs.extend(r.exprs);
                env = gx.get_env().await?;
                if let Some(e) = exprs.pop() {
                    *output = Output::from_expr(&gx, &env, e);
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

    pub async fn run(mut self) -> Result<()> {
        let (tx, mut from_gx) = mpsc::channel(100);
        let gx = self.init(tx).await?;
        let script = self.mode.file_mode();
        let mut input = InputReader::new();
        let mut output = if script { Output::EmptyScript } else { Output::None };
        let mut newenv = None;
        let mut exprs = vec![];
        let mut env = self.load_env(&gx, &mut newenv, &mut output, &mut exprs).await?;
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
                        Ok(Signal::CtrlC) => output.clear().await,
                        Ok(Signal::CtrlD) => break Ok(()),
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
                                            PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
                                            || println!("-: {}", typ)
                                        );
                                        output = Output::from_expr(&gx, &env, e);
                                    } else {
                                        output.clear().await
                                    }
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}
