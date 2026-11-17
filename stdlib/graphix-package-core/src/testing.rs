use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use graphix_compiler::{
    BindId, CFlag, ParMode, PruneMode,
    env::Env,
    expr::{ExprId, Load, Origin, ResolverRef, Source, VfsEntry, VfsResolver},
    rep,
    typ::Type,
};
use graphix_rt::{
    Callable, CompRes, GXConfig, GXConfigBuilder, GXEvent, GXHandle, GXRt, NoExt, Ref,
    RegistrationImage,
};
use netidx::publisher::Value;
use netidx_core::path::Path;
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{fmt::Write, sync::LazyLock, time::Duration};
use tokio::{
    sync::{OnceCell, mpsc, oneshot},
    time::Instant,
};

pub struct TestCtx {
    pub rt: GXHandle<NoExt>,
}

/// The fusion outcome a [`run!`] fixture asserts for its program,
/// checked bidirectionally in `jit` mode: fusing when it shouldn't, or
/// failing to when it should, fails the test.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FuseExpect {
    /// The program builds a fused kernel and the JIT runs it natively.
    // CR claude for claude: [doc-drift/low] These variant docs promise more than
    // check_fuse_expectation asserts. Jit says the JIT runs the kernel, and None says
    // the program makes no kernel at all. Both are judged only by CompRes::fused at the
    // end of the build: a kernel need not run before the judged value, and a run-time
    // bind fuses later. Say what is asserted: Jit means the build fuses a region, None
    // means the build fuses none. The run! doc below also says each mode is a
    // #[tokio::test(flavor = "current_thread")]; the expansion is a plain #[test]
    // calling run_fixture. Found by reading; not run. (core-aux-06)
    Jit,
    /// The program produces no fused kernel at all.
    None,
}

/// What a fixture's jit run observed of fusion: the regions its build
/// fused, `None` when the build was refused (a refusal asserts nothing
/// about fusion), and the kernel and JIT-wrapper runs counted while it ran.
#[derive(Debug, Clone, Copy)]
pub struct FuseObserved {
    pub regions: Option<usize>,
    pub runs: (u64, u64),
}

/// Assert what a fixture's build fused matches `expect`: `Jit` when it
/// fused a region (a fixture whose point is that one expression fuses
/// says `#[native]` on it), `None` when it fused none. A region may first
/// run after the value the fixture judges, so runs decide nothing; a
/// region that ran must have entered its JIT wrapper.
#[cfg(debug_assertions)]
pub fn check_fuse_expectation(observed: FuseObserved, expect: FuseExpect) {
    let Some(regions) = observed.regions else { return };
    let (fusion, jit) = observed.runs;
    match expect {
        FuseExpect::Jit => {
            assert!(
                regions > 0,
                "fuse: Jit — expected the build to fuse a region but it fused none \
                 (the JIT couldn't compile it, so it node-walks). Downgrade to \
                 `fuse: None`, or fix the cliff.",
            );
            assert!(
                fusion == 0 || jit > 0,
                "fuse: Jit — FUSION>0 but JIT_INVOCATIONS=0: a fused \
                 region ran without entering its JIT wrapper; investigate.",
            );
        }
        FuseExpect::None => {
            assert!(
                regions == 0,
                "fuse: None — expected NO fusion but the build fused {regions} \
                 region(s); this program now fuses. Upgrade the annotation to \
                 `fuse: Jit`.",
            );
        }
    }
}

impl TestCtx {
    /// Start the runtime `cfg` configures ([`config`]).
    pub async fn start(cfg: GXConfigBuilder<NoExt>) -> Result<Self> {
        let st = std::time::Instant::now();
        let rt = cfg.build()?.start().await?;
        log::info!("runtime start time: {:?}", st.elapsed());
        Ok(TestCtx { rt })
    }

    pub async fn shutdown(self) {
        drop(self.rt);
    }

    /// Snapshot the compile-time fusion outcome counters. See
    /// [`graphix_compiler::FusionStats`] and [`GXHandle::fusion_stats`].
    pub async fn fusion_stats(&self) -> Result<graphix_compiler::FusionStats> {
        self.rt.fusion_stats().await
    }
}

/// A package instance to register into a test context. (Packages are ZSTs, so
/// `&'static dyn` references are free.) Test registries are built by the
/// `graphix_package::package_refs!()` macro.
pub type PackageRef = &'static dyn graphix_package::Package<NoExt>;

/// A test runtime's configuration: `register`'s packages registered,
/// `setup` run over the context, their root and `resolvers` (after the
/// packages') set. Chain what else it needs (flags, a registration
/// image, a program, a trace, lsp_mode) and run it with
/// [`TestCtx::start`], which compiles the root unless a registration
/// image restores it.
pub fn config(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    setup: impl FnOnce(&mut TestState),
) -> Result<GXConfigBuilder<NoExt>> {
    Ok(configure(sub, register, resolvers, setup)?.0)
}

/// [`config`], and the root it compiles: the text naming `register`'s
/// packages.
fn configure(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    setup: impl FnOnce(&mut TestState),
) -> Result<(GXConfigBuilder<NoExt>, ArcStr)> {
    let _ = env_logger::try_init();
    // A fixture that recurses without end must abort instead of
    // eating the box; an explicit GRAPHIX_STACK_BUDGET still wins.
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        graphix_compiler::set_stack_budget(1 << 30);
    }
    // Nothing seeds NetConfig, so a runtime that touches sys::net gets
    // a process-internal netidx of its own, on demand: tests may reuse
    // paths.
    let st = std::time::Instant::now();
    let mut ctx = GXRt::<NoExt>::new_state()?;
    log::info!("context creation time: {:?}", st.elapsed());
    let st = std::time::Instant::now();
    let (modules, root) =
        graphix_package::register_packages(&mut ctx, register.iter().copied())?;
    log::info!("package registration time: {:?}", st.elapsed());
    setup(&mut ctx);
    let mut all_resolvers = vec![modules];
    all_resolvers.extend(resolvers);
    let cfg = GXConfig::builder(ctx, sub).root(root.clone()).resolvers(all_resolvers);
    Ok((cfg, root))
}

/// Where a test runtime's package root comes from.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Root {
    /// Compiled from source at the start, as `--no-cache`, `--check` and
    /// the language server compile it.
    Compiled,
    /// Restored from the image of that root this process compiled once,
    /// as a warm shell start restores it.
    Restored,
}

impl Root {
    /// `self`, unless `GRAPHIX_TEST_ROOT=compile` has every start compile
    /// its root.
    pub fn chosen(self) -> Root {
        match std::env::var("GRAPHIX_TEST_ROOT") {
            Err(_) => self,
            Ok(v) if v == "compile" => Root::Compiled,
            Ok(v) => panic!("GRAPHIX_TEST_ROOT={v}: its one value is `compile`"),
        }
    }
}

/// Start [`config`]'s runtime under `flags`, its root compiled or
/// restored as `how` says. A root meant to restore that compiles instead
/// fails the start.
async fn start_rooted(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    setup: impl FnOnce(&mut TestState),
    flags: BitFlags<CFlag>,
    how: Root,
) -> Result<TestCtx> {
    let (cfg, root) = configure(sub, register, resolvers, setup)?;
    let cfg = cfg.flags(flags);
    match how.chosen() {
        Root::Compiled => TestCtx::start(cfg).await,
        Root::Restored => {
            let image = root_image(&root, register).await?;
            let ctx =
                TestCtx::start(cfg.registration(RegistrationImage::load(image))).await?;
            if !ctx.rt.env_stats().await?.restored {
                bail!(
                    "the package root's image did not restore, so the root compiled \
                     (RUST_LOG=graphix_rt=warn says why)"
                )
            }
            Ok(ctx)
        }
    }
}

/// The image of the package root `root`, which `register` registers. Its
/// flags are fixed, so the image is a function of the root: one is
/// compiled per root per process, by the first start that asks, the
/// others waiting for it. A build that fails is not kept: the next start
/// builds again.
async fn root_image(root: &ArcStr, register: &[PackageRef]) -> Result<Bytes> {
    static IMAGES: LazyLock<
        Mutex<ahash::AHashMap<ArcStr, triomphe::Arc<OnceCell<Bytes>>>>,
    > = LazyLock::new(|| Mutex::new(ahash::AHashMap::default()));
    let cell = IMAGES.lock().entry(root.clone()).or_default().clone();
    let image = cell
        .get_or_try_init(|| async {
            let (tx, _rx) = mpsc::channel(1);
            let (image_tx, image_rx) = oneshot::channel();
            let (cfg, _) = configure(tx, register, vec![], |_| {})?;
            cfg.registration(RegistrationImage::save(image_tx))
                .build()?
                .compile()
                .await?;
            image_rx.await.context("the root's compile sent no image")?
        })
        .await?;
    Ok(image.clone())
}

/// A runtime with `register`'s packages and nothing else, its root
/// restored.
pub async fn init(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
) -> Result<TestCtx> {
    start_rooted(sub, register, vec![], |_| {}, BitFlags::empty(), Root::Restored).await
}

pub type Events = mpsc::Receiver<GPooled<Vec<GXEvent>>>;

/// How a differential test runs a program: the node-walk or fused, each
/// serial or with every fork point forked.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mode {
    Interp,
    Jit,
    Par,
    JitPar,
    /// The node-walk walking every boundary: walk pruning's reference
    /// (`design/walk_pruning.md`). Not among [`Mode::ALL`].
    Unpruned,
}

impl Mode {
    pub const ALL: [Mode; 4] = [Mode::Interp, Mode::Jit, Mode::Par, Mode::JitPar];

    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp | Mode::Par | Mode::Unpruned => CFlag::FusionDisabled.into(),
            Mode::Jit | Mode::JitPar => BitFlags::empty(),
        }
    }

    pub fn node_walk(self) -> bool {
        matches!(self, Mode::Interp | Mode::Par | Mode::Unpruned)
    }

    /// Whether the runtime skips clean boundaries when the mode prunes for
    /// real: what `Unpruned` and the fuzzer's `Pair::Prune` compare against.
    pub fn prune(self) -> PruneMode {
        match self {
            Mode::Unpruned => PruneMode::Off,
            _ => PruneMode::from_env(),
        }
    }

    /// A fixture's pruning, unless `GRAPHIX_PRUNE` says otherwise: real
    /// skips in `Jit` and `Par`, every skip audited in `Interp` and
    /// `JitPar`, so each engine and each fork mode holds a predicate both
    /// to pruned runs and to the walk of every boundary. A fixture that
    /// measures pruning sets [`Mode::prune`] in its setup; one that bounds
    /// what `Auto` decides prunes for real too.
    pub fn fixture_prune(self) -> PruneMode {
        match self {
            Mode::Unpruned => PruneMode::Off,
            _ if std::env::var_os("GRAPHIX_PRUNE").is_some() => PruneMode::from_env(),
            Mode::Jit | Mode::Par => PruneMode::On,
            Mode::Interp | Mode::JitPar => PruneMode::Audit,
        }
    }

    /// The serial modes fork nothing unless `GRAPHIX_PAR` says otherwise.
    pub fn par(self) -> ParMode {
        match self {
            Mode::Interp | Mode::Jit | Mode::Unpruned => {
                match std::env::var_os("GRAPHIX_PAR") {
                    Some(_) => ParMode::from_env(),
                    None => ParMode::Off,
                }
            }
            Mode::Par | Mode::JitPar => ParMode::Force,
        }
    }
}

/// `let result = {code}`, a fixture's `/test.gx` text.
pub fn result_text(code: &str) -> ArcStr {
    ArcStr::from(format_compact!("let result = {code}").as_str())
}

/// [`result_text`] as a resolver entry.
pub fn result_source(code: &str) -> VfsEntry {
    VfsEntry::from(result_text(code))
}

/// A runtime for `mode` whose resolver serves `files`, with the
/// receiver of its events.
pub async fn fixture_runtime<'a, F>(
    files: impl IntoIterator<Item = (&'a str, VfsEntry)>,
    register: &[PackageRef],
    mode: Mode,
    setup: F,
) -> Result<(TestCtx, Events)>
where
    F: FnOnce(&mut TestState),
{
    let (tx, rx) = mpsc::channel(1024);
    let tbl = ahash::AHashMap::from_iter(
        files.into_iter().map(|(path, entry)| (Path::from(ArcStr::from(path)), entry)),
    );
    let resolvers = vec![VfsResolver::new(tbl)];
    let setup = |ctx: &mut TestState| {
        ctx.control.set_par_mode(mode.par());
        ctx.control.set_prune_mode(mode.fixture_prune());
        setup(ctx)
    };
    let ctx = start_rooted(tx, register, resolvers, setup, mode.flags(), Root::Restored)
        .await?;
    Ok((ctx, rx))
}

/// One [`run!`] fixture: its program's files, the predicate its value or
/// refusal must satisfy, and what its jit run must fuse.
pub struct RunFixture {
    pub name: &'static str,
    pub files: Vec<(&'static str, ArcStr)>,
    pub pred: Box<dyn Fn(Result<&Value>) -> bool>,
    pub expect: FuseExpect,
    pub shape: Option<graphix_compiler::node_shape::NodeShape>,
    pub register: &'static [PackageRef],
}

/// `f` as a [`RunFixture`]'s predicate.
pub fn predicate(
    f: impl Fn(Result<&Value>) -> bool + 'static,
) -> Box<dyn Fn(Result<&Value>) -> bool> {
    Box::new(f)
}

/// Run `f` in `mode` on a runtime of its own; the node-walk also holds
/// the check to the build when `check`.
pub fn run_fixture(f: RunFixture, mode: Mode, check: bool) -> Result<()> {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?
        .block_on(fixture_test(&f, mode, check))
}

async fn fixture_test(f: &RunFixture, mode: Mode, check: bool) -> Result<()> {
    if mode != Mode::Jit {
        return fixture_mode(f, mode, false, false, check).await.map(|_| ());
    }
    // GRAPHIX_FUSION_DISCOVERY: run without asserting and print the
    // observed level (`FUSEMAP <path> <level>`).
    #[cfg(debug_assertions)]
    if std::env::var("GRAPHIX_FUSION_DISCOVERY").is_ok() {
        let observed = fixture_mode(f, Mode::Jit, true, false, false).await?;
        let (_, jit) = observed.runs;
        eprintln!(
            "FUSEMAPF\t{}\t{}",
            f.name,
            if observed.regions.unwrap_or(0) > 0 { "Fuses" } else { "None" },
        );
        eprintln!("FUSEMAPJ\t{}\t{}", f.name, if jit > 0 { "Jit" } else { "NoJit" });
        return Ok(());
    }
    // GRAPHIX_FUSE_AUDIT: report the observed fusion level against the
    // annotation (`FUSEAUDIT` lines) instead of asserting it.
    #[cfg(debug_assertions)]
    if std::env::var("GRAPHIX_FUSE_AUDIT").is_ok() {
        let regions =
            fixture_mode(f, Mode::Jit, true, false, false).await?.regions.unwrap_or(0);
        let observed = if regions > 0 { FuseExpect::Jit } else { FuseExpect::None };
        eprintln!(
            "FUSEAUDIT\t{}\t{:?}\t{:?}\t{}",
            f.name,
            f.expect,
            observed,
            if f.expect == observed { "OK" } else { "MISMATCH" },
        );
        return Ok(());
    }
    let _invocations = fixture_mode(f, Mode::Jit, true, true, false).await?;
    #[cfg(debug_assertions)]
    check_fuse_expectation(_invocations, f.expect);
    Ok(())
}

/// What a fixture gives under the check by bounds, where that is not what
/// its own predicate says: the check as it is and the check by bounds are
/// both tested until one replaces the other, and where they differ by rule
/// (a refusal's words, a program one refuses and the other takes) the
/// difference is written in `graphix-tests/bounds.manifest`, a line a
/// fixture: its name, `refuses` or `accepts`, and the refusal's last
/// message or the value printed.
#[derive(Debug)]
enum ByBounds {
    Refuses(String),
    Accepts(String),
}

impl ByBounds {
    fn observed(v: Result<&Value>) -> Self {
        match v {
            Ok(v) => Self::Accepts(v.to_string()),
            Err(e) => Self::Refuses(renumbered(&e.root_cause().to_string()).to_string()),
        }
    }

    fn holds(&self, v: Result<&Value>) -> bool {
        match (self, v) {
            (Self::Accepts(text), Ok(v)) => *text == v.to_string(),
            (Self::Refuses(text), Err(e)) => {
                renumbered(&format!("{e:#}")).contains(text.as_str())
            }
            _ => false,
        }
    }

    fn line(&self, name: &str) -> String {
        let flat = |s: &str| s.replace('\\', "\\\\").replace('\n', "\\n").replace('\t', " ");
        match self {
            Self::Refuses(text) => format!("{name}\trefuses\t{}", flat(text)),
            Self::Accepts(text) => format!("{name}\taccepts\t{}", flat(text)),
        }
    }
}

/// Whether the tests run with the check by bounds switched on.
pub fn by_bounds() -> bool {
    std::env::var_os("GRAPHIX_BOUNDS").is_some()
}

static BY_BOUNDS: LazyLock<ahash::AHashMap<String, ByBounds>> = LazyLock::new(|| {
    let path = std::env::var("GRAPHIX_BOUNDS_MANIFEST").unwrap_or_else(|_| {
        concat!(env!("CARGO_MANIFEST_DIR"), "/../graphix-tests/bounds.manifest").into()
    });
    let text = std::fs::read_to_string(path).unwrap_or_default();
    let unflat = |s: &str| s.replace("\\n", "\n").replace("\\\\", "\\");
    let entry = |line: &str| {
        let mut parts = line.splitn(3, '\t');
        let (name, kind, text) = (parts.next()?, parts.next()?, parts.next()?);
        let expected = match kind {
            "refuses" => ByBounds::Refuses(unflat(text)),
            "accepts" => ByBounds::Accepts(unflat(text)),
            _ => return None,
        };
        Some((name.to_string(), expected))
    };
    text.lines().filter(|l| !l.starts_with('#')).filter_map(entry).collect()
});

/// The fixture's predicate over `v`: under the check by bounds, what the
/// manifest says of it where it says anything. With `GRAPHIX_BOUNDS_BLESS`
/// naming a file, a fixture that fails there has what it gave appended to
/// the file, to be reviewed and moved to the manifest.
fn expected(f: &RunFixture, mode: Mode, v: Result<&Value>) -> bool {
    if !by_bounds() {
        return (f.pred)(v);
    }
    let held = match BY_BOUNDS.get(f.name) {
        Some(expected) => expected.holds(match &v {
            Ok(v) => Ok(*v),
            Err(e) => Err(anyhow::anyhow!("{e:#}")),
        }),
        None => (f.pred)(match &v {
            Ok(v) => Ok(*v),
            Err(e) => Err(anyhow::anyhow!("{e:#}")),
        }),
    };
    if !held
        && mode == Mode::Interp
        && let Ok(path) = std::env::var("GRAPHIX_BOUNDS_BLESS")
    {
        use std::io::Write as _;
        static FILE: Mutex<()> = Mutex::new(());
        let _one = FILE.lock();
        let line = ByBounds::observed(v).line(f.name);
        if let Ok(mut file) =
            std::fs::OpenOptions::new().create(true).append(true).open(path)
        {
            let _ = writeln!(file, "{line}");
        }
    }
    held
}

async fn fixture_mode(
    f: &RunFixture,
    mode: Mode,
    reset_counters_after_init: bool,
    check_shape: bool,
    check: bool,
) -> Result<FuseObserved> {
    let (ctx, mut rx) = fixture_runtime(
        f.files.iter().map(|(path, code)| (*path, VfsEntry::from(code.clone()))),
        f.register,
        mode,
        |_| {},
    )
    .await?;
    // only the fixture's own runs count
    if reset_counters_after_init {
        ctx.rt.control().reset_invocations();
    }
    let checked = if check { Some(check_fixture(&ctx).await) } else { None };
    // the stdlib root's own cycles fork too: count the fixture's
    let forks_before = ctx.rt.control().forks();
    let built = compile_result(&ctx).await;
    let check_refusal = match checked {
        Some(checked) => verdicts_agree(checked, built.as_ref().map(|_| ()))?,
        None => None,
    };
    let regions = built.as_ref().ok().map(|res| res.fused);
    match built {
        Err(e) => {
            if let Err(dead) = ctx.rt.get_env().await {
                bail!("the runtime died compiling the fixture ({dead:#}): {e:#}")
            }
            if let Some(c) = check_refusal {
                assert!(expected(f, mode, dbg!(Err(c))), "the check's refusal")
            }
            assert!(expected(f, mode, dbg!(Err(e))))
        }
        Ok(res) => {
            let eid = res.exprs[0].id;
            if check_shape {
                if let Some(spec) = f.shape.clone() {
                    ctx.rt.match_shape(eid, spec).await?;
                }
            }
            let v = next_update_raw(&ctx.rt, &mut rx, eid).await?;
            let v = observed(&ctx, &res.exprs[0].typ, v).await?;
            eprintln!("{v}");
            assert!(expected(f, mode, Ok(&v)));
        }
    }
    let invocations = ctx.rt.control().invocations();
    #[cfg(debug_assertions)]
    assert!(
        !mode.node_walk() || invocations == (0, 0),
        "{mode:?} ran fused code: (kernel runs, JIT entries) = {invocations:?}"
    );
    // The blocker list includes stdlib-root noise (stats are per-ExecCtx).
    if std::env::var("GRAPHIX_FUSE_AUDIT").is_ok()
        && !mode.flags().contains(CFlag::FusionDisabled)
    {
        if let Ok(stats) = ctx.fusion_stats().await {
            for failure in stats.failed.iter() {
                eprintln!(
                    "FUSEAUDIT-BLOCKER\t{}\t{:?}\t{}",
                    f.name, failure.id, failure.reason
                );
            }
        }
    }
    // a forced mode that forked nothing: a finding when the fixture has
    // fork points
    if std::env::var("GRAPHIX_FUSE_AUDIT").is_ok()
        && matches!(mode, Mode::Par | Mode::JitPar)
        && ctx.rt.control().forks() == forks_before
    {
        eprintln!("PARAUDIT-NOFORK\t{}\t{mode:?}", f.name);
    }
    ctx.shutdown().await;
    Ok(FuseObserved { regions, runs: invocations })
}

/// A [`run_with_tempdir!`] fixture's program, `code` inside `let result =
/// ..`, run in `mode`; its value, held to its type, goes to `expect`. The
/// node-walk also holds the check to the build. An error the program raises
/// and does not catch is printed, so a failing operation names itself
/// beside the wait it leaves unanswered.
pub async fn run_tempdir_fixture(
    code: &str,
    register: &[PackageRef],
    mode: Mode,
    expect: &dyn Fn(Value) -> Result<()>,
) -> Result<()> {
    let code =
        format_compact!("let result = {{ catch(e) println(\"uncaught: [e]\"); {code} }}");
    let entry = VfsEntry::from(ArcStr::from(code.as_str()));
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", entry)], register, mode, |_| {}).await?;
    let checked = match mode {
        Mode::Interp => Some(check_fixture(&ctx).await),
        _ => None,
    };
    let built = compile_result(&ctx).await;
    if let Some(checked) = checked {
        verdicts_agree(checked, built.as_ref().map(|_| ()))?;
    }
    // the fixture's expression lives as long as `built`
    let built = built?;
    let v = next_update_raw(&ctx.rt, &mut rx, built.exprs[0].id).await?;
    expect(observed(&ctx, &built.exprs[0].typ, v).await?)?;
    ctx.shutdown().await;
    Ok(())
}

/// Compile `{ mod test; test::result }`, the expression a fixture's
/// value is read from; it lives as long as the returned `CompRes`.
pub async fn compile_result(ctx: &TestCtx) -> Result<CompRes<NoExt>> {
    ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await
}

/// The check of a fixture's program, which a run holds against its build:
/// elaboration refusing what the check accepted is a type-system bug.
/// A check rolls back everything it compiled.
pub async fn check_fixture(ctx: &TestCtx) -> Result<()> {
    let program = Load::Text(arcstr::literal!("{ mod test; test::result }"));
    ctx.rt.check(program, None).await.map(|_| ())
}

/// The check refuses exactly when the build does: the check's refusal
/// when both refuse, which a fixture's predicate holds as it holds the
/// build's.
pub fn verdicts_agree(
    checked: Result<()>,
    built: Result<(), &anyhow::Error>,
) -> Result<Option<anyhow::Error>> {
    match (checked, built) {
        (Ok(()), Ok(())) => Ok(None),
        (Err(e), Err(_)) => Ok(Some(e)),
        (Ok(()), Err(e)) => bail!("the check accepts what the build refuses: {e:#}"),
        (Err(e), Ok(())) => bail!("the check refuses what the build accepts: {e:#}"),
    }
}

/// `v` as a test reads it: written as data ([`rep::strip`]), so a
/// tuple, struct or variant is the array a writer writes.
pub fn data(v: Value) -> Value {
    rep::strip(&v).unwrap_or(v)
}

/// What a fixture's predicate sees of its value `v`: [`data`], once `v`
/// is held to be of its static type `typ` in every form it carries.
pub async fn observed(ctx: &TestCtx, typ: &Type, v: Value) -> Result<Value> {
    let env = ctx.rt.get_env().await?;
    if !typ.is_a(&env, &v) {
        bail!(
            "the value {} is not of its type {}",
            graphix_compiler::typ::Naked(&v),
            graphix_compiler::format_with_flags(
                graphix_compiler::PrintFlag::DerefTVars,
                || typ.to_string()
            )
        )
    }
    Ok(data(v))
}

/// The next update of `id`, as [`data`].
pub async fn next_update(
    rt: &GXHandle<NoExt>,
    rx: &mut Events,
    id: ExprId,
) -> Result<Value> {
    next_update_raw(rt, rx, id).await.map(data)
}

/// The next update of `id` as the runtime delivered it. A runtime that
/// settles without one fails at once, since nothing outstanding can bring
/// it; otherwise the wait lasts as long as the program's work does under
/// the box's load, up to [`HANG_GUARD`].
pub async fn next_update_raw(
    rt: &GXHandle<NoExt>,
    rx: &mut Events,
    id: ExprId,
) -> Result<Value> {
    let settled = rt.wait_settled();
    tokio::pin!(settled);
    tokio::select! {
        v = async {
            loop {
                let mut batch = rx.recv().await.context("the runtime died")?;
                if let Some(v) = update_of(&mut batch, id) {
                    break Ok(v);
                }
            }
        } => v,
        r = &mut settled => {
            r.context("the runtime died")?;
            // every update is in the stream before the settle answers
            while let Ok(mut batch) = rx.try_recv() {
                if let Some(v) = update_of(&mut batch, id) {
                    return Ok(v);
                }
            }
            bail!("the runtime settled without an update of {id:?}")
        }
        () = tokio::time::sleep(HANG_GUARD) => {
            bail!("no update of {id:?} in {HANG_GUARD:?}")
        }
    }
}

fn update_of(batch: &mut GPooled<Vec<GXEvent>>, id: ExprId) -> Option<Value> {
    batch.drain(..).find_map(|e| match e {
        GXEvent::Updated(eid, v) if eid == id => Some(v),
        _ => None,
    })
}

/// Every update of `id` as `until` says, each wait as [`next_update`]'s.
pub async fn updates_until(
    rt: &GXHandle<NoExt>,
    rx: &mut Events,
    id: ExprId,
    until: Collect,
) -> Result<Vec<Value>> {
    match until {
        Collect::First(n) => {
            let mut values = Vec::with_capacity(n);
            while values.len() < n {
                values.push(next_update(rt, rx, id).await?);
            }
            Ok(values)
        }
        Collect::Quiet(window) => updates_until_settled(rt, rx, id, Some(window)).await,
        Collect::Settled => updates_until_settled(rt, rx, id, None).await,
    }
}

/// Where a quiet window stands: `Busy` until the runtime answers idle, the
/// window itself `Open`, then `Expired` until idle answers again; a batch
/// in that last wait reopens it, so a cycle running past the window is
/// never taken for quiet.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Window {
    Busy,
    Open,
    Expired,
}

/// Every update of `id` until the runtime settles, or, with a `quiet`
/// window, until it is idle and none arrives for the window.
async fn updates_until_settled(
    rt: &GXHandle<NoExt>,
    rx: &mut Events,
    id: ExprId,
    quiet: Option<Duration>,
) -> Result<Vec<Value>> {
    fn take(batch: &mut GPooled<Vec<GXEvent>>, id: ExprId, values: &mut Vec<Value>) {
        for e in batch.drain(..) {
            if let GXEvent::Updated(eid, v) = e
                && eid == id
            {
                values.push(data(v));
            }
        }
    }
    let mut values = Vec::new();
    let settled = rt.wait_settled();
    tokio::pin!(settled);
    let hang = tokio::time::sleep(HANG_GUARD);
    tokio::pin!(hang);
    let idle = rt.wait_idle();
    tokio::pin!(idle);
    let mut window = Window::Busy;
    loop {
        let counting = quiet.is_some() && !values.is_empty();
        let batch = async {
            match quiet {
                Some(quiet) if counting && window == Window::Open => {
                    tokio::time::timeout(quiet, rx.recv()).await
                }
                _ => Ok(rx.recv().await),
            }
        };
        tokio::select! {
            r = &mut settled => {
                if r.is_err() {
                    bail!("the runtime died")
                }
                // every update is in the stream before the settle answers
                while let Ok(mut batch) = rx.try_recv() {
                    take(&mut batch, id, &mut values)
                }
                return Ok(values);
            }
            () = &mut hang => {
                bail!("the program did not quiesce: {values:?}")
            }
            r = &mut idle, if counting && window != Window::Open => {
                r.context("the runtime died")?;
                // every update is in the stream before the idle answers
                let mut batches = 0;
                while let Ok(mut batch) = rx.try_recv() {
                    batches += usize::from(!batch.is_empty());
                    take(&mut batch, id, &mut values);
                }
                if window == Window::Expired && batches == 0 {
                    return Ok(values);
                }
                window = Window::Open;
            }
            r = batch => match r {
                Err(_) => {
                    window = Window::Expired;
                    idle.set(rt.wait_idle());
                }
                Ok(None) => bail!("the runtime died"),
                Ok(Some(mut batch)) => {
                    // the runtime answers a wait_idle request with an empty
                    // batch: it is no activity
                    if !batch.is_empty() {
                        take(&mut batch, id, &mut values);
                        window = Window::Busy;
                        idle.set(rt.wait_idle());
                    }
                }
            },
        }
    }
}

/// `values`, every one an i64.
pub fn as_i64s(values: &[Value]) -> Vec<i64> {
    values
        .iter()
        .map(|v| match v {
            Value::I64(n) => *n,
            v => panic!("expected i64, got {v:?}"),
        })
        .collect()
}

/// A [`run!`] predicate for a program that must be refused: its compile
/// error contains `phrase`. A parse error or a dead runtime does not
/// match the rule's message.
pub fn refused(phrase: &'static str) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains(phrase))
}

/// `text` with each generated variable's id (`'_<digits>`) renumbered
/// from 0 in order of first appearance, so two runs' types compare.
pub fn renumbered(mut text: &str) -> CompactString {
    let mut ids: LPooled<Vec<&str>> = LPooled::take();
    let mut out = CompactString::default();
    while let Some(at) = text.find("'_") {
        out.push_str(&text[..at + 2]);
        text = &text[at + 2..];
        let n = text.bytes().take_while(u8::is_ascii_digit).count();
        if n != 0 {
            let id = &text[..n];
            let i = ids.iter().position(|s| *s == id).unwrap_or_else(|| {
                ids.push(id);
                ids.len() - 1
            });
            write!(out, "{i}").unwrap();
            text = &text[n..];
        }
    }
    out.push_str(text);
    out
}

/// [`refused`], with the error's generated variables [`renumbered`].
pub fn refused_renumbered(phrase: &'static str) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| matches!(v, Err(e) if renumbered(&format!("{e:#}")).contains(phrase))
}

/// A [`run!`] predicate for a program whose value is `expected`.
pub fn is(expected: Value) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| matches!(v, Ok(v) if *v == expected)
}

/// A [`run!`] predicate for a program whose value prints as `expected`.
pub fn prints(expected: &'static str) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == expected)
}

/// The compile error `let result = {code}` gets, in full; an error if it
/// compiles, or if the runtime died compiling it.
// CR claude for claude: [test-gap/medium] refusal builds the program and never checks
// it, so its callers' refusals escape the rule run!'s interp mode enforces: the check
// refuses exactly when the build does. In lang::traits that covers the twelve
// check-level refusals of implementations_are_declared_ones,
// trait_method_value_frames_quote_the_program and trait_method_variadic_value_refused
// (impl placement, the orphan rule, a variadic method as a value). A change that moved
// one of them into elaboration would pass these tests. Run check_fixture first and
// require verdicts_agree. Give the callers whose refusal only a build makes (the
// #[native] and par-attribute ones) a build-only variant, as run! has build_only.
// compile_error has the same gap. Found by reading; --check refuses all twelve today.
// (tests-traits-a-05)
// CR claude for claude: [test-gap/medium] refusal, compile_error, eval (with
// eval_with_setup and first_value), collect and Fixture::new compile without the check.
// A refusal or value they pin therefore says nothing about whether --check agrees. The
// order sweeps in lang/types.rs run most of their orders only through refusal or eval:
// field_read_refuses_a_lambda_two_members_could_decide,
// literal_refuses_a_capture_two_functions_could_decide_in_every_order,
// field_read_names_the_two_functions_that_are_not_one_type and
// literal_of_schemes_beside_two_functions_in_every_order. An order-dependent split
// between check and build is exactly what they exist to catch. The gate itself is
// written out three times, in fixture_mode, run_tempdir_fixture and eval_modes. Fix:
// fold it into one helper beside compile_result that checks, compiles and calls
// verdicts_agree, and use it here, in compile_error, and in the sweeps that call eval.
// probe: the 108 sweep programs agree under graphix --check today, so this is a gap,
// not a live split. (core-aux-04)
pub async fn refusal(code: &str, register: &[PackageRef]) -> Result<String> {
    let (ctx, _rx) =
        fixture_runtime([("/test.gx", result_source(code))], register, Mode::Jit, |_| {})
            .await?;
    let r = compile_result(&ctx).await;
    if let Err(dead) = ctx.rt.get_env().await {
        bail!("the runtime died compiling {code} ({dead:#})")
    }
    ctx.shutdown().await;
    match r {
        Ok(_) => bail!("compiled: {code}"),
        Err(e) => Ok(format!("{e:#}")),
    }
}

/// The error compiling `text` gets, with `files` served by the resolver;
/// an error if it compiles.
pub async fn compile_error(
    files: &[(&str, &str)],
    text: &str,
    register: &[PackageRef],
) -> Result<anyhow::Error> {
    let tbl =
        ahash::AHashMap::from_iter(files.iter().map(|(p, t)| {
            (Path::from(ArcStr::from(*p)), VfsEntry::from(ArcStr::from(*t)))
        }));
    let (tx, _rx) = mpsc::channel(10);
    let resolvers = vec![VfsResolver::new(tbl)];
    let ctx =
        start_rooted(tx, register, resolvers, |_| {}, BitFlags::empty(), Root::Restored)
            .await?;
    let e = ctx.rt.compile(ArcStr::from(text)).await.err();
    ctx.shutdown().await;
    e.ok_or_else(|| anyhow::anyhow!("{text} compiled"))
}

/// The `BindId` of a module-qualified name like `"test::clicks"`. Scope
/// keys are generated paths (`/do…/test`), so the module is matched as a
/// suffix.
pub fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    let Some((module, var)) = name.split_once("::") else {
        bail!("expected module::var, got {name}")
    };
    let suffix = format_compact!("/{module}");
    for (scope, vars) in &env.lexical.binds {
        if Path::as_ref(&scope.0).ends_with(suffix.as_str())
            && let Some(bid) = vars.get(var)
        {
            return Ok(*bid);
        }
    }
    bail!("no binding {name} found in env")
}

/// Compile the lambda bound to `name` into a callable. The `Ref` and the
/// `Callable` keep it alive: the caller holds both while it uses the id.
pub async fn compile_named_callable(
    gx: &GXHandle<NoExt>,
    env: &Env,
    name: &str,
) -> Result<(Ref<NoExt>, Callable<NoExt>)> {
    let bid = find_bind_id(env, name)?;
    let r = gx.compile_ref(bid).await.with_context(|| format!("compile_ref {name}"))?;
    let val = r.last.clone().with_context(|| format!("no value for {name}"))?;
    let cb = gx
        .compile_callable(val)
        .await
        .with_context(|| format!("compile_callable {name}"))?;
    Ok((r, cb))
}

/// The state a test's setup closure is handed before the runtime starts.
pub type TestState = graphix_compiler::ExecState<GXRt<NoExt>>;

/// What every widget harness holds beside its widget tree: the runtime, the
/// program `/test.gx` compiled as `test::result`, its update channel, the
/// variables the test watches and the callables it made, kept alive.
pub struct Fixture {
    _ctx: TestCtx,
    pub gx: GXHandle<NoExt>,
    pub compiled: CompRes<NoExt>,
    pub rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    pub watches: Watches,
    refs: Vec<Ref<NoExt>>,
    callables: Vec<Callable<NoExt>>,
}

impl Fixture {
    /// Compile `code` as `/test.gx` under `register`, its root restored,
    /// `setup` run on the state first; the fixture and `test::result`'s
    /// first value.
    pub async fn new(
        code: &str,
        register: &[PackageRef],
        setup: impl FnOnce(&mut TestState),
    ) -> Result<(Self, Value)> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = ahash::AHashMap::from_iter([(
            Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let resolvers = vec![VfsResolver::new(vfs)];
        let ctx = start_rooted(
            tx,
            register,
            resolvers,
            setup,
            BitFlags::empty(),
            Root::Restored,
        )
        .await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let initial = next_update_raw(&ctx.rt, &mut rx, compiled.exprs[0].id).await?;
        let f = Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            watches: Watches::default(),
            refs: Vec::new(),
            callables: Vec::new(),
        };
        Ok((f, initial))
    }

    /// The id of `test::result`'s updates.
    pub fn result_id(&self) -> ExprId {
        self.compiled.exprs[0].id
    }

    /// Watch the module-qualified variable `name`; its value as it stands.
    pub async fn watch(&mut self, name: &str) -> Result<Value> {
        self.watches.watch(&self.gx, &self.compiled.env, name).await
    }

    /// The function `name` holds as a callable, kept alive with the
    /// fixture: dropping it would invalidate the id.
    pub async fn compile_named_callable(
        &mut self,
        name: &str,
    ) -> Result<graphix_rt::CallableId> {
        let (r, cb) = compile_named_callable(&self.gx, &self.compiled.env, name)
            .await
            .with_context(|| format!("compile_named_callable {name}"))?;
        let id = cb.id();
        self.refs.push(r);
        self.callables.push(cb);
        Ok(id)
    }
}

/// Variables a test harness watches by name: their refs, kept alive, and
/// their latest values.
#[derive(Default)]
pub struct Watches {
    refs: Vec<graphix_rt::Ref<NoExt>>,
    values: nohash::IntMap<ExprId, Value>,
    names: ahash::AHashMap<String, ExprId>,
}

impl Watches {
    /// Watch the module-qualified variable `name`; its value as it stands.
    pub async fn watch(
        &mut self,
        gx: &graphix_rt::GXHandle<NoExt>,
        env: &Env,
        name: &str,
    ) -> Result<Value> {
        let bid = find_bind_id(env, name).with_context(|| format!("watch: {name}"))?;
        let r = gx.compile_ref(bid).await.with_context(|| format!("watch: {name}"))?;
        let initial = r.last.clone().unwrap_or(Value::Null);
        self.values.insert(r.id, initial.clone());
        self.names.insert(name.to_string(), r.id);
        self.refs.push(r);
        Ok(initial)
    }

    /// Record `v` if `id` is a watched variable's.
    pub fn note(&mut self, id: ExprId, v: &Value) {
        if let Some(slot) = self.values.get_mut(&id) {
            *slot = v.clone();
        }
    }

    /// The latest value of the watched variable `name`.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.names.get(name).and_then(|id| self.values.get(id))
    }
}

/// Deliver every update from `rx` to `deliver` until the runtime is idle
/// with nothing left to deliver; answers whether `deliver` reported a
/// change and when the last update came.
pub async fn drain_idle(
    gx: &graphix_rt::GXHandle<NoExt>,
    rx: &mut Events,
    mut deliver: impl AsyncFnMut(ExprId, Value) -> Result<bool>,
) -> Result<(bool, Option<Instant>)> {
    drain_events(gx, rx, async |e| match e {
        GXEvent::Updated(id, v) => deliver(id, v).await,
        GXEvent::Env(_) | GXEvent::Idle => Ok(false),
    })
    .await
}

/// [`drain_idle`] handing `on_event` every event in order, a
/// [`GXEvent::Idle`] included.
pub async fn drain_events(
    gx: &graphix_rt::GXHandle<NoExt>,
    rx: &mut Events,
    mut on_event: impl AsyncFnMut(GXEvent) -> Result<bool>,
) -> Result<(bool, Option<Instant>)> {
    let (mut changed, mut last) = (false, None);
    loop {
        let idle = gx.wait_idle();
        tokio::pin!(idle);
        let mut delivered = false;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = rx.recv() => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(..) = e {
                            delivered = true;
                            last = Some(Instant::now());
                        }
                        changed |= on_event(e).await?;
                    }
                }
                r = &mut idle => break r?,
            }
        }
        if !delivered {
            return Ok((changed, last));
        }
    }
}

/// Evaluate a graphix expression and return its first value, as
/// [`data`], with the test context (caller must shut it down).
pub async fn eval(code: &str, register: &[PackageRef]) -> Result<(Value, TestCtx)> {
    eval_with_setup(code, register, |_| {}).await
}

/// [`eval`], the value as the runtime delivered it: for a Rust reader of
/// the runtime form, kind markers included.
pub async fn eval_raw(code: &str, register: &[PackageRef]) -> Result<(Value, TestCtx)> {
    first_value([("/test.gx", result_source(code))], register, |_| {}).await
}

pub async fn eval_with_setup<F>(
    code: &str,
    register: &[PackageRef],
    setup: F,
) -> Result<(Value, TestCtx)>
where
    F: FnOnce(&mut TestState),
{
    let (v, ctx) =
        first_value([("/test.gx", result_source(code))], register, setup).await?;
    Ok((data(v), ctx))
}

async fn first_value<'a, F>(
    files: impl IntoIterator<Item = (&'a str, VfsEntry)>,
    register: &[PackageRef],
    setup: F,
) -> Result<(Value, TestCtx)>
where
    F: FnOnce(&mut TestState),
{
    let (ctx, mut rx) = fixture_runtime(files, register, Mode::Jit, setup).await?;
    let res = compile_result(&ctx).await?;
    let v = next_update_raw(&ctx.rt, &mut rx, res.exprs[0].id).await?;
    Ok((v, ctx))
}

/// [`eval`] in every [`Mode`], the check held to the build's verdict in
/// [`Mode::Interp`] as [`run!`] holds it: the first value, or the
/// refusal's message. The modes must agree on which, and on the value.
pub async fn eval_modes(
    code: &str,
    register: &[PackageRef],
) -> Result<std::result::Result<Value, String>> {
    let mut first: Option<std::result::Result<Value, String>> = None;
    for mode in Mode::ALL {
        let (ctx, mut rx) =
            fixture_runtime([("/test.gx", result_source(code))], register, mode, |_| {})
                .await?;
        let checked = match mode {
            Mode::Interp => Some(check_fixture(&ctx).await),
            _ => None,
        };
        let built = compile_result(&ctx).await;
        if let Some(checked) = checked {
            verdicts_agree(checked, built.as_ref().map(|_| ()))
                .with_context(|| format!("{code}"))?;
        }
        let got = match built {
            Ok(res) => Ok(next_update(&ctx.rt, &mut rx, res.exprs[0].id).await?),
            Err(e) => Err(format!("{e:#}")),
        };
        ctx.shutdown().await;
        match &first {
            None => first = Some(got),
            // printed: a function value equals only itself, in its runtime
            Some(Ok(v)) if got.as_ref().is_ok_and(|g| g.to_string() == v.to_string()) => {
            }
            Some(Err(_)) if got.is_err() => (),
            Some(prior) => {
                bail!("{mode:?} gives {got:?} where Interp gives {prior:?}: {code}")
            }
        }
    }
    Ok(first.expect("Mode::ALL is not empty"))
}

/// How long a fixture may run before it counts as a hang: a guard against
/// a test that never ends, never what one asserts.
pub const HANG_GUARD: Duration = Duration::from_secs(600);

/// How long [`collect`] reads a program's result.
#[derive(Debug, Clone, Copy)]
pub enum Collect {
    /// Its first `n` updates.
    First(usize),
    /// Every update until the runtime settles, or until none arrives for
    /// the window after the first (a program with a standing source). The
    /// window counts only while the runtime is idle: it starts at a
    /// [`GXHandle::wait_idle`] answer, so a cycle that runs long is never a
    /// quiet one.
    Quiet(Duration),
    /// Every update until the runtime settles, however far apart they
    /// come: a program that ends by itself.
    Settled,
}

/// `code` (as `let result = {code}`) run in `mode`: its result's updates
/// as `until` says ([`updates_until`]).
pub async fn collect(
    code: &str,
    register: &[PackageRef],
    mode: Mode,
    until: Collect,
) -> Result<Vec<Value>> {
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", result_source(code))], register, mode, |_| {})
            .await?;
    let res = compile_result(&ctx).await?;
    let values = updates_until(&ctx.rt, &mut rx, res.exprs[0].id, until).await?;
    ctx.shutdown().await;
    Ok(values)
}

/// Like [`eval`], but ships the `/test.gx` module as a packed pre-parsed
/// AST (`serialize::pack_module`) rather than source, so the resolver
/// takes its `unpack_module` path. The result must match [`eval`].
pub async fn eval_packed(
    code: &str,
    register: &[PackageRef],
) -> Result<(Value, TestCtx)> {
    let source = result_text(code);
    let ori = Origin::new(None, Source::Vfs(arcstr::literal!("/test")), source.clone());
    let exprs = graphix_compiler::expr::parser::parse(ori)?;
    let packed = graphix_compiler::expr::serialize::pack_module(&exprs)?;
    let entry = VfsEntry { source, packed: Some(packed) };
    let (v, ctx) = first_value([("/test.gx", entry)], register, |_| {}).await?;
    Ok((data(v), ctx))
}

use graphix_compiler::expr::parser::GRAPHIX_STRINGS;
pub use poolshark::local::LPooled;

pub fn escape_path(path: std::path::Display) -> LPooled<String> {
    let mut buf: LPooled<String> = LPooled::take();
    let mut res: LPooled<String> = LPooled::take();
    write!(buf, "{path}").unwrap();
    GRAPHIX_STRINGS.escape().escape_to(&*buf, &mut res);
    res
}

/// Run a graphix fixture in each [`Mode`] and assert the supplied
/// predicate holds for the value it produces in each. Nothing compares
/// the modes' values with each other: a predicate that pins the value
/// exactly is what makes them agree, and a refusal names its message
/// ([`refused`]).
///
/// - **interp**: `CFlag::FusionDisabled`; the program runs purely
///   through the node-walk, and fails (debug builds) if any fused code
///   ran, a run-time bind's included. It first checks the program alone (the
///   `--check` path) and fails unless the check refuses exactly when the
///   build does; the `; build_only` form skips that for a refusal only a
///   build makes (`#[native]`, the definition assertions).
/// - **jit**: the full fusion + JIT path. Asserts the `FuseExpect`
///   annotation (and the optional `; shape:` NodeShape) against the
///   live post-fusion graph; the annotation only in debug builds, where
///   the counters exist.
/// - **par**, **jit_par**: the same two with every fork point forked
///   (`ParMode::Force`, `design/parallel_eval.md`).
///
/// Expands to `mod $name { … }` holding one
/// `#[tokio::test(flavor = "current_thread")]` per mode; the
/// `; jit_only` form holds only the fused two (and so no check).
#[macro_export]
macro_rules! run {
    // The `; shape:` and `; jit_only` arms must precede the plain
    // `; $fexpect` arms so the longer token sequence matches first.
    // A refusal only a build makes (`#[native]`, the definition
    // assertions): no check test.
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; build_only) => {
        $crate::run!(@impl false, cfg(all()), $name, $pred, $fexpect, ::std::option::Option::None, "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+ ; $fexpect:expr; build_only) => {
        $crate::run!(@impl false, cfg(all()), $name, $pred, $fexpect, ::std::option::Option::None, $($path => $code),+);
    };
    // The JIT alone: a program the node-walk cannot run (a native loop
    // deeper than the stack budget allows activations).
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; jit_only) => {
        $crate::run!(@impl true, cfg(any()), $name, $pred, $fexpect, ::std::option::Option::None, "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; shape: $shape:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $fexpect, ::std::option::Option::Some($shape), "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $code:expr, $pred:expr; shape: $shape:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $crate::testing::FuseExpect::Jit, ::std::option::Option::Some($shape), "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, $($path => $code),+);
    };
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $fexpect, ::std::option::Option::None, "/test.gx" => $crate::testing::result_text(&$code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+ ; $fexpect:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $fexpect, ::std::option::Option::None, $($path => $code),+);
    };
    (@impl $check:literal, $interp:meta, $name:ident, $pred:expr, $fexpect:expr, $shape:expr, $($path:literal => $code:expr),+) => {
        mod $name {
            use super::*;

            fn fixture() -> $crate::testing::RunFixture {
                $crate::testing::RunFixture {
                    name: module_path!(),
                    files: ::std::vec![$(($path, ::arcstr::ArcStr::from($code))),+],
                    pred: $crate::testing::predicate($pred),
                    expect: $fexpect,
                    shape: $shape,
                    register: &crate::TEST_REGISTER,
                }
            }

            #[$interp]
            #[test]
            fn interp() -> ::anyhow::Result<()> {
                $crate::testing::run_fixture(fixture(), $crate::testing::Mode::Interp, $check)
            }

            #[$interp]
            #[test]
            fn par() -> ::anyhow::Result<()> {
                $crate::testing::run_fixture(fixture(), $crate::testing::Mode::Par, false)
            }

            #[test]
            fn jit() -> ::anyhow::Result<()> {
                $crate::testing::run_fixture(fixture(), $crate::testing::Mode::Jit, false)
            }

            #[test]
            fn jit_par() -> ::anyhow::Result<()> {
                $crate::testing::run_fixture(fixture(), $crate::testing::Mode::JitPar, false)
            }
        }
    };
}

#[macro_export]
macro_rules! run_with_tempdir {
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        expect_error
    ) => {
        $crate::run_with_tempdir! {
            name: $test_name,
            code: $code,
            setup: |$temp_dir| $setup,
            expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
                if matches!(v, ::netidx::subscriber::Value::Error(_)) {
                    Ok(())
                } else {
                    panic!("expected Error value, got: {v:?}")
                }
            }
        }
    };
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        verify: |$verify_dir:ident| $verify:block
    ) => {
        $crate::run_with_tempdir! {
            name: $test_name,
            code: $code,
            setup: |$temp_dir| $setup,
            expect: |v: ::netidx::subscriber::Value| -> ::anyhow::Result<()> {
                if !matches!(v, ::netidx::subscriber::Value::Null) {
                    panic!("expected Null (success), got: {v:?}");
                }
                Ok(())
            },
            verify: |$verify_dir| $verify
        }
    };
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        expect: $expect_payload:expr
        $(, verify: |$verify_dir:ident| $verify:block)?
    ) => {
        /// One test per `testing::Mode`, each over a fresh directory; the
        /// node-walk also holds the check to the build.
        mod $test_name {
            #[allow(unused_imports)]
            use super::*;

            async fn run_mode(mode: $crate::testing::Mode) -> ::anyhow::Result<()> {
                let $temp_dir = ::tempfile::tempdir()?;
                let test_file = { $setup };
                let code = format!(
                    $code,
                    $crate::testing::escape_path(test_file.display())
                );
                $crate::testing::run_tempdir_fixture(
                    &code,
                    &crate::TEST_REGISTER,
                    mode,
                    &$expect_payload,
                )
                .await?;
                $(
                    let $verify_dir = &$temp_dir;
                    $verify
                )?
                Ok(())
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn interp() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::Interp).await
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn jit() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::Jit).await
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn par() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::Par).await
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn jit_par() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::JitPar).await
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn both_refusing_answers_the_checks_refusal() {
        let built = anyhow::anyhow!("the build's reason");
        let checked =
            verdicts_agree(Err(anyhow::anyhow!("the check's reason")), Err(&built));
        let refusal = checked.unwrap().expect("both refuse");
        assert!(refused("the check's reason")(Err(refusal)));
        assert!(verdicts_agree(Ok(()), Ok(())).unwrap().is_none());
    }

    #[test]
    fn fixtures_prune_for_real_and_audited_in_each_engine() {
        if std::env::var_os("GRAPHIX_PRUNE").is_some() {
            return;
        }
        for prune in [PruneMode::On, PruneMode::Audit] {
            for node_walk in [true, false] {
                assert!(
                    Mode::ALL
                        .iter()
                        .any(|m| m.node_walk() == node_walk && m.fixture_prune() == prune),
                    "no {} mode runs fixtures under {prune:?}",
                    if node_walk { "node-walk" } else { "jit" }
                )
            }
        }
    }
}
