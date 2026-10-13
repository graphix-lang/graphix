use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use graphix_compiler::{
    BindId, CFlag, ParMode,
    env::Env,
    expr::{ExprId, Origin, ResolverRef, Source, VfsEntry, VfsResolver},
};
use graphix_rt::{
    Callable, CompRes, GXConfig, GXEvent, GXHandle, GXRt, NoExt, ProgramImage, Ref,
    RegistrationImage,
};
use netidx::publisher::Value;
use netidx_core::path::Path;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::{
    sync::{mpsc, oneshot},
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
    Jit,
    /// The program produces no fused kernel at all.
    None,
}

/// Assert the observed fusion counters match `expect`. Called after a
/// fixture runs, in `jit` mode only; the runtime's counters (its
/// `Control`, debug builds only) are reset after init so they reflect
/// only the fixture's own program. `Jit` holds when any region ran fused:
/// a fixture whose point is that one expression fuses says `#[native]`
/// on it.
#[cfg(debug_assertions)]
pub fn check_fuse_expectation((fusion, jit): (u64, u64), expect: FuseExpect) {
    match expect {
        FuseExpect::Jit => {
            assert!(
                fusion > 0,
                "fuse: Jit — expected a fused kernel to run but \
                 FUSION_INVOCATIONS=0 (no kernel built/ran — the JIT \
                 couldn't compile it, so it node-walked). Downgrade to \
                 `fuse: None`, or fix the cliff.",
            );
            assert!(
                jit > 0,
                "fuse: Jit — FUSION>0 but JIT_INVOCATIONS=0: a fused \
                 region ran without entering its JIT wrapper; investigate.",
            );
        }
        FuseExpect::None => {
            assert!(
                fusion == 0,
                "fuse: None — expected NO fusion but FUSION_INVOCATIONS>0; \
                 this program now fuses. Upgrade the annotation to \
                 `fuse: Jit`.",
            );
        }
    }
}

impl TestCtx {
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

pub async fn init_with_resolvers(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
) -> Result<TestCtx> {
    init_with_setup(sub, register, resolvers, |_| {}).await
}

pub async fn init(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
) -> Result<TestCtx> {
    init_with_setup(sub, register, vec![], |_| {}).await
}

pub async fn init_with_setup<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_with_flags_and_setup(sub, register, resolvers, BitFlags::empty(), setup).await
}

/// Like [`init_with_setup`] but lets the caller pin the compile-time
/// flags (`CFlag::FusionDisabled`, etc.) the runtime passes to every
/// `compile()` it dispatches.
pub async fn init_with_flags_and_setup<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    flags: BitFlags<CFlag>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(sub, register, resolvers, flags, false, None, None, None, None, setup)
        .await
}

/// A runtime that restores its registration from an image, or sends
/// the image of the registration it compiled; see [`RegistrationImage`].
pub async fn init_with_registration(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    registration: RegistrationImage,
) -> Result<TestCtx> {
    init_with_session(sub, register, BitFlags::empty(), registration, None, None).await
}

/// A runtime built around a registration image, with a program
/// compiled at construction (or restored with the image) whose own
/// image `program_image` receives; see `GXConfig::program`.
pub async fn init_with_session(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    flags: BitFlags<CFlag>,
    registration: RegistrationImage,
    program: Option<Source>,
    program_image: Option<oneshot::Sender<Result<ProgramImage>>>,
) -> Result<TestCtx> {
    init_inner(
        sub,
        register,
        vec![],
        flags,
        false,
        Some(registration),
        program,
        program_image,
        None,
        |_| {},
    )
    .await
}

/// [`init_with_session`] with module resolvers, a trace armed before
/// the program's init cycle (`GXConfig::trace`) and a context setup.
pub async fn init_session_with_setup<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    flags: BitFlags<CFlag>,
    registration: RegistrationImage,
    program: Option<Source>,
    program_image: Option<oneshot::Sender<Result<ProgramImage>>>,
    trace: Option<(usize, u64)>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(
        sub,
        register,
        resolvers,
        flags,
        false,
        Some(registration),
        program,
        program_image,
        trace,
        setup,
    )
    .await
}

/// [`init_with_registration`] for an **lsp_mode** runtime.
pub async fn init_lsp_with_registration(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    registration: RegistrationImage,
) -> Result<TestCtx> {
    let setup = |_: &mut _| {};
    init_inner(
        sub,
        register,
        vec![],
        BitFlags::empty(),
        true,
        Some(registration),
        None,
        None,
        None,
        setup,
    )
    .await
}

/// Like [`init_with_flags_and_setup`] but builds an **lsp_mode** runtime —
/// the `check` path, which compiles to verify types and then deletes the
/// nodes without ever executing them.
pub async fn init_lsp_mode<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    flags: BitFlags<CFlag>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(sub, register, resolvers, flags, true, None, None, None, None, setup).await
}

// CR claude for claude: [structure/low] Nine public init_* wrappers exist only to
// thread arguments into this function, which takes ten positional parameters (four
// Options and a bool) and then just feeds a GXConfig builder. Each new knob adds
// another wrapper or another parameter. The setup closure's bound `FnOnce(&mut
// ExecState<GXRt<NoExt>, ..UserEvent>)` is spelled out eight times in this file. The
// run! arms each format `let result = {}` although `result_source` exists, and
// eval_packed formats it again. Return the configured builder from one function
// (register packages, run setup, set root and resolvers) and let callers chain
// flags/registration/program/trace/lsp_mode. Add a `TestState` alias for the closure's
// argument, and use result_source in run! and eval_packed. The ../netidx admin tests
// call init, init_with_session and init_with_flags_and_setup. (x-dup-09)
async fn init_inner<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    flags: BitFlags<CFlag>,
    lsp_mode: bool,
    registration: Option<RegistrationImage>,
    program: Option<Source>,
    program_image: Option<oneshot::Sender<Result<ProgramImage>>>,
    trace: Option<(usize, u64)>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
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
    let mut cfg = GXConfig::builder(ctx, sub)
        .root(root)
        .resolvers(all_resolvers)
        .flags(flags)
        .lsp_mode(lsp_mode);
    if let Some(r) = registration {
        cfg = cfg.registration(r);
    }
    if let Some(p) = program {
        cfg = cfg.program(p);
    }
    if let Some(tx) = program_image {
        cfg = cfg.program_image(tx);
    }
    if let Some((max_events, max_cycles)) = trace {
        cfg = cfg.trace((max_events, max_cycles));
    }
    let st = std::time::Instant::now();
    let rt = cfg.build()?.start().await?;
    log::info!("runtime start time: {:?}", st.elapsed());
    Ok(TestCtx { rt })
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
}

impl Mode {
    pub const ALL: [Mode; 4] = [Mode::Interp, Mode::Jit, Mode::Par, Mode::JitPar];

    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp | Mode::Par => CFlag::FusionDisabled.into(),
            Mode::Jit | Mode::JitPar => BitFlags::empty(),
        }
    }

    pub fn node_walk(self) -> bool {
        matches!(self, Mode::Interp | Mode::Par)
    }

    /// The serial modes fork nothing unless `GRAPHIX_PAR` says otherwise.
    pub fn par(self) -> ParMode {
        match self {
            Mode::Interp | Mode::Jit => match std::env::var_os("GRAPHIX_PAR") {
                Some(_) => ParMode::from_env(),
                None => ParMode::Off,
            },
            Mode::Par | Mode::JitPar => ParMode::Force,
        }
    }
}

/// `let result = {code}`, a fixture's `/test.gx`.
pub fn result_source(code: &str) -> VfsEntry {
    VfsEntry::from(ArcStr::from(format_compact!("let result = {code}").as_str()))
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
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    let (tx, rx) = mpsc::channel(1024);
    let tbl = ahash::AHashMap::from_iter(
        files.into_iter().map(|(path, entry)| (Path::from(ArcStr::from(path)), entry)),
    );
    let ctx = init_with_flags_and_setup(
        tx,
        register,
        vec![VfsResolver::new(tbl)],
        mode.flags(),
        |ctx| {
            ctx.control.set_par_mode(mode.par());
            setup(ctx)
        },
    )
    .await?;
    Ok((ctx, rx))
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
    let program = Source::Program(arcstr::literal!("{ mod test; test::result }"));
    ctx.rt.check(program, None).await.map(|_| ())
}

/// The check refuses exactly when the build does.
pub fn verdicts_agree(
    checked: Result<()>,
    built: Result<(), &anyhow::Error>,
) -> Result<()> {
    match (checked, built) {
        (Ok(()), Ok(())) | (Err(_), Err(_)) => Ok(()),
        (Ok(()), Err(e)) => bail!("the check accepts what the build refuses: {e:#}"),
        (Err(e), Ok(())) => bail!("the check refuses what the build accepts: {e:#}"),
    }
}

/// The next update of `id`, failing at `deadline`.
pub async fn next_update(
    rx: &mut Events,
    id: ExprId,
    deadline: Instant,
) -> Result<Value> {
    loop {
        let mut batch = tokio::time::timeout_at(deadline, rx.recv())
            .await
            .map_err(|_| anyhow::anyhow!("timeout waiting for an update of {id:?}"))?
            .context("the runtime died")?;
        for e in batch.drain(..) {
            if let GXEvent::Updated(eid, v) = e
                && eid == id
            {
                return Ok(v);
            }
        }
    }
}

/// Every update of `id` until no batch arrives for `quiet`, failing at
/// `deadline` (a program that never quiesces).
pub async fn updates_until_quiet(
    rx: &mut Events,
    id: ExprId,
    quiet: Duration,
    deadline: Instant,
) -> Result<Vec<Value>> {
    let mut values = Vec::new();
    loop {
        if Instant::now() >= deadline {
            bail!("the program did not quiesce: {values:?}");
        }
        match tokio::time::timeout(quiet, rx.recv()).await {
            Err(_) => return Ok(values),
            Ok(None) => bail!("the runtime died"),
            Ok(Some(mut batch)) => {
                for e in batch.drain(..) {
                    if let GXEvent::Updated(eid, v) = e
                        && eid == id
                    {
                        values.push(v);
                    }
                }
            }
        }
    }
}

/// A [`run!`] predicate for a program that must be refused: its compile
/// error contains `phrase`. A parse error or a dead runtime does not
/// match the rule's message.
pub fn refused(phrase: &'static str) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains(phrase))
}

/// The compile error `let result = {code}` gets, in full; an error if it
/// compiles, or if the runtime died compiling it.
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

/// The `BindId` of a module-qualified name like `"test::clicks"`. Scope
/// keys are generated paths (`/do…/test`), so the module is matched as a
/// suffix.
pub fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    let Some((module, var)) = name.split_once("::") else {
        bail!("expected module::var, got {name}")
    };
    let suffix = format_compact!("/{module}");
    for (scope, vars) in &env.binds {
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
                        if let GXEvent::Updated(id, v) = e {
                            delivered = true;
                            last = Some(Instant::now());
                            changed |= deliver(id, v).await?;
                        }
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

/// Evaluate a graphix expression and return its first value with the
/// test context (caller must shut it down).
pub async fn eval(code: &str, register: &[PackageRef]) -> Result<(Value, TestCtx)> {
    eval_with_setup(code, register, |_| {}).await
}

pub async fn eval_with_setup<F>(
    code: &str,
    register: &[PackageRef],
    setup: F,
) -> Result<(Value, TestCtx)>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    first_value([("/test.gx", result_source(code))], register, setup).await
}

async fn first_value<'a, F>(
    files: impl IntoIterator<Item = (&'a str, VfsEntry)>,
    register: &[PackageRef],
    setup: F,
) -> Result<(Value, TestCtx)>
where
    F: FnOnce(
        &mut graphix_compiler::ExecState<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    let (ctx, mut rx) = fixture_runtime(files, register, Mode::Jit, setup).await?;
    let res = compile_result(&ctx).await?;
    let v =
        next_update(&mut rx, res.exprs[0].id, Instant::now() + Duration::from_secs(5))
            .await?;
    Ok((v, ctx))
}

/// Like [`eval`], but for a program that converges over several cycles:
/// the LAST value of the result once the runtime has gone quiet.
pub async fn eval_converged(
    code: &str,
    register: &[PackageRef],
) -> Result<(Value, TestCtx)> {
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", result_source(code))], register, Mode::Jit, |_| {})
            .await?;
    let res = compile_result(&ctx).await?;
    let deadline = Instant::now() + Duration::from_secs(5);
    let mut values = updates_until_quiet(
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(500),
        deadline,
    )
    .await?;
    match values.pop() {
        Some(v) => Ok((v, ctx)),
        None => bail!("no update before the runtime went quiet"),
    }
}

/// Like [`eval`], but ships the `/test.gx` module as a packed pre-parsed
/// AST (`serialize::pack_module`) rather than source, so the resolver
/// takes its `unpack_module` path. The result must match [`eval`].
pub async fn eval_packed(
    code: &str,
    register: &[PackageRef],
) -> Result<(Value, TestCtx)> {
    let source = ArcStr::from(format_compact!("let result = {code}").as_str());
    let ori = Origin {
        parent: None,
        source: Source::Vfs(arcstr::literal!("/test")),
        text: source.clone(),
    };
    let exprs = graphix_compiler::expr::parser::parse(ori)?;
    let packed = graphix_compiler::expr::serialize::pack_module(&exprs)?;
    let entry = VfsEntry { source, packed: Some(packed) };
    first_value([("/test.gx", entry)], register, |_| {}).await
}

pub use graphix_compiler::expr::parser::GRAPHIX_ESC;
pub use poolshark::local::LPooled;

pub fn escape_path(path: std::path::Display) -> LPooled<String> {
    use std::fmt::Write;
    let mut buf: LPooled<String> = LPooled::take();
    let mut res: LPooled<String> = LPooled::take();
    write!(buf, "{path}").unwrap();
    GRAPHIX_ESC.escape_to(&*buf, &mut res);
    res
}

/// Run a graphix fixture in each [`Mode`] and assert the supplied
/// predicate holds for the value it produces in each. Nothing compares
/// the modes' values with each other: a predicate that pins the value
/// exactly is what makes them agree, and a refusal names its message
/// ([`refused`]).
///
/// - **interp**: `CFlag::FusionDisabled`; the program runs purely
///   through the node-walk. It first checks the program alone (the
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
    // The JIT alone: a program the node-walk cannot run (a native loop
    // deeper than the stack budget allows activations).
    // A refusal only a build makes (`#[native]`, the definition
    // assertions): no check test.
    // CR claude for claude: [readability/low] The comment above this arm, "The JIT
    // alone: a program the node-walk cannot run ...", describes the `; jit_only` arm
    // further down, not this build_only arm, so it reads as the wrong arm's rationale.
    // Move it directly above the jit_only arm. Keep the ordering note at the top of the
    // macro and only the build_only sentence above this arm. (core-aux-11)
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; build_only) => {
        $crate::run!(@impl false, cfg(all()), $name, $pred, 30, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+ ; $fexpect:expr; build_only) => {
        $crate::run!(@impl false, cfg(all()), $name, $pred, 30, $fexpect, ::std::option::Option::None, $($path => $code),+);
    };
    // CR claude for claude: [dead/low] No fixture uses this arm: every `; build_only`
    // fixture passes FuseExpect::None. The arm also could never pass. A build_only
    // fixture is a refusal, a refusal fuses nothing, and so its jit test fails
    // check_fuse_expectation under the FuseExpect::Jit this arm hard-codes. Delete the
    // arm. (tests-lang-rest-b-10)
    ($name:ident, $code:expr, $pred:expr; build_only) => {
        $crate::run!(@impl false, cfg(all()), $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; jit_only) => {
        $crate::run!(@impl true, cfg(any()), $name, $pred, 30, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; shape: $shape:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $fexpect, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr; shape: $shape:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $timeout, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, $($path => $code),+);
    };
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr; $fexpect:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, $timeout, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+ ; $fexpect:expr) => {
        $crate::run!(@impl true, cfg(all()), $name, $pred, 30, $fexpect, ::std::option::Option::None, $($path => $code),+);
    };
    (@impl $check:literal, $interp:meta, $name:ident, $pred:expr, $timeout:expr, $fexpect:expr, $shape:expr, $($path:literal => $code:expr),+) => {
        mod $name {
            use super::*;

            /// The optional `NodeShape` spec to check against the
            /// compiled graph.
            #[allow(dead_code)]
            fn shape_spec(
            ) -> ::std::option::Option<::graphix_compiler::node_shape::NodeShape> {
                $shape
            }

            async fn run_mode(
                mode: $crate::testing::Mode,
                reset_counters_after_init: bool,
                check_shape: bool,
                check: bool,
            ) -> ::anyhow::Result<(u64, u64)> {
                let pred = $pred;
                let (ctx, mut rx) = $crate::testing::fixture_runtime(
                    [$(($path, ::graphix_compiler::expr::VfsEntry::from(::arcstr::ArcStr::from($code)))),+],
                    &crate::TEST_REGISTER,
                    mode,
                    |_| {},
                ).await?;
                // Init compiles the stdlib root and may fuse there;
                // only the fixture's own compile should count.
                if reset_counters_after_init {
                    ctx.rt.control().reset_invocations();
                }
                let checked = if check {
                    ::std::option::Option::Some($crate::testing::check_fixture(&ctx).await)
                } else {
                    ::std::option::Option::None
                };
                let built = $crate::testing::compile_result(&ctx).await;
                if let ::std::option::Option::Some(checked) = checked {
                    $crate::testing::verdicts_agree(checked, built.as_ref().map(|_| ()))?;
                }
                match built {
                    Err(e) => {
                        if let ::std::result::Result::Err(dead) = ctx.rt.get_env().await {
                            ::anyhow::bail!("the runtime died compiling the fixture ({dead:#}): {e:#}")
                        }
                        assert!(pred(dbg!(Err(e))))
                    }
                    Ok(res) => {
                        let eid = res.exprs[0].id;
                        if check_shape {
                            if let ::std::option::Option::Some(spec) = shape_spec() {
                                ctx.rt.match_shape(eid, spec).await?;
                            }
                        }
                        let deadline = ::tokio::time::Instant::now()
                            + ::std::time::Duration::from_secs($timeout);
                        let v = $crate::testing::next_update(&mut rx, eid, deadline).await?;
                        eprintln!("{v}");
                        assert!(pred(Ok(&v)));
                    }
                }
                let invocations = ctx.rt.control().invocations();
                // The blocker list includes stdlib-root noise (stats
                // are per-ExecCtx).
                if ::std::env::var("GRAPHIX_FUSE_AUDIT").is_ok()
                    && !mode.flags().contains(::graphix_compiler::CFlag::FusionDisabled)
                {
                    if let ::std::result::Result::Ok(stats) = ctx.fusion_stats().await {
                        for failure in stats.failed.iter() {
                            eprintln!(
                                "FUSEAUDIT-BLOCKER\t{}\t{:?}\t{}",
                                module_path!(),
                                failure.id,
                                failure.reason
                            );
                        }
                    }
                }
                ctx.shutdown().await;
                Ok(invocations)
            }

            #[$interp]
            #[::tokio::test(flavor = "current_thread")]
            async fn interp() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::Interp, false, false, $check).await.map(|_| ())
            }

            #[$interp]
            #[::tokio::test(flavor = "current_thread")]
            // CR claude for claude: [test-gap/low] The par and jit_par tests run under
            // ParMode::Force but assert only the predicate; nothing reads
            // Control::forks(). If a construct stopped forking (a planner or analysis
            // change that makes it opaque), these tests would quietly become copies of
            // interp and jit and still pass. Only a few hand pins in par_attrs and
            // image check fork counts. Have run_mode return the fork count in the Par
            // and JitPar modes, and either report it under an env-gated audit like
            // GRAPHIX_FUSE_AUDIT (a fixture with fork points that forked nothing is a
            // finding) or add an optional `; forks` expectation that fails when Force
            // forked nothing. (core-aux-10)
            async fn par() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::Par, false, false, false).await.map(|_| ())
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn jit() -> ::anyhow::Result<()> {
                // GRAPHIX_FUSION_DISCOVERY: run without asserting and
                // print the observed level (`FUSEMAP <path> <level>`).
                #[cfg(debug_assertions)]
                if ::std::env::var("GRAPHIX_FUSION_DISCOVERY").is_ok() {
                    let (fusion, jit) =
                        run_mode($crate::testing::Mode::Jit, true, false, false).await?;
                    eprintln!(
                        "FUSEMAPF\t{}\t{}",
                        module_path!(),
                        if fusion > 0 { "Fuses" } else { "None" },
                    );
                    eprintln!(
                        "FUSEMAPJ\t{}\t{}",
                        module_path!(),
                        if jit > 0 { "Jit" } else { "NoJit" },
                    );
                    return Ok(());
                }
                // GRAPHIX_FUSE_AUDIT: report the observed fusion level
                // against the annotation (`FUSEAUDIT` lines) instead
                // of asserting it.
                #[cfg(debug_assertions)]
                if ::std::env::var("GRAPHIX_FUSE_AUDIT").is_ok() {
                    let (fusion, _) =
                        run_mode($crate::testing::Mode::Jit, true, false, false).await?;
                    let expected = $fexpect;
                    let observed = if fusion > 0 {
                        $crate::testing::FuseExpect::Jit
                    } else {
                        $crate::testing::FuseExpect::None
                    };
                    eprintln!(
                        "FUSEAUDIT\t{}\t{:?}\t{:?}\t{}",
                        module_path!(),
                        expected,
                        observed,
                        if expected == observed { "OK" } else { "MISMATCH" },
                    );
                    return Ok(());
                }
                let _invocations = run_mode($crate::testing::Mode::Jit, true, true, false).await?;
                #[cfg(debug_assertions)]
                $crate::testing::check_fuse_expectation(_invocations, $fexpect);
                Ok(())
            }

            /// Fused, with every fork point forked, kernel loops' slots
            /// included.
            #[::tokio::test(flavor = "current_thread")]
            async fn jit_par() -> ::anyhow::Result<()> {
                run_mode($crate::testing::Mode::JitPar, false, false, false).await.map(|_| ())
            }
        }
    };
}

// CR claude for claude: [test-gap/medium] run_with_tempdir! expands to one
// #[tokio::test] over testing::init: fusion on, the default par mode, no check_fixture,
// and compiled through rt.compile instead of the script path. Every db.rs fixture uses
// it, and so do fs/{read,write,readdir,metadata,dir_ops} and xls.rs. Their
// transactions, conflicts and subscriptions across an arm's sleep and wake therefore
// never run under the node-walk, with every fork point forced, or with the
// check-versus-build agreement that run! enforces. A node-walk-only or forced-fork
// regression in an async db builtin ships with these suites green. Fix: expand per
// testing::Mode like run! does (fixture_runtime(mode) plus check_fixture in interp,
// with the tempdir setup run per mode), or move these fixtures onto run! with a setup
// hook. (tests-lib-a-04)
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
        #[tokio::test(flavor = "current_thread")]
        async fn $test_name() -> ::anyhow::Result<()> {
            let (tx, mut rx) = ::tokio::sync::mpsc::channel::<
                ::poolshark::global::GPooled<Vec<::graphix_rt::GXEvent>>
            >(10);
            let ctx = $crate::testing::init(tx, &crate::TEST_REGISTER).await?;
            let $temp_dir = ::tempfile::tempdir()?;

            let test_file = { $setup };

            let code = format!(
                $code,
                $crate::testing::escape_path(test_file.display())
            );
            // an error the fixture raises and does not catch is printed, so a
            // failing operation names itself beside the timeout
            let code = format!("{{ catch(e) println(\"uncaught: [e]\"); {code} }}");
            let compiled = ctx.rt.compile(::arcstr::ArcStr::from(code)).await?;
            let eid = compiled.exprs[0].id;

            let timeout = ::tokio::time::sleep(::std::time::Duration::from_secs(2));
            ::tokio::pin!(timeout);

            loop {
                ::tokio::select! {
                    _ = &mut timeout => panic!("timeout waiting for result"),
                    Some(mut batch) = rx.recv() => {
                        for event in batch.drain(..) {
                            if let ::graphix_rt::GXEvent::Updated(id, v) = event {
                                if id == eid {
                                    $expect_payload(v)?;
                                    $(
                                        let $verify_dir = &$temp_dir;
                                        $verify
                                    )?
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }
        }
    };
}
