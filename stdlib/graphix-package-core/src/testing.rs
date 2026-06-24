use anyhow::{bail, Result};
use enumflags2::BitFlags;
use graphix_compiler::{expr::ModuleResolver, CFlag};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

pub struct TestCtx {
    pub internal_only: netidx::InternalOnly,
    pub rt: GXHandle<NoExt>,
}

/// The fusion outcome a [`run!`] fixture asserts for its program.
/// Checked bidirectionally in the `jit` mode: a fixture that fuses
/// when it shouldn't (or fails to fuse / JIT when it should) fails the
/// test, so the suite is a live, drift-detecting map of the fusion
/// frontier.
///
/// Fusion is JIT-only. A kernel that
/// can't JIT-compile is never spliced; its original nodes node-walk
/// instead. So there is no "fuses but runs on the interpreter" state:
/// a program either fuses + JITs (`Jit`) or doesn't fuse at all
/// (`None`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FuseExpect {
    /// The program builds a fused kernel AND the JIT compiles + runs
    /// it natively. `FUSION > 0 && JIT > 0` in `jit` mode.
    Jit,
    /// The program produces no fused kernel at all — async/IO edges,
    /// error-expecting fixtures, language-feature tests with no sync
    /// subgraph to fuse, or sync subgraphs the JIT can't lower yet
    /// (which fall back to the node-walk). `FUSION == 0`.
    None,
}

/// Assert the observed fusion counters match `expect`. Called after a
/// fixture runs, in the `jit` mode only. Reads the per-thread
/// `FUSION_INVOCATIONS` / `JIT_INVOCATIONS` counters (reset after
/// runtime init, so they reflect only the fixture's own program).
///
/// A fused kernel always JITs (`fuse()`
/// only splices a kernel it could JIT-compile), so `FUSION > 0` and
/// `JIT > 0` move together. A kernel that can't JIT isn't spliced —
/// FUSION stays 0 and the program node-walks.
#[cfg(debug_assertions)]
pub fn check_fuse_expectation(expect: FuseExpect) {
    use graphix_compiler::fusion::emit_helpers::{fusion_invocations, jit_invocations};
    let fusion = fusion_invocations();
    let jit = jit_invocations();
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
                "fuse: Jit — FUSION>0 but JIT_INVOCATIONS=0. With the \
                 interpreter gone this should be impossible; investigate.",
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
        self.internal_only.shutdown().await
    }

    /// Snapshot the compile-time fusion outcome counters. See
    /// [`graphix_compiler::FusionStats`] and [`GXHandle::fusion_stats`].
    pub async fn fusion_stats(&self) -> Result<graphix_compiler::FusionStats> {
        self.rt.fusion_stats().await
    }
}

pub type RegisterFn = fn(
    &mut graphix_compiler::ExecCtx<GXRt<NoExt>, <NoExt as graphix_rt::GXExt>::UserEvent>,
    &mut ahash::AHashMap<netidx_core::path::Path, arcstr::ArcStr>,
    &mut graphix_package::IndexSet<arcstr::ArcStr>,
) -> Result<()>;

pub async fn init_with_resolvers(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    resolvers: Vec<ModuleResolver>,
) -> Result<TestCtx> {
    init_with_setup(sub, register, resolvers, |_| {}).await
}

pub async fn init(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
) -> Result<TestCtx> {
    init_with_setup(sub, register, vec![], |_| {}).await
}

pub async fn init_with_setup<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    resolvers: Vec<ModuleResolver>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_with_flags_and_setup(sub, register, resolvers, BitFlags::empty(), setup).await
}

/// Like [`init_with_setup`] but lets the caller pin the
/// compile-time flags (`CFlag::FusionDisabled`,
/// etc.) the runtime will pass to every `compile()` it dispatches.
/// Used by the [`run!`] macro to drive the same fixture through
/// the interp / jit modes.
pub async fn init_with_flags_and_setup<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    resolvers: Vec<ModuleResolver>,
    flags: BitFlags<CFlag>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(sub, register, resolvers, flags, false, setup).await
}

/// Like [`init_with_flags_and_setup`] but builds an **lsp_mode** runtime —
/// the `check` path, which compiles to verify types and then deletes the
/// nodes without ever executing them. Used to test that fusion runs, and
/// never panics, in the check/LSP path (fusion is a compile-time pass, so
/// it runs during a check even though no kernel is ever executed).
pub async fn init_lsp_mode<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    resolvers: Vec<ModuleResolver>,
    flags: BitFlags<CFlag>,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(sub, register, resolvers, flags, true, setup).await
}

async fn init_inner<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    resolvers: Vec<ModuleResolver>,
    flags: BitFlags<CFlag>,
    lsp_mode: bool,
    setup: F,
) -> Result<TestCtx>
where
    F: FnOnce(
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    let _ = env_logger::try_init();
    let env = netidx::InternalOnly::new().await?;
    let mut ctx = graphix_compiler::ExecCtx::new(GXRt::<NoExt>::new(
        env.publisher().clone(),
        env.subscriber().clone(),
    ))?;
    let mut modules = ahash::AHashMap::default();
    let mut root_mods = graphix_package::IndexSet::new();
    for f in register {
        f(&mut ctx, &mut modules, &mut root_mods)?;
    }
    setup(&mut ctx);
    let mut parts = Vec::new();
    for name in &root_mods {
        if name == "core" {
            parts.push(format!("mod core;\nuse core"));
        } else {
            parts.push(format!("mod {name}"));
        }
    }
    let root = arcstr::ArcStr::from(parts.join(";\n"));
    let mut all_resolvers = vec![ModuleResolver::VFS(modules)];
    all_resolvers.extend(resolvers);
    Ok(TestCtx {
        internal_only: env,
        rt: GXConfig::builder(ctx, sub)
            .root(root)
            .resolvers(all_resolvers)
            .flags(flags)
            .lsp_mode(lsp_mode)
            .build()?
            .start()
            .await?,
    })
}

/// Evaluate a graphix expression and return its Value.
///
/// Compiles `code` as `let result = {code}` in a throwaway module,
/// waits for the first update, and returns the resulting value along
/// with the test context (caller must shut it down).
pub async fn eval(code: &str, register: &[RegisterFn]) -> Result<(Value, TestCtx)> {
    eval_with_setup(code, register, |_| {}).await
}

pub async fn eval_with_setup<F>(
    code: &str,
    register: &[RegisterFn],
    setup: F,
) -> Result<(Value, TestCtx)>
where
    F: FnOnce(
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    let (tx, mut rx) = mpsc::channel(10);
    let gx_code = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        arcstr::ArcStr::from(gx_code),
    )]);
    let resolver = ModuleResolver::VFS(tbl);
    let ctx = init_with_setup(tx, register, vec![resolver], setup).await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting for graphix result"),
            batch = rx.recv() => match batch {
                None => bail!("graphix runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                return Ok((v, ctx));
                            }
                        }
                    }
                }
            }
        }
    }
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

/// Run a graphix fixture under two modes and assert the supplied
/// predicate holds for the produced Value in each:
///
/// - **interp**: `CFlag::FusionDisabled` set. Fusion is a no-op; the
///   program runs purely through the Update-trait node-walk. Ground
///   truth for differential testing.
/// - **jit**: no flags set. The full fusion + JIT path. Resets the
///   fusion + JIT invocation counters at start, runs, asserts the
///   `FuseExpect` annotation (and the optional `; shape:` NodeShape)
///   against the live post-fusion graph. The shape is checked here
///   (was in the removed `fused` mode) — with the interpreter gone, a
///   fused kernel always JITs, so the graph shape is identical whether
///   we observe it in jit mode or not. `cfg(debug_assertions)`-gated
///   since the counters only exist in debug builds.
///
/// There is no third "fuse but don't JIT" mode: fusion is JIT-only,
/// so `FusionDisabled` toggles all of fusion (compile-time AND the
/// runtime per-slot HOF path, via `ExecCtx::fusion_enabled`) on or off.
/// The macro expands to a child module `mod $name {
/// fn interp() … fn jit() … }` — two `#[tokio::test(flavor =
/// "current_thread")]` functions, one result per mode.
#[macro_export]
macro_rules! run {
    // ── NodeShape-bearing forms (trailing `; shape: <NodeShape>`) ──
    // Pin the compiled node-graph shape in addition to the value + fusion
    // expectation. The spec is checked in `fused` mode (graph shape is
    // backend-independent). Must precede the plain `; $fexpect` arms so
    // the longer token sequence matches first.
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; shape: $shape:expr) => {
        $crate::run!(@impl $name, $pred, 30, $fexpect, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr; shape: $shape:expr) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    // Default form: assert the program fuses AND JITs (`FuseExpect::Jit`).
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr) => {
        $crate::run!(@impl $name, $pred, $timeout, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, $($path => $code),+);
    };
    // Explicit fusion-expectation form: trailing `; FuseExpect::X`.
    // The `;` separator makes the expectation insertable at the very
    // end of any invocation regardless of its argument shape, which
    // is what the run_no_jit!→run! migration relied on.
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr) => {
        $crate::run!(@impl $name, $pred, 30, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr; $fexpect:expr) => {
        $crate::run!(@impl $name, $pred, $timeout, $fexpect, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+ ; $fexpect:expr) => {
        $crate::run!(@impl $name, $pred, 30, $fexpect, ::std::option::Option::None, $($path => $code),+);
    };
    (@impl $name:ident, $pred:expr, $timeout:expr, $fexpect:expr, $shape:expr, $($path:literal => $code:expr),+) => {
        mod $name {
            use super::*;

            /// The optional `NodeShape` spec to check against the
            /// compiled graph (`None` for fixtures that don't pin it).
            #[allow(dead_code)]
            fn shape_spec(
            ) -> ::std::option::Option<::graphix_compiler::node_shape::NodeShape> {
                $shape
            }

            async fn run_with_flags(
                flags: ::graphix_compiler::BitFlags<::graphix_compiler::CFlag>,
                reset_counters_after_init: bool,
                fusion_check: bool,
                check_shape: bool,
            ) -> ::anyhow::Result<()> {
                let pred = $pred;
                let (tx, mut rx) = ::tokio::sync::mpsc::channel(10);
                let tbl = ::ahash::AHashMap::from_iter([
                    $((::netidx_core::path::Path::from($path), ::arcstr::ArcStr::from($code))),+
                ]);
                let resolver = ::graphix_compiler::expr::ModuleResolver::VFS(tbl);
                let ctx = $crate::testing::init_with_flags_and_setup(
                    tx, &crate::TEST_REGISTER, vec![resolver], flags,
                    |_ctx| {},
                ).await?;
                // Reset the JIT + fusion invocation counters AFTER
                // runtime init — init compiles the stdlib root module
                // and may fuse/JIT things there. We only want to count
                // activity caused by the fixture's own compile below.
                if reset_counters_after_init {
                    #[cfg(debug_assertions)]
                    {
                        ::graphix_compiler::fusion::emit_helpers::reset_jit_invocations();
                        ::graphix_compiler::fusion::emit_helpers::reset_fusion_invocations();
                        ::graphix_compiler::fusion::emit_helpers::reset_fuse_bails();
                    }
                }
                let bs = &ctx.rt;
                match bs.compile(::arcstr::literal!("{ mod test; test::result }")).await {
                    Err(e) => assert!(pred(dbg!(Err(e)))),
                    Ok(e) => {
                        let eid = e.exprs[0].id;
                        // Assert the compiled graph's shape (only in
                        // modes where fusion ran, against the live
                        // post-fusion graph). A mismatch fails here
                        // with the offending path + reason.
                        if check_shape {
                            if let ::std::option::Option::Some(spec) = shape_spec() {
                                bs.match_shape(eid, spec).await?;
                            }
                        }
                        let timeout = ::tokio::time::sleep(
                            ::std::time::Duration::from_secs($timeout),
                        );
                        ::tokio::pin!(timeout);
                        loop {
                            ::tokio::select! {
                                _ = &mut timeout => ::anyhow::bail!(
                                    "timeout after {}s waiting for result", $timeout,
                                ),
                                batch = rx.recv() => match batch {
                                    None => ::anyhow::bail!("runtime died"),
                                    Some(mut batch) => {
                                        let mut done = false;
                                        for e in batch.drain(..) {
                                            match e {
                                                ::graphix_rt::GXEvent::Env(_) => (),
                                                ::graphix_rt::GXEvent::Updated(id, v) => {
                                                    eprintln!("{v}");
                                                    assert_eq!(id, eid);
                                                    assert!(pred(Ok(&v)));
                                                    done = true;
                                                }
                                            }
                                        }
                                        if done { break; }
                                    }
                                }
                            }
                        }
                    }
                }
                // Bidirectional fusion-expectation check (debug only).
                // `fusion_check` is true only in `jit` mode; the interp
                // (FusionDisabled) mode never fuses, so there's nothing
                // to assert there.
                #[cfg(debug_assertions)]
                if fusion_check {
                    $crate::testing::check_fuse_expectation($fexpect);
                }
                // Audit diagnostics: under GRAPHIX_FUSE_AUDIT, dump
                // the per-region blocker list — the "why didn't it
                // fuse" companion to the FUSEAUDIT verdict line.
                // Includes stdlib-root noise (the stats are
                // per-ExecCtx, no baseline subtraction here) — filter
                // by eye. Gate on fusion actually having run (not the
                // interp mode's FusionDisabled).
                if ::std::env::var("GRAPHIX_FUSE_AUDIT").is_ok()
                    && !flags
                        .contains(::graphix_compiler::CFlag::FusionDisabled)
                {
                    if let ::std::result::Result::Ok(stats) =
                        ctx.fusion_stats().await
                    {
                        for (id, why) in stats.failed.iter() {
                            eprintln!(
                                "FUSEAUDIT-BLOCKER\t{}\t{:?}\t{}",
                                module_path!(),
                                id,
                                why
                            );
                        }
                    }
                }
                ctx.shutdown().await;
                Ok(())
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn interp() -> ::anyhow::Result<()> {
                // Ground truth: fusion fully disabled (node-walk only),
                // no fusion/shape check.
                run_with_flags(
                    ::graphix_compiler::CFlag::FusionDisabled.into(),
                    false,
                    false,
                    false,
                ).await
            }

            #[::tokio::test(flavor = "current_thread")]
            #[cfg(debug_assertions)]
            async fn jit() -> ::anyhow::Result<()> {
                // Discovery mode: when GRAPHIX_FUSION_DISCOVERY is set,
                // run every fixture (checked or not) through the full
                // fusion+JIT path WITHOUT asserting, then print the
                // observed level (`FUSEMAP <path> <level>`). Harvests
                // the whole-corpus fusion-state map in one run. With the
                // interpreter gone, "fuses" and "JITs" coincide — a
                // single full-path run measures both.
                if ::std::env::var("GRAPHIX_FUSION_DISCOVERY").is_ok() {
                    run_with_flags(
                        ::graphix_compiler::BitFlags::empty(),
                        true,
                        false,
                        false,
                    ).await?;
                    let fusion =
                        ::graphix_compiler::fusion::emit_helpers::fusion_invocations();
                    let jit =
                        ::graphix_compiler::fusion::emit_helpers::jit_invocations();
                    eprintln!(
                        "FUSEMAPF\t{}\t{}",
                        module_path!(),
                        if fusion > 0 { "Fuses" } else { "None" },
                    );
                    {
                        let bails =
                            ::graphix_compiler::fusion::emit_helpers::take_fuse_bails();
                        let joined = bails
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(",");
                        eprintln!(
                            "FUSEBAIL\t{}\t{}",
                            module_path!(),
                            joined,
                        );
                    }
                    eprintln!(
                        "FUSEMAPJ\t{}\t{}",
                        module_path!(),
                        if jit > 0 { "Jit" } else { "NoJit" },
                    );
                    return Ok(());
                }
                // Coverage-audit mode: when GRAPHIX_FUSE_AUDIT is
                // set, run the fixture normally but REPORT the
                // observed fusion level against the `FuseExpect`
                // annotation instead of asserting it. Harvest:
                // `GRAPHIX_FUSE_AUDIT=1 cargo test -p graphix-tests
                // -- jit --nocapture 2>&1 | grep FUSEAUDIT` — the
                // MISMATCH lines are the coverage delta to review
                // (and FUSEAUDIT-BLOCKER lines say why a region
                // didn't fuse). Built for the F1→F2 flip audit; kept
                // as the standing no-assert coverage observer.
                if ::std::env::var("GRAPHIX_FUSE_AUDIT").is_ok() {
                    run_with_flags(
                        ::graphix_compiler::BitFlags::empty(),
                        true,
                        false,
                        false,
                    ).await?;
                    let fusion =
                        ::graphix_compiler::fusion::emit_helpers::fusion_invocations();
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
                // Full fusion + JIT; checks the precise `FuseExpect`
                // and (since the interpreter is gone, so the old
                // `fused`-mode shape check is gone with it) the
                // backend-independent NodeShape against the live graph.
                run_with_flags(
                    ::graphix_compiler::BitFlags::empty(),
                    true,
                    true,
                    true,
                ).await
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
