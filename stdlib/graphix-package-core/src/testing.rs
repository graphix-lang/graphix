use anyhow::{Result, bail};
use bytes::Bytes;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag,
    expr::{ResolverRef, Source, VfsResolver},
};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt, RegistrationImage};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

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
/// fixture runs, in `jit` mode only; the per-thread counters are reset
/// after runtime init so they reflect only the fixture's own program.
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
        &mut graphix_compiler::ExecCtx<
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
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    // A fixture that recurses without end must abort instead of
    // eating the box; an explicit GRAPHIX_STACK_BUDGET still wins.
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        graphix_compiler::set_stack_budget(1 << 30);
    }
    init_inner(sub, register, resolvers, flags, false, None, None, None, setup).await
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
    program_image: Option<oneshot::Sender<Result<Bytes>>>,
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
        |_| {},
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
        &mut graphix_compiler::ExecCtx<
            GXRt<NoExt>,
            <NoExt as graphix_rt::GXExt>::UserEvent,
        >,
    ),
{
    init_inner(sub, register, resolvers, flags, true, None, None, None, setup).await
}

async fn init_inner<F>(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[PackageRef],
    resolvers: Vec<ResolverRef>,
    flags: BitFlags<CFlag>,
    lsp_mode: bool,
    registration: Option<RegistrationImage>,
    program: Option<Source>,
    program_image: Option<oneshot::Sender<Result<Bytes>>>,
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
    // Nothing seeds NetConfig, so tests that touch sys::net share one
    // process-internal netidx materialized on demand.
    let mut ctx = graphix_compiler::ExecCtx::new(GXRt::<NoExt>::new())?;
    let mut modules = ahash::AHashMap::default();
    let mut root_mods = graphix_package::IndexSet::new();
    for p in register {
        p.register(&mut ctx, &mut modules, &mut root_mods)?;
    }
    setup(&mut ctx);
    let root = graphix_package::root_module_source(&root_mods);
    let mut all_resolvers = vec![VfsResolver::new(modules)];
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
    Ok(TestCtx { rt: cfg.build()?.start().await? })
}

/// Evaluate a graphix expression and return its Value.
///
/// Compiles `code` as `let result = {code}` in a throwaway module,
/// waits for the first update, and returns the resulting value along
/// with the test context (caller must shut it down).
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
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(gx_code)),
    )]);
    let resolver = VfsResolver::new(tbl);
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

/// Like [`eval`], but for a program that converges over several cycles:
/// collects updates for a brief window and returns the LAST value of the
/// result expr.
pub async fn eval_converged(
    code: &str,
    register: &[PackageRef],
) -> Result<(Value, TestCtx)> {
    let (tx, mut rx) = mpsc::channel(10);
    let gx_code = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(gx_code)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx = init_with_setup(tx, register, vec![resolver], |_| {}).await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let deadline = tokio::time::sleep(std::time::Duration::from_millis(500));
    tokio::pin!(deadline);
    let mut last: Option<Value> = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => break,
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e
                            && id == eid
                        {
                            last = Some(v);
                        }
                    }
                }
            }
        }
    }
    match last {
        Some(v) => Ok((v, ctx)),
        None => bail!("no update within deadline"),
    }
}

/// Like [`eval`], but ships the `/test.gx` module as a packed pre-parsed
/// AST (`serialize::pack_module`) rather than source, so the resolver
/// takes its `unpack_module` path. The result must match [`eval`].
pub async fn eval_packed(
    code: &str,
    register: &[PackageRef],
) -> Result<(Value, TestCtx)> {
    let (tx, mut rx) = mpsc::channel(10);
    let gx_code = format!("let result = {code}");
    let source = arcstr::ArcStr::from(gx_code);
    let ori = graphix_compiler::expr::Origin {
        parent: None,
        source: graphix_compiler::expr::Source::Internal(arcstr::literal!("test")),
        text: source.clone(),
    };
    let exprs = graphix_compiler::expr::parser::parse(ori)?;
    let packed = graphix_compiler::expr::serialize::pack_module(&exprs)?;
    let entry = graphix_compiler::expr::VfsEntry { source, packed: Some(packed) };
    let tbl =
        ahash::AHashMap::from_iter([(netidx_core::path::Path::from("/test.gx"), entry)]);
    let resolver = VfsResolver::new(tbl);
    let ctx = init_with_resolvers(tx, register, vec![resolver]).await?;
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
/// - **interp**: `CFlag::FusionDisabled` set; the program runs purely
///   through the node-walk.
/// - **jit**: the full fusion + JIT path. Asserts the `FuseExpect`
///   annotation (and the optional `; shape:` NodeShape) against the
///   live post-fusion graph; debug builds only, where the counters exist.
///
/// Expands to `mod $name { fn interp() … fn jit() … }` — two
/// `#[tokio::test(flavor = "current_thread")]` functions.
#[macro_export]
macro_rules! run {
    // The `; shape:` arms must precede the plain `; $fexpect` arms so
    // the longer token sequence matches first.
    ($name:ident, $code:expr, $pred:expr; $fexpect:expr; shape: $shape:expr) => {
        $crate::run!(@impl $name, $pred, 30, $fexpect, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr; shape: $shape:expr) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::Some($shape), "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr) => {
        $crate::run!(@impl $name, $pred, $timeout, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl $name, $pred, 30, $crate::testing::FuseExpect::Jit, ::std::option::Option::None, $($path => $code),+);
    };
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
            /// compiled graph.
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
                    $((
                        ::netidx_core::path::Path::from($path),
                        ::graphix_compiler::expr::VfsEntry::from(::arcstr::ArcStr::from($code)),
                    )),+
                ]);
                let resolver = ::graphix_compiler::expr::VfsResolver::new(tbl);
                let ctx = $crate::testing::init_with_flags_and_setup(
                    tx, &crate::TEST_REGISTER, vec![resolver], flags,
                    |_ctx| {},
                ).await?;
                // Init compiles the stdlib root and may fuse there;
                // only the fixture's own compile should count.
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
                                                ::graphix_rt::GXEvent::Diagnostic(
                                                    _,
                                                    d,
                                                ) => eprintln!("{d}"),
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
                #[cfg(debug_assertions)]
                if fusion_check {
                    $crate::testing::check_fuse_expectation($fexpect);
                }
                // The blocker list includes stdlib-root noise (stats
                // are per-ExecCtx).
                if ::std::env::var("GRAPHIX_FUSE_AUDIT").is_ok()
                    && !flags
                        .contains(::graphix_compiler::CFlag::FusionDisabled)
                {
                    if let ::std::result::Result::Ok(stats) =
                        ctx.fusion_stats().await
                    {
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
                Ok(())
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn interp() -> ::anyhow::Result<()> {
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
                // GRAPHIX_FUSION_DISCOVERY: run without asserting and
                // print the observed level (`FUSEMAP <path> <level>`).
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
                // GRAPHIX_FUSE_AUDIT: report the observed fusion level
                // against the annotation (`FUSEAUDIT` lines) instead
                // of asserting it.
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
