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

impl TestCtx {
    pub async fn shutdown(self) {
        drop(self.rt);
        self.internal_only.shutdown().await
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
    init_with_flags_and_setup(sub, register, resolvers, BitFlags::empty(), setup)
        .await
}

/// Like [`init_with_setup`] but lets the caller pin the
/// compile-time flags (`CFlag::FusionDisabled`, `CFlag::JitDisabled`,
/// etc.) the runtime will pass to every `compile()` it dispatches.
/// Used by the [`run!`] macro to drive the same fixture through
/// the interp / fused / jit modes.
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

/// Run a graphix fixture under three modes and assert the supplied
/// predicate holds for the produced Value in each.
///
/// **Mode 1 (interp)**: `fusion_disabled=true`. Every lambda runs as
///   `GXLambda`. This is the ground-truth path.
/// **Mode 2 (fused)**: `fusion_disabled=false, jit_mode=Off,
///   whole_graph=true`. Fused kernels dispatch through `gir_interp`.
/// **Mode 3 (jit)**: `fusion_disabled=false, jit_mode=Forced,
///   whole_graph=true`. Soft JIT failures panic; the JIT
///   invocation counter must be > 0 after the run.
///
/// The macro expands to a child module with three
/// `#[tokio::test(flavor = "current_thread")]` functions named
/// `interp`, `fused`, and `jit`. Each runs the same fixture under
/// its mode and asserts `$pred`.
///
/// Use `run_no_jit!` for fixtures that legitimately can't JIT
/// (async builtins, network IO, or any program that won't produce
/// a kernel — yet — but should still round-trip through interp).
/// Run a graphix fixture under three modes and assert the supplied
/// predicate holds for the produced Value in each:
///
/// - **interp**: `CFlag::FusionDisabled` set. Fusion is a no-op;
///   the program runs purely through the Update-trait node-graph
///   interpreter. Ground truth for differential testing.
/// - **fused**: `CFlag::JitDisabled` set. Fusion builds and
///   splices kernels but they dispatch via [`crate::gir_interp`]
///   instead of native code. Validates the GIR translation
///   independently of the JIT backend.
/// - **jit**: no flags set. The full fusion + JIT path. Resets
///   the JIT invocation counter at start, runs, asserts the
///   counter is `> 0` at the end — proves the JIT actually ran
///   rather than silently falling back. `cfg(debug_assertions)`-
///   gated since the counter only exists in debug builds.
///
/// Expands to a child module `mod $name { fn interp() … fn fused()
/// … fn jit() … }` so cargo test reports a separate result per
/// mode.
///
/// Use [`run_no_jit!`] for fixtures that legitimately can't JIT —
/// async builtins, network IO, or anything whose kernel build
/// doesn't succeed today but should still round-trip through
/// interp + fused.
#[macro_export]
macro_rules! run {
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl $name, $pred, 30, true, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr) => {
        $crate::run!(@impl $name, $pred, $timeout, true, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl $name, $pred, 30, true, $($path => $code),+);
    };
    (@impl $name:ident, $pred:expr, $timeout:expr, $with_jit:expr, $($path:literal => $code:expr),+) => {
        mod $name {
            use super::*;

            async fn run_with_flags(
                flags: ::graphix_compiler::BitFlags<::graphix_compiler::CFlag>,
                reset_jit_counter_after_init: bool,
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
                // Reset the JIT invocation counter AFTER runtime
                // init — init compiles the stdlib root module and
                // may JIT-fuse things there. We only want to count
                // JIT activity caused by the fixture's own compile
                // below.
                if reset_jit_counter_after_init {
                    #[cfg(debug_assertions)]
                    ::graphix_compiler::gir_jit_helpers::reset_jit_invocations();
                }
                let bs = &ctx.rt;
                match bs.compile(::arcstr::literal!("{ mod test; test::result }")).await {
                    Err(e) => assert!(pred(dbg!(Err(e)))),
                    Ok(e) => {
                        let eid = e.exprs[0].id;
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
                                        for e in batch.drain(..) {
                                            match e {
                                                ::graphix_rt::GXEvent::Env(_) => (),
                                                ::graphix_rt::GXEvent::Updated(id, v) => {
                                                    eprintln!("{v}");
                                                    assert_eq!(id, eid);
                                                    assert!(pred(Ok(&v)));
                                                    ctx.shutdown().await;
                                                    return Ok(());
                                                }
                                            }
                                        }
                                    }
                                }
                            }
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
                ).await
            }

            #[::tokio::test(flavor = "current_thread")]
            async fn fused() -> ::anyhow::Result<()> {
                run_with_flags(
                    ::graphix_compiler::CFlag::JitDisabled.into(),
                    false,
                ).await
            }

            #[::tokio::test(flavor = "current_thread")]
            #[cfg(debug_assertions)]
            async fn jit() -> ::anyhow::Result<()> {
                if !$with_jit {
                    // `run_no_jit!` expanded — fixture opted out of
                    // the JIT mode. The `cfg(debug_assertions)`
                    // gate would have already skipped the test in
                    // release builds; here we have to keep the
                    // function present (the macro emits one body
                    // for both arms) but no-op it.
                    return Ok(());
                }
                run_with_flags(::graphix_compiler::BitFlags::empty(), true).await?;
                let inv = ::graphix_compiler::gir_jit_helpers::jit_invocations();
                if inv == 0 {
                    ::anyhow::bail!(
                        "JIT_INVOCATIONS=0 — the JIT compiled but never \
                         ran for this fixture. Either fix the underlying \
                         cliff or migrate the test to `run_no_jit!`.",
                    );
                }
                Ok(())
            }
        }
    };
}

/// Like [`run!`] but skips the `jit` mode. For fixtures that
/// legitimately can't JIT today — async builtins, network IO, fs
/// watchers, or any program whose kernel build doesn't yet
/// succeed but should still round-trip through interp + fused.
#[macro_export]
macro_rules! run_no_jit {
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!(@impl $name, $pred, 30, false, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $code:expr, $pred:expr, timeout: $timeout:expr) => {
        $crate::run!(@impl $name, $pred, $timeout, false, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        $crate::run!(@impl $name, $pred, 30, false, $($path => $code),+);
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
