use anyhow::Result;
use graphix_compiler::expr::ModuleResolver;
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
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
    &mut fxhash::FxHashMap<netidx_core::path::Path, arcstr::ArcStr>,
) -> Result<()>;

pub async fn init_with_resolvers(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    root: arcstr::ArcStr,
    mut resolvers: Vec<ModuleResolver>,
) -> Result<TestCtx> {
    let _ = env_logger::try_init();
    let env = netidx::InternalOnly::new().await?;
    let mut ctx = graphix_compiler::ExecCtx::new(GXRt::<NoExt>::new(
        env.publisher().clone(),
        env.subscriber().clone(),
    ))?;
    let mut modules = fxhash::FxHashMap::default();
    for f in register {
        f(&mut ctx, &mut modules)?;
    }
    resolvers.insert(0, ModuleResolver::VFS(modules));
    Ok(TestCtx {
        internal_only: env,
        rt: GXConfig::builder(ctx, sub)
            .root(root)
            .resolvers(resolvers)
            .build()?
            .start()
            .await?,
    })
}

pub async fn init(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    register: &[RegisterFn],
    root: arcstr::ArcStr,
) -> Result<TestCtx> {
    init_with_resolvers(sub, register, root, vec![]).await
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

#[macro_export]
macro_rules! run {
    ($name:ident, $code:expr, $pred:expr) => {
        $crate::run!($name, $pred, "/test.gx" => format!("let result = {}", $code));
    };
    ($name:ident, $pred:expr, $($path:literal => $code:expr),+) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $name() -> ::anyhow::Result<()> {
            let (tx, mut rx) = ::tokio::sync::mpsc::channel(10);
            let tbl = ::fxhash::FxHashMap::from_iter([
                $((::netidx_core::path::Path::from($path), ::arcstr::ArcStr::from($code))),+
            ]);
            let resolver = ::graphix_compiler::expr::ModuleResolver::VFS(tbl);
            let ctx = $crate::testing::init_with_resolvers(
                tx, &crate::TEST_REGISTER, crate::TEST_ROOT, vec![resolver],
            ).await?;
            let bs = &ctx.rt;
            match bs.compile(::arcstr::literal!("{ mod test; test::result }")).await {
                Err(e) => assert!($pred(dbg!(Err(e)))),
                Ok(e) => {
                    dbg!("compilation succeeded");
                    let eid = e.exprs[0].id;
                    loop {
                        match rx.recv().await {
                            None => ::anyhow::bail!("runtime died"),
                            Some(mut batch) => {
                                for e in batch.drain(..) {
                                    match e {
                                        ::graphix_rt::GXEvent::Env(_) => (),
                                        ::graphix_rt::GXEvent::Updated(id, v) => {
                                            eprintln!("{v}");
                                            assert_eq!(id, eid);
                                            assert!($pred(Ok(&v)));
                                            return Ok(());
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
            let ctx = $crate::testing::init(tx, &crate::TEST_REGISTER, crate::TEST_ROOT).await?;
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
