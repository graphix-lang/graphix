use anyhow::Result;
use enumflags2::BitFlags;
use graphix_compiler::ExecCtx;
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use poolshark::global::GPooled;
use std::sync::OnceLock;
use tokio::sync::{mpsc, Mutex};

#[cfg(test)]
mod lang;

#[cfg(test)]
mod lib;

pub struct TestCtx {
    pub _internal_only: netidx::InternalOnly,
    pub rt: GXHandle<NoExt>,
}

// Serialize InternalOnly creation to work around Windows socket binding issues.
// tokio::net::TcpListener only sets SO_REUSEADDR on Unix, not Windows, causing
// error 10013 (WSAEACCES) when multiple tests try to bind concurrently.
static INIT_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent<NoExt>>>>) -> Result<TestCtx> {
    let _ = env_logger::try_init();

    // Acquire lock to serialize InternalOnly::new() calls
    let lock = INIT_LOCK.get_or_init(|| Mutex::new(()));
    let _guard = lock.lock().await;

    let env = netidx::InternalOnly::new().await?;

    // Release lock after socket is bound
    drop(_guard);

    let mut ctx = ExecCtx::new(GXRt::<NoExt>::new(
        env.publisher().clone(),
        env.subscriber().clone(),
    ));
    let (root, mods) = crate::register(&mut ctx, BitFlags::all())?;
    Ok(TestCtx {
        _internal_only: env,
        rt: GXConfig::builder(ctx, sub)
            .root(root)
            .resolvers(vec![mods])
            .build()?
            .start()
            .await?,
    })
}

#[macro_export]
macro_rules! run {
    ($name:ident, $code:expr, $pred:expr) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $name() -> ::anyhow::Result<()> {
            let (tx, mut rx) = tokio::sync::mpsc::channel(10);
            let ctx = $crate::test::init(tx).await?;
            let bs = ctx.rt;
            match bs.compile(arcstr::ArcStr::from($code)).await {
                Err(e) => assert!($pred(dbg!(Err(e)))),
                Ok(e) => {
                    dbg!("compilation succeeded");
                    let eid = e.exprs[0].id;
                    loop {
                        match rx.recv().await {
                            None => bail!("runtime died"),
                            Some(mut batch) => {
                                for e in batch.drain(..) {
                                    match e {
                                        graphix_rt::GXEvent::Env(_) => (),
                                        graphix_rt::GXEvent::Updated(id, v) => {
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
            Ok(())
        }
    };
}

use graphix_compiler::expr::parser::GRAPHIX_ESC;
use poolshark::local::LPooled;
use std::{fmt::Write, path::Display};

fn escape_path(path: Display) -> LPooled<String> {
    let mut buf: LPooled<String> = LPooled::take();
    let mut res: LPooled<String> = LPooled::take();
    write!(buf, "{path}").unwrap();
    GRAPHIX_ESC.escape_to(&*buf, &mut res);
    res
}

/// run a test with a temp dir and setup code. The final output of the setup
/// block is a path that will be passed to the code using format!
#[macro_export]
macro_rules! run_with_tempdir {
    // Error expectation case - delegates to main pattern
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        expect_error
    ) => {
        run_with_tempdir! {
            name: $test_name,
            code: $code,
            setup: |$temp_dir| $setup,
            expect: |v: Value| -> Result<()> {
                if matches!(v, Value::Error(_)) {
                    Ok(())
                } else {
                    panic!("expected Error value, got: {v:?}")
                }
            }
        }
    };
    // Success case with verification - delegates to main pattern
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        verify: |$verify_dir:ident| $verify:block
    ) => {
        run_with_tempdir! {
            name: $test_name,
            code: $code,
            setup: |$temp_dir| $setup,
            expect: |v: Value| -> Result<()> {
                if !matches!(v, Value::Null) {
                    panic!("expected Null (success), got: {v:?}");
                }
                Ok(())
            },
            verify: |$verify_dir| $verify
        }
    };
    // Main pattern with custom expectation and optional verification
    (
        name: $test_name:ident,
        code: $code:literal,
        setup: |$temp_dir:ident| $setup:block,
        expect: $expect_payload:expr
        $(, verify: |$verify_dir:ident| $verify:block)?
    ) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $test_name() -> Result<()> {
            let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent<_>>>>(10);
            let ctx = init(tx).await?;
            let $temp_dir = tempfile::tempdir()?;

            // Run setup block which should return test_file
            let test_file = { $setup };

            let code = format!($code, crate::test::escape_path(test_file.display()));
            let compiled = ctx.rt.compile(ArcStr::from(code)).await?;
            let eid = compiled.exprs[0].id;

            let timeout = tokio::time::sleep(Duration::from_secs(2));
            tokio::pin!(timeout);

            loop {
                tokio::select! {
                    _ = &mut timeout => panic!("timeout waiting for result"),
                    Some(mut batch) = rx.recv() => {
                        for event in batch.drain(..) {
                            if let GXEvent::Updated(id, v) = event {
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
