// Dense delivery (design/dense_delivery.md): bottom is a production and
// a builtin arg never rides its previous value. Do not adjust these
// expectations without a ruling.

use anyhow::{Result, bail};
use graphix_compiler::CFlag;
use graphix_package_core::{PrintSink, testing::init_with_flags_and_setup};
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

/// Run `code` (wrapped as `let result = {code}`) to quiescence in one
/// mode, collecting every update of the result expression and the
/// captured print output. Quiescence = no events for 700ms.
pub(super) async fn run_delta(
    code: &str,
    fusion_disabled: bool,
) -> Result<(Vec<Value>, String)> {
    let (tx, mut rx) = mpsc::channel(10);
    let gx_code = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(gx_code)),
    )]);
    let resolver = graphix_compiler::expr::VfsResolver::new(tbl);
    let flags = if fusion_disabled {
        CFlag::FusionDisabled.into()
    } else {
        graphix_compiler::BitFlags::empty()
    };
    let sink = PrintSink::default();
    let seeded = sink.clone();
    let ctx = init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![resolver],
        flags,
        move |ctx| {
            *ctx.libstate.get_or_default::<PrintSink>() = seeded;
        },
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let mut values = Vec::new();
    let deadline = tokio::time::sleep(std::time::Duration::from_secs(20));
    tokio::pin!(deadline);
    loop {
        let quiet = tokio::time::sleep(std::time::Duration::from_millis(700));
        tokio::pin!(quiet);
        tokio::select! {
            _ = &mut deadline => bail!("global deadline: program did not quiesce"),
            _ = &mut quiet => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        match e {
                            GXEvent::Updated(id, v) if id == eid => values.push(v),
                            GXEvent::Diagnostic(_, d) => eprintln!("{d}"),
                            _ => (),
                        }
                    }
                }
            }
        }
    }
    let out = sink.take();
    ctx.shutdown().await;
    Ok((values, out))
}

pub(super) fn as_i64s(values: &[Value]) -> Vec<i64> {
    values
        .iter()
        .map(|v| match v {
            Value::I64(n) => *n,
            v => panic!("expected i64, got {v:?}"),
        })
        .collect()
}

// A constant print message fires once in both engines.
const PRINT_CONST_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 5 => n <- (x ~ n) + 1, _ => never() };
  let r = { println("A"); n };
  r
}"#;

async fn print_const_once(fusion_disabled: bool) -> Result<()> {
    let (values, out) = run_delta(PRINT_CONST_ONCE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![0, 1, 2, 3, 4, 5]);
    assert_eq!(out, "A\n");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn print_const_once_interp() -> Result<()> {
    print_const_once(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn print_const_once_jit() -> Result<()> {
    print_const_once(false).await
}

// A callback's print fires once per element, not per kernel invocation.
const PRINT_HOF_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 3 => n <- (x ~ n) + 1, _ => never() };
  let a = [i64:1, i64:2];
  let r = { let m = array::map(a, |x| { println("@P[x]"); x * i64:2 }); (n, array::len(m)) };
  println("@R[r]");
  i64:0
}"#;

async fn print_hof_once(fusion_disabled: bool) -> Result<()> {
    let (values, out) = run_delta(PRINT_HOF_ONCE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![0]);
    assert_eq!(out, "@P1\n@P2\n@R(0, 2)\n@R(1, 2)\n@R(2, 2)\n@R(3, 2)\n");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn print_hof_once_interp() -> Result<()> {
    print_hof_once(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn print_hof_once_jit() -> Result<()> {
    print_hof_once(false).await
}

// A bottomed builtin arg bottoms the invocation: epochs give
// [1, 9] and the bottoming third epoch emits nothing.
const BOTTOM_PROPAGATES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 2 => n + 1, _ => never() };
  let in0 = select ep { 2 => i64:1, _ => i64:0 };
  let in1 = select ep { 1 => true, _ => false };
  let v0 = i64:1 - in0;
  select in1 { true => i64:9, _ => max(in0 * i64:10, i64:1 / v0) }
}"#;

async fn builtin_bottom_propagates(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(BOTTOM_PROPAGATES, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 9]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn builtin_bottom_propagates_interp() -> Result<()> {
    builtin_bottom_propagates(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn builtin_bottom_propagates_jit() -> Result<()> {
    builtin_bottom_propagates(false).await
}
