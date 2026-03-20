use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{run, testing, ProgramArgs};
use netidx::subscriber::Value;

const ARGS_EMPTY: &str = r#"
    sys::args()
"#;

run!(args_empty, ARGS_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.is_empty(),
    _ => false,
});

#[tokio::test(flavor = "current_thread")]
async fn args_injected() -> Result<()> {
    let code = r#"sys::args()"#;
    let (v, ctx) = testing::eval_with_setup(code, &crate::TEST_REGISTER, |ctx| {
        ctx.libstate.set(ProgramArgs(vec![
            ArcStr::from("script.gx"),
            ArcStr::from("--port"),
            ArcStr::from("8080"),
        ]));
    })
    .await?;
    match &v {
        Value::Array(a) => {
            assert_eq!(a.len(), 3);
            assert_eq!(a[0], Value::String(ArcStr::from("script.gx")));
            assert_eq!(a[1], Value::String(ArcStr::from("--port")));
            assert_eq!(a[2], Value::String(ArcStr::from("8080")));
        }
        other => panic!("expected Array, got {other:?}"),
    }
    ctx.shutdown().await;
    Ok(())
}
