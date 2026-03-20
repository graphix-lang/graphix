use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_package_core::{testing, ProgramArgs};
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;

fn get_field<'a>(v: &'a Value, name: &str) -> Option<&'a Value> {
    match v {
        Value::Array(a) => {
            for pair in a.iter() {
                if let Value::Array(kv) = pair {
                    if kv.len() == 2 {
                        if let Value::String(k) = &kv[0] {
                            if &**k == name {
                                return Some(&kv[1]);
                            }
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn get_command(v: &Value) -> Result<&[Value]> {
    match get_field(v, "command") {
        Some(Value::Array(a)) => Ok(a),
        other => bail!("expected command array, got {other:?}"),
    }
}

fn get_values(v: &Value) -> Result<&CMap<Value, Value, 32>> {
    match get_field(v, "values") {
        Some(Value::Map(m)) => Ok(m),
        other => bail!("expected values map, got {other:?}"),
    }
}

fn map_get<'a>(m: &'a CMap<Value, Value, 32>, key: &str) -> Option<&'a Value> {
    m.get(&Value::String(ArcStr::from(key)))
}

const PARSE_DEFAULTS: &str = r#"
{
    use args;
    args::parse(
        args::command(
            #name: "test",
            [
                args::option(#name: "count", #default: "1"),
                args::flag(#name: "verbose"),
            ]
        )
    )
}
"#;

#[tokio::test(flavor = "current_thread")]
async fn parse_defaults() -> Result<()> {
    let (v, ctx) =
        testing::eval_with_setup(PARSE_DEFAULTS, &crate::TEST_REGISTER, |ctx| {
            ctx.libstate.set(ProgramArgs(vec![ArcStr::from("script.gx")]));
        })
        .await?;
    assert!(!matches!(&v, Value::Error(_)), "unexpected error: {v:?}");
    let cmd = get_command(&v)?;
    assert!(cmd.is_empty(), "expected no subcommand, got {cmd:?}");
    let vals = get_values(&v)?;
    assert_eq!(
        map_get(vals, "count"),
        Some(&Value::String(ArcStr::from("1"))),
        "default not applied"
    );
    assert_eq!(
        map_get(vals, "verbose"),
        Some(&Value::String(ArcStr::from("false"))),
        "flag should default to false"
    );
    ctx.shutdown().await;
    Ok(())
}

const PARSE_FLAGS: &str = r#"
{
    use args;
    args::parse(
        args::command(
            #name: "test",
            [
                args::flag(#name: "verbose", #short: "v"),
            ]
        )
    )
}
"#;

#[tokio::test(flavor = "current_thread")]
async fn parse_flags() -> Result<()> {
    let (v, ctx) = testing::eval_with_setup(PARSE_FLAGS, &crate::TEST_REGISTER, |ctx| {
        ctx.libstate
            .set(ProgramArgs(vec![ArcStr::from("script.gx"), ArcStr::from("-v")]));
    })
    .await?;
    assert!(!matches!(&v, Value::Error(_)), "unexpected error: {v:?}");
    let vals = get_values(&v)?;
    assert_eq!(
        map_get(vals, "verbose"),
        Some(&Value::String(ArcStr::from("true"))),
        "flag should be true when passed"
    );
    ctx.shutdown().await;
    Ok(())
}

const PARSE_OPTIONS: &str = r#"
{
    use args;
    args::parse(
        args::command(
            #name: "test",
            [
                args::option(#name: "port", #short: "p", #default: "8080"),
            ]
        )
    )
}
"#;

#[tokio::test(flavor = "current_thread")]
async fn parse_options() -> Result<()> {
    let (v, ctx) =
        testing::eval_with_setup(PARSE_OPTIONS, &crate::TEST_REGISTER, |ctx| {
            ctx.libstate.set(ProgramArgs(vec![
                ArcStr::from("script.gx"),
                ArcStr::from("--port"),
                ArcStr::from("3000"),
            ]));
        })
        .await?;
    assert!(!matches!(&v, Value::Error(_)), "unexpected error: {v:?}");
    let vals = get_values(&v)?;
    assert_eq!(
        map_get(vals, "port"),
        Some(&Value::String(ArcStr::from("3000"))),
        "option value not extracted"
    );
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn parse_option_default() -> Result<()> {
    let (v, ctx) =
        testing::eval_with_setup(PARSE_OPTIONS, &crate::TEST_REGISTER, |ctx| {
            ctx.libstate.set(ProgramArgs(vec![ArcStr::from("script.gx")]));
        })
        .await?;
    assert!(!matches!(&v, Value::Error(_)), "unexpected error: {v:?}");
    let vals = get_values(&v)?;
    assert_eq!(
        map_get(vals, "port"),
        Some(&Value::String(ArcStr::from("8080"))),
        "default not applied when option omitted"
    );
    ctx.shutdown().await;
    Ok(())
}

const PARSE_SUBCOMMANDS: &str = r#"
{
    use args;
    args::parse(
        args::command(
            #name: "test",
            #subcommands: [
                args::command(
                    #name: "serve",
                    [args::option(#name: "port", #default: "8080")]
                )
            ],
            []
        )
    )
}
"#;

#[tokio::test(flavor = "current_thread")]
async fn parse_subcommands() -> Result<()> {
    let (v, ctx) =
        testing::eval_with_setup(PARSE_SUBCOMMANDS, &crate::TEST_REGISTER, |ctx| {
            ctx.libstate.set(ProgramArgs(vec![
                ArcStr::from("script.gx"),
                ArcStr::from("serve"),
                ArcStr::from("--port"),
                ArcStr::from("9090"),
            ]));
        })
        .await?;
    assert!(!matches!(&v, Value::Error(_)), "unexpected error: {v:?}");
    let cmd = get_command(&v)?;
    assert_eq!(cmd, &[Value::String(ArcStr::from("serve"))]);
    let vals = get_values(&v)?;
    assert_eq!(
        map_get(vals, "port"),
        Some(&Value::String(ArcStr::from("9090"))),
        "subcommand option not extracted"
    );
    ctx.shutdown().await;
    Ok(())
}

const PARSE_ERROR: &str = r#"
{
    use args;
    args::parse(
        args::command(
            #name: "test",
            [
                args::positional(#name: "file", #required: true),
            ]
        )
    )
}
"#;

#[tokio::test(flavor = "current_thread")]
async fn parse_error_missing_required() -> Result<()> {
    let (v, ctx) = testing::eval_with_setup(PARSE_ERROR, &crate::TEST_REGISTER, |ctx| {
        ctx.libstate.set(ProgramArgs(vec![ArcStr::from("script.gx")]));
    })
    .await?;
    assert!(matches!(&v, Value::Error(_)), "expected error, got: {v:?}");
    ctx.shutdown().await;
    Ok(())
}
