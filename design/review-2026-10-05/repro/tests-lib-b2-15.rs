/// Whether `pid` names a live process (a zombie is dead).
#[cfg(target_os = "linux")]
fn alive(pid: i64) -> bool {
    std::fs::read_to_string(format!("/proc/{pid}/stat")).is_ok_and(|stat| {
        stat.rsplit(')').next().is_some_and(|rest| !rest.trim_start().starts_with('Z'))
    })
}

// Dropping the expression that holds the last handle to a child spawned
// with #kill_on_drop kills the child (a module's bindings would outlive
// the expression that loaded it, so the program is compiled bare).
#[cfg(target_os = "linux")]
#[tokio::test(flavor = "current_thread")]
async fn kill_on_drop_kills_the_child() -> Result<()> {
    use graphix_package_core::testing::{Mode, fixture_runtime, next_update};
    use std::time::Duration;
    use tokio::time::Instant;
    let src = r#"{
  let child = sys::process::spawn(sys::process::options(
    #args: ["30"],
    #kill_on_drop: true,
    "/bin/sleep"
  ))?;
  sys::process::pid(child.proc)
}"#;
    let (ctx, mut rx) =
        fixture_runtime([], crate::TEST_REGISTER, Mode::Jit, |_| {}).await?;
    let res = ctx.rt.compile(arcstr::ArcStr::from(src)).await?;
    let deadline = Instant::now() + Duration::from_secs(5);
    let pid = match next_update(&mut rx, res.exprs[0].id, deadline).await? {
        Value::I64(pid) => pid,
        v => anyhow::bail!("expected a pid, got {v:?}"),
    };
    assert!(alive(pid), "the child is not running");
    drop(res);
    ctx.rt.wait_idle().await?;
    let deadline = Instant::now() + Duration::from_secs(5);
    while alive(pid) {
        if Instant::now() >= deadline {
            anyhow::bail!("child {pid} outlived the expression that held it");
        }
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    ctx.shutdown().await;
    Ok(())
}
