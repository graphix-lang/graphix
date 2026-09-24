// A JIT module's code arena is freed once the module and every kernel
// compiled into it have dropped: a context per iteration reserves an
// arena, and the reservations do not accumulate. Its own test binary,
// so no other test's contexts move the address space it measures.

#[cfg(target_os = "linux")]
mod linux {
    use anyhow::{Context, Result};
    use graphix_package_core::testing::eval;
    use netidx_value::Value;
    use std::time::Duration;

    fn vm_size_kb() -> Result<u64> {
        let status = std::fs::read_to_string("/proc/self/status")?;
        let line = status
            .lines()
            .find(|l| l.starts_with("VmSize:"))
            .context("no VmSize in /proc/self/status")?;
        Ok(line.split_whitespace().nth(1).context("VmSize value")?.parse()?)
    }

    async fn fuse_once() -> Result<()> {
        let packages: &[graphix_package_core::testing::PackageRef] =
            graphix_package::package_refs!();
        let (v, ctx) =
            eval("{ let f = |x: i64| x * 2 + 1; #[native] f(20) }", packages).await?;
        assert_eq!(v, Value::I64(41));
        ctx.shutdown().await;
        // Let the runtime task see its handle gone and drop its context.
        tokio::time::sleep(Duration::from_millis(50)).await;
        Ok(())
    }

    #[tokio::test(flavor = "current_thread")]
    async fn jit_arenas_are_freed() -> Result<()> {
        const N: u64 = 16;
        fuse_once().await?;
        let before = vm_size_kb()?;
        for _ in 0..N {
            fuse_once().await?;
        }
        let grown_mb = vm_size_kb()?.saturating_sub(before) / 1024;
        // A leaked arena is a 256MB reservation.
        assert!(grown_mb < N * 64, "address space grew {grown_mb}MB over {N} contexts");
        Ok(())
    }
}
