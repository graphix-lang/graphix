//! `GXHandle::interrupt()` / `abort()` recover from and shut down a
//! wedged runtime. These run on a `multi_thread` runtime: the wedged
//! `do_cycle` blocks one worker while the test fires the control flag
//! from another. The sync tail loop wedges: fused it is a native loop in
//! constant memory; node-walked every call is a retained activation, so
//! its memory grows until the interrupt lands.

use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::CFlag;
use graphix_package_core::testing::{TestCtx, config};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::{sync::mpsc, time::Duration};

/// An unbounded sync tail loop: `v + 1` wraps, so it spins forever
/// within one cycle. Native in jit mode, node-walked in interp mode.
const TAIL_LOOP: &str = "{ let rec f = |v: i64| -> i64 f(v + 1); f(0) }";

async fn init_flags(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    flags: BitFlags<CFlag>,
) -> Result<TestCtx> {
    TestCtx::start(config(sub, crate::TEST_REGISTER, vec![], |_| {})?.flags(flags)).await
}

/// Wedge the runtime with `program`, confirm a probe cannot be served,
/// then `interrupt()` and confirm the probe completes.
async fn interrupt_recovers(program: &str, flags: BitFlags<CFlag>) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    // Drain the event channel; a full channel would let the runtime
    // service commands via its send-timeout fallback, masking the wedge.
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, flags).await?;
    // `compile` is answered before `do_cycle` runs the program.
    ctx.rt.compile(ArcStr::from(program)).await?;
    let mut probe = {
        let rt = ctx.rt.clone();
        tokio::spawn(async move { rt.get_env().await })
    };
    let wedged = tokio::time::timeout(Duration::from_millis(500), &mut probe).await;
    if wedged.is_ok() {
        bail!("runtime did not wedge — {program:?} is not an unbounded loop");
    }
    ctx.rt.interrupt();
    match tokio::time::timeout(Duration::from_secs(5), probe).await {
        Err(_) => bail!("runtime did not recover after interrupt() — probe still hung"),
        Ok(Err(e)) => bail!("probe task panicked: {e}"),
        Ok(Ok(Err(e))) => bail!("get_env failed after recovery: {e}"),
        Ok(Ok(Ok(_env))) => {}
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_recovers_tail_loop_interp() -> Result<()> {
    interrupt_recovers(TAIL_LOOP, CFlag::FusionDisabled.into()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_recovers_tail_loop_fused() -> Result<()> {
    interrupt_recovers(TAIL_LOOP, BitFlags::empty()).await
}

/// A wedged runtime cannot be shut down by dropping the handle while
/// commands are in flight; `abort()` is a `&self` store that fires
/// anyway and every blocked caller gets an error.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn abort_unblocks_pending_commands() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    ctx.rt.compile(ArcStr::from(TAIL_LOOP)).await?;
    // Queue several commands; each blocks on its response oneshot.
    let mut pending = Vec::new();
    for _ in 0..3 {
        let rt = ctx.rt.clone();
        pending.push(tokio::spawn(async move { rt.get_env().await.map(|_| ()) }));
    }
    for _ in 0..2 {
        let rt = ctx.rt.clone();
        pending.push(tokio::spawn(async move { rt.env_stats().await.map(|_| ()) }));
    }
    tokio::time::sleep(Duration::from_millis(300)).await;
    ctx.rt.abort();
    for (i, p) in pending.into_iter().enumerate() {
        match tokio::time::timeout(Duration::from_secs(5), p).await {
            Err(_) => bail!("command {i} hung after abort() instead of erroring"),
            Ok(Err(e)) => bail!("command {i} task panicked: {e}"),
            Ok(Ok(Ok(()))) => {
                bail!("command {i} unexpectedly served by a wedged+aborted runtime")
            }
            Ok(Ok(Err(_))) => {}
        }
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// The next value of the root `id`, within `wait`.
async fn value_of(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    id: graphix_compiler::expr::ExprId,
    wait: Duration,
) -> Option<netidx::publisher::Value> {
    let deadline = tokio::time::Instant::now() + wait;
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await.ok()??;
        for e in batch.iter() {
            if let GXEvent::Updated(eid, v) = e
                && *eid == id
            {
                return Some(graphix_package_core::testing::data(v.clone()));
            }
        }
    }
}

/// An interrupt that lands inside a recursion, fused or not, leaves its
/// value riding the last result (`GXHandle::interrupt`): a sample taken
/// after it reads that result, never a bottom.
async fn interrupted_recursion_rides(flags: BitFlags<CFlag>) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init_flags(tx, flags).await?;
    let rep = |s: &str| {
        format!(
            r#"str::replace(#pat: "a", #rep: "aaaaaaaaaa", str::replace(#pat: "a", #rep: "aaaaaaaaaa", {s}))"#
        )
    };
    let setup = [
        r#"let rec f = |n: i64, s: string| -> i64 select n { 0 => 0, n => select str::contains(#part: "b[n]", s) { true => 2, false => 1 } + f(n - 1, s) }"#.to_string(),
        format!("let s0 = \"{}\"", "a".repeat(100)),
        format!("let s1 = {}", rep("s0")),
        format!("let s = {}", rep("s1")),
        "let r = f(15000, s)".to_string(),
    ];
    // a compiled input lives while its result is held
    let mut held = Vec::new();
    for stmt in setup.iter() {
        held.push(ctx.rt.compile(ArcStr::from(stmt.as_str())).await?);
    }
    held.push(ctx.rt.compile(ArcStr::from("r")).await?);
    let r = held.last().unwrap().exprs[0].id;
    let Some(_) = value_of(&mut rx, r, Duration::from_secs(60)).await else {
        bail!("r never produced")
    };
    // a rerun binds nothing, so it is timed apart from the first run
    let rerun = |part: &str| {
        format!(
            r#"s <- sys::time::timer(duration:10.ms, false) ~ str::concat(s, "{part}")"#
        )
    };
    let started = tokio::time::Instant::now();
    held.push(ctx.rt.compile(ArcStr::from(rerun("b7"))).await?);
    let Some(first) = value_of(&mut rx, r, Duration::from_secs(60)).await else {
        bail!("r never ran again")
    };
    let took = started.elapsed().saturating_sub(Duration::from_millis(10));
    // run the recursion again and interrupt it halfway
    held.push(ctx.rt.compile(ArcStr::from(rerun("b8"))).await?);
    wedged(&ctx, took / 4).await?;
    ctx.rt.interrupt();
    held.push(
        ctx.rt
            .compile(ArcStr::from("sys::time::timer(duration:100.ms, false) ~ r"))
            .await?,
    );
    let after = held.last().unwrap().exprs[0].id;
    let rode = value_of(&mut rx, after, Duration::from_secs(10) + took * 2).await;
    drop(held);
    ctx.shutdown().await;
    match rode {
        Some(v) if v == first => Ok(()),
        Some(v) => bail!("after the interrupt r is {v}, not its last result {first}"),
        None => bail!("after the interrupt r is bottom, not its last result {first}"),
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupted_recursion_rides_interp() -> Result<()> {
    interrupted_recursion_rides(CFlag::FusionDisabled.into()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupted_recursion_rides_fused() -> Result<()> {
    interrupted_recursion_rides(BitFlags::empty()).await
}

/// One run of [`interrupted_reply_is_delivered_again`]: whether `cnt`
/// went on counting after the interrupt.
async fn timer_survives_interrupt() -> Result<bool> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    let mut held = Vec::new();
    for stmt in [
        "let rec spin = |n: i64| -> i64 select n < 0 { true => n, false => spin(n + 1) }",
        "let cnt = 0",
        "let go = sys::time::timer(duration:100.ms, false); let r = spin(go ~ 0); \
         let f = |x: i64| { let t = sys::time::timer(duration:100.ms, true); t ~ x }; \
         let tk = f(1); cnt <- tk ~ cnt + 1",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(stmt)).await?);
    }
    // XCR claude for claude: [risk/low] This interrupt fires on a wall-clock guess: 400
    // ms after compile, assuming go's cycle is already spinning. GX::do_cycle clears
    // the interrupt bit at each cycle's start, so an interrupt that lands before that
    // cycle is dropped. spin then never ends, and the compile("cnt") below waits
    // forever: a hang, not a failure. interrupted_recursion_rides has the same timing
    // shape (`10ms + took/2`). Interrupt only once the wedge is seen, e.g. `while
    // timeout(50ms, rt.get_env()).await.is_err() { rt.interrupt() }`, as
    // interrupt_recovers waits for its probe to time out. Found by reading; not run.
    // (tests-lib-b1-02)
    // 2026-10-27 claude: Both timing guesses are gone. timer_survives_interrupt calls
    // interrupt_wedge: wait until a probe stays unanswered 100 ms (the spin's cycle
    // has started), then interrupt again until the runtime answers.
    // interrupted_recursion_rides interrupts once a probe stays unanswered took/4.
    // wedged pauses as long as its probe between probes (a tight get_env loop kept
    // go's cycle from ever starting: 44M answers in 30 s, the test hung) and fails
    // after 30 s instead of hanging. No pin can fail for a timing guess; the interrupt
    // tests passed 3 runs in a row and once under taskset -c 0.
    interrupt_wedge(&ctx).await?;
    held.push(ctx.rt.compile(ArcStr::from("cnt")).await?);
    let cnt = held.last().unwrap().exprs[0].id;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    let mut counting = false;
    while let Some(v) =
        value_of(&mut rx, cnt, deadline - tokio::time::Instant::now()).await
    {
        if matches!(v, netidx::publisher::Value::I64(n) if n >= 5) {
            counting = true;
            break;
        }
    }
    ctx.shutdown().await;
    Ok(counting)
}

/// A reply an interrupt kept from its reader, here a repeating timer's
/// fire in a function body the interrupt skipped, is delivered again the
/// next cycle, so the timer re-arms. `go` and the timer in `f` land in one
/// cycle about half the time, the cycle `spin` wedges and the interrupt
/// cuts short: six runs at once, of which all but about one in 64 would
/// see a dead timer without the re-delivery.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupted_reply_is_delivered_again() -> Result<()> {
    let runs: Vec<_> = (0..6).map(|_| tokio::spawn(timer_survives_interrupt())).collect();
    for (i, run) in runs.into_iter().enumerate() {
        if !run.await?? {
            bail!("run {i}: the timer inside f died at the interrupt")
        }
    }
    Ok(())
}

/// Wait until the runtime leaves a probe unanswered for `wait`: a cycle
/// is wedged. Each answered probe is followed by a pause as long, since a
/// runtime busy answering commands does not start the cycle.
async fn wedged(ctx: &TestCtx, wait: Duration) -> Result<()> {
    let deadline = tokio::time::Instant::now() + Duration::from_secs(30);
    while tokio::time::timeout(wait, ctx.rt.get_env()).await.is_ok() {
        if tokio::time::Instant::now() > deadline {
            bail!("the runtime never wedged")
        }
        tokio::time::sleep(wait).await;
    }
    Ok(())
}

/// Interrupt a wedged cycle, again until the runtime answers: an interrupt
/// that lands before the cycle starts is dropped.
async fn interrupt_wedge(ctx: &TestCtx) -> Result<()> {
    let probe = Duration::from_millis(100);
    wedged(ctx, probe).await?;
    loop {
        ctx.rt.interrupt();
        if tokio::time::timeout(probe, ctx.rt.get_env()).await.is_ok() {
            return Ok(());
        }
    }
}

/// A reply to a collection slot the interrupt kept the walk from, here a
/// builtin called directly as the fold's callback, is delivered again
/// the next cycle. The first cycle spins in `w` while the timers of `go`
/// and of the slot expire, so both replies land in the second, which
/// spins in `r`, before the fold, until the interrupt.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupted_slot_reply_is_delivered_again() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    let mut held = Vec::new();
    for stmt in [
        "let rec spin = |n: i64| -> i64 select n < 0 { true => n, false => spin(n + 1) }",
        "let go = sys::time::after_idle(duration:50.ms, 0); let r = spin(go); \
         let v = array::fold([duration:1.s], duration:50.ms, sys::time::after_idle); \
         let w = spin(0)",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(stmt)).await?);
    }
    for _ in 0..2 {
        wedged(&ctx, Duration::from_millis(100)).await?;
        tokio::time::sleep(Duration::from_millis(200)).await;
        ctx.rt.interrupt();
    }
    held.push(ctx.rt.compile(ArcStr::from("v")).await?);
    let v = held.last().unwrap().exprs[0].id;
    let got = value_of(&mut rx, v, Duration::from_secs(5)).await;
    drop(held);
    ctx.shutdown().await;
    match got {
        Some(netidx::publisher::Value::Duration(d)) if *d == Duration::from_secs(1) => {
            Ok(())
        }
        got => bail!("the fold's slot reply was lost at the interrupt: v is {got:?}"),
    }
}
