//! tests-ui.r2-09: a large sparkline `history_seconds` panics the cutoff
//! math (widget compile on the GUI thread; the table's dispatch task).
//!
//! data_table/types.rs:235 accepts `history_seconds` whenever it is finite
//! and > 0. Both history pushes compute `now - Duration::from_secs_f64(hs)`
//! (subscriptions.rs:207 in the dispatch task, :665 at widget compile and on
//! a source update). `from_secs_f64` panics for hs >= 2^64 s on every
//! platform; the `Instant` subtraction panics for hs > ~9.2e18 s on Linux
//! (std's Windows `Instant` carries a `u64::MAX / 4` ns epoch offset, so
//! there it panics past ~4.6e9 s, about 146 years).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_tests_ui_r2_09.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_tests_ui_r2_09 -- --nocapture
//!
//! Each case compiles a `data_table` as the GUI does (`widgets::compile`)
//! and feeds it every runtime update (`handle_update`).
//!
//! EXPECTED: every case compiles, no panic, and both dispatch cases log
//! load=1, name=1, load=2, name=2 through on_update (an out-of-range
//! history is clamped or replaced as a negative one is).
//!
//! OBSERVED (HEAD c722befe, Linux):
//!   dispatch history_seconds= 60.0: on_update log [name=1, load=1,
//!     load=2, name=2]; panics []
//!   dispatch history_seconds= 1e20: on_update log []; panics
//!     ["[thread tokio-rt-worker] cannot convert float seconds to Duration:
//!     value is either too big or NaN"]   (the dispatch task died on the
//!     first value: no column of the table, the plain "name" one included,
//!     ever updates again, though `c` moved to 2 at 2000 ms)
//!   compile  history_seconds= 60.0: compiled
//!   compile  history_seconds= 1e20: PANICKED: cannot convert float seconds
//!     to Duration: value is either too big or NaN
//!   compile  history_seconds= 1e19: PANICKED at
//!     data_table/subscriptions.rs:665:30: overflow when subtracting
//!     duration from instant
//!   test sparkline_history_seconds_large_values ... FAILED

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use futures::FutureExt;
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
    panic::AssertUnwindSafe,
    sync::{Mutex, Once},
    time::Duration,
};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const COMPILE_CASE: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0"], columns: [
        "anchor",
        { name: "spark", typ: `Sparkline({ history_seconds: HS, min: null, max: null }),
            display_name: null,
            source: &"7.5",
            on_resize: &null, width: &null }
    ] };
let result = data_table(#table: &tbl)
"#;

const DISPATCH_CASE: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let log = "";
let c = 1;
c <- time::timer(duration:2000.ms, false) ~ 2;
sys::net::publish("/local/PFX/r0/load", c);
sys::net::publish("/local/PFX/r0/name", c);
let tbl = { rows: ["/local/PFX/r0"], columns: [
        "name",
        { name: "load", typ: `Sparkline({ history_seconds: HS, min: null, max: null }),
            display_name: null,
            source: &`Netidx(null),
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #table: &tbl
)
"#;

static PANICS: Mutex<Vec<String>> = Mutex::new(Vec::new());

fn install_panic_log() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let prev = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let p = info.payload();
            let msg = p
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| p.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "<non-string payload>".into());
            let th = std::thread::current().name().unwrap_or("<unnamed>").to_string();
            PANICS.lock().unwrap().push(format!("[thread {th}] {msg}"));
            prev(info);
        }));
    });
}

fn panic_count() -> usize {
    PANICS.lock().unwrap().len()
}

fn panics_since(n: usize) -> Vec<String> {
    PANICS.lock().unwrap()[n..].to_vec()
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

/// A running program; `compiled` must outlive the widget, dropping it
/// deletes the program's nodes.
struct Prog {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    root: Value,
    compiled: CompRes<NoExt>,
}

async fn load(code: &str) -> Result<Prog> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled: CompRes<NoExt> = gx
        .compile(arcstr::literal!("{ mod test; test::result }"))
        .await
        .context("compile")?;
    let root_id = compiled.exprs[0].id;
    let timeout = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(timeout);
    let root = loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    break v;
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the root value"),
        }
    };
    Ok(Prog { _ctx: ctx, gx, rx, root, compiled })
}

/// `widgets::compile`, a panic caught and returned as its message.
async fn compile_widget(p: &Prog) -> Result<std::result::Result<GuiW<NoExt>, String>> {
    let n = panic_count();
    match AssertUnwindSafe(widgets::compile(p.gx.clone(), p.root.clone()))
        .catch_unwind()
        .await
    {
        Ok(r) => Ok(Ok(r.context("compile widget")?)),
        Err(_) => Ok(Err(panics_since(n).join(" | "))),
    }
}

async fn compile_case(hs: &str) -> Result<Option<String>> {
    let p = load(&COMPILE_CASE.replace("HS", hs)).await?;
    Ok(compile_widget(&p).await?.err())
}

/// Compile the dispatch table and deliver runtime updates to it for 4 s;
/// returns every value `log` took and the panics seen meanwhile.
async fn dispatch_case(pfx: &str, hs: &str) -> Result<(Vec<String>, Vec<String>)> {
    let n = panic_count();
    let mut p = load(&DISPATCH_CASE.replace("PFX", pfx).replace("HS", hs)).await?;
    let log_ref = p.gx.compile_ref(find_bind_id(&p.compiled.env, "test::log")?).await?;
    let mut w = match compile_widget(&p).await? {
        Ok(w) => w,
        Err(msg) => bail!("dispatch case widget compile panicked: {msg}"),
    };
    let mut log = vec![];
    let rt = tokio::runtime::Handle::current();
    let deadline = tokio::time::Instant::now() + Duration::from_secs(4);
    while tokio::time::Instant::now() < deadline {
        let tick = tokio::time::sleep(Duration::from_millis(20));
        tokio::select! {
            biased;
            Some(mut batch) = p.rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == log_ref.id {
                            log.push(format!("{v}"));
                        }
                        tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                    }
                }
            }
            _ = tick => {}
        }
        w.before_view();
    }
    Ok((log, panics_since(n)))
}

#[tokio::test(flavor = "multi_thread")]
async fn sparkline_history_seconds_large_values() -> Result<()> {
    install_panic_log();
    let mut failures = vec![];
    for (pfx, hs) in [("r209a", "60.0"), ("r209b", "1e20")] {
        let (log, panics) = dispatch_case(pfx, hs).await?;
        println!("dispatch history_seconds={hs:>5}: on_update log {log:?}; panics {panics:?}");
        let want = [
            format!("\"/local/{pfx}/r0/load=1\""),
            format!("\"/local/{pfx}/r0/name=1\""),
            format!("\"/local/{pfx}/r0/load=2\""),
            format!("\"/local/{pfx}/r0/name=2\""),
        ];
        let missing: Vec<&String> = want.iter().filter(|w| !log.contains(w)).collect();
        if !panics.is_empty() || !missing.is_empty() {
            failures.push(format!(
                "dispatch {hs}: panics {panics:?}, on_update never logged {missing:?}"
            ));
        }
    }
    for hs in ["60.0", "1e20", "1e19"] {
        let r = compile_case(hs).await?;
        println!(
            "compile  history_seconds={hs:>5}: {}",
            match &r {
                None => "compiled".to_string(),
                Some(m) => format!("PANICKED: {m}"),
            }
        );
        if let Some(m) = r {
            failures.push(format!("compile {hs}: {m}"));
        }
    }
    assert!(failures.is_empty(), "history_seconds failures:\n{}", failures.join("\n"));
    Ok(())
}
