//! gui-core-10: OS resize is written to the reference's mirror cell, so
//! `#size: &sz` never sees it.
//!
//! `ToGui::ResizeSettled` (stdlib/graphix-package-gui/src/event_loop.rs:252)
//! writes with `TRef::set` -> `Ref::set(self.bid)` (graphix-rt/src/lib.rs:
//! 175-179): into the cell the `&` minted, not into what the reference
//! names. `&sz`'s cell only mirrors `sz` (bind.rs ByRef::publish), and
//! `*r` reads `sz` through `env.byref_chain` (bind.rs Deref::address), so
//! neither `sz` nor `*w.size` moves. A place (`&st.size`) is lost the same
//! way; only a chainless reference (the default `&{..}` literal) keeps it.
//!
//! No window, no GPU: the window is resolved exactly as
//! `reconcile_windows` resolves it (compile_ref on the root array's
//! element, `ResolvedWindow::compile` on its value), and the resize is
//! the body of `ToGui::ResizeSettled` (event_loop.rs:245-258) on that
//! window's `size` TRef.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_10.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_10 -- --nocapture
//!
//! Expected: after the settled resize to 1024 x 768, `sz`, `*w.size`, the
//! label "[sz.width] x [sz.height]", `*p.size` and `st.size` all read
//! 1024 x 768 (the TUI's size references reach the variable: they write
//! with `set_deref`, tui/src/block.rs:343), and the test passes.
//! Observed at c722befe (dev profile; the test FAILS):
//!   RESULT after ResizeSettled 1024x768:
//!     sz     = 800 x 600
//!     label  = Some(String("800 x 600"))
//!     seen   = 800 x 600            (*w.size, w = window(#size: &sz, ..))
//!     dseen  = 1024 x 768           (*dflt.size, default &{..} literal)
//!     pseen  = 800 x 600            (*p.size, p = window(#size: &st.size, ..))
//!     pst    = 800 x 600            (st.size)
//!     gui w.size: chain target yes, t = Some("1024 x 768")
//!     gui dflt.size: chain target none, t = Some("1024 x 768")
//!     gui p.size: chain target none, t = Some("1024 x 768")
//!   RESULT after set_deref 640x480 (w and p):
//!     sz = seen = 640 x 480, label = "640 x 480"   (the chain reaches sz)
//!     pseen = pst = 800 x 600   (set_deref has no target for a place)
//!   Error: after the OS resize to 1024 x 768: sz = Some((800.0, 600.0)),
//!     *w.size = Some((800.0, 600.0)), *p.size = Some((800.0, 600.0)),
//!     st.size = Some((800.0, 600.0)), label = Some(String("800 x 600"))
//!     (unchanged)

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef};
use graphix_package_gui::{types::SizeV, window::ResolvedWindow};
use graphix_rt::{GXEvent, GXHandle, NoExt, Ref};
use iced_core::Size;
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const PROG: &str = r#"
use gui::{window, text::text};
let sz = {width: 800.0, height: 600.0};
let label = "[sz.width] x [sz.height]";
let w = window(#size: &sz, &text(&label));
let seen = *w.size;
let dflt = window(&text(&"default"));
let dseen = *dflt.size;
let st = {n: 0, size: {width: 800.0, height: 600.0}};
let p = window(#size: &st.size, &text(&"place"));
let pseen = *p.size;
let pst = st.size;
let result = [&w, &dflt, &p]
"#;

type Rx = mpsc::Receiver<GPooled<Vec<GXEvent>>>;

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    let suffix = "/test";
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(suffix) {
            if let Some(bid) = vars.get(name) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding test::{name}")
}

struct Watch {
    name: &'static str,
    r: Ref<NoExt>,
    last: Option<Value>,
}

async fn drain(rx: &mut Rx, watches: &mut [Watch]) -> Result<()> {
    let timeout = tokio::time::sleep(Duration::from_millis(600));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e {
                        for w in watches.iter_mut() {
                            if w.r.id == id {
                                w.last = Some(v.clone());
                            }
                        }
                    }
                }
                timeout.as_mut().reset(
                    tokio::time::Instant::now() + Duration::from_millis(300),
                );
            }
            _ = &mut timeout => break,
        }
    }
    Ok(())
}

async fn root_value(rx: &mut Rx, root: ExprId) -> Result<Value> {
    loop {
        let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
            .await
            .context("timeout waiting for the root")?
            .context("runtime closed")?;
        let hit = batch.drain(..).find_map(|e| match e {
            GXEvent::Updated(id, v) if id == root => Some(v),
            _ => None,
        });
        if let Some(v) = hit {
            return Ok(v);
        }
    }
}

/// `reconcile_windows` for one element of the root array.
async fn resolve(gx: &GXHandle<NoExt>, bid: u64) -> Result<(Ref<NoExt>, ResolvedWindow<NoExt>)> {
    let wref = gx.compile_ref(BindId::from(bid)).await?;
    let v = wref.last.clone().context("window has no value")?;
    let resolved = ResolvedWindow::compile(gx.clone(), v).await?;
    Ok((wref, resolved))
}

/// The body of `ToGui::ResizeSettled` on one window.
fn resize_settled(rw: &mut ResolvedWindow<NoExt>, sz: SizeV) -> Result<()> {
    if rw.size.t.as_ref() != Some(&sz) {
        rw.size.set(sz)?;
    }
    Ok(())
}

fn show(watches: &[Watch], gui: &[(&str, &ResolvedWindow<NoExt>)], when: &str) {
    eprintln!("RESULT {when}:");
    for w in watches {
        match size_of(&w.last) {
            Some((wd, ht)) => eprintln!("  {:<6} = {wd} x {ht}", w.name),
            None => eprintln!("  {:<6} = {:?}", w.name, w.last),
        }
    }
    for (n, rw) in gui {
        let t = rw.size.t.map(|s| format!("{} x {}", s.0.width, s.0.height));
        eprintln!(
            "  gui {n}.size: chain target {}, t = {t:?}",
            if rw.size.r.target_bid.is_some() { "yes" } else { "none" },
        );
    }
}

fn size_of(v: &Option<Value>) -> Option<(f64, f64)> {
    #[derive(netidx_derive::FromValue)]
    struct S {
        width: f64,
        height: f64,
    }
    let s: S = v.clone()?.cast_to().ok()?;
    Some((s.width, s.height))
}

#[tokio::test(flavor = "multi_thread")]
async fn os_resize_reaches_the_size_variable() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(PROG)),
    )]);
    let ctx =
        testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let root = root_value(&mut rx, compiled.exprs[0].id).await?;
    let bids: Vec<u64> = root.cast_to().context("root array of bind ids")?;
    eprintln!("root = {bids:?}");
    let (_wref, mut win) = resolve(&gx, bids[0]).await?;
    let (_dref, mut dwin) = resolve(&gx, bids[1]).await?;
    let (_pref, mut pwin) = resolve(&gx, bids[2]).await?;
    let mut watches = vec![];
    for name in ["sz", "label", "seen", "dseen", "pseen", "pst"] {
        let r = gx.compile_ref(find_bind_id(&compiled.env, name)?).await?;
        let last = r.last.clone();
        watches.push(Watch { name, r, last });
    }
    drain(&mut rx, &mut watches).await?;
    show(&watches, &[("w", &win), ("dflt", &dwin), ("p", &pwin)], "before");
    let label_before = watches[1].last.clone();

    let target = SizeV(Size::new(1024.0, 768.0));
    resize_settled(&mut win, target)?;
    resize_settled(&mut dwin, target)?;
    resize_settled(&mut pwin, target)?;
    drain(&mut rx, &mut watches).await?;
    show(
        &watches,
        &[("w", &win), ("dflt", &dwin), ("p", &pwin)],
        "after ResizeSettled 1024x768",
    );
    let after_set: Vec<Option<(f64, f64)>> =
        watches.iter().map(|w| size_of(&w.last)).collect();
    let label_after_set = watches[1].last.clone();

    // contrast: the same write through the chain, as the TUI does
    win.size.set_deref(SizeV(Size::new(640.0, 480.0)))?;
    pwin.size.set_deref(SizeV(Size::new(640.0, 480.0)))?;
    drain(&mut rx, &mut watches).await?;
    show(
        &watches,
        &[("w", &win), ("dflt", &dwin), ("p", &pwin)],
        "after set_deref 640x480 (w and p)",
    );

    let want = Some((1024.0, 768.0));
    let mut bad = vec![];
    if after_set[0] != want {
        bad.push(format!("sz = {:?}", after_set[0]));
    }
    if after_set[2] != want {
        bad.push(format!("*w.size = {:?}", after_set[2]));
    }
    if after_set[3] != want {
        bad.push(format!("*dflt.size = {:?}", after_set[3]));
    }
    if after_set[4] != want {
        bad.push(format!("*p.size = {:?}", after_set[4]));
    }
    if after_set[5] != want {
        bad.push(format!("st.size = {:?}", after_set[5]));
    }
    if label_after_set == label_before {
        bad.push(format!("label = {:?} (unchanged)", label_after_set));
    }
    drop(ctx);
    if !bad.is_empty() {
        bail!("after the OS resize to 1024 x 768: {}", bad.join(", "));
    }
    Ok(())
}
