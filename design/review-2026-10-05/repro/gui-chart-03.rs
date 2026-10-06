//! gui-chart-03: a pie whose values sum to zero loops forever in plotters'
//! `Pie::draw` and grows memory until the process dies. `ChartMode::Pie` in
//! stdlib/graphix-package-gui/src/widgets/chart/draw.rs (line 747 on) hands
//! the raw values to plotters 0.3.7's `Pie`, whose `draw` computes
//! `ratio = slice / total`; with `total == 0.0` the first positive slice has
//! `ratio == inf`, so `while offset_theta <= theta_final { points.push(..);
//! offset_theta += radian_increment }` never ends.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_03.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_03 -- --nocapture
//!
//! The test compiles `chart(&[pie(&data)])`, builds the widget and draws it
//! through a headless wgpu renderer as `GuiHandler::about_to_wait` does
//! (`UserInterface::build` + `ui.draw`, event_loop.rs:321-347), first a
//! control pie, then a pie of signed flows `[("in", 100.0), ("out", -100.0)]`.
//! A watchdog thread samples the process RSS and aborts the process once the
//! draw has grown it by 1 GiB or run 60 s, so the probe cannot take the
//! machine down. GC03_DATA / GC03_ARGS replace the second case.
//!
//! Expected: both draws return (a pie with no positive total draws nothing).
//! Observed at c722befe (dev profile), the default case:
//!   control [(a, 60), (b, 40)]: draw returned in 0.020s, RSS 88 MiB -> 90 MiB
//!     case pie(&[("in", 100.0), ("out", -100.0)]): draw running 0.5s, RSS 392 MiB (+291 MiB)
//!     case pie(&[("in", 100.0), ("out", -100.0)]): draw running 1.0s, RSS 662 MiB (+561 MiB)
//!     case pie(&[("in", 100.0), ("out", -100.0)]): draw running 1.5s, RSS 868 MiB (+767 MiB)
//!     case pie(&[("in", 100.0), ("out", -100.0)]): draw running 2.0s, RSS 1110 MiB (+1009 MiB)
//!   case pie(&[("in", 100.0), ("out", -100.0)]): the draw has NOT returned after
//!     2.1s and grew RSS from 100 MiB to 1125 MiB; aborting
//!   process didn't exit successfully: ... (signal: 6, SIGABRT: process abort signal)
//! Other cases, prefixed to the same command:
//!   GC03_DATA='[("a", 1.0), ("b", -0.999999)]'   not returned after 2.8s, +1 GiB
//!     (at radius 105 the loop would run 1.8e9 times: ~15 GB of points)
//!   GC03_DATA='[("a", 60.0), ("b", 40.0)]' GC03_ARGS='#start_angle: 1.0 / z, '
//!     not returned after 1.0s, +1 GiB (an infinite start angle hangs the
//!     same loop with ordinary data)
//!   GC03_DATA='[("out", -100.0), ("in", 100.0)]'  returned in 0.000s (the
//!     first slice's ratio is -inf, every later angle is NaN, nothing loops)
//! In the shell the draw runs on the GUI's main thread: the window freezes
//! and memory grows until the allocator or the OOM killer ends the process.

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Size, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
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

const RSS_LIMIT: u64 = 1 << 30;
const TIME_LIMIT: Duration = Duration::from_secs(60);

async fn renderer() -> widgets::Renderer {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
        ..Default::default()
    });
    let adapter = match instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: None,
            force_fallback_adapter: false,
            ..Default::default()
        })
        .await
    {
        Ok(a) => a,
        Err(_) => instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                compatible_surface: None,
                force_fallback_adapter: true,
                ..Default::default()
            })
            .await
            .expect("no GPU adapter available"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("failed to create GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

struct Chart {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    widget: GuiW<NoExt>,
}

async fn pie_chart(data: &str, args: &str) -> Result<Chart> {
    let code = format!(
        "use gui::chart::{{chart, pie}};\n\
         let z = 0.0;\n\
         let data = {data};\n\
         let result = chart(&[pie({args}&data)])"
    );
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
        .await?;
    let compiled = ctx
        .rt
        .compile(arcstr::literal!("{ mod test; test::result }"))
        .await
        .context("compile graphix code")?;
    let id = compiled.exprs[0].id;
    let root = loop {
        let mut batch = tokio::time::timeout(Duration::from_secs(5), rx.recv())
            .await
            .context("timeout waiting for the widget value")?
            .context("event channel closed")?;
        let found = batch.drain(..).find_map(|e| match e {
            GXEvent::Updated(i, v) if i == id => Some(v),
            _ => None,
        });
        if let Some(v) = found {
            break v;
        }
    };
    let mut widget =
        widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
    let rt = tokio::runtime::Handle::current();
    while let Ok(Some(mut batch)) =
        tokio::time::timeout(Duration::from_millis(200), rx.recv()).await
    {
        for e in batch.drain(..) {
            if let GXEvent::Updated(i, v) = e {
                widget.handle_update(&rt, i, &v)?;
            }
        }
    }
    Ok(Chart { _ctx: ctx, _compiled: compiled, widget })
}

fn draw(widget: &GuiW<NoExt>, renderer: &mut widgets::Renderer) {
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let mut ui = UserInterface::build(
        widget.view(),
        Size::new(400.0, 300.0),
        Cache::default(),
        renderer,
    );
    ui.draw(renderer, &theme, &style, mouse::Cursor::Unavailable);
}

fn rss() -> u64 {
    let statm = std::fs::read_to_string("/proc/self/statm").unwrap_or_default();
    let pages: u64 =
        statm.split_whitespace().nth(1).and_then(|s| s.parse().ok()).unwrap_or(0);
    pages * 4096
}

fn mib(b: u64) -> u64 {
    b >> 20
}

/// Draws on this thread while a watchdog samples RSS; the watchdog aborts
/// the process when the draw outgrows `RSS_LIMIT` or `TIME_LIMIT`.
fn watched_draw(what: &'static str, widget: &GuiW<NoExt>, r: &mut widgets::Renderer) {
    let base = rss();
    let done = Arc::new(AtomicBool::new(false));
    let start = Instant::now();
    let watchdog = {
        let done = done.clone();
        std::thread::spawn(move || {
            let mut last_report = Instant::now();
            while !done.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(20));
                let now = rss();
                let grown = now.saturating_sub(base);
                if last_report.elapsed() >= Duration::from_millis(500) {
                    eprintln!(
                        "  {what}: draw running {:.1}s, RSS {} MiB (+{} MiB)",
                        start.elapsed().as_secs_f64(),
                        mib(now),
                        mib(grown)
                    );
                    last_report = Instant::now();
                }
                if grown >= RSS_LIMIT || start.elapsed() >= TIME_LIMIT {
                    eprintln!(
                        "{what}: the draw has NOT returned after {:.1}s and grew \
                         RSS from {} MiB to {} MiB; aborting",
                        start.elapsed().as_secs_f64(),
                        mib(base),
                        mib(now)
                    );
                    std::process::abort();
                }
            }
        })
    };
    draw(widget, r);
    done.store(true, Ordering::SeqCst);
    let _ = watchdog.join();
    eprintln!(
        "{what}: draw returned in {:.3}s, RSS {} MiB -> {} MiB",
        start.elapsed().as_secs_f64(),
        mib(base),
        mib(rss())
    );
}

/// GC03_DATA (a Graphix array literal) and GC03_ARGS (labeled pie
/// arguments, each followed by a comma; `z` is 0.0) replace the zero-sum
/// case when set.
#[tokio::test(flavor = "current_thread")]
async fn zero_sum_pie_draws() -> Result<()> {
    let mut r = renderer().await;
    let control = pie_chart(r#"[("a", 60.0), ("b", 40.0)]"#, "").await?;
    watched_draw("control [(a, 60), (b, 40)]", &control.widget, &mut r);
    let data = std::env::var("GC03_DATA")
        .unwrap_or_else(|_| r#"[("in", 100.0), ("out", -100.0)]"#.into());
    let args = std::env::var("GC03_ARGS").unwrap_or_default();
    let what: &'static str = format!("case pie({args}&{data})").leak();
    let case = pie_chart(&data, &args).await?;
    watched_draw(what, &case.widget, &mut r);
    Ok(())
}
