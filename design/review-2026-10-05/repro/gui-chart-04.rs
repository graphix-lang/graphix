//! gui-chart-04: mesh counts reach plotters as unchecked `i64 as usize`:
//! `x_light_lines: 0` panics a time-series chart's draw.
//! MeshStyle's label and light-line counts are `[i64, null]`
//! (stdlib/graphix-package-gui/src/widgets/chart/types.rs:288-294) and
//! stdlib/graphix-package-gui/src/widgets/chart/draw.rs:365-376 (2D) and
//! 865-882 (3D) hand them to plotters as `n as usize`.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_04.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_04 -- --nocapture
//!
//! Each case compiles `chart(..)`, builds the widget and draws it through a
//! headless wgpu renderer as `GuiHandler` does (`UserInterface::build` +
//! `draw`) under `catch_unwind`, and prints `OK` or `PANIC: <message>`. A
//! watchdog ends the process when a draw runs 15 s or grows RSS by 1 GiB, so
//! the last case (a hang) cannot take the machine down.
//!
//! Expected: every case draws (book/src/ui/gui/chart.md documents
//! `x_light_lines: 0` as "0 draws bold lines only").
//!
//! Observed at c722befe (test profile: overflow checks on):
//!   RESULT time series, no mesh counts (control): OK
//!   RESULT time series, #x_light_lines: 0: PANIC: attempt to exponentiate with overflow
//!   RESULT time series, #x_labels: 0: PANIC: attempt to exponentiate with overflow
//!   RESULT time series, #y_light_lines: 0: OK
//!   RESULT numeric, #x_light_lines: 0: OK
//!   RESULT numeric, #y_labels: -1: PANIC: attempt to multiply with overflow
//!     (plotters-0.3.7/src/chart/mesh.rs:469:51)
//!   RESULT bar of one category, #x_light_lines: 0: PANIC: assertion failed: step != 0
//!   RESULT bar of two categories, #x_light_lines: 0: OK
//!   RESULT 3d, #x_light_lines: 0: OK
//!   RESULT 3d, #x_labels: -1 (last: may not return): the draw has NOT returned
//!     after 15.0s; RSS 108 MiB -> 100 MiB (an endless loop, no allocation)
//!
//! The time-series panic is plotters' `compute_period_per_point(total_ns, 0)`:
//! `10u64.pow(inf as u32)`. Without overflow checks (release) the pow wraps to
//! 0 and the next `total_ns / actual_ns_per_point` panics "attempt to divide
//! by zero" (that function copied verbatim, built with -C overflow-checks=off).
//! A negative 2D count, wrapped, makes `compute_f64_key_points` allocate
//! without bound (same method: OOM-killed at a 2 GiB cap). A negative 3D label
//! count is `BoldPoints(usize::MAX)`, whose `npoints as usize > max_points`
//! exit can never hold. The GUI draws on the main thread with no
//! `catch_unwind`, so a panic ends the program.

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
    panic::{AssertUnwindSafe, catch_unwind},
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
const TIME_LIMIT: Duration = Duration::from_secs(15);

const TS_DATA: &str = "[(datetime:\"2026-01-01T00:00:00Z\", 1.0), \
                       (datetime:\"2026-01-01T01:00:00Z\", 2.0)]";
const NUM_DATA: &str = "[(0.0, 0.0), (5.0, 5.0)]";
const XYZ_DATA: &str = "[(0.0, 0.0, 0.0), (1.0, 2.0, 3.0)]";

struct Gpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

async fn gpu() -> Gpu {
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
    Gpu { adapter, device, queue }
}

fn renderer(g: &Gpu) -> widgets::Renderer {
    let engine = iced_wgpu::Engine::new(
        &g.adapter,
        g.device.clone(),
        g.queue.clone(),
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

async fn chart(data: &str, mesh: &str, dataset: &str) -> Result<Chart> {
    let code = format!(
        "use gui::chart::{{chart, chart_style, mesh_style, line, bar, scatter3d}};\n\
         let data = {data};\n\
         let style = chart_style(#mesh: mesh_style({mesh}));\n\
         let result = chart(#width: &`Fill, #height: &`Fixed(300.0), \
           #style: &style, &[{dataset}(&data)])"
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

fn panic_message(p: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = p.downcast_ref::<&str>() {
        s.to_string()
    } else if let Some(s) = p.downcast_ref::<String>() {
        s.clone()
    } else {
        "<non-string panic payload>".into()
    }
}

fn watched_draw(what: &str, widget: &GuiW<NoExt>, r: &mut widgets::Renderer) {
    let base = rss();
    let done = Arc::new(AtomicBool::new(false));
    let start = Instant::now();
    let watchdog = {
        let done = done.clone();
        let what = what.to_string();
        std::thread::spawn(move || {
            while !done.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(20));
                let now = rss();
                if now.saturating_sub(base) >= RSS_LIMIT || start.elapsed() >= TIME_LIMIT
                {
                    eprintln!(
                        "RESULT {what}: the draw has NOT returned after {:.1}s; \
                         RSS {} MiB -> {} MiB; ending the process",
                        start.elapsed().as_secs_f64(),
                        base >> 20,
                        now >> 20
                    );
                    std::process::exit(3);
                }
            }
        })
    };
    let res = catch_unwind(AssertUnwindSafe(|| draw(widget, r)));
    done.store(true, Ordering::SeqCst);
    let _ = watchdog.join();
    match res {
        Ok(()) => eprintln!(
            "RESULT {what}: OK (draw returned in {:.3}s)",
            start.elapsed().as_secs_f64()
        ),
        Err(p) => eprintln!("RESULT {what}: PANIC: {}", panic_message(&*p)),
    }
}

#[tokio::test(flavor = "current_thread")]
async fn mesh_counts_draw() -> Result<()> {
    let g = gpu().await;
    let cases: &[(&str, &str, &str, &str)] = &[
        ("time series, no mesh counts (control)", TS_DATA, "", "line"),
        ("time series, #x_light_lines: 0", TS_DATA, "#x_light_lines: 0", "line"),
        ("time series, #x_labels: 0", TS_DATA, "#x_labels: 0", "line"),
        ("time series, #y_light_lines: 0", TS_DATA, "#y_light_lines: 0", "line"),
        ("numeric, #x_light_lines: 0", NUM_DATA, "#x_light_lines: 0", "line"),
        ("numeric, #y_labels: -1", NUM_DATA, "#y_labels: -1", "line"),
        ("bar of one category, #x_light_lines: 0", "[(\"a\", 1.0)]", "#x_light_lines: 0", "bar"),
        ("bar of two categories, #x_light_lines: 0", "[(\"a\", 1.0), (\"b\", 2.0)]", "#x_light_lines: 0", "bar"),
        ("3d, #x_light_lines: 0", XYZ_DATA, "#x_light_lines: 0", "scatter3d"),
        ("3d, #x_labels: -1 (last: may not return)", XYZ_DATA, "#x_labels: -1", "scatter3d"),
    ];
    for (what, data, mesh, dataset) in cases {
        let c = chart(data, mesh, dataset).await?;
        let mut r = renderer(&g);
        watched_draw(what, &c.widget, &mut r);
    }
    Ok(())
}
