//! gx-ui-05: gui chart: a datetime #x_range is decoded as a numeric range
//! and silently ignored.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gx_ui_05.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gx_ui_05 -- --nocapture
//!
//! The program builds three charts: a time series with
//! `#x_range: &{min: 2024-01-05, max: 2024-01-06}`, the same series with
//! no range, and a numeric series with `#x_range: &{min: 2.0, max: 3.0}`
//! (the control). Each is built by `widgets::compile`, laid out and drawn
//! once with a headless wgpu renderer; the x range the chart drew over is
//! read back from `ChartState::plot_info` (milliseconds for a time
//! series). The range value itself is also decoded with
//! `OptXAxisRange::from_value`, as `TRef::new` does.
//!
//! Expected: the range decodes as `XAxisRange::DateTime` and the time
//! series is drawn over [2024-01-05, 2024-01-06] (book: "Manual x-axis
//! range as {min: f64, max: f64} or {min: datetime, max: datetime}").
//! Observed at c722befe:
//!   range value: {max: DateTime(2024-01-06T00:00:00Z),
//!                 min: DateTime(2024-01-05T00:00:00Z)}
//!   OptXAxisRange::from_value: Numeric { min: 1704412800, max: 1704499200 }
//!   time series, #x_range 2024-01-05..06: drew x over
//!     Some((1703985120000.0, 1705790880000.0))
//!   time series, no #x_range:            drew x over
//!     Some((1703985120000.0, 1705790880000.0))
//!   requested range in ms:               (1704412800000.0, 1704499200000.0)
//!   numeric control, #x_range 2..3:      drew x over Some((2.0, 3.0))
//! and the test fails on "datetime #x_range decoded as Numeric { .. }".
//! `f64::from_value` casts a DateTime to epoch seconds, so the numeric
//! decode at types.rs:374 always wins and the TimeSeries draw
//! (draw.rs:534-537), which honours only `XAxisRange::DateTime`, falls
//! back to the data's automatic range.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{
        self, GuiW,
        chart::{ChartState, OptXAxisRange, XAxisRange},
    },
};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{
    Color, Font, Layout, Pixels, Rectangle, Size, layout, mouse, renderer::Style,
    widget::Tree,
};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::{FromValue, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CODE: &str = r#"use gui::chart::{chart, line};
let ts = [
  (datetime:"2024-01-01T00:00:00Z", 1.0),
  (datetime:"2024-01-10T00:00:00Z", 5.0),
  (datetime:"2024-01-20T00:00:00Z", 3.0)
];
let range = {min: datetime:"2024-01-05T00:00:00Z", max: datetime:"2024-01-06T00:00:00Z"};
let result = (
  range,
  chart(#x_range: &range, #width: &`Fixed(600.0), #height: &`Fixed(300.0), &[line(&ts)]),
  chart(#width: &`Fixed(600.0), #height: &`Fixed(300.0), &[line(&ts)]),
  chart(
    #x_range: &{min: 2.0, max: 3.0},
    #width: &`Fixed(600.0),
    #height: &`Fixed(300.0),
    &[line(&[(0.0, 1.0), (10.0, 2.0)])]
  )
)"#;

type Rx = mpsc::Receiver<GPooled<Vec<GXEvent>>>;

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
            .expect("no GPU adapter"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, Font::DEFAULT, Pixels(16.0))
}

async fn first_value(rx: &mut Rx, target: ExprId) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev && id == target {
                        return Ok(v);
                    }
                }
            }
            _ = &mut timeout => bail!("no widget value"),
        }
    }
}

/// Draw the chart once at 600x300 and return the x range it drew over.
fn drawn_x_range(
    w: &GuiW<NoExt>,
    renderer: &mut widgets::Renderer,
    theme: &GraphixTheme,
) -> Option<(f64, f64)> {
    let mut el = w.view();
    let mut tree = Tree::new(&el);
    let size = Size::new(600.0, 300.0);
    let limits = layout::Limits::new(Size::ZERO, size);
    let node = el.as_widget_mut().layout(&mut tree, &*renderer, &limits);
    el.as_widget().draw(
        &tree,
        renderer,
        theme,
        &Style { text_color: Color::BLACK },
        Layout::new(&node),
        mouse::Cursor::Unavailable,
        &Rectangle::with_size(size),
    );
    tree.state.downcast_ref::<ChartState>().plot_info.get().map(|i| i.x_range)
}

fn describe(r: &OptXAxisRange) -> String {
    match &r.0 {
        None => "None".into(),
        Some(XAxisRange::Numeric { min, max }) => {
            format!("Numeric {{ min: {min}, max: {max} }}")
        }
        Some(XAxisRange::DateTime { min, max }) => {
            format!("DateTime {{ min: {min}, max: {max} }}")
        }
    }
}

fn ms(s: &str) -> f64 {
    chrono::DateTime::parse_from_rfc3339(s).unwrap().timestamp_millis() as f64
}

#[tokio::test(flavor = "current_thread")]
async fn datetime_x_range_is_honoured() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(CODE)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
    let parts: Vec<Value> = match first_value(&mut rx, compiled.exprs[0].id).await? {
        Value::Array(a) => a.iter().cloned().collect(),
        v => bail!("expected a tuple, got {v}"),
    };
    let range_v = parts[0].clone();
    eprintln!("range value: {range_v:?}");
    let decoded = OptXAxisRange::from_value(range_v).context("decode range")?;
    eprintln!("OptXAxisRange::from_value: {}", describe(&decoded));

    let mut ws: Vec<GuiW<NoExt>> = Vec::new();
    for p in parts[1..].iter().cloned() {
        ws.push(widgets::compile(gx.clone(), p).await.context("widget")?);
    }
    let mut renderer = renderer().await;
    let theme = GraphixTheme { inner: iced_core::Theme::Light, overrides: None };
    let with_range = drawn_x_range(&ws[0], &mut renderer, &theme);
    let no_range = drawn_x_range(&ws[1], &mut renderer, &theme);
    let numeric = drawn_x_range(&ws[2], &mut renderer, &theme);
    let want = (ms("2024-01-05T00:00:00Z"), ms("2024-01-06T00:00:00Z"));
    eprintln!("time series, #x_range 2024-01-05..06: drew x over {with_range:?}");
    eprintln!("time series, no #x_range:            drew x over {no_range:?}");
    eprintln!("requested range in ms:               {want:?}");
    eprintln!("numeric control, #x_range 2..3:      drew x over {numeric:?}");

    assert_eq!(numeric, Some((2.0, 3.0)), "control: a numeric #x_range is honoured");
    assert!(no_range.is_some(), "control: the time series was drawn");
    assert!(
        matches!(decoded.0, Some(XAxisRange::DateTime { .. })),
        "datetime #x_range decoded as {}",
        describe(&decoded)
    );
    assert_eq!(with_range, Some(want), "datetime #x_range ignored");
    Ok(())
}
