//! gui-chart-06: a datetime `#x_range` decodes as numeric seconds, so a
//! time-series chart ignores it.
//!
//! `OptXAxisRange::from_value` (stdlib/graphix-package-gui/src/widgets/chart/
//! types.rs:369-385) tries `cast_to::<AxisRange>()` first, and netidx's f64
//! `FromValue` casts a `Value::DateTime` to seconds since the epoch, so
//! `{min: datetime, max: datetime}` always becomes `XAxisRange::Numeric`.
//! `ChartMode::TimeSeries` (draw.rs:534-538) honours only
//! `XAxisRange::DateTime` and falls back to the auto range.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_06.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_06 -- --nocapture
//!
//! Each case compiles `chart(..)`, builds the widget with `widgets::compile`,
//! lays it out at 400x300 and draws it through a headless wgpu renderer, then
//! reads the x range the draw recorded in `ChartState::plot_info` (data units
//! in numeric mode, epoch milliseconds in time-series mode).
//!
//! Expected: the time-series chart with `#x_range` set to the first hour of a
//! one-day series plots (1767225600000, 1767229200000).
//! Observed at c722befe (test profile), the range is the auto range of the
//! whole (padded) day, exactly as with no `#x_range`; the test fails:
//!   decode {min: 2026-01-01 00:00:00 UTC, max: 2026-01-01 01:00:00 UTC}:
//!     Numeric { min: 1767225600, max: 1767229200 }
//!   control, numeric data, #x_range {min: 0.0, max: 100.0}: plotted x (0.0, 100.0)
//!   one-day series, no #x_range: plotted x (ms) (1767221280000.0, 1767316320000.0)
//!   one-day series, #x_range = first hour: plotted x (ms)
//!     (1767221280000.0, 1767316320000.0), want (1767225600000.0, 1767229200000.0)
//!   assertion `left == right` failed: the datetime #x_range was not applied

use ahash::AHashMap;
use anyhow::{Context, Result};
use chrono::{TimeZone, Utc};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{
        self, GuiW,
        chart::{ChartState, OptXAxisRange, XAxisRange},
    },
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Point, Rectangle, Size,
    layout::{Layout, Limits},
    mouse,
    renderer::Style,
    widget::Tree,
};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx_value::{FromValue, ValArray, Value};
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

async fn chart(code: &str) -> Result<Chart> {
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

/// Lays the chart out at 400x300, draws it, and returns the x range the
/// draw recorded.
fn plotted_x_range(widget: &GuiW<NoExt>, renderer: &mut widgets::Renderer) -> (f64, f64) {
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let size = Size::new(400.0, 300.0);
    let mut element = widget.view();
    let mut tree = Tree::new(element.as_widget());
    let node =
        element.as_widget_mut().layout(&mut tree, &*renderer, &Limits::new(Size::ZERO, size));
    element.as_widget().draw(
        &tree,
        renderer,
        &theme,
        &style,
        Layout::new(&node),
        mouse::Cursor::Unavailable,
        &Rectangle::new(Point::ORIGIN, size),
    );
    let state = tree.state.downcast_ref::<ChartState>();
    state.plot_info.get().expect("the draw records the plot area").x_range
}

const SERIES: &str = r#"use gui::chart::{chart, line};
let data = [
  (datetime:"2026-01-01T00:00:00Z", 1.0),
  (datetime:"2026-01-01T12:00:00Z", 3.0),
  (datetime:"2026-01-02T00:00:00Z", 2.0)
];
let result = chart(ARGS&[line(&data)])"#;

const FIRST_HOUR: &str = r#"#x_range: &{min: datetime:"2026-01-01T00:00:00Z", max: datetime:"2026-01-01T01:00:00Z"}, "#;

fn describe(r: &OptXAxisRange) -> String {
    match &r.0 {
        None => "None".into(),
        Some(XAxisRange::Numeric { min, max }) => format!("Numeric {{ min: {min}, max: {max} }}"),
        Some(XAxisRange::DateTime { min, max }) => format!("DateTime {{ min: {min}, max: {max} }}"),
    }
}

#[tokio::test(flavor = "current_thread")]
async fn datetime_x_range_is_honoured() -> Result<()> {
    let t0 = Utc.with_ymd_and_hms(2026, 1, 1, 0, 0, 0).unwrap();
    let t1 = Utc.with_ymd_and_hms(2026, 1, 1, 1, 0, 0).unwrap();
    let field = |name: &'static str, v: Value| {
        Value::Array(ValArray::from_iter_exact([Value::String(name.into()), v].into_iter()))
    };
    let decoded = OptXAxisRange::from_value(Value::Array(ValArray::from_iter_exact(
        [field("max", Value::from(t1)), field("min", Value::from(t0))].into_iter(),
    )))?;
    eprintln!("decode {{min: {t0}, max: {t1}}}: {}", describe(&decoded));

    let mut r = renderer().await;
    let numeric = chart(
        "use gui::chart::{chart, line};\n\
         let result = chart(#x_range: &{min: 0.0, max: 100.0}, &[line(&[(1.0, 1.0), (2.0, 2.0)])])",
    )
    .await?;
    eprintln!(
        "control, numeric data, #x_range {{min: 0.0, max: 100.0}}: plotted x {:?}",
        plotted_x_range(&numeric.widget, &mut r)
    );
    let auto = chart(&SERIES.replace("ARGS", "")).await?;
    let auto_x = plotted_x_range(&auto.widget, &mut r);
    eprintln!("one-day series, no #x_range: plotted x (ms) {auto_x:?}");
    let ranged = chart(&SERIES.replace("ARGS", FIRST_HOUR)).await?;
    let ranged_x = plotted_x_range(&ranged.widget, &mut r);
    let want = (t0.timestamp_millis() as f64, t1.timestamp_millis() as f64);
    eprintln!(
        "one-day series, #x_range = first hour: plotted x (ms) {ranged_x:?}, want {want:?}"
    );
    assert_eq!(ranged_x, want, "the datetime #x_range was not applied");
    Ok(())
}
