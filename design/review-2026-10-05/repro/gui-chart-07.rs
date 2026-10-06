//! gui-chart-07: the bar tooltip reads each dataset by the hovered slot's
//! position, not by its category.
//!
//! To run, copy this file to
//! stdlib/graphix-package-gui/tests/review_gui_chart_07.rs, then from the
//! repository root:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_07 -- --nocapture
//!
//! Each case compiles `chart(&[bar(..), ..])`, builds the widget, lays it
//! out at 600x400 and draws it through a headless wgpu renderer (the
//! canvas `draw` records `PlotInfo` as in the GUI), reads the drawn bars
//! back from a screenshot (each series has its own pure colour), then
//! sends a `CursorMoved` to the centre of every category slot through the
//! canvas widget's `update` and reads `ChartState::snap_point`, the
//! tooltip `draw` would show.
//!
//! Expected: over each slot the tooltip names that slot's category and the
//! value of a bar drawn there (the test passes).
//! Observed at c722befe (dev profile), the test FAILS, 4 slots wrong:
//!   == control, one series A=[a, b]
//!      slot 0 'a': drawn Red up to 1.0; tooltip "A: a: 1.00"
//!      slot 1 'b': drawn Red up to 2.0; tooltip "A: b: 2.00"
//!   == the finding's data, B=[b, c] then A=[a, b]
//!      categories ["b", "c", "a"]
//!      slot 0 'b': drawn Red up to 2.0, Blue up to 10.0; tooltip "B: b: 10.00"
//!      slot 1 'c': drawn Blue up to 20.0; tooltip "B: c: 20.00"
//!      slot 2 'a': drawn Red up to 1.0; tooltip NONE   <-- WRONG
//!   == A=[a] then B=[b, a]
//!      slot 0 'a': drawn Blue up to 3.0, Red up to 4.0; tooltip "A: a: 4.00"
//!      slot 1 'b': drawn Blue up to 2.0; tooltip "B: a: 3.00"   <-- WRONG
//!   == one series repeating a category, A=[a, a, b]
//!      slot 0 'a': drawn Red up to 3.0; tooltip "A: a: 1.00"   <-- WRONG
//!      slot 1 'b': drawn Red up to 5.0; tooltip "A: a: 2.00"   <-- WRONG
//!   Error: 4 hovered slots show a tooltip that does not describe their bar

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{
        self, GuiW,
        chart::{ChartState, PlotInfo},
    },
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Color, Event, Layout, Point, Rectangle, Shell, Size, clipboard, layout, mouse,
    renderer::Style, widget::Tree,
};
use iced_wgpu::{
    graphics::{Shell as GpuShell, Viewport},
    wgpu,
};
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

const W: u32 = 600;
const H: u32 = 400;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Paint {
    Red,
    Blue,
}

impl Paint {
    fn graphix(self) -> &'static str {
        match self {
            Paint::Red => "color(#r: 1.0)$",
            Paint::Blue => "color(#b: 1.0)$",
        }
    }

    fn of(px: &[u8]) -> Option<Paint> {
        match (px[0], px[1], px[2]) {
            (r, g, b) if r > 200 && g < 60 && b < 60 => Some(Paint::Red),
            (r, g, b) if b > 200 && r < 60 && g < 60 => Some(Paint::Blue),
            _ => None,
        }
    }
}

struct Series {
    label: &'static str,
    paint: Paint,
    data: &'static [(&'static str, f64)],
}

struct Case {
    name: &'static str,
    series: &'static [Series],
}

const CASES: &[Case] = &[
    Case {
        name: "control, one series A=[a, b]",
        series: &[Series { label: "A", paint: Paint::Red, data: &[("a", 1.0), ("b", 2.0)] }],
    },
    Case {
        name: "the finding's data, B=[b, c] then A=[a, b]",
        series: &[
            Series { label: "B", paint: Paint::Blue, data: &[("b", 10.0), ("c", 20.0)] },
            Series { label: "A", paint: Paint::Red, data: &[("a", 1.0), ("b", 2.0)] },
        ],
    },
    Case {
        name: "A=[a] then B=[b, a]",
        series: &[
            Series { label: "A", paint: Paint::Red, data: &[("a", 4.0)] },
            Series { label: "B", paint: Paint::Blue, data: &[("b", 2.0), ("a", 3.0)] },
        ],
    },
    Case {
        name: "one series repeating a category, A=[a, a, b]",
        series: &[Series {
            label: "A",
            paint: Paint::Red,
            data: &[("a", 1.0), ("a", 2.0), ("b", 5.0)],
        }],
    },
];

fn program(case: &Case) -> String {
    let mut code = String::from("use gui::{color, chart::{chart, bar}};\n");
    let mut sets = Vec::new();
    for (i, s) in case.series.iter().enumerate() {
        let data: Vec<String> =
            s.data.iter().map(|(c, v)| format!("(\"{c}\", {v:?})")).collect();
        code.push_str(&format!("let d{i} = [{}];\n", data.join(", ")));
        sets.push(format!(
            "bar(#label: \"{}\", #color: {}, &d{i})",
            s.label,
            s.paint.graphix()
        ));
    }
    code.push_str(&format!("let result = chart(&[{}])\n", sets.join(", ")));
    code
}

/// The x axis's categories, merged across series as draw.rs builds them.
fn categories(case: &Case) -> Vec<&'static str> {
    let mut cats: Vec<&'static str> = Vec::new();
    for s in case.series {
        for (c, _) in s.data {
            if !cats.contains(c) {
                cats.push(*c);
            }
        }
    }
    cats
}

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
        GpuShell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

struct Chart {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    widget: GuiW<NoExt>,
}

async fn build(code: &str) -> Result<Chart> {
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
        let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
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
        tokio::time::timeout(Duration::from_millis(300), rx.recv()).await
    {
        for e in batch.drain(..) {
            if let GXEvent::Updated(i, v) = e {
                tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
            }
        }
    }
    Ok(Chart { _ctx: ctx, _compiled: compiled, widget })
}

/// The colour runs drawn in the pixel column at `x`, bottom up from the
/// zero line, each with the data value at its top.
fn column(px: &[u8], x: f32, info: &PlotInfo) -> Vec<(Paint, f64)> {
    let (y_min, y_max) = info.y_range;
    let to_value =
        |py: f32| y_max - ((py - info.rect.y) / info.rect.height) as f64 * (y_max - y_min);
    let base = info.rect.y + (y_max / (y_max - y_min)) as f32 * info.rect.height;
    let xi = x.round() as u32;
    let mut runs: Vec<(Paint, f64)> = Vec::new();
    let mut py = base.floor() as i64 - 2;
    while py >= info.rect.y as i64 && py >= 0 && (py as u32) < H && xi < W {
        let i = ((py as u32 * W + xi) * 4) as usize;
        let Some(p) = Paint::of(&px[i..i + 4]) else { break };
        let v = to_value(py as f32);
        match runs.last_mut() {
            Some((q, top)) if *q == p => *top = v,
            _ => runs.push((p, v)),
        }
        py -= 1;
    }
    runs
}

fn fmt_runs(runs: &[(Paint, f64)]) -> String {
    if runs.is_empty() {
        return "nothing".into();
    }
    runs.iter().map(|(p, v)| format!("{p:?} up to {v:.1}")).collect::<Vec<_>>().join(", ")
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn bar_tooltip_describes_the_hovered_bar() -> Result<()> {
    let mut r = renderer().await;
    let theme = GraphixTheme { inner: iced_core::Theme::Light, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let size = Size::new(W as f32, H as f32);
    let viewport = Rectangle::with_size(size);
    let mut wrong = 0;
    for case in CASES {
        let chart = build(&program(case)).await?;
        let cats = categories(case);
        let mut el = chart.widget.view();
        let mut tree = Tree::new(el.as_widget());
        let node = el.as_widget_mut().layout(
            &mut tree,
            &r,
            &layout::Limits::new(Size::ZERO, size),
        );
        iced_core::Renderer::reset(&mut r, viewport);
        el.as_widget().draw(
            &tree,
            &mut r,
            &theme,
            &style,
            Layout::new(&node),
            mouse::Cursor::Unavailable,
            &viewport,
        );
        let px = r.screenshot(&Viewport::with_physical_size(Size::new(W, H), 1.0), Color::BLACK);
        let info = tree
            .state
            .downcast_ref::<ChartState>()
            .plot_info
            .get()
            .context("draw recorded no PlotInfo")?;
        eprintln!(
            "== {}\n   categories {:?}; PlotInfo x_range {:?}, y_range ({:.2}, {:.2})",
            case.name, cats, info.x_range, info.y_range.0, info.y_range.1
        );
        let n = cats.len() as f32;
        for (k, cat) in cats.iter().enumerate() {
            let x = info.rect.x + (k as f32 + 0.5) * info.rect.width / n;
            let pos = Point::new(x, info.rect.y + info.rect.height * 0.5);
            let drawn = column(&px, x, &info);
            let mut msgs: Vec<widgets::Message> = Vec::new();
            let mut shell = Shell::new(&mut msgs);
            el.as_widget_mut().update(
                &mut tree,
                &Event::Mouse(mouse::Event::CursorMoved { position: pos }),
                Layout::new(&node),
                mouse::Cursor::Available(pos),
                &r,
                &mut clipboard::Null,
                &mut shell,
                &viewport,
            );
            let snap = tree.state.downcast_ref::<ChartState>().snap_point.clone();
            let holders: Vec<(&str, f64)> = case
                .series
                .iter()
                .filter(|s| s.data.iter().any(|(c, _)| c == cat))
                .map(|s| {
                    let sum: f64 =
                        s.data.iter().filter(|(c, _)| c == cat).map(|(_, v)| v).sum();
                    (s.label, sum)
                })
                .collect();
            let ok = match &snap {
                None => false,
                Some(s) => holders
                    .iter()
                    .any(|(l, v)| s.label == *l && s.value == format!("{cat}: {v:.2}")),
            };
            if !ok {
                wrong += 1;
            }
            eprintln!(
                "   slot {k} '{cat}': drawn {}; series holding '{cat}' {:?}; tooltip {}{}",
                fmt_runs(&drawn),
                holders,
                match &snap {
                    None => "NONE".to_string(),
                    Some(s) => format!("\"{}: {}\"", s.label, s.value),
                },
                if ok { "" } else { "   <-- WRONG" }
            );
        }
    }
    if wrong > 0 {
        bail!("{wrong} hovered slots show a tooltip that does not describe their bar");
    }
    Ok(())
}
