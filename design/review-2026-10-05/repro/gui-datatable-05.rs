//! gui-datatable-05: a sort column subscribes `<row>/<col>` whatever the
//! column's source is, and the render path reads any `cells` entry before
//! the source, so a column with a static (string or Map) source shows,
//! and sorts by, the netidx value at `<row>/<col>` once it is listed in
//! `#sort_by`.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_05.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_05 -- --nocapture
//!
//! The program publishes `<row>/real` and `<row>/state` for three absolute
//! rows and overrides the "state" column with a static Map source of labels
//! (the book's `array::map` override pattern). The widget is compiled with
//! `widgets::compile`, fed every runtime update through `handle_update`,
//! flushed with `before_view`, laid out by a headless iced `UserInterface`
//! and read back with an `Operation` that collects every rendered text.
//!
//! Expected (book, data_table.md "Source": a Map source is "No
//! subscription"; "#sort_by": sort values come "from the source's stored
//! value" when the source is not `Netidx`): both tables show x-label,
//! z-label, y-label in the state column; the sorted one orders the rows
//! r0 (x-label), r2 (y-label), r1 (z-label).
//! Observed at c722befe (static_column_sorted_shows_labels FAILS, the
//! control passes):
//!   [/local/dt05a] rendered rows (#sort_by: ""):
//!     name | real | state
//!     r0 | real-r0 | x-label
//!     r1 | real-r1 | z-label
//!     r2 | real-r2 | y-label
//!   [/local/dt05b] rendered rows (#sort_by: [{ column: "state", .. }]):
//!     name | real | state ▲
//!     r1 | real-r1 | a-raw
//!     r2 | real-r2 | b-raw
//!     r0 | real-r0 | c-raw
//!   left: ["r1=a-raw", "r2=b-raw", "r0=c-raw"]
//!   right: ["r0=x-label", "r2=y-label", "r1=z-label"]

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Rectangle, Size,
    widget::{Id, Operation},
};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

fn program(base: &str, sort_by: &str) -> String {
    format!(
        r#"
use gui::data_table::{{data_table, text_column}};
sys::net::publish("{base}/r0/real", "real-r0");
sys::net::publish("{base}/r1/real", "real-r1");
sys::net::publish("{base}/r2/real", "real-r2");
sys::net::publish("{base}/r0/state", "c-raw");
sys::net::publish("{base}/r1/state", "a-raw");
sys::net::publish("{base}/r2/state", "b-raw");
let tbl = {{
    rows: ["{base}/r0", "{base}/r1", "{base}/r2"],
    columns: array::map(["real", "state"], |n| select n {{
        "state" => text_column(
            #name: "state",
            #source: &{{"r0" => "x-label", "r1" => "z-label", "r2" => "y-label"}}
        ),
        n => n
    }})
}};
let result = data_table({sort_by}#table: &tbl)
"#
    )
}

/// Every rendered text fragment with its bounds.
struct Texts(Vec<(Rectangle, String)>);

impl Operation for Texts {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<()>)) {
        operate(self)
    }

    fn text(&mut self, _id: Option<&Id>, bounds: Rectangle, text: &str) {
        self.0.push((bounds, text.to_string()));
    }
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
            .expect("no GPU adapter"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("no GPU device");
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

async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the root value"),
        }
    }
}

struct H {
    _ctx: TestCtx,
    /// Dropping it deletes the program, its publications included.
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)])
                .await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root = wait_for_update(&mut rx, compiled.exprs[0].id).await?;
        let widget =
            widgets::compile(gx.clone(), root).await.context("compile the widget")?;
        Ok(Self {
            _ctx: ctx,
            _compiled: compiled,
            rx,
            rt: tokio::runtime::Handle::current(),
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
        })
    }

    async fn drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(100));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            let rt = self.rt.clone();
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(100)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    /// One frame as the event loop builds it: `before_view`, layout, and
    /// the rendered texts grouped into rows, top to bottom, left to right.
    fn frame(&mut self) -> Vec<Vec<String>> {
        let Self { widget, renderer, cache, .. } = self;
        widget.before_view();
        let mut ui = UserInterface::build(
            widget.view(),
            Size::new(600.0, 300.0),
            std::mem::take(cache),
            renderer,
        );
        let mut texts = Texts(Vec::new());
        ui.operate(renderer, &mut texts);
        *cache = ui.into_cache();
        let mut cells = texts.0;
        cells.sort_by(|(a, _), (b, _)| {
            (a.y.round() as i64, a.x.round() as i64).cmp(&(b.y.round() as i64, b.x.round() as i64))
        });
        let mut rows: Vec<(i64, Vec<String>)> = Vec::new();
        for (b, t) in cells {
            let y = b.y.round() as i64;
            match rows.last_mut() {
                Some((ry, r)) if *ry == y => r.push(t),
                _ => rows.push((y, vec![t])),
            }
        }
        rows.into_iter().map(|(_, r)| r).collect()
    }

    /// Frames until every `real` cell arrived, then one more second of
    /// frames so any other subscription can land too.
    async fn settle(&mut self) -> Result<Vec<Vec<String>>> {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut arrived: Option<Instant> = None;
        loop {
            self.drain().await?;
            let g = self.frame();
            let reals = g.iter().flatten().filter(|t| t.starts_with("real-r")).count();
            if reals == 3 && arrived.is_none() {
                arrived = Some(Instant::now());
            }
            if let Some(t) = arrived {
                if t.elapsed() >= Duration::from_secs(1) {
                    return Ok(g);
                }
            }
            if Instant::now() >= deadline {
                bail!("the netidx `real` cells never arrived: {g:?}");
            }
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
    }
}

/// (row basename, state cell) of every body row, in display order.
fn body(g: &[Vec<String>]) -> Vec<(String, String)> {
    g.iter()
        .filter(|r| r.first().map(|n| n.starts_with('r') && n.len() == 2).unwrap_or(false))
        .map(|r| (r[0].clone(), r.last().cloned().unwrap_or_default()))
        .collect()
}

fn label(row: &str) -> &'static str {
    match row {
        "r0" => "x-label",
        "r1" => "z-label",
        "r2" => "y-label",
        _ => "?",
    }
}

async fn run(base: &str, sort_by: &str, want_order: &[&str]) -> Result<()> {
    let mut h = H::new(&program(base, sort_by)).await?;
    let g = h.settle().await?;
    println!("[{base}] rendered rows (#sort_by: {sort_by:?}):");
    for r in g.iter() {
        println!("  {}", r.join(" | "));
    }
    let b = body(&g);
    let order: Vec<&str> = b.iter().map(|(r, _)| r.as_str()).collect();
    let shown: Vec<String> = b.iter().map(|(r, s)| format!("{r}={s}")).collect();
    let want: Vec<String> = want_order.iter().map(|r| format!("{r}={}", label(r))).collect();
    println!("[{base}] EXPECTED state cells, in order: {want:?}");
    println!("[{base}] OBSERVED state cells, in order: {shown:?}");
    assert_eq!(order.len(), 3, "three body rows");
    assert_eq!(
        shown, want,
        "a static Map source column must show its labels and sort by them"
    );
    Ok(())
}

/// Control: no `#sort_by`, the static column shows its labels.
#[tokio::test(flavor = "multi_thread")]
async fn static_column_unsorted_shows_labels() -> Result<()> {
    run("/local/dt05a", "", &["r0", "r1", "r2"]).await
}

/// The same table sorted by the static column.
#[tokio::test(flavor = "multi_thread")]
async fn static_column_sorted_shows_labels() -> Result<()> {
    run(
        "/local/dt05b",
        "#sort_by: &[{ column: \"state\", direction: `Ascending }], ",
        &["r0", "r2", "r1"],
    )
    .await
}
