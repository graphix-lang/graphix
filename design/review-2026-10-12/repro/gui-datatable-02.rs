//! gui-datatable-02: sorting by a stored-source column never re-sorts when
//! its source changes (`DataTableW::handle_update` reconciles on a source
//! fire only when the source switches between netidx and stored).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_02 -- --nocapture
//!
//! Expected: after p0 <- "0" the priority column reads 0 1 2 (r0 first).
//! Observed at 7324207f (dev profile), the test FAILS:
//!   initial: [.., "r1", "1", "r2", "2", "r0", "3"]
//!   after p0 <- "0": [.., "r1", "1", "r2", "2", "r0", "0"]  (r0 shows 0, stays last)
//!   after the table refires (control): [.., "r0", "0", "r1", "1", "r2", "2"]

use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::{GXEvent, GXHandle, NoExt};
use iced_core::{
    Font, Pixels, Rectangle, Size, clipboard, mouse,
    widget::{Id, Operation},
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
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

const VIEWPORT: Size = Size::new(600.0, 400.0);

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

struct H {
    _ctx: TestCtx,
    _gx: GXHandle<NoExt>,
    _compiled: graphix_rt::CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    errors: Vec<String>,
}

impl H {
    async fn new(code: &str) -> Result<H> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = ahash::AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
        let v = first_value(&mut rx, compiled.exprs[0].id).await?;
        let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
        let renderer = renderer().await;
        let mut h = H {
            _ctx: ctx,
            _gx: gx,
            _compiled: compiled,
            rx,
            widget,
            renderer,
            cache: Cache::default(),
            errors: vec![],
        };
        h.drain_for(Duration::from_millis(150)).await;
        Ok(h)
    }

    async fn drain_for(&mut self, dur: Duration) {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + dur;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if let Err(e) = tokio::task::block_in_place(|| self.widget.handle_update(&rt, id, &v)) {
                                self.errors.push(format!("{e:#}"));
                            }
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return,
            }
        }
    }

    /// Every text the frame shows, in tree order (two frames, so the
    /// layout's viewport reaches the widget).
    fn texts(&mut self) -> Vec<String> {
        let mut seen = Seen::default();
        for _ in 0..2 {
            seen.texts.clear();
            self.widget.before_view();
            let element = self.widget.view();
            let cache = std::mem::take(&mut self.cache);
            let mut ui =
                UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
            let mut msgs = Vec::new();
            let _ = ui.update(
                &[],
                mouse::Cursor::Unavailable,
                &mut self.renderer,
                &mut clipboard::Null,
                &mut msgs,
            );
            ui.operate(&self.renderer, &mut seen);
            self.cache = ui.into_cache();
        }
        seen.texts
    }
}

#[derive(Default)]
struct Seen {
    texts: Vec<String>,
}

impl Operation for Seen {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<()>)) {
        operate(self)
    }

    fn text(&mut self, _id: Option<&Id>, _bounds: Rectangle, text: &str) {
        self.texts.push(text.to_string())
    }
}

fn priorities(texts: &[String]) -> Vec<String> {
    texts.iter().filter(|t| t.len() == 1 && t.as_bytes()[0].is_ascii_digit()).cloned().collect()
}

#[tokio::test(flavor = "multi_thread")]
async fn sort_by_stored_source_follows_the_source() -> Result<()> {
    let code = r#"use gui::data_table::*;
let p0 = "3";
let p1 = "1";
let p2 = "2";
let rows = ["r0", "r1", "r2"];
p0 <- sys::time::after_idle(duration:800.ms, "0");
rows <- sys::time::after_idle(duration:2000.ms, ["r0", "r1", "r2"]);
let tbl = { rows, columns: [
        { name: "priority", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => p0, "r1" => p1, "r2" => p2 },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #sort_by: &[{ column: "priority", direction: `Ascending }],
    #table: &tbl
)"#;
    let mut h = H::new(code).await?;
    let t0 = h.texts();
    eprintln!("initial: texts {t0:?} errors {:?}", h.errors);
    h.drain_for(Duration::from_millis(1200)).await;
    let t1 = h.texts();
    eprintln!("after p0 <- \"0\": texts {t1:?} errors {:?}", h.errors);
    h.drain_for(Duration::from_millis(1200)).await;
    let t2 = h.texts();
    eprintln!("after the table refires (control): texts {t2:?} errors {:?}", h.errors);
    assert_eq!(priorities(&t0), vec!["1", "2", "3"], "initial sort");
    assert_eq!(priorities(&t2), vec!["0", "1", "2"], "control: a table refire re-sorts");
    assert_eq!(
        priorities(&t1),
        vec!["0", "1", "2"],
        "r0's priority is now 0, so it sorts first"
    );
    Ok(())
}
