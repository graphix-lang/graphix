//! gui-widgets-a-02: a list child that fails to compile wipes out every
//! sibling permanently (`Children::update`, stdlib/graphix-package-gui/
//! src/widgets/mod.rs).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_02 -- --nocapture
//!
//! Expected: the column shows "a" before, "a" (and ideally "b" once the
//! size is valid) after.
//! Observed at 7324207f (dev profile), FAILS:
//!   before: texts ["a"] errors []
//!   after show <- true (b's size is 0): texts [], errors ["column
//!     children: text: size: a text size must be finite and positive, got 0"]
//!   after bad <- 16.0: texts [] (no retry: the ref's last value already
//!     equals the list)

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

const VIEWPORT: Size = Size::new(400.0, 300.0);

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
    async fn new(code: &str, _watch: &[&'static str]) -> Result<H> {
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

    /// Deliver every update for `dur` as the event loop does: an error is
    /// logged and the next update goes on.
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

    fn operate(&mut self, op: &mut dyn Operation) {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(&[], mouse::Cursor::Unavailable, &mut self.renderer, &mut clipboard::Null, &mut msgs);
        ui.operate(&self.renderer, op);
        self.cache = ui.into_cache();
    }
}

/// What an operation sees: every text.
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

/// A list's new child that fails to compile (a text size of 0, refused by
/// `TextSizeV`) takes every kept sibling with it, and the size becoming
/// valid later brings nothing back.
#[tokio::test(flavor = "multi_thread")]
async fn children_survive_a_failed_child() -> Result<()> {
    let code = r#"use gui::{column::column, text::text};
let bad = 0.0;
let show = false;
show <- sys::time::after_idle(duration:1500.ms, true);
bad <- sys::time::after_idle(duration:2500.ms, 16.0);
let a = text(&"a");
let b = text(#size: &bad, &"b");
let kids = select show { true => [a, b], false => [a] };
let result = column(&kids)"#;
    let mut h = H::new(code, &[]).await?;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("before: texts {:?} errors {:?}", seen.texts, h.errors);
    h.drain_for(Duration::from_millis(2000)).await;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("after show <- true (b's size is 0): texts {:?}, errors {:?}", seen.texts, h.errors);
    h.drain_for(Duration::from_millis(1500)).await;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("after bad <- 16.0: texts {:?}, errors {:?}", seen.texts, h.errors);
    assert!(
        seen.texts.contains(&"a".to_string()),
        "the kept child `a` vanished: texts {:?}",
        seen.texts
    );
    Ok(())
}

