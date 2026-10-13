//! gui-widgets-a-06: Handler updates never ask for a redraw.
//! A button whose on_press goes null -> fn draws differently (iced draws a
//! Button without on_press as disabled), yet ButtonW::handle_update
//! returns false, so GuiWindow::handle_update leaves needs_redraw false
//! and the event loop renders no frame showing the enabled button.
//!
//! Command (with this file copied to
//!   stdlib/graphix-package-gui/tests/review_gui_widgets_a_06.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_06 -- --nocapture
//! Expected: handle_update asks for a redraw (true) when on_press arrives.
//! Observed at 7324207f: before: click publishes 0 calls; the handler
//!   update returns false (no redraw); after: click publishes 1 call.
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::{
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{
    Event, Font, Pixels, Point, Size, clipboard, mouse,
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{path::Path, publisher::Value};
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

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
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
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
    watched: AHashMap<ExprId, Value>,
    names: AHashMap<&'static str, ExprId>,
    refs: Vec<Ref<NoExt>>,
    errors: Vec<String>,
}

impl H {
    async fn new(code: &str, watch: &[&'static str]) -> Result<H> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
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
            gx,
            compiled,
            rx,
            widget,
            renderer,
            cache: Cache::default(),
            cursor: Point::ORIGIN,
            watched: AHashMap::default(),
            names: AHashMap::default(),
            refs: vec![],
            errors: vec![],
        };
        for &name in watch {
            let bid = find_bind_id(&h.compiled.env, name)?;
            let r = h.gx.compile_ref(bid).await?;
            h.watched.insert(r.id, r.last.clone().unwrap_or(Value::Null));
            h.names.insert(name, r.id);
            h.refs.push(r);
        }
        h.drain_for(Duration::from_millis(150)).await;
        let _ = h.frame(&[]);
        Ok(h)
    }

    /// Deliver every update for `dur`, as the event loop does: an error is
    /// logged and the next update goes on. Returns whether any update
    /// asked for a redraw.
    async fn drain_for(&mut self, dur: Duration) -> bool {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + dur;
        let mut changed = false;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if let Some(slot) = self.watched.get_mut(&id) {
                                *slot = v.clone();
                            }
                            match tokio::task::block_in_place(|| self.widget.handle_update(&rt, id, &v)) {
                                Ok(c) => changed |= c,
                                Err(e) => self.errors.push(format!("{e:#}")),
                            }
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return changed,
            }
        }
    }

    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    fn click(&mut self, pos: Point, button: mouse::Button) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(button))]));
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(button))]));
        all
    }

    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<usize> {
        let mut n = 0;
        for m in msgs {
            if let Message::Call(id, args) = m {
                self.gx.call(id, args)?;
                n += 1;
            }
        }
        self.drain_for(Duration::from_millis(300)).await;
        Ok(n)
    }

    fn get(&self, name: &str) -> Value {
        self.names
            .get(name)
            .and_then(|id| self.watched.get(id))
            .cloned()
            .unwrap_or(Value::Null)
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn handler_change_redraws_button() -> Result<()> {
    let code = r#"use gui::{button::button, text::text};
let hit = false;
let on = false;
on <- sys::time::after_idle(duration:1000.ms, true);
let result = button(#on_press: select on { true => |e| hit <- e ~ true, false => null }, &text(&"go"))"#;
    let mut h = H::new(code, &[]).await?;
    let msgs = h.click(Point::new(10.0, 10.0), mouse::Button::Left);
    let before = msgs.iter().filter(|m| matches!(m, Message::Call(..))).count();
    eprintln!("before: a click on the button publishes {before} call(s)");
    let changed = h.drain_for(Duration::from_millis(2000)).await;
    eprintln!("on_press null -> fn: handle_update asked for a redraw: {changed}; errors {:?}", h.errors);
    let msgs = h.click(Point::new(10.0, 10.0), mouse::Button::Left);
    let after = msgs.iter().filter(|m| matches!(m, Message::Call(..))).count();
    eprintln!("after: a click on the button publishes {after} call(s)");
    assert_eq!(before, 0);
    assert_eq!(after, 1);
    assert!(changed, "the handler arrived (the button now draws enabled) but no redraw was asked for");
    Ok(())
}
