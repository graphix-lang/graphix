//! gui-widgets-b-07: `text_input` drops a keystroke typed before the
//! runtime echoes the previous one.
//!
//! `TextInputW::view` (src/widgets/text_input.rs:145) hands iced the
//! last value the runtime delivered, and the event loop rebuilds the UI
//! from `view()` for every batch of window events (event_loop.rs:316-321),
//! then sends each `on_input` to the runtime with the fire-and-forget
//! `GXHandle::call` (event_loop.rs:401-403). A key whose batch is built
//! before the echo of the previous `on_input` lands edits the stale
//! string, and its `on_input` overwrites the earlier keystroke.
//!
//! `Gui::frame` below is one `about_to_wait`; `Gui::deliver_arrived`
//! is the `ToGui::Update`s winit hands the loop before the next one.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_07.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_07 -- --nocapture
//!
//! Expected: a and b end with v == "ab", c with v == "item1", d (a
//! checkbox clicked twice) with c == false.
//! Observed (HEAD c722befe):
//!   a_control_echo_between_keys ... ok   callbacks "a", "ab"; v = "ab"
//!   b_key_before_echo ... FAILED         callbacks "a", "b";  v = "b"
//!   c_fast_keys_over_a_search ... FAILED callbacks "i", "t", "ie", "im",
//!     "t1"; v = "t1" (the echo of "i" took ~63 ms, two keys)
//!   d_checkbox_second_click_before_echo ... FAILED  callbacks true, true;
//!     c = true

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, Renderer};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse};
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

async fn renderer() -> Renderer {
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

fn find_bind_id(env: &Env, module: &str, var: &str) -> Result<BindId> {
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if netidx::path::Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {module}::{var}")
}

fn key(ch: char) -> Event {
    let s: iced_core::SmolStr = ch.to_string().into();
    Event::Keyboard(keyboard::Event::KeyPressed {
        key: keyboard::Key::Character(s.clone()),
        modified_key: keyboard::Key::Character(s.clone()),
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::empty(),
        text: Some(s),
        repeat: false,
    })
}

fn show(v: &Value) -> String {
    match v {
        Value::String(s) => format!("{:?}", s.as_str()),
        v => format!("{v}"),
    }
}

struct Gui {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    watch: Ref<NoExt>,
    watched: Value,
    start: Instant,
}

impl Gui {
    /// `code` binds `result` (the widget) and `var` (the state to watch).
    async fn new(code: &str, var: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let root = tokio::time::timeout(Duration::from_secs(60), async {
            loop {
                let mut batch = rx.recv().await.context("channel closed")?;
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    return Ok::<Value, anyhow::Error>(v);
                }
            }
        })
        .await
        .context("timeout waiting for the root value")??;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        let bid = find_bind_id(&compiled.env, "test", var)?;
        let watch = gx.compile_ref(bid).await.context("ref to the watched var")?;
        let watched = watch.last.clone().unwrap_or(Value::Null);
        Ok(Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            watch,
            watched,
            start: Instant::now(),
        })
    }

    fn ms(&self) -> f64 {
        self.start.elapsed().as_secs_f64() * 1000.0
    }

    /// `GuiHandler::user_event(ToGui::Update(..))` for one runtime batch.
    fn apply(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                if id == self.watch.id {
                    println!("  {:7.1} ms  echo     state = {}", self.ms(), show(&v));
                    self.watched = v.clone();
                }
                let w = &mut self.widget;
                match rt.runtime_flavor() {
                    tokio::runtime::RuntimeFlavor::MultiThread => {
                        tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?
                    }
                    _ => w.handle_update(&rt, id, &v)?,
                };
            }
        }
        Ok(())
    }

    /// Every update that has arrived by now, as winit hands the
    /// `ToGui::Update`s to the loop before its next `about_to_wait`.
    fn deliver_arrived(&mut self) -> Result<()> {
        while let Ok(batch) = self.rx.try_recv() {
            self.apply(batch)?;
        }
        Ok(())
    }

    /// Deliver everything until the runtime has been quiet for `quiet`.
    async fn settle(&mut self, quiet: Duration) -> Result<()> {
        loop {
            match tokio::time::timeout(quiet, self.rx.recv()).await {
                Ok(Some(batch)) => self.apply(batch)?,
                Ok(None) => bail!("runtime channel closed"),
                Err(_) => return Ok(()),
            }
        }
    }

    /// One `GuiHandler::about_to_wait`: build the UI from `view()`, feed
    /// this batch of events, then `gx.call` each `Call` without waiting.
    fn frame(&mut self, events: &[Event]) -> Result<()> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(element, Size::new(300.0, 50.0), cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        for m in msgs {
            if let Message::Call(id, args) = m {
                let arg = args.iter().next().map(show).unwrap_or_default();
                println!("  {:7.1} ms  callback {arg}", self.ms());
                self.gx.call(id, args)?;
            }
        }
        Ok(())
    }

    fn click(&mut self, pos: Point) -> Result<()> {
        self.cursor = pos;
        self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })])?;
        self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))])?;
        self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))])
    }
}

const HIT: Point = Point::new(10.0, 10.0);

const TYPED: &str = "use gui::text_input::text_input;\n\
     let v = \"\";\n\
     let result = text_input(#on_input: |s| v <- s, &v)";

/// Control: the echo of 'a' is delivered before 'b' is pressed.
#[tokio::test(flavor = "current_thread")]
async fn a_control_echo_between_keys() -> Result<()> {
    println!("a_control_echo_between_keys");
    let mut g = Gui::new(TYPED, "v").await?;
    g.settle(Duration::from_millis(300)).await?;
    g.click(HIT)?;
    g.frame(&[key('a')])?;
    g.settle(Duration::from_millis(300)).await?;
    g.frame(&[key('b')])?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  final v = {}", show(&g.watched));
    assert_eq!(show(&g.watched), "\"ab\"");
    Ok(())
}

/// 'b' is pressed in the next batch, before the echo of 'a' arrived.
#[tokio::test(flavor = "current_thread")]
async fn b_key_before_echo() -> Result<()> {
    println!("b_key_before_echo");
    let mut g = Gui::new(TYPED, "v").await?;
    g.settle(Duration::from_millis(300)).await?;
    g.click(HIT)?;
    g.frame(&[key('a')])?;
    g.frame(&[key('b')])?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  final v = {}", show(&g.watched));
    assert_eq!(show(&g.watched), "\"ab\"", "typed \"ab\", the text input holds");
    Ok(())
}

/// Search as you type over 200k strings; keys 30 ms apart (the GNOME
/// key-repeat interval). Every update that arrived is delivered before
/// each key.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn c_fast_keys_over_a_search() -> Result<()> {
    println!("c_fast_keys_over_a_search");
    let code = "use gui::text_input::text_input;\n\
         let v = \"\";\n\
         let big = array::init(200000, |i| \"item[i]\");\n\
         let hits = array::sort(array::filter(big, |s| str::contains(#part: v, s)));\n\
         let result = text_input(\
             #placeholder: &\"[array::len(hits)] matches\", \
             #on_input: |s| v <- s, \
             &v)";
    let mut g = Gui::new(code, "v").await?;
    g.settle(Duration::from_millis(1000)).await?;
    g.click(HIT)?;
    for ch in "item1".chars() {
        g.deliver_arrived()?;
        println!("  {:7.1} ms  key      {ch:?}", g.ms());
        g.frame(&[key(ch)])?;
        tokio::time::sleep(Duration::from_millis(30)).await;
    }
    g.settle(Duration::from_millis(2000)).await?;
    println!("  final v = {}", show(&g.watched));
    assert_eq!(show(&g.watched), "\"item1\"", "typed \"item1\", the text input holds");
    Ok(())
}

/// The same race on a checkbox: two clicks, the second before the echo.
#[tokio::test(flavor = "current_thread")]
async fn d_checkbox_second_click_before_echo() -> Result<()> {
    println!("d_checkbox_second_click_before_echo");
    let code = "use gui::checkbox::checkbox;\n\
         let c = false;\n\
         let result = checkbox(#label: &\"check\", #on_toggle: |b| c <- b, &c)";
    let mut g = Gui::new(code, "c").await?;
    g.settle(Duration::from_millis(300)).await?;
    g.click(HIT)?;
    g.click(HIT)?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  final c = {}", show(&g.watched));
    assert_eq!(show(&g.watched), "false", "clicked twice, the checkbox holds");
    Ok(())
}
