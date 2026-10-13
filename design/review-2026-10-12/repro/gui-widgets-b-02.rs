//! gui-widgets-b-02: a send the program never echoes is shown forever, and
//! the echo queue grows without bound (`Echoes`, stdlib/graphix-package-gui/
//! src/widgets/mod.rs; `TextEditorW::pending`, widgets/text_editor.rs).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_b_02 -- --nocapture
//!
//! Expected: a checkbox over `ok = false` whose handler writes nothing
//! still shows `false`, so a second click sends `true` again; a text input
//! over `v = ""` whose handler writes nothing sends "b" for the key `b`
//! after `a`; an editor whose edits go to another variable holds no copy
//! per keystroke.
//! Observed at 7324207f (the first assert fails; all three are wrong):
//!   checkbox over flag=false, handler writes nothing: click 1 sent
//!     [Bool(true)], click 2 sent [Bool(false)]   (box still drawn checked)
//!   text_input over v="", handler writes nothing: key a sent [String("a")],
//!     key b sent [String("ab")]                  (field still shows "a")
//!   text_editor, 150 keys into a 64 KiB doc: on_edit writes `other`:
//!     heap +94 KB/key, dropping the widget frees 38.56 MB; on_edit writes
//!     `loaded` (echoed): +5.5 KB/key, frees 28.43 MB; no keys: 28.71 MB.
//!     The ~10 MB difference is `TextEditorW::pending`: 155 doc copies.

use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, Events, TestCtx};
use graphix_package_gui::{
    frame,
    theme::GraphixTheme,
    widgets::{self, GuiW, GuiWidget, IcedElement, Message, MessageShell},
};
use graphix_rt::{GXEvent, GXHandle, NoExt};
use iced_core::{
    Event, Font, Pixels, Point, Size, clipboard, keyboard, mouse,
};
use iced_runtime::user_interface::Cache;
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicIsize, Ordering},
    time::Duration,
};

struct Counting;
static LIVE: AtomicIsize = AtomicIsize::new(0);
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        LIVE.fetch_add(l.size() as isize, Ordering::Relaxed);
        unsafe { System.alloc(l) }
    }
    unsafe fn dealloc(&self, p: *mut u8, l: Layout) {
        LIVE.fetch_sub(l.size() as isize, Ordering::Relaxed);
        unsafe { System.dealloc(p, l) }
    }
    unsafe fn realloc(&self, p: *mut u8, l: Layout, n: usize) -> *mut u8 {
        LIVE.fetch_add(n as isize - l.size() as isize, Ordering::Relaxed);
        unsafe { System.realloc(p, l, n) }
    }
}
#[global_allocator]
static A: Counting = Counting;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const IMPORTS: &str = "use gui::*;\n\
use gui::checkbox::{self, *};\n\
use gui::text_input::{self, *};\n\
use gui::text_editor::{self, *};\n";

const VIEWPORT: Size = Size::new(400.0, 300.0);
const HIT: Point = Point::new(10.0, 10.0);

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

async fn first_value(rx: &mut Events, target: ExprId) -> Result<Value> {
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
            _ = &mut timeout => anyhow::bail!("no widget value"),
        }
    }
}

struct Dummy;
impl GuiWidget<NoExt> for Dummy {
    fn handle_update(&mut self, _: &tokio::runtime::Handle, _: ExprId, _: &Value) -> Result<bool> {
        Ok(false)
    }
    fn view(&self) -> IcedElement<'_> {
        iced_widget::Space::new().into()
    }
}

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    rx: Events,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: mouse::Cursor,
    nupdates: usize,
    _compiled: graphix_rt::CompRes<NoExt>,
}

impl H {
    async fn new(code: &str) -> Result<H> {
        let (tx, mut rx) = tokio::sync::mpsc::channel(100);
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
            gx,
            rx,
            widget,
            renderer,
            cache: Cache::default(),
            cursor: mouse::Cursor::Unavailable,
            nupdates: 0,
            _compiled: compiled,
        };
        h.drain().await?;
        Ok(h)
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let widget = &mut self.widget;
        let n = &mut self.nupdates;
        testing::drain_idle(&self.gx, &mut self.rx, async |id, v: Value| {
            *n += 1;
            tokio::task::block_in_place(|| widget.handle_update(&rt, id, &v))
        })
        .await?;
        Ok(())
    }

    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        for ev in events {
            if let Event::Mouse(mouse::Event::CursorMoved { position }) = ev {
                self.cursor = mouse::Cursor::Available(*position);
            }
        }
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let mut ops = Vec::new();
        self.widget.take_ops(&mut ops);
        self.widget.before_view();
        let mut messages = Vec::new();
        frame::frame(
            self.widget.view(),
            VIEWPORT,
            &mut self.cache,
            &mut self.renderer,
            &mut ops,
            events,
            self.cursor,
            &mut clipboard::Null,
            &mut messages,
            &theme,
        );
        messages
    }

    fn click(&mut self) -> Vec<Message> {
        let mut all = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: HIT })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]));
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]));
        all
    }

    fn key(&mut self, c: &str) -> Vec<Message> {
        let s: iced_core::SmolStr = c.into();
        self.frame(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: keyboard::Key::Character(s.clone()),
            modified_key: keyboard::Key::Character(s.clone()),
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
            text: Some(s),
            repeat: false,
        })])
    }

    /// Apply messages as the event loop does, then let the runtime run.
    /// Answers the first argument of every call made.
    async fn apply(&mut self, msgs: Vec<Message>) -> Result<Vec<Value>> {
        let mut calls = Vec::new();
        let widget = &mut self.widget;
        frame::apply_messages(&self.gx, msgs, |msg, pending| {
            if let Message::Call(_, args) = msg
                && let Some(a) = args.first()
            {
                calls.push(a.clone());
            }
            let mut shell = MessageShell::default();
            widget.on_message(msg, &mut shell);
            pending.extend(shell.out.drain(..));
        });
        self.drain().await?;
        Ok(calls)
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn unechoed_sends() -> Result<()> {
    // (a) checkbox: the handler writes nothing; `ok` stays false
    let code = format!(
        "{IMPORTS}let flag = false;\nlet result = checkbox(#on_toggle: |b| b ~ null, &flag)"
    );
    let mut h = H::new(&code).await?;
    let m = h.click();
    let first = h.apply(m).await?;
    let m = h.click();
    let second = h.apply(m).await?;
    eprintln!("checkbox over ok=false, handler writes nothing: click 1 sent {first:?}, click 2 sent {second:?}");
    let cb_ok = second == vec![Value::Bool(true)];
    drop(h);

    // (a) text_input: the handler writes nothing; `v` stays ""
    let code = format!(
        "{IMPORTS}let v = \"\";\nlet result = text_input(#on_input: |s| s ~ null, &v)"
    );
    let mut h = H::new(&code).await?;
    let m = h.click();
    h.apply(m).await?;
    let m = h.key("a");
    let a = h.apply(m).await?;
    let m = h.key("b");
    let b = h.apply(m).await?;
    eprintln!("text_input over v=\"\", handler writes nothing: key a sent {a:?}, key b sent {b:?}");
    let ti_ok = b == vec![Value::from("b")];
    drop(h);

    // (b) text_editor: edits go to another variable; `loaded` never changes
    let doc = "x".repeat(65536);
    let mut growth = vec![];
    let mut held = vec![];
    for (target, keys) in [("other", 150isize), ("loaded", 150), ("other", 0)] {
        let code = format!(
            "{IMPORTS}let loaded = \"{doc}\";\nlet other = \"\";\n\
             let result = text_editor(#on_edit: |s| {target} <- s, &loaded)"
        );
        let mut h = H::new(&code).await?;
        let m = h.click();
        h.apply(m).await?;
        for _ in 0..5 {
            let m = h.key("y");
            h.apply(m).await?;
        }
        let before = LIVE.load(Ordering::Relaxed);
        let mut ncalls = 0;
        let mut last_len = 0;
        for _ in 0..keys {
            let m = h.key("y");
            for c in h.apply(m).await? {
                ncalls += 1;
                if let Value::String(s) = c {
                    last_len = s.len();
                }
            }
        }
        eprintln!("text_editor on_edit writes `{target}`: {ncalls} calls, last text {last_len} bytes, {} updates delivered", h.nupdates);
        tokio::time::sleep(Duration::from_millis(200)).await;
        h.drain().await?;
        let after = LIVE.load(Ordering::Relaxed);
        let g = after - before;
        eprintln!(
            "text_editor on_edit writes `{target}`: live heap grew {g} bytes over {keys} keys of a 64 KiB doc ({} per key)",
            g / keys.max(1)
        );
        growth.push(g);
        let before_free = LIVE.load(Ordering::Relaxed);
        let w = std::mem::replace(&mut h.widget, Box::new(Dummy));
        drop(w);
        let freed = before_free - LIVE.load(Ordering::Relaxed);
        eprintln!("text_editor on_edit writes `{target}`: dropping the widget after {keys} keys freed {freed} bytes");
        held.push(freed);
        drop(h);
    }
    {
        use iced_widget::text_editor::{Action, Content, Edit};
        let b0 = LIVE.load(Ordering::Relaxed);
        let mut c: Content<widgets::Renderer> = Content::with_text(&doc);
        let b1 = LIVE.load(Ordering::Relaxed);
        for _ in 0..155 {
            c.perform(Action::Edit(Edit::Insert('y')));
        }
        let b2 = LIVE.load(Ordering::Relaxed);
        eprintln!("bare Content: {} bytes at creation, {} after 155 inserts", b1 - b0, b2 - b0);
        drop(c);
    }
    assert!(cb_ok, "checkbox: the second click should send true (program holds false), sent {second:?}");
    assert!(ti_ok, "text_input: key b should send \"b\" (program holds \"\"), sent {b:?}");
    assert!(
        held[0] < 2 * 1024 * 1024,
        "text_editor: the widget holds {} bytes after 150 keys whose edits go elsewhere (write-back control: {})",
        held[0],
        held[1]
    );
    Ok(())
}
