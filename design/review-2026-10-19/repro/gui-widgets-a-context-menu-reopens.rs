//! gui-widgets-a-context-menu-reopens: a context menu whose items go
//! empty while it is open comes back by itself, at the old position, when
//! they return (context_menu_widget.rs: `state.open` stays true while
//! `overlay` returns no menu for empty items).
//!
//! Expected: 0 calls (the menu is gone until the next right click).
//! Observed at 9445a1d21:
//!   context menu: after items went [] and back, a click where the item was
//!   makes 1 call(s)
//!
//! To run, copy this file to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_2.rs, then:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_2 -- --nocapture
use anyhow::{Context, Result};
use graphix_package_core::testing::{self, Fixture, PackageRef};
use graphix_package_gui::{
    frame,
    theme::GraphixTheme,
    widgets::{self, GuiW, Message, Renderer},
};
use graphix_rt::NoExt;
use iced_core::{Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface;
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

struct NoClip;

impl clipboard::Clipboard for NoClip {
    fn read(&self, _kind: clipboard::Kind) -> Option<String> {
        None
    }
    fn write(&mut self, _kind: clipboard::Kind, _contents: String) {}
}

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
            .expect("no adapter"),
    };
    let (device, queue) =
        adapter.request_device(&wgpu::DeviceDescriptor::default()).await.unwrap();
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

struct H {
    f: Fixture,
    widget: GuiW<NoExt>,
    renderer: Renderer,
    cache: user_interface::Cache,
    cursor: mouse::Cursor,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (f, initial) = Fixture::new(code, REGISTER, |_| ()).await?;
        let widget = widgets::compile(f.gx.clone(), initial).await?;
        Ok(Self {
            f,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: mouse::Cursor::Unavailable,
        })
    }

    /// Deliver every update; the errors handle_update returned.
    async fn drain(&mut self) -> Result<Vec<String>> {
        let Self { f, widget, .. } = self;
        let rt = tokio::runtime::Handle::current();
        let mut errors = vec![];
        testing::drain_idle(&f.gx, &mut f.rx, async |id, v: Value| {
            match tokio::task::block_in_place(|| widget.handle_update(&rt, id, &v)) {
                Ok(c) => Ok(c),
                Err(e) => {
                    errors.push(format!("{e:#}"));
                    Ok(false)
                }
            }
        })
        .await?;
        Ok(errors)
    }

    async fn set(&mut self, name: &str, v: Value) -> Result<()> {
        let bid = testing::find_bind_id(&self.f.compiled.env, name)?;
        self.f.gx.compile_ref(bid).await?.set(v)?;
        Ok(())
    }

    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        for ev in events {
            if let Event::Mouse(mouse::Event::CursorMoved { position }) = ev {
                self.cursor = mouse::Cursor::Available(*position)
            }
        }
        let mut messages = vec![];
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        frame::frame(
            &mut self.widget,
            Size::new(300.0, 600.0),
            &mut self.cache,
            &mut self.renderer,
            events,
            self.cursor,
            &mut NoClip,
            &mut messages,
            &theme,
        );
        messages
    }

    /// The calls a left click at `p` makes.
    fn click(&mut self, p: Point) -> usize {
        let mut calls = 0;
        for ev in [
            mouse::Event::CursorMoved { position: p },
            mouse::Event::ButtonPressed(mouse::Button::Left),
            mouse::Event::ButtonReleased(mouse::Button::Left),
        ] {
            calls += self
                .frame(&[Event::Mouse(ev)])
                .iter()
                .filter(|m| matches!(m, Message::Call(..)))
                .count();
        }
        calls
    }
}


fn right_click(h: &mut H, p: Point) {
    for ev in [
        mouse::Event::CursorMoved { position: p },
        mouse::Event::ButtonPressed(mouse::Button::Right),
        mouse::Event::ButtonReleased(mouse::Button::Right),
    ] {
        h.frame(&[Event::Mouse(ev)]);
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn context_menu_reopens_by_itself() -> Result<()> {
    let code = r#"
        use gui::{menu, text::text, container::container};
        let show = true;
        let items = select show { true => [menu::action(#on_click: |c| null, &"Copy")], false => [] };
        let result = menu::context_menu(&items, &container(#width: &`Fill, #height: &`Fixed(40.0), &text(&"target")))
    "#;
    let mut h = H::new(code).await?;
    h.drain().await?;
    // where the item is when the menu opens at (20, 20)
    let item = Point::new(60.0, 30.0);
    right_click(&mut h, Point::new(20.0, 20.0));
    h.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: Point::new(250.0, 300.0) })]);
    h.set("test::show", Value::Bool(false)).await?;
    h.drain().await?;
    h.frame(&[]);
    h.set("test::show", Value::Bool(true)).await?;
    h.drain().await?;
    h.frame(&[]);
    let calls = h.click(item);
    println!("context menu: after items went [] and back, a click where the item was makes {calls} call(s)");
    assert_eq!(calls, 0, "the menu came back without a right click");
    Ok(())
}

