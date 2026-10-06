//! gui-widgets-b-06: slider and vertical_slider narrow f64 to f32.
//!
//! stdlib/graphix-package-gui/src/widgets/slider.rs:196-198 cast value,
//! min and max to f32 and :214 the step, so iced's Slider/VerticalSlider
//! run over f32 and :209 widens the result back. iced's sliders are
//! generic over any `Copy + From<u8> + PartialOrd + Into<f64> +
//! FromPrimitive`, f64 included. Every value a slider delivers is
//! rounded to f32: a keyboard step past 2^24 rounds back to the current
//! value and is dropped (iced publishes only a change), and fractional
//! steps reach the program as f32 noise.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_06.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_b_06 -- --nocapture
//!
//! sliders_keep_f64 drives the real widgets headlessly as the event loop
//! does (UserInterface::build/update over a kept cache), sends every
//! Call they publish through the runtime and feeds the program's updates
//! back, so `v` is the slider's value as the program sees it and `label`
//! is `"[v]"`, what `text(&"[v]")` shows. iced_f64_control runs iced's
//! sliders over f64 through the same events: what the widgets do
//! without the casts.
//!
//! Expected (the f64 control, which passes):
//!   keys/vkeys ([0, 1e8] step 1, from 16777215, three ArrowUps):
//!     16777216, 16777217, 16777218
//!   label_key ([0, 1] step 0.1, from 0.1, ArrowUp): 0.2
//!   label_click ([0, 1] step 0.01, click at 7%): 0.07
//!   palette (book/src/examples/gui/custom_palette.gx's slider, ArrowUp
//!     from 0): Brightness: 0.05
//! Observed at c722befe (sliders_keep_f64 FAILS):
//!   keys and vkeys: ArrowUp -> 16777216.0, then nothing, nothing; v
//!     stays 16777216
//!   label_key: 0.20000000298023224
//!   label_click: 0.07000000029802322
//!   palette: Brightness: 0.05000000074505806

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
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{cell::RefCell, rc::Rc, time::Duration};
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const KEYS: &str = "use gui::slider::slider;
let v = 16777215.0;
let label = \"[v]\";
let result = slider(#min: &0.0, #max: &100000000.0, #step: &1.0, #on_change: |x| v <- x, &v)";

const VKEYS: &str = "use gui::vertical_slider::vertical_slider;
let v = 16777215.0;
let label = \"[v]\";
let result = vertical_slider(#min: &0.0, #max: &100000000.0, #step: &1.0, #on_change: |x| v <- x, &v)";

const LABEL_KEY: &str = "use gui::slider::slider;
let v = 0.1;
let label = \"[v]\";
let result = slider(#min: &0.0, #max: &1.0, #step: &0.1, #on_change: |x| v <- x, &v)";

const LABEL_CLICK: &str = "use gui::slider::slider;
let v = 0.5;
let label = \"[v]\";
let result = slider(#min: &0.0, #max: &1.0, #step: &0.01, #on_change: |x| v <- x, &v)";

/// The slider and label of book/src/examples/gui/custom_palette.gx.
const PALETTE: &str = "use gui::slider::slider;
let br = 0.0;
let label = \"Brightness: [br]\";
let result = slider(#min: &0.0, #max: &0.5, #step: &0.05, #on_change: |v| br <- v, &br)";

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
    use netidx::path::Path;
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

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
    cursor: Point,
    label_id: ExprId,
    label: Value,
    _refs: Vec<Ref<NoExt>>,
}

impl H {
    async fn new(code: &str, viewport: Size, cursor: Point) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
        let target = compiled.exprs[0].id;
        let timeout = tokio::time::sleep(Duration::from_secs(10));
        tokio::pin!(timeout);
        let v = loop {
            tokio::select! {
                biased;
                Some(mut batch) = rx.recv() => {
                    let mut found = None;
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev && id == target {
                            found = Some(v);
                        }
                    }
                    if let Some(v) = found { break v }
                }
                _ = &mut timeout => bail!("no widget value"),
            }
        };
        let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
        let bid = find_bind_id(&compiled.env, "test::label")?;
        let r = gx.compile_ref(bid).await.context("ref label")?;
        let mut h = Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            viewport,
            cursor,
            label_id: r.id,
            label: r.last.clone().unwrap_or(Value::Null),
            _refs: vec![r],
        };
        let _ = &h.compiled;
        h.pump(Duration::from_millis(300)).await?;
        h.events(&[Event::Mouse(mouse::Event::CursorMoved { position: cursor })]);
        Ok(h)
    }

    fn events(&mut self, events: &[Event]) -> Vec<Message> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, self.viewport, cache, &mut self.renderer);
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

    fn arrow_up(&mut self) -> Vec<Message> {
        let key = keyboard::Key::Named(keyboard::key::Named::ArrowUp);
        self.events(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: key.clone(),
            modified_key: key,
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
            text: None,
            repeat: false,
        })])
    }

    fn click(&mut self, pos: Point) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonPressed(
            mouse::Button::Left,
        ))]));
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonReleased(
            mouse::Button::Left,
        ))]));
        all
    }

    /// Send the published Calls through the runtime, then feed the
    /// program's updates back to the widget.
    async fn dispatch(&mut self, msgs: &[Message]) -> Result<()> {
        for m in msgs {
            if let Message::Call(id, args) = m {
                self.gx.call(*id, args.clone())?;
            }
        }
        self.pump(Duration::from_millis(300)).await
    }

    async fn pump(&mut self, window: Duration) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + window;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == self.label_id {
                                self.label = v.clone();
                            }
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return Ok(()),
            }
        }
    }

    fn label(&self) -> String {
        match &self.label {
            Value::String(s) => s.to_string(),
            v => format!("{v}"),
        }
    }
}

/// The f64 each on_change Call carried, or "nothing".
fn delivered(msgs: &[Message]) -> String {
    let parts: Vec<String> = msgs
        .iter()
        .filter_map(|m| match m {
            Message::Call(_, args) => match args.iter().next() {
                Some(Value::F64(x)) => Some(format!("{x:?}")),
                _ => None,
            },
            _ => None,
        })
        .collect();
    if parts.is_empty() { "nothing".to_string() } else { parts.join(", ") }
}

async fn three_ups(h: &mut H) -> Result<Vec<String>> {
    let mut seen = Vec::new();
    for _ in 0..3 {
        let msgs = h.arrow_up();
        seen.push(format!("ArrowUp -> {}; label {}", delivered(&msgs), {
            h.dispatch(&msgs).await?;
            h.label()
        }));
    }
    Ok(seen)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn sliders_keep_f64() -> Result<()> {
    let mut failures = Vec::new();

    let mut h = H::new(KEYS, Size::new(300.0, 200.0), Point::new(150.0, 8.0)).await?;
    let keys = three_ups(&mut h).await?;
    eprintln!("keys (slider, from 16777215, step 1): {keys:#?}");
    if h.label() != "16777218" {
        failures.push(format!("slider: three ArrowUps from 16777215 left v = {}", h.label()));
    }

    let mut h = H::new(VKEYS, Size::new(300.0, 200.0), Point::new(8.0, 100.0)).await?;
    let vkeys = three_ups(&mut h).await?;
    eprintln!("vkeys (vertical_slider, from 16777215, step 1): {vkeys:#?}");
    if h.label() != "16777218" {
        failures.push(format!(
            "vertical_slider: three ArrowUps from 16777215 left v = {}",
            h.label()
        ));
    }

    let mut h = H::new(LABEL_KEY, Size::new(300.0, 200.0), Point::new(150.0, 8.0)).await?;
    let msgs = h.arrow_up();
    let d = delivered(&msgs);
    h.dispatch(&msgs).await?;
    eprintln!("label_key ([0,1] step 0.1, from 0.1): ArrowUp -> {d}; label {}", h.label());
    if h.label() != "0.2" {
        failures.push(format!("ArrowUp from 0.1 by 0.1 shows {}", h.label()));
    }

    let mut h = H::new(LABEL_CLICK, Size::new(300.0, 200.0), Point::new(150.0, 8.0)).await?;
    let msgs = h.click(Point::new(21.0, 8.0));
    let d = delivered(&msgs);
    h.dispatch(&msgs).await?;
    eprintln!("label_click ([0,1] step 0.01, click at 7%): -> {d}; label {}", h.label());
    if h.label() != "0.07" {
        failures.push(format!("a click at 7% of [0, 1] by 0.01 shows {}", h.label()));
    }

    let mut h = H::new(PALETTE, Size::new(300.0, 200.0), Point::new(150.0, 8.0)).await?;
    let msgs = h.arrow_up();
    let d = delivered(&msgs);
    h.dispatch(&msgs).await?;
    eprintln!("palette (custom_palette.gx, [0,0.5] step 0.05, from 0): ArrowUp -> {d}; label {}", h.label());
    if h.label() != "Brightness: 0.05" {
        failures.push(format!("custom_palette's first ArrowUp shows {}", h.label()));
    }

    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}

/// iced's own sliders instantiated over f64, driven by the same events
/// with the value fed back between presses: what the widgets would do
/// without the casts.
fn f64_slider(
    vertical: bool,
    range: std::ops::RangeInclusive<f64>,
    step: f64,
    value: f64,
    seen: Rc<RefCell<Vec<f64>>>,
) -> widgets::IcedElement<'static> {
    let on_change = move |v: f64| {
        seen.borrow_mut().push(v);
        Message::Nop
    };
    if vertical {
        iced_widget::VerticalSlider::<f64, Message, GraphixTheme>::new(range, value, on_change).step(step).into()
    } else {
        iced_widget::Slider::<f64, Message, GraphixTheme>::new(range, value, on_change).step(step).into()
    }
}

fn run_ui(
    renderer: &mut widgets::Renderer,
    cache: &mut user_interface::Cache,
    element: widgets::IcedElement<'_>,
    cursor: Point,
    events: &[Event],
) {
    let mut ui =
        UserInterface::build(element, Size::new(300.0, 200.0), std::mem::take(cache), renderer);
    let mut msgs = Vec::new();
    let _ = ui.update(
        events,
        mouse::Cursor::Available(cursor),
        renderer,
        &mut clipboard::Null,
        &mut msgs,
    );
    *cache = ui.into_cache();
}

fn arrow_up_event() -> Event {
    let key = keyboard::Key::Named(keyboard::key::Named::ArrowUp);
    Event::Keyboard(keyboard::Event::KeyPressed {
        key: key.clone(),
        modified_key: key,
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::empty(),
        text: None,
        repeat: false,
    })
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn iced_f64_control() -> Result<()> {
    let mut r = renderer().await;
    let mut out = Vec::new();
    for (vertical, cursor) in [(false, Point::new(150.0, 8.0)), (true, Point::new(8.0, 100.0))] {
        let mut cache = user_interface::Cache::default();
        let mut value = 16777215.0;
        let mut ups = Vec::new();
        for _ in 0..3 {
            let seen = Rc::new(RefCell::new(Vec::new()));
            let el = f64_slider(vertical, 0.0..=100000000.0, 1.0, value, seen.clone());
            run_ui(&mut r, &mut cache, el, cursor, &[Event::Mouse(mouse::Event::CursorMoved {
                position: cursor,
            })]);
            let el = f64_slider(vertical, 0.0..=100000000.0, 1.0, value, seen.clone());
            run_ui(&mut r, &mut cache, el, cursor, &[arrow_up_event()]);
            if let Some(v) = seen.borrow().last() {
                value = *v;
            }
            ups.push(format!("{value}"));
        }
        eprintln!("f64 control, vertical {vertical}: three ArrowUps from 16777215 -> {ups:?}");
        out.push(value);
    }
    let cursor = Point::new(150.0, 8.0);
    let mut cache = user_interface::Cache::default();
    let seen = Rc::new(RefCell::new(Vec::new()));
    for ev in [Event::Mouse(mouse::Event::CursorMoved { position: cursor }), arrow_up_event()] {
        let el = f64_slider(false, 0.0..=1.0, 0.1, 0.1, seen.clone());
        run_ui(&mut r, &mut cache, el, cursor, &[ev]);
    }
    let key = seen.borrow().last().copied();
    eprintln!("f64 control: ArrowUp from 0.1 by 0.1 -> {key:?}");
    let click = Point::new(21.0, 8.0);
    let mut cache = user_interface::Cache::default();
    let seen = Rc::new(RefCell::new(Vec::new()));
    for ev in [
        Event::Mouse(mouse::Event::CursorMoved { position: click }),
        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
    ] {
        let el = f64_slider(false, 0.0..=1.0, 0.01, 0.5, seen.clone());
        run_ui(&mut r, &mut cache, el, click, &[ev]);
    }
    let clicked = seen.borrow().last().copied();
    eprintln!("f64 control: click at 7% of [0, 1] by 0.01 -> {clicked:?}");
    let mut cache = user_interface::Cache::default();
    let seen = Rc::new(RefCell::new(Vec::new()));
    for ev in [Event::Mouse(mouse::Event::CursorMoved { position: cursor }), arrow_up_event()] {
        let el = f64_slider(false, 0.0..=0.5, 0.05, 0.0, seen.clone());
        run_ui(&mut r, &mut cache, el, cursor, &[ev]);
    }
    let palette = seen.borrow().last().copied();
    eprintln!("f64 control: custom_palette ArrowUp from 0 by 0.05 -> {palette:?}");
    assert_eq!(palette, Some(0.05));
    assert_eq!(out, vec![16777218.0, 16777218.0]);
    assert_eq!(key, Some(0.2));
    assert_eq!(clicked, Some(0.07));
    Ok(())
}
