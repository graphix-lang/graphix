//! gui-datatable-12: ensure_selection_visible scrolls to an arbitrary
//! selected cell, and prefix matching picks the wrong row.
//! `ensure_selection_visible`
//! (stdlib/graphix-package-gui/src/widgets/data_table/layout.rs:246-278)
//! scrolls to the first selected path in AHashSet order even when a
//! selected cell is in view, and its `strip_prefix` match resolves a
//! row's cell to an ancestor row.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_12.rs:
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_datatable_12 -- --nocapture
//!
//! The table is driven as `GuiHandler::about_to_wait` drives it: headless
//! wgpu layout (`UserInterface::build`), real wheel and mouse events, the
//! `Message`s they produce dispatched through `on_message`, callables
//! called through the runtime, the runtime's updates fed to
//! `handle_update`. The rows on screen are read back from the laid-out
//! tree (iced's `Operation::text` reports every text widget).
//!
//! multi_select_click_keeps_view: 1000 rows, `on_select` appends to the
//! selection (multi-select, which the book names as a supported use).
//! Click the cell of row 5, wheel down to row 495, click the cell of row
//! 500. Expected: the view stays where it is; the cell just clicked is
//! selected and on screen. Observed: in about half the trials (AHashSet
//! iteration order) the view jumps back to row 5.
//!
//! hierarchical_row_click_keeps_view: rows "/t/a", 500 fillers, "/t/a/b";
//! single-select. Wheel to the bottom and click the cell of "/t/a/b".
//! Expected: the view stays on "/t/a/b". Observed: the view jumps to the
//! top, to row "/t/a", every time.
//!
//! Observed at c722befe (dev profile), both tests FAIL:
//!   control (single-select): on screen before the click on c0r500:
//!     c0r495..c0r503; after: c0r495..c0r503
//!   trial 0 (multi-select, selection {k0r5/x, k0r500/x}): before the
//!     click on k0r500 k0r495..k0r503; after k0r5..k0r13  <- JUMPED
//!   ... (trials 1, 3, 4, 5, 8, 9, 10 the same; 2, 6, 7, 11 kept the view)
//!   multi-select: the view jumped away from the cell just clicked in 8
//!     of 12 trials: [0, 1, 3, 4, 5, 8, 9, 10]
//!   control (rows /t/c, /t/f0../t/f499, /t/a/b): before the click on
//!     /t/a/b/x [f493..f499, b]; after [f493..f499, b]
//!   probe (rows /t/a, /t/f0../t/f499, /t/a/b): before the click on
//!     /t/a/b/x ["f493", .., "f499", "b"]; after ["a", "f0", .., "f7"]
//!   panicked: selecting /t/a/b/x scrolled the view away from row /t/a/b

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    render::GpuState,
    widgets::{self, GuiW, Message, MessageShell},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Event, Point, Rectangle, Size, clipboard, mouse,
    widget::{Id, Operation},
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::wgpu;
use poolshark::global::GPooled;
use std::{collections::VecDeque, time::Duration};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const VIEWPORT: Size = Size::new(600.0, 200.0);
/// Over the table body, away from the scrollbars.
const REST: Point = Point::new(300.0, 100.0);
/// data_table/mod.rs ROW_HEIGHT_ESTIMATE, every row's height here.
const ROW_H: f32 = 22.0;

const MULTI: &str = "sel <- path ~ array::push(sel, path)";
const SINGLE: &str = "sel <- [path]";

async fn gpu() -> GpuState {
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
    GpuState {
        instance,
        adapter,
        device,
        queue,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
    }
}

/// A table over `rows` with one Text column "x" whose every cell reads
/// "v"; `on_select` is the body of `|#path: string| ..`.
fn table_code(rows: &[String], on_select: &str) -> String {
    let rows = rows.iter().map(|r| format!("\"{r}\"")).collect::<Vec<_>>().join(", ");
    format!(
        "use gui::*; use gui::data_table::{{self, *}}; use sys::*;\n\
         let sel: Array<string> = [];\n\
         let tbl = {{ rows: [{rows}], columns: [\n\
           {{ name: \"x\", typ: `Text({{ on_edit: null }}), display_name: null,\n\
              source: &\"v\", on_resize: &null, width: &null }}\n\
         ] }};\n\
         let result = data_table(\n\
           #selection: &sel,\n\
           #on_select: |#path: string| {on_select},\n\
           #table: &tbl\n\
         )\n"
    )
}

/// Every text widget in the laid-out tree, with its bounds.
struct Texts(Vec<(String, Rectangle)>);

impl Operation for Texts {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation)) {
        operate(self);
    }

    fn text(&mut self, _id: Option<&Id>, bounds: Rectangle, text: &str) {
        self.0.push((text.to_string(), bounds));
    }
}

struct Table {
    ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt: tokio::runtime::Handle,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
}

impl Table {
    async fn new(code: String, gpu: &GpuState) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
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
        let widget =
            widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
        let mut t = Table {
            ctx,
            _compiled: compiled,
            rx,
            widget,
            rt: tokio::runtime::Handle::current(),
            renderer: gpu.create_renderer(),
            cache: Cache::default(),
            cursor: REST,
        };
        t.drain().await?;
        let _ = t.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: REST })]);
        Ok(t)
    }

    /// One frame as `GuiHandler::about_to_wait` runs it.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let mut clip = clipboard::Null;
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clip,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// Lay the table out; every text it renders, with its bounds.
    fn texts(&mut self) -> Vec<(String, Rectangle)> {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut t = Texts(Vec::new());
        ui.operate(&self.renderer, &mut t);
        self.cache = ui.into_cache();
        t.0
    }

    /// The row names on screen, top to bottom.
    fn visible_rows(&mut self, is_row: &dyn Fn(&str) -> bool) -> Vec<String> {
        let mut rows: Vec<(String, Rectangle)> =
            self.texts().into_iter().filter(|(s, _)| is_row(s)).collect();
        rows.sort_by(|a, b| a.1.y.total_cmp(&b.1.y));
        rows.into_iter().map(|(s, _)| s).collect()
    }

    /// Dispatch FIFO as `about_to_wait` does, then deliver the runtime's
    /// updates to the widget.
    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<()> {
        let mut pending: VecDeque<Message> = msgs.into();
        while let Some(m) = pending.pop_front() {
            match m {
                Message::Nop => {}
                Message::Call(id, args) => self.ctx.rt.call(id, args)?,
                other => {
                    let mut shell = MessageShell::new(self.cursor);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        self.drain().await
    }

    async fn drain(&mut self) -> Result<()> {
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(150), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    self.widget.handle_update(&self.rt, i, &v)?;
                }
            }
        }
        Ok(())
    }

    fn click(&mut self, p: Point) -> Vec<Message> {
        self.cursor = p;
        let mut msgs =
            self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: p })]);
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]),
        );
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]),
        );
        self.cursor = REST;
        let _ = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: REST })]);
        msgs
    }

    /// Click the "x" cell of the row whose name cell reads `name`, and
    /// dispatch what the click produced; the `CellClick` it carried.
    async fn click_cell(&mut self, name: &str) -> Result<(usize, String)> {
        let texts = self.texts();
        let nb = texts
            .iter()
            .find(|(s, _)| s == name)
            .map(|(_, b)| *b)
            .with_context(|| format!("row {name} is not on screen"))?;
        let cb = texts
            .iter()
            .find(|(s, b)| s == "v" && (b.center_y() - nb.center_y()).abs() < 3.0)
            .map(|(_, b)| *b)
            .with_context(|| format!("no data cell beside row {name}"))?;
        let msgs = self.click(cb.center());
        let hit = msgs
            .iter()
            .find_map(|m| match m {
                Message::CellClick(r, c) => Some((*r, c.to_string())),
                _ => None,
            })
            .with_context(|| format!("the click on row {name} produced no CellClick"))?;
        self.dispatch(msgs).await?;
        Ok(hit)
    }

    /// Wheel down `px` pixels over the table and dispatch the overlay's
    /// `Scroll`; the offset it reported.
    async fn wheel_down(&mut self, px: f32) -> Result<f32> {
        self.cursor = REST;
        let msgs = self.frame(&[Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Pixels { x: 0.0, y: -px },
        })]);
        let oy = msgs
            .iter()
            .find_map(|m| match m {
                Message::Scroll(_, y, _, _) => Some(*y),
                _ => None,
            })
            .context("the wheel produced no Scroll message")?;
        self.dispatch(msgs).await?;
        Ok(oy)
    }
}

/// One multi- or single-select run over 1000 rows named `<prefix><i>`:
/// click row 5, wheel to row 495, click row 500. The rows on screen
/// after the second click.
async fn click_far_apart(
    gpu: &GpuState,
    prefix: &str,
    on_select: &str,
) -> Result<(Vec<String>, Vec<String>)> {
    let names: Vec<String> = (0..1000).map(|i| format!("{prefix}{i}")).collect();
    let is_row = |s: &str| s.starts_with(prefix);
    let mut tb = Table::new(table_code(&names, on_select), gpu).await?;
    let hit = tb.click_cell(&names[5]).await?;
    assert_eq!(hit, (5, "x".to_string()));
    let oy = tb.wheel_down(495.0 * ROW_H).await?;
    let before = tb.visible_rows(&is_row);
    assert!(
        before.contains(&names[500]),
        "{prefix}: after the wheel (offset {oy}) row 500 must be on screen: {before:?}"
    );
    let hit = tb.click_cell(&names[500]).await?;
    assert_eq!(hit, (500, "x".to_string()));
    let after = tb.visible_rows(&is_row);
    Ok((before, after))
}

#[tokio::test(flavor = "current_thread")]
async fn multi_select_click_keeps_view() -> Result<()> {
    let gpu = gpu().await;
    let (before, after) = click_far_apart(&gpu, "c0r", SINGLE).await?;
    eprintln!(
        "control (single-select): on screen before the click on c0r500: \
         {before:?}; after: {after:?}"
    );
    assert_eq!(before, after, "single-select: the view moved");
    let trials = 12;
    let mut jumped = Vec::new();
    for t in 0..trials {
        let prefix = format!("k{t}r");
        let (before, after) = click_far_apart(&gpu, &prefix, MULTI).await?;
        let ok = before == after;
        eprintln!(
            "trial {t} (multi-select, selection {{{prefix}5/x, {prefix}500/x}}): \
             before the click on {prefix}500 {}..{}; after {}..{}{}",
            before.first().unwrap(),
            before.last().unwrap(),
            after.first().map(|s| s.as_str()).unwrap_or("?"),
            after.last().map(|s| s.as_str()).unwrap_or("?"),
            if ok { "" } else { "  <- JUMPED away from the clicked cell" }
        );
        if !ok {
            jumped.push(t);
        }
    }
    eprintln!(
        "multi-select: the view jumped away from the cell just clicked in {} of \
         {trials} trials: {jumped:?}",
        jumped.len()
    );
    assert!(
        jumped.is_empty(),
        "the view jumped away from the selected cell the user just clicked in \
         {} of {trials} trials",
        jumped.len()
    );
    Ok(())
}

async fn hierarchical(gpu: &GpuState, first: &str) -> Result<(Vec<String>, Vec<String>)> {
    let mut names = vec![first.to_string()];
    names.extend((0..500).map(|i| format!("/t/f{i}")));
    names.push("/t/a/b".to_string());
    let base = first.rsplit('/').next().unwrap().to_string();
    let is_row = move |s: &str| {
        s == base || s == "b" || (s.starts_with('f') && s[1..].parse::<u32>().is_ok())
    };
    let mut tb = Table::new(table_code(&names, SINGLE), gpu).await?;
    let _ = tb.wheel_down(1.0e7).await?;
    let before = tb.visible_rows(&is_row);
    let hit = tb.click_cell("b").await?;
    assert_eq!(hit, (501, "x".to_string()));
    let after = tb.visible_rows(&is_row);
    Ok((before, after))
}

#[tokio::test(flavor = "current_thread")]
async fn hierarchical_row_click_keeps_view() -> Result<()> {
    let gpu = gpu().await;
    let (before, after) = hierarchical(&gpu, "/t/c").await?;
    eprintln!(
        "control (rows /t/c, /t/f0../t/f499, /t/a/b): before the click on \
         /t/a/b/x {before:?}; after {after:?}"
    );
    assert_eq!(before, after, "control: the view moved");
    let (before, after) = hierarchical(&gpu, "/t/a").await?;
    eprintln!(
        "probe (rows /t/a, /t/f0../t/f499, /t/a/b): before the click on \
         /t/a/b/x {before:?}; after {after:?}"
    );
    assert_eq!(
        before, after,
        "selecting /t/a/b/x scrolled the view away from row /t/a/b"
    );
    Ok(())
}
