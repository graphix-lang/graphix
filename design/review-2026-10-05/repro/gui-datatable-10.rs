//! gui-datatable-10: sparkline histories are never pruned or aged
//! against now, so a row that left the table, or scrolled out of the
//! subscription window, keeps stretching the column's shared auto
//! y-axis; and `push_defaults_to_sparklines` writes a column's numeric
//! fallback into the history of live netidx rows whenever any column's
//! source ref (or the table) fires.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_10.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_10 -- --nocapture
//!
//! Each program is compiled, its widget built with `widgets::compile`,
//! fed the runtime's updates through `handle_update` and drawn headlessly
//! as `GuiHandler::about_to_wait` does (`before_view`, `view`,
//! `UserInterface::build`, `draw`), then read back with
//! `Renderer::screenshot`. A sparkline is the only thing drawn in its
//! colour (0.3, 0.7, 1.0); each run of screen rows holding that colour is
//! one cell's line, and its height in pixels is printed.
//!
//! STALE (auto y-axis, `history_seconds: 1.5`): rows r0..r79 publish
//! `load` every 100 ms, values 10..19; r0 publishes 1000 for 300 ms. The
//! column's axis is the union of the rows' histories, so while the spike
//! is in r0's history every line is squashed to the bottom of its cell.
//!   control: r0 stays in view; 3 s later the spike has aged out.
//!   scroll:  a user scroll to the last rows drops r0's subscription.
//!   remove:  the table's rows become [r1].
//! Expected: 3 s after the spike (twice history_seconds) every line spans
//! its cell again in all three cases.
//!
//! FALLBACK: one live row publishing 50..59, a sparkline column with a
//! per-row fallback `Netidx({"r0" => 0.0})` and a fixed 0..100 axis, and a
//! text column whose source is the variable `note`. Expected: the live
//! line stays in the middle band of its cell (a short run) whatever
//! `note` or the table does.
//!
//! Observed at c722befe (dev/test profile), the test FAILS in 38 s.
//! Line heights in px, top to bottom (a cell's canvas is 16 px; 14 is the
//! last row, clipped; 1 is a line squashed onto the bottom by r0's 1000):
//!   [dt10c] before the spike           [16, 16, 16, 16, 16, 16, 16, 16, 14]
//!   [dt10c] spike in r0's history      [16, 1, 1, 1, 1, 1, 1, 1, 1]
//!   [dt10c] control, r0 in view, 3 s   [16, 16, 16, 16, 16, 16, 16, 16, 14]
//!   [dt10s] scrolled to r71..r79, 3 s  [1, 1, 1, 1, 1, 1, 1, 1, 1]
//!   [dt10s] 8 s                        [1, 1, 1, 1, 1, 1, 1, 1, 1]
//!   [dt10s] scrolled back to r0..r8    [16, 16, 16, 16, 16, 16, 16, 16, 14]
//!   [dt10s] and to r71..r79 again      [16, 16, 16, 16, 16, 16, 16, 16, 14]
//!   [dt10r] rows = [r1], 3 s           [1]
//!   [dt10r] rows = [r1], 8 s           [1]
//! The same rows r71..r79 are drawn squashed or not depending on whether
//! r0 was re-subscribed (its history trimmed) in between.
//! FALLBACK, (first screen row, height) of the live line:
//!   live r0 at 50..59, axis 0..100               [(33, 3)]
//!   after `note` (another column's source) fired [(33, 10)]
//!   2.5 s later                                  [(33, 3)]
//!   after the table re-fired (same rows)         [(33, 10)]
//! Rows 33..42 reach the bottom of the cell: a 0.0 point, which only the
//! fallback map holds, in the history of a row that is live throughout.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, Message, MessageShell},
};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use iced_core::{Color, Font, Pixels, Point, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const W: u32 = 400;
const H: u32 = 220;
const ROW_H: f32 = 22.0;

const STALE: &str = r#"
use gui::data_table::{data_table, sparkline_column};
use sys::time;

let tick = time::timer(duration:100.ms, true);
let n = count(tick);
let spike = false;
let phase = 0;

let published = array::init(80, |i| sys::net::publish(
  "/local/PFX/r[i]/load",
  select i {
    0 => select spike {
      true => 1000.0,
      false => 10.0 + cast<f64>(n % 10)$
    },
    _ => 10.0 + cast<f64>((n + i) % 10)$
  }
));

let all = array::init(80, |i| "/local/PFX/r[i]");

let rows = select phase {
  0 => all,
  _ => ["/local/PFX/r1"]
};

let tbl = {
  rows,
  columns: [sparkline_column(#name: "load", #history_seconds: 1.5, #width: &200.0)]
};

let result = data_table(#table: &tbl)
"#;

const FALLBACK: &str = r#"
use gui::data_table::{Source, data_table, sparkline_column, text_column};
use sys::time;

let tick = time::timer(duration:100.ms, true);
let n = count(tick);

sys::net::publish("/local/PFX/r0/load", 50.0 + cast<f64>(n % 10)$);

let note: Source = "a";
let ver = 0;

let tbl = {
  rows: select ver { _ => ["/local/PFX/r0"] },
  columns: [
    sparkline_column(
      #name: "load",
      #history_seconds: 1.5,
      #min: 0.0,
      #max: 100.0,
      #source: &`Netidx({"r0" => 0.0}),
      #width: &200.0
    ),
    text_column(#name: "note", #source: &note, #width: &100.0)
  ]
};

let result = data_table(#table: &tbl)
"#;

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
    let timeout = tokio::time::sleep(Duration::from_secs(30));
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

struct Session {
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    cache: Cache,
    refs: Vec<Ref<NoExt>>,
}

impl Session {
    async fn new(code: &str, prefix: &str) -> Result<Self> {
        let code = code.replace("PFX", prefix);
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(vfs)]).await?;
        let compiled = ctx
            .rt
            .compile(literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let v = first_value(&mut rx, compiled.exprs[0].id).await?;
        let widget =
            widgets::compile(ctx.rt.clone(), v).await.context("compile widget")?;
        Ok(Self { ctx, compiled, rx, widget, cache: Cache::default(), refs: vec![] })
    }

    /// Deliver the runtime's updates to the widget for `d`, as the event
    /// loop does.
    async fn run_for(&mut self, d: Duration) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + d;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => break,
            }
        }
        self.widget.before_view();
        Ok(())
    }

    async fn set(&mut self, var: &str, v: Value) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let mut r = self.ctx.rt.compile_ref(bid).await?;
        r.set(v)?;
        self.refs.push(r);
        Ok(())
    }

    /// A user scroll: what the overlay's `on_scroll` publishes.
    fn scroll_to_row(&mut self, row: usize) {
        let mut shell = MessageShell::new(Point::ORIGIN);
        let msg = Message::Scroll(0.0, row as f32 * ROW_H, W as f32, H as f32);
        let w = &mut self.widget;
        tokio::task::block_in_place(|| w.on_message(&msg, &mut shell));
        self.widget.before_view();
    }

    /// One frame as the event loop draws it; the heights in pixels of
    /// the runs of rows holding sparkline-coloured pixels, top to bottom,
    /// with the first row of each run.
    fn lines(&mut self, renderer: &mut widgets::Renderer) -> Vec<(u32, u32)> {
        self.widget.before_view();
        let theme = GraphixTheme { inner: iced_core::Theme::Light, overrides: None };
        let style = Style { text_color: Color::BLACK };
        let rgba = {
            let mut ui = UserInterface::build(
                self.widget.view(),
                Size::new(W as f32, H as f32),
                std::mem::take(&mut self.cache),
                renderer,
            );
            let mut messages: Vec<Message> = Vec::new();
            let cursor = mouse::Cursor::Unavailable;
            let _ = ui.update(&[], cursor, renderer, &mut clipboard::Null, &mut messages);
            ui.draw(renderer, &theme, &style, cursor);
            self.cache = ui.into_cache();
            renderer.screenshot(
                &Viewport::with_physical_size(Size::new(W, H), 1.0),
                Color::WHITE,
            )
        };
        let is_line = |x: u32, y: u32| {
            let i = ((y * W + x) * 4) as usize;
            let (r, g, b) = (rgba[i] as i32, rgba[i + 1] as i32, rgba[i + 2] as i32);
            b >= 200 && b - r >= 80 && g > r
        };
        let mut runs: Vec<(u32, u32)> = Vec::new();
        let mut cur: Option<(u32, u32)> = None;
        for y in 0..H {
            if (0..300).any(|x| is_line(x, y)) {
                cur = Some(match cur {
                    None => (y, 1),
                    Some((y0, h)) => (y0, h + 1),
                });
            } else if let Some(r) = cur.take() {
                runs.push(r);
            }
        }
        if let Some(r) = cur.take() {
            runs.push(r);
        }
        runs
    }
}

fn heights(runs: &[(u32, u32)]) -> Vec<u32> {
    runs.iter().map(|(_, h)| *h).collect()
}

fn all_tall(runs: &[(u32, u32)]) -> bool {
    !runs.is_empty() && runs.iter().all(|(_, h)| *h >= 10)
}

fn all_flat(runs: &[(u32, u32)]) -> bool {
    !runs.is_empty() && runs.iter().all(|(_, h)| *h <= 4)
}

/// Lay out once (the table learns its viewport), then spike r0.
async fn spiked(prefix: &str, renderer: &mut widgets::Renderer) -> Result<Session> {
    let mut s = Session::new(STALE, prefix).await?;
    s.run_for(Duration::from_millis(1000)).await?;
    let _ = s.lines(renderer);
    s.run_for(Duration::from_millis(1700)).await?;
    let before = s.lines(renderer);
    println!("[{prefix}] before the spike, line heights {:?}", heights(&before));
    s.set("spike", Value::Bool(true)).await?;
    s.run_for(Duration::from_millis(300)).await?;
    s.set("spike", Value::Bool(false)).await?;
    s.run_for(Duration::from_millis(200)).await?;
    let during = s.lines(renderer);
    println!("[{prefix}] spike in r0's history, line heights {:?}", heights(&during));
    // r0's own line spans 10..1000; every other row's is squashed
    if !all_tall(&before) || during.len() < 2 || !all_flat(&during[1..]) {
        bail!("[{prefix}] probe does not see the axis: before {before:?} during {during:?}");
    }
    Ok(s)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn sparkline_history_lifecycle() -> Result<()> {
    let mut renderer = renderer().await;
    let mut failures: Vec<String> = Vec::new();

    // control: r0 stays in view
    {
        let mut s = spiked("dt10c", &mut renderer).await?;
        s.run_for(Duration::from_millis(3000)).await?;
        let after = s.lines(&mut renderer);
        println!("[dt10c] control, r0 in view, 3 s later: line heights {:?}", heights(&after));
        if !all_tall(&after) {
            failures.push(format!("control: the spike did not age out: {after:?}"));
        }
    }

    // scroll: r0 leaves the subscription window
    {
        let mut s = spiked("dt10s", &mut renderer).await?;
        s.scroll_to_row(71);
        s.run_for(Duration::from_millis(3000)).await?;
        let after = s.lines(&mut renderer);
        println!(
            "[dt10s] scrolled to r71..r79, 3 s later: line heights {:?}",
            heights(&after)
        );
        s.run_for(Duration::from_millis(5000)).await?;
        let later = s.lines(&mut renderer);
        println!("[dt10s] 8 s later: line heights {:?}", heights(&later));
        if !all_tall(&later) {
            failures.push(format!(
                "scroll: 8 s after the spike every visible line is still squashed: {later:?}"
            ));
        }
        s.scroll_to_row(0);
        s.run_for(Duration::from_millis(1000)).await?;
        let back = s.lines(&mut renderer);
        println!("[dt10s] scrolled back to r0..r8: line heights {:?}", heights(&back));
        s.scroll_to_row(71);
        s.run_for(Duration::from_millis(1000)).await?;
        let again = s.lines(&mut renderer);
        println!("[dt10s] and to r71..r79 again: line heights {:?}", heights(&again));
    }

    // remove: r0 leaves the table
    {
        let mut s = spiked("dt10r", &mut renderer).await?;
        s.set("phase", Value::I64(1)).await?;
        s.run_for(Duration::from_millis(3000)).await?;
        let after = s.lines(&mut renderer);
        println!("[dt10r] rows = [r1], 3 s later: line heights {:?}", heights(&after));
        s.run_for(Duration::from_millis(5000)).await?;
        let later = s.lines(&mut renderer);
        println!("[dt10r] rows = [r1], 8 s later: line heights {:?}", heights(&later));
        if !all_tall(&later) {
            failures.push(format!(
                "remove: 8 s after the spike r1's line is still squashed by r0, \
                 which is no longer in the table: {later:?}"
            ));
        }
    }

    // fallback: a live row's line gets the fallback injected
    {
        let mut s = Session::new(FALLBACK, "dt10b").await?;
        s.run_for(Duration::from_millis(1000)).await?;
        let _ = s.lines(&mut renderer);
        s.run_for(Duration::from_millis(2500)).await?;
        let quiet = s.lines(&mut renderer);
        println!("[dt10b] live r0 at 50..59, axis 0..100: line {quiet:?}");
        s.set("note", Value::String(literal!("b"))).await?;
        s.run_for(Duration::from_millis(300)).await?;
        let after_note = s.lines(&mut renderer);
        println!("[dt10b] after `note` (another column's source) fired: line {after_note:?}");
        s.run_for(Duration::from_millis(2500)).await?;
        let aged = s.lines(&mut renderer);
        println!("[dt10b] 2.5 s later: line {aged:?}");
        s.set("ver", Value::I64(1)).await?;
        s.run_for(Duration::from_millis(300)).await?;
        let after_tbl = s.lines(&mut renderer);
        println!("[dt10b] after the table re-fired (same rows): line {after_tbl:?}");
        let tallest = |runs: &[(u32, u32)]| runs.iter().map(|(_, h)| *h).max().unwrap_or(0);
        if quiet.len() != 1 || tallest(&quiet) > 6 || tallest(&aged) > 6 {
            bail!("fallback probe does not see one flat live line: {quiet:?} {aged:?}");
        }
        if tallest(&after_note) > tallest(&quiet) + 3 {
            failures.push(format!(
                "fallback: another column's source update pulled the live line \
                 to the fallback 0.0: {after_note:?}, was {quiet:?}"
            ));
        }
        if tallest(&after_tbl) > tallest(&quiet) + 3 {
            failures.push(format!(
                "fallback: a table update pulled the live line to the fallback 0.0: \
                 {after_tbl:?}, was {quiet:?}"
            ));
        }
    }

    for f in failures.iter() {
        println!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "{} failure(s), see above", failures.len());
    Ok(())
}
