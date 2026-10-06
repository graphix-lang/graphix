//! gui-datatable-01: `truncate_to_width` never terminates on multi-byte text
//! (stdlib/graphix-package-gui/src/widgets/data_table/types.rs:69-97).
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_01.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_01 -- --nocapture
//!
//! Each probe compiles a one-column `data_table` whose Text cell holds the
//! probe text, builds the widget as the event loop does and lays it out
//! once (`before_view`, `view`, `UserInterface::build` with the headless
//! wgpu renderer), which runs `render_with_size` inside `responsive`, which
//! calls `truncate_to_width` on the cell.
//!
//! The widths to probe come from the search of types.rs:69-97 copied
//! below and driven by the real `measure_text` (same `Paragraph` call,
//! same font system): a step that leaves `(lo, hi)` unchanged without
//! leaving the loop repeats forever, since the step is a function of
//! `(lo, hi)` alone.
//!
//! Expected: every layout returns; a cell too narrow for its text shows a
//! prefix and "...". The test passes once the search terminates.
//!
//! Observed (HEAD c722befe, debug profile, this machine's system fonts):
//!   predict: "Zürich-Österreich" at width 33: stuck at lo=1 hi=3, the char at
//!     lo is 'ü' (2 bytes)
//!   predict: "Zürich-Österreich" (full width 106.0px): 13 of 281 column widths
//!     in 20..=300 never return: [33..=45]
//!   predict: "naïve café résumé": 10 of 281 never return: [41..=50]
//!   predict: "日本語のテキストです": 54 of 281 never return: [65..=116, 143, 144]
//!   predict: auto width (300px) "東京都千代田区丸の内一丁目九番一号 東京駅 八重洲中央口
//!     改札前": never returns (stuck at lo=62 hi=65); the Greek sentence too
//!     (lo=80 hi=82); the German, French and Russian ones return
//!   control: ASCII text, width 60: layout returned in 464µs
//!   control: "Zürich-Österreich", width 46 (predicted "Zür..."): layout
//!     returned in 168µs
//!   HANG: "東京都千代田区…" in an auto-width column: layout has not returned
//!     after 20.0s; the laying-out thread used 1859 CPU ticks (1/100 s)
//!     meanwhile
//!   process didn't exit successfully (signal: 6, SIGABRT)
//! The test binary run under `gdb -batch -ex run -ex 'thread apply all bt'`
//! shows the spinning thread at the abort in
//!   measure_text <- truncate_to_width <- DataTableW::render_with_size
//!   <- Responsive::layout <- UserInterface::build
//! the call `GuiHandler::about_to_wait` makes on the winit thread.

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    render::GpuState,
    widgets::{self, GuiW},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Size, text::Paragraph as _};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use poolshark::global::GPooled;
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

type Paragraph = <widgets::Renderer as iced_core::text::Renderer>::Paragraph;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

/// data_table/mod.rs: CELL_H_PADDING + RESIZE_HANDLE_WIDTH.
const CELL_CHROME: f32 = 10.0 + 5.0;
/// data_table/mod.rs: DEFAULT_MAX_COL_WIDTH, the auto-width cap.
const AUTO_MAX: f32 = 300.0;
const WATCHDOG: Duration = Duration::from_secs(20);

/// types.rs:18-31.
fn measure_text(text: &str) -> f32 {
    let para = Paragraph::with_text(iced_core::Text {
        content: text.into(),
        bounds: Size::new(f32::INFINITY, f32::INFINITY),
        size: iced_core::Pixels(13.0),
        line_height: iced_core::text::LineHeight::default(),
        font: iced_core::Font::DEFAULT,
        align_x: iced_core::alignment::Horizontal::Left.into(),
        align_y: iced_core::alignment::Vertical::Top,
        shaping: iced_core::text::Shaping::Advanced,
        wrapping: iced_core::text::Wrapping::None,
    });
    para.min_bounds().width
}

/// types.rs:55-105 with one change: a step that leaves `(lo, hi)` as it
/// was without leaving the loop returns `Err((lo, hi))`, where the real
/// function spins forever.
fn predict(text: &str, max_px: f32) -> Result<String, (usize, usize)> {
    let avail = max_px - CELL_CHROME;
    if avail <= 0.0 || text.is_empty() {
        return Ok(String::new());
    }
    if measure_text(text) <= avail {
        return Ok(text.into());
    }
    let target = avail - measure_text("...");
    if target <= 0.0 {
        return Ok("...".into());
    }
    let mut lo = 0usize;
    let mut hi = text.len();
    while lo < hi {
        let before = (lo, hi);
        let mid = (lo + hi + 1) / 2;
        let mid = if mid >= text.len() {
            text.len()
        } else {
            let mut m = mid;
            while m > 0 && !text.is_char_boundary(m) {
                m -= 1;
            }
            m
        };
        if mid == 0 {
            break;
        }
        let w = measure_text(&text[..mid]);
        if w <= target {
            lo = mid;
            if lo == hi {
                break;
            }
        } else {
            hi = mid - 1;
            while hi > 0 && !text.is_char_boundary(hi) {
                hi -= 1;
            }
        }
        if (lo, hi) == before && lo < hi {
            return Err((lo, hi));
        }
    }
    if lo == 0 {
        Ok("...".into())
    } else {
        while lo > 0 && !text.is_char_boundary(lo) {
            lo -= 1;
        }
        Ok(format!("{}...", &text[..lo]))
    }
}

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

struct Table {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    _rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
}

/// A one-row, one-column table whose Text cell is `text`; `width` is
/// the column's `#width` (`None`: auto width).
async fn table(text: &str, width: Option<f32>) -> Result<Table> {
    let w = match width {
        Some(w) => format!("{w:.1}"),
        None => "null".into(),
    };
    let code = format!(
        "use gui::*; use gui::data_table::{{self, *}}; use sys::*;\n\
         let w: [f64, null] = {w};\n\
         let tbl = {{ rows: [\"r0\"], columns: [\n\
           {{ name: \"c0\", typ: `Text({{ on_edit: null }}), display_name: null,\n\
              source: &\"{text}\", on_resize: &null, width: &w }}\n\
         ] }};\n\
         let result = data_table(#table: &tbl)\n"
    );
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    let vfs = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
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
    let mut widget =
        widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
    let rt = tokio::runtime::Handle::current();
    while let Ok(Some(mut batch)) =
        tokio::time::timeout(Duration::from_millis(300), rx.recv()).await
    {
        for e in batch.drain(..) {
            if let GXEvent::Updated(i, v) = e {
                widget.handle_update(&rt, i, &v)?;
            }
        }
    }
    Ok(Table { _ctx: ctx, _compiled: compiled, _rx: rx, widget })
}

/// utime + stime of thread `tid` of this process, in clock ticks.
fn cpu_ticks(tid: &str) -> u64 {
    let stat = std::fs::read_to_string(format!("/proc/self/task/{tid}/stat"))
        .unwrap_or_default();
    let rest = stat.rsplit_once(") ").map(|(_, r)| r).unwrap_or("");
    let f: Vec<&str> = rest.split_whitespace().collect();
    f.get(11).and_then(|s| s.parse().ok()).unwrap_or(0)
        + f.get(12).and_then(|s| s.parse().ok()).unwrap_or(0)
}

/// Lay the table out once as `about_to_wait` does. A watchdog aborts the
/// process if the layout has not returned within `WATCHDOG`, reporting
/// the CPU time the laying-out thread spent meanwhile.
fn layout(t: &mut Table, renderer: &mut widgets::Renderer, what: &str) -> Duration {
    let tid = std::fs::read_link("/proc/thread-self")
        .ok()
        .and_then(|p| p.file_name().map(|s| s.to_string_lossy().into_owned()))
        .unwrap_or_default();
    let done = Arc::new(AtomicBool::new(false));
    let watch = done.clone();
    let what_w = what.to_string();
    std::thread::spawn(move || {
        let t0 = Instant::now();
        let c0 = cpu_ticks(&tid);
        while t0.elapsed() < WATCHDOG {
            if watch.load(Ordering::SeqCst) {
                return;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        let ticks = cpu_ticks(&tid) - c0;
        eprintln!(
            "HANG: {what_w}: layout has not returned after {:?}; the laying-out \
             thread used {ticks} CPU ticks (1/100 s) meanwhile",
            t0.elapsed()
        );
        std::process::abort();
    });
    t.widget.before_view();
    let t0 = Instant::now();
    let element = t.widget.view();
    let ui = UserInterface::build(
        element,
        Size::new(600.0, 200.0),
        user_interface::Cache::default(),
        renderer,
    );
    drop(ui);
    let el = t0.elapsed();
    done.store(true, Ordering::SeqCst);
    el
}

#[tokio::test(flavor = "current_thread")]
async fn truncate_to_width_terminates() -> Result<()> {
    let gpu = gpu().await;
    let mut renderer = gpu.create_renderer();
    let explicit = ["Zürich-Österreich", "naïve café résumé", "日本語のテキストです"];
    let auto = [
        "Größenänderung der Spaltenbreite für die Übersicht über alle Einträge",
        "Les élèves étudient l'éloquence à Montréal et à Québec chaque été",
        "Привет, это длинная строка на русском языке для проверки таблицы",
        "東京都千代田区丸の内一丁目九番一号 東京駅 八重洲中央口 改札前",
        "Ελληνικά κείμενα με τόνους και διαλυτικά για τη δοκιμή του πίνακα",
    ];
    let mut explicit_hang: Option<(&str, f32)> = None;
    let mut explicit_ok: Option<(&str, f32, String)> = None;
    for t in explicit {
        let mut hang_widths = Vec::new();
        let mut px = 20.0f32;
        while px <= AUTO_MAX {
            match predict(t, px) {
                Err((lo, hi)) => {
                    hang_widths.push(px as u32);
                    if explicit_hang.is_none() {
                        let c = t[lo..].chars().next().unwrap();
                        eprintln!(
                            "predict: {t:?} at width {px}: stuck at lo={lo} hi={hi}, \
                             the char at lo is {c:?} ({} bytes)",
                            c.len_utf8()
                        );
                        explicit_hang = Some((t, px));
                    }
                }
                Ok(s) => {
                    if explicit_ok.is_none() && s.ends_with("...") && s != "..." {
                        explicit_ok = Some((t, px, s));
                    }
                }
            }
            px += 1.0;
        }
        eprintln!(
            "predict: {t:?} (full width {:.1}px): {} of {} column widths in 20..=300 \
             never return: {hang_widths:?}",
            measure_text(t),
            hang_widths.len(),
            (AUTO_MAX - 20.0) as usize + 1
        );
    }
    let mut auto_hang: Option<&str> = None;
    for t in auto {
        let r = predict(t, AUTO_MAX);
        eprintln!(
            "predict: auto width (300px) {t:?} (full width {:.1}px): {}",
            measure_text(t),
            match &r {
                Ok(s) => format!("returns {s:?}"),
                Err((lo, hi)) => format!("never returns (stuck at lo={lo} hi={hi})"),
            }
        );
        if r.is_err() && auto_hang.is_none() {
            auto_hang = Some(t);
        }
    }

    let ascii = "Zurich-Osterreich-Zurich-Osterreich";
    let mut t = table(ascii, Some(60.0)).await?;
    let el = layout(&mut t, &mut renderer, "control: ASCII text, width 60");
    eprintln!("control: ASCII text {ascii:?}, width 60: layout returned in {el:?}");
    if let Some((text, px, s)) = explicit_ok {
        let mut t = table(text, Some(px)).await?;
        let el = layout(&mut t, &mut renderer, "control: non-ASCII, terminating width");
        eprintln!(
            "control: {text:?}, width {px} (predicted {s:?}): layout returned in {el:?}"
        );
    }
    let (text, width) = match (auto_hang, explicit_hang) {
        (Some(t), _) => (t, None),
        (None, Some((t, px))) => (t, Some(px)),
        (None, None) => {
            eprintln!("no width hangs with this machine's fonts; nothing to probe");
            return Ok(());
        }
    };
    let what = match width {
        None => format!("{text:?} in an auto-width column"),
        Some(px) => format!("{text:?} in a column of width {px}"),
    };
    let mut t = table(text, width).await?;
    eprintln!(
        "probe: laying out {what}; the copied search now says {:?}",
        predict(text, width.unwrap_or(AUTO_MAX))
    );
    let el = layout(&mut t, &mut renderer, &what);
    eprintln!("probe: {what}: layout returned in {el:?}");
    Ok(())
}
