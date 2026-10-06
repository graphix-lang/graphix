//! gui-chart-01: one +-inf sample (or a finite span wider than f64::MAX)
//! hangs the chart's draw forever in plotters' tick loop; an all-inf
//! series, a user range with an inf end, and a drag on a chart too small
//! to have a plot area also hand plotters NaN or infinite ranges.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_01.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_01 -- --nocapture
//!
//! `chart_draws_return` runs each scenario in a child process of the test
//! binary. The child compiles `use gui::chart; let result = <chart>`,
//! builds the widget and draws it through a headless wgpu renderer as
//! `GuiHandler::about_to_wait` does (`UserInterface::build` + `draw`); a
//! drag scenario then delivers CursorMoved, ButtonPressed(Left) and
//! CursorMoved through `UserInterface::update` and draws again. A draw
//! that has not returned after 10 s is reported with the drawing thread's
//! CPU time and its top stack frames (eu-stack + c++filt), and the child
//! exits.
//!
//! Expected: every draw returns (a non-finite sample is skipped; a range
//! that is not finite, or not min < max, falls back to a finite one).
//! Observed at c722befe (dev profile), the test FAILS in 82 s:
//!   pad_range(1.0, inf) = (-inf, inf)   pad_range(inf, inf) = (NaN, NaN)
//!   pad_range(-1e308, 1e308) = (-inf, inf)
//!   finite (control): returned
//!   one inf sample [(0.0, 1.0), (1.0, 1.0 / 0.0), (2.0, 2.0)]: HUNG, the
//!     drawing thread used 10.0 s of CPU in 10 s; top frames:
//!       plotters::coord::ranged1d::types::numeric::compute_f64_key_points
//!       <Cartesian2d<RangedCoordf64, RangedCoordf64>>::draw_mesh
//!       <DrawingArea<IcedBackend, ..>>::draw_mesh
//!       <ChartContext<IcedBackend, ..>>::draw_mesh
//!       <MeshStyle<RangedCoordf64, ..>>::draw
//!       <ChartW<NoExt> as iced_widget::canvas::program::Program<..>>::draw
//!   all samples inf: PANICKED at plotters-0.3.7 numeric.rs:243:
//!     assertion failed: !(range.0.is_nan() || range.1.is_nan())
//!   finite span that overflows [(0.0, -1e308), (1.0, 1e308)]: HUNG (same)
//!   user y_range {min: 0.0, max: 1.0 / 0.0}: HUNG (same)
//!   bar inf value: HUNG (same, over a SegmentedCoord x axis)
//!   scatter3d inf sample: HUNG in compute_f64_key_points via
//!     <ChartContext<.., Cartesian3d<..>>>::get_key_points <- Axes3dStyle::draw
//!   vertical drag, 400x300 chart (control): returned
//!   first draw only, 60x40 chart (control): returned
//!   vertical drag (0, +5), 60x40 chart: PANICKED (the same NaN assertion)
//!   horizontal drag (+5, 0), 60x40 chart: HUNG (same stack as one inf sample)

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, chart::pad_range},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Event, Point, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
    ffi::{c_int, c_ulong},
    io::{Read, Write},
    path::Path,
    process::{Command, Stdio},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

/// A draw that has not returned after this long is reported as hung.
const HANG: Duration = Duration::from_secs(10);

struct Scenario {
    name: &'static str,
    control: bool,
    chart: &'static str,
    viewport: (f32, f32),
    /// Press the left button at the canvas center, then move the cursor
    /// by (dx, dy), then draw again.
    drag: Option<(f32, f32)>,
}

const SCENARIOS: &[Scenario] = &[
    Scenario {
        name: "finite (control)",
        control: true,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "one inf sample",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 1.0 / 0.0), (2.0, 2.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "all samples inf",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0 / 0.0), (1.0, 1.0 / 0.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "finite span that overflows",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, -1e308), (1.0, 1e308)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "user y_range max inf",
        control: false,
        chart: "chart::chart(#y_range: &{min: 0.0, max: 1.0 / 0.0}, \
                #width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "bar inf value",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::bar(&[(\"a\", 1.0), (\"b\", 1.0 / 0.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "scatter3d inf sample",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::scatter3d(&[(0.0, 0.0, 0.0), (1.0, 1.0 / 0.0, 1.0)])])",
        viewport: (400.0, 300.0),
        drag: None,
    },
    Scenario {
        name: "vertical drag, 400x300 chart (control)",
        control: true,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (400.0, 300.0),
        drag: Some((0.0, 5.0)),
    },
    Scenario {
        name: "first draw only, 60x40 chart (control)",
        control: true,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (60.0, 40.0),
        drag: None,
    },
    Scenario {
        name: "vertical drag, 60x40 chart",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (60.0, 40.0),
        drag: Some((0.0, 5.0)),
    },
    Scenario {
        name: "horizontal drag, 60x40 chart",
        control: false,
        chart: "chart::chart(#width: &`Fill, #height: &`Fill, \
                &[chart::line(&[(0.0, 1.0), (1.0, 3.0), (2.0, 2.0)])])",
        viewport: (60.0, 40.0),
        drag: Some((5.0, 0.0)),
    },
];

unsafe extern "C" {
    fn prctl(option: c_int, ...) -> c_int;
    fn _exit(status: c_int) -> !;
}

const PR_SET_PTRACER: c_int = 0x59616d61;
const PR_SET_PTRACER_ANY: c_ulong = c_ulong::MAX;

struct H {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt: tokio::runtime::Handle,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
}

async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the widget value"),
        }
    }
}

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

impl H {
    async fn new(code: &str, viewport: Size) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)])
                .await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let initial = wait_for_update(&mut rx, compiled.exprs[0].id).await?;
        let widget = widgets::compile(gx.clone(), initial).await.context("widgets")?;
        let mut h = Self {
            _ctx: ctx,
            _compiled: compiled,
            rx,
            widget,
            rt: tokio::runtime::Handle::current(),
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            viewport,
        };
        h.drain().await?;
        Ok(h)
    }

    async fn drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(300));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            let rt = self.rt.clone();
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(200)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    /// One frame's event delivery, as `GuiHandler::about_to_wait` does it.
    fn process(&mut self, events: &[Event], at: Point) {
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(self.widget.view(), self.viewport, cache, &mut self.renderer);
        let mut messages: Vec<widgets::Message> = Vec::new();
        let mut clipboard = clipboard::Null;
        let _ = ui.update(
            events,
            mouse::Cursor::Available(at),
            &mut self.renderer,
            &mut clipboard,
            &mut messages,
        );
        self.cache = ui.into_cache();
    }

    /// One frame's draw, as `GuiHandler::about_to_wait` does it.
    fn draw(&mut self) {
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let style = Style { text_color: theme.palette().text };
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(self.widget.view(), self.viewport, cache, &mut self.renderer);
        ui.draw(&mut self.renderer, &theme, &style, mouse::Cursor::Unavailable);
        self.cache = ui.into_cache();
    }
}

/// Seconds of CPU (user + system) a thread has used, from
/// `/proc/<pid>/task/<tid>/stat` (USER_HZ is 100 on x86_64 Linux).
fn thread_cpu_secs(task: Option<&str>) -> Option<f64> {
    let stat = std::fs::read_to_string(format!("/proc/{}/stat", task?)).ok()?;
    let rest = &stat[stat.rfind(')')? + 2..];
    let f: Vec<&str> = rest.split_whitespace().collect();
    let ticks = f.get(11)?.parse::<f64>().ok()? + f.get(12)?.parse::<f64>().ok()?;
    Some(ticks / 100.0)
}

/// A demangled frame name without crate hashes, cut to a readable length.
fn tidy(name: &str) -> String {
    let mut s = String::new();
    let mut rest = name;
    while let Some(i) = rest.find('[') {
        s.push_str(&rest[..i]);
        rest = &rest[i..];
        match rest.find(']') {
            Some(j) if rest[1..j].chars().all(|c| c.is_ascii_hexdigit()) => {
                rest = &rest[j + 1..]
            }
            _ => {
                s.push('[');
                rest = &rest[1..];
            }
        }
    }
    s.push_str(rest);
    if s.len() > 150 {
        let mut cut = 150;
        while !s.is_char_boundary(cut) {
            cut -= 1;
        }
        s.truncate(cut);
        s.push_str(" ..");
    }
    s
}

/// The top frames of one thread of this process, by eu-stack and c++filt.
fn print_stack(task: Option<&str>) {
    let tid = task.and_then(|t| t.rsplit('/').next()).unwrap_or("?");
    let header = format!("TID {tid}:");
    let out = match Command::new("eu-stack")
        .args(["-p", &std::process::id().to_string()])
        .output()
    {
        Ok(out) => out,
        Err(e) => return println!("STACK: eu-stack failed: {e}"),
    };
    let text = String::from_utf8_lossy(&out.stdout);
    let mut in_thread = false;
    let mut syms = Vec::new();
    for line in text.lines() {
        if line.starts_with("TID ") {
            in_thread = line.trim() == header;
        } else if in_thread && syms.len() < 8 {
            syms.push(line.split_whitespace().nth(2).unwrap_or("?").to_string());
        }
    }
    if syms.is_empty() {
        let why = String::from_utf8_lossy(&out.stderr);
        return println!("STACK: no frames for {header} ({})", why.lines().next().unwrap_or(""));
    }
    let names = Command::new("c++filt")
        .args(&syms)
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).lines().map(String::from).collect())
        .unwrap_or_else(|_| syms.clone());
    for (i, n) in names.iter().enumerate() {
        println!("STACK: #{i} {}", tidy(n));
    }
}

struct Finished(Arc<AtomicBool>);

impl Drop for Finished {
    fn drop(&mut self) {
        self.0.store(true, Ordering::Release);
    }
}

/// Run `f` on this thread; when it has not returned after `HANG`, report
/// the CPU it burned and its stack, and end the process with status 3.
fn watched<T>(what: &str, f: impl FnOnce() -> T) -> T {
    let me = std::fs::read_link("/proc/thread-self")
        .ok()
        .map(|p| p.to_string_lossy().into_owned());
    let flag = Arc::new(AtomicBool::new(false));
    let finished = Finished(flag.clone());
    let what_s = what.to_string();
    let watchdog = std::thread::spawn(move || {
        let cpu0 = thread_cpu_secs(me.as_deref());
        let start = Instant::now();
        while start.elapsed() < HANG {
            if flag.load(Ordering::Acquire) {
                return;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        let cpu = match (cpu0, thread_cpu_secs(me.as_deref())) {
            (Some(a), Some(b)) => format!("{:.1}", b - a),
            _ => "?".into(),
        };
        println!(
            "HANG: the {what_s} did not return within {} s; the drawing thread used {cpu} s of CPU in that time",
            HANG.as_secs()
        );
        print_stack(me.as_deref());
        let _ = std::io::stdout().flush();
        let _ = std::io::stderr().flush();
        unsafe { _exit(3) }
    });
    println!("STAGE: {what}");
    let r = f();
    drop(finished);
    let _ = watchdog.join();
    println!("STAGE: {what} returned");
    r
}

/// The body of one scenario, run in a child process of `chart_draws_return`.
#[tokio::test(flavor = "multi_thread")]
async fn scenario_child() -> Result<()> {
    let Ok(name) = std::env::var("REVIEW_SCENARIO") else { return Ok(()) };
    unsafe {
        prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0 as c_ulong, 0 as c_ulong, 0 as c_ulong);
    }
    let sc = SCENARIOS.iter().find(|s| s.name == name).context("unknown scenario")?;
    let code = format!("use gui::chart;\nlet result = {}", sc.chart);
    let (vw, vh) = sc.viewport;
    let mut harness = H::new(&code, Size::new(vw, vh)).await?;
    watched("first draw", || harness.draw());
    if let Some((dx, dy)) = sc.drag {
        let p0 = Point::new(vw / 2.0, vh / 2.0);
        let p1 = Point::new(p0.x + dx, p0.y + dy);
        harness.process(&[Event::Mouse(mouse::Event::CursorMoved { position: p0 })], p0);
        harness.process(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))], p0);
        harness.process(&[Event::Mouse(mouse::Event::CursorMoved { position: p1 })], p1);
        watched("draw after the drag", || harness.draw());
    }
    println!("RESULT: every draw returned");
    Ok(())
}

fn run_child(exe: &Path, name: &str) -> (String, Vec<String>) {
    let mut child = Command::new(exe)
        .args(["--exact", "scenario_child", "--nocapture", "--include-ignored"])
        .env("REVIEW_SCENARIO", name)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn the scenario child");
    let mut so = child.stdout.take().unwrap();
    let mut se = child.stderr.take().unwrap();
    let t_out = std::thread::spawn(move || {
        let mut s = String::new();
        let _ = so.read_to_string(&mut s);
        s
    });
    let t_err = std::thread::spawn(move || {
        let mut s = String::new();
        let _ = se.read_to_string(&mut s);
        s
    });
    let deadline = Instant::now() + Duration::from_secs(150);
    let status = loop {
        if let Some(st) = child.try_wait().expect("wait for the child") {
            break Some(st);
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            let _ = child.wait();
            break None;
        }
        std::thread::sleep(Duration::from_millis(100));
    };
    let out = t_out.join().unwrap();
    let err = t_err.join().unwrap();
    let mut lines: Vec<String> = out
        .lines()
        .filter(|l| {
            ["STAGE:", "HANG:", "STACK:", "RESULT:"].iter().any(|p| l.starts_with(p))
        })
        .map(String::from)
        .collect();
    let err_lines: Vec<&str> = err.lines().collect();
    let mut panic_msg = None;
    for (i, l) in err_lines.iter().enumerate() {
        if l.contains("panicked at") {
            let msg = err_lines.get(i + 1).copied().unwrap_or("");
            lines.push(format!("PANIC: {l} {msg}"));
            panic_msg.get_or_insert_with(|| format!("{l} {msg}"));
        } else if l.starts_with("Error:") {
            lines.push(format!("ERROR: {l}"));
        }
    }
    let code = status.and_then(|s| s.code());
    let verdict = match (status, code) {
        (None, _) => "killed by the parent after 150 s".to_string(),
        (Some(_), Some(0)) if out.contains("RESULT: every draw returned") => {
            "returned".to_string()
        }
        (Some(_), Some(3)) if out.contains("HANG:") => "HUNG".to_string(),
        (Some(_), Some(101)) if panic_msg.is_some() => {
            format!("PANICKED: {}", panic_msg.unwrap())
        }
        (Some(s), _) => format!("other: {s}"),
    };
    (verdict, lines)
}

#[test]
fn chart_draws_return() {
    if std::env::var_os("REVIEW_SCENARIO").is_some() {
        return;
    }
    println!("pad_range(1.0, inf)       = {:?}", pad_range(1.0, f64::INFINITY));
    println!("pad_range(inf, inf)       = {:?}", pad_range(f64::INFINITY, f64::INFINITY));
    println!("pad_range(-1e308, 1e308)  = {:?}", pad_range(-1e308, 1e308));
    let exe = std::env::current_exe().expect("test binary path");
    let mut failures = Vec::new();
    for sc in SCENARIOS {
        let t = Instant::now();
        let (verdict, lines) = run_child(&exe, sc.name);
        println!("== {} [{:.1} s]: {verdict}", sc.name, t.elapsed().as_secs_f64());
        for l in &lines {
            println!("     {l}");
        }
        if sc.control {
            assert_eq!(verdict, "returned", "control scenario {} failed", sc.name);
        } else if verdict != "returned" {
            failures.push(format!("{}: {verdict}", sc.name));
        }
    }
    assert!(failures.is_empty(), "chart draws that did not return: {failures:#?}");
}
