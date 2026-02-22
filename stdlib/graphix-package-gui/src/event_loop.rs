//! Main GUI event loop.
//!
//! Runs on a dedicated OS thread with its own single-threaded tokio
//! runtime. Integrates winit's event loop via `pump_app_events` with
//! graphix update processing and iced rendering.

use crate::{
    compile, convert,
    render::{GpuState, WindowSurface},
    window::TrackedWindow,
    Message, ToGui,
};
use anyhow::{Context, Result};
use fxhash::FxHashMap;
use graphix_compiler::env::Env;
use graphix_rt::{CompExp, GXExt, GXHandle};
use iced_core::{mouse, renderer::Style};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use log::error;
use std::{collections::VecDeque, sync::Arc, time::Duration};
use tokio::sync::oneshot;
use winit::{
    application::ApplicationHandler,
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    keyboard::ModifiersState,
    platform::pump_events::{EventLoopExtPumpEvents, PumpStatus},
    window::{Window, WindowAttributes, WindowId},
};

/// State for the winit ApplicationHandler callback.
struct WinitHandler {
    events: VecDeque<(WindowId, WindowEvent)>,
    windows_to_create: Vec<WindowAttributes>,
    created_windows: Vec<Arc<Window>>,
    modifiers: ModifiersState,
    close_requested: Option<WindowId>,
}

impl WinitHandler {
    fn new() -> Self {
        Self {
            events: VecDeque::new(),
            windows_to_create: Vec::new(),
            created_windows: Vec::new(),
            modifiers: ModifiersState::default(),
            close_requested: None,
        }
    }
}

impl ApplicationHandler for WinitHandler {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        for attrs in self.windows_to_create.drain(..) {
            match event_loop.create_window(attrs) {
                Ok(w) => self.created_windows.push(Arc::new(w)),
                Err(e) => error!("failed to create window: {e:?}"),
            }
        }
    }

    fn window_event(
        &mut self,
        _event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        if let WindowEvent::ModifiersChanged(m) = &event {
            self.modifiers = m.state();
        }
        if let WindowEvent::CloseRequested = &event {
            self.close_requested = Some(window_id);
        }
        self.events.push_back((window_id, event));
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, _: ()) {
        for attrs in self.windows_to_create.drain(..) {
            match event_loop.create_window(attrs) {
                Ok(w) => self.created_windows.push(Arc::new(w)),
                Err(e) => error!("failed to create window: {e:?}"),
            }
        }
    }
}

/// Entry point for the process main thread. Called by the shell's
/// `main()` when a package declares `MAIN_THREAD`.
///
/// Currently the winit event loop still runs on the spawned GUI
/// thread via `pump_app_events`. This function simply blocks until
/// the tokio side exits. Once the event loop is migrated to
/// `EventLoop::run_app()` here, macOS (NSApplication) will work
/// correctly.
pub(crate) fn run_main_thread(tx: graphix_package::MainThreadTx) {
    // Nothing to do yet — drop the sender and return.
    // main() will join the tokio thread which keeps running.
    drop(tx);
}

pub(crate) fn run<X: GXExt>(
    gx: GXHandle<X>,
    env: Env,
    root_exp: CompExp<X>,
    to_rx: std::sync::mpsc::Receiver<ToGui>,
    stop: oneshot::Sender<()>,
    rt_handle: tokio::runtime::Handle,
) {
    if let Err(e) = run_inner::<X>(gx, env, root_exp, to_rx, stop, &rt_handle) {
        error!("gui::run returned {e:?}");
    }
}

fn run_inner<X: GXExt>(
    gx: GXHandle<X>,
    _env: Env,
    root_exp: CompExp<X>,
    to_rx: std::sync::mpsc::Receiver<ToGui>,
    stop: oneshot::Sender<()>,
    rt: &tokio::runtime::Handle,
) -> Result<()> {
    let mut stop = Some(stop);
    let gpu = rt.block_on(GpuState::new()).context("gpu init")?;

    let mut event_loop = EventLoop::<()>::with_user_event()
        .build()
        .context("event loop creation")?;
    event_loop.set_control_flow(ControlFlow::Wait);

    let mut handler = WinitHandler::new();

    // Request initial window creation
    handler.windows_to_create.push(
        WindowAttributes::default()
            .with_title("Graphix")
            .with_inner_size(winit::dpi::LogicalSize::new(800.0, 600.0)),
    );

    // First pump to trigger resumed() and create the window
    event_loop.pump_app_events(Some(Duration::ZERO), &mut handler);

    let mut surfaces: FxHashMap<WindowId, WindowSurface> = FxHashMap::default();
    let mut tracked: Option<TrackedWindow<X>> = None;
    let mut ui_caches: FxHashMap<WindowId, user_interface::Cache> = FxHashMap::default();
    let mut messages: Vec<Message> = Vec::new();

    // Initialize the window surface (root starts empty, populated on first update)
    if let Some(window) = handler.created_windows.pop() {
        let win_id = window.id();
        let ws = WindowSurface::new(&gpu, window.clone())
            .context("window surface creation")?;
        surfaces.insert(win_id, ws);
    }

    loop {
        let status =
            event_loop.pump_app_events(Some(Duration::from_millis(8)), &mut handler);

        if matches!(status, PumpStatus::Exit(_)) {
            break;
        }

        // Handle newly created windows
        for window in handler.created_windows.drain(..) {
            let win_id = window.id();
            match WindowSurface::new(&gpu, window.clone()) {
                Ok(ws) => {
                    surfaces.insert(win_id, ws);
                }
                Err(e) => error!("surface creation failed: {e:?}"),
            }
        }

        // Dispatch winit events to tracked windows
        for (win_id, event) in handler.events.drain(..) {
            if let Some(tw) = &mut tracked {
                if tw.window_id() == win_id {
                    let scale = tw.window.scale_factor();
                    let iced_events =
                        convert::window_event(&event, scale, handler.modifiers);
                    for ev in iced_events {
                        if let iced_core::Event::Mouse(mouse::Event::CursorMoved {
                            position,
                        }) = &ev
                        {
                            tw.cursor_position = *position;
                        }
                        tw.push_event(ev);
                    }
                    if let WindowEvent::Resized(size) = event {
                        if let Some(ws) = surfaces.get_mut(&win_id) {
                            ws.resize(
                                &gpu,
                                size.width,
                                size.height,
                                tw.window.scale_factor(),
                            );
                        }
                        tw.needs_redraw = true;
                    }
                }
            }
        }

        // Handle close requests
        if let Some(_close_id) = handler.close_requested.take() {
            if let Some(s) = stop.take() { let _ = s.send(()); }
            break;
        }

        // Process graphix updates (non-blocking)
        let mut should_stop = false;
        while let Ok(msg) = to_rx.try_recv() {
            match msg {
                ToGui::Stop(tx) => {
                    let _ = tx.send(());
                    if let Some(s) = stop.take() { let _ = s.send(()); }
                    should_stop = true;
                    break;
                }
                ToGui::Update(id, v) => {
                    if id == root_exp.id {
                        // Root changed — (re)compile the window state
                        if let Some(window) = surfaces.keys().next().copied() {
                            if surfaces.contains_key(&window) {
                                // Find or create the window Arc
                                let win_arc = tracked
                                    .as_ref()
                                    .map(|tw| tw.window.clone())
                                    .or_else(|| {
                                        // First time: find in created_windows or reconstruct
                                        None
                                    });
                                if let Some(w) = win_arc {
                                    match rt.block_on(TrackedWindow::compile(
                                        gx.clone(),
                                        w,
                                        v,
                                    )) {
                                        Ok(tw) => tracked = Some(tw),
                                        Err(e) => error!("window compile: {e:?}"),
                                    }
                                }
                            }
                        }
                    } else if let Some(tw) = &mut tracked {
                        match tw.handle_update(id, &v) {
                            Ok(needs_recompile) => {
                                if needs_recompile {
                                    if let Some(cv) = tw.content_ref.last.as_ref() {
                                        match rt.block_on(compile(
                                            gx.clone(),
                                            cv.clone(),
                                        )) {
                                            Ok(w) => tw.content = w,
                                            Err(e) => {
                                                error!("content recompile: {e:?}")
                                            }
                                        }
                                    }
                                }
                            }
                            Err(e) => error!("handle_update: {e:?}"),
                        }
                    }
                }
            }
        }
        if should_stop {
            break;
        }

        // Render windows that need redraw
        if let Some(tw) = &mut tracked {
            if tw.needs_redraw {
                let win_id = tw.window_id();
                if let Some(ws) = surfaces.get_mut(&win_id) {
                    let cache = ui_caches
                        .remove(&win_id)
                        .unwrap_or_default();
                    let element = tw.content.view();
                    let viewport_size = ws.logical_size();
                    let mut ui = UserInterface::build(
                        element,
                        viewport_size,
                        cache,
                        &mut ws.renderer,
                    );
                    let (_, _statuses) = ui.update(
                        &tw.pending_events,
                        tw.cursor(),
                        &mut ws.renderer,
                        &mut iced_core::clipboard::Null,
                        &mut messages
                    );
                    let theme = tw.iced_theme();
                    let style = Style {
                        text_color: theme.palette().text,
                    };
                    ui.draw(&mut ws.renderer, &theme, &style, tw.cursor());
                    ui_caches.insert(win_id, ui.into_cache());
                    tw.pending_events.clear();

                    // Present to screen
                    match ws.surface.get_current_texture() {
                        Ok(frame) => {
                            let view = frame.texture.create_view(
                                &wgpu::TextureViewDescriptor::default(),
                            );
                            ws.renderer.present(
                                None,
                                gpu.format,
                                &view,
                                &ws.viewport,
                            );
                            frame.present();
                        }
                        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                            ws.surface.configure(&gpu.device, &ws.config);
                            tw.needs_redraw = true;
                        }
                        Err(e) => error!("surface frame error: {e:?}"),
                    }
                    tw.needs_redraw = false;
                }
            }
        }

        // Process widget messages → set graphix refs
        for msg in messages.drain(..) {
            match msg {
                Message::Set(bid, v) => {
                    if let Err(e) = gx.set(bid, v) {
                        error!("failed to set ref: {e:?}");
                    }
                }
            }
        }
    }
    Ok(())
}
