//! Main GUI event loop.
//!
//! Runs on the main OS thread (via MainThreadHandle) using winit's
//! standard `run_app` model. Graphix updates are delivered as native
//! winit user events via `EventLoopProxy<ToGui>`.
//!
//! Supports multiple windows, each tracked by its graphix BindId.
//! Windows are created/destroyed as the root `Array<&Window>` changes.

use crate::{
    convert,
    render::{GpuState, WindowSurface},
    types::SizeV,
    widgets::Message,
    window::{ResolvedWindow, TrackedWindow},
    ToGui,
};
use anyhow::{Context, Result};
use fxhash::FxHashMap;
use graphix_compiler::BindId;
use graphix_rt::{CompExp, GXExt, GXHandle};
use iced_core::{clipboard, mouse, renderer::Style, window, Size};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use log::error;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use std::cell::RefCell;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, oneshot};
use winit::{
    application::ApplicationHandler,
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy},
    keyboard::ModifiersState,
    window::{CursorIcon, WindowId},
};

/// System clipboard backed by arboard.
struct Clipboard {
    state: RefCell<Option<arboard::Clipboard>>,
}

impl Clipboard {
    fn new() -> Self {
        Self { state: RefCell::new(arboard::Clipboard::new().ok()) }
    }
}

impl clipboard::Clipboard for Clipboard {
    fn read(&self, kind: clipboard::Kind) -> Option<String> {
        let mut cb = self.state.borrow_mut();
        let cb = cb.as_mut()?;
        match kind {
            clipboard::Kind::Standard => cb.get_text().ok(),
            clipboard::Kind::Primary => {
                #[cfg(target_os = "linux")]
                {
                    use arboard::GetExtLinux;
                    cb.get().clipboard(arboard::LinuxClipboardKind::Primary).text().ok()
                }
                #[cfg(not(target_os = "linux"))]
                None
            }
        }
    }

    fn write(&mut self, kind: clipboard::Kind, contents: String) {
        let mut cb = self.state.borrow_mut();
        let Some(cb) = cb.as_mut() else { return };
        match kind {
            clipboard::Kind::Standard => {
                let _ = cb.set_text(contents);
            }
            clipboard::Kind::Primary => {
                #[cfg(target_os = "linux")]
                {
                    use arboard::SetExtLinux;
                    let _ = cb
                        .set()
                        .clipboard(arboard::LinuxClipboardKind::Primary)
                        .text(contents);
                }
            }
        }
    }
}

fn mouse_interaction_to_cursor(interaction: mouse::Interaction) -> CursorIcon {
    match interaction {
        mouse::Interaction::None | mouse::Interaction::Idle => CursorIcon::Default,
        mouse::Interaction::Hidden => CursorIcon::Default,
        mouse::Interaction::Pointer => CursorIcon::Pointer,
        mouse::Interaction::Grab => CursorIcon::Grab,
        mouse::Interaction::Grabbing => CursorIcon::Grabbing,
        mouse::Interaction::Text => CursorIcon::Text,
        mouse::Interaction::Crosshair => CursorIcon::Crosshair,
        mouse::Interaction::Cell => CursorIcon::Cell,
        mouse::Interaction::Help => CursorIcon::Help,
        mouse::Interaction::ContextMenu => CursorIcon::ContextMenu,
        mouse::Interaction::Progress => CursorIcon::Progress,
        mouse::Interaction::Wait => CursorIcon::Wait,
        mouse::Interaction::Alias => CursorIcon::Alias,
        mouse::Interaction::Copy => CursorIcon::Copy,
        mouse::Interaction::Move => CursorIcon::Move,
        mouse::Interaction::NoDrop => CursorIcon::NoDrop,
        mouse::Interaction::NotAllowed => CursorIcon::NotAllowed,
        mouse::Interaction::ResizingHorizontally => CursorIcon::EwResize,
        mouse::Interaction::ResizingVertically => CursorIcon::NsResize,
        mouse::Interaction::ResizingDiagonallyUp => CursorIcon::NeswResize,
        mouse::Interaction::ResizingDiagonallyDown => CursorIcon::NwseResize,
        mouse::Interaction::ResizingColumn => CursorIcon::ColResize,
        mouse::Interaction::ResizingRow => CursorIcon::RowResize,
        mouse::Interaction::AllScroll => CursorIcon::AllScroll,
        mouse::Interaction::ZoomIn => CursorIcon::ZoomIn,
        mouse::Interaction::ZoomOut => CursorIcon::ZoomOut,
    }
}

/// During active resize, cap the render+configure rate to avoid
/// saturating the GPU queue and accumulating latency.
const RESIZE_RENDER_INTERVAL: Duration = Duration::from_millis(8);

/// All GUI state, implementing winit's ApplicationHandler.
struct GuiHandler<X: GXExt> {
    gx: GXHandle<X>,
    root_exp: CompExp<X>,
    gpu: Option<GpuState>,
    rt: tokio::runtime::Handle,
    stop: Option<oneshot::Sender<()>>,
    windows: FxHashMap<BindId, TrackedWindow<X>>,
    win_to_bid: FxHashMap<WindowId, BindId>,
    surfaces: FxHashMap<WindowId, WindowSurface>,
    ui_caches: FxHashMap<WindowId, user_interface::Cache>,
    clipboard: Clipboard,
    resize_tx: mpsc::UnboundedSender<(WindowId, SizeV)>,
    messages: Vec<Message>,
    modifiers: ModifiersState,
}

impl<X: GXExt> ApplicationHandler<ToGui> for GuiHandler<X> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        event_loop.set_control_flow(ControlFlow::Wait);
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        if let WindowEvent::ModifiersChanged(m) = &event {
            self.modifiers = m.state();
        }

        if let Some(&bid) = self.win_to_bid.get(&window_id) {
            if let Some(tw) = self.windows.get_mut(&bid) {
                if let WindowEvent::Resized(size) = &event {
                    let scale = tw.window.scale_factor();
                    tw.pending_resize = Some((size.width, size.height, scale));
                    tw.needs_redraw = true;
                    let logical = size.to_logical::<f32>(scale);
                    let _ = self.resize_tx.send((
                        window_id,
                        SizeV(Size::new(logical.width, logical.height)),
                    ));
                } else if let WindowEvent::RedrawRequested = &event {
                    tw.needs_redraw = true;
                } else {
                    let scale = tw.window.scale_factor();
                    let mut iced_events =
                        convert::window_event(&event, scale, self.modifiers);
                    for ev in iced_events.drain(..) {
                        if let iced_core::Event::Mouse(mouse::Event::CursorMoved {
                            position,
                        }) = &ev
                        {
                            tw.cursor_position = *position;
                        }
                        tw.push_event(ev);
                    }
                }
            }
        }

        if let WindowEvent::CloseRequested = &event {
            if let Some(bid) = self.win_to_bid.remove(&window_id) {
                self.windows.remove(&bid);
                self.surfaces.remove(&window_id);
                self.ui_caches.remove(&window_id);
            }
            if self.windows.is_empty() {
                self.surfaces.clear();
                self.ui_caches.clear();
                self.gpu = None;
                if let Some(s) = self.stop.take() {
                    let _ = s.send(());
                }
                event_loop.exit();
            }
        }
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, event: ToGui) {
        match event {
            ToGui::Stop(tx) => {
                let _ = tx.send(());
                self.windows.clear();
                self.surfaces.clear();
                self.ui_caches.clear();
                self.gpu = None;
                if let Some(s) = self.stop.take() {
                    let _ = s.send(());
                }
                event_loop.exit();
            }
            ToGui::ResizeTimer(window_id, sz) => {
                if let Some(&bid) = self.win_to_bid.get(&window_id) {
                    if let Some(tw) = self.windows.get_mut(&bid) {
                        if tw.size.t.as_ref() != Some(&sz) {
                            tw.last_set_size = Some(sz);
                            if let Err(e) = tw.size.set(sz) {
                                error!("failed to set window size: {e:?}");
                            }
                        }
                        tw.push_event(iced_core::Event::Window(
                            iced_core::window::Event::Resized(sz.0),
                        ));
                    }
                }
            }
            ToGui::Update(id, v) => {
                if id == self.root_exp.id {
                    if let Err(e) = reconcile_windows(
                        &self.gx,
                        &self.rt,
                        &mut self.gpu,
                        event_loop,
                        &mut self.windows,
                        &mut self.win_to_bid,
                        &mut self.surfaces,
                        &mut self.ui_caches,
                        v,
                    ) {
                        error!("reconcile windows: {e:?}");
                    }
                } else {
                    for tw in self.windows.values_mut() {
                        if let Err(e) = tw.handle_update(&self.rt, id, &v) {
                            error!("handle_update: {e:?}");
                        }
                    }
                }
            }
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(gpu) = self.gpu.as_ref() else { return };
        let mut deferred_until: Option<Instant> = None;
        for tw in self.windows.values_mut() {
            if !tw.needs_redraw {
                continue;
            }
            let win_id = tw.window_id();
            // During resize, throttle the entire render+configure cycle
            if tw.pending_resize.is_some() {
                let elapsed = tw.last_render.elapsed();
                if elapsed < RESIZE_RENDER_INTERVAL {
                    let wake = tw.last_render + RESIZE_RENDER_INTERVAL;
                    deferred_until = Some(deferred_until.map_or(wake, |d| d.min(wake)));
                    continue;
                }
            }
            if let Some(ws) = self.surfaces.get_mut(&win_id) {
                if let Some((pw, ph, scale)) = tw.pending_resize.take() {
                    ws.resize(gpu, pw, ph, scale);
                    tw.push_event(iced_core::Event::Window(
                        iced_core::window::Event::Resized(ws.logical_size()),
                    ));
                }
                let cache = self.ui_caches.remove(&win_id).unwrap_or_default();
                let element = tw.content.view();
                let viewport_size = ws.logical_size();
                let mut ui =
                    UserInterface::build(element, viewport_size, cache, &mut ws.renderer);
                let (state, _statuses) = ui.update(
                    &tw.pending_events,
                    tw.cursor(),
                    &mut ws.renderer,
                    &mut self.clipboard,
                    &mut self.messages,
                );
                if let user_interface::State::Updated { mouse_interaction, .. } = &state {
                    if tw.last_mouse_interaction != *mouse_interaction {
                        tw.last_mouse_interaction = *mouse_interaction;
                        match mouse_interaction {
                            mouse::Interaction::Hidden => {
                                tw.window.set_cursor_visible(false);
                            }
                            _ => {
                                tw.window.set_cursor_visible(true);
                                tw.window.set_cursor(mouse_interaction_to_cursor(
                                    *mouse_interaction,
                                ));
                            }
                        }
                    }
                }
                let theme = tw.iced_theme();
                let style = Style { text_color: theme.palette().text };
                ui.draw(&mut ws.renderer, &theme, &style, tw.cursor());

                self.ui_caches.insert(win_id, ui.into_cache());
                tw.pending_events.clear();

                match ws.surface.get_current_texture() {
                    Ok(frame) => {
                        let view = frame
                            .texture
                            .create_view(&wgpu::TextureViewDescriptor::default());
                        ws.renderer.present(None, gpu.format, &view, &ws.viewport);
                        frame.present();
                        tw.last_render = Instant::now();
                        tw.needs_redraw = match &state {
                            user_interface::State::Outdated => true,
                            user_interface::State::Updated { redraw_request, .. } => {
                                !matches!(redraw_request, window::RedrawRequest::Wait)
                            }
                        };
                    }
                    Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                        ws.surface.configure(&gpu.device, &ws.config);
                        tw.needs_redraw = true;
                        continue;
                    }
                    Err(e) => {
                        error!("surface frame error: {e:?}");
                        tw.needs_redraw = false;
                    }
                }
                if tw.needs_redraw {
                    tw.window.request_redraw();
                }
            }
        }

        for msg in self.messages.drain(..) {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => {
                    if let Err(e) = self.gx.call(id, args) {
                        error!("failed to call: {e:?}");
                    }
                }
                Message::EditorAction(id, action) => {
                    for tw in self.windows.values_mut() {
                        if let Some((callable_id, v)) = tw.editor_action(id, &action) {
                            if let Err(e) =
                                self.gx.call(callable_id, ValArray::from_iter([v]))
                            {
                                error!("failed to call editor callback: {e:?}");
                            }
                            break;
                        }
                    }
                }
            }
        }

        if let Some(wake) = deferred_until {
            event_loop.set_control_flow(ControlFlow::WaitUntil(wake));
        }
    }
}

pub(crate) fn run<X: GXExt>(
    gx: GXHandle<X>,
    root_exp: CompExp<X>,
    proxy_tx: oneshot::Sender<EventLoopProxy<ToGui>>,
    stop: oneshot::Sender<()>,
    rt: tokio::runtime::Handle,
) {
    let event_loop = match EventLoop::<ToGui>::with_user_event().build() {
        Ok(el) => el,
        Err(e) => {
            error!("event loop creation failed: {e:?}");
            return;
        }
    };
    let _ = proxy_tx.send(event_loop.create_proxy());
    let (resize_tx, resize_rx) = mpsc::unbounded_channel();
    let debounce_proxy = event_loop.create_proxy();
    rt.spawn(resize_debounce(resize_rx, debounce_proxy));
    let mut handler = GuiHandler {
        gx,
        root_exp,
        gpu: None,
        rt,
        stop: Some(stop),
        windows: FxHashMap::default(),
        win_to_bid: FxHashMap::default(),
        surfaces: FxHashMap::default(),
        ui_caches: FxHashMap::default(),
        clipboard: Clipboard::new(),
        resize_tx,
        messages: Vec::new(),
        modifiers: ModifiersState::default(),
    };
    if let Err(e) = event_loop.run_app(&mut handler) {
        error!("gui event loop error: {e:?}");
    }
}

/// Debounce resize events: collect per-window sizes and only forward
/// to the event loop 100ms after the last resize for that window.
async fn resize_debounce(
    mut rx: mpsc::UnboundedReceiver<(WindowId, SizeV)>,
    proxy: EventLoopProxy<ToGui>,
) {
    use tokio::time::{sleep_until, Duration, Instant};
    let far = || Instant::now() + Duration::from_secs(86400);
    let timer = sleep_until(far());
    tokio::pin!(timer);
    let mut pending: FxHashMap<WindowId, SizeV> = FxHashMap::default();
    loop {
        tokio::select! {
            msg = rx.recv() => {
                match msg {
                    Some((wid, sz)) => {
                        pending.insert(wid, sz);
                        timer.as_mut().reset(Instant::now() + Duration::from_millis(100));
                    }
                    None => break,
                }
            }
            _ = &mut timer => {
                for (wid, sz) in pending.drain() {
                    let _ = proxy.send_event(ToGui::ResizeTimer(wid, sz));
                }
                timer.as_mut().reset(far());
            }
        }
    }
}

/// Reconcile the tracked windows with a new root array value.
///
/// The root value is `Array<&Window>` — an array of `Value::U64(bindid)`.
/// We diff old vs new BindIds: add new windows, remove stale ones.
fn reconcile_windows<X: GXExt>(
    gx: &GXHandle<X>,
    rt: &tokio::runtime::Handle,
    gpu: &mut Option<GpuState>,
    event_loop: &ActiveEventLoop,
    windows: &mut FxHashMap<BindId, TrackedWindow<X>>,
    win_to_bid: &mut FxHashMap<WindowId, BindId>,
    surfaces: &mut FxHashMap<WindowId, WindowSurface>,
    ui_caches: &mut FxHashMap<WindowId, user_interface::Cache>,
    root_value: Value,
) -> Result<()> {
    let arr =
        root_value.cast_to::<LPooled<Vec<u64>>>().context("root array of bind ids")?;
    let new_bids =
        arr.iter().map(|&id| BindId::from(id)).collect::<LPooled<Vec<BindId>>>();

    // Remove windows no longer in the array
    let to_remove = windows
        .keys()
        .filter(|bid| !new_bids.contains(bid))
        .copied()
        .collect::<LPooled<Vec<BindId>>>();
    for bid in to_remove.iter() {
        if let Some(tw) = windows.remove(bid) {
            let wid = tw.window_id();
            win_to_bid.remove(&wid);
            surfaces.remove(&wid);
            ui_caches.remove(&wid);
        }
    }

    // Add new windows
    for &bid in new_bids.iter() {
        if windows.contains_key(&bid) {
            continue;
        }
        let wref =
            rt.block_on(gx.compile_ref(bid)).context("compile_ref for window bind id")?;
        let window_value = match wref.last.as_ref() {
            Some(v) => v.clone(),
            None => {
                error!("window bind id {bid:?} has no initial value, skipping");
                continue;
            }
        };
        let resolved = rt
            .block_on(ResolvedWindow::compile(gx.clone(), window_value))
            .context("resolve window")?;
        let win_arc = Arc::new(
            event_loop
                .create_window(resolved.window_attrs())
                .context("failed to create window")?,
        );
        let wid = win_arc.id();
        let gpu = match gpu {
            Some(gpu) => gpu,
            None => {
                *gpu = Some(
                    rt.block_on(GpuState::new(win_arc.clone())).context("gpu init")?,
                );
                gpu.as_mut().unwrap()
            }
        };
        let ws = WindowSurface::new(gpu, win_arc.clone())
            .context("window surface creation")?;
        surfaces.insert(wid, ws);
        let tw = resolved.into_tracked(wref, win_arc);
        win_to_bid.insert(wid, bid);
        windows.insert(bid, tw);
    }

    Ok(())
}
