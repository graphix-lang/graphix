//! Main GUI event loop.
//!
//! Runs on the main OS thread (via MainThreadHandle) using winit's
//! standard `run_app` model. Graphix updates are delivered as native
//! winit user events via `EventLoopProxy<ToGui>`.
//!
//! Supports multiple windows, each tracked by its graphix BindId.
//! Windows are created/destroyed as the root `Array<&Window>` changes.

use crate::{
    ToGui, convert,
    render::{GpuState, WindowSurface},
    types::SizeV,
    widgets::{Message, MessageShell},
    window::{ResolvedWindow, TrackedWindow},
};
use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::BindId;
use graphix_rt::{CompExp, GXExt, GXHandle};
use iced_core::{Size, clipboard, mouse, renderer::Style, window};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use log::error;
use netidx::publisher::Value;
use nohash::IntMap;
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    sync::Arc,
    time::{Duration, Instant},
};
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

/// All GUI state, implementing winit's ApplicationHandler.
struct GuiHandler<X: GXExt> {
    gx: GXHandle<X>,
    root_exp: CompExp<X>,
    gpu: Option<GpuState>,
    rt: tokio::runtime::Handle,
    stop: Option<oneshot::Sender<()>>,
    windows: IntMap<BindId, TrackedWindow<X>>,
    win_to_bid: AHashMap<WindowId, BindId>,
    surfaces: AHashMap<WindowId, WindowSurface>,
    ui_caches: AHashMap<WindowId, user_interface::Cache>,
    clipboard: Clipboard,
    /// Proxy cloned into the resize-burst render timer tasks so they
    /// can post `ToGui::ResizeTimer` back into the main loop when
    /// their timer elapses.
    resize_proxy: EventLoopProxy<ToGui>,
    /// Channel into `resize_end_debounce`: receives the logical size
    /// of every `Resized` event. The debounce task resets its timer
    /// on each message and emits `ToGui::ResizeEnd` once the window
    /// has been quiet for `RESIZE_END_DEBOUNCE`.
    resize_end_tx: mpsc::UnboundedSender<(WindowId, SizeV)>,
    /// Scratch buffer for `iced`-emitted messages. Drained each pass
    /// of `about_to_wait` into a local `VecDeque`. Pooled so the
    /// backing `Vec` is recycled across frames.
    messages: LPooled<Vec<Message>>,
    modifiers: ModifiersState,
}

/// Render cadence during a resize drag (≈60 Hz). The timer is armed
/// on the first `Resized` event of a burst and NOT reset by
/// subsequent events — every ~16 ms we render the latest pending
/// size so the UI appears to follow the drag without rendering on
/// every cursor move. Measured at 83 Hz sustained on a slow
/// development machine with the old unbounded scheme, so 60 Hz
/// should stay comfortably inside the frame budget.
const RESIZE_RENDER_PERIOD: Duration = Duration::from_millis(16);

/// Time with no `Resized` events that counts as drag-end. Longer
/// than `RESIZE_RENDER_PERIOD` so the debounce reliably outlasts
/// the render cadence. When this elapses we write the final size
/// to the runtime's size ref exactly once — doing so on every
/// render-timer fire instead would cause request_inner_size
/// feedback against stale `last_set_size` state, visible as the
/// window continuing to resize seconds after the user releases
/// the drag handle.
const RESIZE_END_DEBOUNCE: Duration = Duration::from_millis(200);

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
                    if !tw.resize_timer_armed {
                        tw.resize_timer_armed = true;
                        let proxy = self.resize_proxy.clone();
                        self.rt.spawn(async move {
                            tokio::time::sleep(RESIZE_RENDER_PERIOD).await;
                            let _ = proxy.send_event(ToGui::ResizeTimer(window_id));
                        });
                    }
                    let logical = size.to_logical::<f32>(scale);
                    let _ = self.resize_end_tx.send((
                        window_id,
                        SizeV(Size::new(logical.width, logical.height)),
                    ));
                } else if let WindowEvent::RedrawRequested = &event {
                    // The OS pairs every `Resized` with a
                    // `RedrawRequested`. Setting `needs_redraw`
                    // here during a drag would fire one render per
                    // resize frame, ignoring the 100 ms timer. While
                    // a resize timer is armed, the timer is the
                    // sole render driver; outside a burst, treat
                    // `RedrawRequested` normally.
                    if !tw.resize_timer_armed {
                        tw.needs_redraw = true;
                    }
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
            ToGui::ResizeTimer(window_id) => {
                // Render cadence tick during a drag. Purely
                // schedules a redraw — does NOT write to `tw.size`.
                // The size ref is updated by `ResizeEnd` exactly
                // once per drag so the runtime's echo can't race
                // against a stream of intermediate sizes.
                if let Some(&bid) = self.win_to_bid.get(&window_id) {
                    if let Some(tw) = self.windows.get_mut(&bid) {
                        tw.resize_timer_armed = false;
                        if tw.pending_resize.is_some() {
                            tw.needs_redraw = true;
                        }
                    }
                }
            }
            ToGui::ResizeEnd(window_id, sz) => {
                // Drag-end debounce fired: no `Resized` for
                // `RESIZE_END_DEBOUNCE`. This is the only site that
                // writes OS-driven size updates back into the
                // graphix-level `tw.size` TRef — `ResizeTimer` and
                // the in-render `ws.resize` deal with the iced
                // viewport, which is independent of the graphix
                // size ref that user code reactively reads. Firing
                // once per burst (rather than once per render) also
                // keeps `last_set_size`'s echo dedupe sound; more
                // than one in-flight echo at a time and stale
                // `size.set`s leak out as `request_inner_size`
                // calls against the OS.
                if let Some(&bid) = self.win_to_bid.get(&window_id) {
                    if let Some(tw) = self.windows.get_mut(&bid) {
                        if tw.size.t.as_ref() != Some(&sz) {
                            tw.last_set_size = Some(sz);
                            if let Err(e) = tw.size.set(sz) {
                                error!("failed to set window size: {e:?}");
                            }
                        }
                        tw.needs_redraw = true;
                    }
                }
            }
            ToGui::Redraw => {
                // A background task (typically a data_table cell
                // subscription) mutated state outside the iced event
                // cycle. Flag every window for redraw so `about_to_wait`
                // runs the render pass and picks up the change.
                for tw in self.windows.values_mut() {
                    tw.needs_redraw = true;
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
        let mut next_redraw: Option<Instant> = None;
        for tw in self.windows.values_mut() {
            if !tw.needs_redraw {
                continue;
            }
            let win_id = tw.window_id();
            // Cap render rate while `pending_resize` is Some. In
            // normal drags the `ResizeTimer` cadence already limits
            // us to one render per `RESIZE_RENDER_PERIOD`; this is a
            // backstop for the edge case where an event slips
            // through the arm guard (e.g. a widget animation
            // firing `shell.request_redraw()` mid-drag).
            if tw.pending_resize.is_some() {
                let elapsed = tw.last_render.elapsed();
                if elapsed < RESIZE_RENDER_PERIOD {
                    let wake = tw.last_render + RESIZE_RENDER_PERIOD;
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
                // Let widgets flush deferred state (e.g. data_table
                // re-sorting after sort-column subscriptions arrive)
                // before we build the iced element tree.
                tw.content.before_view();
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
                        let redraw = match &state {
                            user_interface::State::Outdated => {
                                Some(window::RedrawRequest::NextFrame)
                            }
                            user_interface::State::Updated { redraw_request, .. } => {
                                match redraw_request {
                                    window::RedrawRequest::Wait => None,
                                    r => Some(*r),
                                }
                            }
                        };
                        tw.needs_redraw = redraw.is_some();
                        if let Some(r) = redraw {
                            let t = match r {
                                window::RedrawRequest::NextFrame => Instant::now(),
                                window::RedrawRequest::At(t) => t,
                                window::RedrawRequest::Wait => unreachable!(),
                            };
                            next_redraw = Some(next_redraw.map_or(t, |nr| nr.min(t)));
                        }
                    }
                    Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                        ws.surface.configure(&gpu.device, &ws.config);
                        tw.needs_redraw = true;
                        let now = Instant::now();
                        next_redraw = Some(next_redraw.map_or(now, |nr| nr.min(now)));
                        continue;
                    }
                    Err(e) => {
                        error!("surface frame error: {e:?}");
                        tw.needs_redraw = false;
                    }
                }
            }
        }
        // Drain messages by dispatching each one through each
        // window's widget tree via `on_message`. Widgets that need
        // to emit follow-ups (e.g. a `Call` from a column-resize
        // drag tick) publish through the shell, which we feed back
        // into the queue. `Call` is the one message the event loop
        // handles directly — there's no widget to forward it to;
        // it's just a side-effect on the graphix runtime.
        // `VecDeque` + `pop_front` preserves FIFO order (matches the
        // original `Vec::drain(..)` semantics). LIFO would reorder
        // dependent messages (e.g. CellEdit → CellEditSubmit).
        let mut pending: std::collections::VecDeque<Message> =
            self.messages.drain(..).collect();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => {
                    if let Err(e) = self.gx.call(id, args) {
                        error!("failed to call: {e:?}");
                    }
                }
                other => {
                    for tw in self.windows.values_mut() {
                        let mut shell = MessageShell::new(tw.cursor_position);
                        if tw.content.on_message(&other, &mut shell) {
                            tw.needs_redraw = true;
                        }
                        pending.extend(shell.out.drain(..));
                    }
                }
            }
        }

        let wake = match (deferred_until, next_redraw) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (a, b) => a.or(b),
        };
        if let Some(wake) = wake {
            event_loop.set_control_flow(ControlFlow::WaitUntil(wake));
        } else {
            event_loop.set_control_flow(ControlFlow::Wait);
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
    let proxy = event_loop.create_proxy();
    let _ = crate::REDRAW_WAKER.set(crate::RedrawWaker::new(proxy.clone()));
    let _ = proxy_tx.send(proxy);
    let resize_proxy = event_loop.create_proxy();
    let (resize_end_tx, resize_end_rx) = mpsc::unbounded_channel();
    let resize_end_proxy = event_loop.create_proxy();
    rt.spawn(resize_end_debounce(resize_end_rx, resize_end_proxy));
    let mut handler = GuiHandler {
        gx,
        root_exp,
        gpu: None,
        rt,
        stop: Some(stop),
        windows: IntMap::default(),
        win_to_bid: AHashMap::default(),
        surfaces: AHashMap::default(),
        ui_caches: AHashMap::default(),
        clipboard: Clipboard::new(),
        resize_proxy,
        resize_end_tx,
        messages: LPooled::take(),
        modifiers: ModifiersState::default(),
    };
    if let Err(e) = event_loop.run_app(&mut handler) {
        error!("gui event loop error: {e:?}");
    }
}

/// Debounces `Resized` events per window. On each message, records
/// the latest size and (re)sets a timer to `RESIZE_END_DEBOUNCE`.
/// When the timer elapses with no new messages, fires exactly one
/// `ToGui::ResizeEnd` carrying the last seen size. This is what
/// makes the runtime's size ref update fire once at drag-end rather
/// than N times during the drag.
async fn resize_end_debounce(
    mut rx: mpsc::UnboundedReceiver<(WindowId, SizeV)>,
    proxy: EventLoopProxy<ToGui>,
) {
    use tokio::time::{Duration, Instant, sleep_until};
    let far = || Instant::now() + Duration::from_secs(86400);
    let timer = sleep_until(far());
    tokio::pin!(timer);
    let mut pending: LPooled<AHashMap<WindowId, SizeV>> = LPooled::take();
    loop {
        tokio::select! {
            msg = rx.recv() => match msg {
                Some((wid, sz)) => {
                    pending.insert(wid, sz);
                    timer.as_mut().reset(Instant::now() + RESIZE_END_DEBOUNCE);
                }
                None => break,
            },
            _ = &mut timer => {
                for (wid, sz) in pending.drain() {
                    let _ = proxy.send_event(ToGui::ResizeEnd(wid, sz));
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
    windows: &mut IntMap<BindId, TrackedWindow<X>>,
    win_to_bid: &mut AHashMap<WindowId, BindId>,
    surfaces: &mut AHashMap<WindowId, WindowSurface>,
    ui_caches: &mut AHashMap<WindowId, user_interface::Cache>,
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
