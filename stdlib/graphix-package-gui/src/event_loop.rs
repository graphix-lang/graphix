//! Main GUI event loop: runs on the main OS thread via winit's
//! `run_app`; graphix updates arrive as `EventLoopProxy<ToGui>` user
//! events. One window per BindId in the root `Array<&Window>`.

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
use graphix_package::Stop;
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
    stop: Option<Stop>,
    // CR claude for claude: [structure] Per-window state lives in four maps that must
    // agree: windows and win_to_bid, plus surfaces and ui_caches keyed by WindowId.
    // CloseRequested and reconcile_windows each repeat the four-map removal, Stop
    // clears them all, and reconcile_windows takes nine parameters to borrow the
    // pieces. Move the WindowSurface and the ui cache into TrackedWindow so there is
    // one map plus the WindowId index, one removal, and reconcile as a method. The
    // redraw cadence (needs_redraw, pending_resize, resize_render_timer_armed,
    // last_render) is likewise four pub fields written from two files, and they already
    // disagree. Input pushed while the resize timer is armed, after a frame already
    // took pending_resize, does not set needs_redraw, and ResizeRenderTick sets it only
    // if pending_resize is still set. That input then waits for ResizeSettled (about
    // 200 ms) or the next input event. (gui-core-17)
    windows: IntMap<BindId, TrackedWindow<X>>,
    win_to_bid: AHashMap<WindowId, BindId>,
    surfaces: AHashMap<WindowId, WindowSurface>,
    ui_caches: AHashMap<WindowId, user_interface::Cache>,
    clipboard: Clipboard,
    /// Proxy for the resize-burst render timer tasks.
    resize_proxy: EventLoopProxy<ToGui>,
    /// Channel into `resize_end_debounce`; carries every `Resized` size.
    resize_end_tx: mpsc::UnboundedSender<(WindowId, SizeV)>,
    /// Scratch buffer for `iced`-emitted messages, drained each `about_to_wait`.
    messages: LPooled<Vec<Message>>,
    modifiers: ModifiersState,
}

/// Render cadence during a resize drag. The timer is armed on the
/// first `Resized` of a burst and not reset by later ones.
const RESIZE_RENDER_PERIOD: Duration = Duration::from_millis(16);

/// Quiet time that counts as drag-end; must exceed
/// `RESIZE_RENDER_PERIOD`. The size ref is written once per drag.
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
                    if !tw.resize_render_timer_armed {
                        tw.resize_render_timer_armed = true;
                        let proxy = self.resize_proxy.clone();
                        self.rt.spawn(async move {
                            tokio::time::sleep(RESIZE_RENDER_PERIOD).await;
                            let _ = proxy.send_event(ToGui::ResizeRenderTick(window_id));
                        });
                    }
                    let logical = size.to_logical::<f32>(scale);
                    let _ = self.resize_end_tx.send((
                        window_id,
                        SizeV(Size::new(logical.width, logical.height)),
                    ));
                } else if let WindowEvent::RedrawRequested = &event {
                    // While a resize timer is armed it is the sole render driver.
                    if !tw.resize_render_timer_armed {
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
                    let _ = s.send(Ok(()));
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
                    let _ = s.send(Ok(()));
                }
                event_loop.exit();
            }
            ToGui::ResizeRenderTick(window_id) => {
                // Schedules a redraw only; `ResizeSettled` owns the `tw.size` write.
                if let Some(&bid) = self.win_to_bid.get(&window_id) {
                    if let Some(tw) = self.windows.get_mut(&bid) {
                        tw.resize_render_timer_armed = false;
                        if tw.pending_resize.is_some() {
                            tw.needs_redraw = true;
                        }
                    }
                }
            }
            ToGui::ResizeSettled(window_id, sz) => {
                // The only site writing OS-driven sizes into `tw.size`; once
                // per drag keeps `last_set_size`'s echo dedupe sound.
                if let Some(&bid) = self.win_to_bid.get(&window_id) {
                    if let Some(tw) = self.windows.get_mut(&bid) {
                        if tw.size.t.as_ref() != Some(&sz) {
                            tw.last_set_size = Some(sz);
                            // CR claude for claude: [bug] This writes the cell the `&`
                            // minted, not what the reference names. For `window(#size:
                            // &sz, ..)` that cell only mirrors `sz`, and `*w.size`
                            // reads `sz` through the byref chain, so after an OS resize
                            // `sz`, `*w.size` and any label built from `sz` keep the
                            // old size while `tw.size.t` holds the new one. A place
                            // `#size: &st.size` loses the resize the same way; only the
                            // default `&{..}` literal keeps it. Write through the
                            // reference instead: a place patches its root, a chained
                            // reference sets its bind, a chainless one sets the cell
                            // (`Ref::set_deref`, which the TUI uses, has no target for
                            // a place). probe:
                            // design/review-2026-10-05/repro/gui-core-10.rs, run as
                            // stdlib/graphix-package-gui/tests/review_gui_core_10.rs
                            // (sz stays 800 x 600 after ResizeSettled to 1024 x 768).
                            // (gui-core-10)
                            if let Err(e) = tw.size.set(sz) {
                                error!("failed to set window size: {e:?}");
                            }
                        }
                        tw.needs_redraw = true;
                    }
                }
            }
            ToGui::Redraw => {
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
            // Backstop for redraws that bypass the resize-timer cadence.
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
                // CR claude for claude: [bug] This frame never passes
                // window::Event::RedrawRequested(now) to ui.update before ui.draw. iced
                // 0.14 widgets set the status they draw with only on that event, and
                // view() rebuilds them every frame, so the status is always None. So
                // every button, text_input, checkbox and toggler draws with its
                // Disabled style even when it has a callback, and looks the same as
                // #disabled: &true. A ButtonSpec background is always dimmed, typed
                // text uses the placeholder colour, no widget shows hover, focus or
                // pressed, and the text cursor never blinks. iced's own shell calls
                // ui.update(&[Event::Window(window::Event::RedrawRequested(Instant::now()))],
                // ..) right before draw and folds that update's redraw request into the
                // next wakeup. probe: design/review-2026-10-05/repro/gui-core-01.rs
                // (copy it to stdlib/graphix-package-gui/tests/review_gui_core_01.rs,
                // then cargo test -p graphix-package-gui --test review_gui_core_01): an
                // enabled button renders [62,72,178], the disabled colour, where Active
                // is [88,101,242]. (gui-core-01)
                ui.draw(&mut ws.renderer, &theme, &style, tw.cursor());

                self.ui_caches.insert(win_id, ui.into_cache());
                tw.pending_events.clear();

                match ws.surface.get_current_texture() {
                    Ok(frame) => {
                        let view = frame
                            .texture
                            .create_view(&wgpu::TextureViewDescriptor::default());
                        // CR claude for claude: [bug] present(None) makes iced_wgpu load
                        // the freshly acquired surface texture (LoadOp::Load), which
                        // wgpu zero-fills, so the theme's background is never painted.
                        // Every window is black whatever its theme or palette
                        // background says, contrary to the book's "background -- window
                        // and container backgrounds". Under a light theme the default
                        // text is dark on that black; Light is black text on black, so
                        // the text is invisible. iced's own compositor passes
                        // Some(background_color); pass
                        // Some(Base::base(&theme.inner).background_color) here. probe:
                        // design/review-2026-10-05/repro/gui-core-03.rs (a headless
                        // test; copy it to
                        // stdlib/graphix-package-gui/tests/review_gui_core_03.rs and
                        // run it as its header says). (gui-core-03)
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
        // FIFO: dependent messages (CellEdit -> CellEditSubmit) must stay ordered.
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
                    // CR claude for claude: [bug] Every non-Call message goes to every
                    // window's content. The data-table messages (CellClick, CellEdit,
                    // CellEditInput, CellEditSubmit, CellEditCancel, TableKey, Scroll,
                    // ColumnResizeStart; widgets/mod.rs:105-126) name no widget, so
                    // every DataTableW in the program acts on every one of them. A
                    // click, arrow key or scroll in one table also selects, moves or
                    // scrolls every other table. An edit submitted in one table calls
                    // every other table's on_edit with that table's own cell path and
                    // the typed text, which is a write to a path the user never touched
                    // (on_edit is typically sys::net::write). Fix: carry the table's
                    // identity in these messages, as EditorAction carries its ExprId,
                    // and drop messages addressed to another table. probe:
                    // design/review-2026-10-05/repro/gui-core-04.rs (editing A's row 1
                    // also yields eb="b1/price=42" and ec="c1/price=42"). (gui-core-04)
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

        // CR claude for claude: [bug] The windows are drawn before the messages are
        // drained. A message that on_message applies sets needs_redraw (line 410) but
        // schedules no wake: wake comes only from deferred_until and next_redraw. The
        // loop goes back to ControlFlow::Wait with the window dirty, so the change
        // shows only at the next unrelated OS event. One wheel notch over an editable
        // text_editor stays unscrolled until the mouse moves, because Action::Scroll
        // makes no graphix call that would wake the loop. probe:
        // design/review-2026-10-05/repro/gui-core-06.py (headless kwin: the capture 2.5
        // s after the wheel equals the one before it; a 1 px motion then shows the
        // scroll). A window still dirty after the drain needs a wake (WaitUntil(now)),
        // or the messages must be applied and the UI rebuilt before the draw, as iced
        // does. (gui-core-06)
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
    proxy_tx: oneshot::Sender<Result<EventLoopProxy<ToGui>>>,
    stop: Stop,
    rt: tokio::runtime::Handle,
) {
    // CR claude for claude: [bug] Every GUI session builds a new winit EventLoop here,
    // but winit allows only one per process. build() sets a process-wide flag before it
    // touches the platform and never clears it, so every later build returns
    // RecreationAttempt. In the REPL, a second GUI (after the first window closes, or
    // after a failed start) fails with "creating the event loop: EventLoop can't be
    // recreated" until the shell is restarted. Build the loop once on the main thread
    // and run each session with run_app_on_demand; REDRAW_WAKER then stays valid.
    // probe: design/review-2026-10-05/repro/gui-core-11.py (gui-core-11)
    let event_loop = match EventLoop::<ToGui>::with_user_event().build() {
        Ok(el) => el,
        Err(e) => {
            let _ = proxy_tx.send(Err(e).context("creating the event loop"));
            return;
        }
    };
    let proxy = event_loop.create_proxy();
    let _ = crate::REDRAW_WAKER.set(crate::RedrawWaker::new(proxy.clone()));
    let _ = proxy_tx.send(Ok(proxy));
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
    if let Err(e) = event_loop.run_app(&mut handler)
        && let Some(s) = handler.stop.take()
    {
        let _ = s.send(Err(e).context("running the event loop"));
    }
}

/// Debounces `Resized` events per window: fires one `ToGui::ResizeSettled`
/// with the last size after `RESIZE_END_DEBOUNCE` of quiet.
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
                    let _ = proxy.send_event(ToGui::ResizeSettled(wid, sz));
                }
                timer.as_mut().reset(far());
            }
        }
    }
}

/// Reconcile the tracked windows with a new root `Array<&Window>`
/// (an array of `Value::U64(bindid)`).
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

    for &bid in new_bids.iter() {
        // CR claude for claude: [bug] A window the user closed is missing from `windows`
        // as well, because CloseRequested (line 203) removes it only from the loop's
        // maps. The program's root array still lists it, and the program is never told.
        // So the next root update, whatever causes it, reopens every window the user
        // closed. The fix is either to tell the program (an on_close callback or a
        // closed ref in Window), or to keep user-closed bids out of this loop until
        // they leave the array. probe: design/review-2026-10-05/repro/gui-core-09.sh
        // (close gxprobe-a, then the program adds gxprobe-b to the root, and gxprobe-a
        // comes back with a new X window id). (gui-core-09)
        if windows.contains_key(&bid) {
            continue;
        }
        let wref =
            rt.block_on(gx.compile_ref(bid)).context("compile_ref for window bind id")?;
        // CR claude for claude: [bug] If a window's value is still bottom when the root
        // array fires, the window is lost for good. Its Ref is dropped here, and `[&w]`
        // does not fire again when `w` arrives, because a reference fires its id only
        // at init. Nothing retries it, so unless the root array changes for another
        // reason the GUI runs with no window. Every child widget ref with no value is
        // kept and shown as EmptyW until it arrives (compile_child/update_child, and
        // ResolvedWindow::compile for content). Keep this Ref pending and create the
        // window on its first update. probe:
        // design/review-2026-10-05/repro/gui-core-12.gx (needs a desktop; its header
        // has the text-mode run showing the root fires only once). (gui-core-12)
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
