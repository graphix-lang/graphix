//! Main GUI event loop: runs on the main OS thread via winit's
//! `run_app`; graphix updates arrive as `EventLoopProxy<ToGui>` user
//! events. One window per BindId in the root `Array<&Window>`.

use crate::{
    ToGui, convert,
    frame::{apply_messages, frame},
    render::{GpuState, WindowSurface},
    types::WindowSizeV,
    widgets::{Message, MessageShell},
    window::{ResolvedWindow, TrackedWindow},
};
use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::{BindId, FAILURE_TARGET};
use graphix_package::{Ended, Stop};
use graphix_rt::{CompExp, GXExt, GXHandle, Ref};
use iced_core::{InputMethod, clipboard, mouse, window};
use iced_runtime::user_interface;
use iced_wgpu::wgpu;
use indexmap::IndexMap;
use log::error;
use netidx::publisher::Value;
use nohash::{BuildNoHashHasher, IntMap, IntSet};
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    sync::Arc,
    time::{Duration, Instant},
};
use tokio::sync::{mpsc, oneshot};
use winit::{
    application::ApplicationHandler,
    dpi::{LogicalPosition, LogicalSize},
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy},
    keyboard::ModifiersState,
    platform::run_on_demand::EventLoopExtRunOnDemand,
    window::{CursorIcon, WindowId},
};

/// The interface's clipboard: the process's one instance.
struct Clipboard;

impl clipboard::Clipboard for Clipboard {
    fn read(&self, kind: clipboard::Kind) -> Option<String> {
        crate::clipboard::clipboard(|cb| match kind {
            clipboard::Kind::Standard => cb.get_text(),
            #[cfg(target_os = "linux")]
            clipboard::Kind::Primary => {
                use arboard::GetExtLinux;
                cb.get().clipboard(arboard::LinuxClipboardKind::Primary).text()
            }
            #[cfg(not(target_os = "linux"))]
            clipboard::Kind::Primary => Err(arboard::Error::ClipboardNotSupported),
        })
        .ok()
    }

    fn write(&mut self, kind: clipboard::Kind, contents: String) {
        let _ = crate::clipboard::clipboard(|cb| match kind {
            clipboard::Kind::Standard => cb.set_text(contents),
            #[cfg(target_os = "linux")]
            clipboard::Kind::Primary => {
                use arboard::SetExtLinux;
                cb.set().clipboard(arboard::LinuxClipboardKind::Primary).text(contents)
            }
            #[cfg(not(target_os = "linux"))]
            clipboard::Kind::Primary => Ok(()),
        });
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
    /// One per window open, its surface and interface state inside, in
    /// the order they opened.
    windows: IndexMap<BindId, TrackedWindow<X>, BuildNoHashHasher<BindId>>,
    win_to_bid: AHashMap<WindowId, BindId>,
    /// Windows the user closed: kept closed until the program drops them
    /// from its root array.
    closed: IntSet<BindId>,
    /// Windows in the root array whose value has not arrived yet.
    pending: IntMap<BindId, Ref<X>>,
    clipboard: Clipboard,
    /// Proxy for the resize-burst render timer tasks.
    resize_proxy: EventLoopProxy<ToGui>,
    /// Channel into `resize_end_debounce`; carries every `Resized` size.
    resize_end_tx: mpsc::UnboundedSender<(WindowId, WindowSizeV)>,
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

fn redraw_deadline(dirty: bool, at: Option<Instant>, now: Instant) -> Option<Instant> {
    if dirty { Some(now) } else { at }
}

/// Open each of `bids`, a failure stopping none after it: the first
/// failure is the result, each later one is reported here.
fn open_windows(
    bids: &[BindId],
    mut open: impl FnMut(BindId) -> Result<()>,
) -> Result<()> {
    let mut first_error = None;
    for &bid in bids {
        if let Err(e) = open(bid) {
            match first_error {
                None => first_error = Some(e),
                Some(_) => error!(target: FAILURE_TARGET, "open window: {e:?}"),
            }
        }
    }
    match first_error {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn draw_frame<T>(frame: T, draw: impl FnOnce(&T)) -> T {
    if let Err(panic) = catch_unwind(AssertUnwindSafe(|| draw(&frame))) {
        // wgpu discards an unpresented texture only outside unwinding.
        drop(frame);
        resume_unwind(panic);
    }
    frame
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{cell::Cell, panic::panic_any};

    #[test]
    fn an_unpresented_frame_drops_before_unwinding_resumes() {
        struct Frame<'a>(&'a Cell<Option<bool>>);
        impl Drop for Frame<'_> {
            fn drop(&mut self) {
                self.0.set(Some(std::thread::panicking()));
            }
        }
        let unwinding = Cell::new(None);
        let panic = catch_unwind(AssertUnwindSafe(|| {
            draw_frame(Frame(&unwinding), |_| panic_any(42_u8));
        }))
        .unwrap_err();
        assert_eq!(panic.downcast_ref::<u8>(), Some(&42));
        assert_eq!(unwinding.get(), Some(false));
    }

    #[test]
    fn redraw_waits_for_the_cursor_deadline_unless_dirty() {
        let now = Instant::now();
        let blink = now + Duration::from_millis(500);
        assert_eq!(redraw_deadline(false, Some(blink), now), Some(blink));
        assert_eq!(redraw_deadline(true, Some(blink), now), Some(now));
        assert_eq!(redraw_deadline(false, None, now), None);
        assert!(redraw_deadline(false, Some(now), blink).unwrap() <= blink);
    }

    #[test]
    fn a_failed_window_does_not_skip_later_windows() {
        let bids = [BindId::new(), BindId::new(), BindId::new()];
        let mut visited = 0;
        let result = open_windows(&bids, |bid| {
            assert_eq!(bid, bids[visited]);
            visited += 1;
            if visited == 2 { Ok(()) } else { anyhow::bail!("window {visited}") }
        });
        assert_eq!(visited, 3);
        assert_eq!(result.unwrap_err().to_string(), "window 1");
        assert!(open_windows(&bids, |_| Ok(())).is_ok());
    }
}

impl<X: GXExt> GuiHandler<X> {
    fn window_mut(&mut self, id: WindowId) -> Option<&mut TrackedWindow<X>> {
        self.windows.get_mut(self.win_to_bid.get(&id)?)
    }

    /// Drop a window, and the GPU with the last one.
    fn remove(&mut self, bid: BindId) {
        if let Some(tw) = self.windows.shift_remove(&bid) {
            self.win_to_bid.remove(&tw.window_id());
        }
        if self.windows.is_empty() {
            self.gpu = None;
        }
    }

    fn finish(&mut self, event_loop: &ActiveEventLoop, result: Result<Ended>) {
        self.windows.clear();
        self.win_to_bid.clear();
        self.pending.clear();
        self.gpu = None;
        if let Some(s) = self.stop.take() {
            let _ = s.send(result);
        }
        event_loop.exit();
    }

    /// Open the window a root array names, once its value is known.
    fn open(
        &mut self,
        event_loop: &ActiveEventLoop,
        bid: BindId,
        wref: Ref<X>,
    ) -> Result<()> {
        let Some(window_value) = wref.last.clone() else {
            self.pending.insert(bid, wref);
            return Ok(());
        };
        let resolved = self
            .rt
            .block_on(ResolvedWindow::compile(self.gx.clone(), window_value))
            .context("resolve window")?;
        let window = Arc::new(
            event_loop
                .create_window(resolved.window_attrs())
                .context("failed to create window")?,
        );
        let gpu = match &mut self.gpu {
            Some(gpu) => gpu,
            gpu @ None => {
                let g = self
                    .rt
                    .block_on(GpuState::new(window.clone()))
                    .context("gpu init")?;
                gpu.insert(g)
            }
        };
        let surface =
            WindowSurface::new(gpu, window.clone()).context("window surface creation")?;
        self.win_to_bid.insert(window.id(), bid);
        self.windows.insert(bid, resolved.into_tracked(wref, window, surface));
        Ok(())
    }

    /// Reconcile the windows with a new root `Array<&Window>` (bind ids).
    fn reconcile(&mut self, event_loop: &ActiveEventLoop, root: Value) -> Result<()> {
        let bids =
            root.cast_to::<LPooled<Vec<BindId>>>().context("root array of references")?;
        let gone = self
            .windows
            .keys()
            .filter(|bid| !bids.contains(bid))
            .copied()
            .collect::<LPooled<Vec<BindId>>>();
        for bid in gone.iter() {
            self.remove(*bid);
        }
        self.closed.retain(|bid| bids.contains(bid));
        self.pending.retain(|bid, _| bids.contains(bid));
        open_windows(&bids, |bid| {
            if self.windows.contains_key(&bid)
                || self.closed.contains(&bid)
                || self.pending.contains_key(&bid)
            {
                return Ok(());
            }
            let wref = self
                .rt
                .block_on(self.gx.compile_ref(bid))
                .context("compile_ref for window bind id")?;
            self.open(event_loop, bid, wref)
        })
    }

    /// Render a window that needs it; the time it next wants a frame.
    fn render(&mut self, bid: BindId) -> Option<Instant> {
        let gpu = self.gpu.as_ref()?;
        let tw = self.windows.get_mut(&bid)?;
        let now = Instant::now();
        let deadline = redraw_deadline(tw.needs_redraw, tw.redraw_at, now)?;
        if deadline > now {
            return Some(deadline);
        }
        if tw.pending_resize.is_some() && tw.last_render.elapsed() < RESIZE_RENDER_PERIOD
        {
            return Some(tw.last_render + RESIZE_RENDER_PERIOD);
        }
        if let Some((pw, ph, scale)) = tw.pending_resize.take() {
            tw.surface.resize(gpu, pw, ph, scale);
            let size = tw.surface.logical_size();
            tw.push_event(iced_core::Event::Window(window::Event::Resized(size)));
        }
        let theme = tw.iced_theme();
        let state = frame(
            &mut tw.props.content.w,
            tw.surface.logical_size(),
            &mut tw.cache,
            &mut tw.surface.renderer,
            &tw.pending_events,
            tw.cursor,
            &mut self.clipboard,
            &mut self.messages,
            &theme,
        );
        tw.pending_events.clear();
        if let user_interface::State::Updated { input_method, .. } = &state {
            match input_method {
                InputMethod::Disabled if tw.ime_enabled => {
                    tw.window.set_ime_allowed(false);
                    tw.ime_enabled = false;
                }
                InputMethod::Disabled => (),
                InputMethod::Enabled { cursor, purpose, .. } => {
                    if !tw.ime_enabled {
                        tw.window.set_ime_allowed(true);
                        tw.ime_enabled = true;
                    }
                    tw.window.set_ime_cursor_area(
                        LogicalPosition::new(cursor.x, cursor.y),
                        LogicalSize::new(cursor.width, cursor.height),
                    );
                    tw.window.set_ime_purpose(convert::ime_purpose(*purpose));
                }
            }
        }
        if let user_interface::State::Updated { mouse_interaction, .. } = &state
            && tw.last_mouse_interaction != *mouse_interaction
        {
            tw.last_mouse_interaction = *mouse_interaction;
            match mouse_interaction {
                mouse::Interaction::Hidden => tw.window.set_cursor_visible(false),
                _ => {
                    tw.window.set_cursor_visible(true);
                    tw.window.set_cursor(mouse_interaction_to_cursor(*mouse_interaction));
                }
            }
        }
        let ws = &mut tw.surface;
        match ws.surface.get_current_texture() {
            Ok(frame) => {
                let frame = draw_frame(frame, |frame| {
                    let view = frame
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default());
                    let background = theme.palette().background;
                    ws.renderer.present(
                        Some(background),
                        gpu.format,
                        &view,
                        &ws.viewport,
                    );
                });
                frame.present();
                tw.last_render = Instant::now();
                let next = match &state {
                    user_interface::State::Outdated => Some(Instant::now()),
                    user_interface::State::Updated { redraw_request, .. } => {
                        match redraw_request {
                            window::RedrawRequest::Wait => None,
                            window::RedrawRequest::NextFrame => Some(Instant::now()),
                            window::RedrawRequest::At(t) => Some(*t),
                        }
                    }
                };
                tw.needs_redraw = false;
                tw.redraw_at = next;
                next
            }
            Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                ws.surface.configure(&gpu.device, &ws.config);
                tw.needs_redraw = true;
                Some(Instant::now())
            }
            Err(e) => {
                error!("surface frame error: {e:?}");
                tw.needs_redraw = false;
                tw.redraw_at = None;
                None
            }
        }
    }

    /// Apply what the widgets published; a message changing a widget
    /// wants a frame.
    fn apply_messages(&mut self) {
        let windows = &mut self.windows;
        apply_messages(&self.gx, self.messages.drain(..), |msg, pending| {
            let mut handled = false;
            for tw in windows.values_mut() {
                let mut shell = MessageShell::default();
                if tw.props.content.w.on_message(msg, &mut shell) {
                    tw.needs_redraw = true;
                }
                pending.extend(shell.out.drain(..));
                handled |= shell.call_handled;
            }
            handled
        });
    }
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
        if let WindowEvent::CloseRequested = &event {
            if let Some(bid) = self.win_to_bid.get(&window_id).copied() {
                self.closed.insert(bid);
                self.remove(bid);
            }
            if self.windows.is_empty() {
                self.finish(event_loop, Ok(Ended::Requested));
            }
            return;
        }
        let (proxy, end_tx, modifiers) =
            (self.resize_proxy.clone(), self.resize_end_tx.clone(), self.modifiers);
        let rt = self.rt.clone();
        let Some(tw) = self.window_mut(window_id) else { return };
        match &event {
            WindowEvent::Resized(size) => {
                let scale = tw.window.scale_factor();
                tw.pending_resize = Some((size.width, size.height, scale));
                if !tw.resize_render_timer_armed {
                    tw.resize_render_timer_armed = true;
                    rt.spawn(async move {
                        tokio::time::sleep(RESIZE_RENDER_PERIOD).await;
                        let _ = proxy.send_event(ToGui::ResizeRenderTick(window_id));
                    });
                }
                if let Some(sz) = WindowSizeV::reported(*size, scale) {
                    let _ = end_tx.send((window_id, sz));
                }
            }
            WindowEvent::RedrawRequested => {
                if !tw.resize_render_timer_armed {
                    tw.needs_redraw = true;
                }
            }
            _ => {
                let scale = tw.window.scale_factor();
                let mut iced_events = convert::window_event(&event, scale, modifiers);
                for ev in iced_events.drain(..) {
                    crate::window::track_cursor(&mut tw.cursor, &ev);
                    tw.push_event(ev);
                }
            }
        }
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, event: ToGui) {
        match event {
            ToGui::Stop(tx) => {
                if !tx.is_closed() {
                    let _ = tx.send(());
                    self.finish(event_loop, Ok(Ended::Requested));
                }
            }
            ToGui::ResizeRenderTick(window_id) => {
                if let Some(tw) = self.window_mut(window_id) {
                    tw.resize_render_timer_armed = false;
                    tw.needs_redraw = true;
                }
            }
            ToGui::ResizeSettled(window_id, sz) => {
                if let Some(tw) = self.window_mut(window_id) {
                    if tw.props.size.t.as_ref() != Some(&sz) {
                        tw.last_set_size = Some(sz);
                        // through the reference, to what it names
                        if let Err(e) = tw.props.size.set_deref(sz) {
                            error!(target: FAILURE_TARGET, "failed to set window size: {e:?}");
                        }
                    }
                    tw.needs_redraw = true;
                }
            }
            ToGui::Redraw => {
                for tw in self.windows.values_mut() {
                    tw.needs_redraw = true;
                }
            }
            ToGui::Idle => {
                for tw in self.windows.values_mut() {
                    tw.idle();
                }
            }
            ToGui::Update(id, v) => {
                if id == self.root_exp.id {
                    if let Err(e) = self.reconcile(event_loop, v) {
                        error!(target: FAILURE_TARGET, "reconcile windows: {e:?}");
                        if self.windows.is_empty() && self.pending.is_empty() {
                            self.finish(event_loop, Err(e));
                        }
                    }
                    return;
                }
                let arrived =
                    self.pending.iter().find(|(_, r)| r.id == id).map(|(b, _)| *b);
                if let Some(bid) = arrived
                    && let Some(mut wref) = self.pending.remove(&bid)
                {
                    wref.last = Some(v);
                    if let Err(e) = self.open(event_loop, bid, wref) {
                        error!(target: FAILURE_TARGET, "open window: {e:?}");
                        if self.windows.is_empty() && self.pending.is_empty() {
                            self.finish(event_loop, Err(e));
                        }
                    }
                    return;
                }
                // CR claude for eric: [perf/low] Each ToGui::Update walks every widget
                // of every window: each widget compares the id with each of its refs
                // and recurses into its children, through Child::update and
                // Children::update. A cycle costs updates x widgets x props comparisons
                // and dynamic calls on the main thread. A 1000-widget UI that updates a
                // few hundred values a cycle does on the order of a million of them
                // before drawing. Options: an IntMap<ExprId, owner> filled when refs
                // and children compile and cleared when they recompile, so an update
                // goes to its owner only. Or keep the walk and accept the cost. The
                // index has to track widget recompiles. (gui-core-08)
                // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no
                // ruling needed. Build the IntMap<ExprId, owner> index, maintained on
                // widget recompile.
                // 2026-10-24 claude: Not obvious after all, so back to Eric (question 49
                // in cr-questions.md): the index needs every one of the 33 widget types
                // to list its refs and to follow subtree recompiles, and a ref one omits
                // silently stops updating, which the walk cannot do. Proposed: measure
                // the walk on a synthetic 1000-widget UI first.
                // 2026-10-24 claude: Eric ruled (2026-10-24): measure first, on a
                // synthetic 1000-widget UI, and decide on the numbers.
                // 2026-10-25 claude: Measured (quick profile, current_thread harness,
                // headless): a column of n text widgets, each over its own reference to
                // one variable, so one write delivers n updates and each walks all n
                // widgets. The walk alone (the update_widget calls over the delivered
                // batches, the runtime's cycle excluded), median of 10 writes: n=100, 80
                // us a write (0.8 us an update); n=1000, 5.0 ms a write (5.0 us an
                // update, about 5 ns a widget visited). The whole write at n=1000,
                // runtime cycle and walk, is 31 ms median (12 to 57), so the walk is a
                // sixth of it; at a few hundred updates in a 1000-widget UI it is about
                // 1.5 ms. Quadratic as stated, and visible at the frame budget only when
                // most of a 1000-widget UI updates at once. Back to eric to decide:
                // build the index, or accept.
                for tw in self.windows.values_mut() {
                    if let Err(e) = tw.handle_update(&self.rt, id, &v) {
                        error!(target: FAILURE_TARGET, "handle_update: {e:?}");
                    }
                }
            }
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let bids = self.windows.keys().copied().collect::<LPooled<Vec<BindId>>>();
        let mut wake = bids.iter().filter_map(|bid| self.render(*bid)).min();
        self.apply_messages();
        // a message that changed a widget wants the frame that shows it
        if self.windows.values().any(|tw| tw.needs_redraw && tw.pending_resize.is_none())
        {
            wake = Some(Instant::now());
        }
        event_loop.set_control_flow(match wake {
            Some(wake) => ControlFlow::WaitUntil(wake),
            None => ControlFlow::Wait,
        });
    }
}

thread_local! {
    /// winit allows one event loop per process, and refuses a second build
    /// even after a failed first: built on the main thread by the first
    /// session and run again by each later one, or the first build's error.
    static EVENT_LOOP: RefCell<Option<Result<EventLoop<ToGui>, String>>> =
        const { RefCell::new(None) };
}

pub(crate) fn run<X: GXExt>(
    gx: GXHandle<X>,
    root_exp: CompExp<X>,
    proxy_tx: oneshot::Sender<Result<EventLoopProxy<ToGui>>>,
    stop: Stop,
    rt: tokio::runtime::Handle,
) {
    let built = EVENT_LOOP.with(|l| l.borrow_mut().take()).unwrap_or_else(|| {
        EventLoop::<ToGui>::with_user_event().build().map_err(|e| format!("{e}"))
    });
    let mut event_loop = match built {
        Ok(el) => el,
        Err(e) => {
            let _ = proxy_tx.send(Err(anyhow::anyhow!("creating the event loop: {e}")));
            EVENT_LOOP.with(|l| *l.borrow_mut() = Some(Err(e)));
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
        windows: IndexMap::default(),
        win_to_bid: AHashMap::default(),
        closed: IntSet::default(),
        pending: IntMap::default(),
        clipboard: Clipboard,
        resize_proxy,
        resize_end_tx,
        messages: LPooled::take(),
        modifiers: ModifiersState::default(),
    };
    if let Err(e) = event_loop.run_app_on_demand(&mut handler)
        && let Some(s) = handler.stop.take()
    {
        let _ = s.send(Err(e).context("running the event loop"));
    }
    EVENT_LOOP.with(|l| *l.borrow_mut() = Some(Ok(event_loop)));
}

/// Debounces `Resized` events per window: fires one `ToGui::ResizeSettled`
/// with the last size after `RESIZE_END_DEBOUNCE` of quiet.
async fn resize_end_debounce(
    mut rx: mpsc::UnboundedReceiver<(WindowId, WindowSizeV)>,
    proxy: EventLoopProxy<ToGui>,
) {
    use tokio::time::{Duration, Instant, sleep_until};
    let far = || Instant::now() + Duration::from_secs(86400);
    let timer = sleep_until(far());
    tokio::pin!(timer);
    let mut pending: LPooled<AHashMap<WindowId, WindowSizeV>> = LPooled::take();
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
