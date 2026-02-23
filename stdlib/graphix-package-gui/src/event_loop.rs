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
    window::{ResolvedWindow, TrackedWindow},
    Message, ToGui,
};
use anyhow::{Context, Result};
use fxhash::FxHashMap;
use graphix_compiler::BindId;
use graphix_rt::{CompExp, GXExt, GXHandle};
use iced_core::{mouse, renderer::Style, Size};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use log::error;
use netidx::{
    protocol::valarray::ValArray,
    publisher::Value,
};
use poolshark::local::LPooled;
use std::sync::Arc;
use tokio::sync::oneshot;
use winit::{
    application::ApplicationHandler,
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy},
    keyboard::ModifiersState,
    window::WindowId,
};

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
                let scale = tw.window.scale_factor();
                let iced_events = convert::window_event(&event, scale, self.modifiers);
                for ev in iced_events {
                    if let iced_core::Event::Mouse(mouse::Event::CursorMoved {
                        position,
                    }) = &ev
                    {
                        tw.cursor_position = *position;
                    }
                    tw.push_event(ev);
                }
                if let WindowEvent::Resized(size) = &event {
                    if let Some(ws) = self.surfaces.get_mut(&window_id) {
                        let gpu = self.gpu.as_ref().unwrap();
                        ws.resize(gpu, size.width, size.height, tw.window.scale_factor());
                    }
                    let scale = tw.window.scale_factor();
                    let logical = size.to_logical::<f32>(scale);
                    let new_sz = SizeV(Size::new(logical.width, logical.height));
                    if tw.size.t.as_ref() != Some(&new_sz) {
                        tw.last_set_size = Some(new_sz);
                        if let Err(e) = tw.size.set(new_sz) {
                            error!("failed to set window size: {e:?}");
                        }
                    }
                    tw.needs_redraw = true;
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

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        let Some(gpu) = self.gpu.as_ref() else { return };
        for tw in self.windows.values_mut() {
            if !tw.needs_redraw {
                continue;
            }
            let win_id = tw.window_id();
            if let Some(ws) = self.surfaces.get_mut(&win_id) {
                let cache = self.ui_caches.remove(&win_id).unwrap_or_default();
                let element = tw.content.view();
                let viewport_size = ws.logical_size();
                let mut ui =
                    UserInterface::build(element, viewport_size, cache, &mut ws.renderer);
                let (_, _statuses) = ui.update(
                    &tw.pending_events,
                    tw.cursor(),
                    &mut ws.renderer,
                    &mut iced_core::clipboard::Null,
                    &mut self.messages,
                );
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
                    }
                    Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                        ws.surface.configure(&gpu.device, &ws.config);
                        tw.needs_redraw = true;
                        continue;
                    }
                    Err(e) => error!("surface frame error: {e:?}"),
                }
                tw.needs_redraw = false;
            }
        }

        for msg in self.messages.drain(..) {
            match msg {
                Message::Set(bid, v) => {
                    if let Err(e) = self.gx.set(bid, v) {
                        error!("failed to set ref: {e:?}");
                    }
                }
                Message::Call(id) => {
                    if let Err(e) = self.gx.call(id, ValArray::from_iter([Value::Null])) {
                        error!("failed to call: {e:?}");
                    }
                }
            }
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
        messages: Vec::new(),
        modifiers: ModifiersState::default(),
    };
    if let Err(e) = event_loop.run_app(&mut handler) {
        error!("gui event loop error: {e:?}");
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
