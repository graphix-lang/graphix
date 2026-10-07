use crate::{
    render::WindowSurface,
    types::{ImageSourceV, SizeV, ThemeV},
    widgets::Child,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::mouse;
use iced_runtime::user_interface::Cache;
use netidx::publisher::Value;
use std::{sync::Arc, time::Instant};

use futures::try_join;
use winit::window::{Window, WindowAttributes, WindowId};

/// Resolved window state — all refs compiled but no OS window yet.
pub struct ResolvedWindow<X: GXExt> {
    pub gx: GXHandle<X>,
    pub title: TRef<X, ArcStr>,
    pub size: TRef<X, SizeV>,
    pub theme: TRef<X, ThemeV>,
    pub icon: TRef<X, ImageSourceV>,
    pub decoded_icon: Option<winit::window::Icon>,
    pub content: Child<X>,
}

impl<X: GXExt> ResolvedWindow<X> {
    /// Compile a window struct value into resolved refs without creating an OS window.
    pub async fn compile(gx: GXHandle<X>, source: Value) -> Result<Self> {
        let (content, icon, size, theme, title) = try_join!(
            Child::field(&gx, &source, "content"),
            gx.compile_field(&source, "icon"),
            gx.compile_field(&source, "size"),
            gx.compile_field(&source, "theme"),
            gx.compile_field(&source, "title"),
        )
        .context("window")?;
        let icon = TRef::new(icon).context("window tref icon")?;
        let decoded_icon =
            icon.t.as_ref().and_then(|s: &ImageSourceV| match s.decode_icon() {
                Ok(i) => i,
                Err(e) => {
                    log::warn!("failed to decode window icon: {e}");
                    None
                }
            });
        Ok(Self {
            gx,
            title: TRef::new(title).context("window tref title")?,
            size: TRef::new(size).context("window tref size")?,
            theme: TRef::new(theme).context("window tref theme")?,
            icon,
            decoded_icon,
            content,
        })
    }

    /// Build winit WindowAttributes from the resolved title/size refs.
    pub fn window_attrs(&self) -> WindowAttributes {
        let title = self.title.t.as_ref().map(|t| t.as_str()).unwrap_or("Graphix");
        let (w, h) = self
            .size
            .t
            .as_ref()
            .map(|sz| (sz.0.width, sz.0.height))
            .unwrap_or((800.0, 600.0));
        WindowAttributes::default()
            .with_title(title)
            .with_inner_size(winit::dpi::LogicalSize::new(w, h))
            .with_window_icon(self.decoded_icon.clone())
    }

    /// Consume self and attach an OS window and its surface, producing a
    /// TrackedWindow.
    pub fn into_tracked(
        self,
        window_ref: Ref<X>,
        window: Arc<Window>,
        surface: WindowSurface,
    ) -> TrackedWindow<X> {
        TrackedWindow {
            window_ref,
            gx: self.gx,
            window,
            title: self.title,
            size: self.size,
            theme: self.theme,
            icon: self.icon,
            decoded_icon: self.decoded_icon,
            content: self.content,
            surface,
            cache: Cache::default(),
            cursor: mouse::Cursor::Unavailable,
            ime_enabled: false,
            last_mouse_interaction: mouse::Interaction::default(),
            pending_events: Vec::new(),
            needs_redraw: true,
            last_set_size: None,
            pending_resize: None,
            resize_render_timer_armed: false,
            last_render: Instant::now(),
        }
    }
}

/// The pointer as iced sees it after `ev`: where it last moved, and
/// unavailable once it has left the window.
pub fn track_cursor(cursor: &mut mouse::Cursor, ev: &iced_core::Event) {
    match ev {
        iced_core::Event::Mouse(mouse::Event::CursorMoved { position }) => {
            *cursor = mouse::Cursor::Available(*position)
        }
        iced_core::Event::Mouse(mouse::Event::CursorLeft) => {
            *cursor = mouse::Cursor::Unavailable
        }
        _ => (),
    }
}

/// Per-window state tracking.
pub struct TrackedWindow<X: GXExt> {
    pub window_ref: Ref<X>,
    pub gx: GXHandle<X>,
    pub window: Arc<Window>,
    pub title: TRef<X, ArcStr>,
    pub size: TRef<X, SizeV>,
    pub theme: TRef<X, ThemeV>,
    pub icon: TRef<X, ImageSourceV>,
    pub decoded_icon: Option<winit::window::Icon>,
    pub content: Child<X>,
    pub surface: WindowSurface,
    /// The interface's state between frames.
    pub cache: Cache,
    /// Where the pointer is, unavailable while it is outside the window.
    pub cursor: mouse::Cursor,
    /// Whether the window takes IME input, as the interface last asked.
    pub ime_enabled: bool,
    pub last_mouse_interaction: mouse::Interaction,
    pub pending_events: Vec<iced_core::Event>,
    pub needs_redraw: bool,
    pub last_set_size: Option<SizeV>,
    pub pending_resize: Option<(u32, u32, f64)>,
    /// True while a resize render timer is armed; at most one per drag.
    pub resize_render_timer_armed: bool,
    pub last_render: Instant,
}

impl<X: GXExt> TrackedWindow<X> {
    pub fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<()> {
        if id == self.window_ref.id && self.window_ref.last.as_ref() != Some(v) {
            self.window_ref.last = Some(v.clone());
            let resolved = rt
                .block_on(ResolvedWindow::compile(self.gx.clone(), v.clone()))
                .context("window ref recompile")?;
            self.title = resolved.title;
            self.size = resolved.size;
            self.theme = resolved.theme;
            self.icon = resolved.icon;
            self.decoded_icon = resolved.decoded_icon;
            self.content = resolved.content;
            if let Some(t) = self.title.t.as_ref() {
                self.window.set_title(t);
            }
            if let Some(sz) = self.size.t.as_ref() {
                let _ = self.window.request_inner_size(winit::dpi::LogicalSize::new(
                    sz.0.width,
                    sz.0.height,
                ));
            }
            self.window.set_window_icon(self.decoded_icon.clone());
            self.needs_redraw = true;
            return Ok(());
        }
        let mut changed = false;
        if let Some(t) = self.title.update(id, v).context("window update title")? {
            self.window.set_title(t);
            changed = true;
        }
        if let Some(sz) = self.size.update(id, v).context("window update size")? {
            if self.last_set_size.take() != Some(*sz) {
                let _ = self.window.request_inner_size(winit::dpi::LogicalSize::new(
                    sz.0.width,
                    sz.0.height,
                ));
            }
            changed = true;
        }
        if self.theme.update(id, v).context("window update theme")?.is_some() {
            changed = true;
        }
        if self.icon.update(id, v).context("window update icon")?.is_some() {
            self.decoded_icon =
                self.icon.t.as_ref().and_then(|s: &ImageSourceV| match s.decode_icon() {
                    Ok(i) => i,
                    Err(e) => {
                        log::warn!("failed to decode window icon: {e}");
                        None
                    }
                });
            self.window.set_window_icon(self.decoded_icon.clone());
            changed = true;
        }
        changed |= self.content.update(rt, &self.gx, id, v).context("window content")?;
        self.needs_redraw |= changed;
        Ok(())
    }

    pub fn window_id(&self) -> WindowId {
        self.window.id()
    }

    pub fn iced_theme(&self) -> crate::theme::GraphixTheme {
        self.theme.t.as_ref().map(|t| t.0.clone()).unwrap_or(crate::theme::GraphixTheme {
            inner: iced_core::Theme::Dark,
            overrides: None,
        })
    }

    pub fn push_event(&mut self, event: iced_core::Event) {
        self.pending_events.push(event);
        // During a resize drag the render timer drives redraws.
        if !self.resize_render_timer_armed {
            self.needs_redraw = true;
        }
    }
}
