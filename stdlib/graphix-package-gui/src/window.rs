use crate::{
    types::{SizeV, ThemeV},
    widgets::{compile, EmptyW, GuiW},
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::mouse;
use netidx::publisher::Value;
use std::sync::Arc;
use std::time::Instant;
use tokio::try_join;
use winit::window::{Window, WindowAttributes, WindowId};

/// Resolved window state — all refs compiled but no OS window yet.
pub(crate) struct ResolvedWindow<X: GXExt> {
    pub gx: GXHandle<X>,
    pub title: TRef<X, String>,
    pub size: TRef<X, SizeV>,
    pub theme: TRef<X, ThemeV>,
    pub content_ref: Ref<X>,
    pub content: GuiW<X>,
}

impl<X: GXExt> ResolvedWindow<X> {
    /// Compile a window struct value into resolved refs without creating an OS window.
    pub async fn compile(gx: GXHandle<X>, source: Value) -> Result<Self> {
        let [(_, content), (_, size), (_, theme), (_, title)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("window flds")?;
        let (content_ref, size, theme, title) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(size),
            gx.compile_ref(theme),
            gx.compile_ref(title),
        }?;
        let compiled_content: GuiW<X> = match content_ref.last.as_ref() {
            None => Box::new(EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("window content")?,
        };
        Ok(Self {
            gx,
            title: TRef::new(title).context("window tref title")?,
            size: TRef::new(size).context("window tref size")?,
            theme: TRef::new(theme).context("window tref theme")?,
            content_ref,
            content: compiled_content,
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
    }

    /// Consume self and attach an OS window, producing a TrackedWindow.
    pub fn into_tracked(
        self,
        window_ref: Ref<X>,
        window: Arc<Window>,
    ) -> TrackedWindow<X> {
        TrackedWindow {
            window_ref,
            gx: self.gx,
            window,
            title: self.title,
            size: self.size,
            theme: self.theme,
            content_ref: self.content_ref,
            content: self.content,
            cursor_position: iced_core::Point::ORIGIN,
            last_mouse_interaction: mouse::Interaction::default(),
            pending_events: Vec::new(),
            needs_redraw: true,
            last_set_size: None,
            last_resize_draw: Instant::now(),
        }
    }
}

/// Per-window state tracking.
pub(crate) struct TrackedWindow<X: GXExt> {
    pub window_ref: Ref<X>,
    pub gx: GXHandle<X>,
    pub window: Arc<Window>,
    pub title: TRef<X, String>,
    pub size: TRef<X, SizeV>,
    pub theme: TRef<X, ThemeV>,
    pub content_ref: Ref<X>,
    pub content: GuiW<X>,
    pub cursor_position: iced_core::Point,
    pub last_mouse_interaction: mouse::Interaction,
    pub pending_events: Vec<iced_core::Event>,
    pub needs_redraw: bool,
    pub last_set_size: Option<SizeV>,
    pub last_resize_draw: Instant,
}

impl<X: GXExt> TrackedWindow<X> {
    pub fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<()> {
        if id == self.window_ref.id {
            self.window_ref.last = Some(v.clone());
            let resolved = rt
                .block_on(ResolvedWindow::compile(self.gx.clone(), v.clone()))
                .context("window ref recompile")?;
            self.title = resolved.title;
            self.size = resolved.size;
            self.theme = resolved.theme;
            self.content_ref = resolved.content_ref;
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
        if id == self.content_ref.id {
            self.content_ref.last = Some(v.clone());
            self.content = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("window content recompile")?;
            changed = true;
        }
        changed |= self.content.handle_update(rt, id, v)?;
        self.needs_redraw |= changed;
        Ok(())
    }

    pub fn window_id(&self) -> WindowId {
        self.window.id()
    }

    pub fn iced_theme(&self) -> iced_core::Theme {
        self.theme.t.as_ref().map(|t| t.0.clone()).unwrap_or(iced_core::Theme::Dark)
    }

    pub fn push_event(&mut self, event: iced_core::Event) {
        self.pending_events.push(event);
        self.needs_redraw = true;
    }

    pub fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(graphix_compiler::BindId, Value)> {
        self.content.editor_action(id, action)
    }

    pub fn cursor(&self) -> mouse::Cursor {
        mouse::Cursor::Available(self.cursor_position)
    }
}
