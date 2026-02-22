use crate::{
    compile,
    types::{SizeV, ThemeV},
    GuiW,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::{mouse, Size};
use netidx::publisher::Value;
use std::sync::Arc;
use tokio::try_join;
use winit::window::{Window, WindowId};

/// Per-window state tracking.
pub(crate) struct TrackedWindow<X: GXExt> {
    pub window: Arc<Window>,
    pub title: TRef<X, String>,
    pub size: TRef<X, SizeV>,
    pub theme: TRef<X, ThemeV>,
    pub content_ref: Ref<X>,
    pub content: GuiW<X>,
    pub cursor_position: iced_core::Point,
    pub pending_events: Vec<iced_core::Event>,
    pub needs_redraw: bool,
}

impl<X: GXExt> TrackedWindow<X> {
    pub async fn compile(
        gx: GXHandle<X>,
        window: Arc<Window>,
        source: Value,
    ) -> Result<Self> {
        let [(_, content), (_, size), (_, theme), (_, title)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("window flds")?;
        let (content_ref, size, theme, title) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(size),
            gx.compile_ref(theme),
            gx.compile_ref(title),
        }?;
        let compiled_content: GuiW<X> = match content_ref.last.as_ref() {
            None => Box::new(crate::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("window content")?,
        };
        let title_ref: TRef<X, String> = TRef::new(title).context("window tref title")?;
        if let Some(t) = title_ref.t.as_ref() {
            window.set_title(t);
        }
        let size_ref: TRef<X, SizeV> = TRef::new(size).context("window tref size")?;
        if let Some(sz) = size_ref.t.as_ref() {
            let _ = window.request_inner_size(winit::dpi::LogicalSize::new(
                sz.0.width, sz.0.height,
            ));
        }
        Ok(Self {
            window,
            title: title_ref,
            size: size_ref,
            theme: TRef::new(theme).context("window tref theme")?,
            content_ref,
            content: compiled_content,
            cursor_position: iced_core::Point::ORIGIN,
            pending_events: Vec::new(),
            needs_redraw: true,
        })
    }

    pub fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<bool> {
        if let Some(t) = self.title.update(id, v).context("window update title")? {
            self.window.set_title(t);
            self.needs_redraw = true;
        }
        if let Some(sz) = self.size.update(id, v).context("window update size")? {
            let _ = self.window.request_inner_size(winit::dpi::LogicalSize::new(
                sz.0.width, sz.0.height,
            ));
        }
        if self.theme.update(id, v).context("window update theme")?.is_some() {
            self.needs_redraw = true;
        }
        self.content.handle_update(id, v)?;
        self.needs_redraw = true;
        // Return true if children ref changed (needs recompile)
        Ok(id == self.content_ref.id)
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

    pub fn viewport_size(&self) -> Size {
        let phys = self.window.inner_size();
        let scale = self.window.scale_factor();
        Size::new(
            (phys.width as f64 / scale) as f32,
            (phys.height as f64 / scale) as f32,
        )
    }

    pub fn cursor(&self) -> mouse::Cursor {
        mouse::Cursor::Available(self.cursor_position)
    }
}
