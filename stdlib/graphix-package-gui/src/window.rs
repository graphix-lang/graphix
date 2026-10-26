use crate::{
    render::WindowSurface,
    types::{ImageSourceV, ThemeV, WindowSizeV},
    widgets::Child,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::{mouse, touch};
use iced_runtime::user_interface::Cache;
use netidx::publisher::Value;
use std::{sync::Arc, time::Instant};

use futures::try_join;
use winit::window::{Window, WindowAttributes, WindowId};

/// What an update changed of a window's own properties.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Changed {
    Title,
    /// A size the window does not show yet.
    Size(WindowSizeV),
    Theme,
    Icon,
}

/// Resolved window state — all refs compiled but no OS window yet.
pub struct ResolvedWindow<X: GXExt> {
    pub gx: GXHandle<X>,
    pub title: TRef<X, ArcStr>,
    pub size: TRef<X, WindowSizeV>,
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
        let decoded_icon = window_icon(&icon);
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
            .map(|sz| (sz.size().width, sz.size().height))
            .unwrap_or((800.0, 600.0));
        WindowAttributes::default()
            .with_title(title)
            .with_inner_size(winit::dpi::LogicalSize::new(w, h))
            .with_window_icon(self.decoded_icon.clone())
    }

    /// Take an update of one of the window's own properties, and say what
    /// it changed: a size the window does not already show is one to
    /// request, and a new icon is decoded.
    pub fn update(&mut self, id: ExprId, v: &Value) -> Result<Option<Changed>> {
        if self.title.update_changed(id, v).context("window update title")?.is_some() {
            return Ok(Some(Changed::Title));
        }
        // XCR claude for claude: [bug/medium] These TRefs fire whenever their
        // reference's cell is written, not only when the value changes. A place
        // reference such as `#size: &mut app.size` refires on every write of `app`.
        // Each size refire calls request_inner_size with the program's size unless
        // last_set_size matches, and last_set_size is only set at ResizeSettled. So any
        // unrelated write of `app` before a user resize settles (RESIZE_END_DEBOUNCE)
        // snaps the window back to the old size, and the resize is lost. Each icon
        // refire also re-reads and re-decodes the image on the main thread and re-sends
        // it to the window system, and each theme refire rebuilds a custom theme. Fix:
        // keep the previous value and act only when it changed, as ImageW does with
        // ImageSourceV::same_as. Request the size only when it differs from the
        // previous `p.size.t` and from last_set_size, and decode the icon only when
        // `!old.same_as(new)`. probe:
        // design/review-2026-10-26/repro/gui-core-size-ref-refires.gx (shows the
        // refire; the window side was found by reading). (gui-core-01)
        // 2026-10-26 claude: ResolvedWindow::update now takes the window's property
        // updates through TRef::update_changed (graphix-rt), which skips a value equal
        // to the last one taken before casting it, so a refire neither decodes the
        // icon, rebuilds the theme, sets the title nor redraws; it returns
        // Changed::Size only for a size the window does not already show
        // (TRef::set_deref at ResizeSettled sets it), which replaces last_set_size.
        // Pin: graphix-package-gui
        // test::window_test::a_property_delivered_unchanged_changes_nothing (fails with
        // update in place of update_changed, and with the shown-size check removed).
        let shown = self.size.t;
        if let Some(sz) = self.size.update_changed(id, v).context("window update size")? {
            return Ok((shown != Some(*sz)).then_some(Changed::Size(*sz)));
        }
        if self.theme.update_changed(id, v).context("window update theme")?.is_some() {
            return Ok(Some(Changed::Theme));
        }
        if self.icon.update_changed(id, v).context("window update icon")?.is_some() {
            self.decoded_icon = window_icon(&self.icon);
            return Ok(Some(Changed::Icon));
        }
        Ok(None)
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
            props: self,
            window,
            surface,
            cache: Cache::default(),
            cursor: mouse::Cursor::Unavailable,
            ime_enabled: false,
            last_mouse_interaction: mouse::Interaction::default(),
            pending_events: Vec::new(),
            needs_redraw: true,
            redraw_at: None,
            pending_resize: None,
            resize_render_timer_armed: false,
            last_render: Instant::now(),
        }
    }
}

/// The icon `icon` names, or none when it names none or fails to decode.
fn window_icon<X: GXExt>(icon: &TRef<X, ImageSourceV>) -> Option<winit::window::Icon> {
    icon.t.as_ref().and_then(|s| match s.decode_icon() {
        Ok(i) => i,
        Err(e) => {
            log::warn!("failed to decode window icon: {e}");
            None
        }
    })
}

/// The pointer as iced sees it after `ev`: where a mouse or a finger
/// last was, and unavailable once the mouse has left the window.
pub fn track_cursor(cursor: &mut mouse::Cursor, ev: &iced_core::Event) {
    match ev {
        iced_core::Event::Mouse(mouse::Event::CursorMoved { position })
        | iced_core::Event::Touch(
            touch::Event::FingerPressed { position, .. }
            | touch::Event::FingerMoved { position, .. }
            | touch::Event::FingerLifted { position, .. }
            | touch::Event::FingerLost { position, .. },
        ) => *cursor = mouse::Cursor::Available(*position),
        iced_core::Event::Mouse(mouse::Event::CursorLeft) => {
            *cursor = mouse::Cursor::Unavailable
        }
        _ => (),
    }
}

/// Per-window state tracking.
pub struct TrackedWindow<X: GXExt> {
    pub window_ref: Ref<X>,
    /// What the window's value says it is, replaced whole when it changes.
    pub props: ResolvedWindow<X>,
    pub window: Arc<Window>,
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
    pub redraw_at: Option<Instant>,
    pub pending_resize: Option<(u32, u32, f64)>,
    /// True while a resize render timer is armed; at most one per drag.
    pub resize_render_timer_armed: bool,
    pub last_render: Instant,
}

impl<X: GXExt> TrackedWindow<X> {
    /// The runtime is idle: the widgets judge the sends they wait on.
    pub fn idle(&mut self) {
        self.needs_redraw |= self.props.content.w.idle();
    }

    pub fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<()> {
        if id == self.window_ref.id && self.window_ref.last.as_ref() != Some(v) {
            self.props = rt
                .block_on(ResolvedWindow::compile(self.props.gx.clone(), v.clone()))
                .context("window ref recompile")?;
            self.window_ref.last = Some(v.clone());
            let p = &self.props;
            if let Some(t) = p.title.t.as_ref() {
                self.window.set_title(t);
            }
            if let Some(sz) = p.size.t {
                self.request_size(sz);
            }
            self.window.set_window_icon(p.decoded_icon.clone());
            self.needs_redraw = true;
            return Ok(());
        }
        let mut changed = match self.props.update(id, v)? {
            None => false,
            Some(Changed::Title) => {
                if let Some(t) = self.props.title.t.as_ref() {
                    self.window.set_title(t);
                }
                true
            }
            Some(Changed::Size(sz)) => {
                self.request_size(sz);
                true
            }
            Some(Changed::Theme) => true,
            Some(Changed::Icon) => {
                self.window.set_window_icon(self.props.decoded_icon.clone());
                true
            }
        };
        let p = &mut self.props;
        changed |= p.content.update(rt, &p.gx, id, v).context("window content")?;
        self.needs_redraw |= changed;
        Ok(())
    }

    fn request_size(&self, sz: WindowSizeV) {
        let _ = self.window.request_inner_size(winit::dpi::LogicalSize::new(
            sz.size().width,
            sz.size().height,
        ));
    }

    pub fn window_id(&self) -> WindowId {
        self.window.id()
    }

    pub fn iced_theme(&self) -> crate::theme::GraphixTheme {
        self.props.theme.t.as_ref().map(|t| t.0.clone()).unwrap_or(
            crate::theme::GraphixTheme { inner: iced_core::Theme::Dark, overrides: None },
        )
    }

    pub fn push_event(&mut self, event: iced_core::Event) {
        self.pending_events.push(event);
        // During a resize drag the render timer drives redraws.
        if !self.resize_render_timer_armed {
            self.needs_redraw = true;
        }
    }
}
