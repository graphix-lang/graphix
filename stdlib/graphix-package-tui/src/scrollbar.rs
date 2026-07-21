use super::{EmptyW, SizeV, StyleV, TRef, TuiW, TuiWidget, compile, validate};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Scrollbar, ScrollbarOrientation, ScrollbarState},
};
use tokio::try_join;

#[derive(Clone)]
struct ScrollbarOrientationV(ScrollbarOrientation);

impl FromValue for ScrollbarOrientationV {
    fn from_value(v: Value) -> Result<Self> {
        let v = match &*v.cast_to::<ArcStr>()? {
            "VerticalRight" => ScrollbarOrientation::VerticalRight,
            "VerticalLeft" => ScrollbarOrientation::VerticalLeft,
            "HorizontalBottom" => ScrollbarOrientation::HorizontalBottom,
            "HorizontalTop" => ScrollbarOrientation::HorizontalTop,
            s => bail!("invalid ScrollBarOrientation {s}"),
        };
        Ok(Self(v))
    }
}

pub(super) struct ScrollbarW<X: GXExt> {
    gx: GXHandle<X>,
    begin_style: TRef<X, Option<StyleV>>,
    begin_symbol: TRef<X, Option<ArcStr>>,
    child: TuiW,
    child_ref: Ref<X>,
    content_length: TRef<X, Option<i64>>,
    viewport_length: TRef<X, Option<i64>>,
    end_style: TRef<X, Option<StyleV>>,
    end_symbol: TRef<X, Option<ArcStr>>,
    orientation: TRef<X, Option<ScrollbarOrientationV>>,
    position: TRef<X, Option<i64>>,
    style: TRef<X, Option<StyleV>>,
    thumb_style: TRef<X, Option<StyleV>>,
    thumb_symbol: TRef<X, Option<ArcStr>>,
    track_style: TRef<X, Option<StyleV>>,
    track_symbol: TRef<X, Option<ArcStr>>,
    size_ref: Ref<X>,
    last_size: SizeV,
    state: ScrollbarState,
    last_warned_position: Option<i64>,
    last_warned_content_length: Option<i64>,
    last_warned_viewport_length: Option<i64>,
}

impl<X: GXExt> ScrollbarW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [
            (_, begin_style),
            (_, begin_symbol),
            (_, child),
            (_, content_length),
            (_, end_style),
            (_, end_symbol),
            (_, orientation),
            (_, position),
            (_, size),
            (_, style),
            (_, thumb_style),
            (_, thumb_symbol),
            (_, track_style),
            (_, track_symbol),
            (_, viewport_length),
        ] = v.cast_to::<[(ArcStr, u64); 15]>().context("scrollbar flds")?;
        let (
            begin_style,
            begin_symbol,
            mut child_ref,
            content_length,
            end_style,
            end_symbol,
            orientation,
            position,
            size_ref,
            style,
            thumb_style,
            thumb_symbol,
            track_style,
            track_symbol,
            viewport_length,
        ) = try_join! {
            gx.compile_ref(begin_style),
            gx.compile_ref(begin_symbol),
            gx.compile_ref(child),
            gx.compile_ref(content_length),
            gx.compile_ref(end_style),
            gx.compile_ref(end_symbol),
            gx.compile_ref(orientation),
            gx.compile_ref(position),
            gx.compile_ref(size),
            gx.compile_ref(style),
            gx.compile_ref(thumb_style),
            gx.compile_ref(thumb_symbol),
            gx.compile_ref(track_style),
            gx.compile_ref(track_symbol),
            gx.compile_ref(viewport_length)
        }?;
        let begin_style = TRef::<X, Option<StyleV>>::new(begin_style)
            .context("scrollbar tref begin_style")?;
        let begin_symbol = TRef::<X, Option<ArcStr>>::new(begin_symbol)
            .context("scrollbar tref begin_symbol")?;
        let child = match child_ref.last.take() {
            Some(v) => compile(gx.clone(), v).await?,
            None => Box::new(EmptyW),
        };
        let content_length = TRef::<X, Option<i64>>::new(content_length)
            .context("scrollbar tref content_length")?;
        let end_style = TRef::<X, Option<StyleV>>::new(end_style)
            .context("scrollbar tref end_style")?;
        let end_symbol = TRef::<X, Option<ArcStr>>::new(end_symbol)
            .context("scrollbar tref end_symbol")?;
        let orientation = TRef::<X, Option<ScrollbarOrientationV>>::new(orientation)
            .context("scrollbar tref orientation")?;
        let position =
            TRef::<X, Option<i64>>::new(position).context("scrollbar tref position")?;
        let style =
            TRef::<X, Option<StyleV>>::new(style).context("scrollbar tref style")?;
        let thumb_style = TRef::<X, Option<StyleV>>::new(thumb_style)
            .context("scrollbar tref thumb_style")?;
        let thumb_symbol = TRef::<X, Option<ArcStr>>::new(thumb_symbol)
            .context("scrollbar tref thumb_symbol")?;
        let track_style = TRef::<X, Option<StyleV>>::new(track_style)
            .context("scrollbar tref track_style")?;
        let track_symbol = TRef::<X, Option<ArcStr>>::new(track_symbol)
            .context("scrollbar tref track_symbol")?;
        let viewport_length = TRef::<X, Option<i64>>::new(viewport_length)
            .context("scrollbar tref viewport_length")?;
        // Initial state's content_length: clamp the user-provided value
        // (negative becomes 0). The draw loop re-applies this each frame
        // through the same helper, so the warn here would just be a
        // duplicate — suppress by using a throwaway dedup slot.
        let initial_len = content_length
            .t
            .and_then(|t| t)
            .map(|raw| {
                validate::clamp_usize("scrollbar", "content_length", &mut None, raw)
            })
            .unwrap_or(50);
        let state = ScrollbarState::new(initial_len);
        Ok(Box::new(Self {
            begin_style,
            begin_symbol,
            child_ref,
            child,
            size_ref,
            last_size: SizeV::default(),
            content_length,
            end_style,
            end_symbol,
            orientation,
            position,
            style,
            thumb_symbol,
            thumb_style,
            gx,
            track_style,
            track_symbol,
            viewport_length,
            state,
            last_warned_position: None,
            last_warned_content_length: None,
            last_warned_viewport_length: None,
        }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ScrollbarW<X> {
    async fn handle_event(&mut self, e: Event, v: Value) -> Result<()> {
        self.child.handle_event(e, v).await
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            gx,
            begin_style,
            begin_symbol,
            child,
            child_ref,
            size_ref: _,
            last_size: _,
            content_length,
            end_style,
            end_symbol,
            orientation,
            position,
            style,
            thumb_style,
            thumb_symbol,
            track_style,
            track_symbol,
            viewport_length,
            state: _,
            last_warned_position: _,
            last_warned_content_length: _,
            last_warned_viewport_length: _,
        } = self;
        begin_style.update(id, &v).context("scrollbar update begin_style")?;
        begin_symbol.update(id, &v).context("scrollbar update begin_symbol")?;
        if child_ref.id == id {
            *child = compile(gx.clone(), v.clone()).await?;
        }
        end_style.update(id, &v).context("scrollbar update end_style")?;
        end_symbol.update(id, &v).context("scrollbar update end_symbol")?;
        orientation.update(id, &v).context("scrollbar update orientation")?;
        position.update(id, &v).context("scrollbar update position")?;
        style.update(id, &v).context("scrollbar update style")?;
        thumb_style.update(id, &v).context("scrollbar update thumb_style")?;
        thumb_symbol.update(id, &v).context("scrollbar update thumb_symbol")?;
        track_style.update(id, &v).context("scrollbar update track_style")?;
        track_symbol.update(id, &v).context("scrollbar update track_symbol")?;
        content_length.update(id, &v).context("scrollbar update content_length")?;
        viewport_length.update(id, &v).context("scrollbar update viewport_length")?;
        child.handle_update(id, v).await
    }

    fn draw(&mut self, frame: &mut Frame, mut rect: Rect) -> Result<()> {
        let Self {
            gx: _,
            begin_style,
            begin_symbol,
            child,
            child_ref: _,
            size_ref,
            last_size,
            content_length,
            end_style,
            end_symbol,
            orientation,
            position,
            style,
            thumb_style,
            thumb_symbol,
            track_style,
            track_symbol,
            viewport_length,
            state,
            last_warned_position,
            last_warned_content_length,
            last_warned_viewport_length,
        } = self;
        let orientation = orientation
            .t
            .as_ref()
            .and_then(|t| t.as_ref().map(|t| t.0.clone()))
            .unwrap_or(ScrollbarOrientation::VerticalRight);
        let mut bar = Scrollbar::new(orientation.clone());
        if let Some(Some(s)) = begin_style.t {
            bar = bar.begin_style(s.0);
        }
        if let Some(s) = &begin_symbol.t {
            bar = bar.begin_symbol(s.as_ref().map(|s| s.as_str()));
        }
        if let Some(Some(s)) = end_style.t {
            bar = bar.end_style(s.0);
        }
        if let Some(s) = &end_symbol.t {
            bar = bar.end_symbol(s.as_ref().map(|s| s.as_str()));
        }
        if let Some(Some(p)) = position.t {
            let p =
                validate::clamp_usize("scrollbar", "position", last_warned_position, p);
            *state = state.position(p);
        }
        if let Some(Some(s)) = style.t {
            bar = bar.style(s.0);
        }
        if let Some(Some(s)) = thumb_style.t {
            bar = bar.thumb_style(s.0);
        }
        if let Some(Some(s)) = &thumb_symbol.t {
            bar = bar.thumb_symbol(s);
        }
        if let Some(Some(s)) = track_style.t {
            bar = bar.track_style(s.0);
        }
        if let Some(s) = &track_symbol.t {
            bar = bar.track_symbol(s.as_ref().map(|s| s.as_str()));
        }
        if let Some(Some(l)) = content_length.t.take() {
            let l = validate::clamp_usize(
                "scrollbar",
                "content_length",
                last_warned_content_length,
                l,
            );
            *state = state.content_length(l);
        }
        if let Some(Some(l)) = viewport_length.t.take() {
            let l = validate::clamp_usize(
                "scrollbar",
                "viewport_length",
                last_warned_viewport_length,
                l,
            );
            *state = state.viewport_content_length(l);
        }
        frame.render_stateful_widget(bar, rect, state);
        match orientation {
            ScrollbarOrientation::HorizontalBottom => {
                if rect.height > 0 {
                    rect.height -= 1
                }
            }
            ScrollbarOrientation::HorizontalTop => {
                if rect.height > 0 && rect.y < u16::MAX {
                    rect.height -= 1;
                    rect.y += 1
                }
            }
            ScrollbarOrientation::VerticalLeft => {
                if rect.width > 0 && rect.x < u16::MAX {
                    rect.width -= 1;
                    rect.x += 1
                }
            }
            ScrollbarOrientation::VerticalRight => {
                if rect.width > 0 {
                    rect.width -= 1
                }
            }
        };
        let size = SizeV::from(rect);
        if *last_size != size {
            *last_size = size;
            size_ref.set_deref(size)?
        }
        child.draw(frame, rect)
    }
}
