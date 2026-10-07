use super::{ChildW, SizeReport, StyleV, TuiW, TuiWidget, validate::Index};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::{FromValue, Value};
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Scrollbar, ScrollbarOrientation, ScrollbarState},
};

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

graphix_rt::props! {
    struct Props {
        begin_style: Option<StyleV>,
        begin_symbol: Option<ArcStr>,
        content_length: Option<Index>,
        end_style: Option<StyleV>,
        end_symbol: Option<ArcStr>,
        orientation: Option<ScrollbarOrientationV>,
        position: Option<Index>,
        style: Option<StyleV>,
        thumb_style: Option<StyleV>,
        thumb_symbol: Option<ArcStr>,
        track_style: Option<StyleV>,
        track_symbol: Option<ArcStr>,
        viewport_length: Option<Index>,
    }
}

pub(super) struct ScrollbarW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    child: ChildW<X>,
    size: SizeReport<X>,
}

impl<X: GXExt> ScrollbarW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("scrollbar")?;
        let child = ChildW::compile(&gx, &v, "child").await.context("scrollbar")?;
        let size = SizeReport::compile(&gx, &v).await?;
        Ok(Box::new(Self { gx, p, child, size }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ScrollbarW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        self.child.w.handle_event(v).await
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("scrollbar")?;
        self.child.update(&self.gx, id, v).await
    }

    fn draw(&mut self, frame: &mut Frame, mut rect: Rect) -> Result<()> {
        let p = &self.p;
        let orientation = p
            .orientation
            .t
            .as_ref()
            .and_then(|t| t.as_ref().map(|t| t.0.clone()))
            .unwrap_or(ScrollbarOrientation::VerticalRight);
        let mut bar = Scrollbar::new(orientation.clone());
        if let Some(Some(s)) = p.begin_style.t {
            bar = bar.begin_style(s.0);
        }
        if let Some(s) = &p.begin_symbol.t {
            bar = bar.begin_symbol(s.as_deref());
        }
        if let Some(Some(s)) = p.end_style.t {
            bar = bar.end_style(s.0);
        }
        if let Some(s) = &p.end_symbol.t {
            bar = bar.end_symbol(s.as_deref());
        }
        if let Some(Some(s)) = p.style.t {
            bar = bar.style(s.0);
        }
        if let Some(Some(s)) = p.thumb_style.t {
            bar = bar.thumb_style(s.0);
        }
        if let Some(Some(s)) = &p.thumb_symbol.t {
            bar = bar.thumb_symbol(s);
        }
        if let Some(Some(s)) = p.track_style.t {
            bar = bar.track_style(s.0);
        }
        if let Some(s) = &p.track_symbol.t {
            bar = bar.track_symbol(s.as_deref());
        }
        let n = |r: &crate::TRef<X, Option<Index>>| r.t.flatten().map(|i| i.0);
        let mut state = ScrollbarState::new(n(&p.content_length).unwrap_or(0))
            .position(n(&p.position).unwrap_or(0));
        if let Some(l) = n(&p.viewport_length) {
            state = state.viewport_content_length(l);
        }
        frame.render_stateful_widget(bar, rect, &mut state);
        match orientation {
            ScrollbarOrientation::HorizontalBottom => {
                rect.height = rect.height.saturating_sub(1);
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
                rect.width = rect.width.saturating_sub(1);
            }
        };
        self.size.report(rect)?;
        self.child.w.draw(frame, rect)
    }
}
