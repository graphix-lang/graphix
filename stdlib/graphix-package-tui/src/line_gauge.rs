use super::{LineV, StyleV, TuiW, TuiWidget, into_borrowed_line, validate::Ratio};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{Frame, layout::Rect, widgets::LineGauge};

graphix_rt::props! {
    struct Props {
        filled_style: Option<StyleV>,
        filled_symbol: Option<ArcStr>,
        label: Option<LineV>,
        ratio: Ratio,
        style: Option<StyleV>,
        unfilled_style: Option<StyleV>,
        unfilled_symbol: Option<ArcStr>,
    }
}

pub(super) struct LineGaugeW<X: GXExt>(Props<X>);

impl<X: GXExt> LineGaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        Ok(Box::new(Self(Props::compile(&gx, &v).await.context("line_gauge")?)))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for LineGaugeW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.0.update(id, &v).context("line_gauge")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.0;
        let mut g = LineGauge::default().ratio(p.ratio.t.map_or(0.0, |r| r.0));
        if let Some(Some(LineV(l))) = &p.label.t {
            g = g.label(into_borrowed_line(l));
        }
        if let Some(Some(s)) = &p.filled_symbol.t {
            g = g.filled_symbol(s);
        }
        if let Some(Some(s)) = &p.style.t {
            g = g.style(s.0);
        }
        if let Some(Some(s)) = &p.filled_style.t {
            g = g.filled_style(s.0);
        }
        if let Some(Some(s)) = &p.unfilled_style.t {
            g = g.unfilled_style(s.0);
        }
        if let Some(Some(s)) = &p.unfilled_symbol.t {
            g = g.unfilled_symbol(s);
        }
        frame.render_widget(g, rect);
        Ok(())
    }
}
