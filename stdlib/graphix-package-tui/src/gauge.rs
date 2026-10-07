use super::{SpanV, StyleV, TuiW, TuiWidget, validate::Ratio};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{Frame, layout::Rect, widgets::Gauge};

graphix_rt::props! {
    struct Props {
        gauge_style: Option<StyleV>,
        label: Option<SpanV>,
        ratio: Ratio,
        style: Option<StyleV>,
        use_unicode: Option<bool>,
    }
}

pub(super) struct GaugeW<X: GXExt>(Props<X>);

impl<X: GXExt> GaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        Ok(Box::new(Self(Props::compile(&gx, &v).await.context("gauge")?)))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for GaugeW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.0.update(id, &v).context("gauge")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.0;
        let mut g = Gauge::default().ratio(p.ratio.t.map_or(0.0, |r| r.0));
        if let Some(Some(s)) = &p.label.t {
            g = g.label(s.0.clone());
        }
        if let Some(Some(s)) = &p.style.t {
            g = g.style(s.0);
        }
        if let Some(Some(s)) = &p.gauge_style.t {
            g = g.gauge_style(s.0);
        }
        if let Some(Some(u)) = p.use_unicode.t {
            g = g.use_unicode(u);
        }
        frame.render_widget(g, rect);
        Ok(())
    }
}
