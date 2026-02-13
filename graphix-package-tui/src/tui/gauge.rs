use super::{SpanV, StyleV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use netidx::publisher::Value;
use ratatui::{layout::Rect, widgets::Gauge, Frame};
use tokio::try_join;

pub(super) struct GaugeW<X: GXExt> {
    gauge_style: TRef<X, Option<StyleV>>,
    label: TRef<X, Option<SpanV>>,
    ratio: TRef<X, f64>,
    style: TRef<X, Option<StyleV>>,
    use_unicode: TRef<X, Option<bool>>,
}

impl<X: GXExt> GaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, gauge_style), (_, label), (_, ratio), (_, style), (_, use_unicode)] =
            v.cast_to::<[(ArcStr, u64); 5]>()?;
        let (gauge_style, label, ratio, style, use_unicode) = try_join! {
            gx.compile_ref(gauge_style),
            gx.compile_ref(label),
            gx.compile_ref(ratio),
            gx.compile_ref(style),
            gx.compile_ref(use_unicode)
        }?;
        Ok(Box::new(Self {
            gauge_style: TRef::new(gauge_style).context("gage tref gauge_style")?,
            label: TRef::new(label).context("gage tref label")?,
            ratio: TRef::new(ratio).context("gage tref ratio")?,
            style: TRef::new(style).context("gage tref style")?,
            use_unicode: TRef::new(use_unicode).context("gage tref use_unicode")?,
        }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for GaugeW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.gauge_style.update(id, &v).context("gage update gauge_style")?;
        self.label.update(id, &v).context("gage update label")?;
        self.ratio.update(id, &v).context("gage update ratio")?;
        self.style.update(id, &v).context("gage update style")?;
        self.use_unicode.update(id, &v).context("gage update use_unicode")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let mut g = Gauge::default().ratio(self.ratio.t.unwrap_or(0.0));
        if let Some(Some(s)) = &self.label.t {
            g = g.label(s.0.clone());
        }
        if let Some(Some(s)) = &self.style.t {
            g = g.style(s.0);
        }
        if let Some(Some(s)) = &self.gauge_style.t {
            g = g.gauge_style(s.0);
        }
        if let Some(Some(u)) = self.use_unicode.t {
            g = g.use_unicode(u);
        }
        frame.render_widget(g, rect);
        Ok(())
    }
}
