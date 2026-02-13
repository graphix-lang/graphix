use super::{into_borrowed_line, LineV, StyleV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use netidx::publisher::Value;
use ratatui::{layout::Rect, widgets::LineGauge, Frame};
use tokio::try_join;

pub(super) struct LineGaugeW<X: GXExt> {
    filled_style: TRef<X, Option<StyleV>>,
    filled_symbol: TRef<X, Option<ArcStr>>,
    label: TRef<X, Option<LineV>>,
    ratio: TRef<X, f64>,
    style: TRef<X, Option<StyleV>>,
    unfilled_style: TRef<X, Option<StyleV>>,
    unfilled_symbol: TRef<X, Option<ArcStr>>,
}

impl<X: GXExt> LineGaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, filled_style), (_, filled_symbol), (_, label), (_, ratio), (_, style), (_, unfilled_style), (_, unfilled_symbol)] =
            v.cast_to::<[(ArcStr, u64); 7]>()?;
        let (
            filled_style,
            filled_symbol,
            label,
            ratio,
            style,
            unfilled_style,
            unfilled_symbol,
        ) = try_join! {
            gx.compile_ref(filled_style),
            gx.compile_ref(filled_symbol),
            gx.compile_ref(label),
            gx.compile_ref(ratio),
            gx.compile_ref(style),
            gx.compile_ref(unfilled_style),
            gx.compile_ref(unfilled_symbol)
        }?;
        Ok(Box::new(Self {
            filled_style: TRef::new(filled_style)
                .context("line_gauge tref filled_style")?,
            filled_symbol: TRef::new(filled_symbol)
                .context("line_gauge tref filled_symbol")?,
            label: TRef::new(label).context("line_gauge tref label")?,
            ratio: TRef::new(ratio).context("line_gauge tref ratio")?,
            style: TRef::new(style).context("line_gauge tref style")?,
            unfilled_style: TRef::new(unfilled_style)
                .context("line_gauge tref unfilled_style")?,
            unfilled_symbol: TRef::new(unfilled_symbol)
                .context("line_gauge tref unfilled_symbol")?,
        }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for LineGaugeW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            filled_style,
            filled_symbol,
            label,
            ratio,
            style,
            unfilled_style,
            unfilled_symbol,
        } = self;
        filled_style.update(id, &v).context("line_gauge update filled_style")?;
        filled_symbol.update(id, &v).context("line_gauge update filled_symbol")?;
        label.update(id, &v).context("line_gauge update label")?;
        ratio.update(id, &v).context("line_gauge update ratio")?;
        style.update(id, &v).context("line_gauge update style")?;
        unfilled_style.update(id, &v).context("line_gauge update unfilled_style")?;
        unfilled_symbol.update(id, &v).context("line_gauge update unfilled_symbol")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self {
            filled_style,
            filled_symbol,
            label,
            ratio,
            style,
            unfilled_style,
            unfilled_symbol,
        } = self;
        let mut g = LineGauge::default().ratio(ratio.t.unwrap_or(0.0));
        if let Some(Some(LineV(l))) = &label.t {
            g = g.label(into_borrowed_line(l));
        }
        if let Some(Some(s)) = &filled_symbol.t {
            g = g.filled_symbol(&*s);
        }
        if let Some(Some(s)) = &style.t {
            g = g.style(s.0);
        }
        if let Some(Some(s)) = &filled_style.t {
            g = g.filled_style(s.0);
        }
        if let Some(Some(s)) = &unfilled_style.t {
            g = g.unfilled_style(s.0);
        }
        if let Some(Some(s)) = &unfilled_symbol.t {
            g = g.unfilled_symbol(s);
        }
        frame.render_widget(g, rect);
        Ok(())
    }
}
