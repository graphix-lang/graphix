use super::{into_borrowed_line, LineV, StyleV, TRef, TuiW, TuiWidget};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::{FromValue, Value};
use ratatui::{layout::Rect, symbols, widgets::LineGauge, Frame};
use tokio::try_join;

#[derive(Clone, Copy)]
struct LineSetV(symbols::line::Set);

impl FromValue for LineSetV {
    fn from_value(v: Value) -> Result<Self> {
        let s = match &*v.cast_to::<ArcStr>()? {
            "Normal" => symbols::line::NORMAL,
            "Rounded" => symbols::line::ROUNDED,
            "Double" => symbols::line::DOUBLE,
            "Thick" => symbols::line::THICK,
            s => bail!("invalid line set {s}"),
        };
        Ok(Self(s))
    }
}

pub(super) struct LineGaugeW<X: GXExt> {
    filled_style: TRef<X, Option<StyleV>>,
    label: TRef<X, Option<LineV>>,
    line_set: TRef<X, Option<LineSetV>>,
    ratio: TRef<X, f64>,
    style: TRef<X, Option<StyleV>>,
    unfilled_style: TRef<X, Option<StyleV>>,
}

impl<X: GXExt> LineGaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, filled_style), (_, label), (_, line_set), (_, ratio), (_, style), (_, unfilled_style)] =
            v.cast_to::<[(ArcStr, u64); 6]>()?;
        let (filled_style, label, line_set, ratio, style, unfilled_style) = try_join! {
            gx.compile_ref(filled_style),
            gx.compile_ref(label),
            gx.compile_ref(line_set),
            gx.compile_ref(ratio),
            gx.compile_ref(style),
            gx.compile_ref(unfilled_style)
        }?;
        Ok(Box::new(Self {
            filled_style: TRef::new(filled_style)
                .context("line_gauge tref filled_style")?,
            label: TRef::new(label).context("line_gauge tref label")?,
            line_set: TRef::new(line_set).context("line_gauge tref line_set")?,
            ratio: TRef::new(ratio).context("line_gauge tref ratio")?,
            style: TRef::new(style).context("line_gauge tref style")?,
            unfilled_style: TRef::new(unfilled_style)
                .context("line_gauge tref unfilled_style")?,
        }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for LineGaugeW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self { filled_style, label, line_set, ratio, style, unfilled_style } = self;
        filled_style.update(id, &v).context("line_gauge update filled_style")?;
        label.update(id, &v).context("line_gauge update label")?;
        line_set.update(id, &v).context("line_gauge update line_set")?;
        ratio.update(id, &v).context("line_gauge update ratio")?;
        style.update(id, &v).context("line_gauge update style")?;
        unfilled_style.update(id, &v).context("line_gauge update unfilled_style")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self { filled_style, label, line_set, ratio, style, unfilled_style } = self;
        let mut g = LineGauge::default().ratio(ratio.t.unwrap_or(0.0));
        if let Some(Some(LineV(l))) = &label.t {
            g = g.label(into_borrowed_line(l));
        }
        if let Some(Some(ls)) = line_set.t {
            g = g.line_set(ls.0);
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
        frame.render_widget(g, rect);
        Ok(())
    }
}
