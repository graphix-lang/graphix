use super::{AlignmentV, LinesV, StyleV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{Frame, layout::Rect, style::Style, text::Text};
use std::mem;

graphix_rt::props! {
    struct Props {
        alignment: Option<AlignmentV>,
        lines: LinesV,
        style: StyleV,
    }
}

pub(super) struct TextW<X: GXExt> {
    p: Props<X>,
    text: Text<'static>,
}

impl<X: GXExt> TextW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, source: Value) -> Result<TuiW> {
        let mut p = Props::compile(&gx, &source).await.context("text")?;
        let text = Text {
            alignment: p.alignment.t.and_then(|a| a.map(|a| a.0)),
            style: p.style.t.map_or(Style::new(), |s| s.0),
            lines: p.lines.t.take().map(|l| l.0).unwrap_or_default(),
        };
        Ok(Box::new(Self { p, text }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for TextW<X> {
    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        frame.render_widget(&self.text, rect);
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self { p, text } = self;
        if let Some(a) = p.alignment.update(id, &v).context("text alignment")? {
            text.alignment = a.map(|a| a.0);
        }
        if let Some(l) = p.lines.update(id, &v).context("text lines")? {
            text.lines = mem::take(&mut l.0);
        }
        if let Some(s) = p.style.update(id, &v).context("text style")? {
            text.style = s.0;
        }
        Ok(())
    }
}
