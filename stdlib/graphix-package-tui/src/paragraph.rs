use super::{AlignmentV, LinesV, ScrollV, StyleV, TuiW, TuiWidget, into_borrowed_lines};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Paragraph, Wrap},
};

graphix_rt::props! {
    struct Props {
        alignment: Option<AlignmentV>,
        lines: LinesV,
        scroll: ScrollV,
        style: StyleV,
        trim: bool,
        wrap: bool,
    }
}

pub(super) struct ParagraphW<X: GXExt>(Props<X>);

impl<X: GXExt> ParagraphW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, source: Value) -> Result<TuiW> {
        Ok(Box::new(Self(Props::compile(&gx, &source).await.context("paragraph")?)))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ParagraphW<X> {
    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.0;
        let lines = p.lines.t.as_ref().map(|l| &l.0[..]).unwrap_or(&[]);
        let mut para = Paragraph::new(into_borrowed_lines(lines));
        if let Some(Some(a)) = p.alignment.t {
            para = para.alignment(a.0);
        }
        if let Some(s) = p.style.t {
            para = para.style(s.0);
        }
        if p.wrap.t.unwrap_or(true) {
            para = para.wrap(Wrap { trim: p.trim.t.unwrap_or(true) });
        }
        if let Some(s) = p.scroll.t {
            para = para.scroll((s.y.0, s.x.0))
        }
        frame.render_widget(para, rect);
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.0.update(id, &v).context("paragraph")?;
        Ok(())
    }
}
