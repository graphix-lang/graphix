use super::{
    AlignmentV, LinesV, ScrollV, StyleV, TuiW, TuiWidget, into_borrowed_line,
    into_borrowed_lines,
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{
    Frame,
    layout::Rect,
    text::Line,
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
        Ok(TuiW::new(Self(Props::compile(&gx, &source).await.context("paragraph")?)))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ParagraphW<X> {
    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.0;
        let mut lines = p.lines.t.as_ref().map(|l| &l.0[..]).unwrap_or(&[]);
        let wrap =
            p.wrap.t.unwrap_or(true).then(|| Wrap { trim: p.trim.t.unwrap_or(true) });
        let (x, mut y) = p.scroll.t.map(|s| (s.x.0, s.y.0)).unwrap_or_default();
        let height = rect.height as usize;
        match wrap {
            None => {
                lines = lines.get(y..).unwrap_or(&[]);
                y = 0;
            }
            Some(wrap) => {
                let width = rect.intersection(frame.area()).width;
                let rows = |l: &Line<'static>| {
                    Paragraph::new(into_borrowed_line(l)).wrap(wrap).line_count(width)
                };
                while y > u16::MAX as usize
                    && let Some((first, rest)) = lines.split_first()
                {
                    let r = rows(first);
                    if r > y {
                        break;
                    }
                    y -= r;
                    lines = rest;
                }
            }
        }
        // every line takes at least one row, so none past these shows
        lines = &lines[..lines.len().min(y.saturating_add(height))];
        let mut para = Paragraph::new(into_borrowed_lines(lines))
            .scroll((y.min(u16::MAX as usize) as u16, x));
        if let Some(wrap) = wrap {
            para = para.wrap(wrap);
        }
        if let Some(Some(a)) = p.alignment.t {
            para = para.alignment(a.0);
        }
        if let Some(s) = p.style.t {
            para = para.style(s.0);
        }
        frame.render_widget(para, rect);
        Ok(())
    }

    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.0.update(id, v).context("paragraph")?;
        Ok(None)
    }
}
