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
        Ok(TuiW::new(Self(Props::compile(&gx, &source).await.context("paragraph")?)))
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
        // CR claude for claude: [bug/medium] scroll.y is a validate::Offset clamped to
        // u16::MAX for ratatui, so a text of more than 65536 lines cannot show its
        // tail: y = 69995 shows line 65535, and the clamp only logs. The pin
        // a_paragraph_scrolls_past_a_thousand_lines states the rule 'an offset counts
        // content lines, past any screen size'. Read y as validate::Index. Without
        // wrap, slice the lines here (`lines.get(y..)`, at most rect.height of them)
        // and pass scroll (0, x): exact, and it stops converting every line on every
        // frame (70000 static lines cost about 2.5 ms per unrelated redraw). With wrap,
        // pass only the first y + rect.height lines. probe:
        // design/review-2026-10-26/repro/tui-widgets-paragraph-scroll-cap.sh
        // (tui-widgets-04)
        if let Some(s) = p.scroll.t {
            para = para.scroll((s.y.0, s.x.0))
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
