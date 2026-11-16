use super::{
    HighlightSpacingV, LineV, StyleV, TuiW, TuiWidget, into_borrowed_line,
    validate::Index,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{List, ListState},
};

graphix_rt::props! {
    struct Props {
        highlight_spacing: Option<HighlightSpacingV>,
        highlight_style: Option<StyleV>,
        highlight_symbol: Option<ArcStr>,
        items: Vec<LineV>,
        repeat_highlight_symbol: Option<bool>,
        scroll: Option<Index>,
        selected: Option<Index>,
        style: Option<StyleV>,
    }
}

pub(super) struct ListW<X: GXExt> {
    p: Props<X>,
    /// ratatui's offset when no scroll is set, which follows the selection
    state: ListState,
}

impl<X: GXExt> ListW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("list")?;
        Ok(TuiW::new(Self { p, state: ListState::default() }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ListW<X> {
    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("list")?;
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        // CR claude for claude: [perf/medium] Every frame builds a ratatui item, with
        // its own span Vec, for every element of `items`, though only rect.height of
        // them can show. A long list costs O(items) per redraw: with 2^18 lines a 1 ms
        // selection timer gets through about 370 ticks in 6 s against about 2900 with 2
        // lines. TableW::draw does the same with every row and cell. Window it as
        // ParagraphW does. Every item is one row, so pass `items[s..e]` with `s =
        // min(offset, selected)` and `e = max(offset, selected) + rect.height`, shift
        // the selection and offset by `s`, and add `s` back to the state's offset after
        // the render. For TableW, bound the end by summing the RowV heights and
        // margins. probe:
        // design/review-2026-11-16/repro/tui-widgets-list-full-rebuild.sh
        // (tui-widgets-03)
        let mut list =
            List::new(p.items.t.iter().flatten().map(|l| into_borrowed_line(&l.0)));
        if let Some(Some(hs)) = &p.highlight_spacing.t {
            list = list.highlight_spacing(hs.0.clone());
        }
        if let Some(Some(s)) = &p.highlight_style.t {
            list = list.highlight_style(s.0);
        }
        // CR claude for claude: [bug/low] The highlight symbol reaches ratatui uncut.
        // ratatui's List rendering places items at `row_area.x +
        // highlight_symbol.width() as u16`, so a 65535-column symbol in a list inside a
        // bordered block panics under overflow checks; in release it wraps and the
        // items vanish. This is the u16-width class the UPSTREAM.md entry "Chart and
        // BarChart add text widths in u16" covers for charts and bars, at a site that
        // fix did not reach. Cut it to the rect first, `sym.unicode_truncate(rect.width
        // as usize).0`, and name the list in that UPSTREAM.md entry. probe:
        // design/review-2026-11-16/repro/tui-widgets-list-highlight-overflow.sh
        // (tui-widgets-06)
        if let Some(Some(sym)) = &p.highlight_symbol.t {
            list = list.highlight_symbol(sym.as_str());
        }
        if let Some(Some(r)) = p.repeat_highlight_symbol.t {
            list = list.repeat_highlight_symbol(r);
        }
        if let Some(Some(s)) = &p.style.t {
            list = list.style(s.0);
        }
        // a render rewrites the state (an empty list deselects, a selection
        // past the end clamps), so the program's values go in every frame
        self.state.select(p.selected.t.flatten().map(|i| i.0));
        if let Some(Some(s)) = p.scroll.t {
            *self.state.offset_mut() = s.0;
        }
        frame.render_stateful_widget(list, rect, &mut self.state);
        Ok(())
    }
}
