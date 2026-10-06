use super::{HighlightSpacingV, LineV, StyleV, TuiW, TuiWidget, into_borrowed_line};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{List, ListState},
};
use tokio::try_join;

pub(super) struct ListW<X: GXExt> {
    highlight_spacing: TRef<X, Option<HighlightSpacingV>>,
    highlight_style: TRef<X, Option<StyleV>>,
    highlight_symbol: TRef<X, Option<ArcStr>>,
    items: TRef<X, Vec<LineV>>,
    repeat_highlight_symbol: TRef<X, Option<bool>>,
    scroll: TRef<X, Option<u32>>,
    selected: TRef<X, Option<u32>>,
    style: TRef<X, Option<StyleV>>,
    state: ListState,
}

impl<X: GXExt> ListW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            highlight_spacing: u64,
            highlight_style: u64,
            highlight_symbol: u64,
            items: u64,
            repeat_highlight_symbol: u64,
            scroll: u64,
            selected: u64,
            style: u64,
        }
        let Fields {
            highlight_spacing,
            highlight_style,
            highlight_symbol,
            items,
            repeat_highlight_symbol,
            scroll,
            selected,
            style,
        } = v.cast_to().context("list fields")?;
        let (
            highlight_spacing,
            highlight_style,
            highlight_symbol,
            items,
            repeat_highlight_symbol,
            scroll,
            selected,
            style,
        ) = try_join! {
            gx.compile_ref(highlight_spacing),
            gx.compile_ref(highlight_style),
            gx.compile_ref(highlight_symbol),
            gx.compile_ref(items),
            gx.compile_ref(repeat_highlight_symbol),
            gx.compile_ref(scroll),
            gx.compile_ref(selected),
            gx.compile_ref(style)
        }?;
        let mut t = Self {
            highlight_spacing: TRef::new(highlight_spacing)
                .context("list tref highlight_spacing")?,
            highlight_style: TRef::new(highlight_style)
                .context("list tref highlight_style")?,
            highlight_symbol: TRef::new(highlight_symbol)
                .context("list tref highlight_symbol")?,
            items: TRef::new(items).context("list tref items")?,
            repeat_highlight_symbol: TRef::new(repeat_highlight_symbol)
                .context("list tref repeat_highlight_symbol")?,
            scroll: TRef::new(scroll).context("list tref scroll")?,
            selected: TRef::new(selected).context("list tref selected")?,
            style: TRef::new(style).context("list tref style")?,
            state: ListState::default(),
        };
        if let Some(Some(s)) = t.scroll.t {
            t.state = t.state.with_offset(s as usize);
        }
        if let Some(s) = t.selected.t {
            t.state = t.state.with_selected(s.map(|s| s as usize));
        }
        Ok(Box::new(t))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ListW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            highlight_spacing,
            highlight_style,
            highlight_symbol,
            items,
            repeat_highlight_symbol,
            scroll,
            selected,
            style,
            state,
        } = self;
        highlight_spacing.update(id, &v).context("list update highlight_spacing")?;
        highlight_style.update(id, &v).context("list update highlight_style")?;
        highlight_symbol.update(id, &v).context("list update highlight_symbol")?;
        items.update(id, &v).context("list update items")?;
        repeat_highlight_symbol
            .update(id, &v)
            .context("list update repeat_highlight_symbol")?;
        // CR claude for eric: [bug] `selected` and `scroll` reach the ListState only
        // here and at compile, but ratatui's List render rewrites that state on every
        // draw. An empty list sets the selection to None and the offset to 0, and a
        // selection past the end is clamped to the last item. Nothing restores them
        // when the items come back, because the refs did not change. A list whose items
        // load late shows no highlight at all. A list that shrinks and regrows keeps
        // the highlight on the clamped row while `selected` still names the old one,
        // and a requested scroll is lost the same way. Re-apply both from the refs
        // before every render, as TableW does for its selection; probe:
        // design/review-2026-10-05/repro/tui-widgets-08.py (tui-widgets-08)
        if let Some(Some(s)) = scroll.update(id, &v).context("list update scroll")? {
            *state = state.clone().with_offset(*s as usize);
        }
        if let Some(s) = selected.update(id, &v).context("list update selected")? {
            *state = state.clone().with_selected(s.map(|s| s as usize));
        }
        style.update(id, &v).context("list update style")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self {
            highlight_spacing,
            highlight_style,
            highlight_symbol,
            items,
            repeat_highlight_symbol,
            scroll: _,
            selected: _,
            style,
            state,
        } = self;
        let mut list =
            List::new(items.t.iter().flat_map(|l| l).map(|l| into_borrowed_line(&l.0)));
        if let Some(Some(hs)) = &highlight_spacing.t {
            list = list.highlight_spacing(hs.0.clone());
        }
        if let Some(Some(s)) = &highlight_style.t {
            list = list.highlight_style(s.0);
        }
        if let Some(Some(sym)) = &highlight_symbol.t {
            list = list.highlight_symbol(sym.as_str());
        }
        if let Some(Some(r)) = repeat_highlight_symbol.t {
            list = list.repeat_highlight_symbol(r);
        }
        if let Some(Some(s)) = &style.t {
            list = list.style(s.0);
        }
        // CR claude for eric: [bug] `state` keeps whatever ratatui's previous render
        // wrote into it. An empty list sets the selection to None and the offset to 0,
        // a selection past the end is clamped to the last item, and both persist,
        // because `selected` and `scroll` are written into the state only when their
        // refs update (lines 86-91, 121-126). So after one frame drawn with fewer
        // items, the highlight stops following the program's `selected` until that ref
        // fires again. Items that arrive after the first frame never show the
        // selection, and a list that shrinks and grows back highlights a row other than
        // the selected one. In netidx-admin's services panel (services.gx:402): delete
        // the last unit, create another, and the highlight stays on the second row
        // while the detail pane and the s/R/e/d keys act on the third. Before each
        // render, re-apply `selected`, and `scroll` when it is set, as table.rs does.
        // probe: design/review-2026-10-05/repro/tui-widgets.r2-01.gx
        // (tui-widgets.r2-01)
        frame.render_stateful_widget(list, rect, state);
        Ok(())
    }
}
