use super::{
    into_borrowed_line, HighlightSpacingV, LineV, StyleV, TRef, TuiW, TuiWidget,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use ratatui::{
    layout::Rect,
    widgets::{List, ListState},
    Frame,
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
        let [(_, highlight_spacing), (_, highlight_style), (_, highlight_symbol), (_, items), (_, repeat_highlight_symbol), (_, scroll), (_, selected), (_, style)] =
            v.cast_to::<[(ArcStr, u64); 8]>().context("list fields")?;
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
        frame.render_stateful_widget(list, rect, state);
        Ok(())
    }
}
