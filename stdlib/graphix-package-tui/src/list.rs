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
        Ok(Box::new(Self { p, state: ListState::default() }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ListW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("list")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut list =
            List::new(p.items.t.iter().flatten().map(|l| into_borrowed_line(&l.0)));
        if let Some(Some(hs)) = &p.highlight_spacing.t {
            list = list.highlight_spacing(hs.0.clone());
        }
        if let Some(Some(s)) = &p.highlight_style.t {
            list = list.highlight_style(s.0);
        }
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
