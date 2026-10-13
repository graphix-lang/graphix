use super::{
    LineV, SizeReport, SpanV, StyleV, TuiW, TuiWidget, compile, compile_each,
    into_borrowed_line, validate::Index,
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use ratatui::{Frame, layout::Rect, widgets::Tabs};

graphix_rt::props! {
    struct Props {
        divider: Option<SpanV>,
        highlight_style: Option<StyleV>,
        padding_left: Option<LineV>,
        padding_right: Option<LineV>,
        selected: Option<Index>,
        style: Option<StyleV>,
    }
}

pub(super) struct TabsW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    tabs: Vec<(LineV, TuiW)>,
    tabs_ref: Ref<X>,
    size: SizeReport<X>,
}

impl<X: GXExt> TabsW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("tabs")?;
        let size = SizeReport::compile(&gx, &v).await?;
        let tabs_ref = gx.compile_field(&v, "tabs").await?;
        let mut t = Self { gx, p, tabs: vec![], tabs_ref, size };
        if let Some(v) = t.tabs_ref.last.take() {
            t.set_tabs(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_tabs(&mut self, v: Value) -> Result<()> {
        // CR claude for claude: [bug/medium] set_tabs compiles every child widget again
        // each time the tabs array fires. A tab title is a plain Line inside that
        // array, so a badge count in a title rebuilds every tab's widget tree. The
        // rebuild loses view state: a ListW gets a fresh ListState and its scroll
        // offset jumps, and a TableW loses its TableState. Every prop is also compiled
        // again through compile_ref round trips. Probe:
        // design/review-2026-10-12/repro/tui-widgets-01.gx. Fix: keep each child's spec
        // Value next to its TuiW and reuse the old widget when the new spec at that
        // position is equal, so that only the titles change. ChildW::update and the
        // compile_each users (layout children, overlay layers) have the same
        // rebuild-on-refire shape and need the same reuse. (tui-widgets-01)
        self.tabs = compile_each(v, |v| {
            let gx = self.gx.clone();
            async move {
                let (l, v) = v.cast_to::<(LineV, Value)>()?;
                Ok((l, compile(gx, v).await?))
            }
        })
        .await?;
        Ok(())
    }

    /// The tab shown: `selected`, at most the last tab.
    fn selected(&self) -> usize {
        let s = self.p.selected.t.flatten().map_or(0, |i| i.0);
        s.min(self.tabs.len().saturating_sub(1))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for TabsW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        let idx = self.selected();
        if let Some((_, child)) = self.tabs.get_mut(idx) {
            child.handle_event(v).await?;
        }
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("tabs")?;
        if self.tabs_ref.id == id {
            self.set_tabs(v.clone()).await?;
        }
        for (_, c) in &mut self.tabs {
            c.handle_update(id, v.clone()).await?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let idx = self.selected();
        let p = &self.p;
        let titles = self.tabs.iter().map(|(l, _)| into_borrowed_line(&l.0));
        let mut t = Tabs::new(titles).select(idx);
        if let Some(Some(s)) = &p.style.t {
            t = t.style(s.0);
        }
        if let Some(Some(s)) = &p.highlight_style.t {
            t = t.highlight_style(s.0);
        }
        if let Some(Some(s)) = &p.divider.t {
            t = t.divider(s.0.clone());
        }
        if let Some(Some(l)) = &p.padding_left.t {
            t = t.padding_left(into_borrowed_line(&l.0));
        }
        if let Some(Some(r)) = &p.padding_right.t {
            t = t.padding_right(into_borrowed_line(&r.0));
        }
        let mut bar_rect = rect;
        bar_rect.height = bar_rect.height.min(1);
        frame.render_widget(t, bar_rect);
        let mut child_rect = rect;
        if child_rect.height > 0 {
            child_rect.y = child_rect.y.saturating_add(1);
            child_rect.height -= 1;
        }
        self.size.report(child_rect)?;
        if let Some((_, child)) = self.tabs.get_mut(idx) {
            child.draw(frame, child_rect)?;
        }
        Ok(())
    }
}
