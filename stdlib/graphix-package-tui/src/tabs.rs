use super::{
    LineV, SizeReport, SpanV, StyleV, TuiW, TuiWidget, compile, into_borrowed_line,
    reconcile, update_children, validate::Index,
};
use anyhow::{Context, Result, bail};
use async_trait::async_trait;
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
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
    titles: Vec<LineV>,
    tabs: Vec<(Value, TuiW)>,
    tabs_ref: Ref<X>,
    size: SizeReport<X>,
}

impl<X: GXExt> TabsW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("tabs")?;
        let size = SizeReport::compile(&gx, &v).await?;
        let tabs_ref = gx.compile_field(&v, "tabs").await?;
        let mut t = Self { gx, p, titles: vec![], tabs: vec![], tabs_ref, size };
        if let Some(v) = t.tabs_ref.last.take() {
            t.set_tabs(v).await?;
        }
        Ok(TuiW::new(t))
    }

    async fn set_tabs(&mut self, v: Value) -> Result<()> {
        let mut sources: LPooled<Vec<Value>> = LPooled::take();
        self.titles.clear();
        for tab in v.cast_to::<ValArray>()?.iter() {
            let Some([title, source]) = rep::tuple_elems(tab) else {
                bail!("invalid tab {tab}")
            };
            self.titles.push(title.clone().cast_to()?);
            sources.push(source.clone());
        }
        reconcile(&mut self.tabs, sources, |v| compile(self.gx.clone(), v)).await
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

    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("tabs")?;
        if self.tabs_ref.id == id {
            return Ok(Some(Box::pin(self.set_tabs(v.clone()))));
        }
        update_children(self.tabs.iter_mut().map(|(_, c)| c), id, v)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let idx = self.selected();
        let p = &self.p;
        let titles = self.titles.iter().map(|l| into_borrowed_line(&l.0));
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
