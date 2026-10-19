use super::{
    FlexV, HighlightSpacingV, LineV, StyleV, TRef, TuiW, TuiWidget, into_borrowed_line,
    layout::ConstraintV,
    reconcile,
    validate::{Dim, Index},
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::{BindId, expr::ExprId};
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Cell, Row, Table, TableState},
};

#[derive(Debug, Clone, Copy, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct SelectedV {
    x: Index,
    y: Index,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct CellV {
    content: LineV,
    style: Option<StyleV>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct RowV {
    cells: Vec<CellV>,
    height: Option<Dim>,
    style: Option<StyleV>,
    top_margin: Option<Dim>,
    bottom_margin: Option<Dim>,
}

impl RowV {
    fn build(&self) -> Row<'_> {
        let mut r = Row::new(self.cells.iter().map(|cell| {
            let c = Cell::new(into_borrowed_line(&cell.content.0));
            match &cell.style {
                Some(s) => c.style(s.0),
                None => c,
            }
        }));
        if let Some(v) = self.height {
            r = r.height(v.0);
        }
        if let Some(s) = self.style {
            r = r.style(s.0);
        }
        if let Some(v) = self.top_margin {
            r = r.top_margin(v.0);
        }
        if let Some(v) = self.bottom_margin {
            r = r.bottom_margin(v.0);
        }
        r
    }
}

graphix_rt::props! {
    struct Props {
        cell_highlight_style: Option<StyleV>,
        column_highlight_style: Option<StyleV>,
        column_spacing: Option<Dim>,
        flex: Option<FlexV>,
        footer: Option<RowV>,
        header: Option<RowV>,
        highlight_spacing: Option<HighlightSpacingV>,
        highlight_symbol: Option<ArcStr>,
        row_highlight_style: Option<StyleV>,
        style: Option<StyleV>,
        widths: Option<Vec<ConstraintV>>,
    }
}

graphix_rt::props! {
    struct Selection {
        selected: Option<Index>,
        selected_cell: Option<SelectedV>,
        selected_column: Option<Index>,
    }
}

pub(super) struct TableW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    sel: Selection<X>,
    /// The selected (row, column): what the selection ref that fired last
    /// says, null included. A render rewrites the state, so it is applied
    /// every frame.
    row: Option<usize>,
    column: Option<usize>,
    rows: Vec<(Value, TRef<X, RowV>)>,
    rows_ref: Ref<X>,
    state: TableState,
}

impl<X: GXExt> TableW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("table")?;
        let sel = Selection::compile(&gx, &v).await.context("table")?;
        let rows_ref = gx.compile_field(&v, "rows").await?;
        let mut t = Self {
            gx,
            p,
            sel,
            row: None,
            column: None,
            rows: vec![],
            rows_ref,
            state: TableState::default(),
        };
        let index = |r: &TRef<X, Option<Index>>| r.t.flatten().map(|i| i.0);
        t.column = index(&t.sel.selected_column);
        t.row = index(&t.sel.selected);
        if let Some(Some(c)) = t.sel.selected_cell.t {
            (t.row, t.column) = (Some(c.y.0), Some(c.x.0));
        }
        if let Some(v) = t.rows_ref.last.take() {
            t.set_rows(v).await?;
        }
        Ok(TuiW::new(t))
    }

    // XCR claude for claude: [perf/medium] This recompiles every element each time the
    // caller's list ref fires, and a ref fires on every production, whether or not the
    // value changed. `TableW::set_rows` uses it for `&Array<&Row>`: the browser's `rows =
    // array::map(cells, |row| &tui::table::row(..))` fires whenever any visible cell
    // changes. Each live update then recompiles every visible row ref: one runtime round
    // trip, one node created and one deleted per row. Chart datasets, bar-chart groups and
    // canvas shapes go through here too. `reconcile` below already keeps the elements whose
    // source value is unchanged. Move these callers to it (they would hold `Vec<(Value,
    // T)>`) and delete this helper. Found by reading; not run. (tui-core-04)
    // 2026-10-19 claude: compile_each is deleted. Table rows, canvas shapes, chart
    // datasets, bar-chart groups and each group's bars hold Vec<(Value, T)> and go through
    // graphix_rt::reconcile, so an element whose source is unchanged is kept. No pin counts
    // the round trips a refire costs; the widget tests (render_test, widgets_test,
    // panic_test) cover the draw paths.
    async fn set_rows(&mut self, v: Value) -> Result<()> {
        reconcile(&mut self.rows, v.cast_to()?, |id| {
            let gx = self.gx.clone();
            async move { TRef::new(gx.compile_ref(id.cast_to::<BindId>()?).await?) }
        })
        .await
        .context("rows")
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for TableW<X> {
    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("table")?;
        let sel = &mut self.sel;
        if let Some(r) = sel.selected.update(id, v).context("table selected")? {
            self.row = r.map(|i| i.0);
        }
        if let Some(c) = sel.selected_column.update(id, v).context("table column")? {
            self.column = c.map(|i| i.0);
        }
        if let Some(c) = sel.selected_cell.update(id, v).context("table cell")? {
            (self.row, self.column) = match c {
                Some(c) => (Some(c.y.0), Some(c.x.0)),
                None => (None, None),
            };
        }
        if self.rows_ref.id == id {
            return Ok(Some(Box::pin(self.set_rows(v.clone()))));
        }
        for (_, r) in self.rows.iter_mut() {
            r.update(id, v)?;
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut table = Table::default()
            .rows(self.rows.iter().filter_map(|(_, r)| r.t.as_ref().map(|r| r.build())));
        if let Some(Some(s)) = p.cell_highlight_style.t {
            table = table.cell_highlight_style(s.0);
        }
        if let Some(Some(s)) = p.column_highlight_style.t {
            table = table.column_highlight_style(s.0);
        }
        if let Some(Some(f)) = p.flex.t {
            table = table.flex(f.0);
        }
        if let Some(Some(widths)) = &p.widths.t {
            table = table.widths(widths.iter().map(|c| c.0));
        }
        if let Some(Some(s)) = p.column_spacing.t {
            table = table.column_spacing(s.0);
        }
        if let Some(Some(h)) = &p.header.t {
            table = table.header(h.build());
        }
        if let Some(Some(h)) = &p.footer.t {
            table = table.footer(h.build());
        }
        if let Some(Some(hs)) = &p.highlight_spacing.t {
            table = table.highlight_spacing(hs.0.clone());
        }
        if let Some(Some(s)) = &p.row_highlight_style.t {
            table = table.row_highlight_style(s.0);
        }
        if let Some(Some(sym)) = &p.highlight_symbol.t {
            table = table.highlight_symbol(sym.as_str());
        }
        if let Some(Some(s)) = &p.style.t {
            table = table.style(s.0);
        }
        self.state.select(self.row);
        self.state.select_column(self.column);
        frame.render_stateful_widget(table, rect, &mut self.state);
        Ok(())
    }
}
