use super::{
    FlexV, HighlightSpacingV, LineV, StyleV, TRef, TuiW, TuiWidget, compile_each,
    into_borrowed_line,
    layout::ConstraintV,
    validate::{Dim, Index},
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Cell, Row, Table, TableState},
};

#[derive(Debug, Clone, Copy, FromValue)]
struct SelectedV {
    x: Index,
    y: Index,
}

#[derive(FromValue)]
struct CellV {
    content: LineV,
    style: Option<StyleV>,
}

#[derive(FromValue)]
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
    rows: Vec<TRef<X, RowV>>,
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
        Ok(Box::new(t))
    }

    async fn set_rows(&mut self, v: Value) -> Result<()> {
        self.rows = compile_each(v, |id| {
            let gx = self.gx.clone();
            async move { TRef::new(gx.compile_ref(id.cast_to::<u64>()?).await?) }
        })
        .await
        .context("rows")?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for TableW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("table")?;
        let sel = &mut self.sel;
        if let Some(r) = sel.selected.update(id, &v).context("table selected")? {
            self.row = r.map(|i| i.0);
        }
        if let Some(c) = sel.selected_column.update(id, &v).context("table column")? {
            self.column = c.map(|i| i.0);
        }
        if let Some(c) = sel.selected_cell.update(id, &v).context("table cell")? {
            (self.row, self.column) = match c {
                Some(c) => (Some(c.y.0), Some(c.x.0)),
                None => (None, None),
            };
        }
        if self.rows_ref.id == id {
            self.set_rows(v.clone()).await?;
        }
        for r in self.rows.iter_mut() {
            r.update(id, &v)?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut table = Table::default()
            .rows(self.rows.iter().filter_map(|r| r.t.as_ref().map(|r| r.build())));
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
