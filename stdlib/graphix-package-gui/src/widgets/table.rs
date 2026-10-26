use super::{Child, Children, GuiW, GuiWidget, IcedElement, Item, compile_children};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct ColumnProps {
        halign: HAlignV,
        valign: VAlignV,
        width: LengthV,
    }
}

struct CompiledColumn<X: GXExt> {
    header: Child<X>,
    p: ColumnProps<X>,
}

graphix_rt::props! {
    struct Props {
        padding: Option<f64>,
        separator: Option<f64>,
        width: LengthV,
    }
}

pub(crate) struct TableW<X: GXExt> {
    gx: GXHandle<X>,
    columns: Children<X, CompiledColumn<X>>,
    /// Each row's cells beside the value they compiled from.
    // CR claude for claude: [bug/low] Rows are kept by whole-row value. A row whose
    // value changed goes through compile_children, which compiles every cell again. So
    // when one cell changes its widget (a select that swaps text for text_input, say),
    // its siblings in that row are rebuilt as well. A text_editor beside it loses its
    // Content (cursor, selection, scroll) and its unechoed edits, and a text_input or
    // toggle loses its pending sends. row and column keep unchanged children, because
    // Children reconciles per widget. Hold each row's cells beside their values, and
    // reconcile a changed row's cells against the old row at the same index
    // (reconcile(&mut old_cells, new_items, ..)) instead of compiling them all. Found
    // by reading; not run. (gui-widgets-b-02)
    cells: Children<X, Vec<GuiW<X>>>,
    p: Props<X>,
}

impl<X: GXExt> Item<X> for CompiledColumn<X> {
    async fn compile(gx: &GXHandle<X>, item: Value) -> Result<Self> {
        let (header, p) = try_join!(
            Child::field(gx, &item, "header"),
            ColumnProps::compile(gx, &item),
        )
        .context("table column")?;
        Ok(CompiledColumn { header, p })
    }

    fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.header.update(rt, gx, id, v).context("table column header")?;
        Ok(self.p.update(id, v).context("table column")? || changed)
    }
}

/// A table row: its cells.
impl<X: GXExt> Item<X> for Vec<GuiW<X>> {
    fn compile(gx: &GXHandle<X>, v: Value) -> impl Future<Output = Result<Self>> + Send {
        compile_children(gx.clone(), v)
    }

    fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        _gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        for cell in self {
            changed |= cell.handle_update(rt, id, v)?;
        }
        Ok(changed)
    }
}

impl<X: GXExt> TableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (columns, cells, p) = try_join!(
            Children::field(&gx, &source, "columns"),
            Children::field(&gx, &source, "rows"),
            Props::compile(&gx, &source),
        )
        .context("table")?;
        Ok(GuiW::new(Self { gx: gx.clone(), columns, cells, p }))
    }
}

impl<X: GXExt> GuiWidget<X> for TableW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("table")?;
        changed |= self.columns.update(rt, &self.gx, id, v).context("table columns")?;
        changed |= self.cells.update(rt, &self.gx, id, v).context("table rows")?;
        Ok(changed)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        self.columns.ws.iter_mut().for_each(|(_, c)| f(&mut c.header.w));
        self.cells.ws.iter_mut().flat_map(|(_, r)| r.iter_mut()).for_each(f)
    }

    fn view(&self) -> IcedElement<'_> {
        // iced's table divides its cells by its column count at layout.
        if self.columns.ws.is_empty() {
            return iced_widget::Space::new().into();
        }
        let num_rows = self.cells.ws.len();
        let cells = &self.cells.ws;
        let cols = self.columns.ws.iter().enumerate().map(|(c, (_, col))| {
            let header = col.header.w.view();
            let mut tc = widget::table::column(header, move |row: usize| {
                match cells.get(row).and_then(|(_, r)| r.get(c)) {
                    Some(cell) => cell.view(),
                    None => iced_widget::Space::new().into(),
                }
            });
            if let Some(w) = col.p.width.t.as_ref() {
                tc = tc.width(w.0);
            }
            if let Some(a) = col.p.halign.t.as_ref() {
                tc = tc.align_x(a.0);
            }
            if let Some(a) = col.p.valign.t.as_ref() {
                tc = tc.align_y(a.0);
            }
            tc
        });
        let mut t = widget::table::table(cols, 0..num_rows);
        if let Some(w) = self.p.width.t.as_ref() {
            t = t.width(w.0);
        }
        if let Some(Some(p)) = self.p.padding.t {
            t = t.padding(p as f32);
        }
        if let Some(Some(s)) = self.p.separator.t {
            t = t.separator(s as f32);
        }
        t.into()
    }
}
