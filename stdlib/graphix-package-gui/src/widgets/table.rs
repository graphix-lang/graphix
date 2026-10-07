use super::{Child, GuiW, GuiWidget, IcedElement, compile_children, reconcile};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
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
    columns_ref: Ref<X>,
    /// Each column beside the value it compiled from.
    columns: Vec<(Value, CompiledColumn<X>)>,
    rows_ref: Ref<X>,
    /// Each row's cells beside the value they compiled from.
    cells: Vec<(Value, Vec<GuiW<X>>)>,
    p: Props<X>,
}

async fn compile_column<X: GXExt>(
    gx: &GXHandle<X>,
    item: Value,
) -> Result<CompiledColumn<X>> {
    let (header, p) =
        try_join!(Child::field(gx, &item, "header"), ColumnProps::compile(gx, &item),)
            .context("table column")?;
    Ok(CompiledColumn { header, p })
}

impl<X: GXExt> TableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (columns_ref, rows_ref, p) = try_join!(
            gx.compile_field(&source, "columns"),
            gx.compile_field(&source, "rows"),
            Props::compile(&gx, &source),
        )
        .context("table")?;
        let items = |r: &Ref<X>| match r.last.as_ref() {
            None => Ok(vec![]),
            Some(v) => v.clone().cast_to::<Vec<Value>>(),
        };
        let columns = reconcile([], items(&columns_ref)?, |c| compile_column(&gx, c))
            .await
            .context("table columns")?;
        let cells = reconcile([], items(&rows_ref)?, |r| compile_children(gx.clone(), r))
            .await
            .context("table rows")?;
        Ok(Box::new(Self { gx: gx.clone(), columns_ref, columns, rows_ref, cells, p }))
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
        if id == self.columns_ref.id && self.columns_ref.last.as_ref() != Some(v) {
            self.columns_ref.last = Some(v.clone());
            let items = v.clone().cast_to::<Vec<Value>>()?;
            let (gx, old) = (&self.gx, self.columns.drain(..));
            self.columns = rt
                .block_on(reconcile(old, items, |c| compile_column(gx, c)))
                .context("table columns")?;
            changed = true;
        }
        for (_, col) in &mut self.columns {
            changed |=
                col.header.update(rt, &self.gx, id, v).context("table column header")?;
            changed |= col.p.update(id, v).context("table column")?;
        }
        if id == self.rows_ref.id && self.rows_ref.last.as_ref() != Some(v) {
            self.rows_ref.last = Some(v.clone());
            let items = v.clone().cast_to::<Vec<Value>>()?;
            let (gx, old) = (&self.gx, self.cells.drain(..));
            self.cells = rt
                .block_on(reconcile(old, items, |r| compile_children(gx.clone(), r)))
                .context("table rows")?;
            changed = true;
        }
        for (_, row) in &mut self.cells {
            for cell in row {
                changed |= cell.handle_update(rt, id, v)?;
            }
        }
        Ok(changed)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        self.columns.iter_mut().for_each(|(_, c)| f(&mut c.header.w));
        self.cells.iter_mut().flat_map(|(_, r)| r.iter_mut()).for_each(f)
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        self.columns.iter().for_each(|(_, c)| f(&c.header.w));
        self.cells.iter().flat_map(|(_, r)| r.iter()).for_each(f)
    }

    fn view(&self) -> IcedElement<'_> {
        // iced's table divides its cells by its column count at layout.
        if self.columns.is_empty() {
            return iced_widget::Space::new().into();
        }
        let num_rows = self.cells.len();
        let cells = &self.cells;
        let cols = self.columns.iter().enumerate().map(|(c, (_, col))| {
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
