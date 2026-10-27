use super::{Child, Children, GuiW, GuiWidget, IcedElement, Item, Shape};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;
use poolshark::local::LPooled;

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

/// The rows property's layout: every row's cells in one list, so a new
/// table keeps each cell whose value it still holds, wherever it moved,
/// and compiles only the cells it did not.
#[derive(Default)]
pub(crate) struct RowEnds(LPooled<Vec<usize>>);

impl Shape for RowEnds {
    fn items(v: Value) -> Result<(LPooled<Vec<Value>>, Self)> {
        let mut items: LPooled<Vec<Value>> = LPooled::take();
        let mut ends: LPooled<Vec<usize>> = LPooled::take();
        for row in v.cast_to::<LPooled<Vec<Value>>>()?.drain(..) {
            items.extend(row.cast_to::<LPooled<Vec<Value>>>()?.drain(..));
            ends.push(items.len());
        }
        Ok((items, RowEnds(ends)))
    }
}

type Rows<X> = Children<X, GuiW<X>, RowEnds>;

impl<X: GXExt> Rows<X> {
    fn len(&self) -> usize {
        self.shape.0.len()
    }

    fn row(&self, i: usize) -> &[(Value, GuiW<X>)] {
        let ends = &self.shape.0;
        let start = i.checked_sub(1).map_or(0, |j| ends[j]);
        &self.ws[start..ends[i]]
    }
}

pub(crate) struct TableW<X: GXExt> {
    gx: GXHandle<X>,
    columns: Children<X, CompiledColumn<X>>,
    pub(crate) rows: Rows<X>,
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

impl<X: GXExt> TableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (columns, rows, p) = try_join!(
            Children::field(&gx, &source, "columns"),
            async { Children::field(&gx, &source, "rows").await.context("table rows") },
            Props::compile(&gx, &source),
        )
        .context("table")?;
        Ok(GuiW::new(Self { gx: gx.clone(), columns, rows, p }))
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
        changed |= self.rows.update(rt, &self.gx, id, v).context("table rows")?;
        Ok(changed)
    }

    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        self.columns.ws.iter_mut().for_each(|(_, c)| f(&mut c.header.w));
        self.rows.ws.iter_mut().for_each(|(_, cell)| f(cell))
    }

    fn view(&self) -> IcedElement<'_> {
        // iced's table divides its cells by its column count at layout.
        if self.columns.ws.is_empty() {
            return iced_widget::Space::new().into();
        }
        let rows = &self.rows;
        let cols = self.columns.ws.iter().enumerate().map(|(c, (_, col))| {
            let header = col.header.w.view();
            let mut tc = widget::table::column(header, move |row: usize| {
                match rows.row(row).get(c) {
                    Some((_, cell)) => cell.view(),
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
        let mut t = widget::table::table(cols, 0..rows.len());
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
