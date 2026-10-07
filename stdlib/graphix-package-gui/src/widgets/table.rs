use super::{Child, GuiW, GuiWidget, IcedElement, compile_children, reconcile};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

struct CompiledColumn<X: GXExt> {
    header: Child<X>,
    width: TRef<X, LengthV>,
    halign: TRef<X, HAlignV>,
    valign: TRef<X, VAlignV>,
}

pub(crate) struct TableW<X: GXExt> {
    gx: GXHandle<X>,
    columns_ref: Ref<X>,
    /// Each column beside the value it compiled from.
    columns: Vec<(Value, CompiledColumn<X>)>,
    rows_ref: Ref<X>,
    /// Each row's cells beside the value they compiled from.
    cells: Vec<(Value, Vec<GuiW<X>>)>,
    width: TRef<X, LengthV>,
    padding: TRef<X, Option<f64>>,
    separator: TRef<X, Option<f64>>,
}

async fn compile_column<X: GXExt>(
    gx: &GXHandle<X>,
    item: Value,
) -> Result<CompiledColumn<X>> {
    #[derive(FromValue)]
    struct Fields {
        halign: u64,
        header: u64,
        valign: u64,
        width: u64,
    }
    let Fields { halign, header, valign, width } =
        item.cast_to().context("table column flds")?;
    let (halign, header, valign, width) = try_join! {
        gx.compile_ref(halign),
        gx.compile_ref(header),
        gx.compile_ref(valign),
        gx.compile_ref(width),
    }?;
    Ok(CompiledColumn {
        header: Child::compile(gx, header).await.context("table column header")?,
        width: TRef::new(width).context("table column tref width")?,
        halign: TRef::new(halign).context("table column tref halign")?,
        valign: TRef::new(valign).context("table column tref valign")?,
    })
}

impl<X: GXExt> TableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            columns: u64,
            padding: u64,
            rows: u64,
            separator: u64,
            width: u64,
        }
        let Fields { columns, padding, rows, separator, width } =
            source.cast_to().context("table flds")?;
        let (columns_ref, padding, rows_ref, separator, width) = try_join! {
            gx.compile_ref(columns),
            gx.compile_ref(padding),
            gx.compile_ref(rows),
            gx.compile_ref(separator),
            gx.compile_ref(width),
        }?;
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
        Ok(Box::new(Self {
            gx: gx.clone(),
            columns_ref,
            columns,
            rows_ref,
            cells,
            width: TRef::new(width).context("table tref width")?,
            padding: TRef::new(padding).context("table tref padding")?,
            separator: TRef::new(separator).context("table tref separator")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TableW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.width.update(id, v).context("table update width")?.is_some();
        changed |= self.padding.update(id, v).context("table update padding")?.is_some();
        changed |=
            self.separator.update(id, v).context("table update separator")?.is_some();
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
            changed |=
                col.width.update(id, v).context("table col update width")?.is_some();
            changed |=
                col.halign.update(id, v).context("table col update halign")?.is_some();
            changed |=
                col.valign.update(id, v).context("table col update valign")?.is_some();
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
            if let Some(w) = col.width.t.as_ref() {
                tc = tc.width(w.0);
            }
            if let Some(a) = col.halign.t.as_ref() {
                tc = tc.align_x(a.0);
            }
            if let Some(a) = col.valign.t.as_ref() {
                tc = tc.align_y(a.0);
            }
            tc
        });
        let mut t = widget::table::table(cols, 0..num_rows);
        if let Some(w) = self.width.t.as_ref() {
            t = t.width(w.0);
        }
        if let Some(Some(p)) = self.padding.t {
            t = t.padding(p as f32);
        }
        if let Some(Some(s)) = self.separator.t {
            t = t.separator(s as f32);
        }
        t.into()
    }
}
