use super::{compile, compile_children, GuiW, GuiWidget, IcedElement};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use smallvec::SmallVec;
use tokio::try_join;

struct CompiledColumn<X: GXExt> {
    header_ref: Ref<X>,
    header: GuiW<X>,
    width: TRef<X, LengthV>,
    halign: TRef<X, HAlignV>,
    valign: TRef<X, VAlignV>,
}

pub(crate) struct TableW<X: GXExt> {
    gx: GXHandle<X>,
    columns_ref: Ref<X>,
    columns: Vec<CompiledColumn<X>>,
    rows_ref: Ref<X>,
    cells: Vec<Vec<GuiW<X>>>,
    width: TRef<X, LengthV>,
    padding: TRef<X, Option<f64>>,
    separator: TRef<X, Option<f64>>,
}

async fn compile_columns<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<Vec<CompiledColumn<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut cols = Vec::with_capacity(items.len());
    for item in items {
        let [(_, halign_id), (_, header_id), (_, valign_id), (_, width_id)] =
            item.cast_to::<[(ArcStr, u64); 4]>().context("table column flds")?;
        let (halign, header_ref, valign, width) = try_join! {
            gx.compile_ref(halign_id),
            gx.compile_ref(header_id),
            gx.compile_ref(valign_id),
            gx.compile_ref(width_id),
        }?;
        let header = match header_ref.last.as_ref() {
            None => Box::new(super::EmptyW) as GuiW<X>,
            Some(v) => compile(gx.clone(), v.clone()).await.context("table column header")?,
        };
        cols.push(CompiledColumn {
            header_ref,
            header,
            width: TRef::new(width).context("table column tref width")?,
            halign: TRef::new(halign).context("table column tref halign")?,
            valign: TRef::new(valign).context("table column tref valign")?,
        });
    }
    Ok(cols)
}

async fn compile_rows<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<Vec<Vec<GuiW<X>>>> {
    let rows = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut result = Vec::with_capacity(rows.len());
    for row in rows {
        let cells = compile_children(gx.clone(), row).await.context("table row")?;
        result.push(cells);
    }
    Ok(result)
}

impl<X: GXExt> TableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, columns), (_, padding), (_, rows), (_, separator), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("table flds")?;
        let (columns_ref, padding, rows_ref, separator, width) = try_join! {
            gx.compile_ref(columns),
            gx.compile_ref(padding),
            gx.compile_ref(rows),
            gx.compile_ref(separator),
            gx.compile_ref(width),
        }?;
        let compiled_columns = match columns_ref.last.as_ref() {
            None => vec![],
            Some(v) => compile_columns(&gx, v.clone()).await.context("table columns")?,
        };
        let compiled_rows = match rows_ref.last.as_ref() {
            None => vec![],
            Some(v) => compile_rows(&gx, v.clone()).await.context("table rows")?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            columns_ref,
            columns: compiled_columns,
            rows_ref,
            cells: compiled_rows,
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
        if id == self.columns_ref.id {
            self.columns_ref.last = Some(v.clone());
            self.columns = rt
                .block_on(compile_columns(&self.gx, v.clone()))
                .context("table columns recompile")?;
            changed = true;
        }
        for col in &mut self.columns {
            if id == col.header_ref.id {
                col.header_ref.last = Some(v.clone());
                col.header = rt
                    .block_on(compile(self.gx.clone(), v.clone()))
                    .context("table column header recompile")?;
                changed = true;
            }
            changed |= col.header.handle_update(rt, id, v)?;
            changed |=
                col.width.update(id, v).context("table col update width")?.is_some();
            changed |=
                col.halign.update(id, v).context("table col update halign")?.is_some();
            changed |=
                col.valign.update(id, v).context("table col update valign")?.is_some();
        }
        if id == self.rows_ref.id {
            self.rows_ref.last = Some(v.clone());
            self.cells = rt
                .block_on(compile_rows(&self.gx, v.clone()))
                .context("table rows recompile")?;
            changed = true;
        }
        for row in &mut self.cells {
            for cell in row {
                changed |= cell.handle_update(rt, id, v)?;
            }
        }
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        for col in &mut self.columns {
            if let some @ Some(_) = col.header.editor_action(id, action) {
                return some;
            }
        }
        for row in &mut self.cells {
            for cell in row {
                if let some @ Some(_) = cell.editor_action(id, action) {
                    return some;
                }
            }
        }
        None
    }

    fn view(&self) -> IcedElement<'_> {
        let num_rows = self.cells.len();
        let cells = &self.cells;
        let cols = self.columns.iter().enumerate().map(|(c, col)| {
            let header = col.header.view();
            let mut tc = widget::table::column(header, move |row: usize| {
                if row < cells.len() && c < cells[row].len() {
                    cells[row][c].view()
                } else {
                    iced_widget::Space::new().into()
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
