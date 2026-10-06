use super::{GuiW, GuiWidget, IcedElement, compile, compile_children};
use crate::types::{HAlignV, LengthV, VAlignV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
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
    #[derive(FromValue)]
    struct Fields {
        halign: u64,
        header: u64,
        valign: u64,
        width: u64,
    }
    let mut cols = Vec::with_capacity(items.len());
    for item in items {
        let Fields {
            halign: halign_id,
            header: header_id,
            valign: valign_id,
            width: width_id,
        } = item.cast_to().context("table column flds")?;
        let (halign, header_ref, valign, width) = try_join! {
            gx.compile_ref(halign_id),
            gx.compile_ref(header_id),
            gx.compile_ref(valign_id),
            gx.compile_ref(width_id),
        }?;
        let header = match header_ref.last.as_ref() {
            None => Box::new(super::EmptyW) as GuiW<X>,
            Some(v) => {
                compile(gx.clone(), v.clone()).await.context("table column header")?
            }
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

async fn compile_rows<X: GXExt>(gx: &GXHandle<X>, v: Value) -> Result<Vec<Vec<GuiW<X>>>> {
    let rows = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut result = Vec::with_capacity(rows.len());
    // CR claude for claude: [perf] Rows are compiled one after another, and so are
    // columns (line 44), menu groups and menu items (menu_bar.rs:123 and 87). Each
    // element's compile_ref and compile_callable requests wait for the previous
    // element's, one pass of the runtime loop apiece, all inside `rt.block_on` on the
    // GUI thread. A 500-row table update costs at least 500 sequential round trips and
    // a 5x10 menu about 105, each waiting out any cycle in progress, and input and
    // drawing stop meanwhile. `futures::future::try_join_all`, which compile_children
    // already uses, would send each level's requests together. (gui-widgets-b-14)
    for row in rows {
        let cells = compile_children(gx.clone(), row).await.context("table row")?;
        result.push(cells);
    }
    Ok(result)
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

    // CR claude for claude: [bug] TableW forwards on_message to its headers and cells by
    // hand but not before_view. It also keeps the default empty children_mut, so the
    // event loop's before_view (event_loop.rs:317) never reaches a widget inside a
    // table. TooltipW (tooltip.rs:53) lists only child, so its tip gets neither
    // before_view nor on_message. A data_table in a table cell therefore never applies
    // its live sort at a frame (sort_col_dirty, data_table/mod.rs:294), nor the
    // subscription reconcile that layout requests: its rows stay in table order until
    // some unrelated graphix update reaches it through handle_update. One child visitor
    // used by the default on_message and before_view (headers then cells here, child
    // then tip in tooltip) would replace the slice accessors and this hand forwarding.
    // probe: design/review-2026-10-05/repro/gui-widgets-b-08.rs (a column-wrapped
    // control sorts at every frame; the table cell stays unsorted until an unrelated
    // text update; the tooltip's tip never sees a click). (gui-widgets-b-08)
    fn on_message(
        &mut self,
        msg: &super::Message,
        shell: &mut super::MessageShell,
    ) -> bool {
        // Two child groups do not fit one `&mut [GuiW<X>]`, so forward manually.
        let mut changed = false;
        for col in &mut self.columns {
            changed |= col.header.on_message(msg, shell);
        }
        for row in &mut self.cells {
            for cell in row {
                changed |= cell.on_message(msg, shell);
            }
        }
        changed
    }

    fn view(&self) -> IcedElement<'_> {
        // iced's table divides its cells by its column count at layout.
        if self.columns.is_empty() {
            return iced_widget::Space::new().into();
        }
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
