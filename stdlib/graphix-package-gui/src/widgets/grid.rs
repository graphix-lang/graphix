use super::{compile_children, GuiW, GuiWidget, IcedElement};
use crate::types::{GridColumnsV, GridSizingV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct GridW<X: GXExt> {
    gx: GXHandle<X>,
    spacing: TRef<X, f64>,
    columns: TRef<X, GridColumnsV>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, GridSizingV>,
    children_ref: Ref<X>,
    children: Vec<GuiW<X>>,
}

impl<X: GXExt> GridW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, children), (_, columns), (_, height), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("grid flds")?;
        let (children_ref, columns, height, spacing, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(columns),
            gx.compile_ref(height),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let compiled_children = match children_ref.last.as_ref() {
            None => vec![],
            Some(v) => {
                compile_children(gx.clone(), v.clone()).await.context("grid children")?
            }
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            spacing: TRef::new(spacing).context("grid tref spacing")?,
            columns: TRef::new(columns).context("grid tref columns")?,
            width: TRef::new(width).context("grid tref width")?,
            height: TRef::new(height).context("grid tref height")?,
            children_ref,
            children: compiled_children,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for GridW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.spacing.update(id, v).context("grid update spacing")?.is_some();
        changed |= self.columns.update(id, v).context("grid update columns")?.is_some();
        changed |= self.width.update(id, v).context("grid update width")?.is_some();
        changed |= self.height.update(id, v).context("grid update height")?.is_some();
        if id == self.children_ref.id {
            self.children_ref.last = Some(v.clone());
            self.children = rt
                .block_on(compile_children(self.gx.clone(), v.clone()))
                .context("grid children recompile")?;
            changed = true;
        }
        for child in &mut self.children {
            changed |= child.handle_update(rt, id, v)?;
        }
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        for child in &mut self.children {
            if let some @ Some(_) = child.editor_action(id, action) {
                return some;
            }
        }
        None
    }

    fn view(&self) -> IcedElement<'_> {
        let mut g = widget::Grid::new();
        if let Some(sp) = self.spacing.t {
            g = g.spacing(sp as f32);
        }
        if let Some(cols) = self.columns.t.as_ref() {
            match cols {
                GridColumnsV::Fixed(n) => g = g.columns(*n),
                GridColumnsV::Fluid(max_w) => g = g.fluid(*max_w),
            }
        }
        if let Some(Some(w)) = self.width.t {
            g = g.width(w as f32);
        }
        if let Some(h) = self.height.t.as_ref() {
            g = g.height(h.0);
        }
        for child in &self.children {
            g = g.push(child.view());
        }
        g.into()
    }
}
