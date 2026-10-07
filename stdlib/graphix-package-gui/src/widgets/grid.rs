use super::{Children, GuiW, GuiWidget, IcedElement};
use crate::types::{GridColumnsV, GridSizingV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct GridW<X: GXExt> {
    gx: GXHandle<X>,
    spacing: TRef<X, f64>,
    columns: TRef<X, GridColumnsV>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, GridSizingV>,
    children: Children<X>,
}

impl<X: GXExt> GridW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            children: u64,
            columns: u64,
            height: u64,
            spacing: u64,
            width: u64,
        }
        let Fields { children, columns, height, spacing, width } =
            source.cast_to().context("grid flds")?;
        let (children_ref, columns, height, spacing, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(columns),
            gx.compile_ref(height),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let children =
            Children::compile(&gx, children_ref).await.context("grid children")?;
        Ok(Box::new(Self {
            gx: gx.clone(),
            spacing: TRef::new(spacing).context("grid tref spacing")?,
            columns: TRef::new(columns).context("grid tref columns")?,
            width: TRef::new(width).context("grid tref width")?,
            height: TRef::new(height).context("grid tref height")?,
            children,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for GridW<X> {
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        self.children.ws.iter_mut().for_each(f)
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        self.children.ws.iter().for_each(f)
    }

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
        changed |= self.children.update(rt, &self.gx, id, v).context("grid children")?;
        Ok(changed)
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
        for child in &self.children.ws {
            g = g.push(child.view());
        }
        g.into()
    }
}
