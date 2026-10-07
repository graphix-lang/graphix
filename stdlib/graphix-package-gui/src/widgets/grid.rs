use super::{Children, GuiW, GuiWidget, IcedElement};
use crate::types::{GridColumnsV, GridSizingV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        columns: GridColumnsV,
        height: GridSizingV,
        spacing: f64,
        width: Option<f64>,
    }
}

pub(crate) struct GridW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    children: Children<X>,
}

impl<X: GXExt> GridW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, children) = try_join!(
            Props::compile(&gx, &source),
            Children::field(&gx, &source, "children"),
        )
        .context("grid")?;
        Ok(Box::new(Self { gx, p, children }))
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
        let mut changed = self.p.update(id, v).context("grid")?;
        changed |= self.children.update(rt, &self.gx, id, v).context("grid children")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut g = widget::Grid::new();
        if let Some(sp) = self.p.spacing.t {
            g = g.spacing(sp as f32);
        }
        if let Some(cols) = self.p.columns.t.as_ref() {
            match cols {
                GridColumnsV::Fixed(n) => g = g.columns(*n),
                GridColumnsV::Fluid(max_w) => g = g.fluid(*max_w),
            }
        }
        if let Some(Some(w)) = self.p.width.t {
            g = g.width(w as f32);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            g = g.height(h.0);
        }
        for child in &self.children.ws {
            g = g.push(child.view());
        }
        g.into()
    }
}
