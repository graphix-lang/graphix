use crate::{GuiW, GuiWidget, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget::rule;
use netidx::publisher::Value;

pub(crate) struct HorizontalRuleW<X: GXExt> {
    height: TRef<X, f64>,
}

impl<X: GXExt> HorizontalRuleW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, height)] =
            source.cast_to::<[(ArcStr, u64); 1]>().context("horizontal_rule flds")?;
        let height = gx.compile_ref(height).await?;
        Ok(Box::new(Self {
            height: TRef::new(height).context("horizontal_rule tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for HorizontalRuleW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        Ok(self.height.update(id, v).context("horizontal_rule update height")?.is_some())
    }

    fn view(&self) -> IcedElement<'_> {
        let h = self.height.t.unwrap_or(1.0) as f32;
        rule::horizontal(h).into()
    }
}

pub(crate) struct VerticalRuleW<X: GXExt> {
    width: TRef<X, f64>,
}

impl<X: GXExt> VerticalRuleW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, width)] =
            source.cast_to::<[(ArcStr, u64); 1]>().context("vertical_rule flds")?;
        let width = gx.compile_ref(width).await?;
        Ok(Box::new(Self {
            width: TRef::new(width).context("vertical_rule tref width")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for VerticalRuleW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        Ok(self.width.update(id, v).context("vertical_rule update width")?.is_some())
    }

    fn view(&self) -> IcedElement<'_> {
        let w = self.width.t.unwrap_or(1.0) as f32;
        rule::vertical(w).into()
    }
}
