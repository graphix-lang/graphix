use super::{GuiW, GuiWidget, IcedElement};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget::rule;
use netidx::publisher::Value;

graphix_rt::props! {
    struct HorizontalProps {
        height: f64,
    }
}

pub(crate) struct HorizontalRuleW<X: GXExt>(HorizontalProps<X>);

impl<X: GXExt> HorizontalRuleW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let p =
            HorizontalProps::compile(&gx, &source).await.context("horizontal_rule")?;
        Ok(Box::new(Self(p)))
    }
}

impl<X: GXExt> GuiWidget<X> for HorizontalRuleW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.0.update(id, v).context("horizontal_rule")
    }

    fn view(&self) -> IcedElement<'_> {
        rule::horizontal(self.0.height.t.unwrap_or(1.0) as f32).into()
    }
}

graphix_rt::props! {
    struct VerticalProps {
        width: f64,
    }
}

pub(crate) struct VerticalRuleW<X: GXExt>(VerticalProps<X>);

impl<X: GXExt> VerticalRuleW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let p = VerticalProps::compile(&gx, &source).await.context("vertical_rule")?;
        Ok(Box::new(Self(p)))
    }
}

impl<X: GXExt> GuiWidget<X> for VerticalRuleW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.0.update(id, v).context("vertical_rule")
    }

    fn view(&self) -> IcedElement<'_> {
        rule::vertical(self.0.width.t.unwrap_or(1.0) as f32).into()
    }
}
